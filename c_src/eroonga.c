// =============================================================================
// =============================================================================

#include <string.h> // strlen

#include "ei.h"

#include "internal.h"

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static void encode_errbuf(ei_x_buff *x, grn_ctx *ctx) {

  char buf[8];
  sprintf(buf, "GRN%04d", ctx->rc);

  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "error");
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, buf);
  ei_x_encode_string(x, ctx->errbuf);
}

static void encode_row(grn_ctx *ctx,
                       grn_id id, grn_obj *column, grn_obj *value, ei_x_buff *x) {

  int n = grn_column_name(ctx, column, NULL, 0);
  char name[n];

  grn_column_name(ctx, column, name, n);

  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_binary(x,  name, n);

  GRN_BULK_REWIND(value);

  switch (grn_obj_get_range(ctx, column)) {
    //GRN_DB_BOOL: GRN_BOOL_VALUE
    //GRN_DB_INT8: GRN_INT8_VALUE
    //GRN_DB_UINT8: GRN_UINT8_VALUE
    //GRN_DB_INT16: GRN_INT16_VALUE
    //GRN_DB_UINT16: GRN_UINT16_VALUE
    //GRN_DB_INT32: GRN_INT32_VALUE
  case GRN_DB_UINT32:
    GRN_UINT32_INIT(value, 0);
    grn_obj_get_value(ctx, column, id, value);
    ei_x_encode_ulong(x, GRN_UINT32_VALUE(value));
    break;
    //GRN_DB_INT64: GRN_INT64_VALUE
    //GRN_DB_UINT64: GRN_UINT64_VALUE
    //GRN_DB_FLOAT: GRN_FLOAT_VALUE
    //GRN_DB_TIME: GRN_TIME_VALUE
  case GRN_DB_SHORT_TEXT:
  case GRN_DB_TEXT:
  case GRN_DB_LONG_TEXT:
    GRN_TEXT_INIT(value, 0);
    grn_obj_get_value(ctx, column, id, value);
    ei_x_encode_binary(x, GRN_TEXT_VALUE(value), GRN_TEXT_LEN(value));
    break;
    //GRN_DB_TOKYO_GEO_POINT: GRN_GEO_POINT_VALUE
    //GRN_DB_WGS84_GEO_POINT: GRN_GEO_POINT_VALUE
  default:
    ei_x_encode_binary(x,  "?", 1);
    break;
  }
}

static void encode_rows(grn_ctx *ctx, grn_obj *table,
                        int n, grn_obj *columns[], grn_obj *expr, ei_x_buff *x) {

  grn_obj *tmp = grn_table_select(ctx, table, expr, NULL, GRN_OP_OR);

  if (NULL != tmp) {

    grn_table_cursor *tc =
      grn_table_cursor_open(ctx, tmp, NULL, 0, NULL, 0, 0, -1, GRN_CURSOR_ASCENDING);

    if (NULL != tc) {

      void *key;
      grn_id id;
      grn_obj value;
      int i;

      ei_x_encode_tuple_header(x, 2);
      ei_x_encode_atom(x, "ok");

      while (GRN_ID_NIL != grn_table_cursor_next(ctx, tc)) {

        if (sizeof(grn_id) == grn_table_cursor_get_key(ctx, tc, &key)) {

          id = *(grn_id *)key;

          ei_x_encode_list_header(x, 1);

          ei_x_encode_tuple_header(x, n);
          for (i = 0; i < n; i++) {
            encode_row(ctx, id, columns[i], &value, x);
          }
        }
      }

      ei_x_encode_empty_list(x);

      grn_obj_unlink(ctx, &value); // TODO

      grn_table_cursor_close(ctx, tc);

    } else {
      encode_errbuf(x, ctx);
    }

    grn_obj_unlink(ctx, tmp);

  } else {
    encode_errbuf(x, ctx);
  }
}

static void get_columns(grn_ctx *ctx, grn_hash *hash, grn_obj *objs[], int size) {

  grn_hash_cursor *hc = grn_hash_cursor_open(ctx, hash, NULL, 0, NULL, 0, 0, -1, 0);

  if (NULL != hc) {

    void *key;
    int i;

    for (i = 0; i < size && GRN_ID_NIL != grn_hash_cursor_next(ctx, hc); i++) {

      if (sizeof(grn_id) == grn_hash_cursor_get_key(ctx, hc, &key)) {
        objs[i] = grn_ctx_at(ctx, *(grn_id *)key);
      }
    }

    grn_hash_cursor_close(ctx, hc);
  }
}

static void table_select(grn_ctx *ctx, const char *name, const char *str, ei_x_buff *x) {

  grn_obj *table = grn_ctx_get(ctx, name, strlen(name));

  if (NULL != table) {

    grn_hash *hash =
      grn_hash_create(ctx, NULL, sizeof(grn_id), 0, GRN_OBJ_TABLE_HASH_KEY);

    if (NULL != hash) {

      const int n = grn_table_columns(ctx, table, NULL, 0, (grn_obj *)hash);

      if (0 < n) {

        grn_obj *columns[n];
        get_columns(ctx, hash, columns, n);

        grn_obj *expr, *var;
        grn_expr_flags flags =
          GRN_EXPR_SYNTAX_SCRIPT | GRN_EXPR_ALLOW_PRAGMA |GRN_EXPR_ALLOW_COLUMN;

        GRN_EXPR_CREATE_FOR_QUERY(ctx, table, expr, var);

        if (GRN_SUCCESS == grn_expr_parse(ctx, expr, str, strlen(str),
                                          NULL, GRN_OP_MATCH, GRN_OP_AND, flags)) {

          encode_rows(ctx, table, n, columns, expr, x);

          grn_obj_unlink(ctx, var);
          grn_obj_unlink(ctx, expr);

        } else {
          encode_errbuf(x, ctx);
        }

        for (int i = 0; i < n; i++) {
          grn_obj_unlink(ctx, columns[i]);
        }
      }

      grn_hash_close(ctx, hash);
    }

    grn_obj_unlink(ctx, table);

  } else {
    encode_errbuf(x, ctx);
  }
}

/*
 */
ERL_NIF_TERM eroonga_table_select(ErlNifEnv* env,
                                  grn_ctx *ctx, const char *name, const char *expr) {
  ei_x_buff x;
  ei_x_new_with_version(&x);

  table_select(ctx, name, expr, &x);

  ERL_NIF_TERM term;
  unsigned char *p = enif_make_new_binary(env, x.index, &term);

  memcpy(p, x.buff, x.index);

  ei_x_free(&x);

  return term;
}

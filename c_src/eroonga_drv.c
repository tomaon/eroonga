// =============================================================================
// Copyright 2013-2014 AONO Tomohiko
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License version 2.1 as published by the Free Software Foundation.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
// =============================================================================

#include <errno.h>
#include <string.h>

#include "ei.h"
#include "erl_interface.h"
#include "erl_driver.h"

#include "eroonga.h"

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

struct _driver_data {
  //
  ErlDrvPort port;
  grn_ctx *ctx;
  //
  char encoding[32]; // TODO
};

typedef struct _driver_data driver_data_t;

typedef void (*driver_func_t)(driver_data_t *data,
                              char *buf, int *index, ei_x_buff *x);

struct _driver_option_t {
  const char *name;
  const void *value;
};

typedef struct _driver_option_t driver_option_t;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static void encode_error(ei_x_buff *x, const char *error) {
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "error");
  ei_x_encode_atom(x, error);
}

static void encode_error1(ei_x_buff *x, const char *error, const char *arg1) {
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "error");
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, error);
  ei_x_encode_string(x, arg1);
}

static void encode_ok(ei_x_buff *x) {
  ei_x_encode_atom(x, "ok");
}

void encode_errbuf(ei_x_buff *x, grn_ctx *ctx) {
  char buf[8];
  sprintf(buf, "GRN%04d", ctx->rc);
  encode_error1(x, buf, ctx->errbuf);
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static void control_db_open(driver_data_t *data,
                            char *buf, int *index, ei_x_buff *x) {
  int arity;
  ei_decode_list_header(buf, index, &arity);

  if (1 == arity) {

    ei_term term;
    ei_decode_ei_term(buf, index, &term);

    if (ERL_BINARY_EXT == term.ei_type) {

      char str[term.size];
      long len;
      ei_decode_binary(buf, index, str, &len); str[len] = '\0';

      grn_obj *database = NULL;
      (void)GRN_DB_OPEN_OR_CREATE(data->ctx, str, NULL, database);

      if (NULL != database) {
        encode_ok(x);
      } else {
        encode_errbuf(x, data->ctx);
      }

      return;
    }
  }

  encode_error(x, "badarg");
}

static void control_set_options(driver_data_t *data,
                                char *buf, int *index, ei_x_buff *x) {

  const driver_option_t TABLE[] = {
    { "encoding", data->encoding },
  };

  const size_t TABLE_SIZE = sizeof(TABLE) / sizeof(TABLE[0]);

  int arity, found = 0;
  ei_term term;
  char atom[MAXATOMLEN_UTF8] = "unknown";

  ei_decode_list_header(buf, index, &arity);

  for (int size = arity, i = 0; !found && i < size; i++) {

    ei_decode_tuple_header(buf, index, &arity);

    if (2 == arity) {

      found = -1;

      ei_decode_atom(buf, index, atom);
      ei_decode_ei_term(buf, index, &term);

      for (size_t j = 0; found && j < TABLE_SIZE; j++) {

        if (0 == strcmp(TABLE[j].name, atom)) {

          switch (term.ei_type) { // erl_interface-*/include/ei.h
          case ERL_STRING_EXT: {
            char p[term.size];
            ei_decode_string(buf, index, p);
            strcpy((char *)TABLE[j].value, p);
            found = 0;
          } break;
          case ERL_BINARY_EXT: {
            char p[term.size+1];
            long len;
            ei_decode_binary(buf, index, p, &len); p[len] = '\0';
            strcpy((char *)TABLE[j].value, p);
            found = 0;
          } break;
          case ERL_INTEGER_EXT:    case ERL_SMALL_INTEGER_EXT: {
            *((long *)TABLE[j].value) = term.value.i_val;
            found = 0;
          } break;
          case ERL_FLOAT_EXT:      case NEW_FLOAT_EXT: {
            *((double *)TABLE[j].value) = term.value.d_val;
            found = 0;
          } break;
          case ERL_ATOM_EXT:       case ERL_ATOM_UTF8_EXT:
          case ERL_SMALL_ATOM_EXT: case ERL_SMALL_ATOM_UTF8_EXT: {
            strcpy((char *)TABLE[j].value, term.value.atom_name);
            found = 0;
          } break;
          case ERL_REFERENCE_EXT:  case ERL_NEW_REFERENCE_EXT: {
            memcpy((erlang_ref *)TABLE[j].value, &term.value.ref, sizeof(erlang_ref));
            found = 0;
          } break;
          case ERL_PORT_EXT: {
            memcpy((erlang_port *)TABLE[j].value, &term.value.port, sizeof(erlang_port));
            found = 0;
          } break;
          case ERL_PID_EXT: {
            memcpy((erlang_pid *)TABLE[j].value, &term.value.pid, sizeof(erlang_pid));
            found = 0;
          } break;
          default:
            // LIST, (SMALL|LARGE)_TUPLE, NIL, ...
            break;
          }
        }
      } // for, j < TABLE_SIZE
    }
  } // for, i < size

  !found ? encode_ok(x) : encode_error1(x, "badarg", atom);
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

extern void eroonga_table_select(grn_ctx *ctx,
                                 const char *name, const char *str, ei_x_buff *x);

static void output_table_select(driver_data_t *data,
                                char *buf, int *index, ei_x_buff *x) {
  int arity;
  ei_decode_list_header(buf, index, &arity);

  if (2 == arity) {

    char name[256];  // TODO
    char expr[4096]; // TODO

    ei_term term;

    ei_decode_ei_term(buf, index, &term);
    if (ERL_BINARY_EXT == term.ei_type) {
      char str[term.size];
      long len;
      ei_decode_binary(buf, index, str, &len); str[len] = '\0';
      strncpy(name, str, sizeof(name)-1);
    }

    ei_decode_ei_term(buf, index, &term);
    if (ERL_BINARY_EXT == term.ei_type) {
      char str[term.size];
      long len;
      ei_decode_binary(buf, index, str, &len); str[len] = '\0';
      strncpy(expr, str, sizeof(expr)-1);
    }

    eroonga_table_select(data->ctx, name, expr, x);

    return;
  }

  encode_error(x, "badarg");
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static int init(void) {
  return GRN_SUCCESS == grn_init() ? 0 : -1;
}

static ErlDrvData start(ErlDrvPort port, char *command) {

  UNUSED(command); // = driver_name

  void *ptr = driver_alloc(sizeof(driver_data_t));

  if (NULL != ptr) {

    driver_data_t *data = (driver_data_t *)ptr;

    int flags = 0;
    grn_ctx *ctx = grn_ctx_open(flags);

    if (NULL != ctx) {

      set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

      data->port = port;
      data->ctx = ctx;

      strcpy(data->encoding, "utf8");

      return (ErlDrvData)data;
    }

    driver_free(data);

    return ERL_DRV_ERROR_GENERAL;
  }

  errno = ENOMEM;
  return ERL_DRV_ERROR_ERRNO;
}

static void stop(ErlDrvData drv_data) {

  driver_data_t *data = (driver_data_t *)drv_data;

  if (NULL != data) {

    grn_ctx *ctx = data->ctx;

    if (NULL != ctx) {
      grn_ctx_close(ctx);
    }

    driver_free(data);
  }
}

static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {

  UNUSED(len);

  static const driver_func_t TABLE[] = {
    /* ERN_OUTPUT_TABLE_SELECT */       output_table_select,
  };

  static const size_t TABLE_SIZE = sizeof(TABLE) / sizeof(TABLE[0]);

  driver_data_t *data = (driver_data_t *)drv_data;

  ei_x_buff x;
  ei_x_new_with_version(&x);

  int index = 0, version, arity;
  ei_decode_version(buf, &index, &version); // version = ERL_VERSION_MAGIC (131)
  ei_decode_tuple_header(buf, &index, &arity);

  if (2 == arity) {

    ei_x_buff x2;
    ei_x_new(&x2);

    unsigned long command;
    ei_decode_ulong(buf, &index, &command);

    driver_func_t func = TABLE_SIZE > command ? TABLE[command] : NULL;

    if (NULL != func) {
      func(data, buf, &index, &x2);
    } else {
      encode_error(&x2, "enosys");
    }

    ei_x_append(&x, &x2);

    ei_x_free(&x2);

  } else {
    encode_error(&x, "badarg");
  }

  driver_output(data->port, x.buff, x.index);

  ei_x_free(&x);
}

static void finish(void) {
  grn_fin();
}

static ErlDrvSSizeT control(ErlDrvData drv_data, unsigned int command,
                            char *buf, ErlDrvSizeT len,
                            char **rbuf, ErlDrvSizeT rlen) {
  UNUSED(len);

  static const driver_func_t TABLE[] = {
    /* ERN_CONTROL_DB_OPEN     */       control_db_open,
    /* ERN_CONTROL_SET_OPTIONS */       control_set_options,
  };

  static const size_t TABLE_SIZE = sizeof(TABLE) / sizeof(TABLE[0]);

  driver_data_t *data = (driver_data_t *)drv_data;

  ei_x_buff x;
  ei_x_new_with_version(&x);

  int index = 0, version;
  ei_decode_version(buf, &index, &version); // version = ERL_VERSION_MAGIC (131)

  driver_func_t func = TABLE_SIZE > command ? TABLE[command] : NULL;

  if (NULL != func) {
    func(data, buf, &index, &x);
  } else {
    encode_error(&x, "enosys");
  }

  ErlDrvSizeT result = x.index;

  if (rlen >= result) {

    memcpy(*rbuf, x.buff, result);

  } else {

    ErlDrvBinary *binary = driver_alloc_binary(result);

    if (NULL != binary) {

      memcpy(&binary->orig_bytes[0], x.buff, result);

      *rbuf = (char *)binary;

    } else {

      ei_x_new_with_version(&x); // reset
      encode_error(&x, "nomem");

      memcpy(*rbuf, x.buff, result = x.index);
    }
  }

  ei_x_free(&x);

  return result;
}

static ErlDrvSSizeT call(ErlDrvData drv_data, unsigned int command,
                         char *buf, ErlDrvSizeT len,
                         char **rbuf, ErlDrvSizeT rlen, unsigned int *flags) {
  UNUSED(flags); // ?

  return control(drv_data, command, buf, len, rbuf, rlen); // > rlen -> badarg
}

static ErlDrvEntry driver_entry = {
  .init = init,
  .start = start,
  .stop = stop,
  .output = output,
  .ready_input = NULL,
  .ready_output = NULL,
  .driver_name = "eroonga_drv",
  .finish = finish,
  .handle = NULL,
  .control = control,
  .timeout = NULL,
  .outputv = NULL,
  .ready_async = NULL,
  .flush = NULL,
  .call = call,
  .event = NULL,
  .extended_marker = ERL_DRV_EXTENDED_MARKER,
  .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
  .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION,
  .driver_flags= ERL_DRV_FLAG_USE_PORT_LOCKING,
  .handle2 = NULL,
  .process_exit = NULL,
  .stop_select = NULL,
};

/*
 */
DRIVER_INIT(driver) {
  return &driver_entry;
}

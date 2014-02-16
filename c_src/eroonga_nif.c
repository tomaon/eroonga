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

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "eroonga.h"

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

typedef struct _eroonga_nif_t eroonga_nif_t;

struct _eroonga_nif_t {
  grn_ctx *ctx;
};

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static eroonga_nif_t *alloc_resource(ErlNifEnv *env) {
  ErlNifResourceType *type = (ErlNifResourceType *)enif_priv_data(env);
  return enif_alloc_resource(type, sizeof(eroonga_nif_t));
}

static void release_resource(eroonga_nif_t **resource) {
  enif_release_resource(*resource);
  *resource = NULL;
}


static int get_resource(ErlNifEnv *env, ERL_NIF_TERM term, eroonga_nif_t **objp) {
  ErlNifResourceType *type = (ErlNifResourceType *)enif_priv_data(env);
  return enif_get_resource(env, term, type, (void **)objp);
}

static int get_string(ErlNifEnv *env, ERL_NIF_TERM term, char* buf, unsigned size) {
  return enif_get_string(env, term, buf, size, ERL_NIF_LATIN1);
}


static ERL_NIF_TERM make_atom(ErlNifEnv *env, const char *string) {
  ERL_NIF_TERM atom;
  return enif_make_existing_atom(env, string, &atom, ERL_NIF_LATIN1)
    ? atom : enif_make_string(env, string, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM make_resource(ErlNifEnv *env, const void *obj) {
  return enif_make_resource(env, (void *)obj);
}

static ERL_NIF_TERM make_string(ErlNifEnv *env, const char *string) {
  return enif_make_string(env, string, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM make_tuple2(ErlNifEnv *env, const ERL_NIF_TERM e1, const ERL_NIF_TERM e2) {
  return enif_make_tuple2(env, e1, e2);
}


static ERL_NIF_TERM make_error(ErlNifEnv *env, const char *reason) {
  return make_tuple2(env, make_atom(env, "error"), make_string(env, reason));
}

static ERL_NIF_TERM make_errbuf(ErlNifEnv *env, const grn_ctx *ctx) {
  char buf[8];
  sprintf(buf, "GRN%04d", ctx->rc);
  return make_tuple2(env,
                     make_atom(env, "error"),
                     make_tuple2(env,
                                 make_string(env, buf),
                                 make_string(env, ctx->errbuf)));
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static ERL_NIF_TERM db_open_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {

  unsigned path_size;

  if (2 == argc &&
      enif_is_binary(env, argv[0]) &&
      enif_is_list(env, argv[1]) && enif_get_list_length(env, argv[1], &path_size)) {

    eroonga_nif_t *handle = NULL;
    char path[path_size+1];

    if (get_resource(env, argv[0], &handle) &&
        get_string(env, argv[1], path, sizeof(path))) {

      grn_obj *database = NULL;
      (void)GRN_DB_OPEN_OR_CREATE(handle->ctx, path, NULL, database);

      if (NULL == database) {
        return make_errbuf(env, handle->ctx);
      }

      return make_atom(env, "ok");
    }
  }

  return make_error(env, "badarg");
}

static ERL_NIF_TERM table_select_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {

  unsigned name_size, expr_size;

  if (3 == argc &&
      enif_is_binary(env, argv[0]) &&
      enif_is_list(env, argv[1]) && enif_get_list_length(env, argv[1], &name_size) &&
      enif_is_list(env, argv[2]) && enif_get_list_length(env, argv[2], &expr_size)) {

    eroonga_nif_t *handle = NULL;
    char name[name_size+1];
    char expr[expr_size+1];

    if (get_resource(env, argv[0], &handle) &&
        get_string(env, argv[1], name, sizeof(name)) &&
        get_string(env, argv[2], expr, sizeof(expr))) {

      return eroonga_table_select(env, handle->ctx, name, expr);
    }
  }

  return make_error(env, "badarg");
}


static ERL_NIF_TERM new_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {

  if (1 == argc &&
      enif_is_empty_list(env, argv[0])) {

    eroonga_nif_t *resource = alloc_resource(env);

    if (NULL != resource) {

      resource->ctx = NULL;

    } else {
      return make_error(env, "enomem");
    }

    int flags = 0;
    grn_ctx *ctx = grn_ctx_open(flags);

    if (NULL != ctx) {

      resource->ctx = ctx;

    } else {
      return make_error(env, "enomem");
    }

    return make_tuple2(env, make_atom(env, "ok"), make_resource(env, resource));
  }

  return make_error(env, "badarg");
}

static ERL_NIF_TERM delete_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {

  if (1 == argc &&
      enif_is_binary(env, argv[0])) {

    eroonga_nif_t *resource = NULL;

    if (get_resource(env, argv[0], &resource)) {

      release_resource(&resource);

      return make_atom(env, "ok");
    }
  }

  return make_error(env, "badarg");
}

static void dtor(ErlNifEnv *env, void *obj) {

  UNUSED(env);

  eroonga_nif_t *resource = (eroonga_nif_t *)obj;

  if (NULL != resource->ctx) {
    grn_ctx_close(resource->ctx);
    resource->ctx = NULL;
  }
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static ErlNifFunc funcs[] = {
  {"new_nif", 1, new_nif},
  {"delete_nif", 1, delete_nif},
  {"db_open_nif", 2, db_open_nif},
  {"table_select_nif", 3, table_select_nif},
};

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {

  if (!enif_is_empty_list(env, load_info)) {
    return EINVAL;
  }

  ErlNifResourceType *type =
    enif_open_resource_type(env, NULL, "eroonga_nif_t", dtor, ERL_NIF_RT_CREATE, NULL);

  if (NULL == type) {
    return ENOMEM;
  }

  if (GRN_SUCCESS != grn_init()) {
    return ECONNREFUSED;
  }

  *priv_data = type;

  return 0;
}

static int upgrade(ErlNifEnv *env,
                   void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info) {

  UNUSED(env), UNUSED(load_info);

  *priv_data = *old_priv_data;

  return 0;
}

static void unload(ErlNifEnv *env, void *priv_data) {

  UNUSED(env), UNUSED(priv_data);

  grn_fin();

  //enif_clear_env(env); ?
}

/*
 */
ERL_NIF_INIT(eroonga_nif, funcs, load, NULL, upgrade, unload);

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

#ifndef EROONGA_H
#define EROONGA_H

#include "groonga/groonga.h"

#include "erl_nif.h"

// -- --

#define UNUSED(p) (void)(p)

// -- --

extern ERL_NIF_TERM eroonga_table_select(ErlNifEnv* env,
                                         grn_ctx *ctx, const char *name, const char *expr);

#endif // EROONGA_H

%% =============================================================================
%% Copyright 2013-2014 AONO Tomohiko
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License version 2.1 as published by the Free Software Foundation.
%%
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%% =============================================================================

-module(eroonga_nif).

-include("internal.hrl").

%% -- public --
-export([on_load/0]).
-export([new/0]).
-export([db_open/2]).
-export([table_select/3]).

%% -- private --
-record(eroonga_nif, {
          handle :: binary()
         }).

-type(eroonga_nif() :: #eroonga_nif{}).

-define(APP, eroonga).

%% == public ==

-on_load(on_load/0).

-spec on_load() -> ok|{error,_}.
on_load() ->
    Path = filename:join([baseline_app:lib_dir(?APP),?MODULE_STRING]),
    LoadInfo = [],
    erlang:load_nif(Path, LoadInfo).


-spec new() -> {ok,eroonga_nif()}|{error,_}.
new() ->
    case new_nif([]) of
        {ok, Binary} ->
            {ok, #eroonga_nif{handle = Binary}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec db_open(eroonga_nif(),string()) -> ok|{error,_}.
db_open(#eroonga_nif{handle=H}, Path)
  when is_binary(H), is_list(Path) ->
    db_open_nif(H, Path).


-spec table_select(eroonga_nif(),string(),string()) -> {ok,term()}|{error,_}.
table_select(#eroonga_nif{handle=H}, Table, Expr)
  when is_binary(H), is_list(Table), is_list(Expr) ->
    binary_to_term(table_select_nif(H, Table, Expr)).


%% == private: nif ==

new_nif(_Args) ->
    erlang:nif_error(not_loaded).

db_open_nif(_Handle, _Path) ->
    erlang:nif_error(not_loaded).

table_select_nif(_Handle, _Table, _Expr) ->
    erlang:nif_error(not_loaded).

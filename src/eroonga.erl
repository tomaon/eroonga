%% =============================================================================
%% Copyright 2013 AONO Tomohiko
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

-module(eroonga).

%% -- public: application --
-export([start/0, stop/0, get_client_version/0]).

%% -- public: pool --
-export([connect/1, close/2]).

%% -- public: worker --
-export([command/2]).

-export([status/1, table_list/1]).

%% == public: application ==

-spec start() -> ok|{error,_}.
start() ->
    ok = lists:foreach(fun application:start/1, eroonga_app:deps()),
    application:start(?MODULE).

-spec stop() -> ok|{error,_}.
stop() ->
    application:stop(?MODULE).

-spec get_client_version() -> {ok,[non_neg_integer()]}.
get_client_version() ->
    {ok, eroonga_app:version()}.

%% == public: pool ==

-spec connect(atom()) -> {ok,pid()}|{error,_}.
connect(Pool)
  when is_atom(Pool) ->
    connect(Pool, false).

-spec connect(atom(),boolean()) -> {ok,pid()}|{error,_}.
connect(Pool, Block)
  when is_atom(Pool), is_boolean(Block) ->
    connect(Pool, Block, timer:seconds(5)).

-spec connect(atom(),boolean(),timeout()) -> {ok,pid()}|{error,_}.
connect(Pool, Block, Timeout)
  when is_atom(Pool), is_boolean(Block) ->
    eroonga_app:checkout(Pool, Block, Timeout).

-spec close(atom(),pid()) -> ok|{error,_}.
close(Pool, Worker)
  when is_atom(Pool), is_pid(Worker) ->
    eroonga_app:checkin(Pool, Worker).

%% == public: worker ==

-spec command(pid(),binary()) -> {ok,term()}|{error,_}.
command(Pid, Binary)
  when is_pid(Pid), is_binary(Binary) ->
    eroonga_client:call(Pid, {call,[Binary]}).


-spec status(pid()) -> {ok,term()}|{error,_}.
status(Pid)
  when is_pid(Pid) ->
    command(Pid, <<"status">>).

-spec table_list(pid()) -> {ok,term()}|{error,_}.
table_list(Pid)
  when is_pid(Pid) ->
    command(Pid, <<"table_list">>).

%% TODO
%%  cache_limit, check, clearlock, column_create, column_list, column_remove,
%%  column_rename, define_selector, defrag, delete, dump, load, log_level,
%%  log_put, log_reopen, normalize, quit, register, ruby_eval, ruby_load,
%%  select, shutdown, suggest, table_create, table_remove, tokenize,truncate

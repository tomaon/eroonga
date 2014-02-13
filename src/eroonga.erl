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

-module(eroonga).

-include("internal.hrl").

%% -- public --
-export([start/0, start/1, stop/0, version/0]).

%% -- public: client --
-export([connect/1, close/2]).
-export([command/2, command/3]).

%% -- public: driver --
-export([new/0]).
-export([open/2]).
-export([select/3]).

%% == public ==

-spec start() -> ok|{error,_}.
start() ->
    start(temporary).

-spec start(atom()) -> ok|{error,_}.
start(Type)
  when is_atom(Type) ->
    baseline_app:ensure_start(?MODULE, Type).

-spec stop() -> ok|{error,_}.
stop() ->
    application:stop(?MODULE).

-spec version() -> [non_neg_integer()].
version() ->
    baseline_app:version(?MODULE).

%% == public: client ==

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


-spec command(pid(),binary()) -> {ok,term()}|{error,_}.
command(Pid, Binary)
  when is_pid(Pid), is_binary(Binary) ->
    command(eroonga_client, Pid, Binary). % TODO

-spec command(module(),pid(),binary()) -> {ok,term()}|{error,_}.
command(Module, Pid, Binary)
  when is_atom(Module), is_pid(Pid), is_binary(Binary) ->
    apply(Module, call, [Pid, {call,[Binary]}]).


%% == public: driver ==

-spec new() -> {ok,tuple()}|{error,_}.
new() ->
    eroonga_nif:new().


-spec open(tuple(),string()) -> ok|{error,_}.
open(Handle, Path)
  when is_tuple(Handle), is_list(Path) ->
    eroonga_nif:db_open(Handle, Path).


-spec select(tuple(),string(),string()) -> {ok,term()}|{error,_}.
select(Handle, Table, Expr)
  when is_tuple(Handle), is_list(Table), is_list(Expr) ->
    eroonga_nif:table_select(Handle, Table, Expr).

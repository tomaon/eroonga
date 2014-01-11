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
-export([start/0, stop/0, version/0]).

%% -- public: pool --
-export([connect/1, close/2]).

%% -- public: client --
-export([command/2, command/3]).

%% -- public: driver --
-export([select/3, select/4]).

%% == public ==

-spec start() -> ok|{error,_}.
start() ->
    baseline_app:start(?MODULE).

-spec stop() -> ok|{error,_}.
stop() ->
    baseline_app:stop(?MODULE).

-spec version() -> [non_neg_integer()].
version() ->
    baseline_app:version(?MODULE).

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

%% == public: client ==

-spec command(pid(),binary()) -> {ok,term()}|{error,_}.
command(Pid, Binary)
  when is_pid(Pid), is_binary(Binary) ->
    command(eroonga_client, Pid, Binary). % TODO

-spec command(module(),pid(),binary()) -> {ok,term()}|{error,_}.
command(Module, Pid, Binary)
  when is_atom(Module), is_pid(Pid), is_binary(Binary) ->
    apply(Module, call, [Pid, {call,[Binary]}]).


%% == public: driver ==

-spec select(pid(),binary(),binary()) -> {ok,term()}|{error,_}.
select(Pid, Table, Expression)
  when is_pid(Pid), is_binary(Table), is_binary(Expression) ->
    select(eroonga_port, Pid, Table, Expression). % TODO

-spec select(module(),pid(),binary(),binary()) -> {ok,term()}|{error,_}.
select(Module, Pid, Table, Expression)
  when is_atom(Module), is_pid(Pid), is_binary(Table), is_binary(Expression) ->
    apply(Module, call, [Pid,?ERN_OUTPUT_TABLE_SELECT,[Table,Expression]]).

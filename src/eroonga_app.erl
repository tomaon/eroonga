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

-module(eroonga_app).

-include("eroonga_internal.hrl").

%% -- public --
-export([checkin/2, checkout/3]).
-export([env/0, deps/0, version/0]).

%% -- behaviour: application --
-behaviour(application).
-export([start/2, stop/1]).

%% == public ==

-spec checkin(atom(),pid()) -> ok|{error,_}.
checkin(Pool, Worker)
  when is_atom(Pool), is_pid(Worker) ->
    case eroonga_sup:find(eroonga_sup, Pool) of
        undefined ->
            {error, badarg};
        Child ->
            poolboy:checkin(Child, Worker)
    end.

-spec checkout(atom(),boolean(),timeout()) -> {ok,pid()}|{error,_}.
checkout(Pool, Block, Timeout)
  when is_atom(Pool), is_boolean(Block) ->
    case eroonga_sup:find(eroonga_sup, Pool) of
        undefined ->
            {error, badarg};
        Child ->
            case poolboy:checkout(Child, Block, Timeout) of
                full ->
                    {error, full};
                Pid ->
                    {ok, Pid}
            end
    end.

-spec env() ->[property()].
env() ->
    _ = application:load(eroonga),
    lists:sort(application:get_all_env(eroonga)).

-spec deps() -> [atom()].
deps() ->
    _ = application:load(eroonga),
    {ok, List} = application:get_key(eroonga, applications),
    lists:foldl(fun proplists:delete/2, List, [kernel,stdlib]).

-spec version() -> [non_neg_integer()].
version() ->
    _ = application:load(eroonga),
    {ok, List} = application:get_key(eroonga, vsn),
    lists:map(fun list_to_integer/1, string:tokens(List, ".")).

%% == behaviour: application ==

-record(state, {
          driver :: tuple(), % eroonga_driver:handle()
          sup :: pid()
         }).

start(_StartType, StartArgs) ->
    try lists:foldl(fun setup/2, setup(StartArgs), env()) of
        #state{sup=P}=S ->
            {ok, P, S}
    catch
        {Reason, State} ->
            cleanup(State),
            {error, Reason}
    end.

stop(State) ->
    cleanup(State).

%% == private: state ==

cleanup(#state{sup=P}=S)
  when undefined =/= P ->
    _ = eroonga_sup:stop(P),
    cleanup(S#state{sup = undefined});
cleanup(#state{driver=D}=S)
  when undefined =/= D ->
    _ = eroonga_driver:unload(D),
    cleanup(S#state{driver = undefined});
cleanup(#state{}) ->
    eroonga_util:flush().

setup([]) ->
    #state{}.

setup({driver,Term}, #state{driver=undefined}=S)
  when is_list(Term) ->
    case eroonga_driver:load(Term) of
        {ok, Driver} ->
            S#state{driver = Driver};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({poolboy,Term}, #state{driver=D,sup=undefined}=S)
  when is_list(Term), is_tuple(D) ->
    T = {one_for_one, 0, timer:seconds(1)},
    L = [ poolboy:child_spec(Pool,
                             [{worker_module,Worker}|PoolArgs],
                             [{driver,D}|WorkerArgs]
                            ) || {Pool,PoolArgs,Worker,WorkerArgs} <- Term ],
    case eroonga_sup:start_link({local,eroonga_sup}, {T,L}) of
        {ok, Pid} ->
            S#state{sup = Pid};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup(_Ignore, #state{}=S) ->
    S.

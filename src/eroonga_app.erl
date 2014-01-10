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

-include("internal.hrl").

%% -- public --
-export([checkin/2, checkout/3]).

%% -- behaviour: application --
-behaviour(application).
-export([start/2, stop/1]).

%% -- private --
-record(state, {
          handle :: tuple(), % baseline_port:handle()
          sup :: pid()
         }).

%% == public ==

-spec checkin(atom(),pid()) -> ok|{error,_}.
checkin(Pool, Worker)
  when is_atom(Pool), is_pid(Worker) ->
    case baseline_sup:find(eroonga_sup, Pool) of
        undefined ->
            {error, badarg};
        Child ->
            poolboy:checkin(Child, Worker)
    end.

-spec checkout(atom(),boolean(),timeout()) -> {ok,pid()}|{error,_}.
checkout(Pool, Block, Timeout)
  when is_atom(Pool), is_boolean(Block) ->
    case baseline_sup:find(eroonga_sup, Pool) of
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

%% == behaviour: application ==

start(_StartType, StartArgs) ->
    try lists:foldl(fun setup/2, #state{}, env(StartArgs)) of
        #state{sup=P}=S ->
            {ok, P, S}
    catch
        {Reason, State} ->
            ok = cleanup(State),
            {error, Reason}
    end.

stop(State) ->
    cleanup(State).

%% == private: state ==

cleanup(#state{sup=P}=S)
  when undefined =/= P ->
    _ = baseline_sup:stop(P),
    cleanup(S#state{sup = undefined});
cleanup(#state{handle=H}=S)
  when undefined =/= H ->
    _ = baseline_port:unload(H),
    cleanup(S#state{handle = undefined});
cleanup(#state{}) ->
    baseline:flush().

setup({handle,Term}, #state{handle=undefined}=S)
  when is_list(Term) ->
    case baseline_port:load(Term) of
        {ok, Handle} ->
            S#state{handle = Handle};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({poolboy,Term}, #state{handle=H,sup=undefined}=S)
  when is_list(Term), is_tuple(H) ->
    T = {one_for_one, 0, timer:seconds(1)},
    L = [ poolboy:child_spec(Pool,
                             [{worker_module,Worker}|PoolArgs],
                             [{driver,H}|WorkerArgs]
                            ) || {Pool,PoolArgs,Worker,WorkerArgs} <- Term ],
    case baseline_sup:start_link({local,eroonga_sup}, {T,L}) of
        {ok, Pid} ->
            S#state{sup = Pid};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup(_Ignore, #state{}=S) ->
    S.

%% == private ==

env(List) ->
    env(eroonga, List).

env(App, List) ->
    merge(baseline_app:env(App), List).

merge(List1, List2) ->
    List1 ++ baseline_lists:merge(List1, List2).

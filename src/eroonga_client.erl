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

-module(eroonga_client).

-include("eroonga_internal.hrl").

%% -- public --
-export([start_link/1, stop/1]).
-export([call/2, cast/2]).

%% -- behaviour: gen_server --
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- private --
-record(state, {
          handle :: tuple()
         }).

%% == public ==

-spec start_link([property()]) -> {ok,pid()}|ignore|{error,_}.
start_link(Args)
  when is_list(Args) ->
    case gen_server:start_link(?MODULE, [], []) of
        {ok, Pid} ->
            case gen_server:call(Pid, {setup,Args}, infinity) of
                ok ->
                    {ok, Pid};
                {error, Reason} ->
                    stop(Pid),
                    {error, Reason}
            end;
        Other ->
            Other
    end.

-spec stop(pid()) -> ok.
stop(Pid)
  when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

-spec call(pid(),term()) -> term().
call(Pid, Command)
  when is_pid(Pid) ->
    gen_server:call(Pid, Command).

-spec cast(pid(),term()) -> ok.
cast(Pid, Command)
  when is_pid(Pid) ->
    gen_server:cast(Pid, Command).

%% == behaviour: gen_server ==

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({setup,Args}, _From, #state{}=S) ->
    case setup(Args, S) of
        {ok, State} ->
            {reply, ok, State};
        {error, Reason, State} ->
            {reply, {error,Reason}, State}
    end;
handle_call({Func,Args}, _From, #state{handle=H}=S)
  when is_atom(Func), is_list(Args)->
    case apply(eroonga_protocol_qgtp, Func, [H|Args]) of
        {ok, Term, Handle} ->
            {reply, {ok,Term}, S#state{handle = Handle}};
        {error, Reason, Handle} ->
            {reply, {error,Reason}, S#state{handle = Handle}}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error,badarg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT',_Pid,Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% == private: state ==

cleanup(#state{handle=H}=S)
  when undefined =/= H ->
    ok = eroonga_protocol_qgtp:close(H),
    cleanup(S#state{handle = undefined});
cleanup(#state{}) ->
    flush().

setup([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

setup(Args, #state{handle=undefined}=S) ->
    L = [
         proplists:get_value(address, Args, "localhost"),
         proplists:get_value(port, Args, 10041),
         proplists:get_value(options, Args, default_options()),
         timer:seconds(proplists:get_value(timeout, Args, 10))
        ],
    case apply(eroonga_protocol_qgtp, connect, [L]) of
        {ok, Handle} ->
            setup(Args, S#state{handle = Handle});
        {error, Reason, Handle} ->
            {error, Reason, S#state{handle = Handle}}
    end;
setup(_Args, #state{}=S) ->
    {ok, S}.

%% == private ==

default_options() ->
    [
     {active, false},
     {buffer, 293976},
     {keepalive, true},
     {mode, binary},
     {packet, raw},
     {recbuf, 146988},
     {sndbuf, 146988}
    ].

flush() ->
    receive _ -> flush() after 0 -> ok end.

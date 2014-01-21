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

-module(eroonga_drv).

-include("internal.hrl").

%% -- public --
-export([start_link/3, stop/1]).
-export([call/3, call/4]).

%% -- callback: poolboy_worker --
-export([start_link/1]).

%% == public ==

-spec start_link(string()|binary(),[property()],[property()]) -> {ok,pid()}|{error,_}.
start_link(Name, Settings, Options)
  when is_list(Name), is_list(Settings), is_list(Options) ->
    case baseline_drv:start_link(Name, Settings) of
        {ok, Pid} ->
            try lists:foldl(fun setup/2, Pid, Options) of
                Pid ->
                    {ok, Pid}
            catch
                Reason ->
                    ok = stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
start_link(Name, Settings, Options)
  when is_binary(Name), is_list(Settings), is_list(Options) ->
    start_link(binary_to_list(Name), Settings, Options).

-spec stop(pid()) -> ok.
stop(Pid)
  when is_pid(Pid) ->
    baseline_drv:stop(Pid).


-spec call(pid(),integer(),[term()]) -> term()|{error,_}.
call(Pid, Command, Args)
  when is_pid(Pid), is_integer(Command), is_list(Args) ->
    call(Pid, Command, Args, timer:seconds(5)).

-spec call(pid(),integer(),[term()],timeout()) -> term()|{error,_}.
call(Pid, Command, Args, Timeout)
  when is_pid(Pid), is_integer(Command), is_list(Args) ->
    baseline_drv:call(Pid, command, Command, Args, Timeout).

%% == callback: poolboy_worker ==

-spec start_link([term()]) -> {ok,pid()}|{error,_}.
start_link(Args)
  when is_list(Args), 3 =:= length(Args) ->
    apply(?MODULE, start_link, Args).

%% == private ==

setup({path,Term}, Pid)
  when is_pid(Pid) ->
    Path = if is_binary(Term) -> Term;
              is_list(Term)   -> list_to_binary(Term);
              true -> throw({badarg,path})
           end,
    case baseline_drv:call(Pid, control, ?ERN_CONTROL_DB_OPEN, [Path], infinity) of
        ok ->
            Pid;
        {error, Reason} ->
            throw(Reason)
    end;
setup({options,Term}, Pid)
  when is_pid(Pid) ->
    Args = if is_list(Term) -> proplists:unfold(Term);
              true -> throw({badarg,options})
           end,
    case baseline_drv:call(Pid, control, ?ERN_CONTROL_SET_OPTIONS, Args, infinity) of
        ok ->
            Pid;
        {error, Reason} ->
            throw(Reason)
    end;
setup(_Ignore, Pid) ->
    Pid.

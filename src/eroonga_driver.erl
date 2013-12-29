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

-module(eroonga_driver).

-include("eroonga_internal.hrl").

%% -- public --
-export([load/1, unload/1]).
-export([path/1, name/1, settings/1]).
-export([open/1, open/2, close/1]).
-export([call/3, command/3, control/3]).

%% -- private --
-record(handle, {
          path :: string(),
          name :: string(),
          settings :: [property()]
         }).

-type(handle() :: #handle{}).

%% == public ==

-spec load([property()]) -> {ok,handle()}|{error,_}.
load(Configs)
  when is_list(Configs) ->
    try lists:foldl(fun set_handle/2, setup(), Configs) of
        #handle{path=P,name=N}=H ->
            case erl_ddll:load(P, N) of
                ok ->
                    {ok, H};
                {error, already_loaded} ->
                    {ok, H};
                {error, Reason} ->
                    ok = error_logger:error_report([erl_ddll:format_error(Reason),H]),
                    {error, Reason}
            end
    catch
        Reason ->
            {error, Reason}
    end.

-spec unload(handle()) -> ok|{error,_}.
unload(#handle{name=N})
  when undefined =/= N ->
    erl_ddll:unload(N).

-spec path(handle()) -> string().
path(#handle{path=P})
  when undefined =/= P ->
    P.

-spec name(handle()) -> string().
name(#handle{name=N})
  when undefined =/= N ->
    N.

-spec settings(handle()) -> [property()].
settings(#handle{settings=L})
  when undefined =/= L ->
    L.

-spec open(handle()) -> {ok,port()}|{error,_}.
open(#handle{settings=L}=H) ->
    open(H, L).

-spec open(handle(),[property()]) -> {ok,port()}|{error,_}.
open(#handle{name=N}, Settings)
  when undefined =/= N, is_list(Settings) ->
    try open_port({spawn_driver,N}, [binary|proplists:delete(binary,Settings)]) of
        Port ->
            {ok, Port}
    catch
        error:Reason ->
            {error, Reason}
    end.

-spec close(port()) -> ok.
close(Port)
  when is_port(Port)->
    true = unlink(Port),
    true = port_close(Port),
    ok.

-spec call(port(),integer(),[term()]) -> term()|{error,_}.
call(Port, Command, Args)
  when is_port(Port), is_integer(Command), is_list(Args) ->
    try erlang:port_call(Port, Command, Args) of
        Term when not is_binary(Term) ->
            Term
    catch
        error:badarg ->
            {error, badarg}
    end.

-spec command(port(),integer(),[term()]) -> ok|{error,_}.
command(Port, Command, Args)
  when is_port(Port), is_integer(Command), is_list(Args) ->
    try port_command(Port, term_to_binary({Command,Args})) of
        true ->
            ok
    catch
        error:badarg ->
            {error, badarg}
    end.

-spec control(port(),integer(),[term()]) -> term()|{error,_}.
control(Port, Command, Args)
  when is_port(Port), is_integer(Command), is_list(Args) ->
    try port_control(Port, Command, term_to_binary(Args)) of
        Term when is_binary(Term) ->
            binary_to_term(Term)
    catch
        error:badarg ->
            {error, badarg}
    end.

%% == private ==

lib_dir(Application) ->
    filename:join(lib_dir(Application,priv), "lib").

lib_dir(Application, SubDir) ->
    case code:lib_dir(Application, SubDir) of
        {error, bad_name} ->
            {ok, Dir} = file:get_cwd(),
            filename:join(Dir, atom_to_list(SubDir));
        Dir ->
            Dir
    end.

set_handle({path,Term}, #handle{}=H) ->
    if is_binary(Term) -> H#handle{path = binary_to_list(Term)};
       is_list(Term)   -> H#handle{path = Term};
       true -> throw({badarg,path})
    end;
set_handle({name,Term}, #handle{}=H) ->
    if is_binary(Term) -> H#handle{name = binary_to_list(Term)};
       is_list(Term)   -> H#handle{name = Term};
       true -> throw({badarg,name})
    end;
set_handle({settings,Term}, #handle{}=H) ->
    if is_list(Term) -> H#handle{settings = Term};
       true -> throw({badarg,settings})
    end;
set_handle(_Ignore, #handle{}=H) ->
    H.

setup() ->
    #handle{path = lib_dir(undefined), name = "eroonga_drv", settings = []}.

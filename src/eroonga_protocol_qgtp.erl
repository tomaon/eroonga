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

-module(eroonga_protocol_qgtp).

-include("eroonga_internal.hrl").

%% -- public --
-export([connect/1, close/1]).
-export([call/2]).

%% -- private --
-record(handle, {
          socket :: socket(),
          timeout :: timeout(),
          quit = false :: boolean()|undefined
         }).

-type(handle() :: #handle{}).

%% == public ==

-spec connect([property()]) -> {ok,handle()}|ignore|{error,_}.
connect(Args)
  when is_list(Args) ->
    try apply(gen_tcp, connect, Args) of
        {ok, Socket} ->
            {ok, #handle{socket = Socket, timeout = lists:last(Args)}};
        {error, Reason} ->
            {error, Reason}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec close(handle()) -> ok.
close(#handle{quit=false}=H) ->
    case call(H, <<"quit">>) of
        {ok, _, Handle} ->
            close(Handle#handle{quit = true});
        {error, _, Handle} ->
            close(Handle#handle{quit = undefined})
    end;
close(#handle{quit=true}=H) ->
    case send(H, <<"ACK">>) of
        {ok, Handle} ->
            close(Handle#handle{quit = undefined})
    end;
close(#handle{socket=S}) ->
    gen_tcp:close(S).

-spec call(handle(),binary()) -> {ok,term(),handle()}|{error,_,handle()}.
call(#handle{}=H, Binary)
  when is_binary(Binary) ->
    case send(H, Binary, size(Binary)) of
        {ok, Handle} ->
            recv(Handle)
    end.

%% -- private --

recv(#handle{}=H) ->
    case recv(H, 0, 0, ?GRN_CTX_MORE, []) of
        {0, ?GRN_CONTENT_JSON, Binary, Handle} ->
            {ok, jsonx:decode(Binary), Handle};
        {0, QueryType, _, Handle} ->
            {error, {not_implemented,QueryType}, Handle};
        {Status, _, Binary, Handle} ->
            {error, {Status,Binary}, Handle}
    end.

recv(Handle, Type, Result, Bits, List)
  when ?ISSET(Bits, ?GRN_CTX_TAIL) ->
    {Result, Type, list_to_binary(lists:reverse(List)), Handle};
recv(#handle{socket=S, timeout=T}=H, _Type, _Result, Bits, List)
  when ?ISSET(Bits, ?GRN_CTX_MORE) ->
    case gen_tcp:recv(S, 24, T) of
        {ok, Header} ->
            <<
              ?GRN_COM_PROTO_GQTP,
              QueryType:8,
              _KeyLength:16,
              _Level:8,
              Flags:8,
              Status:16,
              Size:32,
              _Opaque:32,
              _Cas:64
            >> = Header,
            case gen_tcp:recv(S, Size, T) of
                {ok, Body} ->
                    recv(H, QueryType, Status, Flags, [Body|List]);
                {error, Reason} ->
                    {error, Reason, H}
            end;
        {error, Reason} ->
            {error, Reason, H}
    end.

send(#handle{}=H, Binary)
  when is_binary(Binary) ->
    send(H, Binary, size(Binary)).

send(#handle{socket=S}=H, Binary, Size) ->
    B = <<
          ?GRN_COM_PROTO_GQTP, % protocol
          ?GRN_CONTENT_NONE,   % query_type
          0,0,                 % key_length (unused)
          0,                   % level (unused)
          0,                   % flags
          0,0,                 % status
          Size:32,             % size
          0,0,0,0,             % opaque (unused)
          0,0,0,0,0,0,0,0      % cas (unused)
        >>,
    {gen_tcp:send(S,list_to_binary([B,Binary])), H}.

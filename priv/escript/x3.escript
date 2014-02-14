#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -config priv/conf/driver

run(0, H) ->
    L = [
         { "Entries", "content @ 'fast'" }
        ],
    [ io:format("select=~p~n", [eroonga:select(H,T,E)]) || {T,E} <- L ].

run(direct) ->
    io:format("run: direct~n"),
    case eroonga:new() of
        {ok, H} ->
            case eroonga:open(H,"/tmp/groonga/x3") of
                ok ->
                    run(0, H),
                    ok;
                {error, Reason} ->
                    io:format("ERROR: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason])
    end.

main(_) ->
    L = [
         direct
        ],
    [ run(A) || A <- L ].

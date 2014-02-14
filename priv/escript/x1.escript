#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/baseline/ebin -pa deps/poolboy/ebin -pa deps/jsonx/ebin -config files/client

run(0, Pid) ->
    io:format("status=~p~n", [eroonga:command(Pid, <<"status">>)]).

run(pool) ->
    io:format("run: pool~n"),
    case eroonga:start() of
        ok ->
            case eroonga:connect(N = eroonga_pool) of
                {ok, Pid} ->
                    [ run(E,Pid) || E <- [0] ],
                    ok = eroonga:close(N, Pid);
                {error, Reason} ->
                    io:format("ERROR: ~p~n", [Reason])
            end,
            _ = eroonga:stop();
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason])
    end.

main(_) ->
    L = [
         pool
        ],
    [ run(A) || A <- L ].

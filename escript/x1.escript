#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/poolboy/ebin -pa deps/jsonx/ebin -config files/client -s eroonga

run(Pid) ->
    io:format("status=~p~n", [eroonga:command(Pid, <<"status">>)]).

main(_) ->
    case eroonga:connect(N = eroonga_pool) of
        {ok, Pid} ->
            run(Pid),
            ok = eroonga:close(N, Pid);
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason])
    end.

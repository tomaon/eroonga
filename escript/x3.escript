#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/baseline/ebin -pa deps/poolboy/ebin -pa deps/jsonx/ebin -config files/driver

run(0, Pid) ->
    L = [
         {0, [<<"Entries">>, <<"content @ 'fast'">>]},
         {99, []}
        ],
    [ io:format("call(~p)=~p~n", [C,eroonga_drv:call(Pid,C,A)]) || {C,A} <- L ].

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
    end;
run(direct) ->
    io:format("run: direct~n"),
    N = <<"eroonga_drv">>,
    case baseline_drv:load([{name,N}]) of
        ok ->
            L = [
                 {path,<<"/tmp/groonga/x3">>},
                 {options, [
                            %%{s, "zyx"},
                            %%{b, <<"wvu">>},
                            %%{i, 987},
                            %%{f, 12.345},
                            %%{a, false},
                            %%x
                            %%{x, make_ref()}
                            %%{x, self()}
                            %%{x, lists:min(erlang:ports())}
                            {encoding, utf8}
                           ]}
                ],
            case eroonga_drv:start_link(N, [], L) of
                {ok, Pid} ->
                    unlink(Pid),
                    [ run(E,Pid) || E <- [0] ],
                    eroonga_drv:stop(Pid);
                {error, Reason} ->
                    io:format("ERROR: ~p (port)~n", [Reason])
            end,
            baseline_drv:unload(N);
        {error, Reason} ->
            io:format("ERROR: ~p (driver)~n", [Reason])
    end.

main(_) ->
    L = [
         pool,
         direct
        ],
    [ run(A) || A <- L ].

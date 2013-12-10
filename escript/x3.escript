#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/poolboy/ebin -pa deps/jsonx/ebin -config files/driver

run(0, Pid) ->
    L = [
         {0, [<<"Entries">>, <<"content @ 'fast'">>]},
         {99, []}
        ],
    [ io:format("call(~p)=~p~n", [C,eroonga_port:call(Pid,C,A)]) || {C,A} <- L ].

run(pool) ->
    io:format("run: pool~n"),
    _ = eroonga:start(),
    case eroonga:connect(N = eroonga_pool) of
        {ok, Pid} ->
            run(0, Pid),
            ok = eroonga:close(N, Pid);
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason])
    end;
run(direct) ->
    io:format("run: direct~n"),
    case eroonga_driver:load([{name,"eroonga_drv"}]) of
        {ok, T} ->
            L = [
                 {driver,T},
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
            case eroonga_port:start_link(L) of
                {ok, Pid} ->
                    run(0, Pid),
                    timer:sleep(2000),
                    eroonga_port:stop(Pid);
                {error, Reason} ->
                    io:format("ERROR: ~p (port)~n", [Reason])
            end,
            eroonga_driver:unload(T);
        {error, Reason} ->
            io:format("ERROR: ~p (driver)~n", [Reason])
    end.

main(_) ->
    L = [
%        pool,
         direct
        ],
    [ run(A) || A <- L ].

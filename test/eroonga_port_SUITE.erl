%% =============================================================================
%% =============================================================================

-module(eroonga_port_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("eroonga_internal.hrl").

all() -> [
          {group, test_normal}
         ].

groups() ->
    [
     {test_normal, [], [
                        call_test
                       ]}
    ].

init_per_suite(Config) ->
    _ = application:load(eroonga),
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(_Group, Config) ->
    L = [ fun start/1 ],
    lists:foldl(fun(E,A) -> E(A) end, Config, L).

end_per_group(_Group, Config) ->
    L = [ fun stop/1 ],
    lists:foldl(fun(E,A) -> E(A) end, Config, L).

%% == group:  ==

call_test(_Config) ->
    {skip, not_supported}.

%% == ==

start(Config) ->
    case eroonga:start() of
        ok ->
            Config;
        {error, Reason} ->
            ct:fail(Reason)
    end.

stop(Config) ->
    case eroonga:stop() of
        ok ->
            Config;
        {error, Reason} ->
            ct:fail(Reason)
    end.

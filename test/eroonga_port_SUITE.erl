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
    {ok, Dir} = file:get_cwd(),
    L = [
         {path, filename:join([Dir,"..","..","priv","lib"])}
        ],
    case eroonga_driver:load(L) of
        {ok, T} ->
            [{driver,T}|Config];
        {error, Reason} ->
            ct:fail(Reason)
    end.

end_per_suite(Config) ->
    case erooga_driver:unload(?config(driver,Config)) of
        ok ->
            proplists:delete(driver, Config);
        {error, Reason} ->
            ct:fail(Reason)
    end.

init_per_group(_Group, Config) ->
    L = [
         {driver, ?config(driver,Config)},
         {path, <<"/tmp/groonga/x3">>},
         {options, [
                    {encoding, utf8}
                   ]}
        ],
    case eroonga_port:start_link(L) of
        {ok, Pid} ->
            [{pid,Pid}|Config];
        {error, Reason} ->
            ct:fail(Reason) % badarg, WHY??
    end.

end_per_group(_Group, Config) ->
    case erooga_port:stop(?config(pid,Config)) of
        ok ->
            proplists:delete(pid, Config);
        {error, Reason} ->
            ct:fail(Reason)
    end.

%% == group:  ==

call_test(_Config) ->
    {skip, not_supported}.

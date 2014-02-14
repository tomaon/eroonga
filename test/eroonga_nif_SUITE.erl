%% =============================================================================
%% =============================================================================

-module(eroonga_nif_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).
-export([init_per_group/2, end_per_group/2]).

%% -- public --
-export([
         new_test/1
        ]).

%% == callback: ct ==

all() -> [
          new_test
         ].

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

%% == public ==

new_test(_Config) ->
    {ok, _} = test(new, []).

%% == private ==

test(Function, Args) ->
    baseline_ct:test(eroonga_nif, Function, Args).

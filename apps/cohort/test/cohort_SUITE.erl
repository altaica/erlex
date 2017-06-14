%%% @doc Test suite for failover between 2 nodes.
%%% @copyright 2017 Phil Dempster

-module(cohort_SUITE).
-compile([nowarn_unused_function]).
-export([all/0]).
-include_lib("common_test/include/ct.hrl").

all() -> [stop_primary_node,
          takeover_by_secondary,
          terminate_secondary].

% Start both nodes, save pids
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    % terminate primary.
    ok.

stop_primary_node(_Config) ->
    ok.

takeover_by_secondary(_Config) ->
    ok.

terminate_secondary(_Config) ->
    ok.


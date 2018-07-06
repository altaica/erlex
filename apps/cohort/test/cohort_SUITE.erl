%%% @doc Test suite for failover between 2 nodes.
%%% @copyright 2017 Phil Dempster

-module(cohort_SUITE).
-compile([nowarn_unused_function]).
-export([all/0]).
-include_lib("common_test/include/ct.hrl").

all() -> [].%stop_primary_node,
          %takeover_by_secondary,
          %terminate_secondary].

% Start application on both nodes.
init_per_suite(Config) ->
    start_app(magnumopus),
    %B = start_app(obsequilis),
    Config.%[A, B | Config].

end_per_suite(_Config) ->
    % terminate primary.
    ok.

stop_primary_node(_Config) ->
    ok.

takeover_by_secondary(_Config) ->
    %rpc:call(Node
    ok.

terminate_secondary(_Config) ->
    ok.

start_app(NodeId) ->
    NodeName = atom_to_list(NodeId),
    ErlFlags =
        "-config apps/cohort/config/sys.config" ++
        "-args_file apps/cohort/config/" ++ NodeName ++ ".args",
    NodeArgs = [
        {kill_if_fail, true},
        {monitor_master, true},
        {init_timeout, 3000},
        {startup_timeout, 3000},
        {startup_functions, [{cohort, start, []}]},
        {erl_flags, ErlFlags}],
    {ok, Node} = ct_slave:start(NodeId, NodeArgs),
    pong = net_adm:ping(Node).
    %ct:print("\e[32m Node ~p [OK] \e[0m", [Node]).

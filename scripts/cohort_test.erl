%#!/usr/bin/env escript
%%! -sname cohort_test@localhost -setcookie cohort
% The above sets magical "emulator arguments", as described in the escript docs.
-module(cohort_test).
-export([main/1]).

-define(APP_NAME, cohort).
-define(TIMEOUT_MS, 5000).

main([]) ->
    %logger:set_primary_config(level, debug),
    Nodes = ['magnumopus@localhost', 'obsequilis@localhost'],
    takeover(Nodes),
    failover(Nodes),
    Node = get_active_node(Nodes),
    logger:notice("Active node is now ~p", [Node]),
    wait().

takeover(Nodes) ->
    Node = get_standby_node(Nodes),
    logger:notice("Standby node ~p performing takeover", [Node]),
    ok = rpc:call(Node, application, takeover, [?APP_NAME, temporary]),
    wait().

failover(Nodes) ->
    Node = get_active_node(Nodes),
    logger:notice("Failing over active node ~p", [Node]),
    ok = rpc:call(Node, init, reboot, []),
    wait().

get_active_node([A, B]) ->
    case {check_node(A), check_node(B)} of
        {true, false} -> A;
        {false, true} -> B
    end.

get_standby_node([A, B]) ->
    case {check_node(A), check_node(B)} of
        {true, false} -> B;
        {false, true} -> A
    end.

check_node(Node) ->
    case rpc:call(Node, application, which_applications, []) of
        {badrpc, nodedown} ->
            logger:debug("Node ~p is down", [Node]),
            false;
        Apps ->
            is_running(lists:keyfind(?APP_NAME, 1, Apps), Node)
    end.

is_running(false, Node) ->
    logger:debug("~p is NOT running on node ~p", [?APP_NAME, Node]),
    false;
is_running({?APP_NAME, _, _}, Node) ->
    logger:debug("~p is running on node ~p", [?APP_NAME, Node]),
    true.

wait() ->
    timer:sleep(?TIMEOUT_MS).

#!/usr/bin/env escript
%%! -sname cohort_test@localhost -setcookie cohort
% The above sets magical "emulator arguments", as described in the escript docs.

-define(APP_NAME, cohort).
-define(TIMEOUT_MS, 5000).

main([]) ->
    Nodes = ['magnumopus@localhost', 'obsequilis@localhost'],

    Node = get_standby_node(Nodes),
    takeover(Node),

    Node = get_active_node(Nodes),
    failover(Node),

    Node = get_standby_node(Nodes).

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

takeover(Node) ->
    io:format("~n=== Node ~p performing takeover ===~n", [Node]),
    ok = rpc:call(Node, application, takeover, [?APP_NAME, temporary]),
    wait().

failover(Node) ->
    io:format("~n=== Failing over node ~p ===~n", [Node]),
    ok = rpc:call(Node, init, reboot, []),
    wait().

check_node(Node) ->
    case rpc:call(Node, application, which_applications, []) of
        {badrpc, nodedown} ->
            io:format("Node ~p is down~n", [Node]),
            false;
        Apps ->
            is_running(lists:keyfind(?APP_NAME, 1, Apps), Node)
    end.

is_running(false, Node) ->
    io:format("~p is NOT running on node ~p~n", [?APP_NAME, Node]),
    false;
is_running({?APP_NAME, _, _}, Node) ->
    io:format("~p is running on node ~p~n", [?APP_NAME, Node]),
    true.

wait() ->
    timer:sleep(?TIMEOUT_MS).

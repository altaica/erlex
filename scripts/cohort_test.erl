#!/usr/bin/env escript
%%! -sname cohort_test@localhost -setcookie cohort
% The above sets magical "emulator arguments", as described in the escript docs.

-define(APP_NAME, cohort).
-define(TIMEOUT_MS, 5000).

main([]) ->
    Nodes = ['magnumopus@localhost', 'obsequilis@localhost'],
    
    start_nodes(Nodes),
    
    N1 = get_active_node(Nodes),
    failover(N1),

    N2 = get_standby_node(Nodes),
    takeover(N2),

    takeover(get_standby_node(Nodes)),
    failover(get_active_node(Nodes)),

    get_active_node(Nodes),

    kill_nodes(Nodes).


start_nodes(Nodes) ->
    io:format("=== Starting nodes ===~n", []),
    [start_node(Node) || Node <- Nodes],
    timer:sleep(1000).

start_node(Node) ->
    [] = os:cmd("erl -sname " ++ atom_to_list(Node) ++ " -args_file apps/cohort/config/vm.args &").

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
    ok = rpc:call(Node, init, restart, []),
    wait().

kill_nodes(Nodes) ->
    io:format("~n=== Terminating nodes ===~n", []),
    [kill_node(Node) || Node <- Nodes].

kill_node(Node) ->
    ok = rpc:call(Node, init, stop, []),
    io:format("~p stopped~n", [Node]).

check_node(Node) ->
    Apps = rpc:call(Node, application, which_applications, []),
    is_running(lists:keyfind(?APP_NAME, 1, Apps), Node).

is_running(false, Node) ->
    io:format("~p is NOT running on node ~p~n", [?APP_NAME, Node]),
    false;
is_running({?APP_NAME, _, _}, Node) ->
    io:format("~p is running on node ~p~n", [?APP_NAME, Node]),
    true.

wait() ->
    timer:sleep(?TIMEOUT_MS).


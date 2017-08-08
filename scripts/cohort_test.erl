#!/usr/bin/env escript
%%! -sname cohort_test@localhost -setcookie cohort
% The above sets magical "emulator arguments", as described in the escript docs.

main([]) ->
    timer:sleep(1000),
    takeover('obsequilis@localhost'),
    timer:sleep(1000),
    failover('obsequilis@localhost').

takeover(Node) ->
    io:format("Node ~p performing takeover~n", [Node]),
    rpc:call(Node, application, takeover, [cohort, temporary]).

failover(Node) ->
    io:format("Failing over node ~p~n", [Node]),
    rpc:call(Node, init, restart, []).

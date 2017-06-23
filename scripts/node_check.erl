#!/usr/bin/env escript
%%! -sname node_check@localhost -setcookie cohort
% The above sets magical "emulator arguments", as described in the escript docs.

main([NodeName]) ->
    Node = list_to_atom(NodeName ++ "@localhost"),
    handle_ping_response(net_adm:ping(Node)).

handle_ping_response(pong) -> ok;
handle_ping_response(pang) -> halt(1).

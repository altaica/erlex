%%% @doc Coverage tests for traffic light example.
%%% @copyright 2017 Phil Dempster

-module(traffic_light01_test).
-include_lib("eunit/include/eunit.hrl").

transition_from_green_test() ->
    % Duration specified by Highways Agency.
    ?assertMatch({amber, 3000}, traffic_light01:transition(green)).

transition_from_amber_test() ->
    ?assertMatch({red, _Duration}, traffic_light01:transition(amber)).

transition_from_red_test() ->
    % Duration specified by Highways Agency.
    ?assertMatch({redamber, 2000}, traffic_light01:transition(red)).

transition_from_redamber_test() ->
    ?assertMatch({green, _Duration}, traffic_light01:transition(redamber)).

loop_test() ->
    ?assertError(function_clause, traffic_light01:loop({dummy, 0})).

start_test() ->
    Pid = traffic_light01:start(),
    exit(Pid, normal).


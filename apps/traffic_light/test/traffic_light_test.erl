%%% @doc Coverage tests for traffic light example.
%%% @copyright 2017 Phil Dempster

-module(traffic_light_test).
-include_lib("eunit/include/eunit.hrl").

transition_from_green_test() ->
    % Duration specified by Highways Agency.
    ?assertMatch({amber, 3000}, traffic_light:transition(green)).

transition_from_amber_test() ->
    ?assertMatch({red, _Duration}, traffic_light:transition(amber)).

transition_from_red_test() ->
    % Duration specified by Highways Agency.
    ?assertMatch({redamber, 2000}, traffic_light:transition(red)).

transition_from_redamber_test() ->
    ?assertMatch({green, _Duration}, traffic_light:transition(redamber)).

loop_test() ->
    ?assertError(function_clause, traffic_light:loop({dummy, 0})).

init_test() ->
    Pid = spawn(fun traffic_light:init/0),
    exit(Pid, normal).


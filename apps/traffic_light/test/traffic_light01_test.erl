%%% @doc Coverage tests for traffic light example.
%%% @copyright 2017 Phil Dempster

-module(traffic_light01_test).
-include_lib("eunit/include/eunit.hrl").

next_state_from_green_test() ->
    ?assertMatch(amber, traffic_light01:next_state(green)).

next_state_from_amber_test() ->
    ?assertMatch(red, traffic_light01:next_state(amber)).

next_state_from_red_test() ->
    ?assertMatch(redamber, traffic_light01:next_state(red)).

next_state_from_redamber_test() ->
    ?assertMatch(green, traffic_light01:next_state(redamber)).

loop_test() ->
    Timings = #{
        green       => 0,
        amber       => 0,
        red         => 0,
        redamber    => -1   % Force exception
    },
    ?assertError(timeout_value, traffic_light01:loop(green, Timings)).

start_test() ->
    Pid = traffic_light01:start(),
    exit(Pid, normal).


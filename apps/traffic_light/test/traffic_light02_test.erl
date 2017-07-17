%%% @doc Coverage tests for traffic light example.
%%% @copyright 2017 Phil Dempster

-module(traffic_light02_test).
-include_lib("eunit/include/eunit.hrl").
-define(TIMINGS, #{
    green       => 20000,
    amber       => 3000,
    red         => 30000,
    redamber    => 2000
}).

transition_from_green_test() ->
    % Duration specified by Highways Agency.
    ?assertMatch({next_state, amber, _Timings, {state_timeout, 3000, transition}},
        traffic_light02:handle_event(state_timeout, transition, green, ?TIMINGS)).

transition_from_amber_test() ->
    ?assertMatch({next_state, red, _Timings, {state_timeout, _Period, transition}},
        traffic_light02:handle_event(state_timeout, transition, amber, ?TIMINGS)).

transition_from_red_test() ->
    % Duration specified by Highways Agency.
    ?assertMatch({next_state, redamber, _Timings, {state_timeout, 2000, transition}},
        traffic_light02:handle_event(state_timeout, transition, red, ?TIMINGS)).

transition_from_redamber_test() ->
    ?assertMatch({next_state, green, _Timings, {state_timeout, _Period, transition}},
        traffic_light02:handle_event(state_timeout, transition, redamber, ?TIMINGS)).

start_test() ->
    {ok, Pid} = traffic_light02:start(),
    gen_statem:stop(Pid).

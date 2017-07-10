%%% @doc Coverage tests for traffic light example.
%%% @copyright 2017 Phil Dempster

-module(traffic_light02_test).
-include_lib("eunit/include/eunit.hrl").

transition_from_green_test() ->
    % Duration specified by Highways Agency.
    ?assertMatch({next_state, amber, {100, 100}, {state_timeout, 3000, transition}},
        traffic_light02:handle_event(state_timeout, transition, green, {100, 100})).

transition_from_amber_test() ->
    ?assertMatch({next_state, red, {100, 100}, {state_timeout, _Period, transition}},
        traffic_light02:handle_event(state_timeout, transition, amber, {100, 100})).

transition_from_red_test() ->
    % Duration specified by Highways Agency.
    ?assertMatch({next_state, redamber, {100, 100}, {state_timeout, 2000, transition}},
        traffic_light02:handle_event(state_timeout, transition, red, {100, 100})).

transition_from_redamber_test() ->
    ?assertMatch({next_state, green, {100, 100}, {state_timeout, _Period, transition}},
        traffic_light02:handle_event(state_timeout, transition, redamber, {100, 100})).

start_test() ->
    {ok, Pid} = traffic_light02:start(),
    gen_statem:stop(Pid).

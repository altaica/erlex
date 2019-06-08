%%% @doc Traffic light state machine.
%%%      References:
%%%      [1] Highways Agency document TR 2500 Issue A November 2005,
%%%          "Specification for Traffic Signal Controller".
%%%
%%% @copyright 2017-2018 Phil Dempster

-module(traffic_light01).
-export([start/0]).

-define(AMBER_PERIOD, 3000).
-define(REDAMBER_PERIOD, 2000).

start() ->
    InitTimings = #{
        green       => 20000,
        amber       => ?AMBER_PERIOD,
        red         => 30000,
        redamber    => ?REDAMBER_PERIOD
    },
    spawn(fun() -> loop(amber, InitTimings) end).

loop(Phase, Timings) ->
    error_logger:info_msg("Phase: ~s~n", [Phase]),
    receive
    after maps:get(Phase, Timings) ->
        loop(next_state(Phase), Timings)
    end.

next_state(green)       -> amber;
next_state(amber)       -> red;
next_state(red)         -> redamber;
next_state(redamber)    -> green.

%%% @doc Traffic light state machine.
%%%      See Highways Agency document TR 2500 Issue A November 2005,
%%%      "Specification for Traffic Signal Controller".
%%% @copyright 2017 Phil Dempster

-module(traffic_light01).
-export([start/0]).

start() ->
    InitTimings = #{
        green       => 20000,
        amber       => 3000,    % Duration specified by Highways Agency.
        red         => 30000,
        redamber    => 2000     % Duration specified by Highways Agency.
    },
    spawn(fun() -> loop(red, InitTimings) end).

loop(State, Timings) ->
    error_logger:info_msg("State: ~s~n", [State]),
    receive
    after maps:get(State, Timings) ->
        loop(next_state(State), Timings)
    end.

next_state(green)       -> amber;
next_state(amber)       -> red;
next_state(red)         -> redamber;
next_state(redamber)    -> green.

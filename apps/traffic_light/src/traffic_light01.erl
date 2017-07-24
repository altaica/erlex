%%% @doc Traffic light state machine.
%%%      See Highways Agency document TR 2500 Issue A November 2005,
%%%      "Specification for Traffic Signal Controller".
%%% @copyright 2017 Phil Dempster

-module(traffic_light01).
-export([start/0]).

start() ->
    spawn(fun() -> loop(transition(amber)) end).

loop({StateName, Period}) ->
    error_logger:info_msg("State: ~s~n", [StateName]),
    receive
    after Period ->
        loop(transition(StateName))
    end.

transition(green)       -> {amber,       3000}; % Specified by Highways Agency.
transition(amber)       -> {red,        30000};
transition(red)         -> {redamber,    2000}; % Specified by Highways Agency.
transition(redamber)    -> {green,      20000}.


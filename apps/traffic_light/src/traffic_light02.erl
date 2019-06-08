%%% @doc Traffic light state machine.
%%%      See Highways Agency document TR 2500 Issue A November 2005,
%%%      "Specification for Traffic Signal Controller".
%%% @copyright 2017 Phil Dempster

-module(traffic_light02).
-behaviour(gen_statem).

%% API functions.
-export([start/0]).

%% Behaviour callbacks.
-export([init/1,
         callback_mode/0,
         handle_event/4]).

start() ->
    gen_statem:start(?MODULE, [20000, 30000], []).

init([Green, Red]) ->
    InitTimings = #{
        green       => Green,
        amber       => 3000,    % Duration specified by Highways Agency.
        red         => Red,
        redamber    => 2000     % Duration specified by Highways Agency.
    },
    {ok, amber, InitTimings, {next_event, state_timeout, transition}}.

callback_mode() -> handle_event_function.

%handle_event(call, {set_timings, NewTimings}, _StateName, Timings) ->
%    {keep_state, maps:merge(Timings, NewTimings), {reply, self(), ok}};
handle_event(state_timeout, transition, State, Timings) ->
    transition(next_state(State), Timings).

transition(NextState, Timings) ->
    logger:info("Phase: ~s~n", [NextState]),
    {next_state, NextState, Timings,
        {state_timeout, maps:get(NextState, Timings), transition}}.

next_state(green)       -> amber;
next_state(amber)       -> red;
next_state(red)         -> redamber;
next_state(redamber)    -> green.

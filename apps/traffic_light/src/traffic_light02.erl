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
    gen_statem:start(?MODULE, [{30000, 20000}], []).

init([InitTimings]) ->
    {ok, amber, InitTimings, {next_event, state_timeout, transition}}.

callback_mode() ->
    handle_event_function.

handle_event(state_timeout, transition, StateName, _Data) ->
    {NextState, Period} = transition(StateName),
    error_logger:info_msg("State: ~s~n", [NextState]),
    {next_state, NextState, _Data, {state_timeout, Period, transition}}.

transition(green)       -> {amber,       3000}; % Specified by Highways Agency.
transition(amber)       -> {red,        30000};
transition(red)         -> {redamber,    2000}; % Specified by Highways Agency.
transition(redamber)    -> {green,      20000}.


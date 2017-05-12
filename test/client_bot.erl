%%% @doc OTP gen_server implementation of client bot.
%%%
%%% When the server informs the bot that another caller has joined,
%%% the bot will send a greeting {hello, <Pid>} to the new caller.
%%%
%%% @end
%%% @copyright 2017 Phil Dempster

-module(client_bot).
-behaviour(gen_server).
-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

% TODO - use record for state variable


%%% API

%% @doc Start a call with the specified server.
%% @returns Tuple containing the Pid of the client bot and a list of the current conference participants.
-spec start(module()) -> {pid(), list(pid())}.
start(Server) ->
    {ok, Pid} = gen_server:start(?MODULE, [Server], []),
    {_Server, _Expected, Participants} = gen_server:call(Pid, get_state),
    {Pid, Participants}.

%% @doc Terminate our call.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).


%%% Implementation

init([Server]) ->
    {joined, Expected} = Server:join(),
    {ok, {Server, Expected, Expected}}.

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {stop, undefined, State}.

handle_info({connected, Pid}, {Server, _Expected, _Participants} = State) ->
    % Send greeting to new caller.
    ok = Server:send({hello, Pid}),
    {noreply, State};
handle_info({disconnected, Pid}, {Server, Expected, Participants}) ->
    % Pid should not be in expected list.
    {noreply, {Server, Expected, lists:delete(Pid, Participants)}};
handle_info({message, From, {hello, Pid}}, {Server, Expected, Participants})
        when From =/= Pid, Pid =:= self() ->
    % Somebody else has greeted us. Remove from the expected list.
    {noreply, {Server, lists:delete(From, Expected), Participants}};
handle_info({message, _From, {hello, _Pid}}, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, {_Server, [], []}) ->
    % Verify that we have been greeted by all the original participants.
    ok.

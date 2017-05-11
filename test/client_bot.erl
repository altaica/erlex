%%% @doc OTP gen_server implementation of client bot.
%%% @copyright 2017 Phil Dempster

-module(client_bot).
-behaviour(gen_server).
-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


%%% API

-spec start(module()) -> {pid(), list(pid())}.
start(Server) ->
    {ok, Pid} = gen_server:start(?MODULE, [Server], []),
    {_Server, _Expected, InitState} = gen_server:call(Pid, get_state),
    {Pid, InitState}.

-spec stop(pid()) -> ok.
stop(Pid) ->
    {_Server, [], _InitState} = gen_server:call(Pid, get_state),
    gen_server:stop(Pid).


%%% Implementation

init([Server]) ->
    {joined, Expected} = Server:join(),
    {ok, {Server, Expected, Expected}}.

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {stop, undefined, State}.

handle_info({connected, Pid}, {Server, _Expected, _InitState} = State) ->
    % Send greeting to new caller.
    ok = Server:send({hello, Pid}),
    {noreply, State};
handle_info({disconnected, _Pid}, State) ->
    % Pid should not be in expected list.
    {noreply, State};
handle_info({message, From, {hello, Pid}}, {Server, Expected, InitState})
        when From =/= Pid, Pid =:= self() ->
    % Somebody else has greeted us. Remove from the expected list.
    {noreply, {Server, lists:delete(From, Expected), InitState}};
handle_info({message, _From, {hello, _Pid}}, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

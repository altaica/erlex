-module(gen_call_srv).
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%send(Message) ->
%    gen_server:call

init([State]) ->
    {ok, State}.

handle_call(send, _From, State) ->
    {stop, normal, State}.

handle_cast({joined, Caller}, State) ->
    error_logger:info_msg("~p: ~p has joined~n", [State, Caller]),
    {noreply, State};
handle_cast({data, _Caller, _Message}, State) ->
    %TBD
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

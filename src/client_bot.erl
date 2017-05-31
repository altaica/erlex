%%% @doc OTP gen_server implementation of client bot.
%%%
%%% When the server informs the bot that another caller has joined,
%%% the bot will send a greeting {hello, Pid} to the new caller.
%%%
%%% The bot expects that calls will be torn down in the same order
%%% as they are set up. This allows for disconnect testing.
%%%
%%% @end
%%% @copyright 2017 Phil Dempster

-module(client_bot).
-behaviour(gen_server).
-export([start/1, stop/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-type calls():: [pid()].

-record(state, {
    server::        module(),
    expected::      calls(),
    init_calls::    calls()
}).

%%% API

%% @doc Start a call with the specified server.
%% @returns Tuple containing the Pid of the client bot and
%%          a list of the current conference participants.
-spec start(module()) -> {pid(), calls()}.
start(Server) ->
    {ok, Pid} = gen_server:start(?MODULE, [Server], []),
    State = gen_server:call(Pid, get_state),
    {Pid, State#state.init_calls}.

%% @doc Terminate our call.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).


%%% Implementation

init([Server]) ->
    {joined, Calls} = apply(Server, join, []),
    {ok, #state{server      = Server,
                expected    = Calls,
                init_calls  = lists:reverse(Calls)}}.

handle_call(get_state, _From, #state{init_calls = Calls} = State) ->
    {reply, State#state{init_calls = lists:reverse(Calls)}, State}.

handle_cast(_Msg, State) ->
    {stop, normal, State}.

handle_info({connected, Pid}, #state{server = Server} = State) ->
    % Send greeting to new caller.
    ok = apply(Server, send, [{hello, Pid}]),
    {noreply, State};
handle_info({disconnected, Pid}, #state{init_calls = [Pid | Calls]} = State) ->
    % NB Pid should not be in expected list.
    {noreply, State#state{init_calls = Calls}};
handle_info({message, From, {hello, Pid}}, #state{expected = Expected} = State)
        when From =/= Pid, Pid =:= self() ->
    % Somebody else has greeted us. Remove them from the expected list.
    {noreply, State#state{expected = lists:delete(From, Expected)}};
handle_info({message, _From, {hello, _Pid}}, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{expected = [], init_calls = []}) ->
    % Verify that we have been greeted by all the original participants
    % and that they have all left before us.
    ok.

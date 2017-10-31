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

-module(chatbot).
-behaviour(gen_server).
-export([start/2, stop/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-type calls():: [pid()].

-record(state, {
    server::        module(),
    expected::      calls(),
    init_calls::    calls(),
    bot_name::      atom()
}).

%%% API

%% @doc Start a call with the specified server.
%% @returns Tuple containing the Pid of the client bot and
%%          a list of the current conference participants.
-spec start(module(), atom()) -> {pid(), calls()}.
start(Server, Name) ->
    {ok, Pid} = gen_server:start({local, Name}, ?MODULE, [Server, Name], []),
    Callers = gen_server:call(Pid, get_callers),
    {Pid, Callers}.

%% @doc Terminate our call.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).


%%% Implementation

init([Server, Name]) ->
    {joined, Calls} = apply(Server, join, []),
    {ok, #state{server      = Server,
                expected    = Calls,
                init_calls  = lists:reverse(Calls),
                bot_name    = Name}}.

handle_call(get_callers, _From, State) ->
    Callers = lookup(State#state.init_calls),
    {reply, lists:reverse(Callers), State}.

handle_cast(_Msg, State) ->
    {stop, normal, State}.

handle_info({connected, Pid}, #state{server = Server} = State) ->
    % Send greeting to new caller.
    ok = apply(Server, send, [{hello, pid_to_name(Pid)}]),
    {noreply, State};
handle_info({disconnected, Pid}, #state{init_calls = [Pid | Calls]} = State) ->
    % NB Pid should not be in expected list.
    {noreply, State#state{init_calls = Calls}};
handle_info({message, From, {hello, Name}},
            #state{expected = Expected, bot_name = BotName} = State)
        when From =/= self(), Name =:= BotName ->
    % Somebody else has greeted us. Remove them from the expected list.
    error_logger:info_msg("~w: ~w greeted us~n", [BotName, From]),
    {noreply, State#state{expected = lists:delete(From, Expected)}};
handle_info({message, _From, {hello, _Name}}, State) ->
    {noreply, State}.

terminate(_Reason, #state{expected = [], init_calls = []}) ->
    % Verify that we have been greeted by all the original participants
    % and that they have all left before us.
    ok.

lookup(Calls) ->
    [pid_to_name(Pid) || Pid <- Calls].

pid_to_name(Pid) ->
    {registered_name, Name} = erlang:process_info(Pid, registered_name),
    Name.

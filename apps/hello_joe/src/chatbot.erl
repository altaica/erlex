%%%---------------------------------------------------------------------
%%% @copyright 2017 Phil Dempster
%%%
%%% @doc OTP gen_server implementation of client bot.
%%%
%%% When the server informs the bot that another caller has joined,
%%% the bot will send a greeting <code>{hello, Pid}</code> to the new
%%% caller.
%%%
%%% The bot expects that calls will be torn down in the same order
%%% as they are set up. This allows for disconnect testing.
%%%
%%% @end
%%%---------------------------------------------------------------------

-module(chatbot).
-behaviour(gen_server).

-export([start/2, stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).


-type calls() :: [pid()].

-record(state, {
    server :: module(),
    expected :: calls(),
    init_calls :: calls(),
    bot_name :: atom()
}).

-type state() :: #state{}.


%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

%% @doc Start a call with the specified server,
%%      returning the Pid of the client bot and a list of the current
%%      conference participants.
%% @end

-spec start(Module, Name) -> T when
    Module :: module(),
    Name :: atom(),
    T :: {ClientBot, Conference},
    ClientBot :: pid(),
    Conference :: calls().

start(Server, Name) ->
    {ok, Pid} = gen_server:start({local, Name}, ?MODULE, [{Server, Name}], []),
    Callers = gen_server:call(Pid, get_callers),
    {Pid, Callers}.


%% @doc Terminate our call.

-spec stop(Pid) -> ok when
    Pid :: pid().

stop(Pid) ->
    gen_server:stop(Pid).


%%%---------------------------------------------------------------------
%%% OTP callbacks
%%%---------------------------------------------------------------------

%% @doc Join the conference server.

-spec init(Args) -> {ok, State} when
    Args :: [{Server, Name}],
    Server :: module(),
    Name :: atom(),
    State :: state().

init([{Server, Name}]) ->
    {joined, Calls} = apply(Server, join, []),
    {ok, #state{server      = Server,
                expected    = Calls,
                init_calls  = lists:reverse(Calls),
                bot_name    = Name}}.


%% @doc Get the list of current callers.

-spec handle_call(Message, From, State) -> {reply, Callers, State} when
    Message :: get_callers,
    From :: {pid(), reference()},
    State :: state(),
    Callers :: calls().

handle_call(get_callers, _From, State) ->
    Callers = lookup(State#state.init_calls),
    {reply, lists:reverse(Callers), State}.


%% @private OTP asynchronous message callback - unused.

handle_cast(_Msg, State) ->
    {stop, normal, State}.


%% @doc Handle incoming connections, disconnections and greetings.
%% <ul>
%%      <li><code>connected</code>:     New caller - send greeting to them</li>
%%      <li><code>disconnected</code>:  This pid should not be in the expected list</li>
%%      <li><code>message</code>:       Somebody else greeted us. Remove them from the expected list</li>
%%  </ul>
%% @end

-spec handle_info(Message, State) -> {noreply, State} when
    Message :: {connected | disconnected, Pid} | {message, From, {hello, Name}},
    Pid :: pid(),
    From :: pid(),
    Name :: atom(),
    State :: state().

handle_info({connected, Pid}, #state{server = Server} = State) ->
    ok = apply(Server, send, [{hello, pid_to_name(Pid)}]),
    {noreply, State};
handle_info({disconnected, Pid}, #state{init_calls = [Pid | Calls]} = State) ->
    {noreply, State#state{init_calls = Calls}};
handle_info({message, From, {hello, Name}}, State)
        when From =/= self(), Name =:= State#state.bot_name ->
    error_logger:info_msg("~w: ~w greeted us~n", [State#state.bot_name, From]),
    {noreply, State#state{expected = lists:delete(From, State#state.expected)}};
handle_info({message, _From, {hello, _Name}}, State) ->
    {noreply, State}.


%% @doc Verify that we have been greeted by all the original participants
%%      and that they have all left before us.
%% @end

terminate(_Reason, #state{expected = [], init_calls = []}) ->
    ok.


%%%---------------------------------------------------------------------
%%%
%%%---------------------------------------------------------------------

lookup(Calls) ->
    [pid_to_name(Pid) || Pid <- Calls].

pid_to_name(Pid) ->
    {registered_name, Name} = erlang:process_info(Pid, registered_name),
    Name.

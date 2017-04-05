-module(conf_srv07).
-export([start/0, stop/0, join/1, send/1]).

start() ->
    Pid = spawn(fun() -> loop([]) end),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

join(Caller) ->
    ?MODULE ! {self(), {join, Caller}},
    receive
        {?MODULE, {joined, Callers}} -> {ok, Callers}
        after 5000 -> {error, timeout}
    end.

send(Message) ->
    ?MODULE ! {self(), {send, Message}}.

loop(Conference) ->
    receive
        {From, {join, Caller}} ->
            From ! {?MODULE, {joined, callers(Conference)}},
            announce(Caller, Conference),
            loop([{From, Caller} | Conference]);
        {_From, {send, Message}} ->
            broadcast(Message, Conference),
            loop(Conference);
        stop ->
            ok
    end.

callers(Conference) ->
    [Caller || {_From, Caller} <- Conference].

announce(Caller, Conference) ->
    [To ! {?MODULE, {joined, Id, Caller}} || {To, Id} <- Conference].

broadcast(Message, Conference) ->
    [To ! {?MODULE, {data, Message}} || {To, _Id} <- Conference].

-module(common).
-export([join/2]).
-include_lib("common_test/include/ct.hrl").

join(Server, Config) ->
    {_Prev, Conference} = get_saved_config(?config(saved_config, Config)),
    F = fun() ->
        {joined, Conference} = Server:join(),
        loop(Server, [self() | Conference])
    end,
    Pid = spawn(F),
    {save_config, [Pid | Conference]}.

get_saved_config(undefined) -> {undefined, []};
get_saved_config(SavedConfig) -> SavedConfig.

loop(Server, Conference) ->
    receive
        {connected, Pid} ->
            ok = Server:send({hello, Pid}),
            loop(Server, [Pid | Conference]);
        {diconnected, Pid} ->
            loop(Server, lists:delete(Pid, Conference));
        {message, From, {hello, Pid}} when From =/= Pid, Pid =:= self() ->
            % Somebody has greeted us.
            ok;
        _Msg ->
            loop(Server, Conference)
    end.

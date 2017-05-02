-module(conf_srv_SUITE).
-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, Server} || Server <- instances()].

groups() ->
    [{Server, testcases()} || Server <- instances()].

init_per_group(Server, Config) ->
    {ok, _Pid} = Server:start(),
    [{server, Server} | Config].

end_per_group(Server, _Config) ->
    ok = Server:stop().


instances() ->
    [conf_srv01, conf_srv02, conf_srv03].

testcases() ->
    [joe_joins, mike_joins, robert_joins, elephant, all_leave].

joe_joins(Config) ->    join(Config).
mike_joins(Config) ->   join(Config).
robert_joins(Config) -> join(Config).
elephant(Config) ->     join(Config).
all_leave(Config) ->
    {elephant, Conference} = get_saved_config(?config(saved_config, Config)),
    lists:foreach(fun(Caller) -> Caller ! hup end, Conference).

join(Config) ->
    Server = ?config(server, Config),
    {_Prev, Conference} = get_saved_config(?config(saved_config, Config)),
    F = fun() ->
        {joined, Conference} = Server:join(),
        ok = loop(Server, [self() | Conference])
    end,
    Pid = spawn(F),
    {save_config, [Pid | Conference]}.

get_saved_config(undefined) -> {undefined, []};
get_saved_config(SavedConfig) -> SavedConfig.

loop(Server, Conference) ->
    receive
        {connected, Pid} ->
            % Send greeting to new caller.
            ok = Server:send({hello, Pid}),
            loop(Server, [Pid | Conference]);
        {diconnected, Pid} ->
            loop(Server, lists:delete(Pid, Conference));
        {message, From, {hello, Pid}} when From =/= Pid, Pid =:= self() ->
            % Somebody else has greeted us.
            loop(Server, Conference);
        hup -> ok;
        Msg ->
            ct:fail({unexpected, Msg})
    end.

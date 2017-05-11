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

join(Config) ->
    {_Prev, Conference} = get_saved_config(?config(saved_config, Config)),
    {Pid, Conference} = client_bot:start(?config(server, Config)),
    {save_config, [Pid | Conference]}.

all_leave(Config) ->
    {elephant, Conference} = get_saved_config(?config(saved_config, Config)),
    lists:foreach(fun client_bot2:stop/1, Conference).

get_saved_config(undefined) -> {undefined, []};
get_saved_config(SavedConfig) -> SavedConfig.

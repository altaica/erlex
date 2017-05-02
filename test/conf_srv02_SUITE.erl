-module(conf_srv02_SUITE).
-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

all() -> [joe_joins, mike_joins, robert_joins].

init_per_suite(Config) ->
    {ok, _Pid} = conf_srv02:start(),
    [{server, conf_srv02} | Config].

end_per_suite(Config) ->
    Server = ?config(server, Config),
    ok = Server:stop().

joe_joins(Config) ->    common:join(Config).
mike_joins(Config) ->   common:join(Config).
robert_joins(Config) -> common:join(Config).

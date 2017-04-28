-module(conf_srv03_SUITE).
-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

all() -> [joe_joins, mike_joins, robert_joins].

init_per_suite(Config) ->
    {ok, _Pid} = conf_srv03:start(),
    Config.

end_per_suite(_Config) ->
    ok = conf_srv03:stop().

joe_joins(Config) ->    common:join(conf_srv03, Config).
mike_joins(Config) ->   common:join(conf_srv03, Config).
robert_joins(Config) -> common:join(conf_srv03, Config).

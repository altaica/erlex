-module(conf_srv02_SUITE).
-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

all() -> [joe_joins, mike_joins, robert_joins].

init_per_suite(Config) ->
    {ok, _Pid} = conf_srv02:start(),
    Config.

end_per_suite(_Config) ->
    ok = conf_srv02:stop().

joe_joins(Config) ->    common:join(conf_srv02, Config).
mike_joins(Config) ->   common:join(conf_srv02, Config).
robert_joins(Config) -> common:join(conf_srv02, Config).

%%% @doc Test suite for simple conference server.
%%% @copyright 2017 Phil Dempster

-module(hello_joe_SUITE).
-compile([nowarn_unused_function]).
-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, Server} || Server <- instances()].

groups() ->
    [{Server, testcases()} || Server <- instances()].

init_per_group(Server, Config) ->
    {ok, _Pid} = apply(Server, start, []),
    [{server, Server} | Config].

end_per_group(Server, _Config) ->
    ok = apply(Server, stop, []).


instances() ->
    [conf_srv01, conf_srv02, conf_srv03].

testcases() ->
    [hello_joe, hello_mike, hello_robert, goodbye].

hello_joe(Config) ->    new_chatbot(Config, joe).
hello_mike(Config) ->   new_chatbot(Config, mike).
hello_robert(Config) -> new_chatbot(Config, robert).

new_chatbot(Config, Name) ->
    {_Prev, Conference} = get_saved_config(?config(saved_config, Config)),
    {_Pid,  Conference} = chatbot:start(?config(server, Config), Name),
    {save_config, [Name | Conference]}.

goodbye(Config) ->
    {_Prev, Conference} = get_saved_config(?config(saved_config, Config)),
    lists:foreach(fun chatbot:stop/1, lists:reverse(Conference)).

get_saved_config(undefined) -> {undefined, []};
get_saved_config(SavedConfig) -> SavedConfig.

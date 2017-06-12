# Hello Joe

Copyright 2017 Phil Dempster

This example application is designed as a tutorial based on 3 iterations of a simple conference server with identical functionality:

1. Direct implementation
2. Implementation with generic code separated
3. OTP gen_server implementation

## Common funtionality

The server exports the following API:

Function                        | Description
--------                        | -----------
`start() -> {ok, pid()}`        | Start the conference server
`join() -> {joined, [pid()]}`   | Join the conference; returns list of existing participants
`send(Message::term()) -> ok`   | Send a broadcast message to all participants
`stop() -> ok`                  | Stop the conference server

On joining the server, the caller is announced to the rest of the participants. Participants remain part of the conference as long as the caller process lives. Once the caller terminates, the server announces their disconnection to all remaining participants.

## Chatbot

A "chatbot" is used to facilitate both automated and manual testing.

When the server informs the bot that another caller has joined, the bot will send a greeting {hello, Pid} to the new caller.

NB the bot expects that calls will be torn down in the same order as they are set up. This allows for disconnect testing.

## Manual testing

From a terminal, use `rebar3 shell`, e.g.:

    Eshell V8.3  (abort with ^G)
    1> conf_srv01:start().
    {ok,<0.102.0>}
    2> {Joe, []} = chatbot:start(conf_srv01).
    {<0.104.0>,[]}
    3> {Mike, [Joe]} = chatbot:start(conf_srv01).
    {<0.106.0>,[<0.104.0>]}
    4> {Robert, [Mike, Joe]} = chatbot:start(conf_srv01).
    {<0.108.0>,[<0.106.0>,<0.104.0>]}
    5> {joined, [Robert, Mike, Joe]} = conf_srv01:join().
    {joined,[<0.108.0>,<0.106.0>,<0.104.0>]}
    6> flush().
    Shell got {message,<0.108.0>,{hello,<0.95.0>}}
    Shell got {message,<0.106.0>,{hello,<0.95.0>}}
    Shell got {message,<0.104.0>,{hello,<0.95.0>}}
    7> conf_srv01:send({hello, Joe}).
    ok
    8> flush().
    Shell got {message,<0.95.0>,{hello,<0.104.0>}}
    ok
    9> chatbot:stop(Joe).
    ok
    10> flush().
    Shell got {disconnected,<0.104.0>}
    ok


[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Build Status](https://travis-ci.org/altaica/simple_conference.svg?branch=master)](https://travis-ci.org/altaica/simple_conference)

Simple Conference Server
========================

Copyright 2017 Phil Dempster

This repository contains a super-simple conference server in 3 flavours with identical functionality:

1. Direct implementation
2. Implementation with generic code separated
3. OTP gen_server implementation

The server exports the following API:

Function                        | Description
--------                        | -----------
`start() -> {ok, pid()}`        | Start the conference server
`join() -> {joined, [pid()]}`   | Join the conference; returns list of existing participants
`send(Message::term()) -> ok`   | Send a broadcast message to all participants
`stop() -> ok`                  | Stop the conference server

On joining the server, the caller is announced to the rest of the participants. Participants remain part of the conference as long as the caller process lives. Once the caller terminates, the server announces their disconnection to all remaining participants.

Build
-----

The project is set up to build using [rebar3].

VSCode integration
------------------
`Ctrl+Shift+B` (or `Cmd+Shift+B` on Mac) will build & test from within [VSCode].

Linter
------
The [Elvis] rebar3 plugin is used to provide code linting.

From a terminal, use `rebar3 as lint lint`.

Code coverage
-------------
From a terminal, use `rebar3 do eunit, ct, cover`.

Detailed coverage reports can be found by opening `_build/test/cover/index.html`.

Manual testing
--------------
From a terminal, use `rebar3 shell`, e.g.:

    Eshell V8.3  (abort with ^G)
    1> c("test/client_bot").
    {ok,client_bot}
    2> S = conf_srv01:start().
    {ok,<0.102.0>}
    3> {C1, []} = client_bot:start(conf_srv01).
    {<0.104.0>,[]}
    4> {C2, [C1]} = client_bot:start(conf_srv01).
    {<0.106.0>,[<0.104.0>]}
    5> {C3, [C2, C1]} = client_bot:start(conf_srv01).
    {<0.108.0>,[<0.106.0>,<0.104.0>]}
    6> {joined, [C3, C2, C1]} = conf_srv01:join().
    {joined,[<0.108.0>,<0.106.0>,<0.104.0>]}
    7> flush().
    Shell got {message,<0.108.0>,{hello,<0.95.0>}}
    Shell got {message,<0.106.0>,{hello,<0.95.0>}}
    Shell got {message,<0.104.0>,{hello,<0.95.0>}}
    8> conf_srv01:send({hello, C1}).
    ok
    9> flush().
    Shell got {message,<0.95.0>,{hello,<0.104.0>}}
    ok
    10> client_bot:stop(C1).
    ok
    11> flush().
    Shell got {disconnected,<0.104.0>}
    ok

<!-- Tools -->
[rebar3]:   http://www.rebar3.org/
[VSCode]:   https://code.visualstudio.com
[Elvis]:    https://github.com/inaka/elvis

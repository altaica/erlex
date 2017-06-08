[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/altaica/simple_conference.svg?branch=master)](https://travis-ci.org/altaica/simple_conference)

# Simple Conference Server

Copyright 2017 Phil Dempster

This repository contains the source code for a tutorial based on 3 iterations of a simple conference server with identical functionality:

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

## Build

The project is set up to build using [rebar3].

## VSCode integration

`Ctrl+Shift+B` (or `Cmd+Shift+B` on Mac) will build & test from within [VSCode].

## Linter

The [Elvis] rebar3 plugin is used to provide code linting.

From a terminal, use `rebar3 as lint lint`.

## Code coverage

From a terminal, use `rebar3 do eunit, ct, cover`.

Detailed coverage reports can be found by opening `_build/test/cover/index.html`.

## Manual testing

From a terminal, use `rebar3 shell`, e.g.:

    Eshell V8.3  (abort with ^G)
    1> conf_srv01:start().
    {ok,<0.100.0>}
    2> {Joe, []} = client_bot:start(conf_srv01, joe).
    {<0.102.0>,[]}
    3> {Mike, [joe]} = client_bot:start(conf_srv01, mike).
    {<0.104.0>,[joe]}
    4> {Robert, [mike,joe]} = client_bot:start(conf_srv01, robert).
    {<0.106.0>,[mike,joe]}
    5> register(me, self()).
    true
    6> {joined, [Robert,Mike,Joe]} = conf_srv01:join().
    {joined,[<0.106.0>,<0.104.0>,<0.102.0>]}
    7> flush().
    Shell got {message,<0.106.0>,{hello,me}}
    Shell got {message,<0.104.0>,{hello,me}}
    Shell got {message,<0.102.0>,{hello,me}}
    ok
    8> conf_srv01:send({hello, joe}).
    ok
    9> flush().
    Shell got {message,<0.97.0>,{hello,joe}}
    ok
    10> client_bot:stop(Joe).
    ok
    11> flush().
    Shell got {disconnected,<0.102.0>}
    ok

## License

Licensed under either of

 * Apache License, Version 2.0
 * MIT license

at your option.

### Apache License, Version 2.0

> Copyright 2017 Phil Dempster
>
> Licensed under the Apache License, Version 2.0 (the "License");
> you may not use this file except in compliance with the License.
> You may obtain a copy of the License at
>
> <http://www.apache.org/licenses/LICENSE-2.0>
>
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.

### MIT License

> Copyright 2017 Phil Dempster
>
> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
>
> The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


<!-- Tools -->
[rebar3]:   http://www.rebar3.org/
[VSCode]:   https://code.visualstudio.com
[Elvis]:    https://github.com/inaka/elvis

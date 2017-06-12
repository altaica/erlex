[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/altaica/erlex.svg?branch=master)](https://travis-ci.org/altaica/erlex)

# Erlang examples

Copyright 2017 Phil Dempster

1. [hello_joe](docs/hello_joe.md), 3 iterations of a simple conference server
2. [traffic_light](docs/traffic_light.md), a pure Erlang finite state machine

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

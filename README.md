[![Build Status](https://travis-ci.org/altaica/simple_conference.svg?branch=master)](https://travis-ci.org/altaica/simple_conference)

Simple Conference Server
=======================

Copyright 2017 Phil Dempster

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

This repository contains a step by step tutorial for building a super-simple conference server.

Build
-----

The project is set up to build using [rebar3].

VSCode integration
------------------
`Ctrl+Shift+B` (`Cmd+Shift+B` on Mac) will build & test from within VSCode.

Linter
------
A [rebar3] plugin is used to provide code [linting][Elvis].

From a terminal, use `rebar3 as lint lint`.

Code coverage
-------------
From a terminal, use `rebar3 do ct, cover`.

Detailed coverage reports can be found by opening `_build/test/cover/index.html`.

Manual testing
--------------
From a terminal, use `rebar3 shell`, e.g.:

    Eshell V8.2  (abort with ^G)
    1> ...TBD...


<!-- Tools -->
[rebar3]:   http://www.rebar3.org/
[Elvis]:    https://github.com/inaka/elvis

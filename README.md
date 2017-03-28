[![Build Status](https://travis-ci.org/altaica/simple_conference.svg?branch=master)](https://travis-ci.org/altaica/simple_conference)

Simple Conference Server
=======================

This repository contains a step by step tutorial for building a super-simple conference server.

It is set up to build using [rebar3].

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

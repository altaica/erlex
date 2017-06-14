# Cohort

A minimal distributed [Erlang/OTP] application.

Copyright 2017 Phil Dempster

### Overview

> _[cohort] (noun)_: A group of people who share a characteristic.

A _[distributed]_ application is started on all nodes of a cluster, but only runs on one. _[Failover]_ occurs when the node running the application is stopped and another node takes over. _Takeover_ occurs when the application is instructed to run on another node.

The purpose of this example is to:
* Start a distributed Erlang application on a two node cluster
* Demonstrate failover between the nodes
* Demonstrate takeover between the nodes


### Files

| File                      | Description               |
| ------------------------- | ------------------------- |
| src/cohort.erl            | Application source code   |
| src/cohort.app.src        | Application metadata      |
| ebin/cohort.app           | Application metadata¹     |
| config/magnumopus.config  | 1st node configuration    |
| config/obsequilis.config  | 2nd node configuration    |
| config/vm.args            | Run-time system flags     |
| scripts/cohort.cmd        | Start nodes (Windows)     |

¹This is simply a copy of the `.app.src` file.

Most of what is required to make an Erlang application distributed is in the metadata associated with the application (the .app file) and in the configuration passed to the Erlang Run-Time System on startup (the files under `config/` and some other command line parameters). Essentially, we need to tell Erlang that we want the application to be distributed and which nodes we want it to run on.

`cohort.erl` implements the `application` behaviour, which requires it to export the functions `start/2` and `stop/1`. The _StartType_ parameter passed to `start/2` may be `normal` (for inital startup) or a tuple `{failover, Node}` or `{takeover, Node}`, where _Node_ is the name of the node that was previously running.

## Building & running

### Compiling the application

The application can either be compiled standalone with `erlc -o ebin src/cohort.erl` or, using the [rebar3] tool, with `rebar3 compile`.

> Note that [rebar3] uses the `.app.src` file to generate its own copy of the `.app` file.

### Starting the nodes

The cohort application runs on one or both of two nodes, named after the fictional [Roman characters][Asterix] _magnumopus_ and _obsequilis_, and is started by the commands:

    erl -sname magnumopus@localhost         \
        -config config/magnumopus.config    \
        -args_file config/vm.args

And, in another terminal:

    erl -sname obsequilis@localhost         \
        -config config/obsequilis.config    \
        -args_file config/vm.args

What these commands do:
1. Set the (short) name of the node
2. Load the configuration that describes the cluster from the perspective of the node
3. Evaluate file containing common arguments which
    * Set the path to the compiled BEAM file
    * Set the shared 'cookie' that allows the nodes to communicate
    * Start the application

> On Windows, this is automated by the startup script `scripts/cohort.cmd`. Note that the two windows will start on top of each other, with the active node's console hidden behind the standby node.

### Expected output

The active node (initially _magnumopus_) will show:

    (magnumopus@localhost)1> magnumopus@localhost start(normal, [])
    magnumopus@localhost in loop

Terminating the active node will cause a failover to the standby node:

    (obsequilis@localhost)1> obsequilis@localhost start({failover,magnumopus@localhost}, [])
    obsequilis@localhost in loop

> You can run the startup script again to restart the killed node, which will now become the standby node.

Calling `application:takeover/2` on the standby node will force the application to stop on the currently active node and restart on the standby node:

    (obsequilis@localhost)1> application:takeover(cohort, temporary).
    obsequilis@localhost start({takeover,magnumopus@localhost}, [])
    obsequilis@localhost in loop


<!-- References -->
[Erlang/OTP]:   http://www.erlang.org/
[cohort]:       https://dictionary.cambridge.org/dictionary/english/cohort
[distributed]:  http://learnyousomeerlang.com/distributed-otp-applications
[failover]:     https://en.wikipedia.org/wiki/Failover
[rebar3]:       http://www.rebar3.org/
[Asterix]:      https://en.wikipedia.org/wiki/List_of_Asterix_characters#Romans

# Cohort

A minimalist demonstration of failover in a distributed OTP application running on a 2 node cluster.

Copyright 2017 Phil Dempster

### Overview

> _[cohort] (noun)_: A group of people who share a characteristic.

A _[distributed]_ application is started on all nodes of a cluster, but only runs on one. _[Failover]_ occurs when the node running the application is stopped and another node takes over. _Takeover_ occurs when the application is instructed to run on another node.

The purpose of this example is to:
* Start a distributed Erlang application on a two node cluster
* Demonstrate failover between the nodes
* Demonstrate takeover between the nodes


### Files

| File (under apps/cohort)  | Description               |
| ------------------------- | ------------------------- |
| src/cohort.erl            | Application source code   |
| src/cohort.app.src        | Application metadata      |
| config/cohort.config      | Cluster configuration     |
| config/vm.args            | Virtual machine arguments |

Additionally, there are `start_cohort` scripts provided in the `scripts` directory as a convenience in order to start one or (under Windows) both nodes.

Most of what is required to make an Erlang application distributed is in the metadata associated with the application (the .app file) and in the configuration passed to the Erlang Run-Time System on startup (the files under `config/` and some other command line parameters). Essentially, we need to tell Erlang that we want the application to be distributed and which nodes we want it to run on.

`cohort.erl` implements the `application` behaviour, which requires it to export the functions `start/2` and `stop/1`. The _StartType_ parameter passed to `start/2` may be `normal` (for inital startup) or a tuple `{failover, Node}` or `{takeover, Node}`, where _Node_ is the name of the node that was previously running.

## Building & running

### Compiling the application

The application can either be compiled using the [rebar3] tool, with `rebar3 compile`, or standalone with:

    mkdir ebin && erlc -o ebin apps/cohort/src/cohort.erl && cp apps/cohort/src/cohort.app.src ebin/

### Starting the nodes

The cohort application runs on one or both of two nodes, named after the [fictional Roman characters][Asterix] _Magnumopus_ and _Obsequilis_, and is started by the command `sh scripts/start_cohort.sh` run in two seperate terminals.

After checking if the _magnumopus_ node is already started, this script:
1. Sets the (short) name of the node
2. Evaluates a file containing VM arguments which
    * Set the path to the compiled BEAM file
    * Set the shared 'cookie' that allows the nodes to communicate
    * Loads the configuration that describes the cluster
    * Starts the application

> Since on Windows the Erlang shell can be started as a GUI application, both nodes are started by the script `scripts/start_cohort.cmd`. For convenience, you can create a "Start Cohort" shortcut to this file in the top level `erlex` directory (be sure to set the working directory to the top level directory also). Note that the two windows will start on top of each other, with the active node's console hidden behind the standby node.

### Expected output

The active node (initially _magnumopus_) will show:

    =INFO REPORT==== 14-Jun-2017::10:36:41 ===
    magnumopus@localhost start(normal, [])

    =INFO REPORT==== 14-Jun-2017::10:36:41 ===
    magnumopus@localhost in loop

Calling `application:takeover/2` on the standby node will force the application to stop on the currently active node and restart on the standby node:

    (obsequilis@localhost)1> application:takeover(cohort, temporary).
    ok

    =INFO REPORT==== 14-Jun-2017::10:39:48 ===
    obsequilis@localhost start({takeover,magnumopus@localhost}, [])

    =INFO REPORT==== 14-Jun-2017::10:39:48 ===
    obsequilis@localhost in loop

Terminating the active node will cause a failover to the standby node:

    =INFO REPORT==== 14-Jun-2017::10:40:23 ===
    magnumopus@localhost start({failover,obsequilis@localhost}, [])

    =INFO REPORT==== 14-Jun-2017::10:40:23 ===
    magnumopus@localhost in loop

> You can run the startup script again (or use the Windows shortcut if you have created one) to restart the killed node, which will now become the standby node.


<!-- References -->
[cohort]:       https://dictionary.cambridge.org/dictionary/english/cohort
[distributed]:  http://learnyousomeerlang.com/distributed-otp-applications
[failover]:     https://en.wikipedia.org/wiki/Failover
[rebar3]:       http://www.rebar3.org/
[Asterix]:      https://en.wikipedia.org/wiki/List_of_Asterix_characters#Romans

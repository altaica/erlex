#!/bin/sh

# Function to start a node and run the `cohort' application on it.
start_node()
{
    erl -args_file apps/cohort/config/$1.args \
        -config apps/cohort/config/sys.config \
        -pa _build/default/lib/cohort/ebin ebin
}

# If node not running, start it.
if ! escript scripts/node_check.erl magnumopus; then
    start_node magnumopus
else
    start_node obsequilis
fi

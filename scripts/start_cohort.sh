#!/bin/sh

# Function to start a node and run the `cohort' application on it.
start_node()
{
    erl -sname $1@localhost -args_file apps/cohort/config/vm.args
}

# If node not running, start it.
if ! escript scripts/node_check.erl magnumopus; then
    start_node magnumopus
else
    start_node obsequilis
fi

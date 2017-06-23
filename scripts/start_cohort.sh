#!/bin/sh

CONFIG_DIR=apps/cohort/config

# Function to start a node and run the availability server on it.
start_node()
{
    erl -sname      $1@localhost            \
        -config     $CONFIG_DIR/$1.config   \
        -args_file  $CONFIG_DIR/vm.args
}

# If node not running, [re-]start it.
if ! escript scripts/node_check.erl magnumopus; then
    start_node magnumopus
else
    start_node obsequilis
fi

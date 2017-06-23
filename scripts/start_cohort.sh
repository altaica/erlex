#!/bin/sh

CONFIG_DIR=apps/cohort/config

# Function to start a node and run the availability server on it.
start_node()
{
    erl -sname      $1@localhost            \
        -config     $CONFIG_DIR/$1.config   \
        -args_file  $CONFIG_DIR/vm.args
}

if [ "$1" != "" ]; then
    # [Re]start specified node.
    start_node $1
else
    echo "Usage: $0 <magnumopus|obsequilis>"
fi

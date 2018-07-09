#!/bin/sh
# Run erl_tidy tool in test mode (with some filters)

set -e
erl -noshell -eval "erl_tidy:dir("apps", [{test, true}])." -s init stop \
    | grep -v "tidying directory" \
    | grep -v "apps/[a-z0-9_]*/test""

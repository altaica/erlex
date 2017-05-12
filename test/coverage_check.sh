#!/bin/sh
set -e
N=`grep -c "<td><strong>Total</strong></td><td>100%</td>" _build/test/cover/index.html`
if [ $N -lt 3 ]; then
    echo "Coverage check failed: $N out of 3 modules have full test coverage"
    exit 1
fi

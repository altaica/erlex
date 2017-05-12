#!/bin/sh
set -e
grep -q "<td><strong>Total</strong></td><td>100%</td>" _build/test/cover/index.html

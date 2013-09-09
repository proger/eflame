#!/bin/sh

me="$(dirname "$0")"

sort | uniq -c | sort -n -k1 | awk '{print $2, " ", $1}' | $me/flamegraph.pl

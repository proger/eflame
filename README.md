## eflame

[Flame Graphs](http://dtrace.org/blogs/brendan/2011/12/16/flame-graphs/) for Erlang.

![screenshot](http://i.imgur.com/EsiPMxY.png)

Usage example: https://github.com/proger/active/commit/81e7e40c9dc5a4666742636ea4c5dfafc41508a5

```sh
$ stack_to_flame.sh < stacks.out > flame.svg
$ open flame.svg
```

### Notes

* as stacks are collected through tracing, blocking calls appear like the process is actually working,
  this will change in the future.

* unlike the reference implementation, `flamegraph.pl` does not sort the input to preserve the order of calls
  (since this is possible due to current method of collecting stacks)

```sh
# this invocation preserves order of calls (unlike stack_to_flame.sh)
$ grep 0.90.0 stacks.out | uniq -c | awk '{print $2, "", $1}' | env TITLE='rebar compile apps=eflame' deps/eflame/stack_to_flame.sh > flame.svg
```

![screenshot2](http://i.imgur.com/UG6W9G0.png)

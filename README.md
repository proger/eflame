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

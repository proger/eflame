## eflame

[Flame Graphs](http://dtrace.org/blogs/brendan/2011/12/16/flame-graphs/) for Erlang.  Uses `erlang:trace/3` API.

![screenshot](http://i.imgur.com/XIDAcd3.png)

Usage example: https://github.com/proger/active/commit/81e7e40c9dc5a4666742636ea4c5dfafc41508a5

```erlang
> eflame:apply(normal_with_children, "stacks.out", my_module, awesome_calculation, []).
> eflame:apply(my_module, awesome_calculation, []). % same as above
> eflame:apply(normal, "stacks.out", my_module, awesome_calculation, []). % won't trace children
```

```sh
$ stack_to_flame.sh < stacks.out > flame.svg
$ open flame.svg
```

### Notes

* as stacks are collected through tracing, blocking calls are noticed and are drawn in blue

* unlike the reference implementation, `flamegraph.pl` does not sort the input to preserve the order of calls
  (since this is possible due to current method of collecting stacks)

```sh
$ grep 0.90.0 stacks.out | deps/eflame/flamegraph.pl > flame.svg

# this invocation draws a separate flame graph for each traced process
$ for pid in $(cat stacks.out | awk -F';' '{print $1}' | uniq | tr -d '<>'); do
    grep $pid stacks.out | deps/eflame/flamegraph.pl --title="$pid" > flame_$pid.svg;
done

# you may also use stacks_to_flames.sh (uses zsh)
$ deps/eflame/stacks_to_flames.sh stacks.out
```


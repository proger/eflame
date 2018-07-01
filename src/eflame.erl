-module(eflame).

-export([apply/2,
         apply/3,
         apply/4,
         apply/5]).

-define(RESOLUTION, 1000). %% us
-record(dump, {stack = [], us = 0, acc = []}). % per-process state

-define(DEFAULT_MODE, normal_with_children).
-define(DEFAULT_OUTPUT_FILE, "stacks.out").
-define(DEFAULT_TIMEOUT, 10000).

-define(LOG(Msg, Args), io:format(Msg, Args)).
-define(LOG(Msg), ?LOG(Msg, [])).

-spec apply(function(), [any()]) -> any().
apply(F, A) ->
  do_apply(?DEFAULT_MODE, ?DEFAULT_OUTPUT_FILE, {F, A}).

-spec apply(module(), atom(), [any()]) -> any().
apply(M, F, A) ->
  do_apply(?DEFAULT_MODE, ?DEFAULT_OUTPUT_FILE, {{M, F}, A}).

-spec apply(atom(), string(), function(), [any()]) -> any().
apply(Mode, OutputFile, Fun, Args) ->
  do_apply(Mode, OutputFile, {Fun, Args}).

-spec apply(atom(), string(), module(), atom(), [any()]) -> any().
apply(Mode, OutputFile, M, F, A) ->
  do_apply(Mode, OutputFile, {{M, F}, A}).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec do_apply(atom(), string(), {{module(), atom()} | function(), [any()]}) ->
  {ok, any()} | timeout.
do_apply(Mode, OutputFile, {Fun, Args}) ->
  Tracer = spawn_tracer(),

  start_trace(Tracer, self(), Mode),

  F      = build_fun(Fun, Args),
  Ref    = apply_fun(F, self()),
  Result = wait_result(Ref, ?DEFAULT_TIMEOUT),
  {ok, Bytes} = stop_trace(Tracer, self()),

  ok = file:write_file(OutputFile, Bytes),
  Result.

-spec wait_result(reference(), timeout()) -> {ok, any()} | timeout.
wait_result(Ref, Timeout) ->
  receive {Ref, Result} -> {ok, Result}
  after Timeout -> timeout
  end.

-spec build_fun({module(), atom()} | function(), [any()]) -> function().
build_fun({M, F}, A) ->
  fun() -> erlang:apply(M, F, A) end;
build_fun(F, A) ->
  fun() -> erlang:apply(F, A) end.

-spec apply_fun(function(), pid()) -> function().
apply_fun(InnerFun, Pid) ->
  Ref = make_ref(),
  Fun = fun() ->
            Result = InnerFun(),
            Pid ! {Ref, Result}
        end,
  spawn_link(Fun),
  Ref.

start_trace(Tracer, Target, Mode) ->
  MatchSpec = [{'_', [], [{message, {{cp, {caller}}}}]}],
  erlang:trace_pattern(on_load, MatchSpec, [local]),
  erlang:trace_pattern({'_', '_', '_'}, MatchSpec, [local]),
  erlang:trace(Target, true, [{tracer, Tracer} | trace_flags(Mode)]),
  ok.

stop_trace(Tracer, Target) ->
  erlang:trace(Target, false, [all]),
  Tracer ! {dump_bytes, self()},

  Ret = receive {bytes, B} -> {ok, B}
        after 5000 -> {error, timeout}
        end,

  exit(Tracer, normal),
  Ret.

spawn_tracer() -> spawn(fun() -> trace_listener(#{}) end).

trace_flags(normal) ->
  [call, arity, return_to, timestamp, running];
trace_flags(normal_with_children) ->
  [call, arity, return_to, timestamp, running, set_on_spawn];
trace_flags(like_fprof) ->
  %% fprof does this as 'normal', will not work!
  [ call, return_to, running
  , procs, garbage_collection
  , arity, timestamp, set_on_spawn
  ].

-spec trace_listener(map()) ->
  {stacks, map()} | {bytes, binary()}.
trace_listener(State0) ->
  receive
    {dump, Pid} ->
      Pid ! {stacks, State0};
    {dump_bytes, Pid} ->
      ?LOG("Dumping bytes...~n"),
      IOList = [ dump_to_iolist(TPid, Dump#dump.acc)
                 || {TPid, Dump} <- maps:to_list(State0)
               ],

      Bytes = iolist_to_binary(IOList),
      Pid ! {bytes, Bytes};
    Term ->
      trace_ts  = element(1, Term),
      Pid       = element(2, Term),

      PidState0 = maps:get(Pid, State0, #dump{}),
      PidState1 = trace_proc_stream(Term, PidState0),

      State1    = maps:put(Pid, PidState1, State0),
      trace_listener(State1)
  end.

us({Mega, Secs, Micro}) ->
  Mega * 1000000000000 + Secs * 1000000 + Micro.

new_state(#dump{us = 0} = State, Stack, Ts) ->
  UsTs = us(Ts),
  State#dump{us = UsTs, stack = Stack};
new_state(#dump{us = Us, acc = Acc} = State, Stack, Ts) ->
  %% io:format("new state: ~p ~p ~p~n", [Us, length(Stack), Ts]),
  Diff       = us(Ts) - Us,
  NOverlaps  = Diff div ?RESOLUTION,
  Overlapped = NOverlaps * ?RESOLUTION,
  %% Rem = Diff - Overlapped,
  case NOverlaps >= 1 of
    true ->
      %% ?LOG("Overlaps ~p~n", [NOverlaps]),
      State#dump{ us    = Us + Overlapped
                , acc   = [{NOverlaps, Stack} | Acc]
                , stack = Stack
                };
    false ->
      State#dump{stack = Stack}
  end.

trace_proc_stream( {trace_ts, _Ps, call, MFA, {cp, {_,_,_} = CallerMFA}, Ts}
                 , #dump{stack=[]} = State
                 ) ->
  new_state(State, [MFA, CallerMFA], Ts);
trace_proc_stream( {trace_ts, _Ps, call, MFA, {cp, undefined}, Ts}
                 , #dump{stack=[]} = State
                 ) ->
  new_state(State, [MFA], Ts);
trace_proc_stream( {trace_ts, _Ps, call, MFA, {cp, undefined}, Ts}
                 , #dump{stack=[MFA|_] = Stack} = State
                 ) ->
  new_state(State, Stack, Ts);
trace_proc_stream( {trace_ts, _Ps, call, MFA, {cp, undefined}, Ts}
                 , #dump{stack=Stack} = State
                 ) ->
  new_state(State, [MFA | Stack], Ts);
trace_proc_stream( {trace_ts, _Ps, call, MFA, {cp, MFA}, Ts}
                 , #dump{stack=[MFA|Stack]} = State
                 ) ->
  new_state(State, [MFA|Stack], Ts); % collapse tail recursion
trace_proc_stream( {trace_ts, _Ps, call, MFA, {cp, CpMFA}, Ts}
                 , #dump{stack=[CpMFA|Stack]} = State
                 ) ->
  new_state(State, [MFA, CpMFA|Stack], Ts);
trace_proc_stream( {trace_ts, _Ps, call, _MFA, {cp, _}, _Ts} = TraceTs
                 , #dump{stack=[_|StackRest]} = State
                 ) ->
  trace_proc_stream(TraceTs, State#dump{stack=StackRest});
trace_proc_stream( {trace_ts, _Ps, return_to, MFA, Ts}
                 , #dump{stack=[_Current, MFA|Stack]} = State
                 ) ->
  %% do not try to traverse stack down because we've already collapsed it
  new_state(State, [MFA|Stack], Ts);
trace_proc_stream({trace_ts, _Ps, return_to, undefined, _Ts}, State) ->
  State;
trace_proc_stream({trace_ts, _Ps, return_to, _, _Ts}, State) ->
  State;
trace_proc_stream( {trace_ts, _Ps, in, _MFA, Ts}
                 , #dump{stack=[sleep|Stack]} = State
                 ) ->
  new_state(new_state(State, [sleep|Stack], Ts), Stack, Ts);
trace_proc_stream( {trace_ts, _Ps, in, _MFA, Ts}
                 , #dump{stack=Stack} = State
                 ) ->
  new_state(State, Stack, Ts);
trace_proc_stream( {trace_ts, _Ps, out, _MFA, Ts}
                 , #dump{stack=Stack} = State
                 ) ->
  new_state(State, [sleep|Stack], Ts);
trace_proc_stream(TraceTs, State) ->
  io:format("trace_proc_stream: unknown trace: ~p~n", [TraceTs]),
  State.

%% Conversion to iolist()

stack_collapse(Stack) ->
  intersperse(";", Stack, []).

entry_to_iolist({M, F, A}) ->
  [ atom_to_binary(M, utf8), <<":">>
  , atom_to_binary(F, utf8), <<"/">>
  , integer_to_binary(A)
  ];
entry_to_iolist(A) when is_atom(A) ->
  [atom_to_binary(A, utf8)].

intersperse(_, [], Result)  ->
  Result;
intersperse(Sep, [X | Xs], [])  ->
  intersperse(Sep, Xs, [entry_to_iolist(X)]);
intersperse(Sep, [X | Xs], Result) ->
  intersperse(Sep, Xs, [entry_to_iolist(X), Sep | Result]).

dump_to_iolist(Pid, Stacks) ->
  PidList = pid_to_list(Pid),
  dump_to_iolist(PidList, Stacks, []).

dump_to_iolist(_PidList, [], Result) ->
  Result;
dump_to_iolist(PidList, [{N, Stack} | Rest], Result) ->
  Item  = stack_to_iolist(PidList, N, Stack),
  dump_to_iolist(PidList, Rest, [Item | Result]).

-spec stack_to_iolist(string(), integer(), list()) -> iolist().
stack_to_iolist(PidList, N, Stack) ->
  [ PidList, <<";">>, stack_collapse(Stack)
  ,  <<" ">>, integer_to_binary(N)
  , <<"\n">>].

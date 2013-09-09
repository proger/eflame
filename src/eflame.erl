-module(eflame).
-export([apply/3, apply/4]).

-define(RESOLUTION, 1000). %% us
-record(dump, {stack=[], us=0, acc=[]}).

apply(M, F, A) ->
    ?MODULE:apply("stacks.out", M, F, A).

apply(OutputFile, M, F, A) ->
    Tracer = spawn(fun() -> fprof_dump_listener(dict:new()) end),
    fprof:apply(M, F, A, [{tracer, Tracer}]),

    Tracer ! {dump, self()},
    PS = receive
        {stacks, X} -> X
    after 5000 ->
        timeout
    end,

    Bytes = iolist_to_binary([dump_to_iolist(Pid, Dump) || {Pid, [Dump]} <- PS]),
    ok = file:write_file(OutputFile, Bytes),
    PS.

fprof_dump_listener(State) ->
    receive
        {dump, Pid} ->
            Pid ! {stacks, dict:to_list(State)};
        Term ->
            trace_ts = element(1, Term),
            PidS = element(2, Term),

            PidState = case dict:find(PidS, State) of
                {ok, [Ps]} -> Ps;
                error -> #dump{}
            end,

            NewPidState = fprof_dump_proc_stream(Term, PidState),

            D1 = dict:erase(PidS, State),
            D2 = dict:append(PidS, NewPidState, D1),
            fprof_dump_listener(D2)
    end.

us({Mega, Secs, Micro}) ->
    Mega*1000*1000*1000*1000 + Secs*1000*1000 + Micro.

new_state(#dump{us=Us, acc=Acc} = State, Stack, Ts) ->
    %io:format("new state: ~p ~p ~p~n", [Us, length(Stack), Ts]),
    UsTs = us(Ts),
    case Us of
        0 -> State#dump{us=UsTs, stack=Stack};
        _ when Us > 0 ->
            case us(Ts) - Us of
                X when X >= ?RESOLUTION ->
                    State#dump{us=Us+?RESOLUTION, acc=[lists:reverse(Stack)|Acc], stack=Stack};
                _ ->
                    State#dump{stack=Stack}
            end
    end.

fprof_dump_proc_stream({trace_ts, _Ps, call, MFA, {cp, CallerMFA}, Ts}, #dump{stack=[]} = State) ->
    new_state(State, [MFA, CallerMFA], Ts);

fprof_dump_proc_stream({trace_ts, _Ps, call, MFA, {cp, MFA}, Ts}, #dump{stack=[MFA|Stack]} = State) ->
    new_state(State, [MFA|Stack], Ts); % collapse tail recursion

fprof_dump_proc_stream({trace_ts, _Ps, call, MFA, {cp, CpMFA}, Ts}, #dump{stack=[CpMFA|Stack]} = State) ->
    new_state(State, [MFA, CpMFA|Stack], Ts);

fprof_dump_proc_stream({trace_ts, _Ps, call, _MFA, {cp, _}, _Ts} = TraceTs, #dump{stack=[_|StackRest]} = State) ->
    fprof_dump_proc_stream(TraceTs, State#dump{stack=StackRest});

fprof_dump_proc_stream({trace_ts, _Ps, T, _Args, _Ts} = TraceTs, State) ->
    case T of
        out -> ignore;
        in -> ignore;
        gc_start -> ignore;
        gc_end -> ignore;
        getting_unlinked -> ignore;
        return_to -> ignore;
        _ -> io:format("fprof_dump_proc_stream: unknown trace: ~p~n", [TraceTs])
    end,
    State;

fprof_dump_proc_stream(TraceTs, State) ->
    io:format("fprof_dump_proc_stream: unknown trace: ~p~n", [TraceTs]),
    State.

stack_collapse(Stack) ->
    intercalate(";", [[atom_to_binary(M, utf8), <<":">>, atom_to_binary(F, utf8), <<"/">>, integer_to_list(A)] || {M, F, A} <- Stack]).

dump_to_iolist(Pid, #dump{acc=Acc}) ->
    [[pid_to_list(Pid), <<";">>, stack_collapse(S), <<"\n">>] || S <- Acc].

intercalate(Sep, Xs) -> lists:concat(intersperse(Sep, Xs)).

intersperse(_, []) -> [];
intersperse(_, [X]) -> [X];
intersperse(Sep, [X | Xs]) -> [X, Sep | intersperse(Sep, Xs)].


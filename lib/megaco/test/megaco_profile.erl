%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%

%%
%%----------------------------------------------------------------------
%% megaco_profile: Utility module used for megaco profiling
%%----------------------------------------------------------------------

-module(megaco_profile).

-export([profile/2, prepare/2, analyse/1,
         fprof_to_calltree/1, fprof_to_calltree/2, fprof_to_calltree/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Execute Fun and profile it with fprof.
profile(Slogan, Fun) when is_function(Fun, 0) ->
    Pids = [self()],
    {ok, TraceFile} = prepare(Slogan, Pids),
    Res = (catch Fun()),
    {ok, _DestFile} = analyse(Slogan),
    ok = file:delete(TraceFile),
    {ok, _TreeFile} = fprof_to_calltree(Slogan),
    Res.

%% Prepare for tracing
prepare(Slogan, Pids) ->
    TraceFile = lists:concat(["profile_", Slogan, "-fprof.trace"]),
    {ok, _Pid} = fprof:start(),
    erlang:garbage_collect(),
    TraceOpts = [start, 
		 {cpu_time, false},
		 {procs, Pids},
		 {file, TraceFile}
		],
    ok = fprof:trace(TraceOpts),
    {ok, TraceFile}.

%% Stop tracing and analyse it
analyse(Slogan) ->
    fprof:trace(stop),
    TraceFile = lists:concat(["profile_", Slogan, "-fprof.trace"]),
    DestFile = lists:concat(["profile_", Slogan, ".fprof"]),
    try
        case fprof:profile([{file, TraceFile}]) of
            ok ->
                ok = fprof:analyse([{dest, DestFile}, {totals, false}]),
                {ok, DestFile};
            {error, Reason} ->
                {error, Reason}
        end
    after
         fprof:stop()
    end.

fprof_to_calltree(Slogan) ->
    fprof_to_calltree(Slogan, 0).

fprof_to_calltree(Slogan, MinPercent) ->
    DestFile = lists:concat(["profile_", Slogan, ".fprof"]),
    TreeFile = lists:concat(["profile_", Slogan, ".calltree"]),
    fprof_to_calltree(DestFile, TreeFile, MinPercent).

%% Create a calltree from an fprof file
fprof_to_calltree(FromFile, ToFile, MinPercent) ->
    ReplyTo = self(),
    Ref = make_ref(),
    spawn_link(fun() ->
                       ReplyTo ! {Ref, do_fprof_to_calltree(FromFile, ToFile, MinPercent)}
               end),
    wait_for_reply(Ref).

wait_for_reply(Ref) ->
    receive
        {Ref, Res} ->
            Res;
        {'EXIT', normal} ->
            wait_for_reply(Ref);
        {'EXIT', Reason} ->
            exit(Reason)
    end.

do_fprof_to_calltree(FromFile, ToFile, MinPercent) ->
    {ok, Fd} = file:open(ToFile, [write, raw]),

    {ok, ConsultedFromFile} = file:consult(FromFile),
    [_AnalysisOpts, [_Totals] | Terms] = ConsultedFromFile,
    Processes = split_processes(Terms, [], []),
    Indent = "",
    Summary = collapse_processes(Processes),
    {_Label, _Cnt, Acc, _Own, _Roots, Details} = Summary,
    %% log(Fd, Label, Indent, Acc, {Label, Cnt, Acc, Own}, [], 0),
    gen_calltree(Fd, Indent, Acc, Summary, MinPercent),
    Delim = io_lib:format("\n~80..=c\n\n", [$=]),
    Write = 
        fun(P) ->
                file:write(Fd, Delim),
                gen_calltree(Fd, Indent, Acc, P, MinPercent)
        end,
    lists:foreach(Write, Processes),
    file:write(Fd, Delim),
    gen_details(Fd, Acc, Details),
    file:close(Fd),
    {ok, ToFile}.

%% Split all fprof terms into a list of processes
split_processes([H | T], ProcAcc, TotalAcc) ->
    if
	is_tuple(H) ->
	    split_processes(T, [H | ProcAcc], TotalAcc);
	is_list(H), ProcAcc =:= [] ->
	    split_processes(T, [H], TotalAcc);
	is_list(H) ->
            ProcAcc2 = rearrange_process(lists:reverse(ProcAcc)),
	    split_processes(T, [H], [ProcAcc2 | TotalAcc])
    end;
split_processes([], [], TotalAcc) ->
    lists:reverse(lists:keysort(3, TotalAcc));
split_processes([], ProcAcc, TotalAcc) ->
    ProcAcc2 = rearrange_process(lists:reverse(ProcAcc)),
    lists:reverse(lists:keysort(3, [ProcAcc2 | TotalAcc])).

%% Rearrange the raw process list into a more useful format
rearrange_process([[{Label, _Cnt, _Acc, _Own} | _ ] | Details]) ->
    do_rearrange_process(Details, Details, Label, [], []).

do_rearrange_process([{CalledBy, Current, _Calls} | T], Orig, Label, Roots, Undefs) ->
    case  [{undefined, Cnt, safe_max(Acc, Own), Own} ||
              {undefined, Cnt, Acc, Own} <- CalledBy] of
        [] ->
            do_rearrange_process(T, Orig, Label, Roots, Undefs);
        NewUndefs ->
            do_rearrange_process(T, Orig, Label, [Current | Roots], NewUndefs ++ Undefs)
    end;
do_rearrange_process([], Details, Label, Roots, Undefs) ->
    [{undefined, Cnt, Acc, Own}] = collapse_calls(Undefs, []),
    Details2 = sort_details(3, Details),
    {Label, Cnt, Acc, Own, lists:reverse(lists:keysort(3, Roots)), Details2}.

%% Compute a summary of the rearranged process info
collapse_processes(Processes) ->
    Headers = lists:map(fun({_L, C, A, O, _R, _D}) -> {"SUMMARY", C, A, O} end,
                        Processes),
    [{Label, Cnt, Acc, Own}] = collapse_calls(Headers, []),
    Details = lists:flatmap(fun({_L, _C, _A, _O, _R, D}) -> D end, Processes),
    Details2 = do_collapse_processes(sort_details(1, Details), []),
    Roots = lists:flatmap(fun({_L, _C, _A, _O, R, _D}) -> R end, Processes),
    RootMFAs = lists:usort([MFA || {MFA, _, _, _} <- Roots]),
    Roots2 = [R || RootMFA <- RootMFAs,
                   {_, {MFA, _, _, _} = R, _} <- Details2,
                   MFA =:= RootMFA],
    Roots3 = collapse_calls(Roots2, []),
    {Label, Cnt, Acc, Own, Roots3, Details2}.

do_collapse_processes([{CalledBy1, {MFA, Cnt1, Acc1, Own1}, Calls1} | T1],
                      [{CalledBy2, {MFA, Cnt2, Acc2, Own2}, Calls2} | T2]) ->
    Cnt       = Cnt1 + Cnt2,
    Acc       = Acc1 + Acc2,
    Own       = Own1 + Own2,
    Current   = {MFA, Cnt, Acc, Own},
    CalledBy0 = CalledBy1 ++ CalledBy2,
    Calls0    = Calls1 ++ Calls2,
    CalledBy  = collapse_calls(lists:keysort(3, CalledBy0), []),
    Calls     = collapse_calls(lists:keysort(3, Calls0), []),
    do_collapse_processes(T1, [{CalledBy, Current, Calls} | T2]);
do_collapse_processes([{CalledBy, Current, Calls} | T1],
                      T2) ->
    do_collapse_processes(T1, [{CalledBy, Current, Calls} | T2]);
do_collapse_processes([],
                      T2) ->
    sort_details(3, T2).

%% Reverse sort on acc field
sort_details(Pos, Details) ->
    Pivot = fun({_CalledBy1, Current1, _Calls1},
                {_CalledBy2, Current2, _Calls2}) ->
                    element(Pos, Current1) =< element(Pos, Current2)
            end,
    lists:reverse(lists:sort(Pivot, Details)).

%% Compute a summary from a list of call tuples
collapse_calls([{MFA, Cnt1, Acc1, Own1} | T1],
               [{MFA, Cnt2, Acc2, Own2} | T2]) ->
    Cnt = Cnt1 + Cnt2,
    Acc = safe_sum(Acc1, Acc2),
    Own = Own1 + Own2,
    collapse_calls(T1, [{MFA, Cnt, Acc, Own} | T2]);
collapse_calls([{MFA, Cnt, Acc, Own} | T1],
               T2) ->
    collapse_calls(T1, [{MFA, Cnt, Acc, Own} | T2]);
collapse_calls([],
              T2) ->
    lists:reverse(lists:keysort(3, T2)).

safe_sum(Int1, Int2) ->    
    if
        Int1 =:= undefined -> Int2;
        Int2 =:= undefined -> Int1;
        true               -> Int1 + Int2
    end.

safe_max(Int1, Int2) ->    
    if
        Int1 =:= undefined ->
            io:format("111\n", []),
            Int2;
        Int2 =:= undefined ->
            io:format("222\n", []),
            Int1;
        Int2 > Int1        -> Int2;
        true               -> Int1
    end.

%% Compute a calltree and write it to file
gen_calltree(Fd, Indent, TotalAcc, {Label, Cnt, Acc, Own, Roots, Details}, MinPercent) ->
    Header =  {Label, Cnt, Acc, Own},
    MetaLabel = "Process",
    Diff = length(Label) - length(MetaLabel),
    IoList = io_lib:format("~s~s Lvl  Pct        Cnt        Acc        Own Calls   => MFA\n",
                           [MetaLabel, lists:duplicate(Diff, $\ )]),
    file:write(Fd, IoList),
    log(Fd, Label, Indent, TotalAcc, Header, Roots, MinPercent),
    NewIndent = "  " ++ Indent,
    Fun = fun({MFA, _C, _A, _O}) ->
                  [put_detail(Label, D) || D <- Details],
                  gen_calls(Fd, Label, NewIndent, TotalAcc, MFA, MinPercent)
          end,
    lists:foreach(Fun, Roots).

gen_calls(Fd, Label, Indent, TotalAcc, MFA, MinPercent) ->
    case get_detail(Label, MFA) of
        {read, {_CalledBy,  Current, _Calls}} ->
            log(Fd, Label, Indent, TotalAcc, Current, -1, MinPercent);
        {unread, {_CalledBy, Current, Calls}} ->
            log(Fd, Label, Indent, TotalAcc, Current, Calls, MinPercent),
            NewIndent = "  " ++ Indent,
            Fun = fun({NextMFA, _, _, _}) ->
                          gen_calls(Fd, Label, NewIndent, TotalAcc,
                                    NextMFA, MinPercent)
                  end,
            lists:foreach(Fun, Calls)
    end.

put_detail(Label, {_, {MFA, _, _, _}, _} = Detail) ->
    put({Label, MFA}, {unread, Detail}).

get_detail(Label, MFA) ->
    Val = get({Label, MFA}),
    case Val of
        {unread, Detail} ->
            put({Label, MFA}, {read, Detail}),
            Val;
        {read, _Detail} ->
            Val
    end.

gen_details(Fd, Total, Details) ->
    IoList = io_lib:format("Pct         Cnt        Acc        Own    MFA\n", []),
    file:write(Fd, IoList),
    do_gen_details(Fd, Total, Details).

do_gen_details(Fd, Total, [{_CalledBy, {MFA, Cnt, Acc, Own}, _Calls} | Details]) ->
    MFAStr = io_lib:format("~p", [MFA]),
    {_, Percent} = calc_percent(Acc, Own, Total),
    IoList = io_lib:format("~3.. B% ~10.3B ~10.3f ~10.3f => ~s\n",
                           [Percent, Cnt, Acc, Own, MFAStr]),
    file:write(Fd, IoList),
    do_gen_details(Fd, Total, Details);
do_gen_details(_Fd, _Total, []) ->
    ok.

log(Fd, Label, Indent, Acc, Current, Calls, MinPercent) when is_list(Calls) ->
    log(Fd, Label, Indent, Acc, Current, length(Calls), MinPercent);
log(Fd, Label, Indent, Total, {MFA, Cnt, Acc, Own}, N, MinPercent) ->
    {Max, Percent} = calc_percent(Acc, Own, Total),
    if
        Percent >= MinPercent ->
            do_log(Fd, Label, Indent, Percent, MFA, Cnt, Max, Own, N);
        true ->
            ok
    end.

do_log(Fd, Label, Indent, Percent, MFA, Cnt, Acc, Own, N) ->
    MFAStr = io_lib:format("~p", [MFA]),
    CallsStr = io_lib:format(" ~5.. s ", [lists:concat([N])]),
    IoList = io_lib:format("~s ~3.. B "
                           "~s~3.. B% ~10.. B ~10.. B ~10.. B ~s => ~s\n",
                           [Label, length(Indent) div 2,
                            Indent, Percent, Cnt,
                            round(Acc), round(Own), CallsStr, MFAStr]),
    file:write(Fd, IoList).

calc_percent(Acc, Own, Total) ->
    Max = safe_max(Acc, Own),
    {Max, round((Max * 100) / Total)}.

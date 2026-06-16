%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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

-module(list_bif_SUITE).
-include_lib("common_test/include/ct_event.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,
         groups/0, init_per_testcase/2, end_per_testcase/2]).
-export([hd_test/1,tl_test/1,t_length/1,t_list_to_pid/1,
         t_list_to_ref/1, t_list_to_ext_pidportref/1,
         t_list_to_port/1,t_list_to_float/1,t_list_to_integer/1,
         list_fusion_overlap/1, benchmarks/1]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].


all() ->
    [{group,main}, benchmarks].

groups() ->
    [{main, [],
      [hd_test, tl_test, t_length, t_list_to_pid, t_list_to_port,
       t_list_to_ref, t_list_to_ext_pidportref,
       t_list_to_float, t_list_to_integer, list_fusion_overlap]},
     {benchmarks, [{repeat,10}], [benchmarks]}].

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).

benchmarks(_Config) ->
    Iterations = 5_000_000,

    bench_nonempty_list_get_list(Iterations),
    bench_nonempty_list_get_hd(Iterations),
    bench_nonempty_list_get_tl(Iterations),
    ok.

bench_nonempty_list_get_list(Iterations) ->
    List = [1, 2, 3, 4, 5, 6, 7, 8],
    F = fun(N) -> nonempty_list_get_list_loop(N, List, 0) end,
    time(bench_nonempty_list_get_list, F, Iterations).

bench_nonempty_list_get_hd(Iterations) ->
    List = [1, 2, 3, 4, 5, 6, 7, 8],
    F = fun(N) -> nonempty_list_get_hd_loop(N, List, 0) end,
    time(bench_nonempty_list_get_hd, F, Iterations).

bench_nonempty_list_get_tl(Iterations) ->
    List = [1, 2, 3, 4, 5, 6, 7, 8],
    F = fun(N) -> nonempty_list_get_tl_loop(N, List, 0) end,
    time(bench_nonempty_list_get_tl, F, Iterations).

nonempty_list_get_list_loop(0, _List, Acc) ->
    Acc;
nonempty_list_get_list_loop(N, List, Acc) ->
    {H, _T} = nonempty_list_get_list(List),
    nonempty_list_get_list_loop(N - 1, List, Acc + H).

nonempty_list_get_hd_loop(0, _List, Acc) ->
    Acc;
nonempty_list_get_hd_loop(N, List, Acc) ->
    H = nonempty_list_get_hd(List),
    nonempty_list_get_hd_loop(N - 1, List, Acc + H).

nonempty_list_get_tl_loop(0, _List, Acc) ->
    Acc;
nonempty_list_get_tl_loop(N, List, Acc) ->
    _T = nonempty_list_get_tl(List),
    nonempty_list_get_tl_loop(N - 1, List, Acc + 1).

nonempty_list_get_list([H|T]) ->
    {H, T}.

nonempty_list_get_hd([H|_]) ->
    H.

nonempty_list_get_tl([_|T]) ->
    T.

time(Name, F, Iterations) ->
    Time = element(1, timer:tc(F, [Iterations])),
    ct_event:notify(#event{name = benchmark_data,
                           data = [{value, Time},
                                   {suite, ?MODULE_STRING},
                                   {name, atom_to_list(Name)}]}),
    ct:pal("~s: ~p us", [atom_to_list(Name), Time]),
    Time.

list_fusion_overlap(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    AsmFile = filename:absname(filename:join(DataDir, "list_fusion_overlap")),

    %% Verify the fixture behavior first, then verify the native code shape.
    {ok, Mod, Code} = compile:file(AsmFile, [from_asm,binary,report]),
    {module, Mod} = code:load_binary(Mod, "list_fusion_overlap", Code),
    ok = Mod:Mod(),
    true = code:delete(Mod),
    _ = code:purge(Mod),

    AsmDump = dump_list_fusion_overlap(AsmFile),
    case overlapping_ldp_lines(AsmDump) of
        [] ->
            ok;
        Lines ->
            ct:fail("JIT emitted overlapping ldp base/destination registers:~n~ts",
                    [join_lines(Lines)])
    end.

dump_list_fusion_overlap(AsmFile) ->
    RootDir = required_env("ROOTDIR"),
    BinDir = required_env("BINDIR"),
    Erlexec = filename:join(BinDir, "erlexec"),
    Boot = filename:join([RootDir, "bin", "start_clean"]),
    CompilerEbin = filename:join([RootDir, "lib", "compiler", "ebin"]),
    DumpDir = filename:join(
                filename:absname("."),
                "list_fusion_overlap_asm_" ++
                    integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(DumpDir),
    try
        Eval = io_lib:format(
                 "{ok,M,Code}=compile:file(~p,[from_asm,binary,report]),"
                 "{module,M}=code:load_binary(M,\"list_fusion_overlap\",Code),"
                 "ok=M:M(),halt().",
                 [AsmFile]),
        Args = ["+JDdump", "true",
                "-boot", Boot,
                "-noshell",
                "-pa", CompilerEbin,
                "-eval", lists:flatten(Eval)],
        Env = [{"ROOTDIR", RootDir},
               {"BINDIR", BinDir},
               {"EMU", "beam"},
               {"PROGNAME", "erl"}],
        case run_erlexec(Erlexec, Args, Env, DumpDir) of
            {0, _Output} ->
                DumpFile = filename:join(DumpDir, "list_fusion_overlap.asm"),
                {ok, Dump} = file:read_file(DumpFile),
                Dump;
            {Status, Output} ->
                ct:fail("JIT dump failed with status ~p:~n~ts",
                        [Status, Output])
        end
    after
        _ = file:del_dir_r(DumpDir)
    end.

required_env(Name) ->
    case os:getenv(Name) of
        false ->
            ct:fail("~s must be set to run the built VM", [Name]);
        Value ->
            Value
    end.

run_erlexec(Erlexec, Args, Env, Cwd) ->
    Port = open_port({spawn_executable, Erlexec},
                     [exit_status, stderr_to_stdout, binary,
                      {args, Args}, {env, Env}, {cd, Cwd}]),
    collect_port(Port, []).

collect_port(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port(Port, [Data | Acc]);
        {Port, {exit_status, Status}} ->
            {Status, iolist_to_binary(lists:reverse(Acc))}
    end.

overlapping_ldp_lines(AsmDump) ->
    [Line || Line <- binary:split(AsmDump, <<"\n">>, [global]),
             overlapping_ldp(Line)].

overlapping_ldp(Line) ->
    case re:run(Line,
                "^\\s*ldp\\s+(x[0-9]+),\\s*(x[0-9]+),\\s*\\[(x[0-9]+)(?:[,\\]])",
                [{capture, all_but_first, binary}]) of
        {match, [Dst1, Dst2, Base]} ->
            Dst1 =:= Base orelse Dst2 =:= Base;
        nomatch ->
            false
    end.

join_lines(Lines) ->
    unicode:characters_to_list(
      iolist_to_binary([[Line, $\n] || Line <- Lines])).

%% Tests list_to_integer and string:to_integer
t_list_to_integer(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch list_to_integer("12373281903728109372810937209817320981321ABC")),
    12373281903728109372810937209817320981321 = (catch list_to_integer("12373281903728109372810937209817320981321")),
    12373 = (catch list_to_integer("12373")),
    -12373 =  (catch list_to_integer("-12373")),
    12373 = (catch list_to_integer("+12373")),
    {'EXIT',{badarg,_}} = (catch list_to_integer(abc)),
    {'EXIT',{badarg,_}} = (catch list_to_integer("")),
    {12373281903728109372810937209817320981321,"ABC"} = string:to_integer("12373281903728109372810937209817320981321ABC"),
    {-12373281903728109372810937209817320981321,"ABC"} = string:to_integer("-12373281903728109372810937209817320981321ABC"),
    {12,[345]} = string:to_integer([$1,$2,345]),
    {error,badarg} = string:to_integer([$1,$2,a]),
    {error,no_integer} = string:to_integer([$A]),
    {error,badarg} = string:to_integer($A),

    %% System limit.
    Digits = lists:duplicate(3_000_000, $9),
    {'EXIT',{system_limit,_}} = catch list_to_integer(Digits),
    _ = erlang:garbage_collect(),
    {'EXIT',{system_limit,_}} = catch list_to_integer(Digits, 16),
    _ = erlang:garbage_collect(),
    {error,system_limit} = string:to_integer(Digits),
    _ = erlang:garbage_collect(),

    ok.

%% Test hd/1 with correct and incorrect arguments.
hd_test(Config) when is_list(Config) ->
    $h = hd(id("hejsan")),
    case catch hd(id($h)) of
        {'EXIT', {badarg, _}} -> ok;
        Res ->
            ct:fail("hd/1 with incorrect args succeeded.~nResult: ~p", [Res])
    end,
    ok.


%% Test tl/1 with correct and incorrect arguments.
tl_test(Config) when is_list(Config) ->
    "ejsan" = tl(id("hejsan")),
    case catch tl(id(104)) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("tl/1 with incorrect args succeeded.~nResult: ~p", [Res])
    end,
    ok.


%% Test length/1 with correct and incorrect arguments.

t_length(Config) when is_list(Config) ->
    0 = length(""),
    0 = length([]),
    1 = length([1]),
    2 = length([1,a]),
    2 = length("ab"),
    3 = length("abc"),
    4 = length(id([x|"abc"])),
    6 = length("hejsan"),
    {'EXIT',{badarg,_}} = (catch length(id([a,b|c]))),
    case catch length({tuple}) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("length/1 with incorrect args succeeded.~nResult: ~p", [Res])
    end,
    ok.
	      

%% Test list_to_pid/1 with correct and incorrect arguments.

t_list_to_pid(Config) when is_list(Config) ->
    Me = self(),
    MyListedPid = pid_to_list(Me),
    Me = list_to_pid(MyListedPid),
    case catch list_to_pid(id("Incorrect list")) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("list_to_pid/1 with incorrect arg succeeded.~n"
                    "Result: ~p", [Res])
    end,
    ok.

%% Test list_to_port/1 with correct and incorrect arguments.

t_list_to_port(Config) when is_list(Config) ->
    Me = hd(erlang:ports()),
    MyListedPid = port_to_list(Me),
    Me = list_to_port(MyListedPid),
    case catch list_to_port(id("Incorrect list")) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("list_to_port/1 with incorrect arg succeeded.~n"
                    "Result: ~p", [Res])
    end,
    ok.

t_list_to_ref(Config) when is_list(Config) ->
    Ref = make_ref(),
    RefStr = ref_to_list(Ref),
    Ref = list_to_ref(RefStr),
    case catch list_to_ref(id("Incorrect list")) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("list_to_ref/1 with incorrect arg succeeded.~n"
                    "Result: ~p", [Res])
    end,
    ok.

%% Test list_to_pid/port/ref for external pids/ports/refs.
t_list_to_ext_pidportref(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    Pid = rpc:call(Node, erlang, self, []),
    Port = hd(rpc:call(Node, erlang, ports, [])),
    Ref = rpc:call(Node, erlang, make_ref, []),

    PidStr  = pid_to_list(Pid),
    PortStr = port_to_list(Port),
    RefStr  = ref_to_list(Ref),

    Pid2  = list_to_pid(PidStr),
    Port2 = list_to_port(PortStr),
    Ref2  = list_to_ref(RefStr),

    %% Local roundtrips of externals work from OTP-23
    %% as even though 'creation' is missing in the string formats
    %% we know the 'creation' of the connected node and list_to_* use that.
    true = (Pid =:= Pid2),
    true = (Port =:= Port2),
    true = (Ref =:= Ref2),
    true = (Pid == Pid2),
    true = (Port == Port2),
    true = (Ref == Ref2),

    %% And it works when sent back to the same node instance,
    %% which was connected when list_to_* were called.
    true = rpc:call(Node, erlang, '=:=', [Pid, Pid2]),
    true = rpc:call(Node, erlang, '==',  [Pid, Pid2]),
    true = rpc:call(Node, erlang, '=:=', [Port, Port2]),
    true = rpc:call(Node, erlang, '==',  [Port, Port2]),
    true = rpc:call(Node, erlang, '=:=', [Ref, Ref2]),
    true = rpc:call(Node, erlang, '==',  [Ref, Ref2]),


    peer:stop(Peer),
    ok.

-define(NEW_PID_EXT, 88).
-define(NEW_PORT_EXT, 89).
-define(NEWER_REFERENCE_EXT, 90).

%% Copy pid/port/ref but set creation=0
make_0_creation(X) when is_pid(X); is_port(X); is_reference(X) ->
    B = term_to_binary(X),
    Sz = byte_size(B),
    B2 = case B of
             <<131, ?NEW_PID_EXT, _/binary>> ->
                 PreSz = Sz - 4,
                 <<_:PreSz/binary, Cr:32>> = B,
                 true = (Cr =/= 0),
                 <<B:PreSz/binary, 0:32>>;
             <<131, ?NEW_PORT_EXT, _/binary>> ->
                 PreSz = Sz - 4,
                 <<_:PreSz/binary, Cr:32>> = B,
                 true = (Cr =/= 0),
                 <<B:PreSz/binary, 0:32>>;
             <<131, ?NEWER_REFERENCE_EXT, Len:16, _/binary>> ->
                 PostSz = Len*4,
                 PreSz = Sz - (4 + PostSz),
                 <<_:PreSz/binary, Cr:32, PostFix:PostSz/binary>> = B,
                 true = (Cr =/= 0),
                 <<B:PreSz/binary, 0:32, PostFix/binary>>
         end,
    binary_to_term(B2).


%% Test list_to_float/1 with correct and incorrect arguments.

t_list_to_float(Config) when is_list(Config) ->
    5.89000 = list_to_float(id("5.89")),
    5.89898 = list_to_float(id("5.89898")),
    case catch list_to_float(id("58")) of
        {'EXIT', {badarg, _}} -> ok;
        Res ->
            ct:fail("list_to_float with incorrect arg succeeded.~nResult: ~p", [Res])
    end,
    ok.

id(I) -> I.

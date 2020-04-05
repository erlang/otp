%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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
-module(stdlib_bench_SUITE).
-compile([export_all, nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,2}]}]}].


all() ->
    [{group,unicode},{group,base64},{group,binary},
     {group,gen_server},{group,gen_statem},
     {group,gen_server_comparison},{group,gen_statem_comparison}].

groups() ->
    [{unicode,[{repeat,5}],
      [norm_nfc_list, norm_nfc_deep_l, norm_nfc_binary,
       string_lexemes_list, string_lexemes_binary
      ]},
     %% Only run 1 binary match repeat as it is very slow pre OTP-22.
     %% The results seem to be stable enough anyway
     {binary, [{repeat, 1}],
      [match_single_pattern_no_match,
       matches_single_pattern_no_match,
       matches_single_pattern_eventual_match,
       matches_single_pattern_frequent_match]},
     {base64,[{repeat,5}],
      [decode_binary, decode_binary_to_string,
       decode_list, decode_list_to_string,
       encode_binary, encode_binary_to_string,
       encode_list, encode_list_to_string,
       mime_binary_decode, mime_binary_decode_to_string,
       mime_list_decode, mime_list_decode_to_string]},
     {gen_server, [{repeat,5}], cases(gen_server)},
     {gen_statem, [{repeat,3}], cases(gen_statem)},
     {gen_server_comparison, [],
      [single_small, single_medium, single_big,
       sched_small, sched_medium, sched_big,
       multi_small, multi_medium, multi_big]},
     {gen_statem_comparison, [],
      [single_small, single_big,
       sched_small, sched_big,
       multi_small, multi_big]}].

cases(gen_server) ->
      [simple, simple_timer, simple_mon, simple_timer_mon,
       generic, generic_timer];
cases(gen_statem) ->
    [generic, generic_log, generic_log100, generic_fsm, generic_fsm_transit,
     generic_statem, generic_statem_log, generic_statem_log100,
     generic_statem_transit, generic_statem_complex].

init_per_group(gen_server, Config) ->
    compile_servers(Config),
    [{benchmark_suite,"stdlib_gen_server"}|Config];
init_per_group(gen_statem, Config) ->
    compile_servers(Config),
    [{benchmark_suite,"stdlib_gen_statem"}|Config];
init_per_group(gen_server_comparison, Config) ->
    compile_servers(Config),
    [{cases,cases(gen_server)},
     {benchmark_suite,"stdlib_gen_server"}|Config];
init_per_group(gen_statem_comparison, Config) ->
    compile_servers(Config),
    [{cases,cases(gen_statem)},
     {benchmark_suite,"stdlib_gen_statem"}|Config];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_Func, Conf) ->
    Conf.

end_per_testcase(_Func, _Conf) ->
    ok.


compile_servers(Config) ->
    DataDir = ?config(data_dir, Config),
    Files = filelib:wildcard(filename:join(DataDir, "{simple,generic}*.erl")),
    _ = [{ok, _} = compile:file(File) || File <- Files],
    ok.

comment(Value) ->
    C = lists:flatten(io_lib:format("~p", [Value])),
    {comment, C}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(REPEAT_NORM, 5).

norm_nfc_list(Config) ->
    Bin = norm_data(Config),
    {_N, Mean, _Stddev, Res} = unicode_util_SUITE:time_count(nfc, list, Bin, ?REPEAT_NORM),
    comment(report(1000.0*Res / Mean)).

norm_nfc_deep_l(Config) ->
    Bin = norm_data(Config),
    {_N, Mean, _Stddev, Res} = unicode_util_SUITE:time_count(nfc, deep_l, Bin, ?REPEAT_NORM),
    comment(report(1000.0*Res / Mean)).

norm_nfc_binary(Config) ->
    Bin = norm_data(Config),
    {_N, Mean, _Stddev, Res} = unicode_util_SUITE:time_count(nfc, binary, Bin, ?REPEAT_NORM),
    comment(report(1000.0*Res / Mean)).


string_lexemes_list(Config) ->
    %% Use nth_lexeme instead of lexemes to avoid building a result of
    %% large lists which causes large differences between test runs, gc?
    Bin = norm_data(Config),
    Fun = fun(Str) -> string:nth_lexeme(Str, 200000, [$;,$\n,$\r]), 200000 end,
    {_N, Mean, _Stddev, Res} = string_SUITE:time_func(Fun, list, Bin, 15),
    comment(report(1000.0*Res / Mean)).

string_lexemes_binary(Config) ->
    %% Use nth_lexeme instead of lexemes to avoid building a result of
    %% large lists which causes large differences between test runs, gc?
    Bin = norm_data(Config),
    Fun = fun(Str) -> string:nth_lexeme(Str, 200000, [$;,$\n,$\r]), 200000 end,
    {_N, Mean, _Stddev, Res} = string_SUITE:time_func(Fun, binary, Bin, ?REPEAT_NORM),
    comment(report(1000.0*Res / Mean)).

%%%
report(Tps) ->
    ct_event:notify(#event{name = benchmark_data,
                           data = [{suite,"stdlib_unicode"},{value,round(Tps)}]}),
    Tps.

norm_data(Config) ->
    DataDir0 = proplists:get_value(data_dir, Config),
    DataDir = filename:join(lists:droplast(filename:split(DataDir0))),
    File = filename:join([DataDir,"unicode_util_SUITE_data","NormalizationTest.txt"]),
    {ok, Bin} = file:read_file(File),
    Bin.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match_single_pattern_no_match(_Config) ->
    Binary = binary:copy(<<"ugbcfuysabfuqyfikgfsdalpaskfhgjsdgfjwsalp">>, 1000000),
    comment(test(100, binary, match, [Binary, <<"o">>])).

matches_single_pattern_no_match(_Config) ->
    Binary = binary:copy(<<"ugbcfuysabfuqyfikgfsdalpaskfhgjsdgfjwsalp">>, 1000000),
    comment(test(100, binary, matches, [Binary, <<"o">>])).

matches_single_pattern_eventual_match(_Config) ->
    Binary = binary:copy(<<"ugbcfuysabfuqyfikgfsdalpaskfhgjsdgfjwsal\n">>, 1000000),
    comment(test(100, binary, matches, [Binary, <<"\n">>])).

matches_single_pattern_frequent_match(_Config) ->
    Binary = binary:copy(<<"abc\n">>, 1000000),
    comment(test(100, binary, matches, [Binary, <<"abc">>])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_binary(_Config) ->
    comment(test(base64, decode, [encoded_binary()])).

decode_binary_to_string(_Config) ->
    comment(test(base64, decode_to_string, [encoded_binary()])).

decode_list(_Config) ->
    comment(test(base64, decode, [encoded_list()])).

decode_list_to_string(_Config) ->
    comment(test(base64, decode_to_string, [encoded_list()])).

encode_binary(_Config) ->
    comment(test(base64, encode, [binary()])).

encode_binary_to_string(_Config) ->
    comment(test(base64, encode_to_string, [binary()])).

encode_list(_Config) ->
    comment(test(base64, encode, [list()])).

encode_list_to_string(_Config) ->
    comment(test(base64, encode_to_string, [list()])).

mime_binary_decode(_Config) ->
    comment(test(base64, mime_decode, [encoded_binary()])).

mime_binary_decode_to_string(_Config) ->
    comment(test(base64, mime_decode_to_string, [encoded_binary()])).

mime_list_decode(_Config) ->
    comment(test(base64, mime_decode, [encoded_list()])).

mime_list_decode_to_string(_Config) ->
    comment(test(base64, mime_decode_to_string, [encoded_list()])).

-define(SIZE, 10000).
-define(N, 1000).

encoded_binary() ->
    list_to_binary(encoded_list()).

encoded_list() ->
    L = random_byte_list(round(?SIZE*0.75)),
    base64:encode_to_string(L).

binary() ->
    list_to_binary(list()).

list() ->
    random_byte_list(?SIZE).

test(Mod, Fun, Args) ->
    test(?N, Mod, Fun, Args).
test(Iter, Mod, Fun, Args) ->
    F = fun() -> loop(Iter, Mod, Fun, Args) end,
    {Time, ok} = timer:tc(fun() -> lspawn(F) end),
    report_mfa(Iter, Time, Mod).

loop(0, _M, _F, _A) -> garbage_collect(), ok;
loop(N, M, F, A) ->
    _ = apply(M, F, A),
    loop(N - 1, M, F, A).

lspawn(Fun) ->
    {Pid, Ref} = spawn_monitor(fun() -> exit(Fun()) end),
    receive
        {'DOWN', Ref, process, Pid, Rep} -> Rep
    end.

report_mfa(Iter, Time, Mod) ->
    Tps = round((Iter*1000000)/Time),
    ct_event:notify(#event{name = benchmark_data,
                           data = [{suite, "stdlib_" ++ atom_to_list(Mod)},
                                   {value, Tps}]}),
    Tps.

%% Copied from base64_SUITE.erl.

random_byte_list(N) ->
    random_byte_list(N, []).

random_byte_list(0, Acc) ->
    Acc;
random_byte_list(N, Acc) ->
    random_byte_list(N-1, [rand:uniform(255)|Acc]).

make_big_binary(N) ->
    list_to_binary(mbb(N, [])).

mbb(N, Acc) when N > 256 ->
    B = list_to_binary(lists:seq(0, 255)),
    mbb(N - 256, [B | Acc]);
mbb(N, Acc) ->
    B = list_to_binary(lists:seq(0, N-1)),
    lists:reverse(Acc, B).

simple(Config) when is_list(Config) ->
    comment(do_tests(simple, single_small, Config)).

simple_timer(Config) when is_list(Config) ->
    comment(do_tests(simple_timer, single_small, Config)).

simple_mon(Config) when is_list(Config) ->
    comment(do_tests(simple_mon, single_small, Config)).

simple_timer_mon(Config) when is_list(Config) ->
    comment(do_tests(simple_timer_mon, single_small, Config)).

generic(Config) when is_list(Config) ->
    comment(do_tests(generic, single_small, Config)).

generic_log(Config) when is_list(Config) ->
    comment(do_tests(generic_log, single_small, Config)).

generic_log100(Config) when is_list(Config) ->
    comment(do_tests(generic_log100, single_small, Config)).

generic_timer(Config) when is_list(Config) ->
    comment(do_tests(generic_timer, single_small, Config)).

generic_statem(Config) when is_list(Config) ->
    comment(do_tests(generic_statem, single_small, Config)).

generic_statem_log(Config) when is_list(Config) ->
    comment(do_tests(generic_statem_log, single_small, Config)).

generic_statem_log100(Config) when is_list(Config) ->
    comment(do_tests(generic_statem_log100, single_small, Config)).

generic_statem_transit(Config) when is_list(Config) ->
    comment(do_tests(generic_statem_transit, single_small, Config)).

generic_statem_complex(Config) when is_list(Config) ->
    comment(do_tests(generic_statem_complex, single_small, Config)).

generic_fsm(Config) when is_list(Config) ->
    comment(do_tests(generic_fsm, single_small, Config)).

generic_fsm_transit(Config) when is_list(Config) ->
    comment(do_tests(generic_fsm_transit, single_small, Config)).

single_small(Config) when is_list(Config) ->
    comparison(?config(cases, Config), single_small, Config).

single_medium(Config) when is_list(Config) ->
    comparison(?config(cases, Config), single_medium, Config).

single_big(Config) when is_list(Config) ->
    comparison(?config(cases, Config), single_big, Config).

sched_small(Config) when is_list(Config) ->
    comparison(?config(cases, Config), sched_small, Config).

sched_medium(Config) when is_list(Config) ->
    comparison(?config(cases, Config), sched_medium, Config).

sched_big(Config) when is_list(Config) ->
    comparison(?config(cases, Config), sched_big, Config).

multi_small(Config) when is_list(Config) ->
    comparison(?config(cases, Config), multi_small, Config).

multi_medium(Config) when is_list(Config) ->
    comparison(?config(cases, Config), multi_medium, Config).

multi_big(Config) when is_list(Config) ->
    comparison(?config(cases, Config), multi_big, Config).

comparison(Cases, Kind, Config) ->
    Cases = ?config(cases, Config),
    [RefResult|_] = Results =
        [do_tests(Case, Kind, Config) || Case <- Cases],
    Normalized = [norm(Result, RefResult) || Result <- Results],
    {Parallelism, Message} = bench_params(Kind),
    Wordsize = erlang:system_info(wordsize),
    MSize = Wordsize * erts_debug:flat_size(Message),
    What = io_lib:format("#parallel gen_server instances: ~.4w, "
                         "message flat size: ~.5w bytes",
                         [Parallelism, MSize]),
    Format =
        lists:flatten(
          ["~s: "] ++
              [[atom_to_list(Case),": ~s "] || Case <- Cases]),
    C = lists:flatten(io_lib:format(Format, [What] ++ Normalized)),
    {comment, C}.

norm(T, Ref) ->
    try Ref / T of
        Norm ->
            io_lib:format("~.2f", [Norm])
    catch error:badarith ->
            "---"
    end.

-define(MAX_TIME_SECS, 3).   % s
-define(MAX_TIME, 1000 * ?MAX_TIME_SECS). % ms
-define(CALLS_PER_LOOP, 5).

do_tests(Test, ParamSet, Config) ->
    BenchmarkSuite = ?config(benchmark_suite, Config),
    {Client, ServerMod, ServerArg} = bench(Test),
    {Parallelism, Message} = bench_params(ParamSet),
    Fun = create_clients(Message, ServerMod, ServerArg, Client, Parallelism),
    {TotalLoops, AllPidTime} = run_test(Fun),
    try ?CALLS_PER_LOOP * round((1000 * TotalLoops) / AllPidTime) of
        PerSecond ->
            ct_event:notify(
              #event{
                 name = benchmark_data,
                 data = [{suite,BenchmarkSuite},{value,PerSecond}]}),
            PerSecond
    catch error:badarith ->
            "Time measurement is not working"
    end.

-define(COUNTER, n).

simple_client(N, M, P) ->
    put(?COUNTER, N),
    _ = simple_server:reply(P, M),
    _ = simple_server:reply(P, M),
    _ = simple_server:reply(P, M),
    _ = simple_server:reply(P, M),
    _ = simple_server:reply(P, M),
    simple_client(N+1, M, P).

simple_client_timer(N, M, P) ->
    put(?COUNTER, N),
    _ = simple_server_timer:reply(P, M),
    _ = simple_server_timer:reply(P, M),
    _ = simple_server_timer:reply(P, M),
    _ = simple_server_timer:reply(P, M),
    _ = simple_server_timer:reply(P, M),
    simple_client_timer(N+1, M, P).

simple_client_mon(N, M, P) ->
    put(?COUNTER, N),
    _ = simple_server_mon:reply(P, M),
    _ = simple_server_mon:reply(P, M),
    _ = simple_server_mon:reply(P, M),
    _ = simple_server_mon:reply(P, M),
    _ = simple_server_mon:reply(P, M),
    simple_client_mon(N+1, M, P).

simple_client_timer_mon(N, M, P) ->
    put(?COUNTER, N),
    _ = simple_server_timer_mon:reply(P, M),
    _ = simple_server_timer_mon:reply(P, M),
    _ = simple_server_timer_mon:reply(P, M),
    _ = simple_server_timer_mon:reply(P, M),
    _ = simple_server_timer_mon:reply(P, M),
    simple_client_timer_mon(N+1, M, P).

generic_client(N, M, P) ->
    put(?COUNTER, N),
    _ = generic_server:reply(P, M),
    _ = generic_server:reply(P, M),
    _ = generic_server:reply(P, M),
    _ = generic_server:reply(P, M),
    _ = generic_server:reply(P, M),
    generic_client(N+1, M, P).

generic_timer_client(N, M, P) ->
    put(?COUNTER, N),
    _ = generic_server_timer:reply(P, M),
    _ = generic_server_timer:reply(P, M),
    _ = generic_server_timer:reply(P, M),
    _ = generic_server_timer:reply(P, M),
    _ = generic_server_timer:reply(P, M),
    generic_timer_client(N+1, M, P).

generic_statem_client(N, M, P) ->
    put(?COUNTER, N),
    _ = generic_statem:reply(P, M),
    _ = generic_statem:reply(P, M),
    _ = generic_statem:reply(P, M),
    _ = generic_statem:reply(P, M),
    _ = generic_statem:reply(P, M),
    generic_statem_client(N+1, M, P).

generic_statem_transit_client(N, M, P) ->
    put(?COUNTER, N),
    _ = generic_statem:transit(P, M),
    _ = generic_statem:transit(P, M),
    _ = generic_statem:transit(P, M),
    _ = generic_statem:transit(P, M),
    _ = generic_statem:transit(P, M),
    generic_statem_transit_client(N+1, M, P).

generic_statem_complex_client(N, M, P) ->
    put(?COUNTER, N),
    _ = generic_statem:reply(P, M),
    _ = generic_statem:reply(P, M),
    _ = generic_statem:reply(P, M),
    _ = generic_statem:reply(P, M),
    _ = generic_statem:reply(P, M),
    generic_statem_complex_client(N+1, M, P).

generic_fsm_client(N, M, P) ->
    put(?COUNTER, N),
    _ = generic_fsm:reply(P, M),
    _ = generic_fsm:reply(P, M),
    _ = generic_fsm:reply(P, M),
    _ = generic_fsm:reply(P, M),
    _ = generic_fsm:reply(P, M),
    generic_fsm_client(N+1, M, P).

generic_fsm_transit_client(N, M, P) ->
    put(?COUNTER, N),
    _ = generic_fsm:transit(P, M),
    _ = generic_fsm:transit(P, M),
    _ = generic_fsm:transit(P, M),
    _ = generic_fsm:transit(P, M),
    _ = generic_fsm:transit(P, M),
    generic_fsm_transit_client(N+1, M, P).

bench(simple) ->
    {fun simple_client/3, simple_server, term};
bench(simple_timer) ->
    {fun simple_client_timer/3, simple_server_timer, term};
bench(simple_mon) ->
    {fun simple_client_mon/3, simple_server_mon, term};
bench(simple_timer_mon) ->
    {fun simple_client_timer_mon/3, simple_server_timer_mon, term};
bench(generic) ->
    {fun generic_client/3, generic_server, [term]};
bench(generic_log) ->
    {fun generic_client/3, generic_server, [term,{debug,[log]}]};
bench(generic_log100) ->
    {fun generic_client/3, generic_server, [term,{debug,[{log,100}]}]};
bench(generic_timer) ->
    {fun generic_timer_client/3, generic_server_timer, term};
bench(generic_statem) ->
    {fun generic_statem_client/3, generic_statem, [term]};
bench(generic_statem_log) ->
    {fun generic_statem_client/3, generic_statem, [term,{debug,[log]}]};
bench(generic_statem_log100) ->
    {fun generic_statem_client/3, generic_statem, [term,{debug,[{log,100}]}]};
bench(generic_statem_transit) ->
    {fun generic_statem_transit_client/3, generic_statem, [term]};
bench(generic_statem_complex) ->
    {fun generic_statem_complex_client/3, generic_statem_complex, term};
bench(generic_fsm) ->
    {fun generic_fsm_client/3, generic_fsm, term};
bench(generic_fsm_transit) ->
    {fun generic_fsm_transit_client/3, generic_fsm, term}.

%% -> {Parallelism, MessageTerm}
bench_params(single_small) -> {1, small()};
bench_params(single_medium) -> {1, medium()};
bench_params(single_big) -> {1, big()};
bench_params(sched_small)  -> {parallelism(), small()};
bench_params(sched_medium)  -> {parallelism(), medium()};
bench_params(sched_big)  -> {parallelism(), big()};
bench_params(multi_small)  -> {400, small()};
bench_params(multi_medium)  -> {400, medium()};
bench_params(multi_big)  -> {400, big()}.

small() ->
    small.

medium() ->
    lists:seq(1, 50).

big() ->
    lists:seq(1, 1000).

parallelism() ->
    case erlang:system_info(multi_scheduling) of
        enabled -> erlang:system_info(schedulers_online);
        _ -> 1
    end.

create_clients(M, ServerMod, ServerArg, Client, Parallel) ->
    fun() ->
            ServerPid = ServerMod:start(ServerArg),
            PidRefs = [spawn_monitor(fun() -> Client(0, M, ServerPid) end) ||
                          _ <- lists:seq(1, Parallel)],
            timer:sleep(?MAX_TIME),
            try
                AllPidsN = collect(PidRefs, []),
                TotalLoops = lists:sum(AllPidsN),
                TotalLoops
            after
                ok = ServerMod:stop(ServerPid)
            end
    end.

collect([], Result) ->
    Result;
collect([{Pid, Ref}|PidRefs], Result) ->
    N = case erlang:process_info(Pid, dictionary) of
            {dictionary, Dict} ->
                {?COUNTER, N0} = lists:keyfind(?COUNTER, 1, Dict),
                N0;
            undefined -> % Process did not start in ?MAX_TIME_SECS.
                0
        end,
    exit(Pid, kill),
    receive {'DOWN', Ref, _, _, _} -> ok end,
    collect(PidRefs, [N|Result]).

run_test(Test) ->
    {T1, _} = statistics(runtime),
    Result = Test(),
    {T2, _} = statistics(runtime),
    {Result, T2 - T1}.

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
    [{group,unicode},{group,base64},
     {group,gen_server},{group,gen_server_comparison}].

groups() ->
    [{unicode,[{repeat,5}],
      [norm_nfc_list, norm_nfc_deep_l, norm_nfc_binary,
       string_lexemes_list, string_lexemes_binary
      ]},
     {base64,[{repeat,5}],
      [decode_binary, decode_binary_to_string,
       decode_list, decode_list_to_string,
       encode_binary, encode_binary_to_string,
       encode_list, encode_list_to_string,
       mime_binary_decode, mime_binary_decode_to_string,
       mime_list_decode, mime_list_decode_to_string]},
     {gen_server, [{repeat,5}],
      [simple, simple_timer, simple_mon, simple_timer_mon,
       generic, generic_timer]},
     {gen_server_comparison, [],
      [single_small, single_medium, single_big,
       sched_small, sched_medium, sched_big,
       multi_small, multi_medium, multi_big]}].

init_per_group(GroupName, Config) when GroupName =:= gen_server;
                                       GroupName =:= gen_server_comparison ->
    DataDir = ?config(data_dir, Config),
    Files = filelib:wildcard(filename:join(DataDir, "{simple,generic}*.erl")),
    _ = [{ok, _} = compile:file(File) || File <- Files],
    Config;
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(REPEAT_NORM, 5).

norm_nfc_list(Config) ->
    Bin = norm_data(Config),
    {_N, Mean, _Stddev, Res} = unicode_util_SUITE:time_count(nfc, list, Bin, ?REPEAT_NORM),
    report(1000.0*Res / Mean).

norm_nfc_deep_l(Config) ->
    Bin = norm_data(Config),
    {_N, Mean, _Stddev, Res} = unicode_util_SUITE:time_count(nfc, deep_l, Bin, ?REPEAT_NORM),
    report(1000.0*Res / Mean).

norm_nfc_binary(Config) ->
    Bin = norm_data(Config),
    {_N, Mean, _Stddev, Res} = unicode_util_SUITE:time_count(nfc, binary, Bin, ?REPEAT_NORM),
    report(1000.0*Res / Mean).


string_lexemes_list(Config) ->
    %% Use nth_lexeme instead of lexemes to avoid building a result of
    %% large lists which causes large differences between test runs, gc?
    Bin = norm_data(Config),
    Fun = fun(Str) -> string:nth_lexeme(Str, 200000, [$;,$\n,$\r]), 200000 end,
    {_N, Mean, _Stddev, Res} = string_SUITE:time_func(Fun, list, Bin, 15),
    report(1000.0*Res / Mean).

string_lexemes_binary(Config) ->
    %% Use nth_lexeme instead of lexemes to avoid building a result of
    %% large lists which causes large differences between test runs, gc?
    Bin = norm_data(Config),
    Fun = fun(Str) -> string:nth_lexeme(Str, 200000, [$;,$\n,$\r]), 200000 end,
    {_N, Mean, _Stddev, Res} = string_SUITE:time_func(Fun, binary, Bin, ?REPEAT_NORM),
    report(1000.0*Res / Mean).

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

decode_binary(_Config) ->
    test(decode, encoded_binary()).

decode_binary_to_string(_Config) ->
    test(decode_to_string, encoded_binary()).

decode_list(_Config) ->
    test(decode, encoded_list()).

decode_list_to_string(_Config) ->
    test(decode_to_string, encoded_list()).

encode_binary(_Config) ->
    test(encode, binary()).

encode_binary_to_string(_Config) ->
    test(encode_to_string, binary()).

encode_list(_Config) ->
    test(encode, list()).

encode_list_to_string(_Config) ->
    test(encode_to_string, list()).

mime_binary_decode(_Config) ->
    test(mime_decode, encoded_binary()).

mime_binary_decode_to_string(_Config) ->
    test(mime_decode_to_string, encoded_binary()).

mime_list_decode(_Config) ->
    test(mime_decode, encoded_list()).

mime_list_decode_to_string(_Config) ->
    test(mime_decode_to_string, encoded_list()).

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

test(Func, Data) ->
    F = fun() -> loop(?N, Func, Data) end,
    {Time, ok} = timer:tc(fun() -> lspawn(F) end),
    report_base64(Time).

loop(0, _F, _D) -> garbage_collect(), ok;
loop(N, F, D) ->
    _ = base64:F(D),
    loop(N - 1, F, D).

lspawn(Fun) ->
    {Pid, Ref} = spawn_monitor(fun() -> exit(Fun()) end),
    receive
        {'DOWN', Ref, process, Pid, Rep} -> Rep
    end.

report_base64(Time) ->
    Tps = round((?N*1000000)/Time),
    ct_event:notify(#event{name = benchmark_data,
                           data = [{suite, "stdlib_base64"},
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
    do_tests(simple, single_small).

simple_timer(Config) when is_list(Config) ->
    do_tests(simple_timer, single_small).

simple_mon(Config) when is_list(Config) ->
    do_tests(simple_mon, single_small).

simple_timer_mon(Config) when is_list(Config) ->
    do_tests(simple_timer_mon, single_small).

generic(Config) when is_list(Config) ->
    do_tests(generic, single_small).

generic_timer(Config) when is_list(Config) ->
    do_tests(generic_timer, single_small).

single_small(Config) when is_list(Config) ->
    comparison(single_small).

single_medium(Config) when is_list(Config) ->
    comparison(single_medium).

single_big(Config) when is_list(Config) ->
    comparison(single_big).

sched_small(Config) when is_list(Config) ->
    comparison(sched_small).

sched_medium(Config) when is_list(Config) ->
    comparison(sched_medium).

sched_big(Config) when is_list(Config) ->
    comparison(sched_big).

multi_small(Config) when is_list(Config) ->
    comparison(multi_small).

multi_medium(Config) when is_list(Config) ->
    comparison(multi_medium).

multi_big(Config) when is_list(Config) ->
    comparison(multi_big).

comparison(Kind) ->
    Simple0 = do_tests(simple, Kind),
    SimpleTimer0 = do_tests(simple_timer, Kind),
    SimpleMon0 = do_tests(simple_mon, Kind),
    SimpleTimerMon0 = do_tests(simple_timer_mon, Kind),
    Generic0 = do_tests(generic, Kind),
    GenericTimer0 = do_tests(generic_timer, Kind),
    %% Normalize
    Simple = norm(Simple0, Simple0),
    SimpleTimer = norm(SimpleTimer0, Simple0),
    SimpleMon = norm(SimpleMon0, Simple0),
    SimpleTimerMon = norm(SimpleTimerMon0, Simple0),
    Generic = norm(Generic0, Simple0),
    GenericTimer = norm(GenericTimer0, Simple0),
    {Parallelism, Message} = bench_params(Kind),
    Wordsize = erlang:system_info(wordsize),
    MSize = Wordsize * erts_debug:flat_size(Message),
    What = io_lib:format("#parallel gen_server instances: ~.4w, "
                         "message flat size: ~.5w bytes",
                         [Parallelism, MSize]),
    C = io_lib:format("~s: "
                      "Simple: ~s Simple+Timer: ~s "
                      "Simple+Monitor: ~s Simple+Timer+Monitor: ~s "
                      "Generic: ~s Generic+Timer: ~s",
                     [What, Simple, SimpleTimer, SimpleMon, SimpleTimerMon,
                     Generic, GenericTimer]),
    {comment, C}.

norm(T, Ref) ->
    io_lib:format("~.2f", [Ref/T]).

-define(MAX_TIME_SECS, 3).   % s
-define(MAX_TIME, 1000 * ?MAX_TIME_SECS). % ms
-define(CALLS_PER_LOOP, 5).

do_tests(Test, ParamSet) ->
    {Client, ServerMod} = bench(Test),
    {Parallelism, Message} = bench_params(ParamSet),
    Fun = create_clients(Message, ServerMod, Client, Parallelism),
    {TotalLoops, AllPidTime} = run_test(Fun),
    PerSecond = ?CALLS_PER_LOOP * round((1000 * TotalLoops) / AllPidTime),
    ct_event:notify(#event{name = benchmark_data,
                           data = [{suite,"stdlib_gen_server"},
                                   {value,PerSecond}]}),
    PerSecond.

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

bench(simple) ->
    {fun simple_client/3, simple_server};
bench(simple_timer) ->
    {fun simple_client_timer/3, simple_server_timer};
bench(simple_mon) ->
    {fun simple_client_mon/3, simple_server_mon};
bench(simple_timer_mon) ->
    {fun simple_client_timer_mon/3, simple_server_timer_mon};
bench(generic) ->
    {fun generic_client/3, generic_server};
bench(generic_timer) ->
    {fun generic_timer_client/3, generic_server_timer}.

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

create_clients(M, ServerMod, Client, Parallel) ->
    fun() ->
            State = term,
            ServerPid = ServerMod:start(State),
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

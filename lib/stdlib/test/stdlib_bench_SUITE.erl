%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2017. All Rights Reserved.
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
-include_lib("common_test/include/ct_event.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,2}]}]}].


all() ->
    [{group,unicode}, {group,base64}].

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
       mime_list_decode, mime_list_decode_to_string]}].

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

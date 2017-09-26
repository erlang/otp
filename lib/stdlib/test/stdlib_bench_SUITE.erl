%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
    [{group,unicode}].

groups() ->
    [{unicode,[{repeat,5}],
      [norm_nfc_list, norm_nfc_deep_l, norm_nfc_binary,
       string_lexemes_list, string_lexemes_binary
      ]}].

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


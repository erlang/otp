%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
-module(test_server_unicode_SUITE).

-export([all/1, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export(['#=@: difficult_case_name_äöå'/1,
	 print_and_log_unicode/1,
	 print_and_log_latin1/1]).

-include_lib("common_test/include/ct.hrl").

all(suite) ->
    ['#=@: difficult_case_name_äöå',
     print_and_log_unicode,
     print_and_log_latin1].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case,Config) ->
    init_timetrap(500,Config).

init_timetrap(T,Config) ->
    Dog = ?t:timetrap(T),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case,Config) ->
    cancel_timetrap(Config).

cancel_timetrap(Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.


%%%-----------------------------------------------------------------
%%% Test cases

'#=@: difficult_case_name_äöå'(Config) when is_list(Config) ->
    ok.

print_and_log_unicode(Config) when is_list(Config) ->
    String = "שלום-שלום+של 日本語",
    test_server:comment(String),
    test_server:capture_start(),
    io:format("String with ts: ~ts",[String]),
    test_server:capture_stop(),
    "String with ts: "++String = lists:flatten(test_server:capture_get()),
    ok.

print_and_log_latin1(Config) when is_list(Config) ->
    String = "æøå",
    test_server:comment(String),
    test_server:capture_start(),
    io:format("String with s: ~s",[String]),
    io:format("String with ts: ~ts",[String]),
    test_server:capture_stop(),
    ["String with s: "++String,
     "String with ts: "++String] =
	[lists:flatten(L) || L<- test_server:capture_get()],
    ok.

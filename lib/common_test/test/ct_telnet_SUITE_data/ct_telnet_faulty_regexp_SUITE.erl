%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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

-module(ct_telnet_faulty_regexp_SUITE).

-behaviour(ct_suite).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(name, telnet_server_conn1).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

suite() -> [{require,?name,{unix,[telnet]}},
	    {require,ct_conn_log},
 	    {ct_hooks, [{cth_conn_log,[]}]}].

all() ->
    [expect_pattern,
     expect_pattern_no_string,
     expect_tag_pattern,
     expect_tag_pattern_no_string,
     expect_pattern_unicode,
     expect_tag_pattern_unicode].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_,Config) ->
    ct:log("init_per_testcase: opening telnet connection...",[]),
    {ok,_} = ct_telnet:open(?name),
    ct:log("...done",[]),
    Config.

end_per_testcase(_,_Config) ->
    ct:log("end_per_testcase: closing telnet connection...",[]),
    _ = ct_telnet:close(?name),
    ct:log("...done",[]),
    ok.

expect_pattern(_) ->
    ok = ct_telnet:send(?name, "echo ayt"),
    ok = ct_telnet:expect(?name, "invalid(pattern").

expect_pattern_no_string(_) ->
    ok = ct_telnet:send(?name, "echo ayt"),
    ok = ct_telnet:expect(?name, invalid_pattern).

expect_tag_pattern(_) ->
    ok = ct_telnet:send(?name, "echo ayt"),
    ok = ct_telnet:expect(?name, {tag,"invalid(pattern"}).

expect_tag_pattern_no_string(_) ->
    ok = ct_telnet:send(?name, "echo ayt"),
    ok = ct_telnet:expect(?name, {tag,invalid_pattern}).

%% Test that a unicode pattern can be given without the testcase
%% failing.  Do however notice that there is no real unicode support
%% in ct_telnet yet, that is, the telnet binary mode is not supported.
expect_pattern_unicode(_) ->
    ok = ct_telnet:send(?name, "echo ayt"),
    {error,{prompt,_}} = ct_telnet:expect(?name, "pattern_with_unicode_αβ"),
    ok.

expect_tag_pattern_unicode(_) ->
    ok = ct_telnet:send(?name, "echo ayt"),
    {error,{prompt,_}} = ct_telnet:expect(?name, "pattern_with_unicode_αβ"),
    ok.

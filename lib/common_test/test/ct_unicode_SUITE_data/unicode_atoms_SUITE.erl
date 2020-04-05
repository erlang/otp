%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2018. All Rights Reserved.
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

-module(unicode_atoms_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    ['test_αβ',
     'fail_αβ_1',
     'fail_αβ_2',
     'fail_αβ_3',
     'fail_αβ_4',
     'skip_αβ'].

'test_αβ'(_Config) ->
    ct:log("This is test case ~tw",[?FUNCTION_NAME]),
    ok.

'fail_αβ_1'(_Config) ->
    ct:log("This is test case ~tw",[?FUNCTION_NAME]),
    'α' = 'β',
    ok.

'fail_αβ_2'(_Config) ->
    ct:log("This is test case ~tw",[?FUNCTION_NAME]),
    ct:fail({failing,testcase,?FUNCTION_NAME}),
    ok.

'fail_αβ_3'(_Config) ->
    ct:log("This is test case ~tw",[?FUNCTION_NAME]),
    exit({exiting,testcase,?FUNCTION_NAME}),
    ok.

'fail_αβ_4'(_Config) ->
    ct:log("This is test case ~tw",[?FUNCTION_NAME]),
    S = try throw(ok) catch throw:ok:Stacktrace -> Stacktrace end,
    erlang:raise(error,{error,testcase,?FUNCTION_NAME},S),
    ok.

'skip_αβ'(_Config) ->
    ct:log("This is test case ~tw",[?FUNCTION_NAME]),
    {skip,"Skipping " ++ atom_to_list(?FUNCTION_NAME)}.


%% This should not be listed in all/0. It is only to be run explicitly
%% using a test spec where the config file is declared as well.
'config_αβ'() ->
    [{require,'alias_αβ','key_αβ'}].
'config_αβ'(_Config) ->
    ct:log("This is test case ~tw",[?FUNCTION_NAME]),
    Conf = ct:get_config('alias_αβ'),
    Conf = ct:get_config('key_αβ'),
    ct:log("Required config: ~tp",[Conf]),
    ok.

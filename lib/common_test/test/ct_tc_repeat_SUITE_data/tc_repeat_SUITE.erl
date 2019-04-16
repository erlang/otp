%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-module(tc_repeat_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

init_per_group(_Group,Config) ->
    Config.

end_per_group(_Group,Config) ->
    ok.

all() ->
    [{testcase,tc1,[{repeat,2}]},
     {testcase,tc2,[{repeat_until_ok,3}]},
     {testcase,tc3,[{repeat_until_ok,3}]},
     {testcase,tc4,[{repeat_until_fail,3}]},
     {group,g},
     {group,g_until_ok},
     {group,g_until_fail},
     {group,g,[parallel]},
     {group,g,[sequence]},
     {group,g_mixed,[sequence]}].

groups() ->
    [{g,[{testcase,tc1,[{repeat,2}]}]},
     {g_until_ok,[{testcase,tc2,[{repeat_until_ok,3}]}]},
     {g_until_fail,[{testcase,tc4,[{repeat_until_fail,3}]}]},
     {g_parallel_until_ok,[parallel],[{testcase,tc2,[{repeat_until_ok,3}]}]},
     {g_parallel_until_fail,[parallel],[{testcase,tc1,[{repeat_until_fail,2}]}]},
     {g_sequence_until_ok,[sequence],[{testcase,tc2,[{repeat_until_ok,3}]}]},
     {g_sequence_until_fail,[sequence],[{testcase,tc1,[{repeat_until_fail,2}]}]},
     {g_mixed,[{testcase,tc1,[{repeat,2}]},
               {testcase,tc4,[{repeat,3}]},
               tc1,
               {group,g},
               {subgroup,[tc1,{testcase,tc2,[{repeat,2}]}]},
               {testcase,tc2,[{repeat,2}]},
               tc2,
               {testcase,tc1,[{repeat,2}]},
               tc1]}].

%% Test cases starts here.
tc1(_Config) ->
    ok.

tc2(_Config) ->
    ok.

tc3(_Config) ->
    ct:fail(always_fail).

tc4(Config) ->
    case ?config(saved_config,Config) of
        {tc4,_} ->
            ct:fail(second_time_fail);
        undefined ->
            {save_config,Config}
    end.

tc5(_Config) ->
    {skip,"just skip this"}.

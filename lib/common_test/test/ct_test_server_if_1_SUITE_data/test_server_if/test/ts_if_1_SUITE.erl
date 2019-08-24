%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
-module(ts_if_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,2}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(g1, _Config) ->
    {skip,g1_got_skipped};
init_per_group(g3, _Config) ->
    {skip,g3_got_skipped};
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(tc1, Config) ->
    ct:sleep(5000),
    Config;
init_per_testcase(tc8, _Config) ->
    {skip,"tc8 skipped"};
init_per_testcase(tc11, Config) ->
    bad_format;
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_testcase(tc2, Config) ->
    ct:sleep(5000);
end_per_testcase(tc12, Config) ->
    ct:comment("end_per_testcase(tc12) called!"),
    ct:pal("end_per_testcase(tc12) called!", []),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [{g1,[],[gtc1]},
     {g2,[parallel],[{g3,[],[gtc2]}]},
     
     {seq2,[sequence],[tc4,tc5]}].

sequences() ->
    [{seq1,[tc4,tc5]}].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() -> 
    [tc1, tc2, tc3,
     {sequence,seq1},
     {group,seq2},
     tc6, tc7, 
     tc8, tc9, tc10, 
     tc11,
     {group,g1},
     {group,g2},
     tc12, tc13].

tc1(_) ->
    exit(should_have_been_skipped).

tc2(_) ->
    timeout_in_end_per_testcase.

tc3(_) ->
   ct:sleep(5000).

tc4(_) ->
    exit(failed_on_purpose).

tc5(_) ->
    exit(should_have_been_skipped).

tc6() ->
    [{require,void}].
tc6(_) ->
    exit(should_have_been_skipped).

tc7() ->
    bad_format.
tc7(_) ->
    done.

tc8(_) ->
    exit(should_have_been_skipped).

tc9(_) ->
    {skip,'tc9 skipped'}.

tc10(config) ->
    done.

tc11(_) ->
    exit(should_have_been_skipped).


gtc1(_) ->
    exit(should_have_been_skipped).

gtc2(_) ->
    exit(should_have_been_skipped).

tc12(_) ->
    F = fun() -> ct:abort_current_testcase('stopping tc12') end,
    spawn(F),
    ct:sleep(1000),
    exit(should_have_been_aborted).

tc13(_) ->
    success.



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

-module(ct_cth_update_result_post_end_tc_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() ->
    [{timetrap,{seconds,3}}].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    ok.

init_per_group(_,Config) ->
    Config.

end_per_group(_,_) ->
    ok.

init_per_testcase(_,Config) ->
    Config.

end_per_testcase(EndTimetrap,_) when EndTimetrap==end_timetrap_to_fail;
                                     EndTimetrap==end_timetrap_to_skip->
    timer:sleep(10000);
end_per_testcase(EndFail,_) when EndFail==end_fail_to_fail;
                                 EndFail==end_fail_to_skip->
    ct:fail("change result when end fails");
end_per_testcase(_,_) ->
    ok.

all() ->
    [tc_ok_to_fail,
     tc_ok_to_skip,
     tc_fail_to_ok,
     tc_fail_to_skip,
     tc_timetrap_to_ok,
     tc_timetrap_to_skip,
     tc_skip_to_fail,
     end_fail_to_fail,
     end_fail_to_skip,
     end_timetrap_to_fail,
     end_timetrap_to_skip].

%% Test cases starts here.
tc_ok_to_fail(_Config) ->
    ok.

tc_ok_to_skip(_Config) ->
    ok.

tc_fail_to_ok(_Config) ->
    ct:fail("should be changed to ok").

tc_fail_to_skip(_Config) ->
    ct:fail("should be changed to skip").

tc_timetrap_to_ok(_Config) ->
    timer:sleep(10000), % will time out after 3 sek
    ok.

tc_timetrap_to_skip(_Config) ->
    timer:sleep(10000), % will time out after 3 sek
    ok.

tc_skip_to_fail(_Config) ->
    {skip,"should be changed to fail"}.

end_fail_to_fail(_Config) ->
    ok.

end_fail_to_skip(_Config) ->
    ok.

end_timetrap_to_fail(_Config) ->
    ok.

end_timetrap_to_skip(_Config) ->
    ok.

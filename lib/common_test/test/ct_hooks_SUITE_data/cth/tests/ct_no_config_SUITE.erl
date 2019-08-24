%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-module(ct_no_config_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

%%% This suite is used to verify 2 things:
%%%
%%% 1) All hook pre/post functions get called, even if no init/end
%%%    config functions exist in the suite (new from ver 1.6.1, R15B01).
%%%
%%% 2) The hook functions can read Config list elements, as well as
%%%    required config variables, even if no init/end config
%%%    functions exist. 

suite() ->
    [{timetrap, {seconds,1}},
     {ct_hooks, [verify_config_cth]},
     {require,suite_cfg},
     {default_config,suite_cfg,?MODULE}].

group(test_group) ->
    [{require,group_cfg},
     {default_config,group_cfg,test_group}].

test_case_1() ->    
    [{require,test_case_1_cfg},
     {default_config,test_case_1_cfg,test_case_1}].    

test_case_2() ->    
    [{require,test_case_2_cfg},
     {default_config,test_case_2_cfg,test_case_2}].    

all() ->
    [test_case_1, {group,test_group}].

groups() ->
    [{test_group,[],[test_case_2]}].

test_case_1(Config) ->
    ok.

test_case_2(Config) ->
    ok.

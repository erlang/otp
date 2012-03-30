%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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

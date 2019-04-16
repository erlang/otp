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


-module(all_and_groups_cth).


-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").

%% Send a cth_error event if a callback is called with unexpected arguments
-define(fail(Info),
        gen_event:notify(
          ?CT_EVMGR_REF, 
          #event{ name = cth_error,
                  node = node(),
                  data = {illegal_hook_callback,{?MODULE,?FUNCTION_NAME,Info}}})).

%% CT Hooks
-compile(export_all).

id(Opts) ->
    empty_cth:id(Opts).

post_groups(Suite,Groups) ->
    case empty_cth:post_groups(Suite,ct:get_config(post_groups_return,Groups)) of
        crash -> error(crash_in_post_groups);
        R -> R
    end.

post_all(Suite,Tests,Groups) ->
    case empty_cth:post_all(Suite,ct:get_config(post_all_return,Tests),Groups) of
        crash -> error(crash_in_post_all);
        R -> R
    end.

init(Id, Opts) ->
    empty_cth:init(Id, Opts).

pre_init_per_suite(Suite, Config, State) ->
    empty_cth:pre_init_per_suite(Suite,Config,State).

post_init_per_suite(Suite,Config,Return,State) ->
    empty_cth:post_init_per_suite(Suite,Config,Return,State).

pre_end_per_suite(Suite,Config,State) ->
    empty_cth:pre_end_per_suite(Suite,Config,State).

post_end_per_suite(Suite,Config,Return,State) ->
    empty_cth:post_end_per_suite(Suite,Config,Return,State).

pre_init_per_group(Suite,Group,Config,State) ->
    empty_cth:pre_init_per_group(Suite,Group,Config,State).

post_init_per_group(Suite,Group,Config,Return,State) ->
    empty_cth:post_init_per_group(Suite,Group,Config,Return,State).

pre_end_per_group(Suite,Group,Config,State) ->
    empty_cth:pre_end_per_group(Suite,Group,Config,State).

post_end_per_group(Suite,Group,Config,Return,State) ->
    empty_cth:post_end_per_group(Suite,Group,Config,Return,State).

pre_init_per_testcase(Suite,TC,Config,State) ->
    empty_cth:pre_init_per_testcase(Suite,TC,Config,State).

post_init_per_testcase(Suite,TC,Config,Return,State) ->
    empty_cth:post_init_per_testcase(Suite,TC,Config,Return,State).

pre_end_per_testcase(Suite,TC,Config,State) ->
    empty_cth:pre_end_per_testcase(Suite,TC,Config,State).

post_end_per_testcase(Suite,TC,Config,Return,State) ->
    empty_cth:post_end_per_testcase(Suite,TC,Config,Return,State).

on_tc_fail(Suite,TC,Reason,State) ->
    empty_cth:on_tc_fail(Suite,TC,Reason,State).

on_tc_skip(Suite,TC,Reason,State) ->
    empty_cth:on_tc_skip(Suite,TC,Reason,State).

terminate(State) ->
    empty_cth:terminate(State).

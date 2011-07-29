%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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


-module(state_update_cth).


-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").

%% CT Hooks
-compile(export_all).

init(Id, Opts) ->
    State = empty_cth:init(Id, Opts),
    {ok, [init|State]}.

pre_init_per_suite(Suite, Config, State) ->
    empty_cth:pre_init_per_suite(Suite,Config,State),
    {Config, [pre_init_per_suite|State]}.

post_init_per_suite(Suite,Config,Return,State) ->
    empty_cth:post_init_per_suite(Suite,Config,Return,State),
    {Config, [post_init_per_suite|State]}.

pre_end_per_suite(Suite,Config,State) ->
    empty_cth:pre_end_per_suite(Suite,Config,State),
    {Config, [pre_end_per_suite|State]}.

post_end_per_suite(Suite,Config,Return,State) ->
    empty_cth:post_end_per_suite(Suite,Config,Return,State),
    {Return, [post_end_per_suite|State]}.

pre_init_per_group(Group,Config,State) ->
    empty_cth:pre_init_per_group(Group,Config,State),
    {Config, [pre_init_per_group|State]}.

post_init_per_group(Group,Config,Return,State) ->
    empty_cth:post_init_per_group(Group,Config,Return,State),
    {Return, [post_init_per_group|State]}.

pre_end_per_group(Group,Config,State) ->
    empty_cth:pre_end_per_group(Group,Config,State),
    {Config, [pre_end_per_group|State]}.

post_end_per_group(Group,Config,Return,State) ->
    empty_cth:post_end_per_group(Group,Config,Return,State),
    {Return, [post_end_per_group|State]}.

pre_init_per_testcase(TC,Config,State) ->
    empty_cth:pre_init_per_testcase(TC,Config,State),
    {Config, [pre_init_per_testcase|State]}.

post_end_per_testcase(TC,Config,Return,State) ->
    empty_cth:post_end_per_testcase(TC,Config,Return,State),
    {Return, [post_end_per_testcase|State]}.

on_tc_fail(TC, Reason, State) ->
    empty_cth:on_tc_fail(TC,Reason,State),
    [on_tc_fail|State].

on_tc_skip(TC, Reason, State) ->
    empty_cth:on_tc_skip(TC,Reason,State),
    [on_tc_skip|State].

terminate(State) ->
    empty_cth:terminate(State).

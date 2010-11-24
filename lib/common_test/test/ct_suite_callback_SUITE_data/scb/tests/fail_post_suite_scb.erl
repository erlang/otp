%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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


-module(fail_post_suite_scb).


-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").


%% Suite Callbacks
-compile(export_all).

init(Opts) ->
    empty_scb:init(Opts).

pre_init_per_suite(Suite, Config, State) ->
    empty_scb:pre_init_per_suite(Suite,Config,State).

post_init_per_suite(Suite,Config,Return,State) ->
    empty_scb:post_init_per_suite(Suite,Config,Return,State),
    {{fail, "Test failure"}, State}.

pre_end_per_suite(Suite,Config,State) ->
    empty_scb:pre_end_per_suite(Suite,Config,State).

post_end_per_suite(Suite,Config,Return,State) ->
    empty_scb:post_end_per_suite(Suite,Config,Return,State).

pre_init_per_group(Group,Config,State) ->
    empty_scb:pre_init_per_group(Group,Config,State).

post_init_per_group(Group,Config,Return,State) ->
    empty_scb:post_init_per_group(Group,Config,Return,State).

pre_end_per_group(Group,Config,State) ->
    empty_scb:pre_end_per_group(Group,Config,State).

post_end_per_group(Group,Config,Return,State) ->
    empty_scb:post_end_per_group(Group,Config,Return,State).

pre_init_per_testcase(TC,Config,State) ->
    empty_scb:pre_init_per_testcase(TC,Config,State).

post_end_per_testcase(TC,Config,Return,State) ->
    empty_scb:post_end_per_testcase(TC,Config,Return,State).

on_tc_fail(TC, Reason, State) ->
    empty_scb:on_tc_fail(TC,Reason,State).

on_tc_skip(TC, Reason, State) ->
    empty_scb:on_tc_skip(TC,Reason,State).

terminate(State) ->
    empty_scb:terminate(State).

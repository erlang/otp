%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014. All Rights Reserved.
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
%%----------------------------------------------------------------------
-module(cover_nomerge_remote_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

suite() ->
    [].

all() ->
    [t1,t2].

init_per_suite(Config) ->
    {ok,Host} = inet:gethostname(),
    Node = list_to_atom("ct_nomerge@"++Host),
    pong = net_adm:ping(Node),

%% Include this row, and exclude the equivalent row in end_per_suite =>
%% fails every now and then with missing data. Why?
%%    ct_cover:remove_nodes([Node]),
    ct_cover:add_nodes([Node]),
    [{node,Node}|Config].

end_per_suite(Config) ->
    Node = ?config(node,Config),
    ct_cover:remove_nodes([Node]),
    Config.

init_per_testcase(_Case, Config) ->
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%%-----------------------------------------------------------------
%%% Test cases
break(_Config) ->
    test_server:break(""),
    ok.

t1(Config) ->
    Node = ?config(node,Config),
    cover_compiled = rpc:call(Node, code, which, [cover_test_mod]),
    ok = rpc:call(Node, cover_test_mod, foo, []),
    ok.

t2(Config) ->
    Node = ?config(node,Config),
    cover_compiled = rpc:call(Node, code, which, [cover_test_mod]),
    ok = rpc:call(Node, cover_test_mod, foo, []),
    ok.

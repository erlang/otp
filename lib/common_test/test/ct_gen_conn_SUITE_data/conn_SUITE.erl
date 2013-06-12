%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File        : conn_SUITE
%%% Description : Check that the generic connection handling in CT
%%%               works as expected.
%%%-------------------------------------------------------------------
-module(conn_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{seconds,5}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> 
    [handles_to_multi_conn_pids, handles_to_single_conn_pids,
     names_to_multi_conn_pids, names_to_single_conn_pids].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

handles_to_multi_conn_pids() ->
    [{require,multi_conn_pid}].

handles_to_multi_conn_pids(_Config) ->
    application:set_env(ct_test, reconnect, true),

    Handle1 = proto:open(multi_conn_pid),
    ConnPid1 = ct_gen_conn:get_conn_pid(Handle1),
    {true,true} = {is_process_alive(Handle1),is_process_alive(ConnPid1)},
    Handle2 = proto:open(multi_conn_pid),
    ConnPid2 = ct_gen_conn:get_conn_pid(Handle2),
    {true,true} = {is_process_alive(Handle2),is_process_alive(ConnPid2)},
    Handle3 = proto:open(multi_conn_pid),
    ConnPid3 = ct_gen_conn:get_conn_pid(Handle3),
    {true,true} = {is_process_alive(Handle3),is_process_alive(ConnPid3)},
    
    ok = proto:close(Handle1),
    timer:sleep(100),
    {false,false} = {is_process_alive(Handle1),is_process_alive(ConnPid1)},
    {true,true} = {is_process_alive(Handle2),is_process_alive(ConnPid2)},

    ok = proto:kill_conn_proc(Handle2),
    timer:sleep(100),
    {true,false} = {is_process_alive(Handle2),is_process_alive(ConnPid2)},
    ConnPid2x = ct_gen_conn:get_conn_pid(Handle2),
    true = is_process_alive(ConnPid2x),

    ok = proto:close(Handle2),
    timer:sleep(100),
    {false,false} = {is_process_alive(Handle2),is_process_alive(ConnPid2x)},

    application:set_env(ct_test, reconnect, false),
    ok = proto:kill_conn_proc(Handle3),
    timer:sleep(100),
    {false,false} = {is_process_alive(Handle3),is_process_alive(ConnPid3)},

    ok.

handles_to_single_conn_pids() ->
    [{require,single_conn_pid}].

handles_to_single_conn_pids(_Config) ->
    application:set_env(ct_test, reconnect, true),

    Handle1 = proto:open(single_conn_pid),
    ConnPid = ct_gen_conn:get_conn_pid(Handle1),
    {true,true} = {is_process_alive(Handle1),is_process_alive(ConnPid)},
    Handle2 = proto:open(single_conn_pid),
    ConnPid = ct_gen_conn:get_conn_pid(Handle2),
    {true,true} = {is_process_alive(Handle2),is_process_alive(ConnPid)},
    Handle3 = proto:open(single_conn_pid),
    ConnPid = ct_gen_conn:get_conn_pid(Handle3),
    {true,true} = {is_process_alive(Handle3),is_process_alive(ConnPid)},

    Conns = [{undefined,Handle1,_,_},
	     {undefined,Handle2,_,_},
	     {undefined,Handle3,_,_}] = lists:sort(ct_util:get_connections(ConnPid)),
    ct:pal("CONNS = ~n~p", [Conns]),

    ok = proto:close(Handle1),
    timer:sleep(100),
    {false,true} = {is_process_alive(Handle1),is_process_alive(ConnPid)},

    ok = proto:kill_conn_proc(Handle2),
    timer:sleep(100),
    NewConnPid = ct_gen_conn:get_conn_pid(Handle2),
    NewConnPid = ct_gen_conn:get_conn_pid(Handle3),
    true = is_process_alive(Handle2),
    true = is_process_alive(Handle3),

    ok = proto:close(Handle2),
    timer:sleep(100),
    {false,true} = {is_process_alive(Handle2),is_process_alive(NewConnPid)},

    application:set_env(ct_test, reconnect, false),
    ok = proto:kill_conn_proc(Handle3),
    timer:sleep(100),
    {false,false} = {is_process_alive(Handle3),is_process_alive(NewConnPid)},    

    ok.

names_to_multi_conn_pids() ->
    [{require,mconn1,multi_conn_pid},
     {require,mconn2,multi_conn_pid},
     {require,mconn3,multi_conn_pid}].

names_to_multi_conn_pids(_Config) ->
    application:set_env(ct_test, reconnect, true),

    Handle1 = proto:open(mconn1),
    ConnPid1 = ct_gen_conn:get_conn_pid(Handle1),
    {true,true} = {is_process_alive(Handle1),is_process_alive(ConnPid1)},
    Handle2 = proto:open(mconn2),
    ConnPid2 = ct_gen_conn:get_conn_pid(Handle2),
    {true,true} = {is_process_alive(Handle2),is_process_alive(ConnPid2)},
    Handle3 = proto:open(mconn3),
    ConnPid3 = ct_gen_conn:get_conn_pid(Handle3),
    {true,true} = {is_process_alive(Handle3),is_process_alive(ConnPid3)},

    Handle1 = proto:open(mconn1),

    ok = proto:close(mconn1),
    timer:sleep(100),
    {false,false} = {is_process_alive(Handle1),is_process_alive(ConnPid1)},

    ok = proto:kill_conn_proc(Handle2),
    timer:sleep(100),
    Handle2 = proto:open(mconn2),  % should've been reconnected already
    {true,false} = {is_process_alive(Handle2),is_process_alive(ConnPid2)},
    ConnPid2x = ct_gen_conn:get_conn_pid(Handle2),
    true = is_process_alive(ConnPid2x),

    ok = proto:close(mconn2),
    timer:sleep(100),
    {false,false} = {is_process_alive(Handle2),is_process_alive(ConnPid2x)},
    Handle2y = proto:open(mconn2),
    ConnPid2y = ct_gen_conn:get_conn_pid(Handle2y),
    {true,true} = {is_process_alive(Handle2y),is_process_alive(ConnPid2y)},
    ok = proto:close(mconn2),
    timer:sleep(100),
    {false,false} = {is_process_alive(Handle2y),is_process_alive(ConnPid2y)},

    application:set_env(ct_test, reconnect, false),
    ok = proto:kill_conn_proc(Handle3),
    timer:sleep(100),
    {false,false} = {is_process_alive(Handle3),is_process_alive(ConnPid3)},

    ok.

names_to_single_conn_pids() ->
    [{require,sconn1,single_conn_pid},
     {require,sconn2,single_conn_pid},
     {require,sconn3,single_conn_pid}].

names_to_single_conn_pids(_Config) ->
    application:set_env(ct_test, reconnect, true),

    Handle1 = proto:open(sconn1),
    ConnPid = ct_gen_conn:get_conn_pid(Handle1),
    {true,true} = {is_process_alive(Handle1),is_process_alive(ConnPid)},
    Handle2 = proto:open(sconn2),
    ConnPid = ct_gen_conn:get_conn_pid(Handle2),
    {true,true} = {is_process_alive(Handle2),is_process_alive(ConnPid)},
    Handle3 = proto:open(sconn3),
    ConnPid = ct_gen_conn:get_conn_pid(Handle3),
    {true,true} = {is_process_alive(Handle3),is_process_alive(ConnPid)},

    Handle1 = proto:open(sconn1),

    Conns = [{sconn1,Handle1,_,_},
	     {sconn2,Handle2,_,_},
	     {sconn3,Handle3,_,_}] = lists:sort(ct_util:get_connections(ConnPid)),
    ct:pal("CONNS on ~p = ~n~p", [ConnPid,Conns]),

    ok = proto:close(sconn1),
    timer:sleep(100),
    {false,true} = {is_process_alive(Handle1),is_process_alive(ConnPid)},

    ok = proto:kill_conn_proc(Handle2),
    timer:sleep(100),
    {true,false} = {is_process_alive(Handle2),is_process_alive(ConnPid)},
    Handle2 = proto:open(sconn2),  % should've been reconnected already
    NewConnPid = ct_gen_conn:get_conn_pid(Handle2),
    true = is_process_alive(NewConnPid),
    
    Conns1 = [{sconn2,Handle2,_,_},
	      {sconn3,Handle3,_,_}] = 
	lists:sort(ct_util:get_connections(NewConnPid)),
    ct:pal("CONNS on ~p = ~n~p", [NewConnPid,Conns1]),

    ok = proto:close(sconn2),
    timer:sleep(100),
    {false,true} = {is_process_alive(Handle2),is_process_alive(NewConnPid)},

    application:set_env(ct_test, reconnect, false),
    ok = proto:kill_conn_proc(Handle3),
    timer:sleep(100),
    {false,false} = {is_process_alive(Handle3),is_process_alive(NewConnPid)},

    ok.



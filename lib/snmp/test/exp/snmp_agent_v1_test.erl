%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(snmp_agent_v1_test).

%% TODO
%% * Test fault-tolerance (kill master etc)
%%

-export([]).

-define(application, snmp).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").
-define(SNMP_USE_V3, true).
-include_lib("snmp/include/snmp_types.hrl").


-define(klas1, [1,3,6,1,2,1,7]).
-define(klas2, [1,3,6,1,2,1,9]).
-define(klas3, [1,3,6,1,2,1,8,1]).
-define(klas4, [1,3,6,1,2,1,8,4]).
-define(sa, [1,3,6,1,4,1,193,2]).
-define(system, [1,3,6,1,2,1,1]).
-define(snmp, [1,3,6,1,2,1,11]).
-define(snmpTraps, [1,3,6,1,6,3,1,1,5]).
-define(ericsson, [1,3,6,1,4,1,193]).
-define(testTrap, [1,3,6,1,2,1,15,0]).
-define(xDescr, [1,3,6,1,2,1,17,1]).
-define(xDescr2, [1,3,6,1,2,1,17,2]).

-define(active, 1).
-define(notInService, 2).
-define(notReady, 3).
-define(createAndGo, 4).
-define(createAndWait, 5).
-define(destroy, 6).

-define(TRAP_UDP, 5000).

-define(tooBigStr, "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff").


%% -- test manager defines --
-define(MGR,           snmp_test_mgr).
-define(GN(X),         ?MGR:gn(X)).
-define(G(X),          ?MGR:g(X)).
-define(S(X),          ?MGR:s(X)).
-define(GB(X),         ?MGR:gb(X)).
-define(SEND_BYTES(X), ?MGR:send_bytes(X)).

%% -- agent test lib defines --
-define(LIB,                snmp_agent_test_lib).
-define(INIT_CASE(X),       ?LIB:init_case(X)).
-define(TRY_TEST1(A),       ?LIB:try_test(A)).
-define(TRY_TEST2(A, B),    ?LIB:try_test(A, B)).
-define(TRY_TEST3(A, B, C), ?LIB:try_test(A, B, C)).
-define(START_SA(A, B, C),  ?LIB:start_subagent(A, B, C)).
-define(STOP_SA(A),         ?LIB:stop_subagent(A)).
-define(P1(C),              ?LIB:p(C)).
-define(P2(F),              ?LIB:p(F,[])).
-define(P3(F,A),            ?LIB:p(F,A)).
-define(RPC(N, F, A),       ?LIB:rpc(N, F, A)).


-define(v1_2(V1,V2),
	       case get(vsn) of
		   v1 -> V1;
		   _ -> V2
	       end).
		        
-define(v1_2_3(V1,V2,V3),
	       case get(vsn) of
		   v1 -> V1;
		   v2 -> V2;
		   _ -> V3
	       end).

all(suite) -> {req,
	       [mnesia, distribution,
		{local_slave_nodes, 2}, {time, 360}],
	       [{conf, init, cases(), finish}]}.

init_per_testcase(_Case, Config) when list(Config) ->
    Dog = ?t:timetrap(?t:minutes(6)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) when list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    Config.

cases() ->
    [simple, 
     db_notify_client,
     processing, 
     big, 
     big2, 
     %% implied, 
     loop_mib, 
     api, 
     subagent, 
     mnesia, 
     multiple_reqs,
     sa_register, 
     v1_trap, 
     sa_error, 
     next_across_sa, 
     undo, 
     standard_mibs, 
     sparse_table, 
     cnt_64, 
     opaque,
     %% opaque].
     
     change_target_addr_config,

     reported_bugs,
     tickets
    ].  


init(Config) ->
    init_all(Config),
    init_v1(Config).

finish(Config) ->
    finish_v1(Config),
    finish_all(Config).

init_v1(Config) when list(Config) ->
    ?line SaNode = ?config(snmp_sa, Config),
    ?line create_tables(SaNode),
    ?line AgentDir = ?config(agent_dir, Config),
    ?line MgrDir   = ?config(mgr_dir, Config),
    ?line Ip       = ?config(ip, Config),
    ?line config([v1], MgrDir, AgentDir, tuple_to_list(Ip), tuple_to_list(Ip)),
    [{vsn, v1} | start_v1_agent(Config)].

finish_v1(Config) when list(Config) ->
    delete_tables(),
    C1 = stop_agent(Config),
    delete_files(C1),
    lists:keydelete(vsn, 1, C1).


%%-----------------------------------------------------------------
%% This function takes care of the old OTP-SNMPEA-MIB.
%% Unfortunately, the testcases were written to use the data in the
%% internal tables, and these table are now obsolete and not used
%% by the agent.  Therefore, we emulate them by using
%% OLD-SNMPEA-MIB, which uses the default impl. of all tables.
%%
%% These two rows must exist in intCommunityTable
%%    {[147,214,36,45], "public", 2, readWrite}.
%%    {[147,214,36,45], "standard trap", 2, read}.
%% (But with the manager's IP address)
%%
%%-----------------------------------------------------------------
init_old() ->
    snmpa_local_db:table_create_row(intCommunityTable,
                                   get(mip) ++ [6 | "public"],
                                   {get(mip), "public", 2, 2}),
    snmpa_local_db:table_create_row(intCommunityTable,
                                   get(mip) ++ [13 | "standard trap"],
                                   {get(mip), "standard trap", 2, 1}),
    snmpa_local_db:variable_set(intAgentIpAddress, [127,0,0,1]).
     

%% =========================================================================
%% 
%%                                C A S E S 
%% 
%% =========================================================================

%% -- simple --

simple(suite) -> [];
simple(Config) when list(Config) ->
    ?P1(simple),
    ?INIT_CASE(Config),
    
    ?TRY_TEST1(simple_standard_test).

simple_standard_test() ->
    ?DBG("simple_standard_test -> entry",[]),
    ?GN([[1,1]]),
    ?line expect(1, [{[sysDescr,0], "Erlang SNMP agent"}]),

    ?GN([[1,3]]),
    ?line expect(11, [{[sysDescr,0], "Erlang SNMP agent"}]),

    ?GN([[1,3,6]]),
    ?line expect(12, [{[sysDescr,0], "Erlang SNMP agent"}]),

    ?GN([[1,3,6,1]]),
    ?line expect(13, [{[sysDescr,0], "Erlang SNMP agent"}]),

    ?GN([[1,3,6,1,2]]),
    ?line expect(14, [{[sysDescr,0], "Erlang SNMP agent"}]),

    ?GN([[1,3,6,1,2,1]]),
    ?line expect(15, [{[sysDescr,0], "Erlang SNMP agent"}]),

    ?GN([[1,3,6,1,2,1,1]]),
    ?line expect(16, [{[sysDescr,0], "Erlang SNMP agent"}]),

    ?GN([[sysDescr]]),
    ?line expect(17, [{[sysDescr,0], "Erlang SNMP agent"}]),

    ?G([[sysDescr,0]]),
    ?line expect(2, [{[sysDescr,0], "Erlang SNMP agent"}]),

    ?G([[sysDescr]]),
    ?line ?v1_2(expect(3, noSuchName, 1, any),
		expect(3, [{[sysDescr], noSuchObject}])),

    ?G([[1,6,7,0]]),
    ?line ?v1_2(expect(41, noSuchName, 1, any),
		expect(3, [{[1,6,7,0], noSuchObject}])),

    ?GN([[1,13]]),
    ?line ?v1_2(expect(4, noSuchName,1, any),
		expect(4, [{[1,13], endOfMibView}])),

    ?S([{[sysLocation, 0], "new_value"}]),
    ?line expect(5, [{[sysLocation, 0], "new_value"}]),

    ?G([[sysLocation, 0]]),
    ?line expect(6, [{[sysLocation, 0], "new_value"}]),

    io:format("Testing noSuchName and badValue...~n"),
    ?S([{[sysServices,0], 3}]),
    ?line expect(61, ?v1_2(noSuchName, notWritable), 1, any),

    ?S([{[sysLocation, 0], i, 3}]),
    ?line expect(62, ?v1_2(badValue, wrongType), 1, any),
    ?DBG("simple_standard_test -> done",[]),
    ok.


%% -- db_notify_client --

%% This is run in the agent node
db_notify_client(suite) -> [];
db_notify_client(Config) when list(Config) ->
    ?P1(db_notify_client),
    {SaNode, MgrNode, MibDir} = ?INIT_CASE(Config),
    ?DBG("~n\tSaNode: ~p~n\tMgrNode: ~p~n\tMibDir: ~p",
	   [SaNode,MgrNode,MibDir]),
    snmpa_local_db:register_notify_client(self(),?MODULE),

    %% This call (the manager) will issue to set operations, so
    %% we expect to receive to notify(insert) calls.
    ?TRY_TEST1(db_notify_client_test),

    ?DBG("await first notify",[]),
    receive 
	{db_notify_test_reply,insert} -> ?DBG("first notify received",[]),ok
    end,
    
    ?DBG("await second notify",[]),
    receive 
	{db_notify_test_reply,insert} -> ?DBG("second notify received",[]),ok
    end,

    snmpa_local_db:unregister_notify_client(self()).


%% This is run in the manager node
db_notify_client_test() ->
    ?DBG("set first new sysLocation",[]),
    ?S([{[sysLocation, 0], "new_value"}]),
    ?line expect(5, [{[sysLocation, 0], "new_value"}]),

    ?DBG("set second new sysLocation",[]),
    ?S([{[sysLocation, 0], "new_value"}]),
    ?line expect(5, [{[sysLocation, 0], "new_value"}]).

notify(Pid,What) -> 
    ?DBG("notify(~p,~p) -> called",[Pid,What]),
    Pid ! {db_notify_test_reply,What}.


%% -- processing --

%% Req. Test2
processing(suite) -> [];
processing(Config) when list(Config) ->
    ?P1(processing),
    ?INIT_CASE(Config),

    ?line load_master("Test2"),
    ?TRY_TEST1(v1_proc),
    ?line unload_master("Test2").

v1_proc() ->
    ?DBG("v1_proc -> entry", []),
    %% According to RFC1157.
    %% Template: <Section>:<list no>
    v1_get_p(),
    v1_get_next_p(),
    v1_set_p().
    
v1_get_p() ->
    %% 4.1.2:1
    ?G([[test2]]),
    ?line expect(10, noSuchName, 1, [{[test2], 'NULL'}]),
    ?G([[tDescr]]),
    ?line expect(11, noSuchName, 1, [{[tDescr], 'NULL'}]),
    ?G([[tDescr2,0]]),
    ?line expect(12, noSuchName, 1, [{[tDescr2,0], 'NULL'}]),
    ?G([[tDescr3,0]]),
    ?line expect(131, noSuchName, 1, [{[tDescr3,0], 'NULL'}]),
    ?G([[tDescr4,0]]),
    ?line expect(132, noSuchName, 1, [{[tDescr4,0], 'NULL'}]),
    ?G([[sysDescr, 0], [tDescr,0]]), % Outside mibview
    ?line expect(14, noSuchName, 2, [{[sysDescr, 0], 'NULL'},
				     {[tDescr,0], 'NULL'}]),
    ?G([[sysDescr,3]]),
    ?line expect(15, noSuchName, 1, [{[sysDescr, 3], 'NULL'}]),
    
    %% 4.1.2:2
    ?G([[tTable]]),
    ?line expect(20, noSuchName, 1, [{[tTable], 'NULL'}]),
    ?G([[tEntry]]),
    ?line expect(21, noSuchName, 1, [{[tEntry], 'NULL'}]),
    
    %% 4.1.2:3
    ?G([[tTooBig, 0]]),
    ?line expect(30, tooBig, 0, [{[tTooBig, 0], 'NULL'}]),

    %% 4.1.2:4
    ?G([[tGenErr1, 0]]),
    ?line expect(40, genErr, 1, [{[tGenErr1, 0], 'NULL'}]),
    ?G([[tGenErr2, 0]]),
    ?line expect(41, genErr, 1, [{[tGenErr2, 0], 'NULL'}]),
    ?G([[sysDescr, 0], [tGenErr3, 0]]),
    ?line expect(42, genErr, 2, [{[sysDescr, 0], 'NULL'},
				 {[tGenErr3, 0], 'NULL'}]).
    
v1_get_next_p() ->
    %% 4.1.3:1
    ?GN([[1,3,7,1]]),
    ?line expect(10, noSuchName, 1, [{[1,3,7,1], 'NULL'}]),

    ?GN([[tDescr2]]),
    ?line expect(11, tooBig, 0, any),
    
    %% 4.1.3:2
    ?GN([[tTooBig]]),
    io:format("We currently don't handle tooBig correct!!!\n"),
    %% ?line expect(20, tooBig, 0, [{[tTooBig], 'NULL'}]),
    ?line expect(20, tooBig, 0, any),

    %% 4.1.3:3
    ?GN([[tGenErr1]]),
    %% ?line expect(40, genErr, 1, [{[tGenErr1], 'NULL'}]),
    ?line expect(40, genErr, 1, any),

    ?GN([[tGenErr2]]),
    %% ?line expect(41, genErr, 1, [{[tGenErr2], 'NULL'}]),
    ?line expect(41, genErr, 1, any),

    ?GN([[sysDescr], [tGenErr3]]),
    %% ?line expect(42, genErr, 2, [{[sysDescr], 'NULL'},
    %%	                            {[tGenErr3], 'NULL'}]).
    ?line expect(42, genErr, 2, any).
    
v1_set_p() ->
    %% 4.1.5:1
    ?S([{[1,3,7,0], i, 4}]),
    ?line expect(10, noSuchName, 1, [{[1,3,7,0], 4}]),

    ?S([{[tDescr,0], s, "outside mibview"}]),
    ?line expect(11, noSuchName, 1, [{[tDescr,0], "outside mibview"}]),

    ?S([{[tDescr3,0], s, "read-only"}]),
    ?line expect(12, noSuchName, 1, [{[tDescr3,0], "read-only"}]),

    ?S([{[tDescr3], s, "noSuchObject"}]),
    ?line expect(13, noSuchName, 1, [{[tDescr3], "noSuchObject"}]),

    ?S([{[tDescr3,1], s, "noSuchInstance"}]),
    ?line expect(14, noSuchName, 1, [{[tDescr3,1], "noSuchInstance"}]),

    ?S([{[tDescr2,0], s, "inconsistentName"}]),
    ?line expect(15, noSuchName, 1, [{[tDescr2,0], "inconsistentName"}]),

    %% 4.1.5:2
    ?S([{[tDescr2, 0], i, 4}]),
    ?line expect(20, badValue, 1, [{[tDescr2, 0], 4}]),

    ?S([{[tDescr2, 0], s, "badValue"}]),
    ?line expect(21, badValue, 1, [{[tDescr2, 0], "badValue"}]),
    
    %% 4.1.5:3
    %% The standard is quite incorrect here.  The resp pdu was too big.  In
    %% the resp pdu, we have the original vbs.  In the tooBig pdu we still
    %% have to original vbs => the tooBig pdu is too big as well!!!  It
    %% may not get it to the manager, unless the agent uses 'NULL' instead
    %% of the std-like original value.
    ?S([{[tTooBig, 0], s, ?tooBigStr}]),
    %% according to std:
    %% ?line expect(30, tooBig, 0, [{[tTooBig, 0], ?tooBigStr}]),
    ?line expect(30, tooBig, 0, [{[tTooBig, 0], 'NULL'}]),
    
    %% 4.1.5:4
    ?S([{[tDescr2, 0], s, "is_set_ok_fail"}]),
    ?line expect(40, genErr, 1, [{[tDescr2, 0], "is_set_ok_fail"}]),

    ?S([{[tDescr2, 0], s, "commit_fail"}]),
    ?line expect(41, genErr, 1, [{[tDescr2, 0], "commit_fail"}]).
    

%% -- big --

big(suite) -> [];
big(Config) when list(Config) ->
    ?P1(big),
    {SaNode, _MgrNode, _MibDir} = ?INIT_CASE(Config),

    p("Starting subagent..."),
    ?line pong = net_adm:ping(SaNode),
    
    ?line {ok, SA} = ?START_SA(SaNode, ?klas1, "Klas1"),
    ?DBG("big -> SA: ~p", [SA]),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),

    ?TRY_TEST1(big_test),

    ?line ?STOP_SA(SA),
    ?line unload_master("OLD-SNMPEA-MIB").

%% Req: system group, OLD-SNMPEA-MIB, Klas1
big_test() ->
    ?DBG("big_test -> testing simple next/get/set @ master agent...",[]),
    simple_standard_test(),
    
    ?DBG("big_test -> testing simple next/get/set @ subagent...",[]),
    ?GN([[klas1]]),
    ?line expect(1, [{[fname,0], ""}]),

    ?G([[fname,0]]),
    ?line expect(2, [{[fname,0], ""}]),

    ?S([{[fname,0], s, "test set"}]),
    ?line expect(3, [{[fname,0], "test set"}]),

    ?G([[fname,0]]),
    ?line expect(4, [{[fname,0], "test set"}]),
    
    ?DBG("big_test -> "
	"testing next from last instance in master to subagent...",[]),
    ?GN([[?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(5, [{[fname,0], "test set"}]),

    ?GN([[1,1],
	[?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(51, [{[sysDescr,0], "Erlang SNMP agent"},
		{[fname,0], "test set"}]),
    ?S([{[fname,0], s, ""}]),
    ?line expect(52, [{[fname,0], ""}]),
    
    table_test(),

    ?DBG("big_test -> adding one row in subagent table",[]),
    _FTab = [friendsEntry],
    ?S([{[friendsEntry, [2, 3]], s, "kompis3"},
       {[friendsEntry, [3, 3]], i, ?createAndGo}]),
    ?line expect(6, [{[friendsEntry, [2, 3]], "kompis3"},
	       {[friendsEntry, [3, 3]], ?createAndGo}]),

    ?G([[friendsEntry, [2, 3]],
       [friendsEntry, [3, 3]]]),
    ?line expect(7, [{[friendsEntry, [2, 3]], "kompis3"},
		     {[friendsEntry, [3, 3]], ?active}]),

    ?S([{[friendsEntry, [3, 3]], i, ?destroy}]),
    ?line expect(8, [{[friendsEntry, [3, 3]], ?destroy}]),
    
    otp_1131(),

    ?DBG("big_test -> adding two rows in subagent table with special INDEX",
       []),
    ?S([{[kompissEntry, [1, 3]], s, "kompis3"},
       {[kompissEntry, [2, 3]], i, ?createAndGo}]),
    ?line expect(9, [{[kompissEntry, [1, 3]], "kompis3"},
		     {[kompissEntry, [2, 3]], ?createAndGo}]),

    ?G([[kompissEntry, [1, 3]],
       [kompissEntry, [2, 3]]]),
    ?line expect(10, [{[kompissEntry, [1, 3]], "kompis3"},
		      {[kompissEntry, [2, 3]], ?active}]),

    ?GN([[kompissEntry, [1]],
	[kompissEntry, [2]]]),
    ?line expect(11, [{[kompissEntry, [1, 3]], "kompis3"},
		      {[kompissEntry, [2, 3]], ?active}]),

    ?S([{[kompissEntry, [1, 2]], s, "kompis3"},
       {[kompissEntry, [2, 2]], i, ?createAndGo}]),
    ?line expect(12, [{[kompissEntry, [1, 2]], "kompis3"},
		      {[kompissEntry, [2, 2]], ?createAndGo}]),

    ?GN([[kompissEntry, [1, 1]],
	[kompissEntry, [2, 1]]]),
    ?line expect(13, [{[kompissEntry, [1, 2]], "kompis3"},
		      {[kompissEntry, [2, 2]], ?active}]),

    ?S([{[kompissEntry, [2, 3]], i, ?destroy}]),
    ?line expect(14, [{[kompissEntry, [2, 3]], ?destroy}]),

    ?S([{[kompissEntry, [2, 2]], i, ?destroy}]),
    ?line expect(15, [{[kompissEntry, [2, 2]], ?destroy}]),
    ?DBG("big_test -> done",[]),
    ok.


%% Req. system group, Klas2, OLD-SNMPEA-MIB
big_test_2() ->
    ?P1(big_test_2),

    ?P2("Testing simple next/get/set @ master agent (2)..."),
    simple_standard_test(),

    p("Testing simple next/get/set @ subagent (2)..."),
    ?GN([[klas2]]),
    ?line expect(1, [{[fname2,0], ""}]),

    ?G([[fname2,0]]),
    ?line expect(2, [{[fname2,0], ""}]),
    
    ?S([{[fname2,0], s, "test set"}]),
    ?line expect(3, [{[fname2,0], "test set"}]),

    ?G([[fname2,0]]),
    ?line expect(4, [{[fname2,0], "test set"}]),

    otp_1298(),

    ?P2("Testing next from last object in master to subagent (2)..."),
    ?GN([[?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(5, [{[fname2,0], "test set"}]),

    ?GN([[1,1],
        [?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(51, [{[sysDescr,0], "Erlang SNMP agent"},
                {[fname2,0], "test set"}]),

    table_test(),

    ?P2("Adding one row in subagent table (2)"),
    ?S([{[friendsEntry2, [2, 3]], s, "kompis3"},
       {[friendsEntry2, [3, 3]], i, ?createAndGo}]),
    ?line expect(6, [{[friendsEntry2, [2, 3]], "kompis3"},
               {[friendsEntry2, [3, 3]], ?createAndGo}]),

    ?G([[friendsEntry2, [2, 3]],
       [friendsEntry2, [3, 3]]]),
    ?line expect(7, [{[friendsEntry2, [2, 3]], "kompis3"},
               {[friendsEntry2, [3, 3]], ?active}]),

    ?S([{[friendsEntry2, [3, 3]], i, ?destroy}]),
    ?line expect(8, [{[friendsEntry2, [3, 3]], ?destroy}]),

    ?P2("Adding two rows in subagent table with special INDEX (2)"),
    ?S([{[kompissEntry2, [1, 3]], s, "kompis3"},
       {[kompissEntry2, [2, 3]], i, ?createAndGo}]),
    ?line expect(9, [{[kompissEntry2, [1, 3]], "kompis3"},
               {[kompissEntry2, [2, 3]], ?createAndGo}]),
    ?G([[kompissEntry2, [1, 3]],
       [kompissEntry2, [2, 3]]]),
    ?line expect(10, [{[kompissEntry2, [1, 3]], "kompis3"},
                {[kompissEntry2, [2, 3]], ?active}]),
    ?GN([[kompissEntry2, [1]],
        [kompissEntry2, [2]]]),
    ?line expect(11, [{[kompissEntry2, [1, 3]], "kompis3"},
                {[kompissEntry2, [2, 3]], ?active}]),

    ?S([{[kompissEntry2, [1, 2]], s, "kompis3"},
       {[kompissEntry2, [2, 2]], i, ?createAndGo}]),
    ?line expect(12, [{[kompissEntry2, [1, 2]], "kompis3"},
                {[kompissEntry2, [2, 2]], ?createAndGo}]),

    ?GN([[kompissEntry2, [1, 1]],
        [kompissEntry2, [2, 1]]]),
    ?line expect(13, [{[kompissEntry2, [1, 2]], "kompis3"},
                {[kompissEntry2, [2, 2]], ?active}]),

    ?S([{[kompissEntry2, [2, 3]], i, ?destroy}]),
    ?line expect(14, [{[kompissEntry2, [2, 3]], ?destroy}]),

    ?S([{[kompissEntry2, [2, 2]], i, ?destroy}]),
    ?line expect(15, [{[kompissEntry2, [2, 2]], ?destroy}]),
    ok.


%% -- bug2 --

big2(suite) -> [];
big2(Config) when list(Config) ->
    ?P1(big2),
    %% This is exactly the same tests as 'big', but with the
    %% v2 equivalent of the mibs.
    {SaNode, _MgrNode, _MibDir} = ?INIT_CASE(Config),

    ?P2("Starting subagent..."),
    ?line pong = net_adm:ping(SaNode),
    
    ?line {ok, SA} = ?START_SA(SaNode, ?klas1, "Klas1-v2"),
    ?line load_master("OLD-SNMPEA-MIB-v2"),
    ?line init_old(),

    ?TRY_TEST1(big_test),

    ?line ?STOP_SUBAGENT(SA),
    ?line unload_master("OLD-SNMPEA-MIB-v2").


implied(suite) -> [];
implied(Config) when list(Config) ->
    ?P1(implied),
    ?INIT_CASE(Config),

    ?line load_master("Test1"),

    ?TRY_TEST2(implied_test,[whereis(snmp_master_agent)]),

    ?line unload_master("Test1").

%% Req. Test1
implied_test(MA) ->
    ?LOG("implied_test -> start",[]),

    snmpa:verbosity(MA,trace),
    snmpa:verbosity(MA,trace),

    %% Create two rows, check that they are get-nexted in correct order.
    Idx1 = "apa",
    Idx2 = "qq",

    ?DBG("implied_test -> (send) create row 1 '~s' in table 1",[Idx1]),
    ?S([{[testStatus, Idx1], i, ?createAndGo}, {[testDescr, Idx1],s,"row 1"}]),
    ?line expect(1, [{[testStatus, Idx1], ?createAndGo},
		     {[testDescr, Idx1], "row 1"}]),
    ?DBG("implied_test -> (send) create row 2 '~s' in table 1",[Idx2]),

    ?S([{[testStatus, Idx2], i, ?createAndGo}, {[testDescr, Idx2],s,"row 2"}]),
    ?line expect(2, [{[testStatus, Idx2], ?createAndGo},
		     {[testDescr, Idx2], "row 2"}]),
    ?DBG("implied_test -> get-next(testDescr)",[]),

    ?GN([[testDescr]]),
    ?line expect(3, [{[testDescr,Idx1], "row 1"}]),
    ?DBG("implied_test -> get-next(testDescr) of row 1",[]),

    ?GN([[testDescr,Idx1]]),
    ?line expect(4, [{[testDescr,Idx2], "row 2"}]),

    % Delete the rows
    ?DBG("implied_test -> (send) delete row 1 '~s' from table 1",[Idx1]),
    ?S([{[testStatus, Idx1], i, ?destroy}]),
    ?line expect(5, [{[testStatus, Idx1], ?destroy}]),

    ?DBG("implied_test -> (send) delete row 2 '~s' from table 1",[Idx2]),
    ?S([{[testStatus, Idx2], i, ?destroy}]),
    ?line expect(6, [{[testStatus, Idx2], ?destroy}]),

    %% Try the same in other table
    Idx3 = [1, "apa"],
    Idx4 = [1, "qq"],
    ?DBG("implied_test -> (send) create row 1 '~s' in table 2",[Idx3]),
    ?S([{[testStatus2, Idx3], i, ?createAndGo}, {[testDescr2,Idx3],s,"row 1"}]),
    ?line expect(1, [{[testStatus2, Idx3], ?createAndGo},
		     {[testDescr2, Idx3], "row 1"}]),

    ?DBG("implied_test -> (send) create row 2 '~s' in table 2",[Idx4]),
    ?S([{[testStatus2, Idx4], i, ?createAndGo}, 
	{[testDescr2,Idx4],s,"row 2"}]),
    ?line expect(2, [{[testStatus2, Idx4], ?createAndGo},
		     {[testDescr2, Idx4], "row 2"}]),

    ?DBG("implied_test -> get-next(testDescr2)",[]),
    ?GN([[testDescr2]]),
    ?line expect(3, [{[testDescr2,Idx3], "row 1"}]),
    ?DBG("implied_test -> get-next(testDescr2) of row 1",[]),

    ?GN([[testDescr2,Idx3]]),
    ?line expect(4, [{[testDescr2,Idx4], "row 2"}]),

    % Delete the rows
    ?DBG("implied_test -> (send) delete row 1 '~s' from table 2",[Idx3]),
    ?S([{[testStatus2, Idx3], i, ?destroy}]),
    ?line expect(5, [{[testStatus2, Idx3], ?destroy}]),

    ?DBG("implied_test -> (send) delete row 2 '~s' from table 2",[Idx4]),
    ?S([{[testStatus2, Idx4], i, ?destroy}]),
    ?line expect(6, [{[testStatus2, Idx4], ?destroy}]),

    snmpa:verbosity(MA,log),

    ?LOG("implied_test -> done",[]).
    

%% -- loop_mib --

%%-----------------------------------------------------------------
%% Loop through entire MIB, to make sure that all instrum. funcs
%% works.
%% Load all std mibs that are not loaded by default.
%%-----------------------------------------------------------------
loop_mib(suite) -> [];
loop_mib(Config) when list(Config) ->
    ?P1(loop_mib),
    %% snmpa:verbosity(master_agent,debug),
    %% snmpa:verbosity(mib_server,info),
    {SaNode, MgrNode, MibDir} = ?INIT_CASE(Config),
    ?DBG("loop_mib -> "
	 "~n   SaNode:  ~p"
	 "~n   MgrNode: ~p"
	 "~n   MibDir:  ~p", [SaNode, MgrNode, MibDir]),

    ?DBG("loop_mib -> load mib SNMP-COMMUNITY-MIB",[]),
    ?line load_master_std("SNMP-COMMUNITY-MIB"),
    ?DBG("loop_mib -> load mib SNMP-MPD-MIB",[]),
    ?line load_master_std("SNMP-MPD-MIB"),
    ?DBG("loop_mib -> load mib SNMP-TARGET-MIB",[]),
    ?line load_master_std("SNMP-TARGET-MIB"),
    ?DBG("loop_mib -> load mib SNMP-NOTIFICATION-MIB",[]),
    ?line load_master_std("SNMP-NOTIFICATION-MIB"),
    ?DBG("loop_mib -> load mib SNMP-FRAMEWORK-MIB",[]),
    ?line load_master_std("SNMP-FRAMEWORK-MIB"),
    ?DBG("loop_mib -> load mib SNMP-VIEW-BASED-ACM-MIB",[]),
    ?line load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    ?DBG("loop_mib -> try",[]),

    ?TRY_TEST1(loop_mib),

    ?DBG("loop_mib -> unload mib SNMP-COMMUNITY-MIB",[]),
    ?line unload_master("SNMP-COMMUNITY-MIB"),
    ?DBG("loop_mib -> unload mib SNMP-MPD-MIB",[]),
    ?line unload_master("SNMP-MPD-MIB"),
    ?DBG("loop_mib -> unload mib SNMP-TARGET-MIB",[]),
    ?line unload_master("SNMP-TARGET-MIB"),
    ?DBG("loop_mib -> unload mib SNMP-NOTIFICATION-MIB",[]),
    ?line unload_master("SNMP-NOTIFICATION-MIB"),
    ?DBG("loop_mib -> unload mib SNMP-FRAMEWORK-MIB",[]),
    ?line unload_master("SNMP-FRAMEWORK-MIB"),
    ?DBG("loop_mib -> unload mib SNMP-VIEW-BASED-ACM-MIB",[]),
    ?line unload_master("SNMP-VIEW-BASED-ACM-MIB"),
    %% snmpa:verbosity(master_agent,log),
    %% snmpa:verbosity(mib_server,silence),
    ?LOG("loop_mib -> done",[]).
    
%% Req. As many mibs all possible
loop_mib() ->
    ?DBG("loop_mib -> entry",[]),
    N = loop_it([1,1], 0),
    ?P3("found ~w varibles\n", [N]),
    ?line N = if N < 100 -> 100;
		 true -> N
	      end.
	    
loop_it(Oid, N) ->
    ?DBG("loop_it -> entry with"
	 "~n   Oid: ~p"
	 "~n   N:   ~p", [Oid,N]),
    case get_next_req([Oid]) of
	#pdu{type='get-response', error_status=noError, error_index=0,
	     varbinds=[#varbind{oid = NOid,value = Value}]} when NOid > Oid ->
	    ?DBG("loop_it -> "
		   "~n   NOid:  ~p"
		   "~n   Value: ~p",[NOid,Value]),
	    ?line [Value2] = get_req(1, [NOid]), % must not be same
	    ?DBG("loop_it_1 -> "
		 "~n   Value2: ~p",[Value2]),
	    loop_it(NOid, N+1);

	#pdu{type='get-response', error_status=noSuchName, error_index=1,
	     varbinds=[_]} ->
	    ?DBG("loop_it -> done",[]),
	    N;

	#pdu{type = Type, error_status = Err, error_index = Idx,
	     varbinds = Vbs} ->
	    exit({unexpected_pdu, ?LINE, Type, Err, Idx, Vbs})

    end.
	    

%% -- api --

api(suite) -> [];
api(Config) when list(Config) ->
    ?P1(api),
    ?INIY_CASE(Config),

    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),

    ?TRY_TEST2(api_test, [node()]),

    ?line unload_master("OLD-SNMPEA-MIB").

%% Req. OLD-SNMPEA-MIB
api_test(MaNode) ->
    ?line {value, OID} = ?RPC(MaNode, name_to_oid, [intAgentIpAddress]),
    ?line {value, intAgentIpAddress} = ?RPC(MaNode, oid_to_name, [OID]),
    ?line false = ?RPC(MaNode, name_to_oid, [intAgentIpAddres]),
    ?line false = ?RPC(MaNode, oid_to_name, [[1,5,32,3,54,3,3,34,4]]),
    ?line {value, 2} = ?RPC(MaNode, enum_to_int, [intViewType, excluded]),
    ?line {value, excluded} = ?RPC(MaNode, int_to_enum, [intViewType, 2]),
    ?line false = ?RPC(MaNode, enum_to_int, [intViewType, exclude]),
    ?line false = ?RPC(MaNode, enum_to_int, [intAgentIpAddress, exclude]),
    ?line false = ?RPC(MaNode, enum_to_int, [intAgentIpAddre, exclude]),
    ?line false = ?RPC(MaNode, int_to_enum, [intViewType, 3]),
    ?line false = ?RPC(MaNode, int_to_enum, [intAgentIpAddress, 2]),
    ?line false = ?RPC(MaNode, int_to_enum, [intAgentIpAddre, 2]),
    ?line {value, active} = ?RPC(MaNode, int_to_enum, ['RowStatus', ?active]),
    ?line {value, ?destroy} = 
	?RPC(MaNode, enum_to_int, ['RowStatus', destroy]),
    ?line false = ?RPC(MaNode, enum_to_int, ['RowStatus', xxxdestroy]),
    ?line false = ?RPC(MaNode, enum_to_int, ['xxRowStatus', destroy]),
    ?line false = ?RPC(MaNode, int_to_enum, ['RowStatus', 25]),
    ?line false = ?RPC(MaNode, int_to_enum, ['xxRowStatus', 1]),
    ?line case snmp:date_and_time() of
	      List when list(List), length(List) == 8 -> ok;
	      List when list(List), length(List) == 11 -> ok
    end.


%% -- subagent --

subagent(suite) -> [];
subagent(Config) when list(Config) ->
    ?P1(subagent),
    {SaNode, _MgrNode, MibDir} = ?INIT_CASE(Config),

    ?line {ok, SA} = ?START_SA(SaNode, ?klas1, "Klas1"),

    ?TRY_TEST1(load_test_sa),
    
    ?P2("Testing unregister subagent [~w]...", [SA]),
    MA = whereis(snmp_master_agent),
    ?RPC(SaNode, unregister_subagent, [MA, SA]),
    ?TRY_TEST1(unreg_test),

    ?P2("Loading previous subagent mib in master and testing..."),
    ?line ok = snmpa:load_mibs(MA, [MibDir ++ "Klas1"]),
    ?TRY_TEST1(load_test),

    ?P2("Unloading previous subagent mib in master and testing..."),
    ?line ok = snmpa:unload_mibs(MA, [MibDir ++ "Klas1"]),

    ?TRY_TEST1(unreg_test),

    ?P2("Testing register subagent..."),
    ?RPC(SaNode, register_subagent, [MA, ?klas1, SA]),
    ?TRY_TEST1(load_test_sa),

    ?line ?STOP_SA(SA),
    ?TRY_TEST1(unreg_test).
    
%% Req. Klas1
load_test_sa() ->
    ?GN([[?v1_2(sysServices,sysORLastChange), 0]]),
    ?line expect(1, [{[fname,0], any}]).
    
unreg_test() ->
    ?GN([[?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(1, [{[snmpInPkts, 0], any}]).

load_test() ->
    ?GN([[?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(1, [{[fname,0], ""}]).


%% -- mnesia --

mnesia(suite) -> [];
mnesia(Config) when list(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    p("Starting subagent with mnesia impl..."),
    {ok, SA} = start_subagent(SaNode, ?klas2, "Klas2"),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),

    ?TRY_TEST1(big_test_2),

    p("Testing unregister subagent..."),
    MA = whereis(snmp_master_agent),
    rpc:call(SaNode, snmp, unregister_subagent, [MA, SA]),
    ?TRY_TEST1(unreg_test),
    ?line unload_master("OLD-SNMPEA-MIB"),
    ?line stop_subagent(SA).


%% -- multiple_reqs --

multiple_reqs(suite) ->
    {req, [], {conf, init_mul, mul_cases(), finish_mul}}.

mul_cases() ->
    [mul_get, mul_get_err, mul_next, mul_next_err, mul_set_err].
    
init_mul(Config) when list(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1"),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    [{mul_sub, SA} | Config].

finish_mul(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    SA = ?config(mul_sub, Config),
    
    ?line unload_master("OLD-SNMPEA-MIB"),
    ?line stop_subagent(SA),
    lists:keydelete(mul_sub, 1, Config).
    

%% -- mul_get --

mul_get(suite) -> [];
mul_get(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    p("Testing multiple get..."),
    ?TRY_TEST1(do_mul_get).

%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_get() ->
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?line expect(3, [{[fname,0], "test set"}]),
    g([[sysDescr,0], Key1c4, [fname,0],Key1c3,
	       [sysName,0]]),
    ?line expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
		     {Key1c4, 2},
		     {[fname,0], "test set"},
		     {Key1c3, 2},
		     {[sysName,0], "test"}]),
    g([[1,3,7,1], Key1c4, [sysDescr,0], [1,3,7,2], Key1c3, [sysDescr,0]]),
    ?line ?v1_2(expect(2, noSuchName, [1,4], any),
		expect(2, [{[1,3,7,1], noSuchObject},
			   {Key1c4, 2},
			   {[sysDescr,0], "Erlang SNMP agent"},
			   {[1,3,7,2], noSuchObject},
			   {Key1c3, 2},
			   {[sysDescr,0], "Erlang SNMP agent"}])).


%% -- mul_get_err --

mul_get_err(suite) -> [];
mul_get_err(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    p("Testing multiple get with error..."),
    ?TRY_TEST1(do_mul_get_err).

%% Req. v1, system group, Klas1, OLD-SNMPEA-MIB, *ej* Klas3.
do_mul_get_err() ->
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?line expect(3, [{[fname,0], "test set"}]),
    g([[sysDescr,0],Key1c4,[fname,0], Key1c3, [sysName,2]]),
    ?line ?v1_2(expect(1, noSuchName, 5, any),
		expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
			   {Key1c4, 2},
			   {[fname,0], "test set"},
			   {Key1c3, 2},
			   {[sysName,2], noSuchInstance}])),
    g([[sysDescr,0],Key1c4,[fname3,0], Key1c3, [sysName,1]]),
    ?line ?v1_2(expect(1, noSuchName, [3,5], any),
		expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
			   {Key1c4, 2},
			   {[fname3,0], noSuchObject},
			   {Key1c3, 2},
			   {[sysName,1], noSuchInstance}])).


%% -- mul_next --

mul_next(suite) -> [];
mul_next(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    p("Testing multiple next..."),
    ?TRY_TEST1(do_mul_next).

%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_next() ->
    Key1c3s = [intCommunityEntry,[3],get(mip),is("publi")],
    Key1c4s = [intCommunityEntry,[4],get(mip),is("publi")],
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?line expect(3, [{[fname,0], "test set"}]),
    gn([[sysDescr], Key1c4s, [fname],Key1c3s,[sysName]]),
    ?line expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
	       {Key1c4, 2}, {[fname,0], "test set"},
	       {Key1c3, 2}, {[sysName,0], "test"}]).


%% -- mul_next_err --

mul_next_err(suite) -> [];
mul_next_err(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    p("Testing multiple next..."),
    ?TRY_TEST1(do_mul_next_err).

%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_next_err() ->
    Key1c3s = [intCommunityEntry,[3],get(mip),is("publi")],
    Key1c4s = [intCommunityEntry,[4],get(mip),is("publi")],
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?line expect(3, [{[fname,0], "test set"}]),
    gn([[sysDescr], Key1c4s, [1,3,6,999], [fname],[1,3,90], Key1c3s,[sysName]]),
    ?line ?v1_2(expect(1, noSuchName, [3,5], any),
		expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
			   {Key1c4, 2},
			   {[1,3,6,999], endOfMibView},
			   {[fname,0], "test set"},
			   {[1,3,90], endOfMibView},
			   {Key1c3, 2},
			   {[sysName,0], "test"}])).
		

%% -- mul_set --

mul_set(suite) -> [];
mul_set(Config) when list(Config) ->
    ?P(mul_set),
    ?INIT_CASE(Config),
    
    ?TRY_TEST1(do_mul_set).

%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_set() ->
    p("Adding one row in subagent table, and one in master table"),
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    s([{[friendsEntry, [2, 3]], "kompis3"},
       {NewKeyc3, 2},
       {[sysLocation,0], "new_value"},
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2},
       {[friendsEntry, [3, 3]], ?createAndGo}]),
    ?line expect(1, [{[friendsEntry, [2, 3]], "kompis3"},
	       {NewKeyc3, 2},
	       {[sysLocation,0], "new_value"},
	       {NewKeyc5, ?createAndGo},
	       {NewKeyc4, 2},
	       {[friendsEntry, [3, 3]], ?createAndGo}]),
    g([[friendsEntry, [2, 3]],
	       [sysLocation,0],
	       [friendsEntry, [3, 3]]]),
    ?line expect(2, [{[friendsEntry, [2, 3]], "kompis3"},
	       {[sysLocation,0], "new_value"},
	       {[friendsEntry, [3, 3]], ?active}]),
    g([NewKeyc4]),
    ?line expect(3, [{NewKeyc4, 2}]),
    s([{[friendsEntry, [3, 3]], ?destroy},
       {NewKeyc5, ?destroy}]),
    ?line expect(4, [{[friendsEntry, [3, 3]], ?destroy},
	       {NewKeyc5, ?destroy}]).


%% -- mul_set_err --

mul_set_err(suite) -> [];
mul_set_err(Config) when list(Config) ->
    ?P(mul_set_err),
    ?INIT_CASE(Config),
    
    ?TRY_TEST1(do_mul_set_err).

%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_set_err() ->
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    p("Adding one row in subagent table, and one in master table"),
    s([{[friendsEntry, [2, 3]], s, "kompis3"},
       {NewKeyc3, 2},
       {[sysUpTime,0], 45},   % sysUpTime (readOnly)
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2},
       {[friendsEntry, [3, 3]], ?createAndGo}]),
    ?line expect(1, ?v1_2(noSuchName, notWritable), 3, any),
    g([[friendsEntry, [2, 3]]]),
    ?line ?v1_2(expect(2, noSuchName, 1, any),
		expect(2, [{[friendsEntry, [2,3]], noSuchInstance}])),
    g([NewKeyc4]),
    ?line ?v1_2(expect(3, noSuchName, 1, any),
		expect(3, [{NewKeyc4, noSuchInstance}])).


%% -- sa_register --

sa_register(suite) -> [];
sa_register(Config) when list(Config) ->
    ?P1(sa_register), 
    {SaNode, _MgrNode, MibDir} = ?INIT_CASE(Config),

    ?DBG("sa_register -> start subagent", []),
    ?line {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1"),

    ?DBG("sa_register -> unregister subagent", []),
    ?P2("Testing unregister subagent (2)..."),
    MA = whereis(snmp_master_agent),
    rpc:call(SaNode, snmp, unregister_subagent, [MA, ?klas1]),
    ?TRY_TEST1(unreg_test),

    ?P2("Loading SA-MIB..."),
    ?DBG("sa_register -> unload mibs", []),
    snmpa:unload_mibs(SA, [MibDir ++ "Klas1"]),
    ?DBG("sa_register -> unload mibs", []),
    snmpa:load_mibs(SA, [MibDir ++ "SA-MIB"]),
    ?DBG("sa_register -> register subagent", []),
    rpc:call(SaNode, snmp, register_subagent, [MA,?sa,SA]),
    ?TRY_TEST1(sa_mib),

    ?DBG("sa_register -> stop subagent", []),
    ?line stop_subagent(SA).
    
%% Req. SA-MIB
sa_mib() ->
    g([[sa, [2,0]]]),
    ?line expect(1, [{[sa, [2,0]], 3}]),
    s([{[sa, [1,0]], s, "sa_test"}]),
    ?line expect(2, [{[sa, [1,0]], "sa_test"}]).


%% -- v1_trap --

v1_trap(suite) -> [];
v1_trap(Config) when list(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    p("Testing trap sending from master agent..."),
    MA = whereis(snmp_master_agent),

    ?line load_master("TestTrap"),
    ?line load_master("TestTrapv2"),

    ?TRY_TEST2(ma_trap1, [MA]),
    ?TRY_TEST2(ma_trap2, [MA]),
    ?TRY_TEST2(ma_v2_2_v1_trap, [MA]),
    ?TRY_TEST2(ma_v2_2_v1_trap2, [MA]),

    p("Testing trap sending from subagent..."),
    ?TRY_TEST2(sa_trap1, [SA]),
    ?TRY_TEST2(sa_trap2, [SA]),
    ?TRY_TEST2(sa_trap3, [SA]),
    
    ?line unload_master("TestTrap"),
    ?line unload_master("TestTrapv2"),

    ?line stop_subagent(SA).

ma_trap1(MA) ->
    snmpa:send_trap(MA, testTrap2, "standard trap"),
    ?line expect(1, trap, [system], 6, 1, [{[system, [4,0]],
				    "{mbj,eklas}@erlang.ericsson.se"}]),
    snmpa:send_trap(MA, testTrap1, "standard trap"),
    ?line expect(2, trap, [1,2,3] , 1, 0, [{[system, [4,0]],
				      "{mbj,eklas}@erlang.ericsson.se"}]).

ma_trap2(MA) ->
    snmpa:send_trap(MA,testTrap2,"standard trap",[{sysContact,"pelle"}]),
    ?line expect(3, trap, [system], 6, 1, [{[system, [4,0]], "pelle"}]).

ma_v2_2_v1_trap(MA) ->
    snmpa:send_trap(MA,testTrapv22,"standard trap",[{sysContact,"pelle"}]),
    ?line expect(3, trap, [system], 6, 1, [{[system, [4,0]], "pelle"}]).    

ma_v2_2_v1_trap2(MA) ->
    snmpa:send_trap(MA,linkUp,"standard trap",[{ifIndex, [1], 1},
					      {ifAdminStatus, [1], 1},
					      {ifOperStatus, [1], 2}]),
    ?line expect(3, trap, [1,2,3], 3, 0, [{[ifIndex, 1], 1},
					 {[ifAdminStatus, 1], 1},
					 {[ifOperStatus, 1], 2}]).    

sa_trap1(SA) ->
    snmpa:send_trap(SA, saTrap, "standard trap"),
    ?line expect(4, trap, [ericsson], 6, 1, [{[system, [4,0]],
				      "{mbj,eklas}@erlang.ericsson.se"},
				     {[sa, [1,0]], "sa_test"}]).

sa_trap2(SA) ->
    snmpa:send_trap(SA, saTrap, "standard trap",[{sysContact,"pelle"}]),
    ?line expect(5, trap, [ericsson], 6, 1, [{[system, [4,0]],
				      "pelle"},
				     {[sa, [1,0]], "sa_test"}]).

sa_trap3(SA) ->
    snmpa:send_trap(SA, saTrap2, "standard trap",
			 [{intViewSubtree, [4], [1,2,3,4]}]),
    ?line expect(6, trap, [ericsson], 6, 2, [{[system, [4,0]],
				      "{mbj,eklas}@erlang.ericsson.se"},
				     {[sa, [1,0]], "sa_test"},
				     {[intViewSubtree,4],[1,2,3,4]}]).

ma_v2_trap1(MA) ->
    ?DBG("ma_v2_traps -> entry with MA = ~p => "
	   "send standard trap: testTrapv22",[MA]),
    snmpa:send_trap(MA, testTrapv22, "standard trap"),
    ?line expect(1, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?system ++ [0,1]}]),
    ?DBG("ma_v2_traps -> send standard trap: testTrapv21",[]),
    snmpa:send_trap(MA, testTrapv21, "standard trap"),
    ?line expect(2, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?snmp ++ [1]}]).

ma_v2_trap2(MA) ->
    snmpa:send_trap(MA,testTrapv22,"standard trap",[{sysContact,"pelle"}]),
    ?line expect(3, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?system ++ [0,1]},
			     {[system, [4,0]], "pelle"}]).

ma_v1_2_v2_trap(MA) ->
    snmpa:send_trap(MA,linkDown,"standard trap",[{ifIndex, [1], 1}]),
    ?line expect(2, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?snmpTraps ++ [3]},
			     {[ifIndex, 1], 1},
			     {[snmpTrapEnterprise, 0], [1,2,3]}]).

    
ma_v1_2_v2_trap2(MA) ->
    snmpa:send_trap(MA,testTrap2,"standard trap",[{sysContact,"pelle"}]),
    ?line expect(3, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?system ++ [0,1]},
			     {[system, [4,0]], "pelle"},
			     {[snmpTrapEnterprise, 0], ?system}]).
    

sa_v1_2_v2_trap1(SA) ->
    snmpa:send_trap(SA, saTrap, "standard trap"),
    ?line expect(4, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?ericsson ++ [0, 1]},
			     {[system, [4,0]],
			      "{mbj,eklas}@erlang.ericsson.se"},
			     {[sa, [1,0]], "sa_test"},
			     {[snmpTrapEnterprise, 0], ?ericsson}]).

sa_v1_2_v2_trap2(SA) ->
    snmpa:send_trap(SA, saTrap, "standard trap",[{sysContact,"pelle"}]),
    ?line expect(4, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?ericsson ++ [0, 1]},
			     {[system, [4,0]], "pelle"},
			     {[sa, [1,0]], "sa_test"},
			     {[snmpTrapEnterprise, 0], ?ericsson}]).
			     

sa_v1_2_v2_trap3(SA) ->
    snmpa:send_trap(SA, saTrap2, "standard trap",
			 [{intViewSubtree, [4], [1,2,3,4]}]),
    ?line expect(4, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?ericsson ++ [0, 2]},
			     {[system, [4,0]],
			      "{mbj,eklas}@erlang.ericsson.se"},
			     {[sa, [1,0]], "sa_test"},
			     {[intViewSubtree,4],[1,2,3,4]},
			     {[snmpTrapEnterprise, 0], ?ericsson}]).
			     

%% -- sa_error --

sa_error(suite) -> [];
sa_error(Config) when list(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    ?line {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    p("Testing sa bad value (is_set_ok)..."),
    ?TRY_TEST1(sa_errs_bad_value),

    p("Testing sa gen err (set)..."),
    ?TRY_TEST1(sa_errs_gen_err),

    p("Testing too big..."),
    ?TRY_TEST1(sa_too_big),

    ?line unload_master("OLD-SNMPEA-MIB"),
    stop_subagent(SA).

%% Req. SA-MIB, OLD-SNMPEA-MIB
sa_errs_bad_value() ->
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    s([{NewKeyc3, 2},
       {[sa, [2,0]], 5}, % badValue (i is_set_ok)
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2}]),
    ?line expect(1, badValue, 2, any),   
    s([{NewKeyc3, 2},
       {[sa, [2,0]], 6}, % wrongValue (i is_set_ok)
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2}]),
    ?line expect(1, ?v1_2(badValue, wrongValue), 2, any),   
    g([NewKeyc4]),
    ?line ?v1_2(expect(2, noSuchName, 1, any),
		expect(2, [{NewKeyc4, noSuchInstance}])).

%% Req. SA-MIB, OLD-SNMPEA-MIB
sa_errs_gen_err() ->
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    s([{NewKeyc3, 2},{NewKeyc4, 2},
       {NewKeyc5, ?createAndGo}, {[sa, [3,0]], 5}]),
    ?line expect(1, genErr, 4, any),
% The row might have been added; we don't know.
% (as a matter of fact we do - it is added, because the agent
% first sets its own vars, and then th SAs. Lets destroy it.
    s([{NewKeyc5, ?destroy}]),
    ?line expect(2, [{NewKeyc5, ?destroy}]).

%% Req. SA-MIB, OLD-SNMPEA-MIB
sa_too_big() ->
    g([[sa, [4,0]]]),
    ?line expect(1, tooBig).


%% -- next_across_sa --

next_across_sa(suite) -> [];
next_across_sa(Config) when list(Config) ->
    {SaNode, _MgrNode, MibDir} = init_case(Config),
    MA = whereis(snmp_master_agent),

    ?line {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    p("Loading another subagent mib..."),
    ?line ok = snmpa:load_mibs(SA, [MibDir ++ "Klas1"]),

    rpc:call(SaNode, snmp, register_subagent, [MA, ?klas1, SA]),
    ?TRY_TEST1(load_test_sa),
    
    p("Testing next across subagent (endOfMibView from SA)..."),
    ?TRY_TEST1(next_across_sa),

    p("Unloading mib"),
    snmpa:unload_mibs(SA, [MibDir ++ "Klas1"]),
    rpc:call(SaNode, snmp, unregister_subagent, [MA, ?klas1]),
    ?TRY_TEST1(unreg_test),

    p("Starting another subagent"),
    ?line {ok, SA2} = start_subagent(SaNode, ?klas1, "Klas1"),
    p("Testing next across subagent (wrong prefix from SA)..."),
    ?TRY_TEST1(next_across_sa),
    
    stop_subagent(SA),
    stop_subagent(SA2).

%% Req. Klas1, system group, snmp group (v1/v2)
next_across_sa() ->
    gn([[sysDescr],[klas1,5]]),
    ?line expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
		     {[snmpInPkts, 0], any}]).


%% -- undo --

undo(suite) -> [];
undo(Config) when list(Config) ->
    {SaNode, _MgrNode, MibDir} = init_case(Config),
    MA = whereis(snmp_master_agent),

    ?line {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    p("Testing undo phase at master agent..."),
    ?line ok = snmpa:load_mibs(MA, [MibDir ++ "Klas3"]),
    ?line ok = snmpa:load_mibs(MA, [MibDir ++ "Klas4"]),
    ?TRY_TEST1(undo_test),
    ?TRY_TEST1(api_test2),
    ?line ok = snmpa:unload_mibs(MA, [MibDir ++ "Klas3"]),

    p("Testing bad return values from instrum. funcs..."),
    ?TRY_TEST1(bad_return),

    ?line ok = snmpa:unload_mibs(MA, [MibDir ++ "Klas4"]),

    p("Testing undo phase at subagent..."),
    ?line ok = snmpa:load_mibs(SA, [MibDir ++ "Klas3"]),
    ?line ok = snmpa:load_mibs(SA, [MibDir ++ "Klas4"]),
    ?line ok = snmpa:register_subagent(MA, ?klas3, SA),
    ?line ok = snmpa:register_subagent(MA, ?klas4, SA),
    ?TRY_TEST1(undo_test),
    ?TRY_TEST1(api_test3),

    p("Testing undo phase across master/subagents..."),
    ?TRY_TEST1(undo_test),
    ?TRY_TEST1(api_test3),
    stop_subagent(SA).

%% snmp_test_mgr:s([{[fStatus3, 1], 4}, {[fname3,0], "ok"}]). -> noError
%% snmp_test_mgr:s([{[fStatus3, 1], 4}, {[fname3,0], "hoj"}]). -> {badValue, 2}
%% snmp_test_mgr:s([{[fStatus3, 3], 4}, {[fname3,0], "hoj"}]). -> {genErr, 1}
%% snmp_test_mgr:s([{[fStatus3, 4], 4}, {[fname3,0], "ok"}]). -> {genErr, 1}
%% snmp_test_mgr:s([{[fStatus3, 4], 4}, {[fname3,0], "ufail"}]). -> {genErr, 1}
%% snmp_test_mgr:s([{[fStatus3, 1], 4}, {[fname3,0], "xfail"}]). -> {genErr, 2}
%% Req. Klas3, Klas4
undo_test() ->
    s([{[fStatus3, 1], 4}, {[fname3,0], "ok"}]),
    ?line expect(1, [{[fStatus3, 1], 4}, {[fname3,0], "ok"}]),
    s([{[fStatus3, 1], 4}, {[fname3,0], "hoj"}]),
    ?line expect(2, ?v1_2(badValue, inconsistentValue), 2, any), 
    s([{[fStatus3, 3], 4}, {[fname3,0], "hoj"}]),
    ?line expect(3, ?v1_2(genErr, undoFailed), 1, any), 
    s([{[fStatus3, 4], 4}, {[fname3,0], "ok"}]),
    ?line expect(4, ?v1_2(genErr, commitFailed), 1, any), 
    %% unfortunatly we don't know if we'll get undoFailed or commitFailed.
    %% it depends on which order the agent traverses the varbind list.
    %%    s([{[fStatus3, 4], 4}, {[fname3,0], "ufail"}]),
    %%    ?line expect(5, ?v1_2(genErr, undoFailed), 1, any),
    s([{[fStatus3, 1], 4}, {[fname3,0], "xfail"}]),
    ?line expect(6, genErr, 2, any).
    
%% Req. Klas3, Klas4
bad_return() ->
    g([[fStatus4,4],
       [fName4,4]]),
    ?line expect(4, genErr, 2, any),
    g([[fStatus4,5],
       [fName4,5]]),
    ?line expect(5, genErr, 1, any),
    g([[fStatus4,6],
       [fName4,6]]),
    ?line expect(6, genErr, 2, any),
    gn([[fStatus4,7],
       [fName4,7]]),
    ?line expect(7, genErr, 2, any),
    gn([[fStatus4,8],
       [fName4,8]]),
    ?line expect(8, genErr, 1, any),
    gn([[fStatus4,9],
       [fName4,9]]),
    ?line expect(9, genErr, 2, any).


%% -- standard_mibs --

%%%-----------------------------------------------------------------
%%% Test the implementation of standard mibs.
%%% We should *at least* try to GET all variables, just to make
%%% sure the instrumentation functions work.
%%% Note that many of the functions in the standard mib is
%%% already tested by the normal tests.
%%%-----------------------------------------------------------------
standard_mibs(suite) ->
    [snmp_standard_mib, 
     snmp_community_mib,
     snmp_framework_mib,
     snmp_target_mib, 
     snmp_notification_mib,
     snmp_view_based_acm_mib].


%% -- snmp_standard_mib --

%%-----------------------------------------------------------------
%% For this test, the agent is configured for v1.
%% o  Test the counters and control objects in SNMP-STANDARD-MIB
%%-----------------------------------------------------------------
snmp_standard_mib(suite) -> [];
snmp_standard_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?DBG("snmp_standard_mib -> std_mib_init", []),
    ?TRY_TEST1(std_mib_init),

    ?DBG("snmp_standard_mib -> std_mib_a", []),
    InBadVsns = ?TRY_TEST1(std_mib_a),
    put(vsn, v2),
    ?DBG("snmp_standard_mib -> std_mib_read", []),
    ?TRY_TEST1(std_mib_read),
    put(vsn, v1),

    ?DBG("snmp_standard_mib -> std_mib_b (~w)", [InBadVsns]),
    Bad = ?TRY_TEST2(std_mib_b, [InBadVsns]),
    ?DBG("snmp_standard_mib -> std_mib_read (community: 'bad community')", []),
    ?TRY_TEST3(std_mib_read, [], [{community, "bad community"}]),
    ?DBG("snmp_standard_mib -> std_mib_write (community: 'public')", []),
    ?TRY_TEST3(std_mib_write, [], [{community, "public"}]),
    ?DBG("snmp_standard_mib -> std_mib_asn_err", []),
    ?TRY_TEST1(std_mib_asn_err),
    ?DBG("snmp_standard_mib -> std_mib_c (~w)", [Bad]),
    ?TRY_TEST2(std_mib_c, [Bad]),
    ?DBG("snmp_standard_mib -> std_mib_a", []),
    ?TRY_TEST1(standard_mib_a),
    
    ?DBG("snmp_standard_mib -> std_mib_finish", []),
    ?TRY_TEST1(std_mib_finish),
    ?DBG("snmp_standard_mib -> std_mib_test_finish", []),
    ?TRY_TEST3(standard_mib_test_finish, [], [{community, "bad community"}]).

%% Req. SNMP-STANDARD-MIB
standard_mib_a() ->
    ?line [OutPkts] = get_req(2, [[snmpOutPkts,0]]),
    ?line [OutPkts2] = get_req(3, [[snmpOutPkts,0]]),
    ?line OutPkts2 = OutPkts + 1,
    %% There are some more counters we could test here, but it's not that
    %% important, since they are removed from SNMPv2-MIB.
    ok.

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_init() ->
    %% disable authentication failure traps.  (otherwise w'd get many of
    %% them - this is also a test to see that it works).
    s([{[snmpEnableAuthenTraps,0], 2}]),
    ?line expect(1, [{[snmpEnableAuthenTraps, 0], 2}]).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_finish() ->
    %% enable again
    s([{[snmpEnableAuthenTraps,0], 1}]),
    ?line expect(1, [{[snmpEnableAuthenTraps, 0], 1}]).

%% Req. SNMP-STANDARD-MIB
standard_mib_test_finish() ->
    %% force a authenticationFailure
    std_mib_write(),
    %% check that we got a trap
    ?line expect(2, trap, [1,2,3], 4, 0, []).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_read() ->
    ?DBG("std_mib_read -> entry", []),
    g([[sysUpTime,0]]), % try a bad <something>; msg dropped, no reply
    ?DBG("std_mib_read -> await timeout (i.e. no reply)", []),
    ?line expect(1, timeout). % make sure we don't get a trap!


%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_write() ->
    ?DBG("std_mib_write -> entry", []),
    s([{[sysLocation, 0], "new_value"}]).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_asn_err() ->
    ?SEND_BYTES([48,99,67,12,0,0,0,0,0,0,5]).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_a() ->
    ?line [InPkts] = get_req(2, [[snmpInPkts,0]]),
    ?line [InPkts2] = get_req(3, [[snmpInPkts,0]]),
    ?line InPkts2 = InPkts + 1,

    ?line [InBadVsns] = get_req(4, [[snmpInBadVersions,0]]),
    InBadVsns.

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_b(InBadVsns) ->
    ?line [InBadVsns2] = get_req(1, [[snmpInBadVersions,0]]),
    ?line InBadVsns2 = InBadVsns + 1,
    ?line [InPkts] = get_req(2, [[snmpInPkts,0]]),
    ?line [InPkts2] = get_req(3, [[snmpInPkts,0]]),
    ?line InPkts2 = InPkts + 1,
    ?line [InBadCommunityNames, InBadCommunityUses, InASNErrs] =
	get_req(4, [[snmpInBadCommunityNames,0],
		    [snmpInBadCommunityUses,0],
		    [snmpInASNParseErrs, 0]]),
    {InBadCommunityNames, InBadCommunityUses, InASNErrs}.
    
%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_c({InBadCommunityNames, InBadCommunityUses, InASNErrs}) ->
    ?line [InBadCommunityNames2, InBadCommunityUses2, InASNErrs2] =
	get_req(1, [[snmpInBadCommunityNames,0],
		    [snmpInBadCommunityUses,0],
		    [snmpInASNParseErrs, 0]]),
    ?line InBadCommunityNames2 = InBadCommunityNames + 1,
    ?line InBadCommunityUses2 = InBadCommunityUses + 1,
    ?line InASNErrs2 = InASNErrs + 1.


%% -- snmp_community_mib -- 

%%-----------------------------------------------------------------
%% o  Bad community uses/name is tested already
%%    in SNMPv2-MIB and STANDARD-MIB.
%% o  Test add/deletion of rows.
%%-----------------------------------------------------------------
snmp_community_mib(suite) -> [];
snmp_community_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master_std("SNMP-COMMUNITY-MIB"),
    ?TRY_TEST1(snmp_community_mib),
    ?line unload_master("SNMP-COMMUNITY-MIB").

snmp_community_mib_2(X) -> snmp_community_mib(X).

%% Req. SNMP-COMMUNITY-MIB
snmp_community_mib() ->
    ?INF("NOT YET IMPLEMENTED", []),
    nyi.


%% -- snmp_framework_mib -- 

%%-----------------------------------------------------------------
%% o  Test engine boots / time
%%-----------------------------------------------------------------
snmp_framework_mib(suite) -> [];
snmp_framework_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master_std("SNMP-FRAMEWORK-MIB"),
    ?TRY_TEST1(snmp_framework_mib),
    ?line unload_master("SNMP-FRAMEWORK-MIB").

%% Req. SNMP-FRAMEWORK-MIB
snmp_framework_mib() ->
    ?line ["agentEngine"] = get_req(1, [[snmpEngineID,0]]),
    ?line [EngineTime] = get_req(2, [[snmpEngineTime,0]]),
    sleep(5000),
    ?line [EngineTime2] = get_req(3, [[snmpEngineTime,0]]),
    if 
	EngineTime+7 < EngineTime2 ->
	    ?line ?FAIL({too_large_diff, EngineTime, EngineTime2});
	EngineTime+4 > EngineTime2 ->
	    ?line ?FAIL({too_large_diff, EngineTime, EngineTime2});
	true -> ok
    end,
    ?line case get_req(4, [[snmpEngineBoots,0]]) of
	      [Boots] when integer(Boots) -> ok;
	      Else -> ?FAIL(Else)
	  end,
    ok.


%% -- snmp_target_mib --

snmp_target_mib(suite) -> [];
snmp_target_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master_std("SNMP-TARGET-MIB"),
    ?TRY_TEST1(snmp_target_mib),
    ?line unload_master("SNMP-TARGET-MIB").

snmp_target_mib() ->
    ?INF("NOT YET IMPLEMENTED", []),
    nyi.


%% -- snmp_notification_mib --

snmp_notification_mib(suite) -> [];
snmp_notification_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master_std("SNMP-NOTIFICATION-MIB"),
    ?TRY_TEST1(snmp_notification_mib),
    ?line unload_master("SNMP-NOTIFICATION-MIB").

snmp_notification_mib() ->
    ?INF("NOT YET IMPLEMENTED", []),
    nyi.


%% -- snmp_view_based_acm_mib -- 

%%-----------------------------------------------------------------
%% o  add/delete views and try them
%% o  try boundaries
%%-----------------------------------------------------------------
snmp_view_based_acm_mib(suite) -> [];
snmp_view_based_acm_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    ?line load_master("Test2"),
    snmp_view_based_acm_mib(),
    ?line unload_master("Test2"),
    ?line unload_master("SNMP-VIEW-BASED-ACM-MIB").

snmp_view_based_acm_mib() ->
    snmpa:verbosity(net_if,trace),
    snmpa:verbosity(master_agent,trace),
    ?LOG("start snmp_view_based_acm_mib test",[]),
    %% The user "no-rights" is present in USM, and is mapped to security
    %% name 'no-rights", which is not present in VACM.
    %% So, we'll add rights for it, try them and delete them.
    %% We'll give "no-rights" write access to tDescr.0 and read access
    %% to tDescr2.0
    %% These are the options we'll use to the mgr
    Opts = [{user, "no-rights"}, {community, "no-rights"}],
    %% Find the valid secmodel, and one invalid secmodel.
    {SecMod, InvSecMod} = 
	case get(vsn) of
	    v1 -> {?SEC_V1, ?SEC_V2C};
	    v2 -> {?SEC_V2C, ?SEC_USM};
	    v3 -> {?SEC_USM, ?SEC_V1}
	end,
    ?DBG("assign rights for 'no-rights'",[]),
    ?line ?TRY_TEST3(use_no_rights, [], Opts),

    %% Now, add a mapping from "no-rights" -> "no-rights-group"
    GRow1Status = [vacmSecurityToGroupStatus,[SecMod, 9,"no-rights"]],
    GRow1 = 
	[{[vacmGroupName, [SecMod, 9,"no-rights"]], "no-rights-group"},
	 {GRow1Status, ?createAndGo}],
    ?DBG("set '~p'",[GRow1]),
    ?line ?TRY_TEST2(do_set, [GRow1]),

    ?DBG("assign rights for 'no-rights'",[]),
    ?line ?TRY_TEST3(use_no_rights, [], Opts),

    %% Create a mapping for another sec model, and make sure it dosn't
    %% give us access
    GRow2Status = [vacmSecurityToGroupStatus,[InvSecMod, 9,"no-rights"]],
    GRow2 = [{[vacmGroupName, [InvSecMod, 9, "no-rights"]], "initial"},
	     {GRow2Status, ?createAndGo}],

    ?DBG("set '~p'",[GRow2]),
    ?line ?TRY_TEST2(do_set, [GRow2]),

    ?DBG("assign rights for 'no-rights'",[]),
    ?line ?TRY_TEST3(use_no_rights, [], Opts),

    %% Delete that row
    ?line ?TRY_TEST2(del_row, [GRow2Status]),
    
    RVName = "rv_name",
    WVName = "wv_name",

    %% Access row
    ARow1Idx = [15 | "no-rights-group"] ++ [0, ?SEC_ANY, 1],
    ARow1Status = [vacmAccessStatus, ARow1Idx],
    ARow1 = [{[vacmAccessContextMatch, ARow1Idx], 1},
	     {[vacmAccessReadViewName, ARow1Idx], RVName},
	     {[vacmAccessWriteViewName, ARow1Idx], WVName},
	     {ARow1Status, ?createAndGo}],
    
    %% This access row would give acces, if InvSecMod was valid.
    ARow2Idx = [15 | "no-rights-group"] ++ [0, InvSecMod, 1],
    ARow2Status = [vacmAccessStatus, ARow2Idx],
    ARow2 = [{[vacmAccessContextMatch, ARow2Idx], 1},
	     {[vacmAccessReadViewName, ARow2Idx], "internet"},
	     {[vacmAccessWriteViewName, ARow2Idx], "internet"},
	     {ARow2Status, ?createAndGo}],
    
    ?line ?TRY_TEST2(do_set, [ARow2]),

    ?line ?TRY_TEST3(use_no_rights, [], Opts),

    %% Delete that row
    ?line ?TRY_TEST2(del_row, [ARow2Status]),
    

    %% Add valid row
    ?line ?TRY_TEST2(do_set, [ARow1]),

    ?line ?TRY_TEST3(use_no_rights, [], Opts),

    %% Create the view family
    VRow1Idx = mk_ln(RVName) ++ mk_ln(?xDescr),         % object access
    VRow2Idx = mk_ln(RVName) ++ mk_ln(?xDescr2 ++ [0]), % instance access
    VRow3Idx = mk_ln(WVName) ++ mk_ln(?xDescr),         % object access
    VRow4Idx = mk_ln(WVName) ++ mk_ln(?xDescr ++ [0]),  % instance access
    VRow1Status = [vacmViewTreeFamilyStatus, VRow1Idx],
    VRow2Status = [vacmViewTreeFamilyStatus, VRow2Idx],
    VRow3Status = [vacmViewTreeFamilyStatus, VRow3Idx],
    VRow4Status = [vacmViewTreeFamilyStatus, VRow4Idx],
    
    ?line ?TRY_TEST2(add_row, [VRow1Status]),
    ?line ?TRY_TEST2(add_row, [VRow2Status]),
    ?line ?TRY_TEST2(add_row, [VRow3Status]),

    %% We're supposed to have access now...
    ?line ?TRY_TEST3(use_rights, [], Opts),

    %% Change Row3 to Row4
    ?line ?TRY_TEST2(del_row, [VRow3Status]),
    ?line ?TRY_TEST2(add_row, [VRow4Status]),

    %% We should still have access...
    ?line ?TRY_TEST3(use_rights, [], Opts),

    %% Delete rows
    ?line ?TRY_TEST2(del_row, [GRow1Status]),
    
    ?line ?TRY_TEST3(use_no_rights, [], Opts),

    %% Delete rest of rows
    ?line ?TRY_TEST2(del_row, [ARow1Status]),
    ?line ?TRY_TEST2(del_row, [VRow1Status]),
    ?line ?TRY_TEST2(del_row, [VRow2Status]),
    ?line ?TRY_TEST2(del_row, [VRow4Status]),

    ?line ?TRY_TEST3(use_no_rights, [], Opts),
    snmpa:verbosity(master_agent,log).

do_set(Row) ->
    s(Row),
    expect(1, Row).
    
add_row(RowStatus) ->
    s([{RowStatus, ?createAndGo}]),
    expect(1, [{RowStatus, ?createAndGo}]).

del_row(RowStatus) ->
    s([{RowStatus, ?destroy}]),
    expect(1, [{RowStatus, ?destroy}]).
    
    

use_no_rights() ->
    g([[xDescr,0]]),
    ?v1_2_3(expect(11, noSuchName, 1, any),
	    expect(12, [{[xDescr,0], noSuchObject}]),
	    expect(13, authorizationError, 1, any)),
    g([[xDescr2,0]]),
    ?v1_2_3(expect(21, noSuchName, 1, any),
	    expect(22, [{[xDescr2,0], noSuchObject}]),
	    expect(23, authorizationError, 1, any)),
    gn([[xDescr]]),
    ?v1_2_3(expect(31, noSuchName, 1, any),
	    expect(32, [{[xDescr], endOfMibView}]),
	    expect(33, authorizationError, 1, any)),
    s([{[xDescr,0], "tryit"}]),
    ?v1_2_3(expect(41, noSuchName, 1, any),
	    expect(42, noAccess, 1, any),
	    expect(43, authorizationError, 1, any)).


use_rights() ->
    g([[xDescr,0]]),
    expect(1, [{[xDescr,0], any}]),
    g([[xDescr2,0]]),
    expect(2, [{[xDescr2,0], any}]),
    s([{[xDescr,0], "tryit"}]),
    expect(3, noError, 0, any),
    g([[xDescr,0]]),
    expect(4, [{[xDescr,0], "tryit"}]).


%% -- sparse_table --

sparse_table(suite) -> [];
sparse_table(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = ?INIT_CASE(Config),

    ?line load_master("Test1"),
    ?TRY_TEST1(sparse_table_test),
    ?line unload_master("Test1").

%% Req. Test1
sparse_table_test() ->
    p("Testing sparse table..."),

    %% Create two rows, check that they are get-nexted in correct order.
    Idx1 = 1,
    Idx2 = 2,
    s([{[sparseStatus, Idx1], i, ?createAndGo},
       {[sparseDescr, Idx1], s, "row 1"}]),
    ?line expect(1, [{[sparseStatus, Idx1], ?createAndGo},
		     {[sparseDescr, Idx1], "row 1"}]),
    s([{[sparseStatus, Idx2], i, ?createAndGo},
       {[sparseDescr, Idx2], s, "row 2"}]),
    ?line expect(2, [{[sparseStatus, Idx2], ?createAndGo},
		     {[sparseDescr, Idx2], "row 2"}]),
    ?v1_2(gn([[sparseIndex], [sparseDescr,Idx1], [sparseDescr,Idx2],
	      [sparseStatus,Idx1], [sparseStatus,Idx2]]),
	  gb(0,5,[[sparseIndex]])),
    ?line expect(3, [{[sparseDescr,Idx1], "row 1"},
		     {[sparseDescr,Idx2], "row 2"},
		     {[sparseStatus,Idx1], ?active},
		     {[sparseStatus,Idx2], ?active},
		     {[sparseStr,0], "slut"}]),
    % Delete the rows
    s([{[sparseStatus, Idx1], i, ?destroy}]),
    ?line expect(4, [{[sparseStatus, Idx1], ?destroy}]),
    s([{[sparseStatus, Idx2], i, ?destroy}]),
    ?line expect(5, [{[sparseStatus, Idx2], ?destroy}]).


%% -- cnt_64 --

cnt_64(suite) -> [];
cnt_64(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = ?INIT_CASE(Config),
    MA = whereis(snmp_master_agent),

    ?line load_master("Test1"),
    ?TRY_TEST2(cnt_64_test, [MA]),
    ?line unload_master("Test1").

%% Req. Test1
cnt_64_test(MA) ->
    ?LOG("start cnt64 test (~p)",[MA]),
    snmpa:verbosity(MA,trace),
    ?LOG("start cnt64 test",[]),
    p("Testing Counter64, and at the same time, RowStatus is not last column"),
    
    ?DBG("get cnt64",[]),
    g([[cnt64,0]]),
    ?DBG("await response",[]),
    ?line ?v1_2(expect(1, noSuchName, 1, any),
		expect(1, [{[cnt64,0],18446744073709551615}])),
    ?DBG("get-next cnt64",[]),
    gn([[cnt64]]),
    ?DBG("await response",[]),
    ?line ?v1_2(expect(2, [{[cnt64Str,0], "after cnt64"}]),
		expect(2, [{[cnt64,0],18446744073709551615}])),
    ?DBG("send cntTrap",[]),
    snmpa:send_trap(MA,cntTrap,"standard trap",[{sysContact,"pelle"},
					       {cnt64, 10},
					       {sysLocation, "here"}]),
    ?DBG("await response",[]),
    ?line ?v1_2(expect(3, trap, [test], 6, 1, [{[sysContact,0], "pelle"},
					       {[sysLocation,0], "here"}]),
		expect(3, v2trap, [{[sysUpTime, 0], any},
				   {[snmpTrapOID, 0], ?testTrap ++ [1]},
				   {[sysContact,0], "pelle"},
				   {[cnt64,0], 10},
				   {[sysLocation,0], "here"}])),
    
    %% Create two rows, check that they are get-nexted in correct order.
    Idx1 = 1,
    Idx2 = 2,
    ?DBG("create row (cntStatus): ~p",[Idx1]),
    s([{[cntStatus, Idx1], i, ?createAndGo}]),
    ?DBG("await response",[]),
    ?line expect(1, [{[cntStatus, Idx1], ?createAndGo}]),
    ?DBG("create row (cntStatus): ~p",[Idx2]),
    s([{[cntStatus, Idx2], i, ?createAndGo}]),
    ?DBG("await response",[]),
    ?line expect(2, [{[cntStatus, Idx2], ?createAndGo}]),

    ?DBG("get-next (cntIndex)",[]),
    gn([[cntIndex]]),
    ?DBG("await response",[]),
    ?line ?v1_2(expect(3, [{[cntStatus,Idx1], ?active}]),
		expect(3, [{[cntCnt,Idx1], 0}])),
    % Delete the rows
    ?DBG("delete row (cntStatus): ~p",[Idx1]),
    s([{[cntStatus, Idx1], i, ?destroy}]),
    ?DBG("await response",[]),
    ?line expect(4, [{[cntStatus, Idx1], ?destroy}]),
    ?DBG("delete row (cntStatus): ~p",[Idx2]),
    s([{[cntStatus, Idx2], i, ?destroy}]),
    ?DBG("await response",[]),
    ?line expect(5, [{[cntStatus, Idx2], ?destroy}]),
    catch snmpa:verbosity(MA,log),
    ?DBG("done",[]),
    ok.


%% -- opaque --

opaque(suite) -> [];
opaque(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = ?INIT_CASE(Config),

    ?line load_master("Test1"),
    ?TRY_TEST1(opaque_test),
    ?line unload_master("Test1").

%% Req. Test1
opaque_test() ->
    p("Testing Opaque datatype..."),
    g([[opaqueObj,0]]),
    ?line expect(1, [{[opaqueObj,0], "opaque-data"}]).
    

%% -- change_target_addr_config --

change_target_addr_config(suite) -> [];
change_target_addr_config(Config) when list(Config) ->
    p("Testing changing target address config..."),
    ?LOG("change_target_addr_config -> entry",[]),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    put(sname,snmp_suite),
    put(verbosity,trace),

    MA = whereis(snmp_master_agent),

    ?LOG("change_target_addr_config -> load TestTrap",[]),
    ?line load_master("TestTrap"),

    ?LOG("change_target_addr_config -> set trace verbosity for local_db",[]),
    ?line snmpa:verbosity(local_db,trace),

    %% First send some traps that will arive att the original manager
    ?LOG("change_target_addr_config -> send trap",[]),
    ?TRY_TEST2(ma_trap1, [MA]),

    ?LOG("change_target_addr_config -> set silence verbosity for local_db",[]),
    ?line snmpa:verbosity(local_db,silence),

    %% Start new dummy listener
    ?LOG("change_target_addr_config -> start dummy manager",[]),
    ?line {ok,Pid,NewPort} = dummy_manager_start(MA),
    
    %% Reconfigure
    ?LOG("change_target_addr_config -> reconfigure",[]),
    AgentDir = ?config(agent_dir, Config),
    ?line rewrite_target_addr_conf(AgentDir, NewPort),
    ?line snmp_target_mib:reconfigure(AgentDir),

    %% Send the trap again
    ?LOG("change_target_addr_config -> send trap again",[]),
    catch dummy_manager_send_trap2(Pid),

    ?LOG("change_target_addr_config -> await trap ack",[]),
    catch dummy_manager_await_trap2_ack(),

    ?LOG("change_target_addr_config -> stop dummy manager",[]),
    ?line ok = dummy_manager_stop(Pid),

    ?LOG("change_target_addr_config -> reset target address config",[]),
    ?line reset_target_addr_conf(AgentDir),

    ?LOG("change_target_addr_config -> unload TestTrap",[]),
    ?line unload_master("TestTrap").


dummy_manager_start(MA) ->
    ?DBG("dummy_manager_start -> entry",[]),
    Pid = spawn(get(mgr_node), ?MODULE,dummy_manager_init,[self(),MA]),
    ?DBG("dummy_manager_start -> Pid: ~p",[Pid]),
    await_dummy_manager_started(Pid).

await_dummy_manager_started(Pid) ->
    receive
	{dummy_manager_started,Pid,Port} ->
	    ?DBG("dummy_manager_start -> acknowledge received with"
		"~n   Port: ~p",[Port]),
	    {ok,Pid,Port};
	{'EXIT', Pid, Reason} ->
	    {error, Pid, Reason};
	O ->
	    ?LOG("dummy_manager_start -> received unknown message:"
		 "~n   ~p",[O]),
	    await_dummy_manager_started(Pid)
    end.

dummy_manager_stop(Pid) ->
    ?DBG("dummy_manager_stop -> entry with Pid: ~p",[Pid]),
    Pid ! stop,
    receive
	{dummy_manager_stopping, Pid} -> 
	    ?DBG("dummy_manager_stop -> acknowledge received",[]),
	    ok
    after 10000 ->
	    ?ERR("dummy_manager_stop -> timeout",[]),
	    timeout
    end.

dummy_manager_send_trap2(Pid) ->
    ?DBG("dummy_manager_send_trap2 -> entry",[]),
    Pid ! {send_trap,testTrap2}.

dummy_manager_await_trap2_ack() ->
    ?DBG("dummy_manager_await_trap2 -> entry",[]),
    receive
	{received_trap,Trap} ->
	    ?LOG("dummy_manager_await_trap2 -> received trap: ~p",[Trap]),
	    %% Note: 
	    %% Without this sleep the v2_inform_i testcase failes! There
	    %% is no relation between these two test cases as far as I
	    %% able to figure out...
	    sleep(60000),
	    ok;
	O ->
	    ?ERR("dummy_manager_await_trap2 -> unexpected message: ~p",[O]),
	    ok
    after 10000 ->
	    ?ERR("dummy_manager_await_trap2 -> timeout",[]),
	    timeout
    end.

dummy_manager_init(Parent,MA) ->
    ?DBG("dummy_manager_init -> entry with"
	   "~n   Parent: ~p"
	   "~n   MA:     ~p",[Parent,MA]),
    {ok,S} = gen_udp:open(0,[{recbuf,65535}]),
    ?DBG("dummy_manager_init -> S: ~p",[S]),
    {ok,Port} = inet:port(S),
    ?DBG("dummy_manager_init -> Port: ~p",[Port]),
    Parent ! {dummy_manager_started,self(),Port},
    dummy_manager_loop(Parent,S,MA).

dummy_manager_loop(P,S,MA) ->
    ?LOG("dummy_manager_loop -> ready for receive",[]),
    receive
	{send_trap,Trap} ->
	    ?LOG("dummy_manager_loop -> received trap send request"
		 "~n   Trap: ~p",[Trap]),
	    snmpa:send_trap(MA, Trap, "standard trap"),
	    dummy_manager_loop(P,S,MA);
	{udp, _UdpId, Ip, UdpPort, Bytes} ->
	    ?LOG("dummy_manager_loop -> received upd message"
		 "~n   from: ~p:~p"
		 "~n   size: ~p",
		 [Ip, UdpPort, dummy_manager_message_sz(Bytes)]),
	    R = dummy_manager_handle_message(Bytes),
	    ?DBG("dummy_manager_loop -> R: ~p",[R]),
	    P ! R,
	    dummy_manager_loop(P,S,MA);
	stop ->
	    ?DBG("dummy_manager_loop -> received stop request",[]),
	    P ! {dummy_manager_stopping, self()},
	    gen_udp:close(S),
	    exit(normal);
	O ->
	    ?LOG("dummy_manager_loop -> received unknown message:"
		 "~n   ~p",[O]),
	    dummy_manager_loop(P,S,MA)
    end.

dummy_manager_message_sz(B) when binary(B) ->
    size(B);
dummy_manager_message_sz(L) when list(L) ->
    length(L);
dummy_manager_message_sz(_) ->
    undefined.

dummy_manager_handle_message(Bytes) ->
    case (catch snmp_pdus:dec_message(Bytes)) of
	{'EXIT',Reason} ->
	    ?ERR("dummy_manager_handle_message -> "
		   "failed decoding message only:~n   ~p",[Reason]),
	    {error,Reason};
	M ->
	    ?DBG("dummy_manager_handle_message -> decoded message:"
		   "~n   ~p",[M]),
	    {received_trap,M}
    end.


%% -- reported_bugs --

%%%-----------------------------------------------------------------
%%% Testing of reported bugs and other tickets.
%%%-----------------------------------------------------------------

reported_bugs(suite) ->
    [otp_1128, otp_1129, otp_1131, otp_1162,
     otp_1222, otp_1298, otp_1331, otp_1338,
     otp_1342, otp_2776, otp_2979, otp_3187, otp_3725].

%%-----------------------------------------------------------------
%% Ticket: OTP-1128
%% Slogan: Bug in handling of createAndWait set-requests.
%%-----------------------------------------------------------------
otp_1128(suite) -> [];
otp_1128(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    ?TRY_TEST1(otp_1128),
    ?line unload_master("OLD-SNMPEA-MIB").

otp_1128() ->
    io:format("Testing bug reported in ticket OTP-1128...~n"),

    NewKeyc3 = [intCommunityViewIndex,get(mip),is("test")],
    NewKeyc4 = [intCommunityAccess,get(mip),is("test")],
    NewKeyc5 = [intCommunityStatus,get(mip),is("test")],

    s([{NewKeyc5, ?createAndWait}, {NewKeyc4, 2}]),
    ?line expect(28, [{NewKeyc5, ?createAndWait}, {NewKeyc4, 2}]),
    g([NewKeyc5]),
    ?line expect(29, [{NewKeyc5, ?notReady}]),
    s([{NewKeyc5, ?active}, {NewKeyc3, 2}]),
    ?line expect(30, [{NewKeyc5, ?active}, {NewKeyc3, 2}]),
    g([NewKeyc5]),
    ?line expect(31, [{NewKeyc5, ?active}]),
    s([{NewKeyc5, ?destroy}]),
    ?line expect(32, [{NewKeyc5, ?destroy}]).

%%-----------------------------------------------------------------
%% Ticket: OTP-1129, OTP-1169
%% Slogan: snmpa:int_to_enum crashes on bad oids
%%-----------------------------------------------------------------
otp_1129(suite) -> [];
otp_1129(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("Klas3"),
    ?TRY_TEST2(otp_1129_i, [node()]),
    ?line unload_master("Klas3").

otp_1129_i(MaNode) ->
    io:format("Testing bug reported in ticket OTP-1129...~n"),
    false = rpc:call(MaNode, snmp, int_to_enum, [iso, 1]),
    false = rpc:call(MaNode, snmp, int_to_enum, [isox, 1]).


%%-----------------------------------------------------------------
%% Ticket: OTP-1131
%% Slogan: Agent crashes / erlang node halts if RowIndex in a
%%         setrequest is of bad type, e.g. an INDEX {INTEGER},
%%         and RowIdenx [3,2].
%%-----------------------------------------------------------------
otp_1131(suite) -> [];
otp_1131(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("Klas1"),
    ?TRY_TEST1(otp_1131),
    ?line unload_master("Klas1").

otp_1131() ->
    io:format("Testing bug reported in ticket OTP-1131...~n"),
    s([{[friendsEntry, [2, 3, 1]], s, "kompis3"},
       {[friendsEntry, [3, 3, 1]], i, ?createAndGo}]),
    ?line expect(1, ?v1_2(noSuchName, noCreation), 2, any).


%%-----------------------------------------------------------------
%% Ticket: OTP-1162
%% Slogan: snmp_agent can't handle wrongValue from instrum.func
%%-----------------------------------------------------------------
otp_1162(suite) -> [];
otp_1162(Config) when list(Config) ->
    ?P1(otp_1162),
    {SaNode, _MgrNode, _MibDir} = ?INIT_CASE(Config),
    ?line {ok, SA} = ?START_SA(SaNode, ?sa, "SA-MIB"),
    ?TRY_TEST1(otp_1162),
    ?STOP_SA(SA).

otp_1162() ->
    s([{[sa, [2,0]], 6}]), % wrongValue (i is_set_ok)
    ?line expect(1, ?v1_2(badValue, wrongValue), 1, any).


%%-----------------------------------------------------------------
%% Ticket: OTP-1222
%% Slogan: snmp agent crash if faulty index is returned from instrum
%%-----------------------------------------------------------------
otp_1222(suite) -> [];
otp_1222(Config) when list(Config) ->
    ?P1(otp_1222),
    ?INIT_CASE(Config),
    ?line load_master("Klas3"),
    ?line load_master("Klas4"),
    ?TRY_TEST1(otp_1222),
    ?line unload_master("Klas3"),
    ?line unload_master("Klas4").

otp_1222() ->
    io:format("Testing bug reported in ticket OTP-1222...~n"),
    s([{[fStatus4,1], 4}, {[fName4,1], 1}]),
    ?line expect(1, genErr, 0, any),
    s([{[fStatus4,2], 4}, {[fName4,2], 1}]),
    ?line expect(2, genErr, 0, any).


%%-----------------------------------------------------------------
%% Ticket: OTP-1298
%% Slogan: Negative INTEGER values are treated as positive.
%%-----------------------------------------------------------------
otp_1298(suite) -> [];
otp_1298(Config) when list(Config) ->
    ?P1(otp_1298),
    ?INIT_CASE(Config),
    ?line load_master("Klas2"),
    ?TRY_TEST1(otp_1298),
    ?line unload_master("Klas2").

otp_1298() ->
    io:format("Testing bug reported in ticket OTP-1298...~n"),
    s([{[fint,0], -1}]),
    ?line expect(1298, [{[fint,0], -1}]).
    

%%-----------------------------------------------------------------
%% Ticket: OTP-1331
%% Slogan: snmp_generic should return noError when deleting non-ex row
%%-----------------------------------------------------------------
otp_1331(suite) -> [];
otp_1331(Config) when list(Config) ->
    ?P1(otp_1331),
    ?INIT_CASE(Config),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    ?TRY_TEST1(otp_1331),
    ?line unload_master("OLD-SNMPEA-MIB").

otp_1331() ->
    NewKeyc5 = [intCommunityStatus,[127,32,0,0],is("test")],
    s([{NewKeyc5, ?destroy}]),
    ?line expect(1, [{NewKeyc5, ?destroy}]).


%%-----------------------------------------------------------------
%% Ticket: OTP-1338
%% Slogan: snmp bug in initialisation of default values for mnesia tabs
%%-----------------------------------------------------------------
otp_1338(suite) -> [];
otp_1338(Config) when list(Config) ->
    ?P1(otp_1338),
    ?INIT_CASE(Config),
    ?line load_master("Klas2"),
    ?TRY_TEST1(otp_1338),
    ?line unload_master("Klas2").

otp_1338() ->
    s([{[kStatus2, 7], i, ?createAndGo}]),
    ?line expect(1, [{[kStatus2, 7], ?createAndGo}]),
    g([[kName2, 7]]),
    ?line expect(2, [{[kName2, 7], "JJJ"}]).


%%-----------------------------------------------------------------
%% Ticket: OTP-1342
%% Slogan: default impl of snmp table can't handle bad index access,
%%         Set when INDEX is read-write gets into an infinite loop!
%%-----------------------------------------------------------------
otp_1342(suite) -> [];
otp_1342(Config) when list(Config) ->
    ?P1(otp_1342),
    ?INIT_CASE(Config),
    ?line load_master("Klas4"),
    ?TRY_TEST1(otp_1342),
    ?line unload_master("Klas4").

otp_1342() ->
    s([{[fIndex5, 1], i, 1},
       {[fName5, 1], i, 3},
       {[fStatus5, 1], i, ?createAndGo}]),
    ?line expect(1, ?v1_2(noSuchName, noCreation), 3, any).


%%-----------------------------------------------------------------
%% Ticket: OTP-1366
%% Slogan: snmp traps not sent to all managers
%% Note: NYI! We need a way to tell the test server that we need
%%       mgrs on two different machines.
%%-----------------------------------------------------------------
otp_1366(suite) -> [];
otp_1366(Config) when list(Config) ->
    ?P1(otp_1366),
    ?INIT_CASE(Config),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    ?TRY_TEST1(otp_1366),
    ?line unload_master("OLD-SNMPEA-MIB").

otp_1366() ->
    ?INF("NOT YET IMPLEMENTED", []),
    'NYI'.


%%-----------------------------------------------------------------
%% Ticket: OTP-2776
%% Slogan: snmp:validate_date_and_time() fails when time is 00:00
%%-----------------------------------------------------------------
otp_2776(suite) -> [];
otp_2776(Config) when list(Config) ->
    ?P1(otp_2776),
    ?INIT_CASE(Config),
    ?TRY_TEST1(otp_2776).
 
otp_2776() ->
  io:format("Testing bug reported in ticket OTP-2776...~n"),
 
  Dt01_valid   = [19,98,9,1,1,0,23,0,43,0,0],
  Dt02_valid   = [19,98,9,1,0,0,0,0,43,0,0],  % This is what is fixed: 00:00
  Dt03_valid   = [19,98,2,28,1,0,23,0,43,0,0],
  Dt04_invalid = [19,98,2,29,1,0,23,0,43,0,0],
  Dt05_valid   = [19,96,2,29,1,0,23,0,43,0,0],
  Dt06_valid   = [20,0,2,29,1,0,23,0,43,0,0],
  Dt07_invalid = [19,96,2,30,1,0,23,0,43,0,0], % This is also fixed: 30/2
  Dt08_valid   = [19,98,4,30,1,0,23,0,43,0,0],
  Dt09_invalid = [19,98,4,31,1,0,23,0,43,0,0], % This is also fixed: 31/4
  Dt10_invalid = [], 
  Dt11_invalid = [kalle,hobbe], 
  L = [{ 1, true,  Dt01_valid},
       { 2, true,  Dt02_valid},
       { 3, true,  Dt03_valid},
       { 4, false, Dt04_invalid},
       { 5, true,  Dt05_valid},
       { 6, true,  Dt06_valid},
       { 7, false, Dt07_invalid},
       { 8, true,  Dt08_valid},
       { 9, false, Dt09_invalid},
       {10, false, Dt10_invalid},
       {11, false, Dt11_invalid}],
  
  ?line ok = validate_dat(L).
 

validate_dat(L) -> validate_dat(L,[]).
 
validate_dat([],V) -> 
  Fun = fun({_,X}) -> case X of
                        ok -> false;
                        _  -> true
                      end
        end,
  validate_dat1( lists:reverse( lists:filter(Fun,V) ) );
validate_dat([{Id,E,Dat}|T],V) ->
  validate_dat(T,[validate_dat2(Id,E,Dat) | V]).
 
validate_dat1([]) -> ok;
validate_dat1(L)  -> {error,L}.
 
validate_dat2(Id, E, Dat) ->
  Res = case {E,snmp:validate_date_and_time(Dat)} of
          {E,E} -> ok;
          {E,A} -> {E,A}
        end,
  {Id, Res}.


%%-----------------------------------------------------------------
%% Ticket: OTP-2979
%% Slogan: get-next on more than 1 column in an empty table
%%         returns bad response.
%%-----------------------------------------------------------------
otp_2979(suite) -> [];
otp_2979(Config) when list(Config) ->
    ?P1(otp_2979),
    ?INIT_CASE(Config),
    ?line load_master("Test1"),
    ?line init_old(),
    ?TRY_TEST1(otp_2979),
    ?line unload_master("Test1").

otp_2979() ->
    gn([[sparseDescr], [sparseStatus]]),
    ?line expect(1, [{[sparseStr,0], "slut"},
		     {[sparseStr,0], "slut"}]).

%%-----------------------------------------------------------------
%% Ticket: OTP-3187
%% Slogan: get-next on vacmAccessTable for colums > 5 returns
%%         endOfTable - should return value.
%%-----------------------------------------------------------------
otp_3187(suite) -> [];
otp_3187(Config) when list(Config) ->
    ?P1(otp_3187),
    ?INIT_CASE(Config),
    ?line load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    otp_3187(),
    ?line unload_master("SNMP-VIEW-BASED-ACM-MIB").

otp_3187() ->
    ?line Elements =
       snmp_view_based_acm_mib:vacmAccessTable(get_next,[],[4,5,6]),
    lists:foreach(fun(E) ->
			   ?line if E == endOfTable ->
					?FAIL(endOfTable);
				       true -> ok
				end
		   end, Elements).


%%-----------------------------------------------------------------
%% Ticket: OTP-3542
%% Slogan: 
%%-----------------------------------------------------------------
otp_3542(suite) -> [];
otp_3542(Config) when list(Config) ->
    ?P1(otp_3542),
    ?INIT_CASE(Config),
    ?TRY_TEST1(otp_3542).

otp_3542() ->
    io:format("SNMP v3 discovery...~n"),
    ?line Res = snmp_test_mgr:d(),
    io:format("SNMP v3 discovery result: ~p~n",[Res]).


%%-----------------------------------------------------------------
%% Ticket: OTP-3725
%% Slogan: Slow response time on snmpa:int_to_enum
%%-----------------------------------------------------------------
otp_3725(suite) -> [];
otp_3725(Config) when list(Config) ->
    ?P1(otp_3725),
    ?INIT_CASE(Config),

    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    ?TRY_TEST2(otp_3725_test, [node()]),
    ?line unload_master("OLD-SNMPEA-MIB").

%% Req. OLD-SNMPEA-MIB
otp_3725_test(MaNode) ->
    io:format("Testing feature requested in ticket OTP-3725...~n"),
    ?line rpc:call(MaNode,snmpa,verbosity,[symbolic_store,trace]),
    ?line Db = rpc:call(MaNode,snmp,get_symbolic_store_db,[]),
    ?DBG("otp_3725_test -> Db = ~p",[Db]),

    ?line {value, OID} = rpc:call(MaNode, snmp, name_to_oid,
				  [Db, intAgentIpAddress]),
    ?DBG("otp_3725_test -> name_to_oid for ~p: ~p",[intAgentIpAddress,OID]),
    ?line {value, intAgentIpAddress} = rpc:call(MaNode, snmp, oid_to_name, 
						[Db,OID]),
    ?DBG("otp_3725_test -> oid_to_name for ~p: ~p",[OID,intAgentIpAddress]),
    ?line false = rpc:call(MaNode, snmp, name_to_oid, [Db, intAgentIpAddres]),
    ?line false = rpc:call(MaNode, snmp, oid_to_name,
			   [Db, [1,5,32,3,54,3,3,34,4]]),
    ?line {value, 2} = rpc:call(MaNode, snmp, enum_to_int,
				[Db, intViewType, excluded]),
    ?line {value, excluded} = rpc:call(MaNode, snmp, int_to_enum,
				       [Db, intViewType, 2]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int, 
			   [Db, intViewType, exclude]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int,
			   [Db, intAgentIpAddress, exclude]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int,
			   [Db, intAgentIpAddre, exclude]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, [Db, intViewType, 3]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, 
			   [Db, intAgentIpAddress, 2]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, 
			   [Db, intAgentIpAddre, 2]),
    ?line {value, active} = rpc:call(MaNode, snmp, int_to_enum, 
				     [Db, 'RowStatus', ?active]),
    ?line {value, ?destroy} = rpc:call(MaNode, snmp, enum_to_int, 
				       [Db, 'RowStatus', destroy]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int, 
			   [Db, 'RowStatus', xxxdestroy]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int, 
			   [Db, 'xxRowStatus', destroy]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, [Db, 'RowStatus', 25]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, [Db, 'xxRowStatus', 1]),
    ok.


%% -- tickets --

%% These are (ticket) test cases where the initiation has to be done
%% individually.
tickets(suite) ->
    [otp_4394].


%%-----------------------------------------------------------------
%% Ticket: OTP-4394
%% Slogan: Target mib tag list check invalid
%%-----------------------------------------------------------------

otp_4394(suite) -> {req, [], {conf, 
			      init_otp_4394, 
			      [otp_4394_test], 
			      finish_otp_4394}}.

init_otp_4394(Config) when list(Config) ->
    ?DBG("init_otp_4394 -> entry with"
	   "~n   Config: ~p", [Config]),
    ?line AgentDir = ?config(agent_dir, Config),
    ?line MgrDir   = ?config(mgr_dir, Config),
    ?line Ip       = ?config(ip, Config),
    ?line otp_4394_config(AgentDir, MgrDir, Ip),
    MasterAgentVerbosity = {master_agent_verbosity, trace},
    NetIfVerbosity       = {net_if_verbosity,       trace},
    Opts = [MasterAgentVerbosity,NetIfVerbosity],
    [{vsn, v1} | start_v1_agent(Config,Opts)].

otp_4394_config(AgentDir, MgrDir, Ip0) ->
    ?DBG("otp_4394_config -> entry with"
	   "~n   AgentDir: ~p"
	   "~n   MgrDir:   ~p"
	   "~n   Ip0:      ~p", [AgentDir, MgrDir, Ip0]),
    Vsn = [v1],
    Ip = tuple_to_list(Ip0), 
    ?line snmp_config:write_agent_snmp_files(AgentDir, Vsn, Ip, 
					     ?TRAP_UDP, Ip, 4000, 
					     "OTP-4394 test"),
    ?line case update_usm(Vsn, AgentDir) of
	true ->
	    ?line copy_file(filename:join(AgentDir, "usm.conf"),
			    filename:join(MgrDir, "usm.conf")),
	    ?line update_usm_mgr(Vsn, MgrDir);
	false ->
	    ?line ok
    end,
    C1 = {"a", "all-rights", "initial", "", "pc"},
    C2 = {"c", "secret", "secret_name", "", "secret_tag"},
    ?line write_community_conf(AgentDir, [C1, C2]),
    ?line update_vacm(Vsn, AgentDir),
    Ta1 = {"shelob v1", 
	   [134,138,177,177], 5000, 1500, 3, %% Använd Ip och modda
	   "pc1", 
	   "target_v1", "", 
	   %% [255,255,255,255,0,0], 
	   [],
	   2048},
    Ta2 = {"bifur v1", 
	   [134,138,177,75], 5000, 1500, 3, %% Använd Ip
	   "pc2", 
	   "target_v1", "", 
	   %% [255,255,255,255,0,0],
	   [], 2048},
    ?line write_target_addr_conf(AgentDir, [Ta1, Ta2]),
    ?line write_target_params_conf(AgentDir, Vsn),
    ?line write_notify_conf(AgentDir),
    ok.

finish_otp_4394(Config) when list(Config) ->
    ?DBG("finish_otp_4394 -> entry", []),
    C1 = stop_agent(Config),
    delete_files(C1),
    erase(mgr_node),
    lists:keydelete(vsn, 1, C1).

otp_4394_test(suite) -> [];
otp_4394_test(Config) ->
    ?P1(otp_4394_test),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?TRY_TEST1(otp_4394_test1),
    ?DBG("otp_4394_test -> done", []),
    ok.

otp_4394_test1() ->
    ?DBG("otp_4394_test1 -> entry", []),
    gn([[1,1]]),
    Res = 
	case snmp_test_mgr:expect(1, [{[sysDescr,0],  "Erlang SNMP agent"}]) of
	    %% {error, 1, {"?",[]}, {"~w",[timeout]}}
	    {error, 1, _, {_, [timeout]}} ->
		?DBG("otp_4394_test1 -> expected result: timeout", []),
		ok;
	    Else ->
		Else
	end,
    ?DBG("otp_4394_test1 -> done with: ~p", [Res]),
    Res.


mk_ln(X) ->
    [length(X) | X].

    

%% string used in index
is(S) -> [length(S) | S].

expect(A,B) ->         ok = ?MGR:expect(A,B).
expect(A,B,C) ->       ok = ?MGR:expect(A,B,C).
expect(A,B,C,D) ->     ok = ?MGR:expect(A,B,C,D).
expect(A,B,C,D,E,F) -> ok = ?MGR:expect(A,B,C,D,E,F).


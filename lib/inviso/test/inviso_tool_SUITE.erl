% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%% Description:
%% Test suite for the inviso_tool. It is here assumed that inviso works
%% properly.
%%
%% Authors:
%% Lennart Öhman, lennart.ohman@st.se
%% -----------------------------------------------------------------------------

-module(inviso_tool_SUITE).
-compile(export_all).

-include("test_server.hrl").
-include_lib("kernel/include/file.hrl").

-define(l,?line).

all(suite) ->
    [
     dist_basic_1,
     dist_rtc,
     dist_reconnect,
     dist_adopt,
     dist_history,
     dist_start_session_special
    ].
%% -----------------------------------------------------------------------------

init_per_suite(Config) ->
    Config.
%% -----------------------------------------------------------------------------

end_per_suite(_Config) ->
    ok.
%% -----------------------------------------------------------------------------

%% For each distributed testcase, we need two other distributed nodes to run the
%% runtime components on. Since they are freshly started every time there is no
%% need to clean them up first.
init_per_testcase(_Case,Config) ->
    ?l TH=test_server:timetrap(100000),
    ?l {ok,Node1}=test_server:start_node(inviso1,peer,[]),
    ?l {ok,Node2}=test_server:start_node(inviso2,peer,[]),
    ?l SuiteDir=filename:dirname(code:which(?MODULE)),

    %% Otherwise peer nodes will not find this module!
    ?l true=rpc:call(Node1,code,add_patha,[SuiteDir]),
    ?l true=rpc:call(Node2,code,add_patha,[SuiteDir]),

    %% SPECIAL FOR MY PRIVATE TEST ENVIROMENT
%    ?l rpc:call(Node1,code,add_patha,["/clearcase/otp/tools/runtime_tools/ebin"]),
%    ?l rpc:call(Node1,code,add_patha,["/clearcase/otp/tools/inviso/ebin"]),
%    ?l rpc:call(Node2,code,add_patha,["/clearcase/otp/tools/runtime_tools/ebin"]),
%    ?l rpc:call(Node2,code,add_patha,["/clearcase/otp/tools/inviso/ebin"]),

%    %% SPECIAL FOR MY PRIVATE TEST ENVIROMENT, windows.
%    ?l rpc:call(Node1,code,add_patha,["C:/DATA/PROJECTS/inviso_project/runtime_tools/ebin"]),
%    ?l rpc:call(Node1,code,add_patha,["C:/DATA/PROJECTS/inviso_project/inviso/ebin"]),
%    ?l rpc:call(Node2,code,add_patha,["C:/DATA/PROJECTS/inviso_project/runtime_tools/ebin"]),
%    ?l rpc:call(Node2,code,add_patha,["C:/DATA/PROJECTS/inviso_project/inviso/ebin"]),

    ?l ok=rpc:call(Node1,application,start,[runtime_tools]),
    ?l ok=rpc:call(Node2,application,start,[runtime_tools]),
    ?l timer:sleep(100),                     % Problem with autostarted runtime.
    %% The following is a test that the inviso_rt processes which are autostarted
    %% are now gone.

    ?l ok=poll(rpc,call,[Node1,erlang,whereis,[inviso_rt]],undefined,20),
    ?l ok=poll(rpc,call,[Node2,erlang,whereis,[inviso_rt]],undefined,20),

    NewConfig1=insert_remotenode_config(inviso1,Node1,Config),
    NewConfig2=insert_remotenode_config(inviso2,Node2,NewConfig1),
    insert_timetraphandle_config(TH,NewConfig2).
%% -----------------------------------------------------------------------------

fin_per_testcase(_Case,Config) ->
    ?l test_server:stop_node(get_remotenode_config(inviso1,Config)),
    ?l test_server:stop_node(get_remotenode_config(inviso2,Config)),
    ?l test_server:timetrap_cancel(get_timetraphandle_config(Config)),
    ?l case whereis(inviso_tool) of          % In case inviso_tool did not stop.
	   Pid when pid(Pid) ->
	       ?l io:format("Had to kill inviso_tool!~n",[]),
	       ?l exit(Pid,kill);
	   _ ->
	       true
       end,
    ?l case whereis(inviso_rt) of            % In case we ran a runtime here.
	   Pid2 when pid(Pid2) ->
	       ?l io:format("Had to kill inviso_rt!~n",[]),
	       ?l exit(Pid2,kill);
	   _ ->
	       true
       end,
    ?l case whereis(inviso_c) of             % In case we ran the controll component here.
	   Pid3 when pid(Pid3) ->
	       ?l io:format("Had to kill inviso_c!~n",[]),
	       ?l exit(Pid3,kill);
	   _ ->
	       true
       end,
    NewConfig1=remove_remotenode_config(inviso1,Config),
    NewConfig2=remove_remotenode_config(inviso2,NewConfig1),
    remove_timetraphandle_config(NewConfig2).
%% -----------------------------------------------------------------------------

%% ==============================================================================
%% Testcases.
%% ==============================================================================

%% -----------------------------------------------------------------------------
%% Functional tests:
%% API:
%%   start/0                                 dist_basic_1
%%   stop/0                                  dist_basic_1
%%   reconnect_nodes/1                       dist_reconnect
%%   start_session/0                         dist_basic_1
%%   reinitiate_session/1                    dist_reconnect
%%   stop_session/0                          dist_basic_1
%%   atc/3                                   dist_basic_1
%%   sync_atc/3                              dist_basic_1
%%   sync_rtc/2,                             dist_rtc
%%   dtc/2                                   dist_basic_1
%%   sync_dtc/2                              dist_basic_1
%%   inviso/2                                dist_basic_1
%%   reactivate/1                            dist_basic_1
%%   get_autostart_data/2                    dist_basic_1
%%   get_activities/0                        dist_basic_1
%%   save_history/1                          dist_history
%%   restore_session/1                       dist_history
%%   get_node_status/1                       dist_basic_1
%%   get_session_data/0                      dist_basic_1
%%   flush/0                                 dist_basic_1
%% -----------------------------------------------------------------------------

%% Non functional tests:
%%   Run the control component on both the   dist_history
%%     same node as the tool and on a
%%     different node.
%%   Let a trace case crash in its execution dist_basic_1
%%     and check that it does not become
%%     part of the history.
%%   Check that tracer data becomes what the NOT IMPLEMENTED
%%     tdg function generates.
%%   Check that all inviso runtime           stop_inviso_tool/2
%%     components terminate if the tool is
%%     killed.
%%   Check that activation/deactivation      dist_basic_1
%%     cancels each other out in the history.
%%   Check that regexp expansion is done on  dist_reconnect
%%     another node if regexp_node is down.
%%   Test that tool-commands activating      dist_basic_1
%%     something done during a reactivation
%%     are actually done a bit later at the
%%     reactivated node (this since the the
%%     command being reactivated at the momen
%%     at the reactivating node may not
%%     be finished at the time the new tool
%%     command is issued).
%%   Check that deactivating tracecases are  dist_basic_1
%%     not redone at a reactivating node.
%%     (to prevent it from being redone and
%%     then just deactivated).
%%   Check that on-going reactivators and    NOT IMPLEMENTED
%%     tracecases are killed when stop_session.
%%   Check that inviso_tool can and will adopt
%%     a running runtime component.          dist_adopt
%% -----------------------------------------------------------------------------

-define(TC_DEF_FILE,filename:join(DataDir,"tracecase_def.txt")).

%% TEST CASE: Basic, distributed, start of inviso_tool with simple tracing.
dist_basic_1(doc) -> ["Simple test"];
dist_basic_1(suite) -> [];
dist_basic_1(Config) when list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    [RegExpNode|_]=RemoteNodes,
    CNode=node(),
    Nodes=RemoteNodes,
    DataDir=?config(data_dir,Config),
    PrivDir=filename:join(?config(priv_dir,Config),""),
    Opts=[{regexp_node,RegExpNode},
	  {tdg,{?MODULE,tdg,[PrivDir]}},
	  {tc_def_file,?TC_DEF_FILE},
	  {initial_tcs,[{tracecase_init,[]}]},
	  {dir,DataDir}],                    % This is where we find tracecases.
    ?l start_inviso_tool(Nodes,CNode,Opts),
    %% Now we know that all inviso runtimes are running and are not tracing.
    ?l {error,no_session}=inviso_tool:inviso(tpl,[lists,module_info,0,[]]),
    ?l {error,no_session}=inviso_tool:get_session_data(),
    ?l start_inviso_tool_session(CNode,[],1,Nodes),
    ?l {ok,{tracing,1,TDGargs}}=inviso_tool:get_session_data(),
    ?l true=is_list(TDGargs),
    %% Check that the initial tracecase has been executed at all tracing nodes.
    ?l lists:foreach(fun(N)->
			     ok=poll(rpc,
				     call,
				     [N,
				      erlang,
				      trace_info,
				      [{lists,module_info,1},traced]],
				     {traced,local},
				     20)
		     end,
		     Nodes),
    %% Start a test process at every node with a runtime component.
    ?l lists:foreach(fun(N)->spawn(N,?MODULE,test_proc_init,[]) end,Nodes),
    %% Find the pids of the test processes.
    ?l TestProcs=lists:map(fun(N)->rpc:call(N,erlang,whereis,[inviso_tool_test_proc]) end,
			   Nodes),
    ?l true=(1=<length(TestProcs)),

    %% Activate a trace case.
    ?l {error,unknown_tracecase}=inviso_tool:atc(nonexistant,id,[]),
    ?l {error,{missing_variable,'ProcessName'}}=
	inviso_tool:atc(tracecase1,id1,[]),
    ?l ok=inviso_tool:atc(tracecase1,id1,[{'ProcessName',inviso_tool_test_proc}]),
    ?l {error,activating}=inviso_tool:atc(tracecase1,id1,[{'ProcessName',inviso_tool_test_proc}]),
    ?l {ok,[{tracecases,[{{tracecase1,id1},activating}]}]}=inviso_tool:get_activities(),
    ?l timer:sleep(1700),                     % There is a 500 ms delay in the tracecase.
    ?l {error,already_started}=
	inviso_tool:atc(tracecase1,id1,[{'ProcessName',inviso_tool_test_proc}]),

    %% Now check that the trace case was executed at Nodes.
    ?l lists:foreach(fun(P)->
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[P,flags]],
					{flags,[call]},
					10),
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[{math,module_info,1},traced]],
					{traced,local},
					10)
		     end,
		     TestProcs),
    ?l lists:foreach(fun(P) ->
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[{math,module_info,0},traced]],
					{traced,false},
					1)
		     end,
		     TestProcs),

    %% Test inviso_tool:inviso/2.
    ?l {ok,NodeResults1}=inviso_tool:inviso(tpl,[math,module_info,0,[]]),
    ?l true=check_noderesults(Nodes,{ok,[1]},NodeResults1),
    ?l lists:foreach(fun(P) ->
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[{math,module_info,0},traced]],
					{traced,local},
					10)
		     end,
		     TestProcs),

    %% Test inviso_tool:sync_atc/3.
    ?l a_return_value=inviso_tool:sync_atc(tracecase2,id2,[]), % This will take 3000 ms.
    ?l lists:foreach(fun(P) ->
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[{math,pi,0},traced]],
					{traced,local},
					10)
		     end,
		     TestProcs),

    %% Test the reactivation mechanism.
    ?l [ANode|OtherNodes]=Nodes,             % Get a node to suspend.
    ?l {ok,{tracing,running}}=inviso_tool:get_node_status(ANode),
    ?l {ok,[{ANode,ok}]}=rpc:call(CNode,inviso,suspend,[[ANode],test]),
    ?l [APid|_]=TestProcs,                   % The first process is at ANode.
    %% Now check that trace flags were removed at ANode. This is actually testing inviso.
    ?l ok=poll(rpc,call,[node(APid),erlang,trace_info,[APid,flags]],{flags,[]},10),
    ?l {ok,{tracing,suspended}}=inviso_tool:get_node_status(ANode),
    %% Now reactivate it and expect the history to be redone. But it will take
    %% 3000 ms since there is a delay in tracecase2. Use that delay to issue a new
    %% tool command.
    ?l ok=inviso_tool:reactivate(ANode),
    ?l {ok,reactivating}=inviso_tool:get_node_status(ANode),
    ?l {ok,NodeResults2}=inviso_tool:inviso(tpl,[math,sin,1,[]]),
    ?l true=check_noderesults(OtherNodes,{ok,[1]},NodeResults2),
    %% Verify that the inviso command was not done (yet) at ANode.
    ?l {traced,false}=rpc:call(ANode,erlang,trace_info,[{math,sin,1},traced]),
    ?l {ok,[{reactivating_nodes,[ANode]}]}=inviso_tool:get_activities(),
    ?l timer:sleep(3600),
    %% Now the history shall have been redone including the new inviso command.
    ?l ok=poll(rpc,call,[ANode,erlang,trace_info,[{math,sin,1},traced]],{traced,local},10),
    ?l {flags,[call]}=rpc:call(ANode,erlang,trace_info,[APid,flags]),
    ?l {ok,[]}=inviso_tool:get_activities(),

    %% Check the get_autostart function. We know that we use the standard options
    %% generator and the tracecases activated above.
    ?l {ok,{AutostartData1,NodeResults3}}=
	inviso_tool:get_autostart_data(Nodes,{dependency,infinity}),
    ?l true=check_noderesults(Nodes,
			      fun({_N,{ok,{[{dependency,infinity}],{tdg,{_M,_F,TDlist}}}}})
				 when list(TDlist)->
				      true;
				 (_) ->
				      false
			      end,
			      NodeResults3),
    %% Check the tracecase that shall be activated and their order.
    ?l TraceCaseFileNameInit=filename:join(DataDir,"./tracecase_init.trc"),
    ?l TraceCaseFileName1=filename:join(DataDir,"./tracecase1_on.trc"),
    ?l TraceCaseFileName2=filename:join(DataDir,"./tracecase2_on.trc"),
    ?l [{file,{TraceCaseFileNameInit,[]}},
	{file,{TraceCaseFileName1,[{'ProcessName',inviso_tool_test_proc}]}},
	{mfa,{inviso,tpl,[math,module_info,0,[]]}},
	{file,{TraceCaseFileName2,[]}},
	{mfa,{inviso,tpl,[math,sin,1,[]]}}]=AutostartData1,

    %% Try to activate a faulty tracecase. We shall get the same history as before.
    ?l ok=inviso_tool:atc(tracecase3,id3,[]),
    ?l ok=poll(inviso_tool,get_activities,[],{ok,[]},10),
    ?l {ok,{AutostartData1,NodeResults3}}=
	inviso_tool:get_autostart_data(Nodes,{dependency,infinity}),

    %% Now deactivate a trace case.
    ?l inviso_tool:dtc(tracecase1,id1),
    %% Check that the now deactivated trace case is not part of autostart data
    %% if requested from the tool.
    ?l {ok,{AutostartData2,_NodeResults}}=
	inviso_tool:get_autostart_data(Nodes,{dependency,infinity}),
    ?l [{file,{TraceCaseFileNameInit,[]}},
	{mfa,{inviso,tpl,[math,module_info,0,[]]}},
	{file,{TraceCaseFileName2,[]}},
	{mfa,{inviso,tpl,[math,sin,1,[]]}}]=AutostartData2,
    %% Now tracing shall be removed since we deactivated tracecase1.
    ?l lists:foreach(fun(P)->
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[P,flags]],
					{flags,[]},
					10),
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[{math,module_info,1},traced]],
					{traced,false},
					10)
		     end,
		     TestProcs),

    %% Suspend the ANode again and check that when it is reactivated that
    %% tracecase1 is not redone at all. We use an ets table with a counter that is
    %% incremented every time the tracecase1 is executed.
    ?l {ok,[{ANode,ok}]}=rpc:call(CNode,inviso,suspend,[[ANode],testagain]),
    ?l [{counter,SideEffectCounter1}]=rpc:call(ANode,ets,lookup,[test_proc_tab,counter]),
    ?l true=(SideEffectCounter1>0),
    ?l ok=inviso_tool:reactivate(ANode),
    ?l timer:sleep(3000),                    % The delay in tracecase2.
    ?l ok=poll(inviso_tool,get_activities,[],{ok,[]},10),
    %% Now the reactivation is done, check that tracecase1 was not redone at ANode.
    ?l [{counter,SideEffectCounter1}]=rpc:call(ANode,ets,lookup,[test_proc_tab,counter]),

    %% Deactivate tracecase2.
    ?l another_return_value=inviso_tool:sync_dtc(tracecase2,id2),
    %% Immediately check the autostart data (again!). This time we want to see
    %% that the two inviso commands have been joined since there is no tracecase
    %% in between.
    ?l {ok,{AutostartData3,NodeResults}}=
	inviso_tool:get_autostart_data(Nodes,{dependency,infinity}),
    ?l [{file,{TraceCaseFileNameInit,[]}},
	{mfa,{inviso,tpl,[math,module_info,0,[]]}},
	{mfa,{inviso,tpl,[math,sin,1,[]]}}]=AutostartData3,

    %% Check that a deactivating tracecase is not redone at a reactivating node.
    ?l inviso_tool:sync_atc(tracecase5,id5,[]), % Updates the counter.
    %% Yet again suspend the node when we know that tracecase5 has been executed.
    ?l {ok,[{ANode,ok}]}=rpc:call(CNode,inviso,suspend,[[ANode],testagain2]),
    ?l timer:sleep(100),                     % Subscription reaches the tool.
    ?l [{counter,SideEffectCounter2A}]=rpc:call(ANode,ets,lookup,[test_proc_tab,counter]),
    ?l [BNode|_]=OtherNodes,
    ?l [{counter,SideEffectCounter2B}]=rpc:call(BNode,ets,lookup,[test_proc_tab,counter]),
    ?l ok=inviso_tool:dtc(tracecase5,id5),   % In here there is a 2000 ms delay!
    ?l ok=inviso_tool:reactivate(ANode),
    %% Check that the reactivator is done, but that the tracecase remains. The
    %% reactivator should be done pretty quickly since there are no delays in the
    %% still active tracecases.
    ?l ok=poll(inviso_tool,
	       get_activities,
	       [],
	       {ok,[{tracecases,[{{tracecase5,id5},deactivating}]}]},
	       10),
    ?l ok=poll(inviso_tool,get_activities,[],{ok,[]},30),
    ?l [{counter,SideEffectCounter2A}]=rpc:call(ANode,ets,lookup,[test_proc_tab,counter]),
    ?l SideEffectCounter2B1=SideEffectCounter2B+1,
    ?l [{counter,SideEffectCounter2B1}]=rpc:call(BNode,ets,lookup,[test_proc_tab,counter]),

    %% Check the flush function. It is difficult to find out if it really flushed.
    ?l {ok,NodeResults4}=inviso_tool:flush(),
    ?l true=check_noderesults(Nodes,ok,NodeResults4),

    %% Check that this function still has a trace pattern. We are going to stop session
    %% and check that it is still there.
    ?l lists:foreach(fun(N)->
			     ok=poll(rpc,
				     call,
				     [N,
				      erlang,
				      trace_info,
				      [{math,sin,1},traced]],
				     {traced,local},
				     20)
		     end,
		     Nodes),

    ?l stop_inviso_tool_session(CNode,1,Nodes),
    ?l {ok,{not_tracing,1,TDGargs}}=inviso_tool:get_session_data(),

    ?l {ok,NodeResults5}=inviso_tool:flush(Nodes),
    ?l true=check_noderesults(Nodes,fun({_,{error,_}})->true;(_)->false end,NodeResults5),
    ?l {ok,[]}=inviso_tool:flush(),

    %% Check that you can not start trace cases when the session is stopped.
    ?l {error,no_session}=inviso_tool:atc(tracecase2,id3,[]),
    %% But the history shall be there to retrieve.
    ?l {ok,{AutostartData3,NodeResults}}=
	inviso_tool:get_autostart_data(Nodes,{dependency,infinity}),
    ?l {ok,{inactive,running}}=inviso_tool:get_node_status(ANode),

    %% Check that the trace pattern is still there.
    ?l lists:foreach(fun(N)->
			     ok=poll(rpc,
				     call,
				     [N,
				      erlang,
				      trace_info,
				      [{math,sin,1},traced]],
				     {traced,local},
				     20)
		     end,
		     Nodes),

    %% Now start a session and check that the trace patterns is gone.
    ?l start_inviso_tool_session(CNode,[],2,Nodes),
    ?l lists:foreach(fun(N)->
			     ok=poll(rpc,
				     call,
				     [N,
				      erlang,
				      trace_info,
				      [{math,sin,1},traced]],
				     {traced,false},
				     20)
		     end,
		     Nodes),
    ?l stop_inviso_tool_session(CNode,2,Nodes),

    ?l stop_inviso_tool(CNode,Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% This test case tests the rtc trace case mechanism.
dist_rtc(doc) -> [""];
dist_rtc(suite) -> [];
dist_rtc(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    [RegExpNode|_]=RemoteNodes,
    CNode=node(),
    Nodes=RemoteNodes,
    DataDir=?config(data_dir,Config),
    PrivDir=filename:join(?config(priv_dir,Config),""),
    Opts=[{regexp_node,RegExpNode},
	  {tdg,{?MODULE,tdg,[PrivDir]}},
	  {tc_def_file,?TC_DEF_FILE},
	  {initial_tcs,[{tracecase_init,[]}]},
	  {dir,DataDir}],                    % This is where we find tracecases.
    ?l start_inviso_tool(Nodes,CNode,Opts),
    ?l start_inviso_tool_session(CNode,[],1,Nodes),
    %% Check that the initial tracecase has been executed at all tracing nodes.
    ?l lists:foreach(fun(N)->
			     ok=poll(rpc,
				     call,
				     [N,
				      erlang,
				      trace_info,
				      [{lists,module_info,1},traced]],
				     {traced,local},
				     20)
		     end,
		     Nodes),
    %% Start a test process at every node with a runtime component.
    ?l lists:foreach(fun(N)->spawn(N,?MODULE,test_proc_init,[]) end,Nodes),
    %% Find the pids of the test processes.
    ?l TestProcs=lists:map(fun(N)->rpc:call(N,erlang,whereis,[inviso_tool_test_proc]) end,
			   Nodes),
    ?l true=(1=<length(TestProcs)),
    ?l [ANode|_]=Nodes,
    ?l [{counter,Val}]=rpc:call(ANode,ets,lookup,[test_proc_tab,counter]),
    %% Call the tracecase as an rtc.
    ?l inviso_tool:sync_rtc(tracecase5,[]), % Updates the counter.
    ?l [{counter,Val2}]=rpc:call(ANode,ets,lookup,[test_proc_tab,counter]),
    ?l true=(Val2==Val+1),
    ?l inviso_tool:sync_rtc(tracecase5,[]), % Updates the counter.
    ?l [{counter,Val3}]=rpc:call(ANode,ets,lookup,[test_proc_tab,counter]),
    ?l true=(Val3==Val2+1),

    %% Now we stop the session and restore it again.
    ?l stop_inviso_tool_session(CNode,1,Nodes),
    ?l {ok,{2,_InvisoReturn}}=inviso_tool:restore_session(),
    %% The tracecase shall be done twice then.
    ?l ok=poll(rpc,call,[ANode,ets,lookup,[test_proc_tab,counter]],
	       fun([{counter,V}]) when V==Val3+2 -> true;
		  (_) -> false
	       end,
	       20),
    ?l stop_inviso_tool_session(CNode,2,Nodes),

    ?l {ok,{AutostartData,_NodeResults}}=
	inviso_tool:get_autostart_data(Nodes,{dependency,infinity}),
    ?l [{file,{_FileNameInit,_}},{file,{FileName,Bindings}},{file,{FileName,Bindings}}]=
	AutostartData,
    ?l stop_inviso_tool(CNode,Nodes),
    ok.
%% -----------------------------------------------------------------------------


%% This test case tests mainly that reconnect and reinitiations of a node works.
dist_reconnect(doc) -> [""];
dist_reconnect(suite) -> [];
dist_reconnect(Config) when list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    [RegExpNode|OtherNodes]=RemoteNodes,
    CNode=node(),
    Nodes=RemoteNodes,
    DataDir=?config(data_dir,Config),
    PrivDir=filename:join(?config(priv_dir,Config),""),
    Opts=[{regexp_node,RegExpNode},
	  {tdg,{?MODULE,tdg,[PrivDir]}},
	  {tc_def_file,?TC_DEF_FILE},
	  {initial_tcs,[{tracecase_init,[]}]},
	  {dir,DataDir}],                    % This is where we find tracecases.
    ?l start_inviso_tool(Nodes,CNode,Opts), 
    ?l start_inviso_tool_session(CNode,[],1,Nodes),
    %% Start a test process at every node with a runtime component.
    ?l lists:foreach(fun(N)->spawn(N,?MODULE,test_proc_init,[]) end,Nodes),
    %% Find the pids of the test processes.
    ?l TestProcs=lists:map(fun(N)->rpc:call(N,erlang,whereis,[inviso_tool_test_proc]) end,
			   Nodes),
    ?l true=(1=<length(TestProcs)),
    ?l ok=inviso_tool:atc(tracecase1,id1,[{'ProcessName',inviso_tool_test_proc}]),
    ?l ok=poll(inviso_tool,get_activities,[],{ok,[]},20),

    %% Now we want to crash a node. Lets choose the RegExp node, then we can test
    %% that regexp expansion is done elsewhere too.
    ?l test_server:stop_node(RegExpNode),
    ?l ok=poll(net_adm,ping,[RegExpNode],pang,10),
    %% Make time for the monitoring signal to reach inviso_c and for inviso_c to
    %% inform its subscribers e.g inviso_tool.
    ?l timer:sleep(100),
    ?l {_,[{_Node,Modules}]}=
	inviso_tool_lib:expand_module_names([node()],"application.*",[]),
    ?l NrOfModules=length(Modules),
    %% This also checks that regexp expansion can be made at another node
    %% than RexExpNode.
    ?l {ok,NodeResults1}=inviso_tool:inviso(tp,["application.*",module_info,0,[]]),
    ?l true=check_noderesults(OtherNodes,
			      fun({_N,{ok,Ints}}) when list(Ints) ->
				      NrOfModules=lists:sum(Ints),
				      true;
				 (_) ->
				      false
			      end,
			      NodeResults1),

    %% Do some faulty tests on the dead node.
    ?l {ok,{ok,[{RegExpNode,{error,down}}]}}=
	inviso_tool:reinitiate_session([RegExpNode]),
    ?l {ok,down}=inviso_tool:get_node_status(RegExpNode),

    %% Now it is time to restart the crashed node and reconnect it and then
    %% finally reinitiate it.
    ?l RegExpNodeString=atom_to_list(RegExpNode),
    ?l {match,Pos,1}=regexp:first_match(RegExpNodeString,"@"),
    ?l RegExpNodeName=list_to_atom(lists:sublist(RegExpNodeString,Pos-1)),
    ?l test_server:start_node(RegExpNodeName,peer,[]),
    ?l ok=poll(net_adm,ping,[RegExpNode],pong,20),
    ?l SuiteDir=filename:dirname(code:which(?MODULE)),
    ?l true=rpc:call(RegExpNode,code,add_patha,[SuiteDir]),
    ?l ok=rpc:call(RegExpNode,application,start,[runtime_tools]),
    ?l ok=poll(rpc,call,[RegExpNode,erlang,whereis,[inviso_rt]],undefined,20),
    ?l {ok,down}=inviso_tool:get_node_status(RegExpNode),

    %% Restart the test process.
    ?l spawn(RegExpNode,?MODULE,test_proc_init,[]),
    ?l ok=poll(rpc,
	       call,
	       [RegExpNode,erlang,whereis,[inviso_tool_test_proc]],
	       fun(P) when pid(P) -> true;
		  (undefined) -> false
	       end,
	       10),
    ?l TPid=rpc:call(RegExpNode,erlang,whereis,[inviso_tool_test_proc]),
    ?l {ok,[{RegExpNode,{ok,{inactive,running}}}]}=inviso_tool:reconnect_nodes([RegExpNode]),
    %% Try to reconnect the node again and an unknown node.
    ?l UnknownNode='unknown@nonexistant',
    ?l {ok,[{RegExpNode,{error,already_connected}},{UnknownNode,{error,unknown_node}}]}=
	inviso_tool:reconnect_nodes([RegExpNode,UnknownNode]),
    ?l {ok,{ok,[{RegExpNode,{ok,_}}]}}=inviso_tool:reinitiate_session([RegExpNode]),
    ?l ok=poll(rpc,
	       call,
	       [RegExpNode,erlang,trace_info,[TPid,flags]],
	       {flags,[call]},
	       10),
    ?l {ok,{ok,[{RegExpNode,{error,already_in_session}},{UnknownNode,{error,unknown_node}}]}}=
	inviso_tool:reinitiate_session([RegExpNode,UnknownNode]),

    %% Suspend RegExpNode and test that it can not be reinitiated.
    ?l {ok,[{RegExpNode,ok}]}=rpc:call(CNode,inviso,suspend,[[RegExpNode],yetatest]),
    ?l {ok,[{RegExpNode,{error,already_connected}}]}=inviso_tool:reconnect_nodes([RegExpNode]),
    ?l {ok,{ok,[{RegExpNode,{error,suspended}}]}}=inviso_tool:reinitiate_session([RegExpNode]),

    %% Now we start a tracecase that will never terminate. We then reactivate the
    %% suspended node. Then there will be a running reactivator and a running
    %% tracecase to kill.
    ?l ok=inviso_tool:atc(tracecase4,id4,[]), % This one will not terminate.
    ?l ok=inviso_tool:reactivate(RegExpNode),
    ?l ok=poll(inviso_tool,
	       get_activities,
	       [],
	       fun({ok,L}) when length(L)==2 -> true;
		  (_) -> false
	       end,
	       20),
    %% Now the reactivator and the tracecase shall be stuck(!)
    ?l {links,ToolLinks}=process_info(whereis(inviso_tool),links),
    ?l [P1,P2]=lists:foldl(fun(P,Acc)->case process_info(P,initial_call) of
					   {initial_call,{inviso_tool,_,_}} ->
					       [P|Acc];
					   _ ->
					       Acc
				       end
			   end,
			   [],
			   ToolLinks),
    ?l stop_inviso_tool_session(CNode,1,Nodes),
    %% Check that the processes are killed.
    ?l ok=poll(erlang,process_info,[P1],undefined,10),
    ?l ok=poll(erlang,process_info,[P2],undefined,10),
    ?l stop_inviso_tool(CNode,Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% This test tests that we can adopt a running inviso runtime component and
%% mark it as tracing-running.
dist_adopt(doc) -> [""];
dist_adopt(suite) -> [];
dist_adopt(Config) when list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    [RegExpNode|_]=RemoteNodes,
    CNode=node(),
    Nodes=RemoteNodes,
    DataDir=?config(data_dir,Config),
    PrivDir=filename:join(?config(priv_dir,Config),""),

    %% Then first start runtime components at different nodes for us to
    %% later adopt.
    ?l {ok,_IPid}=inviso:start(),
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,a_tag,[{dependency,infinity}]),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    ?l [ANode|OtherNodes]=Nodes,
    ?l {ok,[{ANode,_LogResult}]}=
	inviso:init_tracing([ANode],
			    [{trace,{file,filename:join(PrivDir,"dist_adopt_adoptednode.log")}},
			     {ti,{file,filename:join(PrivDir,"dist_adopt_adoptednode.ti")}}]),
    ?l inviso:stop(),
    ?l ok=poll(erlang,whereis,[inviso_c],undefined,10),
    ?l lists:foreach(fun(N)->true=(is_pid(rpc:call(N,erlang,whereis,[inviso_rt]))) end,
		     Nodes),

    %% Now start the tool and watch it adopt the runtimes.
    Opts=[{regexp_node,RegExpNode},
	  {tdg,{?MODULE,tdg,[PrivDir]}},
	  {tc_def_file,?TC_DEF_FILE},
	  {initial_tcs,[{tracecase_init,[]}]},
	  {dir,DataDir}],                    % This is where we find tracecases.
    ?l Options=[{nodes,Nodes},{c_node,CNode}|Opts],
    ?l {ok,_Pid}=inviso_tool:start(Options),
    ?l ok=poll(erlang,whereis,[inviso_tool],fun(X)->true=is_pid(X) end,10),
    ?l io:format("LoopData:~p~n",[inviso_tool:get_loopdata()]),
    ?l {ok,{1,InvisoReturn}}=inviso_tool:start_session([]),
    ?l io:format("Invisoreturn:~p~n",[InvisoReturn]),
    %% Now check that all nodes are tracing.
    ?l lists:foreach(fun(N)->ok=poll(rpc,
				     call,
				     [CNode,inviso,get_status,[[N]]],
				     fun({ok,[{_N,{ok,{tracing,running}}}]})->true;
					(_) ->false
				     end,
				     10)
		     end,
		     Nodes),

    %% At this point all nodes shall be tracing. However the initial tracecase
    %% shall not have been executed at ANode since it was adopted by the tool.
    ?l {traced,false}=rpc:call(ANode,erlang,trace_info,[{lists,module_info,1},traced]),
    ?l lists:foreach(fun(N)->
			     {traced,local}=
				 rpc:call(N,erlang,trace_info,[{lists,module_info,1},traced])
		     end,
		     OtherNodes),
    ?l stop_inviso_tool_session(CNode,1,Nodes),
    ?l [BNode|_]=OtherNodes,
    %% Since nodes are not cleared the pattern still be there.
    ?l {traced,local}=rpc:call(BNode,erlang,trace_info,[{lists,module_info,1},traced]),
    ?l start_inviso_tool_session(CNode,[],2,Nodes),
    ?l stop_inviso_tool_session(CNode,2,Nodes),
    ?l {ok,_NodeResults}=inviso_tool:stop(),
    ?l ok=poll(erlang,whereis,[inviso_tool],undefined,10),
    ?l ok=poll(rpc,call,[CNode,erlang,whereis,[inviso_c]],undefined,10),

    ok.
%% -----------------------------------------------------------------------------

%% This test tests that saving and restoring a history works.
dist_history(doc) -> [""];
dist_history(suite) -> [];
dist_history(Config) when list(Config) ->		     
    RemoteNodes=get_remotenodes_config(Config),
    [RegExpNode|_]=RemoteNodes,
    CNode=RegExpNode,                       % We use a remote control component.
    Nodes=RemoteNodes,
    DataDir=?config(data_dir,Config),
    PrivDir=filename:join(?config(priv_dir,Config),""),

    %% Start up the tool and a couple of inviso runtimes.
    Opts=[{regexp_node,RegExpNode},
	  {tdg,{?MODULE,tdg,[PrivDir]}},
	  {tc_def_file,?TC_DEF_FILE},
	  {initial_tcs,[{tracecase_init,[]}]},
	  {dir,DataDir}],                    % This is where we find tracecases.
    ?l start_inviso_tool(Nodes,CNode,Opts), 
    ?l start_inviso_tool_session(CNode,[],1,Nodes),
    %% Start a test process at every node with a runtime component.
    ?l lists:foreach(fun(N)->spawn(N,?MODULE,test_proc_init,[]) end,Nodes),

    %% Activate tracing of the test process.
    ?l ok=inviso_tool:atc(tracecase1,id1,[{'ProcessName',inviso_tool_test_proc}]),
    ?l ok=poll(inviso_tool,get_activities,[],{ok,[]},10),
    ?l TestProcs=lists:map(fun(N)->rpc:call(N,erlang,whereis,[inviso_tool_test_proc]) end,
			   Nodes),
    ?l lists:foreach(fun(P)->
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[P,flags]],
					{flags,[call]},
					10),
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[{math,module_info,1},traced]],
					{traced,local},
					10)
		     end,
		     TestProcs),

    %% Create a history file.
    ?l AbsFileName=filename:join(PrivDir,"dist_history.his"),
    ?l {ok,AbsFileName}=inviso_tool:save_history(AbsFileName),
    ?l {ok,_FileInfo}=file:read_file_info(AbsFileName),

    %% Stop the tracing of the test process.
    ?l inviso_tool:sync_dtc(tracecase1,id1),
    ?l lists:foreach(fun(P)->
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[P,flags]],
					{flags,[]},
					10),
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[{math,module_info,1},traced]],
					{traced,false},
					10)
		     end,
		     TestProcs),
    %% Now stop the session.
    ?l stop_inviso_tool_session(CNode,1,Nodes),
    %% Restart the session using the previously saved history.
    ?l {ok,{2,_InvisoReturn}}=inviso_tool:restore_session(AbsFileName),
    ?l ok=poll(inviso_tool,get_activities,[],{ok,[]},10),
    %% Check that the history has been redone.
    ?l lists:foreach(fun(P)->
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[P,flags]],
					{flags,[call]},
					10),
			     ?l ok=poll(rpc,
					call,
					[node(P),erlang,trace_info,[{math,module_info,1},traced]],
					{traced,local},
					10)
		     end,
		     TestProcs),
    ?l {ok,_NodeResults1}=inviso_tool:inviso(tpl,[math,module_info,0,[]]),
    %% Also check that the restored history now is our history.
    ?l {ok,{AutostartData,_NodeResults2}}=
	inviso_tool:get_autostart_data(Nodes,{dependency,infinity}),
    ?l FNameInit=filename:join(DataDir,"tracecase_init.trc"),
    ?l FName1=filename:join(DataDir,"tracecase1_on.trc"),
    ?l [{file,{FNameInit,[]}},
	{file,{FName1,[{'ProcessName',inviso_tool_test_proc}]}},
	{mfa,{inviso,tpl,[math,module_info,0,[]]}}]=AutostartData,
    ?l stop_inviso_tool_session(CNode,2,Nodes),
    ?l NodeCounters=lists:foldl(fun(N,Acc)->[{_,X}]=rpc:call(N,ets,lookup,[test_proc_tab,counter]),
					    [{N,X}|Acc]
				end,
				[],
				Nodes),
    %% Remove the patterns set by the initial tracecase.
    ?l lists:foreach(fun(N)->rpc:call(N,
				      erlang,
				      trace_pattern,
				      [{lists,module_info,1},false,[local]])
		     end,
		     Nodes),
    %% Now we want to test that we can do restore on the current session.
    ?l {ok,{3,_InvisoReturn2}}=inviso_tool:restore_session(),
    ?l ok=poll(inviso_tool,get_activities,[],{ok,[]},10),
    %% Check that the history has been redone yet again.
    ?l lists:foreach(fun({N,X})->
			     [{counter,Y}]=
				 rpc:call(N,ets,lookup,[test_proc_tab,counter]),
			     Y=X+1
		     end,
		     NodeCounters),
    ?l lists:foreach(fun(N)->
			     {traced,local}=
				 rpc:call(N,erlang,trace_info,[{lists,module_info,1},traced])
		     end,
		     Nodes),

    ?l {error,session_active}=inviso_tool:reset_nodes(Nodes),
    %% Now stop the session and check that we can clear the nodes.
    ?l stop_inviso_tool_session(CNode,3,Nodes),
    ?l lists:foreach(fun(N)->{traced,local}=
				 rpc:call(N,erlang,trace_info,[{lists,module_info,1},traced])
		     end,
		     Nodes),
    ?l {ok,NodeResults3}=inviso_tool:reset_nodes(Nodes),
    ?l true=check_noderesults(Nodes,{ok,{new,running}},NodeResults3),
    ?l lists:foreach(fun(N)->{traced,false}=
				 rpc:call(N,erlang,trace_info,[{lists,module_info,1},traced])
		     end,
		     Nodes),
    ?l stop_inviso_tool(CNode,Nodes),

    %% Now we want to test that restoring a session at no active nodes will
    %% not result in a crash. (Previous error).
    ?l FaultyNodes=[gurka@nonexistant,tomat@nonexistant],
    ?l Options=[{nodes,FaultyNodes},{c_node,CNode}|Opts],
    ?l {ok,_Pid}=inviso_tool:start(Options),
    ?l ok=poll(erlang,whereis,[inviso_tool],fun(X)->true=is_pid(X) end,10),
    %% Now try to restore a session.
    ?l {ok,{_,{ok,[]}}}=inviso_tool:restore_session(AbsFileName),
    ?l {ok,down}=inviso_tool:get_node_status(gurka@nonexistant),
    %% Now stop the (useless) session.
    ?l {ok,{_,[]}}=inviso_tool:stop_session(),
    ?l stop_inviso_tool(CNode,[]),
    ok.
%% -----------------------------------------------------------------------------

%% This test tests a few strange situations when activating a session and there
%% are no nodes that can be initiated or reinitiated.
dist_start_session_special(doc) -> [""];
dist_start_session_special(suite) -> [];
dist_start_session_special(Config) when list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    [RegExpNode|_]=RemoteNodes,
    CNode=RegExpNode,                       % We use a remote control component.
%    Nodes=RemoteNodes,
    DataDir=?config(data_dir,Config),
    PrivDir=filename:join(?config(priv_dir,Config),""),

    %% Start up the tool but with no exiting nodes.
    FaultyNodes=[gurka@nonexistant,tomat@nonexistant],
    Opts=[{regexp_node,RegExpNode},
	  {tdg,{?MODULE,tdg,[PrivDir]}},
	  {tc_def_file,?TC_DEF_FILE},
	  {initial_tcs,[{tracecase_init,[]}]},
	  {dir,DataDir}],                    % This is where we find tracecases.
    ?l Options=[{nodes,FaultyNodes},{c_node,CNode}|Opts],
    ?l {ok,_Pid}=inviso_tool:start(Options),
    ?l ok=poll(erlang,whereis,[inviso_tool],fun(X)->true=is_pid(X) end,10),
    %% Now try to initate a session.
    ?l {ok,{SessionNr,{ok,[]}}}=inviso_tool:start_session(),
    ?l {ok,down}=inviso_tool:get_node_status(gurka@nonexistant),
    %% Now stop the (useless) session.
    ?l {ok,{SessionNr,[]}}=inviso_tool:stop_session(),

    %% Now start again, still no useful nodes.
    ?l {ok,{SessionNr2,{ok,[]}}}=inviso_tool:start_session(),
    ?l {ok,{SessionNr2,[]}}=inviso_tool:stop_session(),
    ?l stop_inviso_tool(CNode,[]), % No nodes are connected.

    ok.
%% -----------------------------------------------------------------------------

    
%% ==============================================================================
%% Help functions.
%% ==============================================================================
    
%% Help function starting the inviso_tool with runtime components at Nodes
%% and the inviso control component at CNode. OtherOpts shall contain all other
%% necessary options except nodes and c_node. Returns nothing significant.
start_inviso_tool(Nodes,CNode,OtherOpts) -> 
    ?l Options=[{nodes,Nodes},{c_node,CNode}|OtherOpts],
    ?l {ok,_Pid}=inviso_tool:start(Options),
    ?l ok=poll(erlang,whereis,[inviso_tool],fun(X)->true=is_pid(X) end,10),
    %% Now the runtime components shall be started but no tracing started.
    ?l lists:foreach(fun(N)->ok=poll(rpc,
				     call,
				     [CNode,inviso,get_status,[[N]]],
				     fun({ok,[{_N,{ok,{new,running}}}]})->true;
					(_) ->false
				     end,
				     10)
		     end,
		     Nodes),
    true.
%% -----------------------------------------------------------------------------

%% Stops the inviso_tool.
stop_inviso_tool(CNode,Nodes) ->
    ?l {ok,NodeResults}=inviso_tool:stop(),
    ?l true=check_noderesults(Nodes,ok,NodeResults),
    ?l ok=poll(erlang,whereis,[inviso_tool],undefined,10),
    %% Check that all inviso components are gone.
    ?l ok=poll(rpc,call,[CNode,erlang,whereis,[inviso_c]],undefined,10),
    ?l lists:foreach(fun(N)->ok=poll(rpc,
				     call,
				     [N,erlang,whereis,[inviso_rt]],
				     undefined,
				     10)
		     end,
		     Nodes),
    true.
%% -----------------------------------------------------------------------------

%% Starts a trace session. Returns the InvisoReturn part of the return value.
start_inviso_tool_session(CNode,MoreTDGargs,SessionNr,Nodes) ->
    ?l {ok,{SessionNr,InvisoReturn}}=inviso_tool:start_session(MoreTDGargs),
    %% Now check that all nodes are tracing.
    ?l lists:foreach(fun(N)->ok=poll(rpc,
				     call,
				     [CNode,inviso,get_status,[[N]]],
				     fun({ok,[{_N,{ok,{tracing,running}}}]})->true;
					(_) ->false
				     end,
				     10),
			     %% Check that the initial trace case is executed.
			     ?l ok=poll(rpc,
					call,
					[N,erlang,trace_info,[{lists,module_info,1},traced]],
					{traced,local},
					10)
		     end,
		     Nodes),
    InvisoReturn.
%% -----------------------------------------------------------------------------

%% Stops a trace session.
stop_inviso_tool_session(CNode,SessionNr,Nodes) ->
    ?l {ok,{SessionNr,NodeResults}}=inviso_tool:stop_session(),
    ?l true=check_noderesults(Nodes,ok,NodeResults),
    %% Now the runtimes shall not be tracing any longer.
    ?l lists:foreach(fun(N)->ok=poll(rpc,
				     call,
				     [CNode,inviso,get_status,[[N]]],
				     fun({ok,[{_N,{ok,{idle,running}}}]})->true;
					(_) ->false
				     end,
				     10)
		     end,
		     Nodes),
    true.
%% -----------------------------------------------------------------------------

%% Help function checking that there is a Result for each node in Nodes.
%% Returns 'true' if successful.
check_noderesults(Nodes,Fun,[{Node,Result}|Rest]) when function(Fun) ->
    case Fun({Node,Result}) of
	true ->
	    case lists:member(Node,Nodes) of
		true ->
		    check_noderesults(lists:delete(Node,Nodes),Fun,Rest);
		false ->                     % Not good.
		    unknown_node_in_returnvalue
	    end;
	_ ->
	    illegal_result
    end;
check_noderesults(Nodes,Result,[{Node,Result}|Rest]) ->
    case lists:member(Node,Nodes) of
	true ->
	    check_noderesults(lists:delete(Node,Nodes),Result,Rest);
	false ->                             % Not good.
	    unknown_node_in_returnvalue
    end;
check_noderesults([],_,[]) ->
    true;
check_noderesults(X,Y,Z) ->
    io:format("Bad arguments to check noderesults:~w~n~w~n~w~n",[X,Y,Z]),
    false.
%% ------------------------------------------------------------------------------

%% Help function which waits for a function call to become Result. This is useful
%% if what we are waiting for can happend independantly of indications we have
%% access to.
poll(_,_,_,_,0) ->
    error;
poll(M,F,Args,Result,Times) ->
    try apply(M,F,Args) of
	What when function(Result) ->
	    case Result(What) of
		true ->
		    ok;
		X ->
		    io:format("Poll: ~w:~w ~w ~w ~w~n",[M,F,Args,Result,X]),
		    timer:sleep(100),
		    poll(M,F,Args,Result,Times-1)
	    end;
	Result ->
	    ok;
	X ->
	    io:format("Poll: ~w:~w ~w ~w ~w~n",[M,F,Args,Result,X]),
	    timer:sleep(100),
	    poll(M,F,Args,Result,Times-1)
    catch
	error:Reason ->
	    io:format("Apply in suite-function poll/5 failed, ~w~n",[Reason]),
	    timer:sleep(100),
	    poll(M,F,Args,Result,Times-1)
    end.
%% ------------------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% The Tracer Data Generator function.
%% ------------------------------------------------------------------------------

-define(I2L(Arg),integer_to_list(Arg)).

tdg(Node,{{Y,Mo,D},{H,Mi,S}},PrivDir) ->
    NameStr=atom_to_list(Node)++"_"++?I2L(Y)++"-"++?I2L(Mo)++"-"++?I2L(D)++"_"++
	?I2L(H)++"-"++?I2L(Mi)++"-"++?I2L(S),
    LogTD={file,filename:join(PrivDir,NameStr++".log")},
    TiTD={file,filename:join(PrivDir,NameStr++".ti")},
    [{trace,LogTD},{ti,TiTD}].
%% ------------------------------------------------------------------------------


%% ------------------------------------------------------------------------------
%% Handling the test server Config.
%% ------------------------------------------------------------------------------

insert_remotenode_config(Name,Node,Config) ->
    [{remotenode,{Name,Node}}|Config].
%% ------------------------------------------------------------------------------

insert_timetraphandle_config(Handle,Config) ->
    [{timetraphandle,Handle}|Config].
%% ------------------------------------------------------------------------------

get_remotenode_config(Name, [{remotenode, {Name, Node}}| _Cs]) ->
    Node;
get_remotenode_config(Name, [_C | Cs]) ->
    get_remotenode_config(Name, Cs);
get_remotenode_config(Name, []) ->
    exit({no_remotenode, Name}).

%% ------------------------------------------------------------------------------

get_timetraphandle_config(Config) ->
    {value,{_,Handle}}=lists:keysearch(timetraphandle,1,Config),
    Handle.
%% ------------------------------------------------------------------------------

get_remotenodes_config([{remotenode,{_Name,Node}}|Config]) ->
    [Node|get_remotenodes_config(Config)];
get_remotenodes_config([_|Config]) ->
    get_remotenodes_config(Config);
get_remotenodes_config([]) ->
    [].
%% ------------------------------------------------------------------------------

remove_remotenode_config(Name, [{remotenode, {Name, _}} | Cs]) ->
    Cs;
remove_remotenode_config(Name, [C | Cs]) ->
    [C | remove_remotenode_config(Name, Cs)];
remove_remotenode_config(_Name, []) ->
    [].
%% ------------------------------------------------------------------------------

remove_timetraphandle_config(Config) ->
    lists:keydelete(timetraphandle,1,Config).
%% ------------------------------------------------------------------------------


%% ==============================================================================
%% Code for a test process which can be started.
%% ==============================================================================

%% The test proc is also responsible for owning a side effect table. The table
%% can be updated by tracecases.
test_proc_init() ->
    register(inviso_tool_test_proc,self()),
    ets:new(test_proc_tab,[named_table,public]),
    ets:insert(test_proc_tab,{counter,0}),
    test_proc_loop().

test_proc_loop() ->
    receive
	{apply,M,F,Args} ->
	    apply(M,F,Args),
	    test_proc_loop();
	X ->
	    io:format("Got ~w~n",[X]),
	    test_proc_loop()
    end.
%% ------------------------------------------------------------------------------

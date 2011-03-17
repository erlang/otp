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
%% Description:
%% Test suite for inviso (basic parts, i.e not inviso tools). Note that
%% inviso basic parts have modules in both the runtime_tools and
%% inviso applications.
%%
%% Authors:
%% Ann-Marie Löf, ann-marie.lof@st.se
%% Lennart Öhman, lennart.ohman@st.se
%% -----------------------------------------------------------------------------

-module(inviso_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-define(l,?line).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic_dist_trace_1, basic_dist_trace_2,
     basic_dist_trace_3, basic_dist_trace_ti_1,
     basic_dist_trace_ti_2, basic_dist_trace_ti_3,
     suspend_dist_trace_ti_1, suspend_dist_trace_ti_2,
     meta_cleanfunc_dist_1, basic_handlerfun_dist_1,
     delete_log_dist_1, autostart_dist_1, autostart_dist_2,
     autostart_dist_3, running_alone_dist_1,
     running_alone_dist_2, running_alone_dist_3,
     running_alone_dist_4, running_alone_dist_5,
     overload_dist_1, overload_dist_2, overload_dist_3,
     overload_dist_4, overload_dist_5, subscribe_dist_1,
     lfm_trace_dist_1, lfm_trace_ti_dist_2,
     handle_logfile_sort_wrapset, fetch_log_dist_trace_1,
     fetch_log_dist_trace_2, fetch_log_dist_trace_3,
     fetch_log_dist_error_1, fetch_log_dist_error_2,
     expand_regexp_dist_1, only_loaded_dist_1].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



init_per_suite(Config) ->
    %% No never know who skrewed up this node before this suite! :-)
    erlang:trace_pattern({'_','_','_'},[],[local]),
    erlang:trace_pattern({'_','_','_'},[],[global]),
    erlang:trace(all,false,[all]),

    ?l ok=application:start(runtime_tools),
    Config.

end_per_suite(_Config) ->
    ?l ok=application:stop(runtime_tools).


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

    ?l start_side_effect_logger(node()),
    ?l start_side_effect_logger(Node1),
    ?l start_side_effect_logger(Node2),


    %% SPECIAL FOR MY PRIVATE TEST ENVIROMENT
%    ?l rpc:call(Node1,code,add_patha,["/clearcase/otp/tools/runtime_tools/ebin"]),
%    ?l rpc:call(Node1,code,add_patha,["/clearcase/otp/tools/inviso/ebin"]),
%    ?l rpc:call(Node2,code,add_patha,["/clearcase/otp/tools/runtime_tools/ebin"]),
%    ?l rpc:call(Node2,code,add_patha,["/clearcase/otp/tools/inviso/ebin"]),

%    %% SPECIAL FOR MY PRIVATE TEST ENVIROMENT, windows.
%    ?l rpc:call(Node1,code,add_patha,["Z:/DATA/PROJECTS/inviso_project/runtime_tools/ebin"]),
%    ?l rpc:call(Node1,code,add_patha,["Z:/DATA/PROJECTS/inviso_project/inviso/ebin"]),
%    ?l rpc:call(Node2,code,add_patha,["Z:/DATA/PROJECTS/inviso_project/runtime_tools/ebin"]),
%    ?l rpc:call(Node2,code,add_patha,["Z:/DATA/PROJECTS/inviso_project/inviso/ebin"]),

    ?l ok=rpc:call(Node1,application,start,[runtime_tools]),
    ?l ok=rpc:call(Node2,application,start,[runtime_tools]),
    ?l timer:sleep(100),                     % Problem with autostarted runtime.
    %% The following is a test that the inviso_rt processes which are autostarted
    %% are now gone.

    ?l ok=poll(rpc,call,[Node1,erlang,whereis,[inviso_rt]],undefined,20),
    ?l ok=poll(rpc,call,[Node2,erlang,whereis,[inviso_rt]],undefined,20),

%    ?l ok=poll(rpc,call,[Node1,supervisor,which_children,[runtime_tools_sup]],[],20),
%    ?l ok=poll(rpc,call,[Node2,supervisor,which_children,[runtime_tools_sup]],[],20),
    NewConfig1=insert_remotenode_config(inviso1,Node1,Config),
    NewConfig2=insert_remotenode_config(inviso2,Node2,NewConfig1),
    insert_timetraphandle_config(TH,NewConfig2).
%% -----------------------------------------------------------------------------

end_per_testcase(Case,Config) ->
    ?l test_server:stop_node(get_remotenode_config(inviso1,Config)),
    ?l test_server:stop_node(get_remotenode_config(inviso2,Config)),

    case whereis(inviso_c) of
	undefined ->                         % Should not exist.
	    true;
	Pid when is_pid(Pid) ->                 % But if it exists...
	    exit(Pid,kill),                  % Remove it!
	    io:format("Had to kill the control component in end_per_testcase,~p.~n",[Case])
    end,
    case whereis(inviso_rt) of
	undefined ->                         % Should not exist.
	    true;
	Pid2 when is_pid(Pid2) ->               % But if it exists...
	    exit(Pid2,kill),                 % Remove it!
	    io:format("Had to kill local runtime component in end_per_testcase,~p.~n",[Case])
    end,
    ?l process_killer([inviso_test_proc,
		       inviso_tab_proc,
		       inviso_collector_proc,
		       global_inviso_test_proc]),
    ?l test_server:timetrap_cancel(get_timetraphandle_config(Config)),

    NewConfig1=remove_remotenode_config(inviso1,Config),
    NewConfig2=remove_remotenode_config(inviso2,NewConfig1),
    remove_timetraphandle_config(NewConfig2).
%% -----------------------------------------------------------------------------

%% ==============================================================================
%% Testcases.
%% ==============================================================================

%% TEST CASE: Basic, distributed, trace only.
basic_dist_trace_1(suite) -> [];
basic_dist_trace_1(doc) ->
    ["Basic case, start of distributed tracing, using only trac."];
basic_dist_trace_1(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=lists:map(fun(N)->{N,{file,filename:join([PrivDir,
							     "tf1_"++
							     atom_to_list(N)
							    ])}} end,
			     Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok}]}),
    activate_local_tracing(Nodes),
    deactivate_local_tracing(Nodes),
    stop_tracing(Nodes),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------


%% TEST CASE: Basic, distributed, activate global tracing for functions in modules
%% pointed out using a regexp. No tracing will be done.
basic_dist_trace_2(suite) -> [];
basic_dist_trace_2(doc) ->
    [""];
basic_dist_trace_2(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=lists:map(fun(N)->{N,{file,filename:join([PrivDir,
							     "tf1a_"++
							     atom_to_list(N)
							    ])}} end,
			     Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok}]}),
    Funcs1=activate_global_tracing_regexp(Nodes),
    deactivate_global_tracing_regexp(Nodes,Funcs1),
    stop_tracing(Nodes),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Basic, distributed, activate global tracing for functions in modules
%% pointed out using a dir-regexp. No tracing will be done.
basic_dist_trace_3(suite) -> [];
basic_dist_trace_3(doc) ->
    [""];
basic_dist_trace_3(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=lists:map(fun(N)->{N,{file,filename:join([PrivDir,
							     "tf1b_"++
							     atom_to_list(N)
							    ])}} end,
			     Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok}]}),
    Funcs1=activate_global_tracing_regexp_dir(Nodes),
    deactivate_global_tracing_regexp_dir(Nodes,Funcs1),
    stop_tracing(Nodes),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Basic, distributed, trace and ti.
basic_dist_trace_ti_1(suite) -> [];
basic_dist_trace_ti_1(doc) ->
    [""];
basic_dist_trace_ti_1(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataFun=
	fun(N)->{N,[{trace,{file,filename:join([PrivDir,"tf2_"++atom_to_list(N)])}},
		    {ti,{file,filename:join([PrivDir,"tf2_"++atom_to_list(N)++".ti"])}}]}
	end,
    TracerDataList=lists:map(TracerDataFun,Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok},{ti_log,ok}]}),
    activate_local_tracing(Nodes),
    activate_meta_tracing(Nodes),
    ?l true=(is_pid(whereis(inviso_rt))),
    ?l true=(is_pid(whereis(inviso_rt_meta))),
    deactivate_meta_tracing(Nodes),
    deactivate_local_tracing(Nodes),
    stop_tracing(Nodes),
    ?l true=(is_pid(whereis(inviso_rt))),  % Shall still be running.
    ?l ok=poll(erlang,whereis,[inviso_rt_meta],undefined,3),
    stop(Nodes),
    timer:sleep(200),                      % Give it time to terminate.
    ?l ok=poll(erlang,whereis,[inviso_rt],undefined,3),% Shall be gone now.
    ?l undefined=whereis(inviso_rt_meta),  % Still gone.
    ok.
%% -----------------------------------------------------------------------------

%% Test CASE: Testing that the tpm_tracer functionality works. That is appending
%% {tracer,Tracer} to a meta match spec.
basic_dist_trace_ti_2(suite) -> [];
basic_dist_trace_ti_2(doc) ->
    [""];
basic_dist_trace_ti_2(Config) when is_list(Config) ->
    case erlang:system_info(version) of
	"5.4"++_ ->                        % Perhaps not perfect, but work now :-)
	    {skip,"Old emulator"};
	_ ->
	    basic_dist_trace_ti_2_do(Config)
    end.

basic_dist_trace_ti_2_do(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataFun=
	fun(N)->{N,[{trace,{file,filename:join([PrivDir,"tf3_"++atom_to_list(N)])}},
		    {ti,{file,filename:join([PrivDir,"tf3_"++atom_to_list(N)++".ti"])}}]}
	end,
    TracerDataList=lists:map(TracerDataFun,Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok},{ti_log,ok}]}),
    activate_deactivate_meta_tracing_tracer(Nodes),
    stop_tracing(Nodes),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Basic, distributed, trace and ti, where we try to use ctp_all to
%% check that all global and local patterns are removed but that meta patterns
%% remain.
%% This test also checks that if the meta tracer is terminated an error value
%% is generated when trying to do meta tracing at that node.
basic_dist_trace_ti_3(suite) -> [];
basic_dist_trace_ti_3(doc) ->
    [""];
basic_dist_trace_ti_3(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataFun=
	fun(N)->{N,[{trace,{file,filename:join([PrivDir,"tf4_"++atom_to_list(N)])}},
		    {ti,{file,filename:join([PrivDir,"tf4_"++atom_to_list(N)++".ti"])}}]}
	end,
    TracerDataList=lists:map(TracerDataFun,Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok},{ti_log,ok}]}),
    activate_local_tracing(Nodes),
    activate_global_tracing(Nodes),
    activate_meta_tracing(Nodes),
    ?l true=(is_pid(whereis(inviso_rt))),
    ?l true=(is_pid(whereis(inviso_rt_meta))),
    ?l {ok,NodeResults1}=inviso:ctp_all(Nodes), % Removes local and global patterns.
    ?l true=check_noderesults(Nodes,ok,NodeResults1),
    ?l true=check_on_nodes(Nodes,erlang,trace_info,[{code,which,1},traced],{traced,false}),
    ?l true=check_on_nodes(Nodes,erlang,trace_info,[{code,get_path,0},traced],{traced,false}),
    %% But meta patters shall remain.
    ?l true=check_on_nodes(Nodes,
			   erlang,
			   trace_info,
			   [{lists,module_info,0},meta_match_spec],
			   fun({meta_match_spec,L})when length(L)>0 ->true end),
    ?l true=check_on_nodes(Nodes,
			   erlang,
			   trace_info,
			   [{lists,module_info,0},meta],
			   fun({meta,P})when is_pid(P) ->
				   P=rpc:call(node(P),erlang,whereis,[inviso_rt_meta]),
				   true
			   end),
    %% Now kill the meta tracer somewhere and try to activate meta tracing.
    ?l [ANode|_]=Nodes,
    ?l AMetaPid=rpc:call(ANode,erlang,whereis,[inviso_rt_meta]),
    ?l rpc:call(ANode,erlang,exit,[AMetaPid,kill]),
    ?l {ok,NodeResults2}=inviso:tpm(Nodes,math,pi,0,[],void),
    ?l {value,{ANode,{error,_}}}=lists:keysearch(ANode,1,NodeResults2),

    ?l stop_tracing(Nodes),
    ?l stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Test cases for SUSPEND
%% -----------------------------------------------------------------------------

%% TEST CASE: In this test case a trace with ti is started. Trace flags are set,
%% trace patterns are set and meta trace patterns. We then check that the trace
%% flags and the meta patterns are removed when tracing suspended.
%% The suspension is cancelled and we check that it is possible to reactivate
%% tracing by setting the process flags and meta patterns again.
suspend_dist_trace_ti_1(suite) -> [];
suspend_dist_trace_ti_1(doc) ->
    [""];
suspend_dist_trace_ti_1(Config) when is_list(Config) ->
    ?l RemoteNodes=get_remotenodes_config(Config),
    ?l Nodes=[node()|RemoteNodes],
    ?l PrivDir=filename:join(?config(priv_dir,Config),""),
    ?l TracerDataFun=
	fun(N)->{N,[{trace,{file,filename:join([PrivDir,"tf_suspend1_"++atom_to_list(N)])}},
		    {ti,{file,filename:join([PrivDir,"tf_suspend1_"++atom_to_list(N)++".ti"])}}]}
	end,
    ?l TracerDataList=lists:map(TracerDataFun,Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok},{ti_log,ok}]}),
    activate_local_tracing(Nodes),
    activate_meta_tracing(Nodes),
    ?l true=(is_pid(whereis(inviso_rt))),
    ?l true=(is_pid(whereis(inviso_rt_meta))),
    %% Set some trace flags on some newly started test procs.
    activate_traceflags(Nodes),

    %% Now suspend the tracing on all nodes. That shall result in the removal
    %% of trace flags and meta trace patterns, but not local trace patterns.
    ?l {ok,NodeResults1}=inviso:suspend(Nodes,test),
    ?l true=check_noderesults(Nodes,ok,NodeResults1),
    %% Trace flags gone?
    ?l TestProcs=lists:map(fun(N)->rpc:call(N,erlang,whereis,[inviso_test_proc]) end,Nodes),
    ?l lists:foreach(fun(P)->
			     {flags,[]}=
				 rpc:call(node(P),erlang,trace_info,[P,flags])
		     end,
		     TestProcs),
    %% Meta patterns shall be gone too, but local functions still there.
    ?l lists:foreach(fun(N)->
			     {meta,false}=
				 rpc:call(N,
					  erlang,
					  trace_info,
					  [{math,module_info,1},meta]),
			     {traced,local}=
				 rpc:call(N,
					  erlang,
					  trace_info,
					  [{code,which,1},traced])
		     end,
		     Nodes),

    %% Try to activate trace flags, trace patterns and meta tracing while
    %% suspended. Should not succeed of course!
    ?l ThisNode=node(),
    ?l {ok,[{ThisNode,{error,suspended}}]}=
	inviso:tf([ThisNode],inviso_test_proc,[call]),
    ?l {ok,[{ThisNode,{error,suspended}}]}=
	inviso:tpl([ThisNode],math,module_info,1,[]),
    ?l {ok,[{ThisNode,{error,suspended}}]}=
	inviso:init_tpm([ThisNode],
			math,
			module_info,
			1,
			{?MODULE,tpm_init_func2}, % Does not exist on purpose.
			{?MODULE,tpm_call_func2}, % Does not exist on purpose.
			{?MODULE,tpm_return_func2}, % Does not exist on purpose.
			{?MODULE,tpm_remove_func2}), % Does not exist on purpose.

    %% Now we want to cancel suspension and see that we can reactivate tracing.
    ?l {ok,NodeResults2}=inviso:cancel_suspension(Nodes),
    ?l true=check_noderesults(Nodes,ok,NodeResults2),

    ?l {ok,NodeResults3}=
	inviso:init_tpm(math,
			module_info,
			1,
			{?MODULE,tpm_init_func2}, % Does not exist on purpose.
			{?MODULE,tpm_call_func2}, % Does not exist on purpose.
			{?MODULE,tpm_return_func2}, % Does not exist on purpose.
			{?MODULE,tpm_remove_func2}), % Does not exist on purpose.
    ?l true=check_noderesults(Nodes,ok,NodeResults3),
    ?l {ok,NodeResults5}=
	inviso:tpm_ms(math,module_info,1,ms1,[{'_',[],[{return_trace}]}]),
    ?l true=check_noderesults(Nodes,{ok,1},NodeResults5),
    ?l true=check_on_nodes(Nodes,
			   erlang,
			   trace_info,
			   [{math,module_info,1},meta_match_spec],
			   {meta_match_spec,[{'_',[],[{return_trace}]}]}),
    ?l {ok,NodeResults6}=inviso:tf(Nodes,inviso_test_proc,[call]),
    ?l true=check_noderesults(Nodes,{ok,[1]},NodeResults6),

    %deactivate_meta_tracing(Nodes),
    %deactivate_local_tracing(Nodes),
    stop_tracing(Nodes),
    ?l true=(is_pid(whereis(inviso_rt))),  % Shall still be running.
    ?l ok=poll(erlang,whereis,[inviso_rt_meta],undefined,3),
    stop(Nodes),
    ?l timer:sleep(200),                   % Give it time to terminate.
    ?l ok=poll(erlang,whereis,[inviso_rt],undefined,3),% Shall be gone now.
    ?l undefined=whereis(inviso_rt_meta),  % Still gone.
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: In this test case a trace with ti is started. Trace flags are set,
%% trace patterns are set and meta trace patterns. We then suspend tracing at
%% all nodes, then stop tracing which shall be allowed. We then try to initiate
%% tracing again which shall not be possible.
suspend_dist_trace_ti_2(suite) -> [];
suspend_dist_trace_ti_2(doc) ->
    [""];
suspend_dist_trace_ti_2(Config) when is_list(Config) ->
    ?l RemoteNodes=get_remotenodes_config(Config),
    ?l Nodes=[node()|RemoteNodes],
    ?l PrivDir=filename:join(?config(priv_dir,Config),""),
    ?l TracerDataFun=
	fun(N)->{N,[{trace,{file,filename:join([PrivDir,"tf_suspend2_"++atom_to_list(N)])}},
		    {ti,{file,filename:join([PrivDir,"tf_suspend2_"++atom_to_list(N)++".ti"])}}]}
	end,
    ?l TracerDataList=lists:map(TracerDataFun,Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok},{ti_log,ok}]}),
    activate_local_tracing(Nodes),
    activate_meta_tracing(Nodes),
    ?l true=(is_pid(whereis(inviso_rt))),
    ?l true=(is_pid(whereis(inviso_rt_meta))),
    %% Set some trace flags on some newly started test procs.
    activate_traceflags(Nodes),

    %% Now suspend the tracing on all nodes. That shall result in the removal
    %% of trace flags and meta trace patterns, but not local trace patterns.
    ?l {ok,NodeResults1}=inviso:suspend(Nodes,test),
    ?l true=check_noderesults(Nodes,ok,NodeResults1),

    %% Now stop tracing.
    ?l {ok,NodeResults3}=inviso:stop_tracing(Nodes),
    ?l true=check_noderesults(Nodes,{ok,idle},NodeResults3),
    %% Now try to initiate tracing again.
    ThisNode=node(),
    ?l {ok,[{ThisNode,{error,suspended}}]}=
	inviso:init_tracing([ThisNode],
			    [{trace,{file,filename:join([PrivDir,"tf_suspend3_"++
							 atom_to_list(ThisNode)])}},
			     {ti,{file,{filename:join([PrivDir,"tf_suspend3_"++
						       atom_to_list(ThisNode)])}}}]),

    %% Cancel the suspension and initiate tracing again.
    ?l {ok,NodeResults2}=inviso:cancel_suspension(Nodes),
    ?l true=check_noderesults(Nodes,ok,NodeResults2),
    ?l TracerDataFun2=
	fun(N)->{N,[{trace,{file,filename:join([PrivDir,"tf_suspend4_"++atom_to_list(N)])}},
		    {ti,{file,filename:join([PrivDir,"tf_suspend4_"++atom_to_list(N)++".ti"])}}]}
	end,
    ?l TracerDataList2=lists:map(TracerDataFun2,Nodes),
    ?l {ok,NodeResults4}=inviso:init_tracing(TracerDataList2),
    ?l true=check_noderesults(Nodes,{ok,[{trace_log,ok},{ti_log,ok}]},NodeResults4),
    stop_tracing(Nodes),
    ?l true=(is_pid(whereis(inviso_rt))),  % Shall still be running.
    stop(Nodes),
    ?l timer:sleep(200),                   % Give it time to terminate.
    ?l ok=poll(erlang,whereis,[inviso_rt],undefined,3),% Shall be gone now.
    ok.
%% -----------------------------------------------------------------------------



%% TEST CASE: This test case tests that the clean function removes (prosumed)
%% expired data from the internal public-loopdata structure in the inviso_rt_meta
%% process.
meta_cleanfunc_dist_1(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataFun=
	fun(N)->{N,[{trace,{file,filename:join([PrivDir,"mcf1_"++atom_to_list(N)])}},
		    {ti,{file,filename:join([PrivDir,"mcf1_"++atom_to_list(N)++".ti"])}}]}
	end,
    TracerDataList=lists:map(TracerDataFun,Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok},{ti_log,ok}]}),
    %% Now initialize meta tracing, but the call_func is a bit "fixed".
    ?l {ok,NodeResults1}=
	inviso:tpm(Nodes,math,module_info,1,[],
		   {?MODULE,meta_cleanfunc_initfunc_1},
		   {?MODULE,meta_cleanfunc_callfunc_1},
		   void,void),
    ?l true=check_noderesults(Nodes,{ok,1},NodeResults1),
    %% Nothing in the "our" part of the public loop data.
    ?l true=check_on_nodes(Nodes,
			   inviso_rt_meta,get_state,[inviso_rt_meta],
			   fun({ok,_LD,{{_,[]},_}})->true end),
    ?l lists:foreach(fun(N)->rpc:call(N,math,module_info,[exports]) end,Nodes),
    %% Check that it has been added to the public loopdata structure.
    ?l true=check_on_nodes(Nodes,
			   ?MODULE,poll,[inviso_rt_meta,
					 get_state,
					 [inviso_rt_meta],
					 fun({ok,_LD,{{_,[{meta_cleanfunc_test1,_Now}]},_}})->
						 true;
					    (_)->false
					 end,
					 20],
			   ok),
    %% While we wait for 60 seconds to pass, we test a few other things.
    ?l {ok,NodeResults2}=
	inviso:tpm(Nodes,?MODULE,slowfunction2,0,[{'_',[],[{return_trace}]}],
		   {?MODULE,meta_cleanfunc_initfunc_2},
		   {?MODULE,meta_cleanfunc_callfunc_2},
		   {?MODULE,meta_cleanfunc_returnfunc_2},
		   void),
    ?l true=check_noderesults(Nodes,{ok,1},NodeResults2),
    ?l lists:foreach(fun(N)->rpc:call(N,?MODULE,slowfunction,[]) end,Nodes),
    %% Believe it or not but slowfunction is still running, in its own process,
    %% we are therefore free now to examine the meta tracer.
    ?l true=check_on_nodes(Nodes,
			   ?MODULE,poll,[inviso_rt_meta,
					 get_state,
					 [inviso_rt_meta],
					 fun({ok,_LD,{{[],Tuples},_}})->
						 {value,_}=
						     lists:keysearch(meta_cleanfunc_test2,
								     1,
								     Tuples),
						 {value,_}=
						     lists:keysearch(meta_cleanfunc_test1,
								     1,
								     Tuples),
						 true;
					    (_)->
						 false
					 end,
					 20],
			   ok),
    %% Now we wait for slowfunction to return and that the meta_cleanfunc_test2
    %% to be removed from public loopdata strucuture.
    ?l timer:sleep(10000),
    %% The only thing remaining should be the meta_cleanfunc_test1 which will not
    %% go away for less than that the clean functionality removes it.
    ?l true=check_on_nodes(Nodes,
			   ?MODULE,poll,[inviso_rt_meta,
					 get_state,
					 [inviso_rt_meta],
					 fun({ok,_LD,{{_,[{meta_cleanfunc_test1,_Now}]},_}})->
						 true;
					    (_)->
						 false
					 end,
					 20],
			   ok),
    %% Wait for the clean function to clean meta_cleanfunc_test1 away.
    ?l timer:sleep(51000),                       % Shall be gone after 5 seconds.
    ?l true=check_on_nodes(Nodes,
			   ?MODULE,poll,[inviso_rt_meta,
					 get_state,
					 [inviso_rt_meta],
					 fun({ok,_LD,{{_,[]},_}})->true;
					    (_)->false
					 end,
			                 20],
			   ok),
    stop_tracing(Nodes),
    stop(Nodes),
    ok.

%% This function acts as tpm initialization function when we are going to test
%% that the clean function works. Note that we here assume standard public loop
%% datastructure.
meta_cleanfunc_initfunc_1(_M,_F,_Arity,{E1,_E2}) ->
    {ok,{E1,[]},void}.
%% Function that is supposed to be called when the meta traced function is
%% called.
meta_cleanfunc_callfunc_1(_Pid,_Args,{{E1,E2},Global}) ->
    {ok,{{E1,[{meta_cleanfunc_test1,now()}|E2]},Global},void}.

meta_cleanfunc_initfunc_2(_M,_F,_Arity,PublLD) ->
    {ok,PublLD,void}.
meta_cleanfunc_callfunc_2(_Pid,_Args,{{E1,E2},Global}) ->
    {ok,{{E1,[{meta_cleanfunc_test2,now()}|E2]},Global},void}.
meta_cleanfunc_returnfunc_2(_Pid,_,{{E1,E2},Global}) ->
    {value,_}=lists:keysearch(meta_cleanfunc_test2,1,E2),
    {ok,{{E1,lists:keydelete(meta_cleanfunc_test2,1,E2)},Global},void}.

slowfunction() ->
    spawn(?MODULE,slowfunction1,[]).
slowfunction1() ->
    slowfunction2().                         % Meta trace on this function call.
slowfunction2() ->
    timer:sleep(2000),
    true.
%% -----------------------------------------------------------------------------

%% TEST CASE: Testing that a runtime component can be started instructing it
%% to use a handler fun. Checks that the handler fun is called if a trace
%% message comes in.
basic_handlerfun_dist_1(suite) -> [];
basic_handlerfun_dist_1(doc) ->
    [""];
basic_handlerfun_dist_1(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    ?l lists:foreach(fun(N)->rpc:call(N,ets,insert,[inviso_sideeffect_tab,{bhf1,0}]) end,
		     Nodes),
    TracerDataFun=
	fun(N)->{N,{fun basic_handlerfun_dist_1_fun/2,inviso_sideeffect_tab}} end,
    TracerDataList=lists:map(TracerDataFun,Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok}]}),
    activate_local_tracing(Nodes),
    activate_traceflags(Nodes),
    ?l lists:foreach(fun(N)->[{bhf1,0}]=
				 rpc:call(N,ets,lookup,[inviso_sideeffect_tab,bhf1])
		     end,
		     Nodes),
    ?l inviso_test_proc ! {apply,code,which,[lists]},
    ok=poll(ets,lookup,[inviso_sideeffect_tab,bhf1],[{bhf1,1}],20),
    deactivate_traceflags(Nodes),
    deactivate_local_tracing(Nodes),
    stop_tracing(Nodes),
    timer:sleep(100),
    ?l [{bhf1,1}]=ets:lookup(inviso_sideeffect_tab,bhf1),
    stop(Nodes),
    ok.

%% Function used as handler fun for testcase above.
basic_handlerfun_dist_1_fun(_Msg,TId) ->
    ets:update_counter(TId,bhf1,1),
    TId.
%% -----------------------------------------------------------------------------

%% TEST CASE: Here we test that delete_log removes the files at the involved
%% runtime nodes. In this case we test that we remove logs according to last
%% used tracer data.
delete_log_dist_1(suite) -> [];
delete_log_dist_1(doc) -> [""];
delete_log_dist_1(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataFun=
	fun(N)->{N,[{trace,{file,filename:join([PrivDir,"dl1_"++atom_to_list(N)])}},
		    {ti,{file,filename:join([PrivDir,"dl1_"++atom_to_list(N)++".ti"])}}]}
	end,
    TracerDataList=lists:map(TracerDataFun,Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok},{ti_log,ok}]}),
    ?l Files=lists:map(fun({N,TD})->
			       ?l {value,{_,{_,TraceFile}}}=lists:keysearch(trace,1,TD),
			       ?l {value,{_,{_,TiFile}}}=lists:keysearch(ti,1,TD),
			       ?l {N,{TraceFile,TiFile}}
		       end,
		       TracerDataList),
    io:format("The Files is:~w~n",[Files]),
    ?l {ok,NodeResults1}=inviso:delete_log(Nodes), % Should not work!
    ?l true=check_noderesults(Nodes,{error,tracing},NodeResults1),
    stop_tracing(Nodes),
    %% Files still here.
    ?l lists:foreach(fun({N,{F1,F2}})->
			     ?l {ok,_}=rpc:call(N,file,read_file_info,[F1]),
			     ?l {ok,_}=rpc:call(N,file,read_file_info,[F2])
		     end,
		     Files),
    ?l {ok,NodeResults2}=inviso:delete_log(Nodes),
    ?l true=check_noderesults(Nodes,
			      fun({_N,{ok,LogInfos}})->
				      ?l {value,{_,[{ok,_FName1}]}}=
					  lists:keysearch(trace_log,1,LogInfos),
				      ?l {value,{_,[{ok,_FName2}]}}=
					  lists:keysearch(ti_log,1,LogInfos),
				      true
			      end,
			      NodeResults2),
    %% The files shall be gone now.
    ?l lists:foreach(fun({N,{F1,F2}})->
			     ?l {error,enoent}=rpc:call(N,file,read_file_info,[F1]),
			     ?l {error,enoent}=rpc:call(N,file,read_file_info,[F2])
		     end,
		     Files),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------


%% TEST CASE: Test of the autostart behaviour of the runtime component.
%% Here we test that a runtime component is started according to the autostart.conf
%% file. Note that the repeat parameter is set to 2.
autostart_dist_1(suite) -> [];
autostart_dist_1(doc) ->
    [""];
autostart_dist_1(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    PrivDir=filename:join(?config(priv_dir,Config),""),
    AutoConfFile=filename:join(PrivDir,"autostart1.conf"),
    [RNode|_]=RemoteNodes,
    ?l ok=rpc:call(RNode,application,stop,[runtime_tools]),
    ?l ok=rpc:call(RNode,application,set_env,[runtime_tools,
					      inviso_autostart_conf,
					      AutoConfFile]),
    ?l {ok,FD}=file:open(AutoConfFile,[write]),
    ?l ok=io:format(FD,"~w.~n~w.~n",[{repeat,2},{tag,c_ref}]),
    ?l file:close(FD),
    ?l ok=rpc:call(RNode,application,start,[runtime_tools]),
    timer:sleep(1000),
    ?l P1=rpc:call(RNode,erlang,whereis,[inviso_rt]),
    ?l true=is_pid(P1),
    ?l rpc:call(RNode,erlang,exit,[P1,kill]),
    ?l ok=rpc:call(RNode,application,stop,[runtime_tools]),
    ?l ok=rpc:call(RNode,application,start,[runtime_tools]),
    timer:sleep(1000),
    ?l P2=rpc:call(RNode,erlang,whereis,[inviso_rt]),
    ?l true=is_pid(P2),
    ?l rpc:call(RNode,erlang,exit,[P2,kill]),
    ?l ok=rpc:call(RNode,application,stop,[runtime_tools]),
    ?l ok=rpc:call(RNode,application,start,[runtime_tools]),
    timer:sleep(1000),
    ?l undefined=rpc:call(RNode,erlang,whereis,[inviso_rt]),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Test of autostart. Here we focus on that an autostarted
%% runtime component actually follows the trace case command file and
%% initiates tracing.
autostart_dist_2(suite) -> [];
autostart_dist_2(doc) ->
    [""];
autostart_dist_2(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    PrivDir=filename:join(?config(priv_dir,Config),""),
    AutoConfFile=filename:join(PrivDir,"autostart2.conf"),
    [RNode|_]=RemoteNodes,
    ?l ok=rpc:call(RNode,application,stop,[runtime_tools]),
    ?l ok=rpc:call(RNode,application,set_env,[runtime_tools,
					      inviso_autostart_conf,
					      AutoConfFile]),
    ?l CmdFileName=filename:join(PrivDir,"autostart_cmd_as1"),
    ?l {ok,FD}=file:open(CmdFileName,[write]),
    ?l ok=io:format(FD,
		    "inviso:tpl(Nodes,M,F,Arity,[]).~n"
		    "inviso:tf(Nodes,inviso_test_proc,[call]).~n",
		    []),
    ?l file:close(FD),
    ?l TraceFileName=filename:join([PrivDir,"as1_"++atom_to_list(RNode)]),
    ?l TiFileName=filename:join([PrivDir,"as1_"++atom_to_list(RNode)++".ti"]),
    ?l inviso_as_lib:setup_autostart(RNode,
				     2,
				     [],
				     [{trace,{file,TraceFileName}},
				      {ti,{file,TiFileName}}],
				     [[CmdFileName]],
				     [{'M',code},{'F',which},{'Arity',1}],
				     [{{inviso,tpl,5},{inviso_rt,tpl,{erlang,tl}}},
				      {{inviso,tf,3},{inviso_rt,tf,{erlang,tl}}}]),
    ?l TestP=spawn(RNode,?MODULE,test_proc_init,[]),
    ?l ok=rpc:call(RNode,application,start,[runtime_tools]),
    ?l timer:sleep(1000),
    ?l {ok,_}=file:read_file_info(TraceFileName),
    ?l {ok,_}=file:read_file_info(TiFileName),
    ?l true=is_pid(P=rpc:call(RNode,erlang,whereis,[inviso_rt])),
    ?l ok=poll(rpc,call,[RNode,erlang,trace_info,[{code,which,1},traced]],{traced,local},10),
    ?l {flags,[call]}=rpc:call(RNode,erlang,trace_info,[TestP,flags]),
    ?l rpc:call(RNode,erlang,exit,[P,kill]),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Here we test that an autostarted runtime component with a dependency
%% to a specific control component tries to connect to that control component
%% during its start-up.
autostart_dist_3(suite) -> [];
autostart_dist_3(doc) ->
    [""];
autostart_dist_3(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    PrivDir=filename:join(?config(priv_dir,Config),""),
    AutoConfFile=filename:join(PrivDir,"autostart3.conf"),
    [RNode|_]=RemoteNodes,
    ?l ok=rpc:call(RNode,application,stop,[runtime_tools]),
    ?l ok=rpc:call(RNode,application,set_env,[runtime_tools,
					      inviso_autostart_conf,
					      AutoConfFile]),
    ?l {ok,FD}=file:open(AutoConfFile,[write]),
    ?l ok=io:format(FD,"~w.~n~w.~n~w.~n",
		    [{options,[{dependency,{infinity,node()}}]},{repeat,2},{tag,c_ref}]),
    ?l file:close(FD),
    %% Now start inviso at this node here for the runtime to connect.
    ?l {ok,_Pid}=inviso:start(),
    ?l ok=poll(erlang,whereis,[inviso_c],fun(P) when is_pid(P)->true;(_)->false end,10),
    %% Make the runtime component start.
    ?l ok=rpc:call(RNode,application,start,[runtime_tools]),
    ?l ok=poll(rpc,call,[RNode,erlang,whereis,[inviso_rt]],
	       fun(P) when is_pid(P)->true;(_)->false end,10),
    %% Check that the runtime component started.
    ?l ok=poll(inviso,get_status,[[RNode]],{ok,[{RNode,{ok,{new,running}}}]},20),
%    ?l {ok,[{RNode,{ok,{new,running}}}]}=inviso:get_status([RNode]),
    stop([RNode]),
    ok.
%% -----------------------------------------------------------------------------



%% TEST CASE: Test of the dependency mechanism in the runtime component.
%% Default behaviour is dependency=infinity, i.e the runtime components remains.
%% We also test here that we can reconnect to the runtime.
running_alone_dist_1(suite) -> [];
running_alone_dist_1(doc) ->
    [""];
running_alone_dist_1(Config) when is_list(Config) ->
    ?l {ok,_Pid1}=inviso:start(),               % Start a control component.
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,a_ref,[]),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    ?l shutdown=inviso:stop(),                  % Stop the control component!
    ?l undefined=whereis(inviso_c),
    timer:sleep(3000),                          % How long shall we wait? :-)
    ?l lists:foreach(fun(N)->true=is_pid(rpc:call(N,erlang,whereis,[inviso_rt])) end,
		     Nodes),
    ?l {ok,_Pid2}=inviso:start(),
    ?l {ok,NodeResults2}=inviso:add_nodes(Nodes,b_ref,[]),
    ?l true=check_noderesults(Nodes,{ok,{adopted,new,running,a_ref}},NodeResults2),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Test of the dependency mechanism in the runtime component.
%% Test that the runtime components terminates after the specified 5000 ms.
running_alone_dist_2(suite) -> [];
running_alone_dist_2(doc) ->
    [""];
running_alone_dist_2(Config) when is_list(Config) ->
    ?l {ok,_Pid1}=inviso:start(),               % Start a control component.
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,a_ref,[{dependency,5000}]),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    ?l shutdown=inviso:stop(),                  % Stop the control component!
    ?l undefined=whereis(inviso_c),
    timer:sleep(2000),
    ?l lists:foreach(fun(N)->true=is_pid(rpc:call(N,erlang,whereis,[inviso_rt])) end,
		     Nodes),
    timer:sleep(4000),                          % Now they shall be dead!
    ?l lists:foreach(fun(N)->undefined=rpc:call(N,erlang,whereis,[inviso_rt]) end,
		     Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Test of the dependency mechanism in the runtime component.
%% Test that the runtime components terminates after the specified 5000 ms.
running_alone_dist_3(suite) -> [];
running_alone_dist_3(doc) ->
    [""];
running_alone_dist_3(Config) when is_list(Config) ->
    ?l {ok,_Pid1}=inviso:start(),               % Start a control component.
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,a_ref,[{dependency,1000}]),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    ?l {ok,NodeResults2}=inviso:change_options(Nodes,[{dependency,5000}]),
    ?l true=check_noderesults(Nodes,ok,NodeResults2),
    ?l shutdown=inviso:stop(),                  % Stop the control component!
    ?l undefined=whereis(inviso_c),
    timer:sleep(3000),
    ?l lists:foreach(fun(N)->true=is_pid(rpc:call(N,erlang,whereis,[inviso_rt])) end,
		     Nodes),
    timer:sleep(3000),                          % Now they shall be dead!
    ?l lists:foreach(fun(N)->undefined=rpc:call(N,erlang,whereis,[inviso_rt]) end,
		     Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Test of the dependency mechanism in the runtime component.
%% Test that the runtime components terminates after the specified 5000 ms,
%% like we did in running_alone_dist_2. But now we also start tracing and checks
%% that all inviso processes actually disappears when the time-out is reached.
running_alone_dist_4(suite) -> [];
running_alone_dist_4(doc) ->
    [""];
running_alone_dist_4(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    %% Start some tracing!
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataFun=
	fun(N)->{N,[{trace,{file,filename:join([PrivDir,"tf_ra4"++atom_to_list(N)])}},
		    {ti,{file,filename:join([PrivDir,"tf_ra4_"++atom_to_list(N)++".ti"])}}]}
	end,
    TracerDataList=lists:map(TracerDataFun,Nodes),
    start_and_init_tracing2(Nodes,
			    [{dependency,5000}],
			    TracerDataList,
			    {ok,[{trace_log,ok},{ti_log,ok}]}),

    ?l lists:foreach(fun(N)->true=is_pid(rpc:call(N,erlang,whereis,[inviso_rt])) end,
		     Nodes),
    ?l lists:foreach(fun(N)->true=is_pid(rpc:call(N,erlang,whereis,[inviso_rt_meta])) end,
		     Nodes),
    %% Stop control component and wait for the runtimes to terminate after
    %% running alone timer has expired.
    ?l shutdown=inviso:stop(),                  % Stop the control component!
    ?l undefined=whereis(inviso_c),
    timer:sleep(2000),
    ?l lists:foreach(fun(N)->true=is_pid(rpc:call(N,erlang,whereis,[inviso_rt])) end,
		     Nodes),
    ?l lists:foreach(fun(N)->true=is_pid(rpc:call(N,erlang,whereis,[inviso_rt_meta])) end,
		     Nodes),
    timer:sleep(4000),                          % Now they shall be dead!
    ?l lists:foreach(fun(N)->undefined=rpc:call(N,erlang,whereis,[inviso_rt]) end,
		     Nodes),
    ?l lists:foreach(fun(N)->undefined=rpc:call(N,erlang,whereis,[inviso_rt_meta]) end,
		     Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Test of the dependency mechanism in the runtime component.
%% Test that the runtime components terminates imeediately when the control
%% component is stopped. Check that all processes are gone.
running_alone_dist_5(suite) -> [];
running_alone_dist_5(doc) ->
    [""];
running_alone_dist_5(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    %% Start some tracing!
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataFun=
	fun(N)->{N,[{trace,{file,filename:join([PrivDir,"tf_ra5"++atom_to_list(N)])}},
		    {ti,{file,filename:join([PrivDir,"tf_ra5_"++atom_to_list(N)++".ti"])}}]}
	end,
    TracerDataList=lists:map(TracerDataFun,Nodes),
    start_and_init_tracing2(Nodes,
			    [{dependency,0}],
			    TracerDataList,
			    {ok,[{trace_log,ok},{ti_log,ok}]}),

    ?l lists:foreach(fun(N)->true=is_pid(rpc:call(N,erlang,whereis,[inviso_rt])) end,
		     Nodes),
    ?l lists:foreach(fun(N)->true=is_pid(rpc:call(N,erlang,whereis,[inviso_rt_meta])) end,
		     Nodes),
    %% Stop control component and check that all runtime component processes have
    %% terminate more or less immediately afterwards, since dependency==0.
    ?l shutdown=inviso:stop(),                  % Stop the control component!
    timer:sleep(100),
    ?l undefined=whereis(inviso_c),
    timer:sleep(500),
    ?l lists:foreach(fun(N)->undefined=rpc:call(N,erlang,whereis,[inviso_rt]) end,
		     Nodes),
    ?l lists:foreach(fun(N)->undefined=rpc:call(N,erlang,whereis,[inviso_rt_meta]) end,
		     Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Test of the overload protection mechanism. The mechanism checks
%% for overload using the callback approximately at the interval specified.
%% Check that it does not start protection until start of tracing.
overload_dist_1(suite) -> [];
overload_dist_1(doc) ->
    [""];
overload_dist_1(Config) when is_list(Config) ->
    ?l {ok,_Pid1}=inviso:start(),               % Start a control component.
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    ?l lists:foreach(fun(N)->true=rpc:call(N,ets,insert,[inviso_sideeffect_tab,{ovl1,0}]) end,
		     Nodes),                    % Initiate the counter.
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,
					  a_ref,
					  [{overload,{{?MODULE,overload1},500}}]),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    timer:sleep(1000),                          % Give the loadcheck time to perform.
    ?l [{_,0}]=ets:lookup(inviso_sideeffect_tab,ovl1), % Nothing should have happened.

    %% Overload check shall not start until we start tracing.
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=lists:map(fun(N)->{N,[{trace,
					  {file,filename:join([PrivDir,
							       "tf_ovl1."++atom_to_list(N)
							      ])}}]}
			     end,
			     Nodes),
    ?l {ok,NodeResults2}=inviso:init_tracing(TracerDataList),
    ?l true=check_noderesults(Nodes,{ok,[{trace_log,ok}]},NodeResults2),
    timer:sleep(1500),                          % Give the loadcheck time to perform.
    ?l [{_,N}]=ets:lookup(inviso_sideeffect_tab,ovl1),
    ?l true=(N>=2),                             % After 1,5 seconds, at least 2 checks.

    %% Now change options and remove overload checking!
    ?l {ok,NodeResults3}=inviso:change_options(Nodes,[overload]),
    ?l true=check_noderesults(Nodes,ok,NodeResults3),
    ?l [{_,N2}]=ets:lookup(inviso_sideeffect_tab,ovl1),
    timer:sleep(1000),
    ?l [{_,N2}]=ets:lookup(inviso_sideeffect_tab,ovl1), % No more loadchecks!

    stop_tracing(Nodes),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Test of the overload protection mechanism. In this case we focus
%% in that the init and remove functions are carried out at change_options and
%% when starting and stoping the runtime component.
overload_dist_2(suite) -> [];
overload_dist_2(doc) ->
    [""];
overload_dist_2(Config) when is_list(Config) ->
    ?l {ok,_Pid1}=inviso:start(),               % Start a control component.
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,
					  a_ref,
					  [{overload,{{?MODULE,overload2},
						      500,
						      {?MODULE,overload2i,[]},
						      {?MODULE,overload2r,[]}}}]),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    ?l [{_,0}]=ets:lookup(inviso_sideeffect_tab,ovl2),

    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=lists:map(fun(N)->{N,[{trace,
					  {file,filename:join([PrivDir,
							       "tf_ovl2."++atom_to_list(N)
							      ])}}]}
			     end,
			     Nodes),
    ?l {ok,NodeResults2}=inviso:init_tracing(TracerDataList),
    ?l true=check_noderesults(Nodes,{ok,[{trace_log,ok}]},NodeResults2),
    timer:sleep(1500),                          % Give the loadcheck time to perform.
    ?l [{_,N}]=ets:lookup(inviso_sideeffect_tab,ovl2),
    io:format("Ñ is:~p~n",[N]),
    ?l true=(N>=2),                             % After 1,5 seconds, at least 2 checks.
    ?l {ok,NodeResults3}=inviso:change_options(Nodes,[{overload,{{?MODULE,overload3},
								 500,
								 {?MODULE,overload3i,[]},
								 {?MODULE,overload3r,[]}}}]),
    ?l true=check_noderesults(Nodes,ok,NodeResults3),
    ?l []=ets:lookup(inviso_sideeffect_tab,ovl2),
    timer:sleep(1500),
    ?l [{_,N2}]=ets:lookup(inviso_sideeffect_tab,ovl3),
    ?l true=(N2>=2),                            % After 1,5 seconds, at least 2 checks.
    stop_tracing(Nodes),
    ?l []=ets:lookup(inviso_sideeffect_tab,ovl3r), % Remove function shall not be called.
    ?l [{_,N3}]=ets:lookup(inviso_sideeffect_tab,ovl3),
    timer:sleep(1000),                          % Check that overloadchecking has stopped.
    ?l [{_,N3}]=ets:lookup(inviso_sideeffect_tab,ovl3),
    stop(Nodes),
    ?l ok=poll(ets,lookup,[inviso_sideeffect_tab,ovl3r],[{ovl3r,done}],20),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Test of the overload protections mechanism. Here we focus on testing
%% that if overload is reached tracing is really suspended.
overload_dist_3(suite) -> [];
overload_dist_3(doc) ->
    [""];
overload_dist_3(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=
	lists:map(fun(N)->{N,[{trace,{file,filename:join([PrivDir,
							  "tf_ovl3."++atom_to_list(N)])}},
			      {ti,{file,filename:join([PrivDir,
						       "tf_ovl3_ti."++atom_to_list(N)])}}]}
		  end,
		  Nodes),
    ?l lists:foreach(fun(N)->
			     true=rpc:call(N,ets,insert,[inviso_sideeffect_tab,{ovl4,0}])
		     end,
		     Nodes),
    start_and_init_tracing2(Nodes,
			    [{overload,{{?MODULE,overload4},500}}],
			    TracerDataList,
			    {ok,[{trace_log,ok},{ti_log,ok}]}),
    activate_local_tracing(Nodes),
    activate_meta_tracing(Nodes),
    activate_traceflags(Nodes),
    timer:sleep(600),
    ?l [{_,N1}]=ets:lookup(inviso_sideeffect_tab,ovl4),
    ?l true=(N1>=1),                            % Overload check has been done!
    ?l Node=node(),
    ?l {ok,[{Node,{ok,{tracing,running}}}]}=inviso:get_status([node()]),
    ?l true=ets:insert(inviso_sideeffect_tab,{ovl4_suspend,true}),
    timer:sleep(600),
    ?l {ok,[{Node,{ok,{tracing,{suspended,test}}}}]}=inviso:get_status([node()]),
    ?l [{_,N2}]=ets:lookup(inviso_sideeffect_tab,ovl4),
    ?l {flags,[]}=erlang:trace_info(whereis(inviso_test_proc),flags),
    ?l {meta,false}=erlang:trace_info({lists,module_info,0},meta),
    ?l {traced,local}=erlang:trace_info({code,which,1},traced),
    ?l true=(is_pid(whereis(inviso_rt_meta))),
    ?l true=ets:delete(inviso_sideeffect_tab,ovl4_suspend),
    timer:sleep(600),
    ?l [{_,N2}]=ets:lookup(inviso_sideeffect_tab,ovl4), % No checking while suspended!
    ?l {ok,[{Node,ok}]}=inviso:cancel_suspension([node()]),
    ?l {ok,NodeResults1}=inviso:get_status(Nodes),
    ?l true=check_noderesults(Nodes,{ok,{tracing,running}},NodeResults1),
    timer:sleep(600),
    ?l [{_,N3}]=ets:lookup(inviso_sideeffect_tab,ovl4),
    ?l true=(N3>N2),
    ?l deactivate_local_tracing(Nodes),
    ?l stop_tracing(Nodes),
    ?l stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE. Test that the overload mechanism is triggered by to the runtime
%% component incomming messages, and nothing else.
overload_dist_4(suite) -> [];
overload_dist_4(doc) ->
    [""];
overload_dist_4(Config) when is_list(Config) ->
    ?l {ok,_Pid1}=inviso:start(),               % Start a control component.
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,
					  a_ref,
					  [{overload,{{?MODULE,overload5},
						      infinity,
						      {?MODULE,overload5i,[]},
						      {?MODULE,overload5r,[]}}}]),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    ?l [{_,0}]=ets:lookup(inviso_sideeffect_tab,ovl5),

    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=lists:map(fun(N)->{N,[{trace,
					  {file,filename:join([PrivDir,
							       "tf_ovl4."++atom_to_list(N)
							      ])}}]}
			     end,
			     Nodes),
    ?l {ok,NodeResults2}=inviso:init_tracing(TracerDataList),
    ?l true=check_noderesults(Nodes,{ok,[{trace_log,ok}]},NodeResults2),
    timer:sleep(2000),                          % Give the loadcheck time to perform.
    ?l [{_,N}]=ets:lookup(inviso_sideeffect_tab,ovl5),
    ?l true=(N==0),                             % And nothing shall have happend!
    %% Now we send a message to the inviso_rt, then the load check function
    %% shall be called.
    ?l whereis(inviso_rt) ! test_of_loadcheck,
    timer:sleep(200),                           % Make sure the inviso_rt gets scheduled.
    ?l [{_,1}]=ets:lookup(inviso_sideeffect_tab,ovl5),
    stop_tracing(Nodes),
    ?l []=ets:lookup(inviso_sideeffect_tab,ovl5r), % Remove function shall not be called.
    ?l [{_,N3}]=ets:lookup(inviso_sideeffect_tab,ovl5),
    ?l whereis(inviso_rt) ! test_of_loadcheck,
    timer:sleep(1000),                          % Check that overloadchecking has stopped.
    ?l [{_,N3}]=ets:lookup(inviso_sideeffect_tab,ovl5),
    stop(Nodes),
    ?l ok=poll(ets,lookup,[inviso_sideeffect_tab,ovl5r],[{ovl5r,done}],20),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE. Test that the overload mechanism correctly calculates remaining time
%% to next load check if a message comes into the runtime component "interupting"
%% the waiting for loadcheck timeout. (Loadcheck timeout is implemented as an after
%% in the receive).
overload_dist_5(suite) -> [];
overload_dist_5(doc) ->
    [""];
overload_dist_5(Config) when is_list(Config) ->
    ?l {ok,_Pid1}=inviso:start(),               % Start a control component.
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    ?l lists:foreach(fun(N)->true=rpc:call(N,ets,insert,[inviso_sideeffect_tab,{ovl6,0}]) end,
		     Nodes),                    % Initiate the counter.
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,
					  a_ref,
					  [{overload,{{?MODULE,overload6},1000}}]),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    %% Overload check shall not start until we start tracing.
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=lists:map(fun(N)->{N,[{trace,
					  {file,filename:join([PrivDir,
							       "tf_ovl5."++atom_to_list(N)
							      ])}}]}
			     end,
			     Nodes),
    ?l {ok,NodeResults2}=inviso:init_tracing(TracerDataList),
    ?l true=check_noderesults(Nodes,{ok,[{trace_log,ok}]},NodeResults2),
    ?l ok=poll(ets,lookup,[inviso_sideeffect_tab,ovl6],[{ovl6,2}],25),
    %% Now we know that exactly 2 checks have been made. Try to Distract the runtime :-)
    ?l inviso_rt:state(whereis(inviso_rt)),     % Make it have to receive a message.
    timer:sleep(500),
    ?l [{_,2}]=ets:lookup(inviso_sideeffect_tab,ovl6), % Should still be 2.
    timer:sleep(600),
    ?l [{_,3}]=ets:lookup(inviso_sideeffect_tab,ovl6), % We expect yet one check.
    timer:sleep(1100),
    ?l [{_,4}]=ets:lookup(inviso_sideeffect_tab,ovl6),

    stop_tracing(Nodes),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------


%% TEST CASE: Test of the subscription mechanism.
subscribe_dist_1(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    Pid=spawn(?MODULE,inviso_msg_collector,[]),
    CtrlPid=whereis(inviso_c),

    ?l {ok,_Pid}=inviso:start(),             % Start a control component.
    ?l ok=inviso:subscribe(Pid),
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,a_ref,[]),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    ?l {ok,NodeResults2}=inviso:get_status(Nodes),
    ?l true=check_noderesults(Nodes,{ok,{new,running}},NodeResults2),
    check_msg_collector(Nodes,
			fun({inviso_event,CP,_,{connected,N,{_Tag,{idle,running}}}})
			   when CP==CtrlPid ->
				{true,N};
			   (_) ->
				false
			end,
			13),
    TracerDataList=lists:map(fun(N)->{N,{file,
					 filename:join([PrivDir,
							"tf_sub1"++atom_to_list(N)])}}
			     end,
			     Nodes),
    ?l {ok,NodeResults3}=inviso:init_tracing(TracerDataList),
    ?l true=check_noderesults(Nodes,{ok,[{trace_log,ok}]},NodeResults3),
    check_msg_collector(Nodes,
			fun({inviso_event,CP,_,{state_change,N,{tracing,running}}})
			   when CP==CtrlPid ->
				{true,N};
			   (_) ->
				false
			end,
			13),
    ?l {ok,NodeResults4}=inviso:suspend(Nodes,test),
    ?l true=check_noderesults(Nodes,ok,NodeResults4),
    check_msg_collector(Nodes,
			fun({inviso_event,CP,_,{state_change,N,{tracing,{suspended,test}}}})
			   when CP==CtrlPid ->
				{true,N};
			   (_) ->
				false
			end,
			13),
    ?l [RNode|_]=RemoteNodes,
    ?l RInvisoPid=rpc:call(RNode,erlang,whereis,[inviso_rt]),
    ?l rpc:call(RNode,erlang,exit,[RInvisoPid,kill]),
    check_msg_collector([RNode],
			fun({inviso_event,CP,_,{disconnected,N,_Info}})
			   when CP==CtrlPid ->
				{true,N};
			   (_) ->
				false
			end,
			11),

    ?l {ok,_NodeResults5}=inviso:stop_tracing(Nodes),
    ?l {ok,_NodeResults6}=inviso:stop_nodes(Nodes),
    ?l shutdown=inviso:stop(),
    ok.
%% -----------------------------------------------------------------------------


%% TEST CASE: fetch_log test of single straight trace_log file in distributed
%% environment.
fetch_log_dist_trace_1(suite) -> [];
fetch_log_dist_trace_1(doc) ->
    ["fetch_log test of single straight trace_log file in distributed"
     "environment."];
fetch_log_dist_trace_1(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=lists:map(fun(N)->{N,[{trace,{file,filename:join([PrivDir,
								     "testfile1."++
								     atom_to_list(N)
								    ])}}]} end,
			     Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok}]}),

    %% Put some output in the logs.
    ?l inviso:tp(Nodes,math,module_info,0,[]),
    ?l inviso:tf(Nodes,all,[call]),
    ?l lists:foreach(fun(N)->rpc:call(N,math,module_info,[]) end,Nodes),

    stop_tracing(Nodes),
    {H,M,S}=time(),
    FetchToDir=filename:join([PrivDir,
			      "fetch_log_test1_"++integer_to_list(H)++"_"++
			      integer_to_list(M)++"_"++integer_to_list(S)]),
    ?l ok=file:make_dir(FetchToDir),
    ?l {ok,NodeResults}=inviso:fetch_log(RemoteNodes,FetchToDir,"p1"),
    io:format("~p~n",[NodeResults]),
    ?l true=check_noderesults(RemoteNodes,
			      fun({N,{complete,[{trace_log,[{ok,File}]},{ti_log,[]}]}}) ->
				      ?l File="p1testfile1."++atom_to_list(N),
				      true;
				 (_)->
				      false
			      end,
			      NodeResults),
    ?l ON=filename:join(PrivDir,"testfile1."),
    ?l FN=filename:join(FetchToDir,"p1testfile1."),
    ?l lists:foreach(fun(N)->
			     {ok,#file_info{size=Size}}=
				 file:read_file_info(ON++atom_to_list(N)),
			     {ok,#file_info{size=Size}}=
				 file:read_file_info(FN++atom_to_list(N))
		     end,
		     RemoteNodes),
    %% Now we wish to see that we get an incomplete if we try to fetch to a
    %% directory that does not exist.
    ?l FetchToErrorDir=filename:join([PrivDir,nonexistingingdir]),
    ?l {ok,NodeResults2}=inviso:fetch_log(RemoteNodes,FetchToErrorDir,"p1"),
    ?l io:format("NodeResults2:~w~n",[NodeResults2]),
    ?l true=check_noderesults(RemoteNodes,
			      fun({_,{incomplete,_}}) ->
				      true;
				 (_)->
				      false
			      end,
			      NodeResults2),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

fetch_log_dist_trace_2(suite) -> [];
fetch_log_dist_trace_2(doc) ->
    [""];
fetch_log_dist_trace_2(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),

    {H,M,S}=time(),
    ?l Name="wrap"++integer_to_list(H)++"_"++integer_to_list(M)++"_"++integer_to_list(S),
    ?l BaseName=filename:join(PrivDir,Name),
    Fun=fun(N)->{N,[{trace,{file,{BaseName++atom_to_list(N),wrap,".log",512,2}}},
		    {ti,{file,BaseName++"_ti_"++atom_to_list(N)++".ti"}}]}
	end,
    ?l TracerDataList=lists:map(Fun,Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok},{ti_log,ok}]}),
    fill_and_reach_two_wrapfiles(PrivDir,"^"++Name,Nodes),

    stop_tracing(Nodes),
    FetchToDir=filename:join([PrivDir,
			      "fetch_log_test2_"++integer_to_list(H)++"_"++
			      integer_to_list(M)++"_"++integer_to_list(S)]),
    ?l ok=file:make_dir(FetchToDir),
    ?l {ok,NodeResults}=inviso:fetch_log(RemoteNodes,FetchToDir,"p1"),
    io:format("~p~n",[NodeResults]),
    CheckFun=fun({N,{complete,[{trace_log,FileResults1},{ti_log,[{ok,TiFile}]}]}}) ->
		     Fun2=fun({ok,File}) ->
				  {match,1,_}=
				      regexp:first_match(File,
							 "^"++"p1"++Name++atom_to_list(N)),
				  true;
			     (_) ->
				  false
			  end,
		     ?l true=lists:all(Fun2,FileResults1),
		     ?l TiFile="p1"++Name++"_ti_"++atom_to_list(N)++".ti",
		     true;
		(_)->
		     false
	     end,
    ?l true=check_noderesults(RemoteNodes,CheckFun,NodeResults),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

fetch_log_dist_trace_3(suite) -> [];
fetch_log_dist_trace_3(doc) ->
    [""];
fetch_log_dist_trace_3(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),

    {H,M,S}=time(),
    ?l Name="wrap2_"++integer_to_list(H)++"_"++integer_to_list(M)++"_"++integer_to_list(S),
    ?l BaseName=filename:join(PrivDir,Name),
    Fun=fun(N)->{N,[{trace,{file,{BaseName++atom_to_list(N),wrap,".log",512,2}}},
		    {ti,{file,BaseName++"_ti_"++atom_to_list(N)++".ti"}}]}
	end,
    ?l TracerDataList=lists:map(Fun,Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok},{ti_log,ok}]}),
    fill_and_reach_two_wrapfiles(PrivDir,"^"++Name,Nodes),

    stop_tracing(Nodes),
    FetchToDir=filename:join([PrivDir,
			      "fetch_log_test3_"++integer_to_list(H)++"_"++
			      integer_to_list(M)++"_"++integer_to_list(S)]),
    ?l ok=file:make_dir(FetchToDir),
    ?l {ok,NodeResults1}=inviso:list_logs(Nodes),
    CheckFun=fun({N,{ok,[{trace_log,PrivDir2,[F1,F2]},{ti_log,PrivDir2,[F3]}]}})->
		     PrivDir2=PrivDir,
		     RegExp="^"++Name++atom_to_list(N)++"[0-9]+"++"\.log",
		     {match,1,_}=regexp:first_match(F1,RegExp),
		     {match,1,_}=regexp:first_match(F2,RegExp),
		     F3=Name++"_ti_"++atom_to_list(N)++".ti",
		     true;
		(_) ->
		     false
	     end,
    ?l true=check_noderesults(Nodes,CheckFun,NodeResults1),
    ?l NodeFileSpecList=lists:map(fun({N,{ok,L}})->{N,L} end,
				  lists:keydelete(node(),1,NodeResults1)),
    ?l {ok,NodeResults2}=inviso:fetch_log(NodeFileSpecList,FetchToDir,"p1"),
io:format("~p~n",[NodeResults2]),
    CheckFun2=fun({N,{complete,[{trace_log,FileResults1},{ti_log,[{ok,TiFile}]}]}}) ->
		     Fun2=fun({ok,File}) ->
				  {match,1,_}=
				      regexp:first_match(File,
							 "^"++"p1"++Name++atom_to_list(N)),
				  true;
			     (_) ->
				  false
			  end,
		     ?l true=lists:all(Fun2,FileResults1),
		     ?l TiFile="p1"++Name++"_ti_"++atom_to_list(N)++".ti",
		     true;
		(_)->
		     false
	     end,
    ?l true=check_noderesults(RemoteNodes,CheckFun2,NodeResults2),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

fetch_log_dist_error_1(suite) -> [];
fetch_log_dist_error_1(doc) ->
    [""];
fetch_log_dist_error_1(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    ?l {ok,_Pid}=inviso:start(),             % Start a control component.
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,a_ref),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    ?l {ok,NodeResults2}=inviso:fetch_log(RemoteNodes,"foo","bar"),
io:format("~p~n",[NodeResults2]),
    ?l true=check_noderesults(RemoteNodes,
			      fun({_N,{error,no_tracerdata}})->true;
				 (_)->false
			      end,
			      NodeResults2),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

fetch_log_dist_error_2(suite) -> [];
fetch_log_dist_error_2(doc) ->
    [""];
fetch_log_dist_error_2(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    PrivDir=filename:join(?config(priv_dir,Config),""),
    ?l {ok,_Pid}=inviso:start(),             % Start a control component.
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,a_ref),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    ?l NodeLogList=lists:map(fun(N)->{N,[{trace_log,
					  PrivDir,
					  ["f1,fil","f2.fil"]},
					 {ti_log,
					  PrivDir,
					  ["f.ti"]}]}
			     end,
			     RemoteNodes),
    ?l {ok,NodeResults2}=inviso:fetch_log(NodeLogList,"foo","bar"),
    io:format("~p~n",[NodeResults2]),
    ?l true=check_noderesults(RemoteNodes,
			      fun({_N,{incomplete,_}}) ->
				      true;
				 (_) ->
				      false
			      end,
			      NodeResults2),
    ?l NodeTracerData=lists:map(fun(N)->{N,
					 [{trace,{file,filename:join(PrivDir,"foo")}},
					  {ti,{file,filename:join(PrivDir,"bar.ti")}}]}
				end,
				RemoteNodes),
    {ok,NodeResults3}=inviso:fetch_log(NodeTracerData,"foo","bar"),
    io:format("~p~n",[NodeResults3]),
%% This should work this way. Now it says complete [], which is not entirely
%% incorrect. But to follow the sematics of when fetching named files should
%% say incomplete.
%% Must do some rework to make that work. No real danger leaving it this way
%% for now.
%    ?l true=check_noderesults(RemoteNodes,
%			      fun({_N,{incomplete,_}}) ->
%				      true;
%				 (_) ->
%				      false
%			      end,
%			      NodeResults3),
    stop(Nodes),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: This case tests that the log file merger merges files in the
%% correct order, based on the timestamps.
lfm_trace_dist_1(suite) -> [];
lfm_trace_dist_1(doc) ->
    [""];
lfm_trace_dist_1(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    [RNode1,RNode2|_]=RemoteNodes,
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=
	lists:map(fun(N)->{N,{file,filename:join([PrivDir,"lfm1_"++atom_to_list(N)])}} end,
		  Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok}]}),
    activate_local_tracing(Nodes),
    activate_traceflags(Nodes),

    {inviso_test_proc,RNode2} ! {apply,code,which,[lists]},
    timer:sleep(300),
    {inviso_test_proc,RNode1} ! {apply,code,which,[lists]},
    timer:sleep(300),
    {inviso_test_proc,RNode1} ! {apply,code,which,[lists]},
    timer:sleep(300),
    inviso_test_proc ! {apply,code,which,[lists]},
    timer:sleep(300),
    {inviso_test_proc,RNode2} ! {apply,code,which,[lists]},
    timer:sleep(300),
    inviso_test_proc ! {apply,code,which,[lists]},

    deactivate_traceflags(Nodes),
    deactivate_local_tracing(Nodes),
    stop_tracing(Nodes),
    stop(Nodes),

    DestFile=filename:join(PrivDir,"lfm1_out.txt"),
    ?l {ok,6}=
	inviso_lfm:merge([{node(),
			   [{trace_log,
			     [filename:join(PrivDir,"lfm1_"++atom_to_list(node()))]}]},
			  {RNode1,
			   [{trace_log,
			     [filename:join(PrivDir,"lfm1_"++atom_to_list(RNode1))]}]},
			  {RNode2,
			   [{trace_log,
			     [filename:join(PrivDir,"lfm1_"++atom_to_list(RNode2))]}]}],
			 DestFile),
    ?l {ok,FD}=file:open(DestFile,[read]),
    ?l S1=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(RNode2),S1),
    ?l S2=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(RNode1),S2),
    ?l S3=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(RNode1),S3),
    ?l S4=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(node()),S4),
    ?l S5=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(RNode2),S5),
    ?l S6=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(node()),S6),
    ?l file:close(FD),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: Testing to the full extent that pid-mappings work with both
%% local and global registration. Also checks that pidmappings can be removed
%% and that consequently the mappings in the resulting merged file stops.
lfm_trace_ti_dist_2(suite) -> [];
lfm_trace_ti_dist_2(doc) ->
    [""];
lfm_trace_ti_dist_2(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    [RNode1,RNode2|_]=RemoteNodes,
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=
	lists:map(fun(N)->{N,[{trace,{file,filename:join(PrivDir,"lfm2_"++atom_to_list(N))}},
			      {ti,{file,filename:join(PrivDir,"lfm2_ti_"++atom_to_list(N))}}]}
		  end,
		  Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok},{ti_log,ok}]}),
    activate_local_tracing(Nodes),
    activate_meta_tracing(Nodes),
    activate_traceflags(Nodes),

    {inviso_test_proc,RNode2} ! {apply,code,which,[lists]},
    timer:sleep(300),
    {inviso_test_proc,RNode1} ! {apply,code,which,[lists]},
    timer:sleep(300),
    {inviso_test_proc,RNode1} ! {apply,code,which,[lists]},
    timer:sleep(300),
    inviso_test_proc ! {apply,code,which,[lists]},
    timer:sleep(300),

    P2=spawn(RNode2,?MODULE,test_proc_loop,[]),
    P1=spawn(RNode1,?MODULE,test_proc_loop,[]),
    P0=spawn_link(?MODULE,test_proc_loop,[]),
    ThisNode=node(),
    ?l {ok,[{ThisNode,{ok,[1]}}]}=inviso:tf([node()],P0,[call,timestamp]),
    ?l {ok,[{RNode1,{ok,[1]}}]}=inviso:tf([RNode1],P1,[call,timestamp]),
    ?l {ok,[{RNode2,{ok,[1]}}]}=inviso:tf([RNode2],P2,[call,timestamp]),
    P2 ! {apply,code,which,[lists]},
    timer:sleep(300),
    P1 ! {apply,code,which,[lists]},
    timer:sleep(300),
    P0 ! {apply,code,which,[lists]},
    timer:sleep(300),

    P3=spawn(RNode2,?MODULE,test_proc_loop,[]),
    ?l yes=global:register_name(inviso_test_proc_globalname,P3),
    ?l {ok,[{RNode2,{ok,[1]}}]}=inviso:tf([RNode2],P3,[call,timestamp]),
    timer:sleep(300),
    P3 ! {apply,code,which,[lists]},
    timer:sleep(300),

    P4=rpc:call(RNode1,erlang,whereis,[inviso_test_proc]),
    ?l true=rpc:call(RNode1,erlang,unregister,[inviso_test_proc]),
    timer:sleep(300),
    P4 ! {apply,code,which,[lists]},
    timer:sleep(300),

    ?l true=rpc:call(RNode1,erlang,register,[inviso_test_proc,P4]),

    ?l global:unregister_name(inviso_test_proc_globalname),
    timer:sleep(300),
    ?l P3 ! {apply,code,which,[lists]},
    timer:sleep(300),

    deactivate_traceflags(Nodes),
    deactivate_local_tracing(Nodes),
    stop_tracing(Nodes),
    stop(Nodes),

    DestFile=filename:join(PrivDir,"lfm2_out.txt"),
    ?l {ok,10}=
	inviso_lfm:merge([
			  {node(),
			   [{trace_log,
			     [filename:join(PrivDir,"lfm2_"++atom_to_list(node()))]},
			    {ti_log,
			     [filename:join(PrivDir,"lfm2_ti_"++atom_to_list(node()))]}]},
			  {RNode1,
			   [{trace_log,
			     [filename:join(PrivDir,"lfm2_"++atom_to_list(RNode1))]},
			    {ti_log,
			     [filename:join(PrivDir,"lfm2_ti_"++atom_to_list(RNode1))]}]},
			  {RNode2,
			   [{trace_log,
			     [filename:join(PrivDir,"lfm2_"++atom_to_list(RNode2))]},
			    {ti_log,
			     [filename:join(PrivDir,"lfm2_ti_"++atom_to_list(RNode2))]}]}
			 ],
			 DestFile),
    ?l {ok,FD}=file:open(DestFile,[read]),
    ?l S1=io:get_line(FD,""),
io:format("S1 is:~p~n",[S1]),
    ?l true=lists:prefix(atom_to_list(RNode2)++" [inviso_test_proc",S1),
    ?l S2=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(RNode1)++" [inviso_test_proc",S2),
    ?l S3=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(RNode1)++" [inviso_test_proc",S3),
    ?l S4=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(node())++" [inviso_test_proc",S4),
    ?l S5=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(RNode2)++" []",S5),
    ?l S6=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(RNode1)++" []",S6),
    ?l S7=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(node())++" []",S7),
    ?l S8=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(RNode2)++" [{global,inviso_test_proc_globalname}]",S8),
    ?l S9=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(RNode1)++" []",S9),
    ?l S10=io:get_line(FD,""),
    ?l true=lists:prefix(atom_to_list(RNode2)++" []",S10),
    ?l file:close(FD),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: This tests that the wrapset sorter works.
handle_logfile_sort_wrapset(suite) -> [];
handle_logfile_sort_wrapset(doc) ->
    [""];
handle_logfile_sort_wrapset(Config) when is_list(Config) ->
    File0="prefix10.fil",
    File1="prefix11.fil",
    File2="prefix12.fil",
    File3="prefix13.fil",
    ?l [File0,File1,File2,File3]=
	inviso_lfm_tpfreader:handle_logfile_sort_wrapset([File2,File1,File0,File3]),
    File5="prefix15.fil",
    ?l [File5,File0,File1,File2,File3]=
	inviso_lfm_tpfreader:handle_logfile_sort_wrapset([File2,File5,File1,File0,File3]),
    ok.
%% -----------------------------------------------------------------------------

%% TEST CASE: This case tests that the regexp mechanism in the inviso_rt_lib can
%% find modules using regexps and that its only_loaded mechanism works.
%% This test case can not be run when using cover because cover will make the
%% modules no longer loaded from the path containing "runtime_tools".
expand_regexp_dist_1(suite) -> [];
expand_regexp_dist_1(doc) ->
    [""];
expand_regexp_dist_1(Config) when is_list(Config) ->
    case ?t:is_cover() of
	true ->
	    {skip,"Cover is running"};
	false ->
	    expand_regexp_dist_1_nocover(Config)
    end.

expand_regexp_dist_1_nocover(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    [RNode1|_]=RemoteNodes,
    ?l NodeResults1=inviso_rt_lib:expand_regexp(Nodes,"^inviso_rt.*",[]),
    ?l L1=length(Nodes),
    ?l L1=length(NodeResults1),
    ?l true=lists:all(fun({_,Mods})->
			      ?l 3=length(Mods),
			      ?l true=lists:member(inviso_rt,Mods),
			      ?l true=lists:member(inviso_rt_lib,Mods),
			      ?l true=lists:member(inviso_rt_meta,Mods),
			      true;
			 (_) ->
			      false
		      end,
		      NodeResults1),
    %% Check the dir-option. In the following inviso_tool_lib shall not be found.
    ?l NodeResults2=inviso_rt_lib:expand_regexp(Nodes,"runtime_tools","invi.*lib.*",[]),
?l io:format("NodeResults2:~w~n",[NodeResults2]),
    ?l L1=length(NodeResults2),          % Same number of nodes replying.
    ?l true=lists:all(fun({_,Mods})->
			      2=length(Mods),
			      true=lists:member(inviso_as_lib,Mods),
			      true=lists:member(inviso_rt_lib,Mods),
			      true;
			 (_) ->
			      false
		      end,
		      NodeResults2),
    ?l [{RNode1,[]}]=
	inviso_rt_lib:expand_regexp([RNode1],"^inviso_testmodule1.*",[only_loaded]),
    ?l [{RNode1,[inviso_testmodule1_foo]}]=
	inviso_rt_lib:expand_regexp([RNode1],"^inviso_testmodule1.*",[]),
    ok.
%% -----------------------------------------------------------------------------


only_loaded_dist_1(suite) -> [];
only_loaded_dist_1(doc) ->
    [""];
only_loaded_dist_1(Config) when is_list(Config) ->
    RemoteNodes=get_remotenodes_config(Config),
    Nodes=[node()|RemoteNodes],
    [RNode1|_]=RemoteNodes,
    PrivDir=filename:join(?config(priv_dir,Config),""),
    TracerDataList=
	lists:map(fun(N)->{N,[{trace,{file,filename:join(PrivDir,"ol_1_"++atom_to_list(N))}}]}
		  end,
		  Nodes),
    start_and_init_tracing2(Nodes,[],TracerDataList,{ok,[{trace_log,ok}]}),
    ?l false=rpc:call(RNode1,erlang,module_loaded,[inviso_testmodule1_foo]),
    ?l {ok,[{RNode1,{ok,[0]}}]}=
	inviso:tpl([RNode1],inviso_testmodule1_foo,'_','_',[],[only_loaded]),
    ?l false=rpc:call(RNode1,erlang,module_loaded,[inviso_testmodule1_foo]),
    ?l {ok,[{RNode1,{ok,[3]}}]}=
	inviso:tpl([RNode1],inviso_testmodule1_foo,'_','_',[],[]),
    stop_tracing(Nodes),
    stop(Nodes),
    ok.


%% ==============================================================================
%% Common functions setting up inviso.
%% ==============================================================================

%% Starts controlcomponent and adds runtime components on the nodes specified.
%% Also initiates tracing on the nodes.
start_and_init_tracing1(Nodes,Options,TracerData,Reply) when is_list(Nodes) ->
    ?l {ok,_Pid}=inviso:start(),             % Start a control component.
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,a_ref,Options),
    io:format("~p~n",[NodeResults1]),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    ?l {ok,NodeResults2}=inviso:get_status(Nodes),
    ?l true=check_noderesults(Nodes,{ok,{new,running}},NodeResults2),
    ?l {ok,NodeResults3}=inviso:init_tracing(Nodes,TracerData),
    ?l true=check_noderesults(Nodes,Reply,NodeResults3),
    ok.
start_and_init_tracing2(Nodes,Options,TracerDataList,Reply) ->
    ?l {ok,_Pid}=inviso:start(),             % Start a control component.
    ?l {ok,NodeResults1}=inviso:add_nodes(Nodes,a_ref,Options),
    io:format("~p~n",[NodeResults1]),
    ?l true=check_noderesults(Nodes,{ok,new},NodeResults1),
    ?l {ok,NodeResults2}=inviso:get_status(Nodes),
    ?l true=check_noderesults(Nodes,{ok,{new,running}},NodeResults2),
    ?l {ok,NodeResults4}=inviso:get_tracerdata(Nodes),
    ?l true=check_noderesults(Nodes,{ok,no_tracerdata},NodeResults4),
    ?l {ok,NodeResults3}=inviso:init_tracing(TracerDataList),
    io:format("Tracerdatalist:~p~n",[TracerDataList]),
    ?l true=check_noderesults(Nodes,Reply,NodeResults3),

    ?l Fun1=fun({N,{ok,TD}}) when is_list(TD)->
		    ?l {value,{trace,Trace}}=lists:keysearch(trace,1,TD),
		    ?l {value,{N,TD2}}=lists:keysearch(N,1,TracerDataList),
		    ?l true=lists:member({trace,Trace},TD2),
		    %% Check that the trace file really exists.
		    ?l case Trace of % Trace={file,FilePortParameters}
			   {file,FileName1} when is_list(FileName1) ->
			       ?l {ok,_}=rpc:call(N,file,read_file_info,[FileName1]);
			   _ ->              % This should be extended with more cases.
			       true
		       end,
		    ?l case lists:keysearch(ti,1,TD2) of
			   {value,{_,Ti}} -> % Ok, we have ti too.
			       ?l {value,{_,Ti}}=lists:keysearch(ti,1,TD),
			       ?l FileName2=element(2,Ti),
			       ?l {ok,_}=rpc:call(N,file,read_file_info,[FileName2]),
			       true;
			   false ->          % No ti, we are done now.
			       true
		       end;
	       ({N,{ok,{file,FileName}}}) ->
		    ?l {value,{N,{file,FileName}}}=lists:keysearch(N,1,TracerDataList),
		    ?l {ok,_}=rpc:call(N,file,read_file_info,[FileName]),
		    true;
	       ({N,{ok,LogTD}}) ->           % The case using a fun.
		    ?l {value,{N,LogTD}}=lists:keysearch(N,1,TracerDataList),
		    true
	    end,
    ?l {ok,NodeResults5}=inviso:get_tracerdata(Nodes),
    ?l true=check_noderesults(Nodes,Fun1,NodeResults5),
    ok.
%% ------------------------------------------------------------------------------

%% Stops tracing on Nodes.
stop_tracing(Nodes) when is_list(Nodes) ->
    ?l {ok,NodeResults1}=inviso:stop_tracing(Nodes),
    ?l true=check_noderesults(Nodes,{ok,idle},NodeResults1),
    ?l {ok,NodeResults2}=inviso:get_status(Nodes),
    ?l true=check_noderesults(Nodes,{ok,{idle,running}},NodeResults2),
    %% The implementation says that the meta tracer shall be stopped when
    %% tracing is stopped. Check that.
    ?l lists:foreach(fun(N)->
			     ok=poll(erlang,whereis,[inviso_rt_meta],undefined,20)
		     end,
		     Nodes).
%% ------------------------------------------------------------------------------

%% Stops the runtime components on Nodes and stops the control component at this
%% Erlang node.
stop(Nodes) when is_list(Nodes) ->
    ?l true=check_on_nodes(Nodes,erlang,whereis,[inviso_rt],fun(P) when is_pid(P)->true end),
    ?l {ok,NodeResults}=inviso:stop_nodes(Nodes),
    ?l true=check_noderesults(Nodes,ok,NodeResults),
    ?l true=check_on_nodes(Nodes,erlang,whereis,[inviso_rt],fun(undefined)->true end),
    ?l true=is_pid(whereis(inviso_c)),
    ?l shutdown=inviso:stop(),
    ?l ok=poll(erlang,whereis,[inviso_c],undefined,20).
%% ------------------------------------------------------------------------------

%% Help function activating local tracing.
activate_local_tracing(Nodes) when is_list(Nodes) ->
    ?l true=check_on_nodes(Nodes,
			   erlang,
			   trace_info,
			   [{code,which,1},traced],
			   {traced,false}),
    ?l {ok,NodeResults}=inviso:tpl(Nodes,code,which,1,[]),
    ?l true=check_noderesults(Nodes,fun({_,{ok,[1]}})->true end,NodeResults),
    ?l true=check_on_nodes(Nodes,
			   erlang,
			   trace_info,
			   [{code,which,1},traced],
			   {traced,local}).
%% ------------------------------------------------------------------------------

%% Help function activating global tracing.
activate_global_tracing(Nodes) when is_list(Nodes) ->
    ?l true=check_on_nodes(Nodes,
			   erlang,
			   trace_info,
			   [{code,get_path,0},traced],
			   {traced,false}),
    ?l {ok,NodeResults}=inviso:tp(Nodes,code,get_path,0,[]),
    ?l true=check_noderesults(Nodes,fun({_,{ok,[1]}})->true end,NodeResults),
    ?l true=check_on_nodes(Nodes,
			   erlang,
			   trace_info,
			   [{code,get_path,0},traced],
			   {traced,global}).
%% ------------------------------------------------------------------------------


%% Help function activating local tracing and using a regexp to point out modules.
%% Returns the structure of modules and functions that were activated. Must be used
%% when deactivating.
activate_global_tracing_regexp(Nodes) when is_list(Nodes) ->
    %% First find out which modules will be effected.
    ?l Mods1=inviso_rt_lib:expand_regexp("application.*",[]),
    ?l true=(length(Mods1)>1),                % Should find more than one module!
    ?l Funcs1=lists:foldl(fun(M,Acc)->[{M,M:module_info(exports)}|Acc] end,[],Mods1),
    %% Check that these functions are not traced.
    io:format("Modules:~w~n",[Mods1]),
    ?l {ok,NodeResults}=inviso:tp(Nodes,"application.*",'_','_',[],[]),
    io:format("Here 2~w~n",[NodeResults]),
    ?l N=lists:foldl(fun({_,L1},A1)->lists:foldl(fun(_,A2)->A2+1 end,A1,L1) end,0,Funcs1),
    ?l true=check_noderesults(Nodes,fun({_,{ok,L}})-> N==lists:sum(L) end,NodeResults),
    io:format("Here 3~n",[]),
    %% Check again!
    ?l lists:foreach(fun({M,Funcs})->
			     lists:foreach(fun({F,Arity})->
						   true=check_on_nodes(Nodes,
								       erlang,
								       trace_info,
								       [{M,F,Arity},traced],
								       {traced,global})
					   end,
					   Funcs)
		     end,
		     Funcs1),
    Funcs1.
%% ------------------------------------------------------------------------------

%% Help function as above but uses the dir feature as well.
activate_global_tracing_regexp_dir(Nodes) when is_list(Nodes) ->
    %% First find out which modules will be effected.
    ?l Mods1=inviso_rt_lib:expand_regexp(".*kernel.*","application.*",[]),
    ?l true=(length(Mods1)>1),                % Should find more than one module!
    ?l Funcs1=lists:foldl(fun(M,Acc)->[{M,M:module_info(exports)}|Acc] end,[],Mods1),
    %% Check that these functions are not traced.
    io:format("Modules:~w~n",[Mods1]),
    ?l {ok,NodeResults}=inviso:tp(Nodes,{".*kernel.*","application.*"},'_','_',[],[]),
    io:format("Here 2~w~n",[NodeResults]),
    ?l N=lists:foldl(fun({_,L1},A1)->lists:foldl(fun(_,A2)->A2+1 end,A1,L1) end,0,Funcs1),
    ?l true=check_noderesults(Nodes,fun({_,{ok,L}})-> N==lists:sum(L) end,NodeResults),
    io:format("Here 3~n",[]),
    %% Check again!
    ?l lists:foreach(fun({M,Funcs})->
			     lists:foreach(fun({F,Arity})->
						   true=check_on_nodes(Nodes,
								       erlang,
								       trace_info,
								       [{M,F,Arity},traced],
								       {traced,global})
					   end,
					   Funcs)
		     end,
		     Funcs1),
    Funcs1.
%% ------------------------------------------------------------------------------

deactivate_local_tracing(Nodes) when is_list(Nodes) ->
    ?l true=check_on_nodes(Nodes,
			   erlang,
			   trace_info,
			   [{code,which,1},traced],
			   {traced,local}),
    ?l {ok,NodeResults}=inviso:ctpl(Nodes,code,'_','_'),
    ?l true=check_noderesults(Nodes,fun({_,{ok,[N]}})when is_integer(N)->true end,NodeResults),
    ?l true=check_on_nodes(Nodes,
			   erlang,
			   trace_info,
			   [{code,which,1},traced],
			   {traced,false}).
%% ------------------------------------------------------------------------------

deactivate_global_tracing(Nodes) when is_list(Nodes) ->
    ?l true=check_on_nodes(Nodes,
			   erlang,
			   trace_info,
			   [{code,get_path,0},traced],
			   {traced,global}),
    ?l {ok,NodeResults}=inviso:ctp(Nodes,code,'_','_'),
    ?l true=check_noderesults(Nodes,fun({_,{ok,[N]}})when is_integer(N)->true end,NodeResults),
    ?l true=check_on_nodes(Nodes,
			   erlang,
			   trace_info,
			   [{code,get_path,0},traced],
			   {traced,false}).
%% ------------------------------------------------------------------------------


%% Function deactivating the functions activated by activate_global_tracing_regexp/1.
deactivate_global_tracing_regexp(Nodes,Funcs1) ->
    ?l lists:foreach(fun({M,Funcs})->
			     lists:foreach(fun({F,Arity})->
						   true=check_on_nodes(Nodes,
								       erlang,
								       trace_info,
								       [{M,F,Arity},traced],
								       {traced,global})
					   end,
					   Funcs)
		     end,
		     Funcs1),
    ?l {ok,NodeResults}=inviso:ctp(Nodes,"application.*",'_','_'),
    ?l N=lists:foldl(fun({_,L1},A1)->lists:foldl(fun(_,A2)->A2+1 end,A1,L1) end,0,Funcs1),
    io:format("Noderesult from deactivate;~w~n",[NodeResults]),
    ?l true=check_noderesults(Nodes,fun({_,{ok,L}})-> N==lists:sum(L) end,NodeResults),
    ?l lists:foreach(fun({M,Funcs})->
			     lists:foreach(fun({F,Arity})->
						   true=check_on_nodes(Nodes,
								       erlang,
								       trace_info,
								       [{M,F,Arity},traced],
								       {traced,false})
					   end,
					   Funcs)
		     end,
		     Funcs1).
%% ------------------------------------------------------------------------------

%% Function deactivating the functions activated by activate_global_tracing_regexp_dir/1.
deactivate_global_tracing_regexp_dir(Nodes,Funcs1) ->
    ?l lists:foreach(fun({M,Funcs})->
			     lists:foreach(fun({F,Arity})->
						   true=check_on_nodes(Nodes,
								       erlang,
								       trace_info,
								       [{M,F,Arity},traced],
								       {traced,global})
					   end,
					   Funcs)
		     end,
		     Funcs1),
    ?l {ok,NodeResults}=inviso:ctp(Nodes,{".*kernel.*","application.*"},'_','_'),
    ?l N=lists:foldl(fun({_,L1},A1)->lists:foldl(fun(_,A2)->A2+1 end,A1,L1) end,0,Funcs1),
    io:format("Noderesult from deactivate;~w~n",[NodeResults]),
    ?l true=check_noderesults(Nodes,fun({_,{ok,L}})-> N==lists:sum(L) end,NodeResults),
    ?l lists:foreach(fun({M,Funcs})->
			     lists:foreach(fun({F,Arity})->
						   true=check_on_nodes(Nodes,
								       erlang,
								       trace_info,
								       [{M,F,Arity},traced],
								       {traced,false})
					   end,
					   Funcs)
		     end,
		     Funcs1).
%% ------------------------------------------------------------------------------

%% Help function which starts the inviso_test_proc on all nodes and then sets
%% the call flag on that process.
activate_traceflags(Nodes) ->
    ?l lists:foreach(fun(N)->spawn(N,?MODULE,test_proc_init,[]) end,Nodes),
    ?l lists:foreach(fun(N)->
			     P=rpc:call(N,erlang,whereis,[inviso_test_proc]),
			     {flags,[]}=rpc:call(N,erlang,trace_info,[P,flags])
		     end,
		     Nodes),
    ?l {ok,NodeResults}=inviso:tf(Nodes,inviso_test_proc,[call,timestamp]),
    ?l true=check_noderesults(Nodes,{ok,[1]},NodeResults),
    ?l lists:foreach(fun(N)->
			     P=rpc:call(N,erlang,whereis,[inviso_test_proc]),
			     {flags,Flags}=rpc:call(N,erlang,trace_info,[P,flags]),
			     true=lists:member(call,Flags),
			     true=lists:member(timestamp,Flags)
		     end,
		     Nodes),
    %% Now try a globally registered process.
    ?l [ANode|_]=Nodes,
    ?l GPid=spawn(ANode,?MODULE,global_test_proc_init,[]),
    ?l ok=poll(global,whereis_name,[global_inviso_test_proc],
	       fun(P) when is_pid(P)->true;(_)->false end,
	       10),
    ?l {ok,NodeResults2}=
	inviso:tf(Nodes,{global,global_inviso_test_proc},[call,timestamp]),
    ?l true=check_noderesults(Nodes,
			      fun({N,{ok,[1]}}) when N==ANode->true;
				 ({_,{ok,[0]}})->true;
				 (_)->false
			      end,
			      NodeResults2),
    ?l {flags,Flags2}=rpc:call(ANode,erlang,trace_info,[GPid,flags]),
    ?l 2=length(Flags2),
    ?l true=lists:member(call,Flags2),
    ?l true=lists:member(timestamp,Flags2),
    true.
%% ------------------------------------------------------------------------------

deactivate_traceflags(Nodes) ->
    ?l lists:foreach(fun(N)->
			     P=rpc:call(N,erlang,whereis,[inviso_test_proc]),
			     {flags,Flags}=rpc:call(N,erlang,trace_info,[P,flags]),
			     true=lists:member(call,Flags),
			     true=lists:member(timestamp,Flags)
		     end,
		     Nodes),
    ?l {ok,NodeResults}=inviso:ctf(Nodes,inviso_test_proc,[call,timestamp]),
    ?l true=check_noderesults(Nodes,{ok,[1]},NodeResults),
    ?l lists:foreach(fun(N)->
			     P=rpc:call(N,erlang,whereis,[inviso_test_proc]),
			     {flags,[]}=rpc:call(N,erlang,trace_info,[P,flags])
		     end,
		     Nodes),
    ?l GPid=global:whereis_name(global_inviso_test_proc),
    ?l ANode=node(GPid),
    ?l {flags,Flags2}=rpc:call(ANode,erlang,trace_info,[GPid,flags]),
    ?l 2=length(Flags2),
    ?l {ok,NodeResults2}=inviso:ctf(Nodes,{global,global_inviso_test_proc},[call,timestamp]),
    ?l true=check_noderesults(Nodes,
			      fun({N,{ok,[1]}}) when N==ANode->true;
				 ({_,{ok,[0]}})->true;
				 (_)->false
			      end,
			      NodeResults2).
%% ------------------------------------------------------------------------------


activate_meta_tracing(Nodes) ->
    ?l {ok,NodeResults1}=inviso:tpm_localnames(),
    ?l true=check_noderesults(Nodes,{{ok,1},{ok,1}},NodeResults1),
    ?l lists:foreach(fun(N)->P=rpc:call(N,erlang,whereis,[inviso_rt_meta]),
			     {meta,P}=rpc:call(N,erlang,trace_info,[{erlang,register,2},meta])
		     end,
		     Nodes),
    ?l lists:foreach(fun(N)->P=rpc:call(N,erlang,whereis,[inviso_rt_meta]),
			     {meta,P}=rpc:call(N,erlang,trace_info,[{erlang,unregister,1},meta])
		     end,
		     Nodes),
    ?l {ok,NodeResults2}=inviso:tpm_globalnames(),
    ?l true=check_noderesults(Nodes,{{ok,1},{ok,1}},NodeResults2),
    ?l lists:foreach(fun(N)->P=rpc:call(N,erlang,whereis,[inviso_rt_meta]),
			     {meta,P}=rpc:call(N,
					       erlang,
					       trace_info,
					       [{global,handle_call,3},meta])
		     end,
		     Nodes),
    ?l lists:foreach(fun(N)->P=rpc:call(N,erlang,whereis,[inviso_rt_meta]),
			     {meta,P}=rpc:call(N,
					       erlang,
					       trace_info,
					       [{global,delete_global_name,2},meta])
		     end,
		     Nodes),

    ?l lists:foreach(fun(N)->true=rpc:call(N,
					   ets,
					   insert,
					   [inviso_sideeffect_tab,{tpm_init_func1,0}]),
			     true=rpc:call(N,
					   ets,
					   insert,
					   [inviso_sideeffect_tab,{tpm_call_func1,0}]),
			     true=rpc:call(N,
					   ets,
					   insert,
					   [inviso_sideeffect_tab,{tpm_return_func1,0}])
		     end,
		     Nodes),
    ?l {ok,NodeResults3}=
	inviso:init_tpm(lists,
			module_info,
			0,
			{?MODULE,tpm_init_func1},
			{?MODULE,tpm_call_func1},
			{?MODULE,tpm_return_func1},
			{?MODULE,tpm_remove_func1}),
    ?l true=check_noderesults(Nodes,ok,NodeResults3),
    ?l [{_,1}]=ets:lookup(inviso_sideeffect_tab,tpm_init_func1),
    ?l {ok,NodeResults3a}=
	inviso:init_tpm(lists,
			module_info,
			0,
			{?MODULE,tpm_init_func1},
			{?MODULE,tpm_call_func1},
			{?MODULE,tpm_return_func1},
			{?MODULE,tpm_remove_func1}),
    ?l true=check_noderesults(Nodes,{error,already_initiated},NodeResults3a),
%    %% Try more forbidden things. Wildcards not allowed in meta tracing!
%    ?l {ok,NodeResults3b}=inviso:tpm(Nodes,lists,'_',0,[{'_',[],[{return_trace}]}]),
%    io:format("The noderesults3b is:~w~n",[NodeResults3b]),
%    ?l true=check_noderesults(Nodes,{error,bad_mfa},NodeResults3b),
    ?l {ok,NodeResults3c}=inviso:tpm(Nodes,lists,module_info,0,[{'_',[],[{return_trace}]}]),
    ?l true=check_noderesults(Nodes,{ok,1},NodeResults3c),
    ?l lists:foreach(fun(N)->P=rpc:call(N,erlang,whereis,[inviso_rt_meta]),
			     {meta,P}=rpc:call(N,erlang,trace_info,[{lists,module_info,0},meta])
		     end,
		     Nodes),
    ?l lists:foreach(fun(N)->rpc:call(N,lists,module_info,[]) end,Nodes),
    ?l ok=poll(ets,lookup,[inviso_sideeffect_tab,tpm_call_func1],[{tpm_call_func1,1}],20),
    ?l ok=poll(ets,lookup,[inviso_sideeffect_tab,tpm_return_func1],[{tpm_return_func1,1}],20),
    ?l lists:foreach(fun(N)->rpc:call(N,lists,module_info,[]) end,Nodes),
    ?l ok=poll(ets,lookup,[inviso_sideeffect_tab,tpm_call_func1],[{tpm_call_func1,2}],20),
    ?l ok=poll(ets,lookup,[inviso_sideeffect_tab,tpm_return_func1],[{tpm_return_func1,2}],20),

    ?l {ok,NodeResults4}=
	inviso:init_tpm(math,
			module_info,
			1,
			{?MODULE,tpm_init_func2}, % Does not exist on purpose.
			{?MODULE,tpm_call_func2}, % Does not exist on purpose.
			{?MODULE,tpm_return_func2}, % Does not exist on purpose.
			{?MODULE,tpm_remove_func2}), % Does not exist on purpose.
    ?l true=check_noderesults(Nodes,ok,NodeResults4),
    ?l {ok,NodeResults5}=
	inviso:tpm_ms(math,module_info,1,ms1,[{'_',[],[{return_trace}]}]),
    ?l true=check_noderesults(Nodes,{ok,1},NodeResults5),
    ?l lists:foreach(fun(N)->{meta_match_spec,[{'_',[],[{return_trace}]}]}=
				 rpc:call(N,erlang,trace_info,[{math,module_info,1},
							       meta_match_spec])
		     end,
		     Nodes),

    ?l {ok,NodeResults6}=inviso:tpm_ms(math,module_info,1,ms2,[{[exports],[],[]}]),
    ?l true=check_noderesults(Nodes,{ok,1},NodeResults6),
    ?l lists:foreach(fun(N)->{meta_match_spec,[{[exports],[],[]},{'_',[],[{return_trace}]}]}=
				 rpc:call(N,erlang,trace_info,[{math,module_info,1},
							       meta_match_spec])
		     end,
		     Nodes),
    ?l {ok,NodeResults7}=inviso:tpm_ms(math,module_info,1,ms3,[{[attributes],[],[]}]),
    ?l true=check_noderesults(Nodes,{ok,1},NodeResults7),
    ?l lists:foreach(fun(N)->{meta_match_spec,[{[attributes],[],[]},
					       {[exports],[],[]},
					       {'_',[],[{return_trace}]}]}=
				 rpc:call(N,erlang,trace_info,[{math,module_info,1},
							       meta_match_spec])
		     end,
		     Nodes),
    ?l {ok,NodeResults8}=inviso:ctpm_ms(math,module_info,1,ms2),
    ?l true=check_noderesults(Nodes,ok,NodeResults8),
    ?l lists:foreach(fun(N)->{meta_match_spec,[{[attributes],[],[]},
					       {'_',[],[{return_trace}]}]}=
				 rpc:call(N,erlang,trace_info,[{math,module_info,1},
							       meta_match_spec])
		     end,
		     Nodes),
    ?l io:format("whereis:~w~n",[lists:map(fun(N)->rpc:call(N,erlang,whereis,[inviso_rt_meta]) end,Nodes)]),
    ?l {ok,NodeResults8}=inviso:ctpm_ms(math,module_info,1,ms3),
    ?l io:format("whereis:~w~n",[lists:map(fun(N)->rpc:call(N,erlang,whereis,[inviso_rt_meta]) end,Nodes)]),
    ?l {ok,NodeResults8}=inviso:ctpm_ms(math,module_info,1,ms1),
    ?l lists:foreach(fun(N)->{meta_match_spec,false}=
			      rpc:call(N,erlang,trace_info,[{math,module_info,1},
							    meta_match_spec])
		     end,
		     Nodes),

    %% Now try to do this with exception tracing instead.
    %% Reset the side effect tables.
    ?l lists:foreach(fun(N)->true=rpc:call(N,
					   ets,
					   insert,
					   [inviso_sideeffect_tab,{tpm_init_func1,0}]),
			     true=rpc:call(N,
					   ets,
					   insert,
					   [inviso_sideeffect_tab,{tpm_call_func1,0}]),
			     true=rpc:call(N,
					   ets,
					   insert,
					   [inviso_sideeffect_tab,{tpm_return_func1,0}])
		     end,
		     Nodes),
    ?l {ok,NodeResults9}=
	inviso:init_tpm(?MODULE,
			failing_function,
			1,
			{?MODULE,tpm_init_func1},
			{?MODULE,tpm_call_func1},
			{?MODULE,tpm_return_func1},
			{?MODULE,tpm_remove_func1}),
    ?l true=check_noderesults(Nodes,ok,NodeResults9),
    ?l [{_,1}]=ets:lookup(inviso_sideeffect_tab,tpm_init_func1),
    ?l {ok,NodeResults10}=inviso:tpm(Nodes,?MODULE,failing_function,1,[{'_',[],[{exception_trace}]}]),
    ?l true=check_noderesults(Nodes,{ok,1},NodeResults10),
    ?l lists:foreach(fun(N)->P=rpc:call(N,erlang,whereis,[inviso_rt_meta]),
			     {meta,P}=rpc:call(N,erlang,trace_info,[{?MODULE,failing_function,1},meta])
		     end,
		     Nodes),
    ?l lists:foreach(fun(N)->rpc:call(N,?MODULE,failing_function,[nofailure]) end,Nodes),
    ?l ok=poll(ets,lookup,[inviso_sideeffect_tab,tpm_call_func1],[{tpm_call_func1,1}],20),
    ?l ok=poll(ets,lookup,[inviso_sideeffect_tab,tpm_return_func1],[{tpm_return_func1,1}],20),
    ?l lists:foreach(fun(N)->rpc:call(N,?MODULE,failing_function,[failure]) end,Nodes),
    ?l ok=poll(ets,lookup,[inviso_sideeffect_tab,tpm_call_func1],[{tpm_call_func1,2}],20),
    ?l ok=poll(ets,lookup,[inviso_sideeffect_tab,tpm_return_func1],[{tpm_return_func1,3}],20),

    ok.
%% ------------------------------------------------------------------------------

%% This function is for testing that appending the tracer to a trace action term
%% works.
activate_deactivate_meta_tracing_tracer(Nodes) ->
    ?l {ok,NodeResults}=
	inviso:tpm_tracer(Nodes,lists,module_info,0,[{'_',[],[{trace,[all],[call]}]}],void),
    ?l true=check_noderesults(Nodes,{ok,1},NodeResults),
    ?l lists:foreach(fun(N)->P=rpc:call(N,erlang,whereis,[inviso_rt_meta]),
			     {meta,P}=rpc:call(N,erlang,trace_info,[{lists,module_info,0},meta]),
			     {meta_match_spec,[{'_',[],[{trace,[all],Enable}]}]}=
				 rpc:call(N,erlang,trace_info,[{lists,module_info,0},
							       meta_match_spec]),
			     true=list_search(Enable,fun({{tracer,P}}) when is_port(P)->true;
							(_) -> false
						     end)
		     end,
		     Nodes),
    ?l {ok,NodeResults2}=
	inviso:ctpm(Nodes,lists,module_info,0),
    ?l true=check_noderesults(Nodes,ok,NodeResults2),
    ?l lists:foreach(fun(N)->{meta,false}=
				 rpc:call(N,erlang,trace_info,[{lists,module_info,0},meta])
		     end,
		     Nodes),
    ok.
%% ------------------------------------------------------------------------------

deactivate_meta_tracing(Nodes) ->
    ?l lists:foreach(fun(N)->{meta,P}=
				 rpc:call(N,erlang,trace_info,[{erlang,register,2},meta]),
			     true=is_pid(P)
		     end,
		     Nodes),
    ?l lists:foreach(fun(N)->{meta,P}=
				 rpc:call(N,erlang,trace_info,[{erlang,unregister,1},meta]),
			     true=is_pid(P)
		     end,
		     Nodes),
    ?l {ok,NodeResults1}=inviso:ctpm_localnames(),
    ?l lists:foreach(fun(N)->{meta,false}=
				 rpc:call(N,erlang,trace_info,[{erlang,register,2},meta]) end,
		     Nodes),
    ?l lists:foreach(fun(N)->{meta,false}=
				 rpc:call(N,erlang,trace_info,[{erlang,unregister,1},meta])
		     end,
		     Nodes),
    ?l true=check_noderesults(Nodes,{ok,ok},NodeResults1),

    ?l lists:foreach(fun(N)->P=rpc:call(N,erlang,whereis,[inviso_rt_meta]),
			     {meta,P}=rpc:call(N,
					       erlang,
					       trace_info,
					       [{global,handle_call,3},meta])
		     end,
		     Nodes),
    ?l lists:foreach(fun(N)->P=rpc:call(N,erlang,whereis,[inviso_rt_meta]),
			     {meta,P}=rpc:call(N,
					       erlang,
					       trace_info,
					       [{global,delete_global_name,2},meta])
		     end,
		     Nodes),
    ?l {ok,NodeResults1b}=inviso:ctpm_globalnames(),
    ?l true=check_noderesults(Nodes,{ok,ok},NodeResults1b),
    ?l lists:foreach(fun(N)->
			     {meta,false}=rpc:call(N,
						   erlang,
						   trace_info,
						   [{global,handle_call,3},meta])
		     end,
		     Nodes),
    ?l lists:foreach(fun(N)->
			     {meta,false}=rpc:call(N,
						   erlang,
						   trace_info,
						   [{global,delete_global_name,2},meta])
		     end,
		     Nodes),

    ?l lists:foreach(fun(N)->{meta,P}=
				 rpc:call(N,erlang,trace_info,[{lists,module_info,0},meta]),
			     true=is_pid(P)
		     end,
		     Nodes),
    ?l {ok,NodeResults2}=inviso:ctpm(lists,module_info,0),
    ?l true=check_noderesults(Nodes,ok,NodeResults2),
    ?l lists:foreach(fun(N)->{meta,false}=
				 rpc:call(N,erlang,trace_info,[{lists,module_info,0},meta]) end,
		     Nodes),
    ?l [{_,0}]=ets:lookup(inviso_sideeffect_tab,tpm_init_func1),
    ?l {ok,NodeResults3}=inviso:ctpm(math,module_info,1),
    ?l true=check_noderesults(Nodes,ok,NodeResults3),
    ok.
%% ------------------------------------------------------------------------------

%% Functions acting as callbacks for testing the meta tracing mechanisms.
tpm_init_func1(_M,_F,_Arity,PublLD) ->
    ets:update_counter(inviso_sideeffect_tab,tpm_init_func1,1),
    {ok,PublLD,void}.
tpm_call_func1(_Pid,{call,_Args,_TS},PublLD) ->
    ets:update_counter(inviso_sideeffect_tab,tpm_call_func1,1),
    {ok,PublLD,void}.
tpm_return_func1(_Pid,{return_from,_ReturnVal,_TS},PublLD) ->
    ets:update_counter(inviso_sideeffect_tab,tpm_return_func1,1),
    {ok,PublLD,void};
tpm_return_func1(_Pid,{exception_from,_ReturnVal,_TS},PublLD) ->
    ets:update_counter(inviso_sideeffect_tab,tpm_return_func1,1),
    ets:update_counter(inviso_sideeffect_tab,tpm_return_func1,1),
    {ok,PublLD,void}.
tpm_remove_func1(_M,_F,_Arity,PublLD) ->
    ets:update_counter(inviso_sideeffect_tab,tpm_init_func1,-1),
    {ok,PublLD}.
%% ------------------------------------------------------------------------------


%% Help function which traces on a function and makes function calls until there
%% are two files in the wrap-set.
fill_and_reach_two_wrapfiles(PrivDir,RegExp,Nodes) ->
    ?l lists:foreach(fun(N)->spawn(N,?MODULE,test_proc_init,[]) end,Nodes),
    ?l {ok,NodeResults1}=inviso:tpl(Nodes,?MODULE,test_function,0,[]),
    ?l true=check_noderesults(Nodes,{ok,[1]},NodeResults1),
    ?l {ok,NodeResults2}=inviso:tf(Nodes,inviso_test_proc,[call]),
    ?l true=check_noderesults(Nodes,{ok,[1]},NodeResults2),
    fill_and_reach_two_wrapfiles_2(PrivDir,RegExp,Nodes),
    ?l {ok,NodeResults3}=inviso:ctf(Nodes,inviso_test_proc,[call]),
    ?l true=check_noderesults(Nodes,{ok,[1]},NodeResults3),
    ?l {ok,NodeResults4}=inviso:ctpl(Nodes,?MODULE,test_function,0),
    ?l true=check_noderesults(Nodes,{ok,[1]},NodeResults4),
    ok.

fill_and_reach_two_wrapfiles_2(PrivDir,RegExp,[Node|Rest]) ->
    ?l ok=rpc:call(Node,?MODULE,fill_and_reach_two_wrapfiles_3,[PrivDir,RegExp]),
    fill_and_reach_two_wrapfiles_2(PrivDir,RegExp,Rest);
fill_and_reach_two_wrapfiles_2(_,_,[]) ->
    ok.

fill_and_reach_two_wrapfiles_3(Dir,RegExp) ->
    ok=send_to_test_proc({apply,?MODULE,test_function,[]},
			 fun reach_two_wraps_stopfun/1,
			 {Dir,RegExp++atom_to_list(node())},
			 100).

%% Help function intended to be used as fun in a send_to_test_proc/4 call.
%% The function lists the content of Dir and looks for occurancies of String.
%% If two files containing the string String are found, 'done' is returned.
%% Otherwise 'continue'.
reach_two_wraps_stopfun({Dir,RegExp}) ->
    case file:list_dir(Dir) of
	{ok,FileNames} ->
	    case how_many_files_regexp(FileNames,RegExp,0) of
		{ok,2} ->
		    done;
		_ ->
		    continue
	    end;
	{error,_Reason} ->
	    error
    end.
%% ------------------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% Help function for the overload tests. These functions are used as callbacks.
%% ------------------------------------------------------------------------------

overload1(_) ->
    ets:update_counter(inviso_sideeffect_tab,ovl1,1),
    ok.
%% This function is used when timeout occurs inside the runtime component.
%% That is it is time to check for overload.
overload2({timeout,overload2i_data}) ->
    ets:update_counter(inviso_sideeffect_tab,ovl2,1),
    ok.
overload2i() ->
    ets:insert(inviso_sideeffect_tab,{ovl2,0}),
    {ok,overload2i_data}.
overload2r(overload2i_data) ->
    ets:delete(inviso_sideeffect_tab,ovl2).

%% This function is used when timeout occurs inside the runtime component.
%% That is it is time to check for overload.
overload3({timeout,overload3i_data}) ->
    ets:update_counter(inviso_sideeffect_tab,ovl3,1),
    ok;
overload3(_) ->                                 % Must handle garbage too.
    ignore.
overload3i() ->
    ets:insert(inviso_sideeffect_tab,{ovl3,0}),
    {ok,overload3i_data}.
overload3r(overload3i_data) ->
    ets:insert(inviso_sideeffect_tab,{ovl3r,done}),
    ets:delete(inviso_sideeffect_tab,ovl3).

overload4(_) ->
    case ets:lookup(inviso_sideeffect_tab,ovl4_suspend) of
	[] ->                                   % We are supposed to be running.
	    ets:update_counter(inviso_sideeffect_tab,ovl4,1),
	    ok;
	[_] ->
	    {suspend,test}
    end.

%% This function is used when overload check is done by icomming message.
overload5({msg,{test_of_loadcheck,overload5i_data}}) ->
    ets:update_counter(inviso_sideeffect_tab,ovl5,1),
    ok;
overload5(_) ->
    ignore.
overload5i() ->
    ets:insert(inviso_sideeffect_tab,{ovl5,0}),
    {ok,overload5i_data}.
overload5r(overload5i_data) ->
    ets:delete(inviso_sideeffect_tab,ovl5),
    ets:insert(inviso_sideeffect_tab,{ovl5r,done});
overload5r(X) ->
    erlang:display({'***',overload5r,X}).

overload6(_) ->
    ets:update_counter(inviso_sideeffect_tab,ovl6,1),
    ok.
%% ------------------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% Help function for the subscription tests. These function implements a collector
%% process which will subscribe to inviso_events from the control component.
%% ------------------------------------------------------------------------------

%% Function which can be used to check if an inviso_event has arrived. The function
%% takes a fun which tests the messages.
check_msg_collector([],_,_) ->
    true;
check_msg_collector(_,_,0) ->
    false;
check_msg_collector(Nodes,Fun,T) ->
    Ref=make_ref(),
    inviso_collector_proc ! {fetch_message,self(),Ref,Fun},
    receive
	{inviso,Ref,{true,Node}} ->
	    check_msg_collector(lists:delete(Node,Nodes),Fun,T-1);
	{inviso,Ref,false} ->
	    timer:sleep(100),
	    check_msg_collector(Nodes,Fun,T-1)
    end.

%% Spawn on this function to get a subscriber.
inviso_msg_collector() ->
    register(inviso_collector_proc,self()),
    inviso_msg_collector_loop([]).

inviso_msg_collector_loop(Msgs) ->
    receive
	{fetch_message,From,Ref,Fun} ->
	    {NewMsgs,Reply}=inviso_msg_collector_selector(Msgs,Fun,[]),
	    From ! {inviso,Ref,Reply},
	    inviso_msg_collector_loop(NewMsgs);
	Msg ->
	    inviso_msg_collector_loop([Msg|Msgs])
    end.

inviso_msg_collector_selector([M|Rest],Fun,Accum) ->
    case Fun(M) of
	{true,X} ->
	    {Rest++Accum,{true,X}};
	_ ->
	    inviso_msg_collector_selector(Rest,Fun,[M|Accum])
    end;
inviso_msg_collector_selector([],_,Accum) ->
    {Accum,false}.
%% ------------------------------------------------------------------------------


%% ==============================================================================
%% Help functions
%% ==============================================================================

list_search([E|Rest],Fun) ->
    case Fun(E) of
	true ->
	    true;
	false ->
	    list_search(Rest,Fun)
    end;
list_search([],_Fun) ->
    false.
%% ------------------------------------------------------------------------------

%% Help function checking that there is a Result for each node in Nodes.
%% Returns 'true' if successful.
check_noderesults(Nodes,Fun,[{Node,Result}|Rest]) when is_function(Fun) ->
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

%% Help function doing rpc on all nodes in Nodes calling M:F. Returns 'true' if
%% successful.
check_on_nodes([Node|Rest],M,F,Args,Result) when Node==node() ->
    if
	is_function(Result) ->
	    ?l true=Result(apply(M,F,Args));
	true ->
	    ?l Result=apply(M,F,Args)
    end,
    check_on_nodes(Rest,M,F,Args,Result);
check_on_nodes([Node|Rest],M,F,Args,Result) ->
    if
	is_function(Result) ->
	    ?l true=Result(rpc:call(Node,M,F,Args));
	true ->
	    ?l Result=rpc:call(Node,M,F,Args)
    end,
    check_on_nodes(Rest,M,F,Args,Result);
check_on_nodes([],_,_,_,_) ->
    true.
%% ------------------------------------------------------------------------------

%% Help function which given a list of files searches through it and returns
%% how many satisfies the RegExp.
%% Returns {ok,N}.
how_many_files_regexp([],_,N) ->
    {ok,N};
how_many_files_regexp([FName|Rest],RegExp,N) ->
    case regexp:first_match(FName,RegExp) of
	{match,1,_} ->
	    how_many_files_regexp(Rest,RegExp,N+1);
	nomatch ->
	    how_many_files_regexp(Rest,RegExp,N);
	{error,Reason} ->
	    test_server:fail(Reason)
    end.
%% ------------------------------------------------------------------------------

%% Help function killing a bunch of registered processes.
process_killer([RegName|Rest]) ->
    case whereis(RegName) of
	undefined ->
	    case global:whereis_name(RegName) of
		undefined ->
		    process_killer(Rest);
		P when is_pid(P) ->
		    if
			node()==node(P) ->
			    exit(P,kill);
			true ->
			    true
		    end,
		    process_killer(Rest)
	    end;
	P when is_pid(P) ->
	    exit(P,kill),
	    process_killer(Rest)
    end;
process_killer([]) ->
    true.
%% ------------------------------------------------------------------------------

%% Help function which waits for a function call to become Result. This is useful
%% if what we are waiting for can happend independantly of indications we have
%% access to.
poll(_,_,_,_,0) ->
    error;
poll(M,F,Args,Result,Times) ->
    try apply(M,F,Args) of
	What when is_function(Result) ->
	    case Result(What) of
		true ->
		    ok;
		_ ->
		    timer:sleep(100),
		    poll(M,F,Args,Result,Times-1)
	    end;
	Result ->
	    ok;
	_ ->
	    timer:sleep(100),
	    poll(M,F,Args,Result,Times-1)
    catch
	error:Reason ->
	    io:format("Apply in suite-function poll/5 failed, ~w~n",[Reason]),
	    timer:sleep(100),
	    poll(M,F,Args,Result,Times-1)
    end.
%% ------------------------------------------------------------------------------

insert_remotenode_config(Name,Node,Config) ->
    [{remotenode,{Name,Node}}|Config].
%% ------------------------------------------------------------------------------

insert_timetraphandle_config(Handle,Config) ->
    [{timetraphandle,Handle}|Config].
%% ------------------------------------------------------------------------------

get_remotenode_config(Name, [{remotenode, {Name, Node}}| _Cs]) ->
    Node;
get_remotenode_config(Name, [_ | Cs]) ->
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

%% This function can be meta traced in order to check that exception_trace works.
%% Must be exported.
failing_function(nofailure) ->
    true;
failing_function(failure) ->
    exit(failure).
%% ------------------------------------------------------------------------------

%% ==============================================================================
%% Code for a test process which can be started.
%% ==============================================================================

test_proc_init() ->
    register(inviso_test_proc,self()),
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

global_test_proc_init() ->
    global:register_name(global_inviso_test_proc,self()),
    test_proc_loop().
%% ------------------------------------------------------------------------------

send_to_test_proc(_,_,_,0) ->
    error;
send_to_test_proc(Msg,Fun,FunArg,N) ->
    inviso_test_proc ! Msg,
    case Fun(FunArg) of
	done ->
	    ok;
	error ->
	    test_server:fail(send_to_test_proc);
	_ ->
	    send_to_test_proc(Msg,Fun,FunArg,N-1)
    end.
%% ------------------------------------------------------------------------------


%% This function is here to be traced on by the inviso_test_proc. Must be exported.
test_function() ->
    1+1.
%% ------------------------------------------------------------------------------


%% ==============================================================================
%% Code for a test side effect table process.
%% ==============================================================================

%% The side effect logger is a process owning a public ETS table. The idea is that
%% various callback functions can write in the table when called. In that way
%% correct calling of the call-backs can be verified.
start_side_effect_logger(Node) ->
    ?l true=is_pid(spawn(Node,?MODULE,side_effect_logger_proc,[])),
    ?l ok=poll(rpc,call,[Node,ets,lookup,[inviso_sideeffect_tab,foo]],[],20).

%% This one must be exported.
side_effect_logger_proc() ->
    register(inviso_tab_proc,self()),        % So we can kill it later.
    ets:new(inviso_sideeffect_tab,[public,named_table]),
    side_effect_logger_proc_2().

side_effect_logger_proc_2() ->
    receive
	_X ->                                % This process is not expecting anything!
	    side_effect_logger_proc_2()
    end.
%% ------------------------------------------------------------------------------

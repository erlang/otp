%% ``The contents of this file are subject to the Erlang Public License,
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
%% Author: Ann-Marie Löf, ann-marie.lof@st.se
%%         Lennart Öhman, lennart.ohman@st.se
%%
%% Description: API module for the inviso system.
%% Inviso consists of a control component and possibly one or more runtime
%% components. This module is simply the API to the inviso system. All normal
%% calls goes through the control component.
%% ------------------------------------------------------------------------------

-module(inviso).

%% ------------------------------------------------------------------------------
%% Exported API functions.
%% ------------------------------------------------------------------------------

-export([start/0, start/1,
	 add_node/1, add_node/2, add_node_if_ref/1, add_node_if_ref/2,
	 add_nodes/2, add_nodes/3, add_nodes_if_ref/2, add_nodes_if_ref/3,
	 change_options/1, change_options/2,
	 init_tracing/1, init_tracing/2, 
	 stop_tracing/0, stop_tracing/1, 
	 clear/0, clear/1, clear/2,
	 flush/0,flush/1,
	 stop/0, stop_nodes/0, stop_nodes/1, stop_all/0,
	 tp/1,tp/2,tp/4,tp/5,tp/6,
	 tpl/1,tpl/2,tpl/4,tpl/5,tpl/6,
	 tpm_localnames/0,tpm_localnames/1,tpm_globalnames/0,tpm_globalnames/1,
	 init_tpm/4,init_tpm/5,init_tpm/7,init_tpm/8,
	 tpm/4,tpm/5,tpm/6,tpm/8,tpm/9,
	 tpm_tracer/4,tpm_tracer/5,tpm_tracer/6,tpm_tracer/8,tpm_tracer/9,
	 tpm_ms/5,tpm_ms/6,
	 tpm_ms_tracer/5,tpm_ms_tracer/6,
	 ctpm_ms/4,ctpm_ms/5,ctpm/3,ctpm/4,
	 ctpm_localnames/0,ctpm_localnames/1,ctpm_globalnames/0,ctpm_globalnames/1,
	 ctp/1,ctp/2,ctp/3,ctp/4,
	 ctpl/1,ctpl/2,ctpl/3,ctpl/4,
	 tf/1, tf/2, tf/3,
	 ctf/1, ctf/2, ctf/3,
	 ctp_all/0, ctp_all/1, ctf_all/0, ctf_all/1,
	 suspend/1, suspend/2,
	 cancel_suspension/0, cancel_suspension/1,
	 get_status/0, get_status/1,
	 get_tracerdata/0, get_tracerdata/1,
	 list_logs/0, list_logs/1,
	 fetch_log/2, fetch_log/3, fetch_log/4,
	 delete_log/0, delete_log/1, delete_log/2,
	 subscribe/0, subscribe/1, 
	 unsubscribe/0, unsubscribe/1]).

%% debuging inviso
-export([state/0, state/1]).

%% ------------------------------------------------------------------------------
%% Macros used in this module.
%% ------------------------------------------------------------------------------

-define(CONTROLLER,inviso_c).

%% Some function calls to runtime components may take long time, we must wait
%% longer than the standard timeout for a reply from the control component.
-define(CALL_TIMEOUT,60000).
%% ------------------------------------------------------------------------------



%% =============================================================================
%% CONTROL COMPONENT API FUNCTIONS.
%% =============================================================================

%% start()={ok,pid()}|{error,Reason}
%% start(Options)={ok,pid()}|{error,Reason} 
%%   Options=[Option,...], the options will be default options to runtime components
%%     later started. See add_node about available options.
%%     There are also options consumed by the control component:
%%       Option={subscribe,pid()}
%%
%% Starts a control component process on the local node. A control component must
%% be started before runtime components can be started manually.
start() ->
    gen_server:start({local,?CONTROLLER},?CONTROLLER,{self(),[]},[]).

start(Options) ->
    gen_server:start({local,?CONTROLLER},?CONTROLLER,{self(),Options},[]).
%% -----------------------------------------------------------------------------

%% add_node(Reference)=NodeResult|{error, Reason}
%% add_node(Reference,Options)=NodeResult|{error, Reason}
%%   Reference=PreviousReference = term(),
%%   Options=[Option,...],
%%   Option={dependency,Dep}
%%     Dep=integer()|'infinity'; The timeout before the runtime component will
%%       terminate if abandoned by this control component.
%%   Option={overload,Overload}; controls how and how often overload checks shall
%%       be performed. Instead of specifying a tuple, the atom 'overload' can be
%%       specified to state no loadcheck. The result will actually be the same
%%       if 'infinity' is used as intervall. It is sometimes necessary to
%%       initialize the overlaod check. This can be done with InitMFA. The
%%       loadchecker must then also be removed by using a RemoveMFA.
%%     Overload=Iterval (int() in milliseconds) |
%%         {LoadMF,Interval}|{LoadMF,Interval,InitMFA,RemoveMFA}
%%           LoadMF={Mod,Func}
%%           InitMFA,RemoveMFA={Mod,Func,ArgList}|void
%%           If just Interval is used, it means using a default overload check.
%%   NodeResult={ok,NAns}|{error,Reason} 
%%   NAns=new|{adopted,State,Status,PreviousReference}|already_added
%%   Status = running | {suspended, SReason}
%%
%% Starts or tries to connect to an existing runtime component at the local
%% node, regardless if the system is distributed or not.
%% Options will override any default options specified at start-up of the
%% control component.
add_node(Reference) ->
    gen_server:call(?CONTROLLER,{add_nodes,[node()],[],Reference,any_ref},?CALL_TIMEOUT).

add_node(Reference,Options) when list(Options) ->
    gen_server:call(?CONTROLLER,{add_nodes,[node()],Options,Reference,any_ref},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% add_node(Reference)=NodeResult|{error,{wrong_reference,OtherRef}}|{error,Reason}
%% add_node(Reference,Options)=NodeResult|{error,{wrong_reference,OtherRef}}|
%%   {error,Reason}
%%
%% As add_node/1,/2 but will only connect to an already existing runtime component
%% if its reference is the same as the one given as argument.
add_node_if_ref(Reference) ->
    gen_server:call(?CONTROLLER,{add_nodes,[node()],[],Reference,if_ref},?CALL_TIMEOUT).

add_node_if_ref(Reference,Options) when list(Options) ->
    gen_server:call(?CONTROLLER,{add_nodes,[node()],Options,Reference,if_ref},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% add_nodes(Nodes,Reference)={ok,NodeResults}|{error,Reason}
%% add_nodes(Nodes,Reference,Options)={ok,NodeResults}|{error,Reason}
%%   Nodes=[Node,...], 
%%   NodeResults=[{Node,NodeResult},...]
%%
%% As add_node/1,/2 but for the nodes specified in Nodes.
%% It is possible but not intended to use this function in a non-distributed
%% system. By speicifying node() as the node where the runtime component shall
%% be started. The return value will then follow the rules of non distributed
%% returnvalues and not have a node indicator.
add_nodes(Nodes,Reference) when list(Nodes) ->
    gen_server:call(?CONTROLLER,{add_nodes,Nodes,[],Reference,any_ref},?CALL_TIMEOUT).

add_nodes(Nodes,Reference,Options) when list(Nodes),list(Options) ->
    gen_server:call(?CONTROLLER,{add_nodes,Nodes,Options,Reference,any_ref},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% add_nodes_if_ref(Nodes,Reference)={ok,NodeResults}|{error,Reason}
%% add_nodes_if_ref(Nodes,Reference,Options)={ok,NodeResults}|{error,Reason}
%%
%% As add_nodes/2,/3 but will only connect to an already existing runtime component
%% if its reference is the same as the one given as argument.
add_nodes_if_ref(Nodes,Reference) when list(Nodes) ->
    gen_server:call(?CONTROLLER,{add_nodes,Nodes,[],Reference,if_ref},?CALL_TIMEOUT).

add_nodes_if_ref(Nodes,Reference,Options) when list(Nodes),list(Options) ->
    gen_server:call(?CONTROLLER,{add_nodes,Nodes,Options,Reference,if_ref},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% change_options(Options)={ok,NodeResults}|NodeResult|{error,Reason}
%% change_options(Nodes,Options)={ok,NodeResults}|{error,Reason}
%%   Nodes=[Node,...], 
%%   Options= see add_node and add_nodes on available options.
%%
%% Change options on all or specified Nodes. This may result in for instance
%% reinitialization of overloadcheck.
change_options(Options) when list(Options) ->
    gen_server:call(?CONTROLLER,{change_options,all,Options},?CALL_TIMEOUT).
change_options(Nodes,Options) when list(Nodes),list(Options)  ->
    gen_server:call(?CONTROLLER,{change_options,Nodes,Options},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% init_tracing(TracerData)={ok,[{Node,NodeResult}]} | NodeResult | {error,Reason}
%% init_tracing(TracerList)={ok,[{Node,NodeResult}]} | {error,Reason}
%% init_tracing(Nodes,TracerData)={ok,[{Node,NodeResult}]}|{error,Reason}
%%   TracerData = [{trace,LogTD} [,{ti,TiTD}]}] | LogTD
%%   LogTD      = {HandlerFun,Data} | collector | {relayer,pid()} |  
%%                {relayer,CollectingNode} | {ip,IPPortParameters} |
%%                {file,FilePortParameters}
%%   TiTD       = {file,FileName} | {file,FileName,TiMFA}
%%   TiMFA      = {Module,Function,ArgumentList} initiating a private loopdata
%%                inside the meta-tracer.
%%   TracerList = [{Node,TracerData}],
%%   IPPortParameters = Portno | {Portno, Qsiz}
%%   Qsiz = 
%%   FilePortParameters = {Filename, wrap, Tail, {time, WrapTime}, WrapCnt} |
%%                        {FileName, wrap, Tail, WrapSize, WrapCnt} |
%%                        {FileName, wrap, Tail, WrapSize} |
%%                        {FileName, wrap, Tail} | FileName
%%                          Tail =/= ""
%%   HandlerFun is a function taking 2 arguments.
%%   Nodes = [node()], 
%%   CollectingNode = pid() | node(),
%%   NodeResult = {ok,LogResults} | {error, NReason} 
%%     LogResults=[LogResult,...]
%%       LogResult={trace_log,LogRes} | {ti_log,LogRes}
%%         LogRes=ok|{error,Reason}
%%
%% Starts tracing on the nodes specified. If just providing a TracerData tracing
%% will be initiated on all our nodes. If it is the non distributed case, that
%% means only on the local non distributed node.
%%
%% {HandlerFun,Data}
%%   Will use the runtime components own process as tracer and handle all
%%   incomming trace message using HandlerFun.
%% {relayer,CollectingNode}
%%   The runtime component addressed will act tracer and relay all incomming trace
%%   messages to Node or Pid, if CollectingNode is not a traced node connected 
%%   to the controll component, the init_tracing call will return an error.
%%   Note that {relayer, Node} only is syntactical sugar for
%%   {relayer, rpc:call(Node,erlang,whereis,[inviso_rt])}
%% collector
%%   The runtime component is used as tracer or collector of relayed 
%%   trace messages using the default handler writing them to io.
%% ip | file - will open a trace-port on Node using PortParameters
init_tracing(TracerDataList) ->
    gen_server:call(?CONTROLLER,{init_tracing,TracerDataList},?CALL_TIMEOUT).

init_tracing(Nodes,TracerData) when list(Nodes) ->
    gen_server:call(?CONTROLLER,{init_tracing,Nodes,TracerData},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% stop_tracing(Nodes)={ok,NodeResults}|{error,Reason}
%% stop_tracing()={ok,NodeResults}|NodeResult
%%   Nodes=[Node,...], 
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult={ok,State}|{error,Reason}
%%     State=new|idle
%% Stops tracing on all or specified Nodes. Flushes trace buffert, 
%% closes trace port and removes all trace flags and meta-patterns.
%% The nodes are called in parallel.
stop_tracing() ->
    gen_server:call(?CONTROLLER,{stop_tracing,all},?CALL_TIMEOUT).

stop_tracing(Nodes) when is_list(Nodes) ->
    gen_server:call(?CONTROLLER,{stop_tracing,Nodes},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% clear()={ok,NodeResults}|NodeResult
%% clear(Nodes,Options)={ok,NodeResults}|{error,Reason}
%% clear(Options)={ok,NodeResults}|NodeResult|{error,Reason}
%%   Nodes=[Node,...], 
%%   Options=[Option,...],
%%   Option=keep_trace_patterns|keep_log_files
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult={ok,{new,Status}}|{error,Reason}
%%     Status=running|{suspended,SReason}
%%
%% Stops all tracing including removing meta-trace patterns. If the node is tracing
%% or idle, logs belonging to the current tracerdata are removed. Hence the node
%% is returned to state 'new'. Note that node can still be suspended.
clear() ->
    gen_server:call(?CONTROLLER,{clear,all,[]},?CALL_TIMEOUT).

clear(Nodes) when list(Nodes) ->
    gen_server:call(?CONTROLLER,{clear,Nodes,[]},?CALL_TIMEOUT).

clear(Nodes,Options) when list(Nodes),list(Options) ->
    gen_server:call(?CONTROLLER,{clear,Nodes,Options},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% flush()={ok,NodeResults} | NodeResult
%% flush(Nodes)={ok,NodeResults}
%%   Nodes=[Node,...]
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult=ok | {error,Reason}
%% Sends a flush request to the trace-port driver on the nodes in Nodes.
%% There will be an error for nodes that are not tracing. It is not an error to
%% try to flush runtime components not using a trace-port.
flush() ->
    gen_server:call(?CONTROLLER,{flush,all},?CALL_TIMEOUT).
flush(Nodes) when is_list(Nodes) ->
    gen_server:call(?CONTROLLER,{flush,Nodes},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% stop()=shutdown
%%
%% Stops the controll component. Runtime components are left as is. They will
%% behave according to their dependency values.
stop() ->
    case catch gen_server:call(?CONTROLLER,stop,?CALL_TIMEOUT) of
	shutdown ->
	    shutdown;
	{'EXIT',{noproc,_}} ->
	    shutdown;
	{'EXIT',Reason} ->
	    exit(Reason)
    end.
%% -----------------------------------------------------------------------------

%% stop_nodes()={ok,NodeResults}|NodeResult
%% stop_nodes(Nodes)={ok,NodeResults}|{error,Reason}
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult=ok|{error,Reason}
%%
%% Stops runtime component on Nodes. stop_nodes/0 will if the control component
%% is running on a distributed node stop all runtime components. And if running
%% on a non distributed node, stop the local and only runtime component.
stop_nodes() ->
    gen_server:call(?CONTROLLER,{stop_nodes,all},?CALL_TIMEOUT).
stop_nodes(Nodes) when is_list(Nodes) ->
    gen_server:call(?CONTROLLER,{stop_nodes,Nodes},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% stop_all()={ok,NodeResults}|NodeResult
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult=ok|{error,Reason}
%%
%% A combination of stop/0 and stop_nodes/0.
stop_all() ->
    gen_server:call(?CONTROLLER, stop_all,?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% tp(Nodes,Module,Function,Arity,MatchSpec,Opts)={ok,NodeResults}|{error,Reason}
%% tp(Nodes,Module,Function,Arity,MatchSpec)={ok,NodeResults}|{error,Reason}
%% tp(Module,Function,Arity,MatchSpec)={ok,NodeResults}|NodeResult|{error,Reason}
%% tp(Nodes,PatternList)={ok,NodeResults}|{error,Reason}
%% tp(PatternList)={ok,NodeResults}|NodeResult|{error,Reason}
%%   Nodes=[Node,...] 
%%   Module,Function=atom() | '_'
%%   Arity=integer() | '_'
%%   MatchSpec=true|false|[]| matchspec()
%%   PatternList=[Pattern,...],
%%   Pattern={Module,Function,Arity,MatchSpec,Opts},
%%   Opts=[Opt,...]
%%     Opt='only_loaded'; means that the runtime component shall not try to load
%%           a module should it not already be present in the runtime system.
%%   NodeResults=[NodeResult,...]
%%   NodeResult={ok,[Ans]}|{error,Reason},
%%   Ans=integer()|{error,Reason}
%%
%% Set trace pattern (global) on specified or all Nodes. The integer replied
%% if the call was successfully describes the matched number of functions.
%% The functions without a Nodes argument means all nodes, in a non-distributed
%% environment it means the local node.
%% When calling several nodes, the nodes are called in parallel.
tp(Nodes,Module,Function,Arity,MatchSpec,Opts) ->
    trace_pattern(Nodes,[{Module,Function,Arity,MatchSpec,Opts}],[global]).

tp(Nodes,Module,Function,Arity,MatchSpec) when list(Nodes) -> 
    trace_pattern(Nodes,[{Module,Function,Arity,MatchSpec,[]}],[global]);
tp(Module,Function,Arity,MatchSpec,Opts) when atom(Module) ->
    trace_pattern(all,[{Module,Function,Arity,MatchSpec,Opts}],[global]).

tp(Module,Function,Arity,MatchSpec) ->
    trace_pattern(all,[{Module,Function,Arity,MatchSpec,[]}],[global]).

tp(Nodes,PatternList) ->
    trace_pattern(Nodes,PatternList,[global]).

tp(PatternList) ->
    trace_pattern(all,PatternList,[global]).
%% -----------------------------------------------------------------------------

%% tpl(Nodes,Module,Function,Arity,MatchSpec)={ok,NodeResults}|{error,Reason}
%% tpl(Module,Function,Arity,MatchSpec)={ok,NodeResults}|NodeResult|{error,Reason}
%% tpl(Nodes,PatternList)={ok,NodeResults}|{error,Reason}
%% tpl(PatternList)={ok,NodeResults}|NodeResult|{error,Reason}
%%   see tp/X for description.
%%
%% Set trace pattern (local) on specified or all Nodes. The integer replied
%% if the command was successfully describes the matched number of functions.
%% The functions without a Nodes argument means all nodes, in a non-distributed
%% environment it means the local node.
%% When calling several nodes, the nodes are called in parallel.
tpl(Nodes,Module,Function,Arity,MatchSpec,Opts) ->
    trace_pattern(Nodes,[{Module,Function,Arity,MatchSpec,Opts}],[local]).

tpl(Nodes,Module,Function,Arity,MatchSpec) when list(Nodes) ->
    trace_pattern(Nodes,[{Module,Function,Arity,MatchSpec,[]}],[local]);
tpl(Module,Function,Arity,MatchSpec,Opts) when atom(Module) ->
    trace_pattern(all,[{Module,Function,Arity,MatchSpec,Opts}],[local]).

tpl(Module,Function,Arity,MatchSpec) ->
    trace_pattern(all,[{Module,Function,Arity,MatchSpec,[]}],[local]).

tpl(Nodes, PatternList) ->
    trace_pattern(Nodes,PatternList,[local]).

tpl(PatternList) ->
    trace_pattern(all,PatternList,[local]).
%% -----------------------------------------------------------------------------

%% ctp(Nodes,Module,Function,Arity)={ok,NodeResults}|{error,Reason}
%% ctp(Module,Function,Arity)={ok,NodeResults}|NodeResult|{error,Reason}
%% ctp(Nodes,PatternList)={ok,NodeResults}|{error,Reason}
%% ctp(PatternList)={ok,NodeResults}|NodeResult|{error,Reason}
%%   PatternList=[{Mod,Func,Arity},...]
%%   see tp/X for other argument descriptions.
%%
%% Clear trace pattern (global) on specified or all Nodes. The integer replied
%% if the call was successfully describes the matched number of functions.
%% The functions without a Nodes argument means all nodes, in a non-distributed
%% environment it means the local node.
%% When calling several nodes, the nodes are called in parallel.
ctp(Nodes,Module,Function,Arity) ->
    trace_pattern(Nodes,[{Module,Function,Arity,false,[only_loaded]}],[global]).

ctp(Module,Function,Arity) ->
    trace_pattern(all,[{Module,Function,Arity,false,[only_loaded]}],[global]).

ctp(Nodes,PatternList) when list(PatternList) ->
    trace_pattern(Nodes,
		  lists:map(fun({M,F,A})->{M,F,A,false,[only_loaded]} end,PatternList),
		  [global]).

ctp(PatternList) when list(PatternList) ->
    trace_pattern(all,
		  lists:map(fun({M,F,A})->{M,F,A,false,[only_loaded]} end,PatternList),
		  [global]).
%% -----------------------------------------------------------------------------

%% ctpl(Nodes,Module,Function,Arity)={ok,NodeResults}|{error,Reason}
%% ctpl(Module,Function,Arity)={ok,NodeResults}|NodeResult|{error,Reason}
%% ctpl(Nodes,PatternList)={ok,NodeResults}|{error,Reason}
%% ctpl(PatternList)={ok,NodeResults}|NodeResult|{error,Reason}
%%   see ctp/X for argument description.
%%
%% Clear trace pattern (local) on specified or all Nodes. The integer replied
%% if the call was successfully describes the matched number of functions.
%% The functions without a Nodes argument means all nodes, in a non-distributed
%% environment it means the local node.
%% When calling several nodes, the nodes are called in parallel.
ctpl(Nodes,Module,Function,Arity) ->
    trace_pattern(Nodes,[{Module,Function,Arity,false,[only_loaded]}],[local]).

ctpl(Module,Function,Arity) ->
    trace_pattern(all,[{Module,Function,Arity,false,[only_loaded]}],[local]).

ctpl(Nodes,PatternList) when list(PatternList) ->
    trace_pattern(Nodes,
		  lists:map(fun({M,F,A})->{M,F,A,false,[only_loaded]} end,PatternList),
		  [local]).

ctpl(PatternList) when list(PatternList) ->
    trace_pattern(all,
		  lists:map(fun({M,F,A})->{M,F,A,false,[only_loaded]} end,PatternList),
		  [local]).
%% -----------------------------------------------------------------------------

%% Help function doing the control component calling for all tp/X, tpl/X, ctp/X
%% and ctpl/X functions.
trace_pattern(Nodes,Patterns,FlagList)  -> 
    gen_server:call(?CONTROLLER, {trace_pattern, Nodes, Patterns, FlagList},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------

%% tf(Nodes,PidSpec,FlagList)={ok,NodeResults}|{error,Reason}
%% tf(PidSpec,FlagList)={ok,NodeResults}|NodeResult|{error,Reason}
%% tf(Nodes,TraceConfList)={ok,NodeResults}|{error,Reason}
%% tf(NodeTraceConfList)={ok,NodeResults}|{error,Reason}
%% tf(TraceConfList)={ok,NodeResults}|NodeResult|{error,Reason}
%%   Nodes=[Node,...],
%%   NodeTraceConfList=[{Node,TraceConfList}]
%%   TraceConfList=[{PidSpec,FlagList},...],
%%   FlagList=[Flags],
%%   PidSpec=all|new|existing|pid()|locally_registered_name()
%%   Flags= all process trace flags allowed.
%%   NodeResult={ok,[Ans]}|{error,Reason},
%%   Ans=integer() | {error,Reason}
%%
%% Set process trace flags on processes on all or specified Nodes. The integer
%% return if the call was successfully describes the matched number of processes.
%% The functions without a Nodes argument means all nodes, in a non-distributed
%% environment it means the local node.
%% There are many combinations which does not make musch scense. For instance
%% specifying a certain pid at all nodes. Or an empty TraceConfList for all
%% nodes.
%% When calling several nodes, the nodes are called in parallel.
tf(Nodes,PidSpec,FlagList) when list(Nodes),list(FlagList) ->
    trace_flags(Nodes,[{PidSpec, FlagList}],true).

tf(Nodes,TraceConfList) when list(Nodes),list(TraceConfList) -> 
    trace_flags(Nodes,TraceConfList,true);
tf(PidSpec,FlagList) when list(FlagList) -> 
    trace_flags(all,[{PidSpec,FlagList}],true).

tf(ArgList) when list(ArgList) ->            % This one has triple functionality!
    case ArgList of
	[{_Process,Flags}|_] when list(Flags),atom(hd(Flags))-> % A call to all nodes.
	    trace_flags(all,ArgList,true);
	[{_Node,TraceConfList}|_] when list(TraceConfList),tuple(hd(TraceConfList)) ->
	    trace_flags(ArgList,true);
	[{_Node,_TraceConfList,_How}|_] ->
	    trace_flags(ArgList);
	[] ->                                % Stupid but allowed.
	    trace_flags(all,ArgList,true)    % Actually doesn't matter which we choose.
    end.
%% -----------------------------------------------------------------------------

%% ctf(Nodes,PidSpec,FlagList)={ok,NodeResults}|{error,Reason}
%% ctf(PidSpec,FlagList)={ok,NodeResults}|NodeResult|{error,Reason}
%% ctf(Nodes,TraceConfList)={ok,NodeResults}|{error,Reason}
%% ctf(TraceConfList)={ok,NodeResults}|NodeResult|{error,Reason}
%%   see tf/X for arguments.
%%
%% Clear process trace flags on all or specified Nodes. The integer replied
%% if the command was successfully describes the matched number of processes.
%% The functions without a Nodes argument means all nodes, in a non-distributed
%% environment it means the local node.
%% When calling several nodes, the nodes are called in parallel.
ctf(Nodes,PidSpec,FlagList) when list(Nodes),list(FlagList) ->
    trace_flags(Nodes,[{PidSpec,FlagList}],false).

ctf(Nodes,TraceConfList) when list(Nodes),list(TraceConfList) -> 
    trace_flags(Nodes,TraceConfList,false);
ctf(PidSpec,FlagList) when list(FlagList) -> 
    trace_flags(all,[{PidSpec,FlagList}],false).

ctf(TraceConfList) when list(TraceConfList) -> 
    trace_flags(all,TraceConfList,false).
%% -----------------------------------------------------------------------------

%% ctf_all(Nodes)={ok,NodeResults}|{error,Reason}
%% ctf_all()={ok,NodeResults}|NodeResult|{error,Reason}
%%   Nodes=[Node,...],
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult=ok|{error,Reason},
%%
%% Clears all trace flags on all or specified nodes. Just for convenience.
ctf_all() ->
    gen_server:call(?CONTROLLER,{trace_flags,all,[{all,[all]}],false},?CALL_TIMEOUT).

ctf_all(Nodes) ->
    gen_server:call(?CONTROLLER,{trace_flags,Nodes,[{all,[all]}],false},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% Help function to tf/X and ctf/X making the call to the control component.
trace_flags(Nodes, TraceConfList, How) -> 
    gen_server:call(?CONTROLLER, {trace_flags, Nodes, TraceConfList, How},?CALL_TIMEOUT).

trace_flags(NodeTraceConfList,How) ->
    gen_server:call(?CONTROLLER,{trace_flags,NodeTraceConfList,How},?CALL_TIMEOUT).

trace_flags(NodeTraceConfListHow) ->
    gen_server:call(?CONTROLLER,{trace_flags,NodeTraceConfListHow},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------

%% tpm_localnames()={ok,NodeResults}|NodeResult|{error,Reason}
%% tpm_localnames(Nodes)={ok,NodeResults}|{error,Reason}
%%   NodeResults=[{Node,NodeResult},...]
%%     NodeResult={ok,N}|{error,Reason}, Note that N can only be 0 or 1.
%%
%% Quick version for setting meta-trace patterns on erlang:register/2. It uses
%% a default CallFunc and ReturnFunc in the meta-tracer server.
%% The main purpose of this function is to create ti-log entries for printing
%% the aliases for process instead of their process identities.
tpm_localnames() ->
    tpm_localnames(all).

tpm_localnames(Nodes) ->
    gen_server:call(?CONTROLLER,{meta_pattern,Nodes,{local_register,[]}},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% tpm_globalnames()={ok,NodeResults}|NodeResult|{error,Reason}
%% tpm_globalnames(Nodes)={ok,NodeResults}|{error,Reason}
%%   NodeResults=[{Node,NodeResult},...]
%%     NodeResult={SubResult,SubResult}
%%       SubResult={ok,N}|{error,Reason}, Note that N can only be 0 or 1.
%% As tpm_locanames/0,/1 but for registering names with global. Note that this
%% actually involves setting meta trace patterns on two functions in global.
tpm_globalnames() ->
    tpm_globalnames(all).

tpm_globalnames(Nodes) ->
    gen_server:call(?CONTROLLER,{meta_pattern,Nodes,{global_register,[]}},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% init_tpm(Mod,Func,Arity,CallFunc)={ok,NodeResults}|NodeResult|{error,Reason}
%% init_tpm(Nodes,Mod,Func,Arity,CallFunc)={ok,NodeResults}|{error,Reason}
%% init_tpm(Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc)=
%%   {ok,NodeResults}|NodeResult|{error,Reason}
%% init_tpm(Nodes,Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc)=
%%   {ok,NodeResults}|{error,Reason}
%%
%%   Mod,Func=Pointing out the function which shall be meta traced, atom().
%%   Arity=As above, integer().
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult=ok|{error,Reason}
%%
%%   InitFunc,RemoveFunc={Module,Function}|fun(), functions being called when
%%     to initialize the public loopdata structure, and to reset it.
%%       InitFunc(Mod,Func,Arity,PublLD)->{ok,NewPublLD,Output}
%%         Supposed to initialize whatever needs to be done before
%%         handling any incoming meta-trace message for the Mod:Func/Arity.
%%       RemoveFunc(Mod,Func,Arity,PublLD)->{ok,NewPublLD}
%%         Called when meta tracing of Mod:Func/Arity is stopped. It is supposed
%%         to clear datastructures away from the PublLD.
%% Initializes the public loopdata for this function. Note that we can not use wildcards
%% here (even if it is perfectly legal in Erlang). It also sets the CallFunc and
%% ReturnFunc for the meta traced function. The function is hence ready to be
%% meta traced with either tpm/5 or tpm_ms/5.
%% When calling several nodes, the nodes are called in parallel.
init_tpm(Mod,Func,Arity,CallFunc) ->
    init_tpm(all,Mod,Func,Arity,CallFunc).

init_tpm(Nodes,Mod,Func,Arity,CallFunc) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,
		     Nodes,
		     {init_tpm,
		      [Mod,Func,Arity,CallFunc]}},
		    ?CALL_TIMEOUT).

init_tpm(Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    init_tpm(all,Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc).

init_tpm(Nodes,Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,
		     Nodes,
		     {init_tpm,
		      [Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc]}},
		    ?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% tpm(Mod,Func,Arity,MS)={ok,NodeResults}|NodeResult|{error,Reason}
%% tpm(Nodes,Mod,Func,Arity,MS)={ok,NodeResults}|{error,Reason}
%% tpm(Mod,Func,Arity,MS,CallFunc)={ok,NodeResults}|NodeResults|{error,Reason}
%% tpm(Nodes,Mod,Func,Arity,MS,CallFunc)={ok,NodeResults}|{error,Reason}
%% tpm(Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc)=
%%   {ok,NodeResults}|NodeResults|{error,Reason}
%% tpm(Nodes,Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc)=
%%   {ok,NodeResults}|{error,Reason}
%%
%%   Mod,Func=atom() and not '_'.
%%   Arity=integer()
%%   MS=list(), matchspecification.
%%   Nodes=List of nodenames.
%%   InitFunc,CallFunc,ReturnFunc,RemoveFunc={Module,Function}|fun(),
%%     functions being called when these functions are called by the meta trace
%%     server at certain events.
%%       CallFunc(CallingPid,ActualArgList,PublLD)->{ok,NewPrivLD,Output}
%%       ReturnFunc(CallingPid,ReturnValue,PublLD)->{ok,NewPrivLD,Output}
%%         When a call respectively return_from trace message arrives for the meta
%%         traced function, the corresponding function is called.
%%         The ReturnFunc must handle the fact that a return_from message arrives
%%         for a call which was never noticed. This because the message queue of the
%%         meta tracer may have been emptied.
%%
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult={ok,N}|{error,Reason}, Note that N can only be 0 or 1.
%%
%% Activates meta-tracing in the inviso_rt_meta tracer. Except when using tpm/6,/8
%% and /9 the function must first have been initiated using init_tpm. If running
%% a non distributed system the variants without Node shall be used. If running
%% in a distributed environment, without Node means all our nodes.
%% When calling several nodes, the nodes are called in parallel.
tpm(Mod,Func,Arity,MS) ->
    tpm(all,Mod,Func,Arity,MS).

tpm(Nodes,Mod,Func,Arity,MS) when integer(Arity) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,Nodes,{tpm,[Mod,Func,Arity,MS]}});
tpm(Mod,Func,Arity,MS,CallFunc) when integer(Arity) ->
    tpm(all,Mod,Func,Arity,MS,CallFunc).

tpm(Nodes,Mod,Func,Arity,MS,CallFunc) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,Nodes,{tpm,[Mod,Func,Arity,MS,CallFunc]}}).

tpm(Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    tpm(all,Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc).

tpm(Nodes,Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,
		     Nodes,
		     {tpm,
		      [Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc]}},
		    ?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% Same as tpm/X but the meta tracer will append {tracer,Tracer} to any enable
%% list in a trace body action term.
tpm_tracer(Mod,Func,Arity,MS) ->
    tpm_tracer(all,Mod,Func,Arity,MS).

tpm_tracer(Nodes,Mod,Func,Arity,MS) when integer(Arity) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,Nodes,{tpm_tracer,[Mod,Func,Arity,MS]}},
		    ?CALL_TIMEOUT);
tpm_tracer(Mod,Func,Arity,MS,CallFunc) when integer(Arity) ->
    tpm_tracer(all,Mod,Func,Arity,MS,CallFunc).

tpm_tracer(Nodes,Mod,Func,Arity,MS,CallFunc) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,Nodes,{tpm_tracer,[Mod,Func,Arity,MS,CallFunc]}}).

tpm_tracer(Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    tpm_tracer(all,Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc).

tpm_tracer(Nodes,Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,
		     Nodes,
		     {tpm_tracer,
		      [Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc]}},
		    ?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% tpm_ms(Mod,Func,Arity,MSname,MS)={ok,NodeResults}|NodeResult|{error,Reason}
%% tpm_ms(Nodes,Mod,Func,Arity,MSname,MS)={ok,NodeResults}|{error,Reason}
%%   Nodes= List of all nodes where the function shall be carried out.
%%   Mod,Func=Pointing out the function to which we shall add a match-spec., atom().
%%   Arity=As above, integer().
%%   MSname=A name to be used if this MS shall be removed later. term().
%%   MatchSpec=List of match specification, Remember {return_trace}
%%     if expecting return_from messages.
%%   NodeResults=[{Node,NodeResult},...]
%%     NodeResult={ok,1}|{ok,0}|{error,Reason} where {ok,1} indicates that
%%       setting the matchspecification for the function succeeded.
%%
%% This function adds a list of match-specs to the already existing ones. It
%% uses an internal database to keep track of existing match-specs. If the
%% match-spec does not result in any meta traced functions (for whatever reason),
%% the MS is not saved in the database. The previously known match-specs are
%% not removed.
%% The function must previously have been initiated in order for this function
%% to add a match-spec.
%% When calling several nodes, the nodes are called in parallel.
tpm_ms(Mod,Func,Arity,MSname,MS) ->
    tpm_ms(all,Mod,Func,Arity,MSname,MS).

tpm_ms(Nodes,Mod,Func,Arity,MSname,MS) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,Nodes,{tpm_ms,[Mod,Func,Arity,MSname,MS]}},
		    ?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% Same as tpm_ms/5, /6 but the meta tracer will append {tracer,Tracer} to any enable
%% list in a trace body action term.
tpm_ms_tracer(Mod,Func,Arity,MSname,MS) ->
    tpm_ms_tracer(all,Mod,Func,Arity,MSname,MS).

tpm_ms_tracer(Nodes,Mod,Func,Arity,MSname,MS) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,Nodes,{tpm_ms_tracer,[Mod,Func,Arity,MSname,MS]}},
		    ?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% ctpm_ms(Mod,Func,Arity,MSname)={ok,NodeResults}|NodeResult|{error,Reason}
%% ctpm_ms(Nodes,Mod,Func,Arity,MSname)={ok,NodeResults}|{error,Reason}
%%   NodeResults=[{Node,NodeResult},...]
%%     NodeResult=ok|{error,Reason}
%%
%% Removes a named match-spec from the meta traced function. Note that it never
%% is a fault to remove an MS. Not even from a function which is non existant.
%% When calling several nodes, the nodes are called in parallel.
ctpm_ms(Mod,Func,Arity,MSname) ->
    ctpm_ms(all,Mod,Func,Arity,MSname).

ctpm_ms(Nodes,Mod,Func,Arity,MSname) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,Nodes,{ctpm_ms,[Mod,Func,Arity,MSname]}},
		    ?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% ctpm(Mod,Func,Arity)={ok,NodeResults}|NodeResult|{error,Reason}
%% ctpm(Node,Mod,Func,Arity)={ok,NodeResults}|{error,Reason}
%%   NodeResults=[{Node,NodeResult},...]
%%     NodeResult=ok|{error,Reason}
%%
%% Removes the meta trace pattern for the function, means stops generating output
%% for this function. The public LD may be cleared by the previously entered
%% RemoveFunc.
%% When calling several nodes, the nodes are called in parallel.
ctpm(Mod,Func,Arity) ->
    ctpm(all,Mod,Func,Arity).

ctpm(Nodes,Mod,Func,Arity) ->
    gen_server:call(?CONTROLLER,
		    {meta_pattern,Nodes,{ctpm,[Mod,Func,Arity]}},
		    ?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% ctpm_localnames()={ok,NodeResults}|NodeResult|{error,Reason}
%% ctpm_localnames(Nodes)={ok,NodeResults}|{error,Reason}
%%   NodeResults=[{Node,NodeResult},...]
%%     NodeResult=ok|{error,Reason}
%%
%% Removes meta-trace pattern for erlang:register/2, previously set by tpm_localnames.
%% When calling several nodes, the nodes are called in parallel.
ctpm_localnames() ->
    ctpm_localnames(all).

ctpm_localnames(Nodes) ->
    gen_server:call(?CONTROLLER,{meta_pattern,Nodes,{remove_local_register,[]}},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% ctpm_localnames()={ok,NodeResults}|NodeResult|{error,Reason}
%% ctpm_localnames(Nodes)={ok,NodeResults}|{error,Reason}
%%   NodeResults=[{Node,NodeResult},...]
%%     NodeResult={SubResult,Subresult}
%%       SubResult=ok|{error,Reason}
%%
%% Removes meta-trace pattern for the register functions in global. Note that there
%% are two of them.
%% When calling several nodes, the nodes are called in parallel.
ctpm_globalnames() ->
    ctpm_globalnames(all).

ctpm_globalnames(Nodes) ->
    gen_server:call(?CONTROLLER,{meta_pattern,Nodes,{remove_global_register,[]}},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% ctp_all(Nodes)={ok,NodeResults}|{error,Reason}
%% ctp_all()={ok,NodeResults}|NodeResult|{error,Reason}
%%   Nodes=[Node,...], 
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult=ok|{error,Reason},
%%
%% Clears all both global and local trace patterns on all or specified nodes.
%% Does not effect meta patterns.
ctp_all() ->
    gen_server:call(?CONTROLLER,{ctp_all,all},?CALL_TIMEOUT).

ctp_all(Nodes) ->
    gen_server:call(?CONTROLLER,{ctp_all,Nodes},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% suspend(Nodes,Reason)={ok,NodeResults}|{error,Reason}
%% suspend(Reason)={ok,NodeResults}|NodeResult|{error,Reason}
%%   Nodes=[Node],
%%   Reason=term(); supposed to describe the reason why suspended.
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult=ok|{error,Reason},
%%
%% Suspend all or specified Nodes with reason Reason. Suspend means that all
%% process trace flags are removed and all meta-patterns.
suspend(Nodes, Reason) ->
    gen_server:call(?CONTROLLER,{suspend,Reason,Nodes},?CALL_TIMEOUT).

suspend(Reason) ->
    gen_server:call(?CONTROLLER,{suspend,Reason,all},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% cancel_suspension(Nodes)={ok,NodeResults}|{error,Reason}
%% cancel_suspension()={ok,NodeResults}|NodeResult|{error,Reason}
%%   Nodes=[Node,...],
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult=ok|{error,Reason},
%% Cancel suspension on all or specified Nodes. Note that this does not imply
%% that "business" is resumed as before. You must reactivate flags and meta-patter
%% your self.
cancel_suspension(Nodes) ->
    gen_server:call(?CONTROLLER,{cancel_suspension,Nodes},?CALL_TIMEOUT).

cancel_suspension() ->
    gen_server:call(?CONTROLLER,{cancel_suspension,all},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% get_status(Nodes)={ok,NodeResults}|{error,Reason}
%% get_status()={ok,NodeResults}|NodeResult|{error,Reason}
%%   Nodes=[Node,...],
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult={ok,{State,Status}}|{error,Reason},
%%   State=new|idle|tracing
%%   Status=running|{suspended,SReason}
%%
%% Get Status form all or specified runtime components.
get_status(Nodes) when list(Nodes) ->
    gen_server:call(?CONTROLLER,{get_status,Nodes},?CALL_TIMEOUT).

get_status() ->
    gen_server:call(?CONTROLLER,{get_status,all},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% get_tracerdata()={ok,NodeResults}|NodeResult|{error,Reason}
%% get_tracerdata(Nodes)={ok,NodeResults}|{error,Reason}
%%   Nodes=[Node,...],
%%   NodeResult={ok,NResult}|{error,Reason},
%%   NResult=TracerData|no_tracerdata
%%   TracerData will be exactly as it was specified when doing init_tracing.
%%
%% Get TracerData form all or specified runtime components.
get_tracerdata() ->
    gen_server:call(?CONTROLLER,{get_tracerdata,all},?CALL_TIMEOUT).

get_tracerdata(Nodes) when is_list(Nodes) -> 
    gen_server:call(?CONTROLLER,{get_tracerdata,Nodes},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% list_logs(TracerData)={ok,NodeResults}|NodeResult|{error,Reason}
%% list_logs(NodeList)={ok,NodeResults}|{error,Reason}
%% list_logs()={ok,NodeResults}|NodeResult|{error,Reason}
%%   TracerData  see init_tracing/1/2
%%   NodeList=[NodeSpec,...]
%%     NodeSpec=Node|{Node,TracerData}
%%   NodeResults=[{Node,NodeResult},...]
%%     NodeResult={ok,FileList}|{ok,no_log}|{error,Reason}
%%       FileList=[FileType,...], one or more of different types.
%%         FileType={trace_log,Dir,Files}|{ti_log,Dir,Files}
%%         Files=[FileNameWithOutPath,...]
%%
%% Ask local or specified runtime components for now existing logs given 
%% TracerData. If TracerData is left out, the runtime components TracerData, 
%% if existing, will be used instead. 
list_logs() ->
    gen_server:call(?CONTROLLER,list_logs,?CALL_TIMEOUT).

list_logs(TracerDataOrNodesList) when list(TracerDataOrNodesList) ->
    gen_server:call(?CONTROLLER,{list_logs,TracerDataOrNodesList},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------

%% fetch_log(LogSpecList,DestDir,Prefix)={ok,NodeResults}|{error,not_distributed}|
%%     {error,Reason} 
%% fetch_log(DestDir,Prefix)={ok,NodeResults}|{error,not_distributed}|
%%     {error,Reason} 
%% fetch_log(ToNode,DestDir,Prefix)=
%% fetch_log(ToNode,LogSpecList,DestDir,Prefix)=
%%   DestDir=string(), to where the fetched files shall be placed.
%%   Prefix=string(), prefix on locally saved fetched files.
%%   LogSpecList=[LogSpec,...],
%%   LogSpec={Node,FileSpecList}|Node|{Node,TracerData} 
%%   TracerData=see init_tracing/1/2
%%   FileSpecList=[{trace_log,Dir,FileList},{ti_log,Dir,FileList}]
%%       where each tuple-item is optional.
%%   FileList=[RemoteFileName,...]
%%   ToNode=atom()
%%   NodeResult={Conclusion,ResultFileSpec}|no_log|{error,NReason}
%%     Conclusion=complete|incomplete
%%     ResultFileSpec=[{trace_log,FileResults},{ti_log,FileResults}]
%%       FileResults=[FileResult,...]
%%         FileResult={ok,FileName}|{error,FReason}
%%     NReason=own_node|Reason
%%   FReason = {file_open,{posix(),FileName}} |
%%             {file_open,{posix(),RemoteFileName}}
%%             {file_open,{posix(),[DestDir,Prefix,RemoteFileName]}}
%%             {file_write,{posix(),FileName}} |
%%             {truncated,FileName}
%%             {truncated,{Reason,FileName}}
%%   posix() - an atom which is named from the Posix error codes used in
%%             Unix, and in the runtime libraries of most C compilers.
%%             See module file in Kernel Reference manual.
%%
%% Copies logfiles over distributed erlang to ToNode. This
%% function can only be used in a distributed system.
%% The resulting transfered files will have the prefix Prefix and will be 
%% located in DestDir.
%% Note that the client process using this function will wait until all files
%% are moved. The job can be cancelled, causing any already copied files to be
%% removed, by simply terminating the waiting client process.
fetch_log(DestDir,Prefix) when list(DestDir),list(Prefix) ->
    gen_server:call(?CONTROLLER,{fetch_log,node(),all,DestDir,Prefix},infinity).

fetch_log(ToNode,DestDir,Prefix) when atom(ToNode),list(DestDir),list(Prefix) ->
    gen_server:call(?CONTROLLER,{fetch_log,ToNode,all,DestDir,Prefix},infinity);

fetch_log(LogSpecList,DestDir,Prefix) when list(LogSpecList),list(DestDir),list(Prefix) ->
    gen_server:call(?CONTROLLER,{fetch_log,node(),LogSpecList,DestDir,Prefix},infinity).

fetch_log(ToNode,LogSpecList,DestDir,Prefix)
  when atom(ToNode),list(LogSpecList),list(DestDir),list(Prefix) ->
    gen_server:call(?CONTROLLER,{fetch_log,ToNode,LogSpecList,DestDir,Prefix},infinity).
%% ------------------------------------------------------------------------------

%% delete_log(Nodes,TracerData)={ok,NodeResults}|{error,Reason}
%% delete_log(NodeSpecList)={ok,NodeResults}|{error,Reason}
%% delete_log(Spec)={ok,NodeResults}|NodeResult|{error,Reason}
%% delete_log(TracerData)={ok,NodeResults}|NodeResult|{error,Reason}
%% delete_log()={ok,NodeResults}|NodeResult|{error,Reason}
%%   Nodes=[Node,...], 
%%   NodeSpecList=[{Node,Spec},...]
%%   Spec=[AbsPathFileName,...]|LogSpecs
%%   LogSpecs=[LogSpec,...]
%%     LogSpec={trace_log,Dir,[FileNameWithoutPath,...]}|
%%             {ti_log,Dir,[FileNameWithoutPath,...]}
%%   TracerData = see init_tracing/1/2
%%   NodeResults=[{Node,NodeResult},...]
%%     NodeResult={ok,no_log}|{ok,LogInfos}|{ok,FileInfos}
%%       LogInfos=[LogInfo,...]
%%         LogInfo={trace_log,FileInfos}|{ti_log,FileInfos}
%%           FileInfos=[FileInfo,...]
%%             FileInfo={ok,FileName}|{error,Reason} whether FileName contains
%%               full path or not depends on if AbsPathFileName or LogSpec was
%%               used when specifying the files.
%%
%% Deletes listed files or files corresponding to TracerData from specified
%% or all Nodes. If no TracerData or list of files is specified in the call the 
%% TracerData at Node will be used to identify log files to delete.
delete_log() ->
    gen_server:call(?CONTROLLER,{delete_log,all},?CALL_TIMEOUT).
delete_log(What) ->
    gen_server:call(?CONTROLLER,{delete_log,What},?CALL_TIMEOUT).
delete_log(Nodes,Spec) ->
    gen_server:call(?CONTROLLER,{delete_log,Nodes,Spec},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% subscribe()= same as subscribe(self())
%% subscribe(Pid)=ok|{error,Reason}
%%   Pid=pid(),  
%%   Reason = 
%% Add Pid or self() to event sending list. Note that it is possible to add a 
%% pid several times and that the Pid then will receive several event messages.
%% All events will be sent to all subscribers in the event sending list.
%%   Event={inviso_event,ControllerPid,erlang:localtime(),Msg},
%%   Msg= 
%%     {connected, Node, {Tag, {State, Status}}}
%%     {disconnected, Node, not_applicable}
%%     {state_change, Node, {State, Status}}
%%     {port_down, Node, Reason}
%%   Node = node() | local_runtime (when running in a non-distributed 
%%                                  environment)
subscribe() ->
    gen_server:call(?CONTROLLER,{subscribe,self()},?CALL_TIMEOUT).

subscribe(Pid) ->
    gen_server:call(?CONTROLLER,{subscribe,Pid},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% unsubscribe()= same as unsubscribe(self())
%% unsubscribe(Pid)=ok
%%   Pid=pid(),  
%%
%% Remove, if present, first occurrence of Pid or self() from event sending 
%% list. Note that it is not an error to remove a non existing subscription.
unsubscribe() ->
    gen_server:call(?CONTROLLER,{unsubscribe,self()},?CALL_TIMEOUT).

unsubscribe(Pid) ->
    gen_server:call(?CONTROLLER,{unsubscribe,Pid},?CALL_TIMEOUT).
%% ------------------------------------------------------------------------------



%% debuging the controller
%% ----------------------------------------------------------------------------
state() ->
    ?CONTROLLER ! state.

%% debuging the runtime component
state(Node) ->
    ?CONTROLLER ! {state, Node}.

%%% end of file

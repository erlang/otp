%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
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
%% The runtime component of the trace tool Inviso.
%%
%% Authors:
%% Lennart Öhman, lennart.ohman@st.se
%% -----------------------------------------------------------------------------

-module(inviso_tool_sh).

%% Inviso Session Handler.
%% This is the code for the session handler process. Its purpose is that we have
%% one session handler process for each trace session started through the
%% start_session inviso tool API. The session handler process is responsible for:
%%
%% -Knowing the state/status of all participating runtime components.
%% -Keeping storage of all tracerdata all our participants have used. This means
%%  also to find out the tracerdata of runtime components connecting by them
%%  selves.
%%
%% STORAGE STRATEGY
%% ----------------
%% The local information storage can be changed by two things. Either by executing
%% commands issued through our APIs. Or by receiving trace_event from the control
%% component. When we execute commands, a corresponding event will also follow.
%% Meaning that in those situations we are informed twice.
%% A simple strategy could be to wait for the event even when doing the changes
%% to the runtime components our self (through commands). But that may result in
%% a small time frame where someone might do yet another command and failing
%% because the local information storage is not uptodate as it would have been
%% expected to be. Therefore we always update the local storage when making changes
%% to a runtime component our selves. There will eventually be a double update
%% through an incoming event. But the storage must coop with that, preventing
%% inconsitancies to happend. An example of a strategy is that the tracerdata table
%% is a bag, not allowing for double entries of the same kind. Therefore a double
%% update is harmless there.

%% ------------------------------------------------------------------------------
%% Module wide constants.
%% ------------------------------------------------------------------------------
-define(LOCAL_RUNTIME,local_runtime).        % Used as node name when non-disitrbuted.
-define(TRACING,tracing).                    % A state defined by the control component.
-define(RUNNING,running).                    % A status according to control componet.

-define(COPY_LOG_FROM,copy_log_from).        % Common fileystem option.
%% ------------------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% API exports.
%% ------------------------------------------------------------------------------
-export([start_link/5,start_link/8]).
-export([cancel_session/1,stop_session/3]).
-export([reactivate/1,reactivate/2]).
-export([ctpl/5,tpl/5,tpl/6,tpl/7,
	 tf/2,tf/3,
	 tpm_localnames/2,init_tpm/6,init_tpm/9,tpm/6,tpm/7,tpm/10,
	 tpm_ms/7,ctpm_ms/6,ctpm/5
	]).
%% ------------------------------------------------------------------------------


%% ------------------------------------------------------------------------------
%% Internal exports.
%% ------------------------------------------------------------------------------
-export([init/1,handle_call/3,handle_info/2,terminate/2]).

-export([get_loopdata/1]).
%% ------------------------------------------------------------------------------


%% ------------------------------------------------------------------------------
%% Includes.
%% ------------------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").     % Necessary for file module.
%% ------------------------------------------------------------------------------


%% ==============================================================================
%% Exported API functions.
%% ==============================================================================

%% start_link(From,NodeParams,CtrlNode,CtrlPid,SafetyCatches,NodesIn,NodesNotIn) =
%%     {ok,Pid} | {error,Reason}
%%   From= pid(), the initial client expecting the reply.
%%   NodeParams=[{Node,TracerData},{Node,TracerData,Opts}...]
%%   CtrlNode=atom() | 'void', the node where the trace control component is.
%%   CtrlPid=pid(), the pid of the trace control component.
%%   SafetyCatches=
%%   Dir=string(), where to place fetched logs and the merged log.
%%   Dbg=debug structure.
%%   NodesIn=[Node,...], list of nodes already in another session.
%%   NodesNotIn=[Node,...], list of nodes not in another session.
%%
%% Starts a session-handler. It keeps track of the the state and status of all
%% participating runtime components. Note that there is a non-distributed case too.
%% In the non-distributed case there is no things such as CtrlNode.
start_link(From,TracerData,CtrlPid,SafetyCatches,Dbg) ->
    gen_server:start_link(?MODULE,
			  {self(),From,TracerData,CtrlPid,SafetyCatches,Dbg},
			  []).

start_link(From,NodeParams,CtrlNode,CtrlPid,SafetyCatches,Dbg,NodesIn,NodesNotIn) ->
    gen_server:start_link(?MODULE,
			  {self(),From,NodeParams,CtrlNode,CtrlPid,
			   SafetyCatches,Dbg,NodesIn,NodesNotIn},
			  []).
%% ------------------------------------------------------------------------------

%% Stops tracing where it is ongoing. Fetches all logfiles.
stop_session(SID,Dir,Prefix) ->
    gen_server:call(SID,{stop_session,Dir,Prefix}).
%% ------------------------------------------------------------------------------

%% stop_session(SID) = ok
%%
%% Cancels the session brutaly. All runtime components are made to stop tracing,
%% all local log files are removed using the tracerdata we know for them.
cancel_session(SID) ->
    gen_server:call(SID,cancel_session).
%% ------------------------------------------------------------------------------

%% reactivate(SID) = {ok,
%% reactivate(SID,Nodes) = {ok,NodeResults} | {error,Reason}.
%%   SID=session id, pid().
%%   Nodes=[Node,...]
%%   NodeResult=[{Node,Result},...]
%%   Result={Good,Bad}
%%   Good,Bad=integer(), the number of redone activities.
%%
%% Function which reactivates runtime components being suspended. This is done
%% replaying all trace flags (in the correct order) to the corresponding nodes.
%% Note that this may also mean turning flags off. Like first turning them on
%% then off a split second later.
reactivate(SID) ->
    gen_server:call(SID,reactivate). %% NOT IMPLEMENTED YET.
reactivate(SID,Nodes) ->
    gen_server:call(SID,{reactivate,Nodes}).
%% ------------------------------------------------------------------------------


%% tpl(SessionID,Mod,Func,Arity,MS)=
%% tpl(SessionID,Mod,Func,Arity,MS,Opts)={ok,N}|{error,Reason}.
%% tpl(SessionID,Nodes,Mod,Func,Arity,MS)=
%% tpl(SessionID,Nodes,Mod,Func,Arity,MS,Opts)={ok,Result}|{error,Reason}
%%   Mod='_' | ModuleName | ModRegExp | {DirRegExp,ModRegExp}
%%     ModRegExp=DirRegExp= string()
%%   Func='_' | FunctionName
%%   Arity='_' | integer()
%%   MS=[] | false | a match specification
%%   Opts=[Opts,...]
%%     Opt={arg,Arg}, disable_safety, {expand_regexp_at,NodeName}, only_loaded
%%   Nodes=[NodeName,...]
tpl(SID,Mod,Func,Arity,MS) ->
    gen_server:call(SID,{tp,tpl,Mod,Func,Arity,MS,[]}).
tpl(SID,Mod,Func,Arity,MS,Opts) when list(MS);MS==true;MS==false ->
    gen_server:call(SID,{tp,tpl,Mod,Func,Arity,MS,Opts});
tpl(SID,Nodes,Mod,Func,Arity,MS) when integer(Arity);Arity=='_' ->
    gen_server:call(SID,{tp,tpl,Nodes,Mod,Func,Arity,MS,[]}).
tpl(SID,Nodes,Mod,Func,Arity,MS,Opts) ->
    gen_server:call(SID,{tp,tpl,Nodes,Mod,Func,Arity,MS,Opts}).
%% ------------------------------------------------------------------------------

%% ctpl(SessionID,Nodes,Mod,Func,Arity)=
%% See tpl/X for arguments.
%%
%% Removes local trace-patterns from functions.
ctpl(SID,Nodes,Mod,Func,Arity) ->
    gen_server:call(SID,{ctp,ctpl,Nodes,Mod,Func,Arity}).
%% ------------------------------------------------------------------------------


tpm_localnames(SID,Nodes) ->
    gen_server:call(SID,{tpm_localnames,Nodes}).

%% tpm_globalnames(SID,Nodes) ->
%%     gen_server:call(SID,{tpm_globalnames,Nodes}).

init_tpm(SID,Nodes,Mod,Func,Arity,CallFunc) ->
    gen_server:call(SID,{init_tpm,Nodes,Mod,Func,Arity,CallFunc}).
init_tpm(SID,Nodes,Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    gen_server:call(SID,
		    {init_tpm,Nodes,Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc}).
tpm(SID,Nodes,Mod,Func,Arity,MS) ->
    gen_server:call(SID,{tpm,Nodes,Mod,Func,Arity,MS}).
tpm(SID,Nodes,Mod,Func,Arity,MS,CallFunc) ->
    gen_server:call(SID,{tpm,Nodes,Mod,Func,Arity,MS,CallFunc}).
tpm(SID,Nodes,Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    gen_server:call(SID,{tpm,Nodes,Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc}).

tpm_ms(SID,Nodes,Mod,Func,Arity,MSname,MS) ->
    gen_server:call(SID,{tpm_ms,Nodes,Mod,Func,Arity,MSname,MS}).

ctpm_ms(SID,Nodes,Mod,Func,Arity,MSname) ->
    gen_server:call(SID,{tpm_ms,Nodes,Mod,Func,Arity,MSname}).

ctpm(SID,Nodes,Mod,Func,Arity) ->
    gen_server:call(SID,{ctpm,Nodes,Mod,Func,Arity}).
%% ------------------------------------------------------------------------------


%% tf(SessionID,Nodes,TraceConfList)=
%%   TraceConfList=[{PidSpec,Flags},...]
%%     PidSpec=pid()|atom()|all|new|existing
%%     Flags=[Flag,...]
tf(SID,TraceConfList) ->
    gen_server:call(SID,{tf,TraceConfList}).
tf(SID,Nodes,TraceConfList) ->
    gen_server:call(SID,{tf,Nodes,TraceConfList}).
%% ------------------------------------------------------------------------------


get_loopdata(SID) ->
    gen_server:call(SID,get_loopdata).
%% ------------------------------------------------------------------------------

%% ==============================================================================
%% Genserver call-backs.
%% ==============================================================================

%% Initial function for the session handler process. The nodes participating in
%% the session must previously have been added to our control component by the tool.
%% The session handler first finds out the state/status of the specified runtime
%% components, then it tries to initiate tracing on those where it is applicable.
%% Note that a reply to the initial (tool)client is done from here instead from
%% the tool-server.
init({Parent,From,TracerData,CtrlPid,SafetyCatches,Dbg}) -> % The non-distributed case.
    {ok,StateStatus}=init_rtcomponent_states([],void,CtrlPid,[?LOCAL_RUNTIME]),
    case is_tool_internal_tracerdata(TracerData) of
	false ->                             % We shall initiate local runtime.
	    case inviso:init_tracing(TracerData) of
		ok ->
		    gen_server:reply(From,{ok,{self(),ok}}),
		    {ok,mk_ld(Parent,
			      void,
			      CtrlPid,
			      to_rtstates([{?LOCAL_RUNTIME,{tracing,?RUNNING},[]}]),
			      [{?LOCAL_RUNTIME,TracerData}],
			      [],
			      SafetyCatches,
			      Dbg)};
		{error,Reason} ->            % It might have become suspended?!
		    gen_server:reply(From,{error,Reason}),
		    {ok,mk_ld(Parent,
			      void,
			      CtrlPid,
			      to_rtstates([{?LOCAL_RUNTIME,StateStatus,[]}]),
			      [{?LOCAL_RUNTIME,TracerData}],
			      [],
			      SafetyCatches,
			      Dbg)}
	    end;
	true ->                              % We shall not pass this one on.
	    gen_server:reply(From,{ok,{self(),ok}}), % Then it is ok.
	    {ok,mk_ld(Parent,
		      void,
		      CtrlPid,
		      to_rtstates([{?LOCAL_RUNTIME,StateStatus,[]}]),
		      [],
		      [?LOCAL_RUNTIME],
		      SafetyCatches,
		      Dbg)}
    end;
init({Parent,From,NodeParams,CtrlNode,CtrlPid,SafetyCatches,Dbg,NodesIn,NodesNotIn}) ->
    case init_rtcomponent_states(NodeParams,CtrlNode,CtrlPid,NodesNotIn) of
	{ok,States} ->                       % A list of {Node,{State,Status},Opts}.
	    {NodeParams2,Nodes2}=remove_nodeparams(NodesIn,NodeParams),
	    case inviso_tool_lib:inviso_cmd(CtrlNode,init_tracing,[NodeParams2]) of
		{ok,Result} ->               % Resulted in state changes!
		    RTStates=set_tracing_rtstates(to_rtstates(States),Result),
		    ReplyValue=init_fix_resultnodes(NodesIn,Nodes2,Result),
		    gen_server:reply(From,{ok,{self(),ReplyValue}}),
		    {ok,mk_ld(Parent,CtrlNode,CtrlPid,RTStates,
			      NodeParams2,Nodes2,SafetyCatches,Dbg)};
		{error,Reason} ->            % Some general failure.
		    inviso_tool_lib:inviso_cmd(CtrlNode,unsubscribe,[]),
		    gen_server:reply(From,{error,{init_tracing,Reason}}),
		    {stop,{init_tracing,Reason}};
		What ->
		    io:format("GOT:~n~w~n",[What]),
		    exit(foo)
	    end;
	{error,Reason} ->                    % Unable to get the state/status.
	    inviso_tool_lib:inviso_cmd(CtrlNode,unsubscribe,[]),
	    gen_server:reply(From,{error,Reason}),
	    {stop,{error,Reason}};
	What ->
	    io:format("GOT:~n~w~n",[What]),
	    exit(foo)
    end.
%% ------------------------------------------------------------------------------

%% To stop a session means stop the tracing and remove all local files on the
%% runtime nodes. We do have a table with all tracer data and that is how we are
%% going to recreate what files to remove.
%% Since runtime components may actually change state when this procedure is
%% on-going, we do not care! It is the state in the session handling process at
%% the time of start of this procedure which is used.
handle_call(cancel_session,_From,LD) ->
    CtrlNode=get_ctrlnode_ld(LD),
    RTStates=get_rtstates_ld(LD),
    Dbg=get_dbg_ld(LD),
    TracingNodes=get_all_tracing_nodes_rtstates(RTStates),
    case stop_all_tracing(CtrlNode,Dbg,TracingNodes) of
	ok->                                 % Hopefully all nodes are stopped now.
	    AvailableNodes=get_all_available_nodes_rtstates(RTStates),
	    TRDstorage=get_trdstorage_ld(LD),
	    remove_all_local_logs(CtrlNode,TRDstorage,AvailableNodes,Dbg),
	    {stop,normal,ok,LD};             % LD actually not correct now!
	{error,Reason} ->                    % Some serious error when stop_tracing.
	    {stop,normal,{error,Reason},LD}
    end;
%% ------------------------------------------------------------------------------

%% *Stop all tracing on runtime components still tracing.
%% *Copy all local log files to the collection directory.
handle_call({stop_session,Dir,Prefix},_From,LD) ->
    case check_directory_exists(Dir) of      % Check that this directory exists here.
	true ->
	    RTStates=get_rtstates_ld(LD),
	    CtrlNode=get_ctrlnode_ld(LD),
	    Dbg=get_dbg_ld(LD),
	    TracingNodes=get_all_tracing_nodes_rtstates(RTStates),
	    case stop_all_tracing(CtrlNode,Dbg,TracingNodes) of
		ok ->                        % Hopefully no node is still tracing now.
		    TRDstorage=get_trdstorage_ld(LD),
		    AvailableNodes=get_all_available_nodes_rtstates(RTStates),
		    {FailedNodes,FetchedFiles}=
			transfer_logfiles(RTStates,CtrlNode,Dir,Prefix,
					  TRDstorage,Dbg,AvailableNodes),
		    RemoveNodes=             % We only delete local logs where fetch ok.
			lists:filter(fun(N)->
					     case lists:keysearch(N,1,FailedNodes) of
						 {value,_} ->
						     false;
						 false ->
						     true
					     end
				     end,
				     AvailableNodes),
		    remove_all_local_logs(CtrlNode,TRDstorage,RemoveNodes,Dbg),
		    {stop,normal,{ok,{FailedNodes,FetchedFiles}},LD};
		{error,Reason} ->            % Some general failure, quit.
		    {stop,normal,{error,Reason},LD}
	    end;
	false ->                             % You specified a non-existing directory!
	    {reply,{error,{faulty_dir,Dir}},LD}
    end;
%% ------------------------------------------------------------------------------

handle_call({reactivate,Nodes},_From,LD) ->
    RTStates=get_rtstates_ld(LD),
    {OurNodes,OtherNodes}=
	remove_nodes_not_ours(Nodes,get_all_session_nodes_rtstates(RTStates)),
    CtrlNode=get_ctrlnode_ld(LD),
    ACTstorage=get_actstorage_ld(LD),
    case h_reactivate(CtrlNode,OurNodes,ACTstorage) of
	{ok,Results} ->                      % A list of {Node,Result}.
	    if
		OtherNodes==[] ->            % Normal case, no non-session nodes.
		    {reply,{ok,Results},LD};
		true ->                      % Add error values for non-session nodes.
		    {reply,
		     {ok,
		      lists:map(fun(N)->{N,{error,not_in_session}} end,OtherNodes)++
		      Results},
		     LD}
	    end;
	{error,Reason} ->                    % Then this error takes presidence.
	    {reply,{error,Reason},LD}
    end;
%% ------------------------------------------------------------------------------

%% Call-back for set trace-pattern for both global and local functions.
handle_call({tp,PatternFunc,Mod,F,A,MS,Opts},_From,LD) ->
    Reply=h_tp(all,PatternFunc,Mod,F,A,MS,Opts,LD), % For all active nodes in the session.
    {reply,Reply,LD};
handle_call({tp,PatternFunc,Nodes,Mod,F,A,MS,Opts},_From,LD) ->
    RTStates=get_rtstates_ld(LD),
    SNodes=get_all_session_nodes_rtstates(RTStates), % Notes belongoing to the session.
    {Nodes2,FaultyNodes}=remove_nodes_not_ours(Nodes,SNodes),
    Reply=h_tp(Nodes2,PatternFunc,Mod,F,A,MS,Opts,LD),
    ErrorReply=lists:map(fun(N)->{N,{error,not_in_session}} end,FaultyNodes),
    {reply,ErrorReply++Reply,LD};
%% ------------------------------------------------------------------------------

%% Call-back handling the removal of both local and global trace-patterns.
%% NOT IMPLEMENTED YET.
handle_call({ctp,PatternFunc,Nodes,Mod,F,A},_From,LD) ->
    Reply=h_ctp(Nodes,PatternFunc,Mod,F,A,LD),
    {reply,Reply,LD};
%% ------------------------------------------------------------------------------

handle_call({tpm_localnames,Nodes},_From,LD) ->
    RTStates=get_rtstates_ld(LD),
    OurNodes=get_all_session_nodes_rtstates(RTStates),
    {Nodes2,NotOurNodes}=remove_nodes_not_ours(Nodes,OurNodes),
    ACTstorage=get_actstorage_ld(LD),
    {Reply,NewACTstorage}=
	h_tpm_localnames(get_ctrlnode_ld(LD),Nodes2,RTStates,ACTstorage),
    ErrorReply=lists:map(fun(N)->{N,{error,not_in_session}} end,NotOurNodes),
    {reply,ErrorReply++Reply,put_actstorage_ld(NewACTstorage,LD)};

handle_call({init_tpm,Nodes,Mod,Func,Arity,CallFunc},_From,LD) ->
    RTStates=get_rtstates_ld(LD),
    OurNodes=get_all_session_nodes_rtstates(RTStates),
    {Nodes2,NotOurNodes}=remove_nodes_not_ours(Nodes,OurNodes),
    ACTstorage=get_actstorage_ld(LD),
    {Reply,NewACTstorage}=
	h_all_tpm(get_ctrlnode_ld(LD),
		  Nodes2,
		  init_tpm,
		  [Mod,Func,Arity,CallFunc],
		  RTStates,
		  ACTstorage),
    ErrorReply=lists:map(fun(N)->{N,{error,not_in_session}} end,NotOurNodes),
    {reply,ErrorReply++Reply,put_actstorage_ld(NewACTstorage,LD)};

handle_call({init_tpm,Nodes,Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc},_From,LD) ->
    RTStates=get_rtstates_ld(LD),
    OurNodes=get_all_session_nodes_rtstates(RTStates),
    {Nodes2,NotOurNodes}=remove_nodes_not_ours(Nodes,OurNodes),
    ACTstorage=get_actstorage_ld(LD),
    {Reply,NewACTstorage}=
	h_all_tpm(get_ctrlnode_ld(LD),
		  Nodes2,
		  init_tpm,
		  [Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc],
		  RTStates,
		  ACTstorage),
    ErrorReply=lists:map(fun(N)->{N,{error,not_in_session}} end,NotOurNodes),
    {reply,ErrorReply++Reply,put_actstorage_ld(NewACTstorage,LD)};

handle_call({tpm,Nodes,Mod,Func,Arity,MS},_From,LD) ->
    RTStates=get_rtstates_ld(LD),
    OurNodes=get_all_session_nodes_rtstates(RTStates),
    {Nodes2,NotOurNodes}=remove_nodes_not_ours(Nodes,OurNodes),
    ACTstorage=get_actstorage_ld(LD),
    {Reply,NewACTstorage}=
	h_all_tpm(get_ctrlnode_ld(LD),Nodes2,tpm,[Mod,Func,Arity,MS],RTStates,ACTstorage),
    ErrorReply=lists:map(fun(N)->{N,{error,not_in_session}} end,NotOurNodes),
    {reply,ErrorReply++Reply,put_actstorage_ld(NewACTstorage,LD)};

handle_call({tpm,Nodes,Mod,Func,Arity,MS,CallFunc},_From,LD) ->
    RTStates=get_rtstates_ld(LD),
    OurNodes=get_all_session_nodes_rtstates(RTStates),
    {Nodes2,NotOurNodes}=remove_nodes_not_ours(Nodes,OurNodes),
    ACTstorage=get_actstorage_ld(LD),
    {Reply,NewACTstorage}=
	h_all_tpm(get_ctrlnode_ld(LD),
		  Nodes2,
		  tpm,
		  [Mod,Func,Arity,MS,CallFunc],
		  RTStates,
		  ACTstorage),
    ErrorReply=lists:map(fun(N)->{N,{error,not_in_session}} end,NotOurNodes),
    {reply,ErrorReply++Reply,put_actstorage_ld(NewACTstorage,LD)};

handle_call({tpm,Nodes,Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc},_From,LD) ->
    RTStates=get_rtstates_ld(LD),
    OurNodes=get_all_session_nodes_rtstates(RTStates),
    {Nodes2,NotOurNodes}=remove_nodes_not_ours(Nodes,OurNodes),
    ACTstorage=get_actstorage_ld(LD),
    {Reply,NewACTstorage}=
	h_all_tpm(get_ctrlnode_ld(LD),
		  Nodes2,
		  tpm,
		  [Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc],
		  RTStates,
		  ACTstorage),
    ErrorReply=lists:map(fun(N)->{N,{error,not_in_session}} end,NotOurNodes),
    {reply,ErrorReply++Reply,put_actstorage_ld(NewACTstorage,LD)};

handle_call({tpm_ms,Nodes,Mod,Func,Arity,MSname,MS},_From,LD) ->
    RTStates=get_rtstates_ld(LD),
    OurNodes=get_all_session_nodes_rtstates(RTStates),
    {Nodes2,NotOurNodes}=remove_nodes_not_ours(Nodes,OurNodes),
    ACTstorage=get_actstorage_ld(LD),
    {Reply,NewACTstorage}=
	h_all_tpm(get_ctrlnode_ld(LD),
		  Nodes2,
		  tpm_ms,
		  [Mod,Func,Arity,MSname,MS],
		  RTStates,
		  ACTstorage),
    ErrorReply=lists:map(fun(N)->{N,{error,not_in_session}} end,NotOurNodes),
    {reply,ErrorReply++Reply,put_actstorage_ld(NewACTstorage,LD)};

handle_call({ctpm_ms,Nodes,Mod,Func,Arity,MSname},_From,LD) ->
    RTStates=get_rtstates_ld(LD),
    OurNodes=get_all_session_nodes_rtstates(RTStates),
    {Nodes2,NotOurNodes}=remove_nodes_not_ours(Nodes,OurNodes),
    ACTstorage=get_actstorage_ld(LD),
    {Reply,NewACTstorage}=
	h_all_tpm(get_ctrlnode_ld(LD),
		  Nodes2,
		  ctpm_ms,
		  [Mod,Func,Arity,MSname],
		  RTStates,
		  ACTstorage),
    ErrorReply=lists:map(fun(N)->{N,{error,not_in_session}} end,NotOurNodes),
    {reply,ErrorReply++Reply,put_actstorage_ld(NewACTstorage,LD)};

handle_call({ctpm,Nodes,Mod,Func,Arity},_From,LD) ->
    RTStates=get_rtstates_ld(LD),
    OurNodes=get_all_session_nodes_rtstates(RTStates),
    {Nodes2,NotOurNodes}=remove_nodes_not_ours(Nodes,OurNodes),
    ACTstorage=get_actstorage_ld(LD),
    {Reply,NewACTstorage}=
	h_all_tpm(get_ctrlnode_ld(LD),Nodes2,ctpm,[Mod,Func,Arity],RTStates,ACTstorage),
    ErrorReply=lists:map(fun(N)->{N,{error,not_in_session}} end,NotOurNodes),
    {reply,ErrorReply++Reply,put_actstorage_ld(NewACTstorage,LD)};
%% ------------------------------------------------------------------------------

%% Call-back for setting process trace-flags. Handles both distributed and non-
%% distributed case.
handle_call({tf,TraceConfList},From,LD) ->
    handle_call({tf,all,TraceConfList},From,LD);
handle_call({tf,Nodes,TraceConfList},_From,LD) ->
    {Reply,NewACTstorage}=h_tf(get_ctrlnode_ld(LD),
			      Nodes,
			      TraceConfList,
			      get_actstorage_ld(LD),
			      get_rtstates_ld(LD)),
    {reply,Reply,put_actstorage_ld(NewACTstorage,LD)};
%% ------------------------------------------------------------------------------



handle_call(get_loopdata,_From,LD) ->
    io:format("The loopdata:~n~p~n",[LD]),
    {reply,ok,LD}.
%% ------------------------------------------------------------------------------


%% Clause handling an incomming state-change event from the control component.
%% Note that it does not have to be one of our nodes since it is not possible
%% to subscribe to certain node-events.
%% We may very well get state-change events for state-changes we are the source
%% to our selves. Those state-changes are already incorporated into the RTStates.
%% There is however no harm in doing them again since we know that this event
%% message will reach us before a reply to a potentially following state-change
%% request will reach us. Hence we will do all state-changes in the correct order,
%% even if sometimes done twice.
handle_info({trace_event,CtrlPid,_Time,{state_change,Node,{State,Status}}},LD) ->
    case get_ctrlpid_ld(LD) of
	CtrlPid ->                           % It is from our control component.
	    case {State,Status} of
		{?TRACING,?RUNNING} ->       % This is the only case when new tracerdata!
		    NewTracerData=add_current_tracerdata_ld(get_ctrlnode_ld(LD),
							    Node,
							    get_rtstates_ld(LD),
							    get_trdstorage_ld(LD)),
		    NewRTStates=statechange_rtstates(Node,State,Status,get_rtstates_ld(LD)),
		    {noreply,put_trdstorage_ld(NewTracerData,
					       put_rtstates_ld(NewRTStates,LD))};
		_ ->                         % In all other cases, just fix rtstates.
		    NewRTStates=statechange_rtstates(Node,State,Status,get_rtstates_ld(LD)),
		    {noreply,put_rtstates_ld(NewRTStates,LD)}
	    end;
	_ ->
	    {noreply,LD}
    end;
%% If a new runtime component connects to our trace control component, and it is
%% in our list of runtime components belonging to this session, we may update its
%% state to now being present. Otherwise it does not belong to this session.
%% Note that we avoid updating an already connected runtime component. This
%% can happend if it connected by itself after we started the session handler,
%% but before we managed to initiate tracing. Doing so or not will not result in
%% any error in the long run, but during a short period of time we might be
%% prevented from doing things with the runtime though it actually is tracing.
handle_info({trace_event,CtrlPid,_Time,{connected,Node,{_Tag,{State,Status}}}},LD) ->
    case get_ctrlpid_ld(LD) of
	CtrlPid ->                           % It is from our control component.
	    case get_statestatus_rtstates(Node,get_rtstates_ld(LD)) of
		{ok,unavailable} ->          % This is the situation when we update!
		    NewRTStates=statechange_rtstates(Node,State,Status,get_rtstates_ld(LD)),
		    {noreply,put_rtstates_ld(NewRTStates,LD)};
		_ ->                         % In all other cases, let it be.
		    {noreply,LD}
	    end;
	_ ->                                 % Not from our control component.
	    {noreply,LD}
    end;
%% If a runtime component disconnects we mark it as unavailable. We must also
%% remove all saved trace-flags in order for them to not be accidently reactivated
%% should the runtime component reconnect and then suspend.
handle_info({trace_event,CtrlPid,_Time,{disconnected,Node,_}},LD) ->
    case get_ctrlpid_ld(LD) of
	CtrlPid ->                           % It is from our control component.
	    NewRTStates=set_unavailable_rtstates(Node,get_rtstates_ld(LD)),
	    NewACTstorage=del_node_actstorage(Node,get_actstorage_ld(LD)),
	    {noreply,put_actstorage_ld(NewACTstorage,put_rtstates_ld(NewRTStates,LD))};
	_ ->
	    {noreply,LD}
    end;
handle_info(_,LD) ->
    {noreply,LD}.
%% ------------------------------------------------------------------------------

%% In terminate we cancel our subscription to event from the trace control component.
%% That should actually not be necessary, but lets do it the correct way!
terminate(_,LD) ->
    case get_ctrlnode_ld(LD) of
	void ->                              % Non-distributed.
	    inviso:unsubscribe();
	Node ->
	    inviso_tool_lib:inviso_cmd(Node,unsubscribe,[])
    end.
%% ------------------------------------------------------------------------------



%% ==============================================================================
%% First level help functions to call-backs.
%% ==============================================================================

%% ------------------------------------------------------------------------------
%% Help functions to init.
%% ------------------------------------------------------------------------------

%% Help function which find out the state/status of the runtime components.
%% Note that since we have just started subscribe to state changes we must
%% check our inqueue to see that we have no waiting messages for the nodes
%% we learned the state/status of. If there is a waiting message we don't
%% know whether that was a state change received before or after the state
%% check was done. We will then redo the state-check.
%% Returns {ok,States} or {error,Reason}.
%% Where States is [{Node,{State,Status},Opts},...].
%% Note that {error,Reason} can not occur in the non-distributed case.
init_rtcomponent_states(NodeParams,void,CtrlPid,Nodes) -> % The non-distributed case.
    ok=inviso:subscribe(),
    init_rtcomponent_states_2(NodeParams,void,CtrlPid,Nodes,[]);
init_rtcomponent_states(NodeParams,CtrlNode,CtrlPid,Nodes) ->
    ok=inviso_tool_lib:inviso_cmd(CtrlNode,subscribe,[]),
    init_rtcomponent_states_2(NodeParams,CtrlNode,CtrlPid,Nodes,[]).

init_rtcomponent_states_2(_,_,_,[],States) ->
    {ok,States};
init_rtcomponent_states_2(NodeParams,void,CtrlPid,_Nodes,States) ->
    case inviso:get_status() of
	{ok,StateStatus} ->                  % Got its state/status, now...
	    {ProblemNodes,NewStates}=
		init_rtcomponent_states_3(NodeParams,CtrlPid,[{?LOCAL_RUNTIME,{ok,StateStatus}}],
					  [],States),
	    init_rtcomponent_states_2(NodeParams,void,CtrlPid,ProblemNodes,NewStates);
	{error,_Reason} ->                   % The runtime is not available!?
	    {ok,[{?LOCAL_RUNTIME,unavailable,[]}]} % Create the return value immediately.
    end;
init_rtcomponent_states_2(NodeParams,CtrlNode,CtrlPid,Nodes,States) ->
    case inviso_tool_lib:inviso_cmd(CtrlNode,get_status,[Nodes]) of
	{ok,NodeResult} ->
	    {ProblemNodes,NewStates}=
		init_rtcomponent_states_3(NodeParams,CtrlPid,NodeResult,[],States),
	    init_rtcomponent_states_2(NodeParams,CtrlNode,CtrlPid,ProblemNodes,NewStates);
	{error,Reason} ->                    % Severe problem, abort the session.
	    {error,{get_status,Reason}}
    end.

%% Traverses the list of returnvalues and checks that we do not have an event
%% waiting in the message queue. If we do have, it is a problem. That node will
%% be asked about its state again.
%% Note that it is here we construct the RTStatesList.
init_rtcomponent_states_3(NodeParams,CtrlPid,[{Node,{ok,{State,Status}}}|Rest],Problems,States) ->
    receive
	{trace_event,CtrlPid,_Time,{state_change,Node,_}} ->
	    init_rtcomponent_states_3(NodeParams,CtrlPid,Rest,[Node|Problems],States)
    after
	0 ->                                 % Not in msg queue, then we're safe!
	    RTState=case lists:keysearch(Node,1,NodeParams) of
			{value,{_Node,_TracerData,Opts}} ->
			    {Node,{State,Status},Opts};
			_ ->                 % No option available, use [].
			    {Node,{State,Status},[]}
		    end,
	    init_rtcomponent_states_3(NodeParams,CtrlPid,Rest,Problems,[RTState|States])
    end;
init_rtcomponent_states_3(NodeParams,CtrlPid,[{Node,{error,_Reason}}|Rest],Problems,States) ->
    RTState=case lists:keysearch(Node,1,NodeParams) of
			{value,{_Node,_TracerData,Opts}} ->
			    {Node,unavailable,Opts};
			_ ->                 % No option available, use [].
			    {Node,unavailable,[]}
		    end,
    init_rtcomponent_states_3(NodeParams,CtrlPid,Rest,Problems,[RTState|States]);
init_rtcomponent_states_3(_,_,[],Problems,States) ->
    {Problems,States}.
%% ------------------------------------------------------------------------------

%% Help function removing nodes from NodeParams. The reason for this can either
%% be that we are using a tool internal tracerdata that shall not be forwarded to
%% the trace control component, or that the node is actually already part of
%% another session.
%% Returns {NewNodeParams,NodesWhichShallNotBeInitiated}.
remove_nodeparams(Nodes,NodesParams) ->
    remove_nodeparams_2(Nodes,NodesParams,[],[]).

remove_nodeparams_2(Nodes,[NodeParam|Rest],NPAcc,NAcc) when  % NPAcc=NodeParamsAcc.
  (is_tuple(NodeParam) and ((size(NodeParam)==2) or (size(NodeParam)==3))) ->
    Node=element(1,NodeParam),
    Params=element(2,NodeParam),             % This is tracerdata!
    case lists:member(Node,Nodes) of
	true ->                              % Remove this one, in another session.
	    remove_nodeparams_2(Nodes,Rest,NPAcc,NAcc);
	false ->                             % Ok so far...
	    case is_tool_internal_tracerdata(Params) of
		false ->                     % Then keep it and use it later!
		    remove_nodeparams_2(Nodes,Rest,[{Node,Params}|NPAcc],NAcc);
		true ->                      % Since it is, remove it from the list.
		    remove_nodeparams_2(Nodes,Rest,NPAcc,[Node|NAcc])
	    end
    end;
remove_nodeparams_2(Nodes,[_|Rest],NPAcc,NAcc) -> % Faulty NodeParam, skip it!
    remove_nodeparams_2(Nodes,Rest,NPAcc,NAcc);
remove_nodeparams_2(_,[],NPAcc,NAcc) ->
    {lists:reverse(NPAcc),NAcc}.
%% ------------------------------------------------------------------------------

%% Help function which adds both the nodes which were already part of another
%% session and the nodes that we actually did not issue any init_tracing for.
%% Returns a new Result list of [{Node,NodeResult},...].
init_fix_resultnodes(NodesOtherSes,NodesNotInit,Result) ->
    NewResult=init_fix_resultnodes_2(NodesOtherSes,{error,in_other_session},Result),
    init_fix_resultnodes_2(NodesNotInit,ok,NewResult).

init_fix_resultnodes_2([Node|Rest],NodeResult,Result) ->
    [{Node,NodeResult}|init_fix_resultnodes_2(Rest,NodeResult,Result)];
init_fix_resultnodes_2([],_,Result) ->
    Result.                                  % Append Result to the end of the list.
%% ------------------------------------------------------------------------------


%% ------------------------------------------------------------------------------
%% Help functions to reactivate.
%% ------------------------------------------------------------------------------

h_reactivate(CtrlNode,Nodes,ACTstorage) ->   % Distributed case.
    case inviso_tool_lib:inviso_cmd(CtrlNode,cancel_suspension,[Nodes]) of
	{ok,CSuspResults} ->
	    {GoodNodes,BadResults}=          % Sort out nodes no longer suspended.
		lists:foldl(fun({Node,ok},{GoodNs,BadNs})->
				    {[Node|GoodNs],BadNs};
			       ({Node,{error,Reason}},{GoodNs,BadNs})->
				    {GoodNs,[{Node,{error,{cancel_suspension,Reason}}}|BadNs]}
			    end,
			    {[],[]},
			    CSuspResults),
	    Results=h_reactivate_redo_activity(CtrlNode,GoodNodes,ACTstorage,[]),
	    {ok,BadResults++Results};
	{error,Reason} ->                    % General failure cancelling suspend.
	    {error,{cancel_suspension,Reason}}
    end.
%% ------------------------------------------------------------------------------

%% Help function which traverses the list of nodes known to be ours and have
%% cancelled their suspend. If we fail redoing one of the activities associated
%% with a node, the node will be reported in the return value as failed. From
%% that point on its state must be considered unknown since we do not know how
%% many of the activities were successfully redone.
h_reactivate_redo_activity(CtrlNode,[Node|Rest],ACTstorage,Acc) ->
    case get_activities_actstorage(Node,ACTstorage) of
	{ok,Activities} ->                   % The node existed in activity storage.
	    {Good,Bad}=h_reactivate_redo_activity_2(CtrlNode,Node,Activities,0,0),
	    h_reactivate_redo_activity(CtrlNode,Rest,ACTstorage,[{Node,{Good,Bad}}|Acc]);
	false ->                             % Node not present in activity storage.
	    h_reactivate_redo_activity(CtrlNode,Rest,ACTstorage,[{Node,{0,0}}|Acc])
    end;
h_reactivate_redo_activity(_CtrlNode,[],_,Acc) ->
    lists:reverse(Acc).

%% Help function actually redoing the activity. Note that there must be one
%% clause here for every type of activity.
%% Returns {NrGoodCmds,NrBadCmds}.
%% The number of good or bad commands refers to inviso commands done. If any
%% of the subparts of such a command returned an error, the command is concidered
%% no good.
h_reactivate_redo_activity_2(CtrlNode,Node,[{tf,{Op,TraceConfList}}|Rest],Good,Bad) ->
    case inviso_tool_lib:inviso_cmd(CtrlNode,Op,[[Node],TraceConfList]) of
	{ok,[{_Node,{ok,Answers}}]} ->
	    case h_reactivate_redo_activity_check_tf(Answers) of
		ok ->
		    h_reactivate_redo_activity_2(CtrlNode,Node,Rest,Good+1,Bad);
		error ->                     % At least oneReports the first encountered error.
		    h_reactivate_redo_activity_2(CtrlNode,Node,Rest,Good,Bad+1)
	    end;
	{ok,[{_Node,{error,_Reason}}]} ->
	    h_reactivate_redo_activity_2(CtrlNode,Node,Rest,Good,Bad+1);
	{error,_Reason} ->                   % General error when doing cmd.
	    h_reactivate_redo_activity_2(CtrlNode,Node,Rest,Good,Bad+1)
    end;
h_reactivate_redo_activity_2(CtrlNode,Node,[{tpm,{Op,InvisoCmdParams}}|Rest],Good,Bad) ->
    case inviso_tool_lib:inviso_cmd(CtrlNode,Op,[[Node]|InvisoCmdParams]) of
	{ok,[{_Node,ok}]} ->
	    h_reactivate_redo_activity_2(CtrlNode,Node,Rest,Good+1,Bad);
	{ok,[{_Node,{error,_Reason}}]} ->
	    h_reactivate_redo_activity_2(CtrlNode,Node,Rest,Good,Bad+1);
	{error,_Reason} ->                   % General error when doing cmd.
	    h_reactivate_redo_activity_2(CtrlNode,Node,Rest,Good,Bad+1)
    end;
h_reactivate_redo_activity_2(_CtrlNode,_Node,[],Good,Bad) ->
    {Good,Bad}.

%% Help function traversing a list of results from inviso:tf/2 or inviso:ctf/2
%% to see if there were any errors.
h_reactivate_redo_activity_check_tf([N|Rest]) when integer(N) ->
    h_reactivate_redo_activity_check_tf(Rest);
h_reactivate_redo_activity_check_tf([{error,_Reason}|_]) ->
    error;
h_reactivate_redo_activity_check_tf([]) ->
    ok.
%% ------------------------------------------------------------------------------


%% ------------------------------------------------------------------------------
%% Help functions to tp (setting trace patterns, both local and global).
%% ------------------------------------------------------------------------------

%% Help function which handles both tpl and tp. Note that the non-distributed case
%% handled with Nodes='all'.
%% Returns what shall be the reply to the client.
h_tp(all,PatternFunc,Mod,F,A,MS,Opts,LD) ->  % All available runtime nodes.
    Nodes=get_all_available_nodes_rtstates(get_rtstates_ld(LD)),
    h_tp(Nodes,PatternFunc,Mod,F,A,MS,Opts,LD);
h_tp(Nodes,PatternFunc,Mod,F,A,MS,Opts,LD) -> % Only certain nodes in the session.
    CtrlNode=get_ctrlnode_ld(LD),
    Dbg=get_dbg_ld(LD),
    SafetyCatches=get_safetycatches_ld(LD),
    case inviso_tool_lib:expand_module_names(Nodes,Mod,Opts) of % Take care of any reg-exps.
	{multinode_expansion,NodeMods} ->
	    NodeTPs=inviso_tool_lib:make_patterns(SafetyCatches,Opts,Dbg,NodeMods,F,A,MS),
	    h_tp_node_by_node(CtrlNode,PatternFunc,Dbg,NodeTPs,[]);
	{singlenode_expansion,Modules} ->
	    TPs=inviso_tool_lib:make_patterns(SafetyCatches,Opts,Dbg,Modules,F,A,MS),
	    h_tp_do_tps(CtrlNode,Nodes,TPs,PatternFunc,Dbg);
	module ->
	    TPs=inviso_tool_lib:make_patterns(SafetyCatches,Opts,Dbg,[Mod],F,A,MS),
	    h_tp_do_tps(CtrlNode,Nodes,TPs,PatternFunc,Dbg);
	wildcard ->                          % Means do for all modules, no safety.
	    h_tp_do_tps(CtrlNode,Nodes,[{Mod,F,A,MS}],PatternFunc,Dbg);
	{error,Reason} ->
	    {error,Reason}
    end.

%% Note that this function can never be called in the non-distributed case.
h_tp_node_by_node(CtrlNode,PatternFunc,Dbg,[{Node,TPs}|Rest],Accum) ->
    case h_tp_do_tps(CtrlNode,[Node],TPs,PatternFunc,Dbg) of
	{ok,[{Node,Result}]} ->
	    h_tp_node_by_node(CtrlNode,PatternFunc,Dbg,Rest,[{Node,Result}|Accum]);
	{error,Reason} ->                    % Failure, but don't stop.
	    h_tp_node_by_node(CtrlNode,PatternFunc,Dbg,Rest,[{Node,{error,Reason}}|Accum])
    end;
h_tp_node_by_node(_,_,_,[],Accum) ->
    {ok,lists:reverse(Accum)}.

%% Help function which does the actual call to the trace control component.
%% Note that Nodes can be a list of nodes (including a single one) or
%% ?LOCAL_RUNTIME if we are not distributed. The non-distributed case is otherwise
%% detected by the 'void' CtrlNode.
%% Returns {ok,[{Node,{ok,{NrOfFunctions,NrOfErrors}}},{Node,{error,Reason}},...]} or
%% {error,Reason}. In the non-distributed case {ok,{NrOfFunctions,NrOfErros}} or
%% {error,Reason}.
h_tp_do_tps(void,_Nodes,TPs,PatternFunc,Dbg) -> % Non distributed case!
    inviso_tool_lib:debug(tp,Dbg,[TPs,PatternFunc]),
    case inviso:PatternFunc(TPs) of
	{ok,Result} ->                       % A list of [Nr1,Nr2,error,...].
	    {ok,
	     lists:foldl(fun(N,{AccNr,AccErr}) when integer(N) ->
				 {AccNr+N,AccErr};
			    (error,{AccNr,AccErr}) ->
				 {AccNr,AccErr+1}
			 end,
			 {0,0},
			 Result)};
	{error,Reason} ->
	    {error,{PatternFunc,Reason}}
    end;
h_tp_do_tps(CtrlNode,Nodes,TPs,PatternFunc,Dbg) ->
    inviso_tool_lib:debug(tp,Dbg,[Nodes,TPs,PatternFunc]),
    case inviso_tool_lib:inviso_cmd(CtrlNode,PatternFunc,[Nodes,TPs]) of
	{ok,Result} ->                       % Result is [{Node,Result},...].
	    {ok,
	     lists:map(fun({Node,{ok,Res}})->
			       {Node,lists:foldl(fun(N,{ok,{AccNr,AccErr}}) when integer(N) ->
							 {ok,{AccNr+N,AccErr}};
						    (error,{AccNr,AccErr}) ->
							 {ok,{AccNr,AccErr+1}}
						 end,
						 {ok,{0,0}},
						 Res)};
			  ({_Node,{error,Reason}})->
			       {error,Reason}
		       end,
		       Result)};
	{error,Reason} ->
	    {error,{PatternFunc,Reason}}
    end.
%% ------------------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% Help functions for removing trace-patterns.
%% ------------------------------------------------------------------------------

%% NOT IMPLEMENTED YET.
h_ctp(Node,PatternFunc,Mod,F,A,LD) ->
    tbd.
%% ------------------------------------------------------------------------------


%% ------------------------------------------------------------------------------
%% Help functions for calling the trace information facility.
%% ------------------------------------------------------------------------------


%% Function handling the meta trace pattern for capturing registration of local
%% process names.
h_tpm_localnames(CtrlNode,Nodes,RTStates,ACTstorage) ->
    AvailableNodes=get_all_available_nodes_rtstates(RTStates),
    {Nodes3,FaultyNodes}=remove_nodes_not_ours(Nodes,AvailableNodes),
    case inviso_tool_lib:inviso_cmd(CtrlNode,tpm_localnames,[Nodes3]) of
	{ok,Result} ->                       % That good we want to modify tpmstorage!
	    NewACTstorage=add_tpm_actstorage(Result,tpm_localnames,[],ACTstorage),
	    ErrorResult=lists:map(fun(N)->{N,{error,not_available}} end,FaultyNodes),
	    {{ok,ErrorResult++Result},NewACTstorage};
	{error,Reason} ->                    % If general failure, do not modify storage.
	    {{error,Reason},ACTstorage}
    end.
%% ------------------------------------------------------------------------------

%% Functions calling meta trace functions for specified nodes. This function is
%% intended for use with all tmp function calls, init_tpm,tpm,tpm_ms,ctpm_ms and
%% ctpm.
%% Note that we must store called meta trace functions and their parameters in the
%% activity storage in order to be able to redo them in case of a reactivate.
h_all_tpm(CtrlNode,Nodes,TpmCmd,InvisoCmdParams,RTStates,ACTstorage) ->
    AvailableNodes=get_all_available_nodes_rtstates(RTStates),
    {Nodes3,FaultyNodes}=remove_nodes_not_ours(Nodes,AvailableNodes),
    case inviso_tool_lib:inviso_cmd(CtrlNode,TpmCmd,[Nodes3|InvisoCmdParams]) of
	{ok,Result} ->                       % That good we want to modify tpmstorage!
	    NewACTstorage=add_tpm_actstorage(Result,TpmCmd,InvisoCmdParams,ACTstorage),
	    ErrorResult=lists:map(fun(N)->{N,{error,not_available}} end,FaultyNodes),
	    {{ok,ErrorResult++Result},NewACTstorage};
	{error,Reason} ->                    % If general failure, do not modify storage.
	    {{error,Reason},ACTstorage}
    end.
%% ------------------------------------------------------------------------------


%% ------------------------------------------------------------------------------
%% Help functions for set trace flags.
%% ------------------------------------------------------------------------------

%% Help function which sets the tracepatterns in TraceConfList for all nodes
%% mentioned in Nodes. Note that non-distributed case is handled with Nodes='all'.
%% Returns {Reply,NewACTstorage} where Reply is whatever shall be returned to caller
%% and NewACTstorage is traceflag storage modified with the flags added to the
%% corresponding nodes.
h_tf(void,_Nodes,TraceConfList,ACTstorage,_RTStates) -> % The non-distributed case.
    Reply=inviso:tf(TraceConfList),
    NewACTstorage=add_tf_actstorage([{?LOCAL_RUNTIME,Reply}],tf,TraceConfList,ACTstorage),
    {Reply,NewACTstorage};
h_tf(CtrlNode,all,TraceConfList,ACTstorage,RTStates) ->
    AllNodes=get_all_session_nodes_rtstates(RTStates),
    h_tf(CtrlNode,AllNodes,TraceConfList,ACTstorage,RTStates);
h_tf(CtrlNode,Nodes,TraceConfList,ACTstorage,_RTStates) ->
    case inviso_tool_lib:inviso_cmd(CtrlNode,tf,[Nodes,TraceConfList]) of
	{ok,Result} ->                       % That good we want to modify actstorage!
	    NewACTstorage=add_tf_actstorage(Result,tf,TraceConfList,ACTstorage),
	    {{ok,Result},NewACTstorage};
	{error,Reason} ->                    % If general failure, do not modify actstorage.
	    {{error,Reason},ACTstorage}
    end.
%% ------------------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% Help functions to stop_session.
%% ------------------------------------------------------------------------------

%% This function fetches all local log-files using our stored tracerdata. Note
%% that there are two major ways of tranfering logfiles. Either via distributed
%% Erlang or by common filesystem (like NFS). The default is distributed Erlang.
%% But there may be info in the RTStates structure about a common file-system.
%% Returns {FailedNodes,FetchedFileNames} where FailedNodes is a list of
%% nodenames where problems occurred. Note that problems does not necessarily
%% mean that no files were copied.
%% FetchedFileNames contains one or two of the tuples {trace_log,Files} and/or
%% {ti_log,Files}, listing all files successfully fetched. Note that the
%% list of fetched files contains sublists of filenames. One for each node and
%% tracerdata.
%% In the non-distributed system we always use copy (since the files always
%% resides locally).
transfer_logfiles(RTStates,CtrlNode,Dir,Prefix,TRDstorage,Dbg,AvailableNodes) ->
    if
	CtrlNode==void ->                    % When non-distributed, always copy!
		fetch_logfiles_copy(CtrlNode,Dir,Prefix,TRDstorage,Dbg,[?LOCAL_RUNTIME]);
	true ->                              % The distributed case.
	    {FetchNodes,CopyNodes}=find_logfile_transfer_methods(AvailableNodes,RTStates),
	    {FailedFetchNodes,FetchedFiles}=
		case fetch_logfiles_distributed(CtrlNode,Dir,Prefix,TRDstorage,Dbg,FetchNodes) of
		    {ok,Failed,Files} ->     % So far no disasters.
			{Failed,Files};
		    {error,Reason} ->        % Means all fetch-nodes failed!
			inviso_tool_lib:debug(transfer_logfiles,Dbg,[FetchNodes,Reason]),
			{lists:map(fun(N)->{N,error} end,FetchNodes),[]}
		end,
	    {FailedCopyNodes,CopiedFiles}=
		fetch_logfiles_copy(CtrlNode,Dir,Prefix,TRDstorage,Dbg,CopyNodes),
	    {FailedFetchNodes++FailedCopyNodes,FetchedFiles++CopiedFiles}
    end.

%% Help function which finds out which node we have a common file system with
%% and from which we must make distributed erlang tranfere.
%% Returns {DistributedNodes,CopyNodes} where CopyNode is [{Node,CopyFromDir},...].
find_logfile_transfer_methods(Nodes,RTStates) ->
    find_logfile_transfer_methods_2(Nodes,RTStates,[],[]).

find_logfile_transfer_methods_2([Node|Rest],RTStates,FetchAcc,CopyAcc) ->
    {ok,Opts}=get_opts_rtstates(Node,RTStates), % Node must be in RTStates!
    case lists:keysearch(?COPY_LOG_FROM,1,Opts) of
	{value,{_,FromDir}} when list(FromDir) -> % Node has common filesystem.
	    find_logfile_transfer_methods_2(Rest,RTStates,FetchAcc,[{Node,FromDir}|CopyAcc]);
	{value,_} ->                         % Can't understand dir option.
	    find_logfile_transfer_methods_2(Rest,RTStates,[Node|FetchAcc],CopyAcc);
	false ->                             % Then we want to use fetch instead.
	    find_logfile_transfer_methods_2(Rest,RTStates,[Node|FetchAcc],CopyAcc)
    end;
find_logfile_transfer_methods_2([],_,FetchAcc,CopyAcc) ->
    {FetchAcc,CopyAcc}.
%% ------------------------------------------------------------------------------

%% Help function which transferes all local logfiles according to the tracerdata
%% stored for the nodes in Nodes.
%% Returns {ok,FailedNodes,FileNodeSpecs} or {error,Reason}.
%% FailedNodes is a list of nodes where fetching logs did not succeed, partially
%% or not at all.
%% FileNames is a list of list of actually fetched files (the name as it is here, including
%% Dir). The sublists are files which belong together.
fetch_logfiles_distributed(CtrlNode,Dir,Prefix,TRDstorage,Dbg,Nodes) ->
    LogSpecList=build_logspeclist(Nodes,TRDstorage),
    case inviso_fetch_log(inviso_tool_lib:inviso_cmd(CtrlNode,
						     fetch_log,
						     [LogSpecList,Dir,Prefix])) of
	{ok,Result} ->
	    Files=get_all_filenames_fetchlog_result(Result,Dbg),
	    FailedNodes=get_all_failednodes_fetchlog_result(Result),
	    {ok,FailedNodes,Files};
	{error,Reason} ->                    % Some general failure!
	    {error,{fetch_log,Reason}}
    end.

%% Help function which constructs a list {Node,TracerData} for all nodes in Nodes.
%% Note that there may be more than one tracerdata for a node, resulting in multiple
%% tuples for that node.
build_logspeclist(Nodes,TRDstorage) ->
    build_logspeclist_2(Nodes,TRDstorage,[]).

build_logspeclist_2([Node|Rest],TRDstorage,Acc) ->
    TRDlist=find_tracerdata_for_node_trd(Node,TRDstorage), % A list of all tracerdata.
    build_logspeclist_2(Rest,
			TRDstorage,
			[lists:map(fun(TRD)->{Node,TRD} end,TRDlist)|Acc]);
build_logspeclist_2([],_,Acc) ->
    lists:flatten(Acc).

%% Help function which translates inviso:fetch_log return values to what I
%% want!
inviso_fetch_log({error,Reason}) ->
    {error,Reason};
inviso_fetch_log({_Success,ResultList}) ->
    {ok,ResultList}.

%% Help function which collects all filenames mentioned in a noderesult structure.
%% The files may or may not be complete.
%% Returns a list of list of filenames. Each sublist contains files which belong
%% together, i.e because they are a wrap-set.
get_all_filenames_fetchlog_result(NodeResult,Dbg) ->
    get_all_filenames_fetchlog_result_2(NodeResult,Dbg,[]).

get_all_filenames_fetchlog_result_2([{Node,{Success,FileInfo}}|Rest],Dbg,Accum) 
  when Success=/=error, list(FileInfo) ->
    SubAccum=get_all_filenames_fetchlog_result_3(FileInfo,[]),
    get_all_filenames_fetchlog_result_2(Rest,Dbg,[{Node,SubAccum}|Accum]);
get_all_filenames_fetchlog_result_2([{Node,{error,FReason}}|Rest],Dbg,Accum) ->
    inviso_tool_lib:debug(fetch_files,Dbg,[Node,FReason]),
    get_all_filenames_fetchlog_result_2(Rest,Dbg,Accum);
get_all_filenames_fetchlog_result_2([],_Dbg,Accum) ->
    Accum.

get_all_filenames_fetchlog_result_3([{FType,Files}|Rest],SubAccum) ->
    FilesOnly=lists:foldl(fun({ok,FName},Acc)->[FName|Acc];(_,Acc)->Acc end,[],Files),
    get_all_filenames_fetchlog_result_3(Rest,[{FType,FilesOnly}|SubAccum]);
get_all_filenames_fetchlog_result_3([],SubAccum) ->
    SubAccum.		  

%% Help function which traverses a noderesult and builds a list as return
%% value containing the nodenames of all nodes not being complete.
%% Note that a node may occur multiple times since may have fetched logfiles
%% for several tracerdata from the same node. Makes sure the list contains
%% unique node names.
%% Returns a list nodes.
get_all_failednodes_fetchlog_result(NodeResult) ->
    get_all_failednodes_fetchlog_result_2(NodeResult,[]).

get_all_failednodes_fetchlog_result_2([{_Node,{complete,_}}|Rest],Acc) ->
    get_all_failednodes_fetchlog_result_2(Rest,Acc);
get_all_failednodes_fetchlog_result_2([{Node,{_Severity,_}}|Rest],Acc) ->
    case lists:member(Node,Acc) of
	true ->                              % Already in the list.
	    get_all_failednodes_fetchlog_result_2(Rest,Acc);
	false ->                             % Not in Acc, add it!
	    get_all_failednodes_fetchlog_result_2(Rest,[Node|Acc])
    end;
get_all_failednodes_fetchlog_result_2([],Acc) ->
    Acc.
%% ------------------------------------------------------------------------------

%% Help function which copies files from one location to Dir and at the same time
%% adds the Prefix to the filename. NodeSpecs contains full path to the files. The
%% reason the node information is still part of NodeSpecs is that otherwise we can
%% not report faulty nodes. Note that one node may occur multiple times since there
%% may be more than one tracerdata for a node.
%% Returns {FailedNodes,Files} where FailedNodes is a list of nodes where problems
%% occurred. Files is a tuple list of [{Node,[{FType,FileNames},...]},...].
fetch_logfiles_copy(CtrlNode,Dir,Prefix,TRDstorage,Dbg,NodeSpecs) ->
    CopySpecList=build_copylist(CtrlNode,Dbg,NodeSpecs,TRDstorage),
    fetch_logfiles_copy_2(Dir,Prefix,Dbg,CopySpecList,[],[]).

fetch_logfiles_copy_2(Dir,Prefix,Dbg,[{Node,CopySpecs}|Rest],FailedNodes,Files) ->
    case fetch_logfiles_copy_3(Dir,Prefix,Dbg,CopySpecs,[],0) of
	{0,LocalFiles} ->                    % Copy went ok and zero errors.
	    fetch_logfiles_copy_2(Dir,Prefix,Dbg,Rest,FailedNodes,[{Node,LocalFiles}|Files]);
	{_N,LocalFiles} ->                   % Copied files, but some went wrong.
	    case lists:member(Node,FailedNodes) of
		true ->                      % Node already in FailedNodes.
		    fetch_logfiles_copy_2(Dir,Prefix,Dbg,Rest,FailedNodes,
					  [{Node,LocalFiles}|Files]);
		false ->                     % Node not marked as failed, yet.
		    fetch_logfiles_copy_2(Dir,Prefix,Dbg,Rest,[Node|FailedNodes],
					  [{Node,LocalFiles}|Files])
	    end
    end;
fetch_logfiles_copy_2(_,_,_,[],FailedNodes,Files) ->
    {FailedNodes,Files}.                     % The return value from fetch_logfiles_copy.

fetch_logfiles_copy_3(Dir,Prefix,Dbg,[{FType,RemoteFiles}|Rest],Results,Errors) ->
    {Err,LocalFiles}=fetch_logfiles_copy_3_1(Dir,Prefix,Dbg,RemoteFiles,[],0),
    fetch_logfiles_copy_3(Dir,Prefix,Dbg,Rest,[{FType,LocalFiles}|Results],Errors+Err);
fetch_logfiles_copy_3(_,_,_,[],Results,Errors) ->
    {Errors,Results}.

%% For each file of one file-type (e.g. trace_log).
fetch_logfiles_copy_3_1(Dir,Prefix,Dbg,[File|Rest],LocalFiles,Errors) ->
    DestName=Prefix++filename:basename(File),
    Destination=filename:join(Dir,DestName),
    case do_copy_file(File,Destination) of
	ok ->
	    fetch_logfiles_copy_3_1(Dir,Prefix,Dbg,Rest,[DestName|LocalFiles],Errors);
	{error,Reason} ->
	    inviso_tool_lib:debug(copy_files,Dbg,[File,Destination,Reason]),
	    fetch_logfiles_copy_3_1(Dir,Prefix,Dbg,Rest,LocalFiles,Errors+1)
    end;
fetch_logfiles_copy_3_1(_,_,_,[],LocalFiles,Errors) ->
    {Errors,LocalFiles}.

%% Help function which builds a [{Node,[{Type,[ListOfRemoteFiles]}},...}]
%% where Type describes trace_log or ti_log and each entry in ListOfRemoteFiles
%% is a complete path to a file to be copied.
build_copylist(CtrlNode,Dbg,NodeSpecList,TRDstorage) ->
    build_copylist_2(CtrlNode,Dbg,NodeSpecList,TRDstorage,[]).

%% For each node specified in the NodeSpecList.
build_copylist_2(CtrlNode,Dbg,[{Node,SourceDir}|Rest],TRDstorage,Acc) ->
    TRDlist=find_tracerdata_for_node_trd(Node,TRDstorage),
    CopySpecList=build_copylist_3(CtrlNode,Dbg,SourceDir,Node,TRDlist),
    build_copylist_2(CtrlNode,Dbg,Rest,TRDstorage,[CopySpecList|Acc]);
build_copylist_2(_,_,[],_,Acc) ->
    lists:flatten(Acc).

%% For each tracerdata found for the node.
build_copylist_3(void,Dbg,SourceDir,Node,[TRD|Rest]) -> % The non-distributed case.
    case inviso:list_logs(TRD) of
	{ok,FileSpec} when list(FileSpec) -> % [{trace_log,Dir,Files},...]
	    NewFileSpec=build_copylist_4(SourceDir,FileSpec,[]),
	    [{Node,NewFileSpec}|build_copylist_3(void,Dbg,SourceDir,Node,Rest)];
	{ok,no_log} ->                       % This tracedata not associated with any log.
	    build_copylist_3(void,Dbg,SourceDir,Node,Rest);
	{error,Reason} ->
	    inviso_tool_lib:debug(list_logs,Dbg,[Node,TRD,Reason]),
	    build_copylist_3(void,Dbg,SourceDir,Node,Rest)
    end;
build_copylist_3(CtrlNode,Dbg,SourceDir,Node,[TRD|Rest]) -> % The distributed case.
    case inviso_tool_lib:inviso_cmd(CtrlNode,list_logs,[[{Node,TRD}]]) of
	{ok,[{Node,{ok,FileSpec}}]} when list(FileSpec) ->
	    NewFileSpec=build_copylist_4(SourceDir,FileSpec,[]),
	    [{Node,NewFileSpec}|build_copylist_3(CtrlNode,Dbg,SourceDir,Node,Rest)];
	{ok,[{Node,{ok,no_log}}]} ->         % It relays to another node, no files!
	    build_copylist_3(CtrlNode,Dbg,SourceDir,Node,Rest);
	{ok,[{Node,{error,Reason}}]} ->
	    inviso_tool_lib:debug(list_logs,Dbg,[Node,TRD,Reason]),
	    build_copylist_3(CtrlNode,Dbg,SourceDir,Node,Rest);
	{error,Reason} ->                    % Some general failure.
	    inviso_tool_lib:debug(list_logs,Dbg,[Node,TRD,Reason]),
	    build_copylist_3(CtrlNode,Dbg,SourceDir,Node,Rest)
    end;
build_copylist_3(_,_,_,_,[]) ->
    [].

%% Help function which makes a [{Type,Files},...] list where each file in Files
%% is with full path as found from our file-system.
build_copylist_4(SourceDir,[{Type,_Dir,Files}|Rest],Accum) ->
    NewFiles=
	lists:foldl(fun(FName,LocalAcc)->[filename:join(SourceDir,FName)|LocalAcc] end,
		    [],
		    Files),
    build_copylist_4(SourceDir,Rest,[{Type,NewFiles}|Accum]);
build_copylist_4(_,[],Accum) ->
    Accum.
				 

%% Help function which copies a file using os:cmd.
%% Returns 'ok' or {error,Reason}.
do_copy_file(Source,Destination) ->
    case os:type() of
	{win32,_} ->
	    os:cmd("copy "++Source++" "++Destination), % Perhaps a test on success?
	    ok;
	{unix,_} ->
	    os:cmd("cp "++Source++" "++Destination), % Perhaps a test on success?
	    ok
    end.
%% ------------------------------------------------------------------------------


%% ------------------------------------------------------------------------------

%% ==============================================================================
%% Various help functions.
%% ==============================================================================

%% Help function going through the Nodes list and checking that only nodes
%% mentioned in OurNodes gets returned. It also makes the nodes in the return
%% value unique.
remove_nodes_not_ours(Nodes,OurNodes) ->
    remove_nodes_not_ours_2(Nodes,OurNodes,[],[]).

remove_nodes_not_ours_2([Node|Rest],OurNodes,OurAcc,OtherAcc) ->
    case lists:member(Node,OurNodes) of
	true ->                              % Ok it is one of our nodes.
	    case lists:member(Node,OurAcc) of
		true ->                      % Already in the list, skip.
		    remove_nodes_not_ours_2(Rest,OurNodes,OurAcc,OtherAcc);
		false ->
		    remove_nodes_not_ours_2(Rest,OurNodes,[Node|OurAcc],OtherAcc)
	    end;
	false ->
	    case lists:member(Node,OtherAcc) of
		true ->
		    remove_nodes_not_ours_2(Rest,OurNodes,OurAcc,OtherAcc);
		false ->
		    remove_nodes_not_ours_2(Rest,OurNodes,OurAcc,[Node|OtherAcc])
	    end
    end;
remove_nodes_not_ours_2([],_,OurAcc,OtherAcc) ->
    {lists:reverse(OurAcc),lists:reverse(OtherAcc)}.
%% ------------------------------------------------------------------------------

%% Help function which returns 'true' or 'false' depending on if TracerData is
%% meant to be used by the session handler (true) or if it supposed to be passed
%% on to the trace system.
is_tool_internal_tracerdata(_) -> % CURRENTLY NO INTERNAL TRACER DATA!
    false.
%% ------------------------------------------------------------------------------

%% Help function which checks that all nodes in the first list of nodes exists
%% in the second list of nodes. Returns 'true' or 'false'. The latter if as much
%% as one incorrect node was found.
check_our_nodes([Node|Rest],AllNodes) ->
    case lists:member(Node,AllNodes) of
	true ->
	    check_our_nodes(Rest,AllNodes);
	false ->                             % Then we can stop right here.
	    false
    end;
check_our_nodes([],_) ->
    true.
%% ------------------------------------------------------------------------------

%% Help function which checks that a directory actually exists. Returns 'true' or
%% 'false'.
check_directory_exists(Dir) ->
    case file:read_file_info(Dir) of
	{ok,#file_info{type=directory}} ->
	    true;
	_ ->                                 % In all other cases it is not valid.
	    false
    end.
%% ------------------------------------------------------------------------------

%% This function stops the tracing on all nodes in Nodes. Preferably Nodes is a list
%% of only tracing runtime components. Not that there will actually be any difference
%% since the return value does not reflect how stopping the nodes went.
%% Returns 'ok' or {error,Reason}, the latter only in case of general failure.
stop_all_tracing(void,Dbg,[?LOCAL_RUNTIME]) ->   % The non-distributed case, and is tracing.
    case inviso:stop_tracing() of
	{ok,_State} ->
	    ok;
	{error,Reason} ->                    % We actually don't care.
	    inviso_tool_lib:debug(stop_tracing,Dbg,[?LOCAL_RUNTIME,Reason]),
	    ok
    end;
stop_all_tracing(void,_,_) ->                % There is no local runtime started.
    ok;
stop_all_tracing(CtrlNode,Dbg,Nodes) ->
    case inviso_tool_lib:inviso_cmd(CtrlNode,stop_tracing,[Nodes]) of
	{ok,Result} ->                       % The result is only used for debug.
	    Failed=lists:foldl(fun({N,{error,Reason}},Acc)->[{N,{error,Reason}}|Acc];
				  (_,Acc)->Acc
			       end,
			       [],
			       Result),
	    if
		Failed==[] ->
		    ok;
		true ->
		    inviso_tool_lib:debug(stop_tracing,Dbg,[Nodes,Failed]),
		    ok
	    end;
	{error,Reason} ->
	    {error,{stop_tracing,Reason}}
    end.
%% ------------------------------------------------------------------------------

%% Help function removing all local logs using the tracerdata to determine what
%% logs to remove from where.
%% There is no significant return value since it is not really clear what to do
%% if removal went wrong. The function can make debug-reports thought.
remove_all_local_logs(CtrlNode,TRDstorage,Nodes,Dbg) ->
    LogSpecList=build_logspeclist_remove_logs(Nodes,TRDstorage),
    case inviso_tool_lib:inviso_cmd(CtrlNode,delete_log,[LogSpecList]) of
	{ok,Results} ->
	    case look_for_errors_resultlist(Results) of
		[] ->                        % No errors found in the result!
		    true;
		Errors ->
		    inviso_tool_lib:debug(remove_all_local_logs,Dbg,[Errors]),
		    true
	    end;
	{error,Reason} ->                    % Some general error.
	    inviso_tool_lib:debug(remove_all_local_logs,Dbg,[{error,Reason}]),
	    true
    end.

%% Help function which puts together a list of {Node,Tracerdata} tuples. Note that
%% we must build one tuple for each tracerdata for one node.
build_logspeclist_remove_logs(Nodes,TRDstorage) ->
    [{Node,TracerData}||Node<-Nodes,TracerData<-find_tracerdata_for_node_trd(Node,TRDstorage)].
%% ------------------------------------------------------------------------------

%% Help function which traverses a resultlist from an inviso function. Such are
%% built up as [{Node,SubResults},...] where SubResult is a list of tuples for each
%% file-type (e.g trace_log) {FType,FileList} where a FileList is either {error,Reason}
%% or {ok,FileName}.
%% Returns a list of {Node,[{error,Reason},...]}.
look_for_errors_resultlist([{Node,{error,Reason}}|Rest]) ->
    [{Node,{error,Reason}}|look_for_errors_resultlist(Rest)];
look_for_errors_resultlist([{Node,{ok,NResults}}|Rest]) when list(NResults) ->
    case look_for_errors_resultlist_2(NResults,[]) of
	[] ->
	    look_for_errors_resultlist(Rest);
	Errors ->                            % A list of lists.
	    [{Node,lists:flatten(Errors)}|look_for_errors_resultlist(Rest)]
    end;
look_for_errors_resultlist([_|Rest]) ->
    look_for_errors_resultlist(Rest);
look_for_errors_resultlist([]) ->
    [].

look_for_errors_resultlist_2([{_FType,NSubResult}|Rest],Accum) ->
    case lists:filter(fun({error,_Reason})->true;(_)->false end,NSubResult) of
	[] ->                                % No errors for this node.
	    look_for_errors_resultlist_2(Rest,Accum);
	Errors ->                            % A list of at least one error.
	    look_for_errors_resultlist_2(Rest,[Errors|Accum])
    end;
look_for_errors_resultlist_2([],Accum) ->
    Accum.
%% ------------------------------------------------------------------------------


%% ------------------------------------------------------------------------------
%% Functions working on the loopdata structure.
%% Its main purpose is to store information about runtime components participating
%% in the session and their current status.
%% ------------------------------------------------------------------------------

-record(ld,{parent,
	    ctrlnode,
	    ctrlpid,                         % To where to send inviso cmd.
	    rtstates,
	    tracerdata,
	    safetycatches,
	    dbg,
	    actstorage                       % Activity storage, for reactivate.
	   }).

%% Function creating the initial datastructure.
%% The datastructure is [{Node,State},...].
%%
%% The tracerdata table is a bag simply for the reason that if we try to insert
%% the same tracerdata for a node twice, we will end up with one tracerdata after
%% all. This is useful when we insert tracerdata ourselves, the tracerdata will
%% come as a state-change too.
mk_ld(Parent,CtrlNode,CtrlPid,RTStates,NodeParams,OtherNodes,SafetyCatches,Dbg) ->
    TRDtableName=list_to_atom("inviso_tool_sh_trdstorage_"++pid_to_list(self())),
    TRDtid=ets:new(TRDtableName,[bag]),
    ACTtableName=list_to_atom("inviso_tool_sh_actstorage_"++pid_to_list(self())),
    ACTtid=ets:new(ACTtableName,[bag]),
    mk_ld_fill_tracerdata(CtrlNode,TRDtid,NodeParams,OtherNodes), % Fill the ETS table.
    #ld{parent=Parent,                       % The tool main process.
	ctrlnode=CtrlNode,                   % Node name where the control component is.
	ctrlpid=CtrlPid,                     % The process id of the control component.
	rtstates=RTStates,                   % All nodes and their state/status.
	tracerdata=TRDtid,
	safetycatches=SafetyCatches,
	dbg=Dbg,
	actstorage=ACTtid
       }.

%% Help function which inserts tracer data for the nodes. Note that we can get
%% tracer data either from the return value from init_tracing or by asking the
%% node for it. The latter is necessary for the nodes which were marked not to
%% be initiated by the session handler. This maybe because those nodes have
%% autostarted.
mk_ld_fill_tracerdata(CtrlNode,TId,NodeParams,OtherNodes) ->
    mk_ld_fill_tracerdata_nodeparams(TId,NodeParams),
    mk_ld_fill_tracerdata_othernodes(CtrlNode,TId,OtherNodes).

mk_ld_fill_tracerdata_nodeparams(TId,[{Node,TracerData}|Rest]) ->
    ets:insert(TId,{Node,TracerData}),
    mk_ld_fill_tracerdata_nodeparams(TId,Rest);
mk_ld_fill_tracerdata_nodeparams(_,[]) ->
    ok.

mk_ld_fill_tracerdata_othernodes(_,_,[]) ->  % Then not necessary to do anything.
    ok;
mk_ld_fill_tracerdata_othernodes(void,TId,[Node]) -> % The non-distributed case.
    case inviso:get_tracerdata() of
	{error,_Reason} ->                   % Perhaps in state new or disconnected.
	    ok;                              % Do nothing.
	{ok,TracerData} ->
	    ets:insert(TId,{Node,TracerData})
    end;
mk_ld_fill_tracerdata_othernodes(CtrlNode,TId,Nodes) ->
    case inviso_tool_lib:invisomd(CtrlNode,get_tracerdata,[Nodes]) of
	{ok,Results} ->
	    mk_ld_fill_tracerdata_othernodes_2(TId,Results);
	{error,_Reason} ->                   % Strange, we will probably crash later.
	    ok
    end.

mk_ld_fill_tracerdata_othernodes_2(TId,[{_Node,{ok,no_tracerdata}}|Rest]) ->
    mk_ld_fill_tracerdata_othernodes_2(TId,Rest); % It was not initiated then!
mk_ld_fill_tracerdata_othernodes_2(TId,[{Node,{ok,TracerData}}|Rest]) ->
    ets:insert(TId,{Node,TracerData}),
    mk_ld_fill_tracerdata_othernodes_2(TId,Rest);
mk_ld_fill_tracerdata_othernodes_2(_,[]) ->
    ok.
%% ------------------------------------------------------------------------------

get_ctrlnode_ld(#ld{ctrlnode=CtrlNode}) ->
    CtrlNode.
%% ------------------------------------------------------------------------------


get_ctrlpid_ld(#ld{ctrlpid=CtrlPid}) ->
    CtrlPid.
%% ------------------------------------------------------------------------------

get_rtstates_ld(#ld{rtstates=RTStates}) ->
    RTStates.

put_rtstates_ld(NewRTStates,LD) ->
    LD#ld{rtstates=NewRTStates}.
%% ------------------------------------------------------------------------------

get_trdstorage_ld(#ld{tracerdata=TId}) ->
    TId.

put_trdstorage_ld(_NewTId,LD) ->
    LD.
%% ------------------------------------------------------------------------------

%% Help function which adds the current tracerdata of node Node to the tracerdata
%% storage. We only want to add tracerdata we have not seen before. We therefore
%% avoid adding it if the node already is in state ?TRACING.
%% Returns a new tracerdata (what ever it is)!
add_current_tracerdata_ld(CtrlNode,Node,RTStates,TId) ->
    case get_statestatus_rtstates(Node,RTStates) of
	{ok,{?TRACING,_}} ->                 % Then we have already added the tracerdata.
	    TId;                             % Then do nothing.
	{ok,_} ->                            % Since we were not tracing before.
	    case add_current_tracerdata_ld_fetchtracerdata(CtrlNode,Node) of
		{ok,TracerData} ->
		    ets:insert(TId,{Node,TracerData});
		no_tracerdata ->             % Strange, how could we become tracing
		    ok;
		{error,_Reason} ->           % The node perhaps disconnected!?
		    ok
	    end;
	false ->                             % Very strange, not our node!
	    ok                               % Do nothing.
    end.

add_current_tracerdata_ld_fetchtracerdata(void,_Node) ->
    case inviso:get_tracerdata() of
	{ok,TracerData} ->
	    {ok,TracerData};
	{error,no_tracerdata} ->
	    no_tracerdata;
	{error,Reason} ->
	    {error,Reason}
    end;
add_current_tracerdata_ld_fetchtracerdata(CtrlNode,Node) ->
    case inviso_tool_lib:inviso_cmd(CtrlNode,get_tracerdata,[[Node]]) of
	{ok,[{Node,{ok,TracerData}}]} ->
	    {ok,TracerData};
	{ok,[{Node,{error,no_tracerdata}}]} ->
	    no_tracerdata;
	{ok,[{Node,{error,Reason}}]} ->
	    {error,Reason};
	{error,Reason} ->
	    {error,Reason}
    end.
%% ------------------------------------------------------------------------------


get_safetycatches_ld(#ld{safetycatches=SCs}) ->
    SCs.
%% ------------------------------------------------------------------------------

get_dbg_ld(#ld{dbg=Dbg}) ->
    Dbg.
%% ------------------------------------------------------------------------------

get_actstorage_ld(#ld{actstorage=ACTstorage}) ->
    ACTstorage.

put_actstorage_ld(_NewACTstorage,LD) ->
    LD.
%% ------------------------------------------------------------------------------



%% ------------------------------------------------------------------------------
%% Functions working on the rtstates structure (which is a substructure of loopdata).
%% It is either:
%% [{Node,StateStatus,Opts},...]
%% Node is either the node name of the runtime component erlang node or
%% ?LOCAL_RUNTIME as returned from the trace control component.
%% StateStatus is {State,Status}, 'unavailable' or 'unknown'.
%% Status is the returnvalue from trace control component.
%%   i.e: running | {suspended,Reason}
%% ------------------------------------------------------------------------------

%% Function contructing an rtstates structure from a list of [{Node,StateStatus,Opts},...].
to_rtstates(ListOfStates) when list(ListOfStates) ->
    ListOfStates.
%% ------------------------------------------------------------------------------

%% Function which takes a rtstates structure and returns a list of [{Node,StateStatus},...].
from_rtstates(RTStates) ->
    RTStates.
%% ------------------------------------------------------------------------------

%% Function which takes an rtstates structure and a result as returned from
%% init_tracing. The RTStates is modified for the nodes that changed state as a
%% result of successful init_tracing.
%% Returns a new RTStates.
set_tracing_rtstates([E={Node,_StateStatus,Opts}|Rest],Result) ->
    case lists:keysearch(Node,1,Result) of
	{value,{_,ok}} ->                    % Means state-change to tracing!
	    [{Node,{tracing,running},Opts}|set_tracing_rtstates(Rest,Result)];
	_ ->                                 % Otherwise, leave it as is.
	    [E|set_tracing_rtstates(Rest,Result)]
    end;
set_tracing_rtstates([],_Result) ->
    [].
%% ------------------------------------------------------------------------------

%% Function updating the state/status for a certain runtime component.
%% Returns a new RTStates structure. Note that Node must not necessarily be one
%% of the nodes in the session. Meaning that Node shall not be added to RTStates
%% should it not already be in there.
statechange_rtstates(Node,State,Status,RTStates) when list(RTStates) ->
    case lists:keysearch(Node,1,RTStates) of
	{value,{_,_,Opts}} ->
	    lists:keyreplace(Node,1,RTStates,{Node,{State,Status},Opts});
	_ ->                                 % Then Node does not exist.
	    RTStates                         % Just keep it as is, as keyreplace would have done.
    end.
%% ------------------------------------------------------------------------------

%% Function updating the state/status for a certain runtime component. The
%% state/status is set to 'unavailable'.
%% Returns a new RTStates structure.
set_unavailable_rtstates(Node,RTStates) when list(RTStates) ->
    case lists:keysearch(Node,1,RTStates) of
	{value,{_,_,Opts}} ->
	    lists:keyreplace(Node,1,RTStates,{Node,unavailable,Opts});
	_ ->                                 % Then Node does not exist.
	    RTStates                         % Just keep it as is, as keyreplace would have done.
    end.
%% ------------------------------------------------------------------------------

%% Function finding the statestatus associated with Node in the RTStates structure.
%% Returns {ok,StateStatus} or 'false'.
get_statestatus_rtstates(Node,RTStates) ->
    case lists:keysearch(Node,1,RTStates) of
	{value,{_,StateStatus,_}} ->
	    {ok,StateStatus};
	false ->
	    false
    end.
%% ------------------------------------------------------------------------------

%% Help function which returns a list of all nodes that are currently marked
%% as available to us in the runtime state structure.	      
get_all_available_nodes_rtstates(RTStates) ->
    get_all_session_nodes_rtstates(lists:filter(fun({_N,unavailable,_})->false;
						   (_)->true
						end,
						RTStates)).
%% ------------------------------------------------------------------------------

%% Help function returning a list of all nodes belonging to this session.
get_all_session_nodes_rtstates(RTStates) ->
    lists:map(fun({Node,_,_})->Node end,RTStates).
%% ------------------------------------------------------------------------------

%% Function which returns a list of nodes that are indicated as tracing in the
%% RTStates structure.
get_all_tracing_nodes_rtstates(RTStates) ->
    lists:map(fun({N,_,_})->N end,
	      lists:filter(fun({_,{tracing,_},_})->true;(_)->false end,RTStates)).
%% ------------------------------------------------------------------------------

%% Returns the options associated with Node in the RTStates structure.
get_opts_rtstates(Node,RTStates) ->
    case lists:keysearch(Node,1,RTStates) of
	{value,{_,_,Opts}} ->
	    {ok,Opts};
	false ->
	    false
    end.

%% ------------------------------------------------------------------------------
%% Functions working on the tracerdata structure, which is a part of the loopdata.
%% The tracerdata structure is an ETS-table of type bag storing:
%% {Node,TracerData}.
%% Note that there can of course be multiple entries for a node.
%% ------------------------------------------------------------------------------

%% Help function which takes a tracerdata loopdata structure and returns a list
%% of all stored tracerdata for a certain Node.
find_tracerdata_for_node_trd(Node,TRD) ->
    case ets:lookup(TRD,Node) of
	Result when list(Result) ->
	    lists:map(fun({_Node,TracerData})->TracerData end,Result);
	_ ->                                 % Should probably never happend.
	    []
    end.
%% ------------------------------------------------------------------------------
			 

%% ------------------------------------------------------------------------------
%% Functions working on the activity storage structure, which is part of the
%% loopdata. It stores entries about things that needs to be "redone" in case
%% of a reactivation of the node. The time order is also important.
%% Note that for every ActivityType there must be a "handler" in the reactivation
%% functionality.
%%
%% The structure is a bag of {Node,ActivityType,What}.
%%   ActivityType/What=tf/{Op,TraceConfList}|tpm/{Op,[Mod,Func,Arity,MS,CallFunc]}
%%                                              /{Op,[Mod,Func,Arity,MS,CallFunc,ReturnFunc]}
%%                                              /{Op,[]}
%%     TraceConfList=[{Proc,Flags},...]
%%     How=true|false
%% ------------------------------------------------------------------------------

%% Function that adds meta-pattern activities to the activity storage. Note
%% that one of the parameters to the function is a return value from an
%% inviso call. In that way we do not enter activities that were unsuccessful.
%% Op can be either the setting or clearing of a meta pattern.
%% Returns a new ACTstorage.
add_tpm_actstorage([{Node,ok}|Rest],Op,InvisoCmdParams,ACTstorage) ->
    true=ets:insert(ACTstorage,{Node,tpm,{Op,InvisoCmdParams}}),
    add_tpm_actstorage(Rest,Op,InvisoCmdParams,ACTstorage);
add_tpm_actstorage([_|Rest],Op,InvisoCmdParams,ACTstorage) ->
    add_tpm_actstorage(Rest,Op,InvisoCmdParams,ACTstorage);
add_tpm_actstorage([],_,_,ACTstorage) ->
    ACTstorage.

%% Function that adds process trace-flags to the activity storage. Note that one
%% of the parameters is the return value from an inviso function. Meaning that
%% if the flags failed in their entirety, no activity will be saved. If only
%% some of the flags failed, we will not go through the effort of trying to find
%% out exactly which.
%% Returns a new activity storage structure.
add_tf_actstorage([{_Node,{error,_Reason}}|Rest],Op,TraceConfList,ACTstorage) ->
    add_tf_actstorage(Rest,Op,TraceConfList,ACTstorage);
add_tf_actstorage([{Node,_Result}|Rest],Op,TraceConfList,ACTstorage) ->
    true=ets:insert(ACTstorage,{Node,tf,{Op,TraceConfList}}),
    add_tf_actstorage(Rest,Op,TraceConfList,ACTstorage);
add_tf_actstorage([],_,_,ACTstorage) ->
    ACTstorage.
%% ------------------------------------------------------------------------------

%% Finds all activities associated with Node. Returns a list of them in the
%% same order as they were inserted.
get_activities_actstorage(Node,ACTstorage) ->
    case ets:lookup(ACTstorage,Node) of
	[] ->
	    false;
	Result when list(Result) ->
	    {ok,lists:map(fun({_N,Type,What})->{Type,What} end,Result)}
    end.
%% ------------------------------------------------------------------------------

%% Function removing all activity entries associated with Node. This is useful
%% if the Node disconnects for instance.
del_node_actstorage(Node,ACTstorage) ->
    ets:delete(ACTstorage,Node),
    ACTstorage.
%% ------------------------------------------------------------------------------


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
%% Description: The controlling part of the trace tool Inviso
%%
%% This code implements the inviso control component meant to be run on an Erlang
%% node doing any tracing. It can also be used in a non distributed system where
%% the control component and the runtime component will run on the same virtual
%% machine.
%% The control component is not meant to be started by a supervisor but rather
%% directly by the user when needed.
%% This module does not provide any APIs to users. Those are found in inviso.erl.
%% This module merly has the gen_server call-backs.
%% ------------------------------------------------------------------------------

-module(inviso_c).
-behavior(gen_server).


%% ------------------------------------------------------------------------------
%% gen_server callbacks.
%% ------------------------------------------------------------------------------
-export([init/1,
	 handle_call/3,handle_cast/2,handle_info/2, 
	 terminate/2,
	 code_change/3]).
%% ------------------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% Exported internal functions (used in spawn of help process).
%% ------------------------------------------------------------------------------
-export([log_rec_init/4]).

%% ------------------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% Records.
%% ------------------------------------------------------------------------------

%% #state
%% Record used in the loopdata.
-record(state,{
	  nodes=[],                          % [#node,...]
	  distributed,                       % false | true
	  subscribers=[],                    % [{pid(),monitor_ref()},...]
	  rt_options=[{dependency,{infinity,node()}},{overload, default}]
	 }).
%% ------------------------------------------------------------------------------

%% #node
%% Record storing information about a runtime component connected to this control
%% component.
-record(node,{
	  node,                              % [atom(),...]
	  pid,                               % pid()
	  vsn,
	  ref,                               % monitor_ref()
	  tag                                % term()
	 }).
%% ------------------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% Macros used in this module.
%% ------------------------------------------------------------------------------

-define(RUNTIME,inviso_rt).                  % The module API name of the runtime.
%% ------------------------------------------------------------------------------



%% ==============================================================================
%% Controller component implmentation.
%% ==============================================================================

init({_Parent,Options}) ->
    process_flag(trap_exit,true),
    case check_options(Options,start) of
	{ok,Options2} ->
	    LoopData=initiate_state(Options2),
	    {ok,LoopData};
	Error ->
	    {stop,Error}
    end.
%% ------------------------------------------------------------------------------

handle_call({subscribe,Pid},_From,LD) when pid(Pid) ->
    MRef=erlang:monitor(process,Pid),
    {reply,ok,LD#state{subscribers=[{Pid,MRef}|LD#state.subscribers]}};
handle_call({subscribe,Faulty},_From,LD) ->
    {reply,{error,{badarg,Faulty}},LD};
handle_call({unsubscribe,Pid},_From,LD) ->
    case lists:keysearch(Pid,1,LD#state.subscribers) of
	{value,{_,MRef}} ->
	    erlang:demonitor(MRef),
	    {reply,ok,LD#state{subscribers=lists:keydelete(Pid,1,LD#state.subscribers)}};
	false ->
	    {reply,ok,LD}
    end;
handle_call({add_nodes,Nodes,Opts,Tag,Condition},_From,LD) ->
    case check_options(Opts,add_node) of
	{ok,Opts2} ->
	    Opts3=merge_options(LD#state.rt_options,Opts2),
	    {NewLD,Reply}=do_add_nodes(Nodes,LD,Opts3,Tag,Condition),
	    {reply,adapt_reply(NewLD,Reply),NewLD};
	Error ->
	    {reply,Error,LD}
    end;
handle_call({change_options,Nodes,Opts},_From,LD) ->
    case check_options(Opts,add_node) of
	{ok,Opts2} ->
	    {reply,adapt_reply(LD,do_change_option(Nodes,Opts2,LD)),LD};
	Error ->
	    {reply,Error,LD}
    end;
handle_call({init_tracing,TracerDataList},_From,LD) ->
    {reply,adapt_reply(LD,do_init_tracing(TracerDataList,LD)),LD};
handle_call({init_tracing,Nodes,TracerData},_From,LD) when list(Nodes) ->
    TracerDataList=
	lists:map(fun(N)->{N,TracerData} end,started_trace_nodes(Nodes,LD)),
    {reply,adapt_reply(LD,do_init_tracing(TracerDataList,LD)),LD};
handle_call({init_tracing,Nodes,_TracerData},_From,LD) ->
    {reply,{error,{badarg,Nodes}},LD};
handle_call({trace_pattern,Nodes,Patterns,FlagList},_From,LD) ->
    {reply,adapt_reply(LD,distribute_tp(Nodes,Patterns,FlagList,LD)),LD};
handle_call({trace_flags,Nodes,Args,How},_From,LD) ->
    {reply,adapt_reply(LD,distribute_tf(Nodes,Args,How,LD)),LD};
handle_call({trace_flags,NodeArgs,How},_From,LD) ->
    {reply,distribute_tf(NodeArgs,How,LD),LD}; % Always distributed here.
handle_call({trace_flags,NodeArgHows},_From,LD) ->
    {reply,distribute_tf(NodeArgHows,LD),LD}; % Always distributed here.
handle_call({meta_pattern,Nodes,Args},_From,LD) ->
    {reply,adapt_reply(LD,distribute_metapattern(Nodes,Args,LD)),LD};
handle_call({ctp_all,Nodes},_From,LD) ->
    {reply,adapt_reply(LD,do_ctp_all(Nodes,LD)),LD};
handle_call({suspend,Reason,Nodes},_From,LD) ->
    {reply,adapt_reply(LD,do_suspend(Nodes,Reason,LD)),LD};
handle_call({cancel_suspension,Nodes},_From,LD) ->
    {reply,adapt_reply(LD,do_cancel_suspension(Nodes,LD)),LD};
handle_call({stop_tracing,Nodes},_From,LD) ->
    {reply,adapt_reply(LD,do_stop_tracing(Nodes,LD)),LD};
handle_call({get_status,Nodes},_From,LD) ->
    {reply,adapt_reply(LD,do_get_status(Nodes,LD)),LD};
handle_call({get_tracerdata,Nodes},_From,LD) ->
    {reply,adapt_reply(LD,do_get_tracerdata(Nodes,LD)),LD};
handle_call(list_logs,_From,LD) ->
    {reply,adapt_reply(LD,do_list_logs(all,LD)),LD};
handle_call({list_logs,TracerDataOrNodesList},_From,LD) ->
    {reply,adapt_reply(LD,do_list_logs(TracerDataOrNodesList,LD)),LD};
handle_call({fetch_log,ToNode,Spec,Dest,Prefix},From,LD) ->
    case LD#state.distributed of
	true ->                              % It is a distributed system.
	    do_fetch_log(ToNode,Spec,Dest,Prefix,From,LD),
	    {noreply,LD};                    % Reply will come from collector pid.
	false ->                             % Stupidity! you dont want this!
	    {reply,{error,not_distributed},LD}
    end;
handle_call({delete_log,NodesOrNodeSpecs},_From,LD) ->
    {reply,adapt_reply(LD,do_delete_log(NodesOrNodeSpecs,LD)),LD};
handle_call({delete_log,Nodes,Specs},_From,LD) when list(Nodes) ->
    Reply=do_delete_log(lists:map(fun(N)->{N,Specs} end,Nodes),LD),
    {reply,adapt_reply(LD,Reply),LD};
handle_call({delete_log,FaultyNodes,_Specs},_From,LD) ->
    {reply,{error,{badarg,FaultyNodes}},LD};
handle_call({clear,Nodes,Options},_From,LD) ->
    {reply,adapt_reply(LD,do_clear(Nodes,LD,Options)),LD};
handle_call({flush,Nodes},_From,LD) ->
    {reply,adapt_reply(LD,do_flush(Nodes,LD)),LD};
handle_call({stop_nodes,Nodes},_From,LD) ->
    {NewLD,Reply}=do_stop_nodes(Nodes,LD),
    {reply,adapt_reply(NewLD,Reply),NewLD};
handle_call(stop,_From,LD) ->
    {stop,normal,shutdown,LD};
handle_call(stop_all,_From,LD) ->
    {NewLD,Reply}=do_stop_nodes(started_trace_nodes(all,LD),LD),
    {stop,normal,adapt_reply(NewLD,Reply),NewLD};
handle_call(Request,_From,LD) -> %% for debug purpose only
    {reply,{error,{invalid_request,Request}},LD}.

handle_cast(_Request,LD) ->                  % There are no casts.
    {noreply,LD}.

handle_info({connect,_Node,Pid,_VSN,Tag},LD) -> % From connecting runtime.
    {noreply,do_confirm_connection(Pid,Tag,LD)};
handle_info({trace_event,Event},LD) ->       % A runtime component issues an event.
    send_to_subscribers(Event,LD),           % Relay to our subscribers.
    {noreply,LD};
handle_info({'DOWN',Ref,process,_From,Info},LD) -> % A runtime component died?
    {noreply,do_down_msg(Ref,Info,LD)};
handle_info(state,LD) ->                     % For debug purposes.
    io:format("trace_c state: ~p~n",[LD]),
    {noreply,LD};
handle_info(_Msg,LD) ->
    {noreply,LD}.

terminate(_Reason, _LD) ->
    ok.

code_change(_OldVsn, LoopData, _Extra) ->
    {ok, LoopData}.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Handle call help functions.
%% -----------------------------------------------------------------------------

%% Help function which adapts a reply based on if this is a distributed system
%% or not. The first argument indicates distribution (='true').
%% If we are not running a distributed system, all node references are removed.
adapt_reply(#state{distributed=Distributed},Reply) ->
    adapt_reply(Distributed,Reply);
adapt_reply(true,Reply) ->                   % We are distributed.
    Reply;
adapt_reply(false,{ok,[{_Node,LocalReply}]}) ->
    LocalReply;
adapt_reply(false,{ok,[]}) ->
    {error,not_an_added_node};               % ÄR DET HÄR VERKLIGEN RIKTIGT?
adapt_reply(false,Reply) ->
    Reply.
%% ------------------------------------------------------------------------------


%% ==============================================================================
%% First level help functions to handler functions.
%% ==============================================================================

%% Function starting a runtime component at the nodes in Nodes. If doing non
%% distributed, Nodes will be a list of nonode@nohost.
%% Returns {NewLD,{ok,Replies}} or {LD,{error,Reason}}.
do_add_nodes(Nodes,LD,Options,Tag,Condition) ->
    do_add_nodes_2(Nodes,LD,Options,Tag,Condition,[]).

do_add_nodes_2([Node|Tail],LD,Options,Tag,Condition,Replies) ->
    case find(fun is_started/2,LD#state.nodes,Node) of
	{value,true} ->                      % Already started by us.
	    do_add_nodes_2(Tail,LD,Options,Tag,Condition,[{Node,{ok,already_added}}|Replies]);
	no_match ->
	    case ?RUNTIME:start(Node,Options,Tag,Condition) of
		{node_info,_Node,Pid,VSN,State,Status,new} ->
		    NewLD=do_add_nodes_3(Node,LD,Tag,Pid,VSN,State,Status),
		    do_add_nodes_2(Tail,NewLD,Options,Tag,Condition,[{Node,{ok,new}}|Replies]);
		{node_info,_Node,Pid,VSN,State,Status,{tag,Tag2}} ->
		    NewLD=do_add_nodes_3(Node,LD,Tag,Pid,VSN,State,Status),
		    do_add_nodes_2(Tail,NewLD,Options,Tag,Condition,
				   [{Node,{ok,{adopted,State,Status,Tag2}}}|Replies]);
		Error ->
		    do_add_nodes_2(Tail,LD,Options,Tag,Condition,[{Node,Error}|Replies])
	    end
    end;
do_add_nodes_2([],LD,_,_,_,Replies) ->
    {LD,{ok,lists:reverse(Replies)}};
do_add_nodes_2(Faulty,LD,_,_,_,_) ->         % Not a list of nodes.
    {LD,{error,{badarg,Faulty}}}.

do_add_nodes_3(Node,LD,Tag,Pid,VSN,State,Status) ->
    MRef=erlang:monitor(process,Pid),
    NodeRec=#node{node=Node,pid=Pid,vsn=VSN,ref=MRef,tag=Tag},
    send_to_subscribers({connected,Node,{Tag,{State,Status}}},LD),
    _NewLD=set_node_rec(NodeRec,LD).
%% ------------------------------------------------------------------------------

%% Function calling change_options sequensially on all nodes in Nodes.
%% Returns {ok,Replies} or {error,Reason}.
do_change_option(Nodes,Options,LD) ->
    do_change_option_2(started_trace_nodes(Nodes,LD#state.nodes),Options,LD,[]).

do_change_option_2([Node|Tail],Options,LD,Replies) ->
    case get_node_rec(Node,LD) of
	Rec when record(Rec,node) ->
	    Answer=?RUNTIME:change_options(Rec#node.pid,Options),
	    do_change_option_2(Tail,Options,LD,[{Node,Answer}|Replies]);
	Error ->
	    do_change_option_2(Tail,Options,LD,[{Node,Error}|Replies])
    end;
do_change_option_2([],_Options,_LD,Replies) ->
    {ok,Replies};
do_change_option_2(Faulty,_,_,_) ->
    {error,{badarg,Faulty}}.
%% ------------------------------------------------------------------------------

%% Function which calls the runtime components in TracerDataList and initiates
%% the tracing. TracerDataList may either be just one tracer data which shall be
%% applied to all runtime components, or a list of nodes and tracerdata.
do_init_tracing(TracerDataList,LD) ->
    case inviso_rt_lib:is_tracerdata(TracerDataList) of
	true ->                              % Then we must add all our nodes.
	    List=lists:map(fun(N)->{N,TracerDataList} end,started_trace_nodes(all,LD)),
	    do_init_tracing_2(List,LD,[]);
	false ->                             % Assume it is a list of {Node,Tracerdata}
	    do_init_tracing_2(TracerDataList,LD,[])
    end.

do_init_tracing_2([{Node,TracerData}|Tail],LD,Replies) ->
    case get_node_rec(Node,LD) of
	Rec when is_record(Rec,node) ->
	    case check_modify_tracerdata(TracerData,LD) of
		{ok,NewTracerData} ->        % Tracerdata ok and node->pid.
		    Reply=?RUNTIME:init_tracing(Rec#node.pid,NewTracerData),
		    do_init_tracing_2(Tail,LD,[{Node,Reply}|Replies]);
		{error,Reason} ->            % Unknown tracerdata.
		    do_init_tracing_2(Tail,LD,[{Node,{error,Reason}}|Replies])
	    end;
	{error,Reason} ->
	    do_init_tracing_2(Tail,LD,[{Node,{error,Reason}}|Replies])
    end;
do_init_tracing_2([_|Tail],LD,Replies) ->    % Just ignore item we don't understand.
    do_init_tracing_2(Tail,LD,Replies);      % Will not end up in Replies either.
do_init_tracing_2([],_LD,Replies) ->
    {ok,lists:reverse(Replies)};
do_init_tracing_2(What,_LD,_) ->
    {error,{badarg,What}}.
%% -----------------------------------------------------------------------------

%% Function setting trace patterns on all nodes mentioned in Nodes. Uses a
%% parallel mechanism in the runtime component.
%% Returns {ok,Reply} or {error,Reason}.
distribute_tp(all,Patterns,FlagList,LD) ->
    distribute_tp(started_trace_nodes(all,LD),Patterns,FlagList,LD);
distribute_tp(Nodes,Patterns,FlagList,LD) when list(Nodes) ->
    RTpids=lists:map(fun(N)->case get_node_rec(N,LD) of
				 #node{pid=Pid} ->
				     {Pid,N};
				 Error ->
				     {Error,N}
			     end
		     end,
		     Nodes),
    {ok,?RUNTIME:trace_patterns_parallel(RTpids,Patterns,FlagList)};
distribute_tp(Faulty,_,_,_) ->
    {error,{badarg,Faulty}}.
%% ------------------------------------------------------------------------------

%% Function which in parallel sets trace flags on all nodes in Nodes. Can be
%% either a list of node names or 'all'. Note that in the reply list there will be an
%% error indicating nodes not reachable. Either because such a node disappeared or
%% because it is not one of "our" nodes.
%% Returns {ok,Reply} or {error,Reason}.
distribute_tf(all,Args,How,LD) ->
    distribute_tf(started_trace_nodes(all,LD),Args,How,LD);
distribute_tf(Nodes,Args,How,LD) when list(Nodes) ->
    RTpids=lists:map(fun(Node)->
			     case get_node_rec(Node,LD) of
				 #node{pid=Pid} ->
				     {Pid,Node};
				 Error ->       % Not an added node then!
				     {Error,Node}
			     end
		     end,
		     Nodes),
    {ok,?RUNTIME:trace_flags_parallel(RTpids,Args,How)};
distribute_tf(Faulty,_,_,_) ->
    {error,{badarg,Faulty}}.

%% As above but specific args for each node.
distribute_tf(NodeArgs,How,LD) when list(NodeArgs) ->
    RTpidArgs=lists:map(fun({Node,Args})->
				case get_node_rec(Node,LD) of
				    #node{pid=Pid} ->
					{Pid,Node,Args};
				    Error -> % Not an added node then!
					{Error,Node}
				end
			end,
			NodeArgs),
    {ok,?RUNTIME:trace_flags_parallel(RTpidArgs,How)};
distribute_tf(Faulty,_,_) ->
    {error,{badarg,Faulty}}.

%% As above but both specific args for each node and How (set or remove flag).
distribute_tf(NodeArgHows,LD) when list(NodeArgHows) ->
    RTpidArgHows=
	lists:map(fun({Node,Args,How}) ->
			  case get_node_rec(Node,LD) of
			      #node{pid=Pid} ->
				  {Pid,Node,Args,How};
			      Error ->       % Not an added node then!
				  {Error,Node}
			  end
		  end,
		  NodeArgHows),
    {ok,?RUNTIME:trace_flags_parallel(RTpidArgHows)};
distribute_tf(Faulty,_) ->
    {error,{badarg,Faulty}}.
%% ------------------------------------------------------------------------------

%% Function making a parallel call to all nodes in Nodes, calling the generic
%% meta-tracer function stated in Args.
%% Returns {ok,Reply} or {error,Reason}.
distribute_metapattern(all,Args,LD) ->
    distribute_metapattern(started_trace_nodes(all,LD),Args,LD);
distribute_metapattern(Nodes,Args,LD) when list(Nodes) ->
    RTpids=lists:map(fun(N)->case get_node_rec(N,LD) of
				 #node{pid=Pid} ->
				     {Pid,N};
				 Error ->
				     {Error,N}
			     end
		     end,
		     Nodes),
    {ok,?RUNTIME:meta_tracer_call_parallel(RTpids,Args)};
distribute_metapattern(Faulty,_,_) ->
    {error,{badarg,Faulty}}.
%% ------------------------------------------------------------------------------

%% Function clearing all trace patterns on all node mentioned in Nodes.
%% Returns {ok,Reply} or {error,Reason}.
do_ctp_all(Nodes,LD) ->
    do_ctp_all_2(started_trace_nodes(Nodes,LD),LD,[]).

do_ctp_all_2([Node|Tail],LD,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    do_ctp_all_2(Tail,LD,[{Node,?RUNTIME:clear_all_tp(Pid)}|Replies]);
	Error ->
	    do_ctp_all_2(Tail,LD,[{Node,Error}|Replies])
    end;
do_ctp_all_2([],_LD,Replies) ->
    {ok,lists:reverse(Replies)};
do_ctp_all_2(Faulty,_,_) ->
    {error,{badarg,Faulty}}.
%% ------------------------------------------------------------------------------

%% Function suspending all runtime components mentioned in Nodes.
%% Returns {ok,Reply} or {error,Reason}.
do_suspend(Nodes,Reason,LD) ->
    do_suspend_2(started_trace_nodes(Nodes,LD),Reason,LD,[]).

do_suspend_2([Node|Tail],Reason,LD,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    Answer=?RUNTIME:call_suspend(Pid,Reason),
	    do_suspend_2(Tail,Reason,LD,[{Node,Answer}|Replies]);
	Error ->
	    do_suspend_2(Tail,Reason,LD,[{Node,Error}|Replies])
    end;
do_suspend_2([],_Reason,_LD,Replies) ->
    {ok,lists:reverse(Replies)};
do_suspend_2(Faulty,_,_,_) ->
    {error,{badarg,Faulty}}.
%% ------------------------------------------------------------------------------

%% Function cancelling a suspension at the runtime components mentioned in Nodes.
%% Returns {ok,Reply} or {error,Reason}.
do_cancel_suspension(Nodes,LD) ->
    do_cancel_suspension_2(started_trace_nodes(Nodes,LD),LD,[]).

do_cancel_suspension_2([Node|Tail],LD,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    Answer=?RUNTIME:cancel_suspension(Pid),
	    do_cancel_suspension_2(Tail,LD,[{Node,Answer}|Replies]);
	Error ->
	    do_cancel_suspension_2(Tail,LD,[{Node,Error}|Replies])
    end;
do_cancel_suspension_2([],_LD,Replies) ->
    {ok,lists:reverse(Replies)};
do_cancel_suspension_2(Faulty,_,_) ->
    {error,{badarg,Faulty}}.
%% ------------------------------------------------------------------------------

%% Function which stops tracing on all nodes in Nodes. The function is performed
%% in parallel over the nodes to get a more precise stop-time.
%% Return {ok,Reply} or {error,Reason}.
do_stop_tracing(all,LD) ->
    do_stop_tracing(started_trace_nodes(all,LD),LD);
do_stop_tracing(Nodes,LD) when list(Nodes) ->
    RTpids=lists:map(fun(N)->case get_node_rec(N,LD) of
				 #node{pid=Pid} ->
				     {Pid,N};
				 Error ->
				     {Error,N}
			     end
		     end,
		     Nodes),
    {ok,?RUNTIME:stop_tracing_parallel(RTpids)};
do_stop_tracing(Faulty,_) ->
    {error,{badarg,Faulty}}.
%% -----------------------------------------------------------------------------

%% Function fetching the current status of the runtime components in Nodes.
%% Returns {ok,Reply} or {error,Reason}.
do_get_status(Nodes,LD) ->
    do_get_status_2(started_trace_nodes(Nodes,LD),LD,[]).

do_get_status_2([Node|Tail],LD,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    do_get_status_2(Tail,LD,[{Node,?RUNTIME:get_status(Pid)}|Replies]);
	Error ->
	    do_get_status_2(Tail,LD,[{Node,Error}|Replies])
    end;
do_get_status_2([],_LD,Replies) ->
    {ok,lists:reverse(Replies)};
do_get_status_2(Faulty,_,_) ->
    {error,{badarg,Faulty}}.
%% ------------------------------------------------------------------------------

%% Function retrieving the tracerdata for the nodes in Nodes.
%% Returns {ok,Reply} or {error,Reason}.
do_get_tracerdata(Nodes,LD) ->
    do_get_tracerdata_2(started_trace_nodes(Nodes,LD),LD,[]).

do_get_tracerdata_2([Node|Tail],LD,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    do_get_tracerdata_2(Tail,LD,[{Node,?RUNTIME:get_tracerdata(Pid)}|Replies]);
	Error ->
	    do_get_tracerdata_2(Tail,LD,[{Node,Error}|Replies])
    end;
do_get_tracerdata_2([],_LD,Replies) ->
    {ok,lists:reverse(Replies)};
do_get_tracerdata_2(Faulty,_,_) ->
    {error,{badarg,Faulty}}.
%% ------------------------------------------------------------------------------

%% Function that lists all logfiles associated with a certain tracerdata
%% or the current tracerdata should no tracerdata be mentioned.
%% Returns {ok,Replies} or {error,Reason}.
do_list_logs(all,LD) ->                     % When doing all known nodes.
    do_list_logs_2(started_trace_nodes(all,LD),LD,[]);
do_list_logs(TracerDataOrNodesList,LD) ->
    case inviso_rt_lib:is_tracerdata(TracerDataOrNodesList) of
	true ->                             % It is tracerdata for this node.
	    do_list_logs_2([{node(),TracerDataOrNodesList}],LD,[]);
	false ->
	    do_list_logs_2(TracerDataOrNodesList,LD,[])
    end.

do_list_logs_2([{Node,TD}|Tail],LD,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    case check_modify_tracerdata(TD,LD) of
		{ok,TracerData} ->
		    Answer=?RUNTIME:list_logs(Pid,TracerData),
		    do_list_logs_2(Tail,LD,[{Node,Answer}|Replies]);
		Error ->
		    do_list_logs_2(Tail,LD,[{Node,Error}|Replies])
	    end;
	Error ->
	    do_list_logs_2(Tail,LD,[{Node,Error}|Replies])
    end;
do_list_logs_2([Node|Tail],LD,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    Answer=?RUNTIME:list_logs(Pid),
	    do_list_logs_2(Tail,LD,[{Node,Answer}|Replies]);
	Error ->
	    do_list_logs_2(Tail,LD,[{Node,Error}|Replies])
    end;
do_list_logs_2([],_LD,Replies) ->
    {ok,lists:reverse(Replies)};
do_list_logs_2(Other,_LD,_Replies) ->
    {error,{badarg,Other}}.
%% -----------------------------------------------------------------------------

%% Function fetching logfiles using distributed erlang. This function does not
%% return anything significant. This since the reply to the client is always
%% sent by the CollectPid unless there is badarg fault detected before the
%% CollectPid is spawned. Note that this function sends a list of fetchers from
%% which the CollectPid shall expect replies.
%% We try to catch some bad arguments like Destination and Prefix not being
%% strings. However the fact that they are lists does not guarantee they are
%% proper strings.
do_fetch_log(ToNode,all,Dest,Prefix,From,LD) ->
    do_fetch_log(ToNode,started_trace_nodes(all,LD),Dest,Prefix,From,LD);
do_fetch_log(ToNode,Specs,Dest,Prefix,From,LD) when list(Dest),list(Prefix) ->
    CollectPid=spawn_link(ToNode,?MODULE,log_rec_init,[self(),Dest,Prefix,From]),
    do_fetch_log_2(Specs,LD,CollectPid,[],[]);
do_fetch_log(_ToNode,_Specs,Dest,Prefix,From,_LD) ->
    gen_server:reply(From,{error,{badarg,[Dest,Prefix]}}).

do_fetch_log_2([{Node,Spec}|Rest],LD,CollectPid,Fetchers,Replies) ->
    if
	Node==node() ->                     % This is plain stupid!
	    do_fetch_log_2(Rest,LD,CollectPid,Fetchers,[{Node,{error,own_node}}|Replies]);
	true ->
	    case get_node_rec(Node,LD) of
		#node{pid=Pid} ->
		    {NewFetchers,NewReplies}=
			do_fetch_log_3(Fetchers,
				       Replies,
				       Node,
				       ?RUNTIME:fetch_log(Pid,CollectPid,Spec)),
		    do_fetch_log_2(Rest,LD,CollectPid,NewFetchers,NewReplies);
		Error ->                    % Most likely the node does not exist.
		    do_fetch_log_2(Rest,LD,CollectPid,Fetchers,[{Node,Error}|Replies])
	    end
    end;
do_fetch_log_2([Node|Rest],LD,CollectPid,Fetchers,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    {NewFetchers,NewReplies}=
		do_fetch_log_3(Fetchers,Replies,Node,?RUNTIME:fetch_log(Pid,CollectPid)),
	    do_fetch_log_2(Rest,LD,CollectPid,NewFetchers,NewReplies);
	Error ->                            % Most likely the node does not exist.
	    do_fetch_log_2(Rest,LD,CollectPid,Fetchers,[{Node,Error}|Replies])
    end;
do_fetch_log_2([],_,CollectPid,Fetchers,Replies) ->
    CollectPid ! {?MODULE,self(),Fetchers,Replies};
do_fetch_log_2(FaultySpec,_,CollectPid,_Fetchers,_Replies) ->
    CollectPid ! {?MODULE,self(),[],{error,{badarg,FaultySpec}}}.

do_fetch_log_3(Fetchers,Replies,Node,{ok,Fetcher}) ->
    {[{Node,Fetcher}|Fetchers],Replies};
do_fetch_log_3(Fetchers,Replies,Node,{complete,no_log}) ->
    {Fetchers,[{Node,{complete,no_log}}|Replies]};
do_fetch_log_3(Fetchers,Replies,Node,{error,Reason}) ->
    {Fetchers,[{Node,{error,Reason}}|Replies]}.
%% -----------------------------------------------------------------------------

%% Function removing files from the runtime components. We can either ask for
%% all files associated with the current tracerdata to be removed, or provide
%% tracerdata or a list of files to be removed.
%% Returns the client reply.
do_delete_log(all,LD) ->
    do_delete_log_2(started_trace_nodes(all,LD),LD,[]);
do_delete_log(NodeSpecs,LD) ->
    case inviso_rt_lib:is_tracerdata(NodeSpecs) of
	true ->                             % It is tracerdata for all nodes.
	    do_delete_log_2(lists:map(fun(N)->{N,NodeSpecs} end,
				      started_trace_nodes(all,LD)),
			    LD,[]);
	false ->
	    if
		list(NodeSpecs),list(hd(NodeSpecs)) -> % A list of files.
		    do_delete_log_2(lists:map(fun(N)->{N,NodeSpecs} end,
					      started_trace_nodes(all,LD)),
				    LD,[]);
		true ->                     % Then use it as is.
		    do_delete_log_2(NodeSpecs,LD,[])
	    end
    end.

do_delete_log_2([{Node,Spec}|Tail],LD,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    do_delete_log_2(Tail,LD,[{Node,?RUNTIME:delete_log(Pid,Spec)}|Replies]);
	Error ->
	    do_delete_log_2(Tail,LD,[{Node,Error}|Replies])
    end;
do_delete_log_2([Node|Tail],LD,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    do_delete_log_2(Tail,LD,[{Node,?RUNTIME:delete_log(Pid)}|Replies]);
	Error ->
	    do_delete_log_2(Tail,LD,[{Node,Error}|Replies])
    end;
do_delete_log_2([],_LD,Replies) ->
    {ok,lists:reverse(Replies)};
do_delete_log_2(Faulty,_,_) ->
    {error,{badarg,Faulty}}.
%% -----------------------------------------------------------------------------

%% Function removing files from runtime components.
%% Returns {ok,Replies} or {error,Reason}.
do_clear(Nodes,LD,Options) ->
    do_clear_2(started_trace_nodes(Nodes,LD),LD,Options,[]).

do_clear_2([Node|Tail],LD,Options,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    do_clear_2(Tail,LD,Options,[{Node,?RUNTIME:clear(Pid,Options)}|Replies]);
	Error ->
	    do_clear_2(Tail,LD,Options,[{Node,Error}|Replies])
    end;
do_clear_2([],_LD,_Options,Replies) ->
    {ok,lists:reverse(Replies)};
do_clear_2(FaultyNodes,_LD,_Options,_Replies) ->
    {error,{badarg,FaultyNodes}}.
%% -----------------------------------------------------------------------------

%% Function doing a flush trace-port.
%% Returns {ok,Replies} or {error,Reason}.
do_flush(Nodes,LD) ->
    do_flush_2(started_trace_nodes(Nodes,LD),LD,[]).

do_flush_2([Node|Rest],LD,Replies) ->
    case get_node_rec(Node,LD) of
	#node{pid=Pid} ->
	    do_flush_2(Rest,LD,[{Node,?RUNTIME:flush(Pid)}|Replies]);
	Error ->
	    do_flush_2(Rest,LD,[{Node,Error}|Replies])
    end;
do_flush_2([],_LD,Replies) ->
    {ok,lists:reverse(Replies)};
do_flush_2(FaultyNodes,_LD,_Replies) ->
    {error,{badarg,FaultyNodes}}.
%% -----------------------------------------------------------------------------

%% Function stopping runtime components. We can only stop runtime components
%% belonging to this control component.
%% Returns {NewLoopdata,Reply}.
do_stop_nodes(Nodes,LD) ->
    do_stop_nodes_2(started_trace_nodes(Nodes,LD),LD,[]).

do_stop_nodes_2([Node|Tail],LD,Replies) ->
    case get_node_rec(Node,LD) of
	#node{ref=MRef} ->
	    erlang:demonitor(MRef),
	    case ?RUNTIME:stop(Node) of
		ok ->
		    NewLD=delete_node_rec(Node,LD),
		    send_to_subscribers({disconnected,Node,void},LD),
		    do_stop_nodes_2(Tail,NewLD,[{Node,ok}|Replies]);
		Error ->
		    do_stop_nodes_2(Tail,LD,[{Node,Error}|Replies])
	    end;
	Error ->
	    do_stop_nodes_2(Tail,LD,[{Node,Error}|Replies])
    end;
do_stop_nodes_2([],LD,Replies) ->
    {LD,{ok,Replies}};
do_stop_nodes_2(Faulty,LD,_) ->
    {LD,{error,{badarg,Faulty}}}.
%% -----------------------------------------------------------------------------

%% Function being called when a runtime component sends a connect message to
%% the controlcomponent. The control component then confirms that is has indeed
%% taken on that runtime component.
%% Returns a new loopdata structure.
do_confirm_connection(Pid,Tag,LD) ->
    case ?RUNTIME:confirm_connection(Pid,Tag) of
	{node_info,Node,_Pid,VSN,State,Status,_Tag} ->
	    MRef=erlang:monitor(process,Pid), % We must monitor it from now on.
	    Rec=#node{node=Node,vsn=VSN,tag=Tag,ref=MRef,pid=Pid},
	    send_to_subscribers({connected,Node,{Tag,{State,Status}}},LD),
	    set_node_rec(Rec,LD);            % Makes new loopdata.
	_Error ->                            % Strange, it wanted us as control!?
	    LD
    end.
%% ------------------------------------------------------------------------------

%% Function handling an incomming DOWN message. This can be one of our runtime
%% components terminating, or a process subscribing to events. Send a trace-event
%% to subscribers if it was a runtime terminating and remove it from
%% our list of runtime components.
%% Note that if a subscriber has subscribed multiple times to events, we will get
%% multiple DOWN messages too, since we have monitored that process multiple
%% times. It is therefore sufficient to remove just one subscription entry here
%% each time (remove on the monitor reference!).
%% Returns a new LoopData structure.
do_down_msg(Ref,Info,LD) ->
   case find(fun ref/2,LD#state.nodes,Ref) of
       {value,Node} ->                       % Yes it was one of our nodes.
	   send_to_subscribers({disconnected,Node,Info},LD),
	   delete_node_rec(Node,LD);
       no_match ->                           % Not one of our nodes.
	   case lists:keysearch(Ref,2,LD#state.subscribers) of
	       {value,{_Pid,_}} ->           % It was a subscriber terminating.
		   LD#state{subscribers=lists:keydelete(Ref,2,LD#state.subscribers)};
	       false ->                      % Not one of our subscribers either.
		   LD                        % Do nothing then.
	   end
   end.
%% ------------------------------------------------------------------------------



%% ==============================================================================
%% Help functions.
%% ==============================================================================


%% Help function which inspects options to start and add_node. Returns a new
%% list of options in {ok,Options} or {error,Reason}.
check_options(Options, Context) ->
    check_options_2(Options, Context, []).

check_options_2([],_Context,Result) ->
    {ok,Result};
check_options_2([{subscribe,Pid}|OptionsTail],start,Result) when pid(Pid) ->
    check_options_2(OptionsTail,start,[{subscribe,Pid}|Result]);
check_options_2([{unsubscribe,Pid}|OptionsTail],start,Result) when pid(Pid) ->
    check_options_2(OptionsTail,start,[{unsubscribe,Pid}|Result]);
check_options_2([{dependency,How}|OptionsTail],Context,Result) ->
    check_options_2(OptionsTail,Context,[{dependency,How}|Result]);
check_options_2([{overload,How}|OptionsTail],Context,Result) ->
    check_options_2(OptionsTail,Context,[{overload,How}|Result]);
check_options_2([overload|OptionsTail],Context,Result) ->
    check_options_2(OptionsTail,Context,[overload|Result]);
check_options_2([UnKnown|_],_Context,_Result) ->
    {error,{unknown_option,UnKnown}};
check_options_2(UnKnown,_Context,_Result) ->
    {error,{unknown_option,UnKnown}}. 
%% ------------------------------------------------------------------------------

%% Help function initiating the #state structure, i.e the loopdata. Since there
%% are some initial values from the record defaults when creating a new #state,
%% those must be compared with possibly specified Options. Specified Options shall
%% of course override defaults.
%% Note that it is not the control component's responsibility to understand all
%% options later given to a runtime component. It mearly stores them in rt_options
%% so they can be passed to the runtime component at start-up.
%% Returns a loopdata structure.
initiate_state(Options) ->
    ResultingOptions=merge_options((#state{})#state.rt_options,Options),
    LD1=initiate_state_2(ResultingOptions,#state{rt_options=[]}),
    case node() of                           % Finally set the distribution flag.
	nonode@nohost ->
	    LD1#state{distributed=false};
	_ ->
	    LD1#state{distributed=true}
    end.

initiate_state_2([{subscribe,Proc}|Tail],LD) when pid(Proc);atom(Proc)->
    MRef=erlang:monitor(process,Proc),
    initiate_state_2(Tail,LD#state{subscribers=[{Proc,MRef}|LD#state.subscribers]});
initiate_state_2([Opt|Tail],LD) when tuple(Opt),size(Opt)>=1 ->
    initiate_state_2(Tail,initiate_state_3(element(1,Opt),Opt,LD));
initiate_state_2([Opt|Tail],LD) when atom(Opt) ->
    initiate_state_2(Tail,initiate_state_3(Opt,Opt,LD));
initiate_state_2([_|Tail],LD) ->
    initiate_state_2(Tail,LD);
initiate_state_2([],LD) ->
    LD.

initiate_state_3(OptName,Opt,LD) ->
    case initiate_state_is_rt_option(OptName) of
	true ->                              % Yes, it shall be part of the rt_options.
	    LD#state{rt_options=[Opt|LD#state.rt_options]};
	false ->                             % Ignore the option then.
	    LD
    end.

%% This is the only place you have to change should there be more rt_options
%% introduced.
initiate_state_is_rt_option(overload) -> true;
initiate_state_is_rt_option(dependency) -> true;
initiate_state_is_rt_option(_) -> false.
%% ------------------------------------------------------------------------------

%% Help function which takes a list of default options and a list of overriding
%% options. The function builds a return value consisting of all default options
%% unless they are overridden by a overriding option.
%% An option can be either {Param,.....} or Param. I.e either a tuple with zero
%% or more values associated with the Parameter, or just an atom.
merge_options([], Options) ->
    Options;
merge_options([T|DefaultTail],Options) when tuple(T),size(T)>=1 ->
    merge_options(DefaultTail,merge_options_2(element(1,T),T,Options));
merge_options([Param|DefaultTail],Options) when atom(Param) ->
    merge_options(DefaultTail,merge_options_2(Param,Param,Options));
merge_options([_|DefaultTail],Options) ->    % Strange, bad default option!
    merge_options(DefaultTail,Options).

merge_options_2(Param,Opt,Options) ->
    case merge_options_find(Param,Options) of
	true ->
	    Options;
	false ->
	    [Opt|Options]
    end.

merge_options_find(Param,[T|_]) when tuple(T),element(1,T)==Param ->
    true;
merge_options_find(Param,[Param|_]) ->
    true;
merge_options_find(Param,[_|Rest]) ->
    merge_options_find(Param,Rest);
merge_options_find(_,[]) ->
    false.
%% ------------------------------------------------------------------------------

%% Help function which transforms certain parts of a tracer data. Those are
%% parts which must be transformed at the controlnode like node to pid mappings.
%% It also checks the formatting of the tracerdata since runtime components
%% does not accept too badly formatted tracerdata.
%% Returns {ok,NewTraceData} or {error,Reason}.
check_modify_tracerdata(TracerData,LoopData) when list(TracerData) ->
    case lists:keysearch(trace,1,TracerData) of
	{value,{_,TraceTD}} ->               % Examine the trace part.
	    case check_modify_tracerdata(TraceTD,LoopData) of
		{ok,NewTraceTD} ->
		    {ok,lists:keyreplace(trace,1,TracerData,{trace,NewTraceTD})};
		{error,Reason} ->            % The trace part was faulty.
		    {error,Reason}           % Ruins everything :-)
	    end;
	false ->                             % Unusual, but no trace part.
	    {ok,TracerData}                  % No modifications necessary.
    end;
check_modify_tracerdata(collector,_LoopData) ->
    {ok, collector};
check_modify_tracerdata({relayer,Collector},_LoopData) when is_pid(Collector) ->
    {ok,{relayer,Collector}};
check_modify_tracerdata({relayer,Collector},LoopData) when is_atom(Collector) ->
    case get_node_rec(Collector,LoopData) of
	Rec when is_record(Rec,node) ->      % Collector is a known node.
	    {ok,{relayer,Rec#node.pid}};
	{error,not_an_added_node} ->
	    {error,{not_an_added_node,Collector}}
    end;
check_modify_tracerdata({Type,Data},_LoopData) when Type==ip;Type==file ->
    {ok,{Type,Data}};
check_modify_tracerdata({Handler,Data},_LoopData) when function(Handler) ->
    {ok,{Handler,Data}};
check_modify_tracerdata(Data,_LoopData) ->
    {error,{bad_tracerdata,Data}}.
%% -----------------------------------------------------------------------------

%% Help function sending an event to all processes subscribing to events.
%% Note that the function manipulates the from Node indicator incase we are not
%% running in a distributed system.
%% Returns nothing significant.
send_to_subscribers(Msg={Event,_Node,Data},LD) ->
    AdaptedMsg=
	case LD#state.distributed of
	    true ->
		Msg;
	    false ->
		{Event,local_runtime,Data}
	end,
    TraceEvent={inviso_event,self(),erlang:localtime(),AdaptedMsg},
    send_to_subscribers_2(LD#state.subscribers,TraceEvent).

send_to_subscribers_2([],_) ->
    ok;
send_to_subscribers_2([{Subscriber,_}|Tail],TraceEvent) ->
    Subscriber ! TraceEvent,
    send_to_subscribers_2(Tail,TraceEvent).
%% -----------------------------------------------------------------------------

%% Help function converting the Nodes parameter to known nodes. Actually today
%% it only converts the all atom to all known nodes.
%% Returns a list of nodes.
started_trace_nodes(all,LoopData) ->
    lists:map(fun(N)->N#node.node end,LoopData#state.nodes);
started_trace_nodes(Nodes,_) ->
    Nodes.
%% ------------------------------------------------------------------------------

%% Help function searching through a list of elements looking for an element
%% containing Key. How the element shall be interpreted is done by the Fun.
%% Returns {value,Element} or 'no_match'.
%% Fun=fun(Element,Key)={return,Value} | false
find(_,[],_Key) -> 
    no_match;
find(Fun,[H|T],Key) ->
    case Fun(H,Key) of
	{return,Value}->
	    {value,Value};
	_ ->
	    find(Fun,T,Key)
    end.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Functions handling the nodes datastructure part of the #state.
%% #state.nodes is a list of #node.
%% -----------------------------------------------------------------------------

%% Function used to build find fun, looking for a certain #node with its monitoring
%% reference set to Ref. Useful when finding out if a DOWN message comes from one
%% of our runtime components.
ref(#node{ref=Ref,node=Node},Ref) ->
    {return,Node};
ref(_,_) ->
    false.
%% -----------------------------------------------------------------------------

%% use in find/3
%% Function used to build find fun, finding out if we have a node with the node
%% name Node.
is_started(#node{node=Node},Node) ->
    {return,true};
is_started(_,_) ->
    false.
%% -----------------------------------------------------------------------------

%% Help function replacing or adding an entry for a node. Works on either a list
%% of #node or a loopdata structure. Returns a new list of #node or a loopdata struct.
set_node_rec(Rec,LD=#state{nodes=NodeList}) ->
    LD#state{nodes=set_node_rec_2(Rec,NodeList)}.

set_node_rec_2(Rec,[]) ->
    [Rec];
set_node_rec_2(Rec,[NodeRec|Tail]) when NodeRec#node.node==Rec#node.node ->
    [Rec|Tail];
set_node_rec_2(Rec,[NodeRec|Tail]) ->
    [NodeRec|set_node_rec_2(Rec,Tail)].
%% ------------------------------------------------------------------------------

%% Help function finding a node record for Node in a list of #node or in loopdata.
%% Returns the #node in question or {error,not_an_added_node}.
get_node_rec(Node,NodeList) when list(NodeList) ->
    get_node_rec_2(Node,NodeList);
get_node_rec(Node,#state{nodes=NodeList}) ->
    get_node_rec_2(Node,NodeList).

get_node_rec_2(_Node,[]) ->
    {error,not_an_added_node};
get_node_rec_2(Node,[NodeRec|_]) when NodeRec#node.node==Node ->
    NodeRec;
get_node_rec_2(Node,[_NodeRec|Tail]) ->
    get_node_rec_2(Node,Tail).
%% ------------------------------------------------------------------------------

%% Help function removing a #node from either a list of #node or from a loopdata
%% structure. Returns a new list of #node or a new loopdata structure.
delete_node_rec(Node,LD=#state{nodes=NodeList}) ->
    LD#state{nodes=delete_node_rec_2(Node,NodeList)};
delete_node_rec(Node,NodeList) when list(NodeList) ->
    delete_node_rec_2(Node,NodeList).

delete_node_rec_2(_,[]) ->
    [];
delete_node_rec_2(#node{node=Node},[#node{node=Node}|Tail]) ->
    Tail;
delete_node_rec_2(Node,[#node{node=Node}|Tail]) ->
    Tail;
delete_node_rec_2(Node,[NRec|Tail]) ->
    [NRec|delete_node_rec_2(Node,Tail)].
%% ------------------------------------------------------------------------------


%% =============================================================================
%% Implementation of the help process receiving all logs from the runtime
%% components. This process is referred to as the CollectPid.
%% It is responsible for sending the reply back to the control component
%% client. If a runtime component becomes suspended, the CollectPid is
%% alerted by the DOWN message.
%% Note that it may take some time before this process responds back to the client.
%% Therefore the client must wait for 'infinity'. The job of transferring the
%% files can be costly. Therefore it is a good idea to stop if no one is really
%% interested in the result. This collector process monitors the From client in
%% order to learn if the job can be cancelled. That will also be a possibility
%% for a client to willfully cancel a fetch job.
%% =============================================================================

%% Intitial function on which the control component spawns. Note that the start
%% must be done in two steps since the runtime components must be informed of
%% the CollectPid. But the CollectPid must also know from which runtime components
%% it can expect files from.
%% InitialReplies: contains {Node,Result} for nodes from where there will be no
%%   files, but which must be part of the final reply.
log_rec_init(Parent,Dest,Prefix,From={ClientPid,_}) ->
    receive
	{?MODULE,Parent,Fetchers,InitialReplies} ->
	    RTs=lists:map(fun({N,F})->
				  {N,erlang:monitor(process,F),void,void,void}
			  end,
			  Fetchers),
	    CMRef=erlang:monitor(process,ClientPid), % Monitor the client.
	    case log_rec_loop(Dest,Prefix,RTs,InitialReplies,CMRef) of
		Reply when list(Reply) ->   % It is an ok value.
		    gen_server:reply(From,{ok,Reply});
		{error,Reason} ->
		    gen_server:reply(From,{error,Reason});
		false ->                    % The client terminated, no response.
		    true                    % Simply terminate, fetchers will notice.
	    end
    end.

log_rec_loop(_Dest,_Prefix,[],Replies,_CMRef) -> % All nodes done!
    Replies;                                % This is the final reply.
log_rec_loop(Dest,Prefix,RTs,Replies,CMRef) ->
    receive
	{Node,open,{FType,RemoteFName}} ->
	    case lists:keysearch(Node,1,RTs) of
		{value,{_,MRef,_,_,_}} ->
		    {NewRTs,NewReplies}=
			log_rec_open(Dest,Prefix,Node,MRef,FType,RemoteFName,RTs,Replies),
		    log_rec_loop(Dest,Prefix,NewRTs,NewReplies,CMRef);
		false ->
		    log_rec_loop(Dest,Prefix,RTs,Replies,CMRef)
	    end;
	{Node,open_failure,{FType,RemoteFName}} ->
	    case lists:keysearch(Node,1,RTs) of
		{value,{_,MRef,_,_,_}} ->
		    {NewRTs,NewReplies}=
			log_rec_open_failure(Node,MRef,FType,RemoteFName,RTs,Replies),
		    log_rec_loop(Dest,Prefix,NewRTs,NewReplies,CMRef);
		false ->
		    log_rec_loop(Dest,Prefix,RTs,Replies,CMRef)
	    end;
	{Node,payload,Bin,FPid} ->          % A chunk of data from a fetcher.
	    case lists:keysearch(Node,1,RTs) of
		{value,{_,_,_,_,void}} ->   % Node has no file open here.
		    FPid ! {self(),cancel_transmission}, % No use sending more to me.
		    log_rec_loop(Dest,Prefix,RTs,Replies,CMRef); % Simply ignore payload.
		{value,{_Node,MRef,FType,FName,FD}} ->
		    case log_rec_payload(Node,MRef,FType,FName,FD,Bin,RTs,Replies) of
			{ok,{NewRTs,NewReplies}} ->
			    FPid ! {self(),chunk_ack}, % For flow control.
			    log_rec_loop(Dest,Prefix,NewRTs,NewReplies,CMRef);
			{error,{NewRTs,NewReplies}} ->
			    FPid ! {self(),cancel_transmission}, % No use sending more to me.
			    log_rec_loop(Dest,Prefix,NewRTs,NewReplies,CMRef)
		    end;
		false ->                    % Node is not part of transfere.
		    FPid ! {self(),cancel_transmission}, % No use sending more to me.
		    log_rec_loop(Dest,Prefix,RTs,Replies,CMRef)
	    end;
	{Node,end_of_file} ->
	    case lists:keysearch(Node,1,RTs) of
		{value,{_,_,_,_,void}} ->   % Node has no file open here.
		    log_rec_loop(Dest,Prefix,RTs,Replies,CMRef);
		{value,{_,MRef,FType,FName,FD}} ->
		    {NewRTs,NewReplies}=
			log_rec_eof(Node,MRef,FType,FName,FD,RTs,Replies),
		    log_rec_loop(Dest,Prefix,NewRTs,NewReplies,CMRef);
		false ->
		    log_rec_loop(Dest,Prefix,RTs,Replies,CMRef)
	    end;
	{Node,end_of_transmission} ->       % This runtime is done!
	    case lists:keysearch(Node,1,RTs) of
		{value,{_Node,MRef,_,_,_}} ->
		    erlang:demonitor(MRef),
		    log_rec_loop(Dest,Prefix,
				 lists:keydelete(Node,1,RTs),
				 log_rec_mkreply(Node,complete,Replies),
				 CMRef);
		false ->                    % Strange, not one of our nodes.
		    log_rec_loop(Dest,Prefix,RTs,Replies,CMRef)
	    end;
	{Node,incomplete} ->                % This runtime is done (with errors).
	    case lists:keysearch(Node,1,RTs) of
		{value,{_,MRef,FType,FName,FD}} ->
		    erlang:demonitor(MRef),
		    {NewRTs,NewReplies}=
			log_rec_incomplete(Node,FType,FName,FD,RTs,Replies),
		    log_rec_loop(Dest,Prefix,NewRTs,NewReplies,CMRef);
		false ->                    % Not our, or not anylonger.
		    log_rec_loop(Dest,Prefix,RTs,Replies,CMRef)
	    end;
	{Node,{error,Reason}} ->            % Remote file read_error.
	    case lists:keysearch(Node,1,RTs) of
		{value,{_,MRef,FType,FName,FD}} ->
		    {NewRTs,NewReplies}=
			log_rec_error(Node,MRef,FType,FName,FD,RTs,Reason,Replies),
		    log_rec_loop(Dest,Prefix,NewRTs,NewReplies,CMRef);
		false ->
		    log_rec_loop(Dest,Prefix,RTs,Replies,CMRef)
	    end;
	{'DOWN',CMRef,process,_,_} ->       % The client got tired waiting.
	    log_rec_cancel(Dest,RTs,Replies), % Close and remove all files.
	    false;                          % Indicate no response message.
	{'DOWN',Ref,process,_P,_Info} ->
	    case lists:keysearch(Ref,2,RTs) of
		{value,{Node,_,FType,FName,FD}} ->
		    {NewRTs,NewReplies}=
			log_rec_incomplete(Node,FType,FName,FD,RTs,Replies),
		    log_rec_loop(Dest,Prefix,NewRTs,NewReplies,CMRef);
		false ->                    % Not our, or not anylonger.
		    log_rec_loop(Dest,Prefix,RTs,Replies,CMRef)
	    end;
	_ ->
	    log_rec_loop(Dest,Prefix,RTs,Replies,CMRef)
    end.

%% Help function opening a new target file on the receiver. It returns
%% {NewRTs,NewReplies}.
%% Note that we must protect us against that some of the strings are not proper
%% strings, but contains garbage.
log_rec_open(Dest,Prefix,Node,MRef,FType,RemoteFName,RTs,Replies) ->
    case catch log_rec_open_2(Dest,Prefix,Node,MRef,FType,RemoteFName,RTs,Replies) of
	{'EXIT',Reason} ->
	    NewRTs=lists:keyreplace(Node,1,RTs,{Node,MRef,void,void,void}),
	    NewReplies=
		log_rec_addreply(Node,
				 FType,
				 {error,{file_open,{Reason,[Dest,Prefix,RemoteFName]}}},Replies),
	    {NewRTs,NewReplies};
	Result ->
	    Result
    end.

log_rec_open_2(Dest,Prefix,Node,MRef,FType,RemoteFName,RTs,Replies) ->
    FName=Prefix++RemoteFName,              % Our file name.
    case file:open(filename:join([Dest,FName]),[write]) of
	{ok,FD} ->
	    NewRTs=lists:keyreplace(Node,1,RTs,{Node,MRef,FType,FName,FD}),
	    {NewRTs,Replies};
	{error,Reason} ->
	    NewRTs=lists:keyreplace(Node,1,RTs,{Node,MRef,void,void,void}),
	    NewReplies=
		log_rec_addreply(Node,FType,{error,{file_open,{Reason,FName}}},Replies),
	    {NewRTs,NewReplies}
    end.

%% Help function adding a file that was unsuccessfully opened as failed.
log_rec_open_failure(Node,MRef,FType,RemoteFName,RTs,Replies) ->
    NewRTs=lists:keyreplace(Node,1,RTs,{Node,MRef,void,void,void}),
    NewReplies=
	log_rec_addreply(Node,
			 FType,
			 {error,{remote_open,RemoteFName}},Replies),
    {NewRTs,NewReplies}.

%% Help function whih writes the Bin to the FD file. If writing was unsuccessful,
%% close the file and modify RTs and add a reply to Replies. Note that we can not
%% stop the runtime from sending us more data belonging to this file. But we will
%% simply just inore it from now on.
%% Returns {SuccessCode,{NewRTs,NewReplies}}.
log_rec_payload(Node,MRef,FType,FName,FD,Bin,RTs,Replies) ->
    case file:write(FD,Bin) of
	ok ->
	    {ok,{RTs,Replies}};
	{error,Reason} ->
	    file:close(FD),
	    NewRTs=lists:keyreplace(Node,1,RTs,{Node,MRef,void,void,void}),
	    NewReplies=
		log_rec_addreply(Node,FType,{error,{file_write,{Reason,FName}}},Replies),
	    {error,{NewRTs,NewReplies}}
    end.

%% Help function whih shall be used when a file has been successfully transfered.
%% This function closes the output file and updates RTs and the Replies.
%% Returns {NewRTs,NewReplies}.
log_rec_eof(Node,MRef,FType,FName,FD,RTs,Replies) ->
    file:close(FD),
    NewRTs=lists:keyreplace(Node,1,RTs,{Node,MRef,void,void,void}),
    {NewRTs,log_rec_addreply(Node,FType,{ok,FName},Replies)}.

%% Help function which, if there is an open file, indicates it as truncated in the
%% replies. And finalize the reply for Node assuming that the node is in total incomplete
%% Returns {NewRTs,NewReplies}.
log_rec_incomplete(Node,_FType,_FName,void,RTs,Replies) ->
    NewRTs=lists:keydelete(Node,1,RTs),     % The node is done.
    {NewRTs,log_rec_mkreply(Node,incomplete,Replies)};
log_rec_incomplete(Node,FType,FName,FD,RTs,Replies) ->
    file:close(FD),                         % Not going to write anymore in this file.
    NewRTs=lists:keydelete(Node,1,RTs),     % The node is done.
    NewReplies=log_rec_addreply(Node,FType,{error,{truncated,FName}},Replies),
    {NewRTs,log_rec_mkreply(Node,incomplete,NewReplies)}.

%% Help function handling the case when runtime component experiences an error
%% transferering the file. That means that there will be no more chunks of this
%% file. Hence it works a bit like EOF.
%% Returns {NewRTs,NewReplies}.
log_rec_error(Node,MRef,FType,FName,FD,RTs,Reason,Replies) ->
    file:close(FD),
    NewRTs=lists:keyreplace(Node,1,RTs,{Node,MRef,void,void,void}),
    {NewRTs,log_rec_addreply(Node,FType,{error,{truncated,{Reason,FName}}},Replies)}.
%% -----------------------------------------------------------------------------

%% Help function adding a reply to the list of replies.
%% Replies is a list {Node,FType,Reply} for each file handled, sucessfully or not.
%% The list may also contain finalized nodes, which will be on the format:
%% {Node,{Conclusion,[{trace_log,TraceLogReplies},{ti_log,TiLogReplies}]}}.
log_rec_addreply(Node,FType,Reply,Replies) ->
    [{Node,FType,Reply}|Replies].

%% Help function which converts the {Node,FType,Reply} tuples in Replies to
%% a finalized reply.
log_rec_mkreply(Node,Conclusion,Replies) ->
    {RemainingReplies,TiReplies,TraceReplies}=
	log_rec_mkreply_node_ftype(Node,Replies,[],[],[]),
    [{Node,{Conclusion,[{trace_log,TraceReplies},{ti_log,TiReplies}]}}|
     RemainingReplies].

%% Help function taking out the ti_log and trace_log file-types replies for
%% Node. Returns {RemainingReplies,Ti,Trace}. 
log_rec_mkreply_node_ftype(Node,[{Node,ti_log,Result}|Rest],Replies,Ti,Trace) ->
    log_rec_mkreply_node_ftype(Node,Rest,Replies,[Result|Ti],Trace);
log_rec_mkreply_node_ftype(Node,[{Node,trace_log,Result}|Rest],Replies,Ti,Trace) ->
    log_rec_mkreply_node_ftype(Node,Rest,Replies,Ti,[Result|Trace]);
log_rec_mkreply_node_ftype(Node,[Reply|Rest],Replies,Ti,Trace) ->
    log_rec_mkreply_node_ftype(Node,Rest,[Reply|Replies],Ti,Trace);
log_rec_mkreply_node_ftype(_,[],Replies,Ti,Trace) ->
    {Replies,Ti,Trace}.
%% -----------------------------------------------------------------------------

%% If the fetching job shall be cancelled, we must close all open files and
%% remove them including all already closed files. Returns nothing significant.
log_rec_cancel(Dest,RTs,Replies) ->
    log_rec_cancel_open(Dest,RTs),          % First close and remove all open files.
    log_rec_cancel_finished(Dest,Replies).  % Remove all already closed files.

log_rec_cancel_open(Dest,[{_Node,_MRef,_FType,_FName,void}|Rest]) ->
    log_rec_cancel_open(Dest,Rest);         % There is no open file to close.
log_rec_cancel_open(Dest,[{_Node,_MRef,_FType,FName,FD}|Rest]) ->
    file:close(FD),
    catch file:delete(filename:join(Dest,FName)), % Will just try to do my best.
    log_rec_cancel_open(Dest,Rest);
log_rec_cancel_open(_Dest,[]) ->
    true.

log_rec_cancel_finished(Dest,[{_N,_FT,Reply}|Rest]) ->
    [FName]=log_rec_cancel_finished_get_fname([Reply]),
    catch file:delete(filename:join(Dest,FName)),
    log_rec_cancel_finished(Dest,Rest);
log_rec_cancel_finished(Dest,[{_N,{_Conclusion,[{_,Replies1},{_,Replies2}]}}|Rest]) ->
    FNames1=log_rec_cancel_finished_get_fname(Replies1),
    lists:foreach(fun(FName)->
			  catch file:delete(filename:join(Dest,FName))
		  end,
		  FNames1),
    FNames2=log_rec_cancel_finished_get_fname(Replies2),
    lists:foreach(fun(FName)->
			  catch file:delete(filename:join(Dest,FName))
		  end,
		  FNames2),
    log_rec_cancel_finished(Dest,Rest);
log_rec_cancel_finished(_Dest,[]) ->
    true.

%% Help function going through all possible reply values for a file. So
%% consequently there must be a clause here for every possible log_rec_addreply
%% call above. Returns a list of filenames that shall be removed in order to
%% restore the disk since the fetch job is cancelled.
log_rec_cancel_finished_get_fname([{error,{file_open,{_,FName}}}|Rest]) ->
    [FName|log_rec_cancel_finished_get_fname(Rest)];
log_rec_cancel_finished_get_fname([{error,{file_write,{_,FName}}}|Rest]) ->
    [FName|log_rec_cancel_finished_get_fname(Rest)];
log_rec_cancel_finished_get_fname([{ok,FName}|Rest]) ->
    [FName|log_rec_cancel_finished_get_fname(Rest)];
log_rec_cancel_finished_get_fname([{error,{truncated,{_,FName}}}|Rest]) ->
    [FName|log_rec_cancel_finished_get_fname(Rest)];
log_rec_cancel_finished_get_fname([{error,{truncated,FName}}|Rest]) ->
    [FName|log_rec_cancel_finished_get_fname(Rest)];
log_rec_cancel_finished_get_fname([_|Rest]) ->       % This shall not happend.
    log_rec_cancel_finished_get_fname(Rest);
log_rec_cancel_finished_get_fname([]) ->
    [].
%% -----------------------------------------------------------------------------

%% EOF

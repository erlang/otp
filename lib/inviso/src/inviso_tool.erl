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
%% The inviso_tool implementation. A tool that uses inviso.
%%
%% Authors:
%% Lennart Öhman, lennart.ohman@st.se
%% -----------------------------------------------------------------------------

-module(inviso_tool).


%% This is the inviso tool, which is a tool using the inviso trace application.
%% It is developed to make tracing using trace cases possible in an environment
%% of distributed Erlang nodes.
%% A current restriction is that the Erlang nodes are supposed to have the same
%% code. This since inviso tool can at this point not handle subsets of nodes.
%% Instead all participating Erlang nodes are treated the same.
%%
%% The main functionality of the inviso tool are:
%%
%% (1) Handles start and stop of tracing at participating nodes.
%% (2) Interprets trace-case files at a distributed network level.
%%     (The inviso runtime component is responsible for interpreting
%%      trace cases at a local level, if run in an autostart).
%% (3) Keeps a command history log from which:
%%     (a) Sequences easily can be repeated.
%%     (b) Autostart configuration files can be created (understood by the
%%         default inviso autostart mechanism).
%% (4) Performs reactivation in case tracing is suspended (manually or by
%%     an overload mechanism).
%% (5) Can reconnect crashed nodes and by using the history bringing them
%%     up to speed.

%% Distributed Erlang
%% ------------------
%% Inviso is built to run in a distributed environment.
%% The inviso tool can also be used in a non distributed environment.

%% Short description
%% -----------------
%% Start-up of the inviso tool
%% During the start-up of the tool, the tool starts runtime components at
%% all participating nodes. A runtime component can already be running at
%% a particular node and will then simply be adopted.
%%
%% Session
%% A session is said to start when tracing is initiated, and ends when
%% made to stop by the user. When a session is stopped, tracing is stopped
%% at all participating nodes. Note that participating nodes may come and
%% go though the time-frame of a session. That means that if a node is
%% reconnected it may resume its tracing in the current session through
%% a 'restart_session'. A runtime component that is already tracing at the
%% time start-session will simply be part of the session without its
%% ingoing tracing being changed.
%%
%% Reactivation
%% A node that is suspended can be reactivated to resume tracing. Note that
%% tracing has in this situation never been stopped at the node in question.
%% The inviso tool resumes the node and applies the history to it.
%%
%% Reconnect
%% A node that is "down" from the inviso tool's perspective can be
%% reconnected. During reconnection the tool restarts the runtime component
%% at that node but does not (re)initiate tracing. The latter is called
%% restart_session and must be done explicitly, unless the node in question
%% is in fact already tracing. If the node is already tracing (due to an autostart
%% for instance), it automatically becomes part of the ongoing session (if
%% there is an ongoing session).
%%
%% Restart Session
%% A node that has been down and has been reconnected can be made to
%% initialize and resume its tracing. This is done by starting the session
%% at the node in question and redoing the current history.

%% Trace files within a session
%% Since it is possible to init-tracing (from an inviso perspective) several
%% times within the same session, a session may leave several trace log files
%% behind. This must be resolved by the tracer data generator function
%% (user supplied) by marking filenames in a chronological order but still
%% making them possible to identify as part of the same session



%% -----------------------------------------------------------------------------
%% API exports.
%% -----------------------------------------------------------------------------

-export([start/0,start/1,stop/0,stop/1]).
-export([reconnect_nodes/0,reconnect_nodes/1,
	 start_session/0,start_session/1,
	 reinitiate_session/0,reinitiate_session/1,
	 restore_session/0,restore_session/1,restore_session/2,
	 stop_session/0,
	 reset_nodes/0,reset_nodes/1,
	 atc/3,sync_atc/3,sync_atc/4,
	 sync_rtc/2,sync_rtc/3,
	 dtc/2,sync_dtc/2,sync_dtc/3,
	 inviso/2]).
-export([reactivate/0,reactivate/1,
	 save_history/1,
	 get_autostart_data/1,get_autostart_data/2,
	 get_activities/0,get_node_status/0,get_node_status/1,get_session_data/0]).
-export([flush/0,flush/1]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Debug exports.
%% -----------------------------------------------------------------------------

-export([get_loopdata/0]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% OTP exports and call backs.
%% -----------------------------------------------------------------------------

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Internal exports.
%% -----------------------------------------------------------------------------

-export([tc_executer/4,reactivator_executer/6]).
-export([std_options_generator/1]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Constants.
%% -----------------------------------------------------------------------------

%% Defines the inviso function calls that shall be possible to do through the
%% inviso API in this tool.
-define(INVISO_CMDS,
	[{tp,5},{tp,4},{tp,1},{tpl,5},{tpl,4},{tpl,1},
	 {ctp,1},{ctp,2},{ctp,3},{ctpl,1},{ctpl,2},{ctpl,3},
	 {tf,2},{tf,1},{ctf,2},{ctf,1},{ctf_all,0},
	 {init_tpm,4},{init_tpm,7},
	 {tpm,4},{tpm,5},{tpm,8},
	 {tpm_tracer,4},{tpm_tracer,5},{init_tpm,8},
	 {tpm_ms,5},{tpm_ms_tracer,5},
	 {ctpm_ms,4},{ctpm,3},
	 {tpm_localnames,0},{ctpm_localnames,0},
	 {tpm_globalnames,0},{ctpm_globalnames,0},
	 {ctp_all,0},
	 {suspend,1},{cancel_suspension,0}]).
%% -----------------------------------------------------------------------------

%% These inviso functions shall be included in the command history log. Others
%% are not relevant to be redone during a recactivation, a restart session or
%% exported to an autostart file.
-define(INVISO_CMD_HISTORY,
	[{tp,5},{tp,4},{tp,1},{tpl,5},{tpl,4},{tpl,1},
	 {ctp,1},{ctp,2},{ctp,3},{ctpl,1},{ctpl,2},{ctpl,3},
	 {tf,2},{tf,1},{ctf,2},{ctf,1},{ctf_all,0},
	 {init_tpm,4},{init_tpm,7},
	 {tpm,4},{tpm,5},{tpm,8},
	 {tpm_tracer,4},{tpm_tracer,5},{init_tpm,8},
	 {tpm_ms,5},{tpm_ms_tracer,5},
	 {ctpm_ms,4},{ctpm,3},
	 {tpm_localnames,0},{ctpm_localnames,0},
	 {tpm_globalnames,0},{ctpm_globalnames,0},
	 {ctp_all,0}]).
%% -----------------------------------------------------------------------------

%% Since many function calls to inviso may take long time, especially if they
%% involve difficult and many trace patterns to set, the default gen_server:call
%% time out can not be used. We just do not want to get stuck for ever if some
%% error occurs.
-define(CALL_TIMEOUT,60000).

%% Default max time to wait for a trace case called synchronously to return.
-define(SYNC_TC_TIMEOUT,10000).

%% Runtime components shall terminate when the tool terminates.
-define(DEFAULT_DEPENDENCY,{dependency,0}).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Record definitions.
%% -----------------------------------------------------------------------------

%% The loopdata record.
-record(ld,{
	  dir=".",                          % Working dir of the tool.
	  nodes=down,                       % The nodesD database, defaults to non-distr.
	  c_node,                           % Location of inviso_c.
	  c_pid,                            % The inviso control component.
	  regexp_node,                      % Node for regexp expansions.
	  tc_dict,                          % Trace case definition db.
	  chl,                              % Command history log.
	  session_state=passive,            % passive | tracing
	  tdg={inviso_tool_lib,std_tdg,[]}, % Tracer data generator func.
	  tracer_data,                      % Current session nr and TDGargs.
	  reactivators=[],                  % Pids of now running reactivators.
	  tc_def_file,                      % Trace case definition file.
	  optg={?MODULE,std_options_generator,[]}, % Generates options to add_nodes/3.
	  initial_tcs=[],                   % Initial trace cases.
	  started_initial_tcs=[],           % Cases that must be stopped when stop_tracing.
	  history_dir,                      % File path for history file.
	  keep_nodes=[],                    % Nodes that shall not be cleared when stopping.
	  debug=false                       % Internal debug mode
	  }).
%% -----------------------------------------------------------------------------


%% =============================================================================
%% API
%% =============================================================================

%% start()={ok,Pid} | {error,{already_started,pid()}}
%% start(Config)
%%   Config=[{Opt,Value},...], list of tuple options.
%%     Opt=dir|nodes|c_node|regexp_node|tdg|tc_def_file|optg|initial_tcs|
%%           history_dir|keep_nodes
%% Starts the inviso_tool process. Options in Config are the same as those
%% which are kept in the #ld structure.
start() ->
    start([]).
start(Config) ->
    gen_server:start({local,?MODULE},?MODULE,Config,[]).
%% -----------------------------------------------------------------------------

%% stop(UntouchedNodes)=
%% stop()={ok,NodeResults} | NodeResult | {error,Reason}
%%   UntouchedNodes=list(), nodes where any trace patterns shall not be removed.
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult=ok | {error,Reason} | patterns_untouched
%% Stops the inviso tool and the inviso control component. Runtime components are
%% stopped by them selves depending on their dependcy of the control component.
%% All runtime components that are not marked as to be kept will have their
%% trace patterns cleared before the inviso control component is shutdown.
%% The NodeResults indicates which nodes were successfullt handled.
stop() ->
    stop([]).
stop(UntouchedNodes) ->
    gen_server:call(?MODULE,{stop,UntouchedNodes},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% reconnect_nodes()=NodeResult; function for the nod-distributed case.
%% reconnect_nodes(Nodes)={ok,NodesResults}
%%   NodesResults=[{Node,NodeResult},...]
%%     NodeResult={ok,{State,Status}} | {error,NReason}
%%       State=tracing | inactive
%%       Status=running | suspended
%%       NReason=unknown_node | already_connected | down
%% (Re)starts the inviso runtime components at Nodes. Depending on its state
%% (new,idle or tracing) and if the tool is running a session or not, it becomes
%% part of the tool's ongoing session. If the newly reconnected node is not
%% tracing but the tool runs a session, the node must be reinitiated to become
%% tracing.
reconnect_nodes() ->
    gen_server:call(?MODULE,{reconnect_nodes,local_runtime},?CALL_TIMEOUT).
reconnect_nodes(Node) when atom(Node) ->
    reconnect_nodes([Node]);
reconnect_nodes(Nodes) when list(Nodes) ->
    gen_server:call(?MODULE,{reconnect_nodes,Nodes},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% start_session()={ok,{SessionNr,InvisoReturn}} | {error,Reason}
%% start_session(MoreTDGargs)=
%%   MoreTDGargs=list(), prepended to the fixed list of args used when calling the
%%     tracer data generator function.
%%   SessionNr=integer(), trace sessions are numbered by the tool.
%%   InvisoReturn=If successful inviso call, the returnvalue from inviso.
%%     Note that individual nodes may be unsuccessful. See inviso:init_tracing/1
%% Initiates tracing at all participating nodes.
start_session() ->
    start_session([]).
start_session(MoreTDGargs) ->
    gen_server:call(?MODULE,{start_session,MoreTDGargs},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% reinitiate_session(Nodes)={ok,InvisoReturn} | {error,Reason}
%%   InvisoReturn=If successful inviso call, the returnvalue from inviso:init_tracing/1.
%%     Note that individual nodes may be unsuccessful. Mentioned nodes not part
%%       of the tool or not in state inactive will be marked as failing by the
%%       tool in the InvisoReturn.
%% To reinitate a node means to (inviso) init tracing at it according to saved
%% tracer data generator arguments for the current session and then redo the current
%% history to bring it up to speed. Note that the tool must be running a session
%% for reinitiate to work.
reinitiate_session() ->
    gen_server:call(?MODULE,{reinitiate_session,local_runtime},?CALL_TIMEOUT).
reinitiate_session(Nodes) ->
    gen_server:call(?MODULE,{reinitiate_session,Nodes},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% restore_session()=
%% restore_session(MoreTDGargs)=
%% restore_session(FileName)=
%% restore_session(FileName,MoreTDGargs)={ok,{SessionNr,InvisoReturn}} | {error,Reason}
%% The two first clauses will start a new session using the last history. This
%% implies that there must have been a session running prior.
%% The two last clauses starts a session and reads a history file and executes the
%% tracecases in it at all inactive nodes.
%% In both cases the reused or read history becomes the current histoy, just if the
%% session had been initiated manually. The tool may not
%% have a session ongoing, and nodes already tracing (nodes which were adopted)
%% are not effected. Just like when starting a session manually.
restore_session() ->
    restore_session([]).
restore_session([]) ->                      % This cant be a filename.
    gen_server:call(?MODULE,{restore_session,[]},?CALL_TIMEOUT);
restore_session(FileNameOrMoreTDGargs) ->
    case is_string(FileNameOrMoreTDGargs) of
	true ->                             % Interpret it as a filename.
	    restore_session(FileNameOrMoreTDGargs,[]);
	false ->                            % The we want to use last session history!
	    gen_server:call(?MODULE,{restore_session,FileNameOrMoreTDGargs},?CALL_TIMEOUT)
    end.
restore_session(FileName,MoreTDGargs) ->
    gen_server:call(?MODULE,{restore_session,{FileName,MoreTDGargs}},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% stop_session()={ok,{SessionNr,Result}} | {error,Reason}
%%   SessionNr=integer()
%%   Result=[{Node,NodeResult},...] | NonDistributedNodeResult
%%     NodeResult=ok | {error,Reason}
%%     NonDistributedNodeResult=[ok] | []
%% Stops inviso tracing at all participating nodes. The inviso runtime components
%% will go to state idle. It is now time to fetch the logfiles. Will most often
%% succeed. Will only return an error if the entire inviso call returned an
%% error. Not if an individual node failed stop tracing successfully.
%% Any running trace case, including reactivator processes will be terminated.
stop_session() ->
    gen_server:call(?MODULE,stop_session,?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% reset_nodes()=NodeResult | {error,Reason}
%% reset_nodes(Nodes)={ok,NodeResults} | {error,Reason}
%%   NodeResults and NodeResult as returned by inviso:clear/1 and /0.
%% Clear nodes from trace flags, trace patterns and meta trace patterns. The tool
%% must not be having a running session.
reset_nodes() ->
    gen_server:call(?MODULE,{reset_nodes,local_runtime},?CALL_TIMEOUT).
reset_nodes(Nodes) ->
    gen_server:call(?MODULE,{reset_nodes,Nodes},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% atc(TC,Id,Vars)=ok | {error,Reason}
%%   TC=atom(), name of the trace case.
%%   Id=term(), given name of this usage of TC.
%%   Vars=list(), list of variable bindings [{Var,Value},...], Var=atom(),Value=term().
%% Function activating a trace case. The trace case must be defined in the
%% trace case dictionary. The 'ok' return value is only a signal that the
%% trace case has started successfully. It may then run for as long as it is
%% programmed to run. An erroneous return value does not necessarily mean that
%% the trace case has not been executed. It rather means that is undetermined
%% what happend.
atc(TC,Id,Vars) ->
    gen_server:call(?MODULE,{atc,{TC,Id,Vars}},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% sync_atc(TC,Id,Vars)=Result | {error,Reason}
%% sync_atc(TC,Id,Vars,TimeOut)=
%%   Result=term(), what ever is returned be the last expression in the trace case.
%%   TimeOut=interger() | infinity, the max wait time for the trace case to finnish.
%% As atc/3 but waits for the trace case to finish.
sync_atc(TC,Id,Vars) ->
    gen_server:call(?MODULE,{sync_atc,{TC,Id,Vars,?SYNC_TC_TIMEOUT}},?CALL_TIMEOUT).
sync_atc(TC,Id,Vars,TimeOut) ->
    gen_server:call(?MODULE,{sync_atc,{TC,Id,Vars,TimeOut}},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% sync_rtc(TC,Vars)=Result | {error,Reason}
%% sync_rtc(TC,Vars,TimeOut)=
%%   Result=term(), what ever is returned be the last expression in the trace case.
%%   TimeOut=interger() | infinity, the max wait time for the trace case to finnish.
%% As sync_atc/3 but the trace case is not marked as activated. It is mearly placed
%% in the history. Hence with sync_rtc a trace case can be "activated" multiple time.
sync_rtc(TC,Vars) ->
    gen_server:call(?MODULE,{sync_rtc,{TC,Vars,?SYNC_TC_TIMEOUT}},?CALL_TIMEOUT).
sync_rtc(TC,Vars,TimeOut) ->
    gen_server:call(?MODULE,{sync_rtc,{TC,Vars,TimeOut}},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% dtc(TC,Id)=ok | {error,Reason}
%% Deactivates a previosly activated trace case. This function can only be used
%% on trace cases that has a deactivation defined in the trace case dictionary.
%% There is of course really no difference between a file containing an activation
%% compared to a deactivation. But to be able cancelling activations out from the
%% history log, a defined deactivation is essential.
%% As with activation, the returned 'ok' simply indicates the start of the trace
%% case.
dtc(TC,Id) ->
    gen_server:call(?MODULE,{dtc,{TC,Id}},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% sync_dtc(TC,Id)=Result | {error,Reason}
%% sync_dtc(TC,Id,TimeOut)=
%%   Synchronous deactivation of trace case. See dtc/2 and sync_atc/3 for
%%   parameters.
sync_dtc(TC,Id) ->
    gen_server:call(?MODULE,{sync_dtc,{TC,Id,?SYNC_TC_TIMEOUT}},?CALL_TIMEOUT).
sync_dtc(TC,Id,TimeOut) ->
    gen_server:call(?MODULE,{sync_dtc,{TC,Id,TimeOut}},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% inviso(Cmd,Args)=Result
%%   Cmd=atom(), the (inviso) function name that shall be called.
%%   Args=list(), the arguments to Cmd.
%%   Result=term(), the result from the inviso function call.
%% This function executes a Cmd in the inviso tool context. The inviso call will
%% be logged in history log and thereby repeated in case of a reactivation.
%% Note that this function is intended for use with inviso function API without
%% specifying any nodes, since the function call is supposed to be carried out on
%% all nodes.
%% When these functions are written to an autostart config file by the tool there
%% is supposed to be a translation to inviso_rt functions.
inviso(Cmd,Args) ->
    gen_server:call(?MODULE,{inviso,{Cmd,Args}},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% reactivate()=ok | {error,Reason}
%% reactivate(Node)=ok | {error,Reason}
%% Moves a runtime component from suspended to the state running. This can be
%% done for both tracing and inactive nodes. The later is necessary since you
%% may have stopped tracing with a node suspended.
%% In case the node is tracing, commands in the command history log are redone at
%% the node in questions.
%% Note that this function returns 'ok' before the node is running. This because the
%% the reactivated history is done by a separate process and there is no guarantee
%% when it will be ready. The reactivated node will not be marked as running in
%% the tool until done reactivating.
%% Further it is important to understand that if there are "ongoing" tracecases
%% (i.e tracecase scripts that are currently executing) and this node was running
%% at the time that tracecase script started to execute, the list of nodes bound
%% to the Nodes variable in that script executer includes this node. Making it
%% no longer suspended makes it start executing inviso commands from where ever
%% such are called. Hence the reactivation may be interferred by that tracecase.
reactivate() ->                             % Non-distributed API.
    reactivate(node()).
reactivate(Node) ->
    gen_server:call(?MODULE,{reactivate,Node},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% save_history(FileName)={ok,AbsFileName} | {error,Reason}
%% Saves the currently collected command history log to a file. The file will
%% be a binary-file. If FileName is an absolute path, it will be saved to that
%% file. Otherwise the history dir will be used. If no history dir was specified
%% the tool dir will be used, prepended to FileName.
save_history(FileName) ->
    gen_server:call(?MODULE,{save_history,FileName},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% get_autostart_data(Nodes,Dependency)={ok,{AutoStartData,NodeResults} |
%%     {ok,{AutoStartData,NodeResult}} | {error,Reason}
%%   Dependency=inviso dependency parameter which will be used for every
%%     autostarted runtime component (included in Options).
%%   NodeResults=[{Node,NodeResult},...]
%%     NodeResult={ok,{Options,{tdg,{M,F,CompleteTDGargs}}}} | {error,Reason}
%%       Options=add_nodes options to the inviso runtime component.
%%       M,F=atom(), the module and function for tracerdata generation.
%%       CompleteTDGargs=list(), all arguments as they are given to the tracer
%%         data generator function.
%%       AutostartData=[CaseSpec,...]
%%         CaseSpec={file,{FileName,Bindings}} | {mfa,{M,F,Args}}
%%           FileName=string(), pointing out the trace case file. Note that this
%%             is the same as the path used by the tool.
%%           Bindings=Var bindings used according to the history for the
%%             invocation.
%%           M,F=atom(), the function that shall be called (normally some inviso).
%%           Args=list(), the actual arguments. Note that this may contain things
%%             which can not be written to file (ports, pids,...).
%% Function returning information on how to autostart a node to make it trace
%% according to the current history. The inviso_tool does not know how to write
%% the necessary files at the nodes in question. That must be done by the user
%% of the tool, guided by the return value from this function.
%% Note that there will be two types of trace case files. Regular trace case
%% files and binaries returned from this function. The latter contains the
%% inviso commands which have been executed. Note that the order amongst the
%% trace cases and binaries is of importance (otherwise they will be redone in
%% an incorrect order).
get_autostart_data(Dependency) ->
    gen_server:call(?MODULE,{get_autostart_data,Dependency},?CALL_TIMEOUT).
get_autostart_data(Nodes,Dependency) ->
    gen_server:call(?MODULE,{get_autostart_data,{Nodes,Dependency}},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% get_activities()={ok,Ongoing} | {error,Reason}
%%   Ongoing=list(); [ [TraceCases] [,Reactivators] ]
%%     TraceCases={tracecases,TraceCaseList}
%%       TraceCaseList=[{{TCname,Id},Phase},...]
%%         Phase=activating | deactivating
%%     Reactivators={reactivating_nodes,ReactivatingNodes}
%%       ReactivatingNodes=[Node,...]
%% Returns a list of assynchronous tracecases and nodes doing reactivation at
%% this momement. This can be useful to implement "home brewn" synchronization,
%% waiting for the runtime components to reach a certain state.
get_activities() ->
    gen_server:call(?MODULE,get_activities,?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% get_status(Node)={ok,StateStatus} | {error,Reason}
%%   StateStatus={State,Status} | reactivating | down
%%     State=tracing | inactive | trace_failure
%%     Status=running | suspended
get_node_status() ->
    get_node_status(local_runtime).
get_node_status(Node) ->
    gen_server:call(?MODULE,{get_node_status,Node},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% get_session_data()={ok,{Status,SessionNr,TDGargs}} | {error,Reason}
%%   Status=tracing | not_tracing, info about current/last session.
%%   SessionNr=integer()
%%   TDGargs=list(), list of the arguments that will be given to the tracer data
%%     generator function (not including the leading Nodes list).
%% Returns data about the current or last session.
get_session_data() ->
    gen_server:call(?MODULE,get_session_data,?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% flush()={ok,NodeResults} | NodeResult | {error,Reason}
%% flush(Nodes)={ok,NodesResults} | {error,Reason}
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult=ok | {error,Reason}
%% Makes runtime components flush their trace ports.
flush() ->
    gen_server:call(?MODULE,flush,?CALL_TIMEOUT).
flush(Nodes) ->
    gen_server:call(?MODULE,{flush,Nodes},?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% get_loopdata()=#ld
%% Debug API returning the internal loopdata structure. See #ld above for details.
get_loopdata() ->
    gen_server:call(?MODULE,get_loopdata,?CALL_TIMEOUT).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Internal APIs.
%% -----------------------------------------------------------------------------

%% tc_executer_reply(To,Reply)=nothing significant
%%   To=pid()
%%   Reply=term()
%% Internal API used by a trace case executer process to signal its completion.
tc_executer_reply(To,Reply) ->
    gen_server:cast(To,{tc_executer_reply,Reply}).
%% -----------------------------------------------------------------------------

%% Internal API used by a reactivator process indicating it is done with the
%% history log it has got so far.
%% Timeout set to infinity since the tool may be busy, then the reactivator just
%% have to wait. If the tool crashes the reactivator will be go down too automatically.
reactivator_reply(TPid,Counter) ->
    gen_server:call(TPid,{reactivator_reply,{Counter,self()}},infinity).
%% -----------------------------------------------------------------------------


%% =============================================================================
%% gen_server implementation.
%% =============================================================================

init(Config) ->
    case fetch_configuration(Config) of     % From conf-file and Config.
	{ok,LD} when record(LD,ld) ->
	    case start_inviso_at_c_node(LD) of
		{ok,CPid} ->
		    LD2=start_runtime_components(LD),
		    LD3=read_trace_case_definitions(LD2),
		    process_flag(trap_exit,true),
		    start_subscribe_inviso_events(LD3#ld.c_node),
		    {ok,LD3#ld{c_pid=CPid}};
		{error,Reason} ->           % Most likely already running.
		    {stop,{error,Reason}}
	    end;
	{error,Reason} ->
	    {stop,{error,{start_up,Reason}}}
    end.
%% -----------------------------------------------------------------------------

%% Help function starting the inviso control component at node c_node, or "here"
%% if it is not a distributed network.
start_inviso_at_c_node(#ld{c_node=undefined}) -> % Non distributed case.
    case inviso:start() of
	{ok,Pid} ->
	    {ok,Pid};
	{error,Reason} ->
	    {error,Reason}
    end;
start_inviso_at_c_node(#ld{c_node=CNode}) ->
    case rpc:call(CNode,inviso,start,[]) of
	{ok,Pid} ->
	    {ok,Pid};
	{error,{already_started,_}} ->      % A control component already started.
	    {error,{inviso_control_already_running,CNode}};
	{error,Reason} ->
	    {error,Reason};
	{badrpc,Reason} ->
	    {error,{inviso_control_node_error,Reason}}
    end.
%% -----------------------------------------------------------------------------

%% Help function starting the runtime components at all particapting nodes.
%% It also updates the nodes structure in the #ld to indicate which nodes where
%% successfully started. Returns a new #ld.
%% Note that a runtime component may actually be running at one or several nodes.
%% This is supposed to be the result of an (wanted) autostart. Meaning that the
%% inviso tool can not handle the situation if a runtime component is not doing
%% what it is supposed to do. In case a runtime component is already running it
%% will be adopted and therefore marked as running.
start_runtime_components(LD=#ld{c_node=undefined}) ->
    start_runtime_components_2(local_runtime,undefined,LD);
start_runtime_components(LD=#ld{c_node=CNode,nodes=NodesD}) ->
    start_runtime_components_2(get_all_nodenames_nodes(NodesD),CNode,LD).
start_runtime_components(Nodes,LD=#ld{c_node=CNode}) ->
    start_runtime_components_2(Nodes,CNode,LD).

start_runtime_components_2(local_runtime,CNode,LD=#ld{optg=OptG}) ->
    Opts=start_runtime_components_mk_opts(local_runtime,OptG),
    case inviso:add_node(mk_rt_tag(),Opts) of
	{ok,NAnsw} ->                       % Should be more clever really!
	    NewNodesD=update_added_nodes(CNode,{ok,NAnsw},LD#ld.nodes),
	    LD#ld{nodes=NewNodesD};
	{error,_Reason} ->
	    LD
    end;
start_runtime_components_2([Node|Rest],CNode,LD=#ld{optg=OptG}) ->
    Opts=start_runtime_components_mk_opts(Node,OptG),
    case rpc:call(CNode,inviso,add_nodes,[[Node],mk_rt_tag(),Opts]) of
	{ok,NodeResults} ->
	    NewNodesD=update_added_nodes(CNode,NodeResults,LD#ld.nodes),
	    start_runtime_components_2(Rest,CNode,LD#ld{nodes=NewNodesD});
	{error,_Reason} ->
	    start_runtime_components_2(Rest,CNode,LD);
	{badrpc,_Reason} ->
	    start_runtime_components_2(Rest,CNode,LD)
    end;
start_runtime_components_2([],_,LD) ->
    LD.

start_runtime_components_mk_opts(Node,{M,F,Args}) ->
    case catch apply(M,F,[Node|Args]) of
	{ok,Opts} when list(Opts) ->
	    start_runtime_component_mk_opts_add_dependency(Opts);
	_ ->
	    [?DEFAULT_DEPENDENCY]
    end.

%% The options generator is not supposed to generate the dependency. Hence this
%% function adds and if necessary removes an incorrectly added dependency tag.
start_runtime_component_mk_opts_add_dependency(Opts) ->
    case lists:keysearch(dependency,1,Opts) of
	{value,_} ->                           % Not allowed!!!
	    [?DEFAULT_DEPENDENCY|lists:keydelete(dependecy,1,Opts)];
	false ->
	    [?DEFAULT_DEPENDENCY|Opts]
    end.
%% -----------------------------------------------------------------------------

%% Help function subscribing to inviso events from the inviso controller. This
%% will make it possible to follow runtime components going down.
start_subscribe_inviso_events(undefined) ->
    inviso:subscribe();
start_subscribe_inviso_events(CNode) ->
    rpc:call(CNode,inviso,subscribe,[self()]). % Don't want the rpc-proc to subscribe!
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% gen_server handle call back functions.
%% -----------------------------------------------------------------------------

handle_call({stop,UntouchedNodes},_From,LD=#ld{nodes=NodesD,c_node=CNode,keep_nodes=KeepNodes})
  when is_list(UntouchedNodes) ->
    {stop,
     normal,
     remove_all_trace_patterns(CNode,
			       UntouchedNodes++KeepNodes,
			       get_available_nodes(NodesD)),
     LD};
handle_call({stop,BadArg},_From,LD) ->
    {reply,{error,{badarg,BadArg}},LD};

handle_call({reconnect_nodes,Nodes},_From,LD) ->
    case h_reconnect_nodes(Nodes,LD) of
	{ok,{Nodes2,NodesErr,NewLD}} ->
	    if
		Nodes==local_runtime ->
		    {reply,
		     build_reconnect_nodes_reply(Nodes,Nodes2,NodesErr,NewLD#ld.nodes),
		     NewLD};
		list(Nodes) ->
		    {reply,
		     {ok,build_reconnect_nodes_reply(Nodes,Nodes2,NodesErr,NewLD#ld.nodes)},
		     NewLD}
	    end;
	{error,Reason} ->
	    {reply,{error,Reason},LD}
    end;

handle_call({start_session,MoreTDGargs},_From,LD=#ld{session_state=SState}) ->
    case is_tracing(SState) of
	false ->                               % No session running.
	    if
		list(MoreTDGargs) ->
		    DateTime=calendar:universal_time(),
		    {M,F,Args}=LD#ld.tdg,
		    TDGargs=inviso_tool_lib:mk_tdg_args(DateTime,MoreTDGargs++Args),
		    case h_start_session(M,F,TDGargs,LD) of
			{ok,{SessionNr,ReturnVal,NewLD}} -> % No nodes to initiate.
			    NewLD2=add_initial_tcs_to_history(NewLD#ld.initial_tcs,
							      NewLD#ld{chl=mk_chl(LD#ld.chl)}),
			    {reply,
			     {ok,{SessionNr,ReturnVal}},
			     NewLD2#ld{session_state=tracing_sessionstate()}};
			{ok,{SessionNr,ReturnVal,Nodes2,NewLD}} ->
			    NewLD2=do_initial_tcs(NewLD#ld.initial_tcs,
						  Nodes2,
						  NewLD#ld{chl=mk_chl(LD#ld.chl)}),
			    {reply,
			     {ok,{SessionNr,ReturnVal}},
			     NewLD2#ld{session_state=tracing_sessionstate()}};
			{error,Reason} ->
			    {reply,{error,Reason},LD}
		    end;
		true ->                        % Faulty TDGargs.
		    {reply,{error,{badarg,MoreTDGargs}},LD}
	    end;
	true ->
	    {reply,{error,session_already_started},LD}
    end;

handle_call({reinitiate_session,Nodes},_From,LD=#ld{session_state=SState}) ->
    case is_tracing(SState) of
	true ->                                % The tool must be tracing.
	    {M,F,_Args}=LD#ld.tdg,
	    TDGargs=get_latest_tdgargs_tracer_data(LD#ld.tracer_data),
	    case h_reinitiate_session(Nodes,M,F,TDGargs,LD) of
		{ok,{NodesErr,ReturnVal,NewLD}} ->
		    {reply,
		     {ok,build_reinitiate_session_reply(Nodes,NodesErr,ReturnVal)},
		     NewLD};
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end;
	false ->                               % Must have a running session!
	    {reply,{error,no_session},LD}
    end;

handle_call({restore_session,{FileName,MoreTDGargs}},_From,LD=#ld{chl=OldCHL})
  when list(MoreTDGargs) ->
    case is_tracing(LD#ld.session_state) of
	false ->
	    case catch make_absolute_path(FileName,LD#ld.dir) of
		AbsFileName when list(AbsFileName) ->
		    case file:read_file(AbsFileName) of
			{ok,Bin} ->
			    if
				list(MoreTDGargs) ->
				    case catch replace_history_chl(OldCHL,
								   binary_to_term(Bin)) of
					{ok,CHL} -> % The file was well formatted.
					    case h_restore_session(MoreTDGargs,
								   LD#ld{chl=CHL}) of
						{ok,{SessionNr,ReturnVal,NewLD}} ->
						    {reply,
						     {ok,{SessionNr,ReturnVal}},
						     NewLD#ld{session_state=
							      tracing_sessionstate()}};
						{error,Reason} ->
						    {reply,{error,Reason},LD}
					    end;
					Error -> % Badly formatted file.
					    {reply,
					     {error,{bad_file,{AbsFileName,Error}}},
					     LD}
				    end;
				true ->
				    {reply,{error,{badarg,MoreTDGargs}},LD}
			    end;
			{error,Reason} ->
			    {reply,{error,{read_file,Reason}},LD}
		    end;
		Error ->
		    {reply,{error,{bad_filename,{FileName,Error}}},LD}
	    end;
	true ->
	    {reply,{error,session_already_started},LD}
    end;
%% This is doing restore session on the current history.
handle_call({restore_session,MoreTDGargs},_From,LD=#ld{chl=CHL}) ->
    case is_tracing(LD#ld.session_state) of
	false ->
	    case history_exists_chl(CHL) of
		true ->                        % There is a history to redo.
		    if
			list(MoreTDGargs) ->
			    case h_restore_session(MoreTDGargs,LD) of
				{ok,{SessionNr,ReturnVal,NewLD}} ->
				    {reply,
				     {ok,{SessionNr,ReturnVal}},
				     NewLD#ld{session_state=tracing_sessionstate()}};
				{error,Reason} ->
				    {reply,{error,Reason},LD}
			    end;
			true ->
			    {reply,{error,{badarg,MoreTDGargs}},LD}
		    end;
		false ->
		    {reply,{error,no_history},LD}
	    end;
	true ->
	    {reply,{error,session_already_started},LD}
    end;

%% To stop tracing means stop_tracing through the inviso API. But we must also
%% remove any help processes executing inviso commands (trace case executers
%% and reactivators).
%% Note that to be really sure we should actually wait for EXIT-signals from those
%% processes before returning a successful returnvalue to the caller. In theory
%% those processes could issue an inviso call effecting a new trace session started
%% with init_tracing shortly after the call to stop_tracing. But too complicated! :-)
%% Further, stop-tracing is done on all nodes in our nodes structure. Regardless
%% if the node is tracing or not
handle_call(stop_session,_From,LD=#ld{session_state=SState,chl=CHL,reactivators=ReAct}) ->
    case is_tracing(SState) of
	true ->
	    NewCHL=stop_all_tc_executer_chl(CHL), % Stop any running trace case proc.
	    NewReAct=stop_all_reactivators(ReAct),  % Stop any running reactivators.
	    case h_stop_session(LD) of
		{ok,{SessionNr,Result}} ->
		    NewNodesD=set_inactive_nodes(Result,LD#ld.nodes),
		    {reply,
		     {ok,{SessionNr,Result}},
		     LD#ld{session_state=passive_sessionstate(),
			   nodes=NewNodesD,
			   chl=NewCHL,
			   reactivators=NewReAct,
			   started_initial_tcs=[]}};
		{error,Reason} ->           % Now we're really in deep shit :-)
		    {reply,{error,{unrecoverable,Reason}},LD}
	    end;
	false ->
	    {reply,{error,no_session},LD}
    end;

handle_call({reset_nodes,Nodes},_From,LD=#ld{session_state=SState}) ->
    case is_tracing(SState) of
	false ->                            % We can not be in a session.
	    {reply,h_reset_nodes(Nodes,LD#ld.c_node),LD};
	true ->
	    {reply,{error,session_active},LD}
    end;

%% Calling a trace-case, or "turning it on".
handle_call({atc,{TC,Id,Vars}},_From,LD=#ld{session_state=SState}) ->
    case is_tracing(SState) of              % Check that we are tracing now.
	true ->
	    case h_atc(TC,Id,Vars,LD) of
		{ok,NewLD} ->               % Trace case executed.
		    {reply,ok,NewLD};
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end;
	false ->                            % Can't activate if not tracing.
	    {reply,{error,no_session},LD}
    end;

handle_call({sync_atc,{TC,Id,Vars,TimeOut}},_From,LD=#ld{session_state=SState}) ->
    case is_tracing(SState) of
	true ->
	    if
		integer(TimeOut);TimeOut==infinity ->
		    case h_sync_atc(TC,Id,Vars,TimeOut,LD) of
			{ok,NewLD,Result} ->
			    {reply,Result,NewLD};
			{error,Reason} ->
			    {reply,{error,Reason},LD}
		    end;
		true ->
		    {reply,{error,{badarg,TimeOut}},LD}
	    end;
	false ->
	    {reply,{error,no_session},LD}
    end;

handle_call({sync_rtc,{TC,Vars,TimeOut}},_From,LD=#ld{session_state=SState}) ->
    case is_tracing(SState) of
	true ->
	    if
		integer(TimeOut);TimeOut==infinity ->
		    case h_sync_rtc(TC,Vars,TimeOut,LD) of
			{ok,NewLD,Result} ->
			    {reply,Result,NewLD};
			{error,Reason} ->
			    {reply,{error,Reason},LD}
		    end;
		true ->
		    {reply,{error,{badarg,TimeOut}},LD}
	    end;
	false ->
	    {reply,{error,no_session},LD}
    end;


handle_call({dtc,{TC,Id}},_From,LD=#ld{session_state=SState}) ->
    case is_tracing(SState) of              % Check that we are tracing now.
	true ->
	    case h_dtc(TC,Id,LD) of
		{ok,NewLD} ->
		    {reply,ok,NewLD};
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end;
	false ->                            % Can't activate if not tracing.
	    {reply,{error,no_session},LD}
    end;

handle_call({sync_dtc,{TC,Id,TimeOut}},_From,LD=#ld{session_state=SState}) ->
    case is_tracing(SState) of              % Check that we are tracing now.
	true ->
	    if
		integer(TimeOut);TimeOut==infinity ->
		    case h_sync_dtc(TC,Id,TimeOut,LD) of
			{ok,NewLD,Result} ->
			    {reply,Result,NewLD};
			{error,Reason} ->
			    {reply,{error,Reason},LD}
		    end;
		true ->
		    {reply,{error,{badarg,TimeOut}},LD}
	    end;
	false ->                            % Can't activate if not tracing.
	    {reply,{error,no_session},LD}
    end;

handle_call({inviso,{Cmd,Args}},_From,LD=#ld{session_state=SState}) ->
    case is_tracing(SState) of
	true ->
	    if
		list(Args) ->
		    case h_inviso(Cmd,Args,LD) of
			{ok,{Reply,NewLD}} ->
			    {reply,Reply,NewLD};
			{error,Reason} ->
			    {reply,{error,Reason},LD}
		    end;
		true ->
		    {reply,{error,{badarg,Args}},LD}
	    end;
	false ->                            % Can't do if not tracing.
	    {reply,{error,no_session},LD}
    end;

handle_call({reactivate,Node},_From,LD=#ld{nodes=NodesD,c_node=CNode}) ->
    case get_state_nodes(Node,NodesD) of
	{trace_failure,_} ->
	    {reply,{error,trace_failure},LD};
	{State,suspended} ->                % The node is infact suspended.
	    case h_reactivate(Node,CNode) of
		ok ->
		    case {State,is_tracing(LD#ld.session_state)} of
			{tracing,true} ->   % Only then shall we redo cmds.
			    {reply,ok,redo_cmd_history(Node,LD)};
			_ ->                % All other just no longer suspended.
			    {reply,ok,LD#ld{nodes=set_running_nodes(Node,NodesD)}}
		    end;
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end;
	reactivating ->
	    {reply,{error,reactivating},LD};
	{_,running} ->
	    {reply,{error,already_running},LD};
	down ->
	    {reply,{error,not_available},LD};
	false ->
	    {reply,{error,unknown_node},LD}
    end;

handle_call({save_history,FileName},_From,LD=#ld{chl=CHL,dir=Dir,history_dir=HDir}) ->
    case lists:keysort(2,get_loglist_chl(CHL)) of
	[] ->                               % Empty history or no history.
	    {reply,{error,no_history},LD};
	Log ->
	    case h_save_history(HDir,Dir,FileName,Log) of
		{ok,AbsFileName} ->
		    {reply,{ok,AbsFileName},LD};
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end
    end;


handle_call({get_autostart_data,{Nodes,Dependency}},_From,LD=#ld{chl=CHL}) ->
    case build_autostart_data(lists:keysort(2,get_loglist_chl(CHL)),LD#ld.tc_dict) of
	{ok,ASD} ->
	    TDGargs=get_latest_tdgargs_tracer_data(LD#ld.tracer_data),
	    {M,F,_}=LD#ld.tdg,
	    OptsG=LD#ld.optg,                       % Addnodes options generator.
	    {reply,
	     h_get_autostart_data(Nodes,LD#ld.c_node,Dependency,ASD,M,F,TDGargs,OptsG),
	     LD};
	{error,Reason} ->                           % Bad datatypes in command args.
	    {reply,{error,Reason},LD}
    end;

handle_call({get_autostart_data,Dependency},From,LD=#ld{c_node=undefined}) ->
    handle_call({get_autostart_data,{local_runtime,Dependency}},From,LD);
handle_call({get_autostart_data,Dependency},From,LD=#ld{nodes=NodesD}) ->
    Nodes=get_all_nodenames_nodes(NodesD),
    handle_call({get_autostart_data,{local_runtime,{Nodes,Dependency}}},From,LD);

handle_call(get_activities,_From,LD=#ld{chl=CHL,reactivators=Reactivators}) ->
    TraceCases=get_ongoing_chl(CHL),
    RNodes=get_all_nodes_reactivators(Reactivators),
    ReturnList1=
	if
	    TraceCases==[] ->
		[];
	    true ->
		[{tracecases,TraceCases}]
	end,
    ReturnList2=
	if
	    RNodes==[] ->
		ReturnList1;
	    true ->
		[{reactivating_nodes,RNodes}|ReturnList1]
	end,
    {reply,{ok,ReturnList2},LD};

handle_call({get_node_status,Node},_Node,LD) ->
    case get_state_nodes(Node,LD#ld.nodes) of
	false ->
	    {reply,{error,unknown_node},LD};
	StateStatus ->
	    {reply,{ok,StateStatus},LD}
    end;

handle_call(get_session_data,_From,LD=#ld{session_state=SState,tracer_data=TD}) ->
    case get_latest_session_nr_tracer_data(TD) of
	undefined ->
	    {reply,{error,no_session},LD};
	SessionNr ->
	    TDGargs=get_latest_tdgargs_tracer_data(TD),
	    case is_tracing(SState) of
		true ->
		    {reply,{ok,{tracing,SessionNr,TDGargs}},LD};
		false ->
		    {reply,{ok,{not_tracing,SessionNr,TDGargs}},LD}
	    end
    end;

handle_call(flush,_From,LD=#ld{c_node=CNode,nodes=NodesD}) ->
    Nodes=get_tracing_nodes(NodesD),
    {reply,h_flush(CNode,Nodes),LD};
handle_call({flush,Nodes},_From,LD=#ld{c_node=CNode}) ->
    {reply,h_flush(CNode,Nodes),LD};

handle_call(get_loopdata,_From,LD) ->
    {reply,LD,LD};

%% Internal handle_call callbacks.

handle_call({reactivator_reply,{Counter,RPid}},_From,LD=#ld{chl=CHL}) ->
    HighestUsedCounter=get_highest_used_counter_chl(CHL),
    if
	HighestUsedCounter>Counter ->       % There are now more log entries.
	    NewUnsortedLog=get_loglist_chl(CHL),
	    {reply,{more,NewUnsortedLog},LD};
	true ->                             % No Counter is youngest log entry.
	    NodesD=LD#ld.nodes,
	    Node=get_node_reactivators(RPid,LD#ld.reactivators),
	    {reply,
	     done,
	     LD#ld{nodes=set_running_nodes(Node,NodesD),
		   reactivators=del_reactivators(RPid,LD#ld.reactivators)}}
    end.
%% -----------------------------------------------------------------------------

%% Handling a notification from a trace case execution process. Receiving this
%% indicated that this phase of the trace case is finnished.
handle_cast({tc_executer_reply,{Phase,ProcH,Result}},LD) ->
    case Phase of
	activating ->                       % The trace case is running now.
	    {ok,NewLD}=h_tc_activation_done(ProcH,Result,LD),
	    {noreply,NewLD};
	stopping ->
	    {ok,NewLD}=h_tc_stopping_done(ProcH,Result,LD),
	    {noreply,NewLD};
	_ ->
	    {noreply,LD}
    end;
handle_cast(_,LD) ->
    {noreply,LD}.
%% -----------------------------------------------------------------------------

%% This is the case when a runtime component goes down. We stop all running
%% reactivators for this node. Note that there can also be tracecases ongoing
%% where this node is part of the Nodes variable. But there is not much we can
%% do about that. Other then informing the user that it is unwise to reconnect
%% this node before those tracecases have stopped being ongoing.
handle_info({inviso_event,_CNode,_Time,{disconnected,Node,_}},LD) ->
    {noreply,LD#ld{nodes=set_down_nodes(Node,LD#ld.nodes),
		   reactivators=stop_node_reactivators(Node,LD#ld.reactivators)}};

%% This is the case when a runtime component gets suspended. Much of the same
%% problem as described above applies.
handle_info({inviso_event,_CNode,_Time,{state_change,Node,{_,{suspended,_}}}},LD) ->
    {noreply,LD#ld{nodes=set_suspended_nodes(Node,LD#ld.nodes),
		   reactivators=stop_node_reactivators(Node,LD#ld.reactivators)}};

handle_info(_,LD) ->
    {noreply,LD}.
%% -----------------------------------------------------------------------------

%% Called when the tool server stops. First clause, termination is initiated by
%% our self and therefore controlled another way. In the second case we are
%% stopping for some external reason, and we must then do more here in terminate/2.
terminate(normal,#ld{c_node=CNode}) ->      % This is when we are stopping our self.
    stop_inviso_at_c_node(CNode);
terminate(_,#ld{c_node=CNode,nodes=NodesD,keep_nodes=KeepNodes}) ->
    remove_all_trace_patterns(CNode,KeepNodes,get_all_nodenames_nodes(NodesD)),
    stop_inviso_at_c_node(CNode).
%% -----------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =============================================================================
%% Handler first level help functions.
%% =============================================================================

%% -----------------------------------------------------------------------------
%% reconnect_nodes
%% -----------------------------------------------------------------------------

%% Help function reconnecting the nodes in Nodes. Listed nodes must be part of
%% the set of nodes handled by the tool. It is not possible to reconnect a node
%% that is not marked as down. This partly because we otherwise risk losing the
%% trace_failure state (which can not be rediscovered).
h_reconnect_nodes(local_runtime,LD=#ld{nodes=NodesD}) -> % Non-distributed.
    case get_state_nodes(local_runtime,NodesD) of
	down ->
	    {ok,{local_runtime,[],start_runtime_components(local_runtime,LD)}};
	_ ->                                   % Allready connected!
	    {ok,{[],{error,already_connected},LD}}
    end;
h_reconnect_nodes(Nodes,LD=#ld{nodes=NodesD}) when list(Nodes) ->
    {Nodes2,NodesErr}=
	lists:foldl(fun(N,{Nodes2,NodesErr})->
			    case get_state_nodes(N,NodesD) of
				down ->        % Yes this node can be reconnected.
				    {[N|Nodes2],NodesErr};
				false ->       % Not part of the node-set!
				    {Nodes2,[{N,{error,unknown_node}}|NodesErr]};
				_ ->           % Allready connected!
				    {Nodes2,[{N,{error,already_connected}}|NodesErr]}
			    end
		    end,
		    {[],[]},
		    Nodes),
    LD2=start_runtime_components(Nodes2,LD),   % Inpect the #ld.nodes for result.
    {ok,{Nodes2,NodesErr,LD2}};
h_reconnect_nodes(Nodes,_LD) ->
    {error,{badarg,Nodes}}.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% start_session
%% -----------------------------------------------------------------------------

%% Help function starting the tracing at all nodes. Note that the tracer data
%% is calculated using a user defined function. This is how for instance the
%% file names (of the log files) are determined.
%% Before the nodes are initiated their (possibly remaining) trace patterns are
%% cleared, both local and global.
h_start_session(M,F,TDGargs,LD=#ld{c_node=CNode,nodes=NodesD,tracer_data=TDs}) ->
    case get_inactive_running_nodes(NodesD) of
	[] ->                               % There are no nodes to initiate!
	    h_start_session_nonodes(TDGargs,LD,[]);
	Nodes ->                            % List of nodes or 'local_runtime'.
	    case h_start_session_ctp_all(CNode,Nodes) of
		{ok,Errors,[]} ->           % Now no nodes to initiate!
		    h_start_session_nonodes(TDGargs,LD,Errors);
		{ok,Errors,Nodes2} ->       % Now these nodes are fresh.
		    case call_tracer_data_generator(CNode,M,F,TDGargs,Nodes2) of
			{ok,TracerList} ->  % Generated our tracerdata.
			    case h_start_session_2(CNode,TracerList,Errors) of
				{ok,ReturnValue} -> % Some nodes are initialized now.
				    {NewNodesD,Nodes3}=
					set_tracing_running_nodes(CNode,ReturnValue,NodesD),
				    {SessionNr,NewTDs}=insert_td_tracer_data(TDGargs,TDs),
				    {ok,{SessionNr,
					 ReturnValue,
					 Nodes3, % The nodes that shall get initial tracases.
					 LD#ld{nodes=NewNodesD,tracer_data=NewTDs}}};
				{error,Reason} ->
				    {error,Reason}
			    end;
			{error,Reason} ->   % Faulty tracer data generator func.
			    {error,{bad_tdg,Reason}}
		    end;
		{error,Reason} ->           % Error clearing patterns.
		    {error,Reason}
	    end
    end.

h_start_session_nonodes(TDGargs,LD=#ld{c_node=CNode,tracer_data=TDs},Errors) ->
    {SessionNr,NewTDs}=insert_td_tracer_data(TDGargs,TDs),
    if
	CNode==undefined ->
	    {ok,{SessionNr,[],LD#ld{tracer_data=NewTDs}}};
	true ->
	    {ok,{SessionNr,{ok,Errors},LD#ld{tracer_data=NewTDs}}}
    end.

%% Help function clearing all trace patterns on all nodes.
h_start_session_ctp_all(CNode,Nodes) ->
    case remove_all_trace_patterns(CNode,[],Nodes) of
	ok ->                               % Non-distributed case1.
	    {ok,[],local_runtime};
	{error,Reason} ->                   % Non-distributed case2 and general failure.
	    {error,Reason};
	{ok,NodeResults} ->
	    h_start_session_ctp_all_2(NodeResults,[],[])
    end.

h_start_session_ctp_all_2([{Node,{error,Reason}}|Rest],Errors,Nodes) ->
    h_start_session_ctp_all_2(Rest,[{Node,{error,Reason}}|Errors],Nodes);
h_start_session_ctp_all_2([{Node,_OkOrPatternsUntouched}|Rest],Errors,Nodes) ->
    h_start_session_ctp_all_2(Rest,Errors,[Node|Nodes]);
h_start_session_ctp_all_2([],Errors,Nodes) ->
    {ok,Errors,Nodes}.

%% Help function doing the actual init_tracing.
h_start_session_2(undefined,TracerData,_Errors) -> % Non distributed case.
    case inviso:init_tracing(TracerData) of
	{ok,LogResult} when list(LogResult) ->
	    {ok,{ok,LogResult}};
	{error,already_initated} ->         % Perhaps adopted!?
	    {ok,{error,already_initiated}}; % Not necessarily wrong.
	{error,Reason} ->
	    {error,Reason}
    end;
h_start_session_2(CNode,TracerList,Errors) ->
    case rpc:call(CNode,inviso,init_tracing,[TracerList]) of
	{ok,NodeResults} ->
	    {ok,{ok,Errors++NodeResults}};
	{error,Reason} ->
	    {error,Reason};
	{badrpc,Reason} ->
	    {error,{inviso_control_node_error,Reason}}
    end.
%% -----------------------------------------------------------------------------

%% Help function starting all initial trace cases. They are actually handled
%% the same way as user started trace cases. We actually only start initial
%% tracecases at Nodes (if Nodes is a list of nodes). This because we may have
%% adopted some nodes some already tracing nodes, and such are supposed to have
%% the correct patterns and flags set.
do_initial_tcs([{TC,Vars}|Rest],Nodes,LD) ->
    Id=make_ref(),                          % Trace case ID.
    case h_atc(TC,Id,Vars,LD,Nodes) of      % Start using regular start methods.
	{ok,NewLD} ->                       % Trace case was successfully started.
	    NewInitialTcs=add_initial_tcs(TC,Id,NewLD#ld.started_initial_tcs),
	    do_initial_tcs(Rest,Nodes,NewLD#ld{started_initial_tcs=NewInitialTcs});
	{error,_Reason} ->
	    do_initial_tcs(Rest,Nodes,LD)
    end;
do_initial_tcs([_|Rest],Nodes,LD) ->
    do_initial_tcs(Rest,Nodes,LD);
do_initial_tcs([],_Nodes,LD) ->
    LD.
%% -----------------------------------------------------------------------------

%% This help functio is used instead of do_initial_tcs/3 if there actually are no
%% nodes to do the trace cases on. The reason we must have this function is that
%% the tracecases must still be entered into the history with bindings and all.
%% But we let them be marked as 'running' immediately (no need for the activator
%% process).
add_initial_tcs_to_history([{TC,Vars}|Rest],LD=#ld{tc_dict=TCdict,chl=CHL}) ->
    case get_tracecase_tc_dict(TC,TCdict) of
	{ok,TraceCase} ->
	    case check_bindings(Vars,TraceCase) of
		{ok,Bindings} ->
		    Id=make_ref(),          % Trace case ID.
		    FakeProcH=make_ref(),   % Need something to enter as activator.
		    NewCHL=set_activating_chl(TC,Id,CHL,Bindings,FakeProcH),
		    NewCHL2=set_running_chl(FakeProcH,TC,Id,void,NewCHL), % Result=void.
		    NewInitialTcs=add_initial_tcs(TC,Id,LD#ld.started_initial_tcs),
		    add_initial_tcs_to_history(Rest,LD#ld{chl=NewCHL2,
							  started_initial_tcs=NewInitialTcs});
		{error,_Reason} ->          % Not much we can do about that.
		    add_initial_tcs_to_history(Rest,LD)
	    end;
	false ->
	    add_initial_tcs_to_history(Rest,LD)
    end;
add_initial_tcs_to_history([],LD) ->
    LD.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% reinitiate_session
%% -----------------------------------------------------------------------------

%% Function doing the reinitiation. That means first do init_tracing at the nodes
%% in question. Then redo the command history to bring them up to speed.
%% But first the runtime component is cleared of all trace patterns.
h_reinitiate_session(Nodes,M,F,TDGargs,LD=#ld{c_node=CNode,nodes=NodesD}) ->
    case h_reinitiate_session_2(Nodes,NodesD,CNode) of
	{ok,{[],NodesErr}} ->               % No nodes to reinitiate.
	    {ok,{NodesErr,{ok,[]},LD}};
	{ok,{Nodes2,NodesErr}} ->           % List of nodes or local_runtime.
	    case call_tracer_data_generator(CNode,M,F,TDGargs,Nodes2) of
		{ok,TracerList} ->
		    case h_start_session_2(CNode,TracerList,[]) of % Borrow from start_session.
			{ok,ReturnValue} -> % Ok, now we must redo cmd history.
			    {NewNodesD,_Nodes}=
				set_tracing_running_nodes(CNode,ReturnValue,NodesD),
			    NewLD=h_reinitiate_session_chl(Nodes2,LD#ld{nodes=NewNodesD}),
			    {ok,{NodesErr,ReturnValue,NewLD}};
			{error,Reason} ->
			    {error,Reason}
		    end;
		{error,Reason} ->
		    {error,{bad_tdg,Reason}}
	    end;
	{error,Reason} ->
	    {error,Reason}
    end.

%% Help function finding out which nodes in Nodes actually can be reinitiated.
%% A node must be up, inactive and not suspended in order for this to work. All the
%% rest is just a matter of how detailed error return values we want to generate.
h_reinitiate_session_2(local_runtime,NodesD,undefined) -> % Non distributed case.
    case get_state_nodes(local_runtime,NodesD) of
	{inactive,running} ->               % Only ok case.
	    case inviso:ctp_all() of
		ok ->
		    {ok,{local_runtime,[]}};
		{error,Reason} ->           % This is strange.
		    {error,Reason}
	    end;
	{_,suspended} ->
	    {ok,{[],{error,suspended}}};
	down ->
	    {ok,{[],{error,down}}};
	_ ->
	    {ok,{[],{error,already_in_session}}}
    end;
h_reinitiate_session_2(Nodes,NodesD,CNode) when list(Nodes) ->
    {ok,lists:foldl(fun(N,{Nodes2,NodesErr})->
			    case get_state_nodes(N,NodesD) of
				{inactive,running} -> % Only ok case.
				    case rpc:call(CNode,inviso,ctp_all,[[N]]) of
					{ok,[{N,ok}]} ->
					    {[N|Nodes2],NodesErr};
					{ok,[{N,{error,Reason}}]} ->
					    {Nodes2,[{N,{error,Reason}}|NodesErr]};
					{error,Reason} ->
					    {Nodes2,[{N,{error,Reason}}|NodesErr]};
					{badrpc,Reason} ->
					    {Nodes2,[{N,{error,{badrpc,Reason}}}|NodesErr]}
				    end;
				{_,suspended} ->
				    {Nodes2,[{N,{error,suspended}}|NodesErr]};
				down ->
				    {Nodes2,[{N,{error,down}}|NodesErr]};
				false ->
				    {Nodes2,[{N,{error,unknown_node}}|NodesErr]};
				_ ->
				    {Nodes2,[{N,{error,already_in_session}}|NodesErr]}
			    end
		    end,
		    {[],[]},
		    Nodes)};
h_reinitiate_session_2(Nodes,_NodesD,_CNode) ->
    {error,{badarg7,Nodes}}.

%% Help function redoing the command history log at all nodes that actually
%% started to trace. Note that we do not modify the return value which will be
%% given to the caller just because we decide not to redo commands. The user
%% must conclude him self from the inviso return value that commands were not
%% redone at a particular node.
h_reinitiate_session_chl(local_runtime,LD) ->
    h_reinitiate_session_chl([local_runtime],LD);
h_reinitiate_session_chl([Node|Rest],LD=#ld{nodes=NodesD}) ->
    case get_state_nodes(Node,NodesD) of
	{tracing,running} ->                % Only case when we shall redo!
	    h_reinitiate_session_chl(Rest,redo_cmd_history(Node,LD));
	_ ->                                % No redo of chl in other cases.
	    h_reinitiate_session_chl(Rest,LD)
    end;
h_reinitiate_session_chl([],LD) ->
    LD.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% restore_session
%% -----------------------------------------------------------------------------

%% Help function starting a session (init tracing) and redoes the history
%% found in CHL.
h_restore_session(MoreTDGargs,LD) ->
    DateTime=calendar:universal_time(),
    {M,F,Args}=LD#ld.tdg,
    TDGargs=inviso_tool_lib:mk_tdg_args(DateTime,MoreTDGargs++Args),
    case h_start_session(M,F,TDGargs,LD) of
	{ok,{SessionNr,ReturnVal,NewLD}} ->   % There were no available nodes.
	    {ok,{SessionNr,ReturnVal,NewLD}};
	{ok,{SessionNr,ReturnVal,Nodes2,NewLD}} ->
	    NewLD2=h_reinitiate_session_chl(Nodes2,NewLD),
	    {ok,{SessionNr,ReturnVal,NewLD2}};
	{error,Reason} ->                     % Risk of out of control.
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% stop_session
%% -----------------------------------------------------------------------------

%% Help function stopping tracing at tracing nodes.
h_stop_session(#ld{c_node=CNode,nodes=NodesD,tracer_data=TDs}) ->
    case h_stop_session_2(CNode,NodesD) of
	{ok,Result} ->
	    {ok,{get_latest_session_nr_tracer_data(TDs),Result}};
	{error,Reason} ->
	    {error,Reason}
    end.

h_stop_session_2(undefined,NodesD) ->  % The non distributed case.
    case get_tracing_nodes(NodesD) of
	{up,{inactive,_}} ->           % Already not tracing!
	    {ok,[]};
	{up,_} ->
	    case inviso:stop_tracing() of
		{ok,_State} ->
		    {ok,[ok]};
		{error,no_response} ->
		    {ok,[]};
		{error,Reason} ->
		    {error,Reason}
	    end;
	down ->
	    {ok,[]}
    end;
h_stop_session_2(CNode,NodesD) ->
    Nodes=get_tracing_nodes(NodesD),
    case rpc:call(CNode,inviso,stop_tracing,[Nodes]) of
	{ok,NodeResults} ->
	    {ok,lists:map(fun({N,{ok,_}})->{N,ok};
			     (NodeError)->NodeError
			  end,
			  NodeResults)};
	{error,Reason} ->
	    {error,Reason};
	{badrpc,Reason} ->
	    {error,{inviso_control_node_error,Reason}}
    end.
%% -----------------------------------------------------------------------------

%% Help function removing any trace flags, trace patterns and meta trace patterns
%% at Nodes. This will cause the nodes to become "fresh".
h_reset_nodes(local_runtime,_CNode) ->
    inviso:clear([keep_log_files]);
h_reset_nodes(Nodes,CNode) ->
    case inviso_tool_lib:inviso_cmd(CNode,clear,[Nodes,[keep_log_files]]) of
	{ok,NodeResults} ->
	    {ok,NodeResults};
	{error,Reason} ->
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% atc
%% -----------------------------------------------------------------------------

%% Function handling ativating a trace case. Trace cases that do not have a
%% particular on/off handling (but just on in some scense) are handled here too.
%% The trace case is entered into the Command History Log.
%% Note that the trace case can not be executed at this node but must be
%% executed where the inviso control component is.
%% Further it is possible to either activated the tracecase for all running and
%% tracing nodes, or just for a specified list of nodes.
%%  TC=tracecase_name(),
%%  Id=term(), identifiying this usage so we can turn it off later.
%%  Vars=list(), list of variable-value bindnings.
h_atc(TC,Id,Vars,LD) ->
    h_atc(TC,Id,Vars,LD,void).              % For all running-tracing nodes.

h_atc(TC,Id,Vars,LD=#ld{c_node=CNode,tc_dict=TCdict,chl=CHL},Nodes) ->
    case find_id_chl(TC,Id,CHL) of
	activating ->                       % Already started.
	    {error,activating};
	stopping ->                         % Not yet stopped.
	    {error,deactivating};
	false ->
	    case get_tracecase_tc_dict(TC,TCdict) of
		{ok,TraceCase} ->           % Such a trace case exists.
		    case check_bindings(Vars,TraceCase) of
			{ok,Bindings} ->    % Necessary vars exists in Vars.
			    if
				list(Nodes) -> % Nodes predefined.
				    h_atc_2(TC,Id,CNode,CHL,LD,TraceCase,Bindings,Nodes);
				true ->     % Use all tracing and running nodes.
				    Nodes1=get_nodenames_running_nodes(LD#ld.nodes),
				    h_atc_2(TC,Id,CNode,CHL,LD,TraceCase,Bindings,Nodes1)
			    end;
			{error,Reason} ->   % Variable def missing.
			    {error,Reason}
		    end;
		false ->
		    {error,unknown_tracecase}
	    end;
	{ok,_Bindings} ->                   % Already activated and running.
	    {error,already_started}
    end.

h_atc_2(TC,Id,CNode,CHL,LD,TraceCase,Bindings,Nodes) ->
    case exec_trace_case_on(CNode,TraceCase,Bindings,Nodes) of
	{ok,ProcH} ->                       % Trace cases have no return values.
	    NewCHL=set_activating_chl(TC,Id,CHL,Bindings,ProcH),
	    {ok,LD#ld{chl=NewCHL}};
	{error,Reason} ->
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% sync_atc
%% -----------------------------------------------------------------------------

h_sync_atc(TC,Id,Vars,TimeOut,LD=#ld{c_node=CNode,tc_dict=TCdict,chl=CHL}) ->
    case find_id_chl(TC,Id,CHL) of
	activating ->                       % Already started.
	    {error,activating};
	stopping ->                         % Not yet stopped.
	    {error,deactivating};
	false ->
	    case get_tracecase_tc_dict(TC,TCdict) of
		{ok,TraceCase} ->           % Such a trace case exists.
		    case check_bindings(Vars,TraceCase) of
			{ok,Bindings} ->    % Necessary vars exists in Vars.
			    {ok,TcFName}=get_tc_activate_fname(TraceCase),
			    Nodes=get_nodenames_running_nodes(LD#ld.nodes),
			    Bindings2=erl_eval:add_binding('Nodes',Nodes,Bindings),
			    RpcNode=get_rpc_nodename(CNode),
			    case rpc:call(RpcNode,file,script,[TcFName,Bindings2],TimeOut) of
				{ok,Value} ->
				    FakeProcH=make_ref(),
				    NewCHL1=set_activating_chl(TC,Id,CHL,Bindings,FakeProcH),
				    NewCHL2=set_running_chl(FakeProcH,TC,Id,Value,NewCHL1),
				    {ok,LD#ld{chl=NewCHL2},Value};
				{error,Reason} ->
				    {error,{faulty_tracecase,{TcFName,Reason}}};
				{badrpc,Reason} ->
				    {error,{badrpc,Reason}}
			    end;
			{error,Reason} ->   % Variable def missing.
			    {error,Reason}
		    end;
		false ->
		    {error,unknown_tracecase}
	    end;
	{ok,_Bindings} ->                   % Already activated and running.
	    {error,already_started}
    end.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% rtc
%% -----------------------------------------------------------------------------

%% Function handling running a trace case without marking it as activated. It
%% is in the history mearly indicated as activated
h_sync_rtc(TC,Vars,TimeOut,LD=#ld{c_node=CNode,tc_dict=TCdict,chl=CHL}) ->
    case get_tracecase_tc_dict(TC,TCdict) of
	{ok,TraceCase} ->                   % Such a trace case exists.
	    case check_bindings(Vars,TraceCase) of
		{ok,Bindings} ->            % Necessary vars exists in Vars.
		    {ok,TcFName}=get_tc_activate_fname(TraceCase),
		    Nodes=get_nodenames_running_nodes(LD#ld.nodes),
		    Bindings2=erl_eval:add_binding('Nodes',Nodes,Bindings),
		    RpcNode=get_rpc_nodename(CNode),
		    case rpc:call(RpcNode,file,script,[TcFName,Bindings2],TimeOut) of
			{ok,Value} ->
			    {ok,LD#ld{chl=add_rtc_chl(TC,Bindings2,CHL)},Value};
			{error,Reason} ->
			    {error,{faulty_tracecase,{TcFName,Reason}}};
			{badrpc,Reason} ->
			    {error,{badrpc,Reason}}
		    end;
		{error,Reason} ->   % Variable def missing.
		    {error,Reason}
	    end;
	false ->
	    {error,unknown_tracecase}
    end.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% dtc
%% -----------------------------------------------------------------------------

%% Function handling turning a trace case off. The trace case must be registered
%% as having an off mechanism. If it has an off mechanism and was previously entered
%% into the Command History Log and is done with its activation phase, it will be
%% executed and removed from the CHL.
h_dtc(TC,Id,LD=#ld{c_node=CNode,tc_dict=TCdict,chl=CHL}) ->
    case find_id_chl(TC,Id,CHL) of
	{ok,Bindings} ->                    % Yes, we have turned it on before.
	    case get_tracecase_tc_dict(TC,TCdict) of
		{ok,TraceCase} ->
		    Nodes=get_nodenames_running_nodes(LD#ld.nodes),
		    case exec_trace_case_off(CNode,TraceCase,Bindings,Nodes) of
			{ok,ProcH} ->
			    NewCHL=set_stopping_chl(TC,Id,CHL,ProcH),
			    {ok,LD#ld{chl=NewCHL}};
			{error,Reason} ->
			    {error,Reason}
		    end;
		false ->                    % Strange, Id ok but no such trace case.
		    {error,unknown_tracecase}
	    end;
	false ->                            % Not previously turned on.
	    {error,unknown_id};
	activating ->
	    {error,activating};
	stopping ->
	    {error,already_deactivating}
    end.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% sync_dtc
%% -----------------------------------------------------------------------------

h_sync_dtc(TC,Id,TimeOut,LD=#ld{c_node=CNode,tc_dict=TCdict,chl=CHL}) ->
    case find_id_chl(TC,Id,CHL) of
	{ok,Bindings} ->                    % Yes, we have turned it on before.
	    case get_tracecase_tc_dict(TC,TCdict) of
		{ok,TraceCase} ->
		    case get_tc_deactivate_fname(TraceCase) of
			{ok,TcFName} ->
			    Nodes=get_nodenames_running_nodes(LD#ld.nodes),
			    Bindings2=erl_eval:add_binding('Nodes',Nodes,Bindings),
			    RpcNode=get_rpc_nodename(CNode),
			    case rpc:call(RpcNode,file,script,[TcFName,Bindings2],TimeOut) of
				{ok,Value} ->
				    FakeProcH=make_ref(),
				    NewCHL1=set_stopping_chl(TC,Id,CHL,FakeProcH),
				    NewCHL2=nullify_chl(FakeProcH,TC,Id,NewCHL1),
				    {ok,LD#ld{chl=NewCHL2},Value};
				{error,Reason} -> % Script fault.
				    {error,{faulty_tracecase,{TcFName,Reason}}};
				{badrpc,Reason} ->
				    {error,{badrpc,Reason}}
			    end;
			false ->
			    {error,no_deactivation}
		    end;
		false ->                    % Strange, Id ok but no such trace case.
		    {error,unknown_tracecase}
	    end;
	false ->                            % Not previously turned on.
	    {error,unknown_id};
	activating ->
	    {error,activating};
	stopping ->
	    {error,already_deactivating}
    end.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% inviso
%% -----------------------------------------------------------------------------

%% Function executing one inviso command. The returnvalue from the inviso
%% function call will be the return value to the client. The command is
%% entered into the history command log.
%% Note that the inviso call may have to be done at another node, dictated
%% by the c_node field. Further, if the module name is not an atom it is
%% most likely a regexp, which must be expanded at the regexp_node. Note
%% this is only relevant for tp and tpl.
h_inviso(Cmd,Args,LD=#ld{c_node=CNode,regexp_node=RegExpNode,chl=CHL}) ->
    Arity=length(Args),
    case check_proper_inviso_call(Cmd,Arity) of
	{true,RegExpFlag} ->                % Yes it is an inviso call.
	    Nodes=get_nodenames_running_nodes(LD#ld.nodes),
	    case h_inviso_2(Cmd,Args,CNode,RegExpNode,RegExpFlag,Nodes) of
		{ok,Result} ->
		    case check_inviso_call_to_history(Cmd,Arity) of
			true ->             % This function shall be added to chl.
			    {ok,{Result,LD#ld{chl=add_inviso_call_chl(Cmd,Args,CHL)}}};
			false ->            % Do not add it.
			    {ok,{Result,LD}}
		    end;
		{error,Reason} ->
		    {error,Reason}
	    end;
	false ->                            % Not an inviso function.
	    {error,invalid_function_name}
    end.

h_inviso_2(Cmd,Args,undefined,_,_,_) ->     % A non distributed system.
    case catch apply(inviso,Cmd,Args) of    % Regexp expansion only relevant when
	{'EXIT',Reason} ->                  % distributed, here let inviso_rt expand.
	    {error,{'EXIT',Reason}};
	Result ->
	    {ok,Result}
    end;
h_inviso_2(Cmd,Args,CNode,RegExpNode,RegExpFlag,Nodes) ->
    case expand_module_regexps(Args,RegExpNode,Nodes,RegExpFlag) of
	{ok,NewArgs} ->
	    case catch inviso_tool_lib:inviso_cmd(CNode,Cmd,[Nodes|NewArgs]) of
		{'EXIT',Reason} ->
		    {error,{'EXIT',Reason}};
		{error,{badrpc,Reason}} ->  % Includes runtime failure.
		    {error,{badrpc,Reason}};
		Result ->
		    {ok,Result}
	    end;
	{error,Reason} ->
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% reactivate
%% -----------------------------------------------------------------------------

h_reactivate(_Node,undefined) ->            % The non-distributed case.
    case inviso:cancel_suspension() of
	ok ->
	    ok;
	{error,Reason} ->
	    {error,Reason}
    end;
h_reactivate(Node,CNode) ->
    case inviso_tool_lib:inviso_cmd(CNode,cancel_suspension,[[Node]]) of
	{ok,[{Node,ok}]} ->
	    ok;
	{ok,[{Node,{error,Reason}}]} ->
	    {error,Reason};
	{error,Reason} ->
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% save_history
%% -----------------------------------------------------------------------------

h_save_history(HDir,Dir,FileName,SortedLog) ->
    Dir0=
	if
	    list(HDir) ->                   % There is a history dir specified.
		HDir;                       % Use it then.
	    true ->
		Dir                         % Else use the tool dir.
	end,
    case catch make_absolute_path(FileName,Dir0) of
	AbsFileName when list(AbsFileName) ->
	    Log2=build_saved_history_data(SortedLog), % Remove stopped tracecases.
	    case file:write_file(AbsFileName,term_to_binary(Log2)) of
		ok ->
		    {ok,AbsFileName};
		{error,Reason} ->
		    {error,{write_file,Reason}}
	    end;
	{'EXIT',_Reason} ->
	    {error,{bad_filename,FileName}}
    end.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%%  get_autostart_data
%% -----------------------------------------------------------------------------

%% Help function building the structures used when exporting autostart information
%% from the tool. Note that we remove the tool-dependency and insert the one
%% specify in the get_autostart_data call.
h_get_autostart_data(local_runtime,_,Dependency,ASD,M,F,TDGargs,OptsG) ->
    CompleteTDGargs=call_tracer_data_generator_mkargs(local_runtime,TDGargs),
    Opts0=start_runtime_components_mk_opts(local_runtime,OptsG),
    Opts=[Dependency|lists:keydelete(dependency,1,Opts0)],
    {ok,{ASD,{ok,{Opts,{tdg,{M,F,CompleteTDGargs}}}}}};

h_get_autostart_data(Nodes,CNode,Dependency,ASD,M,F,TDGargs,OptsG) when list(Nodes) ->
    {ok,{ASD,h_get_autostart_data_2(Nodes,CNode,Dependency,M,F,TDGargs,OptsG)}};
h_get_autostart_data(Nodes,_CNode,_Dependency,_ASD,_M,_F,_TDGargs,_OptsG) ->
    {error,{badarg,Nodes}}.

h_get_autostart_data_2([Node|Rest],CNode,Dependency,M,F,TDGargs,OptsG) ->
    CompleteTDGargs=call_tracer_data_generator_mkargs(Node,TDGargs),
    Opts0=start_runtime_components_mk_opts(Node,OptsG),
    Opts=[Dependency|lists:keydelete(dependency,1,Opts0)],
    [{Node,{ok,{Opts,{tdg,{M,F,CompleteTDGargs}}}}}|
     h_get_autostart_data_2(Rest,CNode,Dependency,M,F,TDGargs,OptsG)];
h_get_autostart_data_2([],_CNode,_Dependency,_M,_F,_TDGargs,_OptsG) ->
    [].
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% flush
%% -----------------------------------------------------------------------------

h_flush(undefined,_Nodes) ->
    inviso:flush();
h_flush(CNode,Nodes) ->
    inviso_tool_lib:inviso_cmd(CNode,flush,[Nodes]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% tc_executer_reply
%% -----------------------------------------------------------------------------

%% Function handling that a trace case has completed its activation phase and
%% shall now be marked in the Command History Log as running.
h_tc_activation_done(ProcH,Result,LD=#ld{chl=CHL}) ->
    case find_tc_executer_chl(ProcH,CHL) of
	{activating,{TC,Id}} ->
	    case Result of
		{ok,Value} ->               % The trace case is successful activated.
		    {ok,LD#ld{chl=set_running_chl(ProcH,TC,Id,Value,CHL)}};
		{error,_} ->                % Then pretend it never happend :-)
		    {ok,LD#ld{chl=del_tc_chl(ProcH,TC,Id,CHL)}} % Remove it.
	    end;
	_ ->                                % Where did this come from?
	    {ok,LD}                         % Well just ignore it then.
    end.
%% -----------------------------------------------------------------------------

%% Function handling that a trace case has completed its stopping phase and
%% shall now be nulled in the Command History Log (meaning that it will not
%% be repeated in the event of a reactivation).
h_tc_stopping_done(ProcH,Result,LD=#ld{chl=CHL}) ->
    case find_tc_executer_chl(ProcH,CHL) of
	{stopping,{TC,Id}} ->
	    case Result of
		{ok,_Result} ->             % _Result is returned from the tracecase.
		    {ok,LD#ld{chl=nullify_chl(ProcH,TC,Id,CHL)}};
		{error,_} ->                % This is difficult, is it still active?
		    {ok,LD#ld{chl=nullify_chl(ProcH,TC,Id,CHL)}}
	    end;
	_ ->                                % Strange.
	    {ok,LD}
    end.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Terminate.
%% -----------------------------------------------------------------------------

%% Help function stopping the inviso control component. Does not return
%% anything significant.
stop_inviso_at_c_node(undefined) ->         % Non distributed case.
    inviso:stop();
stop_inviso_at_c_node(CNode) ->
    rpc:call(CNode,inviso,stop,[]).
%% -----------------------------------------------------------------------------

%% Help function that removes all trace patterns from the nodes that are not
%% marked as such were patterns shall be left after stopping of inviso.
%% Returns {ok,NodeResult} or {error,Reason}. In the non-distributed case
%% 'ok' is returned incase of success, ot 'patterns_untouched'.
remove_all_trace_patterns(undefined,KeepNodes,_Nodes) ->
    case KeepNodes of
	undefined ->                        % No, remove patterns from localruntime.
	    inviso:ctp_all();
	_ ->
	    patterns_untouched
    end;
remove_all_trace_patterns(CNode,KeepNodes,Nodes) ->
    Nodes2=lists:filter(fun(N)->not(lists:member(N,KeepNodes)) end,Nodes),
    case inviso_tool_lib:inviso_cmd(CNode,ctp_all,[Nodes2]) of
	{ok,NodeResults} ->
	    F=fun(N) ->
		      case lists:member(N,KeepNodes) of
			  true ->
			      {N,patterns_untouched};
			  false ->
			      case lists:keysearch(N,1,NodeResults) of
				  {value,Result} ->
				      Result; % {Node,ok}
				  false ->  % Extremely strange.
				      {N,{error,general_error}}
			      end
		      end
	      end,
	    {ok,lists:map(F,Nodes)};
	{error,{badrpc,Reason}} ->
	    {error,{inviso_control_node_error,Reason}};
	{error,Reason} ->
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% Second level help functions.
%% =============================================================================

%% Help function building a reply to a reconnection call based on which nodes
%% where asked to be reconnected and which of those are actually now working.
%% We actually make an effort to serve the return value in the same order as the
%% nodes were mentioned in the original call (Nodes).
build_reconnect_nodes_reply(local_runtime,local_runtime,_NodesErr,NodesD) ->
    case get_state_nodes(local_runtime,NodesD) of
	down ->
	    {error,down};
	{State,Status} ->
	    {ok,{State,Status}}
    end;
build_reconnect_nodes_reply(local_runtime,_,NodesErr,_NodesD) ->
    NodesErr;
build_reconnect_nodes_reply([Node|Rest],Nodes2,NodesErr,NodesD) ->
    case lists:member(Node,Nodes2) of
	true ->                               % Ok, look in the #ld.nodes.
	    case get_state_nodes(Node,NodesD) of
		down ->                       % Somekind of failure, still down.
		    [{Node,{error,down}}|
		     build_reconnect_nodes_reply(Rest,Nodes2,NodesErr,NodesD)];
		{State,Status} ->             % {State,Status}
		    [{Node,{ok,{State,Status}}}|
		     build_reconnect_nodes_reply(Rest,Nodes2,NodesErr,NodesD)]
	    end;
	false ->                              % Error already from the beginning.
	    {value,{_,Error}}=lists:keysearch(Node,1,NodesErr),
	    [{Node,Error}|build_reconnect_nodes_reply(Rest,Nodes2,NodesErr,NodesD)]
    end;
build_reconnect_nodes_reply([],_,_,_) ->
    [].
%% -----------------------------------------------------------------------------

%% Help function building a return value to reinitiate_session. Nodes contains
%% all involved nodes. If the node occurrs in NodesErr, we choose the error in
%% NodesErr. Otherwise the returnvalue in ReturnVal is used.
build_reinitiate_session_reply(Nodes,NodesErr,{ok,NodesResults}) ->
    {ok,build_reinitiate_session_reply_2(Nodes,NodesErr,NodesResults)};
build_reinitiate_session_reply(local_runtime,[],NodeResult) ->
    NodeResult;
build_reinitiate_session_reply(local_runtime,NodesErr,_NodeResult) ->
    NodesErr.
build_reinitiate_session_reply_2([Node|Rest],NodesErr,NodeResults) ->
    case lists:keysearch(Node,1,NodesErr) of
	{value,{_,Error}} ->
	    [{Node,Error}|build_reinitiate_session_reply_2(Rest,NodesErr,NodeResults)];
	false ->
	    case lists:keysearch(Node,1,NodeResults) of
		{value,Value} ->
		    [Value|build_reinitiate_session_reply_2(Rest,NodesErr,NodeResults)]
	    end
    end;
build_reinitiate_session_reply_2([],_NodesErr,_NodeResults) ->
    [].
%% -----------------------------------------------------------------------------

%% Help function returning a history log where stop and stopping entries have
%% been removed. Further all tracecase log entries must be set to running since
%% there can not be such a thing as an activating tracecase stored away in a
%% saved historyfile!
%% We must also take away any #Ref.
build_saved_history_data(SortedLog) ->
    CleanedLog=
	lists:filter(fun({_,_,Stop,_}) when Stop==stop;Stop==stopping -> false;
			(_) -> true
		     end,
		     SortedLog),
    lists:map(fun({{TC,Id},C,activating,B}) -> {{TC,Id},C,running,B};
		 ({{TC,Id},C,S,B}) -> {{TC,Id},C,S,B};
		 ({{M,F,Args,_Ref},C}) -> {{M,F,Args},C};
		 ({{TC,_Ref},C,B}) -> {TC,C,B} % An rtc.
	      end,
	      CleanedLog).
%% -----------------------------------------------------------------------------
    
%% This help function builds the AutoStartData structure which is returned from
%% get_austostart_data. An AutoStartData structure is a list of trace-files and
%% inviso commands. The order is significant since it is the idea that doing
%% the trace case files and inviso commands in that order will bring a node to
%% a certain state in a trace perspective.
%% Returns {ok,AutoStartData} or {error,Reason}
build_autostart_data(SortedLog,TCdict) ->
    build_autostart_data_2(SortedLog,TCdict,[]).

build_autostart_data_2([{_,_C,Stop,_B}|Rest],TCdict,Accum) when Stop==stop;Stop==stopping->
    build_autostart_data_2(Rest,TCdict,Accum); % Simply skip deactivated/deativating.
build_autostart_data_2([{{TCname,_},_C,activating,Bindings}|Rest],TCdict,Accum) ->
    build_autostart_data_tc(TCname,Bindings,TCdict,Rest,Accum);
build_autostart_data_2([{{TCname,_},_C,running,Bindings}|Rest],TCdict,Accum) ->
    build_autostart_data_tc(TCname,Bindings,TCdict,Rest,Accum);
build_autostart_data_2([{{TCname,_Ref},_C,Bindings}|Rest],TCdict,Accum) ->
    build_autostart_data_tc(TCname,Bindings,TCdict,Rest,Accum);
build_autostart_data_2([{{M,F,Args,_Ref},_C}|Rest],TCdict,Accum) ->
    build_autostart_data_2(Rest,TCdict,[{mfa,{M,F,Args}}|Accum]);
build_autostart_data_2([],_TCdict,Accum) ->
    {ok,lists:reverse(Accum)}.

%% Help function placing the filename in the AutoStartData structure.    
build_autostart_data_tc(TCname,Bindings,TCdict,Rest,Accum) ->
    {ok,TC}=get_tracecase_tc_dict(TCname,TCdict),
    {ok,FName}=get_tc_activate_fname(TC),
    build_autostart_data_2(Rest,TCdict,[{file,{FName,Bindings}}|Accum]).
%% -----------------------------------------------------------------------------

%% Help function generating tracerdata to init inviso tracing. The generation
%% is done by the TracerDataGenerator, TDG, function.
%% Individual tracerdata is generated for each node in Nodes.
%% Returns {ok,TracerData} or {error,Reason}.
call_tracer_data_generator(undefined,M,F,TDGargs,_Nodes) -> % Non distributed.
    case catch call_tracer_data_generator_3(M,F,TDGargs,local_runtime) of
	{'EXIT',Reason} ->
	    {error,{'EXIT',Reason}};
	TracerData ->
	    {ok,TracerData}
    end;
call_tracer_data_generator(_CNode,M,F,TDGargs,Nodes) ->
    case catch call_tracer_data_generator_2(M,F,TDGargs,Nodes) of
	{'EXIT',Reason} ->
	    {error,{'EXIT',Reason}};
	TracerList ->
	    {ok,TracerList}
    end.

call_tracer_data_generator_2(M,F,TDGargs,[Node|Rest]) ->
    [{Node,call_tracer_data_generator_3(M,F,TDGargs,Node)}|
     call_tracer_data_generator_2(M,F,TDGargs,Rest)];
call_tracer_data_generator_2(_,_,_,[]) ->
    [].

call_tracer_data_generator_3(M,F,TDGargs,Node) ->
    apply(M,F,call_tracer_data_generator_mkargs(Node,TDGargs)).

%% This function creates the arguments that the tracer data generator function
%% accepts (in an apply call). The reason for making it a sepparate function is
%% that the arguments are constructed in more situations than just when actually
%% doing the apply. By having a function it will become obvious where to change
%% should the arguments change.
call_tracer_data_generator_mkargs(Node,TDGargs) ->
    inviso_tool_lib:mk_complete_tdg_args(Node,TDGargs).
%% -----------------------------------------------------------------------------

%% This function acts as standard options generator function. That is returning
%% the options argument to inviso:add_node/3. Note that this function must not
%% return the dependency part of that option.
std_options_generator(_Node) ->
    [].                                     % No particular options(!)
%% -----------------------------------------------------------------------------


%% Help function checking that Vars contains a binding for every variable
%% listed in the VarNames field in TraceCase. Note that the special variable 'Nodes'
%% is disregarded, since it is always added by the inviso_tool.
%% Returns {ok,Bindings} or {error,Reason}. Where Bindings is a bindngs structure
%% according to file:eval functionality.
check_bindings(Vars,TraceCase) ->
    case catch check_bindings_2(Vars,
				get_tc_varnames(TraceCase),
				erl_eval:new_bindings()) of
	{'EXIT',_Reason} ->
	    {error,variable_error};
	{error,Reason} ->                   % Missing a bindning.
	    {error,Reason};
	{ok,Bindings} ->
	    {ok,Bindings}
    end.

check_bindings_2(Vars,['Nodes'|Rest],Bindings) ->
    check_bindings_2(Vars,Rest,Bindings);   % Disregard Nodes since it is automatic.
check_bindings_2(Vars,[VarName|Rest],Bindings) ->
    case lists:keysearch(VarName,1,Vars) of
	{value,{_,Val}} ->
	    check_bindings_2(Vars,Rest,erl_eval:add_binding(VarName,Val,Bindings));
	false ->                            % Mandatory variable missing.
	    {error,{missing_variable,VarName}} % Quite here then.
    end;
check_bindings_2(_,[],Bindings) ->
    {ok,Bindings}.
%% -----------------------------------------------------------------------------

%% This help function checks that the command the user tries to do is amongst
%% the inviso API. It at the same time returns what kind of command it is.
%% {true,RegExpFlag} or 'false' where RegExpFlag indicates if this command
%% needs to have its argument modified by module regexp expansion or not.
check_proper_inviso_call(Cmd,Arity) ->
    case lists:member({Cmd,Arity},?INVISO_CMDS) of
	true ->                             % It is part of inviso API.
	    {true,check_proper_inviso_call_regexp(Cmd,Arity)};
	false ->
	    false
    end.

%% Returns {Type,Arity,PlaceOfModuleSpec} or 'false'.
check_proper_inviso_call_regexp(tp,5) -> {tp,5,1};
check_proper_inviso_call_regexp(tp,4) -> {tp,4,1};
check_proper_inviso_call_regexp(tp,1) -> {tp,1,1};
check_proper_inviso_call_regexp(tpl,5) -> {tp,5,1};
check_proper_inviso_call_regexp(tpl,4) -> {tp,4,1};
check_proper_inviso_call_regexp(tpl,1) -> {tp,1,1};
check_proper_inviso_call_regexp(ctp,3) -> {ctp,3,1};
check_proper_inviso_call_regexp(ctp,1) -> {ctp,1,1};
check_proper_inviso_call_regexp(ctpl,3) -> {ctp,3,1};
check_proper_inviso_call_regexp(ctpl,1) -> {ctp,1,1};
check_proper_inviso_call_regexp(_,_) ->      % No regexp expansion.
    false.
%% -----------------------------------------------------------------------------

%% Help function checking if this inviso command shall be added to the command
%% history log. Returns true or false.
check_inviso_call_to_history(Cmd,Arity) ->
    case lists:member({Cmd,Arity},?INVISO_CMD_HISTORY) of
	true ->
	    true;
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

%% Help function traversing the arguments and expanding module names stated
%% as regular expressions. This means that the resulting arguments may be longer
%% than the orginal ones.
%% When we run this function it has been determined that we are a distributed
%% system.
%% Also note that if there are no regexps in Args, no regexpansion will be
%% made and RegExpNode may be 'undefined' (as it is if not set at start-up).
%% If RegExpNode is unavailable the nodes found in Nodes will be used until
%% one that works is found.
expand_module_regexps(Args,_RegExpNode,_Nodes,false) ->
    {ok,Args};
expand_module_regexps([PatternList],RegExpNode,Nodes,{tp,1,1}) ->
    case catch expand_module_regexps_tp(PatternList,RegExpNode,Nodes) of
	NewPatternList when list(NewPatternList) ->
	    {ok,[NewPatternList]};
	{error,Reason} ->
	    {error,Reason}
    end;
expand_module_regexps([PatternList],RegExpNode,Nodes,{ctp,1,1}) ->
    case catch expand_module_regexps_ctp(PatternList,RegExpNode,Nodes) of
	NewPatternList when list(NewPatternList) ->
	    {ok,[NewPatternList]};
	{error,Reason} ->
	    {error,Reason}
    end;
expand_module_regexps([M,F,Arity,MS,Opts],RegExpNode,Nodes,{tp,5,1}) ->
    expand_module_regexps([[{M,F,Arity,MS,Opts}]],RegExpNode,Nodes,{tp,1,1});
expand_module_regexps([M,F,Arity,MS],RegExpNode,Nodes,{tp,4,1}) ->
    expand_module_regexps([[{M,F,Arity,MS,[]}]],RegExpNode,Nodes,{tp,1,1});
expand_module_regexps([M,F,Arity],RegExpNode,Nodes,{ctp,3,1}) ->
    expand_module_regexps([[{M,F,Arity}]],RegExpNode,Nodes,{ctp,1,1}).


expand_module_regexps_tp([E={M,_,_,_,_}|Rest],RegExpNode,Nodes) when atom(M) ->
    [E|expand_module_regexps_tp(Rest,RegExpNode,Nodes)];
expand_module_regexps_tp([{M,F,Arity,MS,Opts}|Rest],RegExpNode,Nodes) when list(M);tuple(M) ->
    case inviso_tool_lib:expand_module_names([RegExpNode],
					     M,
					     [{expand_only_at,RegExpNode}]) of
	{singlenode_expansion,Modules} ->
	    expand_module_regexps_tp_2(Modules,F,Arity,MS,Opts,Rest,RegExpNode,Nodes);
	{error,{faulty_node,RegExpNode}} ->    % RegExpNode probably down.
	    case Nodes of
		[NewRegExpNode|RestNodes] ->   % Ok, just choose a node.
		    expand_module_regexps_tp([{M,F,Arity,MS,Opts}|Rest],NewRegExpNode,RestNodes);
		[] ->                          % No more nodes to choose from.
		    throw({error,no_available_regexpnode})
	    end;
	{error,_Reason} ->
	    expand_module_regexps_tp(Rest,RegExpNode,Nodes)
    end;
expand_module_regexps_tp([_|Rest],RegExpNode,Nodes) ->
    expand_module_regexps_tp(Rest,RegExpNode,Nodes); % Skip faulty module specification.
expand_module_regexps_tp([],_RegExpNodes,_Nodes) ->
    [].

expand_module_regexps_tp_2([M|MRest],F,Arity,MS,Opts,Rest,RegExpNode,Nodes) ->
    [{M,F,Arity,MS,Opts}|
     expand_module_regexps_tp_2(MRest,F,Arity,MS,Opts,Rest,RegExpNode,Nodes)];
expand_module_regexps_tp_2([],_,_,_,_,Rest,RegExpNode,Nodes) ->
    expand_module_regexps_tp(Rest,RegExpNode,Nodes).

expand_module_regexps_ctp([E={M,_,_}|Rest],RegExpNode,Nodes) when atom(M) ->
    [E|expand_module_regexps_ctp(Rest,RegExpNode,Nodes)];
expand_module_regexps_ctp([{M,F,Arity}|Rest],RegExpNode,Nodes) when list(M);tuple(M) ->
    case inviso_tool_lib:expand_module_names([RegExpNode],
					     M,
					     [{expand_only_at,RegExpNode}]) of
	{singlenode_expansion,badrpc} ->       % RegExpNode probably down.
	    case Nodes of
		[NewRegExpNode|RestNodes] ->   % Ok, just choose a node.
		    expand_module_regexps_ctp([{M,F,Arity}|Rest],NewRegExpNode,RestNodes);
		[] ->                          % No more nodes to choose from.
		    throw({error,no_available_regexpnode})
	    end;
	{singlenode_expansion,Modules} ->
	    expand_module_regexps_ctp_2(Modules,F,Arity,Rest,RegExpNode,Nodes);
	{error,_Reason} ->
	    expand_module_regexps_ctp(Rest,RegExpNode,Nodes)
    end;
expand_module_regexps_ctp([_|Rest],RegExpNode,Nodes) ->
    expand_module_regexps_tp(Rest,RegExpNode,Nodes); % Skip faulty module specification.
expand_module_regexps_ctp([],_RegExpNodes,_Nodes) ->
    [].

expand_module_regexps_ctp_2([M|MRest],F,Arity,Rest,RegExpNode,Nodes) ->
    [{M,F,Arity}|expand_module_regexps_ctp_2(MRest,F,Arity,Rest,RegExpNode,Nodes)];
expand_module_regexps_ctp_2([],_,_,Rest,RegExpNode,Nodes) ->
    expand_module_regexps_ctp(Rest,RegExpNode,Nodes).
%% -----------------------------------------------------------------------------



%% Help function running the activation of a trace case. Note that this must
%% be done at the inviso control component's Erlang node *and* that it must be
%% done in its own process since there is no telling for how long a trace case
%% may run.
%% Returns {ok,ActivationHandler}.
exec_trace_case_on(CNode,TraceCase,Bindings,Nodes) ->
    {ok,TcFName}=get_tc_activate_fname(TraceCase),
    {ok,exec_trace_case_2(CNode,
			  TcFName,
			  erl_eval:add_binding('Nodes',Nodes,Bindings),
			  activating)}.

%% Help function running the deactivation of a trace case.
exec_trace_case_off(CNode,TraceCase,Bindings,Nodes) ->
    case get_tc_deactivate_fname(TraceCase) of
	{ok,TcFName} ->                     % There is a deactivation.
	    {ok,exec_trace_case_2(CNode,
				  TcFName,
				  erl_eval:add_binding('Nodes',Nodes,Bindings),
				  stopping)};
	false ->
	    {error,no_deactivation}
    end.

exec_trace_case_2(CNode,TcFName,Bindings,Phase) ->
    if
	CNode==undefined ->                 % The non distributed case.
	    spawn_link(?MODULE,tc_executer,[TcFName,Bindings,Phase,self()]);
	true ->
	    spawn_link(CNode,?MODULE,tc_executer,[TcFName,Bindings,Phase,self()])
    end.

%% This function is run in its own process and is responsible for executing
%% the trace case.
tc_executer(TcFName,Bindings,Phase,Parent) ->
    case catch file:script(TcFName,Bindings) of
	{ok,Value} ->
	    tc_executer_reply(Parent,{Phase,self(),{ok,Value}});
	{'EXIT',Reason} ->
	    tc_executer_reply(Parent,{Phase,self(),{error,{'EXIT',Reason}}});
	Error ->
	    tc_executer_reply(Parent,{Phase,self(),Error})
    end.
%% -----------------------------------------------------------------------------

%% Help function which starts a reactivator process redoing command history at
%% Node. It also updates the loopdata to indicate that Node is now in state
%% reactivating. It is a good idea to only handle one node per reactivator process.
%% This because if the node terminates and comes back up, the reactivator must be
%% stopped.
redo_cmd_history(Node,LD=#ld{c_node=CNode,tc_dict=TCdict,chl=CHL,nodes=NodesD}) ->
    P=start_reactivator(Node,CNode,TCdict,CHL),
    LD#ld{nodes=set_reactivating_nodes(Node,NodesD),
	  reactivators=add_reactivators(Node,P,LD#ld.reactivators)}.

%% Help function starting a reactivator process replaying the command history log.
%% Returns a pid of the reactivator process.
start_reactivator(Node,CNode,TCdict,CHL) ->
    UnsortedLog=get_loglist_chl(CHL),       % Must fetch here, later on wrong node.
    if
	CNode==undefined ->                 % The non-distributed case.
	    spawn_link(?MODULE,
		       reactivator_executer,
		       [Node,TCdict,UnsortedLog,self(),0,[]]);
	true ->
	    spawn_link(CNode,
		       ?MODULE,
		       reactivator_executer,
		       [Node,TCdict,UnsortedLog,self(),0,[]])
    end.

%% The strategy is to traverse the CHL ETS table in Counter order, redoing the
%% commands one by one. We wait until one command is finished until we do the
%% next. Commands marked as nullified are not performed. In fact when a command
%% is nullified only the stop will be found in the CHL. Its activation will be
%% removed.
reactivator_executer(Node,TCdict,UnsortedLog,TPid,StartCounter,DoneCases) ->
    SortedLog=lists:keysort(2,UnsortedLog), % Sort on Counter, oldest first.
    Log=reactivator_skip_log_entries(SortedLog,StartCounter),
    case reactivator_executer_2(Node,TCdict,TPid,StartCounter,DoneCases,Log) of
	done ->
	    true;                           % Simply terminate the reactivator then.
	{more,{NewStartCounter,NewDoneCases,NewUnsortedLog}} ->
	    reactivator_executer(Node,TCdict,NewUnsortedLog,TPid,NewStartCounter,NewDoneCases)
    end.

reactivator_executer_2(Node,TCdict,TPid,_Counter,DoneCases,
		       [{{TCname,Id},NextC,running,Bindings}|Rest]) ->
    reactivator_executer_3(Node,TCdict,TPid,DoneCases,Rest,TCname,Id,NextC,Bindings,Rest);
reactivator_executer_2(Node,TCdict,TPid,_Counter,DoneCases,
		       [{{TCname,_Ref},NextC,Bindings}|Rest]) ->
    reactivator_executer_rtc(Node,TCdict,TPid,DoneCases,Rest,TCname,NextC,Bindings,Rest);
reactivator_executer_2(Node,TCdict,TPid,_Counter,DoneCases,
		       [{{TCname,Id},NextC,activating,Bindings}|Rest]) ->
    reactivator_executer_3(Node,TCdict,TPid,DoneCases,Rest,TCname,Id,NextC,Bindings,Rest);
reactivator_executer_2(Node,TCdict,TPid,_Counter,DoneCases,
		       [{{M,F,Args,_Ref},NextC}|Rest]) ->
    reactivator_executer_cmd(Node,M,F,Args),
    reactivator_executer_2(Node,TCdict,TPid,NextC,DoneCases,Rest);
reactivator_executer_2(Node,TCdict,TPid,_Counter,DoneCases,
		       [{{_TCname,_Id},NextC,stopping,_Bindings}|Rest]) ->
    reactivator_executer_2(Node,TCdict,TPid,NextC,DoneCases,Rest);
reactivator_executer_2(Node,TCdict,TPid,_Counter,DoneCases,
		       [{{TCname,Id,_Ref},NextC,stop,Bindings}|Rest]) ->
    case lists:member({TCname,Id},DoneCases) of
	true ->                             % We have activated it, must stop then.
	    case get_tracecase_tc_dict(TCname,TCdict) of
		{ok,{_,_,_,_,FNameOff}} ->
		    reactivator_executer_tc(Node,Bindings,FNameOff),
		    NewDoneCases=lists:delete({TCname,Id},DoneCases),
		    reactivator_executer_2(Node,TCdict,TPid,NextC,NewDoneCases,Rest);
		{ok,_} ->                   % No stop-filename, strange!
		    reactivator_executer_2(Node,TCdict,TPid,NextC,DoneCases,Rest);
		false ->                    % Even stranger, does not exist!?
		    reactivator_executer_2(Node,TCdict,TPid,NextC,DoneCases,Rest)
	    end;
	false ->                            % Never activated in the first place.
	    reactivator_executer_2(Node,TCdict,TPid,NextC,DoneCases,Rest)
    end;
%% Done all log entries found this lap. See if there are more entries by now.
reactivator_executer_2(_Node,_TCdict,TPid,Counter,DoneCases,[]) ->
    case reactivator_reply(TPid,Counter) of % Ask the tool process for more entries.
	done ->                             % No more entries in the CHL.
	    done;
	{more,NewUnsortedLog} ->            % Repeat the procedure
	    {more,{Counter+1,DoneCases,NewUnsortedLog}} % with log entries from Counter+1.
    end.

%% This help function activates a tracecase.
reactivator_executer_3(Node,TCdict,TPid,DoneCases,Rest,TCname,Id,NextC,Bindings,Rest) ->
    case get_tracecase_tc_dict(TCname,TCdict) of
	{ok,{_,_,_,FNameOn}} ->             % A case with just on functionality.
	    reactivator_executer_tc(Node,Bindings,FNameOn),
	    reactivator_executer_2(Node,TCdict,TPid,NextC,[{TCname,Id}|DoneCases],Rest);
	{ok,{_,_,_,FNameOn,_}} ->
	    reactivator_executer_tc(Node,Bindings,FNameOn),
	    reactivator_executer_2(Node,TCdict,TPid,NextC,[{TCname,Id}|DoneCases],Rest);
	false ->                            % Strange, does not exist anylonger!?
	    reactivator_executer_2(Node,TCdict,TPid,NextC,DoneCases,Rest)
    end.

%% Help function executing a trace case in the reactivators context. Does not
%% return anything significant.
reactivator_executer_tc(Node,Bindings,FileName) ->
    catch file:eval(FileName,erl_eval:add_binding('Nodes',[Node],Bindings)).

%% Help function handling trace case that are simply executed - rtc.
reactivator_executer_rtc(Node,TCdict,TPid,DoneCases,Rest,TCname,NextC,Bindings,Rest) ->
    case get_tracecase_tc_dict(TCname,TCdict) of
	{ok,{_,_,_,FNameOn}} ->             % A case with just on functionality.
	    reactivator_executer_tc(Node,Bindings,FNameOn),
	    reactivator_executer_2(Node,TCdict,TPid,NextC,DoneCases,Rest);
	{ok,{_,_,_,FNameOn,_}} ->
	    reactivator_executer_tc(Node,Bindings,FNameOn),
	    reactivator_executer_2(Node,TCdict,TPid,NextC,DoneCases,Rest);
	false ->                            % Strange, does not exist anylonger!?
	    reactivator_executer_2(Node,TCdict,TPid,NextC,DoneCases,Rest)
    end.

reactivator_executer_cmd(nonode@nohost,M,F,Args) ->
    catch apply(M,F,Args);                  % Non-distributed.
reactivator_executer_cmd(Node,M,F,Args) ->
    catch apply(M,F,[[Node]|Args]).

%% Help function returning a list of log entries missing the first entries
%% having a counter less or equal to C1.
reactivator_skip_log_entries([{_,C,_,_}|Rest],C1) when C<C1 ->
    reactivator_skip_log_entries(Rest,C1);
reactivator_skip_log_entries([{_,C}|Rest],C1) when C<C1 ->
    reactivator_skip_log_entries(Rest,C1);
reactivator_skip_log_entries(Log,_) ->
    Log.
%% -----------------------------------------------------------------------------

%% Help function returning the node name to use in an rpc call.
get_rpc_nodename(undefined) ->
    node();
get_rpc_nodename(CNode) ->
    CNode.
%% -----------------------------------------------------------------------------

mk_rt_tag() ->
    inviso_tool.
%% -----------------------------------------------------------------------------

is_string([C|Rest]) when C>=32, C=<255 ->
    is_string(Rest);
is_string([]) ->
    true;
is_string(_) ->
    false.
%% -----------------------------------------------------------------------------

	    
%% -----------------------------------------------------------------------------
%% Functions for handling the configuration file.
%% -----------------------------------------------------------------------------

%% The inviso tool is configured via start arguments and/or a configuration file.
%% Start arguments will override any definitions in a configuration file.
%% The configuration file is pointed out by either a start argument or the
%% inviso application parameter 'inviso_tool_config_file'.

%% Help function building the internal configuration structure. Configurations
%% in the start argument will override parameters found in a configuration file.
fetch_configuration(Config) ->
    case fetch_config_filename(Config) of
	{ok,FName} ->                       % We are supposed to use a conf-file.
	    case read_config_file(FName) of
		{ok,LD} ->                  % Managed to open a file.
		    NewLD=read_config_list(LD,Config),
		    {ok,NewLD};
		{error,_Reason} ->          % Problem finding/opening file.
		    LD=read_config_list(#ld{},Config),
		    {ok,LD}
	    end;
	false ->                            % No filename specified.
	    LD=read_config_list(#ld{},Config),
	    {ok,LD}
    end.

%% Help function determining the name of the file which shall be consulted as
%% the main configuration file.
%% Returns {ok,FileName} or 'false'. The latter if no name could be determined.
fetch_config_filename(Config) ->
    case catch lists:keysearch(config_file,1,Config) of
	{value,{_,FName}} when list(FName) ->
	    {ok,FName};
	_ ->                                % No filename in the start argument.
	    fetch_config_filename_2()
    end.

fetch_config_filename_2() ->
    case application:get_env(inviso_tool_config_file) of
	{ok,FName} when list(FName) ->
	    {ok,FName};
	_ ->                                % Application parameter not specified.
	    false                           % Means no config file will be used.
    end.

%% Help function reading the configuration file. Returns a #conf or {error,Reason}.
read_config_file(FName) ->
    case catch file:consult(FName) of
	{ok,Terms} ->
	    {ok,read_config_list(#ld{},Terms)};
	{error,Reason} ->
	    {error,{file_consult,Reason}};
	{'EXIT',Reason} ->
	    {error,{failure,Reason}}
    end.

%% Help function traversing the Terms list entering known tag-values into #ld.
read_config_list(LD,Terms) ->
    LD1=read_config_list_2(LD,Terms,nodes),
    LD2=read_config_list_2(LD1,Terms,c_node),
    LD3=read_config_list_2(LD2,Terms,regexp_node),
    LD4=read_config_list_2(LD3,Terms,tc_def_file),
    LD6=read_config_list_2(LD4,Terms,tdg),
    LD8=read_config_list_2(LD6,Terms,debug),
    LD10=read_config_list_2(LD8,Terms,initial_tcs),
    LD11=read_config_list_2(LD10,Terms,dir),
    _LD12=read_config_list_2(LD11,Terms,optg).

read_config_list_2(LD,Terms,Tag) ->
    case catch lists:keysearch(Tag,1,Terms) of
	{value,{_,Value}} ->
	    update_ld_record(LD,Tag,Value);
	_ ->
	    LD                              % Tag not found in Terms (or error!)
    end.
%% -----------------------------------------------------------------------------

%% Function updating a named field in a record. Returns a new record. Note that
%% this function must be maintained due the fact that field names are removed
%% at compile time.
update_ld_record(LD,nodes,Value) when record(LD,ld) ->
    case mk_nodes(Value) of
	{ok,NodesD} ->
	    LD#ld{nodes=NodesD};
	error ->
	    LD
    end;
update_ld_record(LD,Tag,Value) when record(LD,ld) ->
    Index=
	case Tag of
	    c_node ->                       % atom()
		#ld.c_node;
	    regexp_node ->                  % atom()
		#ld.regexp_node;
	    tc_def_file ->                  % string()
		#ld.tc_def_file;
	    initial_tcs ->                  % [{TCname,VarList},...]
		#ld.initial_tcs;
	    history_dir ->                  % string()
		#ld.history_dir;
	    debug ->                        % true | false
		#ld.debug;
	    dir ->                          % string()
		#ld.dir;
	    optg ->                         % {Mod,Func,Args}
		#ld.optg;
	    tdg ->                          % {Mod,Func,Args}
		#ld.tdg;
	    keep_nodes ->                   % [Nodes,...]
		#ld.keep_nodes
	end,
    setelement(Index,LD,Value).             % Cheeting!
%% -----------------------------------------------------------------------------


%% Help function which, if it exists, consults the trace definition file. The
%% idea behind the trace definition file is to point out which trace cases there
%% are, where to find them and how to turn them on and off.
%% Trace case definitions are:
%% {TCname,Type,VariableNameList,ActivatioFileName} |
%%     {TCname,Type,VariableNameList,ActivationFileName,DeactivationFileName}
%%   TCname=atom()
%%   Type=on | on_off
%%   VariableNameList=[atom(),...]
%%   ActivationFileName=DeactivationFileName=string()
read_trace_case_definitions(LD) ->
    case LD#ld.tc_def_file of
	TCfileName when list(TCfileName) ->
	    case catch file:consult(TCfileName) of
		{ok,Terms} ->
		    Dir=LD#ld.dir,          % The working directory of the tool.
		    TCdict=read_trace_case_definitions_2(Terms,Dir,mk_tc_dict()),
		    LD#ld{tc_dict=TCdict};
		_ ->
		    LD
	    end;
	_ ->
	    LD
    end.

read_trace_case_definitions_2([{TCname,on,VarNames,FName}|Rest],Dir,TCdict) ->
    FileName=make_absolute_path(FName,Dir),
    read_trace_case_definitions_2(Rest,
				  Dir,
				  insert_tracecase_tc_dict(TCname,
							   on,
							   VarNames,
							   FileName,
							   TCdict));
read_trace_case_definitions_2([{TCname,on_off,VarNames,FNameOn,FNameOff}|Rest],Dir,TCdict) ->
    FileNameOn=make_absolute_path(FNameOn,Dir),
    FileNameOff=make_absolute_path(FNameOff,Dir),
    read_trace_case_definitions_2(Rest,
				  Dir,
				  insert_tracecase_tc_dict(TCname,
							   on_off,
							   VarNames,
							   FileNameOn,
							   FileNameOff,
							   TCdict));
read_trace_case_definitions_2([_|Rest],Dir,TCdict) ->
    read_trace_case_definitions_2(Rest,Dir,TCdict);
read_trace_case_definitions_2([],_Dir,TCdict) ->
    TCdict.

%% Help function returning an absolute path to FName if FName is not already
%% absolute. Dir is the working dir of the tool and supposed to be absolute.
make_absolute_path(FName,Dir) ->
    case filename:pathtype(FName) of
	absolute ->                         % Then do nothing, allready absolute.
	    FName;
	_ ->
	    filename:join(Dir,FName)
    end.    
%% -----------------------------------------------------------------------------

get_status(undefined,_Node) ->
    inviso:get_status();
get_status(CNode,Nodes) ->
    inviso_tool_lib:inviso_cmd(CNode,get_status,[Nodes]).
%% -----------------------------------------------------------------------------


%% =============================================================================
%% Internal data structure functions.
%% =============================================================================

%% -----------------------------------------------------------------------------
%% The nodes database structure.
%% -----------------------------------------------------------------------------

%% The purpose of the nodes database structure is to keep track of what runtime
%% nodes we have, and their current status.
%% Implementation:
%% [{NodeName,AvailableStatus},...] or AvailableStatus in the
%%     non-distributed case.
%%   AvailableStatus={up,Status1} | down
%%     Status1={State,Status} | reactivating
%%       State=tracing | inactive | trace_failure
%%       Status=running | suspended
%%         reactivating=the node is now being brought up to date.
%%         inactive=not tracing, can be initiated and then reactivated.
%% The following states can occure.
%%   {inactive,running}
%%     Mainly when we start the tool, before a session has been started.
%%   {tracing,running}
%%     When a trace session is on-going.
%%   {trace_failure,running}
%%     If init_tracing failed for some reason.
%%   {tracing,suspended}
%%   reactivating
%%     The node is tracing (has always been) but was suspended. It is now
%%     no longer suspended and the tool is redong commands.
%%   {inactive,suspended}
%%     We can end up here if a session is stopped with this node suspended.

%% Returns a nodes database structure filled with the nodes Nodes.
mk_nodes(Nodes) when list(Nodes) ->
    {ok,lists:map(fun(N) when atom(N)->{N,down} end,Nodes)};
mk_nodes(local_runtime) ->                  % The non-distributed case.
    down;
mk_nodes(_Nodes) ->
    error.
%% -----------------------------------------------------------------------------

%% Updates the nodes database structure for each node that has been added.
%% This is the case when we start the tool or reactivate a node. Note that a node
%% may have become adopted instead of started.
%% Returns a new nodes database structure.
update_added_nodes(CNode,[{Node,NodeResult}|Rest],NodesD) ->
    case update_added_nodes_3(NodeResult) of
	already_added ->                    % Already added to the control component.
	    case get_status(CNode,[Node]) of % Examine if it is tracing or not.
		{ok,[{Node,NodeResult2}]} ->
		    Result=mk_nodes_state_from_status(NodeResult2),
		    update_added_nodes_2(CNode,Node,Result,NodesD,Rest);
		{error,_Reason} ->          % Strange, mark it as down now.
		    update_added_nodes_2(CNode,Node,down,NodesD,Rest)
	    end;
	Result ->
	    update_added_nodes_2(CNode,Node,Result,NodesD,Rest)
    end;
update_added_nodes(_CNode,[],NodesD) ->
    NodesD;
update_added_nodes(_CNode,NodeResult,_NodesD) -> % Non distributed case.
    case update_added_nodes_3(NodeResult) of
	already_added ->                    % Already added, most likely autostart.
	    mk_nodes_state_from_status(inviso:get_status());
	Result ->
	    Result                          % Simply replace NodesD.
    end.

update_added_nodes_2(CNode,Node,Result,NodesD,Rest) ->
    case lists:keysearch(Node,1,NodesD) of
	{value,_} ->                        % Node already exists, replace!
	    update_added_nodes(CNode,Rest,lists:keyreplace(Node,1,NodesD,{Node,Result}));
	false ->                            % Strange, unknown node!
	    update_added_nodes(CNode,Rest,NodesD)
    end.

update_added_nodes_3({ok,{adopted,tracing,running,_Tag}}) ->
    {up,{tracing,running}};
update_added_nodes_3({ok,{adopted,tracing,{suspended,_SReason},_Tag}}) ->
    {up,{tracing,suspended}};
update_added_nodes_3({ok,{adopted,_,running,_Tag}}) ->
    {up,{inactive,running}};
update_added_nodes_3({ok,{adopted,_,{suspended,_SReason},_Tag}}) ->
    {up,{inactive,suspended}};
update_added_nodes_3({ok,new}) ->
    {up,{inactive,running}};
update_added_nodes_3({ok,already_added}) ->
    already_added;                          % This is an error value!
update_added_nodes_3({error,_Reason}) ->
    down.
%% -----------------------------------------------------------------------------

%% Function marking all nodes that, according to the returnvalue from init_tracing,
%% now are successfully initiated as tracing and running. Note that nodes that
%% does not fully respond 'ok' when init_tracing are marked as 'trace_failure'.
%% Also note that we assume that the nodes must be running to have made it this far.
%% A node can of course have become suspended in the process, but that node will
%% be marked as suspended later when that inviso event message arrives to the tool.
%% Returns {NewNodesD,Nodes} where Nodes are the nodes that actually got initiated
%% as a result of the init_tracing call (judged from the LogResults).
set_tracing_running_nodes(undefined,{ok,LogResults},_AvailableStatus) -> % Non-distr. case.
    case set_tracing_running_nodes_checkresult(LogResults) of
	ok ->
	    {{up,{tracing,running}},local_runtime};
	error ->
	    {down,[]}
    end;
set_tracing_running_nodes(undefined,{error,already_initiated},_) -> % Non-distributed case.
    {mk_nodes_state_from_status(inviso:get_status()),[]}; % Ask it for its status.
set_tracing_running_nodes(undefined,{error,_Reason},_) -> % Non-distributed case.
    {down,[]};                             % This is questionable!
set_tracing_running_nodes(CNode,{ok,NodeResults},NodesD) ->
    set_tracing_running_nodes_2(CNode,NodeResults,NodesD,[]).

set_tracing_running_nodes_2(CNode,[{Node,{ok,LogResults}}|Rest],NodesD,Nodes) ->
    case set_tracing_running_nodes_checkresult(LogResults) of
	ok ->                              % The result is good.
	    case lists:keysearch(Node,1,NodesD) of
		{value,_} ->
		    NewNodesD=lists:keyreplace(Node,1,NodesD,{Node,{up,{tracing,running}}}),
		    set_tracing_running_nodes_2(CNode,Rest,NewNodesD,[Node|Nodes]);
		false ->                   % Strange.
		    set_tracing_running_nodes_2(CNode,Rest,NodesD,Nodes)
	    end;
	error ->                           % This node is not tracing correctly.
	    NewNodesD=lists:keyreplace(Node,1,NodesD,{Node,down}),
	    set_tracing_running_nodes_2(CNode,Rest,NewNodesD,Nodes)
    end;
set_tracing_running_nodes_2(CNode,[{Node,{error,already_initiated}}|Rest],NodesD,Nodes) ->
    case get_status(CNode,[Node]) of       % Then we must ask what it is doing now.
	{ok,[{Node,NodeResult}]} ->
	    Result=mk_nodes_state_from_status(NodeResult),
	    NewNodesD=lists:keyreplace(Node,1,NodesD,{Node,Result}),
	    set_tracing_running_nodes_2(CNode,Rest,NewNodesD,Nodes);
	{error,_Reason} ->                 % Strange, mark it as down.
	    NewNodesD=lists:keyreplace(Node,1,NodesD,{Node,down}),
	    set_tracing_running_nodes_2(CNode,Rest,NewNodesD,Nodes)
    end;
set_tracing_running_nodes_2(CNode,[{Node,{error,_Reason}}|Rest],NodesD,Nodes) ->
    NewNodesD=lists:keyreplace(Node,1,NodesD,{Node,{up,{trace_failure,running}}}),
    set_tracing_running_nodes_2(CNode,Rest,NewNodesD,Nodes);
set_tracing_running_nodes_2(_CNode,[],NodesD,Nodes) ->
    {NodesD,Nodes}.                        % New NodesD and nodes successfully initiated.

%% Help function checking if a returnvalue from inviso:init_tracing really
%% means that tracing has started as requested.
set_tracing_running_nodes_checkresult(_LogResults) -> ok. % Should really be better!
%% -----------------------------------------------------------------------------

%% Function updating Node in the NodesD structure and sets it to 'down'.
%% Returns a new nodes structure.
set_down_nodes(Node,[{Node,_}|Rest]) ->
    [{Node,down}|Rest];
set_down_nodes(Node,[NodeStruct|Rest]) ->
    [NodeStruct|set_down_nodes(Node,Rest)];
set_down_nodes(_,[]) ->
    [];
set_down_nodes(_,_) ->                     % Non-distributed case.
    down.                                  % One can argue if this can happend.
%% -----------------------------------------------------------------------------

%% Function updating Node in NodesD to now be suspended. Note that if the node is
%% reactivating it must be moved to state tracing because that is what is doing.
set_suspended_nodes(Node,[{Node,{up,reactivating}}|Rest]) ->
    [{Node,{up,{tracing,suspended}}}|Rest];
set_suspended_nodes(Node,[{Node,{up,{State,_}}}|Rest]) ->
    [{Node,{up,{State,suspended}}}|Rest];
set_suspended_nodes(Node,[NodesData|Rest]) ->
    [NodesData|set_suspended_nodes(Node,Rest)];
set_suspended_nodes(_Node,[]) ->           % Hmm, strange why did we end up here?
    [];
set_suspended_nodes(_,{up,reactivating}) -> % Non-distributed case.
    {up,{tracing,suspended}};
set_suspended_nodes(_,{up,{State,_}}) ->
    {up,{State,suspended}}.
%% -----------------------------------------------------------------------------

%% This function is called when reactivation is completed. Hence it moves the
%% node to no longer suspended. Note this can mean that the node is either
%% tracing or inactive. Reactivation is not allowed for a node have trace_failure.
set_running_nodes(Node,NodesD) when list(NodesD) ->
    case lists:keysearch(Node,1,NodesD) of
	{value,{_,AvailableStatus}} ->
	    lists:keyreplace(Node,1,NodesD,{Node,set_running_nodes_2(AvailableStatus)});
	false ->                           % Very strange!
	    NodesD
    end;
set_running_nodes(_,NodesD) ->             % The non-distributed case.
    set_running_nodes_2(NodesD).

set_running_nodes_2({up,reactivating}) ->
    {up,{tracing,running}};
set_running_nodes_2({up,{State,suspended}}) ->
    {up,{State,running}}.
%% -----------------------------------------------------------------------------

%% Function marking node as now reactivating. That means it is not suspended
%% any longer (and tracing), but still not part of the set of nodes which shall
%% get all commands. Returns a new NodesD.
set_reactivating_nodes(Node,[{Node,_}|Rest]) ->
    [{Node,{up,reactivating}}|Rest];
set_reactivating_nodes(Node,[NodesData|Rest]) ->
    [NodesData|set_reactivating_nodes(Node,Rest)];
set_reactivating_nodes(_,[]) ->
    [];
set_reactivating_nodes(_,{up,_}) ->        % The non-distributed case.
    {up,reactivating}.
%% -----------------------------------------------------------------------------

%% Function called when stop-tracing is done. That is all nodes in Nodes shall
%% be inactive now. Note that an inactive node can still be suspended.
%% Returns a new NodesD.
set_inactive_nodes(_,{up,reactivating}) -> % Non-distributed case.
    {up,{inactive,running}};
set_inactive_nodes(_,{up,{_,Status}}) ->   % Tracing or trace_failure.
    {up,{inactive,Status}};
set_inactive_nodes(_,down) ->
    down;
set_inactive_nodes([{Node,ok}|Rest],NodesD) ->
    case lists:keysearch(Node,1,NodesD) of
	{value,{_,{up,reactivating}}} ->
	    set_inactive_nodes(Rest,lists:keyreplace(Node,1,NodesD,{Node,{up,{inactive,running}}}));
	{value,{_,{up,{_,Status}}}} ->     % Tracing or trace_failure.
	    set_inactive_nodes(Rest,lists:keyreplace(Node,1,NodesD,{Node,{up,{inactive,Status}}}));
	_ ->                               % This should not happend.
	    set_inactive_nodes(Rest,NodesD)
    end;
set_inactive_nodes([{_Node,_Error}|Rest],NodesD) ->
    set_inactive_nodes(Rest,NodesD);
set_inactive_nodes([],NodesD) ->
    NodesD.
%% -----------------------------------------------------------------------------

%% Returns a list of all node names. Note that it can only be used in the
%% distributed case.
get_all_nodenames_nodes(NodesD) ->
    lists:map(fun({Node,_})->Node end,NodesD).
%% -----------------------------------------------------------------------------

%% Returns a list of all nodes that are up, tracing and running (not suspended),
%% or 'void' in the non-distributed case. This is the list of nodes that shall get
%% inviso commands.
get_nodenames_running_nodes([{Node,{up,{tracing,running}}}|Rest]) ->
    [Node|get_nodenames_running_nodes(Rest)];
get_nodenames_running_nodes([{_Node,_}|Rest]) ->
    get_nodenames_running_nodes(Rest);
get_nodenames_running_nodes([]) ->
    [];
get_nodenames_running_nodes(_) ->
    void.                                   % When non distributed, N/A.
%% -----------------------------------------------------------------------------

%% Returns a list of nodes that can be made to initiate tracing.
get_inactive_running_nodes({up,{inactive,running}}) ->
    local_runtime;
get_inactive_running_nodes(NonDistributed) when not(is_list(NonDistributed)) ->
    [];
get_inactive_running_nodes([{Node,{up,{inactive,running}}}|Rest]) ->
    [Node|get_inactive_running_nodes(Rest)];
get_inactive_running_nodes([{_Node,_}|Rest]) ->
    get_inactive_running_nodes(Rest);
get_inactive_running_nodes([]) ->
    [].
%% -----------------------------------------------------------------------------

%% Returns a list of nodes that are currently tracing (not necessarily running).
%% In the non-distributed case the status of the runtime component will be
%% returned.
%% Note that nodes showing trace_failure will be included since we like to stop
%% tracing at those nodes too.
get_tracing_nodes([{Node,{up,{tracing,_}}}|Rest]) ->
    [Node|get_tracing_nodes(Rest)];
get_tracing_nodes([{Node,{up,{trace_failure,_}}}|Rest]) ->
    [Node|get_tracing_nodes(Rest)];
get_tracing_nodes([{Node,{up,reactivating}}|Rest]) ->
    [Node|get_tracing_nodes(Rest)];
get_tracing_nodes([_|Rest]) ->
    get_tracing_nodes(Rest);
get_tracing_nodes([]) ->
    [];
get_tracing_nodes(AvailableStatus) ->
    AvailableStatus.
%% -----------------------------------------------------------------------------

%% Returns a list of all nodes that are currently up.
get_available_nodes(down) ->
    undefined;
get_available_nodes([{_Node,down}|Rest]) ->
    get_available_nodes(Rest);
get_available_nodes([{Node,_}|Rest]) ->
    [Node|get_available_nodes(Rest)];
get_available_nodes([]) ->
    [].
%% -----------------------------------------------------------------------------

%% Function returning the "state" of Node. Mainly used to check if the node is
%% suspended or not.
%% Returns {State,Status} | reactivating | down
%% where 
get_state_nodes(Node,NodesD) when list(NodesD) ->
    case lists:keysearch(Node,1,NodesD) of
	{value,{_,AvailableStatus}} ->
	    get_state_nodes_2(AvailableStatus);
	false ->
	    false
    end;
get_state_nodes(_,NodesD) ->                % Non distributed case.
    get_state_nodes_2(NodesD).

get_state_nodes_2({up,{trace_failure,Status}}) ->
    {trace_failure,Status};
get_state_nodes_2({up,{State,suspended}}) -> % {tracing|inactive,suspended}
    {State,suspended};
get_state_nodes_2({up,reactivating}) ->
    reactivating;
get_state_nodes_2({up,{State,running}}) ->
    {State,running};
get_state_nodes_2(down) ->
    down.
%% -----------------------------------------------------------------------------

%% Help function in the case we need to consult the state/status of a runtime
%% component. Returns a nodesD value that can be added to the nodes database.
mk_nodes_state_from_status({ok,{tracing,running}}) ->
    {up,{tracing,running}};
mk_nodes_state_from_status({ok,{tracing,{suspended,_SReason}}}) ->
    {up,{tracing,suspended}};
mk_nodes_state_from_status({ok,{_,running}}) ->
    {up,{inactive,running}};
mk_nodes_state_from_status({ok,{_,{suspended,_SReason}}}) ->
    {up,{inactive,suspended}};
mk_nodes_state_from_status({error,_Reason}) ->
    down.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% The session_state.
%% -----------------------------------------------------------------------------

%% The session state reflects if the inviso_tool is tracing or not.
%% This means that if the tool is tracing a reconnected node can be made to
%% restart_session.

%% Returns the correct value indicating that we are tracing now.
tracing_sessionstate() ->
    tracing.
%% -----------------------------------------------------------------------------

%% Returns true or false depending on if we are tracing now or not.
is_tracing(tracing) ->
    true;
is_tracing(_) ->
    false.
%% -----------------------------------------------------------------------------

%% Returns the correct value indicating that the tool is not tracing.
passive_sessionstate() ->
    idle.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% The tracer_data datastructure.
%% -----------------------------------------------------------------------------

%% The tracer_data structure collects the tracer data arguments used to init tracing
%% by this inviso tool. The args are saved per session. Each session has
%% a number.
%% Implementation:
%%     Sessions=[{SessionNr,TDGargs},...]
%%     SessionNr=integer()
%%     TDGargs=list(), args given to the tracer data generator
%%       minus the first argument which is the Node name.

%% Function taking tracerdata args structure inserting yet another session.
%% Returns {SessionNr,NewTDs}.
insert_td_tracer_data(TDGargs,TDs=[{SNr,_}|_]) ->
    {SNr+1,[{SNr+1,TDGargs}|TDs]};
insert_td_tracer_data(TDGargs,undefined) ->
    {1,[{1,TDGargs}]}.
%% -----------------------------------------------------------------------------

%% Returns the latest session nr.
get_latest_session_nr_tracer_data(undefined) ->
    undefined;
get_latest_session_nr_tracer_data([{SessionNr,_}|_]) ->
    SessionNr.
%% -----------------------------------------------------------------------------

%% Returns the tracer data arguments used when creating the trace data for the
%% latest session.
get_latest_tdgargs_tracer_data(undefined) ->
    undefined;
get_latest_tdgargs_tracer_data([{_,TDGargs}|_]) ->
    TDGargs.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% The tc_dict or trace case dictionary datastructure.
%% -----------------------------------------------------------------------------

%% The tc_dict stores information about all available trace cases.
%% Implementation:
%%   [{TCname,Type,VarNames,FNameOn [,FNameOff]},...]
%%     TCname=atom()
%%     Type=on | on_off
%%     VarNames=[atom(),...]
%%     FNameOn=FNameOff=string()

%% Returns the empty trace case dictionary.
mk_tc_dict() ->
    [].
%% -----------------------------------------------------------------------------

%% Function inserting a new trace case into the trace case dictionary.
insert_tracecase_tc_dict(TCname,on,VarNames,FNameOn,TCdict) ->
    [{TCname,on,VarNames,FNameOn}|TCdict].
insert_tracecase_tc_dict(TCname,on_off,VarNames,FNameOn,FNameOff,TCdict) ->
    [{TCname,on_off,VarNames,FNameOn,FNameOff}|TCdict].
%% -----------------------------------------------------------------------------

%% Function finding a trace case definition in the tc_dict structure.
%% Returns {ok,{TCname,Type,VarNAmes,FNameOn [,FNameOff]}} or 'false'.
get_tracecase_tc_dict(TCname,[Tuple|_]) when element(1,Tuple)==TCname ->
    {ok,Tuple};
get_tracecase_tc_dict(TCname,[_|Rest]) ->
    get_tracecase_tc_dict(TCname,Rest);
get_tracecase_tc_dict(_,[]) ->
    false;
get_tracecase_tc_dict(_,_) ->               % There are no trace cases!
    false.
%% -----------------------------------------------------------------------------

%% Function working on the trace case definition returned by get_tracecase_tc_dict/2
%% function.
%% Returning {ok,ActivationFileName}.
get_tc_activate_fname({_TCname,_Type,_VarNames,FNameOn}) ->
    {ok,FNameOn};
get_tc_activate_fname({_TCname,_Type,_VarNames,FNameOn,_FNameOff}) ->
    {ok,FNameOn}.

get_tc_deactivate_fname({_TCname,_Type,_VarNames,_FNameOn,FNameOff}) ->
    {ok,FNameOff};
get_tc_deactivate_fname(_) ->               % Not a case with off function.
    false.

get_tc_varnames({_TCname,_Type,VarNames,_FNameOn}) ->
    VarNames;
get_tc_varnames({_TCname,_Type,VarNames,_FNameOn,_FNameOff}) ->
    VarNames.

%% -----------------------------------------------------------------------------


%% The Command History Log (CHL) stores commands to make it possible to
%% reactivate suspended nodes, reconnect restarted nodes, and to make
%% autostart files.
%% Each time tracing is initiated (that is started) the CHL is cleared since
%% it would not make scense to repeat commands from an earlier tracing at
%% reactivation for instance.

%% Implementation: {NextCounter,OnGoingList,ETStable}
%%   NextCounter=integer(), next command number - to be able to sort them in order.
%%   OnGoingList=[{ProcH,{TCname,ID}},...]
%%   ID=term(), instance id for this execution of this trace case.
%%   ETStable=tid() -> {{TCname,Id},Counter,State1,Bindings}
%%   ETStable=tid() -> {{TCname,Id},Counter,running,Bindings,Result} |
%%                     {{TCname,Id,#Ref},Counter,stop,Bindings} |
%%                     {{TCname,#Ref},Counter,Bindings} % An rtc
%%                     {{M,F,Args,#Ref},Counter}
%%     Counter=integer(), the order-counter for this logged entry.
%%     State1=activating | stopping
%%       Where:
%%         activating: the activation file for the tracecase is running.
%%         running   : activation is completed.
%%         stopping  : set on the previously running ETS entry when deactivation
%%                       file is currently executing.
%%         stop      : entered with own Counter into the ETS table when
%%                       deactivation file is executing. Remains after too.
%%     Result=term(), the result returned from the tr-case or inviso call.


%% Returning an initial empty CHL.
mk_chl(undefined) ->
    {1,[],ets:new(inviso_tool_chl,[set,protected])};
mk_chl({_,_,TId}) ->
    ets:delete(TId),
    mk_chl(undefined).

%% Help function returning 'true' if there is a current history.
history_exists_chl(undefined) ->
    false;
history_exists_chl({_,_,_}) ->
    true.

%% Function looking up the state of this trace case.    
find_id_chl(TCname,Id,{_NextCounter,_OnGoingList,TId}) ->
    case ets:lookup(TId,{TCname,Id}) of
	[{_,_,running,Bindings,_Result}] -> % The trace case is tracing.
	    {ok,Bindings};
	[{_,_,State,_}] ->                  % activating or stopping.
	    State;
	[] ->
	    false
    end.

%% Function finding the Trace case associated with a process handle
%% doing this trace case's activation or stopping.
find_tc_executer_chl(ProcH,{_,OnGoingList,TId}) ->
    case lists:keysearch(ProcH,1,OnGoingList) of
	{value,{_,{TCname,Id}}} ->
	    [{_,_,State,_}]=ets:lookup(TId,{TCname,Id}),
	    {State,{TCname,Id}};            % Should be activating or stopping.
	false ->
	    false
    end.

%% Adds a Trace case to the CHL. This is done when it is turned on. Or when it
%% is called for trace cases that do not have on/off functionality.
set_activating_chl(TCname,Id,{Counter,OnGoingList,TId},Bindings,ProcH) ->
    ets:insert(TId,{{TCname,Id},Counter,activating,Bindings}),
    {Counter+1,[{ProcH,{TCname,Id}}|OnGoingList],TId}.

%% Function marking a trace case as now running. That is the activation
%% phase is completed. It is normaly completed when the process executing
%% the trace case signals that it is done.
set_running_chl(ProcH,TCname,Id,Result,{NextCounter,OnGoingList,TId}) ->
    [{_,Counter,_,Bindings}]=ets:lookup(TId,{TCname,Id}),
    ets:insert(TId,{{TCname,Id},Counter,running,Bindings,Result}),
    NewOnGoingList=lists:keydelete(ProcH,1,OnGoingList),
    {NextCounter,NewOnGoingList,TId}.

%% Function marking trace case TCname with identifier Id as now in its stopping
%% state. Where ProcH is the handler to the process running the stopping
%% trace case.
set_stopping_chl(TCname,Id,{NextCounter,OnGoingList,TId},ProcH)->
    [{_,Counter,_,Bindings,_}]=ets:lookup(TId,{TCname,Id}),
    ets:insert(TId,{{TCname,Id},Counter,stopping,Bindings}),
    ets:insert(TId,{{TCname,Id,make_ref()},NextCounter,stop,Bindings}),
    {NextCounter+1,[{ProcH,{TCname,Id}}|OnGoingList],TId}.

%% Function removing a TCname-Id from the CHL. This is mostly used
%% if activating the trace case failed for some reason. We do not then
%% expect the user to stop the trace case. Hence it must be removed now.
%% A reactivation process may have noticed the activating-entry and started
%% to activate it. But since the general state reached after an unsuccessful
%% activation can not easily be determined, we don't try to do much about it.
del_tc_chl(ProcH,TCname,Id,{NextCounter,OnGoingList,TId}) ->
   ets:delete(TId,{TCname,Id}),
   NewOnGoingList=lists:keydelete(ProcH,1,OnGoingList),
   {NextCounter,NewOnGoingList,TId}.

%% Function removing the entry TCname+Id from the CHL. This makes it
%% possible to activate a tracecase with this id again. The entry was
%% previously marked as stopping.
nullify_chl(ProcH,TCname,Id,{NextCounter,OnGoingList,TId}) ->
    ets:delete(TId,{TCname,Id}),
    NewOnGoingList=lists:keydelete(ProcH,1,OnGoingList),
    {NextCounter+1,NewOnGoingList,TId}.

%% Function stopping all processes saved as being now running tc executers.
%% This is useful as cleanup during stop tracing for instance.
%% Returns a new CHL which is not in all parts correct. Entries in the
%% ETS table are for instance not properly state-changed. But the CHL will
%% from now on only be used to create command files and similar.
stop_all_tc_executer_chl({NextCounter,[{ProcH,_}|Rest],TId}) ->
    exit(ProcH,kill),
    stop_all_tc_executer_chl({NextCounter,Rest,TId});
stop_all_tc_executer_chl({NextCounter,[],TId}) ->
    {NextCounter,[],TId}.

%% Function adding a "plain" inviso call to the CHL.
add_inviso_call_chl(Cmd,Args,{NextCounter,OnGoingList,TId}) ->
    ets:insert(TId,{{inviso,Cmd,Args,make_ref()},NextCounter}),
    {NextCounter+1,OnGoingList,TId}.

%% Function adding a run trace case entry to the chl.
add_rtc_chl(TCname,Bindings,{NextCounter,OnGoingList,TId}) ->
    ets:insert(TId,{{TCname,make_ref()},NextCounter,Bindings}),
    {NextCounter+1,OnGoingList,TId}.
%% Returns the highest used counter number in the command history log.
get_highest_used_counter_chl({NextCounter,_,_}) ->    
    NextCounter-1.

%% Help function returning a list of {{TCname,Id},Phase} for all ongoing
%% assynchronous tracecases.
get_ongoing_chl(undefined) ->
    [];
get_ongoing_chl({_,OngoingList,TId}) ->
    get_ongoing_chl_2(OngoingList,TId).

get_ongoing_chl_2([{_ProcH,{TCname,Id}}|Rest],TId) ->
    case ets:lookup(TId,{TCname,Id}) of
	[{_,_C,activating,_B}] ->
	    [{{TCname,Id},activating}|get_ongoing_chl_2(Rest,TId)];
	[{_,_C,stopping,_B}] ->
	    [{{TCname,Id},deactivating}|get_ongoing_chl_2(Rest,TId)]
    end;
get_ongoing_chl_2([],_) ->
    [].

%% Function returning a list of log entries. Note that the list is unsorted
%% in respect to Counter.
get_loglist_chl({_,_,TId}) ->
    L=ets:tab2list(TId),
    lists:map(fun({{TC,Id},C,S,B,_Result}) -> {{TC,Id},C,S,B}; % running
		 (Tuple={{_TC,_Id},_C,_S,_B}) -> Tuple;      % activating | stopping
		 (Tuple={{_TC,_Id,_Ref},_C,_S,_B}) -> Tuple; % stop
		 (Tuple={{_M,_F,_Args,_Ref},_C}) -> Tuple;
		 (Tuple={{_TC,_Ref},_C,_B}) -> Tuple
	      end,
	      L);
get_loglist_chl(_) ->                       % The history is not initiated, ever!
    [].

%% Function returning a list of log entries, but only those which are not
%% cancelled out by deactivations.
% get_loglist_active_chl({_,_,TId}) ->
%     L=ets:tab2list(TId),
%     lists:zf(fun({{TC,Id},C,S,B,_Result}) -> {true,{{TC,Id},C,S,B}}; % running
% 		(Tuple={{_TC,_Id},_C,_S,_B}) -> Tuple;      % activating | stopping
% 		(Tuple={{_TC,_Id,_Ref},_C,_S,_B}) -> Tuple; % stop
% 		(Tuple={{_M,_F,_Args,_Ref},_C}) -> Tuple
% 	     end,
% 	     L);
% get_loglist_chl(_) ->                       % The history is not initiated, ever!
%     [].


%% This helpfunction recreates a history from a saved history list. This function
%% is supposed to crash if the log is not well formatted. Note that we must restore
%% the counter in order for the counter to work if new commands are added to the
%% history.
replace_history_chl(OldCHL,SortedLog) ->
    {_,Ongoing,TId}=mk_chl(OldCHL),
    {NewTId,Counter}=replace_history_chl_2(TId,SortedLog,0),
    {ok,{Counter+1,Ongoing,NewTId}}.

replace_history_chl_2(TId,[{{TC,Id},C,running,B}|Rest],_Counter) ->
    ets:insert(TId,{{TC,Id},C,running,B,undefined}),
    replace_history_chl_2(TId,Rest,C);
replace_history_chl_2(TId,[{{M,F,Args},C}|Rest],_Counter) ->
    ets:insert(TId,{{M,F,Args,make_ref()},C}),
    replace_history_chl_2(TId,Rest,C);
replace_history_chl_2(TId,[{TC,C,B}|Rest],_Counter) ->
    ets:insert(TId,{{TC,make_ref()},C,B}),
    replace_history_chl_2(TId,Rest,C);
replace_history_chl_2(TId,[],Counter) ->
    {TId,Counter}.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Reactivators data structure.
%% -----------------------------------------------------------------------------

%% Function adding a new node-reactivatorpid pair to the reactivators structure.
%% In this way we know which reactivators to remove if Node terminates, or when
%% a node is fully updated when a reactivator is done.
add_reactivators(Node,Pid,Reactivators) ->
    [{Node,Pid}|Reactivators].

%% Function removing a reactivator entry from the reactivators structure.
del_reactivators(RPid,[{_Node,RPid}|Rest]) ->
    Rest;
del_reactivators(RPid,[Element|Rest]) ->
    [Element|del_reactivators(RPid,Rest)];
del_reactivators(_,[]) ->                   % This should not happend.
    [].

get_node_reactivators(RPid,Reactivators) ->
    case lists:keysearch(RPid,2,Reactivators) of
	{value,{Node,_}} ->
	    Node;
	false ->                            % This should not happend.
	    false
    end.

%% Returns a list of list all nodes that are currently reactivating.
get_all_nodes_reactivators([{Nodes,_Pid}|Rest]) ->
    [Nodes|get_all_nodes_reactivators(Rest)];
get_all_nodes_reactivators([]) ->
    [].

%% Function stopping all running reactivator processes. Returns a new empty
%% reactivators structure. Note that this function does not set the state of
%% Nodes. It must most often be set to running.
stop_all_reactivators([{_Nodes,Pid}|Rest]) ->
    exit(Pid,kill),
    stop_all_reactivators(Rest);
stop_all_reactivators([]) ->
    [].                                     % Returns an empty reactivators.

%% Help function stopping the reactivator (if any) that reactivates Node.
%% Returns a new list of reactivators structure.
stop_node_reactivators(Node,[{Node,Pid}|Rest]) ->
    exit(Pid,kill),
    Rest;
stop_node_reactivators(Node,[NodePid|Rest]) ->
    [NodePid|stop_node_reactivators(Node,Rest)];
stop_node_reactivators(_,[]) ->
    [].
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Started initial trace cases data structure.
%% -----------------------------------------------------------------------------

%% This datastructure keeps information about ongoing trace cases started
%% automatically at init_tracing. These must be automatically stopped when calling
%% stop_tracing.

add_initial_tcs(TCname,Id,StartedInitialTcs) ->
    [{TCname,Id}|StartedInitialTcs].
%% -----------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

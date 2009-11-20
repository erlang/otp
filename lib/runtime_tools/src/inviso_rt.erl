%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
%% Ann-Marie Löf, ann-marie.lof@st.se
%% Lennart Öhman, lennart.ohman@st.se
%% -----------------------------------------------------------------------------

-module(inviso_rt).


%% -----------------------------------------------------------------------------
%% interface for supervisor
%% -----------------------------------------------------------------------------
-export([start_link_man/3,start_link_auto/1]).

%% API for controll component.
-export([start/4,stop/1, 
	 init_tracing/2,stop_tracing_parallel/1,
	 try_to_adopt/3,confirm_connection/2,get_node_info/1,
	 suspend/2,call_suspend/2,cancel_suspension/1,change_options/2,
	 clear/2,clear_all_tp/1,
	 flush/1,
	 trace_patterns_parallel/3,
	 trace_flags_parallel/3,trace_flags_parallel/2,trace_flags_parallel/1,
	 meta_tracer_call_parallel/2,
	 get_status/1,get_tracerdata/1,list_logs/1,list_logs/2,fetch_log/2,fetch_log/3,
	 delete_log/1,delete_log/2,
	 state/1]).
%% -----------------------------------------------------------------------------

%% API mostly for autostart scripts, instead of corresponding control component
%% apis not available doing local function calls.
-export([init_tracing/1,tp/4,tp/5,tp/1,tpg/4,tpg/5,tpg/1,
	 tpl/4,tpl/5,tpl/1,
	 ctp/1,ctp/3,ctpg/1,ctpg/3,ctpl/1,ctpl/3,
	 init_tpm/4,init_tpm/7,
	 tpm/4,tpm/5,tpm/8,tpm_tracer/4,tpm_tracer/5,tpm_tracer/8,
	 tpm_ms/5,tpm_ms_tracer/5,
	 ctpm_ms/4,
	 local_register/0,global_register/0,
	 ctpm/3,remove_local_register/0,remove_global_register/0,
	 tf/2,tf/1,ctf/2,ctf/1]).
%% -----------------------------------------------------------------------------

%% Internal exports.
-export([init/4,auto_init/2,fetch_init/4]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Constants.
%% -----------------------------------------------------------------------------

-define(DEFAULT_OVERLOAD_FUNC,default_overload_func).
-define(NO_LOADCHECK,no_loadcheck).

-define(RT_SUP,runtime_tools_sup).          % Refers to the registered name.
-define(CTRL,inviso_c).                     % Refers to the registered name.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Record definition.
%% -----------------------------------------------------------------------------

%% #rt
%% All record fields must be bound to listed values when leaving init or 
%% auto_init.
%% dependency: Timeout accepting being without control component.
%% overload : Controlls which module to call, if any, when time for a check.
%% timer_ref: Used when timing delayed shutdown due to lost control component.
-record(rt,{state = new,            % new | idle | tracing
	    status = running,       % running | {suspended, Reason}
	    next_loadcheck = now(), % now | "No Loadcheck"
	    parent,                 % pid()
	    tracerdata,             % undefined|{fun(),term()}|{file,Param}|{ip,Param}
	    tracer_port,            % port() | undefined
	    handler,                % {fun(), term()} | undefined
	    auto_starter,           % pid() | undefined; proc starting interpreters.
	    meta_tracer,            % undefined | pid()
	    fetchers=[],            % [pid(),...] processes transfering logfiles.
%	     spies = [],
	    dependency={infinity,node()}, % {TOut,Node} | TOut; TOut=int()|infinity
	    overload=no_loadcheck,  % ?NO_LOADCHECK|{LoadMF,Interval,InitMFA,RemoveMFA}
	    overload_data=void,     % Datastructure given to LoadMF and RemoveMFA.
	    timer_ref,              % undefined | reference()
	    ctrl,                   % undefined | pid()
	    ctrl_ref,               % undefined | reference()
	    vsn,                    % list()
	    tag                     % term()
	   }).
%% -----------------------------------------------------------------------------


%% ==============================================================================
%% Start API
%% ==============================================================================

%% Note that the runtime component may be started in many different ways.
%% It can be autostarted by the runtime_tools_sup during initial start-up of the
%% system. It is actually most likely that it will be started that way. However
%% if there are no autostart trace-cases to run, the inviso_rt runtime component
%% will terminate. It will then however remain as a child of the runtime_tools_sup
%% supervisor. This means that if the runtime component is started again, manually,
%% by the control component, some actions must be taken.
%% For instance is it very likely that the child already exists. But since it
%% must be started with different arguments when started manually, the child-spec
%% must be changed.
%%
%% The runtime component is not a proper gen_server, to allow full control of
%% what happens. It however mimcs gen_server behaviour to be managed by the
%% runtime_tools_sup supervisor.


%% start_link_auto(AutoModArgs)={ok,Pid}
%%
%% This function is entered into the child-spec when planning on doing autostart
%% of the runtime component. The autostart is controlled by the so called
%% inviso_autostart_mod. It is an application environment parameter of the
%% runtime_tools application. If it exists, it shall point out a module name.
%% If it does not exist, the default 'inviso_autostart' module will be tried.
%% Note that these start_link functions do not implement proper otp-behaviour.
%% For instance they return {ok,Pid} immediately making the init-phase of the
%% runtime component process empty.
%%
%% The inviso_autostart_mod shall export one function:
%%   autostart(AutoModArgs) -> {MFA,Options,Tag}, where
%%     AutoModArgs=term(), comes from the application start parameters in the
%%       runtime_tools application resource file.
%%     MFA={Mod,Func,Args} | term().
%%       If it is MFA it will cause a trace initiator process to start spawning
%%       on spawn_link(Mod,Func,Args). The trace initiator may for instance
%%       initiate the wanted tracing.
start_link_auto(AutoModArgs) ->
    {ok,spawn_link(?MODULE,auto_init,[AutoModArgs,self()])}.
%% ------------------------------------------------------------------------------

%% This function is entered into the child-specification of the runtime_tools_sup
%% if the runtime component shall be started manually via the control component.
start_link_man(Ctrl,Options,Tag) ->
    {ok,spawn_link(?MODULE,init,[Ctrl,Options,Tag,self()])}.
%% ------------------------------------------------------------------------------

%% start(Node,Options,Tag,Condition)=tbd
%%   Node=The node where the runtime component shall be started.
%%   Options=[Opt]; List of options to the runtime component.
%%     Opt={dependency,Val}|{dependency,{Val,Node}}
%%       Val=int()|infinity
%%       If the runtime component may run on its own or not. Val=0 means a runtime
%%       component which will terminate immediately without its control component.
%%       Note that if the runtime component is started manually, the Node part
%%       is never used. The runtime is supposed to be dependent of the Ctrl mentioned
%%       in the start_link_man parameters.
%%     Opt={overload,OverLoad} | overload
%%           The latter means no loadcheck. Necessary if changing the options.
%%       Overload=Iterval (int() in milliseconds) |
%%         {LoadMF,Interval}|{LoadMF,Interval,InitMFA,RemoveMFA}
%%           LoadMF={Mod,Func}|function()
%%           InitMFA,RemoveMFA={Mod,Func,ArgList} where
%%             apply(InitM,InitF,InitArgs) -> {ok,DataStruct}|'void'.
%%             apply(RemoveM,RemoveF,[DataStruct|Args]) -> don't care
%%             LoadMF is called each time loadcheck is performed.
%%               Mod:Func(DataStruct)->ok|{suspend,Reason}
%%           If just Interval is used, it means using a default overload check.
%%     Tag=term(), used to identify an incarnation of a runtime component so that
%%       a control component reconnecting will know if it was its own incarnation
%%       still alive, or some elses.
%%     Condition='if_ref'|term(). Controls if we want to adopt the runtime component.
%%       If 'if_ref' is stated it means that we only want to adopt a runtime component
%%       with the suggested Tag.
%%
%% This is the API used by the control component when tries to start a runtime
%% component. Note that it will try to adopt an already running, if possible.
%% Adoptions are only possible if the runtime component at hand is running
%% without control component.
start(Node, Options, Tag, Condition) when Node == node() ->
    ChildSpec = {?MODULE, {?MODULE, start_link_man, [self(), Options, Tag]}, 
		 temporary, 5000, worker, [?MODULE]},
    case catch supervisor:start_child(?RT_SUP, ChildSpec) of
	{ok, Pid} when is_pid(Pid) ->
	    {node_info, _Node, Pid, VSN, State, Status, _Tag} = 
		get_node_info(Pid),
	    {node_info, Node, Pid, VSN, State, Status, new};
	{error, already_present} ->
	    supervisor:delete_child(?RT_SUP, ?MODULE),
	    start(Node, Options, Tag, Condition);
	{error, {already_started, Pid}} ->
	    try_to_adopt(Pid, Tag, Condition);
	{error,Reason} ->
	    {error,Reason};
	{'EXIT',Reason} ->
	    {error,Reason}
    end;
start(Node, Options, Tag, Condition) ->
    case rt_version(Node) of
	{error,Error} ->
	    {error,Error};
	_VSN ->
	    ChildSpec = {?MODULE, {?MODULE, start_link_man, 
				    [self(), Options, Tag]}, 
			 temporary, 5000, worker, [?MODULE]},
	    case catch rpc:call(Node, supervisor, start_child, 
				[?RT_SUP, ChildSpec]) of
		{ok, Pid} when is_pid(Pid) ->
		    {node_info, _Node, Pid, 
		     VSN, State, Status, _Tag} = get_node_info(Pid),
		    {node_info, Node, Pid, VSN, State, Status, new};
		{error, already_present} ->
		    rpc:call(Node, supervisor, delete_child, 
			     [?RT_SUP, ?MODULE]),
		    start(Node, Options, Tag, Condition);
		{error, {already_started, Pid}} ->
		    try_to_adopt(Pid, Tag, Condition);
		{error,Reason} ->           % Could not start child.
		    {error,Reason};
		{badrpc,nodedown} ->
		    {error,nodedown};
		{badrpc,Reason} ->
		    {error,{badrpc,Reason}};
		{'EXIT',Reason} ->
		    {error,Reason}
	    end
    end.

rt_version(Node) ->
    case catch rpc:call(Node,application,loaded_applications,[]) of
	List when is_list(List) ->
	    case lists:keysearch(runtime_tools,1,List) of
		{value,{_,_,VSN}} ->
		    VSN;
		false ->
		    {error,not_loaded}
	    end;
	{badrpc,nodedown} ->
	    {error,nodedown};
	{'EXIT',Reason} -> 
	    {error,Reason}
    end.
%% ------------------------------------------------------------------------------

%% stop(Node)=ok|{error,Reason}
%% Stops the runtim component on node Node. Note that this is mearly calling the
%% supervisor API to shutdown the inviso_rt child belonging to the runtime_tools_sup.
stop(Node) when Node==node() ->
    supervisor:terminate_child(?RT_SUP,?MODULE),
    supervisor:delete_child(?RT_SUP,?MODULE),
    ok;
stop(Node) ->
    case catch rpc:call(Node,supervisor,terminate_child,[?RT_SUP,?MODULE]) of
	ok ->
	    stop_delete_child(Node);
	{error,_} ->                        % No child running.
	    stop_delete_child(Node);        % Make sure we remove it also.
	{badrpc,Reason} ->
	    {error,{badrpc,Reason}};
	{'EXIT',Reason} ->
	    {error,Reason}
    end.

stop_delete_child(Node) ->
    case catch rpc:call(Node,supervisor,delete_child,[?RT_SUP,?MODULE]) of
	ok ->
	    ok;
	{error,_} ->                        % No child running.
	    ok;
	{badrpc,Reason} ->
	    {error,{badrpc,Reason}};
	{'EXIT',Reason} ->
	    {error,Reason}
    end.
%% ------------------------------------------------------------------------------


%% ==============================================================================
%% API for the control component.
%% ==============================================================================

%% init_tracing(TracerData) ->
%%   TracerData = LogTD | [{trace,LogTD},{ti,TiTD}]
%%   LogTD = {HandlerFun, Data} | collector |
%%           {relayer, pid()} | {ip, IPPortParameters} |
%%           {file, FilePortParameters}
%%   TiTD = {file,FileName} | {file,FileName,{InitPublLD,RemovePublLD,CleanPublLD}}
%%          | {relay,Node} | {relay,Node,{InitPublLD,RemovePublLD,CleanPublLD}}
%%   HandlerFun=fun(TraceMsg,Data)->NewData
%%   IPPortParameters = Portno | {Portno, Qsiz}
%%   Qsiz = 
%%   FilePortParameters = {Filename, wrap, Tail, {time, WrapTime}, WrapCnt} |
%%                        {FileName, wrap, Tail, WrapSize, WrapCnt} |
%%                        {FileName, wrap, Tail, WrapSize} |
%%                        {FileName, wrap, Tail} | FileName
%% Defines a tracer:
%% {HandlerFun, Data} - will be used as handler inside the runtime component for
%%   every incomming trace message.
%% relayer - the runtime component will relay all comming trace messages to
%%   the runtime component Pid.
%% collector - the runtime component is used as tracer or collector of relayed 
%%             trace messages using the default handler writing them to io.
%% ip | file - will start a tracer port using PortParameters
init_tracing(Pid,TracerData) ->
    call(Pid,{init_tracing,TracerData}).
%% ------------------------------------------------------------------------------

%% stop_tracing(RTpids)=[{Node,NodeResult},...]
%%   RTpids=[RTinfo,...]
%%     RTinfo={RTpid,Node} | {{error,Reason},Node}
%%   NodeResult={ok,State} | {error,Reason}
%% Sends a request to stop tracing to all nodes in RTpids, in parallel. Stop
%% tracing means that all trace flags are removed and the nodes go to idle
%% state.
stop_tracing_parallel(RTpids) ->
    call_parallel(lists:map(fun({Pid,Node})->{Pid,Node,stop_tracing};
			       (Error)->Error
			    end,
			    RTpids)).
%% ------------------------------------------------------------------------------

%% try_to_adopt(Pid,NewTag,Condition)=
%%   {node_info,node(),self(),VSN,State,Status,{tag,PreviousTag}}|{error,Reason}
%%   NewTag=term(), the identification tag we want the runtime component to use
%%     from now on if adoption was successful.
%%   Condition='if_ref', only adopt if current tag is NewTag.
%%   PreviousTag= the tag the runtime component had before it accepted the
%%     adoption.
%% This function shall only be used by a control component wishing to adopt this
%% runtime component.
try_to_adopt(Pid, Tag, Condition) ->
    call(Pid,{try_to_adopt,Tag,Condition}).
%% ------------------------------------------------------------------------------

%% confirm_connection(Pid,Tag)= {node_info,node(),self(),VSN,State,Status,Tag}|
%%   {error,refused}.
%% Must only be used by a control component having been contacted by the runtime
%% component Pid. It confirms to the runtime component that the control component
%% has accepted the connect request.
confirm_connection(Pid,Tag) ->
    call(Pid,{confirm_connection,Tag}).
%% ------------------------------------------------------------------------------

%% get_node_info(Pid)={node_info,Node,Pid,VSN,State,Status,Tag}.
get_node_info(Pid) ->
    call(Pid,get_node_info).
%% ------------------------------------------------------------------------------

%% suspend(NodeOrPid,Reason)=ok
%% call_suspend(NodeOrPid,Reason)=ok
%% Makes the runtime component and all of its helpers suspend. suspend/2 is
%% assynchronous.
suspend(NodeOrPid,Reason) ->
    cast(NodeOrPid,{suspend,Reason}).

call_suspend(NodeOrPid,Reason) ->
    call(NodeOrPid,{suspend,Reason}).
%% ------------------------------------------------------------------------------

%% cancel_suspension(Pid)=ok
%% Function moving the runtime component to status running. Regardless of its
%% current status.
cancel_suspension(Pid) ->
    call(Pid,cancel_suspension).
%% ------------------------------------------------------------------------------

%% change_options(Pid,Options)=ok
%%   Options=list(); see the start_link_XXX functions.
%% Changes options according to Options list.
%% Changing the control component we shall be depending on has no effect. The
%% dependency value in self can however be changed, and takes effect immediately.
change_options(Pid,Options) ->
    call(Pid,{change_options,Options}).
%% ------------------------------------------------------------------------------

%% clear_all_tp(Pid)=ok
%% Function removing all, both local and global trace-patterns from the node.
clear_all_tp(Pid) ->
    call(Pid,clear_all_tp).
%% ------------------------------------------------------------------------------

%% clear(Pid,Options)={ok,{new,Status}}
%%   Options=[Opt,...]
%%     Opt=keep_trace_patterns | keep_log_files
%% Resets the runtime component to state 'new' by stopping all ongoing tracing,
%% closing and removing all associated logfiles. The Options can be used to
%% prevent the runtime component from being totally erased.
clear(Pid,Options) ->
    call(Pid,{clear,Options}).
%% ------------------------------------------------------------------------------

%% flush(Pid)=ok | {error,Reason}
%% Sends the flush command to the trace-port, if we are using a trace-port and
%% are tracing.
flush(Pid) ->
    call(Pid,flush).
%% ------------------------------------------------------------------------------

%% trace_patterns_parallel(RTpids,Args,Flags)=[{Node,Answer},...]
%%   RTpids=[{RTpid,Node},...] or [{Error,Node},...]
%%   Args=[Arg,...]
%%     Arg={Mod,Func,Arity,MS}|{Mod,Func,Arity,MS,Opts}
%%       Mod=atom()|reg_exp()|{Dir,reg_exp()}
%%         Dir=reg_exp()
%%   Answer=[Answer,...]
%%     Answer=int()|{error,Reason}
%% API function for the control component sending trace-patterns to a list of
%% runtime components. Returns a [{Node,Answer},...] list in the same order.
trace_patterns_parallel(RTpids,Args,Flags) ->     % Same args and flags for all.
    call_parallel(lists:map(fun({Pid,Node})when is_pid(Pid)->{Pid,Node,{tp,Args,Flags}};
			       (Error)-> Error
			    end,
			    RTpids)).
%% ------------------------------------------------------------------------------

%% trace_flags_parallel(RTpids,Args,How)=
%% trace_flags_parallel(RTpidsArgs,How)=
%% trace_flags_parallel(RTpidsArgsHow)=[{Node,Reply},...]
%%   RTpids=[RTpidEntry,...]
%%     RTpidEntry={RTpid,Node}|{Error,Node}
%%     Error=term(), any term you wish to have as reply in Answer assoc. to Node.
%%   Args=[{Process,Flags},...]
%%     Process=pid()|registeredname()|'all'|'new'|'existing'
%%     Flags=List of the allowed process trace flags.
%%   RTpidsArgs=[RTpidArgEntry,...]
%%     RTpidArgEntry={RTpid,Node,Args}|{Error,Node}
%%   RTpidsArgsHow=[RTpidArgsHowEntry,...]
%%     RTpidArgsHowEntry={RTpid,Node,Args,How}|{Error,Node}
%%   How=true|false
%%   Reply={ok,Answers}
%%     Answers=[Answer,...], one for each Args and in the same order.
%%       Answer=int()|{error,Reason}
%% API function used by the control component to send flags to a list of runtime
%% components. Returns a list of [{Node,Answer},... ] in the same order.
trace_flags_parallel(RTpids,Args,How) ->     % Same args for every node!
    call_parallel(lists:map(fun({Pid,Node})when is_pid(Pid)->{Pid,Node,{tf,Args,How}};
			       (Error)-> Error
			    end,
			    RTpids)).

trace_flags_parallel(RTpidArgs,How) ->       % Different args but same how.
    call_parallel(lists:map(fun({Pid,Node,Args})when is_pid(Pid)->
				    {Pid,Node,{tf,Args,How}};
			       (Error)->
				    Error
			    end,
			    RTpidArgs)).

trace_flags_parallel(RTpidArgsHow) ->        % Both different args and hows.
    call_parallel(lists:map(fun({Pid,Node,Args,How})when is_pid(Pid)->
				    {Pid,Node,{tf,Args,How}};
			       (Error)->
				    Error
			    end,
			    RTpidArgsHow)).
%% ------------------------------------------------------------------------------

%% meta_pattern(RTpids,Args)=[{Node,Answer},...]
%%   RTpids=[{RTpid,Node},...] or [{Error,Node},...]
%%   Args={FunctionName,ArgList}
%%     FunctionName=atom()
%%     ArgList=list(), list of the arguments to FunctionName.
%%   Answer=[Answer,...]
%%     Answer=int()|{error,Reason}
%% Makes a call to the meta-tracer through its runtime component. Returns a list
%% a answers in the same order as RTpids. Note that if "someone" has discovered
%% that there is an error with a particular node, the error answer can be placed
%% in the RTpids list from the start.
meta_tracer_call_parallel(RTpids,Args) ->    % Same args for all nodes.
    call_parallel(lists:map(fun({Pid,Node})when is_pid(Pid)->
				    {Pid,Node,{meta_tracer_call,Args}};
			       (Error)->
				    Error
			    end,
			    RTpids)).
%% ------------------------------------------------------------------------------

%% get_status(Pid)={ok,{State,Status}}
%%   State=new|tracing|idle
%%   Status=running|{suspended,Reason}
get_status(Pid) ->
    call(Pid,get_status).
%% ------------------------------------------------------------------------------

%% get_tracerdata(Pid)={ok,TracerData} | {ok,no_tracerdata} | {error,Reason}
%%   TracerData=see init_tracing
%% Fetches the current tracerdata from the runtime component.
get_tracerdata(Pid) ->
    call(Pid,get_tracerdata).
%% ------------------------------------------------------------------------------

%% list_log(Pid)={ok,no_log}|{ok,LogCollection}|{error,Reason}
%% list_log(Pid,TracerData)=
%%   LogCollection=[LogTypes,...]
%%     LogTypes={trace_log,Dir,Files}|{ti_log,Dir,Files}
%%     Dir=string()
%%     Files=[FileNameWithoutDir,...]
%% Lists all files associated with the current tracerdata. Or finds out which
%% files there are stored in this node given a tracerdata.
list_logs(Pid) ->
    call(Pid,list_logs).
list_logs(Pid,TD) ->
    call(Pid,{list_logs,TD}).
%% ------------------------------------------------------------------------------

%% fetch_log(Pid,CollectPid)={ok,FetcherPid}|{complete,no_log}|{error,Reason}
%% fetch_log(Pid,CollectPid,Spec)=
%%   CollectPid=pid(), the process which will be given the transfered logs.
%%   Spec=TracerData|LogCollection
%% Transferes a number of files using ditributed Erlang to CollectPid. This
%% function is supposed to be used internally by a control component. It returns
%% when the transfer is initiated and does not mean it is done or successful.
fetch_log(Pid,CollectPid) ->
    call(Pid,{fetch_log,CollectPid}).
fetch_log(Pid,CollectPid,Spec) ->
    call(Pid,{fetch_log,CollectPid,Spec}).
%% ------------------------------------------------------------------------------

%% delete_log(Pid,TracerDataOrLogList)={ok,Results}|{error,Reason}
%%   TracerDataOrLogList=[FileNameWithPath,...]|LogCollection|TracerData
%%   Results=[LogType,...]
%%   LogType={trace_log,FileSpecs}|{ti_log,FilesSpecs}
%%     FilesSpecs=[FileSpec,...]
%%       FileSpec={ok,FileName}|{error,{Posix,FileName}}
%%       Filename=string(), the filename without dir-path.
delete_log(Pid) ->
    call(Pid,delete_logs).
delete_log(Pid,TracerDataOrLogList) ->
    call(Pid,{delete_logs,TracerDataOrLogList}).
%% ------------------------------------------------------------------------------

%% state(NodeOrPid)=LoopData
%% Returns the loopdata of the runtime component. Only meant for debugging.
state(NodeOrPid) ->
    call(NodeOrPid,state).
%% ------------------------------------------------------------------------------


%% ==============================================================================
%% API for local calls made from the same node. E.g autostart.
%% ==============================================================================

%% init_tracing(TracerData)=
%% See init_tracing/2.
init_tracing(TracerData) ->
    call_regname(?MODULE,{init_tracing,TracerData}).
%% ------------------------------------------------------------------------------


%% Meaning that these function does most often not have to be called by a
%% control component because there are more efficient ones above.

%% tp(Module,Function,Arity,MatchSpec) ->
%% tp(Module,Function,Arity,MatchSpec,Opts) ->
%% tp(PatternList) -> 
%%   Module = '_'|atom()|ModRegExp|{DirRegExp,ModRegExp}
%%   Function == atom() | '_'
%%   Arity = integer() | '_'
%%   MatchSpec = true | false | [] | matchspec() see ERTS User's guide for a
%%   description of match specifications.
%%   Opts=list(); 'only_loaded'
%%   PatternList = [Pattern],
%%   Pattern = {Module,Function,Arity,MatchSpec,Opts},
%% Set trace pattern (global).
tp(Module,Function,Arity,MatchSpec) ->
    tp(Module,Function,Arity,MatchSpec,[]).
tp(Module,Function,Arity,MatchSpec,Opts) ->
    call_regname(?MODULE,{tp,[{Module,Function,Arity,MatchSpec,Opts}],[global]}).
tp(PatternList) -> 
    call_regname(?MODULE,{tp,PatternList,[global]}).
%% ------------------------------------------------------------------------------

tpg(Mod,Func,Arity,MatchSpec) ->
    tp(Mod,Func,Arity,MatchSpec).
tpg(Mod,Func,Arity,MatchSpec,Opts) ->
    tp(Mod,Func,Arity,MatchSpec,Opts).
tpg(PatternList) ->
    tp(PatternList).
%% ------------------------------------------------------------------------------

%% tpl(Module,Function,Arity,MatchSpec) ->
%% tpl(Module,Function,Arity,MatchSpec,Opts) ->
%% tpl(PatternList) ->
%%   Module = Function == atom() | '_' | RegExpMod | {RegExpDir,RegExpMod}
%%   Arity = integer() | '_'
%%   MatchSpec = true | false | [] | matchspec() see ERTS User's guide for a
%%   Opts=list(); 'only_loaded'
%%   description of match specifications.
%%   PatternList = [Pattern],
%%   Pattern = {Module, Function, Arity, MatchSpec},
%% Set trace pattern (local).
tpl(Module,Function,Arity,MatchSpec) ->
    call_regname(?MODULE,{tp,[{Module,Function,Arity,MatchSpec,[]}],[local]}).
tpl(Module,Function,Arity,MatchSpec,Opts) ->
    call_regname(?MODULE,{tp,[{Module,Function,Arity,MatchSpec,Opts}],[local]}).
tpl(PatternList) ->
    call_regname(?MODULE,{tp,PatternList,[local]}).
%% ------------------------------------------------------------------------------

%% ctp(Module,Function,Arity) ->
%% ctp(PatternList)=
%%   Module = atom()|'_'|RegExpMod|{RegExpDir,RegExpMod}
%%   Function == atom() | '_'
%%   Arity = integer() | '_'
%%   PatternList=[{Mod,Func,Arity},...]
%% Clear trace pattern (global).
%% Note that it is possible to clear patterns using regexps. But we can for
%% natural reasons only clear patterns for loaded modules. Further more there
%% seems to be a fault in the emulator (<=R10B) crashing if we remove patterns
%% for deleted modules. Therefore we use the only_loaded option.
ctp(Module,Function,Arity) ->
    call_regname(?MODULE,{tp,[{Module,Function,Arity,false,[only_loaded]}],[global]}). 
ctp(PatternList) ->
    call_regname(?MODULE,
		 {tp,
		  lists:map(fun({M,F,A})->{M,F,A,false,[only_loaded]} end,PatternList),
		  [global]}).
%% ------------------------------------------------------------------------------

ctpg(Mod,Func,Arity) ->
    ctp(Mod,Func,Arity).
ctpg(PatternList) ->
    ctp(PatternList).
%% ------------------------------------------------------------------------------

%% ctpl(Module,Function,Arity) ->
%%   Module = atom()|'_'|RegExpMod|{RegExpDir,RegExpMod}
%%   Function == atom() | '_'
%%   Arity = integer() | '_'
%%   PatternList=[{Mod,Func,Arity},...]
%% Clear trace pattern (local).
ctpl(Module,Function,Arity) ->
    call_regname(?MODULE,{tp,[{Module,Function,Arity,false,[only_loaded]}],[local]}). 
ctpl(PatternList) ->
    call_regname(?MODULE,
		 {tp,
		  lists:map(fun({M,F,A})->{M,F,A,false,[only_loaded]} end,PatternList),
		  [local]}).
%% ------------------------------------------------------------------------------

init_tpm(Mod,Func,Arity,CallFunc) ->
    call_regname(?MODULE,{meta_tracer_call,{init_tpm,[Mod,Func,Arity,CallFunc]}}).

init_tpm(Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    call_regname(?MODULE,
		 {meta_tracer_call,
		  {init_tpm,
		   [Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc]}}).
%% ------------------------------------------------------------------------------

tpm(Mod,Func,Arity,MS) ->
    call_regname(?MODULE,{meta_tracer_call,{tpm,[Mod,Func,Arity,MS]}}).
tpm(Mod,Func,Arity,MS,CallFunc) ->
    call_regname(?MODULE,{meta_tracer_call,{tpm,[Mod,Func,Arity,MS,CallFunc]}}).
tpm(Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    call_regname(?MODULE,
		 {meta_tracer_call,
		  {tpm,
		   [Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc]}}).
%% ------------------------------------------------------------------------------

tpm_tracer(Mod,Func,Arity,MS) ->
    call_regname(?MODULE,{meta_tracer_call,{tpm_tracer,[Mod,Func,Arity,MS]}}).
tpm_tracer(Mod,Func,Arity,MS,CallFunc) ->
    call_regname(?MODULE,{meta_tracer_call,{tpm_tracer,[Mod,Func,Arity,MS,CallFunc]}}).
tpm_tracer(Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    call_regname(?MODULE,
		 {meta_tracer_call,
		  {tpm_tracer,
		   [Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc]}}).
%% ------------------------------------------------------------------------------

tpm_ms(Mod,Func,Arity,MSname,MS) ->
    call_regname(?MODULE,{meta_tracer_call,{tpm_ms,[Mod,Func,Arity,MSname,MS]}}).
%% ------------------------------------------------------------------------------

tpm_ms_tracer(Mod,Func,Arity,MSname,MS) ->
    call_regname(?MODULE,{meta_tracer_call,{tpm_ms_tracer,[Mod,Func,Arity,MSname,MS]}}).
%% ------------------------------------------------------------------------------

ctpm_ms(Mod,Func,Arity,MSname) ->
    call_regname(?MODULE,{meta_tracer_call,{ctpm_ms,[Mod,Func,Arity,MSname]}}).
%% ------------------------------------------------------------------------------

local_register() ->
    call_regname(?MODULE,{meta_tracer_call,{local_register,[]}}).
%% ------------------------------------------------------------------------------

global_register() ->
    call_regname(?MODULE,{meta_tracer_call,{global_register,[]}}).
%% ------------------------------------------------------------------------------

ctpm(Mod,Func,Arity) ->
    call_regname(?MODULE,{meta_tracer_call,{ctpm,[Mod,Func,Arity]}}).
%% ------------------------------------------------------------------------------

remove_local_register() ->
    call_regname(?MODULE,{meta_tracer_call,{remove_local_register,[]}}).
%% ------------------------------------------------------------------------------

remove_global_register() ->
    call_regname(?MODULE,{meta_tracer_call,{remove_global_register,[]}}).
%% ------------------------------------------------------------------------------

%% tf(PidSpec, FlagList) ->
%% tf(TraceConfList) ->
%%   TraceConfList = [{PidSpec, FlagList}],
%%   FlagList = [Flags],
%%   PidSpec = all | new | existing | pid() | registeredname()
%%   Flags = all | send | 'receive' | procs | call | silent | return_to |
%%   running | garbage_collection | timestamp | cpu_timestamp | arity | 
%%   set_on_spawn | set_on_first_spawn | set_on_link | set_on_first_link
%% Set trace flags.
tf(PidSpec, FlagList) ->
    call_regname(?MODULE,{tf,[{PidSpec,FlagList}],true}).

tf(TraceConfList) -> 
    call_regname(?MODULE,{tf,TraceConfList,true}).
%% ------------------------------------------------------------------------------

%% ctf(PidSpec, FlagList) ->
%% ctf(TraceConfList) ->
%%   TraceConfList = [{PidSpec, FlagList}],
%%   FlagList = [Flags],
%%   PidSpec = all | new | existing | pid() | registeredname()
%%   Flags = all | send | 'receive' | procs | call | silent | return_to |
%%   running | garbage_collection | timestamp | cpu_timestamp | arity | 
%%   set_on_spawn | set_on_first_spawn | set_on_link | set_on_first_link
%% Clear trace flags.
ctf(PidSpec, FlagList) ->
    call_regname(?MODULE,{tf,[{PidSpec,FlagList}],false}).

ctf(TraceConfList) -> 
    call_regname(?MODULE,{tf_as,TraceConfList,false}).
%% ------------------------------------------------------------------------------


%% ------------------------------------------------------------------------------
%% Client side functions.
%% ------------------------------------------------------------------------------

%% Call function managing the client to server communication. This function may
%% be run by a client on a different node.
%% Note that we must use two different functions for calling a named process and
%% calling the runtime component at a specified node.
call(Pid,Request) when is_pid(Pid) ->
    call_2(Pid,Request);
call(Node,Request) when Node==node() ->     % To our node!
    call_2(?MODULE,Request);
call(Node,Request) when is_atom(Node) ->
    call_2({?MODULE,Node},Request);
call(To,_Request) ->
    {error,{badarg,To}}.

call_regname(Name,Request) when is_atom(Name) -> % To a registered name.
    call_2(Name,Request).

call_2(To,Request) ->
    MRef=erlang:monitor(process,To),        % Use a monitor to avoid waiting for ever.
    Ref=make_ref(),
    case catch To ! {Request,self(),Ref} of % Can be a remote pid.
	{'EXIT',_} ->                       % If we use registered name.
	    erlang:demonitor(MRef),         % Maybe not necessary!?
	    receive
		{'DOWN',MRef,_Type,_Obj,_Info} ->
		    true
	    after
		0 ->
		    true
	    end,
	    {error,not_started};
	_ ->                                % At least no obvious error.
	    receive
		{Msg,Ref} ->
		    erlang:demonitor(MRef),
		    Msg;
		{'DOWN',MRef,_Type,_Obj,Info} -> % The runtime component disapeared.
		    {error,{no_response,Info}}
	    end
    end.
%% -----------------------------------------------------------------------------

%% Multicall function taking a list of [{Pid,Node,Request},...] and sends
%% a request to every Pid. This function then also allows you to send multiple
%% requests to the same Pid since it will sit and wait for all replies.
%% Note that RTspec may also be an [{{error,Reason},Node},...]. That tuple will
%% then be used as reply in the reply list.
%% Returns [{Node,Reply},...] for every element in RTspec, in the same order.
call_parallel(RTspec) ->
    Ref=make_ref(),
    {Nr,Pending}=call_parallel_2(RTspec,Ref,0,[]),
    Replies=call_parallel_3(Ref,Pending,Nr,[],[]),
    call_parallel_build_reply(RTspec,1,Replies).

call_parallel_2([{Pid,Node,Request}|Rest],Ref,Nr,Pending) when is_pid(Pid) ->
    Pid ! {Request,self(),{Ref,Nr+1}},
    MRef=erlang:monitor(process,Pid),        % So we won't wait for ever for it.
    call_parallel_2(Rest,Ref,Nr+1,[{Nr+1,Node,MRef}|Pending]);
call_parallel_2([{{error,_Reason},_Node}|Rest],Ref,Nr,Pending) ->
    call_parallel_2(Rest,Ref,Nr,Pending); % Just skip it. This is no process.
call_parallel_2([_Faulty|Rest],Ref,Nr,Pending) -> % Should not happend.
    call_parallel_2(Rest,Ref,Nr,Pending); % But we choose to skip it instead of crash.
call_parallel_2([],_,Nr,Pending) ->
    {Nr,Pending}.

%% Help function collecting reply-messages sent from the runtime components. We
%% count down until we got a reply for every pending request. Or if we get a DOWN
%% message indicating that the runtime component is no longer present. Note that
%% we can by accident read away DOWN messages not belonging to this procedure.
%% They are collected to be reissued after we are done.
call_parallel_3(_Ref,_Pending,0,Replies,DownMsgs) ->  % All expected received.
    lists:foreach(fun({MRef,Pid,Info}) -> self() ! {'DOWN',MRef,process,Pid,Info} end,
		  DownMsgs),                 % Reissue the down messages!
    Replies;
call_parallel_3(Ref,Pending,NrOfPending,Replies,DownMsgs) ->
    receive
	{Reply,{Ref,Nr}} ->
	    case lists:keysearch(Nr,1,Pending) of
		{value,{_Nr,Node,MRef}} ->
		    erlang:demonitor(MRef),
		    call_parallel_3(Ref,Pending,NrOfPending-1,
				    [{Nr,Node,Reply}|Replies],DownMsgs);
		false ->                     % Really strange!
		    call_parallel_3(Ref,Pending,NrOfPending,Replies,DownMsgs)
	    end;
	{'DOWN',MRef,process,Pid,Info} ->    % Probably process we monitor terminated.
	    case lists:keysearch(MRef,3,Pending) of
		{value,{Nr,Node,_}} ->       % Yes it was one of our processes.
		    call_parallel_3(Ref,Pending,NrOfPending-1,
				    [{Nr,Node,{error,no_reponse}}|Replies],DownMsgs);
		false ->                     % We picked up a DOWN msg by misstake.
		    call_parallel_3(Ref,Pending,NrOfPending,Replies,
				    [{MRef,Pid,Info}|DownMsgs])
	    end
    end.

%% Help function which build up the [{Node,Reply},...] list in the same order as RTspec.
call_parallel_build_reply([],_,_) ->
    [];
call_parallel_build_reply([{Pid,Node,_Request}|Rest],Nr,Replies) when is_pid(Pid) ->
    {value,{_Nr,_Node,Reply}}=lists:keysearch(Nr,1,Replies),
    [{Node,Reply}|call_parallel_build_reply(Rest,Nr+1,Replies)];
call_parallel_build_reply([{{error,Reason},Node}|Rest],Nr,Replies) ->
    [{Node,{error,Reason}}|call_parallel_build_reply(Rest,Nr,Replies)];
call_parallel_build_reply([_Faulty|Rest],Nr,Replies) ->
    call_parallel_build_reply(Rest,Nr,Replies).
%% ------------------------------------------------------------------------------

cast(Pid,Request) when is_pid(Pid) ->
    cast2(Pid,Request);
cast(Node,Request) when Node==node() ->
    catch cast2(?MODULE,Request),
    ok;
cast(Node,Request) when is_atom(Node) ->
    catch cast2({?MODULE,Node},Request),
    ok;
cast(BadAddress,_Request) ->
    {error,{badarg,BadAddress}}.

cast2(To,Request) ->
    To ! {Request,void,void}.                % Mimics the call protocol.
%% ------------------------------------------------------------------------------


%% ==============================================================================
%% Implementation of the runtime component (server side).
%% ==============================================================================

%% Since the runtime component is not implemented using gen_sever we are "free"
%% to use what ever functionnames we like.

%% Initial function on which the runtime component is spawned on if started by
%% a controlcomponent.
init(Ctrl, Options, Tag, Parent) when is_list(Options) ->
    %% started from controller
    process_flag(trap_exit,true),
    register(?MODULE,self()),                % Will crash if rt is already running
    do_clear_trace_patterns(),               % Remove potential old patterns left.
    LD1=read_option_list(Options,
			 #rt{state=new,
			     parent=Parent,
			     ctrl=Ctrl,
			     vsn=get_application_vsn(),
			     tag=Tag}),
    OverloadData=initialize_overload(LD1),
    CtrlRef=erlang:monitor(process,Ctrl),    % Monitor our control component.
    loop1(LD1#rt{ctrl_ref=CtrlRef,overload_data=OverloadData}).
%% ----------------------------------------------------------------------------

%% Initial function on which the runtime component is spawned on if started
%% by the runtime_tools supervisor. It is here it is determined if we shall
%% autostart.
auto_init(AutoModArgs,Parent) -> 
    %% autostart
    process_flag(trap_exit, true),
    register(?MODULE, self()),               % Will crash if a rt is already running
    AutoMod=get_autostart_module(),          % Determine which module to use!
    case catch AutoMod:autostart(AutoModArgs) of 
	{MFA,Options,Tag} ->
	    do_clear_trace_patterns(),       % Remove previously left patterns.
	    LD1=read_option_list(Options,#rt{state=new,
					     parent=Parent,
					     vsn=get_application_vsn(),
					     tag=Tag}),
	    case auto_init_connect_control(LD1) of
		{ok,LD2} ->                  % Either connected or running_alone.
		    OverloadData=initialize_overload(LD2),
		    case auto_init_check_mfa(MFA) of
			{ok,{M,F,A}} ->      % We shall start somekind of tracing!
			    P=spawn_link(M,F,A), % It lives its own life, only link!
			    loop1(LD2#rt{auto_starter=P,overload_data=OverloadData});
			false ->
			    loop1(LD2#rt{overload_data=OverloadData})
		    end;
		stop ->                      % Not allowed to run alone!
		    true                     % Simply terminate.
	    end;
	 _ ->                                % Non existent or faulty autostart mod!
	    true                             % Terminate normally.
    end.

auto_init_connect_control(LD1) ->
    case auto_init_connect_find_pid(LD1#rt.dependency) of
	Pid when is_pid(Pid) ->                 % There is a control component.
	    CtrlRef=erlang:monitor(process,Pid),
	    Pid ! {connect,node(),self(),LD1#rt.vsn,LD1#rt.tag},
	    {ok,LD1#rt{ctrl_ref=CtrlRef,ctrl=Pid}};
	_ ->                                 % There is no control component.
	    do_down_message(LD1)             % Will return 'stop' or a LoopData.
    end.

%% Help function which finds the pid of the control component.
auto_init_connect_find_pid({_TimeOut,Node}) when Node==node() ->
    whereis(?CTRL);
auto_init_connect_find_pid({_TimeOut,Node}) when is_atom(Node) ->
    rpc:call(Node,erlang,whereis,[?CTRL]);
auto_init_connect_find_pid(_) ->             % Node is not a proper node.
    undefined.                               % Act as could not find control comp.

%% Help function checking that the parameter is reasonable to be used as
%% spawn_link argument.
auto_init_check_mfa({M,F,A}) when is_atom(M),is_atom(F),is_list(A) ->
    {ok,{M,F,A}};
auto_init_check_mfa(_) ->
    false.

%% Help function to init_auto which finds out which module to call for
%% guidance on how to proceed. Returns an atom.
get_autostart_module() ->
    case application:get_env(inviso_autostart_mod) of
	{ok,Mod} when is_atom(Mod) ->
	    Mod;
	_ ->
	    inviso_autostart                 % The default autostart module.
    end.
%% ----------------------------------------------------------------------------


%% This is the preloop function which performs loadcheck if necessary. Note
%% that it calculates the timeout used in the after in the real loop. There is
%% further no use doing overload checks if we are not tracing or already
%% suspended. There is yet one more situation, we do not want to perform
%% overload checks if the interval is set to infinity. This can be the case if
%% we are using an external source pushing overload information instead.
loop1(LD=#rt{overload=Overload}) ->
    if
	Overload/=?NO_LOADCHECK,element(2,Overload)/=infinity ->
	    Now=now(),
	    if
		LD#rt.status==running,
		LD#rt.state==tracing,
		Now>LD#rt.next_loadcheck ->  % Do loadcheck only then!
		    {NewLD,TimeOut}=do_check_overload(LD,{timeout,LD#rt.overload_data}),
		    loop(NewLD,TimeOut);
		LD#rt.status==running,LD#rt.state==tracing ->
		    Timeout=calc_diff_to_now(Now,LD#rt.next_loadcheck),
		    loop(LD,Timeout);
		true ->                      % Do not spend CPU on this! :-)
		    loop(LD,infinity)
	    end;
	true ->                              % Either no check or infinity.
	    loop(LD,infinity)
    end.

loop(LoopData,Timeout) ->
    receive
	Msg when element(1,Msg)==trace_ts;
		 element(1,Msg)==trace;
		 element(1,Msg)==drop;
		 element(1,Msg)==seq_trace ->
	    case LoopData#rt.handler of
		{HandlerFun,Data} ->
		    NewData=HandlerFun(Msg,Data),
		    loop1(LoopData#rt{handler={HandlerFun,NewData}});
		_ ->
		    loop1(LoopData)
	    end;
	{{tp,Args,Flags},From,Ref} ->
	    if
		LoopData#rt.status==running -> % Not when suspended.
		    Reply=do_set_trace_patterns(Args,Flags),
		    if
			LoopData#rt.state==new -> % No longer new when tp set.
			    reply_and_loop({ok,Reply},From,Ref,LoopData#rt{state=idle});
			true ->
			    reply_and_loop({ok,Reply},From,Ref,LoopData)
		    end;
		true ->                      % We are suspended!
		    reply_and_loop({error,suspended},From,Ref,LoopData)
	    end;
	{{tf,Args,How},From,MRef} ->
	    Reply=
		case How of
		    true ->
			if
			    LoopData#rt.status==running -> 
				case {LoopData#rt.tracer_port,LoopData#rt.handler} of
				    {Port,_} when is_port(Port) ->
					do_set_trace_flags(Port,Args,How);
				    {_,{Handler,_D}} when is_function(Handler) ->
					do_set_trace_flags(self(),Args,How);
				    _ -> 
					{error,no_tracer}
				end;
			    true ->          % Can't turn *on* flags if suspended.
				{error, suspended}
			end;
		    false ->                 % No tracer needed when turning off.
			do_set_trace_flags(void,Args,How)
		end,
	    reply_and_loop(Reply,From,MRef,LoopData);
	{{meta_tracer_call,Args},From,MRef} ->
	    if
		LoopData#rt.status==running ->
		    case LoopData#rt.meta_tracer of
			MPid when is_pid(MPid) ->
			    Reply=do_meta_pattern(MPid,Args),
			    reply_and_loop(Reply,From,MRef,LoopData);
			_ ->
			    reply_and_loop({error,no_metatracer},From,MRef,LoopData)
		    end;
		true ->
		    reply_and_loop({error,suspended},From,MRef,LoopData)
	    end;
	{clear_all_tp,From,MRef} ->
	    do_clear_trace_patterns(),	    
	    reply_and_loop(ok,From,MRef,LoopData);
	{{init_tracing,TracerData},From,MRef} ->
	    {NewLoopData,Reply}= 
		if
		    LoopData#rt.status==running ->
			if
			    LoopData#rt.state==tracing ->
				{LoopData,{error,already_initiated}};
			    true ->          % Otherwise, try to init-tracing!
				case translate_td(TracerData) of
				    {ok,LogTD,MetaTD} ->
					do_init_tracing(LoopData,TracerData,LogTD,MetaTD);
				    Error ->
					{LoopData,Error}
				end
			end;
		    true ->                  % Can't init tracing if not running.
			{LoopData,{error,suspended}}
		end,
	    reply_and_loop(Reply,From,MRef,NewLoopData);
	{stop_tracing,From,MRef} ->
	    case LoopData#rt.state of
		tracing ->                   % Only case we need to do anything.
		    reply_and_loop({ok,idle},From,MRef,do_stop_tracing(LoopData));
		idle ->                      % Already idle!
		    reply_and_loop({ok,idle},From,MRef,LoopData);
		new ->                       % Have actually never traced!
		    reply_and_loop({ok,new},From,MRef,LoopData)
	    end;
	{{suspend,Reason},From,MRef} ->
	    if
		LoopData#rt.status==running ->
		    NewLD=do_suspend(LoopData,Reason),
		    reply_and_loop(ok,From,MRef,NewLD);
		true ->                      % No need suspend if not running!
		    reply_and_loop(ok,From,MRef,LoopData)
	    end;
	{cancel_suspension,From,MRef} ->
	    NewLoopData=LoopData#rt{status=running,next_loadcheck=now()},
	    send_event(state_change,NewLoopData),
	    reply_and_loop(ok,From,MRef,NewLoopData);
	{{clear,Options},From,MRef} ->
	    NewLoopData=do_clear(LoopData,Options),
	    reply_and_loop({ok,{new,NewLoopData#rt.status}},From,MRef,NewLoopData);
	{flush,From,MRef} ->
	    case LoopData#rt.state of
		tracing ->                  % Can only flush if we are tracing.
		    if
			is_port(LoopData#rt.tracer_port) ->
			    trace_port_control(LoopData#rt.tracer_port,flush),
			    reply_and_loop(ok,From,MRef,LoopData);
			true ->             % Not necessary but lets pretend.
			    reply_and_loop(ok,From,MRef,LoopData)
		    end;
		State ->
		    reply_and_loop({error,{not_tracing,State}},From,MRef,LoopData)
	    end;
	{list_logs,From,MRef} ->
	    TracerData=LoopData#rt.tracerdata, % Current tracerdata.
	    if
		TracerData/=undefined ->    % There is tracerdata!
		    reply_and_loop(do_list_logs(TracerData),From,MRef,LoopData);
		true ->                     % Have no current tracerdata!
		    reply_and_loop({error,no_tracerdata},From,MRef,LoopData)
	    end;
	{{list_logs,TracerData},From,MRef} ->
	    reply_and_loop(do_list_logs(TracerData),From,MRef,LoopData);
	{{fetch_log,CollectPid},From,MRef} -> % Fetch according to current tracerdata.
	    TracerData=LoopData#rt.tracerdata, % Current tracerdata.
	    if
		TracerData/=undefined ->    % There is tracerdata!
		    {Reply,NewLD}=do_fetch_log(LoopData,CollectPid,TracerData),
		    reply_and_loop(Reply,From,MRef,NewLD);
		true ->                     % No tracerdata!
		    reply_and_loop({error,no_tracerdata},From,MRef,LoopData)
	    end;
	{{fetch_log,CollectPid,Spec},From,MRef} -> % Either list of files or tracerdata.
	    {Reply,NewLD}=do_fetch_log(LoopData,CollectPid,Spec),
	    reply_and_loop(Reply,From,MRef,NewLD);
	{delete_logs,From,MRef} ->
	    if
		LoopData#rt.state==tracing -> % Can't remove then!
		    reply_and_loop({error,tracing},From,MRef,LoopData);
		true ->
		    TracerData=LoopData#rt.tracerdata,
		    reply_and_loop(do_delete_logs(TracerData),From,MRef,LoopData)
	    end;
	{{delete_logs,TracerDataOrLogList},From,MRef} ->
	    if
		LoopData#rt.state==tracing -> % Can't remove then!
		    reply_and_loop({error,tracing},From,MRef,LoopData);
		true ->
		    reply_and_loop(do_delete_logs(TracerDataOrLogList),From,MRef,LoopData)
	    end;
	{get_node_info,From,MRef} ->
	    Reply=collect_node_info(LoopData),
	    reply_and_loop(Reply,From,MRef,LoopData);
	{{try_to_adopt,Tag,Condition},From,MRef} ->
	    if
		LoopData#rt.ctrl_ref==undefined -> % We have no control component.
		    {Reply,NewLoopData}=do_try_to_adopt(Tag,Condition,LoopData,From),
		    reply_and_loop(Reply,From,MRef,NewLoopData);
		true ->                      % We already have a control component.
		    reply_and_loop({error,refused},From,MRef,LoopData)
	    end;
	{{confirm_connection,_Tag},From,MRef} ->
	    if
		LoopData#rt.ctrl==From ->    % It must be from this process!
		    Reply=collect_node_info(LoopData),
		    reply_and_loop(Reply,From,MRef,LoopData);
		true ->                      % Strange, some one is joking?
		    reply_and_loop({error,refused},From,MRef,LoopData)
	    end;
	{{change_options,Options},From,MRef} ->
	    case do_change_options(Options,LoopData) of
		stop ->                      % Can't run alone with these options!
		    terminate_overload(LoopData),
		    From ! {ok,MRef};        % Don't care if From not a proper pid!
		NewLoopData when is_record(NewLoopData,rt) ->
		    reply_and_loop(ok,From,MRef,NewLoopData)
	    end;
	{get_status,From,MRef} ->
	    Reply={ok,{LoopData#rt.state,LoopData#rt.status}},
	    reply_and_loop(Reply,From,MRef,LoopData);
	{get_tracerdata,From,MRef} ->
	    case LoopData#rt.tracerdata of
		undefined ->
		    reply_and_loop({ok,no_tracerdata},From,MRef,LoopData);
		TracerData ->
		    reply_and_loop({ok,TracerData},From,MRef,LoopData)
	    end;
	{state,From,MRef} ->                 % For debugging purposes.
	    reply_and_loop(LoopData,From,MRef,LoopData);

	{'DOWN',CtrlRef,process,_,_} when CtrlRef==LoopData#rt.ctrl_ref ->
	    case do_down_message(LoopData) of
		stop ->                      % inviso_c gone and we must stop!
		    terminate_overload(LoopData),
		    exit(running_alone);
		{ok,NewLoopData} ->
		    loop1(NewLoopData)
	    end;
	{'EXIT',Pid,Reason} ->
	    case act_on_exit(Pid,Reason,LoopData) of
		exit ->
		    terminate_overload(LoopData),
		    exit(Reason);
		NewLoopData when is_record(NewLoopData,rt) ->
		    loop1(NewLoopData);
		{NewLoopData,NewTimeOut} when is_record(NewLoopData,rt) ->
		    loop(NewLoopData,NewTimeOut)
	    end;
	Other ->                            % Check if it concerns overload.
	    if
		LoopData#rt.overload/=?NO_LOADCHECK,
		LoopData#rt.status==running,
		LoopData#rt.state==tracing ->
		    {NewLD,NewTimeOut}=
			do_check_overload(LoopData,
					  {msg,{Other,LoopData#rt.overload_data}}),
		    loop(NewLD,NewTimeOut);
		true ->
		    NewTimeOut=calc_diff_to_now(now(),LoopData#rt.next_loadcheck),
		    loop(LoopData,NewTimeOut)
	    end
    after
	Timeout ->
	    loop1(LoopData)
    end.

reply_and_loop(Reply,To,MRef,LoopData) when is_pid(To) -> 
    To ! {Reply,MRef},
    loop1(LoopData);
reply_and_loop(_,_,_,LoopData) ->            % Used together with incoming casts.
    loop1(LoopData).
%% -----------------------------------------------------------------------------


%% =============================================================================
%% File transfer process implementation.
%% =============================================================================

%% Files that are to to be transfered from the runtime component to the control
%% component are done so by reading them as binaries and sending them with
%% normal message passing (over distributed Erlang).
%% Reading the files are done in a process separate to the runtime component,
%% to both make the code more simple. But also to free up the runtime component.
%%
%% This help process must be capable of recognizing the fact that the runtime
%% component has been suspended, and then of course also discontinue any file
%% transfere.
fetch_init(Parent,Files,CollectPid,ChunkSize) ->
    process_flag(trap_exit,true),            % We must clean-up.
    process_flag(priority,low),              % Lets be careful.
    case fetch_open_file(Files,CollectPid) of
	{ok,FileName,FD,RestFiles} ->
	    MRef=erlang:monitor(process,CollectPid),
	    fetch_loop(Parent,RestFiles,CollectPid,ChunkSize,FileName,FD,MRef);
	done ->
	    fetch_end(CollectPid);
	error ->
	    fetch_incomplete(CollectPid)
    end.

fetch_loop(Parent,Files,CollectPid,ChunkSize,FName,FD,MRef) ->
    receive
	{suspend,Parent} ->                  % The runtime component is suspended.
	    file:close(FD),                  % We must clean-up.
	    fetch_incomplete(CollectPid);
	{'DOWN',MRef,process,_,_} ->         % The CollectPid terminated!
	    file:close(FD);                  % Close file and terminate.
	{'EXIT',Parent,_Reason} ->           % The runtime component terminated.
	    file:close(FD),
	    fetch_incomplete(CollectPid);
	_ ->
	    fetch_loop(Parent,Files,CollectPid,ChunkSize,FName,FD,MRef)
    after
	0 ->                                 % If non of the above, get to work!
	    case file:read(FD,ChunkSize) of
		{ok,Bin} ->
		    fetch_send_chunk(CollectPid,Bin),
		    case fetch_wait_for_chunk_ack(CollectPid,MRef) of
			ok ->                % Collector ready to receive next chunk.
			    fetch_loop(Parent,Files,CollectPid,ChunkSize,FName,FD,MRef);
			cancel ->            % Send no more files!
			    file:close(FD),  % Close file, send incomplete, terminate!
			    fetch_incomplete(CollectPid);
			'DOWN' ->            % Collector has terminate, stop!
			    file:close(FD)   % Close file and terminate.
		    end;
		eof ->                       % Ok, go on with the next file.
		    file:close(FD),
		    fetch_send_eof(CollectPid),
		    case fetch_open_file(Files,CollectPid) of
			{ok,NewFName,NewFD,RestFiles} ->
			    fetch_loop(Parent,RestFiles,CollectPid,
				       ChunkSize,NewFName,NewFD,MRef);
			done ->
			    fetch_end(CollectPid);
			error ->
			    fetch_incomplete(CollectPid)
		    end;
		{error,Reason} ->            % Do not continue.
		    file:close(FD),
		    fetch_send_readerror(CollectPid,FName,Reason),
		    fetch_incomplete(CollectPid)
	    end
    end.
%% -----------------------------------------------------------------------------

%% Help function which opens the next file to be transferred. It also communicates
%% the opening of the file to the collector process.
%% We know here that it will be a list of three-tuples. But there is no guarantee
%% that Dir or FileName are proper strings.
%% Returns {ok,FileName,FileDescriptor,RemainingFiles} or 'done'.
fetch_open_file([{FType,Dir,FileName}|RestFiles],CollectPid) ->
    case catch file:open(filename:join(Dir,FileName),[read,raw,binary]) of
	{ok,FD} ->
	    CollectPid ! {node(),open,{FType,FileName}},
	    {ok,FileName,FD,RestFiles};
	{error,_Reason} ->
	    CollectPid ! {node(),open_failure,{FType,FileName}},
	    error;
	{'EXIT',_Reason} ->                   % Faulty Dir or FileName.
	    CollectPid ! {node(),open_failure,{FType,FileName}},
	    error
    end;    
fetch_open_file([],_CollectPid) ->
    done.
%% -----------------------------------------------------------------------------

%% A group of help functions sending information to the collector process.
%% Returns nothing significant.
fetch_send_chunk(CollectPid,Bin) ->
    CollectPid ! {node(),payload,Bin,self()}.
%% -----------------------------------------------------------------------------

fetch_send_eof(CollectPid) ->
    CollectPid ! {node(),end_of_file}.
%% -----------------------------------------------------------------------------

fetch_end(CollectPid) ->
    CollectPid ! {node(),end_of_transmission}.
%% -----------------------------------------------------------------------------

fetch_send_readerror(CollectPid,FName,Reason) ->
    CollectPid ! {node(),{error,{file_read,{Reason,FName}}}}.
%% -----------------------------------------------------------------------------

fetch_incomplete(CollectPid) ->
    CollectPid ! {node(),incomplete}.
%% -----------------------------------------------------------------------------

%% Help function waiting for the collector to respond that it is ready to receive
%% the next chunk. This is in order to exercise flow control protecting the
%% collector to get swamped if the node where the collector runs is busy.
fetch_wait_for_chunk_ack(CollectPid,MRef) ->
    receive
	{CollectPid,chunk_ack} ->
	    ok;
	{CollectPid,cancel_transmission} ->  % Some problem at collector side.
	    cancel;
	{'DOWN',MRef,process,_,_} ->         % The collector terminated.
	    'DOWN'
    end.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% First level do-functions, called from the main server loop on incomming
%% requests.
%% =============================================================================

%% Function performing the overload check. Returns {NewLoopData,TimeOut}.
%% Note that this function may also cause a suspend to be carried out if the
%% loadcheck turns out negative.
do_check_overload(LD,Data) ->
    case do_check_overload_2(LD#rt.overload,Data) of
	ignore ->                            % Load check not performed.
	    {LD,calc_diff_to_now(now(),LD#rt.next_loadcheck)};
	{ok,Interval} ->                     % No problem, continue.
	    NextLoadCheck=add_to_now(now(),Interval),
	    {LD#rt{next_loadcheck=NextLoadCheck},Interval};
	{suspend,Reason} ->                  % Emergency! suspend, suspend!
	    NewLD=do_suspend(LD,Reason),
	    {NewLD,infinity};                % No need to do load-checks now!
	{new,NewData,Interval} ->            % The overload was restarted or something.
	    NextLoadCheck=add_to_now(now(),Interval),
	    {LD#rt{overload_data=NewData,next_loadcheck=NextLoadCheck},Interval};
	error ->                             % Inhibit overload check then.
	    {LD#rt{overload=?NO_LOADCHECK},infinity}
    end.

%% Help function performing an overload check. Returns {ok,Interval},
%% {suspend,Reason}, 'error' ir 'ignore'.
do_check_overload_2({{Mod,Func},Interval,_,_},Data) ->
    do_check_overload_3(Interval,catch Mod:Func(Data));
do_check_overload_2({Fun,Interval,_,_},Data) when is_function(Fun) ->
    do_check_overload_3(Interval,catch Fun(Data));
do_check_overload_2(_,_) ->                  % Bad loadcheck configuration.
    error.                                   % Stop using load checks then.

do_check_overload_3(Interval,ok) ->
    {ok,Interval};
do_check_overload_3(Interval,{new,NewData}) ->
    {new,NewData,Interval};
do_check_overload_3(_Interval,{suspend,Reason}) ->
    {suspend,Reason};
do_check_overload_3(_Interval,ignore) ->     % Loadcheck not triggered.
    ignore;
do_check_overload_3(_Interval,_) ->          % Failure or other return value.
    error.                                   % Stop doing loadchecks from now on.
%% ------------------------------------------------------------------------------

%% Function setting the trace-pattern according to Args and Flags. Note that
%% Args can contain regexps which must be expanded here.
%% Returns a list: [Result], where Result can be: int()|{error,Reason}.
%% Sometimes an error tuple will represent an entire pattern, sometimes the
%% pattern will expand to a number of error-tuples.
do_set_trace_patterns(Args,Flags) ->
    Replies=do_set_trace_patterns_2(Args,Flags,[]),
    lists:reverse(Replies).

do_set_trace_patterns_2([{M,F,Arity,MS}|Rest],Flags,Replies) -> % Option-less.
    do_set_trace_patterns_2([{M,F,Arity,MS,[]}|Rest],Flags,Replies);
do_set_trace_patterns_2([{M,F,Arity,MS,Opts}|Rest],Flags,Replies) when is_atom(M) ->
    case load_module_on_option(M,Opts) of
	true ->                             % Already present, loaded or no option!
	    case catch erlang:trace_pattern({M,F,Arity},MS,Flags) of
		No when is_integer(No) ->
		    do_set_trace_patterns_2(Rest,Flags,[No|Replies]);
		{'EXIT',Reason} ->
		    do_set_trace_patterns_2(Rest,
					    Flags,
					    [{error,{bad_trace_args,[{M,F,Arity,MS},Reason]}}|
					     Replies])
	    end;
	false ->                            % Module not present, or not found!
	    do_set_trace_patterns_2(Rest,Flags,[0|Replies])
    end;
do_set_trace_patterns_2([{M,F,Arity,MS,Opts}|Rest],Flags,Replies) when is_list(M) ->
    case check_pattern_parameters(void,F,Arity,MS) of % We don't want to repeat bad params.
	ok ->
	    case inviso_rt_lib:expand_regexp(M,Opts) of % Get a list of real modulnames.
		Mods when is_list(Mods) ->
		    MoreReplies=
			do_set_trace_patterns_2(lists:map(fun(Mod)->
								  {Mod,F,Arity,MS,Opts}
							  end,
							  Mods),
						Flags,
						Replies),
		    do_set_trace_patterns_2(Rest,Flags,MoreReplies);
		{error,Reason} ->
		    do_set_trace_patterns_2(Rest,Flags,[{error,Reason}|Replies])
	    end;
	error ->                            % Bad pattern parameters.
	    do_set_trace_patterns_2(Rest,
				    Flags,
				    [{error,{bad_trace_args,{M,F,Arity,MS}}}|Replies])
    end;
do_set_trace_patterns_2([{{Dir,M},F,Arity,MS,Opts}|Rest],Flags,Replies)
  when is_list(Dir),is_list(M) ->
    case check_pattern_parameters(void,F,Arity,MS) of % We don't want to repeat bad params.
	ok ->
	    case inviso_rt_lib:expand_regexp(Dir,M,Opts) of % Get a list of real modulnames.
		Mods when is_list(Mods) ->
		    MoreReplies=
			do_set_trace_patterns_2(lists:map(fun(Mod)->
								  {Mod,F,Arity,MS,Opts}
							  end,
							  Mods),
						Flags,
						Replies),
		    do_set_trace_patterns_2(Rest,Flags,MoreReplies);
		{error,Reason} ->
		    do_set_trace_patterns_2(Rest,Flags,[{error,Reason}|Replies])
	    end;
	error ->                            % Bad pattern parameters.
	    do_set_trace_patterns_2(Rest,
				    Flags,
				    [{error,{bad_trace_args,{M,F,Arity,MS}}}|Replies])
    end;
do_set_trace_patterns_2([Arg|Rest],Flags,Replies) ->
    do_set_trace_patterns_2(Rest,Flags,[{error,{bad_trace_args,Arg}}|Replies]);
do_set_trace_patterns_2([],_Flags,Replies) ->
    Replies.
%% -----------------------------------------------------------------------------

%% Help function which sets the trace flags for all processes specifed in Args.
%% Args shall be a list of {ProcessSpecification,ProcessTraceFlags}.
%% Returns {ok,Answers} where Answers is a list of integer and error descriptions.
%% Note that a process specification may be a particular pid or a {global,Name}.
%% In the case the process does not exist we will fake a zero instead of an
%% error.
do_set_trace_flags(Tracer,Args,How) ->
    Fun=fun({Proc,Flags}) ->
		case check_traceflag_pidspec(Proc) of
		    {ok,Proc2} ->            % Reg-names converted.
			case check_flags(Flags) of
			    Flags2 when is_list(Flags2) -> % No error!
				case (catch
					 case How of
					     true ->
						 erlang:trace(Proc2,
							      true,
							      [{tracer,Tracer}|Flags2]);
					     false -> % No tracer of turning off.
						 erlang:trace(Proc2,
							      false,
							      Flags2)
					 end) of
				    N when is_integer(N) ->
					N;
				    {'EXIT',Reason} ->
					if
					    is_pid(Proc2) ->
						0;    % Proc2 not alive or not at this node!
					    true ->   % Otherwise, just error!
						{error,
						 {bad_trace_args,
						  [Reason,Proc2,How,Flags2,Tracer]}}
					end
				end;
			    FlagError ->
				FlagError
			end;
		    false ->                 % Skip it.
			0;                   % Indicate that zero processes matched.
		    {error,Reason} ->        % Bad process specification.
			{error,{bad_process,[Reason,Proc]}}
		end;
	   (Faulty) ->
		{error,{bad_process,Faulty}}
	end,
    {ok,lists:map(Fun,Args)}.
%% ------------------------------------------------------------------------------

%% Function calling API:s in the trace information server. Note that we have
%% given the responsibility to form a correct functionsname and argument list
%% to the caller.
%% Returns whatever the called function returns.
do_meta_pattern(MPid,{FuncName,ArgList}) ->
    case catch apply(inviso_rt_meta,FuncName,[MPid|ArgList]) of
	{'EXIT',_Reason} ->
	    {error,{badarg,{FuncName,ArgList}}};
	Result ->
	    Result
    end;
do_meta_pattern(_MPid,BadArgs) ->
    {error,{bad_args,BadArgs}}.
%% ------------------------------------------------------------------------------

%% Function removing *all* patterns. Beaware that the one for local patterns
%% causes a walkthrough of all loaded modules.
do_clear_trace_patterns() ->
    erlang:trace_pattern({'_','_','_'},false,[local]), %% inc. meta, call_count
    erlang:trace_pattern({'_','_','_'},false,[global]).
%% ------------------------------------------------------------------------------

%% Function that takes TracerData and initializes the tracing. That can be
%% opening appropriate logfiles, starting meta-tracer. There must be one
%% clause here for every "type" of logging we want to be able to do.
%% Returns the Reply to be forwarded to the caller.
do_init_tracing(LoopData,TD,{HandlerFun,Data},TiTD) when is_function(HandlerFun) ->
    {NewLoopData,Reply}=
	case do_init_metatracing(TiTD,self()) of
	    {ok,MetaPid} ->
		{LoopData#rt{handler={HandlerFun,Data},
			     tracerdata=TD,
			     meta_tracer=MetaPid,
			     state=tracing},
		 {ok,[{trace_log,ok},{ti_log,ok}]}};
	    false ->                         % No meta tracing requested.
		{LoopData#rt{handler={HandlerFun,Data},
			     tracerdata=TD,
			     state=tracing},
		 {ok,[{trace_log,ok}]}};
	    {error,Reason} ->                % Problems starting meta tracing.
		{LoopData#rt{handler={HandlerFun,Data},
			     tracerdata=TD,
			     state=tracing},
		 {ok,[{trace_log,ok},{ti_log,{error,Reason}}]}}
	end,
    send_event(state_change,NewLoopData),    % Send to subscribing processes.
    {NewLoopData,Reply};
do_init_tracing(LoopData,TD,{Type,Parameters},TiTD) when Type==ip;Type==file ->
    case check_traceport_parameters(Type,Parameters) of
	ok ->
	    case catch trace_port(Type,Parameters) of
		Fun when is_function(Fun) ->
		    case catch Fun() of
			Port when is_port(Port) -> % Ok, our trace-port is open.
			    {NewLoopData,Reply}=
				case do_init_metatracing(TiTD,Port) of
				    {ok,MetaPid} ->
					{LoopData#rt{tracer_port=Port,
						     tracerdata=TD,
						     meta_tracer=MetaPid,
						     state=tracing},
					 {ok,[{trace_log,ok},{ti_log,ok}]}};
				    false -> % No meta tracing requested.
					{LoopData#rt{tracer_port=Port,
						     tracerdata=TD,
						     state=tracing},
					 {ok,[{trace_log,ok}]}};
				    {error,Reason} -> % Problems starting meta tracing.
					{LoopData#rt{tracer_port=Port,
						     tracerdata=TD,
						     state=tracing},
					 {ok,[{trace_log,ok},{ti_log,{error,Reason}}]}}
				end,
			    send_event(state_change,NewLoopData),
			    {NewLoopData,Reply};
			{'EXIT',Reason} ->
			    {LoopData,{error,{bad_port_fun,[Parameters,Reason]}}}
		    end;
		{'EXIT',Reason} ->
		    {LoopData,{error,{bad_port_args,[Parameters,Reason]}}}
	    end;
	{error,Reason} ->                    % Bad traceport parameters.
	    {LoopData,{error,Reason}}
    end.

%% Help function that starts the meta-tracing. Note that the runtime component
%% will becom linked to it.
%% Currently the meta tracer handles two types, 'file' and 'relay'.
%% Note that Tracer tells the meta tracer where regular trace messages shall be
%% sent. This is because the meta tracer is capable of appending a {tracer,Tracer}
%% action term to meta match specs.
do_init_metatracing(LogSpec={_Type,_Arg},Tracer) ->
    case inviso_rt_meta:start(LogSpec,Tracer) of
	{ok,MetaPid} ->
	    {ok,MetaPid};
	{error,Reason} ->
	    {error,Reason}
    end;
do_init_metatracing({Type,Arg,{InitPublLDmfa,RemovePublLDmf,CleanPublLDmf}},Tracer)->
    case inviso_rt_meta:start({Type,Arg},Tracer,InitPublLDmfa,RemovePublLDmf,CleanPublLDmf) of
	{ok,MetaPid} ->
	    {ok,MetaPid};
	{error,Reason} ->
	    {error,Reason}
    end;
do_init_metatracing(void,_) ->              % Means no meta tracer.
    false.
%% -----------------------------------------------------------------------------

%% Function that stops all tracing and closes all open files. This function
%% can't fail :-) It tries as hard as it can.
%% This function also kills the autostarter process if one exists. Otherwise it
%% will not be possible from a control component to end an ongoing autostarted
%% tracing.
%% Returns a new loopdata structure since stopping tracing involves updating it.
do_stop_tracing(LoopData) ->
    do_stop_tracing_kill_autostarter(LoopData#rt.auto_starter),
    do_clear_trace_flags(),                 % Do not generate any more traces.
    NewLoopData1=do_stop_tracing_tracelog(LoopData),
    NewLoopData2=do_stop_tracing_metatracing(NewLoopData1),
    NewLoopData3=NewLoopData2#rt{state=idle,auto_starter=undefined},
    send_event(state_change,NewLoopData3),
    NewLoopData3.

do_stop_tracing_tracelog(LoopData=#rt{tracer_port=Port}) when is_port(Port) ->
    trace_port_control(Port,flush),         % Write buffered trace messages.
    catch port_close(Port),
    LoopData#rt{tracer_port=undefined};
do_stop_tracing_tracelog(LoopData) ->
    LoopData#rt{handler=undefined}.

do_stop_tracing_metatracing(LoopData=#rt{meta_tracer=MPid}) when is_pid(MPid) ->
    inviso_rt_meta:stop(MPid),
    LoopData#rt{meta_tracer=undefined};
do_stop_tracing_metatracing(LoopData) ->    % No meta tracer running!
    LoopData.

%% Help function killing the autostarter, if one is active.
do_stop_tracing_kill_autostarter(P) when is_pid(P) ->
    exit(P,stop_tracing);
do_stop_tracing_kill_autostarter(_) ->      % No autostarter, do nothing.
    true.
%% -----------------------------------------------------------------------------

%% Help function implementing suspending the runtime component.
%% Returns a new loopdata structure.
do_suspend(LD,Reason) ->
    do_clear_trace_flags(),                 % If no process flags, no output!
    do_suspend_metatracer(LD#rt.meta_tracer),
    do_suspend_fetchers(LD#rt.fetchers),
    do_stop_tracing_kill_autostarter(LD#rt.auto_starter),
    NewLD=LD#rt{fetchers=[],status={suspended,Reason},auto_starter=undefined},
    send_event(state_change,NewLD),         % Notify subscribers.
    NewLD.

do_suspend_metatracer(MetaTracer) when is_pid(MetaTracer) ->
    inviso_rt_meta:suspend(MetaTracer);     % This makes it suspended.
do_suspend_metatracer(_) ->
    true.

do_suspend_fetchers([FetcherPid|Rest]) ->
    FetcherPid ! {suspend,self()},          % This makes it terminate.
    do_suspend_fetchers(Rest);
do_suspend_fetchers([]) ->
    true.
%% ------------------------------------------------------------------------------

%% Function that stops all tracing, removes all trace-patterns and removes all
%% logfiles. The idea is to return the runtime component to the 'new' state.
do_clear(LoopData,Opts) when is_list(Opts) ->
    NewLoopData=do_stop_tracing(LoopData),  % First stop tracing, if tracing.
    case lists:member(keep_trace_patterns,Opts) of
	false ->
	    do_clear_trace_patterns();
	_ ->
	    true
    end,
    case lists:member(keep_log_files,Opts) of
	false ->
	    if
		NewLoopData#rt.tracerdata/=undefined ->
		    do_delete_logs(NewLoopData#rt.tracerdata);
		true ->                     % If no tracerdata, nothing to remove!
		    true                    % Do nothing then.
	    end;
	_ ->
	    true
    end,
    NewLoopData#rt{state=new,tracerdata=undefined};
do_clear(LoopData,_Opts) ->                 % Faulty Opts.
    do_clear(LoopData,[]).                  % Then just ignore the options.
%% -----------------------------------------------------------------------------

%% Function which takes a tracerdata, either our own or a "suggested"
%% and tries to find the corresponding files. Note that the return value only
%% contains "types" of logs that the tracerdata is pointing out. Hence
%% is there no ti-log, no one will be mentioned in the return value.
do_list_logs(TracerData) ->                   % Handles both list and tuple.
    case translate_td(TracerData) of
	{ok,LogTD,TiTD} ->
	    {TraceDir,TraceLogs}=list_logs_tracelog(LogTD),
	    {TiDir,TiLogs}=list_logs_tilog(TiTD),
	    case {TraceLogs,TiLogs} of
		{no_log,no_log} ->            % Tracerdata not generating logs!
		    {ok,no_log};
		{_,no_log} ->                 % No ti logs.
		    {ok,[{trace_log,TraceDir,TraceLogs}]};
		{no_log,_} ->                 % Only ti-logs, unusual!
		    {ok,[{ti_log,TiDir,TiLogs}]};
		_ ->                          % Both trace and ti logs.
		    {ok,[{trace_log,TraceDir,TraceLogs},{ti_log,TiDir,TiLogs}]}
	    end;
	{error,Reason} ->
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------

%% Help function implementing fetching logfiles using distributed Erlang.
%% This function works for both situations, a list of specific files are
%% requested, or a tracerdata is specified.
%% Returns {Reply,NewLoopData}.
do_fetch_log(LD,CollectPid,What) ->
    if
	LD#rt.state/=tracing ->
	    case is_list_of_files_or_tracerdata(What) of
		files ->
		    FetcherPid=do_fetch_log_listoffiles(CollectPid,What),
		    {{ok,FetcherPid},add_fetcher_ld(FetcherPid,LD)};
		tracerdata ->
		    case do_fetch_log_tracerdata(CollectPid,What) of
			{Reply,FetcherPid} when is_pid(FetcherPid) ->
			    {Reply,add_fetcher_ld(FetcherPid,LD)};
			{Reply,_} ->        % No fetch process was started.
			    {Reply,LD}
		    end;
		false ->                    % It is an empty list!
		    {{complete,no_log},LD};
		error ->                    % Incorrect parameter.
		    {{error,badarg},LD}
	    end;
	true ->                             % No transfere during tracing.
	    {{error,tracing},LD}
    end.

%% Function taking tracerdata to find out what files to send over to the RemotePid.
%% Note that we will not go back to the loop function from here but rather call
%% the fetch_loop instead, prepresenting the fetch-log state. Unless we encounter
%% a problem.
do_fetch_log_tracerdata(CollectPid,TracerData) ->
    case do_list_logs(TracerData) of
	{ok,no_log} ->
	    {{complete,no_log},void};
	{ok,Logs} ->                          % Ok, some trace_log and ti_log.
	    FetcherPid=do_fetch_log_listoffiles(CollectPid,Logs),
	    {{ok,FetcherPid},FetcherPid};
	{error,Reason} ->                     % Problem with tracerdata!
	    {{error,Reason},void}
    end.

do_fetch_log_listoffiles(CollectPid,FileSpec) ->
    ExpandedFileSpec=do_fetch_log_expand_filespec(FileSpec),
%% !!! try out different ChunkSizes
%	            ChunkSize = 60,
%	            ChunkSize = 7*1024,
    ChunkSize=1024,
    _Fetcher=spawn_link(?MODULE,
			fetch_init,
			[self(),ExpandedFileSpec,CollectPid,ChunkSize]).

%% Help function which expands the list of logs to have tags in front of every
%% file, as required by the fetch_loop.
do_fetch_log_expand_filespec(Logs) ->
    TraceLogs=
	case lists:keysearch(trace_log,1,Logs) of
	    {value,{_,Dir1,Logs1}} ->         % There is a list of trace-logs.
		lists:map(fun(File)->{trace_log,Dir1,File} end,Logs1);
	    false ->                          % No trace-logs!
		[]
	end,
    TiLogs=
	case lists:keysearch(ti_log,1,Logs) of
	    {value,{_,Dir2,Logs2}} ->
		lists:map(fun(File)->{ti_log,Dir2,File} end,Logs2);
	    false ->
		[]
	end,
    TiLogs++TraceLogs.

%% ------------------------------------------------------------------------------

%% Function that removes all logfiles associated with a certain tracerdata.
do_delete_logs(TracerDataOrLogList) ->
    case is_list_of_files_or_tracerdata(TracerDataOrLogList) of
	tracerdata ->
	    case translate_td(TracerDataOrLogList) of
		{ok,LogTD,TiTD} ->
		    case {list_logs_tracelog(LogTD),list_logs_tilog(TiTD)} of
			{{_,no_log},{_,no_log}} ->  % No logs nowhere!
			    {ok,no_log};
			{{LogDir,LogFiles},{_,no_log}} -> % No ti.
			    {ok,[{trace_log,delete_files(LogDir,LogFiles)}]};
			{{_,no_log},{TiDir,TiFiles}} ->
			    {ok,[{ti_log,delete_files(TiDir,TiFiles)}]};
			{{LogDir,LogFiles},{TiDir,TiFiles}} ->
			    {ok,[{trace_log,delete_files(LogDir,LogFiles)},
				 {ti_log,delete_files(TiDir,TiFiles)}]}
		    end;
		{error,Reason} ->
		    {error,Reason}
	    end;
	files ->                              % It is [{trace_log,Dir,Files},..
	    if
		is_list(hd(TracerDataOrLogList)) -> % Just a list of files.
		    {ok,delete_files(".",TracerDataOrLogList)};
		is_tuple(hd(TracerDataOrLogList)) -> % A "modern" logspec.
		    case {lists:keysearch(trace_log,1,TracerDataOrLogList),
			  lists:keysearch(ti_log,1,TracerDataOrLogList)} of
			{false,false} ->      % Hmm, no logs specified!
			    {ok,[]};          % Easy response!
			{{value,{_,LogDir,LogFiles}},false} ->
			    {ok,[{trace_log,delete_files(LogDir,LogFiles)}]};
			{false,{value,{_,TiDir,TiFiles}}} ->
			    {ok,[{ti_log,delete_files(TiDir,TiFiles)}]};
			{{value,{_,LogDir,LogFiles}},{value,{_,TiDir,TiFiles}}} ->
			    {ok,[{trace_log,delete_files(LogDir,LogFiles)},
				 {ti_log,delete_files(TiDir,TiFiles)}]}
		    end
	    end;
	false ->                              % Can't tell which!
	    {ok,[]};
	error ->
	    {error,{badarg,TracerDataOrLogList}}
    end.
%% -----------------------------------------------------------------------------

%% Function handling the request when a control component wishing to take
%% control over this already existing control component. It does not matter
%% what state it is in. It can very well already be tracing.
%% Returns {Reply,NewLoopData}.
%% Where the Reply tells the control component wether it took control of it
%% or not. {node_info,node(),self(),Vsn,State,Status,{tag,Tag}} means that we
%% can be adopted (and more precisely considers ourselves being adopted now).
do_try_to_adopt(Tag,if_ref,LoopData=#rt{tag=Tag},_Ctrl) ->
    {{error,{wrong_reference,LoopData#rt.tag}},LoopData};
do_try_to_adopt(NewTag,_Condition,LoopData,CtrlPid) ->
    case LoopData#rt.timer_ref of           % Do we have a running-alone timer?
	undefined ->                        % No we don't.
	    true;
	TimerRef ->
	    timer:cancel(TimerRef)
    end,
    CtrlRef=erlang:monitor(process,CtrlPid), % Lets monitor our new "master"!
    {DepVal,_}=LoopData#rt.dependency,
    {node_info,Node,Pid,VSN,State,Status,Tag}=collect_node_info(LoopData),
    NewLoopData=
	LoopData#rt{dependency={DepVal,node(CtrlPid)},
		    ctrl=CtrlPid,
		    ctrl_ref=CtrlRef,       % Monitoring our new master.
		    tag=NewTag,             % Use this tag from now on.
		    timer_ref=undefined},
    {{node_info,Node,Pid,VSN,State,Status,{tag,Tag}},NewLoopData}.
%% -----------------------------------------------------------------------------

%% Function changing parameters accoring to a new options list. Note that we
%% can not change control component if the one we have is still working.
%% We can however of course change how this runtime component will react to
%% a running alone scenario.
%% Returns 'stop' or NewLoopData.
do_change_options(Options,LoopData) ->
    NewLoopData=read_option_list(Options,LoopData),
    if
	NewLoopData/=LoopData ->            % Some options changed.
	    case do_change_options_ctrl(LoopData,NewLoopData) of
		stop ->
		    stop;
		{ok,NewLoopData2} ->
		    NewLoopData3=do_change_options_overload(LoopData,NewLoopData2),
		    NewLoopData3#rt{next_loadcheck=now()} % Force a load check next.
	    end;
	true ->
	    LoopData
    end.

%% Help function which sets up the new dependencies. Note that we only do that
%% if do not have a working control component.
%% Returns {ok,NewLoopData} or 'stop'.
do_change_options_ctrl(OldLD,NewLD) ->
    if
	OldLD#rt.timer_ref/=undefined ->    % No control and waiting to terminate.
	    timer:cancel(OldLD#rt.timer_ref),
	    do_down_message(NewLD#rt{timer_ref=undefined});
	OldLD#rt.ctrl==undefiend ->         % No control component.
	    do_down_message(NewLD);
	true ->                             % We have a working control component!
	    {ok,NewLD}
    end.

do_change_options_overload(OldLD,NewLD) ->
    if
	OldLD#rt.overload/=NewLD#rt.overload ->
	    terminate_overload(OldLD),
	    NewOverloadData=initialize_overload(NewLD),
	    NewLD#rt{overload_data=NewOverloadData};
	true ->                             % No changes done.
	    NewLD
    end.
%% -----------------------------------------------------------------------------

%% Help function handling an incoming DOWN message from our control component.
%% If the runtime component is not allowed to run without a control component, it
%% simply terminates which closes the trace-port and process trace flags are
%% therefore automatically removed.
%% Returns 'stop' or a {ok,NewLoopData} structure.
do_down_message(LoopData) ->
    case LoopData#rt.dependency of
	{0,_} ->                            % Not allowed to run without controller.
	    stop;
	{infinity,_} ->                     % Don't care. Just remove the controller.
	    {ok,LoopData#rt{ctrl=undefined,ctrl_ref=undefined}};
	{TimeOut,_} ->                      % Allowed to run TimeOut ms alone.
	    {ok,TimerRef}=timer:exit_after(TimeOut,self(),running_alone),
	    {ok,LoopData#rt{timer_ref=TimerRef,ctrl=undefined,ctrl_ref=undefined}}
    end.
%% -----------------------------------------------------------------------------

%% Function handling incomming exit signals. We can expect exit signals from the
%% following: Our parent supervisor (runtime_tools_sup), a meta-tracer process,
%% a logfile fetcher process, or the auto_starter.
%% A trace-port may also generate an exit signal.
%% In addition it is possible that an overload mechanism generates exit-signals.
%% We can also get the running_alone exit signal from our self. This is the
%% situation if our control component has terminated and this runtime component
%% is not allowed to exist on its own for ever.
%% Also note that after we have stopped tracing, for any reason, it is not
%% impossible that we receive the EXIT signals from still working parts that
%% we are now shuting down. This is no problem, the code will mearly update
%% the loopdata structure once again.
%% Returns 'exit' indicating that the runtime component shall terminate now,
%% {NewLoopData,NewTimeOut} if the exit-signal resulted in an overload check, or
%% a new loopdata structure shall we ignore the exit, or it simply resulted in
%% a state-change.
act_on_exit(Parent,_Reason,#rt{parent=Parent}) ->
    exit;
act_on_exit(_Pid,running_alone,_LoopData) ->
    exit;
act_on_exit(MetaTracer,_Reason,LoopData=#rt{meta_tracer=MetaTracer}) ->
    LoopData#rt{meta_tracer=undefined};     % It does not exit anylonger.
act_on_exit(Port,Reason,LoopData=#rt{tracer_port=Port}) -> 
    send_event({port_down,node(),Reason},LoopData),
    _NewLoopData=do_stop_tracing(LoopData);
act_on_exit(AutoStarter,_Reason,LoopData=#rt{auto_starter=AutoStarter}) ->
    LoopData#rt{auto_starter=undefined};    % The autostarter has terminated.
act_on_exit(Pid,Reason,LoopData) ->
    case remove_fetcher_ld(Pid,LoopData) of
	{true,NewLoopData} ->               % Yes it really was a fetcher.
	    NewLoopData;
	false ->                            % No it was not a fetcher.
	    act_on_exit_overload(Pid,Reason,LoopData)
    end.

%% Help function checking if this exit has anything to do with an overload
%% mechanism. Note that here we run the overload mechanism regardless of
%% if we are tracing or not. This because an exit signal from the overload
%% must most likely always be handled.
act_on_exit_overload(Pid,Reason,LoopData) ->
    if
	LoopData#rt.overload/=?NO_LOADCHECK ->
	    {_NewLD,_NewTimeOut}=
		do_check_overload(LoopData,
				  {'EXIT',{Pid,Reason,LoopData#rt.overload_data}});
	true ->                             % Overload not in use.
	    LoopData
    end.
%% -----------------------------------------------------------------------------















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% ==============================================================================
%% Various help functions.
%% ==============================================================================

%% Help function which calculates a new now-tuple by adding Interval milliseconds
%% to the first argument. Note that Interval may be 'infinity' too.
%% Returns a new now-tuple or "bigvalue" which is greater than any now-tuple.
add_to_now({MegSec,Sec,MicroSec},Interval) when is_integer(Interval) ->
    NewSec=Sec+(Interval div 1000),
    if
	NewSec>=1000000 ->
	    {MegSec+1,NewSec-1000000,MicroSec};
	true ->
	    {MegSec,NewSec,MicroSec}
    end;
add_to_now(_,infinity) ->
    "bigvalue".
%% ------------------------------------------------------------------------------

%% Help function calculating the difference in milliseconds between its first
%% and second argument. This is useful when calculating an after timeout value
%% from current now() and next_loadcheck value.
calc_diff_to_now(T1={_,_,_},T2={_,_,_}) ->
    TimeOut1=timer:now_diff(T2,T1),          % The difference in microseconds.
    if
	TimeOut1<0 ->
	    0;
	true ->                              % Make milliseconds out of it.
	    TimeOut1 div 1000
    end;
calc_diff_to_now(_T1,_) ->                   % Next loadcheck is not activated.
    infinity.                                % The the after timeout is infinity.
%% ------------------------------------------------------------------------------


%% Help function returning information about this runtime component.
collect_node_info(#rt{vsn=VSN,state=State,status=Status,tag=Tag}) -> 
    {node_info,node(),self(),VSN,State,Status,Tag}.
%% ------------------------------------------------------------------------------

%% Help function sending information to the control component that state/status
%% change has occurred. Returns nothing significant.
send_event(state_change,LoopData=#rt{ctrl=CtrlPid}) when is_pid(CtrlPid) ->
    Event={trace_event,{state_change,node(),{LoopData#rt.state,LoopData#rt.status}}},
    CtrlPid ! Event;
send_event(Event,#rt{ctrl=CtrlPid}) when is_pid(CtrlPid) ->
    CtrlPid ! {event,Event};
send_event(_,_) ->                          % We have no control to send to!
    true.                                   % Maybe tracing alone after autostart.
%% ------------------------------------------------------------------------------

%% Help function initializing the overload protection mechanism. This may be
%% necessary if it is a port program or similar. Returns {ok,Data} or 'void'.
%% The datastructure vill be given to LoadMF as argument whenever loadchecks
%% are done.
initialize_overload(#rt{overload={_MF,_Interval,{M,F,Args},_RemoveMFA}}) ->
    case catch apply(M,F,Args) of
	{ok,Data} ->
	    Data;
	_ ->                                % 'EXIT' or other faulty returnvalue.
	    void
    end;
initialize_overload(_) ->
    void.
%% ------------------------------------------------------------------------------

%% Help function which terminates an overload protection mechanism.
%% Returns nothing significant.
terminate_overload(#rt{overload={_MF,_Interval,_InitMFA,{M,F,Args}},
		       overload_data=Data}) ->
    catch apply(M,F,[Data|Args]),           % Interested in the side-effect.
    true;
terminate_overload(_) ->
    true.
%% ------------------------------------------------------------------------------


%% Help function which checks that a process specified for trace flags is correct.
%% Either the built-in "aliases" for groups of processes, a pid, a locally registered
%% name. This function also works for globally registered names. It must then
%% first be established that the process is local for this node before setting any
%% process flags.
%% Returns {ok,PidSpec}, 'false' or {error,Reason}.
check_traceflag_pidspec(all) -> {ok,all};
check_traceflag_pidspec(new) -> {ok,new};
check_traceflag_pidspec(existing) -> {ok,existing};
check_traceflag_pidspec(Name) when is_atom(Name) ->
    check_traceflag_pidspec({local,Name});
check_traceflag_pidspec({local,A}) when is_atom(A) ->
    case whereis(A) of
	undefined ->                         % Then it is considered faulty.
	    {error,{nonexistent_name,A}};
	Pid when is_pid(Pid) ->
	    {ok,Pid}
    end;
check_traceflag_pidspec({global,Name}) when is_atom(Name) ->
    case global:whereis_name(Name) of
	undefined ->                         % Then the name does not exist at all.
	    {error,{nonexistent_name,{global,Name}}};
	Pid when is_pid(Pid) ->                 % Ok, but must check that it is here.
	    if
		node()==node(Pid) ->
		    {ok,Pid};
		true ->                      % Pid is not at this node.
		    false                    % Not an error but cant be used.
	    end
    end;
check_traceflag_pidspec(Pid) when is_pid(Pid) ->
    {ok,Pid};
check_traceflag_pidspec(Proc) ->
    {error,{faulty,Proc}}.
%% ------------------------------------------------------------------------------

%% Help function removing all trace flags from all processes. Useful in connection
%% with suspend. Returns nothing significant.
do_clear_trace_flags() ->
    erlang:trace(all, false, [all]).
%% ------------------------------------------------------------------------------

%% Help function which checks that only valid process trace flags are mentioned.
%% In order to create better fault reports.
%% Returns a list of the approved flags, or {error,Reason}.
check_flags(Flags) ->
    check_flags_2(Flags,Flags).

check_flags_2([send|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2(['receive'|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([call|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([return_to|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([procs|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([garbage_collection|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([running|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([set_on_spawn|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([set_on_first_spawn|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([set_on_link|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([timestamp|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([arity|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([silent|Rest],Flags) -> check_flags_2(Rest,Flags);
check_flags_2([],Flags) -> Flags;
check_flags_2([Faulty|_],_Flags) -> {error,{bad_flag,Faulty}}.
%% ------------------------------------------------------------------------------    

%% Help function which checks parameters to erlang:trace_pattern. The purpose of
%% the function is to avoid to get multiple error return values in the return
%% list for a pattern used together with a regexp expanded module name.
check_pattern_parameters(Mod,Func,Arity,MS) ->
    if
	(Mod=='_') and (Func=='_') and (Arity=='_') and
	(is_list(MS) or (MS==true) or (MS==false)) ->
	    ok;
	(is_atom(Mod) and (Mod/='_')) and (Func=='_') and (Arity=='_') and
	(is_list(MS) or (MS==true) or (MS==false)) ->
	    ok;
	(is_atom(Mod) and (Mod/='_')) and 
	(is_atom(Func) and (Func/='_')) and 
	((Arity=='_') or is_integer(Arity)) and
	(is_list(MS) or (MS==true) or (MS==false)) ->
	    ok;
	true ->
	    error
    end.
%% -----------------------------------------------------------------------------

%% Help function finding out if Mod is loaded, and if not, if it can successfully
%% be loaded. The Opts list can prevent modules from being loaded.
%% Returns 'true' or 'false'.
load_module_on_option(Mod,Opts) when is_list(Opts) ->
    case lists:member(no_loadcheck,Opts) of
	true ->                             % Then just skip this, return true.
	    true;
	false ->
	    case erlang:module_loaded(Mod) of
		true ->
		    true;                   % It is loaded, do no more.
		false ->
		    case lists:member(only_loaded,Opts) of
			true ->             % Then, make no attempts to load.
			    false;
			false ->            % Try to load!
			    case code:ensure_loaded(Mod) of
				{module,_Mod} -> % Successfully loaded!
				    true;
				{error,_Reason} ->
				    false
			    end
		    end
	    end
    end;
load_module_on_option(Mod,_Opts) ->        % Most likely Opts not a list!
    load_module_on_option(Mod,[]).         % Call without options.
%% -----------------------------------------------------------------------------

%% Help function taking a tuplelist of options turning them into a loopdata
%% structure. Returns the loopdata structure with the new values changed.
read_option_list([],LD) ->                  % Done, return loopdata.
    LD;
read_option_list([{dependency,{Value,Node}}|Rest],LD) ->
    read_option_list(Rest,LD#rt{dependency={Value,Node}});
read_option_list([{dependency,Value}|Rest],LD) when is_integer(Value);Value==infinity ->
    read_option_list(Rest,LD#rt{dependency={Value,node()}});
read_option_list([overload|Rest],LD) ->     % So that we can remove loadcheck.
    read_option_list(Rest,LD#rt{overload=?NO_LOADCHECK});
read_option_list([{overload,{MF,Interval}}|Rest],LD)
  when is_integer(Interval);Interval==infinity ->
    read_option_list(Rest,LD#rt{overload={MF,Interval,void,void}});
read_option_list([{overload,{MF,Interval,InitMFA,RemoveMFA}}|Rest],LD)
  when is_integer(Interval);Interval==infinity ->
    read_option_list(Rest,LD#rt{overload={MF,Interval,InitMFA,RemoveMFA}});
read_option_list([{overload,Interval}|Rest],LD)
  when is_integer(Interval);Interval==infinity ->
    read_option_list(Rest,LD#rt{overload={fun ?DEFAULT_OVERLOAD_FUNC/1,
					  Interval,
					  void,
					  void}});
read_option_list([_|Rest],LD) ->            % Unknown option.
    read_option_list(Rest,LD).
%% -----------------------------------------------------------------------------

%% Help function which returns the version number for the runtime_tools
%% application. Since it is called from within the runtime_tools application
%% we can be "sure" that it really exists.
get_application_vsn() ->
    {value,{_,_,VSN}}=lists:keysearch(runtime_tools,1,application:loaded_applications()),
    VSN.
%% -----------------------------------------------------------------------------

%% Help function that examines an argument to determine if it is a list of files
%% or tracerdata. This since they are both complex structures, looking alike.
%% Returns 'tracerdata', 'files', 'false' or 'error'. Error is returned if it
%% can not be decided which it is.
is_list_of_files_or_tracerdata(What) ->
    case inviso_rt_lib:is_tracerdata(What) of
	true ->
	    tracerdata;
	false ->
	    if
		What==[] ->
		    false;
		is_list(What),is_list(hd(What)) ->
		    files;
		is_list(What) ->
		    case lists:keysearch(trace_log,1,What) of
			{value,_} ->
			    files;
			false ->
			    case lists:keysearch(ti_log,1,What) of
				{value,_} ->
				    files;
				false ->
				    error    % Neither tracerdata nor list of files.
			    end
		    end;
		true ->
		    error
	    end
    end.
%% ------------------------------------------------------------------------------

%% Help function which removes all files in the ListOfFiles, assuming they
%% are located in Dir.
%% Returns a list of [{ok,FileName},...{error,Reason},...]
delete_files(Dir,ListOfFiles) ->
    delete_files_2(Dir,ListOfFiles, []).

delete_files_2(Dir,[File|Tail],Reply) when is_list(Dir),is_list(File) ->
    case catch file:delete(filename:join(Dir,File)) of
	ok ->
	    delete_files_2(Dir,Tail,[{ok,File}|Reply]);
	{error,Posix} ->
	    delete_files_2(Dir,Tail,[{error,{Posix,File}}|Reply]);
	{'EXIT',_Reason} ->                  % Probably not proper string.
	    delete_files_2(Dir,Tail,[{error,{badarg,[Dir,File]}}|Reply])
    end;
delete_files_2(Dir,[Faulty|Tail],Reply) ->
    delete_files_2(Dir,Tail,[{error,{badarg,[Dir,Faulty]}}|Reply]);
delete_files_2(_,[],Reply) ->
    Reply.
%% -----------------------------------------------------------------------------

%% Help function which lists all trace logs belonging to this tracerdata.
%% Note that this function operates on internal LogTD structures.
list_logs_tracelog({file,FileName}) when is_list(FileName) ->
    case file:read_file_info(FileName) of
	{ok,_} ->                            % The file exists.
	    {filename:dirname(FileName),[filename:basename(FileName)]};
	_ ->                                 % The file does not exist
	    {filename:dirname(FileName),[]}
    end;
list_logs_tracelog({file,Wrap}) when is_tuple(Wrap),element(2,Wrap)==wrap ->
    case {element(1,Wrap),element(3,Wrap)} of
	{FileName,Tail} when is_list(FileName),is_list(Tail) ->
	    case catch {filename:dirname(FileName),list_wrapset(FileName,Tail)} of
		{'EXIT',_Reason} ->          % Garbage in either lists.
		    {"",no_log};             % Interpret as no log for tracerdata.
		Tuple ->
		    Tuple
	    end;
	_ ->
	    {"",no_log}
    end;
list_logs_tracelog(void) ->                  % Trace log not used.
    {"",no_log};
list_logs_tracelog(_) ->                     % Some fun or similar.
    {"",no_log}.                             % Then there are no files to report.
%% -----------------------------------------------------------------------------

%% Help function which lists all ti-files belonging to this tracerdata.
%% Note that this function operates on the internal TiTD structure.
list_logs_tilog(TiTD)
  when tuple_size(TiTD)>=2,element(1,TiTD)==file,is_list(element(2,TiTD)) ->
    FileName=element(2,TiTD),
    case file:read_file_info(FileName) of
	{ok,_} ->                            % Yes the file exists.
	    {filename:dirname(FileName),[filename:basename(FileName)]};
	_ ->
	    {filename:dirname(FileName),[]}
    end;
list_logs_tilog(void) ->                     % Internal representation for
    {"",no_log};                             % ti-file not in use.
list_logs_tilog(_) ->
    {"",no_log}.
%% -----------------------------------------------------------------------------

%% Help function which lists all files belonging to the wrap-set specified by
%% Prefix and Suffix. Note that there can be a directory in Prefix as well.
%% Will fail if either of Prefix or Suffix are not proper strings.
%% Returns a list of files, without dirname.
list_wrapset(Prefix,Suffix) ->
    Name=filename:basename(Prefix),
    Dirname=filename:dirname(Prefix),
    case file:list_dir(Dirname) of
	{ok,Files} ->
	    RegExp="^"++list_wrapset_escapes(Name)++"[0-9]+"++
		list_wrapset_escapes(Suffix)++"$",
	    list_wrapset_2(Files,RegExp);
	{error,_Reason} ->                   % Translate this to no files!
	    []
    end.

list_wrapset_2([File|Rest],RegExp) ->
    Length=length(File),
    case regexp:first_match(File,RegExp) of
	{match,1,Length} ->                  % This is a member of the set.
	    [File|list_wrapset_2(Rest,RegExp)];
	_ ->
	    list_wrapset_2(Rest,RegExp)
    end;
list_wrapset_2([],_) ->
    [].

%% Help function which inserts escape characters infront of characters which
%% will otherwise be missinterpreted by the regexp function as meta rather than
%% just the character itself.
list_wrapset_escapes([$.|Rest]) ->
    [$\\,$.|list_wrapset_escapes(Rest)];
list_wrapset_escapes([Char|Rest]) ->
    [Char|list_wrapset_escapes(Rest)];
list_wrapset_escapes([]) ->
    [].
%% -----------------------------------------------------------------------------






%% ==============================================================================
%% Handler functions for implementing simple trace-message handlers.
%% ==============================================================================
%%
%% A handler must be a function taking two arguments. The first is the trace-
%% message. The second is datastructure used by the handler. The handler shall
%% returns (possibly) new datastructure.

%% ------------------------------------------------------------------------------
%% Function implementing a relayer. This function is used to creat a fun handler
%% if the relay option is used in tracer-data.
%% ------------------------------------------------------------------------------
relay_handler(Msg,Tracer) ->
    Tracer ! Msg,
    Tracer.

%% ------------------------------------------------------------------------------
%% Function implementing a default terminal io handler.
%% ------------------------------------------------------------------------------

dhandler(end_of_trace, Out) ->
    Out;
dhandler(Trace, Out) when element(1, Trace) == trace, 
			  tuple_size(Trace) >= 3 ->
    dhandler1(Trace, tuple_size(Trace), Out);
dhandler(Trace, Out) when element(1, Trace) == trace_ts, 
			  tuple_size(Trace) >= 4 ->
    dhandler1(Trace, tuple_size(Trace)-1, Out);
dhandler(Trace, Out) when element(1, Trace) == drop, 
			  tuple_size(Trace) == 2 ->
    io:format(Out, "*** Dropped ~p messages.~n", [element(2,Trace)]),
    Out;
dhandler(Trace, Out) when element(1, Trace) == seq_trace, 
			  tuple_size(Trace) >= 3 ->
    SeqTraceInfo = case Trace of
		       {seq_trace, Lbl, STI, TS} ->
			   io:format(Out, "SeqTrace ~p [~p]: ",
				     [TS, Lbl]),
			   STI;
		       {seq_trace, Lbl, STI} ->
			   io:format(Out, "SeqTrace [~p]: ",
				     [Lbl]),
			   STI 
		   end,
    case SeqTraceInfo of
	{send, Ser, Fr, To, Mes} ->
	    io:format(Out, "(~p) ~p ! ~p [Serial: ~p]~n",
		      [Fr, To, Mes, Ser]);
	{'receive', Ser, Fr, To, Mes} ->
	    io:format(Out, "(~p) << ~p [Serial: ~p, From: ~p]~n",
		      [To, Mes, Ser, Fr]);
	{print, Ser, Fr, _, Info} ->
	    io:format(Out, "-> ~p [Serial: ~p, From: ~p]~n",
		      [Info, Ser, Fr]);
	Else ->
	    io:format(Out, "~p~n", [Else])
    end,
    Out;
dhandler(_Trace, Out) ->
    Out.

dhandler1(Trace, Size, Out) ->
%%%!    Self = self(),
    From = element(2, Trace),
    case element(3, Trace) of
	'receive' ->
	    case element(4, Trace) of
		{dbg,ok} -> ok;
		Message -> io:format(Out, "(~p) << ~p~n", [From,Message])
	    end;
	'send' ->
	    Message = element(4, Trace),
	    case element(5, Trace) of
%%%! This causes messages to disappear when used by ttb (observer). Tests
%%%! so far show that there is no difference in results with dbg even if I
%%%! comment it out, so  I hope this is only some old code which isn't
%%%! needed anymore... /siri
%%%!		Self -> ok;
		To -> io:format(Out, "(~p) ~p ! ~p~n", [From,To,Message])
	    end;
	call ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
		    io:format(Out, "(~p) call ~s (~p)~n", 
			      [From,ffunc(MFA),Message]);
		MFA ->
		    io:format(Out, "(~p) call ~s~n", [From,ffunc(MFA)])
	    end;
	return -> %% To be deleted...
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Ret = element(5, Trace),
		    io:format(Out, "(~p) old_ret ~s -> ~p~n", 
			      [From,ffunc(MFA),Ret]);
		MFA ->
		    io:format(Out, "(~p) old_ret ~s~n", [From,ffunc(MFA)])
	    end;
	return_from ->
	    MFA = element(4, Trace),
	    Ret = element(5, Trace),
	    io:format(Out, "(~p) returned from ~s -> ~p~n", 
		      [From,ffunc(MFA),Ret]);
	return_to ->
	    MFA = element(4, Trace),
	    io:format(Out, "(~p) returning to ~s~n", [From,ffunc(MFA)]);
	spawn when Size == 5 ->
	    Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    io:format(Out, "(~p) spawn ~p as ~s~n", [From,Pid,ffunc(MFA)]);
	Op ->
	    io:format(Out, "(~p) ~p ~s~n", [From,Op,ftup(Trace,4,Size)])
    end,
    Out.


%%% These f* functions returns non-flat strings

%% {M,F,[A1, A2, ..., AN]} -> "M:F(A1, A2, ..., AN)"
%% {M,F,A}                 -> "M:F/A"
ffunc({M,F,Argl}) when is_list(Argl) ->
    io_lib:format("~p:~p(~s)", [M, F, fargs(Argl)]);
ffunc({M,F,Arity}) ->
    io_lib:format("~p:~p/~p", [M,F,Arity]);
ffunc(X) -> io_lib:format("~p", [X]).

%% Integer           -> "Integer"
%% [A1, A2, ..., AN] -> "A1, A2, ..., AN"
fargs(Arity) when is_integer(Arity) -> integer_to_list(Arity);
fargs([]) -> [];
fargs([A]) -> io_lib:format("~p", [A]);  %% last arg
fargs([A|Args]) -> [io_lib:format("~p,", [A]) | fargs(Args)];
fargs(A) -> io_lib:format("~p", [A]). % last or only arg

%% {A_1, A_2, ..., A_N} -> "A_Index A_Index+1 ... A_Size"
ftup(Trace, Index, Index) -> 
    io_lib:format("~p", [element(Index, Trace)]);
ftup(Trace, Index, Size) -> 
    [io_lib:format("~p ", [element(Index, Trace)]) 
     | ftup(Trace, Index+1, Size)].
%% ------------------------------------------------------------------------------

%% ==============================================================================
%% Functions handling the trace-port. Copied from dbg.erl
%% ==============================================================================

trace_port_control(Port, flush) ->
    case trace_port_control(Port, $f, "") of
	{ok, [0]} -> ok;
	{ok, _}   -> {error, not_supported_by_trace_driver};
	Other     -> Other
    end.

trace_port_control(Port, Command, Arg) when is_port(Port)->
    case catch port_control(Port, Command, Arg) of
	{'EXIT', _} -> {error, {no_trace_driver, node()}};
	Result      -> Result
    end.


trace_port(file, {Filename, wrap, Tail}) ->
    trace_port(file, {Filename, wrap, Tail, 128*1024});
trace_port(file, {Filename, wrap, Tail, WrapSize}) ->
    trace_port(file, {Filename, wrap, Tail, WrapSize, 8});
trace_port(file, {Filename, wrap, Tail, WrapSize, WrapCnt})
  when is_list(Tail), 
       is_integer(WrapSize), WrapSize >= 0, WrapSize < (1 bsl 32),
       is_integer(WrapCnt), WrapCnt >= 1, WrapCnt < (1 bsl 32) ->
   trace_port1(file, Filename, {wrap, Tail, WrapSize, WrapCnt, 0});
trace_port(file, {Filename, wrap, Tail, {time, WrapTime}, WrapCnt})
  when is_list(Tail), 
       is_integer(WrapTime), WrapTime >= 1, WrapTime < (1 bsl 32),
       is_integer(WrapCnt), WrapCnt >= 1, WrapCnt < (1 bsl 32) ->
    trace_port1(file, Filename, {wrap, Tail, 0, WrapCnt, WrapTime});
trace_port(file, Filename) when is_list(Filename) ->
    trace_port1(file, Filename, nowrap);

trace_port(ip, Portno) when is_integer(Portno) -> 
    trace_port(ip,{Portno,50});

trace_port(ip, {Portno, Qsiz}) when is_integer(Portno), is_integer(Qsiz) -> 
    fun() ->
	    Driver = "trace_ip_drv",
	    Dir1 = filename:join(code:priv_dir(runtime_tools), "lib"),
	    case catch erl_ddll:load_driver(Dir1, Driver) of
		ok ->
		    ok;
		_ ->
		    Dir2 = filename:join(
			     Dir1, 
			     erlang:system_info(system_architecture)),
		    catch erl_ddll:load_driver(Dir2, Driver)
	    end,
	    L = lists:flatten(
		  io_lib:format("~s ~p ~p 2",
				[Driver, Portno, Qsiz])),
	    open_port({spawn, L}, [eof])
    end.

trace_port1(file, Filename, Options) ->
    Driver = "trace_file_drv",
    fun() ->
	    Name = filename:absname(Filename), 
	    %% Absname is needed since the driver uses 
	    %% the supplied name without further investigations, 
	    %% and if the name is relative the resulting path 
	    %% might be too long which can cause a bus error
	    %% on vxworks instead of a nice error code return.
	    %% Also, the absname must be found inside the fun,
	    %% in case the actual node where the port shall be
	    %% started is on another node (or even another host)
	    {Wrap, Tail} =
		case Options of
		    {wrap, T, WrapSize, WrapCnt, WrapTime} ->
			{lists:flatten(
			   io_lib:format("w ~p ~p ~p ~p ", 
					 [WrapSize, WrapCnt, WrapTime, 
					  length(Name)])),
			 T};
		    nowrap ->
			{"", ""}
		end,
	    Command = Driver ++ " " ++ Wrap ++ "n " ++ Name ++ Tail,
	    Dir1 = filename:join(code:priv_dir(runtime_tools), "lib"),
	    case catch erl_ddll:load_driver(Dir1, Driver) of
		ok ->
		    ok;
		_ ->
		    Dir2 = filename:join(
			     Dir1, 
			     erlang:system_info(system_architecture)),
		    catch erl_ddll:load_driver(Dir2, Driver)
	    end,
	    if 	element(1, Options) == wrap ->
		    %% Delete all files from any previous wrap log
		    Files = wrap_postsort(wrap_presort(Name, Tail)),
		    lists:foreach(
		      fun(N) -> file:delete(N) end,
		      Files);
		true -> ok
	    end,
	    open_port({spawn, Command}, [eof])
    end.

%% Find all possible wrap log files.
%% Returns: a list of sort converted filenames.
%%
%% The sort conversion is done by extracting the wrap sequence counter
%% from the filename, and calling wrap_encode/2.
wrap_presort(Filename, Tail) ->
    Name = filename:basename(Filename),
    Dirname = filename:dirname(Filename),
    case file:list_dir(Dirname) of
	{ok, Files} ->
	    lists:zf(
	      fun(N) ->
		      case match_front(N, Name) of
			  false ->
			      false;
			  X ->
			      case match_rear(X, Tail) of
				  false ->
				      false;
				  C -> % Counter
				      case match_0_9(C) of
					  true ->
					      {true, 
%						 filename:join(Dirname, N)}
					       wrap_encode(
						 filename:join(Dirname, N),
						 C)};
					  false ->
					      false
				      end
			      end
		      end
	      end,
	      Files);
	_ ->
	    []
    end.

%% Extract the filenames from a list of sort converted ones.
wrap_postsort(Files) ->    
    lists:map(fun wrap_name/1, Files).

wrap_encode(N, C) ->
    {list_to_integer(C), N}.

wrap_name({_C, N}) ->
    N.

%% Returns what is left of ListA when removing all matching
%% elements from ListB, or false if some element did not match,
%% or if ListA runs out of elements before ListB.
match_front(ListA, []) when is_list(ListA) ->
    ListA;
match_front([], ListB) when is_list(ListB) ->
    false;
match_front([Hd|TlA], [Hd|TlB]) ->
    match_front(TlA,TlB);
match_front([_HdA|_], [_HdB|_]) ->
    false.

%% Reversed version of match_front/2
match_rear(ListA, ListB) when is_list(ListA), is_list(ListB) ->
    case match_front(lists:reverse(ListA), lists:reverse(ListB)) of
	false ->
	    false;
	List ->
	    lists:reverse(List)
    end.

%% Returns true if the non-empty list arguments contains all
%% characters $0 .. $9.
match_0_9([]) ->
    false;
match_0_9([H]) when is_integer(H), $0 =< H, H =< $9 ->
    true;
match_0_9([H|T]) when is_integer(H), $0 =< H, H =< $9 ->
    match_0_9(T);
match_0_9(L) when is_list(L) ->
    false.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Functions working on the tracerdata structure.
%% -----------------------------------------------------------------------------

%% Tracerdata is the structure which specifies to where tracing is logged at this
%% runtime component. It may now (and in the future specify) several things.
%% Currently it can consist of:
%% LogTD: specifying how trace-log data shall be handled.
%% TiTD : trace information, specifying how trace information shall be handled.
%%
%% Tracerdata may also contain quick or standard forms of LogTD and/or TiTD.
%% For instance if a standard handler-fun shall be used. The handler fun is not
%% part of the tracerdata but rather specified by a constant.


%% Help function that translates an input-tracerdata to useful internal formats.
%% This since the tracerdata may consist of specifications which shall be
%% translated into funs or similar.
%% Returns {ok,LogTD,TiTD} or {error,Reason}.
%% Note that TiTD may be 'void' since TiTD is not mandatory.
translate_td(TracerData) when is_list(TracerData) -> % Both log and ti.
    case translate_td_logtd(get_trace_log_tracerdata(TracerData)) of
	{ok,LogTD} ->
	    case translate_td_titd(get_ti_log_tracerdata(TracerData)) of
		{ok,TiTD} ->
		    {ok,LogTD,TiTD};
		{error,Reason} ->
		    {error,Reason}
	    end;
	{error,Reason} ->
	    {error,Reason}
    end;
translate_td(TracerData) ->                  % The it is just LogTD!?
    case translate_td_logtd(TracerData) of
	{ok,LogTD} ->
	    {ok,LogTD,void};
	{error,Reason} ->
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------

%% Help function translating trace-log tracerdata.
translate_td_logtd(collector) ->             % This rt will act as receiver.
    {ok,{fun dhandler/2,user}};              % Simple terminal io.
translate_td_logtd({relayer,Tracer}) when is_pid(Tracer) ->
    {ok,{fun relay_handler/2,Tracer}};       % Relay trace-msg to Tracer-pid.
translate_td_logtd({HandlerFun,Data}) when is_function(HandlerFun) ->
    {ok,{HandlerFun,Data}};                  % Own invented fun.
translate_td_logtd({Type,Parameters}) when Type==ip;Type==file ->
    {ok,{Type,Parameters}};                  % Built in trace-port
translate_td_logtd(false) ->                 % Unusual but no trace log.
    {ok,void};
translate_td_logtd(Arg) ->
    {error,{bad_log_td,Arg}}.
%% -----------------------------------------------------------------------------

%% Help function translating ti-log tracerdata.
translate_td_titd(TiTD={file,FileName}) when is_list(FileName) ->
    {ok,TiTD};
translate_td_titd({file,FileName,
		   {InitPublLDmfa={M1,F1,L1},
		    RemovePublLDmf={M2,F2},
		    CleanPublLDmf={M3,F3}}})
  when is_list(FileName),is_atom(M1),is_atom(F1),is_atom(M2),is_atom(F2),is_list(L1),is_atom(M3),is_atom(F3) ->
    {ok,{file,FileName,{InitPublLDmfa,RemovePublLDmf,CleanPublLDmf}}};
translate_td_titd({file,FileName,
		   {InitPublLDmfa={M1,F1,L1},
		    void,
		    CleanPublLDmf={M3,F3}}})
  when is_list(FileName),is_atom(M1),is_atom(F1),is_list(L1),is_atom(M3),is_atom(F3) ->
    {ok,{file,FileName,{InitPublLDmfa,void,CleanPublLDmf}}};
translate_td_titd(false) ->                  % Means no ti-tracerdata.
    {ok,void};
translate_td_titd(TiTD) ->
    {error,{bad_ti_td,TiTD}}.
%% -----------------------------------------------------------------------------

%% This function retrieves the trace-log part of a TracerData list structure.
%% Returns TraceLogTD or 'false'.
get_trace_log_tracerdata(TracerData) ->
    case lists:keysearch(trace,1,TracerData) of
	{value,{_,LogTD}} ->
	    LogTD;
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

%% This function retrieves the ti-log part of a TracerData list structure.
%% Returns TiLogTD or 'false'.
get_ti_log_tracerdata(TracerData) ->
    case lists:keysearch(ti,1,TracerData) of
	{value,{_,TiTD}} ->
	    TiTD;
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

%% Help function which checks that parameters to the built in trace-port are
%% sane.
check_traceport_parameters(Type,Args) ->
    case {Type,Args} of
	{file,{FileName,wrap,Tail}} when is_list(FileName),is_list(Tail) ->
	    ok;
	{file,{FileName,wrap,Tail,WrapSize}}
	when is_list(FileName),
	is_list(Tail),
	is_integer(WrapSize),WrapSize>=0,WrapSize< (1 bsl 32) ->
	    ok;
	{file,{FileName,wrap,Tail,WrapSize,WrapCnt}}
	when is_list(FileName),is_list(Tail), 
	is_integer(WrapSize), WrapSize >= 0, WrapSize < (1 bsl 32),
	is_integer(WrapCnt), WrapCnt >= 1, WrapCnt < (1 bsl 32) ->
	    ok;
	{file,{FileName,wrap,Tail,{time,WrapTime},WrapCnt}}
	when is_list(FileName),is_list(Tail), 
	is_integer(WrapTime), WrapTime >= 1, WrapTime < (1 bsl 32),
	is_integer(WrapCnt), WrapCnt >= 1, WrapCnt < (1 bsl 32) ->
	    ok;
	{file,FileName} when is_list(FileName) ->
	    ok;
	{ip,Portno} when is_integer(Portno),Portno=<16#FFFF -> 
	    ok;
	{ip,{Portno,Qsiz}} when is_integer(Portno),Portno=<16#FFFF,is_integer(Qsiz) ->
	    ok;
	_ ->
	    {error,{trace_port_args,[Type,Args]}}
    end.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Default overload functionality.
%% -----------------------------------------------------------------------------

%% A default overload protection function. An overload function must take
%% one argument and return 'ok' or {suspend,SuspendReason}.
default_overload_func(_) ->
    case process_info(self(),message_queue_len) of
	{message_queue_len,N} when N > 1000 -> 
	    {suspend,rt_max_queue_len};
	_ ->
	    ok
    end.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% Functions working on the internal loopdata structure.
%% =============================================================================

%% Help function simply adding Fetcher as a fetcher process to the loopdata.
%% Returns a new loopdata structure.
add_fetcher_ld(Fetcher,LD) ->
    LD#rt{fetchers=[Fetcher|LD#rt.fetchers]}.
%% -----------------------------------------------------------------------------

%% Help function investigating if the first argument is a known fetcher process
%% or not. If it is, it also removed it from the fetchers list in the loopdata
%% structure.
%% Returns {true,NewLoopData} or 'false'.
remove_fetcher_ld(Fetcher,LD) ->
    NewFetchers=lists:delete(Fetcher,LD#rt.fetchers),
    if
	NewFetchers/=LD#rt.fetchers ->
	    {true,LD#rt{fetchers=NewFetchers}};
	true ->                             % No it was not a fetcher process.
	    false
    end.
%% -----------------------------------------------------------------------------

%%% end of file


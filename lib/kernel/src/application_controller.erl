%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(application_controller).

%% External exports
-export([start/1, 
	 load_application/1, unload_application/1, 
	 start_application/2, start_boot_application/2, stop_application/1,
	 control_application/1,
	 change_application_data/2, prep_config_change/0, config_change/1,
	 which_applications/0, which_applications/1,
	 loaded_applications/0, info/0,
	 get_pid_env/2, get_env/2, get_pid_all_env/1, get_all_env/1,
	 get_pid_key/2, get_key/2, get_pid_all_key/1, get_all_key/1,
	 get_master/1, get_application/1, get_application_module/1,
	 start_type/1, permit_application/2, do_config_diff/2,
	 set_env/3, set_env/4, unset_env/2, unset_env/3]).

%% Internal exports
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
	 code_change/3, init_starter/4, get_loaded/1]).

%% Test exports, only to be used from the test suites
-export([test_change_apps/2]).

-import(lists, [zf/2, map/2, foreach/2, foldl/3,
		keyfind/3, keydelete/3, keyreplace/4]).

-include("application_master.hrl").

-define(AC, ?MODULE). % Name of process

%%%-----------------------------------------------------------------
%%% The application_controller controls local applications only.  A
%%% local application can be loaded/started/stopped/unloaded and
%%% changed.  The control of distributed applications is taken care of
%%% by another process (default is dist_ac).
%%%
%%% When an application has been started (by a call to application:start)
%%% it can be running or not running (on this node).  For example,
%%% a distributed application must be started on all nodes, but
%%% may be running on one node at the time.
%%%
%%% The external API to this module is in the module 'application'.
%%% 
%%% The process that controls distributed applications (called dist
%%% ac).  calls application_controller:control_application(Name) to
%%% take responsibility for an application.  The interface between AC
%%% and the dist_ac process is message-based:
%%%
%%% AC                                        DIST AC
%%% ==                                        =======
%%%     --> {ac_load_application_req, Name}
%%%     <-- {ac_load_application_reply, Name, LoadReply}
%%%     --> {ac_start_application_req, Name}       (*)
%%%     <-- {ac_start_application_reply, Name, StartReply}
%%%     --> {ac_application_run, Name, Res}
%%%     --> {ac_application_not_run, Name, Res}
%%%     --> {ac_application_stopped, Name}
%%%     --> {ac_application_unloaded, Name}
%%%     <-- {ac_change_application_req, Name, Req} (**)
%%%
%%% Where LoadReply =
%%%   ok              - App is loaded
%%%   {error, R}      - An error occurred
%%% And StartReply =
%%%   start_it        - DIST AC decided that AC should start the app
%%%   {started, Node} - The app is started distributed at Node
%%%   not_started     - The app should not be running at this time
%%%   {takeover, Node}- The app should takeover from Node
%%%   {error, R}      - an error occurred
%%% And Req =
%%%   start_it        - DIST AC wants AC to start the app locally
%%%   stop_it         - AC should stop the app.
%%%   {takeover, Node, RestartType}
%%%                   - AC should start the app as a takeover
%%%   {failover, Node, RestartType}
%%%                   - AC should start the app as a failover
%%%   {started, Node} - The app is started at Node
%%%                     NOTE: The app must have been started at this node
%%%                     before this request is sent!
%%% And Res =
%%%   ok              - Application is started locally
%%%   {error, R}      - Start of application failed
%%%
%%% (*)
%%%  The call to application:start() doesn't return until the
%%%  ac_start_application_reply has been received by AC.  AC
%%%  itself is not blocked however.
%%% (**)
%%%  DIST AC gets ACK to its ac_change_application_req, but not as a
%%%  separate messgage.  Instead the normal messages are used as:
%%%   start_it   generates an ac_application_run
%%%   stop_it    generates an ac_application_not_run
%%%   takeover   generates an ac_application_run
%%%   started    doesn't generate anything
%%%
%%% There is a distinction between application:stop and stop_it
%%% from a dist ac process.  The first one stops the application,
%%% and resets the internal structures as they were before start was
%%% called.  stop_it stops the application, but just marks it as
%%% not being running.
%%%
%%% When a dist ac process has taken control of an application, no
%%% other process can take the control.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Naming conventions:
%%   App = appl_descr()
%%   Appl = #appl
%%   AppName = atom()
%%   Application = App | AppName
%%-----------------------------------------------------------------

-type appname() :: atom().

-record(state, {loading = [], starting = [], start_p_false = [], running = [],
		control = [], started = [], start_req = [], conf_data}).
-type state() :: #state{}.

%%-----------------------------------------------------------------
%% loading     = [{AppName, From}] - Load not yet finished
%% starting    = [{AppName, RestartType, Type, From}] - Start not
%%                 yet finished
%% start_p_false = [{AppName, RestartType, Type, From}] - Start not
%%                 executed because permit == false
%% running     = [{AppName, Pid}] - running locally (Pid == application_master)
%%               [{AppName, {distributed, Node}}] - running on Node
%% control     = [{AppName, Controller}]
%% started     = [{AppName, RestartType}] - Names of all apps that
%%                 have been started (but may not run because
%%                 permission = false)
%% conf_data   = [{AppName, Env}]
%% start_req   = [{AppName, From}] - list of all start requests
%% Id          = AMPid | undefined | {distributed, Node}
%% Env         = [{Key, Value}]
%%-----------------------------------------------------------------

-record(appl, {name, appl_data, descr, id, vsn, restart_type, inc_apps, apps}).

%%-----------------------------------------------------------------
%% Func: start/1
%% Args: KernelApp = appl_descr()
%%       appl_descr() = [{application, Name, [appl_opt()]}]
%%       appl_opt() = {description, string()}           |
%%                    {vsn, string()}                   |
%%                    {id, string()},                   |
%%                    {modules, [Module]}  |
%%                    {registered, [atom()]}            |
%%                    {applications, [atom()]}          |
%%                    {included_applications, [atom()]} |
%%                    {env, [{atom(), term()}]}         |
%%                    {start_phases, [{atom(), term()}]}|
%%                    {maxT, integer()|infinity}        |
%%                    {maxP, integer()|infinity}        |
%%                    {mod, {Module, term()}}
%%         Module = atom()
%% Purpose: Starts the application_controller.  This process starts all
%%          application masters for the applications.
%%          The kernel application is the only application that is
%%          treated specially.  The reason for this is that the kernel
%%          starts user.  This process is special because it should
%%          be group_leader for this process.
%% Pre: All modules are loaded, or will be loaded on demand.
%% Returns: {ok, Pid} | ReasonStr
%%-----------------------------------------------------------------
start(KernelApp) ->
    %% OTP-5811 Don't start as a gen_server to prevent crash report
    %% when (if) the process terminates
    Init = self(),
    AC = spawn_link(fun() -> init(Init, KernelApp) end),
    receive
	{ack, AC, ok} ->
	    {ok, AC};
	{ack, AC, {error, Reason}} ->
	    to_string(Reason); % init doesn't want error tuple, only a reason
	{'EXIT', _Pid, Reason} ->
	    to_string(Reason)
    end.
	
%%-----------------------------------------------------------------
%% Func: load_application/1
%% Args: Application = appl_descr() | atom()
%% Purpose: Loads an application.  Currently just inserts the
%%          application's env.
%% Returns: ok | {error, Reason}
%%-----------------------------------------------------------------
load_application(Application) ->
    gen_server:call(?AC, {load_application, Application}, infinity).

unload_application(AppName) ->
    gen_server:call(?AC, {unload_application, AppName}, infinity).

%%-----------------------------------------------------------------
%% Func: start_application/2
%% Args: Application = atom()
%%       RestartType = permanent | transient | temporary
%% Purpose: Starts a new application.
%%          The RestartType specifies what should happen if the
%%          application dies:
%%          If it is permanent, all other applications are terminated,
%%            and the application_controller dies.
%%          If it is transient, and the application dies normally,
%%            this is reported and no other applications are terminated.
%%            If the application dies abnormally, all other applications
%%            are terminated, and the application_controller dies.
%%          If it is temporary and the application dies this is reported
%%            and no other applications are terminated.  In this way,
%%            an application can run in test mode, without disturbing
%%            the other applications.
%%          The caller of this function is suspended until the application
%%          is started, either locally or distributed.
%% Returns: ok | {error, Reason}
%%-----------------------------------------------------------------
start_application(AppName, RestartType) ->
    gen_server:call(?AC, {start_application, AppName, RestartType}, infinity).

%%-----------------------------------------------------------------
%% Func: start_boot_application/2
%% The same as start_application/2 expect that this function is
%% called from the boot script file. It mustnot be used by the operator.
%% This function will cause a node crash if a permanent application 
%% fails to boot start
%%-----------------------------------------------------------------
start_boot_application(Application, RestartType) ->
    case {application:load(Application), RestartType} of
	{ok, _} ->
	    AppName = get_appl_name(Application),
	    gen_server:call(?AC, {start_application, AppName, RestartType}, infinity);
	{{error, {already_loaded, AppName}}, _} ->
	    gen_server:call(?AC, {start_application, AppName, RestartType}, infinity);
	{{error,{bad_environment_value,Env}}, permanent} ->
	    Txt = io_lib:format("Bad environment variable: ~tp  Application: ~p",
				[Env, Application]),
	    exit({error, list_to_atom(lists:flatten(Txt))});
	{Error, _} ->
	    Error
    end.

stop_application(AppName) ->
    gen_server:call(?AC, {stop_application, AppName}, infinity).

%%-----------------------------------------------------------------
%% Returns: [{Name, Descr, Vsn}]
%%-----------------------------------------------------------------
which_applications() ->
    gen_server:call(?AC, which_applications).    
which_applications(Timeout) ->
    gen_server:call(?AC, which_applications, Timeout).

loaded_applications() ->
    ets:filter(ac_tab,
	       fun([{{loaded, AppName}, #appl{descr = Descr, vsn = Vsn}}]) ->
		       {true, {AppName, Descr, Vsn}};
		  (_) ->
		       false
	       end,
	       []).

%% Returns some debug info
info() ->
    gen_server:call(?AC, info).    

control_application(AppName) ->
    gen_server:call(?AC, {control_application, AppName}, infinity).

%%-----------------------------------------------------------------
%% Func: change_application_data/2
%% Args: Applications = [appl_descr()]
%%       Config = [{AppName, [{Par,Val}]}]
%% Purpose: Change all applications and their parameters on this node.
%%          This function should be used from a release handler, at
%%          the same time as the .app or start.boot file is
%%          introduced.  Note that during some time the ACs may have
%%          different view of e.g. the distributed applications.
%%          This is solved by syncing the release installation.
%%          However, strange things may happen if a node crashes
%%          and two other nodes have different opinons about who's
%%          gonna start the applications.  The release handler must
%%          shutdown each involved node in this case.
%%          Note that this function is used to change existing apps,
%%          adding new/deleting old isn't handled by this function.
%%          Changes an application's vsn, descr and env.
%% Returns: ok | {error, Reason}
%%          If an error occurred, the situation may be inconsistent,
%%          so the release handler must restart the node.  E.g. if
%%          some applicatation may have got new config data.
%%-----------------------------------------------------------------
change_application_data(Applications, Config) ->
    gen_server:call(?AC, 
		    {change_application_data, Applications, Config},
		    infinity).

prep_config_change() ->
    gen_server:call(?AC, 
		    prep_config_change,
		    infinity).


config_change(EnvPrev) ->
    gen_server:call(?AC, 
		    {config_change, EnvPrev},
		    infinity).



get_pid_env(Master, Key) ->
    case ets:match(ac_tab, {{application_master, '$1'}, Master}) of
	[[AppName]] -> get_env(AppName, Key);
	_ -> undefined
    end.

get_env(AppName, Key) ->
    case ets:lookup(ac_tab, {env, AppName, Key}) of
	[{_, Val}] -> {ok, Val};
	_ -> undefined
    end.

get_pid_all_env(Master) ->
    case ets:match(ac_tab, {{application_master, '$1'}, Master}) of
	[[AppName]] -> get_all_env(AppName);
	_ -> []
    end.

get_all_env(AppName) ->
    map(fun([Key, Val]) -> {Key, Val} end,
	ets:match(ac_tab, {{env, AppName, '$1'}, '$2'})).




get_pid_key(Master, Key) ->
    case ets:match(ac_tab, {{application_master, '$1'}, Master}) of
	[[AppName]] -> get_key(AppName, Key);
	_ -> undefined
    end.

get_key(AppName, Key) ->
    case ets:lookup(ac_tab, {loaded, AppName}) of
	[{_, Appl}] ->
	    case Key of 
		description ->
		    {ok, Appl#appl.descr};
		id ->
		    {ok, Appl#appl.id};
		vsn ->
		    {ok, Appl#appl.vsn};
		modules ->
		    {ok, (Appl#appl.appl_data)#appl_data.mods};
		maxP ->
		    {ok, (Appl#appl.appl_data)#appl_data.maxP};
		maxT ->
		    {ok, (Appl#appl.appl_data)#appl_data.maxT};
		registered ->
		    {ok, (Appl#appl.appl_data)#appl_data.regs};
		included_applications ->
		    {ok, Appl#appl.inc_apps};
		applications ->
		    {ok, Appl#appl.apps};
		env ->
		    {ok, get_all_env(AppName)};
		mod ->
		    {ok, (Appl#appl.appl_data)#appl_data.mod};
		start_phases ->
		    {ok, (Appl#appl.appl_data)#appl_data.phases};
		_ -> undefined
	    end;
	_ ->
	    undefined
    end.
	    
get_pid_all_key(Master) ->
    case ets:match(ac_tab, {{application_master, '$1'}, Master}) of
	[[AppName]] -> get_all_key(AppName);
	_ -> []
    end.

get_all_key(AppName) ->
    case ets:lookup(ac_tab, {loaded, AppName}) of
	[{_, Appl}] ->
	    {ok, [{description, Appl#appl.descr},
		  {id, Appl#appl.id},
		  {vsn, Appl#appl.vsn},
		  {modules, (Appl#appl.appl_data)#appl_data.mods},
		  {maxP, (Appl#appl.appl_data)#appl_data.maxP},
		  {maxT, (Appl#appl.appl_data)#appl_data.maxT},
		  {registered, (Appl#appl.appl_data)#appl_data.regs},
		  {included_applications, Appl#appl.inc_apps},
		  {applications, Appl#appl.apps},
		  {env, get_all_env(AppName)},
		  {mod, (Appl#appl.appl_data)#appl_data.mod},
		  {start_phases, (Appl#appl.appl_data)#appl_data.phases}
		 ]};
	_ -> 
	    undefined
    end.


start_type(Master) ->
    case ets:match(ac_tab, {{application_master, '$1'}, Master}) of
	[[AppName]] -> 
	    gen_server:call(?AC, {start_type, AppName}, infinity);
	_X -> 
	    undefined
    end.






get_master(AppName) ->
    case ets:lookup(ac_tab, {application_master, AppName}) of
	[{_, Pid}] -> Pid;
	_ -> undefined
    end.

get_application(Master) ->
    case ets:match(ac_tab, {{application_master, '$1'}, Master}) of
	[[AppName]] -> {ok, AppName};
	_ -> undefined
    end.

get_application_module(Module) ->
    ApplDataPattern = #appl_data{mods='$2', _='_'},
    ApplPattern = #appl{appl_data=ApplDataPattern, _='_'},
    AppModules = ets:match(ac_tab, {{loaded, '$1'}, ApplPattern}),
    get_application_module(Module, AppModules).

get_application_module(Module, [[AppName, Modules]|AppModules]) ->
    case lists:member(Module, Modules) of
	true ->
	    {ok, AppName};
	false ->
	    get_application_module(Module, AppModules)
    end;
get_application_module(_Module, []) ->
    undefined.

permit_application(ApplName, Flag) ->
    gen_server:call(?AC, 
		    {permit_application, ApplName, Flag},
		    infinity).


set_env(AppName, Key, Val) ->
    gen_server:call(?AC, {set_env, AppName, Key, Val, []}).
set_env(AppName, Key, Val, Opts) ->
    Timeout = proplists:get_value(timeout, Opts, 5000),
    gen_server:call(?AC, {set_env, AppName, Key, Val, Opts}, Timeout).

unset_env(AppName, Key) ->
    gen_server:call(?AC, {unset_env, AppName, Key, []}).
unset_env(AppName, Key, Opts) ->
    Timeout = proplists:get_value(timeout, Opts, 5000),
    gen_server:call(?AC, {unset_env, AppName, Key, Opts}, Timeout).

%%%-----------------------------------------------------------------
%%% call-back functions from gen_server
%%%-----------------------------------------------------------------
init(Init, Kernel) ->
    register(?AC, self()),
    process_flag(trap_exit, true),
    put('$ancestors', [Init]), % OTP-5811, for gen_server compatibility
    put('$initial_call', {application_controller, start, 1}),

    case catch check_conf() of
	{ok, ConfData} ->
	    %% Actually, we don't need this info in an ets table anymore.
	    %% This table was introduced because starting applications
	    %% should be able to get som info from AC (e.g. loaded_apps).
	    %% The new implementation makes sure the AC process can be
	    %% called during start-up of any app.
	    case check_conf_data(ConfData) of
		ok ->
		    _ = ets:new(ac_tab, [set, public, named_table,
                                         {read_concurrency,true}]),
		    S = #state{conf_data = ConfData},
		    {ok, KAppl} = make_appl(Kernel),
		    case catch load(S, KAppl) of
			{'EXIT', LoadError} ->
			    Reason = {'load error', LoadError},
			    Init ! {ack, self(), {error, to_string(Reason)}};
                        {error, Error} ->
                            Init ! {ack, self(), {error, to_string(Error)}};
			{ok, NewS} ->
			    Init ! {ack, self(), ok},
			    gen_server:enter_loop(?MODULE, [], NewS,
						  {local, ?AC})
		    end;
		{error, ErrorStr} ->
		    Str = lists:flatten(io_lib:format("invalid config data: ~ts", [ErrorStr])),
		    Init ! {ack, self(), {error, to_string(Str)}}
	    end;
	{error, {File, Line, Str}} ->
	    ReasonStr =
		lists:flatten(io_lib:format("error in config file "
					    "~tp (~w): ~ts",
					    [File, Line, Str])),
	    Init ! {ack, self(), {error, to_string(ReasonStr)}}
    end.


%% Check the syntax of the .config file
%%  [{ApplicationName, [{Parameter, Value}]}].

check_conf_data([]) ->
    ok;
check_conf_data(ConfData) when is_list(ConfData) ->
    [Application | ConfDataRem] = ConfData,
    case Application of
	{kernel, List} when is_list(List) ->
	    case check_para_kernel(List) of
		ok ->
		    check_conf_data(ConfDataRem);
		Error1 ->
		    Error1
	    end;
	{AppName, List} when is_atom(AppName), is_list(List) ->
	    case check_para(List, atom_to_list(AppName)) of
		ok ->
		    check_conf_data(ConfDataRem);
		Error2 ->
		    Error2
	    end;
	{AppName, List} when is_list(List)  ->
	    ErrMsg = "application: "
		++ lists:flatten(io_lib:format("~tp",[AppName]))
		++ "; application name must be an atom",
	    {error, ErrMsg};
	{AppName, _List} ->
	    ErrMsg = "application: "
		++ lists:flatten(io_lib:format("~tp",[AppName]))
		++ "; parameters must be a list",
	    {error, ErrMsg};
	Else ->
	    ErrMsg = "invalid application name: " ++ 
		lists:flatten(io_lib:format(" ~tp",[Else])),
	    {error, ErrMsg}
    end;
check_conf_data(_ConfData) ->
    {error, 'configuration must be a list ended by <dot><whitespace>'}.
    

%% Special check of distributed parameter for kernel
check_para_kernel([]) ->
    ok;
check_para_kernel([{distributed, Apps} | ParaList]) when is_list(Apps) ->
    case check_distributed(Apps) of
	{error, _ErrorMsg} = Error ->
	    Error;
	_ ->
	    check_para_kernel(ParaList)
    end;
check_para_kernel([{distributed, _Apps} | _ParaList]) ->
    {error, "application: kernel; erroneous parameter: distributed"};
check_para_kernel([{Para, _Val} | ParaList]) when is_atom(Para) ->
    check_para_kernel(ParaList);
check_para_kernel([{Para, _Val} | _ParaList]) ->
    {error, "application: kernel; invalid parameter: " ++ 
     lists:flatten(io_lib:format("~tp",[Para]))};
check_para_kernel(Else) ->
    {error, "application: kernel; invalid parameter list: " ++ 
     lists:flatten(io_lib:format("~tp",[Else]))}.
    

check_distributed([]) ->
    ok;
check_distributed([{App, List} | Apps]) when is_atom(App), is_list(List) ->
    check_distributed(Apps);
check_distributed([{App, infinity, List} | Apps]) when is_atom(App), is_list(List) ->
    check_distributed(Apps);
check_distributed([{App, Time, List} | Apps]) when is_atom(App), is_integer(Time), is_list(List) ->
    check_distributed(Apps);
check_distributed(_Else) ->
    {error, "application: kernel; erroneous parameter: distributed"}.


check_para([], _AppName) ->
    ok;
check_para([{Para, _Val} | ParaList], AppName) when is_atom(Para) ->
    check_para(ParaList, AppName);
check_para([{Para, _Val} | _ParaList], AppName) ->
    {error, "application: " ++ AppName ++ "; invalid parameter: " ++ 
     lists:flatten(io_lib:format("~tp",[Para]))};
check_para([Else | _ParaList], AppName) ->
    {error, "application: " ++ AppName ++ "; invalid parameter: " ++ 
     lists:flatten(io_lib:format("~tp",[Else]))}.


-type calls() :: 'info' | 'prep_config_change' | 'which_applications'
               | {'config_change' | 'control_application' |
		  'load_application' | 'start_type' | 'stop_application' |
		  'unload_application', term()}
               | {'change_application_data', _, _}
               | {'permit_application', atom() | {'application',atom(),_},_}
               | {'start_application', _, _}
               | {'unset_env', _, _, _}
               | {'set_env', _, _, _, _}.

-spec handle_call(calls(), {pid(), term()}, state()) ->
        {'noreply', state()} | {'reply', term(), state()}.

handle_call({load_application, Application}, From, S) ->
    case catch do_load_application(Application, S) of
	{ok, NewS} ->
	    AppName = get_appl_name(Application),
	    case cntrl(AppName, S, {ac_load_application_req, AppName}) of
		true ->
		    {noreply, S#state{loading = [{AppName, From} |
						 S#state.loading]}};
		false ->
		    {reply, ok, NewS}
	    end;
	{error, _} = Error ->
	    {reply, Error, S};
	{'EXIT', R} ->
	    {reply, {error, R}, S}
    end;

handle_call({unload_application, AppName}, _From, S) ->
    case lists:keymember(AppName, 1, S#state.running) of
	true -> {reply, {error, {running, AppName}}, S};
	false ->
	    case get_loaded(AppName) of
		{true, _} ->
		    NewS = unload(AppName, S),
		    cntrl(AppName, S, {ac_application_unloaded, AppName}),
		    {reply, ok, NewS};
		false ->
		    {reply, {error, {not_loaded, AppName}}, S}
	    end
    end;

handle_call({start_application, AppName, RestartType}, From, S) ->
    #state{running = Running, starting = Starting, start_p_false = SPF, 
	   started = Started, start_req = Start_req} = S,
    %% Check if the commandline environment variables are OK.
    %% Incase of erroneous variables do not start the application,
    %% if the application is permanent crash the node.
    %% Check if the application is already starting.
    case lists:keyfind(AppName, 1, Start_req) of
	false ->
	    case catch check_start_cond(AppName, RestartType, Started, Running) of
		{ok, Appl} ->
		    Cntrl = cntrl(AppName, S, {ac_start_application_req, AppName}),
		    Perm = application:get_env(kernel, permissions),
		    case {Cntrl, Perm} of
			{true, _} ->
			    {noreply, S#state{starting = [{AppName, RestartType, normal, From} |
							  Starting],
					      start_req = [{AppName, From} | Start_req]}};
			{false, undefined} ->
			    spawn_starter(From, Appl, S, normal),
			    {noreply, S#state{starting = [{AppName, RestartType, normal, From} |
							  Starting],
					      start_req = [{AppName, From} | Start_req]}};
			{false, {ok, Perms}} ->
			    case lists:member({AppName, false}, Perms) of
				false ->
				    spawn_starter(From, Appl, S, normal),
				    {noreply, S#state{starting = [{AppName, RestartType, normal, From} |
								  Starting],
						      start_req = [{AppName, From} | Start_req]}};
				true ->
				    SS = S#state{start_p_false = [{AppName, RestartType, normal, From} |
								  SPF]},
				    {reply, ok, SS}
			    end
		    end;
		{error, _R} = Error ->
		    {reply, Error, S}
	    end;
	{AppName, _FromX} ->
	    SS = S#state{start_req = [{AppName, From} | Start_req]},
	    {noreply, SS}
    end;

handle_call({permit_application, AppName, Bool}, From, S) ->
    Control = S#state.control,
    Starting = S#state.starting,
    SPF = S#state.start_p_false,
    Started = S#state.started,
    Running = S#state.running,
    Start_req = S#state.start_req,
    IsLoaded = get_loaded(AppName),
    IsStarting = lists:keysearch(AppName, 1, Starting),
    IsSPF = lists:keysearch(AppName, 1, SPF),
    IsStarted = lists:keysearch(AppName, 1, Started),
    IsRunning = lists:keysearch(AppName, 1, Running),

    case lists:keymember(AppName, 1, Control) of
	%%========================
	%% distributed application
	%%========================
	true ->
	    case {IsLoaded, IsStarting, IsStarted} of
		%% not loaded
		{false, _, _} ->
		    {reply, {error, {not_loaded, AppName}}, S};
		%% only loaded
		{{true, _Appl}, false, false} ->
		    update_permissions(AppName, Bool),
		    {reply, {distributed_application, only_loaded}, S};
		_ ->
		    update_permissions(AppName, Bool),
		    {reply, distributed_application, S}
	    end;
	%%========================
	%% local application
	%%========================
	false ->
	    case {Bool, IsLoaded, IsStarting, IsSPF, IsStarted, IsRunning} of
		%%------------------------
		%% permit the applicaition
		%%------------------------
		%% already running
		{true, _, _, _, _, {value, _Tuple}} ->
		    {reply, ok, S};
		%% not loaded
		{true, false, _, _, _, _} ->
		    {reply, {error, {not_loaded, AppName}}, S};
		%% only loaded
		{true, {true, _Appl}, false, false, false, false} ->
		    update_permissions(AppName, Bool),
                    {reply, ok, S}; 
		%% starting
		{true, {true, _Appl}, {value, _Tuple}, false, false, false} ->
		    update_permissions(AppName, Bool),
                    {reply, ok, S}; %% check the permission after then app is started
		%% start requested but not started because permit was false
		{true, {true, Appl}, false, {value, Tuple}, false, false} ->
		    update_permissions(AppName, Bool),
		    {_AppName2, RestartType, normal, _From} = Tuple,
		    spawn_starter(From, Appl, S, normal),
		    SS = S#state{starting = [{AppName, RestartType, normal, From} | Starting], 
				 start_p_false = keydelete(AppName, 1, SPF),
				 start_req = [{AppName, From} | Start_req]},
		    {noreply, SS};
		%% started but not running
		{true, {true, Appl}, _, _, {value, {AppName, RestartType}}, false} ->
		    update_permissions(AppName, Bool),
		    spawn_starter(From, Appl, S, normal),
		    SS = S#state{starting = [{AppName, RestartType, normal, From} | Starting], 
				 started = keydelete(AppName, 1, Started),
				 start_req = [{AppName, From} | Start_req]},
		    {noreply, SS};

		%%==========================
		%% unpermit the application
		%%==========================
		%% running
		{false, _, _, _,  _, {value, {_AppName, Id}}} ->
		    {_AppName2, Type} = lists:keyfind(AppName, 1, Started),
		    stop_appl(AppName, Id, Type),
		    NRunning = keydelete(AppName, 1, Running),
		    {reply, ok, S#state{running = NRunning}};
		%% not loaded
		{false, false, _, _, _,  _} ->
		    {reply, {error, {not_loaded, AppName}}, S};
		%% only loaded
		{false, {true, _Appl}, false, false, false, false} ->
		    update_permissions(AppName, Bool),
                    {reply, ok, S}; 
		%% starting
		{false, {true, _Appl}, {value, _Tuple}, false, false, false} ->
		    update_permissions(AppName, Bool),
		    {reply, ok, S};
		%% start requested but not started because permit was false
		{false, {true, _Appl}, false, {value, _Tuple}, false, false} ->
		    update_permissions(AppName, Bool),
		    SS = S#state{start_p_false = keydelete(AppName, 1, SPF)},
		    {reply, ok, SS};
		%% started but not running
		{false, {true, _Appl}, _,  _, {value, _Tuple}, false} ->
		    update_permissions(AppName, Bool),
		    {reply, ok, S}

	    end
    end;

handle_call({stop_application, AppName}, _From, S) ->
    #state{running = Running, started = Started} = S,
    case lists:keyfind(AppName, 1, Running) of
	{_AppName, Id} ->
	    {_AppName2, Type} = lists:keyfind(AppName, 1, Started),
	    stop_appl(AppName, Id, Type),
	    NRunning = keydelete(AppName, 1, Running),
	    NStarted = keydelete(AppName, 1, Started),
	    cntrl(AppName, S, {ac_application_stopped, AppName}),
	    {reply, ok, S#state{running = NRunning, started = NStarted}};
	false ->
	    case lists:keymember(AppName, 1, Started) of
		true ->
		    NStarted = keydelete(AppName, 1, Started),
		    cntrl(AppName, S, {ac_application_stopped, AppName}),
		    {reply, ok, S#state{started = NStarted}};
		false ->
		    {reply, {error, {not_started, AppName}}, S}
	    end
    end;

handle_call({change_application_data, Applications, Config}, _From, S) ->
    OldAppls = ets:filter(ac_tab,
			  fun([{{loaded, _AppName}, Appl}]) ->
				  {true, Appl};
			     (_) ->
				  false
			  end,
			  []),
    case catch do_change_apps(Applications, Config, OldAppls) of
	{error, _} = Error ->
	    {reply, Error, S};
	{'EXIT', R} ->
	    {reply, {error, R}, S};
	{NewAppls, NewConfig} ->
	    lists:foreach(fun(Appl) ->
				  ets:insert(ac_tab, {{loaded, Appl#appl.name},
						      Appl})
			  end, NewAppls),
	    {reply, ok, S#state{conf_data = NewConfig}}
    end;

handle_call(prep_config_change, _From, S) ->
    RunningApps = S#state.running,
    EnvBefore = lists:reverse(do_prep_config_change(RunningApps)),
    {reply, EnvBefore, S};

handle_call({config_change, EnvBefore}, _From, S) ->
    RunningApps = S#state.running,
    R = do_config_change(RunningApps, EnvBefore),
    {reply, R, S};

handle_call(which_applications, _From, S) ->
    Reply = zf(fun({Name, Id}) ->
		       case Id of
			   {distributed, _Node} ->
			       false;
			   _ ->
			       {true, #appl{descr = Descr, vsn = Vsn}} =
				   get_loaded(Name),
			       {true, {Name, Descr, Vsn}}
		       end
	       end, S#state.running),
    {reply, Reply, S};

handle_call({set_env, AppName, Key, Val, Opts}, _From, S) ->
    ets:insert(ac_tab, {{env, AppName, Key}, Val}),
    case proplists:get_value(persistent, Opts, false) of
	true ->
	    Fun = fun(Env) -> lists:keystore(Key, 1, Env, {Key, Val}) end,
	    {reply, ok, S#state{conf_data = change_app_env(S#state.conf_data, AppName, Fun)}};
	false ->
	    {reply, ok, S}
    end;

handle_call({unset_env, AppName, Key, Opts}, _From, S) ->
    ets:delete(ac_tab, {env, AppName, Key}),
    case proplists:get_value(persistent, Opts, false) of
	true ->
	    Fun = fun(Env) -> lists:keydelete(Key, 1, Env) end,
	    {reply, ok, S#state{conf_data = change_app_env(S#state.conf_data, AppName, Fun)}};
	false ->
	    {reply, ok, S}
    end;

handle_call({control_application, AppName}, {Pid, _Tag}, S) ->
    Control = S#state.control,
    case lists:keymember(AppName, 1, Control) of
	false ->
	    link(Pid),
	    {reply, true, S#state{control = [{AppName, Pid} | Control]}};
	true ->
	    {reply, false, S}
    end;

handle_call({start_type, AppName}, _From, S) ->
    Starting = S#state.starting,
    StartType = case lists:keyfind(AppName, 1, Starting) of
		    false ->
			local;
		    {_AppName, _RestartType, Type, _F} ->
			Type
		end,
    {reply, StartType, S};

handle_call(info, _From, S) ->
    Reply = [{loaded, loaded_applications()},
	     {loading, S#state.loading},
	     {started, S#state.started},
	     {start_p_false, S#state.start_p_false},
	     {running, S#state.running},
	     {starting, S#state.starting}],
    {reply, Reply, S}.

-spec handle_cast({'application_started', appname(), _}, state()) ->
        {'noreply', state()} | {'stop', string(), state()}.

handle_cast({application_started, AppName, Res}, S) ->
    handle_application_started(AppName, Res, S).

handle_application_started(AppName, Res, S) ->
    #state{starting = Starting, running = Running, started = Started, 
	   start_req = Start_req} = S,
    Start_reqN = reply_to_requester(AppName, Start_req, Res),
    {_AppName, RestartType, _Type, _From} = lists:keyfind(AppName, 1, Starting),
    case Res of
	{ok, Id} ->
	    case AppName of
		kernel -> check_user();
		_ -> ok
	    end,
	    info_started(AppName, nd(Id)),
	    notify_cntrl_started(AppName, Id, S, ok),
	    NRunning = keyreplaceadd(AppName, 1, Running,{AppName,Id}),
	    NStarted = keyreplaceadd(AppName, 1, Started,{AppName,RestartType}),
	    NewS =  S#state{starting = keydelete(AppName, 1, Starting),
			    running = NRunning,
			    started = NStarted,
			    start_req = Start_reqN},
	    %% The permission may have been changed during start
	    Perm = application:get_env(kernel, permissions),
	    case {Perm, Id} of
		{undefined, _} ->
		    {noreply, NewS};
		%% Check only if the application is started on the own node
		{{ok, Perms}, {distributed, StartNode}} when StartNode =:= node() ->
		    case lists:member({AppName, false}, Perms) of
			true ->
			    #state{running = StopRunning, started = StopStarted} = NewS,
			    case lists:keyfind(AppName, 1, StopRunning) of
				{_AppName, Id} ->
				    {_AppName2, Type} =
					lists:keyfind(AppName, 1, StopStarted),
				    stop_appl(AppName, Id, Type),
				    NStopRunning = keydelete(AppName, 1, StopRunning),
				    cntrl(AppName, NewS, {ac_application_stopped, AppName}),
				    {noreply, NewS#state{running = NStopRunning, 
							started = StopStarted}};
				false ->
				    {noreply, NewS}
			    end;
			false ->
			    {noreply, NewS}
		    end;
		_ ->
		    {noreply, NewS}
	    end;
	{error, R} = Error when RestartType =:= temporary ->
	    notify_cntrl_started(AppName, undefined, S, Error),
	    info_exited(AppName, R, RestartType),
	    {noreply, S#state{starting = keydelete(AppName, 1, Starting),
			      start_req = Start_reqN}};
	{info, R} when RestartType =:= temporary ->
	    notify_cntrl_started(AppName, undefined, S, {error, R}),
	    {noreply, S#state{starting = keydelete(AppName, 1, Starting),
			      start_req = Start_reqN}};
	{ErrInf, R} when RestartType =:= transient, ErrInf =:= error;
			 RestartType =:= transient, ErrInf =:= info ->
	    notify_cntrl_started(AppName, undefined, S, {error, R}),
	    case ErrInf of
		error ->
		    info_exited(AppName, R, RestartType);
		info ->
		    ok
	    end,
	    case R of
		{{'EXIT',normal},_Call} ->
		    {noreply, S#state{starting = keydelete(AppName, 1, Starting),
				      start_req = Start_reqN}};
		_ ->
		    Reason = {application_start_failure, AppName, R},
		    {stop, to_string(Reason), S}
	    end;
	{error, R} = Error -> %% permanent
	    notify_cntrl_started(AppName, undefined, S, Error),
	    info_exited(AppName, R, RestartType),
	    Reason = {application_start_failure, AppName, R},
	    {stop, to_string(Reason), S};
	{info, R} -> %% permanent
	    notify_cntrl_started(AppName, undefined, S, {error, R}),
	    Reason = {application_start_failure, AppName, R},
	    {stop, to_string(Reason), S}
    end.

-spec handle_info(term(), state()) ->
        {'noreply', state()} | {'stop', string(), state()}.

handle_info({ac_load_application_reply, AppName, Res}, S) ->
    case keysearchdelete(AppName, 1, S#state.loading) of
	{value, {_AppName, From}, Loading} ->
	    gen_server:reply(From, Res),
	    case Res of
		ok ->
		    {noreply, S#state{loading = Loading}};
		{error, _R} ->
		    NewS = unload(AppName, S),
		    {noreply, NewS#state{loading = Loading}}
	    end;
	false ->
	    {noreply, S}
    end;

handle_info({ac_start_application_reply, AppName, Res}, S) ->
    Start_req = S#state.start_req,
    case lists:keyfind(AppName, 1, Starting = S#state.starting) of
	{_AppName, RestartType, Type, From} ->
	    case Res of
		start_it ->
		    {true, Appl} = get_loaded(AppName),
		    spawn_starter(From, Appl, S, Type),
		    {noreply, S};
		{started, Node} ->
		    handle_application_started(AppName, 
					       {ok, {distributed, Node}}, 
					       S);
		not_started ->
		    Started = S#state.started,
		    Start_reqN =
			reply_to_requester(AppName, Start_req, ok),
		    {noreply, 
		     S#state{starting = keydelete(AppName, 1, Starting),
			     started = [{AppName, RestartType} | Started],
			     start_req = Start_reqN}};
		{takeover, _Node} = Takeover ->
		    {true, Appl} = get_loaded(AppName),
		    spawn_starter(From, Appl, S, Takeover),
		    NewStarting1 = keydelete(AppName, 1, Starting),
		    NewStarting = [{AppName, RestartType, Takeover, From} | NewStarting1],
		    {noreply, S#state{starting = NewStarting}};
		{error, Reason} = Error when RestartType =:= permanent ->
		    Start_reqN = reply_to_requester(AppName, Start_req, Error),
		    {stop, to_string(Reason), S#state{start_req = Start_reqN}};
		{error, _Reason} = Error ->
		    Start_reqN = reply_to_requester(AppName, Start_req, Error),
		    {noreply, S#state{starting =
					  keydelete(AppName, 1, Starting),
				      start_req = Start_reqN}}
	    end;
	false ->
	    {noreply, S} % someone called stop before control got that
    end;

handle_info({ac_change_application_req, AppName, Msg}, S) ->
    Running = S#state.running,
    Started = S#state.started,
    Starting = S#state.starting,
    case {keyfind(AppName, 1, Running), keyfind(AppName, 1, Started)} of
	{{AppName, Id}, {_AppName2, Type}} ->
	    case Msg of
		{started, Node} ->
		    stop_appl(AppName, Id, Type),
		    NRunning = [{AppName, {distributed, Node}} |
				keydelete(AppName, 1, Running)],
		    {noreply, S#state{running = NRunning}};
		{takeover, _Node, _RT} when is_pid(Id) -> % it is running already
		    notify_cntrl_started(AppName, Id, S, ok),
		    {noreply, S};
		{takeover, Node, RT} ->
		    NewS = do_start(AppName, RT, {takeover, Node}, undefined, S),
		    {noreply, NewS};
		{failover, _Node, _RT} when is_pid(Id) -> % it is running already
		    notify_cntrl_started(AppName, Id, S, ok),
		    {noreply, S};
		{failover, Node, RT} ->
		    case application:get_key(AppName, start_phases) of
			{ok, undefined} ->
			    %% to be backwards compatible the application
			    %% is not started as failover if start_phases  
			    %% is not defined in the .app file
			    NewS = do_start(AppName, RT, normal, undefined, S),
			    {noreply, NewS};
			{ok, _StartPhases} ->
			    NewS = do_start(AppName, RT, {failover, Node}, undefined, S),
			    {noreply, NewS}
		    end;
		stop_it ->
		    stop_appl(AppName, Id, Type),
		    cntrl(AppName, S, {ac_application_not_run, AppName}),
		    NRunning = keyreplace(AppName, 1, Running, 
					 {AppName, {distributed, []}}),
		    {noreply, S#state{running = NRunning}};
		%% We should not try to start a running application!
		start_it when is_pid(Id) ->
		    notify_cntrl_started(AppName, Id, S, ok),
		    {noreply, S};
		start_it ->
		    NewS = do_start(AppName, undefined, normal, undefined, S),
		    {noreply, NewS};
		not_running ->
		    NRunning = keydelete(AppName, 1, Running),
		    {noreply, S#state{running = NRunning}};
		_ ->
		    {noreply, S}
	    end;
	_ ->
	    IsLoaded = get_loaded(AppName),
	    IsStarting = lists:keysearch(AppName, 1, Starting),
	    IsStarted = lists:keysearch(AppName, 1, Started),
	    IsRunning = lists:keysearch(AppName, 1, Running),

	    case Msg of
		start_it ->
		    case {IsLoaded, IsStarting, IsStarted, IsRunning} of
			%% already running
			{_, _, _, {value, _Tuple}} ->
			    {noreply, S};
			%% not loaded
			{false, _, _, _} ->
			    {noreply, S};
			%% only loaded
			{{true, _Appl}, false, false, false} ->
			    {noreply, S};
			%% starting
			{{true, _Appl}, {value, Tuple}, false, false} ->
			    {_AppName, _RStype, _Type, From} = Tuple,
			    NewS = do_start(AppName, undefined, normal, From, S),
			    {noreply, NewS};
			%% started but not running
			{{true, _Appl}, _, {value, {AppName, _RestartType}}, false} ->
			    NewS = do_start(AppName, undefined, normal, undefined, S),
			    SS = NewS#state{started = keydelete(AppName, 1, Started)},
			    {noreply, SS}
		    end;
		{started, Node} ->
		    NRunning = [{AppName, {distributed, Node}} |
				keydelete(AppName, 1, Running)],
		    {noreply, S#state{running = NRunning}};
		_ ->
		    {noreply, S} % someone called stop before control got that
	    end
    end;

%%-----------------------------------------------------------------
%% An application died.  Check its restart_type.  Maybe terminate
%% all other applications.
%%-----------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, S) ->
    ets:match_delete(ac_tab, {{application_master, '_'}, Pid}),
    NRunning = keydelete(Pid, 2, S#state.running),
    NewS = S#state{running = NRunning},
    case lists:keyfind(Pid, 2, S#state.running) of
	{AppName, _AmPid} ->
	    cntrl(AppName, S, {ac_application_stopped, AppName}),
	    case lists:keyfind(AppName, 1, S#state.started) of
		{_AppName, temporary} ->
		    info_exited(AppName, Reason, temporary),
		    {noreply, NewS};
		{_AppName, transient} when Reason =:= normal ->
		    info_exited(AppName, Reason, transient),
		    {noreply, NewS};
		{_AppName, Type} ->
		    info_exited(AppName, Reason, Type),
		    {stop, to_string({application_terminated, AppName, Reason}), NewS}
	    end;
	false ->
	    {noreply, S#state{control = del_cntrl(S#state.control, Pid)}}
    end;
    
handle_info(_, S) ->
    {noreply, S}.

-spec terminate(term(), state()) -> 'ok'.

terminate(Reason, S) ->
    case application:get_env(kernel, shutdown_func) of
	{ok, {M, F}} ->
	    catch M:F(Reason);
	_ ->
	    ok
    end,
    ShutdownTimeout =
	case application:get_env(kernel, shutdown_timeout) of
	    undefined -> infinity;
	    {ok,T} -> T
	end,
    foreach(fun({_AppName, Id}) when is_pid(Id) -> 
		    Ref = erlang:monitor(process, Id),
		    unlink(Id),
		    exit(Id, shutdown),
		    receive
			%% Proc died before link
			{'EXIT', Id, _} -> ok
		    after 0 ->
			    receive
				{'DOWN', Ref, process, Id, _} -> ok
			    after ShutdownTimeout ->
				    exit(Id, kill),
				    receive
					{'DOWN', Ref, process, Id, _} -> ok
				    end
			    end
		    end;
	       (_) -> ok
	    end,
	    S#state.running),
    true = ets:delete(ac_tab),
    ok.

-spec code_change(term(), state(), term()) -> {'ok', state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------
cntrl(AppName, #state{control = Control}, Msg) ->
    case lists:keyfind(AppName, 1, Control) of
	{_AppName, Pid} ->
	    Pid ! Msg,
	    true;
	false -> 
	    false
    end.

notify_cntrl_started(_AppName, {distributed, _Node}, _S, _Res) ->
    ok;
notify_cntrl_started(AppName, _Id, S, Res) ->
    cntrl(AppName, S, {ac_application_run, AppName, Res}).
    
del_cntrl([{_, Pid}|T], Pid) ->
    del_cntrl(T, Pid);
del_cntrl([H|T], Pid) ->
    [H|del_cntrl(T, Pid)];
del_cntrl([], _Pid) ->
    [].

get_loaded(App) ->
    AppName = get_appl_name(App),
    case ets:lookup(ac_tab, {loaded, AppName}) of 
	[{_Key, Appl}] -> {true, Appl};
	_  -> false
    end.
    
do_load_application(Application, S) ->
    case get_loaded(Application) of
	{true, _} ->
	    throw({error, {already_loaded, Application}});
	false ->
	    case make_appl(Application) of
		{ok, Appl} -> load(S, Appl);
		Error -> Error
	    end
    end.

%% Recursively load the application and its included apps.
%load(S, {ApplData, ApplEnv, IncApps, Descr, Vsn, Apps}) ->
load(S, {ApplData, ApplEnv, IncApps, Descr, Id, Vsn, Apps}) ->
    Name = ApplData#appl_data.name,
    ConfEnv = get_env_i(Name, S),
    NewEnv = merge_app_env(ApplEnv, ConfEnv),
    CmdLineEnv = get_cmd_env(Name),
    NewEnv2 = merge_app_env(NewEnv, CmdLineEnv),
    NewEnv3 = keyreplaceadd(included_applications, 1, NewEnv2,
			    {included_applications, IncApps}),
    add_env(Name, NewEnv3),
    Appl = #appl{name = Name, descr = Descr, id = Id, vsn = Vsn, 
		 appl_data = ApplData, inc_apps = IncApps, apps = Apps},
    ets:insert(ac_tab, {{loaded, Name}, Appl}),
    NewS =
	foldl(fun(App, S1) ->
		      case get_loaded(App) of
			  {true, _} -> S1;
			  false ->
			      case do_load_application(App, S1) of
				  {ok, S2} -> S2;
				  Error -> throw(Error)
			      end
		      end
	      end, S, IncApps),
    {ok, NewS}.

unload(AppName, S) ->
    {ok, IncApps} = get_env(AppName, included_applications),
    del_env(AppName),
    ets:delete(ac_tab, {loaded, AppName}),
    foldl(fun(App, S1) ->
		  case get_loaded(App) of
		      false -> S1;
		      {true, _} -> unload(App, S1)
		  end
	  end, S, IncApps).

check_start_cond(AppName, RestartType, Started, Running) ->
    validRestartType(RestartType),
    case get_loaded(AppName) of
	{true, Appl} ->
	    %% Check Running; not Started.  An exited app is not running,
	    %% but started.  It must be possible to start an exited app!
	    case lists:keymember(AppName, 1, Running) of
		true ->
		    {error, {already_started, AppName}};
		false ->
		    foreach(
		      fun(AppName2) ->
			      case lists:keymember(AppName2, 1, Started) of
				  true -> ok;
				  false ->
				      throw({error, {not_started, AppName2}})
			      end
		      end, Appl#appl.apps),
		    {ok, Appl}
	    end;
	false ->
	    {error, {not_loaded, AppName}}
    end.

do_start(AppName, RT, Type, From, S) ->
    RestartType = case lists:keyfind(AppName, 1, S#state.started) of
		      {_AppName2, OldRT} ->
			  get_restart_type(RT, OldRT);
		      false ->
			  RT
		  end,
    %% UW 990913: We check start_req instead of starting, because starting 
    %% has already been checked.
    case lists:keymember(AppName, 1, S#state.start_req) of
	false ->
	    {true, Appl} = get_loaded(AppName),
	    Start_req = S#state.start_req,
	    spawn_starter(undefined, Appl, S, Type),
	    Starting = case lists:keymember(AppName, 1, S#state.starting) of
			   false ->
			       %% UW: don't know if this is necessary
			       [{AppName, RestartType, Type, From} | 
				S#state.starting];
			   true ->
			       S#state.starting
		       end,
	    S#state{starting = Starting, 
		    start_req = [{AppName, From} | Start_req]};
	true -> % otherwise we're already starting the app...
	    S
    end.
    
spawn_starter(From, Appl, S, Type) ->
    spawn_link(?MODULE, init_starter, [From, Appl, S, Type]).

init_starter(_From, Appl, S, Type) ->
    process_flag(trap_exit, true),
    AppName = Appl#appl.name,
    gen_server:cast(?AC, {application_started, AppName, 
			  catch start_appl(Appl, S, Type)}).

reply(undefined, _Reply) -> 
    ok;
reply(From, Reply) -> gen_server:reply(From, Reply).

start_appl(Appl, S, Type) ->
    ApplData = Appl#appl.appl_data,
    case ApplData#appl_data.mod of
	[] ->
	    {ok, undefined};
	_ ->
	    %% Name = ApplData#appl_data.name,
	    Running = S#state.running,
	    foreach(
	      fun(AppName) ->
		      case lists:keymember(AppName, 1, Running) of
			  true ->
			      ok;
			  false ->
			      throw({info, {not_running, AppName}})
		      end
	      end, Appl#appl.apps),
	    case application_master:start_link(ApplData, Type) of
		{ok, _Pid} = Ok ->
		    Ok;
		{error, _Reason} = Error ->
		    throw(Error)
	    end
    end.

    
%%-----------------------------------------------------------------
%% Stop application locally.
%%-----------------------------------------------------------------
stop_appl(AppName, Id, Type) when is_pid(Id) ->
    unlink(Id),
    application_master:stop(Id),
    info_exited(AppName, stopped, Type),
    ets:delete(ac_tab, {application_master, AppName});
stop_appl(AppName, undefined, Type) ->
    %% Code-only application stopped
    info_exited(AppName, stopped, Type);
stop_appl(_AppName, _Id, _Type) ->
    %% Distributed application stopped
    ok. 

keysearchdelete(Key, Pos, List) ->
    ksd(Key, Pos, List, []).

ksd(Key, Pos, [H | T], Rest) when element(Pos, H) =:= Key ->
    {value, H, Rest ++ T};
ksd(Key, Pos, [H | T], Rest) ->
    ksd(Key, Pos, T, [H | Rest]);
ksd(_Key, _Pos, [], _Rest) ->
    false.
    
keyreplaceadd(Key, Pos, List, New) ->
    %% Maintains the order!
    case lists:keymember(Key, Pos, List) of
	true -> keyreplace(Key, Pos, List, New);
	false -> [New | List]
    end.

validRestartType(permanent)   -> true;
validRestartType(temporary)   -> true;
validRestartType(transient)   -> true;
validRestartType(RestartType) ->
    throw({error, {invalid_restart_type, RestartType}}).

nd({distributed, Node}) -> Node;
nd(_) -> node().
	  
get_restart_type(undefined, OldRT) ->
    OldRT;
get_restart_type(RT, _OldRT) ->
    RT.

get_appl_name(Name) when is_atom(Name) -> Name;
get_appl_name({application, Name, _}) when is_atom(Name) -> Name;
get_appl_name(Appl) -> throw({error, {bad_application, Appl}}).

make_appl(Name) when is_atom(Name) ->
    FName = atom_to_list(Name) ++ ".app",
    case code:where_is_file(FName) of
	non_existing ->
	    {error, {file:format_error(enoent), FName}};
	FullName ->
	    case prim_consult(FullName) of
		{ok, [Application]} ->
		    {ok, make_appl_i(Application)};
		{error, Reason} -> 
		    {error, {file:format_error(Reason), FName}};
                error ->
                    {error, "bad encoding"}
	    end
    end;
make_appl(Application) ->
    {ok, make_appl_i(Application)}.

prim_consult(FullName) ->
    case erl_prim_loader:get_file(FullName) of
	{ok, Bin, _} ->
            case file_binary_to_list(Bin) of
                {ok, String} ->
                    case erl_scan:string(String) of
                        {ok, Tokens, _EndLine} ->
                            prim_parse(Tokens, []);
                        {error, Reason, _EndLine} ->
                            {error, Reason}
                    end;
                error ->
                    error
            end;
	error ->
	    {error, enoent}
    end.

prim_parse(Tokens, Acc) ->
    case lists:splitwith(fun(T) -> element(1,T) =/= dot end, Tokens) of
	{[], []} ->
	    {ok, lists:reverse(Acc)};
	{Tokens2, [{dot,_} = Dot | Rest]} ->
	    case erl_parse:parse_term(Tokens2 ++ [Dot]) of
		{ok, Term} ->
		    prim_parse(Rest, [Term | Acc]);
		{error, _R} = Error ->
		    Error
	    end;
	{Tokens2, []} ->
	    case erl_parse:parse_term(Tokens2) of
		{ok, Term} ->
		    {ok, lists:reverse([Term | Acc])};
		{error, _R} = Error ->
		    Error
	    end
    end.

make_appl_i({application, Name, Opts}) when is_atom(Name), is_list(Opts) ->
    Descr = get_opt(description, Opts, ""),
    Id = get_opt(id, Opts, ""),
    Vsn = get_opt(vsn, Opts, ""),
    Mods = get_opt(modules, Opts, []),
    Regs = get_opt(registered, Opts, []),
    Apps = get_opt(applications, Opts, []),
    Mod =
	case get_opt(mod, Opts, []) of
	    {M,_A}=MA when is_atom(M) -> MA;
	    [] -> [];
	    Other -> throw({error, {badstartspec, Other}})
	end,
    Phases = get_opt(start_phases, Opts, undefined),
    Env = get_opt(env, Opts, []),
    MaxP = get_opt(maxP, Opts, infinity),
    MaxT = get_opt(maxT, Opts, infinity),
    IncApps = get_opt(included_applications, Opts, []),
    {#appl_data{name = Name, regs = Regs, mod = Mod, phases = Phases,
		mods = Mods, inc_apps = IncApps, maxP = MaxP, maxT = MaxT},
     Env, IncApps, Descr, Id, Vsn, Apps};
make_appl_i({application, Name, Opts}) when is_list(Opts) ->
    throw({error,{invalid_name,Name}});
make_appl_i({application, _Name, Opts}) ->
    throw({error,{invalid_options, Opts}});
make_appl_i(Appl) -> throw({error, {bad_application, Appl}}).


%%-----------------------------------------------------------------
%% Merge current applications with changes.  
%%-----------------------------------------------------------------

%% do_change_apps(Applications, Config, OldAppls) -> NewAppls
%%   Applications = [{application, AppName, [{Key,Value}]}]
%%   Config = [{AppName,[{Par,Value}]} | File]
%%   OldAppls = NewAppls = [#appl{}]
do_change_apps(Applications, Config, OldAppls) ->

    %% OTP-4867
    %% Config = contents of sys.config file
    %% May now contain names of other .config files as well as
    %% configuration parameters.
    %% Therefore read and merge contents.
    {ok, SysConfig, Errors} = check_conf_sys(Config),

    %% Report errors, but do not terminate
    %% (backwards compatible behaviour)
    lists:foreach(fun({error, {SysFName, Line, Str}}) ->
			  Str2 = lists:flatten(io_lib:format("~tp: ~w: ~ts~n",
							     [SysFName, Line, Str])),
			  error_logger:format(Str2, [])
		  end,
		  Errors),

    {map(fun(Appl) ->
		 AppName = Appl#appl.name,
		 case is_loaded_app(AppName, Applications) of
		     {true, Application} ->
			 do_change_appl(make_appl(Application),
					Appl, SysConfig);

		     %% ignored removed apps - handled elsewhere
		     false ->
			 Appl
		 end
	 end, OldAppls),
     SysConfig}.

is_loaded_app(AppName, [{application, AppName, App} | _]) ->
    {true, {application, AppName, App}};
is_loaded_app(AppName, [_ | T]) -> is_loaded_app(AppName, T);
is_loaded_app(_AppName, []) -> false.

do_change_appl({ok, {ApplData, Env, IncApps, Descr, Id, Vsn, Apps}},
	       OldAppl, Config) ->
    AppName = OldAppl#appl.name,

    %% Merge application env with env from sys.config, if any
    ConfEnv = get_opt(AppName, Config, []),
    NewEnv1 = merge_app_env(Env, ConfEnv),

    %% Merge application env with command line arguments, if any
    CmdLineEnv = get_cmd_env(AppName),
    NewEnv2 = merge_app_env(NewEnv1, CmdLineEnv),

    %% included_apps is made into an env parameter as well
    NewEnv3 = keyreplaceadd(included_applications, 1, NewEnv2,
			    {included_applications, IncApps}),

    %% Update ets table with new application env
    del_env(AppName),
    add_env(AppName, NewEnv3),

    OldAppl#appl{appl_data=ApplData,
		 descr=Descr,
		 id=Id,
		 vsn=Vsn,
		 inc_apps=IncApps,
		 apps=Apps};
do_change_appl({error, _R} = Error, _Appl, _ConfData) ->
    throw(Error).

get_opt(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
	{_Key, Val} -> Val;
	_ -> Default
    end.

get_cmd_env(Name) ->
    case init:get_argument(Name) of
	{ok, Args} ->
	   foldl(fun(List, Res) -> conv(List) ++ Res end, [], Args);
	_ -> []
    end.

conv([Key, Val | T]) ->
    [{make_term(Key), make_term(Val)} | conv(T)];
conv(_) -> [].

make_term(Str) -> 
    case erl_scan:string(Str) of
	{ok, Tokens, _} ->		  
	    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
		{ok, Term} ->
		    Term;
		{error, {_,M,Reason}} ->
                    handle_make_term_error(M, Reason, Str)
	    end;
	{error, {_,M,Reason}, _} ->
            handle_make_term_error(M, Reason, Str)
    end.

handle_make_term_error(Mod, Reason, Str) ->
    error_logger:format("application_controller: ~ts: ~ts~n",
        [Mod:format_error(Reason), Str]),
    throw({error, {bad_environment_value, Str}}).

get_env_i(Name, #state{conf_data = ConfData}) when is_list(ConfData) ->
    case lists:keyfind(Name, 1, ConfData) of
	{_Name, Env} -> Env;
	_ -> []
    end;
get_env_i(_Name, _) -> [].

%% Merges envs for all apps.  Env2 overrides Env1
merge_env(Env1, Env2) ->
    merge_env(Env1, Env2, []).

merge_env([{App, AppEnv1} | T], Env2, Res) ->
    case get_env_key(App, Env2) of
	{value, AppEnv2, RestEnv2} ->
	    NewAppEnv = merge_app_env(AppEnv1, AppEnv2),
	    merge_env(T, RestEnv2, [{App, NewAppEnv} | Res]);
	_ ->
	    merge_env(T, Env2, [{App, AppEnv1} | Res])
    end;
merge_env([], Env2, Res) ->
    Env2 ++ Res.

%% Changes the environment for the given application
%% If there is no application, an empty one is created
change_app_env(Env, App, Fun) ->
    case get_env_key(App, Env) of
	{value, AppEnv, RestEnv} ->
	    [{App, Fun(AppEnv)} | RestEnv];
	_ ->
	    [{App, Fun([])} | Env]
    end.

%% Merges envs for an application.  Env2 overrides Env1
merge_app_env(Env1, Env2) ->
    merge_app_env(Env1, Env2, []).

merge_app_env([{Key, Val} | T], Env2, Res) ->
    case get_env_key(Key, Env2) of
	{value, NewVal, RestEnv} ->
	    merge_app_env(T, RestEnv, [{Key, NewVal}|Res]);
	_ ->
	    merge_app_env(T, Env2, [{Key, Val} | Res])
    end;
merge_app_env([], Env2, Res) ->
    Env2 ++ Res.

get_env_key(Key, Env) -> get_env_key(Env, Key, []).
get_env_key([{Key, Val} | T], Key, Res) ->
    {value, Val, T ++ Res};
get_env_key([H | T], Key, Res) ->
    get_env_key(T, Key, [H | Res]);
get_env_key([], _Key, Res) -> Res.

add_env(Name, Env) ->
    foreach(fun({Key, Value}) ->
			  ets:insert(ac_tab, {{env, Name, Key}, Value})
		  end,
		  Env).

del_env(Name) ->
    ets:match_delete(ac_tab, {{env, Name, '_'}, '_'}).

check_user() ->
    case whereis(user) of
	User when is_pid(User) -> group_leader(User, self());
	_ -> ok
    end.


%%-----------------------------------------------------------------
%% Prepare for a release upgrade by reading all the evironment variables.
%%-----------------------------------------------------------------
do_prep_config_change(Apps) ->
    do_prep_config_change(Apps, []).

do_prep_config_change([], EnvBefore) ->
    EnvBefore;
do_prep_config_change([{App, _Id} | Apps], EnvBefore) ->
    Env = application:get_all_env(App),
    do_prep_config_change(Apps, [{App, Env} | EnvBefore]).
    


%%-----------------------------------------------------------------
%% Inform all running applications about the changed configuration.
%%-----------------------------------------------------------------
do_config_change(Apps, EnvBefore) ->
    do_config_change(Apps, EnvBefore, []).

do_config_change([], _EnvBefore, []) ->
    ok;
do_config_change([], _EnvBefore, Errors) ->
    {error, Errors};
do_config_change([{App, _Id} | Apps], EnvBefore, Errors) ->
    AppEnvNow = lists:sort(application:get_all_env(App)),
    AppEnvBefore = case lists:keyfind(App, 1, EnvBefore) of
		       false ->
			   [];
		       {App, AppEnvBeforeT} ->
			   lists:sort(AppEnvBeforeT)
		   end,
    Res = 
	case AppEnvNow of
	    AppEnvBefore ->
		ok;
	    _ ->
		case do_config_diff(AppEnvNow, AppEnvBefore) of
		    {[], [], []} ->
			ok;
		    {Changed, New, Removed} ->
			case application:get_key(App, mod) of
			    {ok, {Mod, _Para}} ->
				case catch Mod:config_change(Changed, New, 
							     Removed) of
				    ok ->
					ok;
				    %% It is not considered as an error
				    %% if the cb-function is not defined 
				    {'EXIT', {undef, _}} ->
					ok;
				    {error, _} = Error ->
					Error;
				    Else ->
					{error, Else}
				end;
			    {ok, []} ->
				{error, {module_not_defined, App}};
			    undefined ->
				{error, {application_not_found, App}}
			end
		end
	end,
    
    case Res of
	ok ->
	    do_config_change(Apps, EnvBefore, Errors);
	{error, NewError} ->
	    do_config_change(Apps, EnvBefore,[NewError | Errors])
    end.


%%-----------------------------------------------------------------
%% Check if the configuration is changed in anyway.
%%-----------------------------------------------------------------
do_config_diff(AppEnvNow, AppEnvBefore) ->
    do_config_diff(AppEnvNow, AppEnvBefore, {[], []}).

do_config_diff([], AppEnvBefore, {Changed, New}) ->
    Removed = lists:foldl(fun({Env, _Value}, Acc) -> [Env | Acc] end, [], AppEnvBefore),
    {Changed, New, Removed};
do_config_diff(AppEnvNow, [], {Changed, New}) ->
    {Changed, AppEnvNow++New, []};
do_config_diff([{Env, Value} | AppEnvNow], AppEnvBefore, {Changed, New}) ->
    case lists:keyfind(Env, 1, AppEnvBefore) of
	{Env, Value} ->
	    do_config_diff(AppEnvNow, lists:keydelete(Env,1,AppEnvBefore), {Changed, New});
	{Env, _OtherValue} ->
	    do_config_diff(AppEnvNow, lists:keydelete(Env,1,AppEnvBefore), 
			   {[{Env, Value} | Changed], New});
	false ->
	    do_config_diff(AppEnvNow, AppEnvBefore, {Changed, [{Env, Value}|New]})
    end.


%%-----------------------------------------------------------------
%% Read the .config files.
%%-----------------------------------------------------------------
check_conf() ->
    case init:get_argument(config) of
	{ok, Files} ->
	    {ok, lists:foldl(
		   fun([File], Env) ->
			   BFName = filename:basename(File,".config"),
			   FName = filename:join(filename:dirname(File),
						 BFName ++ ".config"),
			   case load_file(FName) of
			       {ok, NewEnv} ->
				   %% OTP-4867
				   %% sys.config may now contain names of
				   %% other .config files as well as
				   %% configuration parameters.
				   %% Therefore read and merge contents.
				   if
				       BFName =:= "sys" ->
					   {ok, SysEnv, Errors} =
					       check_conf_sys(NewEnv),

					   %% Report first error, if any, and
					   %% terminate
					   %% (backwards compatible behaviour)
					   case Errors of
					       [] ->
						   merge_env(Env, SysEnv);
					       [{error, {SysFName, Line, Str}}|_] ->
						   throw({error, {SysFName, Line, Str}})
					   end;
				       true ->
					   merge_env(Env, NewEnv)
				   end;
			       {error, {Line, _Mod, Str}} ->
				   throw({error, {FName, Line, Str}})
			   end
		   end, [], Files)};
	_ -> {ok, []}
    end.

check_conf_sys(Env) ->
    check_conf_sys(Env, [], []).

check_conf_sys([File|T], SysEnv, Errors) when is_list(File) ->
    BFName = filename:basename(File, ".config"),
    FName = filename:join(filename:dirname(File), BFName ++ ".config"),
    case load_file(FName) of
	{ok, NewEnv} ->
	    check_conf_sys(T, merge_env(SysEnv, NewEnv), Errors);
	{error, {Line, _Mod, Str}} ->
	    check_conf_sys(T, SysEnv, [{error, {FName, Line, Str}}|Errors])
    end;
check_conf_sys([Tuple|T], SysEnv, Errors) ->
    check_conf_sys(T, merge_env(SysEnv, [Tuple]), Errors);
check_conf_sys([], SysEnv, Errors) ->
    {ok, SysEnv, lists:reverse(Errors)}.

load_file(File) ->
    %% We can't use file:consult/1 here. Too bad.
    case erl_prim_loader:get_file(File) of
	{ok, Bin, _FileName} ->
	    %% Make sure that there is some whitespace at the end of the string
	    %% (so that reading a file with no NL following the "." will work).
            case file_binary_to_list(Bin) of
                {ok, String} ->
                    scan_file(String ++ " ");
                error ->
                    {error, {none, scan_file, "bad encoding"}}
            end;
	error ->
	    {error, {none, open_file, "configuration file not found"}}
    end.

scan_file(Str) ->
    case erl_scan:tokens([], Str, 1) of
	{done, {ok, Tokens, _}, Left} ->
	    case erl_parse:parse_term(Tokens) of
		{ok,L}=Res when is_list(L) ->
		    case only_ws(Left) of
			true ->
			    Res;
			false ->
			    %% There was trailing garbage found after the list.
			    config_error()
		    end;
		{ok,_} ->
		    %% Parsing succeeded but the result is not a list.
		    config_error();
		Error ->
		    Error
	    end;
	{done, Result, _} ->
	    {error, {none, parse_file, tuple_to_list(Result)}};
	{more, _} ->
	    {error, {none, load_file, "no ending <dot> found"}}
    end.

only_ws([C|Cs]) when C =< $\s -> only_ws(Cs);
only_ws([$%|Cs]) -> only_ws(strip_comment(Cs));   % handle comment
only_ws([_|_]) -> false;
only_ws([]) -> true.
    
strip_comment([$\n|Cs]) -> Cs;
strip_comment([_|Cs]) -> strip_comment(Cs);
strip_comment([]) -> [].

config_error() ->
    {error,
     {none, load_file,
      "configuration file must contain ONE list ended by <dot>"}}.

%%-----------------------------------------------------------------
%% Info messages sent to error_logger
%%-----------------------------------------------------------------
info_started(Name, Node) ->
    Rep = [{application, Name},
	   {started_at, Node}],
    error_logger:info_report(progress, Rep).

info_exited(Name, Reason, Type) ->
    Rep = [{application, Name},
	   {exited, Reason},
	   {type, Type}],
    error_logger:info_report(Rep).


%%-----------------------------------------------------------------
%% Reply to all processes waiting this application to be started.  
%%-----------------------------------------------------------------
reply_to_requester(AppName, Start_req, Res) ->
    R = case Res of
	    {ok, _Id} ->
		ok;
	    {info, Reason} ->
		{error, Reason};
	    Error ->
		Error
	end,

    lists:foldl(fun(Sp, AccIn) ->
			case Sp of
			    {AppName, From} ->
				reply(From, R),
				AccIn;
			    _ ->
				[Sp | AccIn]
			end
		end,
		[],
		Start_req).
    

%%-----------------------------------------------------------------
%% Update the environment variable permission for an application.  
%%-----------------------------------------------------------------
update_permissions(AppName, Bool) ->
    T = {env, kernel, permissions},
    case ets:lookup(ac_tab, T) of
	[] ->
	    ets:insert(ac_tab, {T, [{AppName, Bool}]});
	[{_, Perm}] ->
	    Perm2 = lists:keydelete(AppName, 1, Perm),
	    ets:insert(ac_tab, {T, [{AppName, Bool}|Perm2]})
    end.

%%-----------------------------------------------------------------
%% These functions are only to be used from testsuites.  
%%-----------------------------------------------------------------
test_change_apps(Apps, Conf) ->
    Res = test_make_apps(Apps, []),
    test_do_change_appl(Apps, Conf, Res).

test_do_change_appl([], _, _) ->
    ok;
test_do_change_appl([A|Apps], [], [R|Res]) ->
    _ = do_change_appl(R, #appl{name = A}, []),
    test_do_change_appl(Apps, [], Res);
test_do_change_appl([A|Apps], [C|Conf], [R|Res]) ->
    _ = do_change_appl(R, #appl{name = A}, C),
    test_do_change_appl(Apps, Conf, Res).

test_make_apps([], Res) ->
    lists:reverse(Res);
test_make_apps([A|Apps], Res) ->
    test_make_apps(Apps, [make_appl(A) | Res]).

file_binary_to_list(Bin) ->
    Enc = case epp:read_encoding_from_binary(Bin) of
              none -> epp:default_encoding();
              Encoding -> Encoding
          end,
    case catch unicode:characters_to_list(Bin, Enc) of
        String when is_list(String) ->
            {ok, String};
        _ ->
            error
    end.

%%-----------------------------------------------------------------
%% String conversion
%% Exit reason needs to be a printable string
%% (and of length <200, but init now does the chopping).
%%-----------------------------------------------------------------

-spec to_string(term()) -> string().

to_string(Term) ->
    case io_lib:printable_list(Term) of
	true ->
	    Term;
	false ->
	    lists:flatten(io_lib:format("~134217728p", [Term]))
    end.

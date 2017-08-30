%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(dist_ac).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
	 load_application/2,
	 takeover_application/2,
	 permit_application/2,
	 permit_only_loaded_application/2]).

-export([get_known_nodes/0]).

%% Internal exports
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
	 code_change/3, send_timeout/3]).
-export([info/0]).

-import(lists, [zf/2, filter/2, map/2, foreach/2, foldl/3, mapfoldl/3,
		keysearch/3, keydelete/3, keyreplace/4, member/2]).

-define(AC, application_controller).
-define(DIST_AC, ?MODULE).
-define(LOCK_ID, ?MODULE).

%% This is the protocol version for the dist_ac protcol (between nodes)
-define(vsn, 1).

%%%-----------------------------------------------------------------
%%% This module implements the default Distributed Applications
%%% Controller.  It is possible to write other controllers, when
%%% the functionality in this module are not sufficient.
%%% The process cooperates with the application_controller.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Naming conventions:
%%   Appl = #appl
%%   AppName = atom()
%%-----------------------------------------------------------------
-record(state, {appls = [], tmp_locals = [], remote_started = [],
		known = [], started = [], tmp_weights = [],
		dist_loaded = [], t_reqs = [], s_reqs = [], p_reqs = []}).
%%-----------------------------------------------------------------
%% appls           = [#appl()] - these are the applications we control
%% tmp_locals      = [{AppName, Weight, node()}] - tmp, info part of
%%                   application startup for some distrib appls,
%%                   not yet handled.
%% remote_started  = [{Node, AppName}] - info on apps started before
%%                   we were started
%% known           = [Node] - These are the nodes known to us
%% started         = [AppName] - An ordered list of started applications
%%                   (reversed start order)
%% tmp_weight      = [{AppName, MyWeight}] - tmp, if we're forced to
%%                   send a dist_ac_weight message before we're prepared to,
%%                   we remember the weight we sent here, so we can use
%%                   it in the dist_ac_weight msgs later.
%% dist_loaded     = {{Name, Node}, HisNodes, Permission} - info on
%%                   application loaded on other nodes (and own node)
%% t_reqs          = [{AppName, From}] - processes waiting for takeover
%%                   to complete.
%% s_reqs          = [{AppName, From}] - processes waiting for stop
%%                   to complete.
%% p_reqs          = [{From, AppName, Bool, [Node]] - outstanding permit.
%%                   Nodes is a list of nodes we're still waiting for.
%%-----------------------------------------------------------------

-record(appl, {name, id, restart_time = 0,  nodes = [], run = []}).

%%-----------------------------------------------------------------
%% id = local | undefined | {distributed, node()} | waiting | run_waiting |
%%      {failover, Node} | {takeover, Node}
%%      local : local application
%%      undefined : not yet started
%%      {distributed, Node} : running on another node, we're standby
%%      {failover, Node} : failover from Node
%%      {takeover, Node} : takeover from Node
%%      waiting : other node went down, we're waiting for a timeout
%%                to takeover it.  From = pid() | undefined
%%      run_waiting : we have decided to start the app; wait for the
%%                    AC result
%%-----------------------------------------------------------------

start_link() ->
    case gen_server:start_link({local, ?DIST_AC}, ?MODULE, [], []) of
	{ok, Pid} ->
	    gen_server:cast(?DIST_AC, init_sync),
	    {ok, Pid};
	Else ->
	    Else
    end.
    

%%-----------------------------------------------------------------
%% Func: load_application(AppName, DistNodes)
%% Args: AppName = atom()
%%       DistNodes = default | {AppName, Time, [node() | {node()...}]}
%% Purpose: Notifies the dist_ac about distributed nodes for an
%%          application.  DistNodes overrides the kernel 'distributed'
%%          parameter.
%% Returns: ok | {error, Reason}
%%-----------------------------------------------------------------
load_application(AppName, DistNodes) ->
    gen_server:call(?DIST_AC, {load_application, AppName, DistNodes}, infinity).

takeover_application(AppName, RestartType) ->
    case validRestartType(RestartType) of
	true ->
	    wait_for_sync_dacs(),
	    Nodes = get_nodes(AppName),
	    global:trans(
	      {?LOCK_ID, self()},
	      fun() ->
		      gen_server:call(
			?DIST_AC,
			{takeover_application, AppName, RestartType},
			infinity)
	      end,
	      Nodes);
	false ->
	    {error, {invalid_restart_type, RestartType}}
    end.

%%-----------------------------------------------------------------
%% This function controls which applications are permitted to run.  If
%% an application X runs when this function is called as
%% permit_application(X, false), it is moved to another node where it
%% is permitted to run (distributed applications only).  If there is
%% no such node, the application is stopped. (I.e. local applications
%% are always stopped, and distributed applications with no other node
%% alive are stopped as well.)  If later a call to
%% permit_application(X, true) is made, X is restarted.
%% For example, suppose applications app1 and app2 are started and
%% running.
%% If we evaluate
%%   permit_application(app2, false)
%% app2 is stopped and app1 only is running.
%% If we now evaluate
%%   permit_application(app2, true),
%%   permit_application(app3, true)
%% app2 is restarted, but not app3, since it hasn't been started by a
%% call to start_application.
%%-----------------------------------------------------------------
permit_application(AppName, Bool) ->
    wait_for_sync_dacs(),
    LockId = {?LOCK_ID, self()},
    global:trans(
      LockId,
      fun() ->
	      gen_server:call(?DIST_AC,
			      {permit_application, AppName, Bool, LockId, started},
			      infinity)
      end).

permit_only_loaded_application(AppName, Bool) ->
    wait_for_sync_dacs(),
    LockId = {?LOCK_ID, self()},
    global:trans(
      LockId,
      fun() ->
	      gen_server:call(?DIST_AC,
			      {permit_application, AppName, Bool, LockId, only_loaded},
			      infinity)
      end).

get_nodes(AppName) ->
    gen_server:call(?DIST_AC, {get_nodes, AppName}, infinity).

get_known_nodes() ->
    gen_server:call(?DIST_AC, get_known_nodes).

%%%-----------------------------------------------------------------
%%% call-back functions from gen_server
%%%-----------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

sync_dacs(Appls) ->
    Res = global:trans({?LOCK_ID, sync_dacs},
		       fun() ->
			       Nodes = introduce_me(nodes(), Appls),
			       wait_dacs(Nodes, [node()], Appls, [])
		       end),
    ets:insert(ac_tab, {sync_dacs, ok}),
    Res.
  
introduce_me(Nodes, Appls) ->
    Msg = {dist_ac_new_node, ?vsn, node(), Appls, []},
    filter(fun(Node) ->
		   %% This handles nodes without DACs
		   case rpc:call(Node, erlang, whereis, [?DIST_AC]) of
		       Pid when is_pid(Pid) ->
			   Pid ! Msg,
			   true;
		       _ ->
			   false
		   end
	   end, Nodes).

wait_dacs([Node | Nodes], KnownNodes, Appls, RStarted) ->
    monitor_node(Node, true),
    receive
	%% HisAppls =/= [] is the case when our node connects to a running system
	%%
	%% It is always the responsibility of newer versions to understand
	%% older versions of the protocol.  As we don't have any older
	%% versions (that are supposed to work with this version), we
	%% don't handle version mismatch here.
	{dist_ac_new_node, _Vsn, Node, HisAppls, HisStarted} ->
	    monitor_node(Node, false),
	    NRStarted = RStarted ++ HisStarted,
	    NAppls = dist_merge(Appls, HisAppls, Node),
	    wait_dacs(Nodes, [Node | KnownNodes], NAppls, NRStarted);
	{nodedown, Node} ->
	    monitor_node(Node, false),
	    wait_dacs(Nodes, KnownNodes, Appls, RStarted)
    end;
wait_dacs([], KnownNodes, Appls, RStarted) ->
    {KnownNodes, Appls, RStarted}.


info() ->
    gen_server:call(?DIST_AC, info).
    

%%-----------------------------------------------------------------
%% All functions that can affect which applications are running
%% execute within a global lock, to ensure that they are not
%% executing at the same time as sync_dacs.  However, to avoid a
%% deadlock situation where e.g. permit_application gets the lock
%% before sync_dacs, this function is used to ensure that the local
%% sync_dacs always gets the lock first of all.  The lock is still
%% used to not interfere with sync_dacs on other nodes.
%%-----------------------------------------------------------------
wait_for_sync_dacs() ->
    case catch ets:lookup(ac_tab, sync_dacs) of
	[{sync_dacs, ok}] -> ok;
	_ ->
	    receive after 100 -> ok end,
	    wait_for_sync_dacs()
    end.

handle_cast(init_sync, _S) ->
    %% When the dist_ac is started, it receives this msg, and gets into
    %% the receive loop.  'go' is sent from the kernel_config proc when
    %% all nodes that should be pinged has been pinged.  The reason for this
    %% is that dist_ac syncs with the other nodes at start-up.  That is,
    %% it does _not_ handle partitioned nets!  The other nodes tries to call
    %% the local name dist_ac, which means that this name must be registered
    %% before the distribution.  But it can't sync until after the distribution
    %% is started.  Therefore, this 'go'-thing.
    receive
	{go, KernelConfig} ->
	    Appls = case application:get_env(kernel, distributed) of
			{ok, D} -> dist_check(D);
			undefined -> []
		    end,

	    dist_take_control(Appls),
	    %% kernel_config waits for dist_ac to take control over its
	    %% applications. By this we can be sure that the kernel
	    %% application hasn't completed its start before dist_ac has
	    %% taken control over its applications. (OTP-3509)
	    KernelConfig ! dist_ac_took_control,

	    %% we're really just interested in nodedowns.
	    ok = net_kernel:monitor_nodes(true),
	    
	    {Known, NAppls, RStarted} = sync_dacs(Appls),

	    {noreply,
	     #state{appls = NAppls, known = Known, remote_started = RStarted}}
    end.


handle_call(info, _From, S) ->
    {reply, S, S};
    


handle_call({load_application, AppName, DistNodes}, _From, S) ->
    Appls = S#state.appls,
    case catch dist_replace(DistNodes, AppName, Appls) of
	{error, Error} ->
	    {reply, {error, Error}, S};
	{'EXIT', R} -> 
	    {stop, R, {error, R}, S};
	NAppls ->
	    NewS = case dist_find_nodes(NAppls, AppName) of
		       [] -> % No distrib nodes; we ignore it
			   S;
		       _Nodes ->
			   ensure_take_control(AppName, Appls),
			   {ok, S2} = load(AppName, S#state{appls = NAppls}),
			   S2
		   end,
	    {reply, ok, NewS}
    end;

handle_call({takeover_application, AppName, RestartType}, From, S) ->
    Appls = S#state.appls,
    case keysearch(AppName, #appl.name, Appls) of
	{value, Appl} when element(1, Appl#appl.id) =:= distributed ->
	    {distributed, Node} = Appl#appl.id,
	    _ = ac_takeover(req, AppName, Node, RestartType),
	    NAppl = Appl#appl{id = takeover},
	    NAppls = keyreplace(AppName, #appl.name, Appls, NAppl),
	    TR = S#state.t_reqs,
	    {noreply, S#state{appls = NAppls,
			      t_reqs = [{AppName, From} | TR]}};
	{value, #appl{id = local}} ->
	    {reply, {error, {already_running_locally, AppName}}, S};
	_ ->
	    {reply, {error, {not_running_distributed, AppName}}, S}
    end;

handle_call({permit_application, AppName, Bool, LockId, StartInfo}, From, S) ->
    case lists:keymember(AppName, #appl.name, S#state.appls) of
	false ->
	    %% This one covers the case with permit for non-distributed
	    %% applications.  This shouldn't be handled like this, and not
	    %% here, but we have to be backwards-compatible.
	    case application_controller:get_loaded(AppName) of
		{true, _} when not Bool ->
		    _ = ac_stop_it(AppName),
		    {reply, ok, S};
		{true, _} when Bool ->
		    _ = ac_start_it(req, AppName),
		    {reply, ok, S};
		false ->
		    {reply, {error, {not_loaded, AppName}}, S}
	    end;
	true ->
	    NAppls = dist_update_run(S#state.appls, AppName, node(), Bool),
	    NewS = S#state{appls = NAppls},
	    %% Check if the application is running
	    IsRunning = keysearch(AppName, #appl.name, NAppls),
	    IsMyApp = case IsRunning of
			  {value, #appl{id = local}} -> true;
			  _ -> false
		      end,
	    %% Tell everyone about the new permission
	    Nodes = dist_flat_nodes(NAppls, AppName),
	    Msg = {dist_ac_new_permission, node(), AppName, Bool, IsMyApp},
	    send_msg(Msg, Nodes),
	    case StartInfo of
		only_loaded ->
		    {reply, ok, NewS};
		started ->
		    permit(Bool, IsRunning, AppName, From, NewS, LockId)
	    end
    end;

%%-----------------------------------------------------------------
%% The distributed parameter is changed. Update the parameters
%% but the applications are actually not moved to other nodes
%% even if they should.
%%-----------------------------------------------------------------
handle_call({distribution_changed, NewDistribution}, _From, S) ->
    Appls = S#state.appls,
    NewAppls = dist_change_update(Appls, NewDistribution),
    NewS = S#state{appls = NewAppls},
    {reply, ok, NewS};

    
handle_call({get_nodes, AppName}, _From, S) ->
    Alive = intersection(dist_flat_nodes(S#state.appls, AppName),
			 S#state.known),
    {reply, Alive, S};

handle_call(get_known_nodes, _From, S) ->
    {reply, S#state.known, S}.


handle_info({ac_load_application_req, AppName}, S) ->
    {ok, NewS} = load(AppName, S),
    ?AC ! {ac_load_application_reply, AppName, ok},
    {noreply, NewS};
	    
handle_info({ac_application_unloaded, AppName}, S) ->
    {ok, NewS} = unload(AppName, S),
    {noreply, NewS};
	    
handle_info({ac_start_application_req, AppName}, S) ->
    %% We must decide if we or another node should start the application
    Lock = {?LOCK_ID, self()},
    case global:set_lock(Lock, [node()], 0) of
	true ->
	    S2 = case catch start_appl(AppName, S, reply) of
		     {ok, NewS, _} ->
			 NewS;
		     {error, R} -> 
			 ?AC ! {ac_start_application_reply, AppName, {error,R}},
			 S
		 end,
	    global:del_lock(Lock),
	    {noreply, S2};
	false ->
	    send_after(100, {ac_start_application_req, AppName}),
	    {noreply, S}
    end;

handle_info({ac_application_run, AppName, Res}, S) ->
    %% We ordered a start, and here's the result.  Tell all other nodes.
    Appls = S#state.appls,
    Nodes = S#state.known,
    %% Send this to _all_ known nodes, as any node could sync
    %% on this app (not only nodes that can run it).
    send_msg({dist_ac_app_started, node(), AppName, Res}, Nodes),
    NId = case Res of
	       ok -> local;
	       {error, _R} -> undefined
	  end,
    {value, Appl} = keysearch(AppName, #appl.name, Appls),
    %% Check if we have somebody waiting for the takeover result
    NTReqs = del_t_reqs(AppName, S#state.t_reqs, Res),
    NAppl = Appl#appl{id = NId},
    NAppls = keyreplace(AppName, #appl.name, Appls, NAppl),
    {noreply, S#state{appls = NAppls, t_reqs = NTReqs}};


handle_info({ac_application_not_run, AppName}, S) ->
    %% We ordered a stop, and now it has stopped
    {value, Appl} = keysearch(AppName, #appl.name, Appls = S#state.appls),
    %% Check if we have somebody waiting for the takeover result;
    %% if somebody called stop just before takeover was handled,
    NTReqs = del_t_reqs(AppName, S#state.t_reqs, {error, stopped}),
    %% Check if we have somebody waiting for stop to return
    SReqs = filter(fun({Name, From2}) when Name =:= AppName ->
			   gen_server:reply(From2, ok),
			   false;
		      (_) ->
			   true
		   end, S#state.s_reqs),
    RS = case Appl#appl.id of
	     local ->
		 send_msg({dist_ac_app_stopped, AppName}, S#state.known),
		 S#state.remote_started;
	     {distributed, Node} ->
		 [{Node, AppName} | S#state.remote_started];
	     _ ->
		 S#state.remote_started
	 end,
    NAppl = Appl#appl{id = undefined},
    NAppls = keyreplace(AppName, #appl.name, Appls, NAppl),
    {noreply, S#state{appls = NAppls, t_reqs = NTReqs, s_reqs = SReqs,
		      remote_started = RS}};

handle_info({ac_application_stopped, AppName}, S) ->
    %% Somebody called application:stop - reset state as it was before
    %% the application was started.
    {value, Appl} = keysearch(AppName, #appl.name, Appls = S#state.appls),
    %% Check if we have somebody waiting for the takeover result;
    %% if somebody called stop just before takeover was handled,
    NTReqs = del_t_reqs(AppName, S#state.t_reqs, {error, stopped}),
    %% Check if we have somebody waiting for stop to return
    SReqs = filter(fun({Name, From2}) when Name =:= AppName ->
			   gen_server:reply(From2, ok),
			   false;
		      (_) ->
			   true
		   end, S#state.s_reqs),
    RS = case Appl#appl.id of
	     local ->
		 send_msg({dist_ac_app_stopped, AppName}, S#state.known),
		 S#state.remote_started;
	     {distributed, Node} ->
		 [{Node, AppName} | S#state.remote_started];
	     _ ->
		 S#state.remote_started
	 end,
    NAppl = Appl#appl{id = undefined},
    NAppls = keyreplace(AppName, #appl.name, Appls, NAppl),
    Started = lists:delete(AppName, S#state.started),
    {noreply, S#state{appls = NAppls, started = Started,
		      t_reqs = NTReqs, s_reqs = SReqs,
		      remote_started = RS}};


%%-----------------------------------------------------------------
%% A new node gets running.
%% Send him info about our started distributed applications.
%%-----------------------------------------------------------------
handle_info({dist_ac_new_node, _Vsn, Node, HisAppls, []}, S) ->
    Appls = S#state.appls,
    MyStarted = zf(fun(Appl) when Appl#appl.id =:= local ->
			   {true, {node(), Appl#appl.name}};
		      (_) ->
			   false
		   end, Appls),
    {?DIST_AC, Node} ! {dist_ac_new_node, ?vsn, node(), Appls, MyStarted},
    NAppls = dist_merge(Appls, HisAppls, Node),
    {noreply, S#state{appls = NAppls, known = [Node | S#state.known]}};

handle_info({dist_ac_app_started, Node, Name, Res}, S) ->
    case {keysearch(Name, #appl.name, S#state.appls), lists:member(Name, S#state.started)} of
	{{value, Appl}, true} ->
	    Appls = S#state.appls,
	    NId = case Appl#appl.id of
		      _ when element(1, Res) =:= error ->
			  %% Start of appl on some node failed.
			  %% Set Id to undefined.  That node will have
			  %% to take some actions, e.g. reboot
			  undefined;
		      {distributed, _} ->
			  %% Another node tookover from some node. Update
			  %% appl list.
			  {distributed, Node};
		      local -> 
			  %% Another node tookover from me; stop my application
			  %% and update the running list.
			  {distributed, Node};
		      _ ->
			  %% Another node started appl. Update appl list.
			  {distributed, Node}
		  end,
	    _ = ac_started(req, Name, Node),
	    NAppl = Appl#appl{id = NId},
	    NAppls = keyreplace(Name, #appl.name, Appls, NAppl),
	    TmpWeights = keydelete_all(Name, 1, S#state.tmp_weights),
	    NewS = S#state{appls = NAppls, tmp_weights = TmpWeights},
	    NPermitReq = req_del_permit_false(NewS#state.p_reqs, Name),
	    case catch req_start_app(NewS#state{p_reqs = NPermitReq}, Name) of
		{error, R} ->
		    {stop, R};
		{ok, NewS2} ->
		    {noreply, NewS2}
	    end;
	{_, _} ->
	    %% The app has not been started at this node yet; remember this in
	    %% remote started.
	    NRStarted = [{Node, Name} | S#state.remote_started],
	    {noreply, S#state{remote_started = NRStarted}}
    end;

handle_info({dist_ac_app_stopped, AppName}, S) ->
    Appls = S#state.appls,
    case keysearch(AppName, #appl.name, Appls) of
	false ->
	    RStarted = keydelete(AppName, 2, S#state.remote_started),
	    {noreply, S#state{remote_started = RStarted}};
	{value, Appl} ->
	    NAppl = Appl#appl{id = undefined},
	    NAppls = keyreplace(AppName, #appl.name, Appls, NAppl),
	    RStarted = keydelete(AppName, 2, S#state.remote_started),
	    {noreply, S#state{appls = NAppls, remote_started = RStarted}}
    end;

handle_info({dist_ac_weight, Name, Weight, Node}, S) ->
    %% This means another node starts up, and will eventually take over
    %% this appl.  We have a situation like: {Name, [{Node}, node()]}
    %% Node sends us this msg, and we must respond.  It doesn't really
    %% matter what we send him; but it must be a dist_ac_weight msg.
    %% Another situation is {Name, [RNode, {node()}, Node]}.
    %%
    %% Yet another situation is that the node where Name was running crashed,
    %% and Node has got the nodedown message, but we haven't.  In this case,
    %% we must send a correct weight to Node. i.e. the same weight that
    %% we'll send to him later, when we get the nodedown message.
    case keysearch(Name, #appl.name, S#state.appls) of
	{value, Appl} -> 
	    Id = Appl#appl.id,
	    case Id of 
		run_waiting ->
		    {?DIST_AC, Node} ! {dist_ac_weight, Name, 0, node()},
		    {noreply, S};
		undefined -> 
		    {noreply, 
		     S#state{tmp_locals = [{Name, Weight, Node} |
					   S#state.tmp_locals]}};
		{takeover, _} -> 
		    {noreply, 
		     S#state{tmp_locals = [{Name, Weight, Node} |
					   S#state.tmp_locals]}};
		{failover, _} -> 
		    {noreply, 
		     S#state{tmp_locals = [{Name, Weight, Node} |
					   S#state.tmp_locals]}};
		_ ->
		    MyWeight = get_cached_weight(Name, S),
		    {?DIST_AC, Node} ! {dist_ac_weight, Name, MyWeight, node()},
		    NTWs = keyreplaceadd(Name, 1, S#state.tmp_weights,
					 {Name, MyWeight}),
		    {noreply,  S#state{tmp_weights = NTWs}}
	    end;
	_ ->
	    {noreply, 
	     S#state{tmp_locals = [{Name, Weight, Node} | S#state.tmp_locals]}}
    end;

%%-----------------------------------------------------------------
%% A node died.  Check if we should takeover some applications.
%%-----------------------------------------------------------------
handle_info({nodedown, Node}, S) ->
    AppNames = dist_get_runnable(S#state.appls),
    HisAppls = filter(fun(#appl{name = Name, id = {distributed, N}}) 
			 when Node =:= N -> lists:member(Name, AppNames);
			 (_) -> false
		      end,
		      S#state.appls),
    Appls2 = zf(fun(Appl) when Appl#appl.id =:= {distributed, Node} -> 
			case lists:member(Appl#appl.name, AppNames) of
			    true ->
				{true, Appl#appl{id = {failover, Node}}};
			    false -> 
				_ = ac_not_running(Appl#appl.name),
				{true, Appl#appl{id = undefined}}
			end;
		   (_) ->
			true
		end,
		S#state.appls),
    RStarted = filter(fun({Node2, _Name}) when Node2 =:= Node -> false;
			 (_) -> true
		      end,
		      S#state.remote_started),
    Appls3 = dist_del_node(Appls2, Node),
    {NPermitReq, Appls4, SReqs} = req_del_node(S, Node, Appls3),
    NKnown = lists:delete(Node, S#state.known),
    NewS = S#state{appls = Appls4, p_reqs = NPermitReq, known = NKnown,
		   s_reqs = SReqs,
		   remote_started = RStarted},
    restart_appls(HisAppls),
    {noreply, NewS};

handle_info({dist_ac_app_loaded, Node, Name, HisNodes, Permission, HeKnowsMe},
	    S) ->
    Nodes = dist_find_nodes(Appls = S#state.appls, Name),
    case is_loaded(Name, S) of
	true ->
	    case equal_nodes(Nodes, HisNodes) of
		true ->
		    NAppls = dist_update_run(Appls, Name, Node, Permission),
		    if
			not HeKnowsMe ->
			    %% We've got it loaded, but he doesn't know -
			    %% he's a new node connecting to us.
			    Msg = {dist_ac_app_loaded, node(), Name,
				   Nodes, dist_is_runnable(Appls, Name), true},
			    {?DIST_AC, Node} ! Msg,
                            ok;
			true ->
			    ok
		    end,
		    {noreply, S#state{appls = NAppls}};
		false ->
		    dist_mismatch(Name, Node)
	    end;
	false ->
	    Load =[{{Name, Node}, HisNodes, Permission} | S#state.dist_loaded],
	    {noreply, S#state{dist_loaded = Load}}
    end;

handle_info({dist_ac_app_unloaded, Node, Name}, S) ->
    Appls = dist_update_run(S#state.appls, Name, Node, undefined),
    Load = keydelete({Name, Node}, 1, S#state.dist_loaded),
    {noreply, S#state{appls = Appls, dist_loaded = Load}};


handle_info({dist_ac_new_permission, Node, AppName, false, IsHisApp}, S) ->
    Appls = dist_update_run(S#state.appls, AppName, Node, false),
    NewS = S#state{appls =Appls},
    case dist_is_runnable(Appls, AppName) of
	true when IsHisApp ->
	    case catch start_appl(AppName, NewS, req) of
		{ok, NewS2, _}  ->
		    {noreply, NewS2};
		{error, _R} -> % if app was permanent, AC will shutdown the node
		    {noreply, NewS}
	    end;
	_ ->
	    {noreply, NewS}
    end;
handle_info({dist_ac_new_permission, Node, AppName, true, _IsHisApp}, S) ->
    Appls = dist_update_run(S#state.appls, AppName, Node, true),
    {noreply, S#state{appls = Appls}};

handle_info({internal_restart_appl, Name}, S) ->
    case restart_appl(Name, S) of
	{error, R} ->
	    {stop, {error, R}, S};
	NewS ->
	    {noreply, NewS}
    end;
    
handle_info(_, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------
load(AppName, S) ->
    Appls0 = S#state.appls,
    %% Get the dist specification for the app on other nodes
    DistLoaded = get_dist_loaded(AppName, Load1 = S#state.dist_loaded),
    %% Get the local dist specification
    Nodes = dist_find_nodes(Appls0, AppName),
    FNodes = flat_nodes(Nodes),
    %% Update dists spec with our local permission
    Permission = get_default_permission(AppName),
    Appls1 = dist_update_run(Appls0, AppName, node(), Permission),
    %% Compare the local spec with other nodes's specs
    %% If equal, update our spec with his current permission
    {LoadedNodes, Appls2} =
	mapfoldl(
	  fun({Node, HisNodes, HisPermission}, Appls) ->
		  case equal_nodes(Nodes, HisNodes) of
		      true ->
			  {Node, dist_update_run(Appls, AppName,
						 Node, HisPermission)};
		      _ ->
			  dist_mismatch(AppName, Node)
		  end
	  end, Appls1, DistLoaded),
    Load2 = del_dist_loaded(AppName, Load1),
    %% Tell all Nodes about the new appl loaded, and its permission.
    foreach(fun(Node) when Node =/= node() ->
		    Msg = {dist_ac_app_loaded, node(), AppName,
			   Nodes, Permission, member(Node, LoadedNodes)},
		    {?DIST_AC, Node} ! Msg;
	       (_) -> ok
	    end, FNodes),
    {ok, S#state{appls = Appls2, dist_loaded = Load2}}.

ensure_take_control(AppName, Appls) ->
    %% Check if this is a new application that we don't control yet
    case lists:keymember(AppName, #appl.name, Appls) of
	true ->       % we have control
	    ok;
	false ->      % take control!
	    %% Note: this works because this is executed within a
	    %% synchronous call. I.e. we get the control *before*
	    %% application:load returns. (otherwise application:start
	    %% could be called before we got the chance to take control)
	    %% The only reason we have to bother about this is because
	    %% we have to be backwards compatible in the sense that all
	    %% apps don't have to be specified in the 'distributed' parameter,
	    %% but may be implicitly 'distributed' by a call to
	    %% application:load.
	    application_controller:control_application(AppName)
    end.
    
unload(AppName, S) ->
    Appls = S#state.appls,
    Nodes = dist_flat_nodes(Appls, AppName),
    %% Tell all ACs in DistNodes about the unloaded appl
    Msg = {dist_ac_app_unloaded, node(), AppName},
    send_msg(Msg, Nodes),
    {value, Appl} = keysearch(AppName, #appl.name, Appls),
    NAppl = Appl#appl{id = undefined, run = []},
    {ok, S#state{appls = keyreplace(AppName, #appl.name, Appls, NAppl)}}.

start_appl(AppName, S, Type) ->
    %% Get nodes, and check if App is loaded on all involved nodes.
    %% If it is loaded everywhere, we know that we have the same picture
    %% of the nodes; otherwise the load wouldn't have succeeded.
    Appl = case keysearch(AppName, #appl.name, Appls = S#state.appls) of
	       {value, A} -> A;
	       _ -> throw({error, {unknown_application, AppName}})
	   end,
    case Appl#appl.id of
	local ->
	    %% UW 990913: we've already started the app
	    %% this could happen if ac_start_application_req was resent.
	    {ok,S,false};
	_ ->
	    {Id, IsWaiting} = case dist_get_all_nodes(Appl) of
				  {ok, DistNodes, PermittedNodes} ->
				      start_distributed(Appl, AppName, DistNodes,
							PermittedNodes, S, Type);
				  Error -> throw(Error)
			      end,
	    NAppl = Appl#appl{id = Id},
	    NAppls = keyreplaceadd(AppName, #appl.name, Appls, NAppl),
	    {ok, NewS} = req_start_app(S#state{appls = NAppls}, AppName),
	    TmpLocals = keydelete_all(AppName, 1, NewS#state.tmp_locals),
	    TmpWeights = keydelete_all(AppName, 1, NewS#state.tmp_weights),
	    RStarted = keydelete(AppName, 2, S#state.remote_started),
	    Started = replaceadd(AppName, NewS#state.started),
	    {ok,
	     NewS#state{started = Started, tmp_locals = TmpLocals,
			tmp_weights = TmpWeights, remote_started = RStarted},
	     IsWaiting}
    end.


start_distributed(Appl, Name, Nodes, PermittedNodes, S, Type) ->
    case find_start_node(Nodes, PermittedNodes, Name, S) of
	{ok, Node} when Node =:= node() ->
	    _ = case Appl#appl.id of
		    {failover, FoNode} when Type =:= req ->
			ac_failover(Name, FoNode, undefined);
		    {distributed, Node2} when Type =:= req ->
			ac_takeover(req, Name, Node2, undefined);
		    _ when Type =:= reply ->
			case lists:keysearch(Name, 2, S#state.remote_started) of
			    {value, {Node3, _}} ->
				ac_takeover(reply, Name, Node3, undefined);
			    _ ->
				ac_start_it(Type, Name)
			end;
		    _ ->
			ac_start_it(Type, Name)
		end,
	    {run_waiting, true};
	{already_started, Node} ->
	    _ = ac_started(Type, Name, Node),
	    {{distributed, Node}, false};
	{ok, Node} ->
	    case keysearch(Name, #appl.name, S#state.appls) of
		{value, #appl{id = {distributed, Node}}} ->
		    _ = ac_started(Type, Name, Node),
		    {{distributed, Node}, false};
		_ ->
		    wait_dist_start(Node, Appl, Name, Nodes,
				    PermittedNodes, S, Type)
	    end;
	not_started ->
	    wait_dist_start2(Appl, Name, Nodes, PermittedNodes, S, Type);
	no_permission ->
	    _ = ac_not_started(Type, Name),
	    {undefined, false}
    end.

wait_dist_start(Node, Appl, Name, Nodes, PermittedNodes, S, Type) ->
    monitor_node(Node, true),
    receive
	{dist_ac_app_started, Node, Name, ok} ->
	    _ = ac_started(Type, Name, Node),
	    monitor_node(Node, false),
	    {{distributed, Node}, false};
	{dist_ac_app_started, Node, Name, {error, R}} ->
	    _ = ac_error(Type, Name, {Node, R}),
	    monitor_node(Node, false),
	    {Appl#appl.id, false};
	{dist_ac_weight, Name, _Weigth, Node} ->
	    %% This is the situation: {Name, [RNode, {Node}, node()]}
	    %% and permit(false) is called on RNode, and we sent the
	    %% weigth first.  Node handled it in handle_info, and
	    %% now we must send him a weigth msg.  We can use any weigth;
	    %% he wins anyway.
	    monitor_node(Node, false),
	    {?DIST_AC, Node} !
		{dist_ac_weight, Name, get_cached_weight(Name, S), node()},
	    wait_dist_start(Node, Appl, Name, Nodes, PermittedNodes, S, Type);
	{nodedown, Node} ->
	    monitor_node(Node, false),
	    TmpLocals =
		filter(fun({Name2, _Weight, Node2}) when Node2 =:= Node,
							 Name2 =:= Name -> false;
			  (_) -> true
		       end,
		       S#state.tmp_locals),
	    NewS = S#state{tmp_locals = TmpLocals},
	    start_distributed(Appl, Name, Nodes, 
			      lists:delete(Node, PermittedNodes), NewS, Type)
    end.

wait_dist_start2(Appl, Name, Nodes, PermittedNodes, S, Type) ->
    receive
	{dist_ac_app_started, Node, Name, ok} ->
	    _ = ac_started(Type, Name, Node),
	    {{distributed, Node}, false};
	{dist_ac_app_started, Node, Name, {error, R}} ->
	    _ = ac_error(Type, Name, {Node, R}),
	    {Appl#appl.id, false};
	{nodedown, Node} ->
	    %% A node went down, try to start the app again - there may not
	    %% be any more nodes to wait for.
	    TmpLocals =
		filter(fun({Name2, _Weight, Node2}) when Node2 =:= Node,
							 Name2 =:= Name -> false;
			  (_) -> true
		       end,
		       S#state.tmp_locals),
	    NewS = S#state{tmp_locals = TmpLocals},
	    start_distributed(Appl, Name, Nodes, 
			      lists:delete(Node, PermittedNodes), NewS, Type)
    end.


ac_start_it(reply, Name) ->
    ?AC ! {ac_start_application_reply, Name, start_it};
ac_start_it(req, Name) ->
    ?AC ! {ac_change_application_req, Name, start_it}.

ac_started(reply, Name, Node) ->
    ?AC ! {ac_start_application_reply, Name, {started, Node}};
ac_started(req, Name, Node) ->
    ?AC ! {ac_change_application_req, Name, {started, Node}}.

ac_error(reply, Name, Error) ->
    ?AC ! {ac_start_application_reply, Name, {error, Error}};
ac_error(req, _Name, _Error) ->
    ok.

ac_not_started(reply, Name) ->
    ?AC ! {ac_start_application_reply, Name, not_started};
ac_not_started(req, Name) ->
    ?AC ! {ac_change_application_req, Name, stop_it}.

ac_stop_it(Name) ->
  ?AC ! {ac_change_application_req, Name, stop_it}.

ac_takeover(reply, Name, Node, _RestartType) ->
    ?AC ! {ac_start_application_reply, Name, {takeover, Node}};
ac_takeover(req, Name, Node, RestartType) ->
    ?AC ! {ac_change_application_req, Name, 
	   {takeover, Node, RestartType}}.

ac_failover(Name, Node, RestartType) ->
    ?AC ! {ac_change_application_req, Name, 
	   {failover, Node, RestartType}}.

ac_not_running(Name) ->
    ?AC ! {ac_change_application_req, Name, not_running}.

restart_appls(Appls) ->
    foreach(fun(Appl) ->
		    AppName = Appl#appl.name,
		    send_after(Appl#appl.restart_time,
			       {internal_restart_appl, AppName})
	    end, lists:reverse(Appls)).

restart_appl(AppName, S) ->
    case keysearch(AppName, #appl.name, S#state.appls) of
	{value, Appl} when element(1, Appl#appl.id) =:= failover ->
	    case catch start_appl(AppName, S, req) of
		{ok, NewS, _} ->
		    NewS;
		{error, R}  ->
		    error_msg("Error when restarting application ~p: ~p~n",
			      [AppName, R]),
		    S
	    end;
	_ ->
	    S
    end.
    
%% permit(ShouldBeRunning, IsRunning, ...)
permit(false, {value, #appl{id = undefined}}, _AppName, _From, S, _LockId) ->
   {reply, ok, S}; % It's not running
permit(false, {value, #appl{id = Id}}, _AppName, _From, S, _LockId)
  when element(1, Id) =:= distributed ->
    %% It is running at another node already
    {reply, ok, S};
permit(false, {value, _}, AppName, From, S, _LockId) ->
    %% It is a distributed application
    %% Check if there is any runnable node
    case dist_get_runnable_nodes(S#state.appls, AppName) of
	[] ->
	    %% There is no runnable node; stop application
	    _ = ac_stop_it(AppName),
	    SReqs = [{AppName, From} | S#state.s_reqs],
	    {noreply, S#state{s_reqs = SReqs}};
	Nodes ->
	    %% Delete all outstanding 'permit true' requests.
	    PR = req_del_permit_true(S#state.p_reqs, AppName), 
	    NPReqs = [{From, AppName, false, Nodes} | PR],
	    {noreply, S#state{p_reqs = NPReqs}}
    end;
permit(true, {value, #appl{id = local}}, _AppName, _From, S, _LockId) ->
    {reply, ok, S};
permit(true, _, AppName, From, S, LockId) ->
    case catch start_appl(AppName, S, req) of
	{_ErrorTag, {not_running, App}} ->
	    %% Delete all outstanding 'permit false' requests
	    PR = req_del_permit_false(S#state.p_reqs, AppName),
	    NPReqs = [{false, AppName, true, App} | PR],
	    {reply, ok, S#state{p_reqs = NPReqs}};
	{ok, NewS, true} ->
	    %% We have ordered a start or a takeover; we must not return
	    %% until the app is running.
	    TR = NewS#state.t_reqs,
	    %% Delete the lock, so others may start the app
	    global:del_lock(LockId),
	    {noreply, NewS#state{t_reqs = [{AppName, From} | TR]}};
	{ok, _S, false} ->
	    %% Application should be started, but at another node
	    %% State remains the same
	    {reply, ok, S};
	{_ErrorTag, R} ->
	    {stop, R, {error, R}, S}
    end.

do_start_appls(StartApps, S) ->
    SortedStartApps = StartApps,
    Appls = S#state.appls,
    {ok, foldl(
      fun(AppName, NewS) ->
	      case catch start_appl(AppName, NewS, req) of
		  {error, R}  ->
		      throw({{error, NewS}, R});
		  {ok, NewS2, _} ->
		      NewS2
	      end
      end, S#state{appls = Appls}, lists:reverse(SortedStartApps))}.

%%-----------------------------------------------------------------
%% Nodes = [node() | {node(), ..., node()}]
%% A list in priority order.  If it is a tuple, we may pick any of
%% them.  This decision is made by all nodes in the list, and all
%% nodes choose the same.  This is accomplished in the following
%% way:  all Nodes send to all others a msg which tells how many
%% applications each node has started.  The one with least no of
%% appls starts this one.
%%-----------------------------------------------------------------
find_start_node(Nodes, PermittedNodes, Name, S) ->
    AllNodes = intersection(flat_nodes(Nodes), PermittedNodes),
    case lists:member(node(), AllNodes) of
	true ->
	    Weight = get_cached_weight(Name, S),
	    find_start_node(Nodes, Name, S, Weight, AllNodes);
	false ->
	    case keysearch(Name, 2, S#state.remote_started) of
		{value, {Node, _Name}} ->
		    {already_started, Node};
		_ when AllNodes =/= [] ->
		    not_started;
		_ ->
		    no_permission
	    end
    end.

find_start_node([AnyNodes | Nodes], Name, S, Weight, AllNodes)
  when is_tuple(AnyNodes) ->
    case find_any_node(tuple_to_list(AnyNodes), Name, S, Weight, AllNodes) of
	false -> find_start_node(Nodes, Name, S, Weight, AllNodes);
	Res -> Res
    end;
find_start_node([Node | Nodes], Name, S, Weight, AllNodes) ->
    case lists:member(Node, AllNodes) of
	true ->
	    case keysearch(Name, #appl.name, S#state.appls) of
		{value, #appl{id = {distributed, Node}}} ->
		    {already_started, Node};
		_ ->
		    case keysearch(Name, 2, S#state.remote_started) of
			{value, {Node, _Name}} ->
			    {already_started, Node};
			_ ->
			    {ok, Node}
		    end
	    end;
	false -> find_start_node(Nodes, Name, S, Weight, AllNodes)
    end;
find_start_node([], _Name, _S, _Weight, _AllNodes) ->
    not_started.

%%-----------------------------------------------------------------
%% First of all, check if the application is already running
%% somewhere in AnyNodes; in that case we shall not move it!
%%-----------------------------------------------------------------
find_any_node(AnyNodes, Name, S, Weight, AllNodes) ->
    case check_running(Name, S, intersection(AnyNodes, AllNodes)) of
	{already_started, Node} -> {already_started, Node};
	false ->
	    %% Synchronize with all other nodes.
	    send_nodes(AllNodes, {dist_ac_weight, Name, Weight, node()}),
	    Answers = [{Weight, node()} |
		       collect_answers(AllNodes, Name, S, [])],
	    %% Make a decision (the same at every node) (smallest weight wins)
	    find_alive_node(lists:sort(Answers), 
			    intersection(AnyNodes, S#state.known))
    end.

%%-----------------------------------------------------------------
%% Check if another node started the appl before we got alive.
%% If so, check if the node is one of AnyNodes.
%%-----------------------------------------------------------------
check_running(Name, #state{remote_started = RStarted,
			   appls = Appls}, AnyNodes) ->
    case keysearch(Name, 2, RStarted) of
	{value, {Node, _Name}} ->
	    case lists:member(Node, AnyNodes) of
		true -> {already_started, Node};
		false -> false
	    end;
	false ->
	    case keysearch(Name, #appl.name, Appls) of
		{value, #appl{id = {distributed, Node}}} ->
		    case lists:member(Node, AnyNodes) of
			true -> {already_started, Node};
			false -> false
		    end;
		_ ->
		    false
	    end
    end.

find_alive_node([{_, Node} | Nodes], AliveNodes) ->
    case lists:member(Node, AliveNodes) of
	true -> {ok, Node};
	false -> find_alive_node(Nodes, AliveNodes)
    end;
find_alive_node([], _AliveNodes) ->
    false.

%%-----------------------------------------------------------------
%% First, check if the node's msg is buffered (received in our
%% main loop).  Otherwise, wait for msg or nodedown.
%% We have sent the dist_ac_weight message, and will wait for it
%% to be received here (or a nodedown).  This implies that a 
%% dist_ac must *always* be prepared to get this messages, and to
%% send it to us.
%%-----------------------------------------------------------------
collect_answers([Node | Nodes], Name, S, Res) when Node =/= node() ->
    case keysearch(Node, 3, S#state.tmp_locals) of
	{value, {Name, Weight, Node}} ->
	    collect_answers(Nodes, Name, S, [{Weight, Node} | Res]);
	_ ->
	    monitor_node(Node, true),
	    receive
		{dist_ac_weight, Name, Weight, Node} ->
		    monitor_node(Node, false),
		    collect_answers(Nodes, Name, S, [{Weight, Node} | Res]);
		{nodedown, Node} ->
		    monitor_node(Node, false),
		    collect_answers(Nodes, Name, S, Res)
	    end
    end;
collect_answers([_ThisNode | Nodes], Name, S, Res) ->
    collect_answers(Nodes, Name, S, Res);
collect_answers([], _Name, _S, Res) ->
    Res.

send_nodes(Nodes, Msg) ->
    FlatNodes = flat_nodes(Nodes),
    foreach(fun(Node) when Node =/= node() -> {?DIST_AC, Node} ! Msg;
	       (_ThisNode) -> ok
	    end, FlatNodes).

send_after(Time, Msg) when is_integer(Time), Time >= 0 ->
    _Pid = spawn_link(?MODULE, send_timeout, [self(), Time, Msg]),
    ok;
send_after(_,_) -> % infinity
    ok.

send_timeout(To, Time, Msg) ->
    receive
    after Time -> To ! Msg
    end.

send_msg(Msg, Nodes) ->
    foreach(fun(Node) when Node =/= node() -> {?DIST_AC, Node} ! Msg;
	       (_) -> ok
	    end, Nodes).

replaceadd(Item, List) ->
    case member(Item, List) of
	true -> List;
	false -> [Item | List]
    end.

keyreplaceadd(Key, Pos, List, New) ->
    case lists:keymember(Key, Pos, List) of
	true -> lists:keyreplace(Key, Pos, List, New);
	false -> [New | List]
    end.

keydelete_all(Key, N, [H|T]) when element(N, H) =:= Key ->
    keydelete_all(Key, N, T);
keydelete_all(Key, N, [H|T]) ->
    [H|keydelete_all(Key, N, T)];
keydelete_all(_Key, _N, []) -> [].

-ifdef(NOTUSED).
keysearchdelete(Key, Pos, List) ->
    ksd(Key, Pos, List, []).

ksd(Key, Pos, [H | T], Rest) when element(Pos, H) =:= Key ->
    {value, H, Rest ++ T};
ksd(Key, Pos, [H | T], Rest) ->
    ksd(Key, Pos, T, [H | Rest]);
ksd(_Key, _Pos, [], _Rest) ->
    false.
    
get_new_appl(Name, [{application, Name, App} | _]) ->
    {ok, {application, Name, App}};
get_new_appl(Name, [_ | T]) -> get_new_appl(Name, T);
get_new_appl(Name, []) -> false.
-endif.

equal_nodes([H | T1], [H | T2]) when is_atom(H) ->
    equal_nodes(T1, T2);
equal_nodes([H1 | T1], [H2 | T2]) when is_tuple(H1), is_tuple(H2) ->
    case equal(tuple_to_list(H1), tuple_to_list(H2)) of
	true -> equal_nodes(T1, T2);
	false -> false
    end;
equal_nodes([], []) -> true;
equal_nodes(_, _) -> false.

equal([H | T] , S) ->
    case lists:member(H, S) of
	true -> equal(T, lists:delete(H, S));
	false -> false
    end;
equal([], []) -> true;
equal(_, _) -> false.

flat_nodes(Nodes) when is_list(Nodes) ->
    foldl(fun(Node, Res) when is_atom(Node) -> [Node | Res];
	     (Tuple, Res) when is_tuple(Tuple) -> tuple_to_list(Tuple) ++ Res
	  end, [], Nodes);
flat_nodes(Nodes) ->
    throw({error, {badarg, Nodes}}).

get_cached_weight(Name, S) ->
    case lists:keysearch(Name, 1, S#state.tmp_weights) of
	{value, {_, W}} -> W;
	_ -> get_weight()
    end.

%% Simple weight; just count the number of applications running.
get_weight() ->
    length(application:which_applications()).

get_dist_loaded(Name, [{{Name, Node}, HisNodes, Permission} | T]) ->
    [{Node, HisNodes, Permission} | get_dist_loaded(Name, T)];
get_dist_loaded(Name, [_H | T]) ->
    get_dist_loaded(Name, T);
get_dist_loaded(_Name, []) ->
    [].

del_dist_loaded(Name, [{{Name, _Node}, _HisNodes, _Permission} | T]) ->
    del_dist_loaded(Name, T);
del_dist_loaded(Name, [H | T]) ->
    [H | del_dist_loaded(Name, T)];
del_dist_loaded(_Name, []) ->
    [].

req_start_app(State, Name) ->
    {ok, foldl(
	   fun({false, AppName, true, Name2}, S) when Name =:= Name2 ->
		   PR = keydelete(AppName, 2, S#state.p_reqs),
		   NS = S#state{p_reqs = PR},
		   case catch do_start_appls([AppName], NS) of
		       {_ErrorTag, {not_running, App}} ->
			   NRequests = [{false, AppName, true, App} | PR],
			   S#state{p_reqs = NRequests};
		       {ok, NewS} ->
			   NewS;
		       {_ErrorTag, R} ->
			   throw({error, R})
		   end;
	      (_, S) ->
		   S
	   end, State, State#state.p_reqs)}.


req_del_permit_true(Reqs, Name) ->
    filter(fun({From, Name2, true, _}) when Name2 =:= Name ->
		   gen_server:reply(From, ok),
		   false;
	      (_) ->
		   true
	   end, Reqs).

req_del_permit_false(Reqs, Name) ->
    filter(fun({From, Name2, false, _Nodes}) when Name2 =:= Name ->
		   gen_server:reply(From, ok),
		   false;
	      (_) ->
		   true
	   end, Reqs).
    
req_del_node(S, Node, Appls) ->
    check_waiting(S#state.p_reqs, S, Node, Appls, [], S#state.s_reqs).

del_t_reqs(AppName, TReqs, Res) ->
    lists:filter(fun({AN, From}) when AppName =:= AN ->
			 gen_server:reply(From, Res),
			 false;
		    (_) ->
			 true
		 end,
		 TReqs).


check_waiting([{From, AppName, false, Nodes} | Reqs],
	      S, Node, Appls, Res, SReqs) ->
    case lists:delete(Node, Nodes) of
	[] ->
	    _ = ac_stop_it(AppName),
	    NSReqs = [{AppName, From} | SReqs],
	    check_waiting(Reqs, Node, S, Appls, Res, NSReqs);
	NNodes ->
	    check_waiting(Reqs, Node, S, Appls,
			  [{From, AppName, false, NNodes} | Res], SReqs)
    end;
check_waiting([H | Reqs], S, Node, Appls, Res, SReqs) ->
    check_waiting(Reqs, Node, S, Appls, [H | Res], SReqs);
check_waiting([], _Node, _S, Appls, Res, SReqs) ->
    {Res, Appls, SReqs}.
	    
intersection([], _) -> 
    [];
intersection(_, []) -> 
    [];
intersection(L1, L2) ->
    L1 -- (L1 -- L2).
    
get_default_permission(AppName) ->
    case application:get_env(kernel, permissions) of
	{ok, Permissions} ->
	    case keysearch(AppName, 1, Permissions) of
		{value, {_, true}} ->  true;
		{value, {_, false}} -> false;
		{value, {_, X}} -> exit({bad_permission, {AppName, X}});
		false -> true
	    end;
	undefined -> true
    end.
		     
%%-----------------------------------------------------------------
%% ADT dist() - info on how an application is distributed
%% dist() = [{AppName, Time, DistNodes, [{Node, Runnable}]}]
%% Time = int() >= 0 | infinity
%% Nodes = [node() | {node()...}]
%% Runnable = true | false | undefined
%%       An appl may not be started if any Runnable is undefined;
%%       i.e. the appl must be loaded on all Nodes.
%%-----------------------------------------------------------------
dist_check([{AppName, Nodes} | T]) ->
    P = get_default_permission(AppName),
    [#appl{name = AppName, nodes = Nodes, run = [{node(), P}]} | dist_check(T)];
dist_check([{AppName, Time, Nodes} | T]) when is_integer(Time), Time >= 0 ->
    P = get_default_permission(AppName),
    [#appl{name = AppName, restart_time = Time, nodes = Nodes,
	   run = [{node(), P}]} | dist_check(T)];
dist_check([{AppName, infinity, Nodes} | T]) ->
    P = get_default_permission(AppName),
    [#appl{name = AppName, restart_time = infinity,
	   nodes = Nodes, run = [{node(), P}]} |
     dist_check(T)];
dist_check([_ | T]) ->
    dist_check(T);
dist_check([]) ->
    [].

dist_take_control(Appls) ->
    foreach(fun(#appl{name = AppName}) ->
		    application_controller:control_application(AppName)
	    end, Appls).

dist_replace(default, _Name, Appls) -> Appls;
dist_replace({AppName, Nodes}, AppName, Appls) ->
    Run = [{Node, undefined} || Node <- flat_nodes(Nodes)],
    keyreplaceadd(AppName, #appl.name, Appls,
		  #appl{name = AppName, restart_time = 0,
			nodes = Nodes, run = Run});
dist_replace({AppName, Time, Nodes}, AppName, Appls)
  when is_integer(Time), Time >= 0 ->
    Run = [{Node, undefined} || Node <- flat_nodes(Nodes)],
    keyreplaceadd(AppName, #appl.name, Appls,
		  #appl{name = AppName, restart_time = Time,
			nodes = Nodes, run = Run});
dist_replace(Bad, _Name, _Appls) ->
    throw({error, {bad_distribution_spec, Bad}}).

dist_update_run(Appls, AppName, Node, Permission) ->
    map(fun(Appl) when Appl#appl.name =:= AppName ->
		Run = Appl#appl.run,
		NRun = keyreplaceadd(Node, 1, Run, {Node, Permission}),
		Appl#appl{run = NRun};
	   (Appl) ->
		Appl
	end, Appls).



dist_change_update(Appls, []) ->
    Appls;
dist_change_update(Appls, [{AppName, NewNodes} | NewDist]) ->
    NewAppls = do_dist_change_update(Appls, AppName, 0, NewNodes),
    dist_change_update(NewAppls, NewDist);
dist_change_update(Appls, [{AppName, NewTime, NewNodes} | NewDist]) ->
    NewAppls = do_dist_change_update(Appls, AppName, NewTime, NewNodes),
    dist_change_update(NewAppls, NewDist).

do_dist_change_update(Appls, AppName, NewTime, NewNodes) ->
    map(fun(Appl) when Appl#appl.name =:= AppName ->
		Appl#appl{restart_time = NewTime, nodes = NewNodes};
	   (Appl) ->
		Appl
	end, Appls).

%% Merge his Permissions with mine.
dist_merge(MyAppls, HisAppls, HisNode) ->
    zf(fun(Appl) ->
	       #appl{name = AppName, run = Run} = Appl,
%	       #appl{name = AppName, nodes = Nodes, run = Run} = Appl,
%	       HeIsMember = lists:member(HisNode, flat_nodes(Nodes)),
	       HeIsMember = true,
	       case keysearch(AppName, #appl.name, HisAppls) of
		   {value, #appl{run = HisRun}} when HeIsMember ->
		       case keysearch(HisNode, 1, HisRun) of
			   {value, Val} -> % He has it loaded
			       NRun = keyreplaceadd(HisNode, 1, Run, Val),
			       {true, Appl#appl{run = NRun}};
			   false -> % He hasn't loaded it yet
			       Val = {HisNode, undefined},
			       {true, Appl#appl{run = [Val | Run]}}
		       end;
		   _ ->
		       true
	       end
       end, MyAppls).

dist_get_runnable_nodes(Appls, AppName) ->
    case keysearch(AppName, #appl.name, Appls) of
	{value, #appl{run = Run}} ->
	    zf(fun({Node, true}) -> {true, Node};
		  (_) -> false
	       end, Run);
	false ->
	    []
    end.

dist_is_runnable(Appls, AppName) ->
    case keysearch(AppName, #appl.name, Appls) of
	{value, #appl{run = Run}} ->
	    case keysearch(node(), 1, Run) of
		{value, {_, true}} -> true;
		_ -> false
	    end;
	false ->
	    false
    end.

is_loaded(AppName, #state{appls = Appls}) ->
    case keysearch(AppName, #appl.name, Appls) of
	{value, #appl{run = Run}} ->
	    case keysearch(node(), 1, Run) of
		{value, {_Node, undefined}} -> false;
		{value, _} -> true;
		false -> false
	    end;
	false ->
	    false
    end.

dist_get_runnable(Appls) ->
    zf(fun(#appl{name = AppName, run = Run}) ->
	       case keysearch(node(), 1, Run) of
		   {value, {_, true}} -> {true, AppName};
		   _ -> false
	       end
       end, Appls).
	       
dist_get_all_nodes(#appl{name = AppName, nodes = Nodes, run = Run}) ->
    {Res, BadNodes} = check_nodes(Run, [], []),
    case intersection(BadNodes, erlang:nodes(connected)) of
        [] -> {ok, Nodes, Res};
        _ -> {error, {app_not_loaded, AppName, BadNodes}}
    end.

check_nodes([{Node, undefined} | T], Res, BadNodes) ->
    check_nodes(T, Res, [Node | BadNodes]);
check_nodes([{Node, true} | T], Res, BadNodes) ->
    check_nodes(T, [Node | Res], BadNodes);
check_nodes([{_Node, false} | T], Res, BadNodes) ->
    check_nodes(T, Res, BadNodes);
check_nodes([], Res, BadNodes) ->
    {Res, BadNodes}.

-ifdef(NOTUSED).
dist_find_time([#appl{name = Name, restart_time = Time} |_], Name) -> Time;
dist_find_time([_ | T], Name) -> dist_find_time(T, Name);
dist_find_time([], Name) -> 0.
-endif.

%% Find all nodes that can run the app (even if they're not permitted
%% to right now).
dist_find_nodes([#appl{name = Name, nodes = Nodes} |_], Name) -> Nodes;
dist_find_nodes([_ | T], Name) -> dist_find_nodes(T, Name);
dist_find_nodes([], _Name) -> [].

dist_flat_nodes(Appls, Name) ->
    flat_nodes(dist_find_nodes(Appls, Name)).

dist_del_node(Appls, Node) ->
    map(fun(Appl) ->
		NRun = filter(fun({N, _Runnable}) when N =:= Node -> false;
				 (_) -> true
			      end, Appl#appl.run),
		Appl#appl{run = NRun}
	end, Appls).

validRestartType(permanent)   -> true;
validRestartType(temporary)   -> true;
validRestartType(transient)   -> true;
validRestartType(_RestartType) -> false.

dist_mismatch(AppName, Node) ->
    error_msg("Distribution mismatch for application \"~p\" on nodes ~p and ~p~n",
	      [AppName, node(), Node]),
    exit({distribution_mismatch, AppName, Node}).

%error_msg(Format) when is_list(Format) ->
%    error_msg(Format, []).

error_msg(Format, ArgList) when is_list(Format), is_list(ArgList) ->
    error_logger:error_msg("dist_ac on node ~p:~n" ++ Format, [node()|ArgList]).

%info_msg(Format) when is_list(Format) ->
%    info_msg(Format, []).

%info_msg(Format, ArgList) when is_list(Format), is_list(ArgList) ->
%    error_logger:info_msg("dist_ac on node ~p:~n" ++ Format, [node()|ArgList]).

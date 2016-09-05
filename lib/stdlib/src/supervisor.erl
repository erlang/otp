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
-module(supervisor).

-behaviour(gen_server).

%% External exports
-export([start_link/2, start_link/3,
	 start_child/2, restart_child/2,
	 delete_child/2, terminate_child/2,
	 which_children/1, count_children/1,
	 check_childspecs/1, get_childspec/2]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).
-export([try_again_restart/2]).

%% For release_handler only
-export([get_callback_module/1]).

%%--------------------------------------------------------------------------

-export_type([sup_flags/0, child_spec/0, startchild_ret/0, strategy/0]).

%%--------------------------------------------------------------------------

-type child()    :: 'undefined' | pid().
-type child_id() :: term().
-type mfargs()   :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
-type modules()  :: [module()] | 'dynamic'.
-type restart()  :: 'permanent' | 'transient' | 'temporary'.
-type shutdown() :: 'brutal_kill' | timeout().
-type worker()   :: 'worker' | 'supervisor'.
-type sup_name() :: {'local', Name :: atom()}
                  | {'global', Name :: atom()}
                  | {'via', Module :: module(), Name :: any()}.
-type sup_ref()  :: (Name :: atom())
                  | {Name :: atom(), Node :: node()}
                  | {'global', Name :: atom()}
                  | {'via', Module :: module(), Name :: any()}
                  | pid().
-type child_spec() :: #{id := child_id(),       % mandatory
			start := mfargs(),      % mandatory
			restart => restart(),   % optional
			shutdown => shutdown(), % optional
			type => worker(),       % optional
			modules => modules()}   % optional
                    | {Id :: child_id(),
                       StartFunc :: mfargs(),
                       Restart :: restart(),
                       Shutdown :: shutdown(),
                       Type :: worker(),
                       Modules :: modules()}.

-type strategy() :: 'one_for_all' | 'one_for_one'
                  | 'rest_for_one' | 'simple_one_for_one'.

-type sup_flags() :: #{strategy => strategy(),         % optional
		       intensity => non_neg_integer(), % optional
		       period => pos_integer()}        % optional
                   | {RestartStrategy :: strategy(),
                      Intensity :: non_neg_integer(),
                      Period :: pos_integer()}.

%%--------------------------------------------------------------------------
%% Defaults
-define(default_flags, #{strategy  => one_for_one,
			 intensity => 1,
			 period    => 5}).
-define(default_child_spec, #{restart  => permanent,
			      type     => worker}).
%% Default 'shutdown' is 5000 for workers and infinity for supervisors.
%% Default 'modules' is [M], where M comes from the child's start {M,F,A}.

%%--------------------------------------------------------------------------

-record(child, {% pid is undefined when child is not running
	        pid = undefined :: child()
	                         | {restarting, pid() | undefined}
	                         | [pid()],
		name            :: child_id(),
		mfargs          :: mfargs(),
		restart_type    :: restart(),
		shutdown        :: shutdown(),
		child_type      :: worker(),
		modules = []    :: modules()}).
-type child_rec() :: #child{}.

-define(DICTS, dict).
-define(DICT, dict:dict).
-define(SETS, sets).
-define(SET, sets:set).

-record(state, {name,
		strategy               :: strategy() | 'undefined',
		children = []          :: [child_rec()],
                dynamics               :: {'dict', ?DICT(pid(), list())}
                                        | {'set', ?SET(pid())}
                                        | 'undefined',
		intensity              :: non_neg_integer() | 'undefined',
		period                 :: pos_integer() | 'undefined',
		restarts = [],
		dynamic_restarts = 0   :: non_neg_integer(),
	        module,
	        args}).
-type state() :: #state{}.

-define(is_simple(State), State#state.strategy =:= simple_one_for_one).

-callback init(Args :: term()) ->
    {ok, {SupFlags :: sup_flags(), [ChildSpec :: child_spec()]}}
    | ignore.

-define(restarting(_Pid_), {restarting,_Pid_}).

%%% ---------------------------------------------------
%%% This is a general process supervisor built upon gen_server.erl.
%%% Servers/processes should/could also be built using gen_server.erl.
%%% SupName = {local, atom()} | {global, atom()}.
%%% ---------------------------------------------------

-type startlink_err() :: {'already_started', pid()}
                         | {'shutdown', term()}
                         | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

-spec start_link(Module, Args) -> startlink_ret() when
      Module :: module(),
      Args :: term().
start_link(Mod, Args) ->
    gen_server:start_link(supervisor, {self, Mod, Args}, []).
 
-spec start_link(SupName, Module, Args) -> startlink_ret() when
      SupName :: sup_name(),
      Module :: module(),
      Args :: term().
start_link(SupName, Mod, Args) ->
    gen_server:start_link(SupName, supervisor, {SupName, Mod, Args}, []).
 
%%% ---------------------------------------------------
%%% Interface functions.
%%% ---------------------------------------------------

-type startchild_err() :: 'already_present'
			| {'already_started', Child :: child()} | term().
-type startchild_ret() :: {'ok', Child :: child()}
                        | {'ok', Child :: child(), Info :: term()}
			| {'error', startchild_err()}.

-spec start_child(SupRef, ChildSpec) -> startchild_ret() when
      SupRef :: sup_ref(),
      ChildSpec :: child_spec() | (List :: [term()]).
start_child(Supervisor, ChildSpec) ->
    call(Supervisor, {start_child, ChildSpec}).

-spec restart_child(SupRef, Id) -> Result when
      SupRef :: sup_ref(),
      Id :: child_id(),
      Result :: {'ok', Child :: child()}
              | {'ok', Child :: child(), Info :: term()}
              | {'error', Error},
      Error :: 'running' | 'restarting' | 'not_found' | 'simple_one_for_one' |
	       term().
restart_child(Supervisor, Name) ->
    call(Supervisor, {restart_child, Name}).

-spec delete_child(SupRef, Id) -> Result when
      SupRef :: sup_ref(),
      Id :: child_id(),
      Result :: 'ok' | {'error', Error},
      Error :: 'running' | 'restarting' | 'not_found' | 'simple_one_for_one'.
delete_child(Supervisor, Name) ->
    call(Supervisor, {delete_child, Name}).

%%-----------------------------------------------------------------
%% Func: terminate_child/2
%% Returns: ok | {error, Reason}
%%          Note that the child is *always* terminated in some
%%          way (maybe killed).
%%-----------------------------------------------------------------

-spec terminate_child(SupRef, Id) -> Result when
      SupRef :: sup_ref(),
      Id :: pid() | child_id(),
      Result :: 'ok' | {'error', Error},
      Error :: 'not_found' | 'simple_one_for_one'.
terminate_child(Supervisor, Name) ->
    call(Supervisor, {terminate_child, Name}).

-spec get_childspec(SupRef, Id) -> Result when
      SupRef :: sup_ref(),
      Id :: pid() | child_id(),
      Result :: {'ok', child_spec()} | {'error', Error},
      Error :: 'not_found'.
get_childspec(Supervisor, Name) ->
    call(Supervisor, {get_childspec, Name}).

-spec which_children(SupRef) -> [{Id,Child,Type,Modules}] when
      SupRef :: sup_ref(),
      Id :: child_id() | undefined,
      Child :: child() | 'restarting',
      Type :: worker(),
      Modules :: modules().
which_children(Supervisor) ->
    call(Supervisor, which_children).

-spec count_children(SupRef) -> PropListOfCounts when
      SupRef :: sup_ref(),
      PropListOfCounts :: [Count],
      Count :: {specs, ChildSpecCount :: non_neg_integer()}
             | {active, ActiveProcessCount :: non_neg_integer()}
             | {supervisors, ChildSupervisorCount :: non_neg_integer()}
             |{workers, ChildWorkerCount :: non_neg_integer()}.
count_children(Supervisor) ->
    call(Supervisor, count_children).

call(Supervisor, Req) ->
    gen_server:call(Supervisor, Req, infinity).

-spec check_childspecs(ChildSpecs) -> Result when
      ChildSpecs :: [child_spec()],
      Result :: 'ok' | {'error', Error :: term()}.
check_childspecs(ChildSpecs) when is_list(ChildSpecs) ->
    case check_startspec(ChildSpecs) of
	{ok, _} -> ok;
	Error -> {error, Error}
    end;
check_childspecs(X) -> {error, {badarg, X}}.

%%%-----------------------------------------------------------------
%%% Called by restart/2
-spec try_again_restart(SupRef, Child) -> ok when
      SupRef :: sup_ref(),
      Child :: child_id() | pid().
try_again_restart(Supervisor, Child) ->
    cast(Supervisor, {try_again_restart, Child}).

cast(Supervisor, Req) ->
    gen_server:cast(Supervisor, Req).

%%%-----------------------------------------------------------------
%%% Called by release_handler during upgrade
-spec get_callback_module(Pid) -> Module when
      Pid :: pid(),
      Module :: atom().
get_callback_module(Pid) ->
    {status, _Pid, {module, _Mod},
     [_PDict, _SysState, _Parent, _Dbg, Misc]} = sys:get_status(Pid),
    case lists:keyfind(supervisor, 1, Misc) of
	{supervisor, [{"Callback", Mod}]} ->
	    Mod;
	_ ->
	    [_Header, _Data, {data, [{"State", State}]} | _] = Misc,
	    State#state.module
    end.

%%% ---------------------------------------------------
%%% 
%%% Initialize the supervisor.
%%% 
%%% ---------------------------------------------------

-type init_sup_name() :: sup_name() | 'self'.

-type stop_rsn() :: {'shutdown', term()}
                  | {'bad_return', {module(),'init', term()}}
                  | {'bad_start_spec', term()}
                  | {'start_spec', term()}
                  | {'supervisor_data', term()}.

-spec init({init_sup_name(), module(), [term()]}) ->
        {'ok', state()} | 'ignore' | {'stop', stop_rsn()}.

init({SupName, Mod, Args}) ->
    process_flag(trap_exit, true),
    case Mod:init(Args) of
	{ok, {SupFlags, StartSpec}} ->
	    case init_state(SupName, SupFlags, Mod, Args) of
		{ok, State} when ?is_simple(State) ->
		    init_dynamic(State, StartSpec);
		{ok, State} ->
		    init_children(State, StartSpec);
		Error ->
		    {stop, {supervisor_data, Error}}
	    end;
	ignore ->
	    ignore;
	Error ->
	    {stop, {bad_return, {Mod, init, Error}}}
    end.

init_children(State, StartSpec) ->
    SupName = State#state.name,
    case check_startspec(StartSpec) of
        {ok, Children} ->
            case start_children(Children, SupName) of
                {ok, NChildren} ->
                    {ok, State#state{children = NChildren}};
                {error, NChildren, Reason} ->
                    _ = terminate_children(NChildren, SupName),
                    {stop, {shutdown, Reason}}
            end;
        Error ->
            {stop, {start_spec, Error}}
    end.

init_dynamic(State, [StartSpec]) ->
    case check_startspec([StartSpec]) of
        {ok, Children} ->
	    {ok, State#state{children = Children}};
        Error ->
            {stop, {start_spec, Error}}
    end;
init_dynamic(_State, StartSpec) ->
    {stop, {bad_start_spec, StartSpec}}.

%%-----------------------------------------------------------------
%% Func: start_children/2
%% Args: Children = [child_rec()] in start order
%%       SupName = {local, atom()} | {global, atom()} | {pid(), Mod}
%% Purpose: Start all children.  The new list contains #child's
%%          with pids.
%% Returns: {ok, NChildren} | {error, NChildren, Reason}
%%          NChildren = [child_rec()] in termination order (reversed
%%                        start order)
%%-----------------------------------------------------------------
start_children(Children, SupName) -> start_children(Children, [], SupName).

start_children([Child|Chs], NChildren, SupName) ->
    case do_start_child(SupName, Child) of
	{ok, undefined} when Child#child.restart_type =:= temporary ->
	    start_children(Chs, NChildren, SupName);
	{ok, Pid} ->
	    start_children(Chs, [Child#child{pid = Pid}|NChildren], SupName);
	{ok, Pid, _Extra} ->
	    start_children(Chs, [Child#child{pid = Pid}|NChildren], SupName);
	{error, Reason} ->
	    report_error(start_error, Reason, Child, SupName),
	    {error, lists:reverse(Chs) ++ [Child | NChildren],
	     {failed_to_start_child,Child#child.name,Reason}}
    end;
start_children([], NChildren, _SupName) ->
    {ok, NChildren}.

do_start_child(SupName, Child) ->
    #child{mfargs = {M, F, Args}} = Child,
    case catch apply(M, F, Args) of
	{ok, Pid} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress(NChild, SupName),
	    {ok, Pid};
	{ok, Pid, Extra} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress(NChild, SupName),
	    {ok, Pid, Extra};
	ignore ->
	    {ok, undefined};
	{error, What} -> {error, What};
	What -> {error, What}
    end.

do_start_child_i(M, F, A) ->
    case catch apply(M, F, A) of
	{ok, Pid} when is_pid(Pid) ->
	    {ok, Pid};
	{ok, Pid, Extra} when is_pid(Pid) ->
	    {ok, Pid, Extra};
	ignore ->
	    {ok, undefined};
	{error, Error} ->
	    {error, Error};
	What ->
	    {error, What}
    end.

%%% ---------------------------------------------------
%%% 
%%% Callback functions.
%%% 
%%% ---------------------------------------------------
-type call() :: 'which_children' | 'count_children' | {_, _}.	% XXX: refine
-spec handle_call(call(), term(), state()) -> {'reply', term(), state()}.

handle_call({start_child, EArgs}, _From, State) when ?is_simple(State) ->
    Child = hd(State#state.children),
    #child{mfargs = {M, F, A}} = Child,
    Args = A ++ EArgs,
    case do_start_child_i(M, F, Args) of
	{ok, undefined} ->
	    {reply, {ok, undefined}, State};
	{ok, Pid} ->
	    NState = save_dynamic_child(Child#child.restart_type, Pid, Args, State),
	    {reply, {ok, Pid}, NState};
	{ok, Pid, Extra} ->
	    NState = save_dynamic_child(Child#child.restart_type, Pid, Args, State),
	    {reply, {ok, Pid, Extra}, NState};
	What ->
	    {reply, What, State}
    end;

handle_call({start_child, ChildSpec}, _From, State) ->
    case check_childspec(ChildSpec) of
	{ok, Child} ->
	    {Resp, NState} = handle_start_child(Child, State),
	    {reply, Resp, NState};
	What ->
	    {reply, {error, What}, State}
    end;

%% terminate_child for simple_one_for_one can only be done with pid
handle_call({terminate_child, Name}, _From, State) when not is_pid(Name),
							?is_simple(State) ->
    {reply, {error, simple_one_for_one}, State};

handle_call({terminate_child, Name}, _From, State) ->
    case get_child(Name, State, ?is_simple(State)) of
	{value, Child} ->
	    case do_terminate(Child, State#state.name) of
		#child{restart_type=RT} when RT=:=temporary; ?is_simple(State) ->
		    {reply, ok, state_del_child(Child, State)};
		NChild ->
		    {reply, ok, replace_child(NChild, State)}
		end;
	false ->
	    {reply, {error, not_found}, State}
    end;

%% restart_child request is invalid for simple_one_for_one supervisors
handle_call({restart_child, _Name}, _From, State) when ?is_simple(State) ->
    {reply, {error, simple_one_for_one}, State};

handle_call({restart_child, Name}, _From, State) ->
    case get_child(Name, State) of
	{value, Child} when Child#child.pid =:= undefined ->
	    case do_start_child(State#state.name, Child) of
		{ok, Pid} ->
		    NState = replace_child(Child#child{pid = Pid}, State),
		    {reply, {ok, Pid}, NState};
		{ok, Pid, Extra} ->
		    NState = replace_child(Child#child{pid = Pid}, State),
		    {reply, {ok, Pid, Extra}, NState};
		Error ->
		    {reply, Error, State}
	    end;
	{value, #child{pid=?restarting(_)}} ->
	    {reply, {error, restarting}, State};
	{value, _} ->
	    {reply, {error, running}, State};
	_ ->
	    {reply, {error, not_found}, State}
    end;

%% delete_child request is invalid for simple_one_for_one supervisors
handle_call({delete_child, _Name}, _From, State) when ?is_simple(State) ->
    {reply, {error, simple_one_for_one}, State};

handle_call({delete_child, Name}, _From, State) ->
    case get_child(Name, State) of
	{value, Child} when Child#child.pid =:= undefined ->
	    NState = remove_child(Child, State),
	    {reply, ok, NState};
	{value, #child{pid=?restarting(_)}} ->
	    {reply, {error, restarting}, State};
	{value, _} ->
	    {reply, {error, running}, State};
	_ ->
	    {reply, {error, not_found}, State}
    end;

handle_call({get_childspec, Name}, _From, State) ->
    case get_child(Name, State, ?is_simple(State)) of
	{value, Child} ->
            {reply, {ok, child_to_spec(Child)}, State};
	false ->
	    {reply, {error, not_found}, State}
    end;

handle_call(which_children, _From, #state{children = [#child{restart_type = temporary,
							     child_type = CT,
							     modules = Mods}]} =
		State) when ?is_simple(State) ->
    Reply = lists:map(fun(Pid) -> {undefined, Pid, CT, Mods} end,
                      ?SETS:to_list(dynamics_db(temporary, State#state.dynamics))),
    {reply, Reply, State};

handle_call(which_children, _From, #state{children = [#child{restart_type = RType,
							 child_type = CT,
							 modules = Mods}]} =
		State) when ?is_simple(State) ->
    Reply = lists:map(fun({?restarting(_),_}) -> {undefined,restarting,CT,Mods};
			 ({Pid, _}) -> {undefined, Pid, CT, Mods} end,
		      ?DICTS:to_list(dynamics_db(RType, State#state.dynamics))),
    {reply, Reply, State};

handle_call(which_children, _From, State) ->
    Resp =
	lists:map(fun(#child{pid = ?restarting(_), name = Name,
			     child_type = ChildType, modules = Mods}) ->
			  {Name, restarting, ChildType, Mods};
		     (#child{pid = Pid, name = Name,
			     child_type = ChildType, modules = Mods}) ->
			  {Name, Pid, ChildType, Mods}
		  end,
		  State#state.children),
    {reply, Resp, State};


handle_call(count_children, _From, #state{children = [#child{restart_type = temporary,
							     child_type = CT}]} = State)
  when ?is_simple(State) ->
    Sz = ?SETS:size(dynamics_db(temporary, State#state.dynamics)),
    Reply = case CT of
		supervisor -> [{specs, 1}, {active, Sz},
			       {supervisors, Sz}, {workers, 0}];
		worker -> [{specs, 1}, {active, Sz},
			   {supervisors, 0}, {workers, Sz}]
	    end,
    {reply, Reply, State};

handle_call(count_children, _From,  #state{dynamic_restarts = Restarts,
					   children = [#child{restart_type = RType,
							      child_type = CT}]} = State)
  when ?is_simple(State) ->
    Sz = ?DICTS:size(dynamics_db(RType, State#state.dynamics)),
    Active = Sz - Restarts,
    Reply = case CT of
		supervisor -> [{specs, 1}, {active, Active},
			       {supervisors, Sz}, {workers, 0}];
		worker -> [{specs, 1}, {active, Active},
			   {supervisors, 0}, {workers, Sz}]
	    end,
    {reply, Reply, State};

handle_call(count_children, _From, State) ->
    %% Specs and children are together on the children list...
    {Specs, Active, Supers, Workers} =
	lists:foldl(fun(Child, Counts) ->
			   count_child(Child, Counts)
		   end, {0,0,0,0}, State#state.children),

    %% Reformat counts to a property list.
    Reply = [{specs, Specs}, {active, Active},
	     {supervisors, Supers}, {workers, Workers}],
    {reply, Reply, State}.


count_child(#child{pid = Pid, child_type = worker},
	    {Specs, Active, Supers, Workers}) ->
    case is_pid(Pid) andalso is_process_alive(Pid) of
	true ->  {Specs+1, Active+1, Supers, Workers+1};
	false -> {Specs+1, Active, Supers, Workers+1}
    end;
count_child(#child{pid = Pid, child_type = supervisor},
	    {Specs, Active, Supers, Workers}) ->
    case is_pid(Pid) andalso is_process_alive(Pid) of
	true ->  {Specs+1, Active+1, Supers+1, Workers};
	false -> {Specs+1, Active, Supers+1, Workers}
    end.


%%% If a restart attempt failed, this message is cast
%%% from restart/2 in order to give gen_server the chance to
%%% check it's inbox before trying again.
-spec handle_cast({try_again_restart, child_id() | pid()}, state()) ->
			 {'noreply', state()} | {stop, shutdown, state()}.

handle_cast({try_again_restart,Pid}, #state{children=[Child]}=State)
  when ?is_simple(State) ->
    RT = Child#child.restart_type,
    RPid = restarting(Pid),
    case dynamic_child_args(RPid, RT, State#state.dynamics) of
	{ok, Args} ->
	    {M, F, _} = Child#child.mfargs,
	    NChild = Child#child{pid = RPid, mfargs = {M, F, Args}},
	    case restart(NChild,State) of
		{ok, State1} ->
		    {noreply, State1};
		{shutdown, State1} ->
		    {stop, shutdown, State1}
	    end;
	error ->
            {noreply, State}
    end;

handle_cast({try_again_restart,Name}, State) ->
    case lists:keyfind(Name,#child.name,State#state.children) of
	Child = #child{pid=?restarting(_)} ->
	    case restart(Child,State) of
		{ok, State1} ->
		    {noreply, State1};
		{shutdown, State1} ->
		    {stop, shutdown, State1}
	    end;
	_ ->
	    {noreply,State}
    end.

%%
%% Take care of terminated children.
%%
-spec handle_info(term(), state()) ->
        {'noreply', state()} | {'stop', 'shutdown', state()}.

handle_info({'EXIT', Pid, Reason}, State) ->
    case restart_child(Pid, Reason, State) of
	{ok, State1} ->
	    {noreply, State1};
	{shutdown, State1} ->
	    {stop, shutdown, State1}
    end;

handle_info(Msg, State) ->
    error_logger:error_msg("Supervisor received unexpected message: ~p~n", 
			   [Msg]),
    {noreply, State}.

%%
%% Terminate this server.
%%
-spec terminate(term(), state()) -> 'ok'.

terminate(_Reason, #state{children=[Child]} = State) when ?is_simple(State) ->
    terminate_dynamic_children(Child, dynamics_db(Child#child.restart_type,
                                                  State#state.dynamics),
                               State#state.name);
terminate(_Reason, State) ->
    terminate_children(State#state.children, State#state.name).

%%
%% Change code for the supervisor.
%% Call the new call-back module and fetch the new start specification.
%% Combine the new spec. with the old. If the new start spec. is
%% not valid the code change will not succeed.
%% Use the old Args as argument to Module:init/1.
%% NOTE: This requires that the init function of the call-back module
%%       does not have any side effects.
%%
-spec code_change(term(), state(), term()) ->
        {'ok', state()} | {'error', term()}.

code_change(_, State, _) ->
    case (State#state.module):init(State#state.args) of
	{ok, {SupFlags, StartSpec}} ->
	    case set_flags(SupFlags, State) of
		{ok, State1}  ->
                    update_childspec(State1, StartSpec);
		{invalid_type, SupFlags} ->
		    {error, {bad_flags, SupFlags}}; % backwards compatibility
		Error ->
		    {error, Error}
	    end;
	ignore ->
	    {ok, State};
	Error ->
	    Error
    end.

update_childspec(State, StartSpec) when ?is_simple(State) ->
    case check_startspec(StartSpec) of
        {ok, [Child]} ->
            {ok, State#state{children = [Child]}};
        Error ->
            {error, Error}
    end;
update_childspec(State, StartSpec) ->
    case check_startspec(StartSpec) of
	{ok, Children} ->
	    OldC = State#state.children, % In reverse start order !
	    NewC = update_childspec1(OldC, Children, []),
	    {ok, State#state{children = NewC}};
        Error ->
	    {error, Error}
    end.

update_childspec1([Child|OldC], Children, KeepOld) ->
    case update_chsp(Child, Children) of
	{ok,NewChildren} ->
	    update_childspec1(OldC, NewChildren, KeepOld);
	false ->
	    update_childspec1(OldC, Children, [Child|KeepOld])
    end;
update_childspec1([], Children, KeepOld) ->
    %% Return them in (kept) reverse start order.
    lists:reverse(Children ++ KeepOld).

update_chsp(OldCh, Children) ->
    case lists:map(fun(Ch) when OldCh#child.name =:= Ch#child.name ->
			   Ch#child{pid = OldCh#child.pid};
		      (Ch) ->
			   Ch
		   end,
		   Children) of
	Children ->
	    false;  % OldCh not found in new spec.
	NewC ->
	    {ok, NewC}
    end.
    
%%% ---------------------------------------------------
%%% Start a new child.
%%% ---------------------------------------------------

handle_start_child(Child, State) ->
    case get_child(Child#child.name, State) of
	false ->
	    case do_start_child(State#state.name, Child) of
		{ok, undefined} when Child#child.restart_type =:= temporary ->
		    {{ok, undefined}, State};
		{ok, Pid} ->
		    {{ok, Pid}, save_child(Child#child{pid = Pid}, State)};
		{ok, Pid, Extra} ->
		    {{ok, Pid, Extra}, save_child(Child#child{pid = Pid}, State)};
		{error, What} ->
		    {{error, {What, Child}}, State}
	    end;
	{value, OldChild} when is_pid(OldChild#child.pid) ->
	    {{error, {already_started, OldChild#child.pid}}, State};
	{value, _OldChild} ->
	    {{error, already_present}, State}
    end.

%%% ---------------------------------------------------
%%% Restart. A process has terminated.
%%% Returns: {ok, state()} | {shutdown, state()}
%%% ---------------------------------------------------

restart_child(Pid, Reason, #state{children = [Child]} = State) when ?is_simple(State) ->
    RestartType = Child#child.restart_type,
    case dynamic_child_args(Pid, RestartType, State#state.dynamics) of
	{ok, Args} ->
	    {M, F, _} = Child#child.mfargs,
	    NChild = Child#child{pid = Pid, mfargs = {M, F, Args}},
	    do_restart(RestartType, Reason, NChild, State);
	error ->
            {ok, State}
    end;

restart_child(Pid, Reason, State) ->
    Children = State#state.children,
    case lists:keyfind(Pid, #child.pid, Children) of
	#child{restart_type = RestartType} = Child ->
	    do_restart(RestartType, Reason, Child, State);
	false ->
	    {ok, State}
    end.

do_restart(permanent, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    restart(Child, State);
do_restart(_, normal, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(_, shutdown, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(_, {shutdown, _Term}, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(transient, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    restart(Child, State);
do_restart(temporary, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    NState = state_del_child(Child, State),
    {ok, NState}.

restart(Child, State) ->
    case add_restart(State) of
	{ok, NState} ->
	    case restart(NState#state.strategy, Child, NState) of
		{try_again,NState2} ->
		    %% Leaving control back to gen_server before
		    %% trying again. This way other incoming requsts
		    %% for the supervisor can be handled - e.g. a
		    %% shutdown request for the supervisor or the
		    %% child.
		    Id = if ?is_simple(State) -> Child#child.pid;
			    true -> Child#child.name
			 end,
		    ok = try_again_restart(self(), Id),
		    {ok,NState2};
		{try_again, NState2, #child{name=ChName}} ->
		    ok = try_again_restart(self(), ChName),
		    {ok,NState2};
		Other ->
		    Other
	    end;
	{terminate, NState} ->
	    report_error(shutdown, reached_max_restart_intensity,
			 Child, State#state.name),
	    {shutdown, remove_child(Child, NState)}
    end.

restart(simple_one_for_one, Child, State0) ->
    #child{pid = OldPid, mfargs = {M, F, A}} = Child,
    State = case OldPid of
		?restarting(_) ->
		    NRes = State0#state.dynamic_restarts - 1,
		    State0#state{dynamic_restarts = NRes};
		_ ->
		    State0
	    end,
    Dynamics = ?DICTS:erase(OldPid, dynamics_db(Child#child.restart_type,
					       State#state.dynamics)),
    case do_start_child_i(M, F, A) of
	{ok, Pid} ->
            DynamicsDb = {dict, ?DICTS:store(Pid, A, Dynamics)},
	    NState = State#state{dynamics = DynamicsDb},
	    {ok, NState};
	{ok, Pid, _Extra} ->
            DynamicsDb = {dict, ?DICTS:store(Pid, A, Dynamics)},
	    NState = State#state{dynamics = DynamicsDb},
	    {ok, NState};
	{error, Error} ->
	    NRestarts = State#state.dynamic_restarts + 1,
            DynamicsDb = {dict, ?DICTS:store(restarting(OldPid), A, Dynamics)},
	    NState = State#state{dynamic_restarts = NRestarts,
				 dynamics = DynamicsDb},
	    report_error(start_error, Error, Child, State#state.name),
	    {try_again, NState}
    end;
restart(one_for_one, Child, State) ->
    OldPid = Child#child.pid,
    case do_start_child(State#state.name, Child) of
	{ok, Pid} ->
	    NState = replace_child(Child#child{pid = Pid}, State),
	    {ok, NState};
	{ok, Pid, _Extra} ->
	    NState = replace_child(Child#child{pid = Pid}, State),
	    {ok, NState};
	{error, Reason} ->
	    NState = replace_child(Child#child{pid = restarting(OldPid)}, State),
	    report_error(start_error, Reason, Child, State#state.name),
	    {try_again, NState}
    end;
restart(rest_for_one, Child, State) ->
    {ChAfter, ChBefore} = split_child(Child#child.pid, State#state.children),
    ChAfter2 = terminate_children(ChAfter, State#state.name),
    case start_children(ChAfter2, State#state.name) of
	{ok, ChAfter3} ->
	    {ok, State#state{children = ChAfter3 ++ ChBefore}};
	{error, ChAfter3, {failed_to_start_child, ChName, _Reason}}
	  when ChName =:= Child#child.name ->
	    NChild = Child#child{pid=restarting(Child#child.pid)},
	    NState = State#state{children = ChAfter3 ++ ChBefore},
	    {try_again, replace_child(NChild,NState)};
	{error, ChAfter3, {failed_to_start_child, ChName, _Reason}} ->
	    NChild = lists:keyfind(ChName, #child.name, ChAfter3),
	    NChild2 = NChild#child{pid=?restarting(undefined)},
	    NState = State#state{children = ChAfter3 ++ ChBefore},
	    {try_again, replace_child(NChild2,NState), NChild2}
    end;
restart(one_for_all, Child, State) ->
    Children1 = del_child(Child#child.pid, State#state.children),
    Children2 = terminate_children(Children1, State#state.name),
    case start_children(Children2, State#state.name) of
	{ok, NChs} ->
	    {ok, State#state{children = NChs}};
	{error, NChs, {failed_to_start_child, ChName, _Reason}}
	  when ChName =:= Child#child.name ->
	    NChild = Child#child{pid=restarting(Child#child.pid)},
	    NState = State#state{children = NChs},
	    {try_again, replace_child(NChild,NState)};
	{error, NChs, {failed_to_start_child, ChName, _Reason}} ->
	    NChild = lists:keyfind(ChName, #child.name, NChs),
	    NChild2 = NChild#child{pid=?restarting(undefined)},
	    NState = State#state{children = NChs},
	    {try_again, replace_child(NChild2,NState), NChild2}
    end.

restarting(Pid) when is_pid(Pid) -> ?restarting(Pid);
restarting(RPid) -> RPid.

%%-----------------------------------------------------------------
%% Func: terminate_children/2
%% Args: Children = [child_rec()] in termination order
%%       SupName = {local, atom()} | {global, atom()} | {pid(),Mod}
%% Returns: NChildren = [child_rec()] in
%%          startup order (reversed termination order)
%%-----------------------------------------------------------------
terminate_children(Children, SupName) ->
    terminate_children(Children, SupName, []).

%% Temporary children should not be restarted and thus should
%% be skipped when building the list of terminated children, although
%% we do want them to be shut down as many functions from this module
%% use this function to just clear everything.
terminate_children([Child = #child{restart_type=temporary} | Children], SupName, Res) ->
    _ = do_terminate(Child, SupName),
    terminate_children(Children, SupName, Res);
terminate_children([Child | Children], SupName, Res) ->
    NChild = do_terminate(Child, SupName),
    terminate_children(Children, SupName, [NChild | Res]);
terminate_children([], _SupName, Res) ->
    Res.

do_terminate(Child, SupName) when is_pid(Child#child.pid) ->
    case shutdown(Child#child.pid, Child#child.shutdown) of
        ok ->
            ok;
        {error, normal} when Child#child.restart_type =/= permanent ->
            ok;
        {error, OtherReason} ->
            report_error(shutdown_error, OtherReason, Child, SupName)
    end,
    Child#child{pid = undefined};
do_terminate(Child, _SupName) ->
    Child#child{pid = undefined}.

%%-----------------------------------------------------------------
%% Shutdowns a child. We must check the EXIT value 
%% of the child, because it might have died with another reason than
%% the wanted. In that case we want to report the error. We put a 
%% monitor on the child an check for the 'DOWN' message instead of 
%% checking for the 'EXIT' message, because if we check the 'EXIT' 
%% message a "naughty" child, who does unlink(Sup), could hang the 
%% supervisor. 
%% Returns: ok | {error, OtherReason}  (this should be reported)
%%-----------------------------------------------------------------
shutdown(Pid, brutal_kill) ->
    case monitor_child(Pid) of
	ok ->
	    exit(Pid, kill),
	    receive
		{'DOWN', _MRef, process, Pid, killed} ->
		    ok;
		{'DOWN', _MRef, process, Pid, OtherReason} ->
		    {error, OtherReason}
	    end;
	{error, Reason} ->      
	    {error, Reason}
    end;
shutdown(Pid, Time) ->
    case monitor_child(Pid) of
	ok ->
	    exit(Pid, shutdown), %% Try to shutdown gracefully
	    receive 
		{'DOWN', _MRef, process, Pid, shutdown} ->
		    ok;
		{'DOWN', _MRef, process, Pid, OtherReason} ->
		    {error, OtherReason}
	    after Time ->
		    exit(Pid, kill),  %% Force termination.
		    receive
			{'DOWN', _MRef, process, Pid, OtherReason} ->
			    {error, OtherReason}
		    end
	    end;
	{error, Reason} ->      
	    {error, Reason}
    end.

%% Help function to shutdown/2 switches from link to monitor approach
monitor_child(Pid) ->
    
    %% Do the monitor operation first so that if the child dies 
    %% before the monitoring is done causing a 'DOWN'-message with
    %% reason noproc, we will get the real reason in the 'EXIT'-message
    %% unless a naughty child has already done unlink...
    erlang:monitor(process, Pid),
    unlink(Pid),

    receive
	%% If the child dies before the unlik we must empty
	%% the mail-box of the 'EXIT'-message and the 'DOWN'-message.
	{'EXIT', Pid, Reason} -> 
	    receive 
		{'DOWN', _, process, Pid, _} ->
		    {error, Reason}
	    end
    after 0 -> 
	    %% If a naughty child did unlink and the child dies before
	    %% monitor the result will be that shutdown/2 receives a 
	    %% 'DOWN'-message with reason noproc.
	    %% If the child should die after the unlink there
	    %% will be a 'DOWN'-message with a correct reason
	    %% that will be handled in shutdown/2. 
	    ok   
    end.


%%-----------------------------------------------------------------
%% Func: terminate_dynamic_children/3
%% Args: Child    = child_rec()
%%       Dynamics = ?DICT() | ?SET()
%%       SupName  = {local, atom()} | {global, atom()} | {pid(),Mod}
%% Returns: ok
%%
%%
%% Shutdown all dynamic children. This happens when the supervisor is
%% stopped. Because the supervisor can have millions of dynamic children, we
%% can have an significative overhead here.
%%-----------------------------------------------------------------
terminate_dynamic_children(Child, Dynamics, SupName) ->
    {Pids, EStack0} = monitor_dynamic_children(Child, Dynamics),
    Sz = ?SETS:size(Pids),
    EStack = case Child#child.shutdown of
                 brutal_kill ->
                     ?SETS:fold(fun(P, _) -> exit(P, kill) end, ok, Pids),
                     wait_dynamic_children(Child, Pids, Sz, undefined, EStack0);
                 infinity ->
                     ?SETS:fold(fun(P, _) -> exit(P, shutdown) end, ok, Pids),
                     wait_dynamic_children(Child, Pids, Sz, undefined, EStack0);
                 Time ->
                     ?SETS:fold(fun(P, _) -> exit(P, shutdown) end, ok, Pids),
                     TRef = erlang:start_timer(Time, self(), kill),
                     wait_dynamic_children(Child, Pids, Sz, TRef, EStack0)
             end,
    %% Unroll stacked errors and report them
    ?DICTS:fold(fun(Reason, Ls, _) ->
                       report_error(shutdown_error, Reason,
                                    Child#child{pid=Ls}, SupName)
               end, ok, EStack).


monitor_dynamic_children(#child{restart_type=temporary}, Dynamics) ->
    ?SETS:fold(fun(P, {Pids, EStack}) ->
                       case monitor_child(P) of
                           ok ->
                               {?SETS:add_element(P, Pids), EStack};
                           {error, normal} ->
                               {Pids, EStack};
                           {error, Reason} ->
                               {Pids, ?DICTS:append(Reason, P, EStack)}
                       end
               end, {?SETS:new(), ?DICTS:new()}, Dynamics);
monitor_dynamic_children(#child{restart_type=RType}, Dynamics) ->
    ?DICTS:fold(fun(P, _, {Pids, EStack}) when is_pid(P) ->
                       case monitor_child(P) of
                           ok ->
                               {?SETS:add_element(P, Pids), EStack};
                           {error, normal} when RType =/= permanent ->
                               {Pids, EStack};
                           {error, Reason} ->
                               {Pids, ?DICTS:append(Reason, P, EStack)}
                       end;
		  (?restarting(_), _, {Pids, EStack}) ->
		       {Pids, EStack}
               end, {?SETS:new(), ?DICTS:new()}, Dynamics).


wait_dynamic_children(_Child, _Pids, 0, undefined, EStack) ->
    EStack;
wait_dynamic_children(_Child, _Pids, 0, TRef, EStack) ->
	%% If the timer has expired before its cancellation, we must empty the
	%% mail-box of the 'timeout'-message.
    _ = erlang:cancel_timer(TRef),
    receive
        {timeout, TRef, kill} ->
            EStack
    after 0 ->
            EStack
    end;
wait_dynamic_children(#child{shutdown=brutal_kill} = Child, Pids, Sz,
                      TRef, EStack) ->
    receive
        {'DOWN', _MRef, process, Pid, killed} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                                  TRef, EStack);

        {'DOWN', _MRef, process, Pid, Reason} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                                  TRef, ?DICTS:append(Reason, Pid, EStack))
    end;
wait_dynamic_children(#child{restart_type=RType} = Child, Pids, Sz,
                      TRef, EStack) ->
    receive
        {'DOWN', _MRef, process, Pid, shutdown} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                                  TRef, EStack);

        {'DOWN', _MRef, process, Pid, {shutdown, _}} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                                  TRef, EStack);

        {'DOWN', _MRef, process, Pid, normal} when RType =/= permanent ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                                  TRef, EStack);

        {'DOWN', _MRef, process, Pid, Reason} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                                  TRef, ?DICTS:append(Reason, Pid, EStack));

        {timeout, TRef, kill} ->
            ?SETS:fold(fun(P, _) -> exit(P, kill) end, ok, Pids),
            wait_dynamic_children(Child, Pids, Sz, undefined, EStack)
    end.

%%-----------------------------------------------------------------
%% Child/State manipulating functions.
%%-----------------------------------------------------------------

%% Note we do not want to save the parameter list for temporary processes as
%% they will not be restarted, and hence we do not need this information.
%% Especially for dynamic children to simple_one_for_one supervisors
%% it could become very costly as it is not uncommon to spawn
%% very many such processes.
save_child(#child{restart_type = temporary,
		  mfargs = {M, F, _}} = Child, #state{children = Children} = State) ->
    State#state{children = [Child#child{mfargs = {M, F, undefined}} |Children]};
save_child(Child, #state{children = Children} = State) ->
    State#state{children = [Child |Children]}.

save_dynamic_child(temporary, Pid, _, #state{dynamics = Dynamics} = State) ->
    DynamicsDb = dynamics_db(temporary, Dynamics),
    State#state{dynamics = {set, ?SETS:add_element(Pid, DynamicsDb)}};
save_dynamic_child(RestartType, Pid, Args, #state{dynamics = Dynamics} = State) ->
    DynamicsDb = dynamics_db(RestartType, Dynamics),
    State#state{dynamics = {dict, ?DICTS:store(Pid, Args, DynamicsDb)}}.

dynamics_db(temporary, undefined) ->
    ?SETS:new();
dynamics_db(_, undefined) ->
    ?DICTS:new();
dynamics_db(_, {_Tag, DynamicsDb}) ->
    DynamicsDb.

dynamic_child_args(_Pid, temporary, _DynamicsDb) ->
    {ok, undefined};
dynamic_child_args(Pid, _RT, {dict, DynamicsDb}) ->
    ?DICTS:find(Pid, DynamicsDb);
dynamic_child_args(_Pid, _RT, undefined) ->
    error.

state_del_child(#child{pid = Pid, restart_type = temporary}, State) when ?is_simple(State) ->
    NDynamics = ?SETS:del_element(Pid, dynamics_db(temporary, State#state.dynamics)),
    State#state{dynamics = {set, NDynamics}};
state_del_child(#child{pid = Pid, restart_type = RType}, State) when ?is_simple(State) ->
    NDynamics = ?DICTS:erase(Pid, dynamics_db(RType, State#state.dynamics)),
    State#state{dynamics = {dict, NDynamics}};
state_del_child(Child, State) ->
    NChildren = del_child(Child#child.name, State#state.children),
    State#state{children = NChildren}.

del_child(Name, [Ch|Chs]) when Ch#child.name =:= Name, Ch#child.restart_type =:= temporary ->
    Chs;
del_child(Name, [Ch|Chs]) when Ch#child.name =:= Name ->
    [Ch#child{pid = undefined} | Chs];
del_child(Pid, [Ch|Chs]) when Ch#child.pid =:= Pid, Ch#child.restart_type =:= temporary ->
    Chs;
del_child(Pid, [Ch|Chs]) when Ch#child.pid =:= Pid ->
    [Ch#child{pid = undefined} | Chs];
del_child(Name, [Ch|Chs]) ->
    [Ch|del_child(Name, Chs)];
del_child(_, []) ->
    [].

%% Chs = [S4, S3, Ch, S1, S0]
%% Ret: {[S4, S3, Ch], [S1, S0]}
split_child(Name, Chs) ->
    split_child(Name, Chs, []).

split_child(Name, [Ch|Chs], After) when Ch#child.name =:= Name ->
    {lists:reverse([Ch#child{pid = undefined} | After]), Chs};
split_child(Pid, [Ch|Chs], After) when Ch#child.pid =:= Pid ->
    {lists:reverse([Ch#child{pid = undefined} | After]), Chs};
split_child(Name, [Ch|Chs], After) ->
    split_child(Name, Chs, [Ch | After]);
split_child(_, [], After) ->
    {lists:reverse(After), []}.

get_child(Name, State) ->
    get_child(Name, State, false).

get_child(Pid, State, AllowPid) when AllowPid, is_pid(Pid) ->
    get_dynamic_child(Pid, State);
get_child(Name, State, _) ->
    lists:keysearch(Name, #child.name, State#state.children).

get_dynamic_child(Pid, #state{children=[Child], dynamics=Dynamics}) ->
    case is_dynamic_pid(Pid, Dynamics) of
	true ->
	    {value, Child#child{pid=Pid}};
	false ->
	    RPid = restarting(Pid),
	    case is_dynamic_pid(RPid, Dynamics) of
		true ->
		    {value, Child#child{pid=RPid}};
		false ->
		    case erlang:is_process_alive(Pid) of
			true -> false;
			false -> {value, Child}
		    end
	    end
    end.

is_dynamic_pid(Pid, {dict, Dynamics}) ->
    ?DICTS:is_key(Pid, Dynamics);
is_dynamic_pid(Pid, {set, Dynamics}) ->
    ?SETS:is_element(Pid, Dynamics);
is_dynamic_pid(_Pid, undefined) ->
    false.

replace_child(Child, State) ->
    Chs = do_replace_child(Child, State#state.children),
    State#state{children = Chs}.

do_replace_child(Child, [Ch|Chs]) when Ch#child.name =:= Child#child.name ->
    [Child | Chs];
do_replace_child(Child, [Ch|Chs]) ->
    [Ch|do_replace_child(Child, Chs)].

remove_child(Child, State) ->
    Chs = lists:keydelete(Child#child.name, #child.name, State#state.children),
    State#state{children = Chs}.

%%-----------------------------------------------------------------
%% Func: init_state/4
%% Args: SupName = {local, atom()} | {global, atom()} | self
%%       Type = {Strategy, MaxIntensity, Period}
%%         Strategy = one_for_one | one_for_all | simple_one_for_one |
%%                    rest_for_one
%%         MaxIntensity = integer() >= 0
%%         Period = integer() > 0
%%       Mod :== atom()
%%       Args :== term()
%% Purpose: Check that Type is of correct type (!)
%% Returns: {ok, state()} | Error
%%-----------------------------------------------------------------
init_state(SupName, Type, Mod, Args) ->
    set_flags(Type, #state{name = supname(SupName,Mod),
			   module = Mod,
			   args = Args}).

set_flags(Flags, State) ->
    try check_flags(Flags) of
	#{strategy := Strategy, intensity := MaxIntensity, period := Period} ->
	    {ok, State#state{strategy = Strategy,
			     intensity = MaxIntensity,
			     period = Period}}
    catch
	Thrown -> Thrown
    end.

check_flags(SupFlags) when is_map(SupFlags) ->
    do_check_flags(maps:merge(?default_flags,SupFlags));
check_flags({Strategy, MaxIntensity, Period}) ->
    check_flags(#{strategy => Strategy,
		  intensity => MaxIntensity,
		  period => Period});
check_flags(What) ->
    throw({invalid_type, What}).

do_check_flags(#{strategy := Strategy,
		 intensity := MaxIntensity,
		 period := Period} = Flags) ->
    validStrategy(Strategy),
    validIntensity(MaxIntensity),
    validPeriod(Period),
    Flags.

validStrategy(simple_one_for_one) -> true;
validStrategy(one_for_one)        -> true;
validStrategy(one_for_all)        -> true;
validStrategy(rest_for_one)       -> true;
validStrategy(What)               -> throw({invalid_strategy, What}).

validIntensity(Max) when is_integer(Max),
                         Max >=  0 -> true;
validIntensity(What)               -> throw({invalid_intensity, What}).

validPeriod(Period) when is_integer(Period),
                         Period > 0 -> true;
validPeriod(What)                   -> throw({invalid_period, What}).

supname(self, Mod) -> {self(), Mod};
supname(N, _)      -> N.

%%% ------------------------------------------------------
%%% Check that the children start specification is valid.
%%% Input: [child_spec()]
%%% Returns: {ok, [child_rec()]} | Error
%%% ------------------------------------------------------

check_startspec(Children) -> check_startspec(Children, []).

check_startspec([ChildSpec|T], Res) ->
    case check_childspec(ChildSpec) of
	{ok, Child} ->
	    case lists:keymember(Child#child.name, #child.name, Res) of
		%% The error message duplicate_child_name is kept for
		%% backwards compatibility, although
		%% duplicate_child_id would be more correct.
		true -> {duplicate_child_name, Child#child.name};
		false -> check_startspec(T, [Child | Res])
	    end;
	Error -> Error
    end;
check_startspec([], Res) ->
    {ok, lists:reverse(Res)}.

check_childspec(ChildSpec) when is_map(ChildSpec) ->
    catch do_check_childspec(maps:merge(?default_child_spec,ChildSpec));
check_childspec({Name, Func, RestartType, Shutdown, ChildType, Mods}) ->
    check_childspec(#{id => Name,
		      start => Func,
		      restart => RestartType,
		      shutdown => Shutdown,
		      type => ChildType,
		      modules => Mods});
check_childspec(X) -> {invalid_child_spec, X}.

do_check_childspec(#{restart := RestartType,
		     type := ChildType} = ChildSpec)->
    Name = case ChildSpec of
	       #{id := N} -> N;
	       _ -> throw(missing_id)
	   end,
    Func = case ChildSpec of
	       #{start := F} -> F;
	       _ -> throw(missing_start)
	   end,
    validName(Name),
    validFunc(Func),
    validRestartType(RestartType),
    validChildType(ChildType),
    Shutdown = case ChildSpec of
		   #{shutdown := S} -> S;
		   #{type := worker} -> 5000;
		   #{type := supervisor} -> infinity
	       end,
    validShutdown(Shutdown),
    Mods = case ChildSpec of
	       #{modules := Ms} -> Ms;
	       _ -> {M,_,_} = Func, [M]
	   end,
    validMods(Mods),
    {ok, #child{name = Name, mfargs = Func, restart_type = RestartType,
		shutdown = Shutdown, child_type = ChildType, modules = Mods}}.

validChildType(supervisor) -> true;
validChildType(worker) -> true;
validChildType(What) -> throw({invalid_child_type, What}).

validName(_Name) -> true.

validFunc({M, F, A}) when is_atom(M), 
                          is_atom(F), 
                          is_list(A) -> true;
validFunc(Func)                      -> throw({invalid_mfa, Func}).

validRestartType(permanent)   -> true;
validRestartType(temporary)   -> true;
validRestartType(transient)   -> true;
validRestartType(RestartType) -> throw({invalid_restart_type, RestartType}).

validShutdown(Shutdown)
  when is_integer(Shutdown), Shutdown > 0 -> true;
validShutdown(infinity)             -> true;
validShutdown(brutal_kill)          -> true;
validShutdown(Shutdown)             -> throw({invalid_shutdown, Shutdown}).

validMods(dynamic) -> true;
validMods(Mods) when is_list(Mods) ->
    lists:foreach(fun(Mod) ->
		    if
			is_atom(Mod) -> ok;
			true -> throw({invalid_module, Mod})
		    end
		  end,
		  Mods);
validMods(Mods) -> throw({invalid_modules, Mods}).

child_to_spec(#child{name = Name,
		    mfargs = Func,
		    restart_type = RestartType,
		    shutdown = Shutdown,
		    child_type = ChildType,
		    modules = Mods}) ->
    #{id => Name,
      start => Func,
      restart => RestartType,
      shutdown => Shutdown,
      type => ChildType,
      modules => Mods}.

%%% ------------------------------------------------------
%%% Add a new restart and calculate if the max restart
%%% intensity has been reached (in that case the supervisor
%%% shall terminate).
%%% All restarts accured inside the period amount of seconds
%%% are kept in the #state.restarts list.
%%% Returns: {ok, State'} | {terminate, State'}
%%% ------------------------------------------------------

add_restart(State) ->  
    I = State#state.intensity,
    P = State#state.period,
    R = State#state.restarts,
    Now = erlang:monotonic_time(1),
    R1 = add_restart([Now|R], Now, P),
    State1 = State#state{restarts = R1},
    case length(R1) of
	CurI when CurI  =< I ->
	    {ok, State1};
	_ ->
	    {terminate, State1}
    end.

add_restart([R|Restarts], Now, Period) ->
    case inPeriod(R, Now, Period) of
	true ->
	    [R|add_restart(Restarts, Now, Period)];
	_ ->
	    []
    end;
add_restart([], _, _) ->
    [].

inPeriod(Then, Now, Period) ->
    Now =< Then + Period.

%%% ------------------------------------------------------
%%% Error and progress reporting.
%%% ------------------------------------------------------

report_error(Error, Reason, Child, SupName) ->
    ErrorMsg = [{supervisor, SupName},
		{errorContext, Error},
		{reason, Reason},
		{offender, extract_child(Child)}],
    error_logger:error_report(supervisor_report, ErrorMsg).


extract_child(Child) when is_list(Child#child.pid) ->
    [{nb_children, length(Child#child.pid)},
     {id, Child#child.name},
     {mfargs, Child#child.mfargs},
     {restart_type, Child#child.restart_type},
     {shutdown, Child#child.shutdown},
     {child_type, Child#child.child_type}];
extract_child(Child) ->
    [{pid, Child#child.pid},
     {id, Child#child.name},
     {mfargs, Child#child.mfargs},
     {restart_type, Child#child.restart_type},
     {shutdown, Child#child.shutdown},
     {child_type, Child#child.child_type}].

report_progress(Child, SupName) ->
    Progress = [{supervisor, SupName},
		{started, extract_child(Child)}],
    error_logger:info_report(progress, Progress).

format_status(terminate, [_PDict, State]) ->
    State;
format_status(_, [_PDict, State]) ->
    [{data, [{"State", State}]},
     {supervisor, [{"Callback", State#state.module}]}].

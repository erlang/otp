%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-define(MAPS, maps).
-define(MAP, maps:map).

-record(state, {name,
		strategy               :: strategy() | 'undefined',
		children = []          :: [child_id()],
		children_lookup = #{}  :: #{child_id() => child_rec()},
                dynamics               :: {'map', #{pid() => list()}}
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
	{ok, _, _} -> ok;
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
        {ok, Children, ChildrenLookup} ->
            case start_children(Children, ChildrenLookup, SupName) of
                {ok, NChildren, NChildrenLookup} ->
                    {ok, State#state{children=NChildren, children_lookup=NChildrenLookup}};
                {error, NChildren, NChildrenLookup, Reason} ->
                    _ = terminate_children(NChildren, NChildrenLookup, SupName),
                    {stop, {shutdown, Reason}}
            end;
        Error ->
            {stop, {start_spec, Error}}
    end.

init_dynamic(State, [StartSpec]) ->
    case check_startspec([StartSpec]) of
        {ok, Children, ChildrenLookup} ->
	    {ok, State#state{children = Children, children_lookup = ChildrenLookup}};
        Error ->
            {stop, {start_spec, Error}}
    end;
init_dynamic(_State, StartSpec) ->
    {stop, {bad_start_spec, StartSpec}}.

%%-----------------------------------------------------------------
%% Func: start_children/3
%% Args: Children = [child_id()] in start order
%%       ChildrenLookup = #{child_id() => child_rec()}
%%       SupName = {local, atom()} | {global, atom()} | {pid(), Mod}
%% Purpose: Start all children.  The new list contains #child's
%%          with pids.
%% Returns: {ok, NChildren, NChildrenLookup} | {error, NChildren, NChildrenLookup, Reason}
%%          NChildren = [child_id()] in termination order (reversed
%%                       start order)
%%          NChildrenLookup = #{child_id() => child_rec()}
%%-----------------------------------------------------------------
start_children(Children, ChildrenLookup, SupName) ->
    start_children(Children, ChildrenLookup, [], #{}, SupName).

start_children([ChildName|Chs], ChildrenLookup, NChildren, NChildrenLookup, SupName) ->
    Child = maps:get(ChildName, ChildrenLookup),
    case do_start_child(SupName, {ChildName, Child}) of
	{ok, undefined} when Child#child.restart_type =:= temporary ->
	    start_children(Chs, ChildrenLookup, NChildren, NChildrenLookup, SupName);
	{ok, Pid} ->
	    start_children(Chs, ChildrenLookup, [ChildName|NChildren],
                           NChildrenLookup#{ChildName=>Child#child{pid=Pid}},
                           SupName);
	{ok, Pid, _Extra} ->
	    start_children(Chs, ChildrenLookup, [ChildName|NChildren],
                           NChildrenLookup#{ChildName=>Child#child{pid=Pid}},
                           SupName);
	{error, Reason} ->
	    report_error(start_error, Reason, {ChildName, Child}, SupName),
	    {error, lists:reverse(Chs) ++ [ChildName|NChildren],
             NChildrenLookup#{ChildName => Child},
	     {failed_to_start_child,ChildName,Reason}}
    end;
start_children([], _ChildrenLookup, NChildren, NChildrenLookup, _SupName) ->
    {ok, NChildren, NChildrenLookup}.

do_start_child(SupName, {ChildName, Child}) ->
    #child{mfargs = {M, F, Args}} = Child,
    case catch apply(M, F, Args) of
	{ok, Pid} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress({ChildName, NChild}, SupName),
	    {ok, Pid};
	{ok, Pid, Extra} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress({ChildName, NChild}, SupName),
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
    ChildName = hd(State#state.children),
    Child = maps:get(ChildName, State#state.children_lookup),
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
	{ok, {ChildName, Child}} ->
	    {Resp, NState} = handle_start_child(ChildName, Child, State),
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
	{ok, ChildName, Child} ->
	    case do_terminate(ChildName, Child, State#state.name) of
		#child{restart_type=RT} when RT=:=temporary; ?is_simple(State) ->
		    {reply, ok, state_del_child({ChildName, Child}, State)};
		NChild ->
		    {reply, ok, replace_child({ChildName, NChild}, State)}
		end;
	error ->
	    {reply, {error, not_found}, State}
    end;

%% restart_child request is invalid for simple_one_for_one supervisors
handle_call({restart_child, _Name}, _From, State) when ?is_simple(State) ->
    {reply, {error, simple_one_for_one}, State};

handle_call({restart_child, Name}, _From, State) ->
    case get_child(Name, State) of
	{ok, ChildName, Child} when Child#child.pid =:= undefined ->
	    case do_start_child(State#state.name, {Name, Child}) of
		{ok, Pid} ->
		    NState = replace_child({ChildName, Child#child{pid = Pid}}, State),
		    {reply, {ok, Pid}, NState};
		{ok, Pid, Extra} ->
		    NState = replace_child({ChildName, Child#child{pid = Pid}}, State),
		    {reply, {ok, Pid, Extra}, NState};
		Error ->
		    {reply, Error, State}
	    end;
	{ok, _ChildName, #child{pid=?restarting(_)}} ->
	    {reply, {error, restarting}, State};
	{ok, _, _} ->
	    {reply, {error, running}, State};
	_ ->
	    {reply, {error, not_found}, State}
    end;

%% delete_child request is invalid for simple_one_for_one supervisors
handle_call({delete_child, _Name}, _From, State) when ?is_simple(State) ->
    {reply, {error, simple_one_for_one}, State};

handle_call({delete_child, Name}, _From, State) ->
    case get_child(Name, State) of
	{ok, ChildName, Child} when Child#child.pid =:= undefined ->
	    NState = remove_child(ChildName, State),
	    {reply, ok, NState};
	{ok, _ChildName, #child{pid=?restarting(_)}} ->
	    {reply, {error, restarting}, State};
	{ok, _, _} ->
	    {reply, {error, running}, State};
	_ ->
	    {reply, {error, not_found}, State}
    end;

handle_call({get_childspec, Name}, _From, State) ->
    case get_child(Name, State, ?is_simple(State)) of
	{ok, ChildName, Child} ->
            {reply, {ok, child_to_spec(ChildName, Child)}, State};
	error ->
	    {reply, {error, not_found}, State}
    end;

handle_call(which_children, _From, #state{children = [ChildName],
            children_lookup = ChildrenLookup} = State) when ?is_simple(State) ->
    case maps:get(ChildName, ChildrenLookup) of
        #child{restart_type = temporary, child_type = CT, modules = Mods} ->
             Reply =
                 lists:map(fun(Pid) -> {undefined, Pid, CT, Mods}
                           end,
                           ?SETS:to_list(dynamics_db(temporary, State#state.dynamics))),
             {reply, Reply, State};
         #child{restart_type = RType, child_type = CT, modules = Mods} ->
             Reply =
                 lists:map(fun({?restarting(_),_}) -> {undefined,restarting,CT,Mods};
                              ({Pid, _}) -> {undefined, Pid, CT, Mods}
                           end,
                           ?MAPS:to_list(dynamics_db(RType, State#state.dynamics))),
             {reply, Reply, State}
	end;
handle_call(which_children, _From, #state{children_lookup=ChildrenLookup}=State) ->
    MapperFun = fun (ChildName) ->
        case maps:get(ChildName, ChildrenLookup) of
            #child{pid = ?restarting(_), child_type = ChildType, modules = Mods} ->
                {ChildName, restarting, ChildType, Mods};
            #child{pid = Pid, child_type = ChildType, modules = Mods} ->
                {ChildName, Pid, ChildType, Mods}
        end
    end,
    Resp = lists:map(MapperFun, State#state.children), {reply, Resp, State};


handle_call(count_children, _From, #state{children=[ChildName],
            children_lookup=ChildrenLookup,
            dynamic_restarts=Restarts}=State) when ?is_simple(State) ->
    case maps:get(ChildName, ChildrenLookup) of
        #child{restart_type = temporary, child_type = CT} ->
            Sz = ?SETS:size(dynamics_db(temporary, State#state.dynamics)),
            Reply = case CT of
                        supervisor -> [{specs, 1}, {active, Sz},
                                       {supervisors, Sz}, {workers, 0}];
                        worker -> [{specs, 1}, {active, Sz},
                                   {supervisors, 0}, {workers, Sz}]
                    end,
                {reply, Reply, State};
        #child{restart_type = RType, child_type = CT} ->
            Sz = ?MAPS:size(dynamics_db(RType, State#state.dynamics)),
            Active = Sz - Restarts,
            Reply = case CT of
                        supervisor -> [{specs, 1}, {active, Active},
                                       {supervisors, Sz}, {workers, 0}];
                        worker -> [{specs, 1}, {active, Active},
                                   {supervisors, 0}, {workers, Sz}]
                        end,
            {reply, Reply, State}
    end;
handle_call(count_children, _From, State) ->
    %% Specs and children are together on the children list...
    {Specs, Active, Supers, Workers} =
        maps:fold(fun(_ChildName, Child, Counts) ->
                        count_child(Child, Counts)
                  end, {0,0,0,0}, State#state.children_lookup),

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

handle_cast({try_again_restart,Pid}, #state{children=[ChildName],
            children_lookup = ChildrenLookup}=State) when ?is_simple(State) ->
    Child = maps:get(ChildName, ChildrenLookup),
    RT = Child#child.restart_type,
    RPid = restarting(Pid),
    case dynamic_child_args(RPid, RT, State#state.dynamics) of
	{ok, Args} ->
	    {M, F, _} = Child#child.mfargs,
	    NChild = Child#child{pid = RPid, mfargs = {M, F, Args}},
	    case restart({ChildName, NChild},State) of
		{ok, State1} ->
		    {noreply, State1};
		{shutdown, State1} ->
		    {stop, shutdown, State1}
	    end;
	error ->
            {noreply, State}
    end;

handle_cast({try_again_restart,Name}, State) ->
    case maps:find(Name,State#state.children_lookup) of
	{ok, Child = #child{pid=?restarting(_)}} ->
	    case restart({Name, Child},State) of
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
    error_logger:error_msg("Supervisor received unexpected message: ~tp~n",
			   [Msg]),
    {noreply, State}.

%%
%% Terminate this server.
%%
-spec terminate(term(), state()) -> 'ok'.

terminate(_Reason, #state{children=[ChildName],
          children_lookup=ChildrenLookup}=State) when ?is_simple(State) ->
    Child = maps:get(ChildName, ChildrenLookup),
    terminate_dynamic_children(ChildName, Child,
                               dynamics_db(Child#child.restart_type, State#state.dynamics),
                               State#state.name);
terminate(_Reason, State) ->
    terminate_children(State#state.children,
                       State#state.children_lookup, State#state.name).

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
        {ok, [ChildName], ChildrenLookup} ->
            {ok, State#state{children=[ChildName], children_lookup=ChildrenLookup}};
        Error ->
            {error, Error}
    end;
update_childspec(State, StartSpec) ->
    case check_startspec(StartSpec) of
        {ok, Children0, ChildrenLookup} ->
            OldC = [ {Name, maps:get(Name, State#state.children_lookup)}
                     || Name <- State#state.children ], % In reverse start order !
            Children = [ {Name, maps:get(Name, ChildrenLookup)} || Name <- Children0 ],
            NewC0 = update_childspec1(OldC, Children, []),
            NewC = [ ChildName || {ChildName, #child{}} <- NewC0 ],
            NewCLookup = maps:from_list([ {ChildName, Ch}
                                         || {ChildName, #child{} = Ch} <- NewC0 ]),
            {ok, State#state{children=NewC, children_lookup=NewCLookup}};
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

update_chsp({OldChildName, OldCh}, Children) ->
    case lists:map(fun({ChildName, Ch}) when OldChildName =:= ChildName ->
			   {ChildName, Ch#child{pid = OldCh#child.pid}};
		      ({ChildName, Ch}) ->
			   {ChildName, Ch}
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

handle_start_child(ChildName, Child, State) ->
    case get_child(ChildName, State) of
	error ->
	    case do_start_child(State#state.name, {ChildName, Child}) of
		{ok, undefined} when Child#child.restart_type =:= temporary ->
		    {{ok, undefined}, State};
		{ok, Pid} ->
		    {{ok, Pid}, save_child(ChildName, Child#child{pid = Pid}, State)};
		{ok, Pid, Extra} ->
		    {{ok, Pid, Extra}, save_child(ChildName, Child#child{pid = Pid}, State)};
		{error, What} ->
		    {{error, {What, Child}}, State}
	    end;
	{ok, _OldChildName, OldChild} when is_pid(OldChild#child.pid) ->
	    {{error, {already_started, OldChild#child.pid}}, State};
	{ok, _OldChildName, _OldChild} ->
	    {{error, already_present}, State}
    end.

%%% ---------------------------------------------------
%%% Restart. A process has terminated.
%%% Returns: {ok, state()} | {shutdown, state()}
%%% ---------------------------------------------------

restart_child(Pid, Reason, #state{children=[ChildName],
              children_lookup=ChildrenLookup}=State) when ?is_simple(State) ->
    Child = maps:get(ChildName, ChildrenLookup),
    RestartType = Child#child.restart_type,
    case dynamic_child_args(Pid, RestartType, State#state.dynamics) of
	{ok, Args} ->
	    {M, F, _} = Child#child.mfargs,
	    NChild = Child#child{pid = Pid, mfargs = {M, F, Args}},
	    do_restart(RestartType, Reason, {ChildName, NChild}, State);
	error ->
            {ok, State}
    end;

restart_child(Pid, Reason, State) ->
    FilterFun = fun (_, #child{pid = Pid0}) when Pid =:= Pid0 -> true;
	    	    (_, _) -> false
		end,
    case maps:to_list(maps:filter(FilterFun, State#state.children_lookup)) of
	[{ChildName, #child{restart_type = RestartType} = Child}] ->
	    do_restart(RestartType, Reason, {ChildName, Child}, State);
	[] ->
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

restart({ChildName, Child}, State) ->
    case add_restart(State) of
	{ok, NState} ->
	    case restart(NState#state.strategy, {ChildName, Child}, NState) of
		{try_again,NState2} ->
		    %% Leaving control back to gen_server before
		    %% trying again. This way other incoming requsts
		    %% for the supervisor can be handled - e.g. a
		    %% shutdown request for the supervisor or the
		    %% child.
		    Id = if ?is_simple(State) -> Child#child.pid;
			    true -> ChildName
			 end,
		    ok = try_again_restart(self(), Id),
		    {ok,NState2};
		{try_again, NState2, {ChName, #child{}}} ->
		    ok = try_again_restart(self(), ChName),
		    {ok,NState2};
		Other ->
		    Other
	    end;
	{terminate, NState} ->
	    report_error(shutdown, reached_max_restart_intensity,
			 {ChildName, Child}, State#state.name),
	    {shutdown, remove_child({ChildName, Child}, NState)}
    end.

restart(simple_one_for_one, {ChildName, Child}, State0) ->
    #child{pid = OldPid, mfargs = {M, F, A}} = Child,
    State = case OldPid of
		?restarting(_) ->
		    NRes = State0#state.dynamic_restarts - 1,
		    State0#state{dynamic_restarts = NRes};
		_ ->
		    State0
	    end,
    Dynamics = ?MAPS:remove(OldPid, dynamics_db(Child#child.restart_type,
					       State#state.dynamics)),
    case do_start_child_i(M, F, A) of
	{ok, Pid} ->
            DynamicsDb = {map, ?MAPS:put(Pid, A, Dynamics)},
	    NState = State#state{dynamics = DynamicsDb},
	    {ok, NState};
	{ok, Pid, _Extra} ->
            DynamicsDb = {map, ?MAPS:put(Pid, A, Dynamics)},
	    NState = State#state{dynamics = DynamicsDb},
	    {ok, NState};
	{error, Error} ->
	    NRestarts = State#state.dynamic_restarts + 1,
            DynamicsDb = {map, ?MAPS:put(restarting(OldPid), A, Dynamics)},
	    NState = State#state{dynamic_restarts = NRestarts,
				 dynamics = DynamicsDb},
	    report_error(start_error, Error, {ChildName, Child}, State#state.name),
	    {try_again, NState}
    end;
restart(one_for_one, {ChildName, Child}, State) ->
    OldPid = Child#child.pid,
    case do_start_child(State#state.name, {ChildName, Child}) of
        {ok, Pid} ->
            NState = replace_child({ChildName, Child#child{pid = Pid}}, State),
            {ok, NState};
        {ok, Pid, _Extra} ->
            NState = replace_child({ChildName, Child#child{pid = Pid}}, State),
            {ok, NState};
        {error, Reason} ->
            NState = replace_child({ChildName, Child#child{pid = restarting(OldPid)}}, State),
            report_error(start_error, Reason, {ChildName, Child}, State#state.name),
            {try_again, NState}
    end;
restart(rest_for_one, {ChildName, Child}, State) ->
    {ChAfter, ChLAfter, ChBefore, ChLBefore} =
        split_child(Child#child.pid, State#state.children, State#state.children_lookup),
    {ChAfter2, ChLAfter2} = terminate_children(ChAfter, ChLAfter, State#state.name),

    case start_children(ChAfter2, ChLAfter2, State#state.name) of
        {ok, ChAfter3, ChLAfter3} ->
            {ok, State#state{children=ChAfter3++ChBefore,
                             children_lookup=maps:merge(ChLAfter3, ChLBefore)}};
        {error, ChAfter3, ChLAfter3, {failed_to_start_child, ChName, _Reason}}
            when ChName =:= ChildName ->
            NChild = Child#child{pid=restarting(Child#child.pid)},
            NState = State#state{children=ChAfter3++ChBefore,
                                 children_lookup=maps:merge(ChLAfter3, ChLBefore)},
            {try_again, replace_child({ChName, NChild},NState)};
        {error, ChAfter3, ChLAfter3, {failed_to_start_child, ChName, _Reason}} ->
            NChild = maps:get(ChName, ChLAfter3),
            NChild2 = NChild#child{pid=?restarting(undefined)},
            NState = State#state{children=ChAfter3++ChBefore,
                                 children_lookup=maps:merge(ChLAfter3, ChLBefore)},
            {try_again, replace_child({ChName, NChild2},NState), {ChName, NChild2}}
    end;
restart(one_for_all, {ChildName, Child}, State) ->
    {Children1, ChildrenLookup0} = 
        del_child(Child#child.pid, State#state.children, State#state.children_lookup),
    {Children2, ChildrenLookup} = 
        terminate_children(Children1, ChildrenLookup0, State#state.name),

    case start_children(Children2, ChildrenLookup, State#state.name) of
        {ok, NChs, NChLs} ->
            {ok, State#state{children=NChs, children_lookup=NChLs}};
        {error, NChs, NChLs, {failed_to_start_child, ChName, _Reason}}
            when ChName =:= ChildName ->
            NChild = Child#child{pid=restarting(Child#child.pid)},
            NState = State#state{children=NChs, children_lookup=NChLs},
            {try_again, replace_child({ChName, NChild},NState)};
        {error, NChs, NChLs, {failed_to_start_child, ChName, _Reason}} ->
            NChild = maps:get(ChName, NChLs),
            NChild2 = NChild#child{pid=?restarting(undefined)},
            NState = State#state{children=NChs, children_lookup=NChLs},
            {try_again, replace_child({ChName, NChild2},NState), {ChName, NChild2}}
    end.

restarting(Pid) when is_pid(Pid) -> ?restarting(Pid);
restarting(RPid) -> RPid.

%%-----------------------------------------------------------------
%% Func: terminate_children/3
%% Args: Children = [child_id()] in termination order
%%       ChildrenLookup = #{child_id() => child_rec()}
%%       SupName = {local, atom()} | {global, atom()} | {pid(),Mod}
%% Returns: NChildren = [child_id()] in
%%          startup order (reversed termination order)
%%-----------------------------------------------------------------
terminate_children(Children, ChildrenLookup, SupName) ->
    terminate_children(Children, SupName, ChildrenLookup, []).

%% Temporary children should not be restarted and thus should
%% be skipped when building the list of terminated children, although
%% we do want them to be shut down as many functions from this module
%% use this function to just clear everything.
terminate_children([ChildName | Children], SupName, ChildrenLookup, Res) ->
    case maps:get(ChildName, ChildrenLookup) of
        #child{restart_type=temporary} = Child ->
            _ = do_terminate(ChildName, Child, SupName),
            terminate_children(Children, SupName,
                               maps:remove(ChildName, ChildrenLookup),
                               Res);
        Child ->
            NChild = do_terminate(ChildName, Child, SupName),
            terminate_children(Children, SupName,
                               ChildrenLookup#{ChildName=>NChild},
                               [ChildName|Res])
    end;
terminate_children([], _SupName, ChildrenLookup, Res) ->
    {Res, ChildrenLookup}.

do_terminate(ChildName, Child, SupName) when is_pid(Child#child.pid) ->
    case shutdown(Child#child.pid, Child#child.shutdown) of
        ok ->
            ok;
        {error, normal} when Child#child.restart_type =/= permanent ->
            ok;
        {error, OtherReason} ->
            report_error(shutdown_error, OtherReason, {ChildName, Child}, SupName)
    end,
    Child#child{pid = undefined};
do_terminate(_ChildName, Child, _SupName) ->
	io:format("XXX: ~p~n", [Child]),
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
%% Func: terminate_dynamic_children/4
%% Args: ChildName = child_id()
%%       Child     = child_rec()
%%       Dynamics  = ?MAP() | ?SET()
%%       SupName   = {local, atom()} | {global, atom()} | {pid(),Mod}
%% Returns: ok
%%
%%
%% Shutdown all dynamic children. This happens when the supervisor is
%% stopped. Because the supervisor can have millions of dynamic children, we
%% can have an significative overhead here.
%%-----------------------------------------------------------------
terminate_dynamic_children(ChildName, Child, Dynamics, SupName) ->
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
                                    {ChildName, Child#child{pid=Ls}}, SupName)
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
    ?MAPS:fold(fun(P, _, {Pids, EStack}) when is_pid(P) ->
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
save_child(ChildName, #child{restart_type=temporary, mfargs = {M, F, _}}=Child,
           #state{children=Children, children_lookup=ChildrenLookup}=State) ->
    NewChild = Child#child{mfargs={M, F, undefined}},
    State#state{children=[ChildName|Children],
                children_lookup=ChildrenLookup#{ChildName=>NewChild}};
save_child(ChildName, #child{}=Child, #state{children=Children, children_lookup=ChildrenLookup}=State) ->
    State#state{children=[ChildName|Children],
                children_lookup=ChildrenLookup#{ChildName=>Child}}.

save_dynamic_child(temporary, Pid, _, #state{dynamics = Dynamics} = State) ->
    DynamicsDb = dynamics_db(temporary, Dynamics),
    State#state{dynamics = {set, ?SETS:add_element(Pid, DynamicsDb)}};
save_dynamic_child(RestartType, Pid, Args, #state{dynamics = Dynamics} = State) ->
    DynamicsDb = dynamics_db(RestartType, Dynamics),
    State#state{dynamics = {map, ?MAPS:put(Pid, Args, DynamicsDb)}}.

dynamics_db(temporary, undefined) ->
    ?SETS:new();
dynamics_db(_, undefined) ->
    ?MAPS:new();
dynamics_db(_, {_Tag, DynamicsDb}) ->
    DynamicsDb.

dynamic_child_args(_Pid, temporary, _DynamicsDb) ->
    {ok, undefined};
dynamic_child_args(Pid, _RT, {map, DynamicsDb}) ->
    ?MAPS:find(Pid, DynamicsDb);
dynamic_child_args(_Pid, _RT, undefined) ->
    error.

state_del_child({_ChildName, #child{pid=Pid, restart_type=temporary}}, State) when ?is_simple(State) ->
    NDynamics = ?SETS:del_element(Pid, dynamics_db(temporary, State#state.dynamics)),
    State#state{dynamics = {set, NDynamics}};
state_del_child({_ChildName, #child{pid=Pid, restart_type=RType}}, State) when ?is_simple(State) ->
    NDynamics = ?MAPS:remove(Pid, dynamics_db(RType, State#state.dynamics)),
    State#state{dynamics = {map, NDynamics}};
state_del_child({ChildName, _Child}, State) ->
    {NChildren, NChildrenLookup} =
        del_child(ChildName, State#state.children, State#state.children_lookup),
    State#state{children = NChildren, children_lookup = NChildrenLookup}.

child_name_by_pid(Pid, ChildrenLookup) ->
    FilterFun = fun(_ChName, Ch) when Ch#child.pid =:= Pid, Ch#child.restart_type =:= temporary -> true;
                   (_ChName, Ch) when Ch#child.pid =:= Pid -> true;
                   (_, _) -> false
                end,
    [{ChildName, _}] = maps:to_list(maps:filter(FilterFun, ChildrenLookup)),
    ChildName.

del_child(Pid, Children, ChildrenLookup) when is_pid(Pid) ->
    ChildName = child_name_by_pid(Pid, ChildrenLookup),
    del_child(ChildName, Children, ChildrenLookup);
del_child({restarting, _} = Pid, Children, ChildrenLookup) ->
    ChildName = child_name_by_pid(Pid, ChildrenLookup),
    del_child(ChildName, Children, ChildrenLookup);
del_child(Name, Children, ChildrenLookup) ->
    case maps:get(Name, ChildrenLookup) of
        Ch when Ch#child.restart_type =:= temporary ->
            {lists:delete(Name, Children), maps:without([Name], ChildrenLookup)};
        Ch ->
            {Children, ChildrenLookup#{Name=>Ch#child{pid=undefined}}}
    end.

%% Chs = [S4, S3, Ch, S1, S0]
%% Ret: {[S4, S3, Ch], #{S4 => ..., S3 => ..., Ch => ...}, [S1, S0], #{S1 => ..., S0 => ...}}
split_child({restarting, _} = Pid, Chs, ChildrenLookup) ->
    ChildName = child_name_by_pid(Pid, ChildrenLookup),
    split_child(ChildName, Chs, ChildrenLookup);
split_child(Pid, Chs, ChildrenLookup) when is_pid(Pid) ->
    ChildName = child_name_by_pid(Pid, ChildrenLookup),
    split_child(ChildName, Chs, ChildrenLookup);
split_child(Name, Chs, ChildrenLookup) ->
    SplitFun = fun(Name0) when Name0 =:= Name -> false; (_) -> true end,
    {Before0, [Ch | After0]} = lists:splitwith(SplitFun, Chs),
    {Before1, After1} = {Before0 ++ [Ch], After0},

    BeforeChL = [ {Name1, maps:get(Name1, ChildrenLookup)} || Name1 <- Before1 ],
    AfterChL = [ {Name1, maps:get(Name1, ChildrenLookup)} || Name1 <- After1 ],
    {Before1, maps:from_list(BeforeChL), After1, maps:from_list(AfterChL)}.

get_child(Name, State) ->
    get_child(Name, State, false).

get_child(Pid, State, AllowPid) when AllowPid, is_pid(Pid) ->
    get_dynamic_child(Pid, State);
get_child(Name, #state{children_lookup = ChildrenLookup}, _) ->
    case maps:find(Name, ChildrenLookup) of
    	{ok, Val} -> {ok, Name, Val};
    	error -> error
    end.

get_dynamic_child(Pid, #state{children=[ChildName], dynamics=Dynamics, children_lookup=ChildrenLookup}) ->
    Child = maps:get(ChildName, ChildrenLookup),
    case is_dynamic_pid(Pid, Dynamics) of
	true ->
	    {ok, ChildName, Child#child{pid=Pid}};
	false ->
	    RPid = restarting(Pid),
	    case is_dynamic_pid(RPid, Dynamics) of
		true ->
		    {ok, ChildName, Child#child{pid=RPid}};
		false ->
		    case erlang:is_process_alive(Pid) of
			true -> error;
			false -> {ok, ChildName, Child}
		    end
	    end
    end.

is_dynamic_pid(Pid, {map, Dynamics}) ->
    ?MAPS:is_key(Pid, Dynamics);
is_dynamic_pid(Pid, {set, Dynamics}) ->
    ?SETS:is_element(Pid, Dynamics);
is_dynamic_pid(_Pid, undefined) ->
    false.

replace_child({ChildName, Child}, #state{children_lookup = ChildrenLookup} = State) ->
    State#state{children_lookup = ChildrenLookup#{ChildName => Child}}.

remove_child(ChildName, State) ->
    Chs = lists:delete(ChildName, State#state.children),
    ChLs = maps:remove(ChildName, State#state.children_lookup),
    State#state{children = Chs, children_lookup = ChLs}.

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

check_startspec(Children) -> check_startspec(Children, [], #{}).

check_startspec([ChildSpec|T], Res, ResLookup) ->
    case check_childspec(ChildSpec) of
	{ok, {ChildName, Child}} ->
	    case maps:is_key(ChildName, ResLookup) of
		%% The error message duplicate_child_name is kept for
		%% backwards compatibility, although
		%% duplicate_child_id would be more correct.
		true -> {duplicate_child_name, ChildName};
		false -> check_startspec(T, [ChildName|Res], ResLookup#{ChildName=>Child})
	    end;
	Error -> Error
    end;
check_startspec([], Res, ResLookup) ->
    {ok, lists:reverse(Res), ResLookup}.

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
    {ok, {Name, #child{mfargs = Func, restart_type = RestartType,
		         shutdown = Shutdown, child_type = ChildType, modules = Mods}}}.

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

child_to_spec(ChildName, #child{mfargs = Func,
					 restart_type = RestartType,
		    		     	 shutdown = Shutdown,
		    		     	 child_type = ChildType,
		    		     	 modules = Mods}) ->
    #{id => ChildName,
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


extract_child({ChildName, Child}) when is_list(Child#child.pid) ->
    [{nb_children, length(Child#child.pid)},
     {id, ChildName},
     {mfargs, Child#child.mfargs},
     {restart_type, Child#child.restart_type},
     {shutdown, Child#child.shutdown},
     {child_type, Child#child.child_type}];
extract_child({ChildName, Child}) ->
    [{pid, Child#child.pid},
     {id, ChildName},
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

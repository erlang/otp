%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2023. All Rights Reserved.
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
	 check_childspecs/1, check_childspecs/2,
	 get_childspec/2]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

%% logger callback
-export([format_log/1, format_log/2]).

%% For release_handler only
-export([get_callback_module/1]).

-include("logger.hrl").

-define(report_error(Error, Reason, Child, SupName),
        ?LOG_ERROR(#{label=>{supervisor,Error},
                     report=>[{supervisor,SupName},
                              {errorContext,Error},
                              {reason,Reason},
                              {offender,extract_child(Child)}]},
                   #{domain=>[otp,sasl],
                     report_cb=>fun supervisor:format_log/2,
                     logger_formatter=>#{title=>"SUPERVISOR REPORT"},
                     error_logger=>#{tag=>error_report,
                                     type=>supervisor_report,
                                     report_cb=>fun supervisor:format_log/1}})).

%%--------------------------------------------------------------------------

-export_type([sup_flags/0, child_spec/0, strategy/0,
              startchild_ret/0, startchild_err/0,
              startlink_ret/0, startlink_err/0]).

%%--------------------------------------------------------------------------

-type auto_shutdown() :: 'never' | 'any_significant' | 'all_significant'.
-type child()         :: 'undefined' | pid().
-type child_id()      :: term().
-type mfargs()        :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
-type modules()       :: [module()] | 'dynamic'.
-type restart()       :: 'permanent' | 'transient' | 'temporary'.
-type significant()   :: boolean().
-type shutdown()      :: 'brutal_kill' | timeout().
-type worker()        :: 'worker' | 'supervisor'.
-type sup_name()      :: {'local', Name :: atom()}
                       | {'global', Name :: term()}
                       | {'via', Module :: module(), Name :: any()}.
-type sup_ref()       :: (Name :: atom())
                       | {Name :: atom(), Node :: node()}
                       | {'global', Name :: term()}
                       | {'via', Module :: module(), Name :: any()}
                       | pid().
-type child_spec()    :: #{id := child_id(),             % mandatory
			   start := mfargs(),            % mandatory
			   restart => restart(),         % optional
			   significant => significant(), % optional
			   shutdown => shutdown(),       % optional
			   type => worker(),             % optional
			   modules => modules()}         % optional
                       | {Id :: child_id(),
                          StartFunc :: mfargs(),
                          Restart :: restart(),
                          Shutdown :: shutdown(),
                          Type :: worker(),
                          Modules :: modules()}.

-type strategy() :: 'one_for_all' | 'one_for_one'
                  | 'rest_for_one' | 'simple_one_for_one'.

-type sup_flags() :: #{strategy => strategy(),           % optional
		       intensity => non_neg_integer(),   % optional
		       period => pos_integer(),          % optional
		       auto_shutdown => auto_shutdown()} % optional
                   | {RestartStrategy :: strategy(),
                      Intensity :: non_neg_integer(),
                      Period :: pos_integer()}.
-type children() :: {Ids :: [child_id()], Db :: #{child_id() => child_rec()}}.

%%--------------------------------------------------------------------------
%% Defaults
-define(default_flags, #{strategy      => one_for_one,
			 intensity     => 1,
			 period        => 5,
			 auto_shutdown => never}).
-define(default_child_spec, #{restart  => permanent,
			      type     => worker}).
%% Default 'shutdown' is 5000 for workers and infinity for supervisors.
%% Default 'modules' is [M], where M comes from the child's start {M,F,A}.

%%--------------------------------------------------------------------------

-record(child, {% pid is undefined when child is not running
	        pid = undefined :: child()
	                         | {restarting, pid() | undefined}
	                         | [pid()],
		id              :: child_id(),
		mfargs          :: mfargs(),
		restart_type    :: restart(),
		significant     :: significant(),
		shutdown        :: shutdown(),
		child_type      :: worker(),
		modules = []    :: modules()}).
-type child_rec() :: #child{}.

-record(state, {name,
		strategy = one_for_one:: strategy(),
		children = {[],#{}}    :: children(), % Ids in start order
                dynamics               :: {'maps', #{pid() => list()}}
                                        | {'mapsets', #{pid() => []}}
                                        | 'undefined',
		intensity = 1          :: non_neg_integer(),
		period    = 5          :: pos_integer(),
		restarts = [],
		dynamic_restarts = 0   :: non_neg_integer(),
		auto_shutdown = never  :: auto_shutdown(),
	        module,
	        args}).
-type state() :: #state{}.

-define(is_simple(State), State#state.strategy =:= simple_one_for_one).
-define(is_temporary(_Child_), _Child_#child.restart_type=:=temporary).
-define(is_transient(_Child_), _Child_#child.restart_type=:=transient).
-define(is_permanent(_Child_), _Child_#child.restart_type=:=permanent).
-define(is_significant(_Child_), _Child_#child.significant=:=true).

-callback init(Args :: term()) ->
    {ok, {SupFlags :: sup_flags(), [ChildSpec :: child_spec()]}}
    | ignore.

-define(restarting(_Pid_), {restarting,_Pid_}).

%%% ---------------------------------------------------
%%% This is a general process supervisor built upon gen_server.erl.
%%% Servers/processes should/could also be built using gen_server.erl.
%%% SupName = {local, atom()} | {global, term()}.
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
restart_child(Supervisor, Id) ->
    call(Supervisor, {restart_child, Id}).

-spec delete_child(SupRef, Id) -> Result when
      SupRef :: sup_ref(),
      Id :: child_id(),
      Result :: 'ok' | {'error', Error},
      Error :: 'running' | 'restarting' | 'not_found' | 'simple_one_for_one'.
delete_child(Supervisor, Id) ->
    call(Supervisor, {delete_child, Id}).

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
terminate_child(Supervisor, Id) ->
    call(Supervisor, {terminate_child, Id}).

-spec get_childspec(SupRef, Id) -> Result when
      SupRef :: sup_ref(),
      Id :: pid() | child_id(),
      Result :: {'ok', child_spec()} | {'error', Error},
      Error :: 'not_found'.
get_childspec(Supervisor, Id) ->
    call(Supervisor, {get_childspec, Id}).

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
             | {workers, ChildWorkerCount :: non_neg_integer()}.
count_children(Supervisor) ->
    call(Supervisor, count_children).

call(Supervisor, Req) ->
    gen_server:call(Supervisor, Req, infinity).

-spec check_childspecs(ChildSpecs) -> Result when
      ChildSpecs :: [child_spec()],
      Result :: 'ok' | {'error', Error :: term()}.
check_childspecs(ChildSpecs) ->
    check_childspecs(ChildSpecs, undefined).

-spec check_childspecs(ChildSpecs, AutoShutdown) -> Result when
      ChildSpecs :: [child_spec()],
      AutoShutdown :: undefined | auto_shutdown(),
      Result :: 'ok' | {'error', Error :: term()}.
check_childspecs(ChildSpecs, AutoShutdown) when is_list(ChildSpecs) ->
    check_childspecs1(ChildSpecs, AutoShutdown);
check_childspecs(X, _AutoShutdown) -> {error, {badarg, X}}.

check_childspecs1(ChildSpecs, undefined) ->
    check_childspecs2(ChildSpecs, undefined);
check_childspecs1(ChildSpecs, never) ->
    check_childspecs2(ChildSpecs, never);
check_childspecs1(ChildSpecs, any_significant) ->
    check_childspecs2(ChildSpecs, any_significant);
check_childspecs1(ChildSpecs, all_significant) ->
    check_childspecs2(ChildSpecs, all_significant);
check_childspecs1(_, X) -> {error, {badarg, X}}.

check_childspecs2(ChildSpecs, AutoShutdown) ->
    case check_startspec(ChildSpecs, AutoShutdown) of
	{ok, _} -> ok;
	Error -> {error, Error}
    end.

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
    case check_startspec(StartSpec, State#state.auto_shutdown) of
        {ok, Children} ->
            case start_children(Children, SupName) of
                {ok, NChildren} ->
                    %% Static supervisor are not expected to
                    %% have much work to do so hibernate them
                    %% to improve memory handling.
                    {ok, State#state{children = NChildren}, hibernate};
                {error, NChildren, Reason} ->
                    _ = terminate_children(NChildren, SupName),
                    {stop, {shutdown, Reason}}
            end;
        Error ->
            {stop, {start_spec, Error}}
    end.

init_dynamic(State, [StartSpec]) ->
    case check_startspec([StartSpec], State#state.auto_shutdown) of
        {ok, Children} ->
            %% Simple one for one supervisors are expected to
            %% have many children coming and going so do not
            %% hibernate.
	    {ok, dyn_init(State#state{children = Children})};
        Error ->
            {stop, {start_spec, Error}}
    end;
init_dynamic(_State, StartSpec) ->
    {stop, {bad_start_spec, StartSpec}}.

%%-----------------------------------------------------------------
%% Func: start_children/2
%% Args: Children = children() % Ids in start order
%%       SupName = {local, atom()} | {global, term()} | {pid(), Mod}
%% Purpose: Start all children.  The new map contains #child's
%%          with pids.
%% Returns: {ok, NChildren} | {error, NChildren, Reason}
%%          NChildren = children() % Ids in termination order
%%                                   (reversed start order)
%%-----------------------------------------------------------------
start_children(Children, SupName) ->
    Start =
        fun(Id,Child) ->
                case do_start_child(SupName, Child) of
                    {ok, undefined} when ?is_temporary(Child) ->
                        remove;
                    {ok, Pid} ->
                        {update,Child#child{pid = Pid}};
                    {ok, Pid, _Extra} ->
                        {update,Child#child{pid = Pid}};
                    {error, Reason} ->
                        ?report_error(start_error, Reason, Child, SupName),
                        {abort,{failed_to_start_child,Id,Reason}}
                end
        end,
    children_map(Start,Children).

do_start_child(SupName, Child) ->
    #child{mfargs = {M, F, Args}} = Child,
    case do_start_child_i(M, F, Args) of
	{ok, Pid} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress(NChild, SupName),
	    {ok, Pid};
	{ok, Pid, Extra} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress(NChild, SupName),
	    {ok, Pid, Extra};
        Other ->
            Other
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
    Child = get_dynamic_child(State),
    #child{mfargs = {M, F, A}} = Child,
    Args = A ++ EArgs,
    case do_start_child_i(M, F, Args) of
	{ok, undefined} ->
	    {reply, {ok, undefined}, State};
	{ok, Pid} ->
	    NState = dyn_store(Pid, Args, State),
	    {reply, {ok, Pid}, NState};
	{ok, Pid, Extra} ->
	    NState = dyn_store(Pid, Args, State),
	    {reply, {ok, Pid, Extra}, NState};
	What ->
	    {reply, What, State}
    end;

handle_call({start_child, ChildSpec}, _From, State) ->
    case check_childspec(ChildSpec, State#state.auto_shutdown) of
	{ok, Child} ->
	    {Resp, NState} = handle_start_child(Child, State),
	    {reply, Resp, NState};
	What ->
	    {reply, {error, What}, State}
    end;

%% terminate_child for simple_one_for_one can only be done with pid
handle_call({terminate_child, Id}, _From, State) when not is_pid(Id),
                                                      ?is_simple(State) ->
    {reply, {error, simple_one_for_one}, State};

handle_call({terminate_child, Id}, _From, State) ->
    case find_child(Id, State) of
	{ok, Child} ->
	    do_terminate(Child, State#state.name),
            {reply, ok, del_child(Child, State)};
	error ->
	    {reply, {error, not_found}, State}
    end;

%% restart_child request is invalid for simple_one_for_one supervisors
handle_call({restart_child, _Id}, _From, State) when ?is_simple(State) ->
    {reply, {error, simple_one_for_one}, State};

handle_call({restart_child, Id}, _From, State) ->
    case find_child(Id, State) of
	{ok, Child} when Child#child.pid =:= undefined ->
	    case do_start_child(State#state.name, Child) of
		{ok, Pid} ->
		    NState = set_pid(Pid, Id, State),
		    {reply, {ok, Pid}, NState};
		{ok, Pid, Extra} ->
		    NState = set_pid(Pid, Id, State),
		    {reply, {ok, Pid, Extra}, NState};
		Error ->
		    {reply, Error, State}
	    end;
	{ok, #child{pid=?restarting(_)}} ->
	    {reply, {error, restarting}, State};
	{ok, _} ->
	    {reply, {error, running}, State};
	_ ->
	    {reply, {error, not_found}, State}
    end;

%% delete_child request is invalid for simple_one_for_one supervisors
handle_call({delete_child, _Id}, _From, State) when ?is_simple(State) ->
    {reply, {error, simple_one_for_one}, State};

handle_call({delete_child, Id}, _From, State) ->
    case find_child(Id, State) of
	{ok, Child} when Child#child.pid =:= undefined ->
	    NState = remove_child(Id, State),
	    {reply, ok, NState};
	{ok, #child{pid=?restarting(_)}} ->
	    {reply, {error, restarting}, State};
	{ok, _} ->
	    {reply, {error, running}, State};
	_ ->
	    {reply, {error, not_found}, State}
    end;

handle_call({get_childspec, Id}, _From, State) ->
    case find_child(Id, State) of
	{ok, Child} ->
            {reply, {ok, child_to_spec(Child)}, State};
	error ->
	    {reply, {error, not_found}, State}
    end;

handle_call(which_children, _From, State) when ?is_simple(State) ->
    #child{child_type = CT,modules = Mods} = get_dynamic_child(State),
    Reply = dyn_map(fun(?restarting(_)) -> {undefined, restarting, CT, Mods};
                       (Pid) -> {undefined, Pid, CT, Mods}
                    end, State),
    {reply, Reply, State};

handle_call(which_children, _From, State) ->
    Resp =
	children_to_list(
          fun(Id,#child{pid = ?restarting(_),
                        child_type = ChildType, modules = Mods}) ->
                  {Id, restarting, ChildType, Mods};
             (Id,#child{pid = Pid,
                        child_type = ChildType, modules = Mods}) ->
                  {Id, Pid, ChildType, Mods}
          end,
          State#state.children),
    {reply, Resp, State};

handle_call(count_children, _From,  #state{dynamic_restarts = Restarts} = State)
  when ?is_simple(State) ->
    #child{child_type = CT} = get_dynamic_child(State),
    Sz = dyn_size(State),
    Active = Sz - Restarts, % Restarts is always 0 for temporary children
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
	children_fold(fun(_Id, Child, Counts) ->
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
-spec handle_cast({try_again_restart, child_id() | {'restarting',pid()}}, state()) ->
			 {'noreply', state()} | {stop, shutdown, state()}.

handle_cast({try_again_restart,TryAgainId}, State) ->
    case find_child_and_args(TryAgainId, State) of
	{ok, Child = #child{pid=?restarting(_)}} ->
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
    ?LOG_ERROR("Supervisor received unexpected message: ~tp~n",[Msg],
               #{domain=>[otp],
                 error_logger=>#{tag=>error}}),
    {noreply, State}.

%%
%% Terminate this server.
%%
-spec terminate(term(), state()) -> 'ok'.

terminate(_Reason, State) when ?is_simple(State) ->
    terminate_dynamic_children(State);
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
    case check_startspec(StartSpec, State#state.auto_shutdown) of
        {ok, {[_],_}=Children} ->
            {ok, State#state{children = Children}};
        Error ->
            {error, Error}
    end;
update_childspec(State, StartSpec) ->
    case check_startspec(StartSpec, State#state.auto_shutdown) of
	{ok, Children} ->
	    OldC = State#state.children, % In reverse start order !
	    NewC = update_childspec1(OldC, Children, []),
	    {ok, State#state{children = NewC}};
        Error ->
	    {error, Error}
    end.

update_childspec1({[Id|OldIds], OldDb}, {Ids,Db}, KeepOld) ->
    case update_chsp(maps:get(Id,OldDb), Db) of
	{ok,NewDb} ->
	    update_childspec1({OldIds,OldDb}, {Ids,NewDb}, KeepOld);
	false ->
	    update_childspec1({OldIds,OldDb}, {Ids,Db}, [Id|KeepOld])
    end;
update_childspec1({[],OldDb}, {Ids,Db}, KeepOld) ->
    KeepOldDb = maps:with(KeepOld,OldDb),
    %% Return them in (kept) reverse start order.
    {lists:reverse(Ids ++ KeepOld),maps:merge(KeepOldDb,Db)}.

update_chsp(#child{id=Id}=OldChild, NewDb) ->
    case maps:find(Id, NewDb) of
        {ok,Child} ->
            {ok,NewDb#{Id => Child#child{pid = OldChild#child.pid}}};
        error -> % Id not found in new spec.
            false
    end.

    
%%% ---------------------------------------------------
%%% Start a new child.
%%% ---------------------------------------------------

handle_start_child(Child, State) ->
    case find_child(Child#child.id, State) of
	error ->
	    case do_start_child(State#state.name, Child) of
		{ok, undefined} when ?is_temporary(Child) ->
		    {{ok, undefined}, State};
		{ok, Pid} ->
		    {{ok, Pid}, save_child(Child#child{pid = Pid}, State)};
		{ok, Pid, Extra} ->
		    {{ok, Pid, Extra}, save_child(Child#child{pid = Pid}, State)};
		{error, What} ->
		    {{error, {What, Child}}, State}
	    end;
	{ok, OldChild} when is_pid(OldChild#child.pid) ->
	    {{error, {already_started, OldChild#child.pid}}, State};
	{ok, _OldChild} ->
	    {{error, already_present}, State}
    end.

%%% ---------------------------------------------------
%%% Restart. A process has terminated.
%%% Returns: {ok, state()} | {shutdown, state()}
%%% ---------------------------------------------------

restart_child(Pid, Reason, State) ->
    case find_child_and_args(Pid, State) of
        {ok, Child} ->
	    do_restart(Reason, Child, State);
	error ->
	    {ok, State}
    end.

do_restart(Reason, Child, State) when ?is_permanent(Child) ->
    ?report_error(child_terminated, Reason, Child, State#state.name),
    restart(Child, State);
do_restart(normal, Child, State) ->
    NState = del_child(Child, State),
    do_auto_shutdown(Child, NState);
do_restart(shutdown, Child, State) ->
    NState = del_child(Child, State),
    do_auto_shutdown(Child, NState);
do_restart({shutdown, _Term}, Child, State) ->
    NState = del_child(Child, State),
    do_auto_shutdown(Child, NState);
do_restart(Reason, Child, State) when ?is_transient(Child) ->
    ?report_error(child_terminated, Reason, Child, State#state.name),
    restart(Child, State);
do_restart(Reason, Child, State) when ?is_temporary(Child) ->
    ?report_error(child_terminated, Reason, Child, State#state.name),
    NState = del_child(Child, State),
    do_auto_shutdown(Child, NState).

do_auto_shutdown(_Child, State=#state{auto_shutdown = never}) ->
    {ok, State};
do_auto_shutdown(Child, State) when not ?is_significant(Child) ->
    {ok, State};
do_auto_shutdown(_Child, State=#state{auto_shutdown = any_significant}) ->
    {shutdown, State};
do_auto_shutdown(_Child, State=#state{auto_shutdown = all_significant})
  when ?is_simple(State) ->
    case dyn_size(State) of
	0 ->
	    {shutdown, State};
	_ ->
	    {ok, State}
    end;
do_auto_shutdown(_Child, State=#state{auto_shutdown = all_significant}) ->
    case
	children_any(
	    fun
		(_, #child{pid = undefined}) ->
		    false;
		(_, #child{significant = true}) ->
		    true;
		(_, _) ->
		    false
	    end,
	    State#state.children
	)
    of
	true ->
	    {ok, State};
	false ->
	    {shutdown, State}
    end.

restart(Child, State) ->
    case add_restart(State) of
	{ok, NState} ->
	    case restart(NState#state.strategy, Child, NState) of
		{{try_again, TryAgainId}, NState2} ->
		    %% Leaving control back to gen_server before
		    %% trying again. This way other incoming requests
		    %% for the supervisor can be handled - e.g. a
		    %% shutdown request for the supervisor or the
		    %% child.
                    try_again_restart(TryAgainId),
		    {ok,NState2};
		Other ->
		    Other
	    end;
	{terminate, NState} ->
	    ?report_error(shutdown, reached_max_restart_intensity,
			 Child, State#state.name),
	    {shutdown, del_child(Child, NState)}
    end.

restart(simple_one_for_one, Child, State0) ->
    #child{pid = OldPid, mfargs = {M, F, A}} = Child,
    State1 = case OldPid of
		?restarting(_) ->
		    NRes = State0#state.dynamic_restarts - 1,
		    State0#state{dynamic_restarts = NRes};
		_ ->
		    State0
	    end,
    State2 = dyn_erase(OldPid, State1),
    case do_start_child_i(M, F, A) of
	{ok, Pid} ->
            NState = dyn_store(Pid, A, State2),
	    {ok, NState};
	{ok, Pid, _Extra} ->
            NState = dyn_store(Pid, A, State2),
	    {ok, NState};
	{error, Error} ->
            ROldPid = restarting(OldPid),
	    NRestarts = State2#state.dynamic_restarts + 1,
	    State3 = State2#state{dynamic_restarts = NRestarts},
            NState = dyn_store(ROldPid, A, State3),
	    ?report_error(start_error, Error, Child, NState#state.name),
	    {{try_again, ROldPid}, NState}
    end;
restart(one_for_one, #child{id=Id} = Child, State) ->
    OldPid = Child#child.pid,
    case do_start_child(State#state.name, Child) of
	{ok, Pid} ->
	    NState = set_pid(Pid, Id, State),
	    {ok, NState};
	{ok, Pid, _Extra} ->
	    NState = set_pid(Pid, Id, State),
	    {ok, NState};
	{error, Reason} ->
	    NState = set_pid(restarting(OldPid), Id, State),
	    ?report_error(start_error, Reason, Child, State#state.name),
	    {{try_again,Id}, NState}
    end;
restart(rest_for_one, #child{id=Id} = Child, #state{name=SupName} = State) ->
    {ChAfter, ChBefore} = split_child(Id, State#state.children),
    {Return, ChAfter2} = restart_multiple_children(Child, ChAfter, SupName),
    {Return, State#state{children = append(ChAfter2,ChBefore)}};
restart(one_for_all, Child, #state{name=SupName} = State) ->
    Children1 = del_child(Child#child.id, State#state.children),
    {Return, NChildren} = restart_multiple_children(Child, Children1, SupName),
    {Return, State#state{children = NChildren}}.

restart_multiple_children(Child, Children, SupName) ->
    Children1 = terminate_children(Children, SupName),
    case start_children(Children1, SupName) of
	{ok, NChildren} ->
	    {ok, NChildren};
	{error, NChildren, {failed_to_start_child, FailedId, _Reason}} ->
            NewPid = if FailedId =:= Child#child.id ->
                             restarting(Child#child.pid);
                        true ->
                             ?restarting(undefined)
                     end,
	    {{try_again, FailedId}, set_pid(NewPid,FailedId,NChildren)}
    end.

restarting(Pid) when is_pid(Pid) -> ?restarting(Pid);
restarting(RPid) -> RPid.

-spec try_again_restart(child_id() | {'restarting',pid()}) -> 'ok'.
try_again_restart(TryAgainId) ->
    gen_server:cast(self(), {try_again_restart, TryAgainId}).

%%-----------------------------------------------------------------
%% Func: terminate_children/2
%% Args: Children = children() % Ids in termination order
%%       SupName = {local, atom()} | {global, term()} | {pid(),Mod}
%% Returns: NChildren = children() % Ids in startup order
%%                                 % (reversed termination order)
%%-----------------------------------------------------------------
terminate_children(Children, SupName) ->
    Terminate =
        fun(_Id,Child) when ?is_temporary(Child) ->
                %% Temporary children should not be restarted and thus should
                %% be skipped when building the list of terminated children.
                do_terminate(Child, SupName),
                remove;
           (_Id,Child) ->
                do_terminate(Child, SupName),
                {update,Child#child{pid=undefined}}
        end,
    {ok,NChildren} = children_map(Terminate, Children),
    NChildren.

do_terminate(Child, SupName) when is_pid(Child#child.pid) ->
    case shutdown(Child) of
        ok ->
            ok;
        {error, OtherReason} ->
            ?report_error(shutdown_error, OtherReason, Child, SupName)
    end,
    ok;
do_terminate(_Child, _SupName) ->
    ok.

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
shutdown(#child{pid=Pid, shutdown=brutal_kill} = Child) ->
    Mon = monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Mon, process, Pid, Reason0} ->
            case unlink_flush(Pid, Reason0) of
                killed ->
                    ok;
                shutdown when not (?is_permanent(Child)) ->
                    ok;
                {shutdown, _} when not (?is_permanent(Child)) ->
                    ok;
                normal when not (?is_permanent(Child)) ->
                    ok;
                Reason ->
                    {error, Reason}
            end
    end;
shutdown(#child{pid=Pid, shutdown=Time} = Child) ->
    Mon = monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Mon, process, Pid, Reason0} ->
            case unlink_flush(Pid, Reason0) of
                shutdown ->
                    ok;
                {shutdown, _} when not (?is_permanent(Child)) ->
                    ok;
                normal when not (?is_permanent(Child)) ->
                    ok;
                Reason ->
                    {error, Reason}
            end
    after Time ->
        exit(Pid, kill),
        receive
            {'DOWN', Mon, process, Pid, Reason0} ->
                case unlink_flush(Pid, Reason0) of
                    shutdown ->
                        ok;
                    {shutdown, _} when not (?is_permanent(Child)) ->
                        ok;
                    normal when not (?is_permanent(Child)) ->
                        ok;
                    Reason ->
                        {error, Reason}
                end
        end
    end.

unlink_flush(Pid, DefaultReason) ->
    %% We call unlink in order to guarantee that the 'EXIT' has arrived
    %% from the dead process. See the unlink docs for details.
    unlink(Pid),
    receive
        {'EXIT', Pid, Reason} ->
            Reason
    after 0 ->
        DefaultReason
    end.

%%-----------------------------------------------------------------
%% Func: terminate_dynamic_children/1
%% Args: State
%% Returns: ok
%%
%% Shutdown all dynamic children. This happens when the supervisor is
%% stopped. Because the supervisor can have millions of dynamic children, we
%% can have a significative overhead here.
%%-----------------------------------------------------------------
terminate_dynamic_children(State) ->
    Child = get_dynamic_child(State),
    Pids = dyn_fold(
        fun
            (P, Acc) when is_pid(P) ->
                Mon = monitor(process, P),
                case Child#child.shutdown of
                    brutal_kill -> exit(P, kill);
                    _ -> exit(P, shutdown)
                end,
                Acc#{{P, Mon} => true};
            (?restarting(_), Acc) ->
                Acc
        end,
        #{},
        State
    ),
    TRef = case Child#child.shutdown of
        brutal_kill ->
            undefined;
        infinity ->
            undefined;
        Time ->
            erlang:start_timer(Time, self(), kill)
    end,
    Sz = maps:size(Pids),
    EStack = wait_dynamic_children(Child, Pids, Sz, TRef, #{}),
    %% Unroll stacked errors and report them
    maps:foreach(fun(Reason, Ls) ->
                      ?report_error(shutdown_error, Reason,
                                   Child#child{pid=Ls}, State#state.name)
              end, EStack).

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
        {'DOWN', Mon, process, Pid, Reason0}
          when is_map_key({Pid, Mon}, Pids) ->
            case unlink_flush(Pid, Reason0) of
                killed ->
                    wait_dynamic_children(Child, maps:remove({Pid, Mon}, Pids),
                                          Sz-1, TRef, EStack);

                shutdown when not (?is_permanent(Child)) ->
                    wait_dynamic_children(Child, maps:remove({Pid, Mon}, Pids),
                                          Sz-1, TRef, EStack);

                {shutdown, _} when not (?is_permanent(Child)) ->
                    wait_dynamic_children(Child, maps:remove({Pid, Mon}, Pids),
                                          Sz-1, TRef, EStack);

                normal when not (?is_permanent(Child)) ->
                    wait_dynamic_children(Child, maps:remove({Pid, Mon}, Pids),
                                          Sz-1, TRef, EStack);
                Reason ->
                    wait_dynamic_children(Child, maps:remove({Pid, Mon}, Pids),
                                          Sz-1, TRef, maps_prepend(Reason, Pid,
                                                                   EStack))
            end
    end;
wait_dynamic_children(Child, Pids, Sz, TRef, EStack) ->
    receive
        {'DOWN', Mon, process, Pid, Reason0}
          when is_map_key({Pid, Mon}, Pids) ->
            case unlink_flush(Pid, Reason0) of
                shutdown ->
                    wait_dynamic_children(Child, maps:remove({Pid, Mon}, Pids),
                                          Sz-1, TRef, EStack);

                {shutdown, _} when not (?is_permanent(Child)) ->
                    wait_dynamic_children(Child, maps:remove({Pid, Mon}, Pids),
                                          Sz-1, TRef, EStack);

                normal when not (?is_permanent(Child)) ->
                    wait_dynamic_children(Child, maps:remove({Pid, Mon}, Pids),
                                          Sz-1, TRef, EStack);

                Reason ->
                    wait_dynamic_children(Child, maps:remove({Pid, Mon}, Pids),
                                          Sz-1, TRef, maps_prepend(Reason, Pid,
                                                                   EStack))
            end;

        {timeout, TRef, kill} ->
            maps:foreach(fun({P, _}, _) -> exit(P, kill) end, Pids),
            wait_dynamic_children(Child, Pids, Sz, undefined, EStack)
    end.

maps_prepend(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, Values} ->
            maps:put(Key, [Value|Values], Map);
        error ->
            maps:put(Key, [Value], Map)
    end.

%%-----------------------------------------------------------------
%% Access #state.children
%%-----------------------------------------------------------------

%% Note we do not want to save the parameter list for temporary processes as
%% they will not be restarted, and hence we do not need this information.
%% Especially for dynamic children to simple_one_for_one supervisors
%% it could become very costly as it is not uncommon to spawn
%% very many such processes.
-spec save_child(child_rec(), state()) -> state().
save_child(#child{mfargs = {M, F, _}} = Child, State) when ?is_temporary(Child) ->
    do_save_child(Child#child{mfargs = {M, F, undefined}}, State);
save_child(Child, State) ->
    do_save_child(Child, State).

-spec do_save_child(child_rec(), state()) -> state().
do_save_child(#child{id = Id} = Child, #state{children = {Ids,Db}} = State) ->
    State#state{children = {[Id|Ids],Db#{Id => Child}}}.

-spec del_child(child_rec(), state()) -> state();
               (child_id(), children()) -> children().
del_child(#child{pid = Pid}, State) when ?is_simple(State) ->
    dyn_erase(Pid,State);
del_child(Child, State) when is_record(Child,child), is_record(State,state) ->
    NChildren = del_child(Child#child.id, State#state.children),
    State#state{children = NChildren};
del_child(Id, {Ids,Db}) ->
    case maps:get(Id, Db) of
        Child when Child#child.restart_type =:= temporary ->
            {lists:delete(Id, Ids), maps:remove(Id, Db)};
        Child ->
            {Ids, Db#{Id=>Child#child{pid=undefined}}}
    end.

%% In: {[S4, S3, Ch, S1, S0],Db}
%% Ret: {{[S4, S3, Ch],Db1}, {[S1, S0],Db2}}
%% Db1 and Db2 contain the keys in the lists they are associated with.
-spec split_child(child_id(), children()) -> {children(), children()}.
split_child(Id, {Ids,Db}) ->
    {IdsAfter,IdsBefore} = split_ids(Id, Ids, []),
    DbBefore = maps:with(IdsBefore,Db),
    #{Id:=Ch} = DbAfter = maps:with(IdsAfter,Db),
    {{IdsAfter,DbAfter#{Id=>Ch#child{pid=undefined}}},{IdsBefore,DbBefore}}.

split_ids(Id, [Id|Ids], After) ->
    {lists:reverse([Id|After]), Ids};
split_ids(Id, [Other|Ids], After) ->
    split_ids(Id, Ids, [Other | After]).

%% Find the child record for a given Pid (dynamic child) or Id
%% (non-dynamic child). This is called from the API functions.
-spec find_child(pid() | child_id(), state()) -> {ok,child_rec()} | error.
find_child(Pid, State) when is_pid(Pid), ?is_simple(State) ->
    case find_dynamic_child(Pid, State) of
        error ->
            case find_dynamic_child(restarting(Pid), State) of
                error ->
		    case erlang:is_process_alive(Pid) of
			true -> error;
			false -> {ok, get_dynamic_child(State)}
		    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
find_child(Id, #state{children = {_Ids,Db}}) ->
    maps:find(Id, Db).

%% Get the child record - either by child id or by pid.  If
%% simple_one_for_one, then insert the pid and args into the returned
%% child record. This is called when trying to restart the child.
-spec find_child_and_args(IdOrPid, state()) -> {ok, child_rec()} | error when
      IdOrPid :: pid() | {restarting,pid()} | child_id().
find_child_and_args(Pid, State) when ?is_simple(State) ->
    case find_dynamic_child(Pid, State) of
        {ok,#child{mfargs={M,F,_}} = Child} ->
            {ok, Args} = dyn_args(Pid, State),
            {ok, Child#child{mfargs = {M, F, Args}}};
        error ->
            error
    end;
find_child_and_args(Pid, State) when is_pid(Pid) ->
    find_child_by_pid(Pid, State);
find_child_and_args(Id, #state{children={_Ids,Db}})  ->
    maps:find(Id, Db).

%% Given the pid, find the child record for a dynamic child, and
%% include the pid in the returned record.
-spec find_dynamic_child(IdOrPid, state()) -> {ok, child_rec()} | error when
      IdOrPid :: pid() | {restarting,pid()} | child_id().
find_dynamic_child(Pid, State) ->
    case dyn_exists(Pid, State) of
        true ->
            Child = get_dynamic_child(State),
            {ok, Child#child{pid=Pid}};
        false ->
            error
    end.

%% Given the pid, find the child record for a non-dynamic child.
-spec find_child_by_pid(IdOrPid, state()) -> {ok,child_rec()} | error when
      IdOrPid :: pid() | {restarting,pid()}.
find_child_by_pid(Pid,#state{children={_Ids,Db}}) ->
    Fun = fun(_Id,#child{pid=P}=Ch,_) when P =:= Pid ->
                  throw(Ch);
             (_,_,error) ->
                  error
          end,
    try maps:fold(Fun,error,Db)
    catch throw:Child -> {ok,Child}
    end.

%% Get the child record from a simple_one_for_one supervisor - no pid
%% It is assumed that the child can always be found
-spec get_dynamic_child(state()) -> child_rec().
get_dynamic_child(#state{children={[Id],Db}}) ->
    #{Id := Child} = Db,
    Child.

%% Update pid in the given child record and store it in the process state
-spec set_pid(term(), child_id(), state()) -> state();
             (term(), child_id(), children()) -> children().
set_pid(Pid, Id, #state{children=Children} = State) ->
    State#state{children = set_pid(Pid, Id, Children)};
set_pid(Pid, Id, {Ids, Db}) ->
    NewDb = maps:update_with(Id, fun(Child) -> Child#child{pid=Pid} end, Db),
    {Ids,NewDb}.

%% Remove the Id and the child record from the process state
-spec remove_child(child_id(), state()) -> state().
remove_child(Id, #state{children={Ids,Db}} = State) ->
    NewIds = lists:delete(Id,Ids),
    NewDb = maps:remove(Id,Db),
    State#state{children = {NewIds,NewDb}}.

%% In the order of Ids, traverse the children and update each child
%% according to the return value of the Fun.
%% On error, abort and return the merge of the old and the updated map.
%% NOTE: The returned list of Ids is reverted compared to the input.
-spec children_map(Fun, children()) -> {ok, children()} |
                                       {error,children(),Reason} when
      Fun :: fun((child_id(),child_rec()) -> {update,child_rec()} |
                                             remove |
                                             {abort, Reason}),
      Reason :: term().
children_map(Fun,{Ids,Db}) ->
    children_map(Fun, Ids, Db, []).

children_map(Fun,[Id|Ids],Db,Acc) ->
    case Fun(Id,maps:get(Id,Db)) of
        {update,Child} ->
            children_map(Fun,Ids,Db#{Id => Child},[Id|Acc]);
        remove ->
            children_map(Fun,Ids,maps:remove(Id,Db),Acc);
        {abort,Reason} ->
            {error,{lists:reverse(Ids)++[Id|Acc],Db},Reason}
    end;
children_map(_Fun,[],Db,Acc) ->
    {ok,{Acc,Db}}.

%% In the order of Ids, map over all children and return the list
-spec children_to_list(Fun, children()) -> List when
      Fun :: fun((child_id(), child_rec()) -> Elem),
      List :: list(Elem),
      Elem :: term().
children_to_list(Fun,{Ids,Db}) ->
    children_to_list(Fun, Ids, Db, []).
children_to_list(Fun,[Id|Ids],Db,Acc) ->
    children_to_list(Fun,Ids,Db,[Fun(Id,maps:get(Id,Db))|Acc]);
children_to_list(_Fun,[],_Db,Acc) ->
    lists:reverse(Acc).

%% The order is not important - so ignore Ids
-spec children_fold(Fun, Acc0, children()) -> Acc1 when
      Fun :: fun((child_id(), child_rec(), AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().
children_fold(Fun,Init,{_Ids,Db}) ->
    maps:fold(Fun, Init, Db).

%% The order is not important - so ignore Ids
children_any(Pred, {_Ids, Db}) ->
    Iter=maps:iterator(Db),
    children_any1(Pred, maps:next(Iter)).

children_any1(_Pred, none) ->
    false;
children_any1(Pred, {Key, Value, Iter}) ->
    Pred(Key, Value) orelse children_any1(Pred, maps:next(Iter)).

-spec append(children(), children()) -> children().
append({Ids1,Db1},{Ids2,Db2}) ->
    {Ids1++Ids2,maps:merge(Db1,Db2)}.

%%-----------------------------------------------------------------
%% Func: init_state/4
%% Args: SupName = {local, atom()} | {global, term()} | self
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
			   args = Args,
			   auto_shutdown = never}).

set_flags(Flags, State) ->
    try check_flags(Flags) of
	#{strategy := Strategy, intensity := MaxIntensity, period := Period,
	  auto_shutdown := AutoShutdown} ->
	    {ok, State#state{strategy = Strategy,
			     intensity = MaxIntensity,
			     period = Period,
			     auto_shutdown = AutoShutdown}}
    catch
	Thrown -> Thrown
    end.

check_flags(SupFlags) when is_map(SupFlags) ->
    do_check_flags(maps:merge(?default_flags,SupFlags));
check_flags({Strategy, MaxIntensity, Period}) ->
    check_flags(#{strategy => Strategy,
		  intensity => MaxIntensity,
		  period => Period,
		  auto_shutdown => never});
check_flags(What) ->
    throw({invalid_type, What}).

do_check_flags(#{strategy := Strategy,
		 intensity := MaxIntensity,
		 period := Period,
		 auto_shutdown := AutoShutdown} = Flags) ->
    validStrategy(Strategy),
    validIntensity(MaxIntensity),
    validPeriod(Period),
    validAutoShutdown(AutoShutdown),
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

validAutoShutdown(never)           -> true;
validAutoShutdown(any_significant) -> true;
validAutoShutdown(all_significant) -> true;
validAutoShutdown(What)            -> throw({invalid_auto_shutdown, What}).


supname(self, Mod) -> {self(), Mod};
supname(N, _)      -> N.

%%% ------------------------------------------------------
%%% Check that the children start specification is valid.
%%% Input: [child_spec()]
%%%        auto_shutdown()
%%% Returns: {ok, [child_rec()]} | Error
%%% ------------------------------------------------------

check_startspec(Children, AutoShutdown) ->
    check_startspec(Children, [], #{}, AutoShutdown).

check_startspec([ChildSpec|T], Ids, Db, AutoShutdown) ->
    case check_childspec(ChildSpec, AutoShutdown) of
	{ok, #child{id=Id}=Child} ->
	    case maps:is_key(Id, Db) of
		%% The error message duplicate_child_name is kept for
		%% backwards compatibility, although
		%% duplicate_child_id would be more correct.
		true -> {duplicate_child_name, Id};
		false -> check_startspec(T, [Id | Ids], Db#{Id=>Child},
					 AutoShutdown)
	    end;
	Error -> Error
    end;
check_startspec([], Ids, Db, _AutoShutdown) ->
    {ok, {lists:reverse(Ids),Db}}.

check_childspec(ChildSpec, AutoShutdown) when is_map(ChildSpec) ->
    catch do_check_childspec(maps:merge(?default_child_spec,ChildSpec),
			     AutoShutdown);
check_childspec({Id, Func, RestartType, Shutdown, ChildType, Mods},
		AutoShutdown) ->
    check_childspec(#{id => Id,
		      start => Func,
		      restart => RestartType,
		      significant => false,
		      shutdown => Shutdown,
		      type => ChildType,
		      modules => Mods},
		    AutoShutdown);
check_childspec(X, _AutoShutdown) -> {invalid_child_spec, X}.

do_check_childspec(#{restart := RestartType,
		     type := ChildType} = ChildSpec,
		   AutoShutdown)->
    Id = case ChildSpec of
	       #{id := I} -> I;
	       _ -> throw(missing_id)
	   end,
    Func = case ChildSpec of
	       #{start := F} -> F;
	       _ -> throw(missing_start)
	   end,
    validId(Id),
    validFunc(Func),
    validRestartType(RestartType),
    Significant = case ChildSpec of
		      #{significant := Signf} -> Signf;
		      _ -> false
                  end,
    validSignificant(Significant, RestartType, AutoShutdown),
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
    {ok, #child{id = Id, mfargs = Func, restart_type = RestartType,
		significant = Significant, shutdown = Shutdown,
		child_type = ChildType, modules = Mods}}.

validChildType(supervisor) -> true;
validChildType(worker) -> true;
validChildType(What) -> throw({invalid_child_type, What}).

validId(_Id) -> true.

validFunc({M, F, A}) when is_atom(M), 
                          is_atom(F), 
                          is_list(A) -> true;
validFunc(Func)                      -> throw({invalid_mfa, Func}).

validRestartType(permanent)   -> true;
validRestartType(temporary)   -> true;
validRestartType(transient)   -> true;
validRestartType(RestartType) -> throw({invalid_restart_type, RestartType}).

validSignificant(true, _RestartType, never) ->
    throw({bad_combination, [{auto_shutdown, never}, {significant, true}]});
validSignificant(true, permanent, _AutoShutdown) ->
    throw({bad_combination, [{restart, permanent}, {significant, true}]});
validSignificant(Significant, _RestartType, _AutoShutdown)
  when is_boolean(Significant) ->
    true;
validSignificant(Significant, _RestartType, _AutoShutdown) ->
    throw({invalid_significant, Significant}).

validShutdown(Shutdown)
  when is_integer(Shutdown), Shutdown >= 0 -> true;
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

child_to_spec(#child{id = Id,
		    mfargs = Func,
		    restart_type = RestartType,
		    significant = Significant,
		    shutdown = Shutdown,
		    child_type = ChildType,
		    modules = Mods}) ->
    #{id => Id,
      start => Func,
      restart => RestartType,
      significant => Significant,
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
    R1 = add_restart(R, Now, P),
    State1 = State#state{restarts = R1},
    case length(R1) of
	CurI when CurI  =< I ->
	    {ok, State1};
	_ ->
	    {terminate, State1}
    end.

add_restart(Restarts0, Now, Period) ->
    Treshold = Now - Period,
    Restarts1 = lists:takewhile(
                  fun (R) -> R >= Treshold end,
                  Restarts0
                 ),
    [Now | Restarts1].

%%% ------------------------------------------------------
%%% Error and progress reporting.
%%% ------------------------------------------------------
extract_child(Child) when is_list(Child#child.pid) ->
    [{nb_children, length(Child#child.pid)},
     {id, Child#child.id},
     {mfargs, Child#child.mfargs},
     {restart_type, Child#child.restart_type},
     {significant, Child#child.significant},
     {shutdown, Child#child.shutdown},
     {child_type, Child#child.child_type}];
extract_child(Child) ->
    [{pid, Child#child.pid},
     {id, Child#child.id},
     {mfargs, Child#child.mfargs},
     {restart_type, Child#child.restart_type},
     {significant, Child#child.significant},
     {shutdown, Child#child.shutdown},
     {child_type, Child#child.child_type}].

report_progress(Child, SupName) ->
    ?LOG_INFO(#{label=>{supervisor,progress},
                report=>[{supervisor,SupName},
                         {started,extract_child(Child)}]},
              #{domain=>[otp,sasl],
                report_cb=>fun supervisor:format_log/2,
                logger_formatter=>#{title=>"PROGRESS REPORT"},
                error_logger=>#{tag=>info_report,
                                type=>progress,
                                report_cb=>fun supervisor:format_log/1}}).

%% format_log/1 is the report callback used by Logger handler
%% error_logger only. It is kept for backwards compatibility with
%% legacy error_logger event handlers. This function must always
%% return {Format,Args} compatible with the arguments in this module's
%% calls to error_logger prior to OTP-21.0.
format_log(LogReport) ->
    Depth = error_logger:get_format_depth(),
    FormatOpts = #{chars_limit => unlimited,
                   depth => Depth,
                   single_line => false,
                   encoding => utf8},
    format_log_multi(limit_report(LogReport, Depth), FormatOpts).

limit_report(LogReport, unlimited) ->
    LogReport;
limit_report(#{label:={supervisor,progress},
               report:=[{supervisor,_}=Supervisor,{started,Child}]}=LogReport,
             Depth) ->
    LogReport#{report=>[Supervisor,
                        {started,limit_child_report(Child, Depth)}]};
limit_report(#{label:={supervisor,_Error},
               report:=[{supervisor,_}=Supervisor,{errorContext,Ctxt},
                        {reason,Reason},{offender,Child}]}=LogReport,
             Depth) ->
    LogReport#{report=>[Supervisor,
                        {errorContext,io_lib:limit_term(Ctxt, Depth)},
                        {reason,io_lib:limit_term(Reason, Depth)},
                        {offender,limit_child_report(Child, Depth)}]}.

limit_child_report(Report, Depth) ->
    io_lib:limit_term(Report, Depth).

%% format_log/2 is the report callback for any Logger handler, except
%% error_logger.
format_log(Report, FormatOpts0) ->
    Default = #{chars_limit => unlimited,
                depth => unlimited,
                single_line => false,
                encoding => utf8},
    FormatOpts = maps:merge(Default, FormatOpts0),
    IoOpts =
        case FormatOpts of
            #{chars_limit:=unlimited} ->
                [];
            #{chars_limit:=Limit} ->
                [{chars_limit,Limit}]
        end,
    {Format,Args} = format_log_single(Report, FormatOpts),
    io_lib:format(Format, Args, IoOpts).

format_log_single(#{label:={supervisor,progress},
                    report:=[{supervisor,SupName},{started,Child}]},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    {ChildFormat,ChildArgs} = format_child_log_single(Child, "Started:"),
    Format = "Supervisor: "++P++".",
    Args =
        case Depth of
            unlimited ->
                [SupName];
            _ ->
                [SupName,Depth]
        end,
    {Format++ChildFormat,Args++ChildArgs};
format_log_single(#{label:={supervisor,_Error},
                    report:=[{supervisor,SupName},
                             {errorContext,Ctxt},
                             {reason,Reason},
                             {offender,Child}]},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format = lists:append(["Supervisor: ",P,". Context: ",P,
                            ". Reason: ",P,"."]),
    {ChildFormat,ChildArgs} = format_child_log_single(Child, "Offender:"),
    Args =
        case Depth of
            unlimited ->
                [SupName,Ctxt,Reason];
            _ ->
                [SupName,Depth,Ctxt,Depth,Reason,Depth]
        end,
    {Format++ChildFormat,Args++ChildArgs};
format_log_single(Report,FormatOpts) ->
    format_log_multi(Report,FormatOpts).

format_log_multi(#{label:={supervisor,progress},
                   report:=[{supervisor,SupName},
                            {started,Child}]},
                 #{depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format =
        lists:append(
          ["    supervisor: ",P,"~n",
           "    started: ",P,"~n"]),
    Args =
        case Depth of
            unlimited ->
                [SupName,Child];
            _ ->
                [SupName,Depth,Child,Depth]
        end,
    {Format,Args};
format_log_multi(#{label:={supervisor,_Error},
                   report:=[{supervisor,SupName},
                            {errorContext,Ctxt},
                            {reason,Reason},
                            {offender,Child}]},
                 #{depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format =
        lists:append(
          ["    supervisor: ",P,"~n",
           "    errorContext: ",P,"~n",
           "    reason: ",P,"~n",
           "    offender: ",P,"~n"]),
    Args =
        case Depth of
            unlimited ->
                [SupName,Ctxt,Reason,Child];
            _ ->
                [SupName,Depth,Ctxt,Depth,Reason,Depth,Child,Depth]
        end,
    {Format,Args}.

format_child_log_single(Child, Tag) ->
    {id,Id} = lists:keyfind(id, 1, Child),
    case lists:keyfind(pid, 1, Child) of
        false ->
            {nb_children,NumCh} = lists:keyfind(nb_children, 1, Child),
            {" ~s id=~w,nb_children=~w.", [Tag,Id,NumCh]};
        T when is_tuple(T) ->
            {pid,Pid} = lists:keyfind(pid, 1, Child),
            {" ~s id=~w,pid=~w.", [Tag,Id,Pid]}
    end.

p(#{single_line:=Single,depth:=Depth,encoding:=Enc}) ->
    "~"++single(Single)++mod(Enc)++p(Depth);
p(unlimited) ->
    "p";
p(_Depth) ->
    "P".

single(true) -> "0";
single(false) -> "".

mod(latin1) -> "";
mod(_) -> "t".

format_status(terminate, [_PDict, State]) ->
    State;
format_status(_, [_PDict, State]) ->
    [{data, [{"State", State}]},
     {supervisor, [{"Callback", State#state.module}]}].

%%%-----------------------------------------------------------------
%%% Dynamics database access.
%%%
%%% Store all dynamic children in a map with the pid as the key. If
%%% the children are permanent, store the start arguments as the value,
%%% otherwise store [] as the value.
%%%

dyn_size(#state{dynamics = {_Kind,Db}}) ->
    map_size(Db).

dyn_erase(Pid,#state{dynamics={Kind,Db}}=State) ->
    State#state{dynamics={Kind,maps:remove(Pid,Db)}}.

dyn_store(Pid,Args,#state{dynamics={Kind,Db}}=State) ->
    case Kind of
        mapsets ->
            %% Children are temporary. The start arguments
            %% will not be needed again. Store [].
            State#state{dynamics={mapsets,Db#{Pid => []}}};
        maps ->
            %% Children are permanent and may be restarted.
            %% Store the start arguments.
            State#state{dynamics={maps,Db#{Pid => Args}}}
    end.

dyn_fold(Fun,Init,#state{dynamics={_Kind,Db}}) ->
    maps:fold(fun(Pid,_,Acc) -> Fun(Pid,Acc) end, Init, Db).

dyn_map(Fun, #state{dynamics={_Kind,Db}}) ->
    lists:map(Fun, maps:keys(Db)).

dyn_exists(Pid, #state{dynamics={_Kind, Db}}) ->
    is_map_key(Pid, Db).

dyn_args(_Pid, #state{dynamics={mapsets, _Db}}) ->
    {ok,undefined};
dyn_args(Pid, #state{dynamics={maps, Db}}) ->
    maps:find(Pid, Db).

dyn_init(State) ->
    dyn_init(get_dynamic_child(State),State).

dyn_init(Child,State) when ?is_temporary(Child) ->
    State#state{dynamics={mapsets,maps:new()}};
dyn_init(_Child,State) ->
    State#state{dynamics={maps,maps:new()}}.

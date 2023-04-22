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
-module(application).

-export([ensure_all_started/1, ensure_all_started/2, ensure_all_started/3,
	 start/1, start/2,
	 start_boot/1, start_boot/2, stop/1, 
	 load/1, load/2, unload/1, takeover/2,
	 which_applications/0, which_applications/1,
	 loaded_applications/0, permit/2]).
-export([ensure_started/1, ensure_started/2]).
-export([set_env/1, set_env/2, set_env/3, set_env/4, unset_env/2, unset_env/3]).
-export([get_env/1, get_env/2, get_env/3, get_all_env/0, get_all_env/1]).
-export([get_key/1, get_key/2, get_all_key/0, get_all_key/1]).
-export([get_application/0, get_application/1, get_supervisor/1, info/0]).
-export([start_type/0]).

-export_type([start_type/0]).

%%%-----------------------------------------------------------------

-type start_type() :: 'normal'
                    | {'takeover', Node :: node()}
                    | {'failover', Node :: node()}.
-type restart_type() :: 'permanent' | 'transient' | 'temporary'.
-type application_opt() :: {'description', Description :: string()}
                         | {'vsn', Vsn :: string()}
                         | {'id', Id :: string()}
                         | {'modules', [Module :: module()]}
                         | {'registered', Names :: [Name :: atom()]}
                         | {'applications', [Application :: atom()]}
                         | {'included_applications', [Application :: atom()]}
                         | {'env', [{Par :: atom(), Val :: term()}]}
                         | {'start_phases',
                            [{Phase :: atom(), PhaseArgs :: term()}] | 'undefined'}
                         | {'maxT', MaxT :: timeout()}          % max timeout
                         | {'maxP',
                            MaxP :: pos_integer() | 'infinity'} % max processes
                         | {'mod', Start :: {Module :: module(), StartArgs :: term()}}.
-type application_spec() :: {'application',
                             Application :: atom(),
                             AppSpecKeys :: [application_opt()]}.

-type(tuple_of(_T) :: tuple()).

%%------------------------------------------------------------------

-callback start(StartType :: start_type(), StartArgs :: term()) ->
    {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.

-callback stop(State :: term()) ->
    term().

%%%-----------------------------------------------------------------
%%% This module is API towards application_controller and
%%% application_master.
%%%-----------------------------------------------------------------

-spec load(AppDescr) -> 'ok' | {'error', Reason} when
      AppDescr :: Application | (AppSpec :: application_spec()),
      Application :: atom(),
      Reason :: term().

load(Application) ->
    load1(Application, []).

-spec load(AppDescr, Distributed) -> 'ok' | {'error', Reason} when
      AppDescr :: Application | (AppSpec :: application_spec()),
      Application :: atom(),
      Distributed :: {Application,Nodes}
                   | {Application,Time,Nodes}
                   | 'default',
      Nodes :: [node() | tuple_of(node())],
      Time :: pos_integer(),
      Reason :: term().

load(Application, DistNodes) ->
    load1(Application, DistNodes).

%% Workaround due to specs.
load1(Application, DistNodes) ->
    case application_controller:load_application(Application) of
	ok when DistNodes =/= [] ->
	    AppName = get_appl_name(Application),
	    case dist_ac:load_application(AppName, DistNodes) of
		ok ->
		    ok;
		{error, R} ->
		    application_controller:unload_application(AppName),
		    {error, R}
	    end;
	Else ->
	    Else
    end.

-spec unload(Application) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Reason :: term().

unload(Application) ->
    application_controller:unload_application(Application).


-spec ensure_all_started(Applications) -> {'ok', Started} | {'error', Reason} when
      Applications :: atom() | [atom()],
      Started :: [atom()],
      Reason :: term().
ensure_all_started(Application) ->
    ensure_all_started(Application, temporary, serial).

-spec ensure_all_started(Applications, Type) -> {'ok', Started} | {'error', AppReason} when
      Applications :: atom() | [atom()],
      Type :: restart_type(),
      Started :: [atom()],
      AppReason :: {atom(), term()}.
ensure_all_started(Application, Type) ->
    ensure_all_started(Application, Type, serial).

-spec ensure_all_started(Applications, Type, Mode) -> {'ok', Started} | {'error', AppReason} when
      Applications :: atom() | [atom()],
      Type :: restart_type(),
      Mode :: serial | concurrent,
      Started :: [atom()],
      AppReason :: {atom(), term()}.
ensure_all_started(Application, Type, Mode) when is_atom(Application) ->
    ensure_all_started([Application], Type, Mode);
ensure_all_started(Applications, Type, Mode) when is_list(Applications) ->
    Opts = #{type => Type, mode => Mode},

    case enqueue_or_start(Applications, [], #{}, [], [], Opts) of
        {ok, DAG, _Pending, Started} when Mode =:= concurrent ->
            ReqIDs = gen_server:reqids_new(),
            concurrent_dag_start(maps:to_list(DAG), ReqIDs, [], Started, Type);
        {ok, DAG, _Pending, Started} when Mode =:= serial ->
            0 = map_size(DAG),
            {ok, lists:reverse(Started)};
        {error, AppReason, Started} ->
            _ = [stop(Name) || Name <- Started],
            {error, AppReason}
    end.

enqueue_or_start([App | Apps], Optional, DAG, Pending, Started, Opts)
  when is_map_key(App, DAG) ->
    %% We already traversed the application, so only add it as pending
    enqueue_or_start(Apps, Optional, DAG, [App | Pending], Started, Opts);

enqueue_or_start([App | Apps], Optional, DAG, Pending, Started, Opts) when is_atom(App) ->
    %% In case the app is already running, we just skip it instead
    %% of attempting to start all of its children - which would
    %% have already been loaded and started anyway.
    case application_controller:is_running(App) of
        false ->
            case ensure_loaded(App) of
                {ok, Name} ->
                    case enqueue_or_start_app(Name, App, DAG, Pending, Started, Opts) of
                        {ok, NewDAG, NewPending, NewStarted} ->
                            enqueue_or_start(Apps, Optional, NewDAG, NewPending, NewStarted, Opts);
                        ErrorAppReasonStarted ->
                            ErrorAppReasonStarted
                    end;
                {error, {"no such file or directory", _} = Reason} ->
                    case lists:member(App, Optional) of
                        true ->
                            enqueue_or_start(Apps, Optional, DAG, Pending, Started, Opts);
                        false ->
                            {error, {App, Reason}, Started}
                    end;
                {error, Reason} ->
                    {error, {App, Reason}, Started}
            end;
        true ->
            enqueue_or_start(Apps, Optional, DAG, Pending, Started, Opts)
    end;
enqueue_or_start([], _Optional, DAG, Pending, Started, _Opts) ->
    {ok, DAG, Pending, Started}.

enqueue_or_start_app(Name, App, DAG, Pending, Started, Opts) ->
    #{type := Type, mode := Mode} = Opts,
    {ok, ChildApps} = get_key(Name, applications),
    {ok, OptionalApps} = get_key(Name, optional_applications),
    {ok, Mod} = get_key(Name, mod),

    %% If the application has no dependencies and we are either
    %% on serial mode or the app does not have a module callback,
    %% we start it immediately. At the end of serial mode, the DAG
    %% is always empty.
    case enqueue_or_start(ChildApps, OptionalApps, DAG, [], Started, Opts) of
        {ok, NewDAG, NewPending, NewStarted}
        when NewPending =:= [], (Mode =:= serial) or (Mod =:= []) ->
            case application_controller:start_application(App, Type) of
                ok ->
                    {ok, NewDAG, Pending, [App | NewStarted]};
                {error, {already_started, App}} ->
                    {ok, NewDAG, Pending, NewStarted};
                {error, Reason} ->
                    {error, {App, Reason}, NewStarted}
            end;
        {ok, NewDAG, NewPending, NewStarted} ->
            {ok, NewDAG#{App => NewPending}, [App | Pending], NewStarted};
        ErrorAppReasonStarted ->
            ErrorAppReasonStarted
    end.

concurrent_dag_start([], ReqIDs, _Done, Started, _Type) ->
    wait_all_enqueued(ReqIDs, Started, false);
concurrent_dag_start(Pending0, ReqIDs0, Done, Started0, Type) ->
    {Pending1, ReqIDs1} = enqueue_dag_leaves(Pending0, ReqIDs0, [], Done, Type),

    case wait_one_enqueued(ReqIDs1, Started0) of
        {ok, App, ReqIDs2, Started1} ->
            concurrent_dag_start(Pending1, ReqIDs2, [App], Started1, Type);
        {error, AppReason, ReqIDs2} ->
            wait_all_enqueued(ReqIDs2, Started0, AppReason)
    end.

enqueue_dag_leaves([{App, Children} | Rest], ReqIDs, Acc, Done, Type) ->
    case Children -- Done of
        [] ->
            Req = application_controller:start_application_request(App, Type),
            NewReqIDs = gen_server:reqids_add(Req, App, ReqIDs),
            enqueue_dag_leaves(Rest, NewReqIDs, Acc, Done, Type);
        NewChildren ->
            NewAcc = [{App, NewChildren} | Acc],
            enqueue_dag_leaves(Rest, ReqIDs, NewAcc, Done, Type)
    end;
enqueue_dag_leaves([], ReqIDs, Acc, _Done, _Type) ->
    {Acc, ReqIDs}.

wait_one_enqueued(ReqIDs0, Started) ->
    case gen_server:wait_response(ReqIDs0, infinity, true) of
        {{reply, ok}, App, ReqIDs1} ->
            {ok, App, ReqIDs1, [App | Started]};
        {{reply, {error, {already_started, App}}}, App, ReqIDs1} ->
            {ok, App, ReqIDs1, Started};
        {{reply, {error, Reason}}, App, ReqIDs1} ->
            {error, {App, Reason}, ReqIDs1};
        {{error, {Reason, _Ref}}, _App, _ReqIDs1} ->
            exit(Reason);
        no_request ->
            exit(deadlock)
    end.

wait_all_enqueued(ReqIDs0, Started0, LastAppReason) ->
    case gen_server:reqids_size(ReqIDs0) of
        0 when LastAppReason =:= false ->
            {ok, lists:reverse(Started0)};
        0 ->
            _ = [stop(App) || App <- Started0],
            {error, LastAppReason};
        _ ->
            case wait_one_enqueued(ReqIDs0, Started0) of
                {ok, _App, ReqIDs1, Started1} ->
                    wait_all_enqueued(ReqIDs1, Started1, LastAppReason);
                {error, NewAppReason, ReqIDs1} ->
                    wait_all_enqueued(ReqIDs1, Started0, NewAppReason)
            end
    end.

-spec start(Application) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Reason :: term().

start(Application) ->
    start(Application, temporary).

-spec start(Application, Type) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Type :: restart_type(),
      Reason :: term().

start(Application, RestartType) ->
    case ensure_loaded(Application) of
	{ok, Name} ->
	    application_controller:start_application(Name, RestartType);
	Error ->
	    Error
    end.

ensure_loaded(Application) ->
    case load(Application) of
	ok ->
	    {ok, get_appl_name(Application)};
	{error, {already_loaded, Name}} ->
	    {ok, Name};
	Error ->
	    Error
    end.

-spec ensure_started(Application) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Reason :: term().

ensure_started(Application) ->
    ensure_started(Application, temporary).

-spec ensure_started(Application, Type) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Type :: restart_type(),
      Reason :: term().

ensure_started(Application, RestartType) ->
    case start(Application, RestartType) of
	ok ->
	    ok;
	{error, {already_started, Application}} ->
	    ok;
	Error ->
	    Error
    end.

-spec start_boot(Application :: atom()) -> 'ok' | {'error', term()}.

start_boot(Application) ->
    start_boot(Application, temporary).

-spec start_boot(Application :: atom(), RestartType :: restart_type()) ->
	     'ok' | {'error', term()}.

start_boot(Application, RestartType) ->
    application_controller:start_boot_application(Application, RestartType).

-spec takeover(Application, Type) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Type :: restart_type(),
      Reason :: term().

takeover(Application, RestartType) ->
    dist_ac:takeover_application(Application, RestartType).

-spec permit(Application, Permission) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Permission :: boolean(),
      Reason :: term().

permit(Application, Bool) ->
    case Bool of
	true -> ok;
	false -> ok;
	Bad -> exit({badarg, {?MODULE, permit, [Application, Bad]}})
    end,
    case application_controller:permit_application(Application, Bool) of
	distributed_application ->
	    dist_ac:permit_application(Application, Bool);
	{distributed_application, only_loaded} ->
	    dist_ac:permit_only_loaded_application(Application, Bool);
	LocalResult ->
	    LocalResult
    end.

-spec stop(Application) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Reason :: term().

stop(Application) ->
    application_controller:stop_application(Application).

-spec which_applications() -> [{Application, Description, Vsn}] when
      Application :: atom(),
      Description :: string(),
      Vsn :: string().

which_applications() ->
    application_controller:which_applications().

-spec which_applications(Timeout) -> [{Application, Description, Vsn}] when
      Timeout :: timeout(),
      Application :: atom(),
      Description :: string(),
      Vsn :: string().

which_applications(infinity) ->
    application_controller:which_applications(infinity);
which_applications(Timeout) when is_integer(Timeout), Timeout>=0 ->
    application_controller:which_applications(Timeout).

-spec loaded_applications() -> [{Application, Description, Vsn}] when
      Application :: atom(),
      Description :: string(),
      Vsn :: string().

loaded_applications() -> 
    application_controller:loaded_applications().

-spec info() -> term().

info() -> 
    application_controller:info().

-spec set_env(Config) -> 'ok' when
      Config :: [{Application, Env}],
      Application :: atom(),
      Env :: [{Par :: atom(), Val :: term()}].

set_env(Config) when is_list(Config) ->
    set_env(Config, []).

-spec set_env(Config, Opts) -> 'ok' when
      Config :: [{Application, Env}],
      Application :: atom(),
      Env :: [{Par :: atom(), Val :: term()}],
      Opts :: [{timeout, timeout()} | {persistent, boolean()}].

set_env(Config, Opts) when is_list(Config), is_list(Opts) ->
    case application_controller:set_env(Config, Opts) of
	ok -> ok;
	{error, Msg} -> erlang:error({badarg, Msg}, [Config, Opts])
    end.

-spec set_env(Application, Par, Val) -> 'ok' when
      Application :: atom(),
      Par :: atom(),
      Val :: term().

set_env(Application, Key, Val) -> 
    application_controller:set_env(Application, Key, Val).

-spec set_env(Application, Par, Val, Opts) -> 'ok' when
      Application :: atom(),
      Par :: atom(),
      Val :: term(),
      Opts :: [{timeout, timeout()} | {persistent, boolean()}].

set_env(Application, Key, Val, infinity) ->
    set_env(Application, Key, Val, [{timeout, infinity}]);
set_env(Application, Key, Val, Timeout) when is_integer(Timeout), Timeout>=0 ->
    set_env(Application, Key, Val, [{timeout, Timeout}]);
set_env(Application, Key, Val, Opts) when is_list(Opts) ->
    application_controller:set_env(Application, Key, Val, Opts).

-spec unset_env(Application, Par) -> 'ok' when
      Application :: atom(),
      Par :: atom().

unset_env(Application, Key) -> 
    application_controller:unset_env(Application, Key).

-spec unset_env(Application, Par, Opts) -> 'ok' when
      Application :: atom(),
      Par :: atom(),
      Opts :: [{timeout, timeout()} | {persistent, boolean()}].

unset_env(Application, Key, infinity) ->
    unset_env(Application, Key, [{timeout, infinity}]);
unset_env(Application, Key, Timeout) when is_integer(Timeout), Timeout>=0 ->
    unset_env(Application, Key, [{timeout, Timeout}]);
unset_env(Application, Key, Opts) when is_list(Opts) ->
    application_controller:unset_env(Application, Key, Opts).

-spec get_env(Par) -> 'undefined' | {'ok', Val} when
      Par :: atom(),
      Val :: term().

get_env(Key) -> 
    application_controller:get_pid_env(group_leader(), Key).

-spec get_env(Application, Par) -> 'undefined' | {'ok', Val} when
      Application :: atom(),
      Par :: atom(),
      Val :: term().

get_env(Application, Key) -> 
    application_controller:get_env(Application, Key).

-spec get_env(Application, Par, Def) -> Val when
      Application :: atom(),
      Par :: atom(),
      Def :: term(),
      Val :: term().

get_env(Application, Key, Default) ->
    application_controller:get_env(Application, Key, Default).

-spec get_all_env() -> Env when
      Env :: [{Par :: atom(), Val :: term()}].

get_all_env() -> 
    application_controller:get_pid_all_env(group_leader()).

-spec get_all_env(Application) -> Env when
      Application :: atom(),
      Env :: [{Par :: atom(), Val :: term()}].

get_all_env(Application) -> 
    application_controller:get_all_env(Application).

-spec get_key(Key) -> 'undefined' | {'ok', Val} when
      Key :: atom(),
      Val :: term().

get_key(Key) -> 
    application_controller:get_pid_key(group_leader(), Key).

-spec get_key(Application, Key) -> 'undefined' | {'ok', Val} when
      Application :: atom(),
      Key :: atom(),
      Val :: term().

get_key(Application, Key) -> 
    application_controller:get_key(Application, Key).

-spec get_all_key() -> [] | {'ok', Keys} when
      Keys :: [{Key :: atom(),Val :: term()},...].

get_all_key() ->
    application_controller:get_pid_all_key(group_leader()).

-spec get_all_key(Application) -> 'undefined' | Keys when
      Application :: atom(),
      Keys :: {'ok', [{Key :: atom(),Val :: term()},...]}.

get_all_key(Application) -> 
    application_controller:get_all_key(Application).

-spec get_application() -> 'undefined' | {'ok', Application} when
      Application :: atom().

get_application() -> 
    application_controller:get_application(group_leader()).

-spec get_application(PidOrModule) -> 'undefined' | {'ok', Application} when
      PidOrModule :: (Pid :: pid()) | (Module :: module()),
      Application :: atom().

get_application(Pid) when is_pid(Pid) ->
    case process_info(Pid, group_leader) of
	{group_leader, Gl} ->
	    application_controller:get_application(Gl);
	undefined ->
	    undefined
    end;
get_application(Module) when is_atom(Module) ->
    application_controller:get_application_module(Module).

-spec get_supervisor(Application) -> 'undefined' | {'ok', Pid} when
      Pid :: pid(),
      Application :: atom().

get_supervisor(Application) when is_atom(Application) ->
    case application_controller:get_master(Application) of
        undefined -> undefined;
        Master ->
            case application_master:get_child(Master) of
                {Root, _App} -> {ok, Root};
                error -> undefined
            end
    end.

-spec start_type() -> StartType | 'undefined' | 'local' when
      StartType :: start_type().

start_type() ->
    application_controller:start_type(group_leader()).

%% Internal
get_appl_name(Name) when is_atom(Name) -> Name;
get_appl_name({application, Name, _}) when is_atom(Name) -> Name.

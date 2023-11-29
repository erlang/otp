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
	 start_boot/1, start_boot/2, stop/1, ensure_all_stopped/1, ensure_all_stopped/2,
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

-type graph() :: #{
    children := #{atom() => #{atom() => boolean()}},
    parents := #{atom() => #{atom() => []}}
   }.

-type mode() :: serial | concurrent.

-type action() :: {start, start_type()} | stop.

%%------------------------------------------------------------------

-callback start(StartType :: start_type(), StartArgs :: term()) ->
    {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.

-callback stop(State :: term()) ->
    term().

-callback config_change(Changed, New, Removed) -> ok when
      Changed :: [{Par, Val}],
      New :: [{Par, Val}],
      Removed :: [Par],
      Par :: atom(),
      Val :: term().

-callback prep_stop(State) -> NewState when
      State :: term(), NewState :: term().

-callback start_phase(Phase, StartType, PhaseArgs) ->
    ok | {error, Reason} when
      Phase :: atom(),
      StartType :: start_type(),
      PhaseArgs :: term(),
      Reason :: term().

-optional_callbacks([config_change/3, prep_stop/1, start_phase/3]).

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
    case build_dependency_graph(Applications, all) of
        {ok, Graph} -> traverse({start, Type}, Mode, Graph);
        {error, _} = Error -> Error
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

-spec ensure_all_stopped(Applications) -> {'ok', Applications} | {'error', Reason} when
    Applications :: [atom()],
    Reason :: term().
    
ensure_all_stopped(Applications) ->
    ensure_all_stopped(Applications, serial).

-spec ensure_all_stopped(Applications, Mode) -> {'ok', Applications} | {'error', Reason} when
    Applications :: [atom()],
    Mode :: serial | concurrent,
    Reason :: term().
    
ensure_all_stopped(Applications, Mode) ->
    case build_dependency_graph(Applications, restricted) of
        {ok, Graph} -> traverse(stop, Mode, Graph);
        {error, _} = Error -> Error
    end.

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

-spec build_dependency_graph([atom()], all | restricted) -> {ok, graph()} | {error, term()}.

build_dependency_graph(Apps, Mode) ->
    AllowedApps =
        case Mode of
            all -> all;
            restricted -> #{App => [] || App <- Apps}
        end,
    build_dependency_graph(
        [{App, true} || App <- Apps], 
        AllowedApps, 
        #{}, 
        _Parents = #{}, 
        _Children = #{}).

-spec build_dependency_graph(
        [{atom(), boolean()}], 
        all | #{atom() => []}, 
        #{atom() => boolean()}, #{atom() => #{atom() => []}}, 
        #{atom() => #{atom() => boolean()}}) -> {ok, graph()} | {error, term()}.
build_dependency_graph([], _, _Seen, Parents, Children) ->
    {ok, #{parents => Parents, children => Children}};
build_dependency_graph([{App, _} | NextApps], AllowedApps, Seen, Parents, Children) 
  when erlang:is_map_key(App, Seen) ->
    build_dependency_graph(NextApps, AllowedApps, Seen, Parents, Children);
build_dependency_graph([{App, Required} | NextApps0], AllowedApps, Seen, Parents0, Children0) ->
    case ensure_loaded(App) of
        {ok, Name} ->
            {ok, ChildApps0} = get_key(Name, applications),
            {ok, OptionalApps0} = get_key(Name, optional_applications),
            ChildApps1 = [ChildApp || ChildApp <- ChildApps0, is_allowed(ChildApp, AllowedApps)],
            OptionalApps1 = [OptionalApp || OptionalApp <- OptionalApps0, is_allowed(OptionalApp, AllowedApps)],
            ChildApps2 =
                #{
                  ChildApp => true
                  || ChildApp <- ChildApps1,
                     not maps:is_key(ChildApp, Seen) orelse not maps:get(ChildApp, Seen)
                 },
            OptionalApps2 =
                #{
                  ChildApp => true
                  || ChildApp <- OptionalApps1,
                     not maps:is_key(ChildApp, Seen)
                 },
            NextApps1 = maps:to_list(maps:merge(OptionalApps2, ChildApps2)) ++ NextApps0,
            %% update graph
            AllChildren = maps:merge(
                            #{OptionalApp => false || OptionalApp <- OptionalApps1},
                            #{ChildApp => true || ChildApp <- ChildApps1}
                           ),
            Children1 = Children0#{App => AllChildren},
            Parents1 = maps:merge(
                         Parents0,
                         #{
                           ChildApp => (maps:get(ChildApp, Parents0, #{}))#{App => []}
                           || ChildApp := _ <- AllChildren
                          }
                        ),
            Parents2 = Parents1#{App => maps:get(App, Parents1, #{})},
            build_dependency_graph(
                NextApps1,
                AllowedApps,
                Seen#{App => Required},
                Parents2,
                Children1);
        {error, {"no such file or directory", _}} when not Required ->
            build_dependency_graph(
                NextApps0,
                AllowedApps,
                Seen,
                Parents0,
                Children0);
        {error, Reason} ->
            {error, {App, Reason}}
    end.

-spec is_allowed(atom(), all | #{atom() => []}) -> boolean().
is_allowed(_App, all) -> true;
is_allowed(App, AllowedApps) -> maps:is_key(App, AllowedApps).

-spec traverse(action(), mode(), graph()) ->
          {ok, [atom()]} | {error, term()}.
traverse(Action = {start, _}, Mode, #{children := Children}) ->
    traverse(Action, Mode, Children, gen_server:reqids_new(), _Started = []);
traverse(stop, Mode, #{parents := Parents}) ->
    traverse(stop, Mode, Parents, gen_server:reqids_new(), _Stopped = []).

-spec traverse(
        action(),
        mode(),
        #{atom() => #{atom() => _}},
        gen_server:request_id_collection(),
        [atom()]
       ) ->
          {ok, [atom()]} | {error, term()}.
traverse(Action, Mode, Graph0, ReqIDs0, Done0) ->
    case maps:size(Graph0) of
        0 ->
            case wait_all_enqueued(ReqIDs0, Done0, false) of
                {ok, Processed} ->
                    {ok, lists:reverse(Processed)};
                {error, {Reason, Processed}} ->
                    application_action_cleanup(Action, Processed, ReqIDs0, Mode),
                    {error, Reason}
            end;
        _ ->
            case [App || App := Children <- Graph0, maps:size(Children) == 0] of
                [] ->
                    case wait_one_enqueued(ReqIDs0, Done0) of
                        {ok, ReadyApp, ReqIDs1, Done1} ->
                            Graph1 = #{App => maps:without([ReadyApp], Children) || App := Children <- Graph0},
                            traverse(Action, Mode, Graph1, ReqIDs1, Done1);
                        {error, Reason, ReqIDs1} ->
                            application_action_cleanup(Action, Done0, ReqIDs1, Mode),
                            {error, Reason}
                    end;
                NextApps ->
                    case application_action_do(Action, Mode, NextApps, [], ReqIDs0) of
                        {ok, {ReqIDs1, Processed}} ->
                            Graph1 =
                                #{
                                  App => maps:without([ProcessedApp || {ProcessedApp, _} <- Processed], Children)
                                  || App := Children <- Graph0
                                 },
                            Graph2 = maps:without(NextApps, Graph1),
                            traverse(
                              Action,
                              Mode,
                              Graph2,
                              ReqIDs1,
                              [ProcessedApp || {ProcessedApp, StartedOrStopped} <- Processed, StartedOrStopped] ++
                                  Done0
                             );
                        {error, {Reason, Processed0}} ->
                            Processed1 = [
                                          ProcessedApp
                                          || {ProcessedApp, StartedOrStopped} <- Processed0, StartedOrStopped
                                         ],
                            application_action_cleanup(Action, Processed1 ++ Done0, ReqIDs0, Mode),
                            {error, Reason}
                    end
            end
    end.

-spec application_action_cleanup(action(), [atom()], gen_server:request_id_collection(), mode()) -> ok.
application_action_cleanup({start, _}, Applications0, ReqIDs, Mode) ->
    _ = case wait_all_enqueued(ReqIDs, Applications0, false) of
        {ok, Applications1} -> ensure_all_stopped(Applications1, Mode);
        {error, {_LastAppReason, Applications1}} -> ensure_all_stopped(Applications1, Mode)
    end,
    ok;
application_action_cleanup(stop, _, ReqIDs, _) ->
    _ = wait_all_enqueued(ReqIDs, [], false),
    ok.

-spec application_action_do(action(), mode(), [atom()], [{atom(), boolean()}], gen_server:request_id_collection()) ->
          {ok, {gen_server:request_id_collection(), [{atom(), boolean()}]}}
              | {error, {term(), [{atom(), boolean()}]}}.
application_action_do(_, _, [], Processed, ReqIDs) ->
    {ok, {ReqIDs, Processed}};
application_action_do({start, RestartType}, serial, [App | Rest], Started, ReqIDs) ->
    case application_controller:start_application(App, RestartType) of
        ok ->
            application_action_do({start, RestartType}, serial, Rest, [{App, true} | Started], ReqIDs);
        {error, {already_started, App}} ->
            application_action_do({start, RestartType}, serial, Rest, [{App, false} | Started], ReqIDs);
        {error, Reason} ->
            {error, {{App, Reason}, Started}}
    end;
application_action_do({start, RestartType}, concurrent, [App | Rest], Started, ReqIDs0) ->
    ReqId = application_controller:start_application_request(App, RestartType),
    ReqIDs1 = gen_server:reqids_add(ReqId, App, ReqIDs0),
    application_action_do({start, RestartType}, concurrent, Rest, Started, ReqIDs1);
application_action_do(stop, serial, [App | Rest], Stopped, ReqIDs) ->
    case application_controller:stop_application(App) of
        ok -> application_action_do(stop, serial, Rest, [{App, true} | Stopped], ReqIDs);
        {error, {not_started, App}} -> application_action_do(stop, serial, Rest, [{App, false} | Stopped], ReqIDs);
        {error, Reason} -> {error, {{App, Reason}, Stopped}}
    end;
application_action_do(stop, concurrent, [App | Rest], Stopped, ReqIDs0) ->
    ReqId = application_controller:stop_application_request(App),
    ReqIDs1 = gen_server:reqids_add(ReqId, App, ReqIDs0),
    application_action_do(stop, concurrent, Rest, Stopped, ReqIDs1).

-spec wait_one_enqueued(gen_server:request_id_collection(), [atom()]) ->
          {ok, atom(), gen_server:request_id_collection(), [atom()]}
              | {error, {atom(), term()}, gen_server:request_id_collection()}.
wait_one_enqueued(ReqIDs0, Processed) ->
    case gen_server:wait_response(ReqIDs0, infinity, true) of
        {{reply, ok}, App, ReqIDs1} when is_atom(App) ->
            {ok, App, ReqIDs1, [App | Processed]};
        {{reply, {error, {already_started, App}}}, App, ReqIDs1} when is_atom(App) ->
            {ok, App, ReqIDs1, Processed};
        {{reply, {error, {not_started, App}}}, App, ReqIDs1} when is_atom(App) ->
            {ok, App, ReqIDs1, Processed};
        {{reply, {error, Reason}}, App, ReqIDs1} when is_atom(App) ->
            {error, {App, Reason}, ReqIDs1};
        {{error, {Reason, _Ref}}, _App, _ReqIDs1} ->
            exit(Reason);
        no_request ->
            exit(deadlock)
    end.

-spec wait_all_enqueued(gen_server:request_id_collection(), [atom()], false | term()) ->
          {ok, [atom()]} | {error, {term(), [atom()]}}.
wait_all_enqueued(ReqIDs0, Processed0, LastAppReason) ->
    case gen_server:reqids_size(ReqIDs0) of
        0 when LastAppReason =:= false ->
            {ok, Processed0};
        0 ->
            {error, {LastAppReason, Processed0}};
        _ ->
            case wait_one_enqueued(ReqIDs0, Processed0) of
                {ok, _App, ReqIDs1, Processed1} ->
                    wait_all_enqueued(ReqIDs1, Processed1, LastAppReason);
                {error, NewAppReason, ReqIDs1} ->
                    wait_all_enqueued(ReqIDs1, Processed0, NewAppReason)
            end
    end.

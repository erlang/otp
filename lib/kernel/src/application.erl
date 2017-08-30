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
-module(application).

-export([ensure_all_started/1, ensure_all_started/2, start/1, start/2,
	 start_boot/1, start_boot/2, stop/1, 
	 load/1, load/2, unload/1, takeover/2,
	 which_applications/0, which_applications/1,
	 loaded_applications/0, permit/2]).
-export([ensure_started/1, ensure_started/2]).
-export([set_env/3, set_env/4, unset_env/2, unset_env/3]).
-export([get_env/1, get_env/2, get_env/3, get_all_env/0, get_all_env/1]).
-export([get_key/1, get_key/2, get_all_key/0, get_all_key/1]).
-export([get_application/0, get_application/1, info/0]).
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


-spec ensure_all_started(Application) -> {'ok', Started} | {'error', Reason} when
      Application :: atom(),
      Started :: [atom()],
      Reason :: term().
ensure_all_started(Application) ->
    ensure_all_started(Application, temporary).

-spec ensure_all_started(Application, Type) -> {'ok', Started} | {'error', Reason} when
      Application :: atom(),
      Type :: restart_type(),
      Started :: [atom()],
      Reason :: term().
ensure_all_started(Application, Type) ->
    case ensure_all_started(Application, Type, []) of
	{ok, Started} ->
	    {ok, lists:reverse(Started)};
	{error, Reason, Started} ->
	    _ = [stop(App) || App <- Started],
	    {error, Reason}
    end.

ensure_all_started(Application, Type, Started) ->
    case start(Application, Type) of
	ok ->
	    {ok, [Application | Started]};
	{error, {already_started, Application}} ->
	    {ok, Started};
	{error, {not_started, Dependency}} ->
	    case ensure_all_started(Dependency, Type, Started) of
		{ok, NewStarted} ->
		    ensure_all_started(Application, Type, NewStarted);
		Error ->
		    Error
	    end;
	{error, Reason} ->
	    {error, {Application, Reason}, Started}
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
    case load(Application) of
	ok ->
	    Name = get_appl_name(Application),
	    application_controller:start_application(Name, RestartType);
	{error, {already_loaded, Name}} ->
	    application_controller:start_application(Name, RestartType);
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

get_env(Application, Key, Def) ->
    case get_env(Application, Key) of
    {ok, Val} ->
        Val;
    undefined ->
        Def
    end.

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

-spec start_type() -> StartType | 'undefined' | 'local' when
      StartType :: start_type().

start_type() ->
    application_controller:start_type(group_leader()).

%% Internal
get_appl_name(Name) when is_atom(Name) -> Name;
get_appl_name({application, Name, _}) when is_atom(Name) -> Name.

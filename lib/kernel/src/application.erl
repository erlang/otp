%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-moduledoc """
Generic OTP application functions

In OTP, _application_ denotes a component implementing some specific
functionality, that can be started and stopped as a unit, and that can be reused
in other systems. This module interacts with _application controller_, a process
started at every Erlang runtime system. This module contains functions for
controlling applications (for example, starting and stopping applications), and
functions to access information about applications (for example, configuration
parameters).

An application is defined by an _application specification_. The specification
is normally located in an _application resource file_ named `Application.app`,
where `Application` is the application name. For details about the application
specification, see [`app(4)`](app.md).

This module can also be viewed as a behaviour for an application implemented
according to the OTP design principles as a supervision tree. The definition of
how to start and stop the tree is to be located in an _application callback
module_, exporting a predefined set of functions.

For details about applications and behaviours, see
[OTP Design Principles](`e:system:design_principles.md`).

## See Also

[OTP Design Principles](`e:system:design_principles.md`),
[kernel(6)](kernel_app.md), [app(4)](app.md)
""".
-moduledoc(#{titles => [{callback,<<"Callback Module">>}]}).
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

-doc "A tuple where the elements are of type `T`.".
-type(tuple_of(_T) :: tuple()).

%%------------------------------------------------------------------

-doc """
This function is called whenever an application is started using `start/1,2`,
and is to start the processes of the application. If the application is
structured according to the OTP design principles as a supervision tree, this
means starting the top supervisor of the tree.

`StartType`{: #start_type } defines the type of start:

- `normal` if it is a normal startup.
- `normal` also if the application is distributed and started at the current
  node because of a failover from another node, and the application
  specification key `start_phases == undefined`.
- `{takeover,Node}` if the application is distributed and started at the current
  node because of a takeover from `Node`, either because
  [`takeover/2`](`takeover/2`) has been called or because the current node has
  higher priority than `Node`.
- `{failover,Node}` if the application is distributed and started at the current
  node because of a failover from `Node`, and the application specification key
  `start_phases /= undefined`.

`StartArgs` is the `StartArgs` argument defined by the application specification
key `mod`.

The function is to return `{ok,Pid}` or `{ok,Pid,State}`, where `Pid` is the pid
of the top supervisor and `State` is any term. If omitted, `State` defaults to
`[]`. If the application is stopped later, `State` is passed to
[`Module:prep_stop/1`](`c:prep_stop/1`).
""".
-doc(#{title => <<"Callback Module">>}).
-callback start(StartType :: start_type(), StartArgs :: term()) ->
    {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.

-doc """
This function is called whenever an application has stopped. It is intended to
be the opposite of [`Module:start/2`](`c:start/2`) and is to do any necessary
cleaning up. The return value is ignored.

`State` is the return value of [`Module:prep_stop/1`](`c:prep_stop/1`), if such
a function exists. Otherwise `State` is taken from the return value of
[`Module:start/2`](`c:start/2`).
""".
-doc(#{title => <<"Callback Module">>}).
-callback stop(State :: term()) ->
    term().

-doc """
This function is called by an application after a code replacement, if the
configuration parameters have changed.

`Changed` is a list of parameter-value tuples including all configuration
parameters with changed values.

`New` is a list of parameter-value tuples including all added configuration
parameters.

`Removed` is a list of all removed parameters.
""".
-doc(#{title => <<"Callback Module">>}).
-callback config_change(Changed, New, Removed) -> ok when
      Changed :: [{Par, Val}],
      New :: [{Par, Val}],
      Removed :: [Par],
      Par :: atom(),
      Val :: term().

-doc """
This function is called when an application is about to be stopped, before
shutting down the processes of the application.

`State` is the state returned from [`Module:start/2`](`c:start/2`), or `[]` if
no state was returned. `NewState` is any term and is passed to
[`Module:stop/1`](`c:stop/1`).

The function is optional. If it is not defined, the processes are terminated and
then [`Module:stop(State)`](`c:stop/1`) is called.
""".
-doc(#{title => <<"Callback Module">>}).
-callback prep_stop(State) -> NewState when
      State :: term(), NewState :: term().

-doc """
Starts an application with included applications, when synchronization is needed
between processes in the different applications during startup.

The start phases are defined by the application specification key
`start_phases == [{Phase,PhaseArgs}]`. For included applications, the set of
phases must be a subset of the set of phases defined for the including
application.

The function is called for each start phase (as defined for the primary
application) for the primary application and all included applications, for
which the start phase is defined.

For a description of `StartType`, see [`Module:start/2`](`c:start/2`).
""".
-doc(#{title => <<"Callback Module">>}).
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

-doc(#{equiv => load/2}).
-spec load(AppDescr) -> 'ok' | {'error', Reason} when
      AppDescr :: Application | (AppSpec :: application_spec()),
      Application :: atom(),
      Reason :: term().

load(Application) ->
    load1(Application, []).

-doc """
Loads the application specification for an application into the application
controller. It also loads the application specifications for any included
applications. Notice that the function does not load the Erlang object code.

The application can be specified by its name `Application`. In this case, the
application controller searches the code path for the application resource file
`Application.app` and loads the specification it contains.

The application specification can also be specified directly as a tuple
`AppSpec`, having the format and contents as described in [`app(4)`](app.md).

If `Distributed == {Application,[Time,]Nodes}`, the application becomes
distributed. The argument overrides the value for the application in the Kernel
configuration parameter `distributed`. `Application` must be the application
name (same as in the first argument). If a node crashes and `Time` is specified,
the application controller waits for `Time` milliseconds before attempting to
restart the application on another node. If `Time` is not specified, it defaults
to `0` and the application is restarted immediately.

`Nodes` is a list of node names where the application can run, in priority from
left to right. Node names can be grouped using tuples to indicate that they have
the same priority.

_Example:_

```erlang
Nodes = [cp1@cave, {cp2@cave, cp3@cave}]
```

This means that the application is preferably to be started at `cp1@cave`. If
`cp1@cave` is down, the application is to be started at `cp2@cave` or
`cp3@cave`.

If `Distributed == default`, the value for the application in the Kernel
configuration parameter `distributed` is used.
""".
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

-doc """
Unloads the application specification for `Application` from the application
controller. It also unloads the application specifications for any included
applications. Notice that the function does not purge the Erlang object code.
""".
-spec unload(Application) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Reason :: term().

unload(Application) ->
    application_controller:unload_application(Application).


-doc(#{equiv => ensure_all_started/3}).
-doc(#{since => <<"OTP 26.0,OTP R16B02">>}).
-spec ensure_all_started(Applications) -> {'ok', Started} | {'error', Reason} when
      Applications :: atom() | [atom()],
      Started :: [atom()],
      Reason :: term().
ensure_all_started(Application) ->
    ensure_all_started(Application, temporary, serial).

-doc(#{equiv => ensure_all_started/3}).
-doc(#{since => <<"OTP 26.0,OTP R16B02">>}).
-spec ensure_all_started(Applications, Type) -> {'ok', Started} | {'error', AppReason} when
      Applications :: atom() | [atom()],
      Type :: restart_type(),
      Started :: [atom()],
      AppReason :: {atom(), term()}.
ensure_all_started(Application, Type) ->
    ensure_all_started(Application, Type, serial).

-doc """
`Applications` is either an an `t:atom/0` or a list of `t:atom/0` representing
multiple applications.

This function is equivalent to calling [`start/1,2`](`start/1`) repeatedly on
all dependencies that are not yet started of each application. Optional
dependencies will also be loaded and started if they are available.

The `Mode` argument controls if the applications should be started in `serial`
mode (one at a time) or `concurrent` mode. In concurrent mode, a dependency
graph is built and the leaves of the graph are started concurrently and
recursively. In both modes, no assertion can be made about the order the
applications are started. If not supplied, it defaults to `serial`.

Returns `{ok, AppNames}` for a successful start or for an already started
application (which is, however, omitted from the `AppNames` list).

The function reports `{error, {AppName,Reason}}` for errors, where `Reason` is
any possible reason returned by [`start/1,2`](`start/1`) when starting a
specific dependency.

If an error occurs, the applications started by the function are stopped to
bring the set of running applications back to its initial state.
""".
-doc(#{since => <<"OTP 26.0,OTP R16B02">>}).
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

-doc(#{equiv => start/2}).
-spec start(Application) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Reason :: term().

start(Application) ->
    start(Application, temporary).

-doc """
Starts `Application`. If it is not loaded, the application controller first
loads it using [`load/1`](`load/1`). It ensures that any included applications
are loaded, but does not start them. That is assumed to be taken care of in the
code for `Application`.

The application controller checks the value of the application specification key
`applications`, to ensure that all applications needed to be started before this
application are running. If an application is missing and the application is not
marked as optional, `{error,{not_started,App}}` is returned, where `App` is the
name of the missing application. Note this function makes no attempt to start
any of the applications listed in `applications`, not even optional ones. See
[`ensure_all_started/1,2`](`ensure_all_started/1`) for recursively starting the
current application and its dependencies.

Once validated, the application controller then creates an _application master_
for the application. The application master becomes the group leader of all the
processes in the application. I/O is forwarded to the previous group leader,
though, this is just a way to identify processes that belong to the application.
Used for example to find itself from any process, or, reciprocally, to kill them
all when it terminates.

The application master starts the application by calling the application
callback function [`Module:start/2`](`c:start/2`) as defined by the application
specification key `mod`.

Argument `Type` specifies the type of the application. If omitted, it defaults
to `temporary`.

- If a permanent application terminates, all other applications and the entire
  Erlang node are also terminated.
- If a transient application terminates:
  - with `Reason == normal`, this is reported but no other applications are
    terminated.
  - abnormally, all other applications and the entire Erlang node are also
    terminated.
- If a temporary application terminates, this is reported but no other
  applications are terminated.

Notice that an application can always be stopped explicitly by calling
[`stop/1`](`stop/1`). Regardless of the type of the application, no other
applications are affected.

Notice also that the transient type is of little practical use, because when a
supervision tree terminates, the reason is set to `shutdown`, not `normal`.
""".
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

-doc(#{equiv => ensure_started/2}).
-doc(#{since => <<"OTP R16B01">>}).
-spec ensure_started(Application) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Reason :: term().

ensure_started(Application) ->
    ensure_started(Application, temporary).

-doc """
Equivalent to [`start/1,2`](`start/1`) except it returns `ok` for already
started applications.
""".
-doc(#{since => <<"OTP R16B01">>}).
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

-doc false.
-spec start_boot(Application :: atom()) -> 'ok' | {'error', term()}.

start_boot(Application) ->
    start_boot(Application, temporary).

-doc false.
-spec start_boot(Application :: atom(), RestartType :: restart_type()) ->
	     'ok' | {'error', term()}.

start_boot(Application, RestartType) ->
    application_controller:start_boot_application(Application, RestartType).

-doc """
Takes over the distributed application `Application`, which executes at another
node `Node`. At the current node, the application is restarted by calling
[`Module:start({takeover,Node},StartArgs)`](`c:start/2`). `Module` and
`StartArgs` are retrieved from the loaded application specification. The
application at the other node is not stopped until the startup is completed,
that is, when [`Module:start/2`](`c:start/2`) and any calls to
[`Module:start_phase/3`](`c:start_phase/3`) have returned.

Thus, two instances of the application run simultaneously during the takeover,
so that data can be transferred from the old to the new instance. If this is not
an acceptable behavior, parts of the old instance can be shut down when the new
instance is started. However, the application cannot be stopped entirely, at
least the top supervisor must remain alive.

For a description of `Type`, see [`start/1,2`](`start/1`).
""".
-spec takeover(Application, Type) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Type :: restart_type(),
      Reason :: term().

takeover(Application, RestartType) ->
    dist_ac:takeover_application(Application, RestartType).

-doc """
Changes the permission for `Application` to run at the current node. The
application must be loaded using `load/1,2` for the function to have effect.

If the permission of a loaded, but not started, application is set to `false`,
`start` returns `ok` but the application is not started until the permission is
set to `true`.

If the permission of a running application is set to `false`, the application is
stopped. If the permission later is set to `true`, it is restarted.

If the application is distributed, setting the permission to `false` means that
the application will be started at, or moved to, another node according to how
its distribution is configured (see `load/2`).

The function does not return until the application is started, stopped, or
successfully moved to another node. However, in some cases where permission is
set to `true`, the function returns `ok` even though the application is not
started. This is true when an application cannot start because of dependencies
to other applications that are not yet started. When they are started,
`Application` is started as well.

By default, all applications are loaded with permission `true` on all nodes. The
permission can be configured using the Kernel configuration parameter
`permissions`.
""".
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

-doc """
Stops `Application`. The application master calls
[`Module:prep_stop/1`](`c:prep_stop/1`), if such a function is defined, and then
tells the top supervisor of the application to shut down (see `m:supervisor`).
This means that the entire supervision tree, including included applications, is
terminated in reversed start order. After the shutdown, the application master
calls [`Module:stop/1`](`c:stop/1`). `Module` is the callback module as defined
by the application specification key `mod`.

Last, the application master terminates. Notice that all processes with the
application master as group leader, that is, processes spawned from a process
belonging to the application, are also terminated.

When stopped, the application is still loaded.

To stop a distributed application, [`stop/1`](`stop/1`) must be called on all
nodes where it can execute (that is, on all nodes where it has been started).
The call to [`stop/1`](`stop/1`) on the node where the application currently
executes stops its execution. The application is not moved between nodes, as
[`stop/1`](`stop/1`) is called on the node where the application currently
executes before [`stop/1`](`stop/1`) is called on the other nodes.
""".
-spec stop(Application) -> 'ok' | {'error', Reason} when
      Application :: atom(),
      Reason :: term().

stop(Application) ->
    application_controller:stop_application(Application).

-doc(#{equiv => which_applications/1}).
-spec which_applications() -> [{Application, Description, Vsn}] when
      Application :: atom(),
      Description :: string(),
      Vsn :: string().

which_applications() ->
    application_controller:which_applications().

-doc """
Returns a list with information about the applications that are currently
running. `Application` is the application name. `Description` and `Vsn` are the
values of their `description` and `vsn` application specification keys,
respectively.

`which_applications/0` uses the standard `gen_server` time-out value (5000 ms).
A `Timeout` argument can be specified if another time-out value is useful, for
example, in situations where the application controller is heavily loaded.
""".
-spec which_applications(Timeout) -> [{Application, Description, Vsn}] when
      Timeout :: timeout(),
      Application :: atom(),
      Description :: string(),
      Vsn :: string().

which_applications(infinity) ->
    application_controller:which_applications(infinity);
which_applications(Timeout) when is_integer(Timeout), Timeout>=0 ->
    application_controller:which_applications(Timeout).

-doc """
Returns a list with information about the applications, and included
applications, which are loaded using `load/1,2`. `Application` is the
application name. `Description` and `Vsn` are the values of their `description`
and `vsn` application specification keys, respectively.
""".
-spec loaded_applications() -> [{Application, Description, Vsn}] when
      Application :: atom(),
      Description :: string(),
      Vsn :: string().

loaded_applications() -> 
    application_controller:loaded_applications().

-doc false.
-spec info() -> term().

info() -> 
    application_controller:info().

-doc(#{equiv => set_env/2}).
-doc(#{since => <<"OTP 21.3">>}).
-spec set_env(Config) -> 'ok' when
      Config :: [{Application, Env}],
      Application :: atom(),
      Env :: [{Par :: atom(), Val :: term()}].

set_env(Config) when is_list(Config) ->
    set_env(Config, []).

-doc """
Sets the configuration `Config` for multiple applications. It is equivalent to
calling [`set_env/4`](`set_env/4`) on each application individually, except it
is more efficient. The given `Config` is validated before the configuration is
set.

[`set_env/2`](`set_env/2`) uses the standard `gen_server` time-out value (5000
ms). Option `timeout` can be specified if another time-out value is useful, for
example, in situations where the application controller is heavily loaded.

Option `persistent` can be set to `true` to guarantee that parameters set with
[`set_env/2`](`set_env/2`) are not overridden by those defined in the
application resource file on load. This means that persistent values will stick
after the application is loaded and also on application reload.

If an application is given more than once or if an application has the same key
given more than once, the behaviour is undefined and a warning message will be
logged. In future releases, an error will be raised.

[`set_env/1`](`set_env/1`) is equivalent to
[`set_env(Config, [])`](`set_env/2`).

> #### Warning {: .warning }
>
> Use this function only if you know what you are doing, that is, on your own
> applications. It is very application-dependent and configuration
> parameter-dependent when and how often the value is read by the application.
> Careless use of this function can put the application in a weird,
> inconsistent, and malfunctioning state.
""".
-doc(#{since => <<"OTP 21.3">>}).
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

-doc(#{equiv => set_env/4}).
-spec set_env(Application, Par, Val) -> 'ok' when
      Application :: atom(),
      Par :: atom(),
      Val :: term().

set_env(Application, Key, Val) -> 
    application_controller:set_env(Application, Key, Val).

-doc """
Sets the value of configuration parameter `Par` for `Application`.

[`set_env/4`](`set_env/4`) uses the standard `gen_server` time-out value (5000
ms). Option `timeout` can be specified if another time-out value is useful, for
example, in situations where the application controller is heavily loaded.

If [`set_env/4`](`set_env/4`) is called before the application is loaded, the
application environment values specified in file `Application.app` override the
ones previously set. This is also true for application reloads.

Option `persistent` can be set to `true` to guarantee that parameters set with
[`set_env/4`](`set_env/4`) are not overridden by those defined in the
application resource file on load. This means that persistent values will stick
after the application is loaded and also on application reload.

> #### Warning {: .warning }
>
> Use this function only if you know what you are doing, that is, on your own
> applications. It is very application-dependent and configuration
> parameter-dependent when and how often the value is read by the application.
> Careless use of this function can put the application in a weird,
> inconsistent, and malfunctioning state.
""".
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

-doc(#{equiv => unset_env/3}).
-spec unset_env(Application, Par) -> 'ok' when
      Application :: atom(),
      Par :: atom().

unset_env(Application, Key) -> 
    application_controller:unset_env(Application, Key).

-doc """
Removes the configuration parameter `Par` and its value for `Application`.

[`unset_env/2`](`unset_env/2`) uses the standard `gen_server` time-out value
(5000 ms). Option `timeout` can be specified if another time-out value is
useful, for example, in situations where the application controller is heavily
loaded.

[`unset_env/3`](`unset_env/3`) also allows the persistent option to be passed
(see `set_env/4`).

> #### Warning {: .warning }
>
> Use this function only if you know what you are doing, that is, on your own
> applications. It is very application-dependent and configuration
> parameter-dependent when and how often the value is read by the application.
> Careless use of this function can put the application in a weird,
> inconsistent, and malfunctioning state.
""".
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

-doc(#{equiv => get_env/2}).
-spec get_env(Par) -> 'undefined' | {'ok', Val} when
      Par :: atom(),
      Val :: term().

get_env(Key) -> 
    application_controller:get_pid_env(group_leader(), Key).

-doc """
Returns the value of configuration parameter `Par` for `Application`. If the
application argument is omitted, it defaults to the application of the calling
process.

Returns `undefined` if any of the following applies:

- The specified application is not loaded.
- The configuration parameter does not exist.
- The process executing the call does not belong to any application.
""".
-spec get_env(Application, Par) -> 'undefined' | {'ok', Val} when
      Application :: atom(),
      Par :: atom(),
      Val :: term().

get_env(Application, Key) -> 
    application_controller:get_env(Application, Key).

-doc """
Works like `get_env/2` but returns value `Def` when configuration parameter
`Par` does not exist.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec get_env(Application, Par, Def) -> Val when
      Application :: atom(),
      Par :: atom(),
      Def :: term(),
      Val :: term().

get_env(Application, Key, Default) ->
    application_controller:get_env(Application, Key, Default).

-doc(#{equiv => get_all_env/1}).
-spec get_all_env() -> Env when
      Env :: [{Par :: atom(), Val :: term()}].

get_all_env() -> 
    application_controller:get_pid_all_env(group_leader()).

-doc """
Returns the configuration parameters and their values for `Application`. If the
argument is omitted, it defaults to the application of the calling process.

If the specified application is not loaded, or if the process executing the call
does not belong to any application, the function returns `[]`.
""".
-spec get_all_env(Application) -> Env when
      Application :: atom(),
      Env :: [{Par :: atom(), Val :: term()}].

get_all_env(Application) -> 
    application_controller:get_all_env(Application).

-doc(#{equiv => get_key/2}).
-spec get_key(Key) -> 'undefined' | {'ok', Val} when
      Key :: atom(),
      Val :: term().

get_key(Key) -> 
    application_controller:get_pid_key(group_leader(), Key).

-doc """
Returns the value of the application specification key `Key` for `Application`.
If the application argument is omitted, it defaults to the application of the
calling process.

Returns `undefined` if any of the following applies:

- The specified application is not loaded.
- The specification key does not exist.
- The process executing the call does not belong to any application.
""".
-spec get_key(Application, Key) -> 'undefined' | {'ok', Val} when
      Application :: atom(),
      Key :: atom(),
      Val :: term().

get_key(Application, Key) -> 
    application_controller:get_key(Application, Key).

-doc(#{equiv => get_all_key/1}).
-spec get_all_key() -> [] | {'ok', Keys} when
      Keys :: [{Key :: atom(),Val :: term()},...].

get_all_key() ->
    application_controller:get_pid_all_key(group_leader()).

-doc """
Returns the application specification keys and their values for `Application`.
If the argument is omitted, it defaults to the application of the calling
process.

If the specified application is not loaded, the function returns `undefined`. If
the process executing the call does not belong to any application, the function
returns `[]`.
""".
-spec get_all_key(Application) -> 'undefined' | Keys when
      Application :: atom(),
      Keys :: {'ok', [{Key :: atom(),Val :: term()},...]}.

get_all_key(Application) -> 
    application_controller:get_all_key(Application).

-doc(#{equiv => get_application/1}).
-spec get_application() -> 'undefined' | {'ok', Application} when
      Application :: atom().

get_application() -> 
    application_controller:get_application(group_leader()).

-doc """
Returns the name of the application to which the process `Pid` or the module
`Module` belongs. Providing no argument is the same as calling
[`get_application(self())`](`get_application/1`).

If the specified process does not belong to any application, or if the specified
process or module does not exist, the function returns `undefined`.
""".
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

-doc """
Returns the `Pid` of the supervisor running at the root of `Application`.

If the specified application does not exist or does not define a callback
module, the function returns `undefined`.
""".
-doc(#{since => <<"OTP 26.0">>}).
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

-doc """
This function is intended to be called by a process belonging to an application,
when the application is started, to determine the start type, which is
`StartType` or `local`.

For a description of `StartType`, see
[`Module:start/2`](`m:application#start_type`).

`local` is returned if only parts of the application are restarted (by a
supervisor), or if the function is called outside a startup.

If the process executing the call does not belong to any application, the
function returns `undefined`.
""".
-spec start_type() -> StartType | 'undefined' | 'local' when
      StartType :: start_type().

start_type() ->
    application_controller:start_type(group_leader()).

%% Internal
get_appl_name(Name) when is_atom(Name) -> Name;
get_appl_name({application, Name, _}) when is_atom(Name) -> Name.

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
-module(supervisor).
-moduledoc """
Generic supervisor behavior.

This behavior module provides a supervisor, a process that supervises other
processes called child processes. A child process can either be another
supervisor or a worker process. Worker processes are normally implemented using
one of the `m:gen_event`, `m:gen_server`, or `m:gen_statem` behaviors. A
supervisor implemented using this module has a standard set of interface
functions and includes functionality for tracing and error reporting.
Supervisors are used to build a hierarchical process structure called a
supervision tree, a nice way to structure a fault-tolerant application. For more
information, see [Supervisor Behaviour](`e:system:sup_princ.md`) in OTP Design
Principles.

A supervisor expects the definition of which child processes to supervise to be
specified in a callback module exporting a predefined set of functions.

Unless otherwise stated, all functions in this module fail if the specified
supervisor does not exist or if bad arguments are specified.

[](){: #supervision_princ }

## Supervision Principles

The supervisor is responsible for starting, stopping, and monitoring its child
processes. The basic idea of a supervisor is that it must keep its child
processes alive by restarting them when necessary.

The children of a supervisor are defined as a list of _child specifications_.
When the supervisor is started, the child processes are started in order from
left to right according to this list. When the supervisor is going to terminate,
it first terminates its child processes in reversed start order, from right to
left.

[](){: #sup_flags }

### Supervisor flags

The supervisor properties are defined by the supervisor flags. The type
definition for the supervisor flags is as follows:

```erlang
sup_flags() = #{strategy => strategy(),           % optional
                intensity => non_neg_integer(),   % optional
                period => pos_integer(),          % optional
                auto_shutdown => auto_shutdown()} % optional
```

#### Restart Strategies

A supervisor can have one of the following _restart strategies_ specified with
the `strategy` key in the above map:

- `one_for_one` \- If one child process terminates and is to be restarted, only
  that child process is affected. This is the default restart strategy.
- `one_for_all` \- If one child process terminates and is to be restarted, all
  other child processes are terminated and then all child processes are
  restarted.
- `rest_for_one` \- If one child process terminates and is to be restarted, the
  'rest' of the child processes (that is, the child processes after the
  terminated child process in the start order) are terminated. Then the
  terminated child process and all child processes after it are restarted.
- `simple_one_for_one` \- A simplified `one_for_one` supervisor, where all child
  processes are dynamically added instances of the same process type, that is,
  running the same code.

  Functions `delete_child/2` and `restart_child/2` are invalid for
  `simple_one_for_one` supervisors and return `{error,simple_one_for_one}` if
  the specified supervisor uses this restart strategy.

  Function `terminate_child/2` can be used for children under
  `simple_one_for_one` supervisors by specifying the child's `t:pid/0` as the
  second argument. If instead the child specification identifier is used,
  [`terminate_child/2`](`terminate_child/2`) return
  `{error,simple_one_for_one}`.

  As a `simple_one_for_one` supervisor can have many children, it shuts them all
  down asynchronously. This means that the children do their cleanup in
  parallel, and therefore the order in which they are stopped is not defined.

#### Restart intensity and period

To prevent a supervisor from getting into an infinite loop of child process
terminations and restarts, a _maximum restart intensity_ is defined using two
integer values specified with keys `intensity` and `period` in the above map.
Assuming the values `MaxR` for `intensity` and `MaxT` for `period`, then, if
more than `MaxR` restarts occur within `MaxT` seconds, the supervisor terminates
all child processes and then itself. The termination reason for the supervisor
itself in that case will be `shutdown`. `intensity` defaults to `1` and `period`
defaults to `5`.

[](){: #auto_shutdown }

#### Automatic Shutdown

A supervisor can be configured to automatically shut itself down with exit
reason `shutdown` when [significant children](`m:supervisor#significant_child`)
terminate with the `auto_shutdown` key in the above map:

- `never` \- Automic shutdown is disabled. This is the default setting.

  With `auto_shutdown` set to `never`, child specs with the `significant` flag
  set to `true` are considered invalid and will be rejected.

- `any_significant` \- The supervisor will shut itself down when _any_
  significant child terminates, that is, when a `transient` significant child
  terminates normally or when a `temporary` significant child terminates
  normally or abnormally.
- `all_significant` \- The supervisor will shut itself down when _all_
  significant children have terminated, that is, when the _last active_
  significant child terminates. The same rules as for `any_significant` apply.

For more information, see the section
[Automatic Shutdown](`e:system:sup_princ.md#automatic-shutdown`) in Supervisor
Behavior in OTP Design Principles.

> #### Warning {: .warning }
>
> The automatic shutdown feature appeared in OTP 24.0, but applications using
> this feature will also compile and run with older OTP versions.
>
> However, such applications, when compiled with an OTP version that predates
> the appearance of the automatic shutdown feature, will leak processes because
> the automatic shutdowns they rely on will not happen.
>
> It is up to implementors to take proper precautions if they expect that their
> applications may be compiled with older OTP versions.

[](){: #child_spec }

### Child specification

The type definition of a child specification is as follows:

```erlang
child_spec() = #{id => child_id(),             % mandatory
                 start => mfargs(),            % mandatory
                 restart => restart(),         % optional
                 significant => significant(), % optional
                 shutdown => shutdown(),       % optional
                 type => worker(),             % optional
                 modules => modules()}         % optional
```

The old tuple format is kept for backwards compatibility, see `t:child_spec/0`,
but the map is preferred.

- `id` is used to identify the child specification internally by the supervisor.

  The `id` key is mandatory.

  Notice that this identifier on occations has been called "name". As far as
  possible, the terms "identifier" or "id" are now used but to keep backward
  compatibility, some occurences of "name" can still be found, for example in
  error messages.

- `start` defines the function call used to start the child process. It must be
  a module-function-arguments tuple `{M,F,A}` used as
  [`apply(M,F,A)`](`apply/3`).

  The start function _must create and link to_ the child process, and must
  return `{ok,Child}` or `{ok,Child,Info}`, where `Child` is the pid of the
  child process and `Info` any term that is ignored by the supervisor.

  The start function can also return `ignore` if the child process for some
  reason cannot be started, in which case the child specification is kept by the
  supervisor (unless it is a temporary child) but the non-existing child process
  is ignored.

  If something goes wrong, the function can also return an error tuple
  `{error,Error}`.

  Notice that the `start_link` functions of the different behavior modules
  fulfill the above requirements.

  The `start` key is mandatory.

- [](){: #restart } `restart` defines when a terminated child process must be
  restarted. A `permanent` child process is always restarted. A `temporary`
  child process is never restarted (even when the supervisor's restart strategy
  is `rest_for_one` or `one_for_all` and a sibling's death causes the temporary
  process to be terminated). A `transient` child process is restarted only if it
  terminates abnormally, that is, with another exit reason than `normal`,
  `shutdown`, or `{shutdown,Term}`.

  The `restart` key is optional. If it is not specified, it defaults to
  `permanent`.

- [](){: #significant_child } `significant` defines if a child is considered
  significant for [automatic self-shutdown](`m:supervisor#auto_shutdown`) of the
  supervisor.

  Setting this option to `true` when the [restart type](`m:supervisor#restart`)
  is `permanent` is invalid. Also, it is considered invalid to start children
  with this option set to `true` in a supervisor when the
  [`auto_shutdown`](`m:supervisor#auto_shutdown`) supervisor flag is set to
  `never`.

  The `significant` key is optional. If it is not specified, it defaults to
  `false`.

- `shutdown` defines how a child process must be terminated. `brutal_kill` means
  that the child process is unconditionally terminated using
  [`exit(Child,kill)`](`exit/2`). An integer time-out value means that the
  supervisor tells the child process to terminate by calling
  [`exit(Child,shutdown)`](`exit/2`) and then wait for an exit signal with
  reason `shutdown` back from the child process. If no exit signal is received
  within the specified number of milliseconds, the child process is
  unconditionally terminated using [`exit(Child,kill)`](`exit/2`).

  If the child process is another supervisor, the shutdown time must be set to
  `infinity` to give the subtree ample time to shut down.

  > #### Warning {: .warning }
  >
  > Setting the shutdown time to anything other than `infinity` for a child of
  > type `supervisor` can cause a race condition where the child in question
  > unlinks its own children, but fails to terminate them before it is killed.

  It is also allowed to set it to `infinity`, if the child process is a worker.

  > #### Warning {: .warning }
  >
  > Be careful when setting the shutdown time to `infinity` when the child
  > process is a worker. Because, in this situation, the termination of the
  > supervision tree depends on the child process, it must be implemented in a
  > safe way and its cleanup procedure must always return.

  Notice that all child processes implemented using the standard OTP behavior
  modules automatically adhere to the shutdown protocol.

  The `shutdown` key is optional. If it is not specified, it defaults to `5000`
  if the child is of type `worker` and it defaults to `infinity` if the child is
  of type `supervisor`.

- `type` specifies if the child process is a supervisor or a worker.

  The `type` key is optional. If it is not specified, it defaults to `worker`.

- `modules` is used by the release handler during code replacement to determine
  which processes are using a certain module. As a rule of thumb, if the child
  process is a `m:supervisor`, `m:gen_server` or, `m:gen_statem`, this is to be a list
  with one element `[Module]`, where `Module` is the callback module. If the
  child process is an event manager (`m:gen_event`) with a dynamic set of callback
  modules, value `dynamic` must be used. For more information about release
  handling, see [Release Handling](`e:system:release_handling.md`) in OTP Design
  Principles.

  The `modules` key is optional. If it is not specified, it defaults to `[M]`,
  where `M` comes from the child's start `{M,F,A}`.

- Internally, the supervisor also keeps track of the pid `Child` of the child
  process, or `undefined` if no pid exists.

## See Also

`m:gen_event`, `m:gen_statem`, `m:gen_server`, `m:sys`
""".

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
	 terminate/2, code_change/3]).

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
              startlink_ret/0, startlink_err/0,
              sup_name/0, sup_ref/0]).

%%--------------------------------------------------------------------------

-type auto_shutdown() :: 'never' | 'any_significant' | 'all_significant'.
-type child()         :: 'undefined' | pid().
-doc "Not a `t:pid/0`.".
-type child_id()      :: term().
-doc """
Value `undefined` for `A` (the argument list) is only to be used internally in
`m:supervisor`. If the restart type of the child is `temporary`, the process is
never to be restarted and therefore there is no need to store the real argument
list. Value `undefined` is then stored instead.
""".
-type mfargs()        :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
-type modules()       :: [module()] | 'dynamic'.
-type restart()       :: 'permanent' | 'transient' | 'temporary'.
-type significant()   :: boolean().
-type shutdown()      :: 'brutal_kill' | timeout().
-type worker()        :: 'worker' | 'supervisor'.
-doc """
Name specification to use when starting a `supervisor`. See function
[`start_link/2,3`](`start_link/2`) and the type `t:sup_ref/0` below.

- **`{local,LocalName}`** - Register the `supervisor` locally as `LocalName`
  using [`register/2`](`erlang:register/2`).

- **`{global,GlobalName}`** - Register the `supervisor` process id globally as
  `GlobalName` using `global:register_name/2`.

- **`{via,RegMod,ViaName}`** - Register the `supervisor` process with the
  registry represented by `RegMod`. The `RegMod` callback is to export the
  functions `register_name/2`, `unregister_name/1`, `whereis_name/1`, and
  `send/2`, which are to behave like the corresponding functions in `m:global`.
  Thus, `{via,global,GlobalName}` is a valid reference equivalent to
  `{global,GlobalName}`.
""".
-type sup_name()      :: {'local', Name :: atom()}
                       | {'global', Name :: term()}
                       | {'via', Module :: module(), Name :: any()}.
-doc """
Supervisor specification to use when addressing a `supervisor`. See
[`count_children/1`](`count_children/1`), [`delete_child/2`](`delete_child/2`),
[`get_childspec/2`](`get_childspec/2`), [`restart_child/2`](`restart_child/2`),
[`start_child/2`](`start_child/2`), [`terminate_child/2`](`terminate_child/2`),
[`which_children/1`](`which_children/1`) and the type `t:sup_name/0` above.

It can be:

- **`t:pid/0`** - The `supervisor`'s process identifier.

- **`LocalName`** - The `supervisor` is locally registered as `LocalName` with
  [`register/2`](`erlang:register/2`).

- **`{Name,Node}`** - The `supervisor` is locally registered on another node.

- **`{global,GlobalName}`** - The `supervisor` is globally registered in
  `m:global`.

- **`{via,RegMod,ViaName}`** - The `supervisor` is registered in an alternative
  process registry. The registry callback module `RegMod` is to export functions
  `register_name/2`, `unregister_name/1`, `whereis_name/1`, and `send/2`, which
  are to behave like the corresponding functions in `m:global`. Thus,
  `{via,global,GlobalName}` is the same as `{global,GlobalName}`.
""".
-type sup_ref()       :: (Name :: atom())
                       | {Name :: atom(), Node :: node()}
                       | {'global', Name :: term()}
                       | {'via', Module :: module(), Name :: any()}
                       | pid().
-doc """
The tuple format is kept for backward compatibility only. A map is preferred;
see more details [above](`m:supervisor#child_spec`).
""".
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

-doc """
The tuple format is kept for backward compatibility only. A map is preferred;
see more details [above](`m:supervisor#sup_flags`).
""".
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

-doc """
Whenever a supervisor is started using [`start_link/2,3`](`start_link/2`), this
function is called by the new process to find out about restart strategy,
maximum restart intensity, and child specifications.

`Args` is the `Args` argument provided to the start function.

`SupFlags` is the supervisor flags defining the restart strategy and maximum
restart intensity for the supervisor. `[ChildSpec]` is a list of valid child
specifications defining which child processes the supervisor must start and
monitor. See the discussion in section
[`Supervision Principles`](`m:supervisor#supervision_princ`) earlier.

Notice that when the restart strategy is `simple_one_for_one`, the list of child
specifications must be a list with one child specification only. (The child
specification identifier is ignored.) No child process is then started during
the initialization phase, but all children are assumed to be started dynamically
using `start_child/2`.

The function can also return `ignore`.

Notice that this function can also be called as a part of a code upgrade
procedure. Therefore, the function is not to have any side effects. For more
information about code upgrade of supervisors, see section
[Changing a Supervisor](`e:system:appup_cookbook.md#sup`) in OTP Design
Principles.
""".
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

-doc """
Creates a nameless supervisor process as part of a supervision tree.

Equivalent to `start_link/3` except that the supervisor process is not
[`registered`](`erlang:register/2`).
""".
-spec start_link(Module, Args) -> startlink_ret() when
      Module :: module(),
      Args :: term().
start_link(Mod, Args) ->
    gen_server:start_link(supervisor, {self, Mod, Args}, []).
 
-doc """
Creates a supervisor process as part of a supervision tree.

For example, the function ensures that the supervisor is linked to the calling
process (its supervisor).

The created supervisor process calls [`Module:init/1`](`c:init/1`) to find out
about restart strategy, maximum restart intensity, and child processes. To
ensure a synchronized startup procedure, `start_link/2,3` does not return until
[`Module:init/1`](`c:init/1`) has returned and all child processes have been
started.

- If `SupName={local,Name}`, the supervisor is registered locally as `Name`
  using [`register/2`](`register/2`).
- If `SupName={global,Name}`, the supervisor is registered globally as `Name`
  using `global:register_name/2`.
- If `SupName={via,Module,Name}`, the supervisor is registered as `Name` using
  the registry represented by `Module`. The `Module` callback must export the
  functions `register_name/2`, `unregister_name/1`, and `send/2`, which must
  behave like the corresponding functions in `m:global`. Thus,
  `{via,global,Name}` is a valid reference.

`Module` is the name of the callback module.

`Args` is any term that is passed as the argument to
[`Module:init/1`](`c:init/1`).

- If the supervisor and its child processes are successfully created (that is,
  if all child process start functions return `{ok,Child}`, `{ok,Child,Info}`,
  or `ignore`), the function returns `{ok,Pid}`, where `Pid` is the pid of the
  supervisor.
- If there already exists a process with the specified `SupName`, the function
  returns `{error,{already_started,Pid}}`, where `Pid` is the pid of that
  process.
- If [`Module:init/1`](`c:init/1`) returns `ignore`, this function returns
  `ignore` as well, and the supervisor terminates with reason `normal`.
- If [`Module:init/1`](`c:init/1`) fails or returns an incorrect value, this
  function returns `{error,Term}`, where `Term` is a term with information about
  the error, and the supervisor terminates with reason `Term`.
- If any child process start function fails or returns an error tuple or an
  erroneous value, the supervisor first terminates all already started child
  processes with reason `shutdown` and then terminate itself and returns
  `{error, {shutdown, Reason}}`.
""".
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

-doc """
Dynamically adds a child specification to supervisor `SupRef`, which starts the
corresponding child process.

For `one_for_one`, `one_for_all` and `rest_for_one` supervisors, the second
argument must be a valid child specification `ChildSpec`. The child process
is started by using the start function as defined in the child specification.

For `simple_one_for_one` supervisors, the child specification defined in
[`Module:init/1`](`c:init/1`) is used, and the second argument must instead
be an arbitrary list of terms `ExtraArgs`. The child process is then started
by appending `ExtraArgs` to the existing start function arguments, that is, by
calling [`apply(M, F, A++ExtraArgs)`](`apply/3`), where `{M,F,A}` is the start
function defined in the child specification.

- If there already exists a child specification with the specified identifier,
  `ChildSpec` is discarded, and the function returns `{error,already_present}`
  or `{error,{already_started,Child}}`, depending on if the corresponding child
  process is running or not.
- If the child process start function returns `{ok,Child}` or `{ok,Child,Info}`,
  the child specification and pid are added to the supervisor and the function
  returns the same value.
- If the child process start function returns `ignore`, the child specification
  `ChildSpec` is added to the supervisor if it is an `one_for_one`, `one_for_all`
  or `rest_for_one` supervisor, and the pid is set to `undefined`. For
  `simple_one_for_one` supervisors, no child is added to the supervisor. The
  function returns `{ok,undefined}`.

If the child process start function returns an error tuple or an erroneous
value, or if it fails, the child specification is discarded, and the function
returns `{error,Error}`, where `Error` is a term containing information about
the error and child specification.
""".
-spec start_child(SupRef, ChildSpec) -> startchild_ret() when
      SupRef :: sup_ref(),
      ChildSpec :: child_spec();
                 (SupRef, ExtraArgs) -> startchild_ret() when
      SupRef :: sup_ref(),
      ExtraArgs :: [term()].
start_child(Supervisor, ChildSpecOrExtraArgs) ->
    call(Supervisor, {start_child, ChildSpecOrExtraArgs}).

-doc """
Tells supervisor `SupRef` to restart a child process corresponding to the child
specification identified by `Id`. The child specification must exist, and the
corresponding child process must not be running.

Notice that for temporary children, the child specification is automatically
deleted when the child terminates; thus, it is not possible to restart such
children.

If the child specification identified by `Id` does not exist, the function
returns `{error,not_found}`. If the child specification exists but the
corresponding process is already running, the function returns
`{error,running}`.

If the child process start function returns `{ok,Child}` or `{ok,Child,Info}`,
the pid is added to the supervisor and the function returns the same value.

If the child process start function returns `ignore`, the pid remains set to
`undefined` and the function returns `{ok,undefined}`.

If the child process start function returns an error tuple or an erroneous
value, or if it fails, the function returns `{error,Error}`, where `Error` is a
term containing information about the error.
""".
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

-doc """
Tells supervisor `SupRef` to delete the child specification identified by `Id`.
The corresponding child process must not be running. Use `terminate_child/2` to
terminate it.

If successful, the function returns `ok`. If the child specification identified
by `Id` exists but the corresponding child process is running or is about to be
restarted, the function returns `{error,running}` or `{error,restarting}`,
respectively. If the child specification identified by `Id` does not exist, the
function returns `{error,not_found}`.
""".
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

-doc """
Tells supervisor `SupRef` to terminate the specified child.

If the supervisor is not `simple_one_for_one`, `Id` must be the child
specification identifier. The process, if any, is terminated and, unless it is a
temporary child, the child specification is kept by the supervisor. The child
process can later be restarted by the supervisor. The child process can also be
restarted explicitly by calling `restart_child/2`. Use `delete_child/2` to
remove the child specification.

If the child is temporary, the child specification is deleted as soon as the
process terminates. This means that [`delete_child/2`](`delete_child/2`) has no
meaning and [`restart_child/2`](`restart_child/2`) cannot be used for these
children.

If the supervisor is `simple_one_for_one`, `Id` must be the `t:pid/0` of the
child process. If the specified process is alive, but is not a child of the
specified supervisor, the function returns `{error,not_found}`. If the child
specification identifier is specified instead of a `t:pid/0`, the function
returns `{error,simple_one_for_one}`.

If successful, the function returns `ok`. If there is no child specification
with the specified `Id`, the function returns `{error,not_found}`.
""".
-spec terminate_child(SupRef, Id) -> Result when
      SupRef :: sup_ref(),
      Id :: pid() | child_id(),
      Result :: 'ok' | {'error', Error},
      Error :: 'not_found' | 'simple_one_for_one'.
terminate_child(Supervisor, Id) ->
    call(Supervisor, {terminate_child, Id}).

-doc """
Returns the child specification map for the child identified by `Id` under
supervisor `SupRef`. The returned map contains all keys, both mandatory and
optional.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec get_childspec(SupRef, Id) -> Result when
      SupRef :: sup_ref(),
      Id :: pid() | child_id(),
      Result :: {'ok', child_spec()} | {'error', Error},
      Error :: 'not_found'.
get_childspec(Supervisor, Id) ->
    call(Supervisor, {get_childspec, Id}).

-doc """
Returns a newly created list with information about all child specifications and
child processes belonging to supervisor `SupRef`.

Notice that calling this function when supervising many children under low
memory conditions can cause an out of memory exception.

The following information is given for each child specification/process:

- `Id` \- As defined in the child specification or `undefined` for a
  `simple_one_for_one` supervisor.
- `Child` \- The pid of the corresponding child process, the atom `restarting`
  if the process is about to be restarted, or `undefined` if there is no such
  process.
- `Type` \- As defined in the child specification.
- `Modules` \- As defined in the child specification.
""".
-spec which_children(SupRef) -> [{Id,Child,Type,Modules}] when
      SupRef :: sup_ref(),
      Id :: child_id() | undefined,
      Child :: child() | 'restarting',
      Type :: worker(),
      Modules :: modules().
which_children(Supervisor) ->
    call(Supervisor, which_children).

-doc """
Returns a [property list](`t:proplists:proplist/0`) containing the counts for each of
the following elements of the supervisor's child specifications and managed
processes:

- `specs` \- The total count of children, dead or alive.
- `active` \- The count of all actively running child processes managed by this
  supervisor. For a `simple_one_for_one` supervisors, no check is done to ensure
  that each child process is still alive, although the result provided here is
  likely to be very accurate unless the supervisor is heavily overloaded.
- `supervisors` \- The count of all children marked as `child_type = supervisor`
  in the specification list, regardless if the child process is still alive.
- `workers` \- The count of all children marked as `child_type = worker` in the
  specification list, regardless if the child process is still alive.
""".
-doc(#{since => <<"OTP R13B04">>}).
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

-doc(#{equiv => check_childspecs(ChildSpecs, undefined)}).
-spec check_childspecs(ChildSpecs) -> Result when
      ChildSpecs :: [child_spec()],
      Result :: 'ok' | {'error', Error :: term()}.
check_childspecs(ChildSpecs) ->
    check_childspecs(ChildSpecs, undefined).

-doc """
Takes a list of child specification as argument and returns `ok` if all of them
are syntactically correct, otherwise `{error,Error}`.

If the `AutoShutdown` argument is not `undefined`, also
checks if the child specifications are allowed for the given
[auto_shutdown](`m:supervisor#auto_shutdown`) option.
""".
-doc(#{since => <<"OTP 24.0">>}).
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
-doc false.
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

-doc false.
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
                case do_start_child(SupName, Child, info_report) of
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

do_start_child(SupName, Child, Report) ->
    #child{mfargs = {M, F, Args}} = Child,
    case do_start_child_i(M, F, Args) of
	{ok, Pid} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress(NChild, SupName, Report),
	    {ok, Pid};
	{ok, Pid, Extra} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress(NChild, SupName, Report),
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
-doc false.
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
	    case do_start_child(State#state.name, Child, debug_report) of
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
-doc false.
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
-doc false.
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
-doc false.
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
-doc false.
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
	    case do_start_child(State#state.name, Child, debug_report) of
		{ok, undefined} when ?is_temporary(Child) ->
		    {{ok, undefined}, State};
		{ok, Pid} ->
		    {{ok, Pid}, save_child(Child#child{pid = Pid}, State)};
		{ok, Pid, Extra} ->
		    {{ok, Pid, Extra}, save_child(Child#child{pid = Pid}, State)};
		{error, {already_started, _Pid} = What} ->
		    {{error, What}, State};
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
        {ok, undefined} ->
            %% The child returned ignore when being restarted.
            %% In accordance with the behavior of start_child/2
            %% for simple_one_for_one supervisors, it is dropped
            %% from the supervisor.
            %% Automatic shutdown is not taken into consideration,
            %% since it does not make sense to use it in
            %% simple_one_for_one supervisors.
            {ok, State2};
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
    case do_start_child(State#state.name, Child, info_report) of
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

unlink_flush(Pid, noproc) ->
    {links, Ls} = process_info(self(),links),
    %% We know that the process has terminated. If we still have a link, we are
    %% guaranteed to eventually receive the 'EXIT' message containing the
    %% actual exit reason (or a 'noconnection' exit reason if the connection is
    %% lost). If we do not have a link, the 'EXIT' message is already present
    %% in the message queue unless the child process behaved badly (unlinked
    %% itself from us). If it behaved badly, we may or may not receive an 'EXIT'
    %% message.
    Timeout = case lists:member(Pid, Ls) of
                  true -> infinity;
                  false -> 0
              end,
    receive
        {'EXIT', Pid, ExitReason} ->
            ExitReason
    after Timeout ->
            child_process_unlinked
    end;
unlink_flush(Pid, ExitReason) ->
    %% Leave no 'EXIT' message from this process in the message queue.
    unlink(Pid),
    receive
        {'EXIT', Pid, _} -> ok
    after 0 -> ok
    end,
    ExitReason.

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
    validAutoShutdownForStrategy(AutoShutdown, Strategy),
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

validAutoShutdownForStrategy(any_significant, simple_one_for_one) ->
    throw({bad_combination, [{auto_shutdown, any_significant}, {strategy, simple_one_for_one}]});
validAutoShutdownForStrategy(all_significant, simple_one_for_one) ->
    throw({bad_combination, [{auto_shutdown, all_significant}, {strategy, simple_one_for_one}]});
validAutoShutdownForStrategy(_AutoShutdown, _Strategy) ->
    true.


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
    Threshold = Now - Period,
    Restarts1 = lists:takewhile(
                  fun (R) -> R >= Threshold end,
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

report_progress(Child, SupName, info_report) ->
    ?LOG_INFO(#{label=>{supervisor,progress},
                report=>[{supervisor,SupName},
                         {started,extract_child(Child)}]},
              #{domain=>[otp,sasl],
                report_cb=>fun supervisor:format_log/2,
                logger_formatter=>#{title=>"PROGRESS REPORT"},
                error_logger=>#{tag=>info_report,
                                type=>progress,
                                report_cb=>fun supervisor:format_log/1}});
report_progress(Child, SupName, debug_report) ->
    ?LOG_DEBUG(#{label=>{supervisor,progress},
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
-doc false.
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
-doc false.
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

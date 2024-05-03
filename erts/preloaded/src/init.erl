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
%%
%%           New initial version of init.
%% Booting from a script. The script is fetched either from
%% a local file or distributed from another erlang node.
%% 
%% Flags:
%%        -boot File     : Absolute file name of the boot script.
%%        -boot_var Var Value
%%                       : $Var in the boot script is expanded to
%%                         Value.
%%        -loader LoaderMethod
%%                       : efile, inet
%%                         (Optional - default efile)
%%        -hosts [Node]  : List of hosts from which we can boot.
%%                         (Mandatory if -loader inet)
%%        -mode interactive : Auto load modules not needed at startup (default system behaviour).
%%        -mode embedded : Load all modules in the boot script, disable auto loading.
%%        -path          : Override path in bootfile.
%%        -pa Path+      : Add my own paths first.
%%        -pz Path+      : Add my own paths last.
%%        -run           : Start own processes.
%%        -s             : Start own processes.
%%        -S             : Start own processes and terminate further option processing.
%% 
%% Experimental flags:
%%        -profile_boot    : Use an 'eprof light' to profile boot sequence
%%        -init_debug      : Activate debug printouts in init
%%        -loader_debug    : Activate debug printouts in erl_prim_loader
%%        -code_path_choice : strict | relaxed

-module(init).
-moduledoc """
Coordination of system startup.

This module is preloaded and contains the code for the `init` system process
that coordinates the startup of the system. The first function evaluated at
startup is [`boot(BootArgs)`](`boot/1`), where `BootArgs` is a list of
command-line arguments supplied to the Erlang runtime system from the local
operating system; see [`erl(1)`](erl_cmd.md).

`init` reads the boot script, which contains instructions on how to initiate the
system. For more information about boot scripts, see
[`script(4)`](`e:sasl:script.md`).

`init` also contains functions to restart, reboot, and stop the system.

[](){: #flags }

## Command-Line Flags

> #### Warning {: .warning }
>
> The support for loading of code from archive files is experimental. The only
> purpose of releasing it before it is ready is to obtain early feedback. The
> file format, semantics, interfaces, and so on, can be changed in a future
> release.

The `init` module interprets the following command-line flags:

- **`--`** - Everything following `--` up to the next flag is considered plain
  arguments and can be retrieved using `get_plain_arguments/0`.

- **`-code_path_choice Choice`** - Can be set to `strict` or `relaxed`. It
  controls how each directory in the code path is to be interpreted:

  - Strictly as it appears in the `boot script`, or
  - `init` is to be more relaxed and try to find a suitable directory if it can
    choose from a regular `ebin` directory and an `ebin` directory in an archive
    file.

  It defaults to `strict` from OTP 27 and this option is scheduled for removal
  in OTP 28.

- **`-epmd_module Module`** - Specifies the module to use for registration and
  lookup of node names. Defaults to `erl_epmd`.

- **`-eval Expr`** - Scans, parses, and evaluates an arbitrary expression `Expr`
  during system initialization. If any of these steps fail (syntax error, parse
  error, or exception during evaluation), Erlang stops with an error message. In
  the following example Erlang is used as a hexadecimal calculator:

  ```text
  % erl -noshell -eval 'R = 16#1F+16#A0, io:format("~.16B~n", [R])' \\
  -s erlang halt
  BF
  ```

  If multiple `-eval` expressions are specified, they are evaluated sequentially
  in the order specified. `-eval` expressions are evaluated sequentially with
  `-s` and `-run` function calls (this also in the order specified). As with
  `-s` and `-run`, an evaluation that does not terminate blocks the system
  initialization process.

- **`-extra`** - Everything following `-extra` is considered plain arguments and
  can be retrieved using `get_plain_arguments/0`.

  Example:

  ```erlang
  % erl -extra +A 1 --
  ...
  1> init:get_plain_arguments().
  ["+A","1","--"]
  ```

  The `-extra` flag can be passed on the command line, through `ERL_*FLAGS` or
  `-args_file`. It only effects the remaining command-line flags in the entity
  in which it is passed. If multiple `-extra` flags are passed they are
  concatenated using the same order rules as `ERL_*FLAGS` or `-args_file` in
  which they are given.

  Example:

  ```text
  % ERL_AFLAGS="-extra a" ERL_ZFLAGS="-extra d" erl -extra b -extra c
  ...
  1> init:get_plain_arguments().
  ["a","b","-extra","c","d"]
  ```

- **`-S Mod [Func [Arg1, Arg2, ...]]`** - Evaluates the specified function call
  during system initialization. `Func` defaults to `start`. If no arguments are
  provided, the function is assumed to be of arity 0. Otherwise it is assumed to
  be of arity 1, taking the list `[Arg1,Arg2,...]` as argument. All arguments
  are passed as strings. If an exception is raised, Erlang stops with an error
  message.

  Example:

  ```text
  % erl -S httpd serve --port 8080 /var/www/html
  ```

  This starts the Erlang runtime system and evaluates the function
  `httpd:serve(["--port", "8080", "/var/www/html"])`. All arguments up to the
  end of the command line will be passed to the called function.

  The function is executed sequentially in an initialization process, which then
  terminates normally and passes control to the user. This means that a `-S`
  call that does not return blocks further processing; to avoid this, use some
  variant of `spawn` in such cases.

  The `-S` flag is only allowed on the command line. If passed through
  `ERL_*FLAGS` or `-args_file` it will be parsed as a normal command line flag.

- **`-run Mod [Func [Arg1, Arg2, ...]]`** - Evaluates the specified function
  call during system initialization. `Func` defaults to `start`. If no arguments
  are provided, the function is assumed to be of arity 0. Otherwise it is
  assumed to be of arity 1, taking the list `[Arg1,Arg2,...]` as argument. All
  arguments are passed as strings. If an exception is raised, Erlang stops with
  an error message.

  Example:

  ```text
  % erl -run foo -run foo bar -run foo bar baz 1 2
  ```

  This starts the Erlang runtime system and evaluates the following functions:

  ```text
  foo:start()
  foo:bar()
  foo:bar(["baz", "1", "2"]).
  ```

  The functions are executed sequentially in an initialization process, which
  then terminates normally and passes control to the user. This means that a
  `-run` call that does not return blocks further processing; to avoid this, use
  some variant of `spawn` in such cases.

  > #### Note {: .info }
  >
  > This flag will not forward arguments beginning with a hyphen (-) to the
  > specified function, as these will be interpreted as flags to the runtime. If
  > the function uses flags in this form, it is advised to use `-S` instead.

- **`-s Mod [Func [Arg1, Arg2, ...]]`** - Evaluates the specified function call
  during system initialization. `Func` defaults to `start`. If no arguments are
  provided, the function is assumed to be of arity 0. Otherwise it is assumed to
  be of arity 1, taking the list `[Arg1,Arg2,...]` as argument. All arguments
  are passed as atoms. If an exception is raised, Erlang stops with an error
  message.

  Example:

  ```text
  % erl -s foo -s foo bar -s foo bar baz 1 2
  ```

  This starts the Erlang runtime system and evaluates the following functions:

  ```text
  foo:start()
  foo:bar()
  foo:bar([baz, '1', '2']).
  ```

  The functions are executed sequentially in an initialization process, which
  then terminates normally and passes control to the user. This means that a
  `-s` call that does not return blocks further processing; to avoid this, use
  some variant of `spawn` in such cases.

  Because of the limited length of atoms, it is recommended to use `-run`
  instead.

  > #### Note {: .info }
  >
  > This flag will not forward arguments beginning with a hyphen (-) to the
  > specified function, as these will be interpreted as flags to the runtime. If
  > the function uses flags in this form, it is advised to use `-S` instead,
  > with the additional caveat that arguments are passed as strings instead of
  > atoms.

## Example

```erlang
% erl -- a b -children thomas claire -ages 7 3 -- x y
...

1> init:get_plain_arguments().
["a","b","x","y"]
2> init:get_argument(children).
{ok,[["thomas","claire"]]}
3> init:get_argument(ages).
{ok, [["7","3"]]}
4> init:get_argument(silly).
error
```

## See Also

`m:erl_prim_loader`, `m:heart`
""".

-export([restart/1,restart/0,reboot/0,stop/0,stop/1,
	 get_status/0,boot/1,get_arguments/0,get_plain_arguments/0,
	 get_argument/1,script_id/0,script_name/0]).

%% for the on_load functionality; not for general use
-export([run_on_load_handlers/0]).

%% internal exports
-export([fetch_loaded/0,ensure_loaded/1,make_permanent/2,
	 notify_when_started/1,wait_until_started/0, 
	 objfile_extension/0, archive_extension/0,code_path_choice/0,
         get_configfd/1, set_configfd/2]).

-include_lib("kernel/include/file.hrl").

-doc "Current status of init.".
-type internal_status() :: 'starting' | 'started' | 'stopping'.
-doc "Code loading mode.".
-type mode() :: 'embedded' | 'interactive'.

-record(state, {flags = [],
		args = [],
		start = [],
		kernel = []                   :: [{atom(), pid()}],
		bootpid                       :: pid(),
		status = {starting, starting} :: {internal_status(), term()},
		script_id = [],
		loaded = [],
		subscribed = [],
                configfdid_to_config = #{}    :: #{} | #{integer() := term()},
                script_name = {[],[]}         :: {string(), string()}}).
-type state() :: #state{}.

%% Data for eval_script/2.
-record(es,
	{init,
	 debug,
	 path,
	 pa,
	 pz,
	 path_choice,
	 prim_load,
	 load_mode,
	 vars
	}).

-define(ON_LOAD_HANDLER, init__boot__on_load_handler).


debug(false, _) -> ok;
debug(_, T)     -> erlang:display(T).

debug(false, _, Fun) ->
    Fun();
debug(_, T, Fun) ->
    erlang:display(T),
    T1 = erlang:monotonic_time(),
    Val = Fun(),
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T1, native, microsecond),
    erlang:display({done_in_microseconds, Time}),
    Val.

-doc false.
-spec get_configfd(integer()) -> none | term().
get_configfd(ConfigFdId) ->
    request({get_configfd, ConfigFdId}).

-doc false.
-spec set_configfd(integer(), term()) -> 'ok'.
set_configfd(ConfigFdId, Config) ->
    request({set_configfd, ConfigFdId, Config}),
    ok.

-doc """
Returns all command-line flags and the system-defined flags, see
`get_argument/1`.
""".
-spec get_arguments() -> Flags when
      Flags :: [{Flag :: atom(), Values :: [string()]}].
get_arguments() ->
    request(get_arguments).

-doc "Returns any plain command-line arguments as a list of strings (possibly empty).".
-spec get_plain_arguments() -> [Arg] when
      Arg :: string().
get_plain_arguments() ->
    bs2ss(request(get_plain_arguments)).

-doc """
Returns all values associated with the command-line user flag `Flag`.

If `Flag` is provided several times, each `Values` is returned in preserved order.
Example:

```erlang
% erl -a b c -a d
...
1> init:get_argument(a).
{ok,[["b","c"],["d"]]}
```

The following flags are defined automatically and can be retrieved using this
function:

- **`root`** - The installation directory of Erlang/OTP, `$ROOT`:

  ```text
  2> init:get_argument(root).
  {ok,[["/usr/local/otp/releases/otp_beam_solaris8_r10b_patched"]]}
  ```

- **`progname`** - The name of the program which started Erlang:

  ```erlang
  3> init:get_argument(progname).
  {ok,[["erl"]]}
  ```

- **`home`{: #home }** - The home directory (on Unix, the value of $HOME):

  ```erlang
  4> init:get_argument(home).
  {ok,[["/home/harry"]]}
  ```

Returns `error` if no value is associated with `Flag`.
""".
-spec get_argument(Flag) -> {'ok', Arg} | 'error' when
      Flag :: atom(),
      Arg :: [Values :: [string()]].
get_argument(Arg) ->
    request({get_argument, Arg}).

-doc """
Gets the identity of the boot script used to boot the system.

`Id` can be any Erlang term. In the delivered boot scripts, `Id` is `{Name, Vsn}`.
`Name` and `Vsn` are strings.
""".
-spec script_id() -> Id when
      Id :: term().
script_id() ->
    request(script_id).

%% Returns the path to the boot script. The path can only be relative
%% (i.e., not absolute) if prim_file:get_cwd() returned an error tuple
%% during boot. filename:absname/2 is not available during boot so we
%% construct the path here instead of during boot
-doc false.
-spec script_name() -> string().
script_name() ->
    {BootCWD, ScriptPath} = request(get_script_name),
    case BootCWD of
        [] ->
            ScriptPath;
        _ ->
            %% Makes the path absolute if ScriptPath is a relative
            %% path
            filename:absname(ScriptPath, BootCWD)
    end.

%% Module internal function to set the script name during boot
-spec set_script_name(BootCurrentWorkingDir, ScriptPath) -> 'ok' when
      BootCurrentWorkingDir :: string(),
      ScriptPath :: string().
set_script_name(BootCurrentWorkingDir, ScriptPath) ->
    request({set_script_name, {BootCurrentWorkingDir, ScriptPath}}),
    ok.

bs2as(L0) when is_list(L0) ->
    map(fun b2a/1, L0);
bs2as(L) ->
    L.

bs2ss(L0) when is_list(L0) ->
    map(fun b2s/1, L0);
bs2ss(L) ->
    L.

-doc """
The current status of the `init` process can be inspected.

During system startup (initialization), `InternalStatus` is `starting`, and
`ProvidedStatus` indicates how far the boot script has been interpreted. Each
`{progress, Info}` term interpreted in the boot script affects `ProvidedStatus`,
that is, `ProvidedStatus` gets the value of `Info`.
""".
-spec get_status() -> {InternalStatus, ProvidedStatus} when
      InternalStatus :: internal_status(),
      ProvidedStatus :: term().
get_status() ->
    request(get_status).

-doc false.
-spec fetch_loaded() -> [{module(),file:filename()}].
fetch_loaded() ->
    request(fetch_loaded).

%% Handle dynamic code loading until the
%% real code_server has been started.
-doc false.
-spec ensure_loaded(atom()) -> 'not_allowed' | {'module', atom()}.
ensure_loaded(Module) ->
    request({ensure_loaded, Module}).

-doc false.
-spec make_permanent(file:filename(), 'false' | file:filename()) ->
	'ok' | {'error', term()}.
make_permanent(Boot,Config) ->
    request({make_permanent,Boot,Config}).

-doc false.
-spec notify_when_started(pid()) -> 'ok' | 'started'.
notify_when_started(Pid) ->
    request({notify_when_started,Pid}).

-doc false.
-spec wait_until_started() -> 'ok'.
wait_until_started() ->
    receive 
	{init,started} -> ok 
    end.	    

request(Req) ->
    init ! {self(),Req},
    receive 
	{init,Rep} -> 
	    Rep
    end.

-doc "The same as [`restart([])`](`restart/1`).".
-spec restart() -> 'ok'.
restart() -> restart([]).

-doc """
Restart all Erlang applications.

The system is restarted _inside_ the running Erlang node, which means that the
emulator is not restarted. All applications are taken down smoothly, all code is
unloaded, and all ports are closed before the system is booted again in the same
way as initially started.

The same `BootArgs` are used when restarting the system unless the `mode` option
is given, allowing the code loading mode to be set to either `embedded` or
`interactive`. All other `BootArgs` remain the same.

To limit the shutdown time, the time `init` is allowed to spend taking down
applications, command-line flag `-shutdown_time` is to be used.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec restart([{mode, mode()}]) -> 'ok'.
restart([]) ->
    init ! {stop,restart}, ok;
restart([{mode, Mode}]) when Mode =:= embedded; Mode =:= interactive ->
    init ! {stop,{restart,Mode}}, ok;
restart(Opts) when is_list(Opts) ->
    erlang:error(badarg, [Opts]).

-doc """
Reboot the Erlang node.

All applications are taken down smoothly, all code is unloaded, and all ports
are closed before the system terminates.

If command-line flag `-heart` was specified, the `heart` program tries to reboot
 the system. For more information, see `m:heart`.

To limit the shutdown time, the time `init` is allowed to spend taking down
applications, command-line flag `-shutdown_time` is to be used.
""".
-spec reboot() -> 'ok'.
reboot() -> init ! {stop,reboot}, ok.

-doc "The same as [`stop(0)`](`stop/1`).".
-spec stop() -> 'ok'.
stop() -> init ! {stop,stop}, ok.

-doc """
Stop the Erlang node.

All applications are taken down smoothly, all code is unloaded, and all ports
are closed before the system terminates by calling [`halt(Status)`](`halt/1`).
If command-line flag `-heart` was specified, the `heart` program is terminated
before the Erlang node terminates. For more information, see `m:heart`.

To limit the shutdown time, the time `init` is allowed to spend taking down
applications, command-line flag `-shutdown_time` is to be used.
""".
-spec stop(Status) -> 'ok' when
      Status :: non_neg_integer() | string().
stop(Status) when is_integer(Status), Status >= 0 ->
    stop_1(Status);
stop(Status) when is_list(Status) ->
    case is_bytelist(Status) of
        true ->
            stop_1(Status);
        false ->
            erlang:error(badarg)
    end;
stop(_) ->
    erlang:error(badarg).

is_bytelist([B|Bs]) when is_integer(B), B >= 0, B < 256 -> is_bytelist(Bs);
is_bytelist([]) -> true;
is_bytelist(_) -> false.

%% Note that we check the type of Status beforehand to ensure that
%% the call to halt(Status) by the init process cannot fail
stop_1(Status) -> init ! {stop,{stop,Status}}, ok.

-doc """
Starts the Erlang runtime system.

This function is called when the emulator is started and coordinates system startup.

`BootArgs` are all command-line arguments except the emulator flags, that is,
flags and plain arguments; see [`erl(1)`](erl_cmd.md).

`init` interprets some of the flags, see section
[Command-Line Flags](`m:init#flags`) below. The remaining flags ("user flags")
and plain arguments are passed to the `init` loop and can be retrieved by
calling `get_arguments/0` and `get_plain_arguments/0`, respectively.
""".
-spec boot(BootArgs) -> no_return() when
      BootArgs :: [binary()].
boot(BootArgs) ->
    register(init, self()),
    process_flag(trap_exit, true),

    {Start,Flags,Args} = parse_boot_args(BootArgs),
    %% We don't get to profile parsing of BootArgs
    case b2a(get_flag(profile_boot, Flags, false)) of
        false -> ok;
        true  -> debug_profile_start()
    end,
    boot(Start, Flags, Args).

fold_eval_args([Expr]) -> Expr;
fold_eval_args(Exprs) -> Exprs.

%% Ensure that when no arguments were explicitly passed on the command line,
%% an empty arguments list will be passed to the function to be applied.
interpolate_empty_mfa_args({M, F, []}) -> {M, F, [[]]};
interpolate_empty_mfa_args({_M, _F, [_Args]} = MFA) -> MFA.

-spec run_args_to_mfa([binary()]) -> {atom(), atom(), [] | [nonempty_list(binary())]} | no_return().
run_args_to_mfa([]) ->
    erlang:display_string(
      "Error! The -S option must be followed by at least a module to start, such as "
      "`-S Module` or `-S Module Function` to start with a function.\r\n\r\n"
    ),
    halt();
run_args_to_mfa([M]) -> {b2a(M), start, []};
run_args_to_mfa([M, F]) -> {b2a(M), b2a(F), []};
run_args_to_mfa([M, F | A]) -> {b2a(M), b2a(F), [A]}.

%% Convert -run / -s / -S arguments to startup instructions, such that
%% no instructions are emitted if no arguments follow the flag, otherwise,
%% an `{apply, M, F, A}' instruction is.
run_args_to_start_instructions([], _Converter) -> [];
run_args_to_start_instructions(Args, Converter) ->
    {M, F, A} = run_args_to_mfa(Args),
    [{apply, M, F, map(Converter, A)}].

b2a(Bin) when is_binary(Bin) ->
    list_to_atom(b2s(Bin));
b2a(A) when is_atom(A) ->
    A.

b2s(Bin) when is_binary(Bin) ->
    try
	unicode:characters_to_list(Bin,file:native_name_encoding())
    catch
	_:_ -> binary_to_list(Bin)
    end;
b2s(L) when is_list(L) ->
    L.

map(_F, []) ->
    [];
map(F, [X|Rest]) ->
    [F(X) | map(F, Rest)].

-doc false.
-spec code_path_choice() -> 'relaxed' | 'strict'.
code_path_choice() ->
    case get_argument(code_path_choice) of
	{ok,[["strict"]]} ->
	    strict;
	{ok,[["relaxed"]]} ->
	    relaxed;
	_Else ->
	    strict
    end.

boot(Start,Flags,Args) ->
    start_on_load_handler_process(),
    BootPid = do_boot(Flags,Start),
    State = #state{flags = Flags,
		   args = Args,
		   start = Start,
		   bootpid = BootPid},
    boot_loop(BootPid,State).

things_to_string([X|Rest]) ->
    " (" ++ erts_internal:term_to_string(X, 32768) ++ ")" ++
        things_to_string(Rest);
things_to_string([]) ->
    "".

halt_string(String, List) ->
    String ++ things_to_string(List).

%% String = string()
%% List = [string() | atom() | pid() | number() | list() | tuple()]
%% Items in List are truncated if found to be too large
-spec crash(_, _) -> no_return().
crash(String, List) ->
    halt(halt_string(String, List)).

%% Status is {InternalStatus,ProvidedStatus}
-spec boot_loop(pid(), state()) -> no_return().
boot_loop(BootPid, State) ->
    receive
	{BootPid,loaded,NewlyLoaded} ->
	    Loaded = NewlyLoaded ++ State#state.loaded,
	    boot_loop(BootPid, State#state{loaded = Loaded});
	{BootPid,started,KernelPid} ->
	    boot_loop(BootPid, new_kernelpid(KernelPid, BootPid, State));
	{BootPid,progress,started} ->
            {InS,_} = State#state.status,
	    notify(State#state.subscribed),
	    boot_loop(BootPid,State#state{status = {InS,started},
					  subscribed = []});
	{BootPid,progress,NewStatus} ->
            {InS,_} = State#state.status,
	    boot_loop(BootPid,State#state{status = {InS,NewStatus}});
	{BootPid,{script_id,Id}} ->
	    boot_loop(BootPid,State#state{script_id = Id});
	{'EXIT',BootPid,normal} ->
            {_,PS} = State#state.status,
	    notify(State#state.subscribed),
	    loop(State#state{status = {started,PS},
			     subscribed = []});
	{'EXIT',BootPid,Reason} ->
	    % erlang:display({"init terminating in do_boot",Reason}),
	    crash("Runtime terminating during boot", [Reason]);
	{'EXIT',Pid,Reason} ->
	    Kernel = State#state.kernel,
	    terminate(Pid,Kernel,Reason), %% If Pid is a Kernel pid, halt()!
	    boot_loop(BootPid,State);
	{stop,Reason} ->
	    stop(Reason,State);
	{From,fetch_loaded} ->   %% Fetch and reset initially loaded modules.
	    From ! {init,State#state.loaded},
	    garb_boot_loop(BootPid,State#state{loaded = []});
	{From,{ensure_loaded,Module}} ->
	    {Res, Loaded} = ensure_loaded(Module, State#state.loaded),
	    From ! {init,Res},
	    boot_loop(BootPid,State#state{loaded = Loaded});
	Msg ->
	    boot_loop(BootPid,handle_msg(Msg,State))
    end.

ensure_loaded(Module, Loaded) ->
    case erlang:module_loaded(Module) of
	true ->
	    {{module, Module}, Loaded};
	false ->
	    do_ensure_loaded(Module, Loaded)
    end.

do_ensure_loaded(Module, Loaded) ->
    File = atom_to_list(Module) ++ objfile_extension(),
    case erl_prim_loader:get_file(File) of
	{ok,BinCode,FullName} ->
	    case do_load_module(Module, BinCode) of
		ok ->
		    {{module, Module}, [{Module, FullName}|Loaded]};
		error ->
		    {error, [{Module, FullName}|Loaded]}
	    end;
	Error ->
	    {Error, Loaded}
    end.

%% Tell subscribed processes the system has started.
notify(Pids) ->
    lists:foreach(fun(Pid) -> Pid ! {init,started} end, Pids).

%% Garbage collect all info about initially loaded modules.
%% This information is temporarily stored until the code_server
%% is started.
%% We force the garbage collection as the init process holds
%% this information during the initialisation of the system and
%% it will be automatically garbed much later (perhaps not at all
%% if it is not accessed much).

garb_boot_loop(BootPid,State) ->
    garbage_collect(),
    boot_loop(BootPid,State).

new_kernelpid({Name,{ok,Pid}},BootPid,State) when is_pid(Pid) ->
    link(Pid),
    BootPid ! {self(),ok,Pid},
    Kernel = State#state.kernel,
    State#state{kernel = [{Name,Pid}|Kernel]};
new_kernelpid({_Name,ignore},BootPid,State) ->
    BootPid ! {self(),ignore},
    State;
new_kernelpid({Name,What},BootPid,State) ->
    % erlang:display({"could not start kernel pid",Name,What}),
    clear_system(false,BootPid,State),
    crash("Could not start kernel pid", [Name, What]).

%% Here is the main loop after the system has booted.

loop(State) ->
    receive
	{'EXIT',Pid,Reason} ->
	    Kernel = State#state.kernel,
	    terminate(Pid,Kernel,Reason), %% If Pid is a Kernel pid, halt()!
	    loop(State);
	{stop,Reason} ->
	    stop(Reason,State);
	{From,fetch_loaded} ->           %% The Loaded info is cleared in
	    Loaded = State#state.loaded, %% boot_loop but is handled here 
	    From ! {init,Loaded},        %% anyway.
	    loop(State);
	Msg ->
	    loop(handle_msg(Msg,State))
    end.

handle_msg(Msg,State0) ->
    case catch do_handle_msg(Msg,State0) of
	{new_state,State} -> State;
	_                 -> State0
    end.

do_handle_msg(Msg,State) ->
    #state{flags = Flags,
	   status = Status,
	   script_id = Sid,
	   args = Args,
	   subscribed = Subscribed,
           configfdid_to_config = ConfigFdIdToConfig,
           script_name = ScriptName} = State,
    case Msg of
	{From,get_plain_arguments} ->
	    From ! {init,Args};
	{From,get_arguments} ->
	    From ! {init,get_arguments(Flags)};
	{From,{get_argument,Arg}} ->
	    From ! {init,get_argument(Arg,Flags)};
	{From,get_status} -> 
	    From ! {init,Status};
	{From,script_id} -> 
	    From ! {init,Sid};
	{From,{make_permanent,Boot,Config}} ->
	    {Res,State1} = make_permanent(Boot,Config,Flags,State),
	    From ! {init,Res},
	    {new_state,State1};
	{From,{notify_when_started,Pid}} ->
	    case Status of
		{InS,PS} when InS =:= started ; PS =:= started ->
		    From ! {init,started};
		_ ->
		    From ! {init,ok},
		    {new_state,State#state{subscribed = [Pid|Subscribed]}}
	    end;
	{From, {ensure_loaded, _}} ->
	    From ! {init, not_allowed};
	{From, {get_configfd, ConfigFdId}} ->
            case ConfigFdIdToConfig of
                #{ConfigFdId := Config} ->
                    From ! {init, Config};
                _ ->
                    From ! {init, none}
            end;
	{From, {set_configfd, ConfigFdId, Config}} ->
            From ! {init, ok},
            NewConfigFdIdToConfig = ConfigFdIdToConfig#{ConfigFdId => Config},
            NewState = State#state{configfdid_to_config = NewConfigFdIdToConfig},
            {new_state, NewState};
	{From, get_script_name} ->
            From ! {init, ScriptName};
	{From, {set_script_name, NewScriptName}} ->
            From ! {init, ok},
            NewState = State#state{script_name = NewScriptName},
            {new_state, NewState};
        X ->
            case whereis(user) of
                %% io_requests may end up here from various processes that have
                %% init as their group_leader. Most notably all application_master
                %% processes have init as their gl, though they will short-circuit
                %% to `user` if possible.
                User when element(1, X) =:= io_request,
                          User =/= undefined ->
                    User ! X;
                _ ->
                    %% Only call the logger module if the logger_server is running.
                    %% If it is not running, then we don't know that the logger
                    %% module can be loaded.
                    case whereis(logger) =/= undefined of
                        true -> logger:info("init got unexpected: ~p", [X],
                                            #{ error_logger=>#{tag=>info_msg}});
                        false ->
                            erlang:display_string(stdout, "init got unexpected: "),
                            erlang:display(X)
                    end
            end
    end.

%%% -------------------------------------------------
%%% A new release has been installed and made
%%% permanent.
%%% Both restart/0 and reboot/0 shall startup using
%%% the new release. reboot/0 uses new boot script
%%% and configuration file pointed out externally.
%%% In the restart case we have to set new -boot and
%%% -config arguments.
%%% -------------------------------------------------

make_permanent(Boot,Config,Flags0,State) ->
    case set_flag(boot, Boot, Flags0) of
	{ok,Flags1} ->
	    case set_flag(config, Config, Flags1) of
		{ok,Flags} ->
		    {ok,State#state{flags = Flags}};
		Error ->
		    {Error,State}
	    end;
	Error ->
	    {Error,State}
    end.

set_flag(_Flag,false,Flags) ->
    {ok,Flags};
set_flag(Flag,Value,Flags) when is_list(Value) ->
    %% The flag here can be -boot or -config, which means the value is
    %% a file name! Thus the file name encoding is used when converting.
    Encoding = file:native_name_encoding(),
    case catch unicode:characters_to_binary(Value,Encoding,Encoding) of
	{'EXIT',_} ->
	    {error,badarg};
	AValue ->
	    {ok,set_argument(Flags,Flag,AValue)}
    end;
set_flag(_,_,_) ->
    {error,badarg}.

%%% -------------------------------------------------
%%% Stop the system. 
%%% Reason is: restart | {restart, Mode} | reboot | stop
%%% According to reason terminate emulator or restart
%%% system using the same init process again.
%%% -------------------------------------------------

stop(Reason,State) ->
    BootPid = State#state.bootpid,
    {_,Progress} = State#state.status,
    State1 = State#state{status = {stopping, Progress}},
    clear_system(should_unload(Reason),BootPid,State1),
    do_stop(Reason,State1).

%% There is no need to unload code if the system is shutting down
should_unload(stop) -> false;
should_unload({stop, _}) -> false;
should_unload(_) -> true.

do_stop({restart,Mode},#state{start=Start, flags=Flags0, args=Args}) ->
    Flags = update_flag(mode, Flags0, atom_to_binary(Mode)),
    do_restart(Start,Flags,Args);
do_stop(restart,#state{start=Start, flags=Flags, args=Args}) ->
    do_restart(Start,Flags,Args);
do_stop(reboot,_) ->
    halt();
do_stop(stop,State) ->
    stop_heart(State),
    halt();
do_stop({stop,Status},State) ->
    stop_heart(State),
    halt(Status).

do_restart(Start,Flags,Args) ->
    flush(),
    erl_init:restart(),
    boot(Start,Flags,Args).

clear_system(Unload,BootPid,State) ->
    Heart = get_heart(State#state.kernel),
    Logger = get_logger(State#state.kernel),
    shutdown_pids(Heart,Logger,BootPid,State),
    Unload andalso unload(Heart),
    kill_em([Logger]),
    Unload andalso do_unload([logger_server]).

flush() ->
    receive
        _M -> flush()
    after 0 ->
            ok
    end.

stop_heart(State) ->
    case get_heart(State#state.kernel) of
	false ->
	    ok;
	Pid ->
	    %% As heart survives a restart the Parent of heart is init.
	    BootPid = self(),
	    %% ignore timeout
	    shutdown_kernel_pid(Pid, BootPid, self(), State) 
    end.

shutdown_pids(Heart,Logger,BootPid,State) ->
    Timer = shutdown_timer(State#state.flags),
    global_prepare_shutdown(),
    catch shutdown(State#state.kernel,BootPid,Timer,State),
    kill_all_pids(Heart,Logger), % Even the shutdown timer.
    kill_all_ports(Heart), % Logger has no ports
    flush_timout(Timer).

global_prepare_shutdown() ->
    %% Inform global that we are shutting down, so it wont
    %% send 'lost_connection' messages when connections
    %% goes down...
    case whereis(global_name_server) of
        undefined ->
            ok;
        Pid ->
            Mon = erlang:monitor(process, Pid),
            Pid ! {prepare_shutdown, self(), Mon},
            receive
                {Mon, ok} ->
                    erlang:demonitor(Mon, [flush]),
                    ok;
                {'DOWN', Mon, process, Pid, _Reason} ->
                    ok
            end
    end.

get_heart(Kernel) ->
    get_kernelpid(heart,Kernel).

get_logger(Kernel) ->
    get_kernelpid(logger,Kernel).

get_kernelpid(Name,[{Name,Pid}|_Kernel]) -> Pid;
get_kernelpid(Name,[_|Kernel])           -> get_kernelpid(Name,Kernel);
get_kernelpid(_,_)                    -> false.


shutdown([{Except,_Pid}|Kernel],BootPid,Timer,State)
  when Except==heart; Except==logger ->
    shutdown(Kernel, BootPid, Timer, State);
shutdown([{_Name,Pid}|Kernel],BootPid,Timer,State) ->
    shutdown_kernel_pid(Pid, BootPid, Timer, State),
    shutdown(Kernel,BootPid,Timer,State);
shutdown(_,_,_,_) ->
    true.


%%
%% A kernel pid must handle the special case message
%% {'EXIT',Parent,Reason} and terminate upon it!
%%
shutdown_kernel_pid(Pid, BootPid, Timer, State) ->
    Pid ! {'EXIT',BootPid,shutdown},
    shutdown_loop(Pid, Timer, State, []).

%%
%% We have to handle init requests here in case a process
%% performs such a request and cannot shutdown (deadlock).
%% Keep all other EXIT messages in case it was another
%% kernel process. Resend these messages and handle later.
%%
shutdown_loop(Pid,Timer,State,Exits) ->
    receive
	{'EXIT',Pid,_} ->
	    resend(reverse(Exits)),
	    ok;
	{Timer,timeout} ->
	    erlang:display({init,shutdown_timeout}),
	    throw(timeout);
	{stop,_} ->
	    shutdown_loop(Pid,Timer,State,Exits);
	{From,fetch_loaded} ->
	    From ! {init,State#state.loaded},
	    shutdown_loop(Pid,Timer,State,Exits);
	{'EXIT',OtherP,Reason} ->
	    shutdown_loop(Pid,Timer,State,
			  [{'EXIT',OtherP,Reason}|Exits]);
	Msg ->
	    State1 = handle_msg(Msg,State),
	    shutdown_loop(Pid,Timer,State1,Exits)
    end.

resend([ExitMsg|Exits]) ->
    self() ! ExitMsg,
    resend(Exits);
resend(_) ->
    ok.

%%
%% Kill all existing pids in the system (except init and heart).
kill_all_pids(Heart,Logger) ->
    case get_pids(Heart,Logger) of
	[] ->
	    ok;
	Pids ->
	    kill_em(Pids),
	    kill_all_pids(Heart,Logger)  % Continue until all are really killed.
    end.
    
%% All except system processes.
get_pids(Heart,Logger) ->
    Pids = [P || P <- processes(), not erts_internal:is_system_process(P)],
    delete(Heart,Logger,self(),Pids).

delete(Heart,Logger,Init,[Heart|Pids]) -> delete(Heart,Logger,Init,Pids);
delete(Heart,Logger,Init,[Logger|Pids])  -> delete(Heart,Logger,Init,Pids);
delete(Heart,Logger,Init,[Init|Pids])  -> delete(Heart,Logger,Init,Pids);
delete(Heart,Logger,Init,[Pid|Pids])   -> [Pid|delete(Heart,Logger,Init,Pids)];
delete(_,_,_,[])                  -> [].
    
kill_em([Pid|Pids]) ->
    exit(Pid,kill),
    kill_em(Pids);
kill_em([]) ->
    ok.

%%
%% Kill all existing ports in the system (except the heart port),
%% i.e. ports still existing after all processes have been killed.
%%
%% System ports like the async driver port will nowadays be immortal;
%% therefore, it is ok to send them exit signals...
%%
kill_all_ports(Heart) ->
    kill_all_ports(Heart,erlang:ports()).

kill_all_ports(Heart,[P|Ps]) ->
    case erlang:port_info(P,connected) of
	{connected,Heart} ->
	    kill_all_ports(Heart,Ps);
	_ ->
	    exit(P,kill),
	    kill_all_ports(Heart,Ps)
    end;
kill_all_ports(_,_) ->
    ok.

unload(false) ->
    do_unload(erlang:loaded() -- [logger_server|erlang:pre_loaded()]);
unload(_) ->
    do_unload(erlang:loaded() -- [heart,logger_server|erlang:pre_loaded()]).

do_unload([M|Mods]) ->
    catch erlang:purge_module(M),
    catch erlang:delete_module(M),
    catch erlang:purge_module(M),
    do_unload(Mods);
do_unload([]) ->
    ok.

%%% -------------------------------------------------
%%% If the terminated Pid is one of the processes
%%% added to the Kernel, take down the system brutally.
%%% We are not sure that ANYTHING can work anymore,
%%% i.e. halt the system.
%%% Sleep awhile, it is thus possible for the
%%% error_logger (if it is still alive) to write errors
%%% using the simplest method.
%%% -------------------------------------------------

terminate(Pid,Kernel,Reason) ->
    case kernel_pid(Pid,Kernel) of
	{ok,Name} ->
            %% If you change this time, also change the time in logger_simple_h.erl
	    sleep(500), %% Flush error printouts!
	    % erlang:display({"Kernel pid terminated",Name,Reason}),
	    crash("Kernel pid terminated", [Name, Reason]);
	_ ->
	    false
    end.

kernel_pid(Pid,[{Name,Pid}|_]) ->
    {ok,Name};
kernel_pid(Pid,[_|T]) ->
    kernel_pid(Pid,T);
kernel_pid(_,_) ->
    false.

sleep(T) -> receive after T -> ok end.

%%% -------------------------------------------------
%%% Start the loader. 
%%% The loader shall run for ever!
%%% -------------------------------------------------

start_prim_loader(Init, Path0, {Pa,Pz}) ->
    Path = case Path0 of
	       false -> Pa ++ ["."|Pz];
	       _ -> Path0
	   end,
    case erl_prim_loader:start() of
	{ok,Pid} ->
	    erl_prim_loader:set_path(Path),
	    add_to_kernel(Init, Pid);
	{error,Reason} ->
	    erlang:display({"cannot start loader",Reason}),
	    exit(Reason)
    end.

add_to_kernel(Init,Pid) ->
    Init ! {self(),started,{erl_prim_loader,{ok,Pid}}},
    receive
	{Init,ok,Pid} ->
	    unlink(Pid),
	    ok
    end.

%%% -------------------------------------------------
%%% The boot process fetches a boot script and loads
%%% all modules specified and starts spec. processes.
%%% Processes specified with -s or -run are finally started.
%%% -------------------------------------------------

do_boot(Flags,Start) ->
    Self = self(),
    spawn_link(fun() -> do_boot(Self,Flags,Start) end).

do_boot(Init,Flags,Start) ->
    process_flag(trap_exit,true),
    Root = get_root(Flags),
    true = check_bindir(Flags),
    Path = get_flag_list(path, Flags, false),
    {Pa,Pz} = PathFls = path_flags(Flags),
    start_prim_loader(Init, bs2ss(Path), PathFls),
    BootFile = bootfile(Flags,Root),
    BootList = get_boot(BootFile,Root),
    LoadMode = b2a(get_flag(mode, Flags, interactive)),
    Deb = b2a(get_flag(init_debug, Flags, false)),
    catch ?ON_LOAD_HANDLER ! {init_debug_flag,Deb},
    BootVars = get_boot_vars(Root, Flags),

    PathChoice = code_path_choice(),
    Es = #es{init=Init,debug=Deb,path=Path,pa=Pa,pz=Pz,
	     path_choice=PathChoice,
	     prim_load=true,load_mode=LoadMode,
	     vars=BootVars},
    eval_script(BootList, Es),

    start_em(Start),
    case b2a(get_flag(profile_boot,Flags,false)) of
        false -> ok;
        true  ->
            debug_profile_format_mfas(debug_profile_mfas()),
            debug_profile_stop()
    end,
    ok.

get_root(Flags) ->
    case get_argument(root, Flags) of
	{ok,[[Root]]} ->
	    Root;
	_ ->
	    exit(no_or_multiple_root_variables)
    end.

check_bindir(Flags) ->
    case get_argument(bindir, Flags) of
	{ok,[[_Bindir]]} ->
	    true;
	_ ->
	    exit(no_or_multiple_bindir_variables)
    end.

get_boot_vars(Root, Flags) ->
    BootVars = get_boot_vars_1(#{}, Flags),
    RootKey = <<"ROOT">>,
    BootVars#{RootKey=>Root}.

get_boot_vars_1(Vars, [{boot_var,[Key,Value]}|T]) ->
    get_boot_vars_1(Vars#{Key=>Value}, T);
get_boot_vars_1(_, [{boot_var,_}|_]) ->
    exit(invalid_boot_var_argument);
get_boot_vars_1(Vars, [_|T]) ->
    get_boot_vars_1(Vars, T);
get_boot_vars_1(Vars, []) ->
    Vars.

bootfile(Flags,Root) ->
    b2s(get_flag(boot, Flags, Root++"/bin/start")).

path_flags(Flags) ->
    Pa = append(reverse(get_flag_args(pa, Flags))),
    Pz = append(get_flag_args(pz, Flags)),
    {bs2ss(Pa),bs2ss(Pz)}.

get_boot(BootFile0,Root) ->
    BootFile = BootFile0 ++ ".boot",
    
    case get_boot(BootFile) of
	{ok, CmdList} ->
	    CmdList;
	not_found -> %% Check for default.
	    BootF = Root ++ "/bin/" ++ BootFile,
	    case get_boot(BootF)  of
		{ok, CmdList} ->
		    CmdList;
		not_found ->
		    exit({'cannot get bootfile',list_to_atom(BootFile)});
		_ ->
		    exit({'bootfile format error',list_to_atom(BootF)})
	    end;
	_ ->
	    exit({'bootfile format error',list_to_atom(BootFile)})
    end.
    
get_boot(BootFile) ->
    case erl_prim_loader:get_file(BootFile) of
	{ok,Bin,_} ->
	    case binary_to_term(Bin) of
		{script,Id,CmdList} when is_list(CmdList) ->
		    init ! {self(),{script_id,Id}}, % ;-)
                    CWD =
                        case prim_file:get_cwd() of
                            {ok, TheCWD} -> TheCWD;
                            {error, _} -> []
                        end,
                    %% The filename module is not available during
                    %% boot so we call filename:absname/2 in
                    %% init:script_name/0 instead.
                    set_script_name(CWD, BootFile),
                    {ok, CmdList};
		_ ->
		    error
	    end;
	_ ->
	    not_found
    end.

%%
%% Eval a boot script.
%% Load modules and start processes.
%% If a start command does not spawn a new process the
%% boot process hangs (we want to ensure syncronicity).
%%

eval_script([{progress,Info}=Progress|T], #es{debug=Deb}=Es) ->
    debug(Deb, Progress),
    init ! {self(),progress,Info},
    eval_script(T, Es);
eval_script([{preLoaded,_}|T], #es{}=Es) ->
    eval_script(T, Es);
eval_script([{path,Path}|T], #es{path=false,pa=Pa,pz=Pz,
				 path_choice=PathChoice,
				 vars=Vars,debug=Deb}=Es) ->
    debug(Deb, {path,Path},
          fun() ->
                  RealPath0 = make_path(Pa, Pz, Path, Vars),
                  RealPath = patch_path(RealPath0, PathChoice),
                  erl_prim_loader:set_path(RealPath)
          end),
    eval_script(T, Es);
eval_script([{path,_}|T], #es{}=Es) ->
    %% Ignore, use the command line -path flag.
    eval_script(T, Es);
eval_script([{kernel_load_completed}|T], #es{load_mode=Mode}=Es0) ->
    Es = case Mode of
	     embedded -> Es0;
	     _ -> Es0#es{prim_load=false}
	 end,
    eval_script(T, Es);
eval_script([{primLoad,Mods}|T], #es{init=Init,prim_load=PrimLoad,debug=Deb}=Es)
  when is_list(Mods) ->
    case PrimLoad of
	true ->
	    debug(Deb, {primLoad,Mods}, fun() -> load_modules(Mods, Init) end);
	false ->
	    %% Do not load now, code_server does that dynamically!
	    ok
    end,
    eval_script(T, Es);
eval_script([{kernelProcess,Server,{Mod,Fun,Args}}|T],
	    #es{init=Init,debug=Deb}=Es) ->
    debug(Deb, {start,Server}, fun() -> start_in_kernel(Server, Mod, Fun, Args, Init) end),
    eval_script(T, Es);
eval_script([{apply,{Mod,Fun,Args}}=Apply|T], #es{debug=Deb}=Es) ->
    debug(Deb, Apply, fun() -> apply(Mod, Fun, Args) end),
    eval_script(T, Es);
eval_script([], #es{}) ->
    ok;
eval_script(What, #es{}) ->
    exit({'unexpected command in bootfile',What}).

load_modules(Mods0, Init) ->
    Mods = [M || M <- Mods0, not erlang:module_loaded(M)],
    F = prepare_loading_fun(),
    case erl_prim_loader:get_modules(Mods, F) of
	{ok,{Prep0,[]}} ->
	    Prep = [Code || {_,{prepared,Code,_}} <- Prep0],
	    ok = erlang:finish_loading(Prep),
	    Loaded = [{Mod,Full} || {Mod,{_,_,Full}} <- Prep0],
	    Init ! {self(),loaded,Loaded},
	    Beams = [{M,Beam,Full} || {M,{on_load,Beam,Full}} <- Prep0],
	    load_rest(Beams, Init);
	{ok,{_,[_|_]=Errors}} ->
	    Ms = [M || {M,_} <- Errors],
	    exit({load_failed,Ms})
    end.

load_rest([{Mod,Beam,Full}|T], Init) ->
    do_load_module(Mod, Beam),
    Init ! {self(),loaded,[{Mod,Full}]},
    load_rest(T, Init);
load_rest([], _) ->
    ok.

prepare_loading_fun() ->
    fun(Mod, FullName, Beam) ->
	    case erlang:prepare_loading(Mod, Beam) of
		{error,_}=Error ->
		    Error;
		Prepared ->
		    case erlang:has_prepared_code_on_load(Prepared) of
			true ->
			    {ok,{on_load,Beam,FullName}};
			false ->
			    {ok,{prepared,Prepared,FullName}}
		    end
	    end
    end.

make_path(Pa, Pz, Path, Vars) ->
    append([Pa,append([fix_path(Path,Vars),Pz])]).

%% For all Paths starting with $ROOT add rootdir and for those
%% starting with $xxx/, expand $xxx to the value supplied with -boot_var!
%% If $xxx cannot be expanded this process terminates.

fix_path([Path|Ps], Vars) when is_atom(Path) ->
    [add_var(atom_to_list(Path), Vars)|fix_path(Ps, Vars)];
fix_path([Path|Ps], Vars) ->
    [add_var(Path, Vars)|fix_path(Ps, Vars)];
fix_path(_, _) ->
    [].

add_var("$"++Path0, Vars) ->
    {Var,Path} = extract_var(Path0, []),
    Key = list_to_binary(Var),
    case Vars of
	#{Key:=Value0} ->
	    Value = b2s(Value0),
	    Value ++ "/" ++ Path;
	_ ->
	    Error0 = "cannot expand $" ++ Var ++ " in bootfile",
	    Error = list_to_atom(Error0),
	    exit(Error)
    end;
add_var(Path, _) ->
    Path.

extract_var([$/|Path],Var) -> {reverse(Var),Path};
extract_var([H|T],Var)     -> extract_var(T,[H|Var]);
extract_var([],Var)        -> {reverse(Var),[]}.

patch_path(Dirs, strict) ->
    Dirs;
patch_path(Dirs, relaxed) ->
    ArchiveExt = archive_extension(),
    [patch_dir(Dir, ArchiveExt) || Dir <- Dirs].

patch_dir(Orig, ArchiveExt) ->
    case funny_split(Orig, $/) of
	["nibe", RevApp, RevArchive | RevTop] ->
	    App = reverse(RevApp),
	    case funny_splitwith(RevArchive, $.) of
		{Ext, Base} when Ext =:= ArchiveExt, Base =:= App ->
		    %% Orig archive
		    Top = reverse([reverse(C) || C <- RevTop]),
		    Dir = join(Top ++ [App, "ebin"], "/"),
		    Archive = Orig;
		_ ->
		    %% Orig directory
		    Top = reverse([reverse(C) || C <- [RevArchive | RevTop]]),
		    Archive = join(Top ++ [App ++ ArchiveExt, App, "ebin"], "/"),
		    Dir = Orig
	    end,
	    %% First try dir, second try archive and at last use orig if both fails
	    case erl_prim_loader:read_file_info(Dir) of
		{ok, #file_info{type = directory}} ->
		    Dir;
		_ ->
		    case erl_prim_loader:read_file_info(Archive) of
			{ok, #file_info{type = directory}} ->
			    Archive;
			_ ->
			    Orig
		    end
	    end;
	_ ->
	    Orig
    end.

%% Returns all lists in reverse order
funny_split(List, Sep) ->
   funny_split(List, Sep, [], []).

funny_split([Sep | Tail], Sep, Path, Paths) ->
    funny_split(Tail, Sep, [], [Path | Paths]);
funny_split([Head | Tail], Sep, Path, Paths) ->
    funny_split(Tail, Sep, [Head | Path], Paths);
funny_split([], _Sep, Path, Paths) ->
    [Path | Paths].

%% Returns {BeforeSep, AfterSep} where BeforeSep is in reverse order
funny_splitwith(List, Sep) ->
    funny_splitwith(List, Sep, [], List).

funny_splitwith([Sep | Tail], Sep, Acc, _Orig) ->
    {Acc, Tail};
funny_splitwith([Head | Tail], Sep, Acc, Orig) ->
    funny_splitwith(Tail, Sep, [Head | Acc], Orig);
funny_splitwith([], _Sep, _Acc, Orig) ->
    {[], Orig}.

-spec join([string()], string()) -> string().
join([H1, H2 | T], S) ->
    H1 ++ S ++ join([H2 | T], S);
join([H], _) ->
    H.

%% Servers that are located in the init kernel are linked
%% and supervised by init.

start_in_kernel(Server,Mod,Fun,Args,Init) ->
    Res = apply(Mod,Fun,Args),
    Init ! {self(),started,{Server,Res}},
    receive
	{Init,ok,Pid} ->
	    unlink(Pid),  %% Just for sure...
	    ok;
	{Init,ignore} ->
	    ignore
    end.

%% Do start all processes specified at command line using -s!
%% Use apply here instead of spawn to ensure syncronicity for
%% those servers that wish to have it so.
%% Disadvantage: anything started with -s that does not
%% eventually spawn will hang the startup routine.

%% We also handle -eval here. The argument is an arbitrary
%% expression that should be parsed and evaluated.

start_em([S|Tail]) ->
    case whereis(user) of
	undefined -> 
	    ok;
	P when is_pid(P) ->			%Let's set the group_leader()
	    erlang:group_leader(P, self())
    end,
    start_it(S),
    start_em(Tail);
start_em([]) -> ok.

start_it([]) ->
    ok;
start_it({eval,Bin}) ->
    Str = b2s(Bin),
    try
        {ok,Ts,_} = erl_scan:string(Str),
        Ts1 = case reverse(Ts) of
                  [{dot,_}|_] -> Ts;
                  TsR -> reverse([{dot,erl_anno:new(1)} | TsR])
              end,
        {ok,Expr} = erl_parse:parse_exprs(Ts1),
        {value, _Value, _Bs} = erl_eval:exprs(Expr, erl_eval:new_bindings()),
        ok
    catch E:R:ST ->
            Message = [<<"Error! Failed to eval: ">>, Bin, <<"\r\n\r\n">>],
            erlang:display_string(binary_to_list(iolist_to_binary(Message))),
            erlang:raise(E,R,ST)
    end;
start_it({apply,M,F,Args}) ->
    case code:ensure_loaded(M) of
        {module, M} ->
            try apply(M, F, Args)
            catch error:undef:ST ->
                    maybe
                        false ?= erlang:function_exported(M, F, length(Args)),
                        Message = ["Error! ",atom_to_binary(M),":",
                                   atom_to_list(F),"/",integer_to_list(length(Args)),
                                   " is not exported."
                                   "\r\n\r\n"],
                        erlang:display_string(binary_to_list(iolist_to_binary(Message)))
                    end,
                    erlang:raise(error,undef,ST);
                  E:R:ST ->
                    erlang:display({E,R,ST}),
                    erlang:raise(E,R,ST)
            end;
        {error, Reason} ->
            Message = [explain_ensure_loaded_error(M, Reason), <<"\r\n\r\n">>],
            erlang:display_string(binary_to_list(iolist_to_binary(Message))),
            erlang:error(undef)
    end.

explain_ensure_loaded_error(M, badfile) ->
    S = [<<"it requires a more recent Erlang/OTP version "
           "or its .beam file was corrupted.\r\n"
           "(You are running Erlang/OTP ">>,
         erlang:system_info(otp_release), <<".)">>],
    explain_add_head(M, S);
explain_ensure_loaded_error(M, nofile) ->
    S = <<"it cannot be found.\r\n",
          "Make sure that the module name is correct and that its .beam file\r\n",
          "is in the code path.">>,
    explain_add_head(M, S);
explain_ensure_loaded_error(M, Other) ->
    [<<"Error! Failed to load module '", (atom_to_binary(M))/binary,
       "'. Reason: ">>,
     atom_to_binary(Other)].

explain_add_head(M, S) ->
    [<<"Error! Failed to load module '", (atom_to_binary(M))/binary,
       "' because ">>, S].

%% Load a module.

do_load_module(Mod, BinCode) ->
    case erlang:load_module(Mod, BinCode) of
	{module,Mod} ->
	    ok;
	{error,on_load} ->
	    ?ON_LOAD_HANDLER ! {loaded,Mod},
	    ok;
	_ ->
	    error
    end.

%% --------------------------------------------------------
%% If -shutdown_time is specified at the command line
%% this timer will inform the init process that it has to
%% force processes to terminate. It cannot be handled
%% softly any longer.
%% --------------------------------------------------------

shutdown_timer(Flags) ->
    case get_flag(shutdown_time, Flags, infinity) of
	infinity ->
	    self();
	Time ->
	    case catch list_to_integer(binary_to_list(Time)) of
		T when is_integer(T) ->
		    Pid = spawn(fun() -> timer(T) end),
		    receive
			{Pid, started} ->
			    Pid
		    end;
		_ ->
		    self()
	    end
    end.
    
flush_timout(Pid) ->
    receive
	{Pid, timeout} -> true
    after 0            -> true
    end.

timer(T) ->
    init ! {self(), started},
    receive
    after T ->
	init ! {self(), timeout}
    end.
    
%% --------------------------------------------------------
%% Parse the command line arguments and extract things to start, flags
%% and other arguments. We keep the relative of the groups.
%% Returns a triplet in the form `{Start, Flags, Args}':
%% --------------------------------------------------------

parse_boot_args(Args) ->
    parse_boot_args(Args, [], [], []).

parse_boot_args([B|Bs], Ss, Fs, As) ->
    case check(B, Bs) of
	start_extra_arg ->
	    {reverse(Ss),reverse(Fs),lists:reverse(As, Bs)}; % BIF
	start_arg ->
	    {S,Rest} = get_args(Bs, []),
            Instructions = run_args_to_start_instructions(S, fun bs2as/1),
            parse_boot_args(Rest, Instructions ++ Ss, Fs, As);
	start_arg2 ->
	    {S,Rest} = get_args(Bs, []),
            Instructions = run_args_to_start_instructions(S, fun bs2ss/1),
            parse_boot_args(Rest, Instructions ++ Ss, Fs, As);
	ending_start_arg ->
            {S,Rest} = get_args(Bs, []),
            %% Forward any additional arguments to the function we are calling,
            %% such that no init:get_plain_arguments is needed by it later.
            MFA = run_args_to_mfa(S ++ Rest),
            {M, F, [Args]} = interpolate_empty_mfa_args(MFA),
            StartersWithThis = [{apply, M, F,
                                 %% erlexec escapes and -- passed after -S
                                 %% so we un-escape it
                                 [map(fun("\\--") -> "--";
                                         (A) -> A
                                      end, map(fun b2s/1, Args))]} | Ss],
            {reverse(StartersWithThis),reverse(Fs),reverse(As)};
	eval_arg ->
	    {Expr,Rest} = get_args(Bs, []),
            parse_boot_args(Rest, [{eval, fold_eval_args(Expr)} | Ss], Fs, As);
	{flag,A} ->
	    {F,Rest} = get_args(Bs, []),
	    Fl = {A,F},
	    parse_boot_args(Rest, Ss, [Fl|Fs], As);
	arg ->
	    parse_boot_args(Bs, Ss, Fs, [B|As]);
	end_args ->
	    parse_boot_args(Bs, Ss, Fs, As)
    end;
parse_boot_args([], Start, Flags, Args) ->
    {reverse(Start),reverse(Flags),reverse(Args)}.

check(<<"-extra">>, _Bs) ->
    start_extra_arg;
check(<<"-s">>, _Bs) -> start_arg;
check(<<"-run">>, _Bs) -> start_arg2;
check(<<"-S">>, Bs) ->
    case has_end_args(Bs) of
        true ->
            {flag, b2a(<<"S">>)};
        false ->
            ending_start_arg
    end;
check(<<"-eval">>, _Bs) -> eval_arg;
check(<<"--">>, _Bs) -> end_args;
check(<<"-",Flag/binary>>, _Bs) -> {flag,b2a(Flag)};
check(_,_) -> arg.

has_end_args([<<"--">> | _Bs]) ->
    true;
has_end_args([_ | Bs]) ->
    has_end_args(Bs);
has_end_args([]) ->
    false.

get_args([B|Bs], As) ->
    case check(B, Bs) of
	start_extra_arg -> {reverse(As), [B|Bs]};
	start_arg -> {reverse(As), [B|Bs]};
	start_arg2 -> {reverse(As), [B|Bs]};
	eval_arg -> {reverse(As), [B|Bs]};
	end_args -> {reverse(As), Bs};
	ending_start_arg -> {reverse(As), [B|Bs]};
	{flag,_} -> {reverse(As), [B|Bs]};
	arg ->
	    get_args(Bs, [B|As])
    end;
get_args([], As) -> {reverse(As),[]}.

update_flag(Flag, [{Flag, _} | Flags], Value) ->
    [{Flag, [Value]} | Flags];
update_flag(Flag, [Head | Flags], Value) ->
    [Head | update_flag(Flag, Flags, Value)];
update_flag(Flag, [], Value) ->
    [{Flag, [Value]}].

%%
%% Internal get_flag function, with default value.
%% Return: true if flag given without args
%%         atom() if a single arg was given.
%%         list(atom()) if several args were given.
%%
get_flag(F, Flags, Default) ->
    case lists:keyfind(F, 1, Flags) of
	{F,[]} ->
	    true;
	{F,[V]} ->
	    V;
	{F,V} ->
	    V;
	_ ->
	    Default
    end.

%%
%% Internal get_flag function, with default value.
%% Return: list(atom()) 
%%
get_flag_list(F, Flags, Default) ->
    case lists:keyfind(F, 1, Flags) of
	{F,[_|_]=V} ->
	    V;
	_ ->
	    Default
    end.

%%
%% Internal get_flag function.
%% Fetch all occurrences of flag.
%% Return: [Args,Args,...] where Args ::= list(atom())
%%
get_flag_args(F,Flags) -> get_flag_args(F,Flags,[]).

get_flag_args(F,[{F,V}|Flags],Acc) ->
    get_flag_args(F,Flags,[V|Acc]);
get_flag_args(F,[_|Flags],Acc) ->
    get_flag_args(F,Flags,Acc);
get_flag_args(_,[],Acc) ->
    reverse(Acc).

get_arguments([{F,V}|Flags]) ->
    [{F,to_strings(V)}|get_arguments(Flags)];
get_arguments([]) ->
    [].

to_strings([H|T]) when is_atom(H) -> [atom_to_list(H)|to_strings(T)];
to_strings([H|T]) when is_binary(H) -> [b2s(H)|to_strings(T)];
to_strings([])    -> [].

get_argument(Arg, Flags) ->
    case get_argument1(Arg, Flags) of
	[] -> error;
	Value -> {ok,Value}
    end.

get_argument1(Arg, [{Arg,V}|Args]) ->
    [to_strings(V)|get_argument1(Arg, Args)];
get_argument1(Arg, [_|Args]) ->
    get_argument1(Arg, Args);
get_argument1(_, []) ->
    [].

set_argument([{Flag,_}|Flags],Flag,Value) ->
    [{Flag,[Value]}|Flags];
set_argument([Item|Flags],Flag,Value) ->
    [Item|set_argument(Flags,Flag,Value)];
set_argument([],Flag,Value) ->
    [{Flag,[Value]}].

append([E]) -> E;
append([H|T]) ->
    H ++ append(T);
append([]) -> [].

reverse([] = L) ->
    L;
reverse([_] = L) ->
    L;
reverse([A, B]) ->
    [B, A];
reverse([A, B | L]) ->
    lists:reverse(L, [B, A]). % BIF
			
-doc false.
-spec objfile_extension() -> nonempty_string().
objfile_extension() ->
    ".beam".
%%    case erlang:system_info(machine) of
%%      "JAM" -> ".jam";
%%      "VEE" -> ".vee";
%%      "BEAM" -> ".beam"
%%    end.

-doc false.
-spec archive_extension() -> nonempty_string().
archive_extension() ->
    ".ez".

%%%
%%% Support for handling of on_load functions.
%%%

-doc false.
run_on_load_handlers() ->
    Ref = monitor(process, ?ON_LOAD_HANDLER),
    _ = catch ?ON_LOAD_HANDLER ! {run_on_load, Ref},
    receive
	{'DOWN',Ref,process,_,noproc} ->
	    %% There is no on_load handler process,
	    %% probably because init:restart/0 has been
	    %% called and it is not the first time we
	    %% pass through here.
	    ok;
	{'DOWN',Ref,process,_,Ref} ->
            %% All on_load handlers have run succesfully
	    ok;
	{'DOWN',Ref,process,_,Reason} ->
	    %% Failure to run an on_load handler.
	    %% This is fatal during start-up.
	    exit(Reason)
    end.

start_on_load_handler_process() ->
    register(?ON_LOAD_HANDLER,
	     spawn(fun on_load_handler_init/0)).

on_load_handler_init() ->
    on_load_loop([], false).

on_load_loop(Mods, Debug0) ->
    receive
	{init_debug_flag,Debug} ->
	    on_load_loop(Mods, Debug);
	{loaded,Mod} ->
	    on_load_loop([Mod|Mods], Debug0);
	{run_on_load, Ref} ->
	    run_on_load_handlers(Mods, Debug0),
	    exit(Ref)
    end.

run_on_load_handlers([M|Ms], Debug) ->
    debug(Debug,
          {running_on_load_handler,M},
          fun() -> run_on_load_handler(M, Debug) end),
    run_on_load_handlers(Ms, Debug);
run_on_load_handlers([], _) -> ok.

run_on_load_handler(M, Debug) ->
    Fun = fun() ->
                  Res = erlang:call_on_load_function(M),
                  exit(Res)
          end,
    {Pid,Ref} = spawn_monitor(Fun),
    receive
        {'DOWN',Ref,process,Pid,OnLoadRes} ->
            Keep = OnLoadRes =:= ok,
            erlang:finish_after_on_load(M, Keep),
            case Keep of
                false ->
                    Error = {on_load_function_failed,M,OnLoadRes},
                    debug(Debug, Error),
                    exit(Error);
                true ->
                    debug(Debug, {on_load_handler_returned_ok,M})
            end
    end.

%% debug profile (light variant of eprof)
debug_profile_start() ->
    _ = erlang:trace_pattern({'_','_','_'},true,[call_time]),
    _ = erlang:trace_pattern(on_load,true,[call_time]),
    _ = erlang:trace(all,true,[call]),
    ok.

debug_profile_stop() ->
    _ = erlang:trace_pattern({'_','_','_'},false,[call_time]),
    _ = erlang:trace_pattern(on_load,false,[call_time]),
    _ = erlang:trace(all,false,[call]),
    ok.

debug_profile_mfas() ->
    _ = erlang:trace_pattern({'_','_','_'},pause,[call_time]),
    _ = erlang:trace_pattern(on_load,pause,[call_time]),
    MFAs = collect_loaded_mfas() ++ erlang:system_info(snifs),
    collect_mfas(MFAs,[]).

%% debug_profile_format_mfas should be called at the end of the boot phase
%% so all pertinent modules should be loaded at that point.
debug_profile_format_mfas(MFAs0) ->
    MFAs = lists:sort(MFAs0),
    lists:foreach(fun({{Us,C},{M,F,A}}) ->
                          Str = io_lib:format("~w:~w/~w", [M,F,A]),
                          io:format(standard_error,"~55s - ~6w : ~w us~n", [Str,C,Us])
                  end, MFAs),
    ok.

collect_loaded_mfas() ->
    Ms = [M || M <- [element(1, Mi) || Mi <- code:all_loaded()]],
    collect_loaded_mfas(Ms,[]).

collect_loaded_mfas([],MFAs) -> MFAs;
collect_loaded_mfas([M|Ms],MFAs0) ->
    MFAs = [{M,F,A} || {F,A} <- M:module_info(functions)],
    collect_loaded_mfas(Ms,MFAs ++ MFAs0).


collect_mfas([], Info) -> Info;
collect_mfas([MFA|MFAs],Info) ->
    case erlang:trace_info(MFA,call_time) of
        {call_time, []} ->
            collect_mfas(MFAs,Info);
        {call_time, false} ->
            collect_mfas(MFAs,Info);
        {call_time, undefined} ->
            collect_mfas(MFAs,Info);
        {call_time, Data} ->
            case collect_mfa(MFA,Data,0,0) of
                {{0,_},_} ->
                    %% ignore mfas with zero time
                    collect_mfas(MFAs,Info);
                MfaData ->
                    collect_mfas(MFAs,[MfaData|Info])
            end
    end.

collect_mfa(Mfa,[],Count,Time) -> {{Time,Count},Mfa};
collect_mfa(Mfa,[{_Pid,C,S,Us}|Data],Count,Time) ->
    collect_mfa(Mfa,Data,Count + C,Time + S * 1000000 + Us).

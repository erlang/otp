%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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

-module(reltool).
-moduledoc """
Main API of the Reltool application

This is an interface module for the Reltool application.

_Reltool_ is a release management tool. It analyses a given Erlang/OTP
installation and determines various dependencies between applications. The
`graphical` frontend depicts the dependencies and enables interactive
customization of a target system. The backend provides a `batch` interface for
generation of customized target systems.

The tool uses an installed Erlang/OTP system as input. `root_dir` is the root
directory of the analysed system and it defaults to the system executing
Reltool. Applications may also be located outside `root_dir`. `lib_dirs` defines
library directories where additional applications may reside and it defaults to
the directories listed by the operating system environment variable `ERL_LIBS`.
See the module `code` for more info.

An application directory `AppDir` under a library directory is recognized by the
existence of an `AppDir/ebin` directory. If this does not exist, Reltool will
not consider `AppDir` at all when looking for applications.

It is recommended that application directories are named as the application,
possibly followed by a dash and the version number. For example `myapp` or
`myapp-1.1`.

Finally single modules and entire applications may be read from Escripts.

Some configuration parameters control the behavior of Reltool on system (`sys`)
level. Others provide control on application (`app`) level and yet others are on
module (`mod`) level. Module level parameters override application level
parameters and application level parameters override system level parameters.
Escript `escript` level parameters override system level parameters.

The following top level `options` are supported:

- **`config`** - This is the main option and it controls the configuration of
  Reltool. It can either be a `sys` tuple or a name of a `file` containing a sys
  tuple.

- **`trap_exit`** - This option controls the error handling behavior of Reltool.
  By default the window processes traps exit, but this behavior can altered by
  setting `trap_exit` to `false`.

- **`wx_debug`** - This option controls the debug level of `wx`. As its name
  indicates it is only useful for debugging. See `wx:debug/1` for more info.

Besides the already mentioned source parameters `root_dir` and `lib_dirs`, the
following system (`sys`) level options are supported:

- **`erts`** - Erts specific configuration. See application level options below.

- **`escript`** - Escript specific configuration. An escript has a mandatory
  file name and escript level options that are described below.

- **`app`** - Application specific configuration. An application has a mandatory
  name and application level options that are described below.

- **`mod_cond`** - This parameter controls the module inclusion policy. It
  defaults to `all` which means that if an application is included (either
  explicitly or implicitly) all modules in that application will be included.
  This implies that both modules that exist in the `ebin` directory of the
  application, as well as modules that are named in the `app` file will be
  included. If the parameter is set to `ebin`, both modules in the `ebin`
  directory and derived modules are included. If the parameter is set to `app`,
  both modules in the `app` file and derived modules are included. `derived`
  means that only modules that are used by other included modules are included.
  The `mod_cond` setting on system level is used as default for all
  applications.

- **`incl_cond`** - This parameter controls the application and escript
  inclusion policy. It defaults to `derived` which means that the applications
  that do not have any explicit `incl_cond` setting, will only be included if
  any other (explicitly or implicitly included) application uses it. The value
  `include` implies that all applications and escripts that do not have any
  explicit `incl_cond` setting will be included. `exclude` implies that all
  applications and escripts that do not have any explicit `incl_cond` setting
  will be excluded.

- **`boot_rel`** - A target system may have several releases but the one given
  as `boot_rel` will be used as default when the system is booting up.

- **`rel`** - Release specific configuration. Each release maps to a `rel`,
  `script` and `boot` file. See the module `systools` for more info about the
  details. Each release has a name, a version and a set of applications with a
  few release specific parameters such as type and included applications.

- **`relocatable`** - This parameter controls whether the `erl` executable in
  the target system should automatically determine where it is installed or if
  it should use a hardcoded path to the installation. In the latter case the
  target system must be installed with `reltool:install/2` before it can be
  used. If the system is relocatable, the file tree containing the target system
  can be moved to another location without re-installation. The default is
  `true`.

- **`profile`** - The creation of the specification for a target system is
  performed in two steps. In the first step a complete specification is
  generated. It will likely contain much more files than you are interested in
  in your customized target system. In the second step the specification will be
  filtered according to your filters. There you have the ability to specify
  filters per application as well as system wide filters. You can also select a
  `profile` for your system. Depending on the `profile`, different default
  filters will be used. There are three different profiles to choose from:
  `development`, `embedded` and `standalone`. `development` is default. The
  parameters that are affected by the `profile` are: `incl_sys_filters`,
  `excl_sys_filters`, `incl_app_filters` and `excl_app_filters`.

- **`app_file`** - This parameter controls the default handling of the `app`
  files when a target system is generated. It defaults to `keep` which means
  that `app` files are copied to the target system and their contents are kept
  as they are. `strip` means that a new `app` file is generated from the
  contents of the original `app` file where the non included modules are removed
  from the file. `all` does also imply that a new `app` file is generated from
  the contents of the original `app` file, with the difference that all included
  modules are added to the file. If the application does not have any `app` file
  a file will be created for `all` but not for `keep` and `strip`.

- **`debug_info`** - The `debug_info` parameter controls what debug information
  in the beam file should be kept or stripped. `keep` keeps all debug info,
  `strip` strips all debug info, and a list of chunkids keeps only those chunks.

- **`excl_lib`**

  > #### Warning {: .warning }
  > This option is experimental.

  If the `excl_lib` option is set to `otp_root` then reltool will not copy
  anything from the Erlang/OTP installation ($OTPROOT) into the target
  structure. The goal is to create a "slim" release which can be used together
  with an existing Erlang/OTP installation. The target structure will therefore
  only contain a `lib` directory with the applications that were found outside
  of $OTPROOT (typically your own applications), and a `releases` directory with
  the generated `.rel,` `.script` and `.boot` files.

  When starting this release, three things must be specified:

  - **_Which `releases` directory to use_** - Tell the release handler to use
    the `releases` directory in our target structure instead of
    `$OTPROOT/releases`. This is done by setting the SASL environment variable
    `releases_dir`, either from the command line
    (`-sasl releases_dir <target-dir>/releases`) or in `sys.config`.

  - **_Which boot file to use_** - The default boot file is
    `$OTPROOT/bin/start`, but in this case we need to specify a boot file from
    our target structure, typically `<target-dir>/releases/<vsn>/<RelName>`.
    This is done with the `-boot` command line option to `erl`

  - **_The location of our applications_** - The generated .script (and .boot)
    file uses the environment variable `$RELTOOL_EXT_LIB` as prefix for the
    paths to all applications. The `-boot_var` option to `erl` can be used for
    specifying the value of this variable, typically
    `-boot_var RELTOOL_EXT_LIB <target-dir>/lib`.

  Example:

  ```text
  erl -sasl releases_dir \"mytarget/releases\" -boot mytarget/releases/1.0/myrel\
   -boot_var RELTOOL_EXT_LIB mytarget/lib
  ```

- **`incl_sys_filters`** - This parameter normally contains a list of regular
  expressions that controls which files in the system should be included. Each
  file in the target system must match at least one of the listed regular
  expressions in order to be included. Further the files may not match any
  filter in `excl_sys_filters` in order to be included. Which application files
  should be included is controlled with the parameters `incl_app_filters` and
  `excl_app_filters`. This parameter defaults to `[".*"]`.

- **`excl_sys_filters`** - This parameter normally contains a list of regular
  expressions that controls which files in the system should not be included in
  the target system. In order to be included, a file must match some filter in
  `incl_sys_filters` but not any filter in `excl_sys_filters`. This parameter
  defaults to `[]`.

- **`incl_app_filters`** - This parameter normally contains a list of regular
  expressions that controls which application specific files that should be
  included. Each file in the application must match at least one of the listed
  regular expressions in order to be included. Further the files may not match
  any filter in `excl_app_filters` in order to be included. This parameter
  defaults to `[".*"]`.

- **`excl_app_filters`** - This parameter normally contains a list of regular
  expressions that controls which application specific files should not be
  included in the target system. In order to be included, a file must match some
  filter in `incl_app_filters` but not any filter in `excl_app_filters`. This
  parameter defaults to `[]`.

On application (`escript`) level, the following options are supported:

- **`incl_cond`** - The value of this parameter overrides the parameter with the
  same name on system level.

On application (`app`) level, the following options are supported:

- **`vsn`** - The version of the application. In an installed system there may
  exist several versions of an application. The `vsn` parameter controls which
  version of the application will be chosen.

  This parameter is mutual exclusive with `lib_dir`. If `vsn` and `lib_dir` are
  both omitted, the latest version will be chosen.

  Note that in order for reltool to sort application versions and thereby be
  able to select the latest, it is required that the version id for the
  application consists of integers and dots only, for example `1`, `2.0` or
  `3.17.1`.

- **`lib_dir`** - The directory to read the application from. This parameter can
  be used to point out a specific location to fetch the application from. This
  is useful for instance if the parent directory for some reason is no good as a
  library directory on system level.

  This parameter is mutual exclusive with `vsn`. If `vsn` and `lib_dir` are both
  omitted, the latest version will be chosen.

  Note that in order for reltool to sort application versions and thereby be
  able to select the latest, it is required that the version id for the
  application consists of integers and dots only, for example `1`, `2.0` or
  `3.17.1`.

- **`mod`** - Module specific configuration. A module has a mandatory name and
  module level options that are described below.

- **`mod_cond`** - The value of this parameter overrides the parameter with the
  same name on system level.

- **`incl_cond`** - The value of this parameter overrides the parameter with the
  same name on system level.

- **`app_file`** - The value of this parameter overrides the parameter with the
  same name on system level.

- **`debug_info`** - The value of this parameter overrides the parameter with
  the same name on system level.

- **`incl_app_filters`** - The value of this parameter overrides the parameter
  with the same name on system level.

- **`excl_app_filters`** - The value of this parameter overrides the parameter
  with the same name on system level.

On module (`mod`) level, the following options are supported:

- **`incl_cond`** - This parameter controls whether the module is included or
  not. By default the `mod_cond` parameter on application and system level will
  be used to control whether the module is included or not. The value of
  `incl_cond` overrides the module inclusion policy. `include` implies that the
  module is included, while `exclude` implies that the module is not included.
  `derived` implies that the module is included if it is used by any other
  included module.

- **`debug_info`** - The value of this parameter overrides the parameter with
  the same name on application level.
""".

%% Public
-export([
         start/0, start/1, start_link/1, debug/0, % GUI
         start_server/1, get_server/1, get_status/1, stop/1,
         get_config/1, get_config/3, get_rel/2, get_script/2,
         create_target/2, get_target_spec/1, eval_target_spec/3,
         install/2
        ]).

-include("reltool.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Start main window process
-doc """
Start a main window process with default options
""".
-spec start() -> {ok, window_pid()} | {error, reason()}.
start() ->
    start([]).

%% Start main window process
-doc """
Start a main window process with options
""".
-spec start(options()) -> {ok, window_pid()} | {error, reason()}.
start(Options) when is_list(Options) ->
    case start_link(Options) of
	{ok, WinPid} = OK ->
	    unlink(WinPid),
	    OK;
	{error, _Reason} = Error ->
	    Error
    end.

%% Start main window process with wx debugging enabled
-doc false.
-spec debug() -> {ok, window_pid()} | {error, reason()}.
debug() ->
    start([{wx_debug, 2}]).

%% Start main window process with options
-doc """
Start a main window process with options. The process is linked.
""".
-spec start_link(options()) -> {ok, window_pid()} | {error, reason()}.
start_link(Options) when is_list(Options) ->
    case reltool_sys_win:start_link(Options) of
        {ok, _WinPid} = OK ->
            OK;
        {error, Reason} ->
            {error, Reason}
    end.

%% Start server process with options
-doc """
Start a server process with options. The server process identity can be given as
an argument to several other functions in the API.
""".
-spec start_server(options()) -> {ok, server_pid()} | {error, reason()}.
start_server(Options) ->
    case reltool_server:start_link(Options) of
        {ok, ServerPid, _Common, _Sys} ->
            {ok, ServerPid};
        {error, Reason} ->
            {error, Reason}
    end.

%% Start server process with options
-doc """
Return the process identifier of the server process.
""".
-spec get_server(window_pid()) -> {ok, server_pid()} | {error, reason()}.
get_server(WinPid) ->
    case reltool_sys_win:get_server(WinPid) of
        {ok, _ServerPid} = OK ->
            OK;
        {error, Reason} ->
            {error, lists:flatten(io_lib:format("~tp", [Reason]))}
    end.

%% Stop a server or window process
-doc """
Stop a server or window process
""".
-spec stop(server_pid() | window_pid()) -> ok | {error, reason()}.
stop(Pid) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    unlink(Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, _, _, shutdown} ->
            ok;
        {'DOWN', Ref, _, _, Reason} ->
            {error, lists:flatten(io_lib:format("~tp", [Reason]))}
    end.

%% Internal library function
-spec eval_server(server(), boolean(), fun((server_pid()) -> Ret)) ->
	 Ret | {error, reason()} when Ret :: term().
eval_server(Pid, _DisplayWarnings, Fun)
  when is_pid(Pid) ->
    Fun(Pid);
eval_server(Options, DisplayWarnings, Fun)
  when is_list(Options) ->
    TrapExit = process_flag(trap_exit, true),
    Res = case start_server(Options) of
	      {ok, Pid} ->
		  apply_fun(Pid, DisplayWarnings, Fun);
	      {error, _Reason} = Error ->
		  Error
	  end,
    process_flag(trap_exit, TrapExit),
    Res.

apply_fun(Pid, false, Fun) ->
    Res = Fun(Pid),
    stop(Pid),
    Res;
apply_fun(Pid, true, Fun) ->
    case get_status(Pid) of
	{ok, Warnings} ->
	    [io:format("~w: ~ts\n", [?APPLICATION, W]) || W <- Warnings],
	    apply_fun(Pid, false, Fun);
	{error, _Reason} = Error ->
	    stop(Pid),
	    Error
    end.

%% Get status about the configuration
-type warning() :: string().
-doc """
Get status about the configuration
""".
-doc(#{since => <<"OTP R14B">>}).
-spec get_status(server()) -> {ok, [warning()]} | {error, reason()}.
get_status(PidOrOptions)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
    eval_server(PidOrOptions, false,
		fun(Pid) -> reltool_server:get_status(Pid) end).

%% Get reltool configuration
-doc(#{ equiv => get_config(Server,false,false) }).
-spec get_config(server()) -> {ok, config()} | {error, reason()}.
get_config(PidOrOption) ->
    get_config(PidOrOption, false, false).

-doc """
Get reltool configuration.

Normally, only the explicit configuration parameters with values that differ from
their defaults are interesting. But the builtin default values can be returned by
setting `InclDefaults` to `true`. The derived configuration can be returned by
setting `InclDerived` to `true`.
""".
-spec get_config(server(), incl_defaults(), incl_derived()) ->
			{ok, config()} | {error, reason()}.
get_config(PidOrOptions, InclDef, InclDeriv)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
    eval_server(PidOrOptions, true,
		fun(Pid) ->
			reltool_server:get_config(Pid, InclDef, InclDeriv)
		end).

%% Get contents of release file
-doc """
Get contents of a release file. See [`rel`](`e:sasl:rel.md`) for more details.
""".
-spec get_rel(server(), rel_name()) -> {ok, rel_file()} | {error, reason()}.
get_rel(PidOrOptions, RelName)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
    eval_server(PidOrOptions, true,
		fun(Pid) -> reltool_server:get_rel(Pid, RelName) end).

%% Get contents of boot script file
-doc """
Get contents of a boot script file. See [`script`](`e:sasl:script.md`) for more details.
""".
-spec get_script(server(), rel_name()) ->
			{ok, script_file()} | {error, reason()}.
get_script(PidOrOptions, RelName)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
   eval_server(PidOrOptions, true,
	       fun(Pid) -> reltool_server:get_script(Pid, RelName) end).

%% Generate a target system
-doc """
Create a target system.

Gives the same result as `{ok,TargetSpec}=reltool:get_target_spec(Server)` and
`reltool:eval_target_spec(TargetSpec,RootDir,TargetDir)`.
""".
-spec create_target(server(), target_dir()) -> ok | {error, reason()}.
create_target(PidOrOptions, TargetDir)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
    eval_server(PidOrOptions, true,
		fun(Pid) -> reltool_server:gen_target(Pid, TargetDir) end).

%% Generate a target system
-doc """
Return a specification of the target system. The actual target system can be
created with `reltool:eval_target_spec/3`.
""".
-spec get_target_spec(server()) -> {ok, target_spec()} | {error, reason()}.
get_target_spec(PidOrOptions)
  when is_pid(PidOrOptions); is_list(PidOrOptions) ->
    eval_server(PidOrOptions, true,
		fun(Pid) -> reltool_server:gen_spec(Pid) end).

%% Generate a target system
-doc """
Create the actual target system from a specification generated by
`reltool:get_target_spec/1`.

The creation of the specification for a target system is performed in two steps.
In the first step a complete specification will be generated. It will likely contain
much more files than you are interested in your target system. In the second
step the specification will be filtered according to your filters. There you have the
ability to specify filters per application as well as system wide filters. You can
also select a `profile` for your system. Depending on the `profile`, different
default filters will be used.

The top directories `bin`, `releases` and `lib` are treated differently from
other files. All other files are by default copied to the target system. The
`releases` directory contains generated `rel`, `script`, and `boot` files. The
`lib` directory contains the applications. Which applications are included and
if they should be customized (stripped from debug info etc.) is specified with
various configuration parameters. The files in the `bin` directory are copied
from the `erts-vsn/bin` directory, but only those files that were originally
included in the `bin` directory of the source system.

If the configuration parameter `relocatable` was set to `true` there is no need
to install the target system with `reltool:install/2` before it can be started.
In that case the file tree containing the target system can be moved without
re-installation.

In most cases, the `RootDir` parameter should be set to the same as the
`root_dir` configuration parameter used in the call to
`reltool:get_target_spec/1` (or `code:root_dir/0` if the configuration parameter
is not set). In some cases it might be useful to evaluate the same target
specification towards different root directories. This should, however, be used
with great care as it requires equivalent file structures under all roots.
""".
-spec eval_target_spec(target_spec(), root_dir(), target_dir()) ->
			      ok | {error, reason()}.
eval_target_spec(Spec, SourceDir, TargetDir)
  when is_list(SourceDir), is_list(TargetDir) ->
    reltool_target:eval_spec(Spec, SourceDir, TargetDir).

%% Install a target system
-doc """
Install a created target system
""".
-spec install(rel_name(), dir()) -> ok | {error, reason()}.
install(RelName, TargetDir) ->
    reltool_target:install(RelName, TargetDir).

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-module(release_handler).
-moduledoc """
Unpacking and Installation of Release Packages

The _release handler_ process belongs to the SASL application, which is
responsible for _release handling_, that is, unpacking, installation, and
removal of release packages.

An introduction to release handling and an example is provided by
[Release Handling section in OTP Design Principles](`e:system:release_handling.md`).

A _release package_ is a compressed tar file containing code for a certain
version of a release, created by calling
[`systools:make_tar/1,2`](`systools:make_tar/1`). The release package is to be
located in the `$ROOT/releases` directory of the previous version of the
release, where `$ROOT` is the installation root directory,
[`code:root_dir()`](`code:root_dir/0`). Another `releases` directory can be
specified using the SASL configuration parameter `releases_dir` or the OS
environment variable `RELDIR`. The release handler must have write access to
this directory to install the new release. The persistent state of the release
handler is stored there in a file called `RELEASES`.

A release package is always to contain:

- A release resource file, `Name.rel`
- A boot script, `Name.boot`

The `.rel` file contains information about the release: its name, version, and
which ERTS and application versions it uses.

A release package can also contain:

- A release upgrade file, `relup`
- A system configuration file, `sys.config`
- A system configuration source file, `sys.config.src`

The `relup` file contains instructions for how to upgrade to, or downgrade from,
this version of the release.

The release package can be _unpacked_, which extracts the files. An unpacked
release can be _installed_. The currently used version of the release is then
upgraded or downgraded to the specified version by evaluating the instructions
in the `relup` file. An installed release can be made _permanent_. Only one
permanent release can exist in the system, and this release is used if the
system is restarted. An installed release, except the permanent one, can be
_removed_. When a release is removed, all files belonging to that release only
are deleted.

Each release version has a status, which can be `unpacked`, `current`,
`permanent`, or `old`. There is always one latest release, which either has
status `permanent` (normal case) or `current` (installed, but not yet made
permanent). The meaning of the status values are illustrated in the following
table:

```text
        Status     Action                NextStatus
        -------------------------------------------
        -          unpack                unpacked
        unpacked   install               current
                   remove                -
        current    make_permanent        permanent
                   install other         old
                   remove                -
        permanent  make other permanent  old
                   install               permanent
        old        reboot_old            permanent
                   install               current
                   remove                -
```

The release handler process is a locally registered process on each node. When a
release is installed in a distributed system, the release handler on each node
must be called. The release installation can be synchronized between nodes. From
an operator view, it can be unsatisfactory to specify each node. The aim is to
install one release package in the system, no matter how many nodes there are.
It is recommended that software management functions are written that take care
of this problem. Such a function can have knowledge of the system architecture,
so it can contact each individual release handler to install the package.

For release handling to work properly, the runtime system must know which
release it is running. It must also be able to change (in runtime) which boot
script and system configuration file are to be used if the system is restarted.
This is taken care of automatically if Erlang is started as an embedded system.
Read about this in [Embedded System](`e:system:index.html`) in _System
Documentation_. In this case, the system configuration file `sys.config` is
mandatory.

The installation of a new release can restart the system. Which program to use
is specified by the SASL configuration parameter `start_prg`, which defaults to
`$ROOT/bin/start`.

The emulator restart on Windows NT expects that the system is started using the
`erlsrv` program (as a service). Furthermore, the release handler expects that
the service is named `NodeName`\_`Release`, where `NodeName` is the first part
of the Erlang node name (up to, but not including the "@") and `Release` is the
current release version. The release handler furthermore expects that a program
like `start_erl.exe` is specified as "machine" to `erlsrv`. During upgrading
with restart, a new service is registered and started. The new service is set to
automatic and the old service is removed when the new release is made permanent.

The release handler at a node running on a diskless machine, or with a read-only
file system, must be configured accordingly using the following SASL
configuration parameters (for details, see [sasl(6)](sasl_app.md)):

- **`masters`** - This node uses some master nodes to store and fetch release
  information. All master nodes must be operational whenever release information
  is written by this node.

- **`client_directory`** - The `client_directory` in the directory structure of
  the master nodes must be specified.

- **`static_emulator`** - This parameter specifies if the Erlang emulator is
  statically installed at the client node. A node with a static emulator cannot
  dynamically switch to a new emulator, as the executable files are statically
  written into memory.

The release handler can also be used to unpack and install release packages when
not running Erlang as an embedded system. However, in this case the user must
somehow ensure that correct boot scripts and configuration files are used if the
system must be restarted.

Functions are provided for using another file structure than the structure
defined in OTP. These functions can be used to test a release upgrade locally.

## Typical Error Reasons

- **`{bad_masters, Masters}`** - The master nodes `Masters` are not alive.

- **`{bad_rel_file, File}`** - Specified `.rel` file `File` cannot be read or
  does not contain a single term.

- **`{bad_rel_data, Data}`** - Specified `.rel` file does not contain a
  recognized release specification, but another term `Data`.

- **`{bad_relup_file, File}`** - Specified `relup` file `Relup` contains bad
  data.

- **`{cannot_extract_file, Name, Reason}`** - Problems when extracting from a
  tar file, `erl_tar:extract/2` returned `{error, {Name, Reason}}`.

- **`{existing_release, Vsn}`** - Specified release version `Vsn` is already in
  use.

- **`{Master, Reason, When}`** - Some operation, indicated by the term `When`,
  failed on the master node `Master` with the specified error reason `Reason`.

- **`{no_matching_relup, Vsn, CurrentVsn}`** - Cannot find a script for
  upgrading/downgrading between `CurrentVsn` and `Vsn`.

- **`{no_such_directory, Path}`** - The directory `Path`does not exist.

- **`{no_such_file, Path}`** - The path `Path` (file or directory) does not
  exist.

- **`{no_such_file, {Master, Path}}`** - The path `Path` (file or directory)
  does not exist at the master node `Master`.

- **`{no_such_release, Vsn}`** - The specified release version `Vsn` does not
  exist.

- **`{not_a_directory, Path}`** - `Path` exists but is not a directory.

- **`{Posix, File}`** - Some file operation failed for `File`. `Posix` is an
  atom named from the Posix error codes, such as `enoent`, `eacces`, or
  `eisdir`. See `m:file` in Kernel.

- **`Posix`** - Some file operation failed, as for the previous item in the
  list.

## Application Upgrade/Downgrade

The functions in the [Application Upgrade/Downgrade](#application-upgrade-downgrade)
section can be used to test upgrade and downgrade of single applications
(instead of upgrading/downgrading an entire release). A script corresponding to
the instructions in the relup file is created on-the-fly, based on the .appup
file for the application, and evaluated exactly in the same way as
release_handler does.

> #### Warning {: .warning }
>
> These functions are primarily intended for simplified testing of .appup files.
> They are not run within the context of the release_handler process.
> They must therefore not be used together with calls to install_release/1,2,
> as this causes the release_handler to end up in an inconsistent state.
>
> No persistent information is updated, so these functions can be used on ay
> Erlang node, embedded or not. Also, using these functions does not affect which
> code is loaded if there is a reboot.
>
> If the upgrade or downgrade fails, the application can end up in an
> inconsistent state.

## See Also

[OTP Design Principles](`e:system:index.html`),
[`config`](`e:kernel:config.md`), [`rel`](rel.md), [`relup`](relup.md),
[`script`](script.md), `m:sys`, `m:systools`
""".
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

%% External exports
-export([start_link/0,
	 create_RELEASES/1, create_RELEASES/2, create_RELEASES/3, create_RELEASES/4,
	 unpack_release/1,
	 check_install_release/1, check_install_release/2,
	 install_release/1, install_release/2, new_emulator_upgrade/2,
	 remove_release/1, which_releases/0, which_releases/1,
	 make_permanent/1, reboot_old_release/1,
	 set_unpacked/2, set_removed/1, install_file/2]).
-export([upgrade_app/2, downgrade_app/2, downgrade_app/3,
	 upgrade_script/2, downgrade_script/3,
	 eval_appup_script/4]).

%% Internal exports
-export([init/1, handle_call/3, handle_info/2, terminate/2,
	 handle_cast/2, code_change/3]).

%% Internal exports, a client release_handler may call this functions.
-export([do_write_release/3, do_copy_file/2, do_copy_files/2,
	 do_copy_files/1, do_rename_files/1, do_remove_files/1,
	 remove_file/1, do_write_file/2, do_write_file/3,
	 do_ensure_RELEASES/1,
         consult/2,
         root_dir_relative_read_file_info/1,
         root_dir_relative_read_file/1,
         root_dir_relative_rename_file/2,
         root_dir_relative_make_dir/1,
         root_dir_relative_ensure_dir/1]).

-record(state, {unpurged = [],
		root,
		rel_dir,
		releases,
		timer,
		start_prg,
		masters = false,
		client_dir = false,
	        static_emulator = false,
		pre_sync_nodes = []}).

%%-----------------------------------------------------------------
%% status      action                next_status
%% =============================================
%%   -         unpack                unpacked
%% unpacked    install               current
%%             remove                -
%% current     make_permanent        permanent
%%             install other         old
%%             restart node          unpacked
%%             remove                -
%% permanent   make other permanent  old
%%             install               permanent
%% old         reboot_old            permanent
%%             install               current
%%             remove                -
%%-----------------------------------------------------------------
%% libs = [{Lib, Vsn, Dir}]
-record(release, {name, vsn, erts_vsn, libs = [], status}).

-define(timeout, 10000).

%%-----------------------------------------------------------------
%% The version set on the temporary release that will be used when the
%% emulator is upgraded.
-define(tmp_vsn(__BaseVsn__), "__new_emulator__"++__BaseVsn__).




%%-----------------------------------------------------------------
%% Assumes the following file structure:
%% root --- lib --- Appl-Vsn1 --- <src>
%%       |       |             |- ebin
%%       |       |             |_ priv
%%       |       |_ Appl-Vsn2
%%       |
%%       |- bin --- start (default; {sasl, start_prg} overrides
%%       |       |- run_erl
%%       |       |- start_erl (reads start_erl.data)
%%       |       |_ <to_erl>
%%       |
%%       |- erts-EVsn1 --- bin --- <jam44>
%%       |                      |- <epmd>
%%       |                      |_ erl
%%       |- erts-EVsn2
%%       |
%%       |- clients --- ClientName1 --- bin -- start
%%         <clients use same lib and erts as master>
%%       |           |               |_ releases --- start_erl.data
%%       |           |                           |_ Vsn1 -- start.boot
%%       |           |_ ClientName2
%%       |
%%       |- clients --- Type1 --- lib
%%         <clients use own lib and erts>
%%       |           |         |- erts-EVsn
%%       |           |         |- bin -- start
%%       |           |         |_ ClientName1 -- releases -- start_erl.data
%%       |           |                                    |_ start.boot (static)
%%       |           |                                    |_ Vsn1
%%       |           |_ Type2 
%%       |
%%       |- releases --- RELEASES
%%       |            |_ <Vsn1.tar.Z>
%%       |            |
%%       |            |- start_erl.data (generated by rh)
%%       |            |
%%       |            |_ Vsn1 --- start.boot
%%       |            |        |- <sys.config>
%%       |            |        |_ relup
%%       |            |_ Vsn2        
%%       |
%%       |- log --- erlang.log.N (1 .. 5)
%%
%% where <Name> means 'for example Name', and root is
%% init:get_argument(root)
%%
%% It is configurable where the start file is located, and what it
%% is called.
%%   The parameter is {sasl, start_prg} = File
%% It is also configurable where the releases directory is located.
%% Default is $ROOT/releases.  $RELDIR overrides, and
%% {sasl, releases_dir} overrides both.
%%-----------------------------------------------------------------
-doc false.
start_link() ->
    gen_server:start_link({local, release_handler}, ?MODULE, [], []).

%%-----------------------------------------------------------------
%% Args: ReleaseName is the name of the package file
%%       (without .tar.Z (.tar on non unix systems))
%% Purpose: Copies all files in the release package to their
%%          directories.  Checks that all required libs and erts
%%          files are present.
%% Returns: {ok, Vsn} | {error, Reason}
%%          Reason = {existing_release, Vsn} |
%%                   {no_such_file, File} |
%%                   {bad_rel_file, RelFile} |
%%                   {file_missing, FileName} |  (in the tar package)
%%                   exit_reason()
%%-----------------------------------------------------------------
-doc """
unpack_release(Name)

Unpacks a release package `Name.tar.gz` located in the `releases` directory.

Performs some checks on the package, for example, checks that all mandatory
files are present, and extracts its contents.
""".
-spec unpack_release(Name) -> {ok, Vsn} | {error, Reason} when Name :: string(),
   Vsn :: string(),
   Reason :: client_node | term().
unpack_release(ReleaseName) ->
    call({unpack_release, ReleaseName}).
    
%%-----------------------------------------------------------------
%% Purpose: Checks the relup script for the specified version.
%%          The release must be unpacked.
%%          Options = [purge] - all old code that can be soft purged
%%          will be purged if all checks succeeds. This can be useful
%%          in order to reduce time needed in the following call to
%%          install_release.
%% Returns: {ok, FromVsn, Descr} | {error, Reason}
%%          Reason = {illegal_option, IllegalOpt} |
%%                   {already_installed, Vsn} |
%%                   {bad_relup_file, RelFile} |
%%                   {no_such_release, Vsn} |
%%                   {no_such_from_vsn, Vsn} |
%%                   exit_reason()
%%-----------------------------------------------------------------
-doc(#{equiv => check_install_release(Vsn, [])}).
-spec check_install_release(Vsn) -> {ok, OtherVsn, Descr} | {error, Reason} when Vsn :: string(),
   OtherVsn :: string(),
   Descr :: term(),
   Reason :: term().
check_install_release(Vsn) ->
    check_install_release(Vsn, []).

-doc """
check_install_release(Vsn, Opts)

Checks if the specified version `Vsn` of the release can be installed.

The release must not have status `current`. Issues warnings if `relup` file or
`sys.config` is not present. If `relup` file is present, its contents are
checked and `{error,Reason}` is returned if an error is found. Also checks that
all required applications are present and that all new code can be loaded;
`{error,Reason}` is returned if an error is found.

Evaluates all instructions that occur before the `point_of_no_return`
instruction in the release upgrade script.

Returns the same as `install_release/1`. `Descr` defaults to "" if no `relup`
file is found.

If option `purge` is specified, all old code that can be soft-purged is purged
after all other checks are successfully completed. This can be useful to reduce
the time needed by `install_release/1`.
""".
-doc(#{since => <<"OTP R14B04">>}).
-spec check_install_release(Vsn,Opts) -> {ok, OtherVsn, Descr} | {error, Reason} when Vsn :: string(),
   OtherVsn :: string(),
   Opts :: [Opt],
   Opt :: purge,
   Descr :: term(),
   Reason :: term().
check_install_release(Vsn, Opts) ->
    case check_check_install_options(Opts, false) of
	{ok,Purge} ->
	    call({check_install_release, Vsn, Purge});
	Error ->
	    Error
    end.

check_check_install_options([purge|Opts], _) ->
    check_check_install_options(Opts, true);
check_check_install_options([Illegal|_],_Purge) ->
    {error,{illegal_option,Illegal}};
check_check_install_options([],Purge) ->
    {ok,Purge}.


%%-----------------------------------------------------------------
%% Purpose: Executes the relup script for the specified version.
%%          The release must be unpacked.
%% Returns: {ok, FromVsn, Descr} |
%%          {continue_after_restart, FromVsn, Descr} |
%%          {error, Reason}
%%          Reason = {already_installed, Vsn} |
%%                   {bad_relup_file, RelFile} |
%%                   {no_such_release, Vsn} |
%%                   {no_such_from_vsn, Vsn} |
%%                   {could_not_create_hybrid_boot,Why} |
%%                   {missing_base_app,Vsn,App} |
%%                   {illegal_option, Opt}} |
%%                   exit_reason()
%%-----------------------------------------------------------------
-doc(#{equiv => install_release(Vsn, [])}).
-spec install_release(Vsn) -> {ok, OtherVsn, Descr} | {error, Reason} when
      Vsn :: string(),
      OtherVsn :: string(),
      Descr :: term(),
      Reason :: {already_installed, Vsn} |
                {change_appl_data, term()} |
                {missing_base_app, OtherVsn, App} |
                {could_not_create_hybrid_boot, term()} |
                term(),
      App :: atom().
install_release(Vsn) ->
    call({install_release, Vsn, restart, []}).


-doc """
install_release(Vsn, [Opt])

Installs the specified version `Vsn` of the release.

Looks first for a `relup` file for `Vsn` and a script
`{UpFromVsn,Descr1,Instructions1}` in this file for upgrading from the
current version. If not found, the function looks for a `relup` file
for the current version and a script `{Vsn,Descr2,Instructions2}` in
this file for downgrading to `Vsn`.

If a script is found, the first thing that happens is that the application
specifications are updated according to the `.app` files and `sys.config`
belonging to the release version `Vsn`.

After the application specifications have been updated, the instructions in the
script are evaluated and the function returns `{ok,OtherVsn,Descr}` if
successful. `OtherVsn` and `Descr` are the version (`UpFromVsn` or `Vsn`) and
description (`Descr1` or `Descr2`) as specified in the script.

If `{continue_after_restart,OtherVsn,Descr}` is returned, the emulator is
restarted before the upgrade instructions are executed. This occurs if the
emulator or any of the applications Kernel, STDLIB, or SASL are updated. The new
emulator version and these core applications execute after the restart. For all
other applications the old versions are started and the upgrade is performed as
normal by executing the upgrade instructions.

If a recoverable error occurs, the function returns `{error,Reason}` and the
original application specifications are restored. If a non-recoverable error
occurs, the system is restarted.

_Options_:

- **`error_action`** - Defines if the node is to be restarted
  ([`init:restart()`](`init:restart/0`)) or rebooted
  ([`init:reboot()`](`init:reboot/0`)) if there is an error during the
  installation. Default is `restart`.

- **`code_change_timeout`** - Defines the time-out for all calls to
  [`sys:change_code`](`sys:change_code/4`). If no value is specified or
  `default` is specified, the default value defined in `sys` is used.

- **`suspend_timeout`** - Defines the time-out for all calls to
  [`sys:suspend`](`sys:suspend/1`). If no value is specified, the values defined
  by the `Timeout` parameter of the `upgrade` or `suspend` instructions are
  used. If `default` is specified, the default value defined in `sys` is used.

- **`{update_paths,Bool}`** - Indicates if all application code paths are to be
  updated (`Bool==true`) or if only code paths for modified applications are to
  be updated (`Bool==false`, default). This option has only effect for other
  application directories than the default `$ROOT/lib/App-Vsn`, that is,
  application directories specified in argument `AppDirs` in a call to
  `create_RELEASES/4` or `set_unpacked/2`.

  _Example:_

  In the current version `CurVsn` of a release, the application directory of
  `myapp` is `$ROOT/lib/myapp-1.0`. A new version `NewVsn` is unpacked outside
  the release handler and the release handler is informed about this with a call
  as follows:

  ```erlang
  release_handler:set_unpacked(RelFile, [{myapp,"1.0","/home/user"},...]).
  => {ok,NewVsn}
  ```

  If `NewVsn` is installed with option `{update_paths,true}`, then
  [`code:lib_dir(myapp)`](`code:lib_dir/1`) returns `/home/user/myapp-1.0`.

> #### Note {: .info }
>
> Installing a new release can be time consuming if there are many processes in
> the system. The reason is that each process must be checked for references to
> old code before a module can be purged. This check can lead to garbage
> collections and copying of data.
>
> To speed up the execution of [`install_release`](`install_release/1`), first
> call [`check_install_release`](`check_install_release/1`), using option
> `purge`. This does the same check for old code. Then purges all modules that
> can be soft-purged. The purged modules do then no longer have any old code,
> and [`install_release`](`install_release/1`) does not need to do the checks.
>
> This does not reduce the overall time for the upgrade, but it allows checks
> and purge to be executed in the background before the real upgrade is started.

> #### Note {: .info }
>
> When upgrading the emulator from a version older than OTP R15, an attempt is
> made to load new application beam code into the old emulator. Sometimes the
> new beam format cannot be read by the old emulator, so the code loading fails
> and the complete upgrade is terminated. To overcome this problem, the new
> application code is to be compiled with the old emulator. For more information
> about emulator upgrade from pre OTP R15 versions, see
> [Design Principles](`e:system:appup_cookbook.md`) in _System Documentation_.
""".
-spec install_release(Vsn, [Opt]) -> {ok, OtherVsn, Descr} | {continue_after_restart, OtherVsn, Descr} | {error, Reason} when
      Vsn :: string(),
      OtherVsn :: string(),
      Opt :: {error_action, Action} | {code_change_timeout, Timeout} | {suspend_timeout, Timeout} | {update_paths, Bool},
      Action :: restart | reboot,
      Timeout :: default | infinity | pos_integer(),
      Bool :: boolean(),
      Descr :: term(),
      Reason :: {illegal_option, Opt} |
                {already_installed, Vsn} |
                {change_appl_data, term()} |
                {missing_base_app, OtherVsn, App} |
                {could_not_create_hybrid_boot, term()} |
                term(),
      App :: atom().
install_release(Vsn, Opt) ->
    case check_install_options(Opt, restart, []) of
	{ok, ErrorAction, InstallOpt} ->
	    call({install_release, Vsn, ErrorAction, InstallOpt});
	Error ->
	    Error
    end.

check_install_options([Opt | Opts], ErrAct, InstOpts) ->
    case install_option(Opt) of
	{error_action, EAct} ->
	    check_install_options(Opts, EAct, InstOpts);
	true ->
	    check_install_options(Opts, ErrAct, [Opt | InstOpts]);
	false ->
	    {error, {illegal_option, Opt}}
    end;
check_install_options([], ErrAct, InstOpts) ->
    {ok, ErrAct, InstOpts}.

install_option(Opt = {error_action, reboot}) -> Opt;
install_option(Opt = {error_action, restart}) -> Opt;
install_option({code_change_timeout, TimeOut}) ->
    check_timeout(TimeOut);
install_option({suspend_timeout, TimeOut}) ->
    check_timeout(TimeOut);
install_option({update_paths, Bool}) when Bool==true; Bool==false ->
    true;
install_option(_Opt) -> false.

check_timeout(infinity) -> true;
check_timeout(Int) when is_integer(Int), Int > 0 -> true;
check_timeout(_Else) -> false.

%%-----------------------------------------------------------------
%% Purpose: Called by boot script after emulator is restarted due to
%%          new erts version.
%% Returns: Same as install_release/2
%%          If this crashes, the emulator restart will fail
%%          (since the function is called from the boot script)
%%          and there will be a rollback.
%%-----------------------------------------------------------------
-doc false.
new_emulator_upgrade(Vsn, Opts) ->
    Result = call({install_release, Vsn, reboot, Opts}),
    error_logger:info_msg(
      "~w:install_release(~p,~p) completed after node restart "
      "with new emulator version~nResult: ~p~n",[?MODULE,Vsn,Opts,Result]),
    Result.

%%-----------------------------------------------------------------
%% Purpose: Makes the specified release version be the one that is
%%          used when the system starts (or restarts).
%%          The release must be installed (not unpacked).
%% Returns: ok | {error, Reason}
%%          Reason = {bad_status, Status} |
%%                   {no_such_release, Vsn} |
%%                   exit_reason()
%%-----------------------------------------------------------------
-doc """
make_permanent(Vsn)

Makes the specified release version `Vsn` permanent.
""".
-spec make_permanent(Vsn) -> ok | {error, Reason}
                        when
                            Vsn :: string(),
                            Reason ::
                                {bad_status, Status :: term()} | term().
make_permanent(Vsn) ->
    call({make_permanent, Vsn}).

%%-----------------------------------------------------------------
%% Purpose: Reboots the system from an old release.
%%-----------------------------------------------------------------
-doc """
reboot_old_release(Vsn)

Reboots the system by making the old release permanent, and calls
[`init:reboot()`](`init:reboot/0`) directly.

The release must have status `old`.
""".
-spec reboot_old_release(Vsn) -> ok | {error, Reason}
                            when
                                Vsn :: string(),
                                Reason ::
                                    {bad_status, Status :: term()} |
                                    term().
reboot_old_release(Vsn) ->
    call({reboot_old_release, Vsn}).

%%-----------------------------------------------------------------
%% Purpose: Deletes all files and directories used by the release
%%          version, that are not used by any other release.
%%          The release must not be permanent.
%% Returns: ok | {error, Reason}
%%          Reason = {permanent, Vsn} |
%%-----------------------------------------------------------------
-doc """
remove_release(Vsn)

Removes a release and its files from the system.

The release must not be the permanent release. Removes only the files
and directories not in use by another release.
""".
-spec remove_release(Vsn) -> ok | {error, Reason} when Vsn :: string(),
   Reason :: {permanent, Vsn} | client_node | term().
remove_release(Vsn) ->
    call({remove_release, Vsn}).

%%-----------------------------------------------------------------
%% Args: RelFile = string()
%%       Libs = [{Lib, LibVsn, Dir}]
%%       Lib = LibVsn = Dir = string()
%% Purpose: Tells the release handler that a release has been
%%          unpacked, without using the function unpack_release/1.
%%          RelFile is an absolute file name including the extension
%%          .rel.
%%          The release dir will be created.  The necessary files can
%%          be installed by calling install_file/2.
%%          The release_handler remembers where all libs are located.
%%          If remove_release is called later,
%%          those libs are removed as well (if no other releases uses
%%          them).
%% Returns: ok | {error, Reason}
%%-----------------------------------------------------------------
-doc """
set_unpacked(RelFile, AppDirs)

Makes it possible to handle unpacking of releases outside the release handler.

Tells the release handler that the release is unpacked. `Vsn` is extracted from
the release resource file `RelFile`.

`AppDirs` can be used to specify from where the modules for the specified
applications are to be loaded. `App` is the name of an application, `Vsn` is the
version, and `Dir` is the name of the directory where `App-Vsn` is located. The
corresponding modules are to be located under `Dir/App-Vsn/ebin`. The
directories for applications not specified in `AppDirs` are assumed to be
located in `$ROOT/lib`.
""".
-spec set_unpacked(RelFile, AppDirs) -> {ok, Vsn} | {error, Reason} when RelFile :: string(),
   AppDirs :: [{App, Vsn, Dir}],
    App :: atom(),
    Vsn :: string(),
   Dir :: string(),
   Reason :: term().
set_unpacked(RelFile, LibDirs) ->
    call({set_unpacked, RelFile, LibDirs}).

%%-----------------------------------------------------------------
%% Args: Vsn = string()
%% Purpose: Makes it possible to handle removal of releases
%%          outside the release_handler.
%%          This function won't delete any files at all.
%% Returns: ok | {error, Reason}
%%-----------------------------------------------------------------
-doc """
set_removed(Vsn)

Makes it possible to handle removal of releases outside the release handler.

Tells the release handler that the release is removed from the system. This
function does not delete any files.
""".
-spec set_removed(Vsn) -> ok | {error, Reason} when Vsn :: string(),
   Reason :: {permanent, Vsn} | term().
set_removed(Vsn) ->
    call({set_removed, Vsn}).

%%-----------------------------------------------------------------
%% Purpose: Makes it possible to install the start.boot,
%%          sys.config and relup files if they are not part of a 
%%          standard release package.  May be used to
%%          install files that are generated, before install_release
%%          is called.
%% Returns: ok | {error, {no_such_release, Vsn}}
%%-----------------------------------------------------------------
-doc """
install_file(Vsn, File)

Installs a release-dependent file in the release structure.

The release-dependent file must be in the release structure when a new release
is installed: `start.boot`, `relup`, and `sys.config`.

The function can be called, for example, when these files are generated at the
target. The function is to be called after `set_unpacked/2` has been called.
""".
-spec install_file(Vsn, File) -> ok | {error, Reason} when Vsn :: string(),
   File :: string(),
   Reason :: term().
install_file(Vsn, File) when is_list(File) ->
    call({install_file, File, Vsn}).

%%-----------------------------------------------------------------
%% Returns: [{Name, Vsn, [LibName], Status}]
%%          Status = unpacked | current | permanent | old
%%-----------------------------------------------------------------
-doc """
Returns all releases known to the release handler.
""".
-spec which_releases() -> [{Name, Vsn, Apps, Status}] when Name :: string(),
   Vsn :: string(),
   Apps :: [AppVsn :: string()],
   Status :: unpacked | current | permanent | old.
which_releases() ->
    call(which_releases).

%%-----------------------------------------------------------------
%% Returns: [{Name, Vsn, [LibName], Status}]
%%          Status = unpacked | current | permanent | old
%%-----------------------------------------------------------------
-doc """
which_releases(Status)

Returns all releases, known to the release handler, of a specific status.
""".
-doc(#{since => <<"OTP R15B">>}).
-spec which_releases(Status) -> [{Name, Vsn, Apps, Status}] when Name :: string(),
   Vsn :: string(),
   Apps :: [AppVsn :: string()],
   Status :: unpacked | current | permanent | old.
which_releases(Status) ->
    Releases = which_releases(),
    get_releases_with_status(Releases, Status, []).

%%-----------------------------------------------------------------
%% check_script(Script, LibDirs) -> ok | {error, Reason}
%%-----------------------------------------------------------------
check_script(Script, LibDirs) ->
    release_handler_1:check_script(Script, LibDirs).

%%-----------------------------------------------------------------
%% eval_script(Script, Apps, LibDirs, NewLibs, Opts) ->
%%                                             {ok, UnPurged} |
%%                                             restart_emulator |
%%                                             {error, Error}
%%                                             {'EXIT', Reason}
%% If sync_nodes is present, the calling process must have called
%% net_kernel:monitor_nodes(true) before calling this function.
%% No!  No other process than the release_handler can ever call this
%% function, if sync_nodes is used.
%%
%% LibDirs is a list of all applications, while NewLibs is a list of
%% applications that have changed version between the current and the
%% new release.
%% -----------------------------------------------------------------
eval_script(Script, Apps, LibDirs, NewLibs, Opts) ->
    catch release_handler_1:eval_script(Script, Apps, LibDirs, NewLibs, Opts).

%%-----------------------------------------------------------------
%% Func: create_RELEASES(Root, RelFile, LibDirs) -> ok | {error, Reason}
%% Types: Root = RelFile = string()
%% Purpose: Creates an initial RELEASES file.
%%-----------------------------------------------------------------
-doc false.
create_RELEASES([Root, RelFile | LibDirs]) ->
    create_RELEASES(Root, filename:join(Root, "releases"), RelFile, LibDirs).

-doc false.
create_RELEASES(Root, RelFile) ->
    create_RELEASES(Root, filename:join(Root, "releases"), RelFile, []).

-doc(#{equiv => create_RELEASES("", RelDir, RelFile, Appdirs)}).
-doc(#{since => <<"OTP 25.0">>}).
-spec create_RELEASES(RelDir, RelFile, AppDirs) -> ok | {error, Reason} when
      RelDir :: string(),
      RelFile :: string(),
      AppDirs :: [{App, Vsn, Dir}],
      App :: atom(),
      Vsn :: string(),
      Dir :: string(),
      Reason :: term().
create_RELEASES(RelDir, RelFile, LibDirs) ->
    create_RELEASES("", RelDir, RelFile, LibDirs).
-doc """
create_RELEASES(Root, RelDir, RelFile, AppDirs)

Creates an initial `RELEASES` file to be used by the release handler.

This file must exist to install new releases.

`Root` is the root of the installation (`$ROOT`) as described earlier. `RelDir`
is the directory where the `RELEASES` file is to be created (normally
`$ROOT/releases`). `RelFile` is the name of the `.rel` file that describes the
initial release, including the extension `.rel`. If `Root` is not given, the
`RELEASES` file will be location independent (i.e, it will not contain absolute
paths unless there are absolute paths in `AppDirs`). A `RELEASES` file should be
made location independent if the installation's `$ROOT` is unknown. The
`release_handler` module will interpret relative paths in a running system's
`RELEASES` file as being relative to `$ROOT`.

`AppDirs` can be used to specify from where the modules for the specified
applications are to be loaded. `App` is the name of an application, `Vsn` is the
version, and `Dir` is the name of the directory where `App-Vsn` is located. The
corresponding modules are to be located under `Dir/App-Vsn/ebin`. The
directories for applications not specified in `AppDirs` are assumed to be
located in `$ROOT/lib`.
""".
-spec create_RELEASES(Root, RelDir, RelFile, AppDirs) -> ok | {error, Reason} when Root :: string(),
   RelDir :: string(),
   RelFile :: string(),
   AppDirs :: [{App, Vsn, Dir}],
    App :: atom(),
    Vsn :: string(),
   Dir :: string(),
   Reason :: term().
create_RELEASES(Root, RelDir, RelFile, LibDirs) ->
    case catch check_rel(Root, RelFile, LibDirs, false) of
	{error, Reason } ->
	    {error, Reason};
	Rel ->
	    Rel2 = Rel#release{status = permanent},
	    catch write_releases(RelDir, [Rel2], false)
    end.

%%-----------------------------------------------------------------
%% Func: upgrade_app(App, Dir) -> {ok, Unpurged}
%%                              | restart_emulator
%%                              | {error, Error}
%% Types:
%%   App = atom()
%%   Dir = string() assumed to be application directory, the code
%%         located under Dir/ebin
%% Purpose: Upgrade to the version in Dir according to an appup file
%%-----------------------------------------------------------------
-doc """
upgrade_app(App, Dir)

Upgrades an application `App` from the current version to a new version located
in `Dir` according to the `.appup` file.

`App` is the name of the application, which must be started. `Dir` is the new
library directory of `App`. The corresponding modules as well as the `.app` and
`.appup` files are to be located under `Dir/ebin`.

The function looks in the `.appup` file and tries to find an upgrade script from
the current version of the application using `upgrade_script/2`. This script is
evaluated using `eval_appup_script/4`, exactly in the same way as
[`install_release/1,2`](`install_release/1`) does.

Returns one of the following:

- `{ok, Unpurged}` if evaluating the script is successful, where `Unpurged` is a
  list of unpurged modules
- `restart_emulator` if this instruction is encountered in the script
- `{error, Reason}` if an error occurred when finding or evaluating the script

If the `restart_new_emulator` instruction is found in the script,
`upgrade_app/2` returns `{error,restart_new_emulator}`. This because
`restart_new_emulator` requires a new version of the emulator to be started
before the rest of the upgrade instructions can be executed, and this can only
be done by [`install_release/1,2`](`install_release/1`).
""".
-doc(#{group => <<"Application Upgrade/Downgrade">>}).
-spec upgrade_app(App, Dir) -> {ok, Unpurged} | restart_emulator | {error, Reason} when App :: atom(),
   Dir :: string(),
   Unpurged :: [Module],
    Module :: atom(),
   Reason :: term().
upgrade_app(App, NewDir1) ->
    NewDir = root_dir_relative_path(NewDir1),
    try upgrade_script(App, NewDir) of
	{ok, NewVsn, Script} ->
	    eval_appup_script(App, NewVsn, NewDir, Script)
    catch
	throw:Reason ->
	    {error, Reason}
    end.

%%-----------------------------------------------------------------
%% Func: downgrade_app(App, Dir)
%%       downgrade_app(App, Vsn, Dir) -> {ok, Unpurged}
%%                                     | restart_emulator
%%                                     | {error, Error}
%% Types:
%%   App = atom()
%%   Vsn = string(), may be omitted if Dir == App-Vsn
%%   Dir = string() assumed to be application directory, the code
%%         located under Dir/ebin
%% Purpose: Downgrade from the version in Dir according to an appup file
%%          located in the ebin dir of the _current_ version
%%-----------------------------------------------------------------
-doc(#{equiv => downgrade_app/3}).
-doc(#{group => <<"Application Upgrade/Downgrade">>}).
-spec downgrade_app(App, Dir) ->  {ok, Unpurged} | restart_emulator | {error, Reason} when
      App :: atom(),
      Dir :: string(),
      Unpurged :: [Module],
      Module :: atom(),
      Reason :: term().
downgrade_app(App, OldDir) ->
    case string:lexemes(filename:basename(OldDir), "-") of
	[_AppS, OldVsn] ->
	    downgrade_app(App, OldVsn, OldDir);
	_ ->
	    {error, {unknown_version, App}}
    end.
-doc """
downgrade_app(App, OldVsn, Dir)

Downgrades an application `App` from the current version to a previous version
`OldVsn` located in `Dir` according to the `.appup` file.

`App` is the name of the application, which must be started. `OldVsn` is the
previous application version and can be omitted if `Dir` is of the format
`"App-OldVsn"`. `Dir` is the library directory of the previous version of `App`.
The corresponding modules and the old `.app` file are to be located under
`Dir/ebin`. The `.appup` file is to be located in the `ebin` directory of the
_current_ library directory of the application
([`code:lib_dir(App)`](`code:lib_dir/1`)).

The function looks in the `.appup` file and tries to find a downgrade script to
the previous version of the application using `downgrade_script/3`. This script
is evaluated using `eval_appup_script/4`, exactly in the same way as
[`install_release/1,2`](`install_release/1`) does.

Returns one of the following:

- `{ok, Unpurged}` if evaluating the script is successful, where `Unpurged` is a
  list of unpurged modules
- `restart_emulator` if this instruction is encountered in the script
- `{error, Reason}` if an error occurred when finding or evaluating the script
""".
-doc(#{group => <<"Application Upgrade/Downgrade">>}).
-spec downgrade_app(App, OldVsn, Dir) -> {ok, Unpurged} | restart_emulator | {error, Reason} when App :: atom(),
   Dir :: string(),
   OldVsn :: string(),
   Unpurged :: [Module],
    Module :: atom(),
   Reason :: term().
downgrade_app(App, OldVsn, OldDir) ->
    try downgrade_script(App, OldVsn, OldDir) of
	{ok, Script} ->
	    eval_appup_script(App, OldVsn, OldDir, Script)
    catch
	throw:Reason ->
	    {error, Reason}
    end.

-doc """
upgrade_script(App, Dir)

Tries to find an application upgrade script for `App` from the current version
to a new version located in `Dir`.

The upgrade script can then be evaluated using `eval_appup_script/4`. It is
recommended to use `upgrade_app/2` instead, but this function (`upgrade_script`)
is useful to inspect the contents of the script.

`App` is the name of the application, which must be started. `Dir` is the new
library directory of `App`. The corresponding modules as well as the `.app` and
`.appup` files are to be located under `Dir/ebin`.

The function looks in the `.appup` file and tries to find an upgrade script from
the current application version. High-level instructions are translated to
low-level instructions. The instructions are sorted in the same manner as when
generating a `relup` file.

Returns `{ok, NewVsn, Script}` if successful, where `NewVsn` is the new
application version. For details about `Script`, see [`appup(4)`](appup.md).

Failure: If a script cannot be found, the function fails with an appropriate
error reason.
""".
-doc(#{group => <<"Application Upgrade/Downgrade">>}).
-spec upgrade_script(App, Dir) -> {ok, NewVsn, Script}
                        when
                            App :: atom(),
                            Dir :: string(),
                            NewVsn :: string(),
                            Script :: Instructions :: term().
upgrade_script(App, NewDir1) ->
    NewDir = root_dir_relative_path(NewDir1),
    OldVsn = ensure_running(App),
    OldDir = code:lib_dir(App),
    {NewVsn, Script} = find_script(App, NewDir, OldVsn, up),
    OldAppl = read_app(App, OldVsn, OldDir),
    NewAppl = read_app(App, NewVsn, NewDir),
    case systools_rc:translate_scripts(up,
				       [Script],[NewAppl],[OldAppl]) of
	{ok, LowLevelScript} ->
	    {ok, NewVsn, LowLevelScript};
	{error, _SystoolsRC, Reason} ->
	    throw(Reason)
    end.

-doc """
downgrade_script(App, OldVsn, Dir)

Tries to find an application downgrade script for `App` from the current version
to a previous version `OldVsn` located in `Dir`.

The downgrade script can then be evaluated using `eval_appup_script/4`. It is
recommended to use [`downgrade_app/2,3`](`downgrade_app/3`) instead, but this
function (`downgrade_script`) is useful to inspect the contents of the script.

`App` is the name of the application, which must be started. `Dir` is the
previous library directory of `App`. The corresponding modules and the old
`.app` file are to be located under `Dir/ebin`. The `.appup` file is to be
located in the `ebin` directory of the _current_ library directory of the
application ([`code:lib_dir(App)`)](`code:lib_dir/1`).

The function looks in the `.appup` file and tries to find a downgrade script
from the current application version. High-level instructions are translated to
low-level instructions. The instructions are sorted in the same manner as when
generating a `relup` file.

Returns `{ok, Script}` if successful. For details about `Script`, see
[`appup(4)`](appup.md).

Failure: If a script cannot be found, the function fails with an appropriate
error reason.
""".
-doc(#{group => <<"Application Upgrade/Downgrade">>}).
-spec downgrade_script(App, OldVsn, Dir) -> {ok, Script}
                          when
                              App :: atom(),
                              OldVsn :: string(),
                              Dir :: string(),
                              Script :: Instructions :: term().
downgrade_script(App, OldVsn, OldDir) ->
    NewVsn = ensure_running(App),
    NewDir = code:lib_dir(App),
    {NewVsn, Script} = find_script(App, NewDir, OldVsn, down),
    OldAppl = read_app(App, OldVsn, OldDir),
    NewAppl = read_app(App, NewVsn, NewDir),
    case systools_rc:translate_scripts(dn,
				       [Script],[OldAppl],[NewAppl]) of
	{ok, LowLevelScript} ->
	    {ok, LowLevelScript};
	{error, _SystoolsRC, Reason} ->
	    throw(Reason)
    end.

-doc """
eval_appup_script(App, ToVsn, ToDir, Script)

Evaluates an application upgrade or downgrade script `Script`, the result from
calling `upgrade_script/2` or `downgrade_script/3`, exactly in the same way as
[`install_release/1,2`](`install_release/1`) does.

`App` is the name of the application, which must be started. `ToVsn` is the
version to be upgraded/downgraded to, and `ToDir` is the library directory of
this version. The corresponding modules as well as the `.app` and `.appup` files
are to be located under `Dir/ebin`.

Returns one of the following:

- `{ok, Unpurged}` if evaluating the script is successful, where `Unpurged` is a
  list of unpurged modules
- `restart_emulator` if this instruction is encountered in the script
- `{error, Reason}` if an error occurred when finding or evaluating the script

If the `restart_new_emulator` instruction is found in the script,
`eval_appup_script/4` returns `{error,restart_new_emulator}`. This because
`restart_new_emulator` requires a new version of the emulator to be started
before the rest of the upgrade instructions can be executed, and this can only
be done by [`install_release/1,2`](`install_release/1`).
""".
-doc(#{group => <<"Application Upgrade/Downgrade">>}).
-spec eval_appup_script(App, ToVsn, ToDir, Script :: term()) ->
                           {ok, Unpurged} |
                           restart_emulator |
                           {error, Reason}
                           when
                               App :: atom(),
                               ToVsn :: string(),
                               ToDir :: string(),
                               Unpurged :: [Module],
                               Module :: atom(),
                               Reason :: term().
eval_appup_script(App, ToVsn, ToDir, Script) ->
    EnvBefore = application_controller:prep_config_change(),
    AppSpecL = read_appspec(App, ToDir),
    Res = release_handler_1:eval_script(Script,
					[], % [AppSpec]
					[{App, ToVsn, ToDir}],
					[{App, ToVsn, ToDir}],
					[]), % [Opt]
    case Res of
	{ok, _Unpurged} ->
	    application_controller:change_application_data(AppSpecL,[]),
	    application_controller:config_change(EnvBefore);
	_Res ->
	    ignore
    end,
    Res.

ensure_running(App) ->
    case lists:keysearch(App, 1, application:which_applications()) of
	{value, {_App, _Descr, Vsn}} ->
	    Vsn;
	false ->
	    throw({app_not_running, App})
    end.

find_script(App, Dir, OldVsn, UpOrDown) ->
    Appup1 = filename:join([Dir, "ebin", atom_to_list(App)++".appup"]),
    Appup = root_dir_relative_path(Appup1),
    case file:consult(Appup) of
	{ok, [{NewVsn, UpFromScripts, DownToScripts}]} ->
	    Scripts = case UpOrDown of
			  up -> UpFromScripts;
			  down -> DownToScripts
		      end,
	    case systools_relup:appup_search_for_version(OldVsn,Scripts) of
		{ok,Script} ->
		    {NewVsn,Script};
		error ->
		    throw({version_not_in_appup, OldVsn})
	    end;
	{error, enoent} ->
	    throw(no_appup_found);
	{error, Reason} ->
	    throw(Reason)
    end.

read_app(App, Vsn, Dir) ->
    AppS = atom_to_list(App),
    Path = [filename:join(Dir, "ebin")],
    case systools_make:read_application(AppS, Vsn, Path, []) of
	{ok, Appl} ->
	    Appl;
	{error, {not_found, _AppFile}} ->
	    throw({no_app_found, Vsn, Dir});
	{error, Reason} ->
	    throw(Reason)
    end.

read_appspec(App, Dir) ->
    AppS = atom_to_list(App),
    Path = [root_dir_relative_path(filename:join(Dir, "ebin"))],
    case file:path_consult(Path, AppS++".app") of
	{ok, AppSpecL, _File} ->
	    AppSpecL;
	{error, Reason} ->
	    throw(Reason)
    end.
				      
%%-----------------------------------------------------------------
%% call(Request) -> Term
%%-----------------------------------------------------------------
call(Req) ->
    gen_server:call(release_handler, Req, infinity).


%%-----------------------------------------------------------------
%% Call-back functions from gen_server
%%-----------------------------------------------------------------
-doc false.
init([]) ->
    {ok, [[Root]]} = init:get_argument(root),
    {CliDir, Masters} = is_client(),
    ReleaseDir =
	case application:get_env(sasl, releases_dir) of
	    undefined ->
		case os:getenv("RELDIR") of
		    false ->
			if
			    CliDir == false ->
				filename:join([Root, "releases"]);
			    true ->
				filename:join([CliDir, "releases"])
			end;
		    RELDIR ->
			RELDIR
		end;
	    {ok, Dir} ->
		Dir
	end,
    Releases =
	case consult(filename:join(ReleaseDir, "RELEASES"), Masters) of
	    {ok, [Term]} ->
		transform_release(ReleaseDir, Term, Masters);
	    _ ->
		{Name, Vsn} = init:script_id(),
		[#release{name = Name, vsn = Vsn, status = permanent}]
	end,
    StartPrg =
	case application:get_env(start_prg) of
	    {ok, Found2} when is_list(Found2) ->
		{do_check, Found2};
	    _ ->
		{no_check, filename:join([Root, "bin", "start"])}
	end,
    Static =
	case application:get_env(static_emulator) of
	    {ok, SFlag} when is_atom(SFlag) -> SFlag;
	    _                            -> false
	end,
    {ok, #state{root = Root, rel_dir = ReleaseDir, releases = Releases,
		start_prg = StartPrg, masters = Masters,
	        client_dir = CliDir, static_emulator = Static}}.

-doc false.
handle_call({unpack_release, ReleaseName}, _From, S)
  when S#state.masters == false ->
    case catch do_unpack_release(S#state.root, S#state.rel_dir,
				 ReleaseName, S#state.releases) of
	{ok, NewReleases, Vsn} -> 
	    {reply, {ok, Vsn}, S#state{releases = NewReleases}};
	{error, Reason}   ->
	    {reply, {error, Reason}, S}; 
	{'EXIT', Reason} ->
	    {reply, {error, Reason}, S}
    end;
handle_call({unpack_release, _ReleaseName}, _From, S) ->
    {reply, {error, client_node}, S};

handle_call({check_install_release, Vsn, Purge}, _From, S) ->
    case catch do_check_install_release(S#state.rel_dir,
					Vsn,
					S#state.releases,
					S#state.masters,
					Purge) of
	{ok, CurrentVsn, Descr} -> 
	    {reply, {ok, CurrentVsn, Descr}, S};
	{error, Reason}   ->
	    {reply, {error, Reason}, S}; 
	{'EXIT', Reason} ->
	    {reply, {error, Reason}, S}
    end;

handle_call({install_release, Vsn, ErrorAction, Opts}, From, S) ->
    NS = resend_sync_nodes(S),
    case catch do_install_release(S, Vsn, Opts) of
	{ok, NewReleases, [], CurrentVsn, Descr} ->
	    {reply, {ok, CurrentVsn, Descr}, NS#state{releases=NewReleases}};
	{ok, NewReleases, Unpurged, CurrentVsn, Descr} ->
	    Timer =
		case S#state.timer of
		    undefined ->
			{ok, Ref} = timer:send_interval(?timeout, timeout),
			Ref;
		    Ref -> Ref
		end,
	    NewS = NS#state{releases = NewReleases, unpurged = Unpurged,
			    timer = Timer},
	    {reply, {ok, CurrentVsn, Descr}, NewS};
	{error, Reason}   ->
	    {reply, {error, Reason}, NS}; 
	{restart_emulator, CurrentVsn, Descr} ->
	    gen_server:reply(From, {ok, CurrentVsn, Descr}),
	    init:reboot(),
	    {noreply, NS};
	{restart_new_emulator, CurrentVsn, Descr} ->
	    gen_server:reply(From, {continue_after_restart, CurrentVsn, Descr}),
	    init:reboot(),
	    {noreply, NS};
	{'EXIT', Reason} ->
	    io:format("release_handler:"
		      "install_release(Vsn=~tp Opts=~tp) failed, "
		      "Reason=~tp~n", [Vsn, Opts, Reason]),
	    gen_server:reply(From, {error, Reason}),
	    case ErrorAction of
		restart ->
		    init:restart();
		reboot ->
		    init:reboot()
	    end,
	    {noreply, NS}
    end;

handle_call({make_permanent, Vsn}, _From, S) ->
    case catch do_make_permanent(S, Vsn) of
	{ok, Releases, Unpurged} ->
	    {reply, ok, S#state{releases = Releases, unpurged = Unpurged}};
	{error, Reason}   ->
	    {reply, {error, Reason}, S}; 
	{'EXIT', Reason} ->
	    {reply, {error, Reason}, S}
    end;

handle_call({reboot_old_release, Vsn}, From, S) ->
    case catch do_reboot_old_release(S, Vsn) of
	ok ->
	    gen_server:reply(From, ok),
	    init:reboot(),
	    {noreply, S};
	{error, Reason}   ->
	    {reply, {error, Reason}, S}; 
	{'EXIT', Reason} ->
	    {reply, {error, Reason}, S}
    end;

handle_call({remove_release, Vsn}, _From, S)
  when S#state.masters == false ->
    case catch do_remove_release(S#state.root, S#state.rel_dir,
				 Vsn, S#state.releases) of
	{ok, NewReleases} -> 
	    {reply, ok, S#state{releases = NewReleases}};
	{error, Reason}   ->
	    {reply, {error, Reason}, S}; 
	{'EXIT', Reason} ->
	    {reply, {error, Reason}, S}
    end;
handle_call({remove_release, _Vsn}, _From, S) ->
    {reply, {error, client_node}, S};

handle_call({set_unpacked, RelFile, LibDirs}, _From, S) ->
    Root = S#state.root,
    case catch do_set_unpacked(Root, S#state.rel_dir, RelFile,
			       LibDirs, S#state.releases,
			       S#state.masters) of
	{ok, NewReleases, Vsn} -> 
	    {reply, {ok, Vsn}, S#state{releases = NewReleases}};
	{error, Reason}   ->
	    {reply, {error, Reason}, S}; 
	{'EXIT', Reason} ->
	    {reply, {error, Reason}, S}
    end;

handle_call({set_removed, Vsn}, _From, S) ->
    case catch do_set_removed(S#state.rel_dir, Vsn,
			      S#state.releases,
			      S#state.masters) of
	{ok, NewReleases} ->
	    {reply, ok, S#state{releases = NewReleases}};
	{error, Reason}   ->
	    {reply, {error, Reason}, S}; 
	{'EXIT', Reason} ->
	    {reply, {error, Reason}, S}
    end;

handle_call({install_file, File, Vsn}, _From, S) ->
    Reply = 
	case lists:keysearch(Vsn, #release.vsn, S#state.releases) of
	    {value, _} ->
		Dir = filename:join([S#state.rel_dir, Vsn]),
		catch copy_file(File, Dir, S#state.masters);
	    _ ->
		{error, {no_such_release, Vsn}}
	end,
    {reply, Reply, S};

handle_call(which_releases, _From, S) ->
    Reply = lists:map(fun(#release{name = Name, vsn = Vsn, libs = Libs,
				   status = Status}) ->
			      {Name, Vsn, mk_lib_name(Libs), Status}
		      end, S#state.releases),
    {reply, Reply, S}.

mk_lib_name([{LibName, Vsn, _Dir} | T]) ->
    [lists:concat([LibName, "-", Vsn]) | mk_lib_name(T)];
mk_lib_name([]) -> [].

-doc false.
handle_info(timeout, S) ->
    case soft_purge(S#state.unpurged) of
	[] ->
	    _ = timer:cancel(S#state.timer),
	    {noreply, S#state{unpurged = [], timer = undefined}};
	Unpurged ->
	    {noreply, S#state{unpurged = Unpurged}}
    end;

handle_info({sync_nodes, Id, Node}, S) ->
    PSN = S#state.pre_sync_nodes,
    {noreply, S#state{pre_sync_nodes = [{sync_nodes, Id, Node} | PSN]}};

handle_info(Msg, State) ->
    error_logger:info_msg("release_handler: got unknown message: ~p~n", [Msg]),
    {noreply, State}.

-doc false.
terminate(_Reason, _State) ->
    ok.

-doc false.
handle_cast(_Msg, State) ->
    {noreply, State}.
-doc false.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------
is_client() ->
    case application:get_env(masters) of
	{ok, Masters} ->
	    Alive = is_alive(),
	    case atom_list(Masters) of
		true when Alive == true ->
		    case application:get_env(client_directory) of
			{ok, ClientDir} ->
			    case int_list(ClientDir) of
				true ->
				    {ClientDir, Masters};
				_ ->
				    exit({bad_parameter, client_directory,
					  ClientDir})
			    end;
			_ ->
			    {false, false}
		    end;
		_ ->
		    exit({bad_parameter, masters, Masters})
	    end;
	_ ->
	    {false, false}
    end.

atom_list([A|T]) when is_atom(A) -> atom_list(T);
atom_list([])                    -> true;
atom_list(_)                     -> false.

int_list([I|T]) when is_integer(I) -> int_list(T);
int_list([])                       -> true;
int_list(_)                        -> false.

resend_sync_nodes(S) ->
    lists:foreach(fun(Msg) -> self() ! Msg end, S#state.pre_sync_nodes),
    S#state{pre_sync_nodes = []}.

soft_purge(Unpurged) ->
    lists:filter(fun({Mod, _PostPurgeMethod}) ->
			 case code:soft_purge(Mod) of
			     true -> false; % No proc left, don't remember Mod
			     false -> true  % Still proc left, remember it
			 end
		 end,
		 Unpurged).

brutal_purge(Unpurged) ->
    lists:filter(fun({Mod, brutal_purge}) -> code:purge(Mod), false;
		    (_) -> true
		 end,
		 Unpurged).

%%-----------------------------------------------------------------
%% The release package is a RelName.tar.Z (.tar on non unix) file
%% with the following contents:
%%   - RelName.rel   == {release, {Name, Vsn}, {erts, EVsn}, [lib()]}
%%   - <files> according to [lib()]
%%   - lib() = {LibName, LibVsn}
%% In the Dir, there exists a file called RELEASES, which contains
%% a [{Vsn, {erts, EVsn}, {libs, [{LibName, LibVsn, LibDir}]}}].
%% Note that RelDir is an absolute directory name !
%% Note that this function is not executed by a client
%% release_handler.
%%-----------------------------------------------------------------
do_unpack_release(Root, RelDir, ReleaseName, Releases) ->
    Tar = filename:join(RelDir, ReleaseName ++ ".tar.gz"),
    do_check_file(Tar, regular),
    Rel = ReleaseName ++ ".rel",
    _ = extract_rel_file(filename:join("releases", Rel), Tar, Root),
    RelFile = filename:join(RelDir, Rel),
    %% Send an empty string as Root as the library locations should
    %% appear as paths relative to the root
    Release = check_rel("", RelFile, false),
    #release{vsn = Vsn} = Release,
    case lists:keysearch(Vsn, #release.vsn, Releases) of
	{value, _} -> throw({error, {existing_release, Vsn}});
	_          -> ok
    end,
    extract_tar(Root, Tar),
    NewReleases = [Release#release{status = unpacked} | Releases],
    write_releases(RelDir, NewReleases, false),

    %% Keeping this for backwards compatibility reasons with older
    %% systools:make_tar, where there is no copy of the .rel file in
    %% the releases/<vsn> dir. See OTP-9746.
    Dir = filename:join([RelDir, Vsn]),
    copy_file(RelFile, Dir, false),

    %% Clean release
    _ = root_dir_relative_file_delete(Tar),
    _ = root_dir_relative_file_delete(RelFile),

    {ok, NewReleases, Vsn}.
   
check_rel(Root, RelFile, Masters) ->
    check_rel(Root, RelFile, [], Masters).
check_rel(Root, RelFile, LibDirs, Masters) ->
    case consult(RelFile, Masters) of
	{ok, [RelData]} ->
	    check_rel_data(RelData, Root, LibDirs, Masters);
	{ok, _} ->
	    throw({error, {bad_rel_file, RelFile}});
	{error, Reason} when is_tuple(Reason) ->
	    throw({error, {bad_rel_file, RelFile}});
	{error, FileError} -> % FileError is posix atom | no_master
	    throw({error, {FileError, RelFile}})
    end.

check_rel_data({release, {Name, Vsn}, {erts, EVsn}, Libs}, Root, LibDirs,
		Masters) ->
    Libs2 =
	lists:map(fun(LibSpec) ->
			  Lib = element(1, LibSpec),
			  LibVsn = element(2, LibSpec),
			  LibName = lists:concat([Lib, "-", LibVsn]),
			  LibDir = 
			      case lists:keysearch(Lib, 1, LibDirs) of
				  {value, {_Lib, _Vsn, Dir}} ->
				      Path = filename:join(Dir,LibName),
				      check_path(Path, Masters),
				      Path;
				  _ ->
                                      %% If Root is an empty string,
                                      %% we assume that the path is
                                      %% relative to code:root_dir()
                                      %% and save a relative
                                      %% path. This is done to make it
                                      %% easy to create a relocatable
                                      %% RELEASES file.
                                      case string:length(Root) of
                                          0 ->
                                              filename:join("lib", LibName);
                                          _ ->
                                              filename:join([Root, "lib", LibName])
                                      end
			      end,
			  {Lib, LibVsn, LibDir}
		  end,
		  Libs),
    #release{name = Name, vsn = Vsn, erts_vsn = EVsn,
	     libs = Libs2, status = unpacking};
check_rel_data(RelData, _Root, _LibDirs, _Masters) ->
    throw({error, {bad_rel_data, RelData}}).

check_path(Path) ->
	check_path_response(Path, root_dir_relative_read_file_info(Path)).
check_path(Path, false)   -> check_path(Path);
check_path(Path, Masters) -> check_path_master(Masters, Path).

%%-----------------------------------------------------------------
%% check_path at any master node.
%% If the path does not exist or is not a directory
%% at one node it should not exist at any other node either.
%%-----------------------------------------------------------------
check_path_master([Master|Ms], Path) ->
	case rpc:call(Master, ?MODULE, root_dir_relative_read_file_info, [Path]) of
	{badrpc, _} -> consult_master(Ms, Path);
	Res         -> check_path_response(Path, Res)
	end;
check_path_master([], _Path) ->
	{error, no_master}.

check_path_response(_Path, {ok, Info}) when Info#file_info.type==directory ->
	ok;
check_path_response(Path, {ok, _Info}) ->
	throw({error, {not_a_directory, Path}});
check_path_response(Path, {error, _Reason}) ->
	throw({error, {no_such_directory, Path}}).

do_check_install_release(RelDir, Vsn, Releases, Masters, Purge) ->
    case lists:keysearch(Vsn, #release.vsn, Releases) of
	{value, #release{status = current}} ->
	    {error, {already_installed, Vsn}};
	{value, Release} ->
	    LatestRelease = get_latest_release(Releases),
	    VsnDir = filename:join([RelDir, Vsn]),
	    check_file(filename:join(VsnDir, "start.boot"), regular, Masters),
	    IsRelup = check_opt_file(filename:join(VsnDir, "relup"), regular, Masters),
	    check_opt_file(filename:join(VsnDir, "sys.config"), regular, Masters),

	    %% Check that all required libs are present
	    Libs = Release#release.libs,
	    lists:foreach(fun({_Lib, _LibVsn, LibDir}) ->
				  check_file(LibDir, directory, Masters),
				  Ebin = filename:join(LibDir, "ebin"),
				  check_file(Ebin, directory, Masters)
			  end,
			  Libs),

	    if
		IsRelup ->
		    case get_rh_script(LatestRelease, Release, RelDir, Masters) of
			{ok, {CurrentVsn, Descr, Script}} ->
			    case catch check_script(Script, Libs) of
				{ok,SoftPurgeMods} when Purge=:=true ->
				    %% Get modules with brutal_purge
				    %% instructions, but that can be
				    %% soft purged
				    {ok,BrutalPurgeMods} =
					release_handler_1:check_old_processes(
					  Script,brutal_purge),
				    lists:foreach(
				      fun(Mod) ->
					      catch erlang:purge_module(Mod)
				      end,
				      SoftPurgeMods ++ BrutalPurgeMods),
				    {ok, CurrentVsn, Descr};
				{ok,_} ->
				    {ok, CurrentVsn, Descr};
				Else ->
				    Else
			    end;
			Error ->
			    Error
		    end;
		true ->
		    {ok, Vsn, ""}
	    end;
	_ ->
	    {error, {no_such_release, Vsn}}
    end.
	    
do_install_release(#state{start_prg = StartPrg,
			  root = RootDir,
			  rel_dir = RelDir, releases = Releases,
			  masters = Masters,
			  static_emulator = Static},
		   Vsn, Opts) ->
    case lists:keysearch(Vsn, #release.vsn, Releases) of
	{value, #release{status = current}} ->
	    {error, {already_installed, Vsn}};
	{value, Release} ->
	    LatestRelease = get_latest_release(Releases),
	    case get_rh_script(LatestRelease, Release, RelDir, Masters) of
		{ok, {_CurrentVsn, _Descr, [restart_new_emulator|_Script]}}
		  when Static == true ->
		    throw(static_emulator);
		{ok, {CurrentVsn, Descr, [restart_new_emulator|_Script]}} ->
		    %% This will only happen if the upgrade includes
		    %% an emulator upgrade (and it is not a downgrade)
		    %% - then the new emulator must be started before
		    %% new code can be loaded.
		    %% Create a temporary release which includes new
		    %% emulator, kernel, stdlib and sasl - and old
		    %% versions of other applications.
		    {TmpVsn,TmpRelease} =
			new_emulator_make_tmp_release(LatestRelease,Release,
						      RelDir,Opts,Masters),
		    NReleases = [TmpRelease|Releases],

		    %% Then uppgrade to the temporary release.
		    %% The rest of the upgrade will continue after the restart
		    prepare_restart_new_emulator(StartPrg, RootDir,
						 RelDir, TmpVsn, TmpRelease,
						 NReleases, Masters),
		    {restart_new_emulator, CurrentVsn, Descr};
		{ok, {CurrentVsn, Descr, Script}} ->
		    %% In case there has been an emulator upgrade,
		    %% remove the temporary release
		    NReleases =
			new_emulator_rm_tmp_release(
			  LatestRelease#release.vsn,
			  LatestRelease#release.erts_vsn,
			  Vsn,RelDir,Releases,Masters),

		    %% Then execute the relup script
		    mon_nodes(true),
		    EnvBefore = application_controller:prep_config_change(),
		    Apps = change_appl_data(RelDir, Release, Masters),
		    LibDirs = Release#release.libs,
		    NewLibs = get_new_libs(LatestRelease#release.libs,
					   Release#release.libs),
		    case eval_script(Script, Apps, LibDirs, NewLibs, Opts) of
			{ok, Unpurged} ->
			    application_controller:config_change(EnvBefore),
			    mon_nodes(false),
			    NReleases1 = set_status(Vsn, current, NReleases),
			    {ok, NReleases1, Unpurged, CurrentVsn, Descr};
			restart_emulator when Static == true ->
			    throw(static_emulator);
			restart_emulator ->
			    mon_nodes(false),
			    prepare_restart_new_emulator(StartPrg, RootDir,
							 RelDir, Vsn, Release,
							 NReleases, Masters),
			    {restart_emulator, CurrentVsn, Descr};
			Else ->
			    application_controller:config_change(EnvBefore),
			    mon_nodes(false),
			    Else
		    end;
		Error ->
		    Error
	    end;
	_ ->
	    {error, {no_such_release, Vsn}}
    end.

new_emulator_make_tmp_release(CurrentRelease,ToRelease,RelDir,Opts,Masters) ->
    CurrentVsn = CurrentRelease#release.vsn,
    ToVsn = ToRelease#release.vsn,
    TmpVsn = ?tmp_vsn(CurrentVsn),
    case get_base_libs(ToRelease#release.libs) of
	{ok,{Kernel,Stdlib,Sasl},_} ->
	    case get_base_libs(CurrentRelease#release.libs) of
		{ok,_,RestLibs} ->
		    TmpErtsVsn = ToRelease#release.erts_vsn,
		    TmpLibs = [Kernel,Stdlib,Sasl|RestLibs],
		    TmpRelease = CurrentRelease#release{vsn=TmpVsn,
							erts_vsn=TmpErtsVsn,
							libs = TmpLibs,
							status = unpacked},
		    new_emulator_make_hybrid_boot(CurrentVsn,ToVsn,TmpVsn,
						  RelDir,Opts,Masters),
		    new_emulator_make_hybrid_config(CurrentVsn,ToVsn,TmpVsn,
						    RelDir,Masters),
		    {TmpVsn,TmpRelease};
		{error,{missing,Missing}} ->
		    throw({error,{missing_base_app,CurrentVsn,Missing}})
	    end;
	{error,{missing,Missing}} ->
	    throw({error,{missing_base_app,ToVsn,Missing}})
    end.

%% Get kernel, stdlib and sasl libs,
%% and also return the rest of the libs as a list.
%% Return error if any of kernel, stdlib or sasl does not exist.
get_base_libs(Libs) ->
    get_base_libs(Libs,undefined,undefined,undefined,[]).
get_base_libs([{kernel,_,_}=Kernel|Libs],undefined,Stdlib,Sasl,Rest) ->
    get_base_libs(Libs,Kernel,Stdlib,Sasl,Rest);
get_base_libs([{stdlib,_,_}=Stdlib|Libs],Kernel,undefined,Sasl,Rest) ->
    get_base_libs(Libs,Kernel,Stdlib,Sasl,Rest);
get_base_libs([{sasl,_,_}=Sasl|Libs],Kernel,Stdlib,undefined,Rest) ->
    get_base_libs(Libs,Kernel,Stdlib,Sasl,Rest);
get_base_libs([Lib|Libs],Kernel,Stdlib,Sasl,Rest) ->
    get_base_libs(Libs,Kernel,Stdlib,Sasl,[Lib|Rest]);
get_base_libs([],undefined,_Stdlib,_Sasl,_Rest) ->
    {error,{missing,kernel}};
get_base_libs([],_Kernel,undefined,_Sasl,_Rest) ->
    {error,{missing,stdlib}};
get_base_libs([],_Kernel,_Stdlib,undefined,_Rest) ->
    {error,{missing,sasl}};
get_base_libs([],Kernel,Stdlib,Sasl,Rest) ->
    {ok,{Kernel,Stdlib,Sasl},lists:reverse(Rest)}.

new_emulator_make_hybrid_boot(CurrentVsn,ToVsn,TmpVsn,RelDir,Opts,Masters) ->
    FromBootFile = filename:join([RelDir,CurrentVsn,"start.boot"]),
    ToBootFile = filename:join([RelDir,ToVsn,"start.boot"]),
    TmpBootFile = filename:join([RelDir,TmpVsn,"start.boot"]),
    ensure_dir(TmpBootFile,Masters),
    Args = [ToVsn,Opts],
    {ok,FromBoot} = read_file(FromBootFile,Masters),
    {ok,ToBoot} = read_file(ToBootFile,Masters),
    case systools_make:make_hybrid_boot(TmpVsn,FromBoot,ToBoot,Args) of
	{ok,TmpBoot} ->
	    write_file(TmpBootFile,TmpBoot,Masters);
	{error,Reason} ->
	    throw({error,{could_not_create_hybrid_boot,Reason}})
    end.

new_emulator_make_hybrid_config(CurrentVsn,ToVsn,TmpVsn,RelDir,Masters) ->
    FromFile = filename:join([RelDir,CurrentVsn,"sys.config"]),
    ToFile = filename:join([RelDir,ToVsn,"sys.config"]),
    TmpFile = filename:join([RelDir,TmpVsn,"sys.config"]),

    FromConfig =
	case consult(FromFile,Masters) of
	    {ok,[FC]} ->
		FC;
	    {error,Error1} ->
		io:format("Warning: ~w cannot read ~tp: ~tp~n",
			  [?MODULE,FromFile,Error1]),
		[]
	end,

    [Kernel,Stdlib,Sasl] =
	case consult(ToFile,Masters) of
	    {ok,[ToConfig]} ->
		[lists:keyfind(App,1,ToConfig) || App <- [kernel,stdlib,sasl]];
	    {error,Error2} ->
		io:format("Warning: ~w cannot read ~tp: ~tp~n",
			  [?MODULE,ToFile,Error2]),
		[false,false,false]
	end,

    Config1 = replace_config(kernel,FromConfig,Kernel),
    Config2 = replace_config(stdlib,Config1,Stdlib),
    Config3 = replace_config(sasl,Config2,Sasl),

    ConfigStr = io_lib:format("%% ~s~n~tp.~n",
                              [epp:encoding_to_string(utf8),Config3]),
    write_file(TmpFile,unicode:characters_to_binary(ConfigStr),Masters).

%% Take the configuration for application App from the new config and
%% insert in the old config.
%% If no entry exists in the new config, then delete the entry (if it exists)
%% from the old config.
%% If entry exists in the new config, but not in the old config, then
%% add the entry.
replace_config(App,Config,false) ->
    lists:keydelete(App,1,Config);
replace_config(App,Config,AppConfig) ->
    lists:keystore(App,1,Config,AppConfig).

%% Remove all files related to the temporary release
new_emulator_rm_tmp_release(?tmp_vsn(_)=TmpVsn,EVsn,NewVsn,
			    RelDir,Releases,Masters) ->
    case os:type() of
	{win32, nt} ->
	    rename_tmp_service(EVsn,TmpVsn,NewVsn);
	_ ->
	    ok
    end,
    remove_dir(filename:join(RelDir,TmpVsn),Masters),
    lists:keydelete(TmpVsn,#release.vsn,Releases);
new_emulator_rm_tmp_release(_,_,_,_,Releases,_) ->
    Releases.

%% Rename the temporary service (for erts ugprade) to the real ToVsn
rename_tmp_service(EVsn,TmpVsn,NewVsn) ->
    FromName = hd(string:lexemes(atom_to_list(node()),"@")) ++ "_" ++ TmpVsn,
    ToName = hd(string:lexemes(atom_to_list(node()),"@")) ++ "_" ++ NewVsn,
    case erlsrv:get_service(EVsn,ToName) of
	{error, _Error} ->
	    ok;
	_Data ->
	    {ok,_} = erlsrv:remove_service(ToName),
	    ok
    end,
    rename_service(EVsn,FromName,ToName).


%% Rename a service and check that it succeeded
rename_service(EVsn,FromName,ToName) ->
    case erlsrv:rename_service(EVsn,FromName,ToName) of
	{ok,_} ->
	    case erlsrv:get_service(EVsn,ToName) of
		{error,Error1} ->
		    throw({error,Error1});
		_Data2 ->
		    ok
	    end;
	Error2 ->
	    throw({error,{service_rename_failed, Error2}})
    end.


%%% This code chunk updates the services in one of two ways,
%%% Either the emulator is restarted, in which case the old service
%%% is to be removed and the new enabled, or the emulator is NOT restarted
%%% in which case we try to rename the old service to the new name and try
%%% to update heart's view of what service we are really running.
do_make_services_permanent(PermanentVsn,Vsn, PermanentEVsn, EVsn) ->
    PermName = hd(string:lexemes(atom_to_list(node()),"@"))
	++ "_" ++ PermanentVsn,
    Name = hd(string:lexemes(atom_to_list(node()),"@"))
	++ "_" ++ Vsn,
    case erlsrv:get_service(EVsn,Name) of
	{error, _Error} ->
	    %% We probably do not need to replace services, just 
	    %% rename.
	    case os:getenv("ERLSRV_SERVICE_NAME") == PermName of
		true ->
		    rename_service(EVsn,PermName,Name),
		    %% The interfaces for doing this are
		    %% NOT published and may be subject to
		    %% change. Do NOT do this anywhere else!

		    os:putenv("ERLSRV_SERVICE_NAME", Name),

		    %% Restart heart port program, this
		    %% function is only to be used here.
		    heart:cycle();
		false ->
		    throw({error,service_name_missmatch})
	    end;
	Data ->
	    UpdData = erlsrv:new_service(Name, Data, []),
	    case erlsrv:store_service(EVsn,UpdData) of
		ok ->
		    {ok,_} = erlsrv:disable_service(PermanentEVsn, PermName),
		    {ok,_} = erlsrv:enable_service(EVsn, Name),
		    {ok,_} = erlsrv:remove_service(PermName),
		    %%% Read comments about these above...
		    os:putenv("ERLSRV_SERVICE_NAME", Name),
		    ok = heart:cycle();
		Error4 ->
		    throw(Error4)
	    end
    end.

do_make_permanent(#state{releases = Releases,
			 rel_dir = RelDir, unpurged = Unpurged,
			 masters = Masters,
			 static_emulator = Static},
		  Vsn) ->
    case lists:keysearch(Vsn, #release.vsn, Releases) of
	{value, #release{erts_vsn = EVsn, status = Status}}
	  when Status /= unpacked, Status /= old, Status /= permanent ->
	    Dir = filename:join([RelDir, Vsn]),
	    Sys =
		case catch check_file(filename:join(Dir, "sys.config"),
				      regular, Masters) of
		    ok ->     filename:join(Dir, "sys");
		    _ -> false
		end,
	    Boot = filename:join(Dir, "start.boot"),
	    check_file(Boot, regular, Masters),
	    set_permanent_files(RelDir, EVsn, Vsn, Masters, Static),
	    NewReleases = set_status(Vsn, permanent, Releases),
	    write_releases(RelDir, NewReleases, Masters),
	    case os:type() of
		{win32, nt} ->
		    {value, PermanentRelease} = 
				lists:keysearch(permanent, #release.status,
						Releases),
		    PermanentVsn = PermanentRelease#release.vsn,
		    PermanentEVsn = PermanentRelease#release.erts_vsn,
		    case catch do_make_services_permanent(PermanentVsn, 
							  Vsn, 
							  PermanentEVsn,
							  EVsn)  of
			{error,Reason} ->
			    throw({error,{service_update_failed, Reason}});
			_ ->
			    ok
		    end;
		_ ->
		    ok
	    end,
	    ok = init:make_permanent(filename:join(Dir, "start"), Sys),
	    {ok, NewReleases, brutal_purge(Unpurged)};
	{value, #release{status = permanent}} ->
	    {ok, Releases, Unpurged};
	{value, #release{status = Status}} ->
	    {error, {bad_status, Status}};
	false ->
	    {error, {no_such_release, Vsn}}
    end.


do_back_service(OldVersion, CurrentVersion,OldEVsn,CurrentEVsn) ->
    NN = hd(string:lexemes(atom_to_list(node()),"@")),
    OldName = NN ++ "_" ++ OldVersion,
    CurrentName = NN ++ "_" ++ CurrentVersion,
    UpdData = case erlsrv:get_service(CurrentEVsn,CurrentName) of
		  {error, Error} ->
		      throw({error,Error});
		  Data ->
		      erlsrv:new_service(OldName, Data, [])
	      end,
    _ = case erlsrv:store_service(OldEVsn,UpdData) of
	    ok ->
		{ok,_} = erlsrv:disable_service(CurrentEVsn,CurrentName),
		{ok,_} = erlsrv:enable_service(OldEVsn,OldName);
	    Error2 ->
		throw(Error2)
	end,
    OldErlSrv = filename:nativename(erlsrv:erlsrv(OldEVsn)),
    CurrentErlSrv = filename:nativename(erlsrv:erlsrv(CurrentEVsn)),
    case heart:set_cmd(CurrentErlSrv ++ " remove " ++ CurrentName ++ 
		       " & " ++ OldErlSrv ++ " start " ++ OldName) of
	ok ->
	    ok;
	Error3 ->
	    throw({error, {'heart:set_cmd() error', Error3}})
    end.

do_reboot_old_release(#state{releases = Releases,
			     rel_dir = RelDir, masters = Masters,
			     static_emulator = Static},
		      Vsn) ->
    case lists:keysearch(Vsn, #release.vsn, Releases) of
	{value, #release{erts_vsn = EVsn, status = old}} ->
	    CurrentRunning = case os:type() of
				 {win32,nt} ->
				     %% Get the current release on NT
				     case lists:keysearch(permanent, 
							  #release.status,
							  Releases) of
					 false ->
					     lists:keysearch(current,
							     #release.status,
							     Releases);
					 {value,CR} ->
					     CR
				     end;
				 _ ->
				     false
			     end,
	    set_permanent_files(RelDir, EVsn, Vsn, Masters, Static),
	    NewReleases = set_status(Vsn, permanent, Releases),
	    write_releases(RelDir, NewReleases, Masters),
	    case os:type() of
		{win32,nt} ->
		    %% Edit up the services and set a reasonable heart 
		    %% command
		    do_back_service(Vsn,CurrentRunning#release.vsn,EVsn,
				   CurrentRunning#release.erts_vsn);
		_ ->
		    ok
	    end,
	    ok;
	{value, #release{status = Status}} ->
	    {error, {bad_status, Status}};
	false ->
	    {error, {no_such_release, Vsn}}
    end.

%%-----------------------------------------------------------------
%% Depending of if the release_handler is running in normal, client or
%% client with static emulator the new system version is made permanent
%% in different ways.
%%-----------------------------------------------------------------
set_permanent_files(RelDir, EVsn, Vsn, false, _) ->
    write_start(filename:join([RelDir, "start_erl.data"]),
		EVsn ++ " " ++ Vsn,
		false);
set_permanent_files(RelDir, EVsn, Vsn, Masters, false) ->
    write_start(filename:join([RelDir, "start_erl.data"]),
		EVsn ++ " " ++ Vsn,
		Masters);
set_permanent_files(RelDir, _EVsn, Vsn, Masters, _Static) ->
    VsnDir = filename:join([RelDir, Vsn]),
    set_static_files(VsnDir, RelDir, Masters).


do_remove_service(Vsn) ->
    %% Very unconditionally remove the service.
    %% Note that the service could already have been removed when
    %% making another release permanent.
    ServiceName = hd(string:lexemes(atom_to_list(node()),"@"))
	++ "_" ++ Vsn,
    case erlsrv:get_service(ServiceName) of
	{error, _Error} ->
	    ok;
	_Data ->
	    {ok,_} = erlsrv:remove_service(ServiceName),
	    ok
    end.

do_remove_release(Root, RelDir, Vsn, Releases) ->
    % Decide which libs should be removed
    case lists:keysearch(Vsn, #release.vsn, Releases) of
	{value, #release{status = permanent}} ->
	    {error, {permanent, Vsn}};
	{value, #release{libs = RemoveLibs, vsn = Vsn, erts_vsn = EVsn}} ->
	    case os:type() of
		{win32, nt} ->
		    do_remove_service(Vsn);
		_ ->
		    ok
	    end,

	    NewReleases = lists:keydelete(Vsn, #release.vsn, Releases),
	    RemoveThese =
		lists:foldl(fun(#release{libs = Libs}, Remove) ->
				    diff_dir(Remove, Libs)
			    end, RemoveLibs, NewReleases),
	    lists:foreach(fun({_Lib, _LVsn, LDir}) ->
				  remove_file(LDir)
			  end, RemoveThese),
	    remove_file(filename:join([RelDir, Vsn])),
	    case lists:keysearch(EVsn, #release.erts_vsn, NewReleases) of
		{value, _} -> ok;
		false -> % Remove erts library, no more references to it
		    remove_file(filename:join(Root, "erts-" ++ EVsn))
	    end,
	    write_releases(RelDir, NewReleases, false),
	    {ok, NewReleases};
	false ->
	    {error, {no_such_release, Vsn}}
    end.

do_set_unpacked(Root, RelDir, RelFile, LibDirs, Releases, Masters) ->
    Release = check_rel(Root, RelFile, LibDirs, Masters),
    #release{vsn = Vsn} = Release,
    case lists:keysearch(Vsn, #release.vsn, Releases) of
	{value, _} -> throw({error, {existing_release, Vsn}});
	false -> ok
    end,
    NewReleases = [Release#release{status = unpacked} | Releases],
    VsnDir = filename:join([RelDir, Vsn]),
    make_dir(VsnDir, Masters),
    write_releases(RelDir, NewReleases, Masters),
    {ok, NewReleases, Vsn}.

do_set_removed(RelDir, Vsn, Releases, Masters) ->
    case lists:keysearch(Vsn, #release.vsn, Releases) of
	{value, #release{status = permanent}} ->
	    {error, {permanent, Vsn}};
	{value, _} ->
	    NewReleases = lists:keydelete(Vsn, #release.vsn, Releases),
	    write_releases(RelDir, NewReleases, Masters),
	    {ok, NewReleases};
	false ->
	    {error, {no_such_release, Vsn}}
    end.


%%-----------------------------------------------------------------
%% A relup file consists of:
%%   {Vsn, [{FromVsn, Descr, RhScript}], [{ToVsn, Descr, RhScript}]}.
%% It describes how to get to this release from previous releases,
%% and how to get from this release to previous releases.
%% We can get from a FromVsn that's a substring of CurrentVsn (e.g.
%% 1.1 is a substring of 1.1.1, but not 1.2), but when we get to
%% ToVsn, we must have an exact match.
%%
%% We do not put any semantics into the version strings, i.e. we
%% don't know if going from Vsn1 to Vsn2 represents a upgrade or
%% a downgrade.  For both upgrades and downgrades, the relup file
%% is located in the directory of the latest version.  Since we
%% do not which version is latest, we first suppose that ToVsn > 
%% CurrentVsn, i.e. we perform an upgrade.  If we don't find the
%% corresponding relup instructions, we check if it's possible to
%% downgrade from CurrentVsn to ToVsn.
%%-----------------------------------------------------------------
get_rh_script(#release{vsn = ?tmp_vsn(CurrentVsn)},
	      #release{vsn = ToVsn},
	      RelDir,
	      Masters) ->
    {ok,{Vsn,Descr,[restart_new_emulator|Script]}} =
	do_get_rh_script(CurrentVsn,ToVsn,RelDir,Masters),
    {ok,{Vsn,Descr,Script}};
get_rh_script(#release{vsn = CurrentVsn},
	      #release{vsn = ToVsn},
	      RelDir,
	      Masters) ->
    do_get_rh_script(CurrentVsn,ToVsn,RelDir,Masters).

do_get_rh_script(CurrentVsn, ToVsn, RelDir, Masters) ->
    Relup = filename:join([RelDir, ToVsn, "relup"]),
    case try_upgrade(ToVsn, CurrentVsn, Relup, Masters) of
	{ok, RhScript} ->
	    {ok, RhScript};
	_ ->
	    Relup2 = filename:join([RelDir, CurrentVsn,"relup"]),
	    case try_downgrade(ToVsn, CurrentVsn, Relup2, Masters) of
		{ok, RhScript} ->
		    {ok, RhScript};
		_ ->
		    throw({error, {no_matching_relup, ToVsn, CurrentVsn}})
	    end
    end.

try_upgrade(ToVsn, CurrentVsn, Relup, Masters) ->
    case consult(Relup, Masters) of
	{ok, [{ToVsn, ListOfRhScripts, _}]} ->
	    case lists:keysearch(CurrentVsn, 1, ListOfRhScripts) of
		{value, RhScript} -> 
		    {ok, RhScript};
		_ -> 
		    error
	    end;
	{ok, _} ->
	    throw({error, {bad_relup_file, Relup}});
	{error, Reason} when is_tuple(Reason) ->
	    throw({error, {bad_relup_file, Relup}});
	{error, enoent} ->
	    error;
	{error, FileError} -> % FileError is posix atom | no_master
	    throw({error, {FileError, Relup}})
    end.

try_downgrade(ToVsn, CurrentVsn, Relup, Masters) ->
    case consult(Relup, Masters) of
	{ok, [{CurrentVsn, _, ListOfRhScripts}]} ->
	    case lists:keysearch(ToVsn, 1, ListOfRhScripts) of
		{value, RhScript} ->
		    {ok, RhScript};
		_ ->
		    error
	    end;
	{ok, _} ->
	    throw({error, {bad_relup_file, Relup}});
	{error, Reason} when is_tuple(Reason) ->
	    throw({error, {bad_relup_file, Relup}});
	{error, FileError} -> % FileError is posix atom | no_master
	    throw({error, {FileError, Relup}})
    end.


%% Status = current | tmp_current | permanent
set_status(Vsn, Status, Releases) ->
    lists:zf(fun(Release) when Release#release.vsn == Vsn,
		               Release#release.status == permanent ->
		     %% If a permanent rel is installed, it keeps its
		     %% permanent status (not changed to current).
		     %% The current becomes old though.
		     true;
		(Release) when Release#release.vsn == Vsn ->	
		     {true, Release#release{status = Status}};	
		(Release) when Release#release.status == Status ->
		     {true, Release#release{status = old}};
		(_) ->
		     true
	     end, Releases).

get_latest_release(Releases) ->
    case lists:keysearch(current, #release.status, Releases) of
	{value, Release} ->
	    Release;
	false ->
	    {value, Release} = 
		lists:keysearch(permanent, #release.status, Releases),
	    Release
    end.

%% Returns: [{Lib, Vsn, Dir}] to be removed
diff_dir([H | T], L) ->
    case memlib(H, L) of
	true -> diff_dir(T, L);
	false -> [H | diff_dir(T, L)]
    end;
diff_dir([], _) -> [].

memlib({Lib, Vsn, _Dir}, [{Lib, Vsn, _Dir2} | _T]) -> true;
memlib(Lib, [_H | T]) -> memlib(Lib, T);
memlib(_Lib, []) -> false.
			 
%% recursively remove file or directory
-doc false.
remove_file(File) ->
    case root_dir_relative_read_link_info(File) of
	{ok, Info} when Info#file_info.type==directory ->
	    case root_dir_relative_list_dir(File) of
		{ok, Files} ->
		    lists:foreach(fun(File2) ->
					 remove_file(filename:join(File,File2))
				  end, Files),
		    case root_dir_relative_dir_delete(File) of
			ok -> ok;
			{error, Reason} -> throw({error, Reason})
		    end;
		{error, Reason} ->
		    throw({error, Reason})
	    end;
	{ok, _Info} ->
	    case root_dir_relative_file_delete(File) of
		ok -> ok;
		{error, Reason} -> throw({error, Reason})
	    end;
	{error, _Reason} ->
	    throw({error, {no_such_file, File}})

    end.

-doc false.
do_write_file(File, Str) ->
    do_write_file(File, Str, []).
-doc false.
do_write_file(File1, Str, FileOpts) ->
    File = root_dir_relative_path(File1),
    case file:open(File, [write | FileOpts]) of
	{ok, Fd} ->
	    io:put_chars(Fd, Str),
	    ok = file:close(Fd);
	{error, Reason} ->
	    {error, {Reason, File}}
    end.

%%-----------------------------------------------------------------
%% Change current applications (specifically, update their version,
%% description and env.)
%%-----------------------------------------------------------------
change_appl_data(RelDir, #release{vsn = Vsn}, Masters) ->
    Dir = filename:join([RelDir, Vsn]),
    BootFile = filename:join(Dir, "start.boot"),
    case read_file(BootFile, Masters) of
	{ok, Bin} ->
	    Config = case consult(filename:join(Dir, "sys.config"), Masters) of
			 {ok, [Conf]} -> Conf;
			 _ -> []
		     end,
	    Appls = get_appls(binary_to_term(Bin)),
	    case application_controller:change_application_data(Appls,Config) of
		ok -> Appls;
		{error, Reason} -> exit({change_appl_data, Reason})
	    end;
	{error, _Reason} ->
	    throw({error, {no_such_file, BootFile}})
    end.

%%-----------------------------------------------------------------
%% This function is dependent on the application functions and
%% the start script syntax.
%%-----------------------------------------------------------------
get_appls({script, _, Script}) -> get_appls(Script, []).

%% kernel is taken care of separately
get_appls([{kernelProcess, application_controller, 
	    {application_controller, start, [App]}} |T], Res) ->
    get_appls(T, [App | Res]);
%% other applications but kernel
get_appls([{apply, {application, load, [App]}} |T], Res) ->
    get_appls(T, [App | Res]);
get_appls([_ | T], Res) ->
    get_appls(T, Res);
get_appls([], Res) ->
    Res.


mon_nodes(true) ->
    ok = net_kernel:monitor_nodes(true);
mon_nodes(false) ->
    ok = net_kernel:monitor_nodes(false),
    flush().

flush() ->
    receive
	{nodedown, _} -> flush();
	{nodeup, _} -> flush()
    after
	0 -> ok
    end.

prepare_restart_nt(#release{erts_vsn = EVsn, vsn = Vsn},
		   #release{erts_vsn = PermEVsn, vsn = PermVsn},
		   DataFileName) ->
    CurrentServiceName = hd(string:lexemes(atom_to_list(node()),"@"))
	++ "_" ++ PermVsn,
    FutureServiceName = hd(string:lexemes(atom_to_list(node()),"@"))
	++ "_" ++ Vsn,
    CurrentService = case erlsrv:get_service(PermEVsn,CurrentServiceName) of
			 {error, _} = Error1 ->
			     throw(Error1);
			 CS ->
			     CS
		     end,
    FutureService =  erlsrv:new_service(FutureServiceName,
					CurrentService,
					filename:nativename(DataFileName),
					%% This is rather icky... On a
					%% non permanent service, the 
					%% ERLSRV_SERVICE_NAME is 
					%% actually that of an old service,
					%% to make heart commands work...
					CurrentServiceName),
    
    case erlsrv:store_service(EVsn, FutureService) of
	{error, _} = Error2 ->
	    throw(Error2);
	_X ->
	    {ok,_} = erlsrv:disable_service(EVsn, FutureServiceName),
	    ErlSrv = filename:nativename(erlsrv:erlsrv(EVsn)),
	    StartDisabled = ErlSrv ++ " start_disabled " ++ FutureServiceName,
	    case heart:set_cmd(StartDisabled) of
		ok ->
		    ok;
		Error3 ->
		    throw({error, {'heart:set_cmd() error', Error3}})
	    end
    end.
    
%%-----------------------------------------------------------------
%% Set things up for restarting the new emulator.  The actual
%% restart is performed by calling init:reboot() higher up.
%%-----------------------------------------------------------------
prepare_restart_new_emulator(StartPrg, RootDir, RelDir,
			     Vsn, Release, Releases, Masters) ->
    {value, PRelease} = lists:keysearch(permanent, #release.status,Releases),
    NReleases1 = set_status(Vsn, current, Releases),
    NReleases2 = set_status(Vsn,tmp_current,NReleases1),
    write_releases(RelDir, NReleases2, Masters),
    prepare_restart_new_emulator(StartPrg, RootDir, RelDir,
				 Release, PRelease, Masters).

prepare_restart_new_emulator(StartPrg, RootDir, RelDir,
			     Release, PRelease, Masters) ->
    #release{erts_vsn = EVsn, vsn = Vsn} = Release,
    Data = EVsn ++ " " ++ Vsn,
    DataFile = write_new_start_erl(Data, RelDir, Masters),
    %% Tell heart to use DataFile instead of start_erl.data
    case os:type() of
	{win32,nt} ->
	    write_ini_file(RootDir,EVsn,Masters),
	    prepare_restart_nt(Release,PRelease,DataFile); 
	{unix,_} ->
	    StartP = check_start_prg(StartPrg, Masters),
	    case heart:set_cmd(StartP ++ " " ++ DataFile) of
		ok ->
		    ok;
		Error ->
		    throw({error, {'heart:set_cmd() error', Error}})
	    end
    end.

check_start_prg({do_check, StartPrg}, Masters) ->
    check_file(StartPrg, regular, Masters),
    StartPrg;
check_start_prg({_, StartPrg}, _) ->
    StartPrg.

write_new_start_erl(Data, RelDir, Masters) ->
    DataFile = filename:join([RelDir, "new_start_erl.data"]),
    write_file(DataFile, Data, Masters),
    DataFile.

%%-----------------------------------------------------------------
%% When a new emulator shall be restarted, the current release
%% is written with status tmp_current.  When the new emulator
%% is started, this function is called.  The tmp_current release
%% gets status unpacked on disk, and current in memory.  If a reboot
%% is made (due to a crash), the release is just unpacked.  If a crash
%% occurs before a call to transform_release is made, the old emulator
%% is started, and transform_release is called for it.  The tmp_current
%% release is changed to unpacked.
%% If the release is made permanent, this is written to disk.
%%-----------------------------------------------------------------
transform_release(ReleaseDir, Releases, Masters) ->
    case init:script_id() of
	{Name, ?tmp_vsn(_)=TmpVsn} ->
	    %% This is was a reboot due to a new emulator version. The
	    %% current release is a temporary internal release, which
	    %% must be removed. It is the "real new release" that is
	    %% set to unpacked on disk and current in memory.
	    DReleases = lists:keydelete(TmpVsn,#release.vsn,Releases),
	    write_releases(ReleaseDir, DReleases, Masters),
	    set_current({Name,TmpVsn},Releases);
	ScriptId ->
	    F = fun(Release) when Release#release.status == tmp_current ->
			Release#release{status = unpacked};
		   (Release) -> Release
		end,
	    case lists:map(F, Releases) of
		Releases ->
		    Releases;
		DReleases ->
		    write_releases(ReleaseDir, DReleases, Masters),
		    set_current(ScriptId, Releases)
	    end
    end.

set_current(ScriptId, Releases) ->
    F1 = fun(Release) when Release#release.status == tmp_current ->
		 case ScriptId of
		     {_Name,Vsn} when Release#release.vsn == Vsn ->
			 Release#release{status = current};
		     _ ->
			 Release#release{status = unpacked}
		 end;
	    (Release) -> Release
	 end,
    lists:map(F1, Releases).

%%-----------------------------------------------------------------
%% Functions handling files, RELEASES, start_erl.data etc.
%% This functions consider if the release_handler is a client and
%% in that case performs the operations at all master nodes or at
%% none (in case of failure).
%%-----------------------------------------------------------------

check_opt_file(FileName, Type, Masters) ->
    case catch check_file(FileName, Type, Masters) of
	ok ->
	    true;
	_Error ->
	    io:format("Warning: ~tp missing (optional)~n", [FileName]),
	    false
    end.

check_file(FileName, Type, false) ->
    do_check_file(FileName, Type);
check_file(FileName, Type, Masters) ->
    check_file_masters(FileName, Type, Masters).

%% Check that file exists at all masters.
check_file_masters(FileName, Type, [Master|Masters]) ->
    do_check_file(Master, FileName, Type),
    check_file_masters(FileName, Type, Masters);
check_file_masters(_FileName, _Type, []) ->
    ok.

%% Type == regular | directory
do_check_file(FileName, Type) ->
    case root_dir_relative_read_file_info(FileName) of
	{ok, Info} when Info#file_info.type==Type -> ok;
	{error, _Reason} -> throw({error, {no_such_file, FileName}})
    end.

do_check_file(Master, FileName, Type) ->
    case rpc:call(Master, ?MODULE, root_dir_relative_read_file_info, [FileName]) of
	{ok, Info} when Info#file_info.type==Type -> ok;
	_ -> throw({error, {no_such_file, {Master, FileName}}})
    end.

%%-----------------------------------------------------------------
%% If Rel doesn't exists in tar it could have been created
%% by the user in another way, i.e. ignore this here.
%%-----------------------------------------------------------------
extract_rel_file(Rel, Tar, Root) ->
    _ = erl_tar:extract(Tar, [{files, [Rel]}, {cwd, Root}, compressed]).

extract_tar(Root, Tar) ->
    case erl_tar:extract(Tar, [keep_old_files, {cwd, Root}, compressed]) of
	ok ->
	    ok;
	{error, {Name, Reason}} ->		% New erl_tar (R3A).
	    throw({error, {cannot_extract_file, Name, Reason}})
    end.

write_releases(Dir, Releases, Masters) ->
    %% We must never write 'current' to disk, since this will confuse
    %% us after a node restart - since we would then have a permanent
    %% release running, but state set to current for a non-running
    %% release.
    NewReleases = lists:zf(fun(Release) when Release#release.status == current ->
				   {true, Release#release{status = unpacked}};
			      (_) ->
				   true
			   end, Releases),
    write_releases_1(Dir, NewReleases, Masters).


write_releases_1(Dir, NewReleases, false) ->
    case do_write_release(Dir, "RELEASES", NewReleases) of
	ok    -> ok;
	Error -> throw(Error)
    end;
write_releases_1(Dir, NewReleases, Masters) ->
    all_masters(Masters),
    write_releases_m(Dir, NewReleases, Masters).

-doc false.
do_write_release(Dir, RELEASES, NewReleases) ->
    ReleasesFile = root_dir_relative_path(filename:join(Dir, RELEASES)),
    case file:open(ReleasesFile, [write,{encoding,utf8}]) of
	{ok, Fd} ->
	    ok = io:format(Fd, "%% ~s~n~tp.~n",
                           [epp:encoding_to_string(utf8),NewReleases]),
	    ok = file:close(Fd);
	{error, Reason} ->
	    {error, Reason}
    end.

%%-----------------------------------------------------------------
%% Write the "RELEASES" file at all master nodes.
%%   1. Save "RELEASES.backup" at all nodes.
%%   2. Save "RELEASES.change" at all nodes.
%%   3. Update the "RELEASES.change" file at all nodes.
%%   4. Move "RELEASES.change" to "RELEASES".
%%   5. Remove "RELEASES.backup" at all nodes.
%%
%% If one of the steps above fails, all steps is recovered from
%% (as long as possible), except for 5 which is allowed to fail.
%%-----------------------------------------------------------------
write_releases_m(Dir, NewReleases, Masters) ->
    RelFile = filename:join(Dir, "RELEASES"),
    Backup = filename:join(Dir, "RELEASES.backup"),
    Change = filename:join(Dir, "RELEASES.change"),
    ensure_RELEASES_exists(Masters, RelFile),
    case at_all_masters(Masters, ?MODULE, do_copy_files,
			[RelFile, [Backup, Change]]) of
	ok ->
	    case at_all_masters(Masters, ?MODULE, do_write_release,
				[Dir, "RELEASES.change", NewReleases]) of
		ok ->
		    case at_all_masters(Masters, file, rename,
					[Change, RelFile]) of
			ok ->
			    remove_files(all, [Backup, Change], Masters),
			    ok;
			{error, {Master, R}} ->
			    takewhile(Master, Masters, ?MODULE,
                                      root_dir_relative_rename_file,
				      [Backup, RelFile]),
			    remove_files(all, [Backup, Change], Masters),
			    throw({error, {Master, R, move_releases}})
		    end;
		{error, {Master, R}} ->
		    remove_files(all, [Backup, Change], Masters),
		    throw({error, {Master, R, update_releases}})
	    end;
	{error, {Master, R}} ->
	    remove_files(Master, [Backup, Change], Masters),
	    throw({error, {Master, R, backup_releases}})
    end.

ensure_RELEASES_exists(Masters, RelFile) ->
    case at_all_masters(Masters, ?MODULE, do_ensure_RELEASES, [RelFile]) of
	ok ->
	    ok;
	{error, {Master, R}} ->
	    throw({error, {Master, R, ensure_RELEASES_exists}})
    end.

copy_file(File, Dir, false) ->
    case do_copy_file(File, Dir) of
	ok    -> ok;
	Error -> throw(Error)
    end;
copy_file(File, Dir, Masters) ->
    all_masters(Masters),
    copy_file_m(File, Dir, Masters).

%%-----------------------------------------------------------------
%% copy File to Dir at every master node.
%% If an error occurs at a node, the total copy failed.
%% We do not have to cleanup in case of failure as this
%% copy_file is harmless.
%%-----------------------------------------------------------------
copy_file_m(File, Dir, [Master|Masters]) ->
    case rpc:call(Master, ?MODULE, do_copy_file, [File, Dir]) of
	ok                   -> copy_file_m(File, Dir, Masters);
	{error, {Reason, F}} -> throw({error, {Master, Reason, F}});
	Other                -> throw({error, {Master, Other, File}})
    end;
copy_file_m(_File, _Dir, []) ->
    ok.

-doc false.
do_copy_file(File, Dir) ->
    File2 = filename:join(Dir, filename:basename(File)),
    do_copy_file1(File, File2).

do_copy_file1(File, File2) ->
    case root_dir_relative_read_file(File) of
	{ok, Bin} ->
	    case root_dir_relative_write_file(File2, Bin) of
		ok -> ok;
		{error, Reason} ->
		    {error, {Reason, File2}}
	    end;
	{error, Reason} ->
	    {error, {Reason, File}}
    end.

%%-----------------------------------------------------------------
%% Copy File to a list of files.
%%-----------------------------------------------------------------
-doc false.
do_copy_files(File, [ToFile|ToFiles]) ->
    case do_copy_file1(File, ToFile) of
	ok    -> do_copy_files(File, ToFiles);
	Error -> Error
    end;
do_copy_files(_, []) ->
    ok.

%%-----------------------------------------------------------------
%% Copy each Src file to Dest file in the list of files.
%%-----------------------------------------------------------------
-doc false.
do_copy_files([{Src, Dest}|Files]) ->
    case do_copy_file1(Src, Dest) of
	ok    -> do_copy_files(Files);
	Error -> Error
    end;
do_copy_files([]) ->
    ok.

%%-----------------------------------------------------------------
%% Rename each Src file to Dest file in the list of files.
%%-----------------------------------------------------------------
-doc false.
do_rename_files([{Src1, Dest1}|Files]) ->
    Src = root_dir_relative_path(Src1),
    Dest = root_dir_relative_path(Dest1),
    case file:rename(Src, Dest) of
	ok    -> do_rename_files(Files);
	Error -> Error
    end;
do_rename_files([]) ->
    ok.

%%-----------------------------------------------------------------
%% Remove a list of files. Ignore failure.
%%-----------------------------------------------------------------
-doc false.
do_remove_files([File|Files]) ->
    _ = root_dir_relative_file_delete(File),
    do_remove_files(Files);
do_remove_files([]) ->
    ok.


%%-----------------------------------------------------------------
%% Ensure that the RELEASES file exists.
%% If not create an empty RELEASES file.
%%-----------------------------------------------------------------
-doc false.
do_ensure_RELEASES(RelFile) ->
    case root_dir_relative_read_file_info(RelFile) of
	{ok, _} -> ok;
	_       -> do_write_file(RelFile, "[]. ") 
    end.

%%-----------------------------------------------------------------
%% Make a directory, ignore failures (captured later).
%%-----------------------------------------------------------------
make_dir(Dir1, false) ->
    Dir = root_dir_relative_path(Dir1),
    _ = file:make_dir(Dir),
    ok;
make_dir(Dir, Masters) ->
    lists:foreach(fun(Master) -> rpc:call(Master,
                                          ?MODULE,
                                          root_dir_relative_make_dir,
                                          [Dir])
                  end,
		  Masters).

%%-----------------------------------------------------------------
%% Check that all masters are alive.
%%-----------------------------------------------------------------
all_masters(Masters) ->
    case rpc:multicall(Masters, erlang, info, [version]) of
	{_, []}       -> ok;
	{_, BadNodes} -> throw({error, {bad_masters, BadNodes}})
    end.

%%-----------------------------------------------------------------
%% Evaluate {M,F,A} at all masters.
%% {M,F,A} is supposed to return ok. Otherwise at_all_masters
%% returns {error, {Master, Other}}.
%%-----------------------------------------------------------------
at_all_masters([Master|Masters], M, F, A) ->
    case rpc:call(Master, M, F, A) of
	ok    -> at_all_masters(Masters, M, F, A);
	Error -> {error, {Master, Error}}
    end;
at_all_masters([], _, _, _) ->
    ok.

%%-----------------------------------------------------------------
%% Evaluate {M,F,A} at all masters until Master is found.
%% Ignore {M,F,A} return value.
%%-----------------------------------------------------------------
takewhile(Master, Masters, M, F, A) ->
    _ = lists:takewhile(fun(Ma) when Ma == Master ->
				false;
			   (Ma) ->
				rpc:call(Ma, M, F, A),
				true
			end, Masters),
    ok.

-doc false.
consult(File, false)   -> file:consult(root_dir_relative_path(File));
consult(File, Masters) -> consult_master(Masters, File).

%%-----------------------------------------------------------------
%% consult the File at any master node.
%% If the file does not exist at one node it should
%% not exist at any other node either.
%%-----------------------------------------------------------------
consult_master([Master|Ms], File) ->
    case rpc:call(Master, ?MODULE, consult, [File, false]) of
	{badrpc, _} -> consult_master(Ms, File);
	Res         -> Res
    end;
consult_master([], _File) ->
    {error, no_master}.

read_file(File, false) ->
    root_dir_relative_read_file(File);
read_file(File, Masters) ->
    read_master(Masters, File).

write_file(File, Data, false) ->
    case root_dir_relative_write_file(File, Data) of
	ok    -> ok;
	Error -> throw(Error)
    end;
write_file(File, Data, Masters) ->
    case at_all_masters(Masters, file, write_file, [File, Data]) of
	ok    -> ok;
	Error -> throw(Error)
    end.

ensure_dir(File, false) ->
    case root_dir_relative_ensure_dir(File) of
	ok -> ok;
	Error -> throw(Error)
    end;
ensure_dir(File, Masters) ->
    case at_all_masters(Masters,?MODULE,root_dir_relative_ensure_dir,[File]) of
	ok -> ok;
	Error -> throw(Error)
    end.

remove_dir(Dir, false) ->
    remove_file(Dir);
remove_dir(Dir, Masters) ->
    case at_all_masters(Masters,?MODULE,remove_file,[Dir]) of
	ok -> ok;
	Error -> throw(Error)
    end.


%% Ignore status of each delete !
remove_files(Master, Files, Masters) ->
    takewhile(Master, Masters, ?MODULE, do_remove_files, [Files]).

%%-----------------------------------------------------------------
%% read the File at any master node.
%% If the file does not exist at one node it should
%% not exist at any other node either.
%%-----------------------------------------------------------------
read_master([Master|Ms], File) ->
    case rpc:call(Master, ?MODULE, root_dir_relative_read_file, [File]) of
	{badrpc, _} -> read_master(Ms, File);
	Res         -> Res
    end;
read_master([], _File) ->
    {error, no_master}.

%%-----------------------------------------------------------------
%% Write start_erl.data.
%%-----------------------------------------------------------------
write_start(File, Data, false) ->
    case do_write_file(File, Data) of
	ok    -> ok;
	Error -> throw(Error)
    end;
write_start(File, Data, Masters) ->
    all_masters(Masters),
    safe_write_file_m(File, Data, Masters).


%%-----------------------------------------------------------------
%% Copy the "start.boot" and "sys.config" from SrcDir to DestDir at all
%% master nodes.
%%   1. Save DestDir/"start.backup" and DestDir/"sys.backup" at all nodes.
%%   2. Copy files at all nodes.
%%   3. Remove backup files at all nodes.
%%
%% If one of the steps above fails, all steps is recovered from
%% (as long as possible), except for 3 which is allowed to fail.
%%-----------------------------------------------------------------
set_static_files(SrcDir, DestDir, Masters) ->
    all_masters(Masters),
    Boot = "start.boot",
    Config = "sys.config",
    SrcBoot = filename:join(SrcDir, Boot),
    DestBoot = filename:join(DestDir, Boot),
    BackupBoot = filename:join(DestDir, "start.backup"),
    SrcConf = filename:join(SrcDir, Config),
    DestConf = filename:join(DestDir, Config),
    BackupConf = filename:join(DestDir, "sys.backup"),

    case at_all_masters(Masters, ?MODULE, do_copy_files,
			[[{DestBoot, BackupBoot},
			  {DestConf, BackupConf}]]) of
	ok ->
	    case at_all_masters(Masters, ?MODULE, do_copy_files,
				[[{SrcBoot, DestBoot},
				  {SrcConf, DestConf}]]) of
		ok ->
		    remove_files(all, [BackupBoot, BackupConf], Masters),
		    ok;
		{error, {Master, R}} ->
		    takewhile(Master, Masters, ?MODULE, do_rename_files,
			      [{BackupBoot, DestBoot},
			       {BackupConf, DestConf}]),
		    remove_files(all, [BackupBoot, BackupConf], Masters),
		    throw({error, {Master, R, copy_start_config}})
	    end;
	{error, {Master, R}} ->
	    remove_files(Master, [BackupBoot, BackupConf], Masters),
	    throw({error, {Master, R, backup_start_config}})
    end.

%%-----------------------------------------------------------------
%% Write erl.ini
%% Writes the erl.ini file used by erl.exe when (re)starting the erlang node.
%% At first installation, this is done by Install.exe, which means that if
%% the format of this file for some reason is changed, then Install.c must
%% also be updated (and probably some other c-files which read erl.ini)
%%-----------------------------------------------------------------
write_ini_file(RootDir,EVsn,Masters) ->
   BinDir = filename:join([RootDir,"erts-"++EVsn,"bin"]),
   Str0 = io_lib:format("[erlang]~n"
                        "Bindir=~ts~n"
                        "Progname=erl~n"
                        "Rootdir=~ts~n",
		        [filename:nativename(BinDir),
		         filename:nativename(RootDir)]),
   Str = re:replace(Str0,"\\\\","\\\\\\\\",[{return,list},global,unicode]),
   IniFile = filename:join(BinDir,"erl.ini"),
   do_write_ini_file(IniFile,Str,Masters).

do_write_ini_file(File,Data,false) ->
    case do_write_file(File, Data, [{encoding,utf8}]) of
	ok    -> ok;
	Error -> throw(Error)
    end;
do_write_ini_file(File,Data,Masters) ->
    all_masters(Masters),
    safe_write_file_m(File, Data, [{encoding,utf8}], Masters).


%%-----------------------------------------------------------------
%% Write the given file at all master nodes.
%%   1. Save <File>.backup at all nodes.
%%   2. Write <File>.change at all nodes.
%%   3. Move <File>.change to <File>
%%   4. Remove <File>.backup at all nodes.
%%
%% If one of the steps above fails, all steps are recovered from
%% (as long as possible), except for 4 which is allowed to fail.
%%-----------------------------------------------------------------
safe_write_file_m(File, Data, Masters) ->
    safe_write_file_m(File, Data, [], Masters).
safe_write_file_m(File, Data, FileOpts, Masters) ->
    Backup = File ++ ".backup",
    Change = File ++ ".change",
    case at_all_masters(Masters, ?MODULE, do_copy_files,
			[File, [Backup]]) of
	ok ->
	    case at_all_masters(Masters, ?MODULE, do_write_file,
				[Change, Data, FileOpts]) of
		ok ->
		    case at_all_masters(Masters, file, rename,
					[Change, File]) of
			ok ->
			    remove_files(all, [Backup, Change], Masters),
			    ok;
			{error, {Master, R}} ->
			    takewhile(Master, Masters, file, rename,
				      [Backup, File]),
			    remove_files(all, [Backup, Change], Masters),
			    throw({error, {Master, R, rename,
					   filename:basename(Change),
					   filename:basename(File)}})
		    end;
		{error, {Master, R}} ->
		    remove_files(all, [Backup, Change], Masters),
		    throw({error, {Master, R, write, filename:basename(Change)}})
	    end;
	{error, {Master, R}} ->
	    remove_files(Master, [Backup], Masters),
	    throw({error, {Master, R, backup,
			   filename:basename(File),
			   filename:basename(Backup)}})
    end.

%%-----------------------------------------------------------------
%% Figure out which applications that have changed version between the
%% two releases. The paths for these applications must always be
%% updated, even if the relup script does not load any modules. See
%% OTP-9402.
%%
%% A different situation is when the same application version is used
%% in old and new release, but the path has changed. This is not
%% handled here - instead it must be explicitly indicated by the
%% 'update_paths' option to release_handler:install_release/2 if the
%% code path shall be updated then.
%% -----------------------------------------------------------------
get_new_libs([{App,Vsn,_LibDir}|CurrentLibs], NewLibs) ->
    case lists:keyfind(App,1,NewLibs) of
	{App,NewVsn,_} = LibInfo when NewVsn =/= Vsn ->
	    [LibInfo | get_new_libs(CurrentLibs,NewLibs)];
	_ ->
	    get_new_libs(CurrentLibs,NewLibs)
    end;
get_new_libs([],_) ->
    [].

%%-----------------------------------------------------------------
%% Return a list of releases witch a specific status
%%-----------------------------------------------------------------
get_releases_with_status([], _, Acc) ->
    Acc;
get_releases_with_status([ {_, _, _, ReleaseStatus } = Head | Tail],
                         Status, Acc) when ReleaseStatus == Status ->
    get_releases_with_status(Tail, Status, [Head | Acc]);
get_releases_with_status([_ | Tail], Status, Acc) ->
    get_releases_with_status(Tail, Status, Acc).

root_dir_relative_read_link_info(File) ->
    file:read_link_info(root_dir_relative_path(File)).

root_dir_relative_list_dir(File) ->
    file:list_dir(root_dir_relative_path(File)).

root_dir_relative_file_delete(File) ->
    file:delete(root_dir_relative_path(File)).

root_dir_relative_dir_delete(File) ->
    file:del_dir(root_dir_relative_path(File)).

root_dir_relative_path(Pathname) ->
    case filename:pathtype(Pathname) of
        relative ->
            filename:join(code:root_dir(), Pathname);
        _ ->
            Pathname
    end.

root_dir_relative_write_file(File, Bin) ->
    file:write_file(root_dir_relative_path(File), Bin).

%% The root_dir_relative* functions below are exported so that they
%% can be called on other nodes with rpc

-doc false.
root_dir_relative_read_file_info(Path) ->
    file:read_file_info(root_dir_relative_path(Path)).

-doc false.
root_dir_relative_read_file(File) ->
    file:read_file(root_dir_relative_path(File)).

-doc false.
root_dir_relative_rename_file(Source1, Destination1) ->
    Source = root_dir_relative_path(Source1),
    Destination = root_dir_relative_path(Destination1),
    file:rename(Source, Destination).

-doc false.
root_dir_relative_make_dir(Dir) ->
    file:make_dir(root_dir_relative_path(Dir)).

-doc false.
root_dir_relative_ensure_dir(Dir) ->
    filelib:ensure_dir(root_dir_relative_path(Dir)).


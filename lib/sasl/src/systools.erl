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
-module(systools).
-moduledoc """
A Set of Release Handling Tools

This module contains functions to generate boot scripts (`.boot`, `.script`), a
release upgrade file (`relup`), and release packages.

## See Also

[`app(4)`](`e:kernel:app.md`), [`appup(4)`](appup.md),
[`erl(1)`](`e:erts:erl_cmd.md`), [`rel(4)`](rel.md), `m:release_handler`,
[`relup(4)`](relup.md), [`script(4)`](script.md)
""".

%% Usage:
%%    systools:make_script("RelName")     
%%                        Make a boot file from RelName.rel.
%%                        Generates RelName.{script,boot}
%%    systools:make_tar("RelName")     
%%                        Make a release package from RelName.rel.
%%                        Generates RelName.tar,Z
%%    systools:script2boot(File)
%%                        File.script -> File.boot
%%    systools:mk_relup("Target", ["UpFromRel"...], ["DownToRel"...], Opts)
%%			  Gather all relup scripts to the relup file
%%

-export([script2boot/1, script2boot/3, compile_rel/3,
	 make_script/1, make_script/2,
	 make_tar/1, make_tar/2,
	 make_relup/3, make_relup/4]).

-include("erl_compile.hrl").

%%% The behaviour_info functions have been moved to erl_internal in stdlib.

%%-----------------------------------------------------------------
%% Options is a list of {path, Path} | silent | local where path sets
%% the search path, silent suppresses error message printing on console,
%% local generates a script with references to the directories there
%% the applications are found.
%%-----------------------------------------------------------------
-doc(#{equiv => make_script/2}).
-spec make_script(Name) -> Result when
      Name :: string(),
      Result :: ok | error | {ok,Module,Warnings} | {error,Module,Error},
      Module :: atom(),
      Warnings :: term(),
      Error :: term().
make_script([RelName|Opts]) when is_atom(RelName) ->
    systools_make:make_script([RelName], Opts);
make_script(RelName) -> make_script(RelName, []).

-doc """
make_script(Name, [Opt])

Generates a boot script `Name.script` and its binary version, the boot file
`Name.boot`, unless the `{script_name, ScriptName}` option is given, in which
case the names are `ScriptName.script` and `ScriptName.boot`.

The boot file specifies which code to be loaded and which
applications to be started when the Erlang runtime system is
started. See [`script(4)`](script.md).

The release resource file `Name.rel` is read to determine which applications are
included in the release. Then the relevant application resource files `App.app`
are read to determine which modules to be loaded, and if and how the
applications are to be started. (Keys `modules` and `mod`, see
[`app(4)`](`e:kernel:app.md`).

By default, the boot script and boot file are located in the same directory as
`Name.rel`. That is, in the current working directory unless `Name` contains a
path. If option `{outdir,Dir}` is specified, they are located in `Dir` instead.

The correctness of each application is checked as follows:

- The version of an application specified in the `.rel` file is to be the same
  as the version specified in the `.app` file.
- There are to be no undefined applications, that is, dependencies to
  applications that are not included in the release. (Key `applications` in the
  `.app` file).
- There are to be no circular dependencies among the applications.
- There are to be no duplicated modules, that is, modules with the same name but
  belonging to different applications.
- If option `src_tests` is specified, a warning is issued if the source code for
  a module is missing or is newer than the object code.

The applications are sorted according to the dependencies between the
applications. Where there are no dependencies, the order in the `.rel` file is
kept.

The function fails if the mandatory applications Kernel and STDLIB are not
included in the `.rel` file and have start type `permanent` (which is default).

If SASL is not included as an application in the `.rel` file, a warning is
issued because such a release cannot be used in an upgrade. To turn off this
warning, add option `no_warn_sasl`.

All files are searched for in the current path. It is assumed that the `.app`
and `.beam` files for an application are located in the same directory. The
`.erl` files are also assumed to be located in this directory, unless it is an
`ebin` directory in which case they can be located in the corresponding `src`
directory.

If option `{path,[Dir]}` is specified, this path is appended to the current
path. A directory in the path can be specified with a wildcard `*`, this is
expanded to all matching directories. Example: `"lib/*/ebin"`.

In the generated boot script all application directories are structured as
`App-Vsn/ebin`. They are assumed to be located in `$ROOT/lib`, where `$ROOT` is
the root directory of the installed release. If option `local` is specified, the
actual directories where the applications were found are used instead. This is a
useful way to test a generated boot script locally.

Option `variables` can be used to specify an installation directory other than
`$ROOT/lib` for some of the applications. If a variable `{VarName,Prefix}` is
specified and an application is found in a directory
`Prefix/Rest/App[-Vsn]/ebin`, this application gets the path
`VarName/Rest/App-Vsn/ebin` in the boot script. If an application is found in a
directory `Prefix/Rest`, the path is `VarName/Rest/App-Vsn/ebin`. When starting
Erlang, all variables `VarName` are given values using command-line flag
`boot_var`.

_Example:_ If option `{variables,[{"TEST","lib"}]}` is specified and `myapp.app`
is found in `lib/myapp/ebin`, the path to this application in the boot script is
`"$TEST/myapp-1/ebin"`. If `myapp.app` is found in `lib/test`, the path is
`$TEST/test/myapp-1/ebin`.

The checks performed before the boot script is generated can be extended with
some cross reference checks by specifying option `exref`. These checks are
performed with the Xref tool. All applications, or the applications specified
with `{exref,[App]}`, are checked by Xref and warnings are issued for calls to
undefined functions.

By default, errors and warnings are printed to tty and the function returns `ok`
or `error`. If option `silent` is specified, the function instead returns
`{ok,Module,Warnings}` or `{error,Module,Error}`. Warnings and errors can be
converted to strings by calling `Module:format_warning(Warnings)` or
`Module:format_error(Error)`.

If option `warnings_as_errors` is specified, warnings are treated as errors.

If option `no_dot_erlang` is specified, the instruction to load the `.erlang`
file during boot is _not_ included.
""".
-spec make_script(Name, [Opt]) -> Result when
      Name :: string(),
      Opt :: src_tests | {path,[Dir]} | local | {variables,[Var]} | exref | {exref,[App]} |
             silent | {outdir,Dir} | no_dot_erlang | no_warn_sasl | warnings_as_errors | {script_name, Name},
      Dir :: string(),
      Var :: {VarName,Prefix},
      VarName :: string(),
      Prefix :: string(),
      App :: atom(),
      Result :: ok | error | {ok,Module,Warnings} | {error,Module,Error},
      Module :: atom(),
      Warnings :: term(),
      Error :: term().
make_script(RelName, Opt) ->
    systools_make:make_script(RelName, Opt).

%%-----------------------------------------------------------------
%% Options is a list of {path, Path} | silent |
%%    {dirs, [src,include,examples,..]} | {erts, ErtsDir} where path
%% sets the search path, silent suppresses error message printing on console,
%% dirs includes the specified directories (per application) in the
%% release package and erts specifies that the erts-Vsn/bin directory
%% should be included in the release package and there it can be found.
%%-----------------------------------------------------------------
-doc(#{equiv => make_tar(Name, [])}).
-spec make_tar(Name) -> Result when
      Name :: string(),
      Result :: ok | error | {ok, Module :: module(), Warnings :: term()} |
                {error, Module :: module(), Error :: term()}.
make_tar(RelName) -> make_tar(RelName, []).

-doc """
Creates a release package file `Name.tar.gz`.

This file must be uncompressed and unpacked on the target system using
`m:release_handler` before the new release can be installed.

The release resource file `Name.rel` is read to determine which applications are
included in the release. Then the relevant application resource files `App.app`
are read to determine the version and modules of each application (keys `vsn`
and `modules`, see [`app(4)`](`e:kernel:app.md`)).

By default, the release package file is located in the same directory as
`Name.rel`. That is, in the current working directory unless `Name` contains a
path. If option `{outdir,Dir}` is specified, it is located in `Dir` instead.

If SASL is not included as an application in the `.rel` file, a warning is
issued because such a release cannot be used in an upgrade. To turn off this
warning, add option `no_warn_sasl`.

By default, the release package contains the directories `lib/App-Vsn/ebin` and
`lib/App-Vsn/priv` for each included application. If more directories are to be
included, option `dirs` is specified, for example, `{dirs,[src,examples]}`.

All these files are searched for in the current path. If option `{path,[Dir]}`
is specified, this path is appended to the current path. Wildcard `*` is
expanded to all matching directories. Example: `"lib/*/ebin"`.

If the `{extra_files, ExtraFiles}` option is given then the `ExtraFiles` are
added to the tarball after everything else to be included has been added. The
`ExtraFiles` list is a list of files or directories in the same format as the
`add_type()` tuple for [erl_tar:add/3,4](`erl_tar:add/3`)

Option `variables` can be used to specify an installation directory other than
`lib` for some of the applications. If variable `{VarName,Prefix}` is specified
and an application is found in directory `Prefix/Rest/App[-Vsn]/ebin`, this
application is packed into a separate `VarName.tar.gz` file as
`Rest/App-Vsn/ebin`.

_Example:_ If option `{variables,[{"TEST","lib"}]}` is specified and `myapp.app`
is located in `lib/myapp-1/ebin`, application `myapp` is included in
`TEST.tar.gz`:

```text
% tar tf TEST.tar
myapp-1/ebin/myapp.app
...
```

Option `{var_tar,VarTar}` can be used to specify if and where a separate package
is to be stored. In this option `VarTar` is one of the following:

- **`include`** - Each separate (variable) package is included in the main
  `ReleaseName.tar.gz` file. This is the default.

- **`ownfile`** - Each separate (variable) package is generated as a separate
  file in the same directory as the `ReleaseName.tar.gz` file.

- **`omit`** - No separate (variable) packages are generated. Applications that
  are found underneath a variable directory are ignored.

A directory `releases` is also included in the release package, containing
`Name.rel` and a subdirectory `RelVsn`. `RelVsn` is the release version as
specified in `Name.rel`.

`releases/RelVsn` contains the boot script `Name.boot` renamed to `start.boot`
and, if found, the files `relup` and `sys.config` or `sys.config.src`. These
files are searched for in the same directory as `Name.rel`, in the current
working directory, and in any directories specified using option `path`. In the
case of `sys.config` it is not included if `sys.config.src` is found.

If the release package is to contain a new Erlang runtime system, the
`erts-ErtsVsn/bin` directory of the specified runtime system `{erts,Dir}` is
copied to `erts-ErtsVsn/bin`. Some erts executables are not copied by default,
if you want to include all executables you can give the `erts_all` option.

All checks with function [`make_script`](`make_script/1`) are performed before
the release package is created. Options `src_tests` and `exref` are also valid
here.

The return value and the handling of errors and warnings are the same as
described for [`make_script`](`make_script/1`).
""".
-spec make_tar(Name, Opts) -> Result when
      Name :: string(),
      Opts :: [Opt],
      Opt :: {dirs,[IncDir]} | {path,[Dir]} |
             {variables,[Var]} | {var_tar,VarTar} |
             {erts,Dir} | erts_all | src_tests | exref |
             {exref,[App]} | silent | {outdir,Dir} |
             no_warn_sasl | warnings_as_errors |
             {extra_files, ExtraFiles},
      Dir :: file:filename_all(),
      IncDir :: src | include | atom(),
      Var :: {VarName,PreFix},
      VarName :: string(),
      PreFix :: string(),
      VarTar :: include | ownfile | omit,
      App :: atom(),
      Result :: ok | error | {ok, Module :: module(), Warnings :: term()} |
                {error, Module :: module(), Error :: term()},
      ExtraFiles :: [{NameInArchive, file:filename_all()}],
      NameInArchive :: string().
make_tar(RelName, Opt) ->
    systools_make:make_tar(RelName, Opt).

%%-----------------------------------------------------------------
%% Create a binary form of a boot script.
%%-----------------------------------------------------------------
-doc """
script2boot(File)

This function transforms the `File.script` boot script to a binary
term, which is stored in the `File.boot` file.

The Erlang runtime system requires that the contents of the script
used to boot the system is a binary Erlang term.

A boot script generated using [`make_script`](`make_script/1`) is already
transformed to the binary form.
""".
-spec script2boot(File) -> ok | error when File :: string().
script2boot(File) ->
    case systools_lib:file_term2binary(File ++ ".script", File ++ ".boot") of
	{error,Error} ->
	    io:format("~ts", [systools_make:format_error(Error)]),
	    error;
	_ ->
	    ok
    end.

-doc false.
script2boot(File, Output0, _Opt) ->
    Input = File++".script",
    Output = Output0++".boot",
    case systools_lib:file_term2binary(Input, Output) of
	{error,Error} ->
	    io:format("~ts", [systools_make:format_error(Error)]),
	    error;
	_ ->
	    ok
    end.

%%-----------------------------------------------------------------
%% Options is a list of {path, Path} | silent | noexec where path sets
%% search path, silent suppresses error message printing on console,
%% noexec suppresses writing the output "relup" file
%%-----------------------------------------------------------------
-doc(#{equiv => make_relup(Name, UpFrom, DownTo, [])}).
-spec make_relup(Name, UpFrom, DownTo) -> Result when Name :: string(),
   UpFrom :: [Name | {Name,Descr}],
   DownTo :: [Name | {Name,Descr}],
    Descr :: term(),
   Result :: ok | error | {ok,Relup :: term(),Module,Warnings} | {error,Module,Error},
    Module :: atom(),
    Warnings :: term(),
   Error :: term().
make_relup(ReleaseName, UpNameList, DownNameList) ->
    systools_relup:mk_relup(ReleaseName, UpNameList, DownNameList, []).
-doc """
make_relup(Name, UpFrom, DownTo, [Opt])

Generates a release upgrade file `relup` containing instructions for upgrading
from or downgrading to one or more previous releases.

The instructions are used by `m:release_handler` when installing a new
version of a release in runtime.

By default, the `relup` file is located in the current working directory. If option
`{outdir,Dir}` is specified, the `relup` file is located in directory `Dir` instead.

The release resource file `Name.rel` is compared with all release resource files
`Name2.rel`, specified in `UpFrom` and `DownTo`. For each such pair, the
following is deducted:

- Which applications to be deleted, that is, applications listed in `Name.rel`
  but not in `Name2.rel`
- Which applications to be added, that is, applications listed in `Name2.rel`
  but not in `Name.rel`
- Which applications to be upgraded/downgraded, that is, applications listed in
  both `Name.rel` and `Name2.rel` but with different versions
- If the emulator needs to be restarted after upgrading or downgrading, that is,
  if the ERTS version differs between `Name.rel` and `Name2.rel`

Instructions for this are added to the `relup` file in the above order.
Instructions for upgrading or downgrading between application versions are
fetched from the relevant application upgrade files `App.appup`, sorted in the
same order as when generating a boot script, see
[`make_script/1,2`](`make_script/2`). High-level instructions are translated
into low-level instructions and the result is printed to the `relup` file.

The optional `Descr` parameter is included "as is" in the `relup` file, see
[`relup(4)`](relup.md). Defaults to the empty list.

All the files are searched for in the code path. It is assumed that the `.app`
and `.appup` files for an application are located in the same directory.

If option `{path,[Dir]}` is specified, this path is appended to the current
path. Wildcard `*` is expanded to all matching directories, for example,
`lib/*/ebin`.

If option `restart_emulator` is specified, a low-level instruction to restart
the emulator is appended to the `relup` file. This ensures that a complete
reboot of the system is done when the system is upgraded or downgraded.

If an upgrade includes a change from an emulator earlier than OTP R15 to OTP R15
or later, the warning `pre_R15_emulator_upgrade` is issued. For more information
about this, see [Design Principles](`e:system:appup_cookbook.md`) in _System
Documentation_.

By default, errors and warnings are printed to tty and the function returns `ok`
or `error`. If option `silent` is specified, the function instead either returns
`{ok,Relup,Module,Warnings}`, where `Relup` is the release upgrade file, or
`{error,Module,Error}`. Warnings and errors can be converted to strings by
calling `Module:format_warning(Warnings)` or `Module:format_error(Error)`.

If option `noexec` is specified, the function returns the same values as for
`silent` but no `relup` file is created.

If option `warnings_as_errors` is specified, warnings are treated as errors.
""".
-spec make_relup(Name, UpFrom, DownTo, [Opt]) -> Result
                    when
                        Name :: string(),
                        UpFrom :: [Name | {Name, Descr}],
                        DownTo :: [Name | {Name, Descr}],
                        Descr :: term(),
                        Opt ::
                            {path, [Dir]} |
                            restart_emulator | silent | noexec |
                            {outdir, Dir} |
                            warnings_as_errors,
                        Dir :: string(),
                        Result ::
                            ok | error |
                            {ok, Relup :: term(), Module, Warnings} |
                            {error, Module, Error},
                        Module :: atom(),
                        Warnings :: term(),
                        Error :: term().
make_relup(ReleaseName, UpNameList, DownNameList, Opts) ->
    systools_relup:mk_relup(ReleaseName, UpNameList, DownNameList, Opts).

%%-----------------------------------------------------------------
%% Interface for erl_compile to compile .rel files.
%%-----------------------------------------------------------------
-doc false.
compile_rel(Input, Output, Options) ->
    systools_make:make_script(Input, Output, translate_options(Options)).

translate_options(Opts) ->
    [{path, Opts#options.includes}|Opts#options.specific].

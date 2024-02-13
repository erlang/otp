<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Creating and Upgrading a Target System

[](){: #creating-upgrading-target-system }

When creating a system using Erlang/OTP, the simplest way is to install
Erlang/OTP somewhere, install the application-specific code somewhere else, and
then start the Erlang runtime system, making sure the code path includes the
application-specific code.

It is often not desirable to use an Erlang/OTP system as is. A developer can
create new Erlang/OTP-compliant applications for a particular purpose, and
several original Erlang/OTP applications can be irrelevant for the purpose in
question. Thus, there is a need to be able to create a new system based on a
given Erlang/OTP system, where dispensable applications are removed and new
applications are included. Documentation and source code is irrelevant and is
therefore not included in the new system.

This chapter is about creating such a system, which is called a _target system_.

The following sections deal with target systems with different requirements of
functionality:

- A _basic target system_ that can be started by calling the ordinary `erl`
  script.
- A _simple target system_ where also code replacement in runtime can be
  performed.
- An _embedded target system_ where there is also support for logging output
  from the system to file for later inspection, and where the system can be
  started automatically at boot time.

Here is only considered the case when Erlang/OTP is running on a UNIX system.

The `sasl` application includes the example Erlang module `target_system.erl`,
which contains functions for creating and installing a target system. This
module is used in the following examples. The source code of the module is
listed in
[Listing of target_system.erl](create_target.md#listing-of-target-system)

[](){: #create }

## Creating a Target System

It is assumed that you have a working Erlang/OTP system structured according to
the OTP design principles.

_Step 1._ Create a `.rel` file (see the [rel(4)](`e:sasl:rel.md`) manual page in
SASL), which specifies the ERTS version and lists all applications that are to
be included in the new basic target system. An example is the following
`mysystem.rel` file:

```erlang
%% mysystem.rel
{release,
 {"MYSYSTEM", "FIRST"},
 {erts, "5.10.4"},
 [{kernel, "2.16.4"},
  {stdlib, "1.19.4"},
  {sasl, "2.3.4"},
  {pea, "1.0"}]}.
```

The listed applications are not only original Erlang/OTP applications but
possibly also new applications that you have written (here exemplified by the
application Pea (`pea`)).

_Step 2._ Start Erlang/OTP from the directory where the `mysystem.rel` file
resides:

```text
os> erl -pa /home/user/target_system/myapps/pea-1.0/ebin
```

Here also the path to the `pea-1.0` ebin directory is provided.

_Step 3._ Create the target system:

```text
1> target_system:create("mysystem").
```

The function `target_system:create/1` performs the following:

1. Reads the file `mysystem.rel` and creates a new file `plain.rel` that is
   identical to the former, except that it only lists the Kernel and STDLIB
   applications.
1. From the files `mysystem.rel` and `plain.rel` creates the files
   `mysystem.script`, `mysystem.boot`, `plain.script`, and `plain.boot` through
   a call to `systools:make_script/2`.
1. Creates the file `mysystem.tar.gz` by a call to `systools:make_tar/2`. That
   file has the following contents:

```text
erts-5.10.4/bin/
releases/FIRST/start.boot
releases/FIRST/mysystem.rel
releases/mysystem.rel
lib/kernel-2.16.4/
lib/stdlib-1.19.4/
lib/sasl-2.3.4/
lib/pea-1.0/
```

The file `releases/FIRST/start.boot` is a copy of our `mysystem.boot`

The release resource file `mysystem.rel` is duplicated in the tar file.
Originally, this file was only stored in the `releases` directory to make it
possible for the `release_handler` to extract this file separately. After
unpacking the tar file, `release_handler` would automatically copy the file to
`releases/FIRST`. However, sometimes the tar file is unpacked without involving
the `release_handler` (for example, when unpacking the first target system). The
file is therefore now instead duplicated in the tar file so no manual copying is
needed.

1. Creates the temporary directory `tmp` and extracts the tar file
   `mysystem.tar.gz` into that directory.
1. Deletes the files `erl` and `start` from `tmp/erts-5.10.4/bin`. These files
   are created again from source when installing the release.
1. Creates the directory `tmp/bin`.
1. Copies the previously created file `plain.boot` to `tmp/bin/start.boot`.
1. Copies the files `epmd`, `run_erl`, and `to_erl` from the directory
   `tmp/erts-5.10.4/bin` to the directory `tmp/bin`.
1. Creates the directory `tmp/log`, which is used if the system is started as
   embedded with the `bin/start` script.
1. Creates the file `tmp/releases/start_erl.data` with the contents "5.10.4
   FIRST". This file is to be passed as data file to the `start_erl` script.
1. Recreates the file `mysystem.tar.gz` from the directories in the directory
   `tmp` and removes `tmp`.

## Installing a Target System

_Step 4._ Install the created target system in a suitable directory.

```text
2> target_system:install("mysystem", "/usr/local/erl-target").
```

The function `target_system:install/2` performs the following:

1. Extracts the tar file `mysystem.tar.gz` into the target directory
   `/usr/local/erl-target`.
1. In the target directory reads the file `releases/start_erl.data` to find the
   Erlang runtime system version ("5.10.4").
1. Substitutes `%FINAL_ROOTDIR%` and `%EMU%` for `/usr/local/erl-target` and
   `beam`, respectively, in the files `erl.src`, `start.src`, and
   `start_erl.src` of the target `erts-5.10.4/bin` directory, and puts the
   resulting files `erl`, `start`, and `run_erl` in the target `bin` directory.
1. Finally the target `releases/RELEASES` file is created from data in the file
   `releases/mysystem.rel`.

[](){: #start }

## Starting a Target System

Now we have a target system that can be started in various ways. We start it as
a _basic target system_ by invoking:

```text
os> /usr/local/erl-target/bin/erl
```

Here only the Kernel and STDLIB applications are started, that is, the system is
started as an ordinary development system. Only two files are needed for all
this to work:

1. `bin/erl` (obtained from `erts-5.10.4/bin/erl.src`)
1. `bin/start.boot` (a copy of `plain.boot`)

We can also start a distributed system (requires `bin/epmd`).

To start all applications specified in the original `mysystem.rel` file, use
flag `-boot` as follows:

```text
os> /usr/local/erl-target/bin/erl -boot /usr/local/erl-target/releases/FIRST/start
```

We start a _simple target system_ as above. The only difference is that also the
file `releases/RELEASES` is present for code replacement in runtime to work.

To start an _embedded target system_, the shell script `bin/start` is used. The
script calls `bin/run_erl`, which in turn calls `bin/start_erl` (roughly,
`start_erl` is an embedded variant of `erl`).

The shell script `start`, which is generated from erts-5.10.4/bin/start.src
during installation, is only an example. Edit it to suite your needs. Typically
it is executed when the UNIX system boots.

`run_erl` is a wrapper that provides logging of output from the runtime system
to file. It also provides a simple mechanism for attaching to the Erlang shell
(`to_erl`).

`start_erl` requires:

1. The root directory (`"/usr/local/erl-target"`)
1. The releases directory (`"/usr/local/erl-target/releases"`
1. The location of the file `start_erl.data`

It performs the following:

1. Reads the runtime system version (`"5.10.4"`) and release version (`"FIRST"`)
   from the file `start_erl.data`.
1. Starts the runtime system of the version found.
1. Provides the flag `-boot` specifying the boot file of the release version
   found (`"releases/FIRST/start.boot"`).

`start_erl` also assumes that there is `sys.config` in the release version
directory (`"releases/FIRST/sys.config"`). That is the topic of the next
section.

The `start_erl` shell script is normally not to be altered by the user.

## System Configuration Parameters

As was mentioned in the previous section, `start_erl` requires a `sys.config` in
the release version directory (`"releases/FIRST/sys.config"`). If there is no
such file, the system start fails. Such a file must therefore also be added.

If you have system configuration data that is neither file-location-dependent
nor site-dependent, it can be convenient to create `sys.config` early, so it
becomes part of the target system tar file created by `target_system:create/1`.
In fact, if you in the current directory create not only the file
`mysystem.rel`, but also file `sys.config`, the latter file is tacitly put in
the appropriate directory.

However, it can also be convenient to replace variables in within a `sys.config`
on the target after unpacking but before running the release. If you have a
`sys.config.src` it will be included and is not required to be a valid Erlang
term file like `sys.config`. Before running the release you must have a valid
`sys.config` in the same directory, so using `sys.config.src` requires having
some tool to populate what is needed and write `sys.config` to disk before
booting the release.

## Differences From the Install Script

The previous `install/2` procedure differs somewhat from that of the ordinary
`Install` shell script. In fact, `create/1` makes the release package as
complete as possible, and leave to the `install/2` procedure to finish by only
considering location-dependent files.

## Creating the Next Version

In this example the Pea application has been changed, and so are the
applications ERTS, Kernel, STDLIB and SASL.

_Step 1._ Create the file `.rel`:

```erlang
%% mysystem2.rel
{release,
 {"MYSYSTEM", "SECOND"},
 {erts, "6.0"},
 [{kernel, "3.0"},
  {stdlib, "2.0"},
  {sasl, "2.4"},
  {pea, "2.0"}]}.
```

_Step 2._ Create the application upgrade file (see the
[appup(4)](`e:sasl:appup.md`) manual page in SASL) for Pea, for example:

```erlang
%% pea.appup
{"2.0",
 [{"1.0",[{load_module,pea_lib}]}],
 [{"1.0",[{load_module,pea_lib}]}]}.
```

_Step 3._ From the directory where the file `mysystem2.rel` resides, start the
Erlang/OTP system, giving the path to the new version of Pea:

```text
os> erl -pa /home/user/target_system/myapps/pea-2.0/ebin
```

_Step 4._ Create the release upgrade file (see the [relup(4)](`e:sasl:relup.md`)
manual page in SASL):

```text
1> systools:make_relup("mysystem2",["mysystem"],["mysystem"],
    [{path,["/home/user/target_system/myapps/pea-1.0/ebin",
    "/my/old/erlang/lib/*/ebin"]}]).
```

Here `"mysystem"` is the base release and `"mysystem2"` is the release to
upgrade to.

The `path` option is used for pointing out the old version of all applications.
(The new versions are already in the code path - assuming of course that the
Erlang node on which this is executed is running the correct version of
Erlang/OTP.)

_Step 5._ Create the new release:

```text
2> target_system:create("mysystem2").
```

Given that the file `relup` generated in Step 4 is now located in the current
directory, it is automatically included in the release package.

## Upgrading the Target System

This part is done on the target node, and for this example we want the node to
be running as an embedded system with the `-heart` option, allowing automatic
restart of the node. For more information, see
[Starting a Target System](create_target.md#start).

We add `-heart` to `bin/start`:

```text
#!/bin/sh
ROOTDIR=/usr/local/erl-target/

if [ -z "$RELDIR" ]
then
   RELDIR=$ROOTDIR/releases
fi

START_ERL_DATA=${1:-$RELDIR/start_erl.data}

$ROOTDIR/bin/run_erl -daemon /tmp/ $ROOTDIR/log "exec $ROOTDIR/bin/start_erl $ROOTDIR\
$RELDIR $START_ERL_DATA -heart"
```

We use the simplest possible `sys.config`, which we store in `releases/FIRST`:

```text
%% sys.config
[].
```

Finally, to prepare the upgrade, we must put the new release package in the
`releases` directory of the first target system:

```text
os> cp mysystem2.tar.gz /usr/local/erl-target/releases
```

Assuming that the node has been started as follows:

```text
os> /usr/local/erl-target/bin/start
```

It can be accessed as follows:

```text
os> /usr/local/erl-target/bin/to_erl /tmp/erlang.pipe.1
```

Logs can be found in `/usr/local/erl-target/log`. This directory is specified as
an argument to `run_erl`in the start script listed above.

_Step 1._ Unpack the release:

```text
1> {ok,Vsn} = release_handler:unpack_release("mysystem2").
```

_Step 2._ Install the release:

```text
2> release_handler:install_release(Vsn).
{continue_after_restart,"FIRST",[]}
heart: Tue Apr  1 12:15:10 2014: Erlang has closed.
heart: Tue Apr  1 12:15:11 2014: Executed "/usr/local/erl-target/bin/start /usr/local/erl-target/releases/new_start_erl.data" -> 0. Terminating.
[End]
```

The above return value and output after the call to
`release_handler:install_release/1` means that the `release_handler` has
restarted the node by using `heart`. This is always done when the upgrade
involves a change of the applications ERTS, Kernel, STDLIB, or SASL. For more
information, see [Upgrade when Erlang/OTP has Changed](upgrade.md).

The node is accessible through a new pipe:

```text
os> /usr/local/erl-target/bin/to_erl /tmp/erlang.pipe.2
```

Check which releases there are in the system:

```c
1> release_handler:which_releases().
[{"MYSYSTEM","SECOND",
  ["kernel-3.0","stdlib-2.0","sasl-2.4","pea-2.0"],
  current},
 {"MYSYSTEM","FIRST",
  ["kernel-2.16.4","stdlib-1.19.4","sasl-2.3.4","pea-1.0"],
  permanent}]
```

Our new release, "SECOND", is now the current release, but we can also see that
our "FIRST" release is still permanent. This means that if the node would be
restarted now, it would come up running the "FIRST" release again.

_Step 3._ Make the new release permanent:

```text
2> release_handler:make_permanent("SECOND").
```

Check the releases again:

```c
3> release_handler:which_releases().
[{"MYSYSTEM","SECOND",
  ["kernel-3.0","stdlib-2.0","sasl-2.4","pea-2.0"],
  permanent},
 {"MYSYSTEM","FIRST",
  ["kernel-2.16.4","stdlib-1.19.4","sasl-2.3.4","pea-1.0"],
  old}]
```

We see that the new release version is `permanent`, so it would be safe to
restart the node.

[](){: #listing-of-target-system }

## Listing of target_system.erl

This module can also be found in the `examples` directory of the SASL
application.

```erlang

-module(target_system).
-export([create/1, create/2, install/2]).

%% Note: RelFileName below is the *stem* without trailing .rel,
%% .script etc.
%%

%% create(RelFileName)
%%
create(RelFileName) ->
    create(RelFileName,[]).

create(RelFileName,SystoolsOpts) ->
    RelFile = RelFileName ++ ".rel",
    Dir = filename:dirname(RelFileName),
    PlainRelFileName = filename:join(Dir,"plain"),
    PlainRelFile = PlainRelFileName ++ ".rel",
    io:fwrite("Reading file: ~ts ...~n", [RelFile]),
    {ok, [RelSpec]} = file:consult(RelFile),
    io:fwrite("Creating file: ~ts from ~ts ...~n",
              [PlainRelFile, RelFile]),
    {release,
     {RelName, RelVsn},
     {erts, ErtsVsn},
     AppVsns} = RelSpec,
    PlainRelSpec = {release,
                    {RelName, RelVsn},
                    {erts, ErtsVsn},
                    lists:filter(fun({kernel, _}) ->
                                         true;
                                    ({stdlib, _}) ->
                                         true;
                                    (_) ->
                                         false
                                 end, AppVsns)
                   },
    {ok, Fd} = file:open(PlainRelFile, [write]),
    io:fwrite(Fd, "~p.~n", [PlainRelSpec]),
    file:close(Fd),

    io:fwrite("Making \"~ts.script\" and \"~ts.boot\" files ...~n",
	      [PlainRelFileName,PlainRelFileName]),
    make_script(PlainRelFileName,SystoolsOpts),

    io:fwrite("Making \"~ts.script\" and \"~ts.boot\" files ...~n",
              [RelFileName, RelFileName]),
    make_script(RelFileName,SystoolsOpts),

    TarFileName = RelFileName ++ ".tar.gz",
    io:fwrite("Creating tar file ~ts ...~n", [TarFileName]),
    make_tar(RelFileName,SystoolsOpts),

    TmpDir = filename:join(Dir,"tmp"),
    io:fwrite("Creating directory ~tp ...~n",[TmpDir]),
    file:make_dir(TmpDir),

    io:fwrite("Extracting ~ts into directory ~ts ...~n", [TarFileName,TmpDir]),
    extract_tar(TarFileName, TmpDir),

    TmpBinDir = filename:join([TmpDir, "bin"]),
    ErtsBinDir = filename:join([TmpDir, "erts-" ++ ErtsVsn, "bin"]),
    io:fwrite("Deleting \"erl\" and \"start\" in directory ~ts ...~n",
              [ErtsBinDir]),
    file:delete(filename:join([ErtsBinDir, "erl"])),
    file:delete(filename:join([ErtsBinDir, "start"])),

    io:fwrite("Creating temporary directory ~ts ...~n", [TmpBinDir]),
    file:make_dir(TmpBinDir),

    io:fwrite("Copying file \"~ts.boot\" to ~ts ...~n",
              [PlainRelFileName, filename:join([TmpBinDir, "start.boot"])]),
    copy_file(PlainRelFileName++".boot",filename:join([TmpBinDir, "start.boot"])),

    io:fwrite("Copying files \"epmd\", \"run_erl\" and \"to_erl\" from \n"
              "~ts to ~ts ...~n",
              [ErtsBinDir, TmpBinDir]),
    copy_file(filename:join([ErtsBinDir, "epmd"]),
              filename:join([TmpBinDir, "epmd"]), [preserve]),
    copy_file(filename:join([ErtsBinDir, "run_erl"]),
              filename:join([TmpBinDir, "run_erl"]), [preserve]),
    copy_file(filename:join([ErtsBinDir, "to_erl"]),
              filename:join([TmpBinDir, "to_erl"]), [preserve]),

    %% This is needed if 'start' script created from 'start.src' shall
    %% be used as it points out this directory as log dir for 'run_erl'
    TmpLogDir = filename:join([TmpDir, "log"]),
    io:fwrite("Creating temporary directory ~ts ...~n", [TmpLogDir]),
    ok = file:make_dir(TmpLogDir),

    StartErlDataFile = filename:join([TmpDir, "releases", "start_erl.data"]),
    io:fwrite("Creating ~ts ...~n", [StartErlDataFile]),
    StartErlData = io_lib:fwrite("~s ~s~n", [ErtsVsn, RelVsn]),
    write_file(StartErlDataFile, StartErlData),

    io:fwrite("Recreating tar file ~ts from contents in directory ~ts ...~n",
	      [TarFileName,TmpDir]),
    {ok, Tar} = erl_tar:open(TarFileName, [write, compressed]),
    %% {ok, Cwd} = file:get_cwd(),
    %% file:set_cwd("tmp"),
    ErtsDir = "erts-"++ErtsVsn,
    erl_tar:add(Tar, filename:join(TmpDir,"bin"), "bin", []),
    erl_tar:add(Tar, filename:join(TmpDir,ErtsDir), ErtsDir, []),
    erl_tar:add(Tar, filename:join(TmpDir,"releases"), "releases", []),
    erl_tar:add(Tar, filename:join(TmpDir,"lib"), "lib", []),
    erl_tar:add(Tar, filename:join(TmpDir,"log"), "log", []),
    erl_tar:close(Tar),
    %% file:set_cwd(Cwd),
    io:fwrite("Removing directory ~ts ...~n",[TmpDir]),
    remove_dir_tree(TmpDir),
    ok.


install(RelFileName, RootDir) ->
    TarFile = RelFileName ++ ".tar.gz",
    io:fwrite("Extracting ~ts ...~n", [TarFile]),
    extract_tar(TarFile, RootDir),
    StartErlDataFile = filename:join([RootDir, "releases", "start_erl.data"]),
    {ok, StartErlData} = read_txt_file(StartErlDataFile),
    [ErlVsn, _RelVsn| _] = string:tokens(StartErlData, " \n"),
    ErtsBinDir = filename:join([RootDir, "erts-" ++ ErlVsn, "bin"]),
    BinDir = filename:join([RootDir, "bin"]),
    io:fwrite("Substituting in erl.src, start.src and start_erl.src to "
              "form erl, start and start_erl ...\n"),
    subst_src_scripts(["erl", "start", "start_erl"], ErtsBinDir, BinDir,
                      [{"FINAL_ROOTDIR", RootDir}, {"EMU", "beam"}],
                      [preserve]),
    %%! Workaround for pre OTP 17.0: start.src and start_erl.src did
    %%! not have correct permissions, so the above 'preserve' option did not help
    ok = file:change_mode(filename:join(BinDir,"start"),8#0755),
    ok = file:change_mode(filename:join(BinDir,"start_erl"),8#0755),

    io:fwrite("Creating the RELEASES file ...\n"),
    create_RELEASES(RootDir, filename:join([RootDir, "releases",
					    filename:basename(RelFileName)])).

%% LOCALS

%% make_script(RelFileName,Opts)
%%
make_script(RelFileName,Opts) ->
    systools:make_script(RelFileName, [no_module_tests,
				       {outdir,filename:dirname(RelFileName)}
				       |Opts]).

%% make_tar(RelFileName,Opts)
%%
make_tar(RelFileName,Opts) ->
    RootDir = code:root_dir(),
    systools:make_tar(RelFileName, [{erts, RootDir},
				    {outdir,filename:dirname(RelFileName)}
				    |Opts]).

%% extract_tar(TarFile, DestDir)
%%
extract_tar(TarFile, DestDir) ->
    erl_tar:extract(TarFile, [{cwd, DestDir}, compressed]).

create_RELEASES(DestDir, RelFileName) ->
    release_handler:create_RELEASES(DestDir, RelFileName ++ ".rel").

subst_src_scripts(Scripts, SrcDir, DestDir, Vars, Opts) ->
    lists:foreach(fun(Script) ->
                          subst_src_script(Script, SrcDir, DestDir,
                                           Vars, Opts)
                  end, Scripts).

subst_src_script(Script, SrcDir, DestDir, Vars, Opts) ->
    subst_file(filename:join([SrcDir, Script ++ ".src"]),
               filename:join([DestDir, Script]),
               Vars, Opts).

subst_file(Src, Dest, Vars, Opts) ->
    {ok, Conts} = read_txt_file(Src),
    NConts = subst(Conts, Vars),
    write_file(Dest, NConts),
    case lists:member(preserve, Opts) of
        true ->
            {ok, FileInfo} = file:read_file_info(Src),
            file:write_file_info(Dest, FileInfo);
        false ->
            ok
    end.

%% subst(Str, Vars)
%% Vars = [{Var, Val}]
%% Var = Val = string()
%% Substitute all occurrences of %Var% for Val in Str, using the list
%% of variables in Vars.
%%
subst(Str, Vars) ->
    subst(Str, Vars, []).

subst([$%, C| Rest], Vars, Result) when $A =< C, C =< $Z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C| Rest], Vars, Result) when $a =< C, C =< $z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C| Rest], Vars, Result) when  C == $_ ->
    subst_var([C| Rest], Vars, Result, []);
subst([C| Rest], Vars, Result) ->
    subst(Rest, Vars, [C| Result]);
subst([], _Vars, Result) ->
    lists:reverse(Result).

subst_var([$%| Rest], Vars, Result, VarAcc) ->
    Key = lists:reverse(VarAcc),
    case lists:keysearch(Key, 1, Vars) of
        {value, {Key, Value}} ->
            subst(Rest, Vars, lists:reverse(Value, Result));
        false ->
            subst(Rest, Vars, [$%| VarAcc ++ [$%| Result]])
    end;
subst_var([C| Rest], Vars, Result, VarAcc) ->
    subst_var(Rest, Vars, Result, [C| VarAcc]);
subst_var([], Vars, Result, VarAcc) ->
    subst([], Vars, [VarAcc ++ [$%| Result]]).

copy_file(Src, Dest) ->
    copy_file(Src, Dest, []).

copy_file(Src, Dest, Opts) ->
    {ok,_} = file:copy(Src, Dest),
    case lists:member(preserve, Opts) of
        true ->
            {ok, FileInfo} = file:read_file_info(Src),
            file:write_file_info(Dest, FileInfo);
        false ->
            ok
    end.

write_file(FName, Conts) ->
    Enc = file:native_name_encoding(),
    {ok, Fd} = file:open(FName, [write]),
    file:write(Fd, unicode:characters_to_binary(Conts,Enc,Enc)),
    file:close(Fd).

read_txt_file(File) ->
    {ok, Bin} = file:read_file(File),
    {ok, binary_to_list(Bin)}.

remove_dir_tree(Dir) ->
    remove_all_files(".", [Dir]).

remove_all_files(Dir, Files) ->
    lists:foreach(fun(File) ->
                          FilePath = filename:join([Dir, File]),
                          case filelib:is_dir(FilePath) of
                              true ->
                                  {ok, DirFiles} = file:list_dir(FilePath),
                                  remove_all_files(FilePath, DirFiles),
                                  file:del_dir(FilePath);
                              _ ->
                                  file:delete(FilePath)
                          end
                  end, Files).
```

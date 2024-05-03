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
# Releases

[](){: #releases-section }

This section is to be read with the `rel(4)`, `m:systools`, and `script(4)`
manual pages in SASL.

## Release Concept

When you have written one or more applications, you might want to create a
complete system with these applications and a subset of the Erlang/OTP
applications. This is called a _release_.

To do this, create a [release resource file](release_structure.md#res_file) that
defines which applications are included in the release.

The release resource file is used to generate
[boot scripts](release_structure.md#boot) and
[release packages](release_structure.md#pack). A system that is transferred to
and installed at another site is called a _target system_. How to use a release
package to create a target system is described in System Principles.

[](){: #res_file }

## Release Resource File

To define a release, create a _release resource file_, or in short a `.rel`
file. In the file, specify the name and version of the release, which ERTS
version it is based on, and which applications it consists of:

```erlang
{release, {Name,Vsn}, {erts, EVsn},
 [{Application1, AppVsn1},
   ...
  {ApplicationN, AppVsnN}]}.
```

`Name`, `Vsn`, `EVsn`, and `AppVsn` are strings.

The file must be named `Rel.rel`, where `Rel` is a unique name.

Each `Application` (atom) and `AppVsn` is the name and version of an application
included in the release. The minimal release based on Erlang/OTP consists of the
Kernel and STDLIB applications, so these applications must be included in the
list.

If the release is to be upgraded, it must also include the SASL application.

[](){: #ch_rel }

*Example: *A release of `ch_app` from [Applications](applications.md#ch_app) has
the following `.app` file:

```erlang
{application, ch_app,
 [{description, "Channel allocator"},
  {vsn, "1"},
  {modules, [ch_app, ch_sup, ch3]},
  {registered, [ch3]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {ch_app,[]}}
 ]}.
```

The `.rel` file must also contain `kernel`, `stdlib`, and `sasl`, as these
applications are required by `ch_app`. The file is called `ch_rel-1.rel`:

```erlang
{release,
 {"ch_rel", "A"},
 {erts, "5.3"},
 [{kernel, "2.9"},
  {stdlib, "1.12"},
  {sasl, "1.10"},
  {ch_app, "1"}]
}.
```

[](){: #boot }

## Generating Boot Scripts

`systools` in the SASL application includes tools to build and check releases.
The functions read the `rel` and `.app` files and perform syntax and dependency
checks. The `systools:make_script/1,2` function is used to generate a boot
script (see System Principles):

```text
1> systools:make_script("ch_rel-1", [local]).
ok
```

This creates a boot script, both the readable version, `ch_rel-1.script`, and
the binary version, `ch_rel-1.boot`, used by the runtime system.

- `"ch_rel-1"` is the name of the `.rel` file, minus the extension.
- `local` is an option that means that the directories where the applications
  are found are used in the boot script, instead of `$ROOT/lib` (`$ROOT` is the
  root directory of the installed release).

This is a useful way to test a generated boot script locally.

When starting Erlang/OTP using the boot script, all applications from the `.rel`
file are automatically loaded and started:

```text
% erl -boot ch_rel-1
Erlang (BEAM) emulator version 5.3

Eshell V5.3  (abort with ^G)
1>
=PROGRESS REPORT==== 13-Jun-2003::12:01:15 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.33.0>},
                       {name,alarm_handler},
                       {mfa,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

...

=PROGRESS REPORT==== 13-Jun-2003::12:01:15 ===
         application: sasl
          started_at: nonode@nohost

...
=PROGRESS REPORT==== 13-Jun-2003::12:01:15 ===
         application: ch_app
          started_at: nonode@nohost
```

[](){: #pack }

## Creating a Release Package

The `systools:make_tar/1,2` function takes a `.rel` file as input and creates a
zipped tar file with the code for the specified applications, a _release
package_:

```erlang
1> systools:make_script("ch_rel-1").
ok
2> systools:make_tar("ch_rel-1").
ok
```

The release package by default contains:

- The `.app` files
- The `.rel` file
- The object code for all applications, structured according to the
  [application directory structure](applications.md#app_dir)
- The binary boot script renamed to `start.boot`

```text
% tar tf ch_rel-1.tar
lib/kernel-2.9/ebin/kernel.app
lib/kernel-2.9/ebin/application.beam
...
lib/stdlib-1.12/ebin/stdlib.app
lib/stdlib-1.12/ebin/beam_lib.beam
...
lib/sasl-1.10/ebin/sasl.app
lib/sasl-1.10/ebin/sasl.beam
...
lib/ch_app-1/ebin/ch_app.app
lib/ch_app-1/ebin/ch_app.beam
lib/ch_app-1/ebin/ch_sup.beam
lib/ch_app-1/ebin/ch3.beam
releases/A/start.boot
releases/A/ch_rel-1.rel
releases/ch_rel-1.rel
```

A new boot script was generated, without the `local` option set, before the
release package was made. In the release package, all application directories
are placed under `lib`. You do not know where the release package will be
installed, so no hard-coded absolute paths are allowed.

The release resource file `mysystem.rel` is duplicated in the tar file.
Originally, this file was only stored in the `releases` directory to make it
possible for the `release_handler` to extract this file separately. After
unpacking the tar file, `release_handler` would automatically copy the file to
`releases/FIRST`. However, sometimes the tar file is unpacked without involving
the `release_handler` (for example, when unpacking the first target system) and
the file is therefore now instead duplicated in the tar file so no manual
copying is necessary.

If a `relup` file and/or a system configuration file called `sys.config`, or a
`sys.config.src`, is found, these files are also included in the release
package. See [Release Handling](release_handling.md#req).

Options can be set to make the release package include source code and the ERTS
binary as well.

For information on how to install the first target system, using a release
package, see System Principles. For information on how to install a new release
package in an existing system, see [Release Handling](release_handling.md).

[](){: #reldir }

## Directory Structure

The directory structure for the code installed by the release handler from a
release package is as follows:

```text
$ROOT/lib/App1-AVsn1/ebin
                    /priv
         /App2-AVsn2/ebin
                    /priv
         ...
         /AppN-AVsnN/ebin
                    /priv
     /erts-EVsn/bin
     /releases/Vsn
     /bin
```

- `lib` \- Application directories
- `erts-EVsn/bin` \- Erlang runtime system executables
- `releases/Vsn` \- `.rel` file and boot script `start.boot`; if present in the
  release package, `relup` and/or `sys.config` or `sys.config.src`
- `bin` \- Top-level Erlang runtime system executables

Applications are not required to be located under directory `$ROOT/lib`. Several
installation directories, which contain different parts of a system, can thus
exist. For example, the previous example can be extended as follows:

```text
$SECOND_ROOT/.../SApp1-SAVsn1/ebin
                             /priv
                /SApp2-SAVsn2/ebin
                             /priv
                ...
                /SAppN-SAVsnN/ebin
                             /priv

$THIRD_ROOT/TApp1-TAVsn1/ebin
                        /priv
           /TApp2-TAVsn2/ebin
                        /priv
           ...
           /TAppN-TAVsnN/ebin
                        /priv
```

`$SECOND_ROOT` and `$THIRD_ROOT` are introduced as `variables` in the call to
the `systools:make_script/2` function.

### Disk-Less and/or Read-Only Clients

If a complete system consists of disk-less and/or read-only client nodes, a
`clients` directory is to be added to the `$ROOT` directory. A read-only node is
a node with a read-only file system.

The `clients` directory is to have one subdirectory per supported client node.
The name of each client directory is to be the name of the corresponding client
node. As a minimum, each client directory is to contain the `bin` and `releases`
subdirectories. These directories are used to store information about installed
releases and to appoint the current release to the client. The `$ROOT` directory
thus contains the following:

```text
$ROOT/...
    /clients/ClientName1/bin
                        /releases/Vsn
            /ClientName2/bin
                        /releases/Vsn
            ...
            /ClientNameN/bin
                        /releases/Vsn
```

This structure is to be used if all clients are running the same type of Erlang
machine. If there are clients running different types of Erlang machines, or on
different operating systems, the `clients` directory can be divided into one
subdirectory per type of Erlang machine. Alternatively, one `$ROOT` can be set
up per type of machine. For each type, some of the directories specified for the
`$ROOT` directory are to be included:

```text
$ROOT/...
    /clients/Type1/lib
                  /erts-EVsn
                  /bin
                  /ClientName1/bin
                              /releases/Vsn
                  /ClientName2/bin
                              /releases/Vsn
                  ...
                  /ClientNameN/bin
                              /releases/Vsn
            ...
            /TypeN/lib
                  /erts-EVsn
                  /bin
                  ...
```

With this structure, the root directory for clients of `Type1` is
`$ROOT/clients/Type1`.

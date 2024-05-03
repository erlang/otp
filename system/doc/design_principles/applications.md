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
# Applications

[](){: #appl }

This section is to be read with the `app(4)` and `m:application` manual pages in
Kernel.

## Application Concept

When you have written code implementing some specific functionality you might
want to make the code into an _application_, that is, a component that can be
started and stopped as a unit, and which can also be reused in other systems.

To do this, create an
[application callback module](applications.md#callback_module), and describe how
the application is to be started and stopped.

Then, an _application specification_ is needed, which is put in an
[application resource file](applications.md#appl_res_file). Among other things,
this file specifies which modules the application consists of and the name of
the callback module.

If you use `systools`, the Erlang/OTP tools for packaging code (see
[Releases](release_structure.md)), the code for each application is placed in a
separate directory following a pre-defined
[directory structure](applications.md#app_dir).

[](){: #callback_module }

## Application Callback Module

How to start and stop the code for the application, that is, the supervision
tree, is described by two callback functions:

```erlang
start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State}
stop(State)
```

- `start` is called when starting the application and is to create the
  supervision tree by starting the top supervisor. It is expected to return the
  pid of the top supervisor and an optional term, `State`, which defaults to
  `[]`. This term is passed as is to `stop`.
- `StartType` is usually the atom `normal`. It has other values only in the case
  of a takeover or failover, see
  [Distributed Applications](distributed_applications.md).
- `StartArgs` is defined by the key `mod` in the
  [application resource file](applications.md#appl_res_file).
- `stop/1` is called _after_ the application has been stopped and is to do any
  necessary cleaning up. The actual stopping of the application, that is, the
  shutdown of the supervision tree, is handled automatically as described in
  [Starting and Stopping Applications](applications.md#stopping).

[](){: #ch_app }

Example of an application callback module for packaging the supervision tree
from [Supervisor Behaviour](sup_princ.md#ex):

```erlang
-module(ch_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    ch_sup:start_link().

stop(_State) ->
    ok.
```

A library application that cannot be started or stopped, does not need any
application callback module.

[](){: #appl_res_file }

## Application Resource File

To define an application, an _application specification_ is created, which is
put in an _application resource file_, or in short an `.app` file:

```text
{application, Application, [Opt1,...,OptN]}.
```

- `Application`, an atom, is the name of the application. The file must be named
  `Application.app`.
- Each `Opt` is a tuple `{Key,Value}`, which defines a certain property of the
  application. All keys are optional. Default values are used for any omitted
  keys.

The contents of a minimal `.app` file for a library application `libapp` looks
as follows:

```text
{application, libapp, []}.
```

The contents of a minimal `.app` file `ch_app.app` for a supervision tree
application like `ch_app` looks as follows:

```text
{application, ch_app,
 [{mod, {ch_app,[]}}]}.
```

The key `mod` defines the callback module and start argument of the application,
in this case `ch_app` and `[]`, respectively. This means that the following is
called when the application is to be started:

```text
ch_app:start(normal, [])
```

The following is called when the application is stopped.

```text
ch_app:stop([])
```

When using `systools`, the Erlang/OTP tools for packaging code (see Section
[Releases](release_structure.md)), the keys `description`, `vsn`, `modules`,
`registered`, and `applications` are also to be specified:

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

- `description` \- A short description, a string. Defaults to "".
- `vsn` \- Version number, a string. Defaults to "".
- `modules` \- All modules _introduced_ by this application. `systools` uses
  this list when generating boot scripts and tar files. A module must be defined
  in only one application. Defaults to `[]`.
- `registered` \- All names of registered processes in the application.
  `systools` uses this list to detect name clashes between applications.
  Defaults to `[]`.
- `applications` \- All applications that must be started before this
  application is started. `systools` uses this list to generate correct boot
  scripts. Defaults to `[]`. Notice that all applications have dependencies to
  at least Kernel and STDLIB.

> #### Note {: .info }
>
> For details about the syntax and contents of the application resource file,
> see the [app](`e:kernel:app.md`) manual page in Kernel.

[](){: #app_dir }

## Directory Structure

When packaging code using `systools`, the code for each application is placed in
a separate directory, `lib/Application-Vsn`, where `Vsn` is the version number.

This can be useful to know, even if `systools` is not used, since Erlang/OTP is
packaged according to the OTP principles and thus comes with a specific
directory structure. The code server (see the `m:code` manual page in Kernel)
automatically uses code from the directory with the highest version number, if
more than one version of an application is present.

### Directory Structure Guidelines for a Development Environment

Any directory structure for development will suffice as long as the released
directory structure adheres to the
[description below](applications.md#app_dir_released), but it is encouraged that
the same directory structure also be used in a development environment. The
version number should be omitted from the application directory name since this
is an artifact of the release step.

Some sub-directories are _required_. Some sub-directories are _optional_,
meaning that it should only be used if the application itself requires it.
Finally, some sub-directories are _recommended_, meaning it is encouraged that
it is used and used as described here. For example, both documentation and tests
are encouraged to exist in an application for it to be deemed a proper OTP
application.

```text
    ─ ${application}
      ├── doc
      │   ├── internal
      │   ├── examples
      │   └── src
      ├── include
      ├── priv
      ├── src
      │   └── ${application}.app.src
      └── test
```

- `src` \- Required. Contains the Erlang source code, the source of the `.app`
  file and internal include files used by the application itself. Additional
  sub-directories within `src` can be used as namespaces to organize source
  files. These directories should never be deeper than one level.
- `priv` \- Optional. Used for application specific files.
- `include` \- Optional. Used for public include files that must be reachable
  from other applications.
- `doc` \- Recommended. Any source documentation should be placed in
  sub-directories here.
- `doc/internal` \- Recommended. Any documentation that describes implementation
  details about this application, not intended for publication, should be placed
  here.
- `doc/examples` \- Recommended. Source code for examples on how to use this
  application should be placed here. It is encouraged that examples are sourced
  to the public documentation from this directory.
- `doc/src` \- Recommended. All source files for documentation, such as
  Markdown, AsciiDoc or XML-files, should be placed here.
- `test` \- Recommended. All files regarding tests, such as test suites and test
  specifications, should be placed here.

Other directories in the development environment may be needed. If source code
from languages other than Erlang is used, for instance C-code for NIFs, that
code should be placed in a separate directory. By convention it is recommended
to prefix such directories with the language name, for example `c_src` for C,
`java_src` for Java or `go_src` for Go. Directories with `_src` suffix indicates
that it is a part of the application and the compilation step. The final build
artifacts should target the `priv/lib` or `priv/bin` directories.

The `priv` directory holds assets that the application needs during runtime.
Executables should reside in `priv/bin` and dynamically-linked libraries should
reside in `priv/lib`. Other assets are free to reside within the `priv`
directory but it is recommended they do so in a structured manner.

Source files from other languages that generate Erlang code, such as ASN.1 or
Mibs, should be placed in directories, at the top level or in `src`, with the
same name as the source language, for example `asn1` and `mibs`. Build artifacts
should be placed in their respective language directory, such as `src` for
Erlang code or `java_src` for Java code.

The `.app` file for release may reside in the `ebin`\-directory in a development
environment but it is encouraged that this is an artifact of the build step. By
convention a `.app.src` file is used, which resides in the `src` directory. This
file is nearly identical as the `.app` file but certain fields may be replaced
during the build step, such as the application version.

Directory names should not be capitalized.

It is encouraged to omit empty directories.

[](){: #app_dir_released }

### Directory Structure for a Released System

A released application must follow a certain structure.

```text
    ─ ${application}-${version}
      ├── bin
      ├── doc
      │   ├── html
      │   ├── man[1-9]
      │   ├── pdf
      │   ├── internal
      │   └── examples
      ├── ebin
      │   └── ${application}.app
      ├── include
      ├── priv
      │   ├── lib
      │   └── bin
      └── src
```

- `src` \- Optional. Contains the Erlang source code and internal include files
  used by the application itself. This directory is no longer required in a
  released application.
- `ebin` \- Required. Contains the Erlang object code, the `beam` files. The
  `.app` file must also be placed here.
- `priv` \- Optional. Used for application specific files. `code:priv_dir/1` is
  to be used to access this directory.
- `priv/lib` \- Recommended. Any shared-object files that are used by the
  application, such as NIFs or linked-in-drivers, should be placed here.
- `priv/bin` \- Recommended. Any executable that is used by the application,
  such as port-programs, should be placed here.
- `include` \- Optional. Used for public include files that must be reachable
  from other applications.
- `bin` \- Optional. Any executable that is a product of the application, such
  as escripts or shell-scripts, should be placed here.
- `doc` \- Optional. Any released documentation should be placed in
  sub-directories here.
- `doc/man1` \- Recommended. Man pages for Application executables.
- `doc/man3` \- Recommended. Man pages for module APIs.
- `doc/man6` \- Recommended. Man pages for Application overview.
- `doc/html` \- Optional. HTML pages for the entire Application.
- `doc/pdf` \- Optional. PDF documentation for the entire Application.

The `src` directory could be useful to release for debugging purposes but is not
required. The `include` directory should only be released if the applications
has public include files.

The only documentation that is recommended to be released in this way are the
man pages. HTML and PDF will normally be distributed in some other manner.

It is encouraged to omit empty directories.

[](){: #application_controller }

## Application Controller

When an Erlang runtime system is started, a number of processes are started as
part of the Kernel application. One of these processes is the _application
controller_ process, registered as `application_controller`.

All operations on applications are coordinated by the application controller. It
is interacted with through the functions in the module `application`, see the
`m:application` manual page in Kernel. In particular, applications can be
loaded, unloaded, started, and stopped.

## Loading and Unloading Applications

Before an application can be started, it must be _loaded_. The application
controller reads and stores the information from the `.app` file:

```erlang
1> application:load(ch_app).
ok
2> application:loaded_applications().
[{kernel,"ERTS  CXC 138 10","2.8.1.3"},
 {stdlib,"ERTS  CXC 138 10","1.11.4.3"},
 {ch_app,"Channel allocator","1"}]
```

An application that has been stopped, or has never been started, can be
unloaded. The information about the application is erased from the internal
database of the application controller.

```erlang
3> application:unload(ch_app).
ok
4> application:loaded_applications().
[{kernel,"ERTS  CXC 138 10","2.8.1.3"},
 {stdlib,"ERTS  CXC 138 10","1.11.4.3"}]
```

> #### Note {: .info }
>
> Loading/unloading an application does not load/unload the code used by the
> application. Code loading is done the usual way.

[](){: #stopping }

## Starting and Stopping Applications

An application is started by calling:

```erlang
5> application:start(ch_app).
ok
6> application:which_applications().
[{kernel,"ERTS  CXC 138 10","2.8.1.3"},
 {stdlib,"ERTS  CXC 138 10","1.11.4.3"},
 {ch_app,"Channel allocator","1"}]
```

If the application is not already loaded, the application controller first loads
it using `application:load/1`. It checks the value of the `applications` key, to
ensure that all applications that are to be started before this application are
running.

[](){: #application_master }

The application controller then creates an _application master_ for the
application. The application master becomes the group leader of all the
processes in the application. I/O is forwarded to the previous group leader,
though, this is just a way to identify processes that belong to the application.
Used for example to find itself from any process, or, reciprocally, to kill them
all when it terminates.

The application master starts the application by calling the application
callback function `start/2` in the module, and with the start argument, defined
by the `mod` key in the `.app` file.

An application is stopped, but not unloaded, by calling:

```text
7> application:stop(ch_app).
ok
```

The application master stops the application by telling the top supervisor to
shut down. The top supervisor tells all its child processes to shut down, and so
on; the entire tree is terminated in reversed start order. The application
master then calls the application callback function `stop/1` in the module
defined by the `mod` key.

## Configuring an Application

An application can be configured using _configuration parameters_. These are a
list of `{Par,Val}` tuples specified by a key `env` in the `.app` file:

```erlang
{application, ch_app,
 [{description, "Channel allocator"},
  {vsn, "1"},
  {modules, [ch_app, ch_sup, ch3]},
  {registered, [ch3]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {ch_app,[]}},
  {env, [{file, "/usr/local/log"}]}
 ]}.
```

`Par` is to be an atom. `Val` is any term. The application can retrieve the
value of a configuration parameter by calling `application:get_env(App, Par)` or
a number of similar functions, see the `m:application` manual page in Kernel.

_Example:_

```erlang
% erl
Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]

Eshell V5.2.3.6  (abort with ^G)
1> application:start(ch_app).
ok
2> application:get_env(ch_app, file).
{ok,"/usr/local/log"}
```

The values in the `.app` file can be overridden by values in a _system
configuration file_. This is a file that contains configuration parameters for
relevant applications:

```erlang
[{Application1, [{Par11,Val11},...]},
 ...,
 {ApplicationN, [{ParN1,ValN1},...]}].
```

The system configuration is to be called `Name.config` and Erlang is to be
started with the command-line argument `-config Name`. For details, see the
`config(4)` manual page in Kernel.

_Example:_

A file `test.config` is created with the following contents:

```text
[{ch_app, [{file, "testlog"}]}].
```

The value of `file` overrides the value of `file` as defined in the `.app` file:

```erlang
% erl -config test
Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]

Eshell V5.2.3.6  (abort with ^G)
1> application:start(ch_app).
ok
2> application:get_env(ch_app, file).
{ok,"testlog"}
```

If [release handling](release_handling.md#sys) is used, exactly one system
configuration file is to be used and that file is to be called `sys.config`.

The values in the `.app` file and the values in a system configuration file can
be overridden directly from the command line:

```text
% erl -ApplName Par1 Val1 ... ParN ValN
```

_Example:_

```erlang
% erl -ch_app file '"testlog"'
Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]

Eshell V5.2.3.6  (abort with ^G)
1> application:start(ch_app).
ok
2> application:get_env(ch_app, file).
{ok,"testlog"}
```

## Application Start Types

A _start type_ is defined when starting the application:

```text
application:start(Application, Type)
```

`application:start(Application)` is the same as calling
`application:start(Application, temporary)`. The type can also be `permanent` or
`transient`:

- If a permanent application terminates, all other applications and the runtime
  system are also terminated.
- If a transient application terminates with reason `normal`, this is reported
  but no other applications are terminated. If a transient application
  terminates abnormally, that is with any other reason than `normal`, all other
  applications and the runtime system are also terminated.
- If a temporary application terminates, this is reported but no other
  applications are terminated.

An application can always be stopped explicitly by calling `application:stop/1`.
Regardless of the mode, no other applications are affected.

The transient mode is of little practical use, since when a supervision tree
terminates, the reason is set to `shutdown`, not `normal`.

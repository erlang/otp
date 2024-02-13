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
# System Principles

[](){: #system-principles }

## Starting the System

An Erlang runtime system is started with command `erl`:

```text
% erl
Erlang/OTP 17 [erts-6.0] [hipe] [smp:8:8]

Eshell V6.0  (abort with ^G)
1>
```

`erl` understands a number of command-line arguments, see the
[erl(1)](`e:erts:erl_cmd.md`) manual page in ERTS. Some of them are also
described in this chapter.

Application programs can access the values of the command-line arguments by
calling the function `init:get_argument(Key)` or `init:get_arguments()`. See the
`m:init` manual page in ERTS.

## Restarting and Stopping the System

The runtime system is halted by calling `halt/0,1`. For details, see the
`m:erlang` manual page in ERTS.

The module `init` contains functions for restarting, rebooting, and stopping the
runtime system:

```text
init:restart()
init:reboot()
init:stop()
```

For details, see the `m:init` manual page in ERTS.

The runtime system terminates if the Erlang shell is terminated.

[](){: #BOOTSCRIPT }

## Boot Scripts

The runtime system is started using a _boot script_. The boot script contains
instructions on which code to load and which processes and applications to
start.

A boot script file has the extension `.script`. The runtime system uses a binary
version of the script. This _binary boot script_ file has the extension `.boot`.

Which boot script to use is specified by the command-line flag `-boot`. The
extension `.boot` is to be omitted. For example, using the boot script
`start_all.boot`:

```text
% erl -boot start_all
```

If no boot script is specified, it defaults to `ROOT/bin/start`, see
[Default Boot Scripts](system_principles.md#default_boot_scripts).

The command-line flag `-init_debug` makes the `init` process write some debug
information while interpreting the boot script:

```text
% erl -init_debug
{progress,preloaded}
{progress,kernel_load_completed}
{progress,modules_loaded}
{start,heart}
{start,logger}
...
```

For a detailed description of the syntax and contents of the boot script, see
the `script(4)` manual page in SASL.

[](){: #default_boot_scripts }

### Default Boot Scripts

Erlang/OTP comes with these boot scripts:

- `start_clean.boot` \- Loads the code for and starts the applications Kernel
  and STDLIB.
- `start_sasl.boot` \- Loads the code for and starts the applications Kernel,
  STDLIB, and SASL.
- `no_dot_erlang.boot` \- Loads the code for and starts the applications Kernel
  and STDLIB. Skips loading the file `.erlang`. Useful for scripts and other
  tools that are to behave the same irrespective of user preferences.

Which of `start_clean` and `start_sasl` to use as default is decided by the user
when installing Erlang/OTP using `Install`. The user is asked "Do you want to
use a minimal system startup instead of the SASL startup". If the answer is yes,
then `start_clean` is used, otherwise `start_sasl` is used. A copy of the
selected boot script is made, named `start.boot` and placed in directory
`ROOT/bin`.

### User-Defined Boot Scripts

It is sometimes useful or necessary to create a user-defined boot script. This
is true especially when running Erlang in embedded mode, see
[Code Loading Strategy](system_principles.md#code_loading).

A boot script can be written manually. However, it is recommended to create a
boot script by generating it from a release resource file `Name.rel`, using the
function `systools:make_script/1,2`. This requires that the source code is
structured as applications according to the OTP design principles. (The program
does not have to be started in terms of OTP applications, but can be plain
Erlang).

For more information about `.rel` files, see
[OTP Design Principles](`e:system:release_handling.md`) and the
[rel(4)](`e:sasl:rel.md`) manual page in SASL.

The binary boot script file `Name.boot` is generated from the boot script file
`Name.script`, using the function `systools:script2boot(File)`.

[](){: #code_loading }

## Code Loading Strategy

The runtime system can be started in either _embedded_ or _interactive_ mode.
Which one is decided by the command-line flag `-mode`.

```text
% erl -mode embedded
```

Default mode is `interactive` and extra `-mode` flags are ignored.

The mode properties are as follows:

- In embedded mode, all code is loaded during system startup according to the
  boot script. (Code can also be loaded later by explicitly ordering the code
  server to do so.)
- In interactive mode, the code is dynamically loaded when first referenced.
  When a call to a function in a module is made, and the module is not loaded,
  the code server searches the code path and loads the module into the system.

Initially, the code path consists of the current working directory and all
object code directories under `ROOT/lib`, where `ROOT` is the installation
directory of Erlang/OTP. Directories can be named `Name[-Vsn]`. The code server,
by default, chooses the directory with the highest version number among those
which have the same `Name`. The `-Vsn` suffix is optional. If an `ebin`
directory exists under the `Name[-Vsn]` directory, this directory is added to
the code path.

The code path can be extended by using the command-line flags `-pa Directories`
and `-pz Directories`. These add `Directories` to the head or the end of the
code path, respectively. Example:

```text
% erl -pa /home/arne/mycode
```

The code server module `code` contains a number of functions for modifying and
checking the search path, see the `m:code` manual page in Kernel.

## File Types

The following file types are defined in Erlang/OTP:

| _File Type_               | _File Name/Extension_ | _Documented in_                                         |
| ------------------------- | --------------------- | ------------------------------------------------------- |
| Module                    | `.erl`                | [Erlang Reference Manual](`e:system:modules.md`)        |
| Include file              | `.hrl`                | [Erlang Reference Manual](`e:system:modules.md`)        |
| Release resource file     | `.rel`                | [rel(4)](`e:sasl:rel.md`) manual page in SASL           |
| Application resource file | `.app`                | [app(4)](`e:kernel:app.md`) manual page in Kernel       |
| Boot script               | `.script`             | [script(4)](`e:sasl:script.md`) manual page in SASL     |
| Binary boot script        | `.boot`               | -                                                       |
| Configuration file        | `.config`             | [config(4)](`e:kernel:config.md`) manual page in Kernel |
| Application upgrade file  | `.appup`              | [appup(4)](`e:sasl:appup.md`) manual page in SASL       |
| Release upgrade file      | `relup`               | [relup(4)](`e:sasl:relup.md`) manual page in SASL       |

_Table: File Types_

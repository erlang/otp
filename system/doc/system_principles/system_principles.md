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
Erlang/OTP 27 [erts-15.0] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V15.0 (press Ctrl+G to abort, type help(). for help)
1>
```

`erl` understands a number of command-line arguments; see
[erl](`e:erts:erl_cmd.md`) in the ERTS application. Some arguments are
also described in this chapter.

Application programs can access the values of the command-line arguments by
calling one of the following functions:

* [`init:get_argument(Key)`](https://www.erlang.org/doc/man/init#get_argument-1)
* [`init:get_arguments()`](https://www.erlang.org/doc/man/init#get_arguments-0)
* [`init:get_plain_arguments()`](https://www.erlang.org/doc/man/init#get_plain_arguments-0)

## Restarting and Stopping the System

The runtime system is halted by calling
[`halt/0,1,2`](https://www.erlang.org/doc/man/erlang#halt-2).

Module `m:init` contains functions for restarting, rebooting, and stopping the
runtime system:

* [`init:restart()`](https://www.erlang.org/doc/man/init#restart-0)
* [`init:reboot()`](https://www.erlang.org/doc/man/init#reboot-0)
* [`init:stop()`](https://www.erlang.org/doc/man/init#stop-0)

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

If no boot script is specified, it defaults to `ROOT/bin/start`, where
`ROOT` is the installation directory of Erlang/OTP. See [Default Boot
Scripts](system_principles.md#default_boot_scripts).

When the command-line flag `-init_debug` is used, the `init` process will
output debug information while interpreting the boot script.

```text
% erl -init_debug
{progress,preloaded}
{progress,kernel_load_completed}
{progress,modules_loaded}
{start,heart}
{start,logger}
  .
  .
  .
```

For a detailed description of the syntax and contents of the boot script, see
[`script`](https://www.erlang.org/doc/man/script) in the SASL application.

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
when installing Erlang/OTP using `Install`. The user is asked:

```text
Do you want to use a minimal system startup instead of the SASL startup?
```

If the answer is yes, `start_clean` is used, otherwise `start_sasl` is
used. The chosen boot script is copied and renamed as `start.boot`,
then placed into directory `ROOT/bin`.

### User-Defined Boot Scripts

It is sometimes useful or necessary to create a user-defined boot script. This
is true especially when running Erlang in embedded mode; see
[Code Loading Strategy](system_principles.md#code_loading).

While it is possible to manually create a boot script, it is
preferable to generate it from a release resource file called
`Name.rel` using the function
[`systools:make_script/1,2`](https://www.erlang.org/doc/man/systools#make_script-2).
This requires that the source code is structured as applications
according to the OTP design principles.

For more information about `.rel` files, see
[OTP Design Principles](`e:system:release_handling.md`) and the
[rel](`e:sasl:rel.md`) page in SASL.

To generate the binary boot script file `Name.boot` the boot script file
`Name.script`, use the
[`systools:script2boot(File)`](https://www.erlang.org/doc/man/systools#script2boot-1)
function.

[](){: #code_loading }

## Code Loading Strategy

The runtime system can be started in either _embedded_ or _interactive_ mode.
Which one is decided by the command-line flag `-mode`:

```text
% erl -mode embedded
```

The default mode is `interactive`. If more than one `-mode` flag is given,
the first one will be used.

The mode properties are as follows:

- In embedded mode, all code is loaded during system startup according
  to the boot script. (Code can be loaded later by **explicitly**
  ordering the code server to load it.)

- In interactive mode, code is dynamically loaded when first required,
  which means that when an attempt is made to call a function in a
  module that is not loaded, the code server searches the code path
  and loads the module into the system.

Initially, the code path consists of the current working directory and
all object code directories under `ROOT/lib`, where `ROOT` is the
installation directory of Erlang/OTP. Directories can be named
`Name[-Vsn]`, where the `-Vsn` suffix is optional. By default, the
code server chooses the directory with the highest version number
among those which have the same `Name`. If an `ebin` directory exists
under the `Name[-Vsn]` directory, this directory is added to the code
path.

The code path can be extended by using the command-line flags `-pa Directories`
and `-pz Directories`. These add `Directories` to the head or the end of the
code path, respectively. Example:

```text
% erl -pa /home/arne/mycode
```

The `m:code` module contains a number of functions for modifying and
querying the search path.

## File Types

The following file types are defined in Erlang/OTP:

| _File Type_               | _File Name/Extension_ | _Documented in_                                     |
| ------------------------- | --------------------- | --------------------------------------------------- |
| Module                    | `.erl`                | [Erlang Reference Manual](`e:system:modules.md`)    |
| Include file              | `.hrl`                | [Erlang Reference Manual](`e:system:modules.md`)    |
| Release resource file     | `.rel`                | [rel](`e:sasl:rel.md`) in SASL                      |
| Application resource file | `.app`                | [app](`e:kernel:app.md`) in Kernel                  |
| Boot script               | `.script`             | [script](`e:sasl:script.md`) in SASL                |
| Binary boot script        | `.boot`               | -                                                   |
| Configuration file        | `.config`             | [config](`e:kernel:config.md`) in Kernel            |
| Application upgrade file  | `.appup`              | [appup](`e:sasl:appup.md`) in SASL                  |
| Release upgrade file      | `relup`               | [relup](`e:sasl:relup.md`) in SASL                  |

_Table: File Types_

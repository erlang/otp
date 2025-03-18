<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2025. All Rights Reserved.

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
# erlc

Compiler

## Description

The `erlc` program provides a common way to run all compilers in the Erlang
system. Depending on the extension of each input file, `erlc` invokes the
appropriate compiler. Regardless of which compiler is used, the same flags are
used to provide parameters, such as include paths and output directory.

The current working directory, `"."`, is not included in the code path when
running the compiler. This is to avoid loading Beam files from the current
working directory that could potentially be in conflict with the compiler or the
Erlang/OTP system used by the compiler.

## erlc flags file1.ext file2.ext...

Compiles one or more files. The files must include the extension, for example,
`.erl` for Erlang source code, or `.yrl` for Yecc source code. `Erlc` uses the
extension to invoke the correct compiler.

## Generally Useful Flags

The following flags are supported:

- **`-I <Directory>`** - Instructs the compiler to search for include files in
  the `Directory`. When encountering an `-include` or `-include_lib` directive,
  the compiler searches for header files in the following directories:

  - `"."`, the current working directory of the file server
  - The base name of the compiled file
  - The directories specified using option `-I`; the directory specified last is
    searched first

- **`-o <Directory>`** - The directory where the compiler is to place the output
  files. Defaults to the current working directory.

- **`-D<Name>`** - Defines a macro.

- **`-D<Name>=<Value>`** - Defines a macro with the specified value. The value
  can be any Erlang term. Depending on the platform, the value may need to be
  quoted if the shell itself interprets certain characters. On Unix, terms
  containing tuples and lists must be quoted. Terms containing spaces must be
  quoted on all platforms.

- **`-Werror`** - Makes all warnings into errors.

- **`-W<Number>`** - Sets warning level to `Number`. Defaults to `1`. To turn
  off warnings, use `-W0`.

- **`-W`** - Same as `-W1`. Default.

- **`-v`** - Enables verbose output.

- **`-b <Output_type>`** - Specifies the type of output file. `Output_type` is
  the same as the file extension of the output file, but without the period.
  This option is ignored by compilers that have a single output format.

- **`-no-server`** - Do not use the
  [compile server](erlc_cmd.md#compile_server).

- **`-server`** - Use the [compile server](erlc_cmd.md#compile_server).

- **`-enable-feature <Feature>`{: #enable-feature }** - Enables the
  [feature](`e:system:features.md#features`) `feature` during compilation. The
  special feature `all` can be used to enable all features.

- **`-disable-feature <feature>`{: #disable-feature }** - Disables the
  [feature](`e:system:features.md#features`) `feature` during compilation. The
  special feature `all` can be used to disable all non permanent features.

- **`-list-features`** - [](){: #list-features } List short descriptions of the
  current configurable [features](`e:system:features.md#features`).
  Non-configurable features (those with status `rejected` or `permanent`)
  will not be shown.

- **`-describe-feature <feature>`** - [](){: #describe-feature } Show long
  description and history of [feature](`e:system:features.md#features`)
  `feature`.

- **`-M`** - Produces a Makefile rule to track header dependencies. The rule is
  sent to `stdout`. No object file is produced.

- **`-MMD`** - Generate dependencies as a side-effect. The object file will be
  produced as normal. This option overrides the option `-M`.

- **`-MF <Makefile>`** - As option `-M`, except that the Makefile is written to
  `Makefile`. No object file is produced.

- **`-MD`** - Same as `-M -MF <File>.Pbeam`.

- **`-MT <Target>`** - In conjunction with option `-M` or `-MF`, changes the
  name of the rule emitted to `Target`.

- **`-MQ <Target>`** - As option `-MT`, except that characters special to
  `make/1` are quoted.

- **`-MP`** - In conjunction with option `-M` or `-MF`, adds a phony target for
  each dependency.

- **`-MG`** - In conjunction with option `-M` or `-MF`, considers missing
  headers as generated files and adds them to the dependencies.

- **`--`** - Signals that no more options will follow. The rest of the arguments
  is treated as filenames, even if they start with hyphens.

- **`+<Term>`** - A flag starting with a plus (`+`) rather than a hyphen is
  converted to an Erlang term and passed unchanged to the compiler. For example,
  option `export_all` for the Erlang compiler can be specified as follows:

  ```text
  erlc +export_all file.erl
  ```

  Depending on the platform, the value may need to be quoted if the shell itself
  interprets certain characters. On Unix, terms containing tuples and lists must
  be quoted. Terms containing spaces must be quoted on all platforms.

## Special Flags

The following flags are useful in special situations, such as rebuilding the OTP
system:

- **`-pa <Directory>`** - Appends `Directory` to the front of the code path in
  the invoked Erlang emulator. This can be used to invoke another compiler than
  the default one.

- **`-pz <Directory>`** - Appends `Directory` to the code path in the invoked
  Erlang emulator.

## Supported Compilers

The following compilers are supported:

- **`.erl`** - Erlang source code. It generates a `.beam` file.

  Options `-P`, `-E`, and `-S` are equivalent to `+'P'`, `+'E'`, and `+'S'`,
  except that it is not necessary to include the single quotes to protect them
  from the shell.

  Supported options: `-I`, `-o`, `-D`, `-v`, `-W`, `-b`.

- **`.S`** - Erlang assembler source code. It generates a `.beam` file.

  Supported options: same as for `.erl`.

- **`.core`** - Erlang core source code. It generates a `.beam` file.

  Supported options: same as for `.erl`.

- **`.yrl`** - Yecc source code. It generates an `.erl` file.

  Use option `-I` with the name of a file to use that file as a customized
  prologue file (option `includefile`).

  Supported options: `-o`, `-v`, `-I`, `-W`.

- **`.mib`** - MIB for SNMP. It generates a `.bin` file.

  Supported options: `-I`, `-o`, `-W`.

- **`.bin`** - A compiled MIB for SNMP. It generates a `.hrl` file.

  Supported options: `-o`, `-v`.

- **`.rel`** - Script file. It generates a boot file.

  Use option `-I` to name directories to be searched for application files
  (equivalent to the `path` in the option list for `systools:make_script/2`).

  Supported option: `-o`.

- **`.asn1`** - ASN1 file. It creates an `.erl`, `.hrl`, and `.asn1db` file from
  an `.asn1` file. Also compiles the `.erl` using the Erlang compiler unless
  option `+noobj` is specified.

  Supported options: `-I`, `-o`, `-b`, `-W`.

- **`.idl`** - IC file. It runs the IDL compiler.

  Supported options: `-I`, `-o`.

[](){: #compile_server }

## Compile Server

The compile server can be used to potentially speed up the build of multi-file
projects by avoiding to start an Erlang system for each file to compile. Whether
it will speed up the build depends on the nature of the project and the build
machine.

By default, the compile server is not used. It can be enabled by giving `erlc`
the option `-server` or by setting the environment variable `ERLC_USE_SERVER` to
`yes` or `true`.

When the compile server is enabled, `erlc` will automatically use the server if
it is started and start the server if has not already started. The server will
terminate itself when it has been idle for some number of seconds.

`erlc` and the compile server communicate using the Erlang distribution. The
compile server is started as a hidden node, with a name that includes the
current user. Thus, each user on a computer has their own compile server.

Using the compile server does not always speed up the build, as the compile
server sometimes must be restarted to ensure correctness. Here are some examples
of situations that force a restart:

- `erlc` wants to use a different version of Erlang than the compile server is
  using.
- `erlc` wants to use different options for `erl` than the compile server was
  started with. (A change to code path using the option `-pa` could cause
  different parse transforms to be loaded. To be safe, the compile server will
  be restarted when any `erl` option is changed.)
- If the current working directory for `erlc` is different from the working
  directory active when the compile server was started, **and** if the compile
  server has active jobs, it will be restarted as soon as those jobs have
  finished. (Build systems that build files randomly across multiple directories
  in parallel will probably not benefit from the compile server.)

[](){: #environment_variables }

## Environment Variables

- **`ERLC_EMULATOR`** - The command for starting the emulator, which must be
  just the path for the emulator without any parameters. Defaults to `erl`
  in the same directory as the `erlc` program itself, or, if it does not exist,
  `erl` in any of the directories specified in environment variable `PATH`.

- **`ERLC_USE_SERVER`** - Allowed values are `yes` or `true` to use the
  [compile server](erlc_cmd.md#compile_server), and `no` or `false` to not use
  the compile server. If other values are given, `erlc` will print a warning
  message and continue.

- **`ERLC_SERVER_ID`** - Tells `erlc` to identify the
  [compile server](erlc_cmd.md#compile_server) by the given name, allowing a
  single user to run multiple unrelated builds in parallel without them
  affecting each other, which can be useful for shared build machines and the
  like. The name must be alpha­numeric, and it defaults to being empty.

## See Also

[`erl(1)`](erl_cmd.md), `m:compile`, `m:yecc`, `m:snmp`

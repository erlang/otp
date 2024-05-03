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
# config

Configuration file.

## Description

A _configuration file_ contains values for configuration parameters for the
applications in the system. The `erl` command-line argument
[`-config Name`](`e:erts:erl_cmd.md#config`) tells the system to use data in the
system configuration file `Name.config`.

The erl command-line argument [`-configfd`](`e:erts:erl_cmd.md#configfd`) works
the same way as the `-config` option but specifies a file descriptor to read
configuration data from instead of a file.

The configuration data from configuration files and file descriptors are read in
the same order as they are given on the command line. For example,
`erl -config a -configfd 3 -config b -configfd 4` would cause the system to read
configuration data in the following order `a.config`, file descriptor `3`,
`b.config`, and file descriptor `4`. If a configuration parameter is specified
more than once in the given files and file descriptors, the last one overrides
the previous ones.

Configuration parameter values in a configuration file or file descriptor
override the values in the application resource files (see [`app(4)`](app.md)).
The values in the configuration file are always overridden by command-line flags
(see [`erts:erl(1)`](`e:erts:erl_cmd.md`)).

The value of a configuration parameter is retrieved by calling
`application:get_env/1,2`.

## File Syntax

The configuration file is to be called `Name.config`, where `Name` is any name.

File `.config` contains a single Erlang term and has the following syntax:

```erlang
[{Application1, [{Par11, Val11}, ...]},
 ...
 {ApplicationN, [{ParN1, ValN1}, ...]}].
```

- **`Application = atom()`** - Application name.

- **`Par = atom()`** - Name of a configuration parameter.

- **`Val = term()`** - Value of a configuration parameter.

## sys.config

When starting Erlang in embedded mode, it is assumed that exactly one system
configuration file is used, named `sys.config`. This file is to be located in
`$ROOT/releases/Vsn`, where `$ROOT` is the Erlang/OTP root installation
directory and `Vsn` is the release version.

Release handling relies on this assumption. When installing a new release
version, the new `sys.config` is read and used to update the application's
configurations.

This means that specifying another `.config` file, or more `.config` files,
leads to an inconsistent update of application configurations. There is,
however, a way to point out other config files from a `sys.config`. How to do
this is described in the next section.

## Including Files from sys.config and -configfd Configurations

There is a way to include other configuration files from a `sys.config` file and
from a configuration that comes from a file descriptor that has been pointed out
with the [`-configfd`](`e:erts:erl_cmd.md#configfd`) command-line argument.

The syntax for including files can be described by the
[Erlang type language](`e:system:typespec.md`) like this:

```text
[{Application, [{Par, Val}]} | IncludeFile].
```

- **`IncludeFile = string()`** - Name of a `.config` file. The extension
  `.config` can be omitted. It is recommended to use absolute paths. If a
  relative path is used in a `sys.config`, `IncludeFile` is searched, first,
  relative to the `sys.config` directory, then relative to the current working
  directory of the emulator. If a relative path is used in a `-configfd`
  configuration, `IncludeFile` is searched, first, relative to the dictionary
  containing the [boot script](`e:sasl:script.md`) (see also the
  [`-boot`](`e:erts:erl_cmd.md#boot`) command-line argument) for the emulator,
  then relative to the current working directory of the emulator. This makes it
  possible to use `sys.config` for pointing out other `.config` files in a
  release or in a node started manually using `-config` or `-configfd` with the
  same result whatever the current working directory is.

When traversing the contents of a `sys.config` or a `-configfd` configuration
and a filename is encountered, its contents are read and merged with the result
so far. When an application configuration tuple `{Application, Env}` is found,
it is merged with the result so far. Merging means that new parameters are added
and existing parameter values are overwritten.

_Example:_

```text
sys.config:

["/home/user/myconfig1"
 {myapp,[{par1,val1},{par2,val2}]},
 "/home/user/myconfig2"].

myconfig1.config:

[{myapp,[{par0,val0},{par1,val0},{par2,val0}]}].

myconfig2.config:

[{myapp,[{par2,val3},{par3,val4}]}].
```

This yields the following environment for `myapp`:

```text
[{par0,val0},{par1,val1},{par2,val3},{par3,val4}]
```

The run-time system will abort before staring up if an include file specified in
`sys.config` or a `-configfd` configuration does not exist, or is erroneous.
However, installing a new release version will not fail if there is an error
while loading an include file, but an error message is returned and the
erroneous file is ignored.

## See Also

[`app(4)`](app.md), [`erts:erl(1)`](`e:erts:erl_cmd.md`),
[OTP Design Principles](`e:system:design_principles.md`)

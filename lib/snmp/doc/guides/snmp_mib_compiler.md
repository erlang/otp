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
# The MIB Compiler

The chapter _The MIB Compiler_ describes the MIB compiler and contains the
following topics:

- Operation
- Import
- Consistency checking between MIBs
- .hrl file generation
- Emacs integration
- Deviations from the standard

> #### Note {: .info }
>
> When importing MIBs, ensure that the imported MIBs as well as the importing
> MIB are compiled using the same version of the SNMP-compiler.

## Operation

The MIB must be written as a text file in SMIv1 or SMIv2 using an ASN.1 notation
before it will be compiled. This text file must have the same name as the MIB,
but with the suffix `.mib`. This is necessary for handling the `IMPORT`
statement.

The association file, which contains the names of instrumentation functions for
the MIB, should have the suffix `.funcs`. If the compiler does not find the
association file, it gives a warning message and uses default instrumentation
functions. (See [Default Instrumentation](snmp_instr_functions.md#snmp_3) for
more details).

The MIB compiler is started with a call to `snmpc:compile(<mibname>).` For
example:

```erlang
snmpc:compile("RFC1213-MIB").
```

The output is a new file which is called `<mibname>.bin`.

The MIB compiler understands both SMIv1 and SMIv2 MIBs. It uses the
MODULE-IDENTITY statement to determinate if the MIB is written in SMI version 1
or 2.

## Importing MIBs

The compiler handles the `IMPORT` statement. It is important to import the
compiled file and not the ASN.1 (source) file. A MIB must be recompiled to make
changes visible to other MIBs importing it.

The compiled files of the imported MIBs must be present in the current
directory, or a directory in the current path. The path is supplied with the
`{i, Path}` option, for example:

```erlang
snmpc:compile("MY-MIB",
       [{i, ["friend_mibs/", "../standard_mibs/"]}]).
```

It is also possible to import MIBs from OTP applications in an `"include_lib"`
like fashion with the `il` option. Example:

```erlang
snmpc:compile("MY-MIB",
       [{il, ["snmp/priv/mibs/", "myapp/priv/mibs/"]}]).
```

finds the latest version of the `snmp` and `myapp` applications in the OTP
system and uses the expanded paths as include paths.

Note that an SMIv2 MIB can import an SMIv1 MIB and vice versa.

The following MIBs are built-ins of the Erlang SNMP compiler: SNMPv2-SMI,
RFC-1215, RFC-1212, SNMPv2-TC, SNMPv2-CONF, and RFC1155-SMI. They cannot
therefore be compiled separately.

## MIB Consistency Checking

When an MIB is compiled, the compiler detects if several managed objects use the
same `OBJECT IDENTIFIER`. If that is the case, it issues an error message.
However, the compiler cannot detect Oid conflicts between different MIBs. These
kinds of conflicts generate an error at load time. To avoid this, the following
function can be used to do consistency checking between MIBs:

```text

erl>snmpc:is_consistent(ListOfMibNames).
```

`ListOfMibNames` is a list of compiled MIBs, for example
`["RFC1213-MIB", "MY-MIB"]`. The function also performs consistency checking of
trap definitions.

## .hrl File Generation

It is possible to generate an `.hrl` file which contains definitions of Erlang
constants from a compiled MIB file. This file can then be included in Erlang
source code. The file will contain constants for:

- object Identifiers for tables, table entries and variables
- column numbers
- enumerated values
- default values for variables and table columns.

Use the following command to generate a .hrl file from an MIB:

```text
erl>snmpc:mib_to_hrl(MibName).
```

## Emacs Integration

With the Emacs editor, the `next-error` (`` C-X ` ``) function can be used
indicate where a compilation error occurred, provided the error message is
described by a line number.

Use `M-x compile` to compile an MIB from inside Emacs, and enter:

```text
 erl -s snmpc compile <MibName> -noshell
```

An example of `<MibName>` is `RFC1213-MIB`.

## Compiling from a Shell or a Makefile

The `erlc` commands can be used to compile SNMP MIBs. Example:

```text
 erlc MY-MIB.mib
```

All the standard `erlc` flags are supported, e.g.

```text
 erlc -I mymibs -o mymibs -W MY-MIB.mib
```

The flags specific to the MIB compiler can be specified by using the `+` syntax:

```text
 erlc +'{group_check,false}' MY-MIB.mib
```

## Deviations from the Standard

In some aspects the Erlang MIB compiler does not follow or implement the SMI
fully. Here are the differences:

- Tables must be written in the following order: `tableObject`, `entryObject`,
  `column1`, ..., `columnN` (in order).
- Integer values, for example in the `SIZE` expression must be entered in
  decimal syntax, not in hex or bit syntax.
- Symbolic names must be unique within a MIB and within a system.
- Hyphens are allowed in SMIv2 (a pragmatic approach). The reason for this is
  that according to SMIv2, hyphens are allowed for objects converted from SMIv1,
  but not for others. This is impossible to check for the compiler.
- If a word is a keyword in any of SMIv1 or SMIv2, it is a keyword in the
  compiler (deviates from SMIv1 only).
- Indexes in a table must be objects, not types (deviates from SMIv1 only).
- A subset of all semantic checks on types are implemented. For example,
  strictly the `TimeTicks` may not be sub-classed but the compiler allows this
  (standard MIBs must pass through the compiler) (deviates from SMIv2 only).
- The `MIB.Object` syntax is not implemented (since all objects must be unique
  anyway).
- Two different names cannot define the same OBJECT IDENTIFIER.
- The type checking in the SEQUENCE construct is non-strict (i.e. subtypes may
  be specified). The reason for this is that some standard MIBs use this.
- A definition has normally a status field. When the status field has the value
  deprecated, then the MIB-compiler will ignore this definition. With the
  MIB-compiler option `{deprecated,true}` the MIB-compiler does not ignore the
  deprecated definitions.
- An object has a DESCRIPTIONS field. The descriptions-field will not be
  included in the compiled mib by default. In order to get the description, the
  mib must be compiled with the option `description`.

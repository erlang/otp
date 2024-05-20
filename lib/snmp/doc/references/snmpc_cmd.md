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
# snmpc

SNMP MIB compiler frontend

## Description

The `snmpc` program provides a way to run the SNMP MIB compiler of the Erlang
system.

## snmpc \[options] file.mib | file.bin

`snmpc` compile a SNMP MIB file, see [compile/1,2](`snmpc:compile/1`) for more
info.

It can also be used to generate a header file (.hrl) with definitions of Erlang
constants for the objects in the MIB, see [mib_to_hrl/1](`snmpc:mib_to_hrl/1`).

[](){: #options }

## Compiler options

The following options are supported (note that most of these relate to the
compilation of the MIB file):

[](){: #option_help }

- **\--help** - Prints help info.

  [](){: #option_version }

- **\--version** - Prints application and mib format version.

  [](){: #option_verbosity }

- **\--verbosity _verbosity_** - Print debug info.

  `verbosity` = `trace` | `debug` | `log` | `info` | `silence`

  Defaults to `silence`.

  [](){: #option_w } [](){: #option_warnings }

- **\--warnings | --W** - Print warning messages.

  [](){: #option_wae } [](){: #option_werror }

- **\--wae | --Werror** - Warnings as errors. Indicates that warnings shall be
  treated as errors.

  [](){: #option_odir }

- **\--o _directory_** - The directory where the compiler should place the
  output files. If not specified, output files will be placed in the current
  working directory.

  [](){: #option_idir }

- **\--i _Directory_** - Specifies the path to search for imported (compiled)
  MIB files. By default, the current working directory is always included.

  This option can be present several times, each time specifying _one_ path.

  [](){: #option_ildir }

- **\--il _Directory_** - This option (include_lib), specifies a list of
  directories to search for imported MIBs. It assumes that the first element in
  the directory name corresponds to an OTP application. The compiler will find
  the current installed version. For example, the value \["snmp/mibs/"] will be
  replaced by \["snmp-3.1.1/mibs/"] (or what the current version may be in the
  system). The current directory and the "snmp-home"/priv/mibs/ are always
  listed last in the include path.

  [](){: #option_sgc }

- **\--sgc** - This option (skip group check), if present, disables the group
  check of the mib compiler. That is, should the OBJECT-GROUP and the
  NOTIFICATION-GROUP macro(s) be checked for correctness or not.

  [](){: #option_dep }

- **\--dep** - Keep deprecated definition(s). If not specified the compiler will
  ignore deprecated definitions.

  [](){: #option_desc }

- **\--desc** - The DESCRIPTION field will be included.

  [](){: #option_ref }

- **\--ref** - The REFERENCE field will be included.

  [](){: #option_imp }

- **\--imp** - The IMPORTS field will be included.

  [](){: #option_mi }

- **\--mi** - The MODULE-IDENTITY field will be included.

  [](){: #option_mc }

- **\--mc** - The MODULE-COMPLIANCE field will be included.

  [](){: #option_ac }

- **\--ac** - The AGENT-CAPABILITIES field will be included.

  [](){: #option_mod }

- **\--mod _module_** - The module which implements all the instrumentation
  functions.

  The name of all instrumentation functions must be the same as the
  corresponding managed object it implements.

  [](){: #option_nd }

- **\--nd** - The default instrumentation functions will _not_ be used if a
  managed object have no instrumentation function. Instead this will be reported
  as an error, and the compilation aborts.

  [](){: #option_rrnac }

- **\--rrnac** - This option, if present, specifies that the row name assign
  check shall not be done strictly according to the SMI (which allows only the
  value 1).

  With this option, all values greater than zero is allowed (>= 1). This means
  that the error will be converted to a warning.

  By default it is not included, but if this option is present it will be.

  [](){: #see_also }

## SEE ALSO

[erlc(1)](`e:erts:erlc_cmd.md`), `m:compile`, `m:snmpc`

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
# typer

TypEr is a **TYP**e annotator for **ER**lang programs.

## Description

TypEr shows type information for Erlang modules. Additionally, it can
annotate the code of files with such type information.

[](){: #command_line }

## Using TypEr

TypEr is used from the command-line. This section provides a brief description
of the options. The same information can be obtained by writing the following in
a shell:

```text
typer --help
```

_Usage:_

```text
typer [--help] [--version] [--plt PLT] [--edoc]
      [--show | --show-exported | --annotate | --annotate-inc-files | --annotate-in-place]
      [-Ddefine]* [-I include_dir]* [-pa dir]* [-pz dir]*
      [-T application]* file* [-r directory*]
```

> #### Note {: .info }
>
> \* denotes that multiple occurrences of the option are possible.

_Options:_

- **`-r`** - Search directories recursively for .erl files below them. If a list
  of files is given, this option must given be after them.

- **`--show`** - Print type specifications for all functions on stdout. (This is
  the default behaviour; this option is not really needed.)

- **`--show-exported` (or `show_exported`)** - Same as `--show`, but print
  specifications for exported functions only. Specs are displayed sorted
  alphabetically according to the function's name.

- **`--annotate`** - Annotate the specified files with type specifications.

- **`--annotate-inc-files`** - Same as `--annotate`, but annotates all
  `-include()` files as well as all .erl files. (Use this option with caution —
  it is not well-tested.)

- **`--annotate-in-place`** - Annotate directly in the source code files,
  instead of dumping the annotated files in a different directory. (Use this
  option with caution — it is not well-tested.)

- **`--edoc`** - Print type information as Edoc `@spec` comments, not as type
  specs.

- **`--plt`** - Use the specified dialyzer PLT file rather than the default one.

- **`-T file*`** - The specified file(s) already contain type specifications and
  these are to be trusted in order to print specs for the rest of the files.
  (Multiple files or directories, separated by spaces, can be specified.)

- **`-Dname` (or `-Dname=value`)** - Pass the defined name(s) to TypEr. (\*\*)

- **`-I`** - Pass the include_dir to TypEr. (\*\*)

- **`-pa dir`** - Include `dir` in the path for Erlang. This is useful when
  analyzing files that use `-include_lib()` directives or parse transforms.

- **`-pz dir`** - Include `dir` in the path for Erlang. This is useful when
  analyzing files that use `-include_lib()` directives or parse transforms.

- **`--version` (or `-v`)** - Print the TypEr version and some more information
  and exit.

> #### Note {: .info }
>
> \*\* options `-D` and `-I` work the same way as in
> [erlc](`e:erts:erlc_cmd.md`).

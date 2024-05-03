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
# escript

`escript` provides support for running short Erlang programs without having to
compile them first, and an easy way to retrieve the command-line arguments.
`escript`s are created by either writing them by hand or using `escript:create/2`.

escripts are run by directly invoking them (does not work on Windows):

```text
script-name.escript [arg1 arg2...]
```

or by calling the `escript` program (works on all platforms):

```text
escript [escript-flags] script-name.escript [arg1 arg2...]
```

For example:

```text
$ chmod u+x factorial
$ cat factorial
```

```
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname factorial -mnesia debug verbose
main([String]) ->
    try
        N = list_to_integer(String),
        F = fac(N),
        io:format("factorial ~w = ~w\n", [N,F])
    catch
        _:_ ->
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: factorial integer\n"),
    halt(1).

fac(0) -> 1;
fac(N) -> N * fac(N-1).
```

```text
$ ./factorial 5
factorial 5 = 120
$ ./factorial
usage: factorial integer
$ ./factorial five
usage: factorial integer
```

The header of the Erlang script in the example differs from a normal Erlang
module. The first line is intended to be the interpreter line, which invokes
`escript`.

However, if you invoke the `escript` as follows, the contents of the first line
do not matter, but it cannot contain Erlang code as it will be ignored:

```text
$ escript factorial 5
```

The second line in the example contains an optional directive to the `Emacs`
editor, which causes it to enter the major mode for editing Erlang source files.
If the directive is present, it must be located on the second line.

If a comment selecting the [encoding](`m:epp#encoding`) exists, it can be
located on the second line.

> #### Note {: .info }
>
> The encoding specified by the above mentioned comment applies to the script
> itself. The encoding of the I/O-server, however, must be set explicitly as
> follows:
>
> ```erlang
> io:setopts([{encoding, latin1}])
> ```
>
> The default encoding of the I/O-server for
> [`standard_io`](`t:io:standard_io/0`) is `unicode` if its supported. (see
> section
> [Summary of Options](`e:stdlib:unicode_usage.md#unicode_options_summary`)) in
> the STDLIB User's Guide.

On the third line (or second line depending on the presence of the Emacs
directive), arguments can be specified to the emulator, for example:

```text
%%! -sname factorial -mnesia debug verbose
```

Such an argument line must start with `%%!` and the remaining line is
interpreted as arguments to the emulator.

If you know the location of the `escript` executable, the first line can
directly give the path to `escript`, for example:

```text
#!/usr/local/bin/escript
```

As any other type of scripts, Erlang scripts do not work on Unix platforms if
the execution bit for the script file is not set. (To turn on the execution bit,
use `chmod +x script-name`.)

The remaining Erlang script file can either contain Erlang _source code_, an
_inlined beam file_, or an _inlined archive file_.

An Erlang script file must always contain the `main/1` function. When the script
is run, the `main/1` function is called with a list of strings representing the
arguments specified to the script (not changed or interpreted in any way).

If the `main/1` function in the script returns successfully, the exit status for
the script is `0`. If an exception is generated during execution, a short
message is printed and the script terminates with exit status `127`.

To return your own non-zero exit code, call [`halt(ExitCode)`](`halt/1`), for
example:

```text
halt(1).
```

To retrieve the pathname of the script, call `escript:script_name/0` from your
script (the pathname is usually, but not always, absolute).

If the file contains source code (as in the example above), it is processed by
the `m:epp` preprocessor. This means that you, for example, can use predefined
macros (such as `?MODULE`) and include directives like the `-include_lib`
directive. For example, use

```erlang
-include_lib("kernel/include/file.hrl").
```

to include the record definitions for the records used by function
`file:read_link_info/1`. You can also select encoding by including an encoding
comment here, but if a valid encoding comment exists on the second line, it
takes precedence.

The script is checked for syntactic and semantic correctness before it is run.
If there are warnings (such as unused variables), they are printed and the
script will still be run. If there are errors, they are printed and the script
will not be run and its exit status is `127`.

Both the module declaration and the export declaration of the `main/1` function
are optional.

By default, the script will be compiled by the Erlang compiler.

It is possible to force it to be interpreted by including the following line
somewhere in the script file:

```erlang
-mode(interpret).
```

Execution of interpreted code is slower than compiled code, and some language
constructs will not work, but there is no requirement for the Erlang compiler
application to be available.

> #### Change {: .info }
>
> Before the Erlang/OTP 27 the script would be interpreted by default.

## Precompiled escripts

A script can also contains precompiled `beam` code. To create a precompiled
escript it is recommended that you use `escript:create/2`. In a
precompiled script, the interpretation of the script header is the same as in a
script containing source code. This means that you can make a `beam` file
executable by prepending the file with the lines starting with `#!` and `%%!`
mentioned above. In a precompiled script, the `main/1` function must be
exported.

Another option is to have an entire Erlang archive in the script. In an archive
script, the interpretation of the script header is the same as in a script
containing source code. This means that you can make an archive file executable
by prepending the file with the lines starting with `#!` and `%%!` mentioned
above. In an archive script, the `main/1` function must be exported. By default
the `main/1` function in the module with the same name as the basename of the
`escript` file is invoked. This behavior can be overridden by setting flag
`-escript main Module` as one of the emulator flags. `Module` must be the name
of a module that has an exported `main/1` function. For more information about
archives and code loading, see `m:escript` and `m:code`.

It is often very convenient to have a header in the escript, especially on Unix
platforms. However, the header is optional, so you directly can "execute" an
Erlang module, Beam file, or archive file without adding any header to them. But
then you have to invoke the script as follows:

```text
$ escript factorial.erl 5
factorial 5 = 120
$ escript factorial.beam 5
factorial 5 = 120
$ escript factorial.zip 5
factorial 5 = 120
```

## Bundling escripts

It is possible to bundle `escript`(s) with an Erlang runtime system to make it
self-sufficient and relocatable. In such a standalone system, the `escript`(s)
should be located in the top `bin` directory of the standalone system and given
`.escript` as file extension. Further the (built-in) `escript` program should be
copied to the same directory and given the script's original name (without the
`.escript` extension). This will enable use of the bundled Erlang runtime
system.

The (built-in) `escript` program first determines which Erlang runtime system to
use and then starts it to execute your script. Usually the runtime system is
located in the same Erlang installation as the `escript` program itself. But for
standalone systems with one or more escripts it may be the case that the
`escript` program in your path actually starts the runtime system bundled with
the escript. This is intentional, and typically happens when the standalone
system `bin` directory is not in the execution path (as it may cause its `erl`
program to override the desired one) and the `escript`(s) are referred to via
symbolic links from a `bin` directory in the path.

## Options Accepted By escript

- **`-c`** - Compiles the escript regardless of the value of the mode attribute.

- **`-d`** - Debugs the escript. Starts the debugger, loads the module
  containing the `main/1` function into the debugger, sets a breakpoint in
  `main/1`, and invokes `main/1`. If the module is precompiled, it must be
  explicitly compiled with option `debug_info`.

- **`-i`** - Interprets the escript regardless of the value of the mode
  attribute.

- **`-s`** - Performs a syntactic and semantic check of the script file.
  Warnings and errors (if any) are written to the standard output, but the
  script will not be run. The exit status is `0` if any errors are found,
  otherwise `127`.

> #### Note {: .info }
>
> The configuration of the Erlang emulator invoked by `escript` can also be
> controlled using the
> [environment variables understood by `erl`](erl_cmd.md#environment_variables).

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
# Compilation and Code Loading

How code is compiled and loaded is not a language issue, but is
system-dependent. This section describes compilation and code loading in
Erlang/OTP with references to relevant parts of the documentation.

## Compilation

Erlang programs must be _compiled_ to object code. The compiler can generate a
new file that contains the object code. The current abstract machine, which runs
the object code, is called BEAM, therefore the object files get the suffix
`.beam`. The compiler can also generate a binary which can be loaded directly.

The compiler is located in the module `compile` (see the `m:compile` manual page
in Compiler).

```erlang
compile:file(Module)
compile:file(Module, Options)
```

The Erlang shell understands the command `c(Module)` which both compiles and
loads `Module`.

There is also a module `make`, which provides a set of functions similar to the
UNIX type Make functions, see the `m:make` manual page in Tools.

The compiler can also be accessed from the OS prompt, see the
[erl(1)](`e:erts:erl_cmd.md`) manual page in ERTS.

```erlang
% erl -compile Module1...ModuleN
% erl -make
```

The `erlc` program provides an even better way to compile modules from the
shell, see the [erlc(1)](`e:erts:erlc_cmd.md`) manual page in ERTS. It
understands a number of flags that can be used to define macros, add search
paths for include files, and more.

```text
% erlc <flags> File1.erl...FileN.erl
```

[](){: #loading }

## Code Loading

The object code must be _loaded_ into the Erlang runtime system. This is handled
by the _code server_, see the `m:code` manual page in Kernel.

The code server loads code according to a code loading strategy, which is either
_interactive_ (default) or _embedded_. In interactive mode, code is searched for
in a _code path_ and loaded when first referenced. In embedded mode, code is
loaded at start-up according to a _boot script_. This is described in
[System Principles ](`e:system:system_principles.md#code_loading`).

## Code Replacement

Erlang supports change of code in a running system. Code replacement is done on
module level.

The code of a module can exist in two variants in a system: _current_ and _old_.
When a module is loaded into the system for the first time, the code becomes
'current'. If then a new instance of the module is loaded, the code of the
previous instance becomes 'old' and the new instance becomes 'current'.

Both old and current code is valid, and can be evaluated concurrently. Fully
qualified function calls always refer to current code. Old code can still be
evaluated because of processes lingering in the old code.

If a third instance of the module is loaded, the code server removes (purges)
the old code and any processes lingering in it is terminated. Then the third
instance becomes 'current' and the previously current code becomes 'old'.

To change from old code to current code, a process must make a fully qualified
function call.

_Example:_

```erlang
-module(m).
-export([loop/0]).

loop() ->
    receive
        code_switch ->
            m:loop();
        Msg ->
            ...
            loop()
    end.
```

To make the process change code, send the message `code_switch` to it. The
process then makes a fully qualified call to `m:loop()` and changes to current
code. Notice that `m:loop/0` must be exported.

For code replacement of funs to work, use the syntax
`fun Module:FunctionName/Arity`.

[](){: #on_load }

## Running a Function When a Module is Loaded

The `-on_load()` directive names a function that is to be run automatically when
a module is loaded.

Its syntax is as follows:

```text
-on_load(Name/0).
```

It is not necessary to export the function. It is called in a freshly spawned
process (which terminates as soon as the function returns).

The function must return `ok` if the module is to become the new current code
for the module and become callable.

Returning any other value or generating an exception causes the new code to be
unloaded. If the return value is not an atom, a warning error report is sent to
the error logger.

If there already is current code for the module, that code will remain current
and can be called until the `on_load` function has returned. If the `on_load`
function fails, the current code (if any) will remain current. If there is no
current code for a module, any process that makes an external call to the module
before the `on_load` function has finished will be suspended until the `on_load`
function have finished.

> #### Change {: .info }
>
> Before Erlang/OTP 19, if the `on_load` function failed, any previously current
> code would become old, essentially leaving the system without any working and
> reachable instance of the module.

In embedded mode, first all modules are loaded. Then all `on_load` functions are
called. The system is terminated unless all of the `on_load` functions return
`ok`.

_Example:_

```erlang
-module(m).
-on_load(load_my_nifs/0).

load_my_nifs() ->
    NifPath = ...,    %Set up the path to the NIF library.
    Info = ...,       %Initialize the Info term
    erlang:load_nif(NifPath, Info).
```

If the call to `erlang:load_nif/2` fails, the module is unloaded and a warning
report is sent to the error loader.

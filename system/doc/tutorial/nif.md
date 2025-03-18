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
# NIFs

This section outlines an example of how to solve the example problem in
[Problem Example](example.md) by using Native Implemented Functions (NIFs).

NIFs are a simpler and more efficient way of calling C-code than using port
drivers. NIFs are most suitable for synchronous functions, such as `foo` and
`bar` in the example, that do some relatively short calculations without side
effects and return the result.

A NIF is a function that is implemented in C instead of Erlang. NIFs appear as
any other functions to the callers. They belong to a module and are called like
any other Erlang functions. The NIFs of a module are compiled and linked into a
dynamic loadable, shared library (SO in UNIX, DLL in Windows). The NIF library
must be loaded in runtime by the Erlang code of the module.

As a NIF library is dynamically linked into the emulator process, this is the
fastest way of calling C-code from Erlang (alongside port drivers). Calling NIFs
requires no context switches. But it is also the least safe, because a crash in
a NIF brings the emulator down too.

## Erlang Program

Even if all functions of a module are NIFs, an Erlang module is still needed for
two reasons:

- The NIF library must be explicitly loaded by Erlang code in the same module.
- All NIFs of a module must have an Erlang implementation as well.

Normally these are minimal stub implementations that throw an exception. But
they can also be used as fallback implementations for functions that do not have
native implementations on some architectures.

NIF libraries are loaded by calling `erlang:load_nif/2`, with the name of the
shared library as argument. The second argument can be any term that will be
passed on to the library and used for initialization:

```erlang
-module(complex6).
-export([foo/1, bar/1]).
-nifs([foo/1, bar/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./complex6_nif", 0).

foo(_X) ->
    erlang:nif_error(nif_library_not_loaded).
bar(_Y) ->
    erlang:nif_error(nif_library_not_loaded).
```

Here, the directive `on_load` is used to get function `init` to be automatically
called when the module is loaded. If `init` returns anything other than `ok`,
such when the loading of the NIF library fails in this example, the module is
unloaded and calls to functions within it, fail.

Loading the NIF library overrides the stub implementations and cause calls to
`foo` and `bar` to be dispatched to the NIF implementations instead.

## NIF Library Code

The NIFs of the module are compiled and linked into a shared library. Each NIF
is implemented as a normal C function. The macro `ERL_NIF_INIT` together with an
array of structures defines the names, arity, and function pointers of all the
NIFs in the module. The header file `erl_nif.h` must be included. As the library
is a shared module, not a program, no main function is to be present.

The function arguments passed to a NIF appears in an array `argv`, with `argc`
as the length of the array, and thus the arity of the function. The Nth argument
of the function can be accessed as `argv[N-1]`. NIFs also take an environment
argument that serves as an opaque handle that is needed to be passed on to most
API functions. The environment contains information about the calling Erlang
process:

```c
#include <erl_nif.h>

extern int foo(int x);
extern int bar(int y);

static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
        return enif_make_badarg(env);
    }
    ret = foo(x);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM bar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int y, ret;
    if (!enif_get_int(env, argv[0], &y)) {
        return enif_make_badarg(env);
    }
    ret = bar(y);
    return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
    {"foo", 1, foo_nif},
    {"bar", 1, bar_nif}
};

ERL_NIF_INIT(complex6, nif_funcs, NULL, NULL, NULL, NULL)
```

Here, `ERL_NIF_INIT` has the following arguments:

- The first argument must be the name of the Erlang module as a C-identifier. It
  will be stringified by the macro.
- The second argument is the array of `ErlNifFunc` structures containing name,
  arity, and function pointer of each NIF.
- The remaining arguments are pointers to callback functions that can be used to
  initialize the library. They are not used in this simple example, hence they
  are all set to `NULL`.

Function arguments and return values are represented as values of type
`ERL_NIF_TERM`. Here, functions like `enif_get_int` and `enif_make_int` are used
to convert between Erlang term and C-type. If the function argument `argv[0]` is
not an integer, `enif_get_int` returns false, in which case it returns by
throwing a `badarg`\-exception with `enif_make_badarg`.

## Running the Example

_Step 1._ Compile the C code:

```text
unix> gcc -o complex6_nif.so -fpic -shared complex.c complex6_nif.c
windows> cl -LD -MD -Fe complex6_nif.dll complex.c complex6_nif.c
```

_Step 2:_ Start Erlang and compile the Erlang code:

```erlang
> erl
Erlang R13B04 (erts-5.7.5) [64-bit] [smp:4:4] [rq:4] [async-threads:0] [kernel-poll:false]

Eshell V5.7.5  (abort with ^G)
1> c(complex6).
{ok,complex6}
```

_Step 3:_ Run the example:

```erlang
3> complex6:foo(3).
4
4> complex6:bar(5).
10
5> complex6:foo("not an integer").
** exception error: bad argument
     in function  complex6:foo/1
        called as comlpex6:foo("not an integer")
```

Automatic Yielding of C Code
============================

Introduction
------------

Erlang [NIFs](http://erlang.org/doc/tutorial/nif.html) and
[BIFs](http://erlang.org/pipermail/erlang-questions/2009-October/046899.html)
should not run for a too long time without yielding (often referred to
as trapping in the source code of ERTS). The Erlang/OTP system gets
unresponsive, and some task may get prioritized unfairly if NIFs and
BIFs occupy scheduler threads for a too long time. Therefore, the most
commonly used NIFs and BIFs that may run for a long time can yield.

Problems
--------

Erlang NIFs and BIFs are typically implemented in the C programming
language. The C programming language does not have built-in support
for automatic yielding in the middle of a routine (referred to as
[coroutine support](https://en.wikipedia.org/wiki/Coroutine) in other
programming languages). Therefore, most NIFs and BIFs implement
yielding manually. Manual implementation of yielding has the advantage
of giving the programmer control over what should be saved and when
yielding should happen. Unfortunately, manual implementation of
yielding also leads to code with a lot of boilerplate that is more
difficult to read than corresponding code that does not
yield. Furthermore, manual implementation of yielding can be
time-consuming and error-prone, especially if the NIF or BIF is
complicated.

Solution
--------

A source-to-source transformer, called Yielding C Fun (YCF), has been
created to make it easier to implement yielding NIFs and BIFs. YCF is
a tool that takes a set of function names and a C source code file and
transforms the functions with the given names in the source code file
into yieldable versions that can be used as coroutines. YCF has been
created with yielding NIFs and BIFs in mind and has several features
that can be handy when implementing yielding NIFs and BIFs. The reader
is recommended to look at YCF's documentation for a detailed
description of YCF.

Yielding C Fun's Source Code and Documentation
----------------------------------------------

The source code of YCF is included in the folder
`"$ERL_TOP"/erts/lib_src/yielding_c_fun/` inside the source tree of
the Erlang/OTP system. The documentation of YCF can be found in
`"$ERL_TOP"/erts/lib_src/yielding_c_fun/README.md`. A rendered version
of YCF documentation can be found
[here](https://github.com/erlang/otp/blob/master/erts/lib_src/yielding_c_fun/README.md).

Yielding C Fun in the Erlang Run-time System
-------------------------------------------

At the time of writing, YCF is used for the following in ERTS:

* `ets:insert/2` and`ets:insert_new/2` (when these two functions get a list as their second parameter)
* `maps:from_keys/2`, `maps:from_list/1`, `maps:keys/1` and `maps:values/1`
* The functions `erts_qsort_ycf_gen_yielding`,
  `erts_qsort_ycf_gen_continue` and `erts_qsort_ycf_gen_destroy`
  implements a general purpose yieldable sorting routine that is used
  in the implementation of `erlang:term_to_binary/2`

Best Practices for YCF in the ERTS
-------------------------------------------

First of all, before starting to use YCF it is recommended to read
through its documentation in
[erts/lib_src/yielding_c_fun/README.md](https://github.com/erlang/otp/blob/master/erts/lib_src/yielding_c_fun/README.md)
to understand what limitations and functionalities YCF has.

### Mark YCF Transformed Functions

It is important that it is easy to see what functions are transformed
by YCF so that a programmer that edits these function are aware that
they have to follow certain restrictions. The convention for making
this clear is to have a comment above the function that explains that
the function is transformed by YCF (see `maps_values_1_helper` in
`erl_map.c` for an example). If only the transformed version of the
function is used, the convention is to "comment out" the source for the
function by surrounding it with the following `#ifdef` (this way, one
will not get warnings about unused functions):


```
#ifdef INCLUDE_YCF_TRANSFORMED_ONLY_FUNCTIONS
void my_fun() {
    ...
}
#endif /* INCLUDE_YCF_TRANSFORMED_ONLY_FUNCTIONS */
```

While editing the function one can define
`INCLUDE_YCF_TRANSFORMED_ONLY_FUNCTIONS` so that one can see errors
and warnings in the non-transformed source.


### Where to Place YCF Transformed Functions

The convention is to place the non-transformed source for the functions
that are transformed by YCF in the source file where they naturally
belong. For example, the functions for the map BIFs are placed in
`erl_map.c` together with the other map-related functions. When
building, YCF is invoked to generate the transformed versions of the
functions into a header file that is included in the source file that
contains the non-transformed version of the function (search for
YCF in `$ERL_TOP/erts/emulator/Makefile.in` to see examples of how YCF
can be invoked).

If a function `F1` that is transformed by one YCF invocation depends
on a function `F2` that is transformed by another YCF invocation, one
needs to tell YCF that `F2` is an YCF transformed function so that
`F1` can call the transformed version (see the documentation of
`-fexternal` in [YCF's documentation](https://github.com/erlang/otp/blob/master/erts/lib_src/yielding_c_fun/README.md)
for more information about how to do that).


### Reduce Boilerplate Code with `erts_ycf_trap_driver`

The `erts_ycf_trap_driver` is a C function that implements common code
that is needed by all BIFs that do their yielding with YCF. It is
recommended to use this function when possible. A good way to learn
how to use `erts_ycf_trap_driver` is to look at the implementations of
the BIFs `maps:from_keys/2`, `maps:from_list/1`, `maps:keys/1` and
`maps:values/1`.

Some BIFs may not be able to use `erts_ycf_trap_driver` as they need
to do some custom work after yielding. Examples of that are the BIFs
`ets:insert/2` and`ets:insert_new/2` that publish the yield state in
the ETS table structure so that other threads can help in completing
the operation.


### Testing and Finding Problems in YCF Generated Code

A good way to test both code with manual yielding and YCF generated
yielding is to write test cases that cover the places where the code
can yield (yielding points) and setting the yield limit so that it
yields every time the yielding points are reached. With YCF this can
be accomplished by passing a pointer to the value 1 as the
`ycf_nr_of_reductions` parameter (i.e., the first parameter of the
`*_ycf_gen_yielding` and `*_ycf_gen_continue` functions).

The YCF flag `-debug` makes YCF generate code that checks for pointers
to the C stack when yielding. When such a pointer is found the
location of the found pointer will be printed out and the program will
crash. This can save a lot (!) of time when porting already existing C
code to yield with YCF. To make the `-debug` option work as intended,
one has to tell YCF where the stack starts before calling the YCF
generated function. The functions `ycf_debug_set_stack_start` and
`ycf_debug_reset_stack_start` has been created to make this easier
(see the implementation of `erts_ycf_trap_driver` for how to use these
functions). It is recommended to set up building of ERTS so that debug
builds of ERTS runs with YCF code generated with the `-debug` flag
while production code runs with YCF code that has been generated
without the `-debug` flag.

It is a good practice to look through the code generated by YCF to try
to find things that are not correctly transformed. Before doing that
one should format the generated code with an automatic source code
formatter (the generated code is quite unreadable otherwise). If YCF
does not transform something correctly, it is almost certainly possible
to fix that by rewriting the code (see the YCF documentation for what
is supported and what is not supported). For example, if you have a
inline struct variable declaration (for example,
`struct {int field1; int field2;} mystructvar;`), YCF will not recognize this
as a variable declaration but you can fix this by creating a `typedef`
for the struct.

YCF's hooks can be useful when debugging code that has been transformed
by YCF. For examples, the hooks can be used to print the value of a
variable when yielding and when resuming after yielding.

Unfortunately, YCF does not handle C code with syntactical errors very
well and can crash or produce bad output without giving any useful
error message when given syntactically incorrect C code (for example,
a missing parenthesis). Therefore, it is recommended to always check
the code with a normal C compiler before transforming it with YCF.

### Common Pitfalls

* **Pointers to the stack** The stack might be located somewhere else
  when a yielded function continues to execute so pointers to
  variables that are located on the stack can be a problem. As
  mentioned in the previous section, the `-debug` option is a good way
  to detect such pointers. YCF has functionality to make it easier to
  port code that has pointers to the stack (see the documentation of
  `YCF_STACK_ALLOC` in the YCF documentation for more
  information). Another way to fix pointers to the stack, that
  sometimes can be convenient, is to use YCF's hooks to set up
  pointers to the stack correctly when a yielded function resumes.

* **Macros** YCF does not expand macros so variable declarations,
  return statements, and gotos etc that are "hidden" by macros can be
  a problem. It is therefore smart to check all macros in code that is
  transformed by YCF so that they do not contain anything that YCF
  needs to transform.

* **Memory Allocation in Yielding Code** If a process is killed while
  executing a BIF that is yielded, one has to make sure that memory
  and other resources that is allocated by the yielded code is
  freed. This can be done, e.g., by calling the generated
  `*_ycf_gen_destroy` function from the `dtor` of a magic binary that
  holds a reference to trap state. YCF's `ON_DESTROY_STATE` and
  `ON_DESTROY_STATE_OR_RETURN` hooks can be used to free any resources
  that has been manually allocated inside a yielding function when the
  function's `*_ycf_gen_destroy` function is executed. The
  `erts_ycf_trap_driver` takes care of calling the `*_ycf_gen_destroy`
  function so you do not need to worry about that if you are using
  `erts_ycf_trap_driver`.

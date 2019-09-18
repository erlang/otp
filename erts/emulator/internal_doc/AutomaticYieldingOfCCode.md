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
[here](https://github.com/erlang/otp/erts/lib_src/yielding_c_fun/README.md).

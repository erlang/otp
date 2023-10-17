<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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
# Debugging NIFs and Port Drivers

## With great power comes great responsibilty

NIFs and port driver code run inside the Erlang VM OS process (the "Beam"). To
maximize performance the code is called directly by the same threads executing
Erlang beam code and has full access to all the memory of the OS process. A
buggy NIF/driver can thus make severe damage by corrupting memory.

In a best case scenario such memory corruption is detected immediately causing
the Beam to crash generating a core dump file which can be analyzed to find the
bug. However, it is very common for memory corruption bugs to not be immediately
detected when the faulty write happens, but instead much later, for example when
the calling Erlang process is garbage collected. When that happens it can be
very hard to find the root cause of the memory corruption by analysing the core
dump. All traces that could have indicated which specific buggy NIF/driver that
caused the corruption may be long gone.

Another kind of bugs that are hard to find are _memory leaks_. They may go
unnoticed and not cause problem until a deployed system has been running for a
long time.

The following sections describe tools that make it easier to both detect and
find the root cause of bugs like this. These tools are actively used during
development, testing and troubleshooting of the Erlang runtime system itself.

- [Debug emulator](debugging.md#debug)
- [Address Sanitizer](debugging.md#asan)
- [Valgrind](debugging.md#valgrind)
- [rr - Record and Replay](debugging.md#rr)

[](){: #debug }

## Debug emulator

One way to make debugging easier is to run an emulator built with target
`debug`. It will

- _Increase probability of detecting bugs earlier_. It contains a lot more
  runtime checks to ensure correct use of internal interfaces and data
  structures.
- _Generate a core dump that is easier to analyze_. Compiler optimizations are
  turned off, which stops the compiler from "optimizing away" variables, thus
  making it easier/possible to inspect their state.
- _Detect lock order violations_. A runtime lock checker will verify that the
  locks in the [`erl_nif`](`e:erts:erl_nif.md`) and
  [`erl_driver`](`e:erts:erl_driver.md`) APIs are seized in a consistent order
  that cannot result in deadlock bugs.

In fact, we recommend to use the debug emulator as default during development of
NIFs and drivers, regardless if you are troubleshooting bugs or not. Some subtle
bugs may not be detected by the normal emulator and just happen to work anyway
by chance. However, another version of the emulator, or even different
circumstances within the same emulator, may cause the bug to later provoke all
kinds of problems.

The main disadvantage of the `debug` emulator is its reduced performance. The
extra runtime checks and lack of compiler optimizations may result in a slowdown
with a factor of two or more depending on load. The memory footprint should be
about the same.

If the `debug` emulator is part of the Erlang/OTP installation, it can be
started with the [`-emu_type`](`e:erts:erl_cmd.md#emu_type`) option.

```text
> erl -emu_type debug
Erlang/OTP 25 [erts-13.0.2] ... [type-assertions] [debug-compiled] [lock-checking]

Eshell V13.0.2  (abort with ^G)
1>
```

If the `debug` emulator is not part of the installation, you need to
[build it from the Erlang/OTP source code](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_building_how-to-build-a-debug-enabled-erlang-runtime-system`).
After building from source either make an Erlang/OTP installation or you can run
the debug emulator directly in the source tree with the `cerl` script:

```text
> $ERL_TOP/bin/cerl -debug
Erlang/OTP 25 [erts-13.0.2] ... [type-assertions] [debug-compiled] [lock-checking]

Eshell V13.0.2  (abort with ^G)
1>
```

The `cerl` script can also be used as a convenient way to start the debugger
`gdb` for core dump analysis:

```text
> $ERL_TOP/bin/cerl -debug -core core.12345
or
> $ERL_TOP/bin/cerl -debug -rcore core.12345
```

The first variant starts Emacs and runs `gdb` within, while the other `-rcore`
runs `gdb` directly in the terminal. Apart from starting `gdb` with the correct
`beam.debug.smp` executable file it will also read the file
`$ERL_TOP/erts/etc/unix/etp-commands` which contains a lot of `gdb` command for
inspecting a beam core dump. For example, the command `etp` that will print the
content of an Erlang term (`Eterm`) in plain Erlang syntax.

[](){: #asan }

## Address Sanitizer

[AddressSanitizer](https://clang.llvm.org/docs/AddressSanitizer.html) (asan) is
an open source programming tool that detects memory corruption bugs such as
buffer overflows, use-after-free and memory leaks. AddressSanitizer is based on
compiler instrumentation and is supported by both gcc and clang.

Similar to the `debug` emulator, the `asan` emulator runs slower than normal,
about 2-3 times slower. However, it also has a larger memory footprint, about 3
times more memory than normal.

To get full effect you should compile both your own NIF/driver code as well as
the Erlang emulator with AddressSanitizer instrumentation. Compile your own code
by passing option `-fsanitize=address` to gcc or clang. Other recommended
options that will improve the fault identification are `-fno-common` and
`-fno-omit-frame-pointer`.

Build and run the emulator with AddressSanitizer support by using the same
procedure as for the debug emulator, except use the `asan` build target instead
of `debug`.

- **Run in source tree** - If you run the `asan` emulator directly in the source
  tree with the `cerl` script you only need to set environment variable
  `ASAN_LOG_DIR` to the directory where the error log files will be generated.

  ```text
  > export ASAN_LOG_DIR=/my/asan/log/dir
  > $ERL_TOP/bin/cerl -asan
  Erlang/OTP 25 [erts-13.0.2] ... [address-sanitizer]

  Eshell V13.0.2  (abort with ^G)
  1>
  ```

  You may however also want to set `ASAN_OPTIONS="halt_on_error=true"` if you
  want the emulator to crash when an error is detected.

- **Run installed Erlang/OTP** - If you run the `asan` emulator in an installed
  Erlang/OTP with `erl -emu_type asan` you need to set the path to the error log
  _file_ with

  ```text
  > export ASAN_OPTIONS="log_path=/my/asan/log/file"
  ```

  To avoid false positive memory leak reports from the emulator itself set
  `LSAN_OPTIONS` (LSAN=LeakSanitizer):

  ```text
  > export LSAN_OPTIONS="suppressions=$ERL_TOP/erts/emulator/asan/suppress"
  ```

  The `suppress` file is currently not installed but can be copied manually from
  the source tree to wherever you want it.

Memory corruption errors are reported by AddressSanitizer when they happen, but
memory leaks are only checked and reported by default then the emulator
terminates.

## Valgrind

An even more heavy weight debugging tool is [Valgrind](https://valgrind.org). It
can also find memory corruption bugs and memory leaks similar to `asan`.
Valgrind is not as good at buffer overflow bugs, but it will find use of
undefined data, which is a type of error that `asan` cannot detect.

Valgrind is much slower than `asan` and it is incapable at exploiting CPU
multicore processing. We therefore recommend `asan` as the first choice before
trying valgrind.

Valgrind runs as a virtual machine itself, emulating execution of hardware
machine instructions. This means you can run almost any program unchanged on
valgrind. However, we have found that the beam executable benefits from being
compiled with special adaptions for running on valgrind.

Build the emulator with `valgrind` target the same as is done for `debug` and
`asan`. Note that `valgrind` needs to be installed on the machine before the
build starts.

Run the `valgrind` emulator directly in the source tree with the `cerl` script.
Set environment variable `VALGRIND_LOG_DIR` to the directory where the error log
files will be generated.

```text
> export VALGRIND_LOG_DIR=/my/valgrind/log/dir
> $ERL_TOP/bin/cerl -valgrind
Erlang/OTP 25 [erts-13.0.2] ... [valgrind-compiled]

Eshell V13.0.2  (abort with ^G)
1>
```

[](){: #rr }

## rr - Record and Replay

Last but not least, the fantastic interactive debugging tool
[rr](https://rr-project.org/), developed by Mozilla as open source. `rr` stands
for Record and Replay. While a core dump represents only a static snapshot of
the OS process when it crashed, with `rr` you instead record the entire session,
from start of the OS process to the end (the crash). You can then replay that
session from within `gdb`. Single step, set breakpoints and watchpoints, and
even _execute backwards_.

Considering its powerful utility, `rr` is remarkably light weight. It runs on
Linux with any reasonably modern x86 CPU. You may get a two times slowdown when
executing in recording mode. The big weakness is its inability to exploite CPU
multicore processing. If the bug is a race condition between concurrently
running threads, it may be hard to reproduce with `rr`.

`rr` does not require any special instrumented compilation. However, if
possible, run it together with the `debug` emulator, as that will result in a
much nicer debugging experience. You run `rr` in the source tree using the
`cerl` script.

Here is an example of a typical session. First we catch the crash in an rr
recording session:

```text
> $ERL_TOP/bin/cerl -debug -rr
rr: Saving execution to trace directory /home/foobar/.local/share/rr/beam.debug.smp-1.
Erlang/OTP 25 [erts-13.0.2]

Eshell V13.0.2  (abort with ^G)
1> mymod:buggy_nif().
Segmentation fault
```

Now we can replay that session with `rr replay`:

```text
> rr replay
GNU gdb (Ubuntu 9.2-0ubuntu1~20.04.1) 9.2
:
(rr) continue
:
Thread 2 received signal SIGSEGV, Segmentation fault.
(rr) backtrace
```

You get the call stack at the moment of the crash. Bad luck, it is somewhere
deep down in the garbage collection of the beam. But you manage to figure out
that variable `hp` points to a broken Erlang term.

Set a watch point on that memory position and resume execution _backwards_. The
debugger will then stop at the exact position when that memory position `*hp`
was written.

```text
(rr) watch -l *hp
Hardware watchpoint 1: -location *hp
(rr) reverse-continue
Continuing.

Thread 2 received signal SIGSEGV, Segmentation fault.
```

This is a quirk to be aware about. We started by executing forward until it
crashed with SIGSEGV. We are now executing backwards from that point, so we are
hitting the same SIGSEGV again but from the other direction. Just continue
backwards once more to move past it.

```text
(rr) reverse-continue
Continuing.

Thread 2 hit Hardware watchpoint 1: -location *hp

Old value = 42
New value = 0
```

And here we are at the position when someone wrote a broken term on the process
heap. Note that "Old value" and "New value" are reversed when we execute
backwards. In this case the value 42 was written on the heap. Let's see who the
guilty one is:

```text
(rr) backtrace
```

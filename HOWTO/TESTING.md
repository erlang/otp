Testing Erlang/OTP
==================

Before you start testing you need to have the Erlang release which you
are going to test in your path. See [$ERL_TOP/HOWTO/INSTALL.md][] for
instructions on how to build an Erlang release.

*NOTE*: This instructions may vary for different versions of Erlang/OTP,
so make sure to read the instructions for the version that you are testing.

Short version
-------------
Move to the top directory of the Erlang release you want to test, for example:
cd $HOME/git/otp

```bash
export ERL_TOP=`pwd`            # Define where the build root is
./configure && make             # Build all of Erlang/OTP
make test                       # Test all of Erlang/OTP
```

The tests will take a long time to finish and will print a lot of logs to the
console even if tests pass. A full run takes close about 6 hours on a relatively
modern machine. See [Running tests while developing][] for details on how to run
only a subset of the tests.

Where are the tests
-------------------

There are a lot of tests which test Erlang/OTP (as of 2012 about 12000) and
they are located in different places throughout the source tree. Below is a list
of the places where you can expect to find tests:

* $ERL_TOP/lib/AppName/test/
* $ERL_TOP/erts/test/
* $ERL_TOP/erts/emulator/test/
* $ERL_TOP/erts/epmd/test/

Writing tests
-------------

All tests are [common_test][] suites and follow the same pattern as all
[common_test][] suites. However, a couple of corner cases are
handled by a test wrapper called `ts`. `ts` allows the test writer to put
`Makefile.src` and `Makefile.first` files in the [data_dir][] and a special
directory called `all_SUITE_data`.

`Makefile.first` is run before any other Makefile and is typically used to
generate .hrl files which are needed by other test suites. At the moment only
the erl_interface tests use this feature and it should remain that way.

`Makefile.src` is configured to a `Makefile` using the `variables` created when
[configuring the tests][]. These `Makefile`s are later run by the test suite
to compile whatever platform specific code the tests need to run.

Running tests while developing
------------------------------

The `make test` command works when the current directory contains a directory
called test and in the root directory of the source code tree.

Below are some examples that illustrate how `make test` can be
used:

```bash
# ERL_TOP needs to be set correctly
cd /path/to/otp
export ERL_TOP=`pwd`

# Build Erlang/OTP
#
# Note that make test will only compile test code except when
# make test is executed from $ERL_TOP.
./otp_build setup -a

# Run all test suites for an application
(cd $ERL_TOP/lib/asn1 && make test)
make asn1_test

# Run a test suite (The ARGS variable is passed to ct_run)
(cd $ERL_TOP/lib/stdlib && make test ARGS="-suite ets_SUITE")
make stdlib_test ARGS="-suite ets_SUITE"

# Run a test case
(cd $ERL_TOP/erts/emulator && make test ARGS="-suite binary_SUITE -case deep_bitstr_lists")
make emulator_test ARGS="-suite binary_SUITE -case deep_bitstr_lists"

# Run all tests
#
# When executed from $ERL_TOP, "make test" will first release and
# configure all tests and then attempt to run all tests with `ts:run`.
# This will take several hours.
(cd $ERL_TOP && make test)
```

For more examples see [DEVELOPMENT.md](DEVELOPMENT.md)

Releasing tests
---------------

When not testing in the source tree, you first need to release all tests.
The easiest way to do this is to use `otp_build` like this:

```bash
export ERL_TOP=`pwd`; ./otp_build tests
```

This will release all tests in Erlang/OTP to `$ERL_TOP/release/tests/`. If you
want to change the directory where the tests are released to use the `TESTROOT`
environmental variable.

In the `$TESTROOT` you should now see *_test folders. These folders contain
everything needed to test Erlang/OTP and are platform independent; if you are
testing Erlang on multiple platforms you just have to release on one and copy
the tests to all other platforms.

### Releasing cross tests

For releasing tests in a cross compilation environment see [$ERL_TOP/HOWTO/INSTALL-CROSS.md][].

Configuring and Running tests
-----------------------------

Running tests is done by first navigating to the `$TESTROOT/test_server` folder
created when you released the tests and then start `erl` in that directory. The
emulator flags specified will be used in the test runs. For example, if you want
to test using async threads you have to supply `+A 10` to `erl` when you start it.

To configure and run the tests `ts` is used. `ts` is a wrapper module to
[common_test][] which takes care of configuration and build issues before
[common_test][] is started.

`ts` has a lot of special options and functions which can be useful when
testing Erlang/OTP. For a full listing issue `ts:help()` in the erlang shell.

### Configuring the test environment

Before running released tests you have to install them on the target system.
Installing the tests is done by calling `ts:install().` in the Erlang shell
which you intend to test from. `ts:install()` is basically a wrapper to a
configure script and some Erlang code which figures out what your system looks
like and what kind of emulator you are testing with. `ts:install()` can also
take some arguments if necessary, see `ts:help()` for details.

All variables created by `ts:install()` are found in
`$TESTROOT/test_server/variables`.

### Running the tests

To run all test suites go to `$TESTROOT/test_server` fire up an Erlang shell and type:

```erlang
ts:run().
```

Note that running all tests will require several hours, so you may want to run
the test cases for a single application

```erlang
ts:run(Application, [batch]).
```

or even part of the test suite for an application, for example

```erlang
ts:run(emulator, bs_construct_SUITE, [batch]).
```

to run the tests in the `bs_construct_SUITE` module (testing construction of
binaries using the binary syntax).

It is also possible to run a single test case by the specifying the module name and a function name:

```erlang
ts:run(emulator, bs_bincomp_SUITE, byte_aligned, [batch]).
```

Run `ts:help().` for more information.

As of R14B02 it is also possibly to start all tests but the erl_interface tests
by invoking Common Test directly from the released applications test directory,
i.e.

```bash
cd $TESTROOT/test_server
$ERL_TOP/bin/ct_run -suite ../compiler_test/andor_SUITE -case t_orelse
```

Running [ct_run][] from the command line still requires you to do the
`ts:install()` step above.

Examining the results
---------------------

Open the file `$ERL_TOP/release/tests/test_server/index.html` in a web browser. Or open
`$ERL_TOP/release/tests/test_server/suite.log.latest.html` when a test suite is running to
examine the results so far for the currently executing test suite.


Run tests with Address Sanitizer
--------------------------------

First build emulator with `asan` build target.
See [$ERL_TOP/HOWTO/INSTALL.md][].

Set environment variable `ASAN_LOG_DIR` to the directory
where the error logs will be generated.

```bash
export ASAN_LOG_DIR=$TESTROOT/test_server/asan_logs
mkdir $ASAN_LOG_DIR
```

Set environment variable `TS_RUN_EMU` to `asan`.

```bash
export TS_RUN_EMU=asan
```

Then run the tests you want with `ts:run` as [described above](#running-the-tests).
Either inspect the log files directly or use the script at
`$ERL_TOP/erts/emulator/asan/asan_logs_to_html` to read all log files
in `$ASAN_LOG_DIR` and distill them into one html page
`asan_summary.html`. Repeated reports from the same memory leak will
for example be ignored by the script and make it easier to analyze.


Run tests with Valgrind
-----------------------

First make sure [valgrind][] is installed, then build OTP from source
and build the emulator with `valgrind` build target. See
[$ERL_TOP/HOWTO/INSTALL.md][].

Set environment variable `VALGRIND_LOG_DIR` to the directory
where the valgrind error logs will be generated.

```bash
export VALGRIND_LOG_DIR=$TESTROOT/test_server/vg_logs
mkdir $VALGRIND_LOG_DIR
```

Set environment variable `TS_RUN_EMU` to `valgrind`.

```bash
export TS_RUN_EMU=valgrind
```

Then run the tests you want with `ts:run` as [described above](#running-the-tests)
and inspect the log file(s) in `$VALGRIND_LOG_DIR`.


   [ct_run]: http://www.erlang.org/doc/man/ct_run.html
   [ct hook]: http://www.erlang.org/doc/apps/common_test/ct_hooks_chapter.html
   [$ERL_TOP/HOWTO/INSTALL.md]: INSTALL.md
   [$ERL_TOP/HOWTO/INSTALL-CROSS.md]: INSTALL-CROSS.md#testing-the-cross-compiled-system
   [common_test]: http://www.erlang.org/doc/man/ct.html
   [data_dir]: http://www.erlang.org/doc/apps/common_test/write_test_chapter.html#data_priv_dir
   [configuring the tests]: #configuring-the-test-environment
   [valgrind]: https://valgrind.org
   [Running tests while developing]: #running-tests-while-developing

   [?TOC]: true

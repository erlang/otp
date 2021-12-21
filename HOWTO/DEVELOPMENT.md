# Developing Erlang/OTP

The Erlang/OTP development repository is quite large and the make system
contains a lot of functionality to help when a developing. This howto
will try to showcase the most important features of the make system.

The guide is mostly aimed towards development on a Unix platform, but
most things should work also work using WSL on Windows. The guide also
assumes that you are working in the git repositiory. Many of the
scripts and tools described here are not available in the prebuilt tar
archive that you can download for each release.

*WARNING*: Only some of APIs mentioned in this guide are supported. This
means that they may be removed or changed without prior notice, so do
not depend on them in CI. For supported make targets see the
[Install howto](INSTALL.md) and the [Testing howto](TESTING.md).

The make system is not always as robust as one might like, so if for
any reason something does not work, try doing a `git clean -Xfdq` and
start from the beginning again. This normally only needs to be done when
you jump in between different git branches, but it can a good thing to
keep in mind whenever things do not work as you expect them to.

*NOTE*: This instructions may vary for different versions of Erlang/OTP,
so make sure to read the instructions for the version that you are working
with.

1. [Short version](#short-version)
2. [Preparations](#preparations)
    1. [Faster builds](#faster-builds)
3. [Configuring](#configuring)
    1. [Help](#help)
4. [Building and testing](#building-and-testing)
    1. [Build and test a specific application](#build-and-test-a-specific-application)
    2. [Preloaded and Primary Bootstrap](#preloaded-and-primary-bootstrap)
    3. [Types and Flavors](#types-and-Flavors)
    4. [cerl](#cerl)
    5. [Static analysis](#static-analysis)
5. [Running test cases](#running-test-cases)
6. [Writing and building documentation](#writing-and-building-documentation)

## Short version

First make sure you have done all [preparations](#preparations) then
do this:

```bash
git clone -b maint git@github.com:erlang/otp
cd otp && export ERL_TOP=`pwd`
./otp_build configure && make
```

When you have done changes, added tests or updated documentation, build and test like this:

```bash
cd lib/$APPLICATION_NAME
make            # Rebuid application
make test       # Run application tests
make dialyzer   # Run dialyzer
make docs       # Build the docs
make xmllint    # Run xmllint on the docs
```

## Preparations

Before you start working you need to clone the Erlang/OTP git repository
and install any dependencies that you do not have. See
[Required Utilities](INSTALL.md#required-utilities) and
[Optional Utilities](INSTALL.md#optional-utilities) in [INSTALL.md](INSTALL.md)
for a list of utilities to install. (Windows has its own [INSTALL Guide](INSTALL-WIN32.md)
with its own [Required Utilities](INSTALL-WIN32.md#tools-you-need-and-their-environment)).

Then you need to set `ERL_TOP` to point at the repository you are developing in.
Not all make commands needs this environment variable set, but many do so it is
good to get into the habit of always setting it.

```bash
cd /path/to/repository/otp
export ERL_TOP=`pwd`
```

Make sure that you have read the [Contributing to Erlang/OTP](../CONTRIBUTING.md)
guide if you intend to make a contribution to Erlang/OTP.

### Faster builds

Both `configure` and `make` take advantage of running in parallel if told to,
so in order to speed up your development environment make sure to set:

```bash
## Change N to be at least the number of cores or hyper-threads available
export MAKEFLAGS=-jN
```

The Erlang compiler can be run using a [Compile Server](https://www.erlang.org/doc/man/erlc.html#compile-server),
this can cut from the total build time of Erlang/OTP by quite a lot,
especially if you have a relatively slow machine.
To enable set this environment variable:

```bash
export ERLC_USE_SERVER=true
```

Re-building all application in Erlang/OTP can take a while so it is possible
to build only a subset of the applications. This is done by setting either
`OTP_SMALL_BUILD` or `OTP_TINY_BUILD` to `true` when doing make at the top
level. However, the simplest way is probably to just use the `./otp_build`
wrapper that takes the options `-t` (tiny) or `-a` (all) and defaults to
a small build.

```bash
# You need to have done ./configure before calling make or boot.
OTP_TINY_BUILD=true make      ## Equivalent to ./otp_build boot -t
OTP_SMALL_BUILD=true make     ## Equivalent to ./otp_build boot
./otp_build boot -a           ## Equivalent to make
```

## Configuring

You run configure by issuing the command:

```bash
./otp_build configure
```

On all operating systems except Windows you can also just run:

```bash
./configure
```

If you change any `Makefile`s you will need to re-run configure.
If you update any `configure.ac` scripts you need to
[update the configure scripts](INSTALL.md#updating-configure-scripts).

### Help

The toplevel configure will give help about the features that it provides.
To get a full list of all features you need to use:

```bash
./configure --help=r
```

There is documentation for what most of the options mean in the
[INSTALL.md](INSTALL.md#Configuring) howto.

## Building and testing

After you have done configure, you can do

```bash
make
```

on the top of this repository. That will compile all of Erlang/OTP.

You can also build a specific application:

```bash
make stdlib
make common_test
```

These make commands do not manage any dependencies, so if an application needs
something from another you need to make sure that it is built. It is therefore
good practice to first build all of Erlang/OTP and then build just the one that
you are updating.

You can also run tests from the top:

```bash
make test                # Run all tests, takes a **very** long time
make stdlib_test         # Run only stdlib tests, takes less time
                         # Run only lists_SUITE, takes even less time
make stdlib_test ARGS="-suite lists_SUITE"
                         # Run only member testcase in lists_SUITE
make stdlib_test ARGS="-suite lists_SUITE -case member"
```

See [ct_run](https://www.erlang.org/doc/man/ct_run.html#) for a list of all options
that you can pass to ARGS.

You can run static analysis test:

```bash
make dialyzer            # Checks all of Erlang/OTP source code
make xmllint             # Checks all documentation for xmllint errors
```

Most of the above targets also works for a "phony" target called `emulator` that
represents erts and all its tools. So you can do this:

```bash
make emulator            # Build erts, epmd etc
make emulator_test       # Run all emulator tests
```

If you want to pass a run-time flag to the emulator running the tests you can
use the `ERL_ARGS` flags to `make test`. For example if you want to run tests
using [off heap message queue data](https://www.erlang.org/doc/man/erlang.html#process_flag_message_queue_data)
for all process you would do this:

```bash
ERL_ARGS="+hmqd off_heap" make emulator_test
```

### Build and test a specific application

You can also build the application from within itself. Like this:

```bash
cd lib/stdlib && make
```

Each application has a bunch of make targets that you can use.

```bash
make                                # build all source for this application
make test                           # run all tests for this application
make test ARGS="-suite lists_SUITE" # run the lists_SUITE tests
make dialyzer                       # run dialyzer for this application
make docs                           # build all docs for this application
make docs DOC_TARGETS="html"        # build html docs for this application
make xmllint                        # run xmllint on the docs for this application
```

If you want to view what the documentation looks like for only your application
you can do this:

```bash
(cd doc/src && make local_docs)
```

and then view `doc/html/index.html`.

### Preloaded and Primary Bootstrap

The Erlang code loader and compiler are written in Erlang, so in order to
[bootstrap](https://en.wikipedia.org/wiki/Bootstrapping_(compilers))
the system a number of compiled `.beam` files are commited into the
Erlang/OTP git repository.

The Erlang code located in [erts/preloaded/src](../erts/preloaded/src)
is compiled into the VM and used to load enough code so that the code
loader in the `kernel` application can be loaded. If you update any of
that code you need to do a special preloaded update for the changes to
take effect. This is done like this:

```bash
./otp_build update_preloaded [--no-commit]
make  # Need to rebuild system after the preloaded has been updated
```

You need to have a working Erlang compiler in your path for this to work.
In order to be able to compile the Erlang/OTP source code, there also needs
to be a basic Erlang compiler committed into git. This is what is called the
primary bootstrap. It is quite rare that you need to update this, but if you
are extending the Erlang language and would like to use the new extensions
in the Erlang/OTP source code you it needs to be updated. As an example, when
we added `maps` to Erlang we first needed to have a commited primary bootstrap
that could compile code with maps, before we actually could use maps anywhere.
To update the primary bootstrap you do like this:

```bash
./otp_build update_primary [--no-commit]
```

*NOTE*: When submitting a PR to Erlang/OTP you will be asked to not include
any commit updating preloaded or the primary bootstrap. This is because we
cannot review the contents of binary files and thus cannot make sure they do
not contain any malicious data.

### Types and Flavors

Erlang can be built using different types and flavors. Mostly the types and
flavors change how the Erlang VM itself is built, but some also effect how
application are built. Some of the types/flavors are:

* Types
  * opt (default)
  * debug
  * lcnt
  * valgrind
  * asan
  * gcov
* Flavor
  * emu
  * jit (default if available)

To build using a type and or flavor you just pass it as a variable to make.
For example:

```bash
make TYPE=debug
make FLAVOR=emu
make TYPE=lcnt FLAVOR=emu
```

As you can see it is possible to combine type and flavor to create many different
versions of Erlang. You can then run these different versions by passing the
`-emu_type` and/or `-emu_flavor` flags to `erl`. That is:

```bash
erl -emu_type lcnt
erl -emu_flavor emu -emu_type debug
```

When running valgrind, asan or gcov those tools create special output files that
need to be processed. To work with these files there is a special `erl` program
called `cerl` that is only available in the source tree. You can read more about
it in the [cerl section](#cerl) later in this guide.

If you want to run the tests with a special flavor or type, the easiest way to
do that is by setting the TYPE or FLAVOR when calling make. For example if you
want to run the emulator tests using the debug emulator you can do it like this:

```bash
make emulator_test TYPE=debug
```

*NOTE*: Before you run tests using a TYPE or FLAVOR you need to build the **entire**
Erlang/OTP repo using that TYPE or FLAVOR. That is `make TYPE=debug` for the example
above.

### cerl

`cerl` is a program available in `$ERL_TOP/bin/` that has a number of features
useful when developing the Erlang run-time system. It work just as normal `erl`,
but accepts a couple of extra command line switches. Any other command line arguments
passed to `cerl` will be passed on the Erlang as normal. The extra command line
switches are:

* -debug
  * Start a debug run-time system.
* -lcnt
  * Start a lock count run-time system.
* -valgrind
  * Start valgrind with the correct settings and use the `valgrind` [type](types-and-flavors).
  * Set environment variable `VALGRIND_LOG_XML` to true if want xml valgrind logs.
  * Set environment variable `VALGRIND_LOG_DIR` to where you want valgrind logs.
  * Set environment variable `VALGRIND_MISC_FLAGS` for any extra valgrind flags you want to pass.
* -asan
  * Start [Clang Address Sanitizer](https://clang.llvm.org/docs/AddressSanitizer.html)
    with the the correct settings and use the `asan` [type](types-and-flavors).
  * Set environment variable `ASAN_LOG_DIR` to where you want the logs.
  * Set environment variable `ASAN_OPTIONS` for any extra asan options you want to pass.
* -gcov
  * Start a gcov run-time system.
* -gdb
  * Start an Emacs gdb debugging session. Can be combined with -debug.
* -core /path/to/core/file
  * Start an Emacs gdb debugging session for the core specified.
* -rgdb
  * Start a gdb debugging session in the current terminal. Can be combined with -debug.
* -rcore /path/to/core/file
  * Start a gdb debugging session in the current terminal for the core specified.
* -lldb
  * Start a lldb debugging session in the current terminal.
* -rr
  * Start Erlang under [rr](https://rr-project.org/) to record all events. Can be combined with -debug.
* -rr replay [session]
  * Load a recording session using `rr replay`, if no session is specified the latest run session is laoded.

If you want to run tests using `cerl` (for example if you want to run asan on
the nif_SUITE in emulator) you cannot use the `make test` approach to testing
as that uses `ct_run` under the hood and `ct_run` does not support customizing
the emulator start script. Instead you need to use the approach described in
[Run tests with Address Sanitizer](INSTALL.md#run-tests-with-address-sanitizer).


### Static analysis

From the top level of Erlang/OTP you can run:

```bash
make xmllint
make dialyzer
make format-check
```

This will check that the documentation is correct and that there are no
dialyzer errors.

## Running test cases

There is a detailed description about how to run tests in [TESTING.md](TESTING.md).

## Writing and building documentation

Most of the Erlang/OTP documentation is written in XML files located in
`lib/$APPLICATION_NAME/doc/src`. The format of the XML is described in the
[ErlDocgen User's Guide](https://www.erlang.org/doc/apps/erl_docgen/users_guide.html).

There is also some documentation that is written using [edoc](https://www.erlang.org/doc/man/edoc.html).

To view the documentation the simplest way is to release it. *NOTE*: The Erlang/OTP
repository needs to have been [built](#building-and-testing) before you can build
the documentation.

```bash
make release_docs
```

and then you can view `release/*/doc/index.html` in your favourite browser and
make sure that it looks nice.

This takes a while though and to speed up the edit-view cycle you can either
limit what parts of the documentation is built using `DOC_TARGETS`. For example:

```bash
make release_docs DOC_TARGETS=html
```

The different `DOC_TARGETS` built are `html`, `man`, `pdf` and `chunks`.

You can also build the docs only for a single application. For example:

```bash
cd lib/stdlib/doc/src && make local_docs DOC_TARGETS=html
```

and then view the results at `lib/stdlib/doc/html/index.html`.

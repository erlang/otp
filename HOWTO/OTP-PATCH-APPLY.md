Patching OTP Applications
=========================

Introduction
------------

This document describes the process of patching an existing OTP
installation with one or more Erlang/OTP applications of newer versions
than already installed. The tool `otp_patch_apply` is available for this
specific purpose. It resides in the top directory of the Erlang/OTP
source tree.

The `otp_patch_apply` tool utilizes the [runtime_dependencies][] tag in
the [application resource file][]. This information is used to determine
if the patch can be installed in the given Erlang/OTP installation
directory.

Read more about the [version handling][] introduced in Erlang/OTP release
17, which also describes how to determine if an installation includes one
or more patched applications.

If you want to apply patches of multiple OTP applications that resides
in different OTP versions, you have to apply these patches in multiple
steps. It is only possible to apply multiple OTP applications from the
same OTP version at once.

Prerequisites
-------------

It's assumed that the reader is familiar with
[building and installing Erlang/OTP][]. To be able to patch an
application, the following must exist:

* An Erlang/OTP installation.

* An Erlang/OTP source tree containing the updated applications that
  you want to patch into the existing Erlang/OTP installation.

Using otp\_patch\_apply
-----------------------

> *WARNING*: Patching applications is a one-way process.
> Create a backup of your OTP installation directory before
> proceeding.

First of all, build the OTP source tree at `$ERL_TOP` containing
the updated applications.

> *NOTE*: Before applying a patch you need to do a *full* build
> of OTP in the source directory.

If you are building in `git` you first need to generate the
`configure` scripts:

	$ ./otp_build autoconf

Configure and build all applications in OTP:

	$ configure
	$ make

or

	$ ./otp_build configure
	$ ./otp_build boot -a

If you have installed documentation in the OTP installation, also
build the documentation:

	$ make docs

After the successful build it's time to patch. The source tree directory,
the directory of the installation and the applications to patch are given
as arguments to `otp_patch_apply`. The dependencies of each application
are validated against the applications in the installation and the other
applications given as arguments. If a dependency error is detected, the
script will be aborted.

The `otp_patch_apply` syntax:

	$ otp_patch_apply -s <Dir> -i <Dir> [-l <Dir>] [-c] [-f] [-h] \
          [-n] [-v] <App1> [... <AppN>]
	
	-s <Dir>  -- OTP source directory that contains build results.
	-i <Dir>  -- OTP installation directory to patch.
	-l <Dir>  -- Alternative OTP source library directory path(s)
	             containing build results of OTP applications.
	             Multiple paths should be colon separated.
	-c        -- Cleanup (remove) old versions of applications
	             patched in the installation.
	-f        -- Force patch of application(s) even though
	             dependencies are not fulfilled (should only be
	             considered in a test environment).
	-h        -- Print help then exit.
	-n        -- Do not install documentation.
	-v        -- Print version then exit.
	<AppX>    -- Application to patch.
	
	Environment Variable:
	  ERL_LIBS  -- Alternative OTP source library directory path(s)
	               containing build results of OTP applications.
	               Multiple paths should be colon separated.

> *NOTE*: The complete build environment is required while running
> `otp_patch_apply`.

> *NOTE*: All source directories identified by `-s` and `-l` should
> contain build results of OTP applications.

For example, if the user wants to install patched versions of `mnesia`
and `ssl` built in `/home/me/git/otp` into the OTP installation
located in `/opt/erlang/my_otp` type

	$ otp_patch_apply -s /home/me/git/otp -i /opt/erlang/my_otp \
	  mnesia ssl

> *NOTE*: If the list of applications contains core applications,
> i.e `erts`, `kernel`, `stdlib` or `sasl`, the `Install` script in
> the patched Erlang/OTP installation must be rerun.

The patched applications are appended to the list of installed
applications. Take a look at
`<InstallDir>/releases/OTP-REL/installed_application_versions`.

Sanity check
------------

The application dependencies can be checked using the Erlang shell.
Application dependencies are verified among installed applications by
`otp_patch_apply`, but these are not necessarily those actually loaded.
By calling `system_information:sanity_check()` one can validate
dependencies among applications actually loaded.

	1> system_information:sanity_check().
        ok

Please take a look at the reference of [sanity_check()][] for more
information.

[application resource file]: kernel:app
[runtime_dependencies]: kernel:app#runtime_dependencies
[building and installing Erlang/OTP]: INSTALL.md
[version handling]: ../system_principles/versions
[sanity_check()]: runtime_tools:system_information#sanity_check-0

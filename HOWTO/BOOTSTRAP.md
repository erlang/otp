Notes about prebuilt beam files under version control
=====================================================

This information applies mostly to developers, some parts only
to developers of the main branch i.e. Ericsson and HiPE personel.

There are two types of derived code under version control, namely:

primary bootstrap - Resides in the `$ERL_TOP/bootstrap/{lib,bin}` directories.
preloaded code - Resides in the `$ERL_TOP/erts/preloaded` directory.

Primary bootstrap
-----------------

The two types of version controlled code are fundamentally
different. The primary bootstrap is code compiled from source files in
the lib/{kernel,stdlib,compiler} (They are checked in in the version control system
to make it possible to build directly from the code base tree without
the need for an earlier version of the compiler. When a new version of
OTP is released, these files are updated manually (or rather, by using
the `$ERL_TOP/otp_build` script) and checked in. The files can also be
updated due to changes in the compiler during the development
process. The primary bootstrap is always updated as a separate
deliberate process, never during a normal development build.

If a prebuilt open source version of erlang is used, the directory
bootstrap initially does not contain any beam files, the directory is
instead populated by copying beam files from the
`$ERL_TOP/lib/{kernel,stdlib,compiler}/ebin` directories. This
construction is to save space in the distribution, but the result
would be the same. Open source developers need not provide patches for
the precompiled beam files in the primary bootstrap, the bootstrap
update is always performed by the main developers.

Preloaded code
--------------

The directory `$ERL_TOP/preloaded` contains both src and ebin
subdirectories. The preloaded code is compiled into the virtual
machine and always present. When compiling the virtual machine, those
beam files need to be present and they are considered a part of the
virtual machine rather than a part of the kernel application. When
preloaded files are to be updated, the source code is built using a
special Makefile in the `$ERL_TOP/preloaded/src` directory, which
creates beam files in the same directory. When they seem to compile
successfully, they can be used in an emulator build by being copied
to the ebin directory. `otp_build update_preloaded` can be used to
ease the process (there are also similar targets in the
`$ERL_TOP/preloaded/src/Makefile`).

In prebuilt open source distributions, these beam files are also
present, but to update them one might need to change permission on the
`$ERL_TOP/preloaded/ebin` directory, then build and then manually copy
the beam files from the source directory to ../ebin. If patches are
created that involve the source files used to build preloaded code,
always note this specially as the preloaded/ebin directory needs
updating, or provide the new derived files in the patch or as complete
binaries.

/Patrik, OTP

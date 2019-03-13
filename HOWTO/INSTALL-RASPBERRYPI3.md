# Cross Compiling Erlang/OTP - Raspberry Pi 3


## Introduction

This document describes how to build a toolchain and cross compile Erlang/OTP
to Raspberry Pi 3 on macOS Mojave. It is recommended to consult
[Building and Installing Erlang/OTP](https://github.com/erlang/otp/blob/master/HOWTO/INSTALL.md) and [Cross Compiling Erlang/OTP](https://github.com/erlang/otp/blob/master/HOWTO/INSTALL-CROSS.md) before attempting to follow the instructions in this guide.

The whole process takes several hours and depending on the package versions different problems may arise that require additional
fixes not described in this document. In other words, it is not fun to build a toolchain. I assume that you have a Mac and would
like to develop Erlang/OTP applications based on the latest OTP release (or master) that not yet released as a binary for
Raspberry Pi 3.

The first and most time consuming step is building the toolchain from scratch. Once your cross compiler is ready you cross compile
all library dependencies and create the sysroot file system. In the last step you cross compile Erlang/OTP using the new
toolchain and sysroot.

#### Tested Configuration

macOS Mojave 10.14.3<br>
Raspberry Pi Model B Rev 1.2<br>
Crosstools-NG 1.23.0_3

```
build  = x86_64-apple-darwin18.2.0
host   = x86_64-apple-darwin18.2.0
target = armv8-rpi3-linux-gnueabihf
```

> Note: /proc/device/tree/model contains model information of your
> Raspberry Pi.


#### Install Crosstool NG

  (1)

    $ brew install crosstool-ng
    $ brew install grep --default-names # needed by crosstools-ng scripts
    $ brew install md5sha1sum           # needed by crosstools-ng populate script

  (2)

    $ chmod 744 /usr/local/Cellar/crosstool-ng/1.23.0_3/lib/crosstool-ng-1.23.0/scripts/crosstool-NG.sh

#### Create case-sensitive disk images

  (3)

Create two case-sensitive disk images using Disk Utility:

`File -> New Image -> Blank Image...`

Format: `Mac OS Extended (Case-sensitive, Journaled)`

```
/Volumes/xtools-build-env  15 GB
/Volumes/xtools            500 MB
```

> The first image holds all source and object files while building the toolchain. The second image houses the compiled
toolchain.


## Building the Toolchain

### Environment settings

  (4)

    $ ulimit -n 1024

### Inspect target system

  (5)

    $ uname -a
    Linux raspberrypi 4.9.35-v7+ #1014 SMP Fri Jun 30 14:47:43 BST 2017 armv7l GNU/Linux
    $ ld -v
    GNU ld (GNU Binutils for Raspbian) 2.25
    $ ldd --version
    ldd (Debian GLIBC 2.19-18+deb8u10) 2.19
    Copyright (C) 2014 Free Software Foundation, Inc.
    This is free software; see the source for copying conditions.  There is NO
    warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    Written by Roland McGrath and Ulrich Drepper.

> Use the versions available on your target system!

> See https://wiki.osdev.org/Cross-Compiler_Successful_Builds

### Configure crosstool-ng

  (6)

    $ cd /Volumes/xtools-build-env
    $ ct-ng armv8-rpi3-linux-gnueabihf
    $ ct-ng menuconfig

#### Path and misc options

##### crosstool-NG behavior

  (7)

```
[*] Use obsolete features

[*] Debug crosstool-NG
[ ]   Pause between every steps
[*]   Save intermediate steps
[*]     gzip saved states
[*]   Interactive shell on failed commands
```

> Should the build break at a particular build step, you can fix the problem and continue the build from where it broke.

##### Paths

  (8)

* Local tarballs directory: `/Volumes/xtools-build-env/src`
* Working directory: `/Volumes/xtools-build-env/.build`
* Prefix directory: `/Volumes/xtools/${CT_TARGET}`

##### Extracting

  (9)

`[*] Stop after extracting tarballs`

> Stop the build process right after the tarballs have been extracted. This can be handy to fix known source code problems before the actual build process is started.

#### Operating System

  (10)

`Linux kernel version (4.9.20)`

#### Binary utilities

  (11)

`bintutils version (2.28)`

#### C-library

  (12)

`glibc version (2.19 (OBSOLETE))`

#### Sample `ct-ng` commands:

* List all build steps

```
    $ ct-ng list-steps

    Available build steps, in order:
    - companion_tools_for_build
    - companion_libs_for_build
    - binutils_for_build
    - companion_tools_for_host
    - companion_libs_for_host
    - binutils_for_host
    - cc_core_pass_1
    - kernel_headers
    - libc_start_files
    - cc_core_pass_2
    - libc
    - cc_for_build
    - cc_for_host
    - libc_post_cc
    - companion_libs_for_target
    - binutils_for_target
    - debug
    - test_suite
    - finish
```

* Re-run step `companion_libs_for_host`

```
    $ ct-ng companion_libs_for_host
```

* Restart from `companion_libs_for_host`

```
    $ ct-ng companion_libs_for_host+
```

* Run until step `companion_libs_for_host`

```
    $ ct-ng +companion_libs_for_host
```

### Build

  (13)

    $ ct-ng build

> Build process stops just after the tarballs have been extracted.

#### Fix source files

  (14)

    $ pushd .build/src/gettext-0.19.8.1/
    $ autoreconf
    $ popd

#### Update configuration

  (15)

    $ ct-ng menuconfig

Uncheck option:

`[ ] Stop after extracting tarballs`

#### Continue build

  (16)

    $ ct-ng build

> Restart build process from where it previously stopped.

  (17)

    $ export PATH=/Volumes/xtools/armv8-rpi3-linux-gnueabihf/bin:$PATH

### Test

  (18)

    $ cat > test.c
    $ int main() { printf("Hello, world!\n"); return 0; }
    <CTRL+D>
    $ armv8-rpi3-linux-gnueabihf-gcc -o test test.c


## Cross compiling dependencies

  (19)

    $ mkdir local # prefix directory
    $ mkdir 3pps  # OTP dependencies
    $ cd 3pps

#### zlib

  (20)

    $ wget http://zlib.net/zlib-1.2.11.tar.gz
    $ tar xf zlib-1.2.11.tar.gz
    $ pushd zlib-1.2.11
    $ CHOST=armv8-rpi3-linux-gnueabihf ./configure --prefix=/Volumes/xtools-build-env/local
    $ make
    $ make install
    $ popd

#### openssl

  (21)

    $ wget http://openssl.org/source/openssl-1.1.1b.tar.gz
    $ tar xf openssl-1.1.1b.tar.gz
    $ pushd  openssl-1.1.1b
    $ ./Configure linux-generic32 --prefix=/Volumes/xtools-build-env/local \
    --openssldir=/Volumes/xtools-build-env/local/openssl \
    --cross-compile-prefix=armv8-rpi3-linux-gnueabihf-
    $ make
    $ make install
    $ popd

> A compatible openssl library shall be available on the target system!

#### ncurses

  (22)

    $ wget http://ftp.gnu.org/pub/gnu/ncurses/ncurses-5.9.tar.gz
    $ tar xf ncurses-5.9.tar.gz
    $ pushd ncurses-5.9
    $ wget https://gist.githubusercontent.com/peterdmv/1068b2f9e1fec6e1330ad62ed87461ad/\
    raw/065597b63654ed6a9f28d02fdfbca844413847ad/ncurses-5.9.patch
    $ patch -p0 < ncurses-5.9.patch
    $ ./configure --build=x86_64-apple-darwin18.2.0 --host=armv8-rpi3-linux-gnueabihf \
    --without-ada --without-cxx --without-cxx-binding --without-manpages \
    --without-progs --without-tests --prefix=/usr --libdir=/lib \
    --with-build-cc="gcc -D_GNU_SOURCE" --with-shared
    $ make
    $ make DESTDIR=/Volumes/xtools-build-env/local install
    $ popd


## Populating sysroot

  (23)

    $ chmod 755 /Volumes/xtools/armv8-rpi3-linux-gnueabihf/bin
    $ chmod 755 /Volumes/xtools/armv8-rpi3-linux-gnueabihf/bin/armv8-rpi3-linux-gnueabihf-populate
    $ gsed -i 's/"sed"/"gsed"/g' \
    /Volumes/xtools/armv8-rpi3-linux-gnueabihf/bin/armv8-rpi3-linux-gnueabihf-populate
    $ chmod 555 /Volumes/xtools/armv8-rpi3-linux-gnueabihf/bin
    $ chmod 555 /Volumes/xtools/armv8-rpi3-linux-gnueabihf/bin/armv8-rpi3-linux-gnueabihf-populate

  (24)

    $ armv8-rpi3-linux-gnueabihf-populate -s /Volumes/xtools-build-env/local \
    -d /Volumes/xtools-build-env/sysroot
    $ export RPI_SYSROOT=/Volumes/xtools-build-env/sysroot


## Cross compiling Erlang/OTP

  (25)

    $ LC_CTYPE=C && LANG=C && ./otp_build autoconf
    $ ./otp_build configure --xcomp-conf=./xcomp/erl-xcomp-armv8-rpi3-linux-gnueabihf.conf
    $ ./otp_build boot -a
    $ ./otp_build release -a /Volumes/xtools-build-env/otp_22.0

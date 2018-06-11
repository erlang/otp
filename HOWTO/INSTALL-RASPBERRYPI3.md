# Cross Compiling Erlang/OTP - Raspberry Pi 3


## Introduction

This document describes how to build a toolchain and cross compile Erlang/OTP
to Raspberry Pi 3 on macOS High Sierra. It is recommended to consult
[Building and Installing Erlang/OTP](https://github.com/erlang/otp/blob/master/HOWTO/INSTALL.md) and [Cross Compiling Erlang/OTP](https://github.com/erlang/otp/blob/master/HOWTO/INSTALL-CROSS.md) before attempting to follow the instructions in this guide.

The whole process takes several hours and depending on the package versions different problems may arise that require additional
fixes not described in this document. In other words, it is not fun to build a toolchain. I assume that you have a Mac and would
like to develop Erlang/OTP applications based on the latest OTP release (or master) that not yet released as a binary for
Raspberry Pi 3.

The first and most time consuming step is building the toolchain from scratch. Once your cross compiler is ready you cross compile
all library dependencies and create the sysroot file system. In the last step you cross compile Erlang/OTP using the new
toolchain and sysroot.

#### Tested Configuration

macOS High Sierra 10.13.2<br>
Raspberry Pi Model B Rev 1.2<br>
Crosstools-NG 1.23.0_1

> Note: /proc/device/tree/model contains model information of your
> Raspberry Pi.


#### Install Crosstool NG

  (1)

    $ brew install crosstool-ng
    $ brew install grep --default-names # needed by crosstools-ng scripts
    $ brew install md5sha1sum           # needed by crosstools-ng populate script


#### Create case-sensitive disk images

  (2)

Create two case-sensitive disk images using Disk Utility:

`File -> New Image -> Blank Image...`

Format: `Mac OS Extended (Case-sensitive, Journaled)`

```
/Volumes/xtools-build-env  15 GB
/Volumes/xtools           500 MB
```

The first image holds all source and object files while building the toolchain. The second image houses the compiled
toolchain.

## Building the Toolchain


#### Configure crosstool-ng

  (4)

    $ ct-ng armv8-rpi3-linux-gnueabihf
    $ ct-ng menuconfig

#### Modify *path* section

* Local tarballs directory: `/Volumes/xtools-build-env/src`
* Working directory: `/Volumes/xtools-build-env/.build`
* Prefix directory: `/Volumes/xtools/${CT_TARGET}`

#### Modify *Extracting* section

* Check option: _Stop after extracting tarballs_.

> Note: The build shall stop after the tarballs have been extracted to give us time to fix source code problems.

#### Enable STOP / RESTART

Edit  /Volumes/xtools-build-env/.config
  `CT_DEBUG_CT_SAVE_STEPS=y`

Should the build break at a particular build step, you can fix the problem and continue the build from where it broke.

Short summary of the most common `ct-ng` commands:

* Listing all build steps

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

* Re-run step
```
    $ ct-ng step
```

* Restart from step
```
    $ ct-ng step+
```

* Run until step
```
    $ ct-ng +step
```

#### Fix file permissions on crosstool-NG.sh

  (5)

    $ chmod 744 /usr/local/Cellar/crosstool-ng/1.23.0_1/lib/crosstool-ng-1.23.0/scripts/crosstool-NG.sh

#### Run build command

Build process stops just after the tarballs have been extracted.

  (6)

    $ ct-ng build

    Retrieving needed toolchain components' tarballs
    [EXTRA]    Retrieving 'make-4.2.1'
    [EXTRA]    Retrieving 'm4-1.4.18'
    [EXTRA]    Retrieving 'linux-4.10.8'
    [EXTRA]    Retrieving 'gmp-6.1.2'
    [EXTRA]    Retrieving 'mpfr-3.1.5'
    [EXTRA]    Retrieving 'isl-0.16.1'
    [EXTRA]    Retrieving 'mpc-1.0.3'
    [EXTRA]    Retrieving 'expat-2.2.0'
    [EXTRA]    Retrieving 'ncurses-6.0'
    [EXTRA]    Retrieving 'libiconv-1.15'
    [EXTRA]    Retrieving 'gettext-0.19.8.1'
    [EXTRA]    Retrieving 'binutils-2.28'
    [EXTRA]    Retrieving 'gcc-6.3.0'
    [EXTRA]    Retrieving 'glibc-2.25'
    [EXTRA]    Retrieving 'gdb-7.12.1'

#### Fix source files

  (7)

Add macro to /Volumes/xtools-build-env/.build/src/gdb-7.12.1/gdb/doublest.c:
```C
#define min(a,b) \
  ({ typeof (a) _a = (a); \
      typeof (b) _b = (b); \
    _a < _b ? _a : _b; })
```

  (8) Update ulimit

    $ ulimit -n 1024

#### Modify *extract* section

  (8)

    $ ct-ng menuconfig

 Uncheck option: _Stop after extracting tarballs_

#### Re-run build command

Restarts build process from where it previously stopped.

  (9)

    $ ct-ng build

#### Fix gettext

Build will fail at step `companion_tools_for_build` but it can be fixed by running autoreconf:

  (10)

    $ cd .build/src/gettext-0.19.8.1/
    $ ./autoreconf
    $ ct-ng companion_tools_for_build+

#### Test the toolchain

  (11)

    $ cat > test.c
    $ int main() { printf("Hello, world!\n"); return 0; }
    $ /Volumes/xtools/arm-unknown-linux-gnueabi-gcc -o test test.c

  (12) OPTIONAL

    “Render the toolchain read-only” from crosstool-NG’s “Paths and misc options” configuration page.


## Cross compiling dependencies

  (13)

    $ export PATH=/Volumes/xtools/armv8-rpi3-linux-gnueabihf/bin:$PATH

#### Cross compiling zlib

  (14)

    $ wget http://zlib.net/zlib-1.2.11.tar.gz
    $ tar xf zlib-1.2.11.tar.gz
    $ cd zlib-1.2.11
    $ CHOST=armv8-rpi3-linux-gnueabihf ./configure --prefix=/Users/<username>/git/raspberrypi/arm
    $ make
    $ make install

#### Cross compiling openssl

  (15)

    $ wget http://openssl.org/source/openssl-1.1.0g.tar.gz
    $ tar xf openssl-1.1.0g.tar.gz
    $ cd openssl-1.1.0g
    $ ./Configure linux-generic32 --prefix=/Users/<username>/git/raspberrypi/arm --openssldir=/Users/<username>/git/raspberrypi/arm/openssl --cross-compile-prefix=armv8-rpi3-linux-gnueabihf
    $ make
    $ make install

#### Cross compiling ncurses

  (16)

    $ wget http://ftp.gnu.org/pub/gnu/ncurses/ncurses-5.9.tar.gz


  (17)

Apply patch:

```patch
--- a/ncurses/base/MKlib_gen.sh
+++ b/ncurses/base/MKlib_gen.sh
@@ -474,11 +474,22 @@ sed -n -f $ED1 \
    -e 's/gen_$//' \
    -e 's/  / /g' >>$TMP

+cat >$ED1 <<EOF
+s/  / /g
+s/^ //
+s/ $//
+s/P_NCURSES_BOOL/NCURSES_BOOL/g
+EOF
+
+# A patch discussed here:
+#  https://gcc.gnu.org/ml/gcc-patches/2014-06/msg02185.html
+# introduces spurious #line markers.  Work around that by ignoring the system's
+# attempt to define "bool" and using our own symbol here.
+sed -e 's/bool/P_NCURSES_BOOL/g' $TMP > $ED2
+cat $ED2 >$TMP
+
 $preprocessor $TMP 2>/dev/null \
-| sed \
-   -e 's/  / /g' \
-   -e 's/^ //' \
-   -e 's/_Bool/NCURSES_BOOL/g' \
+| sed -f $ED1 \
 | $AWK -f $AW2 \
 | sed -f $ED3 \
 | sed \
```

  (18)

    $ ./configure --build=x86_64-apple-darwin17.3.0 --host=armv8-rpi3-linux-gnueabihf --without-ada --without-cxx --without-cxx-binding --without-manpages --without-progs --without-tests --prefix=/usr --libdir=/lib --with-build-cc="gcc -D_GNU_SOURCE" --with-shared
    $ make
    $ make DESTDIR=/Users/<username>/git/raspberrypi/arm install

  (19)

Compile ncurses test program:

    $ cd test
    $ armv8-rpi3-linux-gnueabihf-gcc -o nctest ncurses.c -I${RPI_SYSROOT}/usr/include -L${RPI_SYSROOT}/lib -lncursesw


## Populating sysroot

  (19)

    Edit /Volumes/xtools/armv8-rpi3-linux-gnueabihf/bin/armv8-rpi3-linux-gnueabihf-populate:

    sed="gsed"

  (20)

    $ armv8-rpi3-linux-gnueabihf-populate -s /Users/<username>/git/raspberrypi/arm -d /Users/<username>/git/raspberrypi/sysroot
    $ export RPI_SYSROOT=/Users/<username>/git/raspberrypi/sysroot


## Cross compiling Erlang/OTP

  (21)

    $ LC_CTYPE=C && LANG=C && ./otp_build autoconf
    $ ./otp_build configure --disable-dynamic-ssl-lib --xcomp-conf=./xcomp/erl-xcomp-armv8-rpi3-linux-gnueabihf.conf
    $ ./otp_build boot -a
    $ ./otp_build release -a /Users/<username>/git/raspberrypi/erlang
    $ tar czf erlang.tgz ./erlang


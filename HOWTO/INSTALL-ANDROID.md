Cross Compiling Erlang/OTP - ANDROID
====================================

Introduction
------------

This document describes how to cross compile Erlang/OTP to Android/Raspberry Pi platforms.


### Download and Install the Android NDK ###

https://developer.android.com/ndk


### Define System Variables ###

    $ export NDK_ROOT=/path/to/android-ndk
    $ export PATH=$NDK_ROOT/toolchains/llvm/prebuilt/linux-x86_64/bin:$PATH
    $ # export PATH=$NDK_ROOT/toolchains/lvvm/prebuilt/darwin-x86_64/bin:$PATH


### Configure Erlang/OTP ###

Use the following commands when compiling an ARM 64-bit version.

    $ export NDK_ABI_PLAT=android21      # When targeting Android 5.0 Lollipop


    $ # Either without OpenSSL support:
    $
    $ ./otp_build configure \
         --xcomp-conf=./xcomp/erl-xcomp-arm64-android.conf  \
         --without-ssl


    $ # Or with OpenSSL linked statically:
    $
    $ cd /path/to/OpenSSL/source/dir/built/for/android-arm64
    $ # First follow the NOTES.ANDROID build instructions from OpenSSL
    $
    $ # Then to avoid the full installation of this cross-compiled build,
    $ # manually create a 'lib' directory at the root of the OpenSSL directory
    $ # (at the same level as 'include') and link 'libcrypto.a' inside it.
    $
    $ mkdir lib
    $ ln -s ../libcrypto.a lib/libcrypto.a
    $ cd -   # Return to the Erlang/OTP directory
    $
    $ # This previous step is needed for the OpenSSL static linking to work as
    $ # the --with-ssl option expects a path with both the 'lib' and 'include'
    $ # directories.
    $ ./otp_build configure \
         --xcomp-conf=./xcomp/erl-xcomp-arm64-android.conf  \
         --with-ssl=/path/to/OpenSSL/source/dir/built/for/android-arm64 \
         --disable-dynamic-ssl-lib


Use the following commands when compiling an ARM 32-bit version.

    $ export NDK_ABI_PLAT=androideabi16  # When targeting Android 4.1 Jelly Bean


    $ # Either without OpenSSL support:
    $
    $ ./otp_build configure \
         --xcomp-conf=./xcomp/erl-xcomp-arm-android.conf  \
         --without-ssl


    $ # Or with OpenSSL linked statically:
    $
    $ cd /path/to/OpenSSL/source/dir/built/for/android-arm
    $ # First follow the NOTES.ANDROID build instructions from OpenSSL
    $
    $ # Then to avoid the full installation of this cross-compiled build,
    $ # manually create a 'lib' directory at the root of the OpenSSL directory
    $ # (at the same level as 'include') and link 'libcrypto.a' inside it.
    $
    $ mkdir lib
    $ ln -s ../libcrypto.a lib/libcrypto.a
    $ cd -   # Return to the Erlang/OTP directory
    $
    $ # This previous step is needed for the OpenSSL static linking to work as
    $ # the --with-ssl option expects a path with both the 'lib' and 'include'
    $ # directories.
    $ ./otp_build configure \
         --xcomp-conf=./xcomp/erl-xcomp-arm-android.conf  \
         --with-ssl=/path/to/OpenSSL/source/dir/built/for/android-arm \
         --disable-dynamic-ssl-lib


Use the following commands when compiling an x86 64-bit version.

    $ export NDK_ABI_PLAT=android21      # When targeting Android 5.0 Lollipop


    $ # Either without OpenSSL support:
    $
    $ ./otp_build configure \
         --xcomp-conf=./xcomp/erl-xcomp-x86_64-android.conf  \
         --without-ssl


    $ # Or with OpenSSL linked statically:
    $
    $ cd /path/to/OpenSSL/source/dir/built/for/android-x86_64
    $ # First follow the NOTES.UNIX build instructions from OpenSSL
    $
    $ # Then to avoid the full installation of this locally-compiled build,
    $ # manually create a 'lib64' directory at the root of the OpenSSL source
    $ # (at the same level as 'include') and link 'libcrypto.a' inside it.
    $
    $ mkdir lib64
    $ ln -s ../libcrypto.a lib64/libcrypto.a
    $ cd -   # Return to the Erlang/OTP directory
    $
    $ # This previous step is needed for the OpenSSL static linking to work
    $ # as the --with-ssl option expects a path with both the 'lib64' and
    $ # 'include' directories.
    $ ./otp_build configure \
         --xcomp-conf=./xcomp/erl-xcomp-x86_64-android.conf  \
         --with-ssl=/path/to/OpenSSL/source/dir/built/for/android-x86_64 \
         --disable-dynamic-ssl-lib


### Compile Erlang/OTP ###

    $ make noboot [-j4]
      or
    $ make [-j4]


### Make Release ###

    $ make RELEASE_ROOT=/path/to/release/erlang release


### Target Deployment for Raspberry Pi ###

Make a tarball out of /path/to/release/erlang and copy it to target
device. Extract it and install.

    $ ./Install /usr/local/erlang


### Target Deployment for Android testing ###

The adb tool from the Android SDK can be used to deploy Erlang/OTP to a target
Android device, for testing purpose mainly, as the /data/local/tmp path used
for installation below is executable only from the adb shell command, but not
from other local applications due to Android sandbox security model.

    $ cd /path/to/release/erlang
    $ # For testing purpose, configure the Erlang/OTP scripts to use the target
    $ # installation path in /data/local/tmp which is executable from adb shell
    $ ./Install -cross -minimal /data/local/tmp/erlang

To properly integrate into an Android application, the installation would have
to target /data/data/[your/app/package/name]/files/[erlang/dir/once/unpacked]
as shown in https://github.com/JeromeDeBretagne/erlanglauncher as an example.

WARNING: adb has issues with symlinks (and java.util.zip too). There is only
one symlink for epmd in recent Erlang/OTP releases (20 to master-based 24) so
it has to be removed before using adb push, and then recreated manually on the
target device itself if needed (or epmd can simply be duplicated instead).

    $ # Make sure that the epmd symlink is not present before adb push
    $ rm bin/epmd
    $ cp erts-X.Y.Z/bin/epmd bin/epmd
    $ cd ..
    $ # The release can now be deployed in the pre-configured target directory
    $ adb push erlang /data/local/tmp/erlang

Start an interactive shell onto the target Android device, and launch erl.

     $ adb shell
     :/ $ /data/local/tmp/erlang/bin/erl
     Eshel VX.Y.Z (abort with ^G)
     1> q().
     ok
     2> :/ $ # Erlang/OTP is running on Android, congratulations! :-)


### Known Issues ###

 * native inet:gethostbyname/1 return {error, nxdomain} on Raspberry PI.
   Use dns resolver to by-pass the issue (see
   http://www.erlang.org/doc/apps/erts/inet_cfg.html)


### References ###

  The port derives some solutions from https://code.google.com/p/erlang4android/

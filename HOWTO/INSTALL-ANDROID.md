Cross Compiling Erlang/OTP - ANDROID
====================================

Introduction
------------

This document describes how to cross compile Erlang/OTP to Android/Rasberry Pi platforms.


### Download and Install Android NDK ###

https://developer.android.com/ndk


### Define System Variables ###

    $ export NDK_ROOT=/path/to/android-ndk
    $ export PATH=$NDK_ROOT/toolchains/llvm/prebuilt/linux-x86_64/bin:$PATH
    $ # export PATH=$NDK_ROOT/toolchains/lvvm/prebuilt/darwin-x86_64/bin:$PATH


### Configure Erlang/OTP ###

If you are building Erlang/OTP from git, you will need to run this
to generate the configure scripts.

    $ ./otp_build autoconf


Use the following when compiling a 64-bit version.

    $ export NDK_ABI_PLAT=android21      # When targeting Android 5.0 Lollipop
    $ ./otp_build configure \
         --xcomp-conf=./xcomp/erl-xcomp-arm64-android.conf  \
         --without-ssl


Use the following instead when compiling a 32-bit version.

    $ export NDK_ABI_PLAT=androideabi21  # When targeting Android 5.0 Lollipop
    $ ./otp_build configure \
         --xcomp-conf=./xcomp/erl-xcomp-arm-android.conf  \
         --without-ssl


### Compile Erlang/OTP ###

    $ # make noboot [-j4] # noboot doesn't work, is it a recent regression?
    $ make [-j4]


### Make Release ###

    $ make RELEASE_ROOT=/path/to/release/erlang_23.0_arm release


### Target Deployment for Rasberry Pi ###

Make a tarball out of /path/to/release/erlang_23.0_arm and copy it to target
device. Extract it and install.

    $ ./Install /usr/local/erlang_23.0_arm


### Target Deployment for Android testing ###

The adb tool from the Android SDK can be used to deploy Erlang/OTP to a target
Android device, for testing purpose mainly, as the /data/local/tmp path used
for installation below is executable only from the adb shell command, but not
from other local applications due to Android sandbox security model.

    $ cd /path/to/release/erlang_23.0_arm
    $ # For testing purpose, configure the Erlang/OTP scripts to use the target
    $ # installation path in /data/local/tmp which is executable from adb shell
    $ ./Install -cross -minimal /data/local/tmp/erlang_23.0

To properly integrate into an Android application, the installation would have
to target /data/data/[your/app/package/name]/files/[erlang/dir/once/unpacked]
as shown in https://github.com/JeromeDeBretagne/erlanglauncher as an example.

TODO: Propose a permanent fix for the following issue.
Adapt the installation specifically for Android, by replacing manually /bin/sh
into /system/bin/sh in the various Erlang/OTP release scripts, such as:
   - bin/erl
   - bin/start
   - bin/start_erl
   - erts-X.Y.Z/bin/erl
   - erts-X.Y.Z/bin/erl.src
   - erts-X.Y.Z/bin/start
   - erts-X.Y.Z/bin/start_erl.src
   - erts-X.Y.Z/bin/start.src
   - etc.

WARNING: adb has issues with symlinks (and java.util.zip too). There is only
one symlink for epmd in recent Erlang/OTP releases (20 to master-based 23) so
it has to be removed before using adb push, and then recreated manually on the
target device itself if needed (or epmd can simply be duplicated instead).

    $ # Make sure that the epmd symlink is not present before adb push
    $ rm bin/epmd
    $ cp erts-X.Y.Z/bin/epmd bin/epmd
    $ cd ..
    $ # The release can now be deployed in the pre-configured target directory
    $ adb push erlang_23.0_arm /data/local/tmp/erlang_23.0

Start an interactive shell onto the target Android device, and launch erl.

     $ adb shell
     :/ $ /data/local/tmp/erlang_23.0/bin/erl
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

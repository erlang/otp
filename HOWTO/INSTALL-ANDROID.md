Cross Compiling Erlang/OTP - ANDROID
====================================

Introduction
------------

This document describes how to cross compile Erlang OTP to Android/Rasberry Pi platforms.

### Download and Install Android NDK ###

https://developer.android.com/tools/sdk/ndk/index.html

### Define System Variables ###

export NDK_ROOT=/usr/local/android
export NDK_PLAT=android-9
export PATH=$NDK_ROOT/toolchains/arm-linux-androideabi-4.8/prebuilt/darwin-x86_64/bin:$PATH

### Configure OTP ###

./otp_build configure \
   --xcomp-conf=./xcomp/erl-xcomp-arm-android.conf  \
   --without-ssl

### Compile OTP ###

make noboot [-j4]

### Make Release ###

./otp_build release -a /usr/local/otp_R16B03_arm

### Target Deployment ###

Make a tarball out of /usr/local/otp_R16B03_arm and copy it to target device
(e.g. Raspberry Pi). Extract it and install

./Install /usr/local/otp_R16B03_arm

Android SDK (adb tool) is used to deploy OTP/Erlang to target device for 
evaluation purpose only.

adb push /usr/local/otp_R16B03_arm /mnt/sdcard/otp_R16B03_arm
adb shell

### Known Issues ###

 * native inet:gethostbyname/1 return {error, nxdomain} on Raspberry PI. Use dns resolver to by-pass the issue (see http://www.erlang.org/doc/apps/erts/inet_cfg.html) 

### References ###

  The port derives some solutions from https://code.google.com/p/erlang4android/

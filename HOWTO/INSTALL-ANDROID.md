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

./otp_build release -a /usr/local/otp-R16B03-arm

### Target Deployment ###

Make a tarball out of /usr/local/otp-R16B03-arm and copy it to target device
(e.g. Raspberry Pi). Extract it and install

./Install /usr/local/otp-R16B03-arm


### Known Issues ###

 * inets:gethostbyname/1 unable to resolve 

### References ###

  The port derives some solutions from https://code.google.com/p/erlang4android/

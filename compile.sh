#!/usr/bin/env bash

CORES=16
ERL_TOP=$PWD

if [ "$1" == "" ]; then
    WHAT=make
else
    WHAT=$1
fi

case $WHAT in
    autoconf)
        ./otp_build autoconf
        ;&
    configure)
        ./otp_build configure
        ;&
    clean)
        make -j$CORES clean
        ;&
    make)
        ;;
    *)
        echo "Usage: $0 [autoconf | configure | clean | make]"
        exit 1
        ;;
esac

./otp_build boot -a || exit 1
./otp_build release -a || exit 1
./otp_build tests || exit 1
cd erts/emulator
env ERL_TOP=$ERL_TOP make -j$CORES debug FLAVOR=plain
env ERL_TOP=$ERL_TOP make -j$CORES debug FLAVOR=smp
cd ../..

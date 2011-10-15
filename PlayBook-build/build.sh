###########################################################################
# Setup environment                                                       #
###########################################################################
set -e

source bbndk.env

export DEV_ROOT=`pwd`
pushd ..
export ERLANG_ROOT=`pwd`
popd

if [ ! -d "build" ]; then
  mkdir build
fi
pushd build

if [ ! -d "PlayBook" ]; then
  mkdir PlayBook
fi

if [ ! -d "linux" ]; then
  mkdir linux
fi

popd

export LINUX_BUILD=$DEV_ROOT/build/linux
export PLAYBOOK_PREFIX=$DEV_ROOT/build/PlayBook


###########################################################################
# Build Erlang Bootstrap                                                  #
###########################################################################
pushd $ERLANG_ROOT

export ERL_TOP=`pwd`

if [ ! -f configure ] ; then
    ./otp_build autoconf
fi

./configure --enable-bootstrap-only

# a missing directory causes a build error, create it to prevent the error
pushd lib/hipe
if [ ! -d "ebin" ]; then
  mkdir ebin
fi
popd

# build fails with -j8 option
make

# Add path to erlc
ERLC_BOOTSTRAP_DIR=`pwd`/bootstrap/bin
if [ ! -f $ERLC_BOOTSTRAP_DIR/erlc ] ; then
    echo "Couldn't find $ERLC_BOOTSTRAP_DIR/erlc; did the Erlang build for Linux fail?"
fi

export PATH=$ERLC_BOOTSTRAP_DIR:$PATH

pushd lib
./configure
make
popd

popd

###########################################################################
# Setup PlayBook Environment Variables                                    #
###########################################################################

# ensure required BBNDK env variables are set
: ${BBNDK_DIR:?"Error: BBNDK_DIR environment variable is not set."}
: ${BBNDK_HOST:?"Error: BBNDK_HOST environment variable is not set."}
: ${BBNDK_TARGET:?"Error: BBNDK_TARGET environment variable is not set."}

#set up env for cross-compiling for PlayBook
export PATH=$BBNDK_HOST/usr/bin:$PATH
export CC="$BBNDK_HOST/usr/bin/qcc -V4.4.2,gcc_ntoarmv7le_cpp "
export CFLAGS="-V4.4.2,gcc_ntoarmv7le_cpp -g "
export CPP="$BBNDK_HOST/usr/bin/qcc -V4.4.2,gcc_ntoarmv7le_cpp -E"
export LD="$BBNDK_HOST/usr/bin/ntoarmv7-ld "
export RANLIB="$BBNDK_HOST/usr/bin/ntoarmv7-ranlib "



###########################################################################
# Build Erlang For PlayBook                                               #
###########################################################################
pushd $ERLANG_ROOT

CPPFLAGS="-D__PLAYBOOK__ -D__QNXNTO__ -DNO_SYSLOG -DLOG_ERR=1 -DSIZEOF_VOID_P=4 -I$BBNDK_TARGET/usr/include " \
LDFLAGS="-L$BBNDK_TARGET/armle-v7/lib -L$BBNDK_TARGET/armle-v7/usr/lib -lm -lsocket " \
erl_xcomp_sysroot=$BBNDK_TARGET/armle-v7 \
erl_xcomp_isysroot=$BBNDK_TARGET/usr/include \
erl_xcomp_bigendian=yes \
erl_xcomp_linux_clock_gettime_correction=yes \
erl_xcomp_linux_nptl=yes \
erl_xcomp_linux_usable_sigusrx=yes \
erl_xcomp_linux_usable_sigaltstack=yes \
erl_xcomp_poll=yes \
erl_xcomp_kqueue=yes \
erl_xcomp_putenv_copy=yes \
erl_xcomp_reliable_fpe=yes \
erl_xcomp_getaddrinfo=yes \
erl_xcomp_gethrvtime_procfs_ioctl=no \
erl_xcomp_clock_gettime_cpu_time=yes \
erl_xcomp_after_morecore_hook=no \
erl_xcomp_dlsym_brk_wrappers=yes \
./configure --build=i686-pc-linux-gnu --host=arm-unknown-nto-qnx6.5.0eabi --prefix=$PLAYBOOK_PREFIX/Erlang --disable-hipe --without-termcap --without-ssl

# comment out -lrt in <ERL_TOP>/erts/emulator/arm-unknown-nto-qnx6.5.0eabi/Makefile (this causes a build error)
pushd erts/emulator/arm-unknown-nto-qnx6.5.0eabi
cat Makefile | sed -e 's/LIBS\ +=\ -lrt/\#LIBS\ +=\ -lrt/' > temp.mk
rm Makefile
mv temp.mk Makefile
popd

make
make release RELEASE_ROOT=$PLAYBOOK_PREFIX/Erlang
pushd $PLAYBOOK_PREFIX/Erlang
./Install -cross -minimal $PLAYBOOK_PREFIX/Erlang
popd

popd

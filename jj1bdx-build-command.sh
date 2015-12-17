gmake clean
export ERL_TOP=`pwd`
export CC=clang CXX=clang CFLAGS="-O -g -fstack-protector" LDFLAGS="-fstack-protector" MAKEFLAGS="-j4"
./configure --enable-native-libs --with-ssl=/usr/local --without-javac --enable-hipe --enable-kernel-poll --without-odbc --without-wx-config --enable-threads --disable-sctp --enable-smp-support --enable-fp-exceptions --disable-silent-rules
gmake

#!/bin/sh
set -e

if [ -z "$WXWIDGETS_VERSION" ]; then
  WXWIDGETS_VERSION=3.1.4
fi

vsn=$WXWIDGETS_VERSION
curl --fail -LO https://github.com/wxWidgets/wxWidgets/releases/download/v$vsn/wxWidgets-$vsn.tar.bz2
tar -xf wxWidgets-$vsn.tar.bz2
mv wxWidgets-$vsn/ wxWidgets

cd wxWidgets
./configure \
  --disable-shared \
  --prefix=$PWD/release \
  --with-cocoa \
  --with-macosx-version-min=10.15 \
  --with-libjpeg=builtin \
  --with-libtiff=builtin
make
make install

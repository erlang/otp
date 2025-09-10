#!/bin/sh

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
##
## Copyright Ericsson AB 2025. All Rights Reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
## %CopyrightEnd%

set -e

export MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN)

if [ -z "$WXWIDGETS_VERSION" ]; then
  WXWIDGETS_VERSION=3.1.5
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
  --with-macosx-version-min=10.15
#  --disable-sys-libs

make
make install

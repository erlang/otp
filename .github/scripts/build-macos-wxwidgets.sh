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

MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN)
export MAKEFLAGS

if [ -z "$WXWIDGETS_VERSION" ]; then
  # Value is set in .github/workflows/main.yaml for MacOS only, this here is the fallback value
  WXWIDGETS_VERSION=3.2.8.1
fi

WX_SELECTED_VERSION=${WXWIDGETS_VERSION}
WX_DOWNLOAD_URL=https://github.com/wxWidgets/wxWidgets/releases/download/v${WX_SELECTED_VERSION}/wxWidgets-${WX_SELECTED_VERSION}.tar.bz2
echo "Downloading WxWidgets from ${WX_DOWNLOAD_URL}"
curl --fail -LO "${WX_DOWNLOAD_URL}"
tar -xf "wxWidgets-${WX_SELECTED_VERSION}.tar.bz2"
mv "wxWidgets-${WX_SELECTED_VERSION}/" wxWidgets

cd wxWidgets
./configure \
  --disable-shared \
  --prefix="${PWD}/release" \
  --with-cocoa \
  --with-macosx-version-min=10.15 \
  --with-libtiff=builtin
#  --disable-sys-libs

make
make install

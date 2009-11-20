#! /bin/sh
# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2009. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# %CopyrightEnd%
# 
# This little helper digs out the current version of microsoft CRT
# by compiling hello world and "parsing" the manifest file...

# To debug using a fake version:

# echo "8.0.50727.763"
# exit 0

cat > hello.c <<EOF
#include <stdio.h>

int main(void)
{
    printf("Hello world\n");
    return 0;
}

EOF
cl /MD hello.c > /dev/null 2>&1
if [ '!' -f hello.exe.manifest ]; then
    echo "This compiler does not generate manifest files - OK if using mingw" >&2
    exit 0
fi
VERSION=`grep '<assemblyIdentity' hello.exe.manifest | sed 's,.*version=.\([0-9\.]*\).*,\1,g' | grep -v '<'`
rm -f hello.c hello.obj hello.exe hello.exe.manifest
if [ -z "$VERSION" ]; then
    exit 1
fi
echo $VERSION
exit 0

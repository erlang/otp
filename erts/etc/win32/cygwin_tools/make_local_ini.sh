#! /bin/bash
# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2003-2016. All Rights Reserved.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 
# %CopyrightEnd%
# 
# Create a local init-file for erlang in the build environment.
if [ -z "$1" ]; then
    if [ -z $ERL_TOP ]; then
	echo "error: $0: No rootdir available"
 	exit 1
    else
	RDIR=$ERL_TOP
    fi
else
    RDIR=$1
fi

DDIR=`(cygpath -d $RDIR 2>/dev/null || cygpath -w $RDIR) | sed 's,\\\,\\\\\\\\,g'`


cat > $RDIR/bin/erl.ini <<EOF
[erlang]
Bindir=$DDIR\\\\bin\\\\win32
Progname=erl
Rootdir=$DDIR
EOF


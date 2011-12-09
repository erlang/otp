#! /bin/bash
# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2003-2011. All Rights Reserved.
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

DDIR=`msys2win_path.sh $RDIR | sed 's,\\\,\\\\\\\\,g'`


cat > $RDIR/bin/erl.ini <<EOF
[erlang]
Bindir=$DDIR\\\\bin\\\\win32
Progname=erl
Rootdir=$DDIR
EOF


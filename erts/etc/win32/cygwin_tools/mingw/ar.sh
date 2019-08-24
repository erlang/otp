#! /bin/sh
# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
CMD=""
if [ -z "$MINGW_EXE_PATH" ]; then
    echo "You have to set MINGW_EXE_PATH to run ar.sh" >&2
    exit 1
fi
while test -n "$1" ; do
    x="$1"
    case "$x" in
	-o|-out:)
	    shift
	    case "$1" in
		/*)
		    MPATH=`cygpath -m $1`;;
		 *)
		    MPATH=$1;;
	    esac
	    CMD="rcv \"$MPATH\" $CMD";; 
	-out:*)
	    y=`echo $x | sed 's,^-out:\(.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="rcv \"$MPATH\" $CMD";;
	-o*)
	    y=`echo $x | sed 's,^-o\(.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="rcv \"$MPATH\" $CMD";; 
	/*)
	    MPATH=`cygpath -m $x`;
	    CMD="$CMD \"$MPATH\"";; 
	*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD \"$y\"";;
    esac
    shift
done

eval $MINGW_EXE_PATH/ar.exe $CMD

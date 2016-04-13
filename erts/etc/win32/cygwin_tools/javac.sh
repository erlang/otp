#! /bin/sh
# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
# Note! This shellscript expects to be run in a cygwin environment,
# it converts erlc command lines to native windows erlc commands, which
# basically means running the command cygpath on whatever is a path...

CMD=""
CLASSPATH=`cygpath -m -p $CLASSPATH`
export CLASSPATH
#echo "CLASSPATH=$CLASSPATH"
SAVE="$@"
while test -n "$1" ; do
    x="$1"
    case "$x" in
	-I/*|-o/*|-d/*)
	    y=`echo $x | sed 's,^-[Iod]\(/.*\),\1,g'`;
	    z=`echo $x | sed 's,^-\([Iod]\)\(/.*\),\1,g'`;
	    #echo "Foooo:$z"
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -$z\"$MPATH\"";; 
	-d|-I|-o)
	    shift;
	    MPATH=`cygpath -m $1`;
	    CMD="$CMD $x $MPATH";; 
	/*)
	    #echo "absolute:"$x;
	    MPATH=`cygpath -m $x`;
	    CMD="$CMD \"$MPATH\"";; 
	*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD \"$y\"";;
    esac
    shift
done
#echo javac.exe $CMD
eval javac.exe $CMD

#! /bin/sh
# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
CMD=""
while test -n "$1" ; do
    x="$1"
    case "$x" in
	-out:)
	    shift
	    case "$1" in
		/*)
		    MPATH=`cygpath -m $1`;;
		 *)
		    MPATH=$1;;
	    esac
	    CMD="$CMD -out:\"$MPATH\"";; 
	-out:/*)
	    y=`echo $x | sed 's,^-out:\(/.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -out:\"$MPATH\"";; 
	/*)
	    MPATH=`cygpath -m $x`;
	    CMD="$CMD \"$MPATH\"";; 
	*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD \"$y\"";;
    esac
    shift
done

eval lib.exe $CMD

#! /bin/sh
# set -x
# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2002-2011. All Rights Reserved.
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
# Save the command line for debug outputs
SAVE="$@"
CMD=""
OUTPUT_FILENAME=""

# Find the correct rc.exe. This could be done by the configure script,
# But as we seldom use the resource compiler, it might as well be done here...
RCC=""
save_ifs=$IFS
IFS=:
for p in $PATH; do 
    if [ -f $p/rc.exe ]; then 
	if [ -n "`$p/rc.exe -? 2>&1 | grep -i "resource compiler"`" ]; then
	    RCC=`echo "$p/rc.exe" | sed 's/ /\\\\ /g'`
	fi
    fi
done
IFS=$save_ifs

if [ -z "$RCC" ]; then
    echo 'rc.exe not found!' >&2
    exit 1
fi

while test -n "$1" ; do
    x="$1"
    case "$x" in
	-o)
	    shift
	    MPATH=`msys2win_path.sh -m $1`;
	    OUTPUT_FILENAME="$MPATH";;
	-o/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    MPATH=`msys2win_path.sh -m $y`;
	    OUTPUT_FILENAME="$MPATH";;
	-I)
	    shift
	    MPATH=`msys2win_path.sh -m $1`;
	    CMD="$CMD -I\"$MPATH\"";;
	-I/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    MPATH=`msys2win_path.sh -m $y`;
	    CMD="$CMD -I\"$MPATH\"";;
	/*)
	    MPATH=`msys2win_path.sh -m $x`;
	    CMD="$CMD \"$MPATH\"";; 
	*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD \"$y\"";;
    esac
    shift
done
p=$$
if [ -n "$OUTPUT_FILENAME" ]; then
    CMD="-Fo$OUTPUT_FILENAME $CMD"
fi
if [ "X$RC_SH_DEBUG_LOG" != "X" ]; then
    echo rc.sh "$SAVE" >>$RC_SH_DEBUG_LOG
    echo rc.exe $CMD >>$RC_SH_DEBUG_LOG
fi
eval $RCC "$CMD"  >/tmp/rc.exe.${p}.1 2>/tmp/rc.exe.${p}.2
RES=$?
tail +2 /tmp/rc.exe.${p}.2 >&2
cat /tmp/rc.exe.${p}.1
rm -f /tmp/rc.exe.${p}.2 /tmp/rc.exe.${p}.1
exit $RES

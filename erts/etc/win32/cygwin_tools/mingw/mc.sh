#! /bin/sh
# set -x
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
# Save the command line for debug outputs
SAVE="$@"
CMD=""
OUTPUT_DIRNAME=""

# Find the correct mc.exe. This could be done by the configure script,
# But as we seldom use the resource compiler, it might as well be done here...
if [ -z "$WINE_EXE_PATH" ]; then
    echo "You have to set MINGW_EXE_PATH to run cc.sh" >&2
    exit 1
fi

MCC="$WINE_EXE_PATH/wmc.exe -i"
RCC="$WINE_EXE_PATH/wrc.exe -i"

while test -n "$1" ; do
    x="$1"
    case "$x" in
	-o)
	    shift
	    OUTPUT_DIRNAME="$1";;
	-o/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    OUTPUT_DIRNAME="$y";;
	-I)
	    shift
	    MPATH=`cygpath -m $1`;
	    CMD="$CMD -I\"$MPATH\"";;
	-I/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -I\"$MPATH\"";;
	*)
	    MPATH=`cygpath -m -a $x`;
	    INFILE=$MPATH;;
	    #CMD="$CMD \"$MPATH\"";; 
    esac
    shift
done
p=$$
if [ "X$MC_SH_DEBUG_LOG" != "X" ]; then
    echo mc.sh "$SAVE" >>$MC_SH_DEBUG_LOG
    echo $MCC $INFILE $CMD >>$MC_SH_DEBUG_LOG
fi
if [ -n "$OUTPUT_DIRNAME" ]; then
    cd $OUTPUT_DIRNAME
    RES=$?
    if [ "$RES" != "0" ]; then
	echo "mc.sh: Error: could not cd to $OUTPUT_DIRNAME">&2
	exit $RES
    fi
fi
eval $MCC "$INFILE" "$CMD"  >/tmp/mc.exe.${p}.1 2>/tmp/mc.exe.${p}.2
RES=$?
cat /tmp/mc.exe.${p}.2 >&2
cat /tmp/mc.exe.${p}.1
rm -f /tmp/mc.exe.${p}.2 /tmp/mc.exe.${p}.1
if [ $RES -eq 0 ]; then
    XINFILE=`echo $INFILE | sed 's,.*/\([^/]*\),\1,' | sed 's,.mc$,.rc,'`
    if [ "X$MC_SH_DEBUG_LOG" != "X" ]; then
	echo $RCC $XINFILE $CMD >>$MC_SH_DEBUG_LOG
    fi
    eval $RCC $XINFILE "$CMD"  >/tmp/rc.exe.${p}.1 2>/tmp/rc.exe.${p}.2
    RES=$?
    cat /tmp/rc.exe.${p}.2 >&2
    cat /tmp/rc.exe.${p}.1
    rm -f /tmp/rc.exe.${p}.2 /tmp/rc.exe.${p}.1
fi
exit $RES

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
OUTPUT_FILENAME=""

if [ -z "$MINGW_EXE_PATH" ]; then
    echo "You have to set MINGW_EXE_PATH to run rc.sh" >&2
    exit 1
fi


# # Find the correct rc.exe. This could be done by the configure script,
# # But as we seldom use the resource compiler, it might as well be done here...
# RCC=""
# save_ifs=$IFS
# IFS=:
# for p in $PATH; do 
#     if [ -f $p/windres.exe ]; then 
# 	if [ -n "`$p/windres.exe --version 2>&1 | grep -i "GNU windres"`" ]; then 
# 	    RCC=$p/windres.exe
# 	fi
#     fi
# done
# IFS=$save_ifs

RCC=$MINGW_EXE_PATH/windres.exe

if [ -z "$RCC" ]; then
    echo 'windres.exe not found!' >&2
    exit 1
fi

while test -n "$1" ; do
    x="$1"
    case "$x" in
	-o)
	    shift
	    MPATH=`cygpath -m $1`;
	    OUTPUT_FILENAME="$MPATH";;
	-o/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    OUTPUT_FILENAME="$MPATH";;
	-I)
	    shift
	    MPATH=`cygpath -m $1`;
	    CMD="$CMD -I\"$MPATH\"";;
	-I/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -I\"$MPATH\"";;
	/*)
	    MPATH=`cygpath -m $x`;
	    CMD="$CMD \"$MPATH\"";; 
	*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD \"$y\"";;
    esac
    shift
done
p=$$
if [ -n "$OUTPUT_FILENAME" ]; then
    CMD="-o $OUTPUT_FILENAME $CMD"
fi
if [ "X$RC_SH_DEBUG_LOG" != "X" ]; then
    echo rc.sh "$SAVE" >>$RC_SH_DEBUG_LOG
    echo windres.exe $CMD >>$RC_SH_DEBUG_LOG
fi
eval $RCC "$CMD"  >/tmp/rc.exe.${p}.1 2>/tmp/rc.exe.${p}.2
RES=$?
cat /tmp/rc.exe.${p}.2 >&2
cat /tmp/rc.exe.${p}.1
rm -f /tmp/rc.exe.${p}.2 /tmp/rc.exe.${p}.1
exit $RES

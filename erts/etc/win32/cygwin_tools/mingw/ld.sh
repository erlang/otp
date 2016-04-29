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
kernel_libs="-lkernel32 -ladvapi32"
gdi_libs="-lgdi32 -luser32 -lcomctl32 -lcomdlg32 -lshell32"
DEFAULT_LIBRARIES="$kernel_libs $gdi_libs"

CMD=""
STDLIB=-lmsvcrt
DEBUG_BUILD=false
STDLIB_FORCED=false
BUILD_DLL=false
OUTPUT_FILENAME=""

if [ -z "$MINGW_EXE_PATH" ]; then
    echo "You have to set MINGW_EXE_PATH to run cc.sh" >&2
    exit 1
fi

while test -n "$1" ; do
    x="$1"
    case "$x" in
	-dll| -DLL | -shared)
	    BUILD_DLL=true;; 
	-L/*|-L.*)
	    y=`echo $x | sed 's,^-L\(.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -L \"$MPATH\"";; 
	-lMSVCRT|-lmsvcrt)
	    STDLIB_FORCED=true;
	    STDLIB=-lmsvcrt;; 
	-lMSVCRTD|-lmsvcrtd)
	    STDLIB_FORCED=true;
	    STDLIB=-lmsvcrtd;; 
	-lLIBCMT|-llibcmt)
	    STDLIB_FORCED=true;
	    STDLIB=-llibcmt;; 
	-lLIBCMTD|-llibcmtd)
	    STDLIB_FORCED=true;
	    STDLIB=-llibcmtd;; 
	-lsocket)
	    DEFAULT_LIBRARIES="$DEFAULT_LIBRARIES -lws2_32";;
	-l*)
	    y=`echo $x | sed 's,^-l\(.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -l\"${MPATH}\"";; 
	-g)
	    DEBUG_BUILD=true;;
	-pdb:none|-incremental:no)
	    ;;
	-implib:*)
	    y=`echo $x | sed 's,^-implib:\(.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -Xlinker --out-implib -Xlinker \"${MPATH}\"";; 
	-entry:*)
	    y=`echo $x | sed 's,^-entry:\(.*\),\1,g'`;
	    CMD="$CMD -Xlinker --entry -Xlinker _$y";; 
	-def:*)
	    ;;
	## Ignore -def: for now as ld.sh core dumps...
	#    y=`echo $x | sed 's,^-def:\(.*\),\1,g'`;
	#    MPATH=`cygpath -m $y`;
	#    CMD="$CMD -Xlinker --output-def -Xlinker \"${MPATH}\"";; 
	-o)
	    shift
	    MPATH=`cygpath -m $1`;
	    OUTPUT_FILENAME="$MPATH";;
	-o/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    OUTPUT_FILENAME="$MPATH";;
	/*)
	    MPATH=`cygpath -m $x`;
	    CMD="$CMD \"$MPATH\"";; 
	*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD \"$y\"";;
    esac
    shift
done
if [ $DEBUG_BUILD = true ]; then
    linktype="-g"
    if [ $STDLIB_FORCED = false ]; then
	STDLIB=-lmsvcrt #d?
    fi
else
    linktype=
fi

if [ $BUILD_DLL = true ];then
    case "$OUTPUT_FILENAME" in
	*.exe|*.EXE)
	    echo "Warning, output set to .exe when building DLL" >&2
	    CMD="-shared -o \"$OUTPUT_FILENAME\" $CMD";;
	*.dll|*.DLL)
	    CMD="-shared -o \"$OUTPUT_FILENAME\" $CMD";;
	"")
	    CMD="-shared -o \"a.dll\" $CMD";;
	*)
	    CMD="-shared -o \"${OUTPUT_FILENAME}.dll\" $CMD";;
    esac
else
    case "$OUTPUT_FILENAME" in
	*.exe|*.EXE)
	    CMD="-o \"$OUTPUT_FILENAME\" $CMD";;
	*.dll|*.DLL)
	    echo "Warning, output set to .dll when building EXE" >&2
	    CMD="-o \"$OUTPUT_FILENAME\" $CMD";;
	"")
	    CMD="-o \"a.exe\" $CMD";;
	*)
	    CMD="-o \"${OUTPUT_FILENAME}.exe\" $CMD";;
    esac
fi    
	    
p=$$
CMD="$linktype $CMD $STDLIB $DEFAULT_LIBRARIES"
if [ "X$LD_SH_DEBUG_LOG" != "X" ]; then
    echo ld.sh "$SAVE" >>$LD_SH_DEBUG_LOG
    echo $MINGW_EXE_PATH/gcc.exe $CMD >>$LD_SH_DEBUG_LOG
fi
eval $MINGW_EXE_PATH/gcc "$CMD"  >/tmp/link.exe.${p}.1 2>/tmp/link.exe.${p}.2
RES=$?
#tail +2 /tmp/link.exe.${p}.2 >&2
cat /tmp/link.exe.${p}.2 >&2
cat /tmp/link.exe.${p}.1
rm -f /tmp/link.exe.${p}.2 /tmp/link.exe.${p}.1
exit $RES

#! /bin/sh
# set -x
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
# Save the command line for debug outputs

SAVE="$@"
kernel_libs="kernel32.lib advapi32.lib"
gdi_libs="gdi32.lib user32.lib comctl32.lib comdlg32.lib shell32.lib"
DEFAULT_LIBRARIES="$kernel_libs $gdi_libs"

CMD=""
STDLIB=MSVCRT.LIB
DEBUG_BUILD=false
STDLIB_FORCED=false
BUILD_DLL=false
OUTPUT_FILENAME=""

while test -n "$1" ; do
    x="$1"
    case "$x" in
	-dll| -DLL)
	    BUILD_DLL=true;; 
	-L/*|-L.*)
	    y=`echo $x | sed 's,^-L\(.*\),\1,g'`;
	    MPATH=`msys2win_path.sh -m $y`;
	    CMD="$CMD -libpath:\"$MPATH\"";; 
	-lMSVCRT|-lmsvcrt)
	    STDLIB_FORCED=true;
	    STDLIB=MSVCRT.LIB;; 
	-lMSVCRTD|-lmsvcrtd)
	    STDLIB_FORCED=true;
	    STDLIB=MSVCRTD.LIB;; 
	-lLIBCMT|-llibcmt)
	    STDLIB_FORCED=true;
	    STDLIB=LIBCMT.LIB;; 
	-lLIBCMTD|-llibcmtd)
	    STDLIB_FORCED=true;
	    STDLIB=LIBCMTD.LIB;; 
	-lsocket)
	    DEFAULT_LIBRARIES="$DEFAULT_LIBRARIES WS2_32.LIB IPHLPAPI.LIB";;
	-l*)
	    y=`echo $x | sed 's,^-l\(.*\),\1,g'`;
	    MPATH=`msys2win_path.sh -m $y`;
	    CMD="$CMD \"${MPATH}.lib\"";; 
	-g)
	    DEBUG_BUILD=true;;
	-pdb:none|-incremental:no)
	    ;;
	-implib:*)
	    y=`echo $x | sed 's,^-implib:\(.*\),\1,g'`;
	    MPATH=`msys2win_path.sh -m $y`;
	    CMD="$CMD -implib:\"${MPATH}\"";; 
	-def:*)
	    y=`echo $x | sed 's,^-def:\(.*\),\1,g'`;
	    MPATH=`msys2win_path.sh -m $y`;
	    CMD="$CMD -def:\"${MPATH}\"";; 
	-o)
	    shift
	    MPATH=`msys2win_path.sh -m $1`;
	    OUTPUT_FILENAME="$MPATH";;
	-o/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    MPATH=`msys2win_path.sh -m $y`;
	    OUTPUT_FILENAME="$MPATH";;
	/*)
	    MPATH=`msys2win_path.sh -m $x`;
	    CMD="$CMD \"$MPATH\"";; 
	*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD \"$y\"";;
    esac
    shift
done
if [ $DEBUG_BUILD = true ]; then
    linktype="-debug -pdb:none"
    if [ $STDLIB_FORCED = false ]; then
	STDLIB=MSVCRTD.LIB
    fi
fi
# Generate a PDB 
linkadd_pdb=""
case "$OUTPUT_FILENAME" in
    *.exe|*.EXE)
	    fn=`echo "$OUTPUT_FILENAME" | sed 's,[eE][xX][eE]$,,g'`;
	    linkadd_pdb="-pdb:\"${fn}pdb\"";;
    *.dll|*.DLL)
	    fn=`echo "$OUTPUT_FILENAME" | sed 's,[dD][lL][lL]$,,g'`;
	    linkadd_pdb="-pdb:\"${fn}pdb\"";;
    "")
	    linkadd_pdb="-pdb:\"a.pdb\"";;
    *)
	    linkadd_pdb="-pdb:\"${OUTPUT_FILENAME}.pdb\"";;
esac
	
    linktype="-debug $linkadd_pdb"

CHMOD_FILE=""

if [ $BUILD_DLL = true ];then
    case "$OUTPUT_FILENAME" in
	*.exe|*.EXE)
	    echo "Warning, output set to .exe when building DLL" >&2
	    CHMOD_FILE="$OUTPUT_FILENAME";
	    CMD="-dll -out:\"$OUTPUT_FILENAME\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}\;2";
	    MANIFEST="${OUTPUT_FILENAME}.manifest";;
	*.dll|*.DLL)
	    CMD="-dll -out:\"$OUTPUT_FILENAME\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}\;2";
	    MANIFEST="${OUTPUT_FILENAME}.manifest";;
	"")
	    CMD="-dll -out:\"a.dll\" $CMD";
	    OUTPUTRES="a.dll\;2";
	    MANIFEST="a.dll.manifest";;
	*)
	    CMD="-dll -out:\"${OUTPUT_FILENAME}.dll\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}.dll\;2";
	    MANIFEST="${OUTPUT_FILENAME}.dll.manifest";;
    esac
else
    case "$OUTPUT_FILENAME" in
	*.exe|*.EXE)
	    CHMOD_FILE="$OUTPUT_FILENAME";
	    CMD="-out:\"$OUTPUT_FILENAME\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}\;1"
	    MANIFEST="${OUTPUT_FILENAME}.manifest";;
	*.dll|*.DLL)
	    echo "Warning, output set to .dll when building EXE" >&2
	    CMD="-out:\"$OUTPUT_FILENAME\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}\;1";
	    MANIFEST="${OUTPUT_FILENAME}.manifest";;
	"")
	    CHMOD_FILE="a.exe";
	    CMD="-out:\"a.exe\" $CMD";
	    OUTPUTRES="a.exe\;1";
	    MANIFEST="a.exe.manifest";;
	*)
	    CMD="-out:\"${OUTPUT_FILENAME}.exe\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}.exe\;1";
	    MANIFEST="${OUTPUT_FILENAME}.exe.manifest";;
    esac
fi    
	    
p=$$
CMD="$linktype -nologo -incremental:no $CMD $STDLIB $DEFAULT_LIBRARIES"
if [ "X$LD_SH_DEBUG_LOG" != "X" ]; then
    echo ld.sh "$SAVE" >>$LD_SH_DEBUG_LOG
    echo link.exe $CMD >>$LD_SH_DEBUG_LOG
fi
eval link.exe "$CMD"  >/tmp/link.exe.${p}.1 2>/tmp/link.exe.${p}.2
RES=$?

CMANIFEST=`win2msys_path.sh $MANIFEST`
if [ "$RES" = "0" -a -f "$CMANIFEST" ]; then
    # Add stuff to manifest to turn off "virtualization"
    sed -n -i '1h;1!H;${;g;s,<trustInfo.*</trustInfo>.,,g;p;}' $CMANIFEST 2>/dev/null
    sed -i "s/<\/assembly>/ <ms_asmv2:trustInfo xmlns:ms_asmv2=\"urn:schemas-microsoft-com:asm.v2\">\n  <ms_asmv2:security>\n   <ms_asmv2:requestedPrivileges>\n    <ms_asmv2:requestedExecutionLevel level=\"AsInvoker\" uiAccess=\"false\"\/>\n   <\/ms_asmv2:requestedPrivileges>\n  <\/ms_asmv2:security>\n <\/ms_asmv2:trustInfo>\n<\/assembly>/" $CMANIFEST 2>/dev/null

    eval mt.exe -nologo -manifest "$MANIFEST" -outputresource:"$OUTPUTRES" >>/tmp/link.exe.${p}.1 2>>/tmp/link.exe.${p}.2
    RES=$?
    if [ "$RES" != "0" ]; then
	REMOVE=`echo "$OUTPUTRES" | sed 's,\\\;[12]$,,g'`
	CREMOVE=`win2msys_path.sh $REMOVE`
        ## If Defender or Search are enabled, they will lock just created files
        ## and then mt will fail :/
        echo "If you get this error, make sure Windows Defender AND Windows Search is disabled">>/tmp/link.exe.${p}.1
	rm -f "$CREMOVE"
    fi
    rm -f "$CMANIFEST"
fi

# This works around some strange behaviour 
# in cygwin 1.7 Beta on Windows 7 with samba drive.
# Configure will think the compiler failed if test -x fails, 
# which it might do as we might not be the owner of the
# file.
if [ '!' -z "$CHMOD_FILE" -a -s "$CHMOD_FILE" -a '!' -x "$CHMOD_FILE" ]; then
    chmod +x $CHMOD_FILE
fi
    
tail -n +2 /tmp/link.exe.${p}.2 >&2
cat /tmp/link.exe.${p}.1
rm -f /tmp/link.exe.${p}.2 /tmp/link.exe.${p}.1
exit $RES

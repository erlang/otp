#! /bin/sh
# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

# first find some tool we know exists, i.e. cl.exe
lookup_prog_in_path ()
{
    PROG=$1
    save_ifs=$IFS
    IFS=:
    for p in $PATH; do
	if [ -f $p/$PROG.exe ]; then
	    echo $p/$PROG
	    break;
	fi
    done
    IFS=$save_ifs
}

remove_path_element()
{
    EL=$1
    PA=$2
    ACC=""
    save_ifs=$IFS
    IFS=/
    set $PA
    N=$#
    while [ $N -gt 1 ]; do
	if [ '!' -z "$1" ]; then
	    ACC="${ACC}/$1"
	fi
	N=`expr $N - 1`
	shift
    done
    UP=`echo $1 | tr [:lower:] [:upper:]`
    ELUP=`echo $EL | tr [:lower:] [:upper:]`
    IFS=$save_ifs
    if [ "$UP" = "$ELUP" ]; then
	echo "$ACC"
    else
	echo "${ACC}/$1"
    fi
    #echo "ACC=$ACC" >&2
    #echo "1=$1" >&2
}
	
add_path_element()
{
    EL=$1
    PA=$2

    ELUP=`echo $EL | tr [:lower:] [:upper:]`
    #echo "PA=$PA" >&2
    for x in ${PA}/*; do
	#echo "X=$x" >&2
	UP=`basename "$x" | tr [:lower:] [:upper:]`
	#echo "UP=$UP" >&2
	if [ "$UP" = "$ELUP" ]; then
	    echo "$x"
	    return 0;
	fi
    done
    echo "$PA"
}

# Look to see if it's 64bit
if [ "$1" = "win64" ]; then
    AMD64DIR=true
    VCREDIST=vcredist_x64
    COMPONENTS="cl amd64 bin vc"
elif [ "$1" = "win32" ]; then
    AMD64DIR=false
    VCREDIST=vcredist_x86
    COMPONENTS="cl bin vc"
else
    echo "TARGET argument should win32 or win64"
    exit 2
fi

if [ x"$VCToolsRedistDir" != x"" ]; then
    File="$VCToolsRedistDir/$VCREDIST.exe"
    if [ -r "$File" ]; then
	echo "$File"
	exit 0
    fi
fi

CLPATH=`lookup_prog_in_path cl`
if [ -z "$CLPATH" ]; then 
    echo "Can not locate cl.exe and vcredist_x86/x64.exe - OK if using mingw" >&2
    exit 1
fi

echo $CLPATH
BPATH=$CLPATH
for x in $COMPONENTS; do
    #echo $x
    NBPATH=`remove_path_element $x "$BPATH"`
    if [ "$NBPATH" = "$BPATH" ]; then
	echo "Failed to locate $VCREDIST.exe because cl.exe was in an unexpected location" >&2
	exit 2
    fi
    BPATH="$NBPATH"
done
BPATH_LIST=$BPATH

# rc.exe is in the Microsoft SDK directory of VS2008
RCPATH=`lookup_prog_in_path rc`
fail=false
if [ '!' -z "$RCPATH" ]; then 
    BPATH=$RCPATH
    allow_fail=false
    if [ $AMD64DIR = true ]; then
	COMPONENTS="rc x64 bin @ANY v6.0A v7.0A v7.1"
    else
	COMPONENTS="rc bin @ANY v6.0A v7.0A v7.1"
    fi
    for x in $COMPONENTS; do
	if [ $x = @ANY ]; then
	    allow_fail=true
	else
	    NBPATH=`remove_path_element $x "$BPATH"`
	    if [ $allow_fail = false -a "$NBPATH" = "$BPATH" ]; then
		fail=true
		break;
	    fi
	    BPATH="$NBPATH"
	fi
    done
    if [ $fail = false ]; then
	BPATH_LIST="$BPATH_LIST $BPATH"
    fi
fi
# echo "BPATH_LIST=$BPATH_LIST"

# Frantic search through two roots with different 
# version directories. We want to be very specific about the
# directory structures as we wouldnt want to find the wrong 
# redistributables...

#echo $BPATH_LIST
for BP in $BPATH_LIST; do
    for verdir in "sdk v2.0" "sdk v3.5" "v6.0A" "v7.0"  "v7.0A" "v7.1" "VC redist 1033"; do
	BPATH=$BP
	fail=false
	allow_fail=false
	for x in $verdir @ANY bootstrapper packages $VCREDIST Redist VC @ALL $VCREDIST.exe; do
	    #echo "x=$x"
	    #echo "BPATH=$BPATH"
	    #echo "allow_fail=$allow_fail"
	    if [ $x = @ANY ]; then
		allow_fail=true
	    elif [ $x = @ALL ]; then
		allow_fail=false
	    else
		NBPATH=`add_path_element $x "$BPATH"`
		if [ $allow_fail = false -a "$NBPATH" = "$BPATH" ]; then
		    fail=true
		    break;
		fi
		BPATH="$NBPATH"
	    fi
	done
	if [ $fail = false ]; then
	    break;
	fi
    done
    if [ $fail = false ]; then
	echo $BPATH
	exit 0
    fi
done

# shortcut for locating $VCREDIST.exe is to put it into $ERL_TOP
if [ -f $ERL_TOP/$VCREDIST.exe ]; then
    echo $ERL_TOP/$VCREDIST.exe
    exit 0
fi

# or $ERL_TOP/.. to share across multiple builds
if [ -f $ERL_TOP/../$VCREDIST.exe ]; then
    echo $ERL_TOP/../$VCREDIST.exe
    exit 0
fi

echo "Failed to locate $VCREDIST.exe because directory structure was unexpected" >&2
exit 3


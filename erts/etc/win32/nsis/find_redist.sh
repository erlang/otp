#! /bin/sh
# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2011. All Rights Reserved.
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

# first find some tool we know exists, i.e. cl.exe
lookup_prog_in_path ()
{
    PROG=$1
    save_ifs=$IFS
    IFS=:
    for p in $PATH; do
	# In cygwin the programs are not always executable and have .exe suffix...
	if [ "X$TARGET" = "Xwin32" ]; then
	    if [ -f $p/$PROG.exe ]; then
		echo $p/$PROG
		break;
	    fi
	else
	    if [ -x $p/$PROG ]; then
		echo $p/$PROG
		break;
	    fi
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

CLPATH=`lookup_prog_in_path cl`

if [ -z "$CLPATH" ]; then 
    echo "Can not locate cl.exe and vcredist_x86.exe - OK if using mingw" >&2
    exit 1
fi

#echo $CLPATH
BPATH=$CLPATH
for x in cl bin vc; do
    #echo $x
    NBPATH=`remove_path_element $x "$BPATH"`
    if [ "$NBPATH" = "$BPATH" ]; then
	echo "Failed to locate vcredist_x86.exe because cl.exe was in an unexpected location" >&2
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
    for x in rc bin @ANY v6.0A v7.0A v7.1; do
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

# Frantic search through two roots with different 
# version directories. We want to be very specific about the
# directory structures as we wouldnt want to find the wrong 
# redistributables...

#echo $BPATH_LIST
for BP in $BPATH_LIST; do
    for verdir in "sdk v2.0" "sdk v3.5" "v6.0A" "v7.0"  "v7.0A" "v7.1"; do
	BPATH=$BP
	fail=false
	allow_fail=false
	for x in $verdir @ANY bootstrapper packages vcredist_x86 Redist VC @ALL vcredist_x86.exe; do
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

# shortcut for locating vcredist_x86.exe is to put it into $ERL_TOP
if [ -f $ERL_TOP/vcredist_x86.exe ]; then
    echo $ERL_TOP/vcredist_x86.exe
    exit 0
fi

# or $ERL_TOP/.. to share across multiple builds
if [ -f $ERL_TOP/../vcredist_x86.exe ]; then
    echo $ERL_TOP/../vcredist_x86.exe
    exit 0
fi

echo "Failed to locate vcredist_x86.exe because directory structure was unexpected" >&2
exit 3


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
# Icky cl wrapper that does it's best to behave like a Unixish cc.
# Made to work for Erlang builds and to make configure happy, not really 
# general I suspect.
# set -x
# Save the command line for debug outputs
SAVE="$@"

# Constants
COMMON_CFLAGS="-mwindows -D__WIN32__ -DWIN32 -DWINDOWS -D_WIN32 -DNT -DWIN32_MINGW"

# Variables
# The stdout and stderr for the compiler
MSG_FILE=/tmp/gcc.exe.$$.1
ERR_FILE=/tmp/gcc.exe.$$.2

# "Booleans" determined during "command line parsing"
# If the stdlib option is explicitly passed to this program
MD_FORCED=false
# If we're preprocession (only) i.e. -E
PREPROCESSING=false
# If this is supposed to be a debug build
DEBUG_BUILD=false
# If this is supposed to be an optimized build (there can only be one...)
OPTIMIZED_BUILD=false
# If we're linking or only compiling
LINKING=true

# This data is accumulated during command line "parsing"
# The stdlibrary option, default multithreaded dynamic
# NOTE! The standard library options are actually ignored by the linker later 
# on, I've retained parsing of them from the VC++ version as they may be 
# needed in the future...
MD=-MD
# Flags for debug compilation
DEBUG_FLAGS=""
# Flags for optimization
OPTIMIZE_FLAGS=""
# The specified output filename (if any), may be either object or exe.
OUTFILE=""
# Unspe3cified command line options for the compiler
CMD=""
# All the c source files, in unix style
SOURCES=""
# All the options to pass to the linker, kept in Unix style
LINKCMD=""
LINKSOURCES=""
DEPENDING=false

if [ -z "$MINGW_EXE_PATH" ]; then
    echo "You have to set MINGW_EXE_PATH to run cc.sh" >&2
    exit 1
fi


# Loop through the parameters and set the above variables accordingly
# Also convert some cygwin filenames to "mixed style" dito (understood by the
# compiler very well), except for anything passed to the linker, that script
# handles those and the sources, which are also kept unixish for now

while test -n "$1" ; do
    x="$1"
    case "$x" in
	-Wall)
	    ;;
	-c) 
	    LINKING=false;;
	-E)
	    PREPROCESSING=true;
	    LINKING=false;; # Obviously...
	-MM|-M)
	    DEPENDING=true;
	    LINKING=false;; # Obviously...
	-O*)
	    # Optimization hardcoded
	    OPTIMIZE_FLAGS="$x";
	    DEBUG_FLAGS="-ggdb";
	    DEBUG_BUILD=false;
	    if [ $MD_FORCED = false ]; then
		MD=-MD;
	    fi
	    OPTIMIZED_BUILD=true;;
	-g|-ggdb)
	    if [ $OPTIMIZED_BUILD = false ];then
		# Hardcoded windows debug flags
		DEBUG_FLAGS="-ggdb";
		if [ $MD_FORCED = false ]; then
		    MD=-MDd;
		fi
		DEBUG_BUILD=true;
	    fi;;
	# Allow forcing of stdlib
	-mt|-MT)
	    MD="-MT";
	    MD_FORCED=true;;
	-md|-MD)
	    MD="-MD";
	    MD_FORCED=true;;
	-ml|-ML)
	    MD="-ML";
	    MD_FORCED=true;;
	-mdd|-MDD|-MDd)
	    MD="-MDd";
	    MD_FORCED=true;;
	-mtd|-MTD|-MTd)
	    MD="-MTd";
	    MD_FORCED=true;;
	-mld|-MLD|-MLd)
	    MD="-MLd";
	    MD_FORCED=true;;
	-o)
	    shift;
	    OUTFILE="$1";;
	-o*)
	    y=`echo $x | sed 's,^-[Io]\(.*\),\1,g'`;
	    OUTFILE="$y";;
	-I/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    z=`echo $x | sed 's,^-\([Io]\)\(/.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -$z\"$MPATH\"";; 
	-I*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD $y";;
	-D*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD $y";;
	-l*)
	    y=`echo $x | sed 's,^-l\(.*\),\1,g'`;
	    LINKCMD="$LINKCMD $x";;
	/*.c)
	    SOURCES="$SOURCES $x";;
	*.c)
	    SOURCES="$SOURCES $x";;
	/*.o)
	    LINKCMD="$LINKCMD $x";;
	*.o)
	    LINKCMD="$LINKCMD $x";;
	*)
	    # Try to quote uninterpreted options
	    y=`echo $x | sed 's,",\\\",g'`;
	    LINKCMD="$LINKCMD $y";;
    esac
    shift
done

#Return code from compiler, linker.sh and finally this script...
RES=0

# Accumulated object names
ACCUM_OBJECTS=""

# A temporary object file location
TMPOBJDIR=/tmp/tmpobj$$
rm -rf $TMPOBJDIR
mkdir $TMPOBJDIR


for x in $SOURCES; do
    # Compile each source
    if [ $LINKING = false ]; then
	# We should have an output defined, which is a directory 
	# or an object file
	case $OUTFILE in
	    /*.o)
		# Simple output, SOURCES should be one single
		n=`echo $SOURCES | wc -w`;
		if [ $n -gt 1 ]; then
		    echo "cc.sh:Error, multiple sources, one object output.";
		    exit 1;
		else
		    output_filename=`cygpath -m $OUTFILE`;
		fi;;
	    *.o)
		# Relative path needs no translation
		n=`echo $SOURCES | wc -w`
		if [ $n -gt 1 ]; then
		    echo "cc.sh:Error, multiple sources, one object output."
		    exit 1
		else
		    output_filename=$OUTFILE
		fi;;
	    /*)
		# Absolute directory
		o=`echo $x | sed 's,.*/,,' | sed 's,\.c$,.o,'`
		output_filename=`cygpath -m $OUTFILE`
		output_filename="$output_filename/${o}";;
	    *)
		# Relative_directory or empty string (.//x.o is valid)
		o=`echo $x | sed 's,.*/,,' | sed 's,\.c$,.o,'`
		output_filename="./${OUTFILE}/${o}";;
	esac
    else
	# We are linking, which means we build objects in a temporary 
	# directory and link from there. We should retain the basename
	# of each source to make examining the exe easier...
	o=`echo $x | sed 's,.*/,,' | sed 's,\.c$,.o,'`
	output_filename=$TMPOBJDIR/$o
	ACCUM_OBJECTS="$ACCUM_OBJECTS $output_filename"
    fi
    # Now we know enough, lets try a compilation...
    MPATH=`cygpath -m $x`
    if [ $DEPENDING = true ]; then
	output_flag="-MM"
    elif [ $PREPROCESSING = true ]; then
	output_flag="-E"
    else
	output_flag="-c -o `cygpath -m ${output_filename}`"
    fi
    params="$COMMON_CFLAGS $DEBUG_FLAGS $OPTIMIZE_FLAGS \
	    $CMD ${output_flag} $MPATH"
    if [ "X$CC_SH_DEBUG_LOG" != "X" ]; then
	echo cc.sh "$SAVE" >>$CC_SH_DEBUG_LOG
	echo $MINGW_EXE_PATH/gcc $params >>$CC_SH_DEBUG_LOG
    fi
    eval $MINGW_EXE_PATH/gcc $params >$MSG_FILE 2>$ERR_FILE
    RES=$?
    if [ $PREPROCESSING = false -a $DEPENDING = false ]; then
	cat $ERR_FILE >&2
	cat $MSG_FILE
    else
	cat $ERR_FILE >&2
	if [ $DEPENDING = true ]; then
	    cat $MSG_FILE | sed 's|\([a-z]\):/|/cygdrive/\1/|g'
	else
	    cat $MSG_FILE
	fi
    fi
    rm -f $ERR_FILE $MSG_FILE
    if [ $RES != 0 ]; then
	rm -rf $TMPOBJDIR
	exit $RES
    fi
done

#If we got here, we succeeded in compiling (if there were anything to compile)
#The output filename should name an executable if we're linking
if [ $LINKING = true ]; then
    case $OUTFILE in
	"")
	    # Use the first source name to name the executable
	    first_source=""
	    for x in $SOURCES; do first_source=$x; break; done;
	    if [ -n "$first_source" ]; then
		e=`echo $x | sed 's,.*/,,' | sed 's,\.c$,.exe,'`;
		out_spec="-o $e";
	    else
		out_spec="";
	    fi;;
	*)
	    out_spec="-o $OUTFILE";;
    esac
    # Descide which standard library to link against
    case $MD in
	-ML)
	    stdlib="-lLIBC";;
	-MLd)
	    stdlib="-lLIBCD";;
	-MD)
	    stdlib="-lMSVCRT";;
	-MDd)
	    stdlib="-lMSVCRTD";;
	-MT)
	    stdlib="-lLIBCMT";;
	-MTd)
	    stdlib="-lLIBMTD";;
    esac
    # And finally call the next script to do the linking...
    params="$out_spec $LINKCMD $stdlib" 
    eval ld.sh $ACCUM_OBJECTS $params
    RES=$?
fi
rm -rf $TMPOBJDIR

exit $RES

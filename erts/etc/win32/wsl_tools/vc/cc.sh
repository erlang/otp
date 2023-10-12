#! /bin/sh
#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2002-2022. All Rights Reserved.
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
COMMON_CFLAGS="-nologo -D__WIN32__ -DWIN32 -DWINDOWS -D_WIN32 -DNT -D_CRT_SECURE_NO_DEPRECATE"

# Variables
# The stdout and stderr for the compiler
MSG_FILE=/tmp/cl.exe.$$.1
ERR_FILE=/tmp/cl.exe.$$.2

# "Booleans" determined during "command line parsing"
# If the stdlib option is explicitly passed to this program
MD_FORCED=false
# If we're preprocession (only) i.e. -E
PREPROCESSING=false
# If we're generating dependencies (implies preprocessing)
DEPENDENCIES=false
# If we're generating fast dependencies
FAST_DEPENDENCIES=false
# If this is supposed to be a debug build
DEBUG_BUILD=false
# If this is supposed to be an optimized build (there can only be one...)
OPTIMIZED_BUILD=false
# If we're linking or only compiling
LINKING=true

# This data is accumulated during command line "parsing"
# The stdlibrary option, default multithreaded dynamic
MD=-MD
# Flags for debug compilation
DEBUG_FLAGS=""
# Flags for optimization
OPTIMIZE_FLAGS=""
# The specified output filename (if any), may be either object or exe.
OUTFILE=""
# Unspecified command line options for the compiler
CMD=""
# All the c source files, in unix style
SOURCES=""
# All the options to pass to the linker, kept in Unix style
LINKCMD=""


# Loop through the parameters and set the above variables accordingly
# Also convert some filenames to "windows style"
# except for anything passed to the linker, that script
# handles those and the sources, which are also kept unixish for now

# If we are in "unix" directory we can't use relative paths
# since cl.exe can't find that path
WINCHECK=`w32_path.sh -m $PWD`
case $WINCHECK in
    //wsl$/*)
        USEABSPATH=true
        ;;
    *)
        USEABSPATH=false
        ;;
esac

while test -n "$1" ; do
    x="$1"
    case "$x" in
	-Wall)
	    ;;
	-c)
	    LINKING=false;;
	    #CMD="$CMD -c";;
	-MM)
	    LINKING=false;
	    FAST_DEPENDENCIES=true;;
	-MS)
	    PREPROCESSING=true;
	    LINKING=false;
	    DEPENDENCIES=true;;
	-E)
	    PREPROCESSING=true;
	    LINKING=false;; # Obviously...
	    #CMD="$CMD -E";;
	-Owx)
	    # Optimization hardcoded of wxErlang
	    OPTIMIZE_FLAGS="-Ob2ity -Gs -Z7";
	    DEBUG_FLAGS="";
	    DEBUG_BUILD=false;
	    if [ $MD_FORCED = false ]; then
		MD=-MD;
	    fi
	    OPTIMIZED_BUILD=true;;
	-O*)
	    # Optimization hardcoded
	    OPTIMIZE_FLAGS="-Ox -Z7";
	    DEBUG_FLAGS="";
	    DEBUG_BUILD=false;
	    if [ $MD_FORCED = false ]; then
		MD=-MD;
	    fi
	    OPTIMIZED_BUILD=true;;
	-g|-ggdb)
	    if [ $OPTIMIZED_BUILD = false ];then
		# Hardcoded windows debug flags
		DEBUG_FLAGS="-Z7";
		if [ $MD_FORCED = false ]; then
		    MD=-MDd;
		fi
		LINKCMD="$LINKCMD -g";
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
	    MPATH=`w32_path.sh -d "$y"`;
	    CMD="$CMD -$z\"$MPATH\"";;
	-I\"/*)
	    y=`echo $x | sed 's,^\"-[Io]\(/.*\)\",\1,g'`;
	    z=`echo $x | sed 's,^\"-\([Io]\)\(/.*\)\",\1,g'`;
	    MPATH=`w32_path.sh -d "$y"`;
	    CMD="$CMD -$z\"$MPATH\"";;
	-I*)
            if [ $USEABSPATH = true ]; then
                y=`echo $x | sed 's,^-[Io]\(.*\),\1,g'`;
	        z=`echo $x | sed 's,^-\([Io]\)\(.*\),\1,g'`;
                MPATH=`w32_path.sh -a -d "$y"`;
                CMD="$CMD -$z$MPATH";
            else
                y=`echo $x | sed 's,",\\\",g'`;
	        CMD="$CMD $y"
            fi;;
	-D*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD $y";;
	-EH*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD $y";;
	-TP|-Tp)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD $y";;
	-l*)
	    y=`echo $x | sed 's,^-l\(.*\),\1,g'`;
	    LINKCMD="$LINKCMD $x";;
        -std=c++*)
            CMD="$CMD /std:c++17 /Zc:__cplusplus";;
	/*.c)
	    SOURCES="$SOURCES $x";;
	*.c)
	    SOURCES="$SOURCES $x";;
	/*.cc)
	    SOURCES="$SOURCES $x";;
	*.cc)
	    SOURCES="$SOURCES $x";;
	/*.cpp)
	    SOURCES="$SOURCES $x"
            CMD="$CMD /EHsc"
            ;;
	*.cpp)
	    SOURCES="$SOURCES $x"
            CMD="$CMD /EHsc"
            ;;
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
TMPOBJDIR=$ERL_TOP/tmpobj$$
mkdir $TMPOBJDIR

WINTMPDIR=`w32_path.sh -w $TMPOBJDIR`

# Sometimes the file server doesn't keep up (parallel file creation)
while true ; do
    DIR_EXISTS=$(cd /mnt/c; cmd.exe /C "IF EXIST $WINTMPDIR (echo yes) ELSE (echo no)")
    case $DIR_EXISTS in   # Contains trash in the end of string
         yes*)
              break
              ;;
         *)
             if [ "X$CC_SH_DEBUG_LOG" != "X" ]; then
                 echo "sleep 1" >> $CC_SH_DEBUG_LOG
             fi;
             echo sleep $WINTMPDIR does not exist >&2
             sleep 1
    esac
done

## This is a faster dependency calculation method that
## only invokes cl.exe once for all files.
if [ $FAST_DEPENDENCIES = true ]; then
    MPATH=
    for x in $SOURCES; do
	if [ $USEABSPATH = true ]; then
	    MPATH="$MPATH $(w32_path.sh -a -d $x)"
	else
	    MPATH="$MPATH $(w32_path.sh -d $x)"
	fi
    done
    params="$COMMON_CFLAGS $MD $DEBUG_FLAGS $OPTIMIZE_FLAGS \
	    $CMD /showIncludes '-Fo`w32_path.sh -a -d ${TMPOBJDIR}`\\' $MPATH"
    if [ "X$CC_SH_DEBUG_LOG" != "X" ]; then
	echo cc.sh "$SAVE" >>$CC_SH_DEBUG_LOG
	echo cl.exe $params >>$CC_SH_DEBUG_LOG
    fi
    eval cl.exe $params > $MSG_FILE 2> $ERR_FILE
    RES=$?
    tail -n +2 $ERR_FILE | grep -v LNK1561 >&2
    if grep "LNK1561" $ERR_FILE; then
	rm -f $ERR_FILE $MSG_FILE
	rm -rf $TMPOBJDIR
	exit $RES
    fi
    perl -e '
my $deps = "";
my @sources = split(" ",$ARGV[1]);

open(FH, "<", $ARGV[0]) or die $!;

sub flushDeps() {
    if ($deps ne "") {
        ## To start a new shell can be very slow so we run all
        ## the wslpath translations in a single run
        my @paths = split("\n",`for x in $deps; do wslpath -u "\$x"; done`);
        foreach (@paths) {
            print "\\\n $_";
        }
        print "\n\n";
    }
}

while (<FH>) {
    $_ =~ tr/\r\n/\n/d;
    if (/^(.*)\.(c|cpp)$/) {
        my $target = "$1";
        flushDeps();
        $deps = shift(@sources) . " ";
        print $target.".o:";
    }
    if (/^Note: including file:\s*(.*)$/) {
        ## Ignore any deps that comes from "Program Files"
        next if /Program Files/;
        my $file = $1;
        $file =~ s/\\/\//g;
        $deps .= "'"'"'$file'"'"' ";
    }
}
flushDeps();
' $MSG_FILE "$MPATH"

    rm -f $ERR_FILE $MSG_FILE
else
    # Compile
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
		        output_filename=`echo $OUTFILE`;
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
		    output_filename=`echo $OUTFILE`
		    output_filename="$output_filename/${o}";;
	        *)
		    # Relative_directory or empty string (.//x.o is valid)
		    o=`echo $x | sed 's,.*/,,' | sed 's,\.cp*$,.o,'`
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
        if [ $USEABSPATH = true ]; then
            MPATH=`w32_path.sh -a -d $x`
        else
            MPATH=`w32_path.sh -d $x`
        fi
        if [ $PREPROCESSING = true ]; then
	    output_flag="-E"
        else
	    output_flag="/FS -c -Fo`w32_path.sh -a -d ${output_filename}`"
        fi
        params="$COMMON_CFLAGS $MD $DEBUG_FLAGS $OPTIMIZE_FLAGS \
	    $CMD ${output_flag} $MPATH"
        if [ "X$CC_SH_DEBUG_LOG" != "X" ]; then
	    echo cc.sh "$SAVE" >>$CC_SH_DEBUG_LOG
	    echo cl.exe $params >>$CC_SH_DEBUG_LOG
        fi
        eval cl.exe $params >$MSG_FILE 2>$ERR_FILE
        RES=$?
        if test $PREPROCESSING = false; then
	    cat $ERR_FILE >&2
	    tail -n +2 $MSG_FILE
        else
	    tail -n +2 $ERR_FILE >&2
	    if test $DEPENDENCIES = true; then
	        perl -e '
my $file = "'$x'";
while (<>) {
      next unless /^#line/;
      next if /$file/o;
      (undef,$_) = split(/\"/);
      next if / /;
      $all{$_} = 1;
}
foreach (sort keys %all) {
      my $w_file;
      ($w_file) = split("\n",`(w32_path.sh -u $_)`);
      push @f, "\\\n $w_file ";
}
if (@f) {
     my $oname = $file;
     $oname =~ s@.*/@@;
     $oname =~ s@[.]cp*@.o@;
     print $oname, ":", @f;
     print "\n\n";
     print STDERR "Made dependencies for $file\n";
}' $MSG_FILE
	    else
	        cat $MSG_FILE
	    fi
        fi
        rm -f $ERR_FILE $MSG_FILE
        if [ $RES != 0 ]; then
	    echo Failed: cl.exe $params
	    rm -rf $TMPOBJDIR
	    exit $RES
        fi
    done
fi
# If we got here, we succeeded in compiling (if there were anything to compile)
# The output filename should name an executable if we're linking
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
    # Decide which standard library to link against
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
    if [ "X$CC_SH_DEBUG_LOG" != "X" ]; then
	echo ld.sh $ACCUM_OBJECTS $params >>$CC_SH_DEBUG_LOG
    fi
    eval ld.sh $ACCUM_OBJECTS $params
    RES=$?
fi
rm -rf $TMPOBJDIR

exit $RES

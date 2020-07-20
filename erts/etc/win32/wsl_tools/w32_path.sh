#!/bin/bash

WIN32=false
SEPARATOR=""
ABSOLUTE=""
UNIX=false
done=false
while [ $done = false ]; do
    case "$1" in
	-w)
	    WIN32=true;
	    SEPARATOR=backslash;
	    shift;;
	-d)
	    WIN32=true;
	    SEPARATOR=double;
	    shift;;
	-m)
	    WIN32=true;
	    SEPARATOR=slash;
	    shift;;
	-u)
	    UNIX=true;
	    shift;;
	-a)
	    ABSOLUTE="-a";
	    shift;;

	*)
	    done=true;;
    esac
done

if [ $WIN32 = false -a $UNIX = false ]; then
    echo "Usage: $0 -m|-w|-d|-u [-a] <path>" >&2
    exit 1;
fi

if [ -z "$1" ]; then
    echo "Usage: $0 -m|-w|-u [-a] <path>" >&2
    exit 1;
fi

case "$1" in
    /*)
        rel_input=false
        ;;
    *)
        rel_input=true
        ;;
esac

if [ $UNIX = true ]; then
    # cl.exe loses //// in the beginning which make dependencies fail
    # and sometimes lowercases the path
    case $1 in
        \\*wsl$\\*)
            y=`echo $1 | sed 's,\\\\\+,/,g'`;
            z=`echo $y | sed 's,^/wsl$/[^/]*\(.*\),\1,g' | sed 's, ,\\ ,g'`;
            echo "$z";
            ;;
        *)
            echo `wslpath -u $ABSOLUTE "$1" | sed 's, ,\\ ,g'`
            ;;
    esac
else
    #  wslpath have changed to always return absolute paths and
    #  ensure the file/dir exists before translation

    if [ $rel_input = true -a "$ABSOLUTE" = "" ]; then
        case "$SEPARATOR" in
	    slash)
                echo $1
                ;;
            backslash)
                echo "$1" | sed 's,/,\\,g'
                ;;
            double)
                echo "$1" | sed 's,/,\\\\,g'
                ;;
        esac
        exit 0
    fi

    # absolute input and/or absolute output

    if [ -d "$1" ]; then
        dir=$1;
        case "$SEPARATOR" in
	    slash)
	        echo `wslpath -m $ABSOLUTE "$dir"`;
	        ;;
	    backslash)
	        echo `wslpath -w $ABSOLUTE "$dir"`;
	        ;;
	    double)
	        DOUBLE=`wslpath -w $ABSOLUTE "$dir" | sed 's,\\\\,\\\\\\\\,g'`;
	        echo $DOUBLE
	        ;;
        esac
        exit 0
    else
        dir=`dirname $1`
        file=`basename $1`

        case "$SEPARATOR" in
	    slash)
	        echo `wslpath -m $ABSOLUTE "$dir"`/$file;
	        ;;
	    backslash)
	        echo `wslpath -w $ABSOLUTE "$dir"`\\$file;
	        ;;
	    double)
	        DOUBLE=`wslpath -w $ABSOLUTE "$dir" | sed 's,\\\\,\\\\\\\\,g'`;
	        echo $DOUBLE\\\\$file
	        ;;
        esac
    fi
fi

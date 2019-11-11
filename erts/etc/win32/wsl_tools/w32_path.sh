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
    case "$SEPARATOR" in
	slash)
	    echo `wslpath -m $ABSOLUTE "$1"`;
	    ;;
	backslash)
	    echo `wslpath -w $ABSOLUTE "$1"`;
	    ;;
	double)
	    DOUBLE=`wslpath -w $ABSOLUTE "$1" | sed 's,\\\\,\\\\\\\\,g'`;
	    echo $DOUBLE
	    ;;
    esac
fi

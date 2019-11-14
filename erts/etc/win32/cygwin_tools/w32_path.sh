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
    echo `cygpath $ABSOLUTE -u $1`
else
    case "$SEPARATOR" in
	slash)
	    echo `cygpath $ABSOLUTE -m $1`;
	    ;;
	backslash)
	    echo `cygpath $ABSOLUTE -w $1`;
	    ;;
	double)
	    DOUBLE=`cygpath $ABSOLUTE -w $1 | sed 's,\\\\,\\\\\\\\,g'`;
	    echo $DOUBLE
	    ;;
    esac
fi

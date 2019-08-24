#! /bin/bash
MIXED=false
ABSOLUTE=false
done=false
while [ $done = false ]; do
    case "$1" in
	-m) 
	    MIXED=true;
	    shift;;
	-a)
	    ABSOLUTE=true;
	    shift;;
	*)
	    done=true;;
    esac
done
if [ -z "$1" ]; then
    echo "Usage: $0 <path>" >&2
    exit 1;
fi
MSYS_PATH=`win2msys_path.sh "$1"` # Clean up spaces
if [ $ABSOLUTE = true ]; then
    MSYS_DIR=`dirname "$MSYS_PATH"`
    MSYS_FILE=`basename "$MSYS_PATH"`
    if [ X"$MSYS_FILE" = X".." ]; then
	MSYS_DIR="$MSYS_DIR/$MSYS_FILE"
	WIN_ADD=""
    else
	WIN_ADD="/$MSYS_FILE"
    fi
    SAVEDIR=`pwd`
    cd $MSYS_DIR
    CURRENT=`pwd`
    cd $SAVEDIR
    WINPATH=`cmd //C echo $CURRENT`
    WINPATH="$WINPATH$WIN_ADD"
else
    WINPATH=`cmd //c echo $MSYS_PATH`
fi
if [ $MIXED = true ]; then
    echo $WINPATH
else
    echo $WINPATH | sed 's,/,\\,g'
fi
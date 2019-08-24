#! /bin/bash
if [ -z "$1" ]; then
    echo "Usage: $0 <path>" >&2
    exit 1;
fi

MSYS_PATH=`echo "$1" | sed 's,^\([a-zA-Z]\):\\\\,/\L\1/,;s,\\\\,/,g'`
if [ -z "$MSYS_PATH" ]; then
    echo "$0: Could not translate $1 to msys format" >&2
    exit 2;
fi

DELBLANK=`echo "$MSYS_PATH" | sed 's, ,,g'`

if [ "X$DELBLANK" != "X$MSYS_PATH" ]; then
    if [ -d "$MSYS_PATH" ]; then
	C1=`(cd "$MSYS_PATH" && cmd //C "for %i in (".") do @echo %~fsi")`
	MSYS_PATH=`echo "$C1" | sed 's,^\([a-zA-Z]\):\\\\,/\L\1/,;s,\\\\,/,g'`
    else
	MSYS_DIR=`dirname "$MSYS_PATH"`
	MSYS_FILE=`basename "$MSYS_PATH"`
	if [ -d "$MSYS_DIR" ]; then
	    	C1=`(cd "$MSYS_DIR" && cmd //C "for %i in (".") do @echo %~fsi")`	
		BAT_FILE=/tmp/w$$.bat
		# I simply cannot get the quoting right for this, 
		# need an intermediate bat file
		cat > $BAT_FILE <<EOF
@echo off
for %%i in ("$MSYS_FILE") do @echo %%~snxi
EOF
		C2=`(cd "$MSYS_DIR" && cmd //C $BAT_FILE)`
		rm -f $BAT_FILE
		MSYS_PATH=`echo "$C1/$C2" | sed 's,^\([a-zA-Z]\):\\\\,/\L\1/,;s,\\\\,/,g;s," ", ,g'`
	fi
    fi
fi		
echo $MSYS_PATH
exit 0
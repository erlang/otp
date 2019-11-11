#! /bin/sh

mkdir -p $ERL_TOP/tmp
BAT_FILE=$ERL_TOP/tmp/w$$.bat
if [ -z "$1" -o -z "$2" ]; then
    echo "Usage:" "$0" '<key> <valuename>'
    exit 1
fi
BACKED=`echo "$1" | sed 's,/,\\\\,g'`

if [ $CONFIG_SUBTYPE = "win64" ]; then
    REG_OPT=" /reg:64"
else
    REG_OPT=" /reg:32"
fi

WIN_BAT_FILE=`w32_path.sh -w $BAT_FILE`
RESULT=`reg.exe query "$BACKED" /v "$2" $REG_OPT | sed 's@\\\@/@g' | tr -d '\r\n'`
echo "$RESULT" | sed "s,.*REG_[^ ]* *,,g"

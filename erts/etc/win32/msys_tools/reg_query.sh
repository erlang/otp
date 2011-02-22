#! /bin/sh
BAT_FILE=/tmp/w$$.bat
if [ -z "$1" -o -z "$2" ]; then
    echo "Usage:" "$0" '<key> <valuename>'
    exit 1
fi
BACKED=`echo "$1" | sed 's,/,\\\\,g'`
# We need to get the 64bit part of the registry, hence we need to execute 
# a 64bit reg.exe, but c:\windows\system32 is redirected to 32bit versions
# if we ate in the 32bit virtual environment, why we need to use the 
# sysnative trick to get to the 64bit executable of reg.exe (ouch!)
if [ -d $WINDIR/sysnative ]; then
    REG_CMD="$WINDIR\\sysnative\\reg.exe"
else
    REG_CMD="reg"
fi
cat > $BAT_FILE <<EOF
@echo off
$REG_CMD query "$BACKED" /v "$2"
EOF
#echo $BAT_FILE
#cat $BAT_FILE
RESULT=`cmd //C $BAT_FILE`
echo $RESULT | sed "s,.*$2 REG_[^ ]* ,,"

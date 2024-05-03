#!/bin/sh

APP_VSNS=$(head -1 ${ERL_TOP}/otp_versions.table | awk -F: '{print $2}' | sed 's:#::g')

FIXUP="-e s:lib/\\.\\./::g"

if [ $(basename $(dirname "$1")) = "system" ]; then
    ADJUST_PATH="../"
    FIXUP="${FIXUP} -e s:\\.\\./\\.\\./\\.\\./doc:\\.\\.:g"
fi

FIXUP="${FIXUP} -e s:${ADJUST_PATH}system/doc/html:doc/system:g"

for APP_VSN in ${APP_VSNS}; do
    APP=$(echo ${APP_VSN} | awk -F- '{print $1}')
    if [ $APP = "erts" ]; then
        FIXUP="${FIXUP} -e s:${ADJUST_PATH}${APP}/doc/html/:${APP_VSN}/doc/html/:g"
    else
        FIXUP="${FIXUP} -e s:${ADJUST_PATH}lib/${APP}/doc/html/:lib/${APP_VSN}/doc/html/:g"
    fi
done

sed $FIXUP -i -- "$@"

#!/bin/sh

# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2025. All Rights Reserved.
#
# %CopyrightEnd%

case :"${1:?}" in
    :start)
        /bin/echo -ne "${4}\r\n\r\n" | ncat -l ${2} ${3} & echo $! > dummy_proxy.pid
        ;;
    :stop)
        Pid=`pgrep -F dummy_proxy.pid`
        if [ ! "$Pid" = "" ]; then
            kill "$Pid" || true
        fi
        ;;
esac

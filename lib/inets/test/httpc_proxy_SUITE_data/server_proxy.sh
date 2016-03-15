#! /bin/sh
##
## Command file to handle external webserver and proxy
## apache2 and tinyproxy.
##
## %CopyrightBegin%
##
## Copyright Ericsson AB 2012-2016. All Rights Reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
## %CopyrightEnd%
##
## Author: Raimo Niskanen, Erlang/OTP
#

PATH=/usr/local/bin:/usr/local/sbin:/bin:/usr/bin:/sbin:/usr/sbin
SHELL=/bin/sh
unset CDPATH ENV BASH_ENV
IFS='
	 '

APACHE_MODS_AVAILABLE_DIR="/etc/apache2/mods-available"
MODS="authz_host.load mime.conf mime.load ssl.conf ssl.load"

APACHE_HTTP_PORT=8080
APACHE_HTTPS_PORT=8443
APACHE_SERVER_NAME=localhost
export APACHE_HTTP_PORT APACHE_HTTPS_PORT APACHE_SERVER_NAME

PROXY_SERVER_NAME=localhost
PROXY_PORT=8000
export PROXY_SERVER_NAME PROXY_PORT

# All stdout goes to the calling erlang port, therefore
# these helpers push all side info to stderr.
status () { echo "$@"; }
info () { echo "$@" 1>&2; }
die () { REASON="$?"; status "$@"; exit "$REASON"; }
cmd () { "$@" 1>&2; }
silent () { "$@" 1>/dev/null 2>&1; }

wait_for_pidfile () {
    PIDFILE="${1:?Missing argument: PidFile}"
    for t in 1 1 1 2 2 3 3 3 4; do
	PID="`head -1 "$1" 2>/dev/null`" && [ :"$PID" != : ] && break
	sleep $t
    done
    [ :"$PID" = : ] && die ":ERROR:No or empty PidFile: $1"
    info "Started $PIDFILE[$PID]."
}

kill_and_wait () {
    PID_FILE="${1:?Missing argument: PidFile}"
    if [ -f "$PID_FILE" ]; then
	PID="`head -1 "$PID_FILE" 2>/dev/null`"
	[ :"$PID" = : ] && \
	    info "Empty Pid file: $1"
	info "Stopping $1 [$PID]..."
	shift
	case :"${1:?Missing argument: kill command}" in
	    :kill)
		[ :"$PID" = : ] || cmd kill "$PID";;
	    :*)
		cmd "$@";;
	esac
	wait "$PID"
	for t in 1 1 1 2; do
	    sleep $t
	    [ -e "$PID_FILE" ] || break
	done
	silent rm "$PID_FILE"
    else
	info "No pid file: $1"
    fi
}


PRIV_DIR="`pwd`"
DATA_DIR="`dirname "$0"`"
DATA_DIR="`cd "$DATA_DIR" && pwd`"

silent type apache2ctl || \
    die ":SKIP: Can not find apache2ctl."
silent type tinyproxy || \
    die ":SKIP: Can not find tinyproxy."

[ -d "$APACHE_MODS_AVAILABLE_DIR" ] || \
    die ":SKIP:Can not locate modules dir $APACHE_MODS_AVAILABLE_DIR."

silent mkdir apache2 tinyproxy
cd apache2 || \
    die ":ERROR:Can not cd to apache2"
CWD="`pwd`"
(cd ../tinyproxy) || \
    die ":ERROR:Can not cd to ../tinyproxy"

unset APACHE_HTTPD APACHE_LYNX APACHE_STATUSURL

## apache2ctl envvars variables
APACHE_CONFDIR="$DATA_DIR/apache2"
[ -f "$APACHE_CONFDIR"/apache2.conf ] || \
    die ":SKIP:No config file: $APACHE_CONFDIR/apache2.conf."
APACHE_RUN_USER=`id | sed 's/^uid=[0-9]\{1,\}(\([^)]*\)).*/\1/'`
APACHE_RUN_GROUP=`id | sed 's/.*[ 	]gid=[0-9]\{1,\}(\([^)]*\)).*/\1/'`
APACHE_RUN_DIR="$CWD/run"
APACHE_PID_FILE="$APACHE_RUN_DIR/pid"
APACHE_LOCK_DIR="$CWD/lock"
APACHE_LOG_DIR="$CWD/log"
export APACHE_CONFDIR APACHE_RUN_USER APACHE_RUN_GROUP
export APACHE_RUN_DIR APACHE_PID_FILE
export APACHE_LOCK_DIR APACHE_LOG_DIR
silent cmd mkdir "$APACHE_CONFDIR"
silent cmd mkdir "$APACHE_RUN_DIR" "$APACHE_LOCK_DIR" "$APACHE_LOG_DIR"

## Our apache2.conf additional variables
APACHE_MODS_DIR="$CWD/mods"
APACHE_DOCROOT="$APACHE_CONFDIR/htdocs"
APACHE_CERTS_DIR="$PRIV_DIR"
export APACHE_MODS_DIR APACHE_DOCROOT APACHE_CERTS_DIR
[ -d "$APACHE_MODS_DIR" ] || {
    cmd mkdir "$APACHE_MODS_DIR"
    for MOD in $MODS; do
	cmd ln -s "$APACHE_MODS_AVAILABLE_DIR/$MOD" "$APACHE_MODS_DIR" || {
	    die ":ERROR:ln of apache 2 module $MOD failed"
	}
    done
}

case :"${1:?}" in

    :start)
	info "Starting apache2..."
	cmd apache2ctl start
	[ $? = 0 ] || \
	    die ":ERROR: apache2 did not start."
	wait_for_pidfile "$APACHE_PID_FILE"
	
	info "Starting tinyproxy..."
	cmd cd ../tinyproxy || \
	    die ":ERROR:Can not cd to `pwd`/../tinyproxy"
	cat >tinyproxy.conf <<EOF
Port $PROXY_PORT

Listen 127.0.0.1
BindSame yes
Timeout 600

DefaultErrorFile "default.html"
Logfile "tinyproxy.log"
PidFile "tinyproxy.pid"

MaxClients 100
MinSpareServers 2
MaxSpareServers 8
StartServers 2
MaxRequestsPerChild 0

ViaProxyName "tinyproxy"

ConnectPort $APACHE_HTTPS_PORT
# to test connect error
ConnectPort $APACHE_HTTP_PORT
EOF
	(tinyproxy -d -c tinyproxy.conf 1>/dev/null 2>&1 </dev/null &)&
	wait_for_pidfile tinyproxy.pid
	
	status ":STARTED:$PROXY_SERVER_NAME:$PROXY_PORT|\
$APACHE_SERVER_NAME:$APACHE_HTTP_PORT:$APACHE_HTTPS_PORT"
	exit 0
	;;

    :stop)
	kill_and_wait ../tinyproxy/tinyproxy.pid kill
	kill_and_wait "$APACHE_PID_FILE" apache2ctl stop

	status ":STOPPED:"
	exit 0
	;;

    :apache2ctl)
	shift
	cmd apache2ctl ${1+"$@"}
	exit
	;;

    :*)
	(exit 1); die ":ERROR: I do not know of command '$1'."
	;;

esac

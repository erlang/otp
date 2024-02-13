#!/bin/sh

# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2024. All Rights Reserved.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 
# %CopyrightEnd%
#

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
LIGHT_CYAN='\033[1;36m'
BOLD='\033[1m'
NC='\033[0m'


print_highlighted_msg_with_printer () {
    COLOR=$1
    MSG_PRINTER=$2
    printf "\n${COLOR}======================================================================${NC}\n"
    echo
    $MSG_PRINTER
    echo
    printf "${COLOR}======================================================================${NC}\n"
}

print_highlighted_msg () {
    COLOR=$1
    MSG=$2
    print_msg () {
        echo "$MSG"
    }
    print_highlighted_msg_with_printer $COLOR print_msg
}

print_all_tests_takes_long_time_warning () {
    print_msg () {
        cat << EOM

WARNING

All tests will require several hours to run. You may want to check the
following text file that describes how to run tests for a specific
application.

  $ERL_TOP/HOWTO/TESTING.md

You can follow the test results here:

  file://$ERL_TOP/release/tests/test_server/index.html

EOM
    }
    print_highlighted_msg_with_printer $YELLOW print_msg
}

print_all_tests_for_application_notes () {
    print_msg () {
        cat << EOM

NOTE 1

ct_run will now attempt to execute tests in the test directory, which
may take a long time to do. One can pass arguments to ct_run by
setting the ARGS variable when invoking "make test".

Example:

make ARGS="-suite asn1_SUITE -case ticket_7407" test

NOTE 2

You may want to look at the more established way of running tests that
is described in the following text file if you encounter strange
errors:

EOM
        echo "$ERL_TOP/HOWTO/TESTING.md"
    }
    print_highlighted_msg_with_printer $LIGHT_CYAN print_msg
}

print_on_error_note () {
    print_msg () {
        cat << EOM
NOTE:

You may want to check "$ERL_TOP/HOWTO/TESTING.md" text file if you
encounter strange errors. Note also that you can rerun a specific test
case by passing parameters to ct_run as in the example below:

make ARGS="-suite asn1_SUITE -case ticket_7407" test

EOM
    }
    print_highlighted_msg_with_printer $NC print_msg
}

release_erlang () {
    local RELEASE_ROOT="${1}"
    if ! (cd $ERL_TOP && make release TYPE= release_docs DOC_TARGETS=chunks RELEASE_ROOT="${RELEASE_ROOT}"); then
        return 1
    fi
    if ! (cd "$RELEASE_ROOT" && ./Install -minimal "`pwd`"); then
        return 1
    fi
    ## Need to release both TYPE= and TYPE=$TYPE for tests to work
    if [ "$TYPE" != "" ]; then
        if ! (cd $ERL_TOP && make release TYPE=$TYPE RELEASE_ROOT="${RELEASE_ROOT}"); then
            return 1
        fi
    fi
    export PATH="${RELEASE_ROOT}/bin:$PATH"
    return 0
}

# Check ERL_TOP

if [ -d "$1" ]
then
    ERL_TOP="$1"
    shift
fi

if [ -z $ERL_TOP ]
then
    ERL_TOP=`git rev-parse --show-toplevel`
    if [ $? = 0 ]
    then
        print_highlighted_msg $LIGHT_CYAN "The environment variable ERL_TOP has been set to the git root"
    else
        echo "The ERL_TOP environment variable need to be set before this script is executed."
        exit 1
    fi
fi

export ERL_TOP=$ERL_TOP

if [ -z "${ARGS}" ]
then
   ARGS="$@"
fi

# make test in root
DIR=`pwd`
if [ "$DIR" -ef "$ERL_TOP" ]
then
    TARGET_SYS=`$ERL_TOP/make/autoconf/config.guess`
    REL_DIR="$ERL_TOP/release/$TARGET_SYS"
    cd "$REL_DIR"
    ./Install -minimal "`pwd`"
    export PATH="$REL_DIR/bin:$PATH"
    cd "$ERL_TOP/release/tests/test_server"
    print_all_tests_takes_long_time_warning
    echo "The tests will start in a few seconds..."
    sleep 45
    cd "$ERL_TOP/release/tests/test_server"
    erl -noinput -eval "ts:install(),erlang:halt()"
    erl -noinput -eval "ts:run([all_tests,batch]),erlang:halt()"
    exit $?
fi

# check that there is a test directory
if [ ! -d test ]
then
    print_highlighted_msg $RED "This target only works in directories containing a test directory or\nin the root directory."
    exit 1
fi

APPLICATION="`basename $DIR`"

if [ "$APPLICATION" = "erts" ]; then
    APPLICATION="system"
fi

CT_RUN="$ERL_TOP/bin/ct_run"
PATH="${ERL_TOP}/bin/:${PATH}"
MAKE_TEST_DIR="`pwd`/make_test_dir"
MAKE_TEST_REL_DIR="$MAKE_TEST_DIR/${APPLICATION}_test"
MAKE_TEST_CT_LOGS="$MAKE_TEST_DIR/ct_logs"
RELEASE_TEST_SPEC_LOG="$MAKE_TEST_DIR/release_tests_spec_log"
INSTALL_TEST_LOG="$MAKE_TEST_DIR/install_tests_log"
COMPILE_TEST_LOG="$MAKE_TEST_DIR/compile_tests_log"
RELEASE_ROOT=${RELEASE_ROOT:-"${MAKE_TEST_DIR}/Erlang ∅⊤℞"}
RELEASE_LOG="$MAKE_TEST_DIR/release_tests_log"

cd test

mkdir -p "$MAKE_TEST_DIR"
mkdir -p "$MAKE_TEST_CT_LOGS"

# Check that we are running a released erlang when we have to
if [ "$TEST_NEEDS_RELEASE" = "true" ]; then
    MSG=$(cat <<EOF
This application needs to be tested using a released Erlang/OTP.
Erlang/OTP will now be released to ${RELEASE_ROOT}.
EOF
       )
    print_highlighted_msg $YELLOW "${MSG}"
    release_erlang "${RELEASE_ROOT}" > "${RELEASE_LOG}" 2>&1
    if [ $? != 0 ]
    then
        print_highlighted_msg $RED "\"make release RELEASE_ROOT=${RELEASE_ROOT}\" failed.\nSee ${RELEASE_LOG} for full logs"
        tail -30 "${RELEASE_LOG}"
        exit 1
    fi
    CT_RUN="${RELEASE_ROOT}/bin/ct_run"
    PATH="${RELEASE_ROOT}/bin/":${PATH}
fi

echo "The tests in test directory for $APPLICATION will be executed with ${CT_RUN}"
if [ -z "${ARGS}" ]
then
    if [ ! -d "$MAKE_TEST_DIR" ]
    then
        print_all_tests_for_application_notes
    fi
fi

make RELEASE_PATH=$MAKE_TEST_DIR release_tests_spec > $RELEASE_TEST_SPEC_LOG 2>&1

if [ $? != 0 ]
then
    cat $RELEASE_TEST_SPEC_LOG
    print_highlighted_msg $RED "\"make RELEASE_PATH="$MAKE_TEST_DIR" release_tests_spec\" failed."
    exit 1
fi
if [ -z "${ARGS}" ]
then
    if [ "${WSLcross}" != "true" ] ; then
        SPEC_FILE_POSTFIX="$MAKE_TEST_REL_DIR/${APPLICATION}_${SPEC_POSTFIX}.spec"
        SPEC_FILE="$MAKE_TEST_REL_DIR/$APPLICATION.spec"
    else
        SPEC_FILE_POSTFIX=`w32_path.sh -m "$MAKE_TEST_REL_DIR/${APPLICATION}_${SPEC_POSTFIX}.spec"`
        SPEC_FILE=`w32_path.sh -m "$MAKE_TEST_REL_DIR/$APPLICATION.spec"`
    fi
    if [ -f "$SPEC_FILE_POSTFIX" ]; then
        SPEC_FILE="$SPEC_FILE_POSTFIX"
   fi
    ARGS="-spec $SPEC_FILE"
fi

ARGS="${ARGS} ${EXTRA_ARGS}"

if ([ -n "${TYPE}" ] || [ -n "${FLAVOR}" ]) && [ "${WSLcross}" = "true" ]; then
    print_highlighted_msg $RED "Setting TYPE or FLAVOR is not implemented yet for WSL"
    exit 1;
fi

if [ -n "${TYPE}" ]; then
    ERL_AFLAGS="${ERL_AFLAGS} -emu_type ${TYPE}"
fi
if [ -n "${FLAVOR}" ]; then
    ERL_AFLAGS="${ERL_AFLAGS} -emu_flavor ${FLAVOR}"
fi

# Compile test server and configure
if [ ! -f "$ERL_TOP/lib/common_test/test_server/variables" ]; then
    cd "$ERL_TOP/lib/common_test/test_server"
    ( make && erl -noshell -eval "ts:install()." -s init stop )  > "$INSTALL_TEST_LOG" 2>&1
    if [ $? != 0 ]
    then
        cat "$INSTALL_TEST_LOG"
        print_highlighted_msg $RED "\"make && erl -eval 'ts:install()'\" in common_test/test_server failed."
        exit 1
    fi
fi

# Run ct_run
cd $MAKE_TEST_REL_DIR

erl -sname test -noshell -pa "$ERL_TOP/lib/common_test/test_server" \
    -eval "ts:compile_datadirs(\"$ERL_TOP/lib/common_test/test_server/variables\",\"*_SUITE_data\")."\
    -s init stop > "$COMPILE_TEST_LOG" 2>&1

if [ $? != 0 ]
then
    cat "$COMPILE_TEST_LOG"
    print_highlighted_msg $RED "\"erl -eval 'ts:compile_datadirs/2'\" failed."
    exit 1
fi

CT_NODENAME=${CT_NODENAME:-test_server}

if [ "${WSLcross}" != "true" ]
then
    if [ -n "${CTRUN_TIMEOUT}" ]; then
        CTRUN_TIMEOUT="timeout -s ABRT --foreground --preserve-status $((${CTRUN_TIMEOUT}+5))m timeout -s USR1 --foreground --preserve-status ${CTRUN_TIMEOUT}m"
    fi
    ERL_AFLAGS="${ERL_AFLAGS}" $CTRUN_TIMEOUT \
      "${CT_RUN}" -logdir $MAKE_TEST_CT_LOGS \
        -pa "$ERL_TOP/lib/common_test/test_server" \
        -config "$ERL_TOP/lib/common_test/test_server/ts.config" \
        -config "$ERL_TOP/lib/common_test/test_server/ts.unix.config" \
        -exit_status ignore_config \
        ${ARGS} \
        -erl_args \
        -env ERL_CRASH_DUMP "$MAKE_TEST_DIR/${APPLICATION}_erl_crash.dump" \
        -boot start_sasl \
        -sasl errlog_type error \
        -pz "$ERL_TOP/lib/common_test/test_server" \
        -pz "." \
        -ct_test_vars "{net_dir,\"\"}" \
        -noinput \
        -sname ${CT_NODENAME}\
        -rsh ssh \
        ${ERL_ARGS}
else
    WIN_MAKE_TEST_CT_LOGS=`w32_path.sh -m "$MAKE_TEST_CT_LOGS"`
    WIN_MAKE_TEST_DIR=`w32_path.sh -m "$MAKE_TEST_DIR"`
    WIN_ERL_TOP=`w32_path.sh -m "$ERL_TOP"`
    "$CT_RUN.exe" -logdir $WIN_MAKE_TEST_CT_LOGS\
        -pa "$WIN_ERL_TOP/lib/common_test/test_server"\
        -config "$WIN_ERL_TOP/lib/common_test/test_server/ts.config"\
        -config "$WIN_ERL_TOP/lib/common_test/test_server/ts.win32.config"\
        -exit_status ignore_config \
        ${ARGS}\
        -erl_args\
        -env ERL_CRASH_DUMP "$WIN_MAKE_TEST_DIR/${APPLICATION}_erl_crash.dump"\
        -boot start_sasl\
        -sasl errlog_type error\
        -pz "$WIN_ERL_TOP/lib/common_test/test_server"\
        -pz "."\
        -ct_test_vars "{net_dir,\"\"}"\
        -noinput\
        -sname ${CT_NODENAME}\
        -rsh ssh\
        ${ERL_ARGS}
fi

CT_RUN_STATUS=$?
if [ $CT_RUN_STATUS = "0" ]
then
    print_highlighted_msg $GREEN "The test(s) ran successfully (ct_run returned a success code)\nTest logs: file://$MAKE_TEST_CT_LOGS/index.html"
    exit 0
else
    print_on_error_note
    print_highlighted_msg $RED "ct_run returned the error code $CT_RUN_STATUS\nTest logs: file://$MAKE_TEST_CT_LOGS/index.html"
    exit $CT_RUN_STATUS
fi

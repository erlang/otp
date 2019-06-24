#!/bin/sh

# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2019. All Rights Reserved.
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

EOM
        echo $ERL_TOP/HOWTO/TESTING.md
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

print_c_files_warning () {
    print_msg () {
        cat << EOM

WARNING

The test directory contains .c files which means that some test cases
will probably not work correctly when run through "make test". The
text file at the following location describes how one can compile and
run all test cases:


EOM
        echo $ERL_TOP/HOWTO/TESTING.md
    }
    print_highlighted_msg_with_printer $YELLOW print_msg
}


print_on_error_note () {
    print_msg () {
        cat << EOM
NOTE:

Some test cases do not work correctly when run through "make test" as
they are designed to be run through the method that is described in
the "$ERL_TOP/HOWTO/TESTING.md" text file. You may want to check this
text file if you encounter strange errors. Note also that you can
rerun a specific test case by passing parameters to ct_run as in the
example below:

make ARGS="-suite asn1_SUITE -case ticket_7407" test

EOM
    }
    print_highlighted_msg_with_printer $NC print_msg
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
    TARGET_SYS=`$ERL_TOP/erts/autoconf/config.guess`
    REL_DIR="$ERL_TOP/release/$TARGET_SYS"
    cd "$REL_DIR"
    ./Install -minimal "`pwd`"
    export PATH="$REL_DIR/bin:$PATH"
    cd "$ERL_TOP/release/tests/test_server"
    print_all_tests_takes_long_time_warning
    echo "The tests will start in a few seconds..."
    sleep 45
    cd "$ERL_TOP/release/tests/test_server"
    erl -eval "ts:install(),erlang:halt()"
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
CT_RUN="$ERL_TOP/bin/ct_run"
MAKE_TEST_DIR="`pwd`/make_test_dir"
MAKE_TEST_REL_DIR="$MAKE_TEST_DIR/${APPLICATION}_test"
MAKE_TEST_CT_LOGS="$MAKE_TEST_DIR/ct_logs"
RELEASE_TEST_SPEC_LOG="$MAKE_TEST_CT_LOGS/release_tests_spec_log"

cd test
echo "The tests in test directory for $APPLICATION will be executed with ct_run"
if [ -z "${ARGS}" ]
then
    if [ ! -d "$MAKE_TEST_DIR" ]
    then
        print_all_tests_for_application_notes
    fi
    if find . -type f -name '*.c' | grep -q "."
    then
        print_c_files_warning
    fi
fi

mkdir -p "$MAKE_TEST_DIR"
mkdir -p "$MAKE_TEST_REL_DIR"
mkdir -p "$MAKE_TEST_CT_LOGS"
make RELSYSDIR=$MAKE_TEST_REL_DIR release_tests_spec > $RELEASE_TEST_SPEC_LOG 2>&1

if [ $? != 0 ]
then
    cat $RELEASE_TEST_SPEC_LOG
    print_highlighted_msg $RED "\"make RELSYSDIR="$MAKE_TEST_REL_DIR" release_tests_spec\" failed."
    exit 1
fi
SPEC_FLAG=""
SPEC_FILE=""
if [ -z "${ARGS}" ]
then
    SPEC_FLAG="-spec"
    SPEC_FILE="$MAKE_TEST_REL_DIR/$APPLICATION.spec"
    ARGS="$SPEC_FLAG $SPEC_FILE"
fi
# Compile test server
(cd "$ERL_TOP/lib/common_test/test_server" && make)
# Run ct_run
cd $MAKE_TEST_REL_DIR
$CT_RUN -logdir $MAKE_TEST_CT_LOGS\
        -pa "$ERL_TOP/lib/common_test/test_server"\
        ${ARGS}\
        -erl_args\
        -env "$PATH"\
        -env ERL_CRASH_DUMP "$MAKE_TEST_DIR/${APPLICATION}_erl_crash.dump"\
        -boot start_sasl\
        -sasl errlog_type error\
        -pz "$ERL_TOP/lib/common_test/test_server"\
        -pz "."\
        -ct_test_vars "{net_dir,\"\"}"\
        -noshell\
        -sname test_server\
        -rsh ssh\
        ${ERL_ARGS}
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

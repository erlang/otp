# 
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2011. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# %CopyrightEnd%

ifneq ($(ERL_TOP),)
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk
else
include $(DIAMETER_TOP)/make/target.mk
include $(DIAMETER_TOP)/make/$(TARGET)/rules.mk
endif

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(DIAMETER_VSN)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/diameter_test

ifeq ($(findstring win32,$(TARGET)),win32)

MAKEFILE_SRC = Makefile.win32.src

else

MAKEFILE_SRC = Makefile.src

endif

ifeq ($(TT_DIR),)
TT_DIR = /tmp
endif


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

EBIN = .

HRL_FILES = diameter_test_lib.hrl

ERL_FILES = $(MODULES:%=%.erl)

SOURCE = $(HRL_FILES) $(ERL_FILES) 

TARGET_FILES = $(MODULES:%=%.$(EMULATOR))

APP_CASES   = app appup

TRANSPORT_CASES = tcp

ALL_CASES   = \
	$(APP_CASES) \
	compiler conf sync session stats reg peer \
	$(TRANSPORT_CASES)


EMAKEFILE  = Emakefile
ifneq ($(ERL_TOP),)
MAKE_EMAKE = $(wildcard $(ERL_TOP)/make/make_emakefile)
else
MAKE_EMAKE = $(wildcard $(DIAMETER_TOP)/make/make_emakefile)
endif

ifeq ($(MAKE_EMAKE),)
BUILDTARGET   = $(TARGET_FILES)
RELTEST_FILES = $(TEST_SPEC_FILE) $(COVER_SPEC_FILE) $(SOURCE)
else
BUILDTARGET   = emakebuild
RELTEST_FILES = $(EMAKEFILE) $(TEST_SPEC_FILE) $(COVER_SPEC_FILE) $(SOURCE)
endif


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../src/app/diameter.mk 

ifeq ($(USE_DIAMETER_TEST_CODE),true)
ERL_COMPILE_FLAGS += -DDIAMETER_TEST_CODE=mona_lisa_spelar_doom
endif

ifeq ($(USE_DIAMETER_HIPE),true)
ERL_COMPILE_FLAGS += +native -DDIAMETER_hipe_special=true
endif

ifneq ($(ERL_TOP),)
ERL_COMPILE_FLAGS += \
	$(DIAMETER_ERL_COMPILE_FLAGS)       \
	-pa $(ERL_TOP)/lib/test_server/ebin \
	-I$(ERL_TOP)/lib/test_server/include
else
ERL_COMPILE_FLAGS += \
	$(DIAMETER_ERL_COMPILE_FLAGS)       \
	-pa $(TEST_SERVER_DIR)/ebin \
	-I$(TEST_SERVER_DIR)/include
endif

ERL_PATH = \
	-pa ../../$(APPLICATION)/ebin \
	-pa ../../et/ebin

ifndef SUITE
SUITE = diameter_SUITE
endif

ESTOP = -s init stop

ifeq ($(DONT_STOP),true)
MAYBE_ESTOP =
else
MAYBE_ESTOP = $(ESTOP)
endif

ETVIEW = -s et_viewer
ifeq ($(USE_ET_VIEWER),true)
MAYBE_ETVIEW =
else
MAYBE_ETVIEW = $(ETVIEW)
endif

ifeq ($(MERL),)
MERL = $(ERL)
endif

ARGS += -noshell

ifeq ($(DISABLE_TC_TIMEOUT),true)
ARGS += -diameter_test_timeout
endif


DIAMETER_TEST_SERVER = diameter_test_server


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(BUILDTARGET)

targets: $(TARGET_FILES)

.PHONY: emakebuild

emakebuild: $(EMAKEFILE)

$(EMAKEFILE): 
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make' | grep -v Warning > $(EMAKEFILE)
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) | grep -v Warning >> $(EMAKEFILE)

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

docs:

info:
	@echo "MAKE_EMAKE        = $(MAKE_EMAKE)"
	@echo "EMAKEFILE         = $(EMAKEFILE)"
	@echo "BUILDTARGET       = $(BUILDTARGET)"
	@echo ""
	@echo "ERL_COMPILE_FLAGS = $(ERL_COMPILE_FLAGS)"
	@echo "ERL  = $(ERL)"
	@echo "ERLC = $(ERLC)"
	@echo "MERL = $(MERL)"
	@echo ""
	@echo "ARGS = $(ARGS)"
	@echo ""
	@echo "HRL_FILES    = $(HRL_FILES)"
	@echo "ERL_FILES    = $(ERL_FILES)"
	@echo "TARGET_FILES = $(TARGET_FILES)"
	@echo ""

help:
	@echo ""
	@echo "This Makefile controls the test of the $(APPLICATION) application. "
	@echo ""
	@echo "There are two separate ways to perform the test of $(APPLICATION)."
	@echo ""
	@echo "  a) Run the official OTP test-server (which we do not describe here)"
	@echo ""
	@echo "  b) Run the test-server provided with this application. "
	@echo "     There are a number of targets to run the entire or parts"
	@echo "     of this applications ($(APPLICATION)) test-suite"
	@echo ""
	@echo "Targets:"
	@echo ""
	@echo "    help"
	@echo "      Print this info"
	@echo ""
	@echo "    info"
	@echo "      Prints various environment variables. "
	@echo "      May be useful when debugging the Makefile. "
	@echo ""
	@echo "    tests | debug | opt "
	@echo "      Compile all test-code. "
	@echo ""
	@echo "    clean "
	@echo "      Remove all targets. "
	@echo ""
	@echo "    test"
	@echo "      Run the entire $(APPLICATION) test-suite. "
	@echo ""
	@echo "    app"
	@echo "      Run the $(APPLICATION) application sub-test-suite. "
	@echo ""
	@echo "    appup"
	@echo "      Run the $(APPLICATION) application upgrade (appup) sub-test-suite. "
	@echo ""
	@echo "    compiler"
	@echo "      Run the $(APPLICATION) compiler sub-test-suite(s). "
	@echo ""
	@echo "    conf"
	@echo "      Run the $(APPLICATION) config sub-test-suite. "
	@echo "      Checks various aspects of the $(APPLICATION) configuration. "
	@echo ""
	@echo "    sync"
	@echo "      Run the $(APPLICATION) sync sub-test-suite. "
	@echo ""
	@echo "    session"
	@echo "      Run the $(APPLICATION) session sub-test-suite. "
	@echo ""
	@echo "    stats"
	@echo "      Run the $(APPLICATION) stats sub-test-suite. "
	@echo ""
	@echo "    reg"
	@echo "      Run the $(APPLICATION) reg sub-test-suite. "
	@echo ""
	@echo "    peer"
	@echo "      Run the $(APPLICATION) peer sub-test-suite"
	@echo ""
	@echo "    ptab"
	@echo "      Run the $(APPLICATION) persistent-table sub-test-suite"
	@echo ""
	@echo "    tcp"
	@echo "      Run the $(APPLICATION) tcp sub-test-suite"
	@echo ""
	@echo ""


# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

all: make
	@echo "make sure epmd is new"
	@epmd -kill > /dev/null
	@echo "Running all sub-suites separatelly"
	@for i in $(ALL_CASES); do \
            echo "SUITE: $$i"; \
            clearmake -V $$i > $$i.log; \
        done

aall: make
	@echo "make sure epmd is new"
	@epmd -kill > /dev/null
	@echo "Running all app sub-suites separatelly"
	@for i in $(APP_CASES); do \
            echo "SUITE: $$i"; \
            clearmake -V $$i > $$i.log; \
        done
	echo "done"

tall: make
	@echo "make sure epmd is new"
	@epmd -kill > /dev/null
	@echo "Running all transport sub-suites separatelly"
	@for i in $(TRANSPORT_CASES); do \
            echo "SUITE: $$i"; \
            clearmake -V $$i > $$i.log; \
        done

make: targets

test: make
	$(MERL) $(ARGS) -sname diameter_test $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t $(SUITE) \
            $(MAYBE_ESTOP)

utest: make
	$(MERL) $(ARGS) -sname diameter_utest $(ERL_PATH) \
            $(MAYBE_ETVIEW) \
            -s $(DIAMETER_TEST_SERVER) t $(SUITE) \
            $(ESTOP)

# ftest: make
# 	$(MERL) $(ARGS) -sname diameter_ftest $(ERL_PATH) \
#             -s diameter_filter \
#             -s $(DIAMETER_TEST_SERVER) t $(SUITE) \
#             $(ESTOP)
# 

##########################

# tickets: make
# 	$(MERL) $(ARGS) -sname diameter_tickets $(ERL_PATH) \
#             -s $(DIAMETER_TEST_SERVER) tickets $(SUITE) \
#             $(ESTOP)
# 

app: make
	$(MERL) $(ARGS) -sname diameter_app $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t diameter_app_test \
            $(ESTOP)

appup: make
	$(MERL) $(ARGS) -sname diameter_appup $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t diameter_appup_test \
            $(ESTOP)

compiler: make
	$(MERL) $(ARGS) -sname diameter_compiler $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t diameter_compiler_test \
            $(ESTOP)

conf: make
	$(MERL) $(ARGS) -sname diameter_config $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t diameter_config_test \
            $(ESTOP)

sync: make
	$(MERL) $(ARGS) -sname diameter_sync $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t diameter_sync_test \
            $(ESTOP)

session: make
	$(MERL) $(ARGS) -sname diameter_session $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t diameter_session_test \
            $(ESTOP)

stats: make
	$(MERL) $(ARGS) -sname diameter_stats $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t diameter_stats_test \
            $(ESTOP)

reg: make
	$(MERL) $(ARGS) -sname diameter_reg $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t diameter_reg_test \
            $(ESTOP)

peer: make
	$(MERL) $(ARGS) -sname diameter_peer $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t diameter_peer_test \
            $(ESTOP)

ptab: make
	$(MERL) $(ARGS) -sname diameter_persistent_table $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t diameter_persistent_table_test \
            $(ESTOP)

tcp: make
	$(MERL) $(ARGS) -sname diameter_tcp $(ERL_PATH) \
            -s $(DIAMETER_TEST_SERVER) t diameter_tcp_test \
            $(ESTOP)


node:
	$(MERL) -sname diameter $(ERL_PATH)


# ----------------------------------------------------
# Release Targets
# ---------------------------------------------------- 

ifneq ($(ERL_TOP),)
include $(ERL_TOP)/make/otp_release_targets.mk
else
include $(DIAMETER_TOP)/make/release_targets.mk
endif

release_spec: 

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR)  $(RELSYSDIR)
	$(INSTALL_DATA) $(RELTEST_FILES) $(RELSYSDIR)
#	$(INSTALL_DATA) $(TEST_SPEC_FILE) $(COVER_SPEC_FILE) \
#                        $(HRL_FILES) $(ERL_FILES) \
#                        $(RELSYSDIR)
#
	chmod -R u+w $(RELSYSDIR)


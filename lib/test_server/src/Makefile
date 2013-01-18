#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1996-2012. All Rights Reserved.
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
#

include $(ERL_TOP)/make/target.mk

# ----------------------------------------------------
# Configuration info.
# ----------------------------------------------------
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(TEST_SERVER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/test_server-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= test_server_ctrl \
	 test_server_gl \
	 test_server_io \
	 test_server_node \
	 test_server \
	 test_server_sup \
	 test_server_h \
	 erl2html2

TS_MODULES= \
	ts \
	ts_run \
	ts_lib \
	ts_make \
	ts_erl_config \
	ts_autoconf_win32 \
	ts_install \
	ts_install_cth \
	ts_benchmark

TARGET_MODULES= $(MODULES:%=$(EBIN)/%)
TS_TARGET_MODULES= $(TS_MODULES:%=$(EBIN)/%)

ERL_FILES= $(MODULES:=.erl)
TS_ERL_FILES= $(TS_MODULES:=.erl)
HRL_FILES = ../include/test_server.hrl ../include/test_server_line.hrl
INTERNAL_HRL_FILES = test_server_internal.hrl
TS_HRL_FILES= ts.hrl
C_FILES = 
AUTOCONF_FILES = configure.in conf_vars.in
PROGRAMS = configure config.sub config.guess install-sh 
CONFIG = ts.config ts.unix.config ts.win32.config

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) \
	       $(APP_TARGET) $(APPUP_TARGET)
TS_TARGET_FILES = $(TS_MODULES:%=$(EBIN)/%.$(EMULATOR))

TARGETS = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(PROGRAMS) \
	  $(APP_TARGET) $(APPUP_TARGET)
TS_TARGETS = $(TS_MODULES:%=$(EBIN)/%.$(EMULATOR))

APP_FILE= test_server.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= test_server.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -I../include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGETS) $(TS_TARGETS)

clean:
	rm -f $(TARGET_FILES) $(TS_TARGET_FILES)
	rm -f core

docs:

configure: configure.in
	autoconf configure.in > configure

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/include"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_tests_spec: opt
	$(INSTALL_DIR) "$(RELEASE_PATH)/test_server"
	$(INSTALL_DATA) $(ERL_FILES) $(TS_ERL_FILES) \
		$(HRL_FILES) $(INTERNAL_HRL_FILES) $(TS_HRL_FILES) \
		$(TS_TARGET_FILES) \
		$(AUTOCONF_FILES) $(C_FILES) $(CONFIG) \
		"$(RELEASE_PATH)/test_server"
	$(INSTALL_SCRIPT) $(PROGRAMS) "$(RELEASE_PATH)/test_server"

release_docs_spec:


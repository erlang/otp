#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2003-2018. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk

# ----------------------------------------------------
# Configuration info.
# ----------------------------------------------------
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COMMON_TEST_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/common_test-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	ct \
	ct_logs \
	ct_framework \
	ct_ftp \
	ct_ssh \
	ct_snmp \
	ct_gen_conn \
	ct_rpc \
	ct_run \
	ct_master \
	ct_telnet \
	ct_util \
	ct_cover \
	ct_testspec \
	ct_event \
	ct_master_event \
	ct_master_logs \
	ct_master_status \
	ct_repeat \
	ct_telnet_client \
	ct_make \
	unix_telnet \
	ct_config \
	ct_config_plain \
	ct_config_xml \
	ct_slave \
        ct_hooks\
        ct_hooks_lock\
	cth_log_redirect\
	cth_surefire \
	ct_netconfc \
	ct_conn_log_h \
	cth_conn_log \
	ct_groups \
	ct_property_test \
	ct_release_test \
	ct_default_gl \
	erl2html2 \
	test_server_ctrl \
	test_server_gl \
	test_server_io \
	test_server_node \
	test_server \
	test_server_sup


TARGET_MODULES= $(MODULES:%=$(EBIN)/%)
BEAM_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

ERL_FILES= $(MODULES:=.erl)
HRL_FILES = \
	ct_util.hrl \
	ct_netconfc.hrl
EXTERNAL_HRL_FILES = \
	../include/ct.hrl \
	../include/ct_event.hrl

EXTERNAL_INC_PATH = ../include

DTD_FILES = \
	mp.dtd

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -pa ../ebin -I../include -I $(ERL_TOP)/lib/snmp/include/ \
	-I../../xmerl/inc/ -I $(ERL_TOP)/lib/kernel/include -Werror

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
TARGET_FILES = \
	$(GEN_ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) \
	$(BEAM_FILES) \
	$(APP_TARGET) $(APPUP_TARGET)

APP_FILE= common_test.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= common_test.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

tests debug opt: $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES) $(GEN_FILES)
	rm -f core

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
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(EXTERNAL_HRL_FILES) "$(RELSYSDIR)/include"

release_tests_spec: opt
	$(INSTALL_DIR) $(RELEASE_PATH)/common_test_test
	$(INSTALL_DATA) $(ERL_FILES)  \
        $(HRL_FILES) $(TARGET_FILES) $(RELEASE_PATH)/common_test_test

release_docs_spec: docs

# Include dependencies -- list below added by Kostis Sagonas
$(EBIN)/cth_log_redirect.beam: ../../kernel/include/logger.hrl ../../kernel/src/logger_internal.hrl

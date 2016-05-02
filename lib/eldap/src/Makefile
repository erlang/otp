#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
#
include $(ERL_TOP)/make/target.mk

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/eldap-$(ELDAP_VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= eldap

ASN1_FILES = ELDAPv3.asn1
ASN1_HRL = $(EBIN)/$(ASN1_FILES:%.asn1=%.hrl)

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(ASN1_FILES:%.asn1=$(EBIN)/%.$(EMULATOR))

EXTERNAL_HRL_FILES = ../include/eldap.hrl

HRL_FILES = $(EXTERNAL_HRL_FILES) $(ASN1_HRL)

APPUP_FILE = eldap.appup
APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

APP_FILE = eldap.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -I../include -I../ebin -Werror

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

debug:
	@${MAKE} TYPE=debug opt

clean:
	rm -f $(TARGET_FILES) $(GEN_FILES) $(APP_TARGET) $(APPUP_TARGET)
	rm -f $(ASN1_FILES:%.asn1=$(EBIN)/%.*)
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(ELDAP_VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(ELDAP_VSN);' $< > $@

docs:

$(TARGET_FILES): $(HRL_FILES)

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
$(ASN1_HRL): ../asn1/$(ASN1_FILES)
	$(asn_verbose)$(ERLC) -o $(EBIN) +legacy_erlang_types -bber $(ERL_COMPILE_FLAGS) ../asn1/ELDAPv3.asn1

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(ASN1_HRL) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/asn1"
	$(INSTALL_DATA) ../asn1/$(ASN1_FILES) "$(RELSYSDIR)/asn1"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(EXTERNAL_HRL_FILES) "$(RELSYSDIR)/include"

release_docs_spec:



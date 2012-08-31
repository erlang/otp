#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2005-2012. All Rights Reserved.
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
#

include $(ERL_TOP)/make/target.mk
EBIN = ../../ebin
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk

VSN = $(INETS_VSN)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
BEHAVIOUR_MODULES= \
	tftp

MODULES = \
	tftp_binary \
	tftp_engine \
	tftp_file \
	tftp_lib \
	tftp_logger \
	tftp_sup 

HRL_FILES = tftp.hrl

ERL_FILES= \
	$(MODULES:%=%.erl) \
	$(BEHAVIOUR_MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

BEHAVIOUR_TARGET_FILES= $(BEHAVIOUR_MODULES:%=$(EBIN)/%.$(EMULATOR))

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../inets_app/inets.mk

ERL_COMPILE_FLAGS += \
	$(INETS_FLAGS) \
        $(INETS_ERL_COMPILE_FLAGS) \
	-I../../include \
	-I../inets_app


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TARGET_FILES): $(BEHAVIOUR_TARGET_FILES)

debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES) $(BEHAVIOUR_TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR)  "$(RELSYSDIR)/src"
	$(INSTALL_DIR)  "$(RELSYSDIR)/src/tftp"
	$(INSTALL_DATA) $(HRL_FILES) $(ERL_FILES) "$(RELSYSDIR)/src/tftp"
	$(INSTALL_DIR)  "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) $(BEHAVIOUR_TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

info:
	@echo "APPLICATION       = $(APPLICATION)"
	@echo "INETS_DEBUG       = $(INETS_DEBUG)"
	@echo "INETS_FLAGS       = $(INETS_FLAGS)"
	@echo "ERL_COMPILE_FLAGS = $(ERL_COMPILE_FLAGS)"

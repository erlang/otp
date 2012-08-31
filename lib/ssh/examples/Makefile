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
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(SSH_VSN)

# ----------------------------------------------------
# Release Macros
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/ssh-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------


MODULES = \
	ssh_sample_cli

ERL_FILES= $(MODULES:=.erl)

# TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
TARGET_FILES = $(MODULES:=.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -pa ../ebin -I ../src
EBIN = .

# ----------------------------------------------------
# Make Rules
# ----------------------------------------------------
debug opt: $(TARGET_FILES)

debug: ERLC_FLAGS += -Ddebug

clean:
	rm -f $(TARGET_FILES) *~ *.beam

docs:

# ----------------------------------------------------
# Release Targets
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/examples"
	$(INSTALL_DATA) $(ERL_FILES) "$(RELSYSDIR)/examples"

release_docs_spec:


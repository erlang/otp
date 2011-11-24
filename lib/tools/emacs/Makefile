# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(TOOLS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/tools-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

MAN_FILES= \
	tags.3

EMACS_FILES= \
	erlang-skels \
	erlang-skels-old \
	erlang_appwiz \
	erlang-start \
	erlang-eunit \
	erlang-flymake \
	erlang

README_FILES= README

EL_FILES = $(EMACS_FILES:%=%.el)

ELC_FILES = $(EMACS_FILES:%=%.elc) 

TEST_FILES = test.erl.indented test.erl.orig

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(EL_FILES)

clean:
	rm -f $(TARGET_FILES) $(ELC_FILES)
	rm -f errs core *~

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/emacs
	$(INSTALL_DATA) $(EL_FILES) $(README_FILES) $(TEST_FILES) \
		$(RELSYSDIR)/emacs

ifeq ($(DOCTYPE),pdf)
release_docs_spec:
else
ifeq ($(DOCTYPE),ps)
release_docs_spec:
else
release_docs_spec: docs
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(MAN_FILES) $(RELEASE_PATH)/man/man3
endif
endif

#
#  %CopyrightBegin%
#  
#  Copyright Ericsson AB 1996-2012. All Rights Reserved.
#  
#  The contents of this file are subject to the Erlang Public License,
#  Version 1.1, (the "License"); you may not use this file except in
#  compliance with the License. You should have received a copy of the
#  Erlang Public License along with this software. If not, it can be
#  retrieved online at http://www.erlang.org/.
#  
#  Software distributed under the License is distributed on an "AS IS"
#  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
#  the License for the specific language governing rights and limitations
#  under the License.
#  
# %CopyrightEnd%
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(APPMON_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/appmon-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	appmon \
	appmon_a \
	appmon_dg \
	appmon_place \
	appmon_txt \
	appmon_lb \
	process_info \
	appmon_web

#	appmon_info \  Moved to runtime tools where it belongs


HRL_FILES= appmon_dg.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= appmon.app
APPUP_FILE= appmon.appup

APP_SRC= $(APP_FILE).src
APPUP_SRC= $(APPUP_FILE).src

APP_TARGET= ../ebin/$(APP_FILE)
APPUP_TARGET= ../ebin/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += +warn_obsolete_guard

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:

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

release_docs_spec:


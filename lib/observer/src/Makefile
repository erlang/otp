#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2002-2012. All Rights Reserved.
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
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(OBSERVER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/observer-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

MODULES= \
	crashdump_viewer \
	crashdump_viewer_html \
	etop \
	etop_gui \
	etop_tr \
	etop_txt \
	observer \
	observer_app_wx \
	observer_lib \
	observer_wx \
	observer_perf_wx \
	observer_pro_wx \
	observer_procinfo \
	observer_sys_wx \
	observer_trace_wx \
	observer_traceoptions_wx \
	observer_tv_table \
	observer_tv_wx \
	ttb \
	ttb_et

HRL_FILES= \
	../include/etop.hrl
INTERNAL_HRL_FILES= \
	observer_tv.hrl \
	observer_defs.hrl \
	crashdump_viewer.hrl \
	etop_defs.hrl
ERL_FILES= $(MODULES:%=%.erl)
EXAMPLE_FILES= multitrace.erl

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

PRIVDIR= ../priv
WEBTOOLFILES= $(PRIVDIR)/crashdump_viewer.tool $(PRIVDIR)/erlang_observer.png
BINDIR= $(PRIVDIR)/bin
ifeq ($(findstring win32,$(TARGET)),win32)
WIN32_EXECUTABLES= $(BINDIR)/etop.bat $(BINDIR)/getop.bat $(BINDIR)/cdv.bat
else
WIN32_EXECUTABLES=
endif
EXECUTABLES= \
	$(BINDIR)/etop \
	$(BINDIR)/getop \
	$(BINDIR)/cdv \
	$(WIN32_EXECUTABLES)
CDVDIR= $(PRIVDIR)/crashdump_viewer
GIF_FILES= \
	$(CDVDIR)/collapsd.gif \
	$(CDVDIR)/exploded.gif

APP_FILE= observer.app

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= observer.appup

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += \
	-I../include \
	-I ../../et/include \
	-I ../../../libraries/et/include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
opt debug: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(TARGET_FILES): $(INTERNAL_HRL_FILES)

docs:

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples"
	$(INSTALL_DATA) $(EXAMPLE_FILES) "$(RELSYSDIR)/examples"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/include"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/bin"
	$(INSTALL_SCRIPT) $(EXECUTABLES) "$(RELSYSDIR)/priv/bin"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/crashdump_viewer"
	$(INSTALL_DATA) $(WEBTOOLFILES) "$(RELSYSDIR)/priv"
	$(INSTALL_DATA) $(GIF_FILES) "$(RELSYSDIR)/priv/crashdump_viewer"

release_docs_spec:








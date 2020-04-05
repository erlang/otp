#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

MODULES= \
	cover                \
	eprof                \
	fprof                \
	cprof                \
	lcnt                 \
	instrument           \
	make		     \
	tags		     \
	xref		     \
	xref_base	     \
	xref_compiler	     \
	xref_parser          \
	xref_reader	     \
	xref_scanner	     \
	xref_utils


HRL_FILES= \
	xref.hrl


ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

YRL_FILE = xref_parser.yrl

APP_FILE = tools.app
APPUP_FILE = tools.appup

APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

PRIVDIR = ../priv
CSS = $(PRIVDIR)/styles.css

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -Werror

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

clean:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)
	rm -f errs core *~

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
	$(INSTALL_DATA) $(ERL_FILES) $(YRL_FILE) $(HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) \
		"$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv"
	$(INSTALL_DATA) $(CSS) "$(RELSYSDIR)/priv"

release_docs_spec:

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
##
## Copyright Ericsson AB 2007-2025. All Rights Reserved.
## Copyright 2006 Richard Carlsson
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
## Alternatively, you may use this file under the terms of the GNU Lesser
## General Public License (the "LGPL") as published by the Free Software
## Foundation; either version 2.1, or (at your option) any later version.
## If you wish to allow use of your version of this file only under the
## terms of the LGPL, you should delete the provisions above and replace
## them with the notice and other provisions required by the LGPL; see
## <http://www.gnu.org/licenses/>. If you do not delete the provisions
## above, a recipient may use your version of this file under the terms of
## either the Apache License or the LGPL.
##
## %CopyrightEnd%
# Authors: Richard Carlsson, Bertil Karlsson
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(EUNIT_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/eunit-$(VSN)


#
# Common Macros
#

EBIN = ../ebin
INCLUDE=../include

ERL_COMPILE_FLAGS += -pa $(EBIN) -pa ../../stdlib/ebin -I$(INCLUDE) \
	 +warn_deprecated_catch +warn_unused_import -Werror

PARSE_TRANSFORM = eunit_autoexport.erl

BEHAVIOUR_SOURCES= \
	eunit_listener.erl

SOURCES= \
	eunit_striptests.erl \
	eunit.erl \
	eunit_tests.erl \
	eunit_server.erl \
	eunit_proc.erl \
	eunit_serial.erl \
	eunit_test.erl \
	eunit_lib.erl \
	eunit_data.erl \
	eunit_tty.erl \
	eunit_surefire.erl \

ERL_FILES=$(PARSE_TRANSFORM) $(BEHAVIOUR_SOURCES) $(SOURCES)

INCLUDE_FILES = eunit.hrl

INTERNAL_HRL_FILES= eunit_internal.hrl

PARSE_TRANSFORM_BIN = $(PARSE_TRANSFORM:%.erl=$(EBIN)/%.$(EMULATOR))

TARGET_FILES= $(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))

BEHAVIOUR_TARGET_FILES= $(BEHAVIOUR_SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))

OBJECTS= $(TARGET_FILES) $(BEHAVIOUR_TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

INCLUDE_DELIVERABLES = $(INCLUDE_FILES:%=$(INCLUDE)/%)

APP_FILE= eunit.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= eunit.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TARGET_FILES): $(BEHAVIOUR_TARGET_FILES)

$(TYPES): $(PARSE_TRANSFORM_BIN) $(OBJECTS)

docs:

all: $(OBJECTS)


clean:
	rm -f $(OBJECTS) $(PARSE_TRANSFORM_BIN)
	rm -f core *~

distclean: clean

info:
	@echo "MODULES: $(MODULES)"
	@echo "EBIN: $(EBIN)"
	@echo "EMULATOR: $(EMULATOR)"
	@echo "APP_TARGET: $(APP_TARGET)"
	@echo "TARGET_FILES: $(TARGET_FILES)"
	@echo "DOC_TARGET_FILES: $(DOC_TARGET_FILES)"
	@echo "DOCDIR/%html: $(DOCDIR)/%.html"

realclean: clean

$(OBJECTS): $(PARSE_TRANSFORM_BIN)

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
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(PARSE_TRANSFORM_BIN) $(OBJECTS) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(PARSE_TRANSFORM) $(SOURCES) $(BEHAVIOUR_SOURCES) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(INCLUDE_DELIVERABLES) "$(RELSYSDIR)/include"

release_docs_spec:

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------
EXTRA_DEP_DEPENDENCIES=$(PARSE_TRANSFORM_BIN)
include $(ERL_TOP)/make/dep.mk

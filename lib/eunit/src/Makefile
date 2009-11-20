#
# Copyright (C) 2008, Ericsson Telecommunications
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

ERL_COMPILE_FLAGS += -pa $(EBIN) -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import +warn_obsolete_guard

SOURCES= \
	eunit_autoexport.erl \
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
	eunit_listener.erl

INCLUDE_FILES = eunit.hrl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

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

debug opt: $(OBJECTS)

docs:

all: $(OBJECTS)


clean:
	rm -f $(OBJECTS)
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

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(OBJECTS) $(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(SOURCES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(INCLUDE_DELIVERABLES) $(RELSYSDIR)/include

release_docs_spec:


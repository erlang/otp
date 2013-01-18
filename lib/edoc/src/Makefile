#
# Copyright (C) 2004, Ericsson Telecommunications
# Author: Richard Carlsson, Bertil Karlsson 
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(EDOC_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/edoc-$(VSN)


#
# Common Macros
#

EBIN = ../ebin
XMERL = ../../xmerl
ERL_COMPILE_FLAGS += -pa $(XMERL) -I../include -I$(XMERL)/include +warn_unused_vars +nowarn_shadow_vars +warn_unused_import +warn_deprecated_guard

SOURCES= \
	edoc.erl edoc_data.erl edoc_doclet.erl edoc_extract.erl \
	edoc_layout.erl edoc_lib.erl edoc_macros.erl edoc_parser.erl \
	edoc_refs.erl edoc_report.erl edoc_run.erl edoc_scanner.erl \
	edoc_specs.erl edoc_tags.erl edoc_types.erl edoc_wiki.erl \
	otpsgml_layout.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

HRL_FILES = edoc.hrl edoc_types.hrl ../include/edoc_doclet.hrl

YRL_FILE = edoc_parser.yrl

APP_FILE= edoc.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= edoc.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(OBJECTS)

all: $(OBJECTS)

$(OBJECTS): $(HRL_FILES) $(XMERL)/include/xmerl.hrl

docs: 

clean:
	rm -f $(OBJECTS) edoc_parser.erl
	rm -f core *~

distclean: clean

realclean: clean

$(EBIN)/%.$(EMULATOR):%.erl
	$(erlc_verbose)erlc -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

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
	$(INSTALL_DATA) $(OBJECTS) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(SOURCES) $(HRL_FILES) $(YRL_FILE) "$(RELSYSDIR)/src"

release_docs_spec:


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

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/edoc-$(EDOC_VSN)


#
# Common Macros
#

GEN_SCRIPT = edoc_generate
PRIV_FILES = stylesheet.css erlang.png edoc.dtd

debug opt:

clean:
	rm -f core *~

docs:

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/priv"
	$(INSTALL_DATA) $(PRIV_FILES) "$(RELSYSDIR)/priv"
	$(INSTALL_SCRIPT) $(GEN_SCRIPT) "$(RELSYSDIR)/priv"


release_docs_spec:

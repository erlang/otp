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
include ../../xmerl/vsn.mk
include ../../syntax_tools/vsn.mk


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/edoc-$(EDOC_VSN)


#
# Common Macros
#

GEN_SCRIPT_SRC = edoc_generate.src
GEN_SCRIPT = edoc_generate
PRIV_FILES = stylesheet.css erlang.png edoc.dtd

debug opt: $(GEN_SCRIPT)

$(GEN_SCRIPT): ../vsn.mk ../../xmerl/vsn.mk ../../syntax_tools/vsn.mk \
		$(GEN_SCRIPT_SRC)
	$(vsn_verbose)sed -e "s/%EDOC_VSN%/$(EDOC_VSN)/g" \
	    -e "s/%XMERL_VSN%/$(XMERL_VSN)/g" \
	    -e "s/%SYNTAX_TOOLS_VSN%/$(SYNTAX_TOOLS_VSN)/g" \
		$(GEN_SCRIPT_SRC) > $(GEN_SCRIPT)

clean:
	rm -f $(GEN_SCRIPT)
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


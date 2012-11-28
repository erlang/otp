#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2003-2012. All Rights Reserved.
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
VSN=$(XMERL_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/xmerl-$(VSN)

# ----------------------------------------------------
# Documentation directory specification
# ----------------------------------------------------

DOCDIR = ../doc


### XMERL use EDOC for documentation, to regenerate update paths as needed!
XMERL_APP = ..

EDOC_APP = ../../edoc

SYNTAX_TOOLS_APP = ../../syntax_tools



# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

EDOC_MODULES = \
	xmerl_scan \
	xmerl \
	xmerl_xs \
	xmerl_eventp \
	xmerl_xpath \
	xmerl_xsd



MODULES = $(EDOC_MODULES) \
	xmerl_b64Bin \
	xmerl_b64Bin_scan \
	xmerl_html \
	xmerl_lib \
	xmerl_otpsgml \
	xmerl_regexp \
	xmerl_sgml \
	xmerl_simple \
	xmerl_text \
	xmerl_ucs \
	xmerl_uri \
	xmerl_validate \
	xmerl_xlate \
	xmerl_xml \
	xmerl_xpath_lib \
	xmerl_xpath_parse \
	xmerl_xpath_pred \
	xmerl_xpath_scan \
	xmerl_xsd_type \
	xmerl_sax_parser \
	xmerl_sax_parser_list \
	xmerl_sax_parser_latin1 \
	xmerl_sax_parser_utf8 \
	xmerl_sax_parser_utf16be \
	xmerl_sax_parser_utf16le \
	xmerl_sax_simple_dom \
	xmerl_sax_old_dom 



HRL_FILES = \
	../include/xmerl.hrl \
	../include/xmerl_xpath.hrl \
	../include/xmerl_xsd.hrl

INCLUDE_DIR = ../include

INTERNAL_HRL_FILES  = \
	xmerl_internal.hrl  \
	xmerl_sax_old_dom.hrl \
	xmerl_sax_parser.hrl

ERL_FILES= $(MODULES:%=%.erl)

APP_FILE= xmerl.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= xmerl.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

DOC_TARGET_FILES = $(EDOC_MODULES:%=$(DOCDIR)/%.html)
TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += \
		-I ../include \
		+warn_unused_vars 

#		+bin_opt_info 


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug opt: $(TARGET_FILES) 

docs:
#docs:	$(DOC_TARGET_FILES)


clean:
	$(RM) $(TARGET_FILES)
	$(RM) xmerl_xpath_parse.erl
	$(RM) xmerl_b64Bin.erl
	$(RM) xmerl_sax_parser_list.erl
	$(RM) xmerl_sax_parser_latin1.erl
	$(RM) xmerl_sax_parser_utf8.erl
	$(RM) xmerl_sax_parser_utf16be.erl
	$(RM) xmerl_sax_parser_utf16le.erl
	$(RM) core *~

info:
	@echo "MODULES: $(MODULES)"
	@echo "EBIN: $(EBIN)"
	@echo "EMULATOR: $(EMULATOR)"
	@echo "APP_TARGET: $(APP_TARGET)"
	@echo "TARGET_FILES: $(TARGET_FILES)"
	@echo "DOC_TARGET_FILES: $(DOC_TARGET_FILES)"
	@echo "DOCDIR/%html: $(DOCDIR)/%.html"

realclean: clean
	$(RM) $(DOC_TARGET_FILES)

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
EDOC_PATHS = \
	-pa $(EDOC_APP)/ebin -pa $(XMERL_APP)/ebin -pa $(SYNTAX_TOOLS_APP)/ebin

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

xmerl_xpath_parse.erl:	xmerl_xpath_parse.yrl
	$(yecc_verbose)$(ERLC)  -o  $(ESRC) $<

xmerl_b64Bin.erl: xmerl_b64Bin.yrl
	$(yecc_verbose)$(ERLC)  -o  $(ESRC) $<

xmerl_sax_parser_list.erl: xmerl_sax_parser_list.erlsrc xmerl_sax_parser_base.erlsrc
	$(gen_verbose)cat xmerl_sax_parser_list.erlsrc xmerl_sax_parser_base.erlsrc >$@

xmerl_sax_parser_latin1.erl: xmerl_sax_parser_latin1.erlsrc xmerl_sax_parser_base.erlsrc
	$(gen_verbose)cat xmerl_sax_parser_latin1.erlsrc xmerl_sax_parser_base.erlsrc >$@

xmerl_sax_parser_utf8.erl: xmerl_sax_parser_utf8.erlsrc xmerl_sax_parser_base.erlsrc
	$(gen_verbose)cat xmerl_sax_parser_utf8.erlsrc xmerl_sax_parser_base.erlsrc >$@

xmerl_sax_parser_utf16be.erl: xmerl_sax_parser_utf16be.erlsrc xmerl_sax_parser_base.erlsrc
	$(gen_verbose)cat xmerl_sax_parser_utf16be.erlsrc xmerl_sax_parser_base.erlsrc >$@

xmerl_sax_parser_utf16le.erl: xmerl_sax_parser_utf16le.erlsrc xmerl_sax_parser_base.erlsrc
	$(gen_verbose)cat xmerl_sax_parser_utf16le.erlsrc xmerl_sax_parser_base.erlsrc >$@

$(EBIN)/%.beam:	%.erl
	$(V_ERLC)  $(ERL_COMPILE_FLAGS) -o $(EBIN) $<

$(DOCDIR)/%.html:	%.erl
	$(ERL) -noshell $(EDOC_PATHS) \
		-run edoc_run file '"$<"' '[{dir,"$(DOCDIR)"}]' -s erlang halt


#$(DOCDIR)/%.html:	%.erl
#	$(ERL) $(EDOC_PATHS) -s edoc file $< ['{dir,"$(DOCDIR)"}'] 

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
#ifeq "${MA_TOP}" ""
#MA_TOP = ../../..
#endif
#include $(MA_TOP)/config/make.release
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(INTERNAL_HRL_FILES) $(APP_SRC) $(APPUP_SRC) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) xmerl_xpath_parse.yrl "$(RELSYSDIR)/src"
	$(INSTALL_DATA) xmerl_b64Bin.yrl "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/include"

release_docs_spec:


release_tests_spec:


#------------------------------------------------------------
# .hrl dependencies
$(EBIN)/xmerl.beam:../include/xmerl.hrl
$(EBIN)/xmerl_lib.beam:../include/xmerl.hrl
$(EBIN)/xmerl_scan.beam:../include/xmerl.hrl
$(EBIN)/xmerl_xlate.beam:../include/xmerl.hrl
$(EBIN)/xmerl_xml.beam:../include/xmerl.hrl
$(EBIN)/xmerl_html.beam:../include/xmerl.hrl
$(EBIN)/xmerl_text.beam:../include/xmerl.hrl
$(EBIN)/xmerl_eventp.beam:../include/xmerl.hrl
$(EBIN)/xmerl_simple.beam:../include/xmerl.hrl
$(EBIN)/xmerl_xpath.beam:../include/xmerl.hrl
$(EBIN)/xmerl_xpath_lib.beam:../include/xmerl.hrl
$(EBIN)/xmerl_xpath_pred.beam:../include/xmerl.hrl
$(EBIN)/xmerl_xpath_scan.beam:../include/xmerl.hrl
#$(EBIN)/xmerl_xsd.beam:../include/xmerl_xsd.hrl
#$(EBIN)/xmerl_xsd.beam:../include/xmerl.hrl



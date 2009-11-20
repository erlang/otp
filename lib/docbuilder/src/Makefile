# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999-2000, Ericsson 
# Utvecklings AB. All Rights Reserved.''
# 
#     $Id$
#

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(DOCB_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/docbuilder-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

MODULES= \
	docb_edoc_xml_cb \
	docb_gen \
	docb_html \
	docb_html_layout \
	docb_html_ref \
	docb_html_util \
	docb_html_util_iso \
	docb_main \
	docb_pretty_format \
	docb_tr_application2html \
	docb_tr_appref2html \
	docb_tr_chapter2html \
	docb_tr_cite2html \
	docb_tr_comref2html \
	docb_tr_cref2html \
	docb_tr_erlref2html \
	docb_tr_fileref2html \
	docb_tr_first2html \
	docb_tr_index2html \
	docb_tr_part2html \
	docb_tr_refs2kwic \
	docb_tr_report2html \
	docb_tr_term2html \
	docb_transform \
	docb_util \
	docb_xmerl_tree_cb \
	docb_xmerl_xml_cb \
	docb_xml_check

HRL_FILES= \
	docb_util.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

APP_FILE= docbuilder.app
APPUP_FILE= docbuilder.appup
APP_SRC= $(APP_FILE).src
APPUP_SRC= $(APPUP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS +=
XMERL = ../../xmerl
ERL_COMPILE_FLAGS += -I$(XMERL)/include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) 

clean:
	rm -f $(TARGET_FILES) $(APP_TARGET) 
	rm -f errs core *~

$(APP_TARGET):	$(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@
$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) $(RELSYSDIR)/ebin

release_docs_spec:






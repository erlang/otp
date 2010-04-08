#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2003-2010. All Rights Reserved.
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
include ../../vsn.mk
VSN=$(COMMON_TEST_VSN)
APPLICATION=common_test
# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

# REMEMBER: links to HTML files for these modules in ref_man.xml
CT_MODULES = \
	ct \
	ct_master \
	ct_cover \
	ct_telnet \
	ct_ftp \
	ct_ssh \
	ct_rpc \
	ct_snmp \
	unix_telnet \
	ct_slave

CT_XML_FILES = $(CT_MODULES:=.xml)

XML_APPLICATION_FILES = ref_man.xml
XML_REF1_FILES = run_test.xml
XML_REF3_FILES = $(CT_XML_FILES)
XML_REF6_FILES = common_test_app.xml

XML_PART_FILES = part.xml 
#	part_notes.xml \
#	part_notes_history.xml

XML_CHAPTER_FILES = \
	basics_chapter.xml \
	install_chapter.xml \
	write_test_chapter.xml \
	test_structure_chapter.xml \
	run_test_chapter.xml \
	example_chapter.xml \
	why_test_chapter.xml \
	config_file_chapter.xml \
	cover_chapter.xml \
	ct_master_chapter.xml \
	event_handler_chapter.xml \
	dependencies_chapter.xml \
	notes.xml \
	notes_history.xml 

MAKE_EDOC = make_edoc

BOOK_FILES = book.xml

GIF_FILES =

INSTALL_NOTES = ../../notes.html

XML_FILES=$(XML_APPLICATION_FILES) $(XML_REF1_FILES)  $(XML_REF3_FILES) $(XML_REF6_FILES) \
	$(XML_PART_FILES) $(XML_CHAPTER_FILES) $(XML_REF_FILES) $(BOOK_FILES)

# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

INFO_FILE = ../../info

MAN1_FILES = $(XML_REF1_FILES:%.xml=$(MAN1DIR)/%.1)
MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
XML_FLAGS += 
DVIPS_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

CT_SRC_DIR = $(ERL_TOP)/../internal_tools/common_test/src

$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@

docs: pdf html man

$(CT_XML_FILES): 
	escript $(DOCGEN)/priv/bin/xml_from_edoc.escript -preprocess true -i $(XMERL_DIR)/include \
		-i ../../../test_server/include -i  ../../include \
		-i ../../../../erts/lib/kernel/include -i ../../../../lib/kernel/include \
		-i ../../../../erts/lib/snmp/include -i ../../../../lib/snmp/include ../../src/$(@:%.xml=%.erl)

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE) 

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

man: $(MAN6_FILES) $(MAN3_FILES) $(MAN1_FILES)


debug opt: 

#
# checkout make.dep before generating new dependecies
#
#make_doc_depend: xml
#	docdepend > make.dep

clean clean_docs:
	rm -f $(CT_XML_FILES)
	rm -rf $(HTMLDIR)/*
	rm -f $(MAN1DIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(MAN6DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~ 

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_docs_spec: docs
	$(INSTALL_DIR) $(RELSYSDIR)/doc/pdf
	$(INSTALL_DATA) $(TOP_PDF_FILE) $(RELSYSDIR)/doc/pdf
	$(INSTALL_DIR) $(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(HTMLDIR)/* \
		$(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(INFO_FILE) $(RELSYSDIR)
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man1
	$(INSTALL_DATA) $(MAN1DIR)/* $(RELEASE_PATH)/man/man1
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(MAN3DIR)/* $(RELEASE_PATH)/man/man3
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man6
	$(INSTALL_DATA) $(MAN6DIR)/* $(RELEASE_PATH)/man/man6

release_spec:

release_tests_spec:

# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------

include make.dep
# DO NOT DELETE



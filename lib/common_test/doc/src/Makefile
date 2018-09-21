#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2003-2018. All Rights Reserved.
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

CT_XML_FILES = $(CT_MODULES:=.xml)

XML_APPLICATION_FILES = ref_man.xml
XML_REF1_FILES = ct_run.xml
# REMEMBER: links to HTML files for these modules in ref_man.xml
XML_REF3_FILES = ct.xml \
	ct_master.xml \
	ct_cover.xml \
	ct_telnet.xml \
	ct_ftp.xml \
	ct_ssh.xml \
	ct_rpc.xml \
	ct_snmp.xml \
	unix_telnet.xml \
	ct_slave.xml \
	ct_property_test.xml \
	ct_netconfc.xml \
	ct_hooks.xml \
        ct_testspec.xml
XML_REF6_FILES = common_test_app.xml

XML_PART_FILES = part.xml

XML_CHAPTER_FILES = \
	introduction.xml \
	basics_chapter.xml \
	getting_started_chapter.xml \
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
	ct_hooks_chapter.xml \
	dependencies_chapter.xml \
	notes.xml

BOOK_FILES = book.xml

GIF_FILES = \
	tc_execution.gif \
	config.gif \
	html_logs.gif

INSTALL_NOTES = ../../notes.html

XML_FILES=$(XML_APPLICATION_FILES) $(XML_REF1_FILES)  $(XML_REF3_FILES) $(XML_REF6_FILES) \
	$(XML_PART_FILES) $(XML_CHAPTER_FILES) $(BOOK_FILES)

# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

INFO_FILE = ../../info

MAN1_FILES = $(XML_REF1_FILES:%.xml=$(MAN1DIR)/%.1)
MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

SPECS_FILES = $(XML_REF3_FILES:%.xml=$(SPECDIR)/specs_%.xml)

TOP_SPECS_FILE = specs.xml

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
XML_FLAGS +=
DVIPS_FLAGS +=
SPECS_FLAGS = -I../../include -I../../../snmp/include \
	-I../../../kernel/include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@

docs: man pdf html

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

man: $(MAN6_FILES) $(MAN3_FILES) $(MAN1_FILES)


debug opt:

clean clean_docs:
	rm -rf $(HTMLDIR)/*
	rm -rf $(XMLDIR)
	rm -f $(MAN1DIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(MAN6DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f $(SPECDIR)/*
	rm -f errs core *~

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk


release_docs_spec: docs
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(HTMLDIR)/* \
		"$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man1"
	$(INSTALL_DATA) $(MAN1DIR)/* "$(RELEASE_PATH)/man/man1"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3DIR)/* "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man6"
	$(INSTALL_DATA) $(MAN6DIR)/* "$(RELEASE_PATH)/man/man6"

release_spec:

release_tests_spec:

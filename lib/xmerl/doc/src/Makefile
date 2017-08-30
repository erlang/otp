#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
VSN=$(XMERL_VSN)
APPLICATION=xmerl

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Help application directory specification
# ----------------------------------------------------

EDOC_DIR = $(ERL_TOP)/lib/edoc
SYNTAX_TOOLS_DIR = $(ERL_TOP)/lib/syntax_tools

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XMERL_DIR = $(ERL_TOP)/lib/$(APPLICATION)/src

XMERL_MODULES = \
	xmerl_scan \
	xmerl \
	xmerl_xs \
	xmerl_eventp \
	xmerl_xpath \
	xmerl_xsd


XML_APPLICATION_FILES = ref_man.xml
XMERL_XML_FILES = $(XMERL_MODULES:=.xml)

XML_REF3_FILES = $(XMERL_XML_FILES) \
	xmerl_sax_parser.xml

XML_PART_FILES =  \
	part.xml \
	part_notes.xml

XML_REF6_FILES =

XML_CHAPTER_FILES =  \
	xmerl_ug.xml \
	notes.xml


HTML_EXAMPLE_FILES =  \
	xmerl_examples.html \
	xmerl_xs_examples.html

HTML_STYLESHEET_FILES = \
	../stylesheet.css

BOOK_FILES = book.xml

XML_HTML_FILES = \
	notes_history.xml

EXAMPLE_FILES = people2.txt people.txt motorcycles.txt motorcycles_dtd.txt \
	new_motorcycles.txt new_motorcycles2.txt result_export.html \
	motorcycles2.txt result_xs.html motorcycles2html.erl

XML_FILES= \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) $(XML_APPLICATION_FILES)

# ----------------------------------------------------
INFO_FILE = ../../info

HTML_FILES = $(XML_REF_MAN:%.xml=$(HTMLDIR)/%.html) \
	$(XML_HTML_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)


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
$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@

docs: pdf html man

$(TOP_PDF_FILE): $(XML_FILES) 

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

$(XMERL_XML_FILES): 
	escript $(DOCGEN)/priv/bin/xml_from_edoc.escript $(XMERL_DIR)/$(@:%.xml=%.erl) 

man: $(MAN3_FILES) $(MAN6_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

xml: $(XMERL_XML_FILES)

debug opt: 

clean clean_docs:
	rm -rf $(HTMLDIR)/*
	rm -f $(MAN3DIR)/*	
	rm -f $(MAN6DIR)/*	
	rm -f $(XMERL_XML_FILES)
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~ 


info:
	@echo "XML_PART_FILES: $(XML_PART_FILES)"
	@echo "XML_APPLICATION_FILES: $(XML_APPLICATION_FILES)"
	@echo "XMERL_XML_FILES: $(XMERL_XML_FILES)"
	@echo "XMERL_MODULES: $(XMERL_MODULES)"
	@echo "HTML_FILES: $(HTML_FILES)"
	@echo "HTMLDIR: $(HTMLDIR)"
	@echo "DEFAULT_GIF_FILES: $(DEFAULT_GIF_FILES)"
	@echo "DEFAULT_HTML_FILES: $(DEFAULT_HTML_FILES)"

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
	$(INSTALL_DATA) $(EXAMPLE_FILES) $(HTML_EXAMPLE_FILES) $(HTML_STYLESHEET_FILES) "$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3DIR)/* "$(RELEASE_PATH)/man/man3"

release_spec:



release_tests_spec:




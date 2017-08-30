#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
VSN=$(PERCEPT_VSN)
APPLICATION=percept

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Help application directory specification
# ----------------------------------------------------

EDOC_DIR = $(ERL_TOP)/lib/edoc

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
PERCEPT_DIR = $(ERL_TOP)/lib/$(APPLICATION)/src
RUNTIME_TOOLS_DIR = $(ERL_TOP)/lib/runtime_tools/src

PERCEPT_MODULES = \
	egd\
	percept

RUNTIME_TOOLS_MODULES = \
	percept_profile

XML_APPLICATION_FILES = \
	ref_man.xml

PERCEPT_XML_FILES = $(PERCEPT_MODULES:=.xml) 

RUNTIME_TOOLS_XML_FILES = $(RUNTIME_TOOLS_MODULES:=.xml)

MODULE_XML_FILES = $(PERCEPT_XML_FILES) $(RUNTIME_TOOLS_XML_FILES)

XML_REF_MAN = \
	ref_man.xml

XML_REF3_FILES = $(MODULE_XML_FILES)

XML_PART_FILES = \
	part.xml \
	part_notes.xml

XML_REF6_FILES =

XML_CHAPTER_FILES = \
	notes.xml \
	egd_ug.xml \
	percept_ug.xml

GEN_XML = \
	egd_ug.xml \
	percept_ug.xml

BOOK_FILES = book.xml

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) $(XML_REF_MAN)

HTML_EXAMPLE_FILES = \
	percept_examples.html

HTML_STYLESHEET_FILES = \
	../stylesheet.css


GIF_FILES = \
	test1.gif \
	test2.gif \
	test3.gif \
	test4.gif \
	percept_overview.gif \
	percept_processes.gif \
	percept_processinfo.gif \
	percept_compare.gif \
	img_esi_result.gif 

# ----------------------------------------------------
INFO_FILE = ../../info

HTML_FILES = \
	$(XML_REF_MAN:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)


HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf


# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
XML_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@

docs: pdf html man

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

clean clean_docs:
	rm -f $(MODULE_XML_FILES) $(GEN_XML)
	rm -rf $(HTMLDIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~

man: $(MAN3_FILES) $(MAN6_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

xml: $(MODULE_XML_FILES)

$(PERCEPT_XML_FILES):
	escript $(DOCGEN)/priv/bin/xml_from_edoc.escript $(PERCEPT_DIR)/$(@:%.xml=%.erl)

$(RUNTIME_TOOLS_XML_FILES):
	escript $(DOCGEN)/priv/bin/xml_from_edoc.escript $(RUNTIME_TOOLS_DIR)/$(@:%.xml=%.erl)

info:
	@echo "XML_PART_FILES: $(XML_PART_FILES)"
	@echo "XML_APPLICATION_FILES: $(XML_APPLICATION_FILES)"
	@echo "PERCEPT_XML_FILES: $(MODULE_XML_FILES)"
	@echo "PERCEPT_MODULES: $(PERCEPT_MODULES)"
	@echo "HTML_FILES: $(HTML_FILES)"
	@echo "HTMLDIR: $(HTMLDIR)"


debug opt: 

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_docs_spec: docs
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(HTML_EXAMPLE_FILES) $(HTML_STYLESHEET_FILES) \
		$(HTMLDIR)/* \
		"$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3DIR)/* "$(RELEASE_PATH)/man/man3"

release_spec:


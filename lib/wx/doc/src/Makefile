#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
include ../../config.mk
APPLICATION=wx

ErlMods         = wx.erl wx_object.erl

GenMods         = $(shell ls ../../src/gen | grep 'wx[A-Z].*\.erl') \
		wx_misc.erl \
		glu.erl \
		gl.erl

ModsNoExt	= $(ErlMods:%.erl=%) $(GenMods:%.erl=%)

# Release directory specification

XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = $(ErlMods:%.erl=%.xml) $(GenMods:%.erl=%.xml)

XML_PART_FILES = part.xml part_notes.xml
XML_CHAPTER_FILES = chapter.xml
XML_NOTES_FILES = notes.xml

BOOK_FILES = book.xml

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) \
	$(XML_NOTES_FILES) $(XML_APPLICATION_FILES)

# ----------------------------------------------------
INFO_FILE = ../../info

HTML_FILES = \
	$(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

SPECS_FILES = $(XML_REF3_FILES:%.xml=$(SPECDIR)/specs_%.xml)

TOP_SPECS_FILE = specs.xml

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
XML_FLAGS +=
SPECS_FLAGS = -I../../include -I../../src
DVIPS_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

docs: pdf html man

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

man: $(MAN3_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

xml: $(XML_REF3_FILES) $(XML_CHAPTER_FILES)

ref_man.xml: ref_man.xml.src
	@echo Preparing ref_man.xml
	@cat ref_man.xml.src > ref_man.xml
	@for d in $(ModsNoExt); do         \
		echo "  <xi:include href=\"$$d.xml\"/>" >> ref_man.xml ; \
	done
	@echo "</application>" >> ref_man.xml
	@echo

$(ErlMods:%.erl=%.xml): ../../src/$(@:%.xml=%.erl)
	escript $(DOCGEN)/priv/bin/xml_from_edoc.escript -def vsn $(VSN) -preprocess true -sort_functions false ../../src/$(@:%.xml=%.erl)

$(GenMods:%.erl=%.xml): ../../src/gen/$(@:%.xml=%.erl)
	escript $(DOCGEN)/priv/bin/xml_from_edoc.escript -def vsn $(VSN) -i ../../src -preprocess true -sort_functions false ../../src/gen/$(@:%.xml=%.erl)

$(XML_CHAPTER_FILES): ../overview.edoc
	escript $(DOCGEN)/priv/bin/xml_from_edoc.escript -def vsn $(VSN) -chapter ../overview.edoc

debug opt:

clean clean_docs:
	rm -rf  $(HTMLDIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f $(SPECDIR)/*
	rm -f errs core *~ ../html/edoc-info
	rm -f $(XML_REF3_FILES) $(XML_CHAPTER_FILES) *.html

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk


release_docs_spec: docs
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(HTMLDIR)/*  \
		"$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3_FILES) "$(RELEASE_PATH)/man/man3"

release_spec:

release_tests_spec:

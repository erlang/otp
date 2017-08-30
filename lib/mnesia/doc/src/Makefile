#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(MNESIA_VSN)
APPLICATION=mnesia

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = \
	mnesia.xml \
	mnesia_frag_hash.xml \
	mnesia_registry.xml

XML_PART_FILES = \
	part.xml \
	part_notes.xml \
	part_notes_history.xml

XML_CHAPTER_FILES = \
	Mnesia_chap1.xml \
	Mnesia_chap2.xml \
	Mnesia_chap3.xml \
	Mnesia_chap4.xml \
	Mnesia_chap5.xml \
	Mnesia_chap7.xml \
	Mnesia_chap8.xml \
	Mnesia_App_A.xml \
	Mnesia_App_B.xml \
	Mnesia_App_C.xml \
	notes.xml

BOOK_FILES = book.xml


XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) $(XML_APPLICATION_FILES)

GIF_FILES = \
	book.gif \
	company.gif \
	mnesia.gif \
	note.gif \
	notes.gif \
	ref_man.gif \
	user_guide.gif \
	warning.gif

XML_HTML_FILES = \
	notes_history.xml


# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_HTML_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

INFO_FILE = ../../info
EXTRA_FILES = summary.html.src \
	$(DEFAULT_GIF_FILES) \
	$(DEFAULT_HTML_FILES) \
	$(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)

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

clean clean_docs:
	rm -rf $(HTMLDIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~

man: $(MAN3_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

$(INDEX_TARGET): $(INDEX_SRC) ../../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

debug opt: 

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
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3_FILES) "$(RELEASE_PATH)/man/man3"

release_spec:

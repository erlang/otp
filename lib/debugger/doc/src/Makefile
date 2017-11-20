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
VSN=$(DEBUGGER_VSN)
APPLICATION=debugger

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = debugger.xml i.xml int.xml

XML_PART_FILES = part.xml
XML_CHAPTER_FILES = introduction.xml debugger_chapter.xml notes.xml

BOOK_FILES = book.xml

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) $(XML_PART_FILES) \
	$(XML_REF3_FILES) $(XML_APPLICATION_FILES) 

GIF_FILES = \
	images/attach.jpg \
	images/cond_break_dialog.jpg \
	images/function_break_dialog.jpg \
	images/interpret.jpg \
	images/line_break_dialog.jpg \
	images/monitor.jpg \
	images/view.jpg

# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

INFO_FILE = ../../info

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
$(HTMLDIR)/%.jpg: %.jpg
	$(INSTALL_DIR) $(HTMLDIR)/images
	$(INSTALL_DATA) $< $@

docs: pdf html man

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

man: $(MAN3_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

clean clean_docs:
	rm -rf $(HTMLDIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~

debug opt: 

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_docs_spec: docs
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html"
	($(CP) -rf  $(HTMLDIR) "$(RELSYSDIR)/doc")
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3DIR)/* "$(RELEASE_PATH)/man/man3"

release_spec:

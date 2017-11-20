# ``Licensed under the Apache License, Version 2.0 (the "License");
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
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(ELDAP_VSN)
APPLICATION=eldap

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = eldap.xml

XML_PART_FILES = usersguide.xml
XML_CHAPTER_FILES = notes.xml

BOOK_FILES = book.xml

XML_FILES = $(BOOK_FILES) $(XML_APPLICATION_FILES) $(XML_REF3_FILES)  $(XML_REF6_FILES) \
            $(XML_PART_FILES) $(XML_CHAPTER_FILES) 

GIF_FILES =

# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

INFO_FILE = ../../info

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

man: $(MAN3_FILES) $(MAN6_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

debug opt valgrind: 

clean clean_docs clean_tex:
	rm -rf $(HTMLDIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(MAN6DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
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
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3DIR)/* "$(RELEASE_PATH)/man/man3"
#       $(INSTALL_DIR) "$(RELEASE_PATH)/man/man6"
#	$(INSTALL_DATA) $(MAN6DIR)/* "$(RELEASE_PATH)/man/man6"


release_spec:


# 
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2012. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

include ../../vsn.mk

VSN = $(DIAMETER_VSN)

RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
include files.mk

XML_REF_FILES = $(XML_REF1_FILES) $(XML_REF3_FILES) $(XML_REF4_FILES)

XML_FILES = $(BOOK_FILES) $(XML_APPLICATION_FILES) \
            $(XML_REF_FILES) \
            $(XML_PART_FILES) $(XML_CHAPTER_FILES) \
            seealso.ent

INTERNAL_HTML_FILES = $(TECHNICAL_DESCR_FILES:%.xml=$(HTMLDIR)/%.html)

HTML_APP_FILES   = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html)
HTML_EXTRA_FILES = $(XML_EXTRA_FILES:%.xml=$(HTMLDIR)/%.html)
HTML_PART_FILES  = $(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

HTML_FILES = $(HTML_APP_FILES) $(HTML_EXTRA_FILES) $(HTML_PART_FILES)

INFO_FILE = ../../info

HTML_REF_FILES = $(XML_REF_FILES:%.xml=$(HTMLDIR)/%.html)

HTML_CHAPTER_FILES = $(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)

EXTRA_FILES = \
	$(DEFAULT_GIF_FILES)  \
	$(DEFAULT_HTML_FILES) \
	$(HTML_REF_FILES)    \
	$(HTML_CHAPTER_FILES)

MAN1_FILES = $(XML_REF1_FILES:%.xml=$(MAN1DIR)/%.1)
MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN4_FILES = $(XML_REF4_FILES:%.xml=$(MAN4DIR)/%.4)

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

STANDARD_DIR = ../standard
STANDARDS = 

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

ldocs: local_docs $(INDEX_TARGET)

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

clean clean_docs: clean_pdf clean_html clean_man
	rm -f errs core *~
	rm -f depend.mk seehere.ent

clean_pdf:
	rm -f $(PDFDIR)/*

clean_man:
	rm -f $(MAN1DIR)/* $(MAN3DIR)/* $(MAN4DIR)/*

clean_html:
	rm -rf $(HTMLDIR)/*

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

man: $(MAN1_FILES) $(MAN3_FILES) $(MAN4_FILES)

$(INDEX_TARGET): $(INDEX_SRC) $(APP_FILE)
	sed -e 's/%VSN%/$(VSN)/; \
	        s/%ERLANG_SITE%/www\.erlang\.se\//; \
	        s/%UP_ONE_LEVEL%/..\/..\/..\/doc\/index.html/; \
	        s/%OFF_PRINT%/pdf\/diameter-$(VSN).pdf/' $< > $@

depend: depend.mk

debug opt:

info:
	@echo "->Makefile<-"
	@echo ""
	@echo "INDEX_FILE   = $(INDEX_FILE)"
	@echo "INDEX_SRC    = $(INDEX_SRC)"
	@echo "INDEX_TARGET = $(INDEX_TARGET)"
	@echo ""
	@echo "XML_APPLICATION_FILES = $(XML_APPLICATION_FILES)"
	@echo "XML_PART_FILES        = $(XML_PART_FILES)"
	@echo "XML_REF1_FILES        = $(XML_REF1_FILES)"
	@echo "XML_REF3_FILES        = $(XML_REF3_FILES)"
	@echo "XML_REF4_FILES        = $(XML_REF4_FILES)"
	@echo "XML_CHAPTER_FILES     = $(XML_CHAPTER_FILES)"
	@echo ""
	@echo "GIF_FILES             = $(GIF_FILES)"
	@echo ""
	@echo "MAN1_FILES            = $(MAN1_FILES)"
	@echo "MAN3_FILES            = $(MAN3_FILES)"
	@echo "MAN4_FILES            = $(MAN4_FILES)"
	@echo ""
	@echo "HTML_FILES            = $(HTML_FILES)"
	@echo "TOP_HTML_FILES        = $(TOP_HTML_FILES)"
	@echo ""
	@echo "DEFAULT_HTML_FILES    = $(DEFAULT_HTML_FILES)"
	@echo "DEFAULT_GIF_FILES     = $(DEFAULT_GIF_FILES)"
	@echo ""
	@echo ""


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 

include $(ERL_TOP)/make/otp_release_targets.mk

release_docs_spec: $(LOCAL)docs
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man1"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man4"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(HTMLDIR)/*.* "$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(MAN1_FILES) "$(RELEASE_PATH)/man/man1"
	$(INSTALL_DATA) $(MAN3_FILES) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN4_FILES) "$(RELEASE_PATH)/man/man4"
	[ -z "$(LOCAL)" ] || cp -r $(HTMLDIR)/js "$(RELSYSDIR)/doc/html"
	echo $(LOCAL)

release_spec:

depend.mk: depend.sed Makefile seealso.ent \
           $(XML_REF_FILES) $(XML_CHAPTER_FILES)
	$(gen_verbose)sed -f seehere.sed seealso.ent > seehere.ent
	$(V_at)(for f in $(XML_REF_FILES) $(XML_CHAPTER_FILES); do \
	     sed -f $< $$f | sed "s@%FILE%@`basename $$f .xml`@g"; \
	 done) \
	> $@

-include depend.mk

.PHONY: clean clean_html clean_man clean_pdf \
        depend debug opt info \
        docs gifs html ldocs man pdf \
        release_docs_spec release_spec

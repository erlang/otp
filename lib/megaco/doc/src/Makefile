# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2000-2009. All Rights Reserved.
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

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(MEGACO_VSN)
APPLICATION=megaco

# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------

ifndef DOCSUPPORT
include make.dep
endif

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include files.mk


# ----------------------------------------------------

INTERNAL_HTML_FILES = $(TECHNICAL_DESCR_FILES:%.xml=$(HTMLDIR)/%.html)

HTML_APP_FILES   = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html)
HTML_EXTRA_FILES = $(XML_EXTRA_FILES:%.xml=$(HTMLDIR)/%.html)
HTML_PART_FILES  = $(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

HTML_FILES = $(HTML_APP_FILES) $(HTML_EXTRA_FILES) $(HTML_PART_FILES)

INFO_FILE = ../../info

HTML_REF3_FILES    = $(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html)
HTML_CHAPTER_FILES = $(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)

EXTRA_FILES = \
	$(DEFAULT_GIF_FILES)  \
	$(DEFAULT_HTML_FILES) \
	$(HTML_REF3_FILES)    \
	$(HTML_CHAPTER_FILES)

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)

ifdef DOCSUPPORT

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

else

TEX_FILES_BOOK = \
	$(BOOK_FILES:%.xml=%.tex)
TEX_FILES_REF_MAN = $(XML_REF3_FILES:%.xml=%.tex) \
	$(XML_APPLICATION_FILES:%.xml=%.tex)
TEX_FILES_USERS_GUIDE = \
	$(XML_CHAPTER_FILES:%.xml=%.tex)

TOP_PDF_FILE = $(APPLICATION)-$(VSN).pdf
TOP_PS_FILE  = $(APPLICATION)-$(VSN).ps

$(TOP_PDF_FILE): book.dvi ../../vsn.mk
	$(DVI2PS) $(DVIPS_FLAGS) -f $< | $(DISTILL) $(DISTILL_FLAGS) > $@

$(TOP_PS_FILE): book.dvi $(APP_FILE)
	$(DVI2PS) $(DVIPS_FLAGS) -f $< > $@


TOP_HTML_FILES = $(INDEX_TARGET) 

endif

INDEX_FILE   = index.html
INDEX_SRC    = $(INDEX_FILE).src
INDEX_TARGET = $(DOCDIR)/$(INDEX_FILE)

STANDARD_DIR = ../standard
STANDARDS = $(STANDARD_DIR)/rfc3015.txt \
            $(STANDARD_DIR)/implementors_guide_v6.pdf

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

ifdef DOCSUPPORT

docs: pdf html man

ldocs: local_docs $(INDEX_TARGET)

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

clean clean_docs: clean_html clean_man
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~

else

ifeq ($(DOCTYPE),pdf)
docs: pdf
else
ifeq ($(DOCTYPE),ps)
docs: ps
else
docs: html gifs man
endif
endif

pdf: $(TOP_PDF_FILE)

ps: $(TOP_PS_FILE)

html: gifs $(HTML_FILES) $(TOP_HTML_FILES)

mhtml: html $(HTML_REF3_FILES) $(HTML_CHAPTER_FILES)

clean: clean_html clean_man clean_pdf
	rm -f core *~ 
	rm -f *.aux *.cites *.citeshd *.dvi *.idx *.ilg *.ind
	rm -f *.indhd *.lof *.lofhd *.lot *.lothd *.otpdef
	rm -f *.otpuse *.terms *.termshd *.toc *.makeindexlog *.dvipslog
	rm -f *.bib *.bbl *.blg *.bibhd

clean_pdf:
	rm -f $(TOP_PDF_FILE) $(TOP_PS_FILE)
	rm -f $(TEX_FILES_USERS_GUIDE) 
	rm -f $(TEX_FILES_REF_MAN) 
	rm -f $(TEX_FILES_BOOK)

endif 

clean_man:
	rm -f $(MAN3DIR)/*

clean_html:
	rm -rf $(HTMLDIR)/*

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

man: $(MAN3_FILES) 

$(INDEX_TARGET): $(INDEX_SRC) $(APP_FILE)
	sed -e 's/%VSN%/$(VSN)/; \
                s/%ERLANG_SITE%/www\.erlang\.se\//; \
                s/%UP_ONE_LEVEL%/..\/..\/..\/doc\/index.html/; \
                s/%OFF_PRINT%/pdf\/megaco-$(VSN).pdf/' $< > $@

debug opt: 

info:
	@echo "->Makefile<-"
	@echo ""
	@echo "DOCSUPPORT = $(DOCSUPPORT)"
	@echo ""
	@echo "INDEX_FILE   = $(INDEX_FILE)"
	@echo "INDEX_SRC    = $(INDEX_SRC)"
	@echo "INDEX_TARGET = $(INDEX_TARGET)"
	@echo ""
	@echo "XML_APPLICATION_FILES = $(XML_APPLICATION_FILES)"
	@echo "XML_PART_FILES        = $(XML_PART_FILES)"
	@echo "XML_REF3_FILES        = $(XML_REF3_FILES)"
	@echo "XML_CHAPTER_FILES     = $(XML_CHAPTER_FILES)"
	@echo ""
	@echo "GIF_FILES             = $(GIF_FILES)"
	@echo ""
	@echo "TEX_FILES_USERS_GUIDE = $(TEX_FILES_USERS_GUIDE)"
	@echo "TEX_FILES_REF_MAN     = $(TEX_FILES_REF_MAN)"
	@echo "TEX_FILES_BOOK        = $(TEX_FILES_BOOK)"
	@echo ""
	@echo "MAN3_FILES            = $(MAN3_FILES)"
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

ifdef DOCSUPPORT

release_docs_spec: docs
	$(INSTALL_DIR) $(RELSYSDIR)/doc/pdf
	$(INSTALL_DATA) $(TOP_PDF_FILE) $(RELSYSDIR)/doc/pdf
	$(INSTALL_DIR) $(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(HTMLDIR)/* \
		$(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(INFO_FILE) $(RELSYSDIR)
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(MAN3DIR)/* $(RELEASE_PATH)/man/man3
	$(INSTALL_DIR)  $(RELSYSDIR)/doc/standard
	$(INSTALL_DATA) $(STANDARDS) $(RELSYSDIR)/doc/standard

else

ifeq ($(DOCTYPE),pdf)
release_docs_spec: pdf
	$(INSTALL_DIR)  $(RELEASE_PATH)/pdf
	$(INSTALL_DATA) $(TOP_PDF_FILE) $(RELEASE_PATH)/pdf
else
ifeq ($(DOCTYPE),ps)
release_docs_spec: ps
	$(INSTALL_DIR)  $(RELEASE_PATH)/ps
	$(INSTALL_DATA) $(TOP_PS_FILE) $(RELEASE_PATH)/ps
else
release_docs_spec: docs
	$(INSTALL_DIR)  $(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(GIF_FILES) $(EXTRA_FILES) $(HTML_FILES) \
		$(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(INFO_FILE) $(RELSYSDIR)
	$(INSTALL_DIR)  $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(MAN3_FILES) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(TOP_HTML_FILES) $(RELSYSDIR)/doc
	$(INSTALL_DIR)  $(RELSYSDIR)/doc/standard
	$(INSTALL_DATA) $(STANDARDS) $(RELSYSDIR)/doc/standard
endif
endif

endif

release_spec:

$(HTMLDIR)/megaco_architecture.html: megaco_architecture.xml
$(HTMLDIR)/megaco_codec_meas.html: megaco_codec_meas.xml
$(HTMLDIR)/megaco_codec_transform.html: megaco_codec_transform.xml
$(HTMLDIR)/megaco_debug.html: megaco_debug.xml
$(HTMLDIR)/megaco_encoder.html: megaco_encoder.xml
$(HTMLDIR)/megaco_encode.html: megaco_encode.xml
$(HTMLDIR)/megaco_examples.html: megaco_examples.xml
$(HTMLDIR)/megaco_flex_scanner.html: megaco_flex_scanner.xml
$(HTMLDIR)/megaco_intro.html: megaco_intro.xml
$(HTMLDIR)/megaco_mib.html: megaco_mib.xml
$(HTMLDIR)/megaco_performance.html: megaco_performance.xml
$(HTMLDIR)/megaco_run.html: megaco_run.xml
$(HTMLDIR)/megaco.html: megaco.xml
$(HTMLDIR)/megaco_tcp.html: megaco_tcp.xml
$(HTMLDIR)/megaco_transport_mechanisms.html: megaco_transport_mechanisms.xml
$(HTMLDIR)/megaco_transport.html: megaco_transport.xml
$(HTMLDIR)/megaco_udp.html: megaco_udp.xml
$(HTMLDIR)/megaco_user.html: megaco_user.xml


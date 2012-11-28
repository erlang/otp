#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2012. All Rights Reserved.
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
VSN = $(SNMP_VSN)
APPLICATION=snmp

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MIBSDIR = ../../mibs
include files.mk

# ----------------------------------------------------

HTML_APP_FILES  = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html)
XML_PART_FILE2 = $(XML_PART_FILES) notes_history.xml
HTML_PART_FILES = \
	../html/notes_history.html \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

HTML_FILES = \
	$(HTML_APP_FILES) \
	$(HTML_PART_FILES)

XML_ERRS = $(XML_FILES:%.xml=%.latex.xmls_errs) \
            $(XML_FILES:%.xml=%.html.xmls_errs)

XML_OUTPUT = $(XML_FILES:%.xml=%.latex.xmls_output) \
              $(XML_FILES:%.xml=%.html.xmls_output)

INFO_FILE = ../../info

#HTML_REF1_FILES = $(XML_REF1_FILES:%.xml=$(HTMLDIR)/%.html)
HTML_REF3_FILES = $(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html)
HTML_REF6_FILES = $(XML_REF6_FILES:%.xml=$(HTMLDIR)/%.html)
HTML_CHAP_FILES = $(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)

EXTRA_FILES = \
        summary.html.src \
        $(DEFAULT_HTML_FILES)  \
        $(HTML_REF1_FILES)     \
        $(HTML_REF3_FILES)     \
        $(HTML_REF6_FILES)     \
        $(HTML_CHAP_FILES)


MAN7DIR = $(DOCDIR)/man7

MAN1_FILES = $(MAN1DIR)/snmpc.1
MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)
MAN7_FILES = $(MIB_FILES:$(MIBSDIR)/%.mib=$(MAN7DIR)/%.7)

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

INDEX_FILE   = index.html
INDEX_SRC    = $(INDEX_FILE).src
INDEX_TARGET = $(DOCDIR)/$(INDEX_FILE)

GIF_TARGETS = $(GIF_FILES:%=$(HTMLDIR)/%)


# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
XML_FLAGS += 
DVIPS_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
$(HTMLDIR)/%.gif: %.gif			# Copy them to ../html
	$(INSTALL_DATA) $< $@

docs: pdf html man

ldocs: local_docs $(INDEX_TARGET)

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)
html2: html $(INDEX_TARGET)

clean clean_docs: clean_html clean_man clean_pdf
	rm -f errs core *~

$(INDEX_TARGET): $(INDEX_SRC) ../../vsn.mk	# Create top make file 
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@	# inserting version number

man: man1 man3 man6 man7

man1: $(MAN1_FILES)

man3: $(MAN3_FILES)

man6: $(MAN6_FILES)

man7: $(MAN7_FILES)

gifs: $(GIF_TARGETS)

debug opt: 

clean_pdf:
	@echo "cleaning pdf:"
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)

clean_man:
	@echo "cleaning man:"
	rm -f $(MAN1DIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(MAN6DIR)/*
	rm -f $(MAN7DIR)/*

clean_html:
	@echo "cleaning html:"
	rm -rf $(HTMLDIR)/*
	rm -f $(INDEX_TARGET)

$(MAN7DIR)/%.7: $(MIBSDIR)/%.mib
	@echo "processing $*"
	@echo ".TH $* 7  \"SNMP\" \"Erlang/OTP\" \"MIB\"" > $@
	@echo ".nf" >> $@
	@cat $< >> $@
	@echo ".fi" >> $@
	@echo "" >> $@


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 

$(MAN1DIR)/snmpc.1: snmpc_cmd.xml
	date=`date +"%B %e %Y"`; \
        xsltproc --output "$@" --stringparam company "Ericsson AB" --stringparam docgen "$(DOCGEN)" --stringparam gendate "$$date" --stringparam appname "$(APPLICATION)" --stringparam appver "$(VSN)" --xinclude -path $(DOCGEN)/priv/dtd  -path $(DOCGEN)/priv/dtd_man_entities $(DOCGEN)/priv/xsl/db_man.xsl $<

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
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man7"
	$(INSTALL_DATA) $(MAN7DIR)/* "$(RELEASE_PATH)/man/man7"

release_spec:

info: info_xml info_man info_html
	@echo "MAN1DIR: $(MAN1DIR)"
	@echo "MAN3DIR: $(MAN3DIR)"
	@echo "MAN6DIR: $(MAN6DIR)"
	@echo "MAN7DIR: $(MAN7DIR)"

info_man: 
	@echo "man files:"
	@echo "MAN1_FILES = $(MAN1_FILES)"
	@echo "MAN3_FILES = $(MAN3_FILES)"
	@echo "MAN6_FILES = $(MAN6_FILES)"
	@echo "MAN7_FILES = $(MAN7_FILES)"
	@echo ""
	@echo "MIB_FILES  = $(MIB_FILES)"

info_xml: 
	@echo "xml files:"
#	@echo "XML_REF1_FILES        = $(XML_REF1_FILES)"
	@echo "XML_REF3_FILES        = $(XML_REF3_FILES)"
	@echo "XML_REF6_FILES        = $(XML_REF6_FILES)"
	@echo "XML_PART_FILES        = $(XML_PART_FILES)"
	@echo "XML_CHAPTER_FILES     = $(XML_CHAPTER_FILES)"
	@echo "XML_APPLICATION_FILES = $(XML_APPLICATION_FILES)"
	@echo ""
	@echo "BOOK_FILES             = $(BOOK_FILES)"
	@echo ""
	@echo "XML_FILES  = $(XML_FILES)"
	@echo "XML_ERRS   = $(XML_ERRS)"
	@echo "XML_OUTPUT = $(XML_OUTPUT)"

info_html:
	@echo "html files:"
	@echo "DOCDIR = $(DOCDIR)"
	@echo "INDEX_FILE = $(INDEX_FILE)"
	@echo "INDEX_SRC = $(INDEX_SRC)"
	@echo "INDEX_TARGET = $(INDEX_TARGET)"
	@echo ""
	@echo "HTMLDIR = $(HTMLDIR)"
	@echo "HTML_APP_FILES  = $(HTML_APP_FILES)"
	@echo "HTML_PART_FILES = $(HTML_PART_FILES)"
	@echo "HTML_FILES  = $(HTML_FILES)"
	@echo ""
	@echo "EXTRA_FILES = $(EXTRA_FILES)"
	@echo ""
	@echo "DEFAULT_HTML_FILES = $(DEFAULT_HTML_FILES)"
	@echo ""
#	@echo "HTML_REF1_FILES = $(HTML_REF1_FILES)"
	@echo "HTML_REF3_FILES = $(HTML_REF3_FILES)"
	@echo "HTML_REF6_FILES = $(HTML_REF6_FILES)"
	@echo "HTML_CHAP_FILES = $(HTML_CHAP_FILES)"

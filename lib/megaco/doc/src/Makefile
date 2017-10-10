#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2000-2017. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(MEGACO_VSN)
APPLICATION=megaco

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include files.mk


# ----------------------------------------------------

XML_FILES = $(BOOK_FILES) $(XML_APPLICATION_FILES) $(XML_REF3_FILES) \
            $(XML_PART_FILES) $(XML_CHAPTER_FILES)

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

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

STANDARD_DIR = ../standard
STANDARDS = $(STANDARD_DIR)/rfc3525.txt \
            $(STANDARD_DIR)/rfc4234.txt \
            $(STANDARD_DIR)/rfc4566.txt \
            $(STANDARD_DIR)/implementors_guide_v10-13.pdf

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

$(HTMLDIR)/%.jpg: %.jpg
	$(INSTALL_DATA) $< $@

$(HTMLDIR)/%.png: %.png
	$(INSTALL_DATA) $< $@

docs: pdf html man

ldocs: local_docs

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: imgs $(HTML_REF_MAN_FILE)

clean clean_docs: clean_html clean_man
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~

clean_man:
	rm -f $(MAN3DIR)/*

clean_html:
	rm -rf $(HTMLDIR)/*

imgs: $(IMG_FILES:%=$(HTMLDIR)/%)

man: $(MAN3_FILES)

debug opt:

info:
	@echo "->Makefile<-"
	@echo ""
	@echo "HTML_REF_MAN_FILE   = $(HTML_REF_MAN_FILE)"
	@echo ""
	@echo "XML_APPLICATION_FILES = $(XML_APPLICATION_FILES)"
	@echo "XML_PART_FILES        = $(XML_PART_FILES)"
	@echo "XML_REF3_FILES        = $(XML_REF3_FILES)"
	@echo "XML_CHAPTER_FILES     = $(XML_CHAPTER_FILES)"
	@echo ""
	@echo "IMG_FILES             = $(IMG_FILES)"
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

release_docs_spec: docs
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(HTMLDIR)/* \
		"$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3DIR)/* "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DIR)  "$(RELSYSDIR)/doc/standard"
	$(INSTALL_DATA) $(STANDARDS) "$(RELSYSDIR)/doc/standard"

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

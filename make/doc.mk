#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2019. All Rights Reserved.
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
# Release directory specification
# ----------------------------------------------------
ifeq ($(APPLICATION),erts)
RELSYSDIR = $(RELEASE_PATH)/$(APPLICATION)-$(VSN)
else
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)
endif
RELCHUNKSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

APP_DIR = $(ERL_TOP)/lib/$(APPLICATION)
APP_SRC_DIR = $(APP_DIR)/src
APP_EBIN_DIR = $(APP_DIR)/src

# ----------------------------------------------------
HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_HTML_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

XML_ALL_REF3_FILES = $(XML_REF3_FILES) $(EDOC_REF3_FILES)
XML_CHAPTER_FILES += $(EDOC_CHAPTER_FILE)
XML_GEN_FILES += $(EDOC_REF3_FILES:%=$(XMLDIR)/%) $(EDOC_CHAPTER_FILE:%=$(XMLDIR)/%)

INFO_FILE = ../../info

MAN1_FILES = $(XML_REF1_FILES:%_cmd.xml=$(MAN1DIR)/%.1)
MAN2_FILES = $(XML_REF2_FILES:%.xml=$(MAN1DIR)/%.2)
MAN3_FILES = $(XML_ALL_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN4_FILES = $(XML_REF4_FILES:%.xml=$(MAN4DIR)/%.4)
MAN5_FILES = $(XML_REF5_FILES:%.xml=$(MAN4DIR)/%.5)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)
MAN7_FILES = $(MIB_REF7_FILES:$(MIBSDIR)/%.mib=$(MAN7DIR)/%.7)

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

ifneq ($(TOP_SPECS_FILE),)
SPECS_FILES = $(XML_ALL_REF3_FILES:%.xml=$(SPECDIR)/specs_%.xml)
endif

ifneq ($(strip $(CHUNKSDIR)),)
_create_chunksdir_dirs := $(shell mkdir -p $(CHUNKSDIR))
endif
CHUNK_REF3_FILES = $(filter-out $(NO_CHUNKS), $(XML_ALL_REF3_FILES))
CHUNK_FILES = $(CHUNK_REF3_FILES:%.xml=$(CHUNKSDIR)/%.chunk)

ERL_CHUNK_FILES = $(patsubst $(APP_EBIN_DIR)/%.BEAM,$(CHUNKSDIR)/%.chunk,$(wildcard $(APP_EBIN_DIR)/*.beam))
EMPTY_CHUNK_FILES = $(filter-out $(NO_CHUNKS:%.xml=$(CHUNKSDIR)/%.chunk) $(CHUNK_FILES), $(ERL_CHUNK_FILES))


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

SPECS_FLAGS = -I$(ERL_TOP)/lib -I$(ERL_TOP)/lib/*/include -I$(ERL_TOP)/lib/*/src



# ----------------------------------------------------
# Targets
# ----------------------------------------------------
$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@
$(HTMLDIR)/%.png: %.png
	$(INSTALL_DATA) $< $@
$(HTMLDIR)/%.jpg: %.jpg
	$(INSTALL_DATA) $< $@

DOC_TARGETS?=man pdf html chunks

docs: $(INFO_FILE) $(DOC_TARGETS)

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: images $(HTML_REF_MAN_FILE) $(HTMLDIR)/$(APPLICATION).eix

man: $(MAN1_FILES) $(MAN2_FILES) $(MAN3_FILES) $(MAN4_FILES) $(MAN5_FILES) $(MAN6_FILES) $(MAN7_FILES)

chunks: $(CHUNK_FILES) $(EMPTY_CHUNK_FILES)

images: $(IMAGE_FILES:%=$(HTMLDIR)/%)

$(EDOC_REF3_FILES:%=$(XMLDIR)/%): $(APP_SRC_DIR)/$(@:$(XMLDIR)/%.xml=%.erl)
	$(gen_verbose)escript $(DOCGEN)/priv/bin/xml_from_edoc.escript \
	  -def vsn $(VSN) $(EDOC_FLAGS) -dir $(XMLDIR) $(APP_SRC_DIR)/$(@:$(XMLDIR)/%.xml=%.erl)
$(XMLDIR)/$(EDOC_CHAPTER_FILE): ../overview.edoc
	$(gen_verbose)escript $(DOCGEN)/priv/bin/xml_from_edoc.escript -def vsn $(VSN) \
	-chapter -dir $(XMLDIR) $<

info:
	@echo "XML_APPLICATION_FILES: $(XML_APPLICATION_FILES)"
	@echo "XML_REF1_FILES:        $(XML_REF1_FILES)"
	@echo "XML_REF2_FILES:        $(XML_REF2_FILES)"
	@echo "XML_REF3_FILES:        $(XML_ALL_REF3_FILES)"
	@echo "XML_REF4_FILES:        $(XML_REF4_FILES)"
	@echo "XML_REF5_FILES:        $(XML_REF5_FILES)"
	@echo "XML_REF6_FILES:        $(XML_REF6_FILES)"
	@echo "XML_REF7_FILES:        $(XML_REF7_FILES)"
	@echo "XML_PART_FILES:        $(XML_PART_FILES)"
	@echo "XML_CHAPTER_FILES:     $(XML_CHAPTER_FILES)"
	@echo "BOOK_FILES:            $(BOOK_FILES)"

debug opt lcnt:

clean clean_docs: clean_xml clean_pdf clean_html clean_man clean_chunks
	rm -rf $(EXTRA_FILES)
	rm -f  errs core *~ *.eps

clean_pdf:
	rm -f $(PDFDIR)/*

clean_man:
	rm -f $(MAN1DIR)/* $(MAN3DIR)/* $(MAN4DIR)/* $(MAN6DIR)/*

clean_xml:
	rm -f  $(SPECDIR)/*
	rm -rf $(XMLDIR)

clean_html:
	rm -rf $(HTMLDIR)/*

clean_chunks:
	rm -f  $(CHUNKSDIR)/*

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

$(RELSYSDIR):
	$(INSTALL_DIR) "$(RELSYSDIR)"

release_pdf_spec: pdf
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"

release_html_spec: html
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html"
	$(INSTALL_DIR_DATA) $(HTMLDIR) "$(RELSYSDIR)/doc/html"
ifneq ($(HTML_EXTRA_FILES),)
	$(INSTALL_DATA) $(HTML_EXTRA_FILES) "$(RELSYSDIR)/doc/html"
endif

release_chunks_spec: chunks
ifneq ($(CHUNK_FILES),)
	$(INSTALL_DIR) "$(RELCHUNKSDIR)/doc/chunks"
	$(INSTALL_DATA) $(CHUNKSDIR)/* "$(RELCHUNKSDIR)/doc/chunks"
endif

release_man_spec: man
ifneq ($(MAN1_FILES),)
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man1"
	$(INSTALL_DATA) $(MAN1DIR)/* "$(RELEASE_PATH)/man/man1"
endif
ifneq ($(MAN2_FILES),)
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man2"
	$(INSTALL_DATA) $(MAN2DIR)/* "$(RELEASE_PATH)/man/man2"
endif
ifneq ($(MAN3_FILES),)
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3DIR)/* "$(RELEASE_PATH)/man/man3"
endif
ifneq ($(MAN4_FILES),)
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man4"
	$(INSTALL_DATA) $(MAN4_FILES) "$(RELEASE_PATH)/man/man4"
endif
ifneq ($(MAN5_FILES),)
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man5"
	$(INSTALL_DATA) $(MAN5_FILES) "$(RELEASE_PATH)/man/man5"
endif
ifneq ($(MAN6_FILES),)
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man6"
	$(INSTALL_DATA) $(MAN6_FILES) "$(RELEASE_PATH)/man/man6"
endif
ifneq ($(MAN7_FILES),)
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man7"
	$(INSTALL_DATA) $(MAN7_FILES) "$(RELEASE_PATH)/man/man7"
endif

release_docs_spec: $(RELSYSDIR) $(INFO_FILE) $(DOC_TARGETS:%=release_%_spec)
	$(INSTALL_DATA) $(INFO_FILE) $(RELSYSDIR)
ifneq ($(STANDARDS),)
	$(INSTALL_DIR) "$(RELEASE_PATH)/doc/standard"
	$(INSTALL_DATA) $(STANDARDS) "$(RELEASE_PATH)/doc/standard"
endif

release_spec:

.PHONY: clean clean_xml clean_html clean_man clean_pdf \
        debug opt info \
        docs images html man pdf chunks \
        release_docs_spec release_spec

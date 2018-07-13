#<copyright>
# <year>2004-2017</year>
# <holder>Ericsson AB, All Rights Reserved</holder>
#</copyright>
#<legalnotice>
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
# The Initial Developer of the Original Code is Ericsson AB.
#</legalnotice>
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(EUNIT_VSN)
APPLICATION=eunit

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Help application directory specification
# ----------------------------------------------------

EDOC_DIR = $(ERL_TOP)/lib/edoc
SYNTAX_TOOLS_DIR = $(ERL_TOP)/lib/syntax_tools

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
EUNIT_DIR = $(ERL_TOP)/lib/eunit/src
EUNIT_INC_DIR = $(ERL_TOP)/lib/eunit/include

EUNIT_MODULES = \
	eunit eunit_surefire

XML_APPLICATION_FILES = ref_man.xml

XML_REF3_FILES =  $(EUNIT_MODULES:=.xml)

XML_PART_FILES =  \
	part.xml

XML_CHAPTER_FILES =  \
	chapter.xml

XML_NOTES_FILES =  \
	notes.xml

HTML_EXAMPLE_FILES =

HTML_STYLESHEET_FILES = \
	../stylesheet.css

BOOK_FILES = book.xml

XML_FILES = \
	$(BOOK_FILES) $(XML_NOTES_FILES) \
	$(XML_PART_FILES) $(XML_APPLICATION_FILES)

XML_GEN_FILES = $(XML_REF3_FILES:%=$(XMLDIR)/%) $(XML_CHAPTER_FILES:%=$(XMLDIR)/%)

# ----------------------------------------------------
INFO_FILE = ../../info

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)


EXTRA_FILES = \
	$(DEFAULT_HTML_FILES) \
	$(DEFAULT_GIF_FILES) \
	$(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_REF6_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)\
	$(XML_NOTES_FILES:%.xml=$(HTMLDIR)/%.html)

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)

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


man: $(MAN3_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

$(XML_REF3_FILES:%=$(XMLDIR)/%):
	$(gen_verbose)escript $(DOCGEN)/priv/bin/xml_from_edoc.escript -def vsn $(EUNIT_VSN) -i $(EUNIT_INC_DIR) -dir $(XMLDIR) $(EUNIT_DIR)/$(@:$(XMLDIR)/%.xml=%.erl)

$(XML_CHAPTER_FILES:%=$(XMLDIR)/%):
	$(gen_verbose)escript $(DOCGEN)/priv/bin/xml_from_edoc.escript -def vsn $(EUNIT_VSN) -chapter -dir $(XMLDIR) ../overview.edoc

info:
	@echo "XML_PART_FILES: $(XML_PART_FILES)"
	@echo "XML_APPLICATION_FILES: $(XML_APPLICATION_FILES)"
	@echo "EUNIT_XML_FILES: $(EUNIT_XML_FILES)"
	@echo "EUNIT_MODULES: $(EUNIT_MODULES)"
	@echo "HTML_FILES: $(HTML_FILES)"
	@echo "HTMLDIR: $(HTMLDIR)"
	@echo "DEFAULT_GIF_FILES: $(DEFAULT_GIF_FILES)"
	@echo "DEFAULT_HTML_FILES: $(DEFAULT_HTML_FILES)"
	@echo "EXTRA_FILES: $(EXTRA_FILES)"

xml: $(XML_REF3_FILES) $(XML_CHAPTER_FILES)

debug opt:

clean clean_docs:
	rm -rf $(HTMLDIR)/*
	rm -rf $(XMLDIR)
	rm -f $(MAN3DIR)/*
	rm -f $(XML_REF3_FILES) $(XML_CHAPTER_FILES) *.html
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


release_spec:

#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
include $(ERL_TOP)/erts/vsn.mk
#VSN=$(SYSTEM_VSN)

APPLICATION=otp-system-documentation
XMLDIR := $(XMLDIR)/installation_guide

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = "$(RELEASE_PATH)/doc/installation_guide"
REDIRECT_HTML_RELSYSDIR = $(RELSYSDIR)/source

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_PART_FILES = part.xml

include xmlfiles.mk

XML_CHAPTER_FILES=$(INST_GUIDE_CHAPTER_FILES)

# ----------------------------------------------------

TOPDOCDIR=..

BOOK_FILES = book.xml

GIF_FILES =

PS_FILES = 

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) 

XML_GEN_FILES = $(INST_GUIDE_CHAPTER_GEN_FILES:%=$(XMLDIR)/%)

# ----------------------------------------------------

HTML_FILES = \
	$(XML_PART_FILES:%.xml=%.html)

HTMLDIR = ../html/installation_guide

HTML_UG_FILE = $(HTMLDIR)/users_guide.html

REDIRECT_HTML_DIR = $(HTMLDIR)/source
REDIRECT_HTML_FILES = \
	$(REDIRECT_HTML_DIR)/INSTALL.html \
	$(REDIRECT_HTML_DIR)/INSTALL-CROSS.html \
	$(REDIRECT_HTML_DIR)/INSTALL-WIN32.html \
	$(REDIRECT_HTML_DIR)/OTP-PATCH-APPLY.html

# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
XML_FLAGS += 
DVIPS_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(XMLDIR)/%.xml: $(ERL_TOP)/HOWTO/%.md $(ERL_TOP)/make/emd2exml
	$(ERL_TOP)/make/emd2exml $< $@

$(REDIRECT_HTML_DIR)/%.html: Makefile
	test -d $(REDIRECT_HTML_DIR) || $(INSTALL_DIR) $(REDIRECT_HTML_DIR)
	echo "<html><head><meta HTTP-EQUIV=\"REFRESH\"" > $@
	echo " content=\"5; url=../"$(notdir $@)"\">" >> $@
	echo "<title>This page has moved</title></head><body>" >> $@
	echo "<h1>This page has moved</h1>" >> $@
	echo "<p>You will be automatically redirected to its new" >> $@
	echo "location in 5 seconds." >> $@
	echo "This <a href=\"../"$(notdir $@)"\">link</a> will" >> $@
	echo "take you there immediately.</p></body></html>" >> $@

docs: $(XML_GEN_FILES) html

local_docs: PDFDIR=../../pdf
local_docs: $(XML_GEN_FILES)

html: $(REDIRECT_HTML_FILES) $(XML_GEN_FILES) $(GIF_FILES) $(HTML_UG_FILE)

debug opt:

clean clean_docs:
	rm -f $(GENERATED_XML_FILES)
	rm -rf $(HTMLDIR)
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~ 

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_docs_spec: docs
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) $(GIF_FILES) $(HTMLDIR)/*.html $(RELSYSDIR)
	$(INSTALL_DIR) $(REDIRECT_HTML_RELSYSDIR)
	$(INSTALL_DATA) $(REDIRECT_HTML_FILES) $(REDIRECT_HTML_RELSYSDIR)

release_spec:



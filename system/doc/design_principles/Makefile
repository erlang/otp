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
include $(ERL_TOP)/erts/vsn.mk

APPLICATION=otp-system-documentation
# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = "$(RELEASE_PATH)/doc/design_principles"

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_PART_FILES = part.xml

include xmlfiles.mk 

XML_CHAPTER_FILES=$(DESIGN_PRINCIPLES_CHAPTER_FILES)

TOPDOCDIR=..

BOOK_FILES = book.xml

GIF_FILES = \
	note.gif \
	clientserver.gif \
	dist1.gif \
	dist2.gif \
	dist3.gif \
	dist4.gif \
	dist5.gif \
	inclappls.gif \
	sup4.gif \
	sup5.gif \
	sup6.gif

PNG_FILES = \
	code_lock.png \
	code_lock_2.png

IMAGE_FILES = $(GIF_FILES) $(PNG_FILES)

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) 

# ----------------------------------------------------

HTML_FILES = \
	$(XML_PART_FILES:%.xml=%.html)

HTMLDIR = ../html/design_principles

HTML_UG_FILE = $(HTMLDIR)/users_guide.html

# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
XML_FLAGS += 
DVIPS_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
_create_dirs := $(shell mkdir -p $(HTMLDIR))

$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@

$(HTMLDIR)/%.png: %.png
	$(INSTALL_DATA) $< $@

docs: html

local_docs: PDFDIR=../../pdf

html: $(HTML_UG_FILE) images

images: $(IMAGE_FILES:%=$(HTMLDIR)/%)

debug opt: 

clean clean_docs:
	rm -rf $(HTMLDIR)
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~ 

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_docs_spec: docs
#	$(INSTALL_DIR) "$(RELEASE_PATH)/pdf"
#	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELEASE_PATH)/pdf"
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) $(IMAGE_FILES) $(HTMLDIR)/*.html \
		$(RELSYSDIR)


release_spec:



#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
XMLDIR := $(XMLDIR)/general_info
# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = "$(RELEASE_PATH)/doc/general_info"

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_PART_FILES = part.xml

include xmlfiles.mk

XML_CHAPTER_FILES=$(GENERAL_INFO_CHAPTER_FILES)

TOPDOCDIR=..

BOOK_FILES = book.xml

GIF_FILES = 

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) 

# ----------------------------------------------------

HTMLDIR = ../html/general_info

HTML_UG_FILE = $(HTMLDIR)/users_guide.html

# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
XML_FLAGS += 
DVIPS_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

DOC_TARGETS?=html

docs: $(DOC_TARGETS)

local_docs: PDFDIR=../../pdf

html: $(GIF_FILES) $(HTML_UG_FILE)

debug opt: 

DEPRECATIONS_SCRIPT = $(ERL_TOP)/lib/stdlib/scripts/update_deprecations

deprecations.xml: DEPRECATIONS
	$(gen_verbose)escript $(DEPRECATIONS_SCRIPT) make_xml deprecations $(ERL_TOP)

scheduled_for_removal.xml: DEPRECATIONS
	$(gen_verbose)escript $(DEPRECATIONS_SCRIPT) make_xml removals $(ERL_TOP)


clean clean_docs:
	rm -f $(XMLDIR)/*.xml
	rm -f $(HTMLDIR)/*.gif $(HTMLDIR)/*.html
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~ 

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_html_spec: html
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(GIF_FILES) $(EXTRA_FILES) $(HTMLDIR)/*.html \
		$(RELSYSDIR)

release_docs_spec: $(DOC_TARGETS:%=release_%_spec)

release_spec:

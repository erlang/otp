#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2000-2018. All Rights Reserved.
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
XMLDIR := $(XMLDIR)/tutorial
# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = "$(RELEASE_PATH)/doc/tutorial"

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_PART_FILES = part.xml

include xmlfiles.mk

XML_CHAPTER_FILES=$(TUTORIAL_CHAPTER_FILES)

TOPDOCDIR=..

BOOK_FILES = book.xml

GIF_FILES= port.gif port_driver.gif


XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) 

XML_GEN_FILES = \
	$(TUTORIAL_CHAPTER_GEN_FILES:%=$(XMLDIR)/%)

# ----------------------------------------------------

C_FILES = \
	cnode_c.c \
	cnode_s.c \
	cnode_s2.c \
	complex.c \
	ei.c \
	erl_comm.c \
	port.c \
	port_driver.c

ERL_FILES = \
	complex1.erl \
	complex2.erl \
	complex3.erl \
	complex4.erl \
	complex5.erl

HTMLDIR = ../html/tutorial

EXTRA_FILES =  \
	$(C_FILES) \
	$(ERL_FILES) 

HTML_UG_FILE = $(HTMLDIR)/users_guide.html

# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
XML_FLAGS += 
DVIPS_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(HTMLDIR)/%.gif: %.gif
	$(CP) $< $@

docs: html

local_docs: PDFDIR=../../pdf

html: $(HTML_UG_FILE) gifs 

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

debug opt: 

clean clean_docs:
	rm -f $(XMLDIR)/*.xml
	rm -f $(HTMLDIR)/*.gif $(HTMLDIR)/*.html
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
	$(INSTALL_DATA) $(GIF_FILES) $(EXTRA_FILES) $(HTMLDIR)/*.html \
		$(RELSYSDIR)

release_spec:

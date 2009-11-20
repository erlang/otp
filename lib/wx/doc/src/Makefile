#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
#

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
include ../../config.mk
APPLICATION=wxErlang

ErlMods         = wx.erl wx_object.erl

GenMods         = $(shell ls ../../src/gen | grep 'wx[A-Z].*\.erl') \
		wx_misc.erl \
		glu.erl \
		gl.erl

# GenMods         =  \
# 		wx_misc.erl \
# 		glu.erl \
# 		gl.erl


ModsNoExt	= $(ErlMods:%.erl=%) $(GenMods:%.erl=%)

ifneq ($(INSIDE_ERLSRC),true)
# ----------------------------------------------------
# Standalone release
# ----------------------------------------------------
Mods            = $(ErlMods:%=../../src/%) $(GenMods:%=../../src/gen/%)
HTML_FILES      = $(ErlMods:%.erl=../html/%.html) $(GenMods:%.erl=../html/%.html)

GIF_FILES       = ../html/erlang.png
EXTRA_FILES     = ../html/index.html \
	../html/modules-frame.html \
	../html/overview-summary.html \
	../html/packages-frame.html

HTML_STYLESHEET_FILES = ../html/stylesheet.css

html:   ../html/edoc-info

docs:	../html/edoc-info

../html/edoc-info: $(Mods)
	@echo  "Building documentation."
	-mkdir -p ../html
	erl -noshell -pa doc -run edoc_run application "wx" '"../.."' '[{private,true},no_packages,{dir,"../html"}, {sort_functions,false}]'

clean:
	rm -f *~
	(cd ../html; rm -f *.html *.css erlang.png edoc-info)

opt debug:	
else
# Release directory specification
# ----------------------------------------------------
# Inside the erlang src
# ----------------------------------------------------

XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = $(ErlMods:%.erl=%.xml) $(GenMods:%.erl=%.xml)

XML_PART_FILES = part.xml part_notes.xml
XML_CHAPTER_FILES = chapter.xml
XML_NOTES_FILES = notes.xml

BOOK_FILES = book.xml

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) \
	$(XML_NOTES_FILES) $(XML_APPLICATION_FILES)


# ----------------------------------------------------
INFO_FILE = ../../info

HTML_FILES = \
	$(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)

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

docs: pdf html man

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

man: $(MAN3_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

xml: $(XML_REF3_FILES) $(XML_CHAPTER_FILES) 

ref_man.xml: ref_man.src.xml 
	@echo Preparing ref_man.xml
	@cat ref_man.src.xml > ref_man.xml
	@for d in $(ModsNoExt); do         \
		echo "  <xi:include href=\"$$d.xml\"/>" >> ref_man.xml ; \
	done
	@echo "</application>" >> ref_man.xml
	@echo 

$(ErlMods:%.erl=%.xml):
	docb_gen -def vsn $(VSN) -sort_functions false ../../src/$(@:%.xml=%.erl)
$(GenMods:%.erl=%.xml):
	docb_gen -def vsn $(VSN) -sort_functions false ../../src/gen/$(@:%.xml=%.erl)

$(XML_CHAPTER_FILES):
	docb_gen -chapter -def vsn $(VSN) ../overview.edoc

debug opt: 

clean clean_docs:
	rm -rf  $(HTMLDIR)/* 
	rm -f $(MAN3DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~ ../html/edoc-info
	rm -f $(XML_REF3_FILES) $(XML_CHAPTER_FILES) *.html

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_docs_spec: docs
	$(INSTALL_DIR) $(RELSYSDIR)/doc/pdf
	$(INSTALL_DATA) $(TOP_PDF_FILE) $(RELSYSDIR)/doc/pdf
	$(INSTALL_DIR) $(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(HTMLDIR)/*  \
		$(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(INFO_FILE) $(RELSYSDIR)
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(MAN3_FILES) $(RELEASE_PATH)/man/man3

release_spec: 

release_tests_spec:

endif

# 
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
# 

SPECS_ESRC = ../../preloaded/src/

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
APPLICATION=erts
# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF1_FILES = epmd.xml \
	erl.xml \
	erlc.xml \
	escript.xml \
	werl.xml \
	erlsrv.xml \
	start_erl.xml \
	run_erl.xml \
	start.xml

XML_REF3_EFILES = \
	erl_prim_loader.xml \
	erlang.xml \
	init.xml \
	zlib.xml

XML_REF3_FILES = \
	driver_entry.xml \
	erl_nif.xml \
	erl_driver.xml \
	erl_prim_loader.xml \
	erlang.xml \
	erts_alloc.xml  \
	init.xml \
	zlib.xml

XML_PART_FILES = \
	part.xml \
	part_notes.xml \
	part_notes_history.xml

XML_CHAPTER_FILES = \
	tty.xml \
	match_spec.xml \
	crash_dump.xml \
	alt_dist.xml \
	driver.xml \
	absform.xml \
	inet_cfg.xml \
	erl_ext_dist.xml \
	erl_dist_protocol.xml \
	communication.xml \
	time_correction.xml \
	notes.xml \
	notes_history.xml

TOPDOCDIR=../../../doc

BOOK_FILES = book.xml

GIF_FILES = \
	erl_ext_fig.gif

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) $(XML_REF1_FILES) $(XML_APPLICATION_FILES)

# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

INFO_FILE     = ../../info
INFO_FILE_SRC = ../../info.src

MAN1_FILES = $(XML_REF1_FILES:%.xml=$(MAN1DIR)/%.1)
MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

SPECS_FILES = $(XML_REF3_EFILES:%.xml=$(SPECDIR)/specs_%.xml)

TOP_SPECS_FILE = specs.xml

# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
XML_FLAGS += 

KERNEL_SRC=$(ERL_TOP)/lib/kernel/src
KERNEL_INCLUDE=$(ERL_TOP)/lib/kernel/include
SPECS_FLAGS = -I$(KERNEL_SRC) -I$(KERNEL_INCLUDE)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@

docs: man pdf html $(INFO_FILE)

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

man: $(MAN1_FILES) $(MAN3_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

$(INFO_FILE): $(INFO_FILE_SRC) $(ERL_TOP)/make/$(TARGET)/otp.mk
	sed -e 's;%RELEASE%;$(SYSTEM_VSN);' $(INFO_FILE_SRC) > $(INFO_FILE)


debug opt: 

clean:
	rm -rf $(HTMLDIR)/*
	rm -f $(MAN1DIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f $(SPECDIR)/*
	rm -f errs core *~ 

$(SPECDIR)/specs_driver_entry.xml:
	escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) \
                -o$(dir $@) -module driver_entry
$(SPECDIR)/specs_erl_nif.xml:
	escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) \
                -o$(dir $@) -module erl_nif
$(SPECDIR)/specs_erl_driver.xml:
	escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) \
                -o$(dir $@) -module erl_driver
$(SPECDIR)/specs_erts_alloc.xml:
	escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) \
                -o$(dir $@) -module erts_alloc

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
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man1"
	$(INSTALL_DATA) $(MAN1_FILES) "$(RELEASE_PATH)/man/man1"

release_spec:

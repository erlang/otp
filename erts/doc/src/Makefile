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

ifeq ($(USE_ESOCK), yes)
XML_REF3_ESOCK_EFILES = socket.xml
XML_CHAPTER_ESOCK_EFILES = socket_usage.xml
ESOCK_USE_SOCKET_XML=<xi:include href="socket.xml"\/>
ESOCK_USE_SOCKET_SPECS_XML=<xi:include href="../specs/specs_socket.xml"/>
ESOCK_USE_SOCKET_USAGE_XML=<xi:include href="socket_usage.xml"/>
else
XML_REF3_ESOCK_EFILES =
XML_CHAPTER_ESOCK_EFILES =
ESOCK_USE_SOCKET_XML =
ESOCK_USE_SOCKET_SPECS_XML =
ESOCK_USE_SOCKET_USAGE_XML =
endif

XML_REF3_EFILES = \
	erl_prim_loader.xml \
	erlang.xml \
	erl_tracer.xml \
	init.xml \
	persistent_term.xml \
	atomics.xml \
	counters.xml \
	zlib.xml \
	$(XML_REF3_ESOCK_EFILES)

XML_REF3_FILES = \
	$(XML_REF3_EFILES) \
	driver_entry.xml \
	erl_nif.xml \
	erl_driver.xml \
	erts_alloc.xml

XML_PART_FILES = \
	part.xml internal.xml

XML_INTERNAL_FILES = \
	CarrierMigration.xml	\
	ThreadProgress.xml	\
	CodeLoading.xml		\
	Tracing.xml		\
	DelayedDealloc.xml	\
	beam_makeops.xml	\
	GarbageCollection.xml	\
	PTables.xml		\
	PortSignals.xml		\
	ProcessManagementOptimizations.xml \
	SuperCarrier.xml        \
	CountingInstructions.xml


XML_CHAPTER_FILES = \
	introduction.xml \
	tty.xml \
	match_spec.xml \
	crash_dump.xml \
	alt_dist.xml \
	alt_disco.xml \
	driver.xml \
	absform.xml \
	inet_cfg.xml \
	$(XML_CHAPTER_ESOCK_EFILES) \
	erl_ext_dist.xml \
	erl_dist_protocol.xml \
	communication.xml \
	time_correction.xml \
	notes.xml

TOPDOCDIR=../../../doc

BOOK_FILES = book.xml

GIF_FILES = \
	erl_ext_fig.gif

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) $(XML_REF1_FILES) $(XML_APPLICATION_FILES)

XML_GEN_FILES = $(XML_INTERNAL_FILES:%=$(XMLDIR)/%)

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

XML_FIGURE_DIR = $(XMLDIR)/figures

INTERNAL_DOC_PNG_FILES = $(wildcard ../../emulator/internal_doc/figures/*.png)
PNG_FILES = $(notdir $(INTERNAL_DOC_PNG_FILES))
XMLDIR_PNG_FILES = $(PNG_FILES:%=$(XML_FIGURE_DIR)/%)

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
_create_dirs := $(shell mkdir -p $(XML_FIGURE_DIR))

$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@

$(XML_FIGURE_DIR)/%.png: ../../emulator/internal_doc/figures/%.png
	$(INSTALL_DATA) $< $@

docs: part ref_man specs figures man pdf html $(INFO_FILE)

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

man: $(MAN1_FILES) $(MAN3_FILES)

ref_man: ref_man.xml
part: part.xml
specs: specs.xml

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

$(INFO_FILE): $(INFO_FILE_SRC) $(ERL_TOP)/make/$(TARGET)/otp.mk
	sed -e 's;%RELEASE%;$(SYSTEM_VSN);' $(INFO_FILE_SRC) > $(INFO_FILE)

figures: $(XMLDIR_PNG_FILES)

debug opt:

ldocs: xmllint local_docs

clean:
	rm -rf $(HTMLDIR)/*
	rm -rf $(XMLDIR)
	rm -f $(MAN1DIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f $(SPECDIR)/*
	rm -f errs core *~

$(SPECDIR)/specs_%.xml:
	$(gen_verbose)escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) \
                -o$(dir $@) -module $(patsubst $(SPECDIR)/specs_%.xml,%,$@)

$(XMLDIR)/%.xml: ../../emulator/internal_doc/%.md $(ERL_TOP)/make/emd2exml
	$(ERL_TOP)/make/emd2exml $< $@

ref_man.xml: ref_man.xml.src
	($(PERL) -p -e 's?%ESOCK_USE_SOCKET_XML%?$(ESOCK_USE_SOCKET_XML)?' \
            $<) > $@

part.xml: part.xml.src
	($(PERL) -p -e 's?%ESOCK_USE_SOCKET_USAGE_XML%?$(ESOCK_USE_SOCKET_USAGE_XML)?' \
            $<) > $@

specs.xml: specs.xml.src
	($(PERL) -p -e 's?%ESOCK_USE_SOCKET_SPECS_XML%?$(ESOCK_USE_SOCKET_SPECS_XML)?' \
            $<) > $@


# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_docs_spec: docs
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html"
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html/figures"
	$(INSTALL_DATA) $(HTMLDIR)/* \
		"$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(XMLDIR)/figures/* \
		"$(RELSYSDIR)/doc/html/figures"
	$(INSTALL_DATA) $(ERL_TOP)/erts/example/time_compat.erl \
		"$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(ERL_TOP)/lib/kernel/examples/gen_tcp_dist/src/gen_tcp_dist.erl \
		"$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3DIR)/* "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man1"
	$(INSTALL_DATA) $(MAN1_FILES) "$(RELEASE_PATH)/man/man1"

release_spec:

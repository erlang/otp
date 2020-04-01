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
XML_REF1_FILES = epmd_cmd.xml \
	erl_cmd.xml \
	erlc_cmd.xml \
	escript_cmd.xml \
	werl_cmd.xml \
	erlsrv_cmd.xml \
	start_erl_cmd.xml \
	run_erl_cmd.xml \
	start_cmd.xml

XML_REF3_EFILES = \
	erl_prim_loader.xml \
	erlang.xml \
	erl_tracer.xml \
	init.xml \
	persistent_term.xml \
	atomics.xml \
	counters.xml \
	zlib.xml

XML_REF3_CREF = \
	driver_entry.xml \
	erl_nif.xml \
	erl_driver.xml \
	erts_alloc.xml

XML_REF3_FILES = \
	$(XML_REF3_EFILES) \
	$(XML_REF3_CREF)

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
	erl_ext_dist.xml \
	erl_dist_protocol.xml \
	communication.xml \
	time_correction.xml \
	notes.xml

TOPDOCDIR=../../../doc

BOOK_FILES = book.xml

IMAGE_FILES = \
	erl_ext_fig.gif

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) $(XML_REF1_FILES) $(XML_APPLICATION_FILES)

HTML_EXTRA_FILES = $(ERL_TOP)/erts/example/time_compat.erl \
	$(ERL_TOP)/lib/kernel/examples/gen_tcp_dist/src/gen_tcp_dist.erl

XML_GEN_FILES = $(XML_INTERNAL_FILES:%=$(XMLDIR)/%)

NO_CHUNKS = $(XML_REF3_CREF) erl_tracer.xml

# ----------------------------------------------------

INFO_FILE_SRC = ../../info.src

TOP_SPECS_FILE = specs.xml

XML_FIGURE_DIR = $(XMLDIR)/figures

INTERNAL_DOC_PNG_FILES = $(wildcard ../../emulator/internal_doc/figures/*.png)
PNG_FILES = $(notdir $(INTERNAL_DOC_PNG_FILES))
XMLDIR_PNG_FILES = $(PNG_FILES:%=$(XML_FIGURE_DIR)/%)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
include $(ERL_TOP)/make/doc.mk

_create_dirs := $(shell mkdir -p $(XML_FIGURE_DIR))

$(XML_FIGURE_DIR)/%.png: ../../emulator/internal_doc/figures/%.png
	$(INSTALL_DATA) $< $@

html: figures

$(INFO_FILE): $(INFO_FILE_SRC) $(ERL_TOP)/make/$(TARGET)/otp.mk
	sed -e 's;%RELEASE%;$(SYSTEM_VSN);' $(INFO_FILE_SRC) > $(INFO_FILE)

figures: $(XMLDIR_PNG_FILES)

## This rule generate dummy specs for all XML_REF3_CREF's
$(XML_REF3_CREF:%.xml=$(SPECDIR)/specs_%.xml): $(@:%.xml=%.xml)
	$(gen_verbose)escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) \
                -o$(dir $@) -module $(patsubst $(SPECDIR)/specs_%.xml,%,$@)

$(XMLDIR)/%.xml: ../../emulator/internal_doc/%.md $(ERL_TOP)/make/emd2exml
	$(ERL_TOP)/make/emd2exml $< $@


# ----------------------------------------------------
# Release Target
# ----------------------------------------------------

release_html_spec: release_figures

release_figures:
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html/figures"
	$(INSTALL_DATA) $(XMLDIR)/figures/* \
		"$(RELSYSDIR)/doc/html/figures"

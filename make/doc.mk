#
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 1997-2025. All Rights Reserved.
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
RELSYSDIR ?= $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

## The "system" applications set this to something else
RELSYS_HTMLDIR ?= $(RELSYSDIR)/doc/html

RELSYS_MANDIR ?= $(RELEASE_PATH)/man

# ----------------------------------------------------
# Application directory structure
# ----------------------------------------------------
APP_DIR ?= ../
INDEX_DIR ?= ../../../../doc
APP_SRC_DIR = $(APP_DIR)/src
APP_EBIN_DIR = $(APP_DIR)/ebin

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ifeq ($(EPUB), false)
EX_DOC_FORMATS=-f html
else
EX_DOC_FORMATS=
endif

# ----------------------------------------------------
# Man dependencies
# ----------------------------------------------------
ERL_FILES := $(wildcard $(APP_SRC_DIR)/*.erl) $(wildcard $(APP_SRC_DIR)/*/*.erl) $(wildcard $(APP_DIR)/preloaded/src/*.erl)
ERL_STRIP := $(strip $(ERL_FILES))
ifneq ($(ERL_STRIP),)
  ERL_FILES_WITH_DOC := $(shell grep -L "moduledoc false." $(ERL_FILES))
else
  ERL_FILES_WITH_DOC :=
endif
ERL_FILENAMES_ONLY := $(notdir $(ERL_FILES_WITH_DOC))
MAN1_DEPS?=$(wildcard */*_cmd.md)
MAN3_DEPS_UNFILTERED?=$(wildcard */src/*.md) $(wildcard src/*.md) \
 $(wildcard */references/*.md) $(wildcard references/*.md) \
 $(ERL_FILENAMES_ONLY)
MAN4_DEPS=$(wildcard references/app.md references/config.md references/appup.md references/rel.md \
 references/relup.md references/script.md references/diameter_dict.md references/erlang.el.md)
MAN3_DEPS=$(filter-out $(wildcard */references/*_cmd.md) $(wildcard references/*_cmd.md) $(MAN4_DEPS),$(MAN3_DEPS_UNFILTERED))
MAN6_DEPS=$(wildcard *_app.md)
MAN7_DEPS=$(wildcard $(APP_DIR)/mibs/*.mib)
MAN1_PAGES=$(MAN1_DEPS:references/%_cmd.md=$(MAN1DIR)/%.1)
MAN3_PAGES=$(MAN3_DEPS:%.erl=$(MAN3DIR)/%.3)
MAN3_PAGES+=$(MAN3_DEPS:src/%.md=$(MAN3DIR)/%.3)
MAN3_PAGES+=$(MAN3_DEPS:references/%.md=$(MAN3DIR)/%.3)
MAN4_PAGES=$(MAN4_DEPS:references/%.md=$(MAN4DIR)/%.4)
MAN6_PAGES=$(MAN6_DEPS:%_app.md=$(MAN6DIR)/%.6)
MAN7_PAGES=$(MAN7_DEPS:$(APP_DIR)/mibs/%.mib=$(MAN7DIR)/%.7)

# 1. Find all possible source directories recursively
ifneq ($(wildcard $(APP_SRC_DIR)),)
ERL_SRC_DIRS := $(shell find $(APP_SRC_DIR) -type d)
else
ERL_SRC_DIRS :=
endif
# 2. Tell make to search for .erl files in all those directories
vpath %.erl $(ERL_SRC_DIRS) $(APP_DIR)/preloaded/src

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
DEFAULT_DOC_TARGETS=html
ifneq ($(CHUNK_FILES),)
DEFAULT_DOC_TARGETS+=chunks
endif
ifneq ($(MAN1_DEPS),)
DEFAULT_DOC_TARGETS+=man
endif
ifneq ($(MAN3_DEPS),)
DEFAULT_DOC_TARGETS+=man
endif
ifneq ($(MAN4_DEPS),)
DEFAULT_DOC_TARGETS+=man
endif
ifneq ($(MAN6_DEPS),)
DEFAULT_DOC_TARGETS+=man
endif
ifneq ($(MAN7_DEPS),)
DEFAULT_DOC_TARGETS+=man
endif

DOC_TARGETS?=$(DEFAULT_DOC_TARGETS)

EX_DOC_WARNINGS_AS_ERRORS?=default

docs: $(DOC_TARGETS)

chunks:

ifneq ($(VSN), $(shell cat "$(ERL_TOP)/OTP_VERSION"))
DOC_VSN=$(shell if ! grep -q rc0 "$(ERL_TOP)/OTP_VERSION"; then echo "$(VSN)"; else echo "$(VSN)-rc0"; fi)
else
DOC_VSN=$(VSN)
endif

HTML_DEPS?=$(wildcard $(APP_EBIN_DIR)/*.beam) $(wildcard *.md) $(wildcard */*.md) $(wildcard assets/*)

$(HTMLDIR)/index.html: $(HTML_DEPS) docs.exs $(ERL_TOP)/make/ex_doc.exs
	$(gen_verbose)EX_DOC_WARNINGS_AS_ERRORS=$(EX_DOC_WARNINGS_AS_ERRORS) ERL_FLAGS="-pz $(ERL_TOP)/erts/ebin" \
	  $(ERL_TOP)/make/ex_doc_wrapper $(EX_DOC_FORMATS) --homepage-url "$(INDEX_DIR)/index.html" "$(APPLICATION)" $(DOC_VSN) $(APP_EBIN_DIR) -o "$(HTMLDIR)" -c $(ERL_TOP)/make/ex_doc.exs

html: $(HTMLDIR)/index.html

man: $(MAN1_PAGES) $(MAN3_PAGES) $(MAN4_PAGES) $(MAN6_PAGES) $(MAN7_PAGES)

MARKDOWN_TO_MAN=$(ERL_TOP)/make/markdown_to_man.escript

man1/%.1: references/%_cmd.md $(MARKDOWN_TO_MAN)
	@escript$(EXEEXT) $(MARKDOWN_TO_MAN) -o $(MAN1DIR) $<

man3/%.3: src/%.md $(MARKDOWN_TO_MAN)
	@escript$(EXEEXT) $(MARKDOWN_TO_MAN) -o $(MAN3DIR) $<

man3/%.3: references/%.md $(MARKDOWN_TO_MAN)
	@escript$(EXEEXT) $(MARKDOWN_TO_MAN) -o $(MAN3DIR) $<

man3/%.3: %.erl $(MARKDOWN_TO_MAN)
	@escript$(EXEEXT) $(MARKDOWN_TO_MAN) -o $(MAN3DIR) $<

man4/%.4: references/%.md $(MARKDOWN_TO_MAN)
	@escript$(EXEEXT) $(MARKDOWN_TO_MAN) -o $(MAN4DIR) -s 4 $<

man6/%.6: %_app.md $(MARKDOWN_TO_MAN)
	@escript$(EXEEXT) $(MARKDOWN_TO_MAN) -o $(MAN6DIR) $<

man7/%.7: $(APP_DIR)/mibs/%.mib
	@mkdir -p man7
	$(eval REL_PATH := $(patsubst $(ERL_TOP)/lib/%,%,$(abspath $<)))
	$(eval APP_NAME := $(shell echo $(firstword $(subst /, ,$(REL_PATH))) |  tr '[:lower:]' '[:upper:]'))
	$(eval MIB_NAME := $(basename $(notdir $<)))
	@echo .TH $(MIB_NAME) 7 \"$(APP_NAME)\" \"Erlang/OTP\" \"MIB\" > $@
	@echo .nf >> $@
	@grep -v '^--' $< >> $@

# ----------------------------------------------------

$(TYPES):

clean clean_docs: clean_html
	rm -rf $(EXTRA_FILES)

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_html_spec: html
	$(INSTALL_DIR) "$(RELSYS_HTMLDIR)"
	$(INSTALL_DIR_DATA) "$(HTMLDIR)/" "$(RELSYS_HTMLDIR)"
	$(V_at)$(ERL_TOP)/make/fixup_doc_links.sh "$(RELSYS_HTMLDIR)"/*.html

release_chunks_spec: chunks
ifneq ($(CHUNK_FILES),)
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/chunks"
	$(INSTALL_DATA) $(CHUNK_FILES) "$(RELSYSDIR)/doc/chunks"
endif

release_man_spec: man
ifneq ($(MAN1_DEPS),)
	$(INSTALL_DIR) "$(RELSYS_MANDIR)/man1"
	$(INSTALL_DIR_DATA) "$(MAN1DIR)" "$(RELSYS_MANDIR)/man1"
endif
ifneq ($(MAN3_DEPS),)
	$(INSTALL_DIR) "$(RELSYS_MANDIR)/man3"
	$(INSTALL_DIR_DATA) "$(MAN3DIR)" "$(RELSYS_MANDIR)/man3"
endif
ifneq ($(MAN4_DEPS),)
	$(INSTALL_DIR) "$(RELSYS_MANDIR)/man4"
	$(INSTALL_DIR_DATA) "$(MAN4DIR)" "$(RELSYS_MANDIR)/man4"
endif
ifneq ($(MAN6_DEPS),)
	$(INSTALL_DIR) "$(RELSYS_MANDIR)/man6"
	$(INSTALL_DIR_DATA) "$(MAN6DIR)" "$(RELSYS_MANDIR)/man6"
endif
ifneq ($(MAN7_DEPS),)
	$(INSTALL_DIR) "$(RELSYS_MANDIR)/man7"
	$(INSTALL_DIR_DATA) "$(MAN7DIR)" "$(RELSYS_MANDIR)/man7"
endif



release_docs_spec: $(DOC_TARGETS:%=release_%_spec)
ifneq ($(STANDARDS),)
	$(INSTALL_DIR) "$(RELEASE_PATH)/doc/standard"
	$(INSTALL_DATA) $(STANDARDS) "$(RELEASE_PATH)/doc/standard"
endif


release_spec:

.PHONY: clean clean_html $(TYPES) docs images html chunks \
	release_docs_spec release_html_spec release_chunks_spec release_spec \
	release_man_spec man

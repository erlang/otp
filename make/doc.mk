#
# %CopyrightBegin%
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
MAN1_DEPS?=$(wildcard */*_cmd.md)

MAN1_PAGES=$(MAN1_DEPS:references/%_cmd.md=$(MAN1DIR)/%.1)

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
DOC_TARGETS?=$(DEFAULT_DOC_TARGETS)

EX_DOC_WARNINGS_AS_ERRORS?=default

docs: $(DOC_TARGETS)

chunks:

HTML_DEPS?=$(wildcard $(APP_EBIN_DIR)/*.beam) $(wildcard *.md) $(wildcard */*.md) $(wildcard assets/*)

$(HTMLDIR)/index.html: $(HTML_DEPS) docs.exs $(ERL_TOP)/make/ex_doc.exs
	$(gen_verbose)EX_DOC_WARNINGS_AS_ERRORS=$(EX_DOC_WARNINGS_AS_ERRORS) ERL_FLAGS="-pz $(ERL_TOP)/erts/ebin" \
	  $(ERL_TOP)/make/ex_doc_wrapper $(EX_DOC_FORMATS) --homepage-url "$(INDEX_DIR)/index.html" "$(APPLICATION)" $(VSN) $(APP_EBIN_DIR) -o "$(HTMLDIR)" -c $(ERL_TOP)/make/ex_doc.exs

html: $(HTMLDIR)/index.html

man: $(MAN1_PAGES)

MARKDOWN_TO_MAN=$(ERL_TOP)/make/markdown_to_man.escript

man1/%.1: references/%_cmd.md $(MARKDOWN_TO_MAN)
	escript$(EXEEXT) $(MARKDOWN_TO_MAN) -o $(MAN1DIR) $<

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

release_docs_spec: $(DOC_TARGETS:%=release_%_spec)
ifneq ($(STANDARDS),)
	$(INSTALL_DIR) "$(RELEASE_PATH)/doc/standard"
	$(INSTALL_DATA) $(STANDARDS) "$(RELEASE_PATH)/doc/standard"
endif


release_spec:

.PHONY: clean clean_html $(TYPES) docs images html chunks \
	release_docs_spec release_html_spec release_chunks_spec release_spec \
	release_man_spec man

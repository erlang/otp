#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2024. All Rights Reserved.
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
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(EDOC_VSN)
APPLICATION=edoc

assets/edoc_doclet.hrl:
	$(V_at)cp "../include/edoc_doclet.hrl" "$@"
assets/edoc.dtd:
	$(V_at)cp "../priv/edoc.dtd" "$@"

$(HTMLDIR)/index.html: assets/edoc_doclet.hrl assets/edoc.dtd

## Include SOURCES, HRL_FILES and YRL_FILES
include ../src/files.mk

NO_PARSE_SOURCES=$(filter-out edoc_parser.erl, $(SOURCES))
CHUNK_FILES=$(NO_PARSE_SOURCES:%.erl=$(CHUNKSDIR)/%.chunk) $(YRL_FILES:%.yrl=$(CHUNKSDIR)/%.chunk)

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/doc.mk

chunks: $(CHUNK_FILES)

$(CHUNKSDIR)/%.chunk: ../src/%.erl
	$(gen_verbose)erl -noinput -eval "edoc:files([\"$<\"],[{doclet, edoc_doclet_chunks},{layout, edoc_layout_chunks},{preprocess,true},{includes,[\"../include\"]}])" -s init stop

# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 1999-2025. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(TOOLS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/tools-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

EMACS_FILES= \
	erldoc \
	erlang-skels \
	erlang-skels-old \
	erlang_appwiz \
	erlang-start \
	erlang-eunit \
	erlang-edoc \
	erlang-flymake \
	erlang-test \
	erlang

README_FILES= README

EL_FILES = $(EMACS_FILES:%=%.el)

ELC_FILES = $(EMACS_FILES:%=%.elc) 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TYPES): $(TARGET_FILES) $(EL_FILES)

clean:
	rm -f $(TARGET_FILES) $(ELC_FILES)
	rm -f errs core *~

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/emacs"
	$(INSTALL_DATA) $(EL_FILES) $(README_FILES) \
		"$(RELSYSDIR)/emacs"

release_html_spec:
release_chunks_spec:
release_man_spec:

release_docs_spec: $(DOC_TARGETS:%=release_%_spec)

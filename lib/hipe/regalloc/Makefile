#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

ifndef EBIN
EBIN = ../ebin
endif

ifndef DOCS
DOCS = ../doc
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(HIPE_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/hipe-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES = hipe_ig hipe_ig_moves hipe_moves \
	  hipe_node_sets hipe_spillcost hipe_reg_worklists \
	  hipe_adj_list \
	  hipe_temp_map \
	  hipe_optimistic_regalloc \
	  hipe_coalescing_regalloc \
	  hipe_graph_coloring_regalloc \
	  hipe_regalloc_loop \
	  hipe_ls_regalloc \
	  hipe_ppc_specific hipe_ppc_specific_fp \
	  hipe_sparc_specific hipe_sparc_specific_fp \
	  hipe_arm_specific \
	  hipe_x86_specific hipe_x86_specific_x87 \
	  hipe_amd64_specific hipe_amd64_specific_sse2 hipe_amd64_specific_x87

HRL_FILES=
ERL_FILES= $(MODULES:%=%.erl)
TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
DOC_FILES= $(MODULES:%=$(DOCS)/%.html)

# APP_FILE= 
# APP_SRC= $(APP_FILE).src
# APP_TARGET= $(EBIN)/$(APP_FILE)
#
# APPUP_FILE= 
# APPUP_SRC= $(APPUP_FILE).src
# APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../native.mk

ERL_COMPILE_FLAGS += -Werror +warn_export_vars #+warn_missing_spec +warn_untyped_record

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) 

docs:

# Moved docs target to edocs so the standard docs rule work properly.

edocs: $(DOC_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

distclean: clean
realclean: clean

$(DOCS)/%.html:%.erl
	erl -noshell -run edoc_run file '"$<"' '[{dir, "$(DOCS)"}]' -s init stop

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------



# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

$(EBIN)/hipe_amd64_specific.beam: hipe_x86_specific.erl
$(EBIN)/hipe_amd64_specific_x87.beam: hipe_x86_specific_x87.erl
$(EBIN)/hipe_coalescing_regalloc.beam: ../main/hipe.hrl
$(EBIN)/hipe_graph_coloring_regalloc.beam: ../main/hipe.hrl
$(EBIN)/hipe_ig.beam: ../main/hipe.hrl ../flow/cfg.hrl hipe_spillcost.hrl
$(EBIN)/hipe_ig_moves.beam: ../util/hipe_vectors.hrl
$(EBIN)/hipe_ls_regalloc.beam: ../main/hipe.hrl
$(EBIN)/hipe_optimistic_regalloc.beam: ../main/hipe.hrl
$(EBIN)/hipe_regalloc_loop.beam: ../main/hipe.hrl
$(EBIN)/hipe_spillcost.beam: hipe_spillcost.hrl
$(EBIN)/hipe_temp_map.beam: ../main/hipe.hrl

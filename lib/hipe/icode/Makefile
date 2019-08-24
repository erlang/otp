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
ifdef HIPE_ENABLED
HIPE_MODULES = hipe_icode_heap_test
else
HIPE_MODULES =
endif

DOC_MODULES = hipe_beam_to_icode \
	hipe_icode hipe_icode_bincomp \
	hipe_icode_callgraph hipe_icode_cfg hipe_icode_coordinator \
	hipe_icode_fp \
	hipe_icode_exceptions \
	hipe_icode_inline_bifs hipe_icode_instruction_counter \
	hipe_icode_liveness \
	hipe_icode_pp hipe_icode_primops \
	hipe_icode_range \
	hipe_icode_split_arith \
	hipe_icode_ssa hipe_icode_ssa_const_prop hipe_icode_call_elim \
	hipe_icode_ssa_copy_prop hipe_icode_ssa_struct_reuse \
	hipe_icode_type $(HIPE_MODULES)

MODULES = $(DOC_MODULES) hipe_icode_ebb hipe_icode_mulret 

HRL_FILES=hipe_icode.hrl hipe_icode_primops.hrl hipe_icode_type.hrl
ERL_FILES= $(MODULES:%=%.erl)
TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
DOC_FILES= $(DOC_MODULES:%=$(DOCS)/%.html)

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

ERL_COMPILE_FLAGS += -Werror +warn_unused_import +warn_export_vars +warn_missing_spec # +warn_untyped_record

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
	$(INSTALL_DIR) "$(RELSYSDIR)/icode"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)/icode"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

$(EBIN)/hipe_beam_to_icode.beam: hipe_icode_primops.hrl ../main/hipe.hrl ../../compiler/src/beam_disasm.hrl
$(EBIN)/hipe_icode.beam: ../main/hipe.hrl
$(EBIN)/hipe_icode_bincomp.beam: ../flow/cfg.hrl
$(EBIN)/hipe_icode_callgraph.beam: hipe_icode_primops.hrl
$(EBIN)/hipe_icode_cfg.beam: ../flow/hipe_bb.hrl ../flow/cfg.hrl ../flow/cfg.inc ../main/hipe.hrl
$(EBIN)/hipe_icode_ebb.beam: ../flow/cfg.hrl ../flow/ebb.inc
$(EBIN)/hipe_icode_exceptions.beam: ../flow/cfg.hrl
$(EBIN)/hipe_icode_fp.beam: ../flow/cfg.hrl
$(EBIN)/hipe_icode_heap_test.beam: ../main/hipe.hrl hipe_icode_primops.hrl ../flow/cfg.hrl ../rtl/hipe_literals.hrl
$(EBIN)/hipe_icode_inline_bifs.beam: ../flow/cfg.hrl
$(EBIN)/hipe_icode_instruction_counter.beam: ../main/hipe.hrl ../flow/cfg.hrl
$(EBIN)/hipe_icode_liveness.beam: ../flow/cfg.hrl ../flow/liveness.inc
$(EBIN)/hipe_icode_mulret.beam: ../main/hipe.hrl hipe_icode_primops.hrl
$(EBIN)/hipe_icode_primops.beam: hipe_icode_primops.hrl
$(EBIN)/hipe_icode_range.beam: ../main/hipe.hrl ../flow/cfg.hrl hipe_icode_primops.hrl
$(EBIN)/hipe_icode_split_arith.beam: ../main/hipe.hrl hipe_icode.hrl ../flow/cfg.hrl
$(EBIN)/hipe_icode_ssa.beam: ../main/hipe.hrl ../ssa/hipe_ssa.inc ../ssa/hipe_ssa_liveness.inc
$(EBIN)/hipe_icode_ssa_const_prop.beam: ../main/hipe.hrl hipe_icode_primops.hrl ../flow/cfg.hrl ../ssa/hipe_ssa_const_prop.inc
$(EBIN)/hipe_icode_ssa_copy_prop.beam: ../flow/cfg.hrl ../ssa/hipe_ssa_copy_prop.inc
$(EBIN)/hipe_icode_type.beam: hipe_icode_primops.hrl ../flow/cfg.hrl hipe_icode_type.hrl
$(EBIN)/hipe_icode_ssa_struct_reuse.beam: ../main/hipe.hrl hipe_icode_primops.hrl ../flow/cfg.hrl

$(TARGET_FILES): hipe_icode.hrl ../misc/hipe_consttab.hrl

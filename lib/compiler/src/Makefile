#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

ifdef BOOTSTRAP_COMPILER
EGEN=$(BOOTSTRAP_COMPILER)/egen
EBIN=$(BOOTSTRAP_COMPILER)/ebin
else
ifdef BOOTSTRAP
EGEN=$(BOOTSTRAP_TOP)/lib/compiler/egen
EBIN=$(BOOTSTRAP_TOP)/lib/compiler/ebin
endif
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COMPILER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/compiler-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES =  \
	beam_a \
	beam_asm \
	beam_block \
	beam_call_types \
	beam_clean \
	beam_dict \
	beam_digraph \
	beam_disasm \
	beam_flatten \
	beam_jump \
	beam_listing \
	beam_opcodes \
	beam_peep \
	beam_ssa \
	beam_ssa_bool \
	beam_ssa_bsm \
	beam_ssa_codegen \
	beam_ssa_dead \
	beam_ssa_funs \
	beam_ssa_lint \
	beam_ssa_opt \
	beam_ssa_pp \
	beam_ssa_pre_codegen \
	beam_ssa_recv \
	beam_ssa_share \
	beam_ssa_type \
	beam_kernel_to_ssa \
	beam_trim \
	beam_types \
	beam_utils \
	beam_validator \
        beam_z \
	cerl \
	cerl_clauses \
	cerl_inline \
	cerl_sets \
	cerl_trees \
	compile \
	core_lib \
	core_lint \
	core_parse \
	core_pp \
	core_scan \
	erl_bifs \
	rec_env \
	sys_core_alias \
	sys_core_bsm \
	sys_core_fold \
	sys_core_fold_lists \
	sys_core_inline \
	sys_core_prepare \
	sys_pre_attributes \
	v3_core \
	v3_kernel \
	v3_kernel_pp

BEAM_H = $(wildcard ../priv/beam_h/*.h)

HRL_FILES= \
	beam_disasm.hrl \
	beam_ssa_opt.hrl \
	beam_ssa.hrl \
	beam_types.hrl \
	core_parse.hrl \
	v3_kernel.hrl

YRL_FILE = core_parse.yrl

EXTRA_FILES= $(EGEN)/beam_opcodes.hrl

ERL_FILES= $(MODULES:%=%.erl)
INSTALL_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)
TARGET_FILES= $(INSTALL_FILES)
 
APP_FILE= compiler.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= compiler.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(NATIVE_LIBS_ENABLED),yes)
ERL_COMPILE_FLAGS += +native
else
ERL_COMPILE_FLAGS += -Werror
endif
ERL_COMPILE_FLAGS += +inline +warn_unused_import \
 -I../../stdlib/include -I$(EGEN) -W +warn_missing_spec

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)
	rm -f $(EGEN)/beam_opcodes.erl $(EGEN)/beam_opcodes.hrl
	rm -f $(EGEN)/core_parse.erl
	rm -f core

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(EGEN)/beam_opcodes.erl $(EGEN)/beam_opcodes.hrl: genop.tab
	$(gen_verbose)$(PERL) $(ERL_TOP)/erts/emulator/utils/beam_makeops -compiler -outdir $(EGEN) $<

$(EBIN)/beam_asm.beam: $(ESRC)/beam_asm.erl $(EGEN)/beam_opcodes.hrl
	$(V_ERLC) $(ERL_COMPILE_FLAGS) -DCOMPILER_VSN='"$(VSN)"' -o$(EBIN) $<

$(EBIN)/cerl_inline.beam: $(ESRC)/cerl_inline.erl
	$(V_ERLC) $(ERL_COMPILE_FLAGS) +nowarn_shadow_vars -o$(EBIN) $<

# Inlining core_parse is slow and has no benefit.
$(EBIN)/core_parse.beam: $(EGEN)/core_parse.erl
	$(V_ERLC) $(subst +inline,,$(ERL_COMPILE_FLAGS)) -o$(EBIN) $<

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(EXTRA_FILES) \
		$(YRL_FILE) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(INSTALL_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

# ----------------------------------------------------
# Dependencies -- alphabetically, please
# ---------------------------------------------------- 

$(EBIN)/beam_call_types.beam: beam_types.hrl
$(EBIN)/beam_disasm.beam: $(EGEN)/beam_opcodes.hrl beam_disasm.hrl
$(EBIN)/beam_listing.beam: core_parse.hrl v3_kernel.hrl beam_ssa.hrl
$(EBIN)/beam_kernel_to_ssa.beam: v3_kernel.hrl beam_ssa.hrl
$(EBIN)/beam_ssa.beam: beam_ssa.hrl
$(EBIN)/beam_ssa_bsm.beam: beam_ssa.hrl
$(EBIN)/beam_ssa_codegen.beam: beam_ssa.hrl
$(EBIN)/beam_ssa_dead.beam: beam_ssa.hrl
$(EBIN)/beam_ssa_funs.beam: beam_ssa.hrl
$(EBIN)/beam_ssa_lint.beam: beam_ssa.hrl
$(EBIN)/beam_ssa_opt.beam: beam_ssa.hrl
$(EBIN)/beam_ssa_pp.beam: beam_ssa.hrl
$(EBIN)/beam_ssa_pre_codegen.beam: beam_ssa.hrl
$(EBIN)/beam_ssa_recv.beam: beam_ssa.hrl
$(EBIN)/beam_ssa_share.beam: beam_ssa.hrl
$(EBIN)/beam_ssa_type.beam: beam_ssa.hrl beam_types.hrl
$(EBIN)/beam_types.beam: beam_types.hrl
$(EBIN)/beam_validator.beam: beam_types.hrl
$(EBIN)/cerl.beam: core_parse.hrl
$(EBIN)/compile.beam: core_parse.hrl ../../stdlib/include/erl_compile.hrl
$(EBIN)/core_lib.beam: core_parse.hrl
$(EBIN)/core_lint.beam: core_parse.hrl
$(EBIN)/core_parse.beam: core_parse.hrl $(EGEN)/core_parse.erl
$(EBIN)/core_pp.beam: core_parse.hrl
$(EBIN)/sys_core_alias.beam: core_parse.hrl
$(EBIN)/sys_core_fold.beam: core_parse.hrl
$(EBIN)/sys_core_fold_lists.beam: core_parse.hrl
$(EBIN)/sys_core_inline.beam: core_parse.hrl
$(EBIN)/v3_core.beam: core_parse.hrl
$(EBIN)/v3_kernel.beam: core_parse.hrl v3_kernel.hrl
$(EBIN)/v3_kernel_pp.beam: v3_kernel.hrl

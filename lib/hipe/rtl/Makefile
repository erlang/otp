#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2001-2010. All Rights Reserved.
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

ifndef EBIN
EBIN = ../ebin
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
HIPE_MODULES = hipe_rtl hipe_rtl_cfg \
	  hipe_rtl_liveness \
	  hipe_icode2rtl hipe_rtl_mk_switch \
	  hipe_rtl_primops \
	  hipe_rtl_varmap hipe_rtl_exceptions \
	  hipe_rtl_binary_match hipe_rtl_binary_construct \
	  hipe_rtl_arith_32 hipe_rtl_arith_64 \
	  hipe_rtl_ssa hipe_rtl_ssa_const_prop \
	  hipe_rtl_cleanup_const hipe_rtl_symbolic hipe_rtl_lcm \
	  hipe_rtl_ssapre hipe_rtl_binary hipe_rtl_ssa_avail_expr \
	  hipe_rtl_arch hipe_tagscheme
else
HIPE_MODULES =
endif

MODULES = $(HIPE_MODULES)

HRL_FILES= hipe_literals.hrl
ERL_FILES= $(MODULES:%=%.erl)
TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

# APP_FILE= 
# App_SRC= $(APP_FILE).src
# APP_TARGET= $(EBIN)/$(APP_FILE)
#
# APPUP_FILE= 
# APPUP_SRC= $(APPUP_FILE).src
# APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS: Please keep +inline below
# ----------------------------------------------------

include ../native.mk

ERL_COMPILE_FLAGS += +inline

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) 

docs:

clean:
	rm -f hipe_literals.hrl
	rm -f $(TARGET_FILES)
	rm -f core erl_crash.dump

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/rtl
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(RELSYSDIR)/rtl
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:


HIPE_MKLITERALS=$(ERL_TOP)/bin/$(TARGET)/hipe_mkliterals

hipe_literals.hrl: $(HIPE_MKLITERALS)
	$(HIPE_MKLITERALS) -e > hipe_literals.hrl

# Need to generate hipe.hrl from one and only one target in one and only
# one makefile; otherwise, clearmake will force rebuilds of hipe over and
# over again.
../main/hipe.hrl: ../vsn.mk ../main/hipe.hrl.src
	(cd ../main && $(MAKE) hipe.hrl)

$(EBIN)/hipe_rtl.beam: hipe_rtl.hrl ../main/hipe.hrl
$(EBIN)/hipe_rtl_arch.beam: hipe_rtl.hrl hipe_literals.hrl
$(EBIN)/hipe_rtl_binary.beam: hipe_rtl.hrl hipe_literals.hrl
$(EBIN)/hipe_rtl_bin_util.beam: hipe_rtl.hrl hipe_literals.hrl
$(EBIN)/hipe_rtl_cfg.beam: hipe_rtl.hrl ../flow/cfg.hrl ../flow/cfg.inc ../main/hipe.hrl
$(EBIN)/hipe_rtl_cleanup_const.beam: hipe_rtl.hrl
$(EBIN)/hipe_rtl_liveness.beam: hipe_rtl.hrl ../flow/cfg.hrl ../flow/liveness.inc
$(EBIN)/hipe_icode2rtl.beam: hipe_literals.hrl ../main/hipe.hrl ../icode/hipe_icode.hrl
$(EBIN)/hipe_tagscheme.beam: hipe_rtl.hrl hipe_literals.hrl
$(EBIN)/hipe_rtl_primops.beam: hipe_rtl.hrl ../icode/hipe_icode_primops.hrl hipe_literals.hrl ../main/hipe.hrl
$(EBIN)/hipe_rtl_arith_32.beam: ../main/hipe.hrl hipe_rtl_arith.inc
$(EBIN)/hipe_rtl_arith_64.beam: ../main/hipe.hrl hipe_rtl_arith.inc
$(EBIN)/hipe_rtl_bs_ops.beam: hipe_literals.hrl ../main/hipe.hrl
$(EBIN)/hipe_rtl_cerl_bs_ops.beam: ../main/hipe.hrl hipe_literals.hrl hipe_rtl.hrl
$(EBIN)/hipe_rtl_exceptions.beam: hipe_literals.hrl ../main/hipe.hrl
$(EBIN)/hipe_rtl_inline_bs_ops.beam: hipe_rtl.hrl hipe_literals.hrl ../main/hipe.hrl
$(EBIN)/hipe_rtl_mk_switch.beam: ../main/hipe.hrl
$(EBIN)/hipe_rtl_lcm.beam: ../flow/cfg.hrl hipe_rtl.hrl
$(EBIN)/hipe_rtl_symbolic.beam: hipe_rtl.hrl hipe_literals.hrl ../flow/cfg.hrl ../icode/hipe_icode_primops.hrl
$(EBIN)/hipe_rtl_varmap.beam: ../main/hipe.hrl ../icode/hipe_icode.hrl

$(EBIN)/hipe_rtl_ssa.beam: ../ssa/hipe_ssa.inc ../main/hipe.hrl ../ssa/hipe_ssa_liveness.inc hipe_rtl.hrl
$(EBIN)/hipe_rtl_ssa_const_prop.beam: hipe_rtl.hrl ../main/hipe.hrl ../flow/cfg.hrl ../ssa/hipe_ssa_const_prop.inc
$(EBIN)/hipe_rtl_ssapre.beam: ../main/hipe.hrl ../flow/cfg.hrl hipe_rtl.hrl

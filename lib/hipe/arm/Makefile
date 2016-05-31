#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
# Please keep this list sorted.
MODULES=hipe_arm \
	hipe_arm_assemble \
	hipe_arm_cfg \
	hipe_arm_defuse \
	hipe_arm_encode \
	hipe_arm_finalise \
	hipe_arm_frame \
	hipe_arm_liveness_gpr \
	hipe_arm_main \
	hipe_arm_pp \
	hipe_arm_ra \
	hipe_arm_ra_finalise \
	hipe_arm_ra_ls \
	hipe_arm_ra_naive \
	hipe_arm_ra_postconditions \
	hipe_arm_registers \
	hipe_rtl_to_arm

HRL_FILES=hipe_arm.hrl
ERL_FILES=$(MODULES:%=%.erl)
TARGET_FILES=$(MODULES:%=$(EBIN)/%.$(EMULATOR))
DOC_FILES= $(MODULES:%=$(DOCS)/%.html)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../native.mk

ERL_COMPILE_FLAGS += -Werror +warn_export_vars

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

# Please keep this list sorted.
$(EBIN)/hipe_arm_assemble.beam: ../main/hipe.hrl ../../kernel/src/hipe_ext_format.hrl ../rtl/hipe_literals.hrl
$(EBIN)/hipe_arm_cfg.beam: ../flow/cfg.hrl ../flow/cfg.inc
$(EBIN)/hipe_arm_frame.beam: ../rtl/hipe_literals.hrl
$(EBIN)/hipe_arm_liveness_gpr.beam: ../flow/liveness.inc
$(EBIN)/hipe_arm_registers.beam: ../rtl/hipe_literals.hrl
$(EBIN)/hipe_rtl_to_arm.beam: ../rtl/hipe_rtl.hrl

$(TARGET_FILES): hipe_arm.hrl ../misc/hipe_consttab.hrl

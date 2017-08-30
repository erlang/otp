#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
MODULES=hipe_amd64_assemble \
	hipe_amd64_defuse \
	hipe_amd64_encode \
	hipe_amd64_frame \
	hipe_amd64_liveness \
	hipe_amd64_main \
	hipe_amd64_pp \
	hipe_amd64_ra \
	hipe_amd64_ra_finalise \
	hipe_amd64_ra_ls \
	hipe_amd64_ra_naive \
	hipe_amd64_ra_postconditions \
	hipe_amd64_ra_sse2_postconditions \
	hipe_amd64_ra_x87_ls \
	hipe_amd64_registers \
	hipe_amd64_spill_restore \
	hipe_amd64_x87 \
	hipe_rtl_to_amd64

ERL_FILES=$(MODULES:%=%.erl)
TARGET_FILES=$(MODULES:%=$(EBIN)/%.$(EMULATOR))
DOC_FILES= $(MODULES:%=$(DOCS)/%.html)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../native.mk

ERL_COMPILE_FLAGS += -DHIPE_AMD64 -Werror +warn_export_vars

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
$(EBIN)/hipe_amd64_assemble.beam: ../main/hipe.hrl ../rtl/hipe_literals.hrl ../x86/hipe_x86.hrl ../../kernel/src/hipe_ext_format.hrl ../misc/hipe_sdi.hrl ../x86/hipe_x86_assemble.erl
$(EBIN)/hipe_amd64_defuse.beam: ../x86/hipe_x86.hrl ../x86/hipe_x86_defuse.erl
$(EBIN)/hipe_amd64_frame.beam: ../main/hipe.hrl ../x86/hipe_x86.hrl ../x86/hipe_x86_frame.erl ../rtl/hipe_literals.hrl
$(EBIN)/hipe_amd64_liveness.beam: ../flow/liveness.inc ../x86/hipe_x86_liveness.erl
$(EBIN)/hipe_amd64_main.beam: ../main/hipe.hrl ../x86/hipe_x86_main.erl
$(EBIN)/hipe_amd64_pp.beam: ../x86/hipe_x86.hrl ../x86/hipe_x86_pp.erl
$(EBIN)/hipe_amd64_ra.beam: ../main/hipe.hrl ../x86/hipe_x86_ra.erl
$(EBIN)/hipe_amd64_ra_dummy.beam: ../main/hipe.hrl ../x86/hipe_x86.hrl
$(EBIN)/hipe_amd64_ra_finalise.beam: ../x86/hipe_x86.hrl ../x86/hipe_x86_ra_finalise.erl
$(EBIN)/hipe_amd64_ra_ls.beam: ../main/hipe.hrl ../x86/hipe_x86_ra_ls.erl
$(EBIN)/hipe_amd64_ra_naive.beam: ../main/hipe.hrl ../x86/hipe_x86.hrl ../x86/hipe_x86_ra_naive.erl
$(EBIN)/hipe_amd64_ra_postconditions.beam: ../main/hipe.hrl ../x86/hipe_x86.hrl ../x86/hipe_x86_ra_postconditions.erl
$(EBIN)/hipe_amd64_ra_sse2_postconditions.beam: ../main/hipe.hrl
$(EBIN)/hipe_amd64_ra_x87_ls.beam: ../main/hipe.hrl ../x86/hipe_x86_ra_x87_ls.erl
$(EBIN)/hipe_amd64_registers.beam: ../rtl/hipe_literals.hrl
$(EBIN)/hipe_amd64_spill_restore.beam: ../main/hipe.hrl ../x86/hipe_x86.hrl ../flow/cfg.hrl ../x86/hipe_x86_spill_restore.erl
$(EBIN)/hipe_amd64_x87.beam: ../x86/hipe_x86_x87.erl
$(EBIN)/hipe_rtl_to_amd64.beam: ../x86/hipe_rtl_to_x86.erl ../rtl/hipe_rtl.hrl

$(TARGET_FILES): ../x86/hipe_x86.hrl ../misc/hipe_consttab.hrl

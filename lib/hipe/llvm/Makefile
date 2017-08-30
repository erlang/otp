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
HIPE_MODULES = elf_format \
	hipe_llvm \
	hipe_llvm_liveness \
	hipe_llvm_main \
	hipe_llvm_merge	\
	hipe_rtl_to_llvm
else
HIPE_MODULES =
endif

MODULES = $(HIPE_MODULES)

HRL_FILES= elf_format.hrl elf32_format.hrl elf64_format.hrl hipe_llvm_arch.hrl
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

ERL_COMPILE_FLAGS += -Werror +inline +warn_export_vars #+warn_missing_spec

# if in 32 bit backend define BIT32 symbol
ARCH = $(shell echo $(TARGET) | sed 's/^\(x86_64\)-.*/64bit/')
ifneq ($(ARCH), 64bit)
ERL_COMPILE_FLAGS += -DBIT32
endif

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs:

clean:
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
	$(INSTALL_DIR) $(RELSYSDIR)/llvm
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(RELSYSDIR)/llvm
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:

$(EBIN)/elf_format.beam: elf_format.hrl elf32_format.hrl  elf64_format.hrl
$(EBIN)/hipe_llvm_main.beam: ../../kernel/src/hipe_ext_format.hrl \
	hipe_llvm_arch.hrl elf_format.hrl elf32_format.hrl  elf64_format.hrl
$(EBIN)/hipe_llvm_merge.beam: ../../kernel/src/hipe_ext_format.hrl \
	hipe_llvm_arch.hrl ../rtl/hipe_literals.hrl ../main/hipe.hrl
$(EBIN)/hipe_rtl_to_llvm.beam: ../rtl/hipe_rtl.hrl ../rtl/hipe_literals.hrl \
	hipe_llvm_arch.hrl

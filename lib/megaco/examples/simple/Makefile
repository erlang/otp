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

include $(ERL_TOP)/make/target.mk

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug -W
endif

EBIN = .
MEGACO_INCLUDEDIR = ../../include

include $(ERL_TOP)/make/$(TARGET)/otp.mk


# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(MEGACO_VSN)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/megaco-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

MODULES = $(MG_MODULES) $(MGC_MODULES) $(COMMON_MODULES)

ERL_FILES = $(MODULES:%=%.erl)	

TARGET_FILES = \
	$(ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR))

MEGACO_ROOT_DIR = $(shell (cd .. ; dirname `pwd`))

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ifeq ($(WARN_UNUSED_WARS),true)
ERL_COMPILE_FLAGS += +warn_unused_vars
endif

ERL_COMPILE_FLAGS += \
	-pa $(ERL_TOP)/lib/megaco/ebin \
	-I../../include

ifneq ($(MGC_HOST),)
MG_START_ARGS = "{mgc_host, $(MGC_HOST)}"
endif

ifneq ($(MG_INLINE_TRACE),true)
MG_MEGACO_FILTER  = -s megaco_filter 
MG_START_ARGS    += "{trace,false}"
else
MG_START_ARGS    += "{trace,true}"
endif

ifneq ($(MG_DEBUG),)
MG_START_ARGS += "{debug,true}"
else
MG_START_ARGS += "{debug,false}"
endif


ifneq ($(MGC_INLINE_TRACE),true)
MGC_MEGACO_FILTER  = -s megaco_filter 
MGC_START_ARGS    += "{trace,false}"
else
MGC_START_ARGS    += "{trace,true}"
endif

ifneq ($(MGC_DEBUG),)
MGC_START_ARGS += "{debug,true}"
else
MGC_START_ARGS += "{debug,false}"
endif


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
opt: $(TARGET_FILES) 

debug:
	@${MAKE} TYPE=debug opt

clean:
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

docs:

info: 
	@echo "MEGACO_ROOT_DIR = $(MEGACO_ROOT_DIR)"

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

mg: opt
	erl -noshell -pa $(MEGACO_ROOT_DIR)/ebin \
          $(MG_MEGACO_FILTER) \
          -s megaco \
          -s megaco_simple_mg start_batch $(MG_START_ARGS)

mgc: opt
	erl -noshell -pa $(MEGACO_ROOT_DIR)/ebin \
          $(MGC_MEGACO_FILTER) \
          -s megaco \
          -s megaco_simple_mgc start_batch $(MGC_START_ARGS)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/examples"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples/simple"
	$(INSTALL_DATA) $(ERL_FILES) $(TARGET_FILES) "$(RELSYSDIR)/examples/simple"


release_docs_spec:


# ----------------------------------------------------
# Include dependencies
# ----------------------------------------------------


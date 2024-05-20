# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2001-2024. All Rights Reserved.
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
	+nowarn_missing_doc +nowarn_missing_spec_documented \
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


DIA_PLT      = megaco_example_simple.plt
DIA_ANALYSIS = $(basename $(DIA_PLT)).dialyzer_analysis
ifeq ($(DIAW_EH),true)
DIA_WARNINGS += -Werror_handling
endif
ifeq ($(DIAW_US),true)
DIA_WARNINGS += -Wunderspecs
endif
ifeq ($(DIAW_UR),true)
DIA_WARNINGS += -Wunmatched_returns
endif
DIA_PLT_APPS = \
	erts asn1 et kernel stdlib compiler debugger \
	runtime_tools crypto mnesia wx


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
opt: $(TARGET_FILES) 

$(filter-out opt, $(TYPES)):
	@${MAKE} TYPE=$@ opt

clean:
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

docs:

info:
	@echo "MEGACO_ROOT_DIR = $(MEGACO_ROOT_DIR)"
	@echo ""
	@echo "DIA_PLT      = $(DIA_PLT)"
	@echo "DIA_ANALYSIS = $(DIA_ANALYSIS)"
	@echo "DIA_PLT_APPS = $(DIA_PLT_APPS)"
	@echo ""


dclean:
	rm -f $(DIA_PLT)
	rm -f $(DIA_ANALYSIS)

dialyzer_plt: $(DIA_PLT)

$(DIA_PLT): $(ERL_FILES)
	@echo "Building $(basename $(DIA_PLT)) plt file"
	@dialyzer --build_plt \
                  --output_plt $@ \
                  --apps --apps $(sort $(APPLICATION) $(DIA_PLT_APPS)) \
                  --output $(DIA_ANALYSIS) \
                  --verbose

dialyzer: $(DIA_PLT)
	@echo "Running dialyzer on megaco example simple"
	@dialyzer --plt $< \
                  -r ../../ebin \
                  $(DIA_WARNINGS) \
                  --verbose


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


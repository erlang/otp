#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2006-2024. All Rights Reserved.
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
include ../../vsn.mk

VSN = $(SNMP_VSN)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifneq ($(MIBS_VERBOSITY),) 
SNMP_FLAGS += +'{verbosity,$(MIBS_VERBOSITY)}'
endif

ERL_COMPILE_FLAGS += -pa $(ERL_TOP)/lib/snmp/ebin
ERL_COMPILE_FLAGS += -I../include \
                     -Dversion=\"$(VSN)$(PRE_VSN)\" \
                     +'{parse_transform,sys_pre_attributes}' \
                     +'{attribute,insert,app_vsn,$(APP_VSN)}' \
                     -I$(ERL_TOP)/lib/stdlib \
		     +nowarn_missing_doc +nowarn_missing_spec_documented \
                     $(SNMP_FLAGS)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR         = $(RELEASE_PATH)/lib/snmp-$(VSN)
EXAMPLE_RELSYSDIR = "$(RELSYSDIR)/examples"
EX2_RELSYSDIR     = $(EXAMPLE_RELSYSDIR)/ex2

# ----------------------------------------------------
# Common macros
# ----------------------------------------------------

MODULES = \
	snmp_ex2_manager \
	snmp_ex2_simple_standard_test

ERL_FILES = $(MODULES:%=%.erl)

TARGET_FILES= \
	$(ERL_FILES:%.erl=%.$(EMULATOR))


DIA_PLT      = snmp_example_ex2.plt
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
DIA_PLT_APPS = erts kernel stdlib crypto mnesia runtime_tools compiler


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TYPES): build

clean:
	rm -f $(TARGET_FILES)
	rm -f core *~

docs:

build: $(TARGET_FILES)


dclean:
	rm -f $(DIA_PLT)
	rm -f $(DIA_ANALYSIS)

dialyzer_plt: $(DIA_PLT)

$(DIA_PLT): $(ERL_FILES)
	@echo "Building ($(basename $(DIA_PLT))) plt file"
	@dialyzer --build_plt \
                  --output_plt $@ \
                  --apps $(sort snmp $(DIA_PLT_APPS)) \
                  --output $(DIA_ANALYSIS) \
                  --verbose

dialyzer: $(DIA_PLT)
	@echo "Running dialyzer on snmp example ex2"
	@dialyzer --plt $< \
                  $(ERL_TOP)/lib/snmp/examples/ex2 \
                  $(DIA_WARNINGS) \
                  --verbose


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(EXAMPLE_RELSYSDIR)
	$(INSTALL_DIR) $(EX2_RELSYSDIR)
	$(INSTALL_DATA) $(ERL_FILES) $(EX2_RELSYSDIR)
	$(INSTALL_DATA) $(TARGET_FILES) $(EX2_RELSYSDIR)

release_docs_spec:

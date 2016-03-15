# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

EBIN = ../../ebin
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

ERL_FILES = \
	$(MODULES:%=%.erl) \
	$(INTERNAL_YRL_FILES:%.yrl=%.erl)

ERL_TARGET_FILES = \
	$(INTERNAL_YRL_FILES:%.yrl=%.erl)

BEAM_TARGET_FILES = \
	$(INTERNAL_YRL_FILES:%.yrl=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

TARGET_FILES = $(ERL_TARGET_FILES) $(BEAM_TARGET_FILES)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug
endif

include ../app/megaco.mk

ERL_COMPILE_FLAGS += \
	$(MEGACO_ERL_COMPILE_FLAGS) \
	-I../../include

# YRL_FLAGS += -pa /clearcase/otp/tools/parsetools/ebin

ifeq ($(YRL_VERBOSE),true)
YRL_FLAGS += -v 
endif


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug:
	@${MAKE} TYPE=debug opt

opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

docs:

info:
	@echo "MODULES            = $(MODULES)"
	@echo ""
	@echo "ERL_FILES          = $(ERL_FILES)"
	@echo ""
	@echo "ERL_TARGET_FILES   = $(ERL_TARGET_FILES)"
	@echo ""
	@echo "BEAM_TARGET_FILES  = $(BEAM_TARGET_FILES)"
	@echo ""
	@echo "TARGET_FILES       = $(TARGET_FILES)"
	@echo ""
	@echo "INTERNAL_YRL_FILES = $(INTERNAL_YRL_FILES)"
	@echo ""
	@echo "INTERNAL_HRL_FILES = $(INTERNAL_HRL_FILES)"
	@echo ""


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

parser: parser_v1 parser_v2 parser_prev3a parser_prev3b

parser_v1: megaco_text_parser_v1.$(EMULATOR)

parser_v2: megaco_text_parser_v2.$(EMULATOR)

parser_prev3a: megaco_text_parser_prev3a.$(EMULATOR)

parser_prev3b: megaco_text_parser_prev3b.$(EMULATOR)


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR)  "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(BEAM_TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR)  "$(RELSYSDIR)/src"
	$(INSTALL_DIR)  "$(RELSYSDIR)/src/text"
	$(INSTALL_DATA) $(ERL_FILES) $(INTERNAL_YRL_FILES) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src/text"


release_docs_spec:


# ----------------------------------------------------
# Include dependencies
# ----------------------------------------------------

include depend.mk


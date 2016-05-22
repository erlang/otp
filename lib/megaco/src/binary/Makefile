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
MEGACO_ENGINEDIR = ../engine

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

ASN1_SPECS = $(ASN1_V1_SPEC) \
             $(ASN1_V2_SPEC) \
             $(ASN1_PREV3A_SPEC) 
ASN1_FILES = $(ASN1_SPECS:%=%.asn)

V1_SPECS = $(BER_ASN1_V1_SPEC)         \
           $(PER_ASN1_V1_SPEC)

V2_SPECS = $(BER_ASN1_V2_SPEC)         \
           $(PER_ASN1_V2_SPEC)

PREV3A_SPECS = $(BER_ASN1_PREV3A_SPEC)         \
               $(PER_ASN1_PREV3A_SPEC)

PREV3B_SPECS = $(BER_ASN1_PREV3B_SPEC)         \
               $(PER_ASN1_PREV3B_SPEC)

PREV3C_SPECS = $(BER_ASN1_PREV3C_SPEC)         \
               $(PER_ASN1_PREV3C_SPEC)

V3_SPECS = $(BER_ASN1_V3_SPEC)         \
           $(PER_ASN1_V3_SPEC)         \
           $(PREV3A_SPECS) $(PREV3B_SPECS) $(PREV3C_SPECS)

SPECS = $(V1_SPECS) $(V2_SPECS) $(V3_SPECS)

V1_SPEC_ASN1DB = $(V1_SPECS:%=%.asn1db)
V2_SPEC_ASN1DB = $(V2_SPECS:%=%.asn1db)
PREV3A_SPEC_ASN1DB = $(PREV3A_SPECS:%=%.asn1db)
PREV3B_SPEC_ASN1DB = $(PREV3B_SPECS:%=%.asn1db)
PREV3C_SPEC_ASN1DB = $(PREV3C_SPECS:%=%.asn1db)
V3_SPEC_ASN1DB = $(V3_SPECS:%=%.asn1db) \
                 $(PREV3A_SPEC_ASN1DB) \
                 $(PREV3B_SPEC_ASN1DB) \
                 $(PREV3C_SPEC_ASN1DB)
SPEC_ASN1DB = $(V1_SPEC_ASN1DB) \
              $(V2_SPEC_ASN1DB)\
              $(V3_SPEC_ASN1DB)

V1_SPEC_BINS = $(V1_SPECS:%=%.erl) $(V1_SPECS:%=%.hrl) 
V2_SPEC_BINS = $(V2_SPECS:%=%.erl) $(V2_SPECS:%=%.hrl) 
PREV3A_SPEC_BINS = $(PREV3A_SPECS:%=%.erl) $(PREV3A_SPECS:%=%.hrl) 
PREV3B_SPEC_BINS = $(PREV3B_SPECS:%=%.erl) $(PREV3B_SPECS:%=%.hrl) 
PREV3C_SPEC_BINS = $(PREV3C_SPECS:%=%.erl) $(PREV3C_SPECS:%=%.hrl) 
V3_SPEC_BINS = $(V3_SPECS:%=%.erl) $(V3_SPECS:%=%.hrl) \
               $(PREV3A_SPEC_BINS) \
               $(PREV3B_SPEC_BINS) \
               $(PREV3C_SPEC_BINS) 
SPEC_BINS    = $(V1_SPEC_BINS) $(V2_SPEC_BINS) $(V3_SPEC_BINS) 

ERL_FILES = $(MODULES:%=%.erl)	
HRL_FILES = $(INTERNAL_HRL_FILES) \
	$(V1_SPECS:%=%.hrl) \
	$(V2_SPECS:%=%.hrl) \
	$(V3_SPECS:%=%.hrl)

TARGET_FILES = \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))


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


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug:
	@${MAKE} TYPE=debug opt

opt: prebuild $(TARGET_FILES) 

prebuild: prebuild.skip

prebuild.skip: 
	$(gen_verbose)
	$(V_colon)@echo "Building prebuild.skip\c"
	@touch prebuild.skip
	@for a in $(SPEC_ASN1DB); do \
		echo $$a >> prebuild.skip; \
	done
	$(V_colon)@echo ""

v1: $(V2_SPEC_BINS)

v2: $(V2_SPEC_BINS)

v3: $(V3_SPEC_BINS)

specs: v1 v2 v3

clean:
	rm -f $(TARGET_FILES) 
	rm -f $(SPEC_BINS)
	rm -f $(SPEC_ASN1DB)
	rm -f prebuild.skip
	rm -f core *~

docs:

info:
	@echo "MODULES:       $(MODULES)"
	@echo "ERL_FILES:     $(ERL_FILES)"
	@echo "HRL_FILES:     $(HRL_FILES)"
	@echo "TARGET_FILES:  $(TARGET_FILES)"
	@echo "ASN1_SPECS:    $(ASN1_SPECS)"
	@echo "SPECS:         $(SPECS)"
	@echo "SPEC_BINS:     $(SPEC_BINS)"


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR)  "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR)  "$(RELSYSDIR)/src"
	$(INSTALL_DIR)  "$(RELSYSDIR)/src/binary"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(ASN1_FILES) "$(RELSYSDIR)/src/binary"


release_docs_spec:


# ----------------------------------------------------
# Include dependencies
# ----------------------------------------------------

include depend.mk


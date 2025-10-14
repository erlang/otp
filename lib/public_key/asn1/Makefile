#
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

EGEN = ../src
ESRC = ../src

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(PUBLIC_KEY_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/public_key-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

.SUFFIXES: .asn1
.PRECIOUS: $(ESRC)/%.erl $(ESRC)/%.hrl

INCLUDE = ../include
EBIN = ../ebin

ASN_TOP = PKCS-FRAME $(ASN_MODULES)

ASN_MODULES = \
          AlgorithmInformation-2009 \
          AttributeCertificateVersion1-2009 \
          CryptographicMessageSyntax-2009 \
          CryptographicMessageSyntaxAlgorithms-2009 \
          CMSAesRsaesOaep-2009 \
          DSS \
          ECPrivateKey \
	  X509-ML-DSA-2025 \
          OCSP-2024-08 \
          OTP-PKIX \
	  OTP-PKIX-Relaxed \
          PKCS-1 PKCS-3 PKCS-10 \
          PKIX-CommonTypes-2009 \
          PKIX-X400Address-2009 \
          PKIX1-PSS-OAEP-Algorithms-2009 \
          PKIX1Explicit-2009 \
          PKIX1Implicit-2009 \
          PKIXAlgs-2009 \
	  PKIXCRMF-2009 \
          EnrollmentMessageSyntax-2009 \
          PKIXAttributeCertificate-2009 \
	  RFC5639 \
          Safecurves-pkix-18 \
          SecureMimeMessageV3dot1-2009

ASN_ASNS = $(ASN_MODULES:%=%.asn1)
ASN_ERLS = $(ASN_TOP:%=$(ESRC)/%.erl)
ASN_HRLS = PKCS-FRAME.hrl
ASN_CONFIGS = OTP-PKIX.asn1config OTP-PKIX-Relaxed.asn1config PKIX1Explicit-2009.asn1config
ASN_DBS = $(ASN_MODULES:%=%.asn1db)
ASN_TABLES = $(ASN_MODULES:%=%.table)

ERL_MODULES = $(ASN_TOP) $(GEN_MODULES)

TARGET_FILES= $(ERL_MODULES:%=$(EBIN)/%.$(EMULATOR))

HRL_FILES = $(ASN_HRLS:%=$(INCLUDE)/%)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
EXTRA_ERLC_FLAGS = 
ERL_COMPILE_FLAGS += $(EXTRA_ERLC_FLAGS)

ASN_FLAGS = -bber +der +noobj +asn1config

ifeq ($(ERL_DETERMINISTIC),yes)
	ASN_FLAGS += +deterministic
endif

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TYPES): $(TARGET_FILES) $(HRL_FILES)

clean:
	-rm -f $(ASN_ERLS) $(ASN_HRLS) $(HRL_FILES) $(ASN_DBS) \
		$(ASN_TABLES)  $(TARGET_FILES) *.beam *~

docs:

$(ESRC)/%.erl $(ESRC)/%.hrl: %.set.asn
	$(asn_verbose)erlc $(ASN_FLAGS) -o $(ESRC) $<

$(ESRC)/%.erl $(ESRC)/%.hrl: %.asn1
	$(asn_verbose)erlc $(ASN_FLAGS) -o $(ESRC) $<

$(INCLUDE)/%.hrl: $(ESRC)/%.hrl
	$(gen_verbose)cp -p $< $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/include"
	$(INSTALL_DIR) "$(RELSYSDIR)/asn1"
	$(INSTALL_DATA) $(ASN_ASNS) $(ASN_CONFIGS) \
		  "$(RELSYSDIR)/asn1"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ASN_ERLS) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

#
# Dependencies
#

$(EBIN)/PKCS-FRAME.beam: 	$(ESRC)/PKCS-FRAME.erl $(INCLUDE)/PKCS-FRAME.hrl
$(ESRC)/PKCS-FRAME.erl $(INCLUDE)/PKCS-FRAME.hrl $(ESRC)/PKCS-FRAME.asn1db: \
        AsymmetricKeyPackageModuleV1.asn1 \
        PKCS5v2-0.asn1

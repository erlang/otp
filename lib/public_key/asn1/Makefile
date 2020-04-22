#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
.PRECIOUS: %.erl

ASN_TOP = OTP-PUB-KEY PKCS-FRAME
ASN_MODULES = PKIX1Explicit88 PKIX1Implicit88 PKIX1Algorithms88 \
	PKIXAttributeCertificate PKCS-1 PKCS-3 PKCS-7 PKCS-8 PKCS-10 PKCS5v2-0 OTP-PKIX \
	InformationFramework RFC5639 CMSAesRsaesOaep OCSP-2013-88
ASN_ASNS = $(ASN_MODULES:%=%.asn1)
ASN_ERLS = $(ASN_TOP:%=%.erl)
ASN_HRLS = $(ASN_TOP:%=%.hrl)
ASN_CONFIGS = OTP-PUB-KEY.asn1config
ASN_DBS = $(ASN_MODULES:%=%.asn1db) OTP-PUB-KEY.asn1db  
ASN_TABLES = $(ASN_MODULES:%=%.table)

GEN_MODULES = 
GEN_ERLS    = $(GEN_MODULES:%=%.erl)
ERL_MODULES = $(ASN_TOP) $(GEN_MODULES)

TARGET_FILES= $(ERL_MODULES:%=$(EBIN)/%.$(EMULATOR))

HRL_FILES = $(ASN_HRLS:%=$(INCLUDE)/%)

INCLUDE = ../include
EBIN = ../ebin

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
EXTRA_ERLC_FLAGS = 
ERL_COMPILE_FLAGS += $(EXTRA_ERLC_FLAGS)

ASN_FLAGS = -bber +der +noobj +asn1config

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(HRL_FILES)

clean:
	-rm -f $(ASN_ERLS) $(GEN_ERLS) $(ASN_HRLS) $(HRL_FILES) $(ASN_DBS) \
		$(ASN_TABLES)  $(TARGET_FILES) *.beam *~

docs:

%.erl %.hrl: %.set.asn
	$(asn_verbose)erlc $(ASN_FLAGS) $<

$(INCLUDE)/%.hrl: %.hrl
	$(gen_verbose)cp -p $<  $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/include"
	$(INSTALL_DIR) "$(RELSYSDIR)/asn1"
	$(INSTALL_DATA) $(ASN_ASNS) $(ASN_ERLS) $(ASN_HRLS) $(ASN_CONFIGS) \
		 $(GEN_ERLS) "$(RELSYSDIR)/asn1"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

#
# Dependencies

$(EBIN)/OTP-PUB-KEY.beam: 	OTP-PUB-KEY.erl OTP-PUB-KEY.hrl
OTP-PUB-KEY.erl OTP-PUB-KEY.hrl:	OTP-PUB-KEY.asn1db
OTP-PUB-KEY.asn1db:		PKIX1Algorithms88.asn1 \
				PKIX1Explicit88.asn1 \
				PKIX1Implicit88.asn1 \
				PKIXAttributeCertificate.asn1 \
				PKCS-1.asn1\
				PKCS-3.asn1\
		                PKCS-7.asn1\
				PKCS-10.asn1\
				InformationFramework.asn1\
				OTP-PKIX.asn1 \
				RFC5639.asn1 \
				OCSP-2013-88.asn1

$(EBIN)/PKCS-FRAME.beam: 	        PKCS-FRAME.erl PKCS-FRAME.hrl
PKCS-FRAME.erl PKCS-FRAME.hrl:		PKCS-FRAME.asn1db
PKCS-FRAME.asn1db:			PKCS5v2-0.asn1\
					PKCS-8.asn1\

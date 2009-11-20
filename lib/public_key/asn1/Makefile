#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2008-2009. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
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

ASN_TOP = OTP-PUB-KEY
ASN_MODULES = PKIX1Explicit88 PKIX1Implicit88 PKIX1Algorithms88 \
	PKIXAttributeCertificate OTP-PKIX 
ASN_ASNS = $(ASN_MODULES:%=%.asn1)
ASN_ERLS = $(ASN_TOP).erl
ASN_HRLS = $(ASN_TOP).hrl
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

ASN_FLAGS = -bber_bin +der +compact_bit_string +optimize +noobj +asn1config +inline

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(HRL_FILES)

clean:
	-rm -f $(ASN_ERLS) $(GEN_ERLS) $(ASN_HRLS) $(HRL_FILES) $(ASN_DBS) \
		$(ASN_TABLES)  $(TARGET_FILES) *.beam *~

docs:

%.erl: %.set.asn
	erlc $(ASN_FLAGS) $<

$(HRL_FILES):	$(ASN_HRLS)
	cp -p $(ASN_HRLS) $(INCLUDE)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(HRL_FILES) $(RELSYSDIR)/include
	$(INSTALL_DIR) $(RELSYSDIR)/asn1
	$(INSTALL_DATA) $(ASN_ASNS) $(ASN_ERLS) $(ASN_HRLS) $(ASN_CONFIGS) \
		 $(GEN_ERLS) $(RELSYSDIR)/asn1
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:

#
# Dependencies

$(EBIN)/OTP-PUB-KEY.beam: 	OTP-PUB-KEY.erl OTP-PUB-KEY.hrl
OTP-PUB-KEY.erl OTP-PUB-KEY.hrl:	OTP-PUB-KEY.asn1db
OTP-PUB-KEY.asn1db:		PKIX1Algorithms88.asn1 \
				PKIX1Explicit88.asn1 \
				PKIX1Implicit88.asn1 \
				PKIXAttributeCertificate.asn1 \
				OTP-PKIX.asn1

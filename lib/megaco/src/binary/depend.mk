#-*-makefile-*-   ; force emacs to enter makefile-mode

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

# Flag description:
# 
# +asn1config
# This is only used by the ber, and means that
# some partial decode functions will be created 
# (as described by the asn1config file).
# 
# +inline
# This means that the ASN.1 runtime library will be inlined.
#

ASN1_CT_OPTS += +noobj +legacy_erlang_types
ifeq ($(MEGACO_INLINE_ASN1_RT),true)
# We need atleast version 1.4.6 of the ANS.1 application
ASN1_CT_OPTS += +inline
endif

BER_V1_FLAGS             = $(ASN1_CT_OPTS) +asn1config
BER_V2_FLAGS             = $(ASN1_CT_OPTS) +asn1config
BER_PREV3A_FLAGS         = $(ASN1_CT_OPTS) +asn1config
BER_PREV3B_FLAGS         = $(ASN1_CT_OPTS) +asn1config
BER_PREV3C_FLAGS         = $(ASN1_CT_OPTS) +asn1config
BER_V3_FLAGS             = $(ASN1_CT_OPTS) +asn1config
PER_V1_FLAGS             = $(ASN1_CT_OPTS)
PER_V2_FLAGS             = $(ASN1_CT_OPTS)
PER_PREV3A_FLAGS         = $(ASN1_CT_OPTS)
PER_PREV3B_FLAGS         = $(ASN1_CT_OPTS)
PER_PREV3C_FLAGS         = $(ASN1_CT_OPTS)
PER_V3_FLAGS             = $(ASN1_CT_OPTS)


# --- Version 1 ---

$(BER_ASN1_V1_SPEC).erl: \
	$(BER_ASN1_V1_SPEC).set.asn \
	$(ASN1_V1_SPEC).asn
	$(V_colon)@echo "$(BER_ASN1_V1_SPEC):"
	$(asn_verbose)$(ERLC) -bber $(BER_V1_FLAGS) $(BER_ASN1_V1_SPEC).set.asn

$(EBIN)/$(BER_ASN1_V1_SPEC).$(EMULATOR): \
	$(BER_ASN1_V1_SPEC).erl

$(PER_ASN1_V1_SPEC).erl: \
	$(PER_ASN1_V1_SPEC).set.asn \
	$(ASN1_V1_SPEC).asn
	$(V_colon)@echo "$(PER_ASN1_V1_SPEC):"
	$(asn_verbose)$(ERLC) -bper $(PER_V1_FLAGS) $(PER_ASN1_V1_SPEC).set.asn

$(EBIN)/$(PER_ASN1_V1_SPEC).$(EMULATOR): \
	$(PER_ASN1_V1_SPEC).erl


# --- Version 2 ---

$(BER_ASN1_V2_SPEC).erl: \
	$(BER_ASN1_V2_SPEC).set.asn \
	$(ASN1_V2_SPEC).asn
	$(V_colon)@echo "$(BER_ASN1_V2_SPEC):"
	$(asn_verbose)$(ERLC) -bber $(BER_V2_FLAGS) $(BER_ASN1_V2_SPEC).set.asn

$(EBIN)/$(BER_ASN1_V2_SPEC).$(EMULATOR): \
	$(BER_ASN1_V2_SPEC).erl

$(PER_ASN1_V2_SPEC).erl: \
	$(PER_ASN1_V2_SPEC).set.asn \
	$(ASN1_V2_SPEC).asn
	$(V_colon)@echo "$(PER_ASN1_V2_SPEC):"
	$(asn_verbose)$(ERLC) -bper $(PER_V2_FLAGS) $(PER_ASN1_V2_SPEC).set.asn

$(EBIN)/$(PER_ASN1_V2_SPEC).$(EMULATOR): \
	$(PER_ASN1_V2_SPEC).erl

# --- Version 3 ---

# -- (prev3a) --

$(BER_ASN1_PREV3A_SPEC).erl: \
	$(BER_ASN1_PREV3A_SPEC).set.asn \
	$(ASN1_PREV3A_SPEC).asn
	$(V_colon)@echo "$(BER_ASN1_PREV3A_SPEC):"
	$(asn_verbose)$(ERLC) -bber $(BER_PREV3A_FLAGS) $(BER_ASN1_PREV3A_SPEC).set.asn

$(EBIN)/$(BER_ASN1_PREV3A_SPEC).$(EMULATOR): \
	$(BER_ASN1_PREV3A_SPEC).erl

$(PER_ASN1_PREV3A_SPEC).erl: \
	$(PER_ASN1_PREV3A_SPEC).set.asn \
	$(ASN1_PREV3A_SPEC).asn
	$(V_colon)@echo "$(PER_ASN1_PREV3A_SPEC):"
	$(asn_verbose)$(ERLC) -bper $(PER_PREV3A_FLAGS) $(PER_ASN1_PREV3A_SPEC).set.asn

$(EBIN)/$(PER_ASN1_PREV3A_SPEC).$(EMULATOR): \
	$(PER_ASN1_PREV3A_SPEC).erl


# -- (prev3b) --

$(BER_ASN1_PREV3B_SPEC).erl: \
	$(BER_ASN1_PREV3B_SPEC).set.asn \
	$(ASN1_PREV3B_SPEC).asn
	$(V_colon)@echo "$(BER_ASN1_PREV3B_SPEC):"
	$(asn_verbose)$(ERLC) -bber $(BER_PREV3B_FLAGS) $(BER_ASN1_PREV3B_SPEC).set.asn

$(EBIN)/$(BER_ASN1_PREV3B_SPEC).$(EMULATOR): \
	$(BER_ASN1_PREV3B_SPEC).erl

$(PER_ASN1_PREV3B_SPEC).erl: \
	$(PER_ASN1_PREV3B_SPEC).set.asn \
	$(ASN1_PREV3B_SPEC).asn
	$(V_colon)@echo "$(PER_ASN1_PREV3B_SPEC):"
	$(asn_verbose)$(ERLC) -bper $(PER_PREV3B_FLAGS) $(PER_ASN1_PREV3B_SPEC).set.asn

$(EBIN)/$(PER_ASN1_PREV3B_SPEC).$(EMULATOR): \
	$(PER_ASN1_PREV3B_SPEC).erl


# -- (prev3c) --

$(BER_ASN1_PREV3C_SPEC).erl: \
	$(BER_ASN1_PREV3C_SPEC).set.asn \
	$(ASN1_PREV3C_SPEC).asn
	$(V_colon)@echo "$(BER_ASN1_PREV3C_SPEC):"
	$(asn_verbose)$(ERLC) -bber $(BER_PREV3C_FLAGS) $(BER_ASN1_PREV3C_SPEC).set.asn

$(EBIN)/$(BER_ASN1_PREV3C_SPEC).$(EMULATOR): \
	$(BER_ASN1_PREV3C_SPEC).erl

$(PER_ASN1_PREV3C_SPEC).erl: \
	$(PER_ASN1_PREV3C_SPEC).set.asn \
	$(ASN1_PREV3C_SPEC).asn
	$(V_colon)@echo "$(PER_ASN1_PREV3C_SPEC):"
	$(asn_verbose)$(ERLC) -bper $(PER_PREV3C_FLAGS) $(PER_ASN1_PREV3C_SPEC).set.asn

$(EBIN)/$(PER_ASN1_PREV3C_SPEC).$(EMULATOR): \
	$(PER_ASN1_PREV3C_SPEC).erl


# -- (v3) --

$(BER_ASN1_V3_SPEC).erl: \
	$(BER_ASN1_V3_SPEC).set.asn \
	$(ASN1_V3_SPEC).asn
	$(V_colon)@echo "$(BER_ASN1_V3_SPEC):"
	$(asn_verbose)$(ERLC) -bber $(BER_V3_FLAGS) $(BER_ASN1_V3_SPEC).set.asn

$(EBIN)/$(BER_ASN1_V3_SPEC).$(EMULATOR): \
	$(BER_ASN1_V3_SPEC).erl

$(PER_ASN1_V3_SPEC).erl: \
	$(PER_ASN1_V3_SPEC).set.asn \
	$(ASN1_V3_SPEC).asn
	$(V_colon)@echo "$(PER_ASN1_V3_SPEC):"
	$(asn_verbose)$(ERLC) -bper $(PER_V3_FLAGS) $(PER_ASN1_V3_SPEC).set.asn

$(EBIN)/$(PER_ASN1_V3_SPEC).$(EMULATOR): \
	$(PER_ASN1_V3_SPEC).erl


# -------------

$(EBIN)/megaco_ber_encoder.$(EMULATOR): megaco_ber_encoder.erl \
        $(MEGACO_ENGINEDIR)/megaco_message_internal.hrl

$(EBIN)/megaco_per_encoder.$(EMULATOR): megaco_per_encoder.erl \
        $(MEGACO_ENGINEDIR)/megaco_message_internal.hrl

$(EBIN)/megaco_binary_encoder_lib.$(EMULATOR): megaco_binary_encoder_lib.erl \
        $(MEGACO_ENGINEDIR)/megaco_message_internal.hrl

$(EBIN)/megaco_binary_encoder.$(EMULATOR): megaco_binary_encoder.erl \
        $(MEGACO_ENGINEDIR)/megaco_message_internal.hrl

$(EBIN)/megaco_binary_name_resolver_v1.$(EMULATOR): \
	megaco_binary_name_resolver_v1.erl \
	../app/megaco_internal.hrl

$(EBIN)/megaco_binary_name_resolver_v2.$(EMULATOR): \
	megaco_binary_name_resolver_v2.erl \
	../app/megaco_internal.hrl

$(EBIN)/megaco_binary_name_resolver_prev3a.$(EMULATOR): \
	megaco_binary_name_resolver_prev3a.erl \
	../app/megaco_internal.hrl

$(EBIN)/megaco_binary_name_resolver_prev3b.$(EMULATOR): \
	megaco_binary_name_resolver_prev3b.erl \
	../app/megaco_internal.hrl

$(EBIN)/megaco_binary_name_resolver_prev3c.$(EMULATOR): \
	megaco_binary_name_resolver_prev3c.erl \
	../app/megaco_internal.hrl

$(EBIN)/megaco_binary_name_resolver_v3.$(EMULATOR): \
	megaco_binary_name_resolver_v3.erl

$(EBIN)/megaco_binary_term_id.$(EMULATOR): megaco_binary_term_id.erl

$(EBIN)/megaco_binary_term_id_gen.$(EMULATOR): megaco_binary_term_id_gen.erl

$(EBIN)/megaco_binary_transformer_v1.$(EMULATOR): \
	megaco_binary_transformer_v1.erl \
	../app/megaco_internal.hrl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

$(EBIN)/megaco_binary_transformer_v2.$(EMULATOR): \
	megaco_binary_transformer_v2.erl \
	../app/megaco_internal.hrl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v2.hrl

$(EBIN)/megaco_binary_transformer_prev3a.$(EMULATOR): \
	megaco_binary_transformer_prev3a.erl \
	../app/megaco_internal.hrl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_prev3a.hrl

$(EBIN)/megaco_binary_transformer_prev3b.$(EMULATOR): \
	megaco_binary_transformer_prev3b.erl \
	../app/megaco_internal.hrl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_prev3b.hrl

$(EBIN)/megaco_binary_transformer_prev3c.$(EMULATOR): \
	megaco_binary_transformer_prev3c.erl \
	../app/megaco_internal.hrl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_prev3c.hrl

$(EBIN)/megaco_binary_transformer_v3.$(EMULATOR): \
	megaco_binary_transformer_v3.erl \
	../app/megaco_internal.hrl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v3.hrl


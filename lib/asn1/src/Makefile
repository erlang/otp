#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
#
# Copyright (C) 1997, Ericsson Telecommunications
# Author: Kenneth Lundin 
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(ASN1_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/asn1-$(VSN)




#
# Common Macros
#

EBIN = ../ebin

EVAL_CT_MODULES = asn1ct_eval_ext

CT_MODULES= \
	asn1ct \
	asn1ct_check \
	asn1_db \
	asn1ct_pretty_format \
	asn1ct_func \
	asn1ct_gen \
	asn1ct_gen_check \
	asn1ct_gen_per \
	asn1ct_name \
	asn1ct_constructed_per \
	asn1ct_constructed_ber_bin_v2 \
	asn1ct_gen_ber_bin_v2 \
	asn1ct_imm \
	asn1ct_rtt \
	asn1ct_value \
	asn1ct_tok \
	asn1ct_parser2 \
	asn1ct_table \
	$(EVAL_CT_MODULES)

RT_MODULES= \
	asn1rt_nif

MODULES= $(CT_MODULES) $(RT_MODULES) 

ERL_FILES = $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

GENERATED_PARSER = $(PARSER_MODULE:%=%.erl)

# internal hrl file
HRL_FILES = asn1_records.hrl

APP_FILE = asn1.app
APPUP_FILE = asn1.appup

APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

EXAMPLES = \
	../examples/P-Record.asn

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(USE_ASN1_HIPE),true)
ERL_COMPILE_FLAGS += +native
endif

ERL_COMPILE_FLAGS += \
	-I$(ERL_TOP)/lib/stdlib \
	-Werror

YRL_FLAGS = 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)


clean:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) $(GENERATED_PARSER)
	rm -f core *~

docs:


info:
	@echo "PARSER_SRC: $(PARSER_SRC)"
	@echo "INSTALL_DATA: $(INSTALL_DATA)"
# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EBIN)/asn1ct.$(EMULATOR):asn1ct.erl
	 $(V_ERLC) -b$(EMULATOR) -o$(EBIN) $(ERL_COMPILE_FLAGS) -Dvsn=\"$(VSN)\" $<

$(EBIN)/asn1ct_func.$(EMULATOR): asn1ct_func.erl
	 $(V_ERLC) -o$(EBIN) $(ERL_COMPILE_FLAGS) -I../rt_templates $<

asn1ct_eval_%.erl: asn1ct_eval_%.funcs
	$(gen_verbose)erl -pa $(EBIN) -noshell -noinput \
	   -run prepare_templates gen_asn1ct_eval $<

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@


# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA)  $(ERL_FILES) $(HRL_FILES) $(APP_SRC) $(APPUP_SRC) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples"
	$(INSTALL_DATA) $(EXAMPLES) "$(RELSYSDIR)/examples"

# there are no include files to be used by the user
#$(INSTALL_DIR) "$(RELSYSDIR)/include"
#$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/include"

release_docs_spec:

#
# Run-time library template files.
#

RT_TEMPLATES = asn1rtt_check \
	       asn1rtt_ext \
	       asn1rtt_per_common \
               asn1rtt_real_common \
	       asn1rtt_ber \
	       asn1rtt_per \
	       asn1rtt_uper

RT_TEMPLATES_ERL = $(RT_TEMPLATES:%=%.erl)
RT_TEMPLATES_TARGET = $(RT_TEMPLATES:%=%.$(EMULATOR))

asn1ct_rtt.erl: prepare_templates.$(EMULATOR) $(RT_TEMPLATES_TARGET)
	$(gen_verbose)erl -noshell -noinput -run prepare_templates gen_asn1ct_rtt \
           $(RT_TEMPLATES_TARGET)

prepare_templates.$(EMULATOR): prepare_templates.erl
	$(V_ERLC) prepare_templates.erl

asn1rtt_%.$(EMULATOR): asn1rtt_%.erl
	$(V_ERLC) +debug_info $<

$(EVAL_CT_MODULES:%=%.erl): prepare_templates.$(EMULATOR) \
			    $(EBIN)/asn1ct_rtt.$(EMULATOR) \
			    $(EBIN)/asn1ct_func.$(EMULATOR) \

#
# Dependencies
#

$(EBIN)/asn1_app.beam: asn1_app.erl
$(EBIN)/asn1_db.beam: asn1_db.erl
$(EBIN)/asn1ct.beam: asn1ct.erl asn1_records.hrl
$(EBIN)/asn1ct_check.beam: asn1ct_check.erl asn1_records.hrl
$(EBIN)/asn1ct_constructed_ber_bin_v2.beam: asn1ct_constructed_ber_bin_v2.erl \
  asn1_records.hrl
$(EBIN)/asn1ct_constructed_per.beam: asn1ct_constructed_per.erl asn1_records.hrl
$(EBIN)/asn1ct_func.beam: asn1ct_func.erl
$(EBIN)/asn1ct_gen.beam: asn1ct_gen.erl asn1_records.hrl
$(EBIN)/asn1ct_gen_ber_bin_v2.beam: asn1ct_gen_ber_bin_v2.erl asn1_records.hrl
$(EBIN)/asn1ct_gen_check.beam: asn1_records.hrl
$(EBIN)/asn1ct_gen_per.beam: asn1ct_gen_per.erl asn1_records.hrl
$(EBIN)/asn1ct_gen_per_rt2ct.beam: asn1ct_gen_per_rt2ct.erl asn1_records.hrl
$(EBIN)/asn1ct_imm.beam: asn1ct_imm.erl
$(EBIN)/asn1ct_name.beam: asn1ct_name.erl
$(EBIN)/asn1ct_parser2.beam: asn1ct_parser2.erl asn1_records.hrl
$(EBIN)/asn1ct_pretty_format.beam: asn1ct_pretty_format.erl
$(EBIN)/asn1ct_table.beam: asn1ct_table.erl
$(EBIN)/asn1ct_tok.beam: asn1ct_tok.erl
$(EBIN)/asn1ct_value.beam: asn1ct_value.erl asn1_records.hrl
$(EBIN)/asn1rt.beam: asn1rt.erl
$(EBIN)/asn1rt_ber_bin.beam: asn1rt_ber_bin.erl asn1_records.hrl
$(EBIN)/asn1rt_ber_bin_v2.beam: asn1rt_ber_bin_v2.erl
$(EBIN)/asn1rt_check.beam: asn1rt_check.erl
$(EBIN)/asn1rt_nif.beam: asn1rt_nif.erl
$(EBIN)/asn1rt_per_bin_rt2ct.beam: asn1rt_per_bin_rt2ct.erl asn1_records.hrl
$(EBIN)/asn1rt_uper_bin.beam: asn1rt_uper_bin.erl asn1_records.hrl

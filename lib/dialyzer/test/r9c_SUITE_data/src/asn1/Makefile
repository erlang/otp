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
# PARSER_SRC = \
#	asn1ct_parser.yrl

# PARSER_MODULE=$(PARSER_SRC:%.yrl=%)

EBIN = ../ebin
CT_MODULES= \
	asn1ct \
	asn1ct_check \
	asn1_db \
	asn1ct_pretty_format \
	asn1ct_gen \
	asn1ct_gen_per \
	asn1ct_gen_per_rt2ct \
	asn1ct_name \
	asn1ct_constructed_per \
	asn1ct_constructed_ber \
	asn1ct_gen_ber \
	asn1ct_constructed_ber_bin_v2 \
	asn1ct_gen_ber_bin_v2 \
	asn1ct_value \
	asn1ct_tok \
	asn1ct_parser2

RT_MODULES= \
	asn1rt \
	asn1rt_per \
	asn1rt_per_bin \
	asn1rt_per_v1 \
	asn1rt_ber_bin \
	asn1rt_ber_bin_v2 \
	asn1rt_per_bin_rt2ct \
	asn1rt_driver_handler \
	asn1rt_check

#	asn1rt_ber_v1 \
#	asn1rt_ber \
# the rt module to use is defined in asn1_records.hrl
# and must be updated when an incompatible change is done in the rt modules


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
ERL_FLAGS +=
ERL_COMPILE_FLAGS += \
	-I$(ERL_TOP)/lib/stdlib \
	+warn_unused_vars
YRL_FLAGS =
# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)


clean:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) $(GENERATED_PARSER)
	rm -f core *~

docs:


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EBIN)/asn1ct.$(EMULATOR):asn1ct.erl
	 $(ERLC) -b$(EMULATOR) -o$(EBIN) $(ERL_COMPILE_FLAGS) -Dvsn=\"$(VSN)\" $<

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@


# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) $(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(PARSER_SRC) $(ERL_FILES) $(HRL_FILES) $(APP_SRC) $(APPUP_SRC) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/examples
	$(INSTALL_DATA) $(EXAMPLES) $(RELSYSDIR)/examples

# there are no include files to be used by the user
#$(INSTALL_DIR) $(RELSYSDIR)/include
#$(INSTALL_DATA) $(HRL_FILES) $(RELSYSDIR)/include

release_docs_spec:

# 
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

EBIN = .

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk

VSN=$(INETS_VSN)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR         = $(RELEASE_PATH)/lib/inets-$(VSN)
EXAMPLE_RELSYSDIR = "$(RELSYSDIR)/examples"
HDLT_RELSYSDIR    = $(EXAMPLE_RELSYSDIR)/httpd_load_test


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

ERL_FILES = $(MODULES:%=%.erl)

SOURCE = $(ERL_FILES) $(INTERNAL_HRL_FILES)

TARGET_FILES =  \
	$(ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR))

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug -W
endif


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../../src/inets_app/inets.mk

ERL_COMPILE_FLAGS += \
	$(INETS_FLAGS) \
	$(INETS_ERL_COMPILE_FLAGS) \
	-I../../include


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------


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


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR)  $(EXAMPLE_RELSYSDIR)
	$(INSTALL_DIR)  $(HDLT_RELSYSDIR)
	$(INSTALL_DATA) $(SCRIPT_SKELETONS) $(HDLT_RELSYSDIR)
	$(INSTALL_DATA) $(CONF_SKELETONS)   $(HDLT_RELSYSDIR)
	$(INSTALL_DATA) $(CERT_FILES)       $(HDLT_RELSYSDIR)
	$(INSTALL_DATA) $(TARGET_FILES)     $(HDLT_RELSYSDIR)
	$(INSTALL_DATA) $(ERL_FILES)        $(HDLT_RELSYSDIR)


release_docs_spec:


# ----------------------------------------------------
# Include dependencies
# ----------------------------------------------------

megaco_codec_transform.$(EMULATOR): megaco_codec_transform.erl

megaco_codec_meas.$(EMULATOR): megaco_codec_meas.erl

megaco_codec_mstone1.$(EMULATOR): megaco_codec_mstone1.erl

megaco_codec_mstone2.$(EMULATOR): megaco_codec_mstone2.erl

megaco_codec_mstone_lib.$(EMULATOR): megaco_codec_mstone_lib.erl


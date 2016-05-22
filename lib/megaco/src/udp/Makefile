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

ERL_FILES = $(MODULES:%=%.erl)	

TARGET_FILES =  \
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

opt: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

docs:


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR)  "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR)  "$(RELSYSDIR)/src"
	$(INSTALL_DIR)  "$(RELSYSDIR)/src/udp"
	$(INSTALL_DATA) $(ERL_FILES) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src/udp"


release_docs_spec:


# ----------------------------------------------------
# Include dependencies
# ----------------------------------------------------

$(EBIN)/megaco_udp.$(EMULATOR): megaco_udp.erl \
	megaco_udp.hrl $(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_udp_server.$(EMULATOR): megaco_udp_server.erl \
	megaco_udp.hrl $(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_udp_sup.$(EMULATOR): megaco_udp_sup.erl \
	megaco_udp.hrl $(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_udp_test.$(EMULATOR): megaco_udp_test.erl \
	megaco_udp.hrl $(MEGACO_INCLUDEDIR)/megaco.hrl


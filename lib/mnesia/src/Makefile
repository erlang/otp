#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
include $(ERL_TOP)/make/target.mk

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug -W
endif
 
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(MNESIA_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/mnesia-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES= \
	mnesia \
	mnesia_app \
	mnesia_backend_type \
	mnesia_backup \
	mnesia_bup \
	mnesia_checkpoint \
	mnesia_checkpoint_sup \
	mnesia_controller \
	mnesia_dumper\
	mnesia_event \
	mnesia_ext_sup \
	mnesia_frag \
	mnesia_frag_hash \
	mnesia_index \
	mnesia_kernel_sup \
	mnesia_late_loader \
	mnesia_lib\
	mnesia_loader \
	mnesia_locker \
	mnesia_log \
	mnesia_monitor \
	mnesia_recover \
	mnesia_registry \
	mnesia_schema\
	mnesia_snmp_hook \
	mnesia_subscr \
	mnesia_sup \
	mnesia_sp  \
	mnesia_text \
	mnesia_tm

HRL_FILES= mnesia.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= mnesia.app

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= mnesia.appup

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)



# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += \
	-Werror \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,vsn,"mnesia_$(MNESIA_VSN)"}'

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt: $(TARGET_FILES)

debug:
	@${MAKE} TYPE=debug

clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:

$(TARGET_FILES): $(HRL_FILES)

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(HRL_FILES) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:


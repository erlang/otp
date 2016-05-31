#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

ifndef EBIN
EBIN = ../ebin
endif

ifndef DOCS
DOCS = ../doc
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(HIPE_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/hipe-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES = cerl_cconv cerl_closurean cerl_hipeify cerl_lib \
	  cerl_messagean cerl_pmatch cerl_prettypr cerl_to_icode \
	  cerl_typean erl_bif_types erl_types 

HRL_FILES= cerl_hipe_primops.hrl
ERL_FILES= $(MODULES:%=%.erl)
TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
DOC_FILES= $(MODULES:%=$(DOCS)/%.html)

# APP_FILE= 
# APP_SRC= $(APP_FILE).src
# APP_TARGET= $(EBIN)/$(APP_FILE)
#
# APPUP_FILE= 
# APPUP_SRC= $(APPUP_FILE).src
# APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../native.mk

ERL_COMPILE_FLAGS += +inline -Werror +warn_export_vars +warn_unused_import +warn_missing_spec #+warn_untyped_record

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) 

docs:

# Moved docs target to edocs so the standard docs rule work properly.
edocs: $(DOC_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

distclean: clean
realclean: clean

$(DOCS)/%.html:%.erl
	erl -noshell -run edoc_run file '"$<"' '[{dir, "$(DOCS)"}]' -s init stop

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------



# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/cerl"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)/cerl"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

$(EBIN)/cerl_cconv.beam: cerl_hipe_primops.hrl
$(EBIN)/cerl_hipeify.beam: cerl_hipe_primops.hrl
$(EBIN)/cerl_to_icode.beam: cerl_hipe_primops.hrl ../icode/hipe_icode_primops.hrl

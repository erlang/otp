#
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
ifdef HIPE_ENABLED
HIPE_MODULES = hipe
else
HIPE_MODULES =
endif
MODULES = hipe_main $(HIPE_MODULES)

## hipe.hrl is automatically generated from hipe.hrl.src -- see below
HRL_FILES= hipe.hrl
ERL_FILES= $(MODULES:%=%.erl)
TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)
DOC_FILES= $(MODULES:%=$(DOCS)/%.html)

APP_FILE= hipe.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= hipe.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../native.mk

ERL_COMPILE_FLAGS += -Werror +nowarn_shadow_vars +warn_export_vars +warn_missing_spec +warn_untyped_record

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

hipe.hrl: ../vsn.mk hipe.hrl.src
	$(vsn_verbose)sed -e "s;%VSN%;$(HIPE_VSN);" ../../hipe/main/hipe.hrl.src > ../../hipe/main/hipe.hrl

$(EBIN)/hipe.beam: hipe.hrl ../../compiler/src/beam_disasm.hrl
$(EBIN)/hipe_main.beam: hipe.hrl ../icode/hipe_icode.hrl #../rtl/hipe_rtl.hrl

debug opt: $(TARGET_FILES) 

docs: 

# Moved docs target to edocs so the standard docs rule work properly.

edocs: $(DOC_FILES)

clean:
	rm -f $(TARGET_FILES) $(DOC_FILES) $(HRL_FILES)
	rm -f core

distclean: clean
realclean: clean

$(DOCS)/%.html:%.erl
	$(gen_verbose)erl -noshell -run edoc_run file '"$<"' '[{dir, "$(DOCS)"}]' -s init stop

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
	$(INSTALL_DATA) ../vsn.mk "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELSYSDIR)/main"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)/main"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

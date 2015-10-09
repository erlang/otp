#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2012. All Rights Reserved.
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
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(GS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/gs-$(VSN)/contribs

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
       mandel

HRL_FILES= 

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR)) $(TARGET_TOOLBOX_FILES)

TOOLNAME = mandel

EXTRA_FILES= $(TOOLNAME).html
TOOLBOX_FILES= $(TOOLNAME).tool $(TOOLNAME).gif
TARGET_TOOLBOX_FILES= $(TOOLBOX_FILES:%=$(EBIN)/%)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)
	rm -f core

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EBIN)/$(TOOLNAME).gif: $(TOOLNAME).gif
	$(gen_verbose)rm -f $@
	$(V_at)cp $(TOOLNAME).gif $@

$(EBIN)/$(TOOLNAME).tool: $(TOOLNAME).tool
	$(gen_verbose)rm -f $@
	$(V_at)cp $(TOOLNAME).tool $@

$(EBIN)/help.gif: help.gif
	$(gen_verbose)rm -f $@
	$(V_at)cp help.gif $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/mandel/bitmaps"
	$(INSTALL_DATA) $(BITMAPS) $(TOOLBOX_FILES) "$(RELSYSDIR)/mandel/bitmaps"
	$(INSTALL_DIR) "$(RELSYSDIR)/mandel"
	$(INSTALL_DATA) $(ERL_FILES) $(EXTRA_FILES) "$(RELSYSDIR)/mandel"

release_docs_spec:

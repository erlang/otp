#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

include ../vsn.mk
ifdef TERTIARY_BOOTSTRAP
 VSN = $(WX_VSN)
 include $(ERL_TOP)/make/target.mk
 include $(ERL_TOP)/make/$(TARGET)/otp.mk
 RELSYSDIR = $(RELEASE_PATH)/lib/wx-$(VSN)
else # Normal build
 include ../config.mk
endif

ESRC = .
EGEN = gen
EBIN = ../ebin
ERLC = erlc
ERLINC = ../include
ERL_COMPILE_FLAGS += -I$(ERLINC) +warn_unused_vars

ARCHIVE         = wx-$(VSN).ez

ErlMods         = wx \
		wx_object \
                wxe_master \
                wxe_server \
		wxe_util 

ERL_FILES       = $(ErlMods:%=%.erl)

HRL_FILES	= wxe.hrl       

GEN_FILES       = $(wildcard gen/wx*.erl) \
		gen/glu.erl \
		gen/gl.erl

GEN_MODS        = $(GEN_FILES:gen/%.erl=%,)

GEN_HRL		= \
		$(EGEN)/wxe_debug.hrl \
		$(EGEN)/wxe_funcs.hrl

EXT_HRL		= $(ERLINC)/wx.hrl \
		$(ERLINC)/gl.hrl \
		$(ERLINC)/glu.hrl

TARGET_FILES    = $(ErlMods:%=$(EBIN)/%.beam) $(GEN_FILES:gen/%.erl=$(EBIN)/%.beam) 
HEADER_FILES    = $(HRL_FILES) $(GEN_HRL) $(EXT_HRL)

APP_FILE   = wx.app
APP_SRC    = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_FILE   = wx.appup
APPUP_SRC    = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

# Targets
ifdef TERTIARY_BOOTSTRAP
 opt: $(EBIN)/wx_object.beam
else
 debug opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)
endif

clean:  
	rm -f $(TARGET_FILES) 
	rm -f $(APP_TARGET) $(APPUP_TARGET)
	rm -f *~

complete_clean: 
	rm -f $(TARGET_FILES) 
	rm -f $(APP_TARGET) $(APPUP_TARGET)
	rm -f $(GEN_FILES)
	rm -f $(GenHrl)
	rm -f *~
	rm -f Makefile

docs:

archive: opt
	(cd ../..; zip -0 wx/$(ARCHIVE) wx wx/ebin wx/ebin/*)

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk Makefile
	$(gen_verbose)sed -e 's;%GEN_MODS%;$(GEN_MODS);' $< > $@.tmp
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $@.tmp > $@
	$(V_at)rm $@.tmp

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk Makefile
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

# Rules
$(EBIN)/%.beam: $(ESRC)/%.erl $(HEADER_FILES)
	$(V_ERLC) -W -bbeam $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(EBIN)/%.beam: $(EGEN)/%.erl $(HEADER_FILES)
	$(V_ERLC) -W -bbeam $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk
release_spec: opt 
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(HRL_FILES) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/src/gen"
	$(INSTALL_DATA) $(GEN_HRL) $(GEN_FILES) "$(RELSYSDIR)/src/gen"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(EXT_HRL) "$(RELSYSDIR)/include"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(APP_TARGET) $(APPUP_TARGET) "$(RELSYSDIR)/ebin"
#	$(INSTALL_DATA) ../$(ARCHIVE) $(RELEASE_PATH)/lib

release_docs_spec:

#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2008-2009. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# %CopyrightEnd%
#

include ../vsn.mk
include ../config.mk

ESRC = .
EGEN = gen
EBIN = ../ebin
ERLC = erlc
ERLINC = ../include
ERL_COMPILE_FLAGS += -I$(ERLINC) +warn_unused_vars

ARCHIVE         = wx-$(VSN).ez

ErlMods         = \
		wx \
		wx_object \
                wxe_master \
                wxe_server \
		wxe_util 

ERL_FILES       = $(ErlMods:%=%.erl)

HRL_FILES	= wxe.hrl       

GEN_FILES       = $(wildcard gen/wx*.erl) \
		gen/glu.erl \
		gen/gl.erl

GEN_HRL		= \
		$(EGEN)/gl_debug.hrl \
		$(EGEN)/wxe_debug.hrl \
		$(EGEN)/wxe_funcs.hrl

EXT_HRL		= $(ERLINC)/wx.hrl \
		$(ERLINC)/gl.hrl \
		$(ERLINC)/glu.hrl

TARGET_FILES    = $(ErlMods:%=$(EBIN)/%.beam) $(GEN_FILES:gen/%.erl=$(EBIN)/%.beam) 
HEADER_FILES    = $(HRL_FILES) $(GEN_HRL) $(EXT_HRL)

# Targets
debug opt: $(TARGET_FILES)

clean:  
	rm -f $(TARGET_FILES) 
	rm -f *~

complete_clean: 
	rm -f $(TARGET_FILES) 
	rm -f $(GEN_FILES)
	rm -f $(GenHrl)
	rm -f *~
	rm -f Makefile

docs:

archive: opt
	(cd ../..; zip -0 wx/$(ARCHIVE) wx wx/ebin wx/ebin/*)


# Rules
$(EBIN)/%.beam: $(ESRC)/%.erl $(HEADER_FILES)
	$(ERLC) -W -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $< 

$(EBIN)/%.beam: $(EGEN)/%.erl $(HEADER_FILES)
	$(ERLC) -W -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $< 

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
ifeq ($(INSIDE_ERLSRC), true)
include $(ERL_TOP)/make/otp_release_targets.mk
release_spec: opt 
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(HRL_FILES) $(ERL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/src/gen
	$(INSTALL_DATA) $(GEN_HRL) $(GEN_FILES) $(RELSYSDIR)/src/gen
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(EXT_HRL) $(RELSYSDIR)/include
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
#	$(INSTALL_DATA) ../$(ARCHIVE) $(RELEASE_PATH)/lib

release_docs_spec:
endif

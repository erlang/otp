#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
EBIN = .
ERLC = erlc
PWD = $(shell pwd)
APPDIR = $(shell dirname $(PWD))
ERL_COMPILE_FLAGS = -pa $(APPDIR)/ebin

Mods =  wxt wx_test_lib \
	wx_app_SUITE \
	wx_basic_SUITE \
	wx_event_SUITE \
	wx_class_SUITE \
	wx_xtra_SUITE \
	wx_opengl_SUITE

ErlSrc          = $(Mods:%=%.erl)
ErlTargets      = $(Mods:%=$(EBIN)/%.beam) 

# Targets
opt debug: $(ErlTargets)

clean:  
	rm -f $(ErlTargets) 
	rm -f core erl_crash.dump
	rm -f *~

docs: 


# Rules
ifneq ($(INSIDE_ERLSRC),true)

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) -W -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

else
RELSYSDIR = $(RELEASE_PATH)/wx_test
include $(ERL_TOP)/make/otp_release_targets.mk
release_spec: 

release_tests_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) wx.spec wx_test_lib.hrl $(ErlSrc) $(ErlTargets) $(RELSYSDIR)
	$(INSTALL_SCRIPT) wxt $(RELSYSDIR)

release_docs_spec:

endif

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
include ../config.mk

ESRC = .
EBIN = .
ERLC = erlc
PWD = $(shell pwd)
APPDIR = $(shell dirname $(PWD))
ERL_COMPILE_FLAGS = -pa $(APPDIR)/ebin

Mods =  wxt wx_test_lib wx_obj_test \
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

RELSYSDIR = $(RELEASE_PATH)/wx_test
include $(ERL_TOP)/make/otp_release_targets.mk
release_spec: 

release_tests_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) wx.spec wx.cover wx_test_lib.hrl $(ErlSrc) $(ErlTargets) "$(RELSYSDIR)"
	$(INSTALL_SCRIPT) wxt "$(RELSYSDIR)"

release_docs_spec:

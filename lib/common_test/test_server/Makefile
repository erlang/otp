#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk

# ----------------------------------------------------
# Configuration info.
# ----------------------------------------------------
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

EBIN=.

TS_MODULES= \
	ts \
	ts_run \
	ts_lib \
	ts_make \
	ts_erl_config \
	ts_autoconf_win32 \
	ts_install \
	ts_install_cth \
	ts_benchmark

TARGET_MODULES= $(MODULES:%=$(EBIN)/%)
TS_TARGET_MODULES= $(TS_MODULES:%=$(EBIN)/%)

TS_ERL_FILES = $(TS_MODULES:=.erl)
TS_HRL_FILES = ts.hrl
AUTOCONF_FILES = configure.in conf_vars.in
PROGRAMS = configure config.sub config.guess install-sh 
CONFIG = ts.config ts.unix.config ts.win32.config

TS_TARGET_FILES = $(TS_MODULES:%=$(EBIN)/%.$(EMULATOR))

TS_TARGETS = $(TS_MODULES:%=$(EBIN)/%.$(EMULATOR))

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -I../include -Werror

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TS_TARGETS)

clean:
	rm -f $(TS_TARGET_FILES)
	rm -f core

docs:

configure: configure.in
	autoconf configure.in > configure

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_tests_spec: opt
	$(INSTALL_DIR) "$(RELEASE_PATH)/test_server"
	$(INSTALL_DATA) $(TS_ERL_FILES) $(TS_HRL_FILES) \
		$(TS_TARGET_FILES) \
		$(AUTOCONF_FILES) $(CONFIG) \
		"$(RELEASE_PATH)/test_server"
	$(INSTALL_SCRIPT) $(PROGRAMS) "$(RELEASE_PATH)/test_server"

release_docs_spec:


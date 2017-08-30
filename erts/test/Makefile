#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

include $(ERL_TOP)/make/$(TARGET)/otp.mk

EBIN = .

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	erlc_SUITE \
	install_SUITE \
	nt_SUITE \
	otp_SUITE \
	ethread_SUITE \
	erl_print_SUITE \
	run_erl_SUITE \
	erlexec_SUITE \
	z_SUITE \
	upgrade_SUITE

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

EXTRA_FILES = install_SUITE_data/install_bin

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/system_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGET_FILES) $(EXTRA_FILES)

install_SUITE_data/install_bin: ../../make/install_bin
	rm -f $@
	cp -p $< $@

clean:
	rm -f $(TARGET_FILES) $(EXTRA_FILES)
	rm -f core *~

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec:

release_tests_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) system.spec system.dynspec system_smoke.spec \
		$(ERL_FILES) $(TARGET_FILES) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	tar cf - *_SUITE_data utils | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:

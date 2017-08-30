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

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	os_mon_SUITE \
	disksup_SUITE \
	memsup_SUITE \
	cpu_sup_SUITE \
	os_mon_mib_SUITE \
	os_sup_SUITE \
	os_mon_conf

EBIN = .

HRL_FILES=

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

SOURCE = $(ERL_FILES) $(HRL_FILES)

EMAKEFILE=Emakefile

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/os_mon_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_MAKE_FLAGS +=
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/snmp/include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

make_emakefile:
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES)\
	> $(EMAKEFILE)

tests debug opt: make_emakefile
	erl $(ERL_MAKE_FLAGS) -make

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES)
	rm -f core *~

docs:


# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec:

release_tests_spec: make_emakefile
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) os_mon.spec os_mon.cover os_mon_smoke.spec \
		$(EMAKEFILE) $(SOURCE) "$(RELSYSDIR)"
	$(INSTALL_DATA) os_mon_mib_SUITE.cfg "$(RELSYSDIR)"

##	tar chf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:

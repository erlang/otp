#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
# Application version
# ----------------------------------------------------
include ../vsn.mk

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/jinterface_test

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
TEST_SPEC_FILE = jinterface.spec jinterface_smoke.spec
COVER_FILE = jinterface.cover

MODULES = nc_SUITE \
	  jinterface_SUITE

GEN_MODULES = jitu

ERL_FILES = $(MODULES:%=%.erl) $(GEN_MODULES:%=%.erl)

GEN_TARGET_FILES = $(GEN_MODULES:%=%.$(EMULATOR))

SUITE_TARGET_FILES = $(MODULES:%=%.$(EMULATOR))

TARGET_FILES = \
	$(GEN_TARGET_FILES) \
	$(SUITE_TARGET_FILES)

# ----------------------------------------------------
# PROGRAMS
# ----------------------------------------------------

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
tests debug opt:

clean:
	rm -f $(TARGET_FILES)
	rm -f errs core *~

docs:

# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------


# ----------------------------------------------------
# Release Targets
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec:

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(TEST_SPEC_FILE) $(COVER_FILE) $(ERL_FILES) "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

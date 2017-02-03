#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2012-2016. All Rights Reserved.
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


INCLUDES= -I. -I ../include

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	eldap_basic_SUITE \
	make_certs


ERL_FILES= $(MODULES:%=%.erl)

HRL_FILES=

TARGET_FILES= \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

SPEC_FILES = eldap.spec

COVER_FILE = eldap.cover


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/eldap_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += $(INCLUDES) -pa $(ERL_TOP)/lib/eldap/ebin

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGET_FILES)


clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(SPEC_FILES) $(ERL_FILES) $(COVER_FILE) $(HRL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
#	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
release_docs_spec:

#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2011-2012. All Rights Reserved.
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
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	sasl_SUITE \
	alarm_handler_SUITE \
	installer \
	release_handler_SUITE \
	systools_SUITE \
	systools_rc_SUITE \
	overload_SUITE \
	rb_SUITE \
	rh_test_lib

ERL_FILES= $(MODULES:%=%.erl)

HRL_FILES= test_lib.hrl

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/sasl_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_MAKE_FLAGS +=
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/test_server/include \
		-I$(ERL_TOP)/lib/sasl/src

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

make_emakefile:
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make'\
	> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES)\
	>> $(EMAKEFILE)

tests debug opt: make_emakefile
	erl $(ERL_MAKE_FLAGS) -make

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: make_emakefile
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) sasl.spec sasl.cover $(EMAKEFILE) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	@tar cfh - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:

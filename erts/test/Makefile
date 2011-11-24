#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2010. All Rights Reserved.
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

EBIN = .

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	autoimport_SUITE \
	erlc_SUITE \
	install_SUITE \
	nt_SUITE \
	otp_SUITE \
	ethread_SUITE \
	erl_print_SUITE \
	run_erl_SUITE \
	erlexec_SUITE \
	z_SUITE

ERL_XML = $(ERL_TOP)/erts/doc/src/erlang.xml
ERL_XML_TARGET = autoimport_SUITE_data/erlang.xml

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

EXTRA_FILES = install_SUITE_data/install_bin $(ERL_XML_TARGET)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/system_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/test_server/include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGET_FILES) $(EXTRA_FILES)

install_SUITE_data/install_bin: ../../make/install_bin
	rm -f $@
	cp -p $< $@

$(ERL_XML_TARGET): $(ERL_XML)
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
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) system.spec system.dynspec system.spec.vxworks \
		$(ERL_FILES) $(TARGET_FILES) $(RELSYSDIR)
	chmod -R u+w $(RELSYSDIR)
	tar cf - *_SUITE_data utils | (cd $(RELSYSDIR); tar xf -)

release_docs_spec:

#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
	ei_accept_SUITE \
	ei_connect_SUITE \
	ei_decode_SUITE \
	ei_decode_encode_SUITE \
	ei_encode_SUITE \
	ei_format_SUITE \
	ei_print_SUITE \
	ei_tmo_SUITE \
	erl_connect_SUITE \
	erl_global_SUITE \
	erl_eterm_SUITE \
	erl_ext_SUITE \
	erl_format_SUITE \
	erl_match_SUITE \
	port_call_SUITE \
	runner

SPEC_FILES = \
	erl_interface.spec \
	erl_interface.dynspec \
	erl_interface.spec.vxworks

ERL_FILES = $(MODULES:%=%.erl)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/erl_interface_test

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt:

clean:

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec:

release_tests_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) $(SPEC_FILES) $(ERL_FILES) $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)
	@tar cf - *_SUITE_data | (cd $(RELSYSDIR); tar xf -)

release_docs_spec:

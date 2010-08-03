#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
#
include $(ERL_TOP)/make/target.mk

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(ORBER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/orber-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

EXTERNAL_HRL_FILES= ../include/corba.hrl \
	../include/ifr_types.hrl \
	../include/orber_pi.hrl

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug opt clean docs:


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(EXTERNAL_HRL_FILES) $(RELSYSDIR)/include


release_docs_spec:



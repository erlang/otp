# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

ifneq ($(ERL_TOP),)
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk
else
include $(DIAMETER_TOP)/make/target.mk
include $(DIAMETER_TOP)/make/$(TARGET)/rules.mk
endif

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

include subdirs.mk

SPECIAL_TARGETS =

# ----------------------------------------------------
# Default Subdir Targets
# ----------------------------------------------------
ifneq ($(ERL_TOP),)
include $(ERL_TOP)/make/otp_subdir.mk
else
include $(DIAMETER_TOP)/make/subdir.mk
endif
#include ../make/subdir.mk

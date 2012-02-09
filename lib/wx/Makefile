#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2008-2012. All Rights Reserved.
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

include ./vsn.mk

ifdef TERTIARY_BOOTSTRAP
 INSIDE_ERLSRC = true
 SUBDIRS = src
else # Normal build
 include ./config.mk
 SUBDIRS = src
 ifeq ($(CAN_BUILD_DRIVER), true)
 SUBDIRS += c_src
 endif
 SUBDIRS += examples doc/src
endif #TERTIARY_BOOTSTRAP

CLEANDIRS = $(SUBDIRS) api_gen

# ----------------------------------------------------
# Default Subdir Targets
# ----------------------------------------------------
SUB_DIRECTORIES=$(SUBDIRS)

include $(ERL_TOP)/make/otp_subdir.mk

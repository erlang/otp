#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2000-2011. All Rights Reserved.
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
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(JINTERFACE_VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

SUB_DIRECTORIES = com/ericsson/otp/erlang

SPECIAL_TARGETS = 

TARGET_FILES= $(POM_TARGET)

SPECIAL_TARGETS =

POM_FILE= pom.xml

POM_TARGET= ../$(POM_FILE)
POM_SRC= $(POM_FILE).src

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(POM_TARGET): $(POM_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@


include $(ERL_TOP)/make/otp_subdir.mk

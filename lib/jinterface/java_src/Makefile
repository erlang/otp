#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2000-2009. All Rights Reserved.
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

# call recursive make explicitly below
# due to separate makefiles for Ronja & OTP
# SUB_DIRECTORIES = com/ericsson/otp/erlang 

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
	sed -e 's;%VSN%;$(VSN);' $< > $@

# ----------------------------------------------------
# Default Subdir Targets
# ----------------------------------------------------

.PHONY: debug opt instr release docs release_docs tests release_tests clean depend

debug opt instr release docs release_docs tests release_tests clean depend: $(TARGET_FILES)
	set -e; set -x; \
	case "$(MAKE)" in *clearmake*) tflag="-T";; *) tflag="";; esac; \
	if test -f com/ericsson/otp/erlang/ignore_config_record.inf; then xflag=$$tflag; fi; \
	(cd com/ericsson/otp/erlang && $(MAKE) -f Makefile.otp $$xflag $@)


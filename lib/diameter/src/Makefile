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

ifeq ($(ERL_TOP),)
include $(DIAMETER_TOP)/make/target.mk
include $(DIAMETER_TOP)/make/$(TARGET)/rules.mk
else
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk
endif

# ----------------------------------------------------
# Application version
# ----------------------------------------------------

include ../vsn.mk

VSN = $(DIAMETER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------

RELSYSDIR = $(RELEASE_PATH)/lib/diameter-$(VSN)

# Where to put/find things.
EBIN   = ../ebin
INCDIR = ../include

# Dumbed down to make 3.80. In 3.81 and later it's just $(realpath $(EBIN)).
ABS_EBIN := $(shell cd $(EBIN) && pwd)

# Where make should look for dependencies.
VPATH = .:base:compiler:transport:gen

# ----------------------------------------------------
# Target specs
# ----------------------------------------------------

include modules.mk

DICT_MODULES = $(DICTS:%=gen/diameter_gen_%)
DICT_ERLS    = $(DICT_MODULES:%=%.erl)
DICT_HRLS    = $(DICT_MODULES:%=%.hrl)

# Modules to build before compiling dictionaries.
COMPILER_MODULES = $(filter compiler/%, $(CT_MODULES))

# All handwritten modules.
MODULES = \
	$(RT_MODULES) \
	$(CT_MODULES)

# Modules whose names are inserted into the app file.
APP_MODULES = \
	$(RT_MODULES) \
	$(DICT_MODULES)

# Modules for which to build beams.
TARGET_MODULES = \
	$(APP_MODULES) \
	$(CT_MODULES)

# What to build for the 'opt' target.
TARGET_FILES = \
	$(patsubst %,$(EBIN)/%.$(EMULATOR),$(notdir $(TARGET_MODULES))) \
	$(APP_TARGET) \
	$(APPUP_TARGET)

# Subdirectories of src to release modules into.
TARGET_DIRS = $(sort $(dir $(TARGET_MODULES)))

APP_FILE   = diameter.app
APP_SRC    = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_FILE   = diameter.appup
APPUP_SRC    = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# Flags
# ----------------------------------------------------

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug
endif

ERL_COMPILE_FLAGS += \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,app_vsn,$(APP_VSN)}' \
	+warn_export_vars \
	+warn_unused_vars \
	-pa $(ABS_EBIN) \
	-I $(INCDIR) \
	-I gen
# -pa is to be able to include_lib from the include directory: the
# path must contain the application name.

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

# erl/hrl from dictionary file.
gen/diameter_gen_%.erl gen/diameter_gen_%.hrl: dict/%.dia
	../bin/diameterc -o gen -i $(EBIN) $<

opt: $(TARGET_FILES)

debug:
	@$(MAKE) TYPE=debug opt

# Generate the app file.
$(APP_TARGET): $(APP_SRC) ../vsn.mk modules.mk
	M=`echo $(notdir $(APP_MODULES)) | tr ' ' ,`; \
	sed -e 's;%VSN%;$(VSN);' \
	    -e "s;%MODULES%;$$M;" \
	  $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

app:  $(APP_TARGET) $(APPUP_TARGET)
dict: $(DICT_ERLS)

docs:

list = echo $(1):; echo $($(1)) | tr ' ' '\n' | sort | sed 's@^@  @'

info:
	@echo ========================================
	@$(call list,DICTS)
	@echo
	@$(call list,RT_MODULES)
	@echo
	@$(call list,CT_MODULES)
	@echo
	@$(call list,TARGET_MODULES)
	@echo
	@$(call list,TARGET_DIRS)
	@echo
	@$(call list,EXTERNAL_HRLS)
	@echo
	@$(call list,INTERNAL_HRLS)
	@echo
	@$(call list,EXAMPLES)
	@echo
	@$(call list,BINS)
	@echo ========================================

clean:
	rm -f $(TARGET_FILES) $(DICT_ERLS) $(DICT_HRLS)
	rm -f depend.mk

# ----------------------------------------------------
# Release targets
# ----------------------------------------------------

ifeq ($(ERL_TOP),)
include $(DIAMETER_TOP)/make/release_targets.mk
else
include $(ERL_TOP)/make/otp_release_targets.mk
endif

# Can't $(INSTALL_DIR) more than one directory at a time on Solaris.

release_spec: opt
	for d in bin ebin examples include src/dict $(TARGET_DIRS:%/=src/%); do \
	    $(INSTALL_DIR) $(RELSYSDIR)/$$d; \
	done
	$(INSTALL_SCRIPT) $(BINS:%=../bin/%) $(RELSYSDIR)/bin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(EXAMPLES:%=../examples/%) $(RELSYSDIR)/examples
	$(INSTALL_DATA) $(EXTERNAL_HRLS:%=../include/%) $(DICT_HRLS) \
	                $(RELSYSDIR)/include
	$(INSTALL_DATA) $(DICTS:%=dict/%.dia) $(RELSYSDIR)/src/dict
	$(MAKE) $(TARGET_DIRS:%/=release_src_%)

$(TARGET_DIRS:%/=release_src_%): release_src_%:
	$(INSTALL_DATA) $(filter $*/%,$(TARGET_MODULES:%=%.erl) \
	                              $(INTERNAL_HRLS)) \
	                $(RELSYSDIR)/src/$*

release_docs_spec:

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------

gen/diameter_gen_base_accounting.erl gen/diameter_gen_relay.erl \
gen/diameter_gen_base_accounting.hrl gen/diameter_gen_relay.hrl: \
	$(EBIN)/diameter_gen_base_rfc3588.$(EMULATOR)

gen/diameter_gen_base_rfc3588.erl gen/diameter_gen_base_rfc3588.hrl: \
	$(COMPILER_MODULES:compiler/%=$(EBIN)/%.$(EMULATOR))

$(DICT_MODULES:gen/%=$(EBIN)/%.$(EMULATOR)): \
	$(INCDIR)/diameter.hrl \
	$(INCDIR)/diameter_gen.hrl

depend: depend.mk

# Generate dependencies makefile.
depend.mk: depend.sed $(MODULES:%=%.erl) Makefile
	(for f in $(MODULES); do \
	     (echo $$f; cat $$f.erl) | sed -f $<; \
	 done) \
	> $@

-include depend.mk

.PRECIOUS: $(DICT_ERLS) $(DICT_HRLS)
.PHONY: app clean depend dict info release_subdir
.PHONY: debug opt release_docs_spec release_spec
.PHONY: $(TARGET_DIRS:%/=%) $(TARGET_DIRS:%/=release_src_%)

# ----------------------------------------------------
# Targets using secondary expansion (make >= 3.81)
# ----------------------------------------------------

.SECONDEXPANSION:

# Make beams from a subdirectory.
$(TARGET_DIRS:%/=%): \
  $$(patsubst $$@/%,$(EBIN)/%.$(EMULATOR),$$(filter $$@/%,$(TARGET_MODULES)))

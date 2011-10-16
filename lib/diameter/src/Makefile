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

# Where make should look for dependencies.
VPATH = .:base:compiler:transport:gen

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

DICT_FILES     = $(RT_DICTS:%=dict/%.dia)
DICT_MODULES   = $(RT_DICTS:%=gen/diameter_gen_%)
DICT_ERL_FILES = $(DICT_MODULES:%=%.erl)
DICT_HRL_FILES = $(DICT_MODULES:%=%.hrl)

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

EXAMPLE_FILES = $(EXAMPLES:%=../examples/%)
BIN_FILES     = $(BINS:%=../bin/%)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug
endif

ERL_COMPILE_FLAGS += \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,app_vsn,$(APP_VSN)}' \
	+warn_unused_vars \
	-pa $(EBIN) \
	-I $(INCDIR) \
	-I gen

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug:
	@$(MAKE) TYPE=debug opt

opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES) $(DICT_ERL_FILES) $(DICT_HRL_FILES)
	rm -f $(APP_TARGET) $(APPUP_TARGET)
	rm -f errs core *~ gen/diameter_gen_*.forms gen/diameter_gen_*.spec
	rm -f depend.mk

docs:

list = echo $(1):; echo $($(1)) | tr ' ' '\n' | sort | sed 's@^@  @'

info:
	@echo ========================================
	@$(call list,RT_DICTS)
	@echo
	@$(call list,RT_MODULES)
	@echo
	@$(call list,CT_MODULES)
	@echo
	@$(call list,TARGET_MODULES)
	@echo
	@$(call list,TARGET_DIRS)
	@echo
	@$(call list,EXTERNAL_HRL_FILES)
	@echo
	@$(call list,INTERNAL_HRL_FILES)
	@echo
	@$(call list,EXAMPLE_FILES)
	@echo
	@$(call list,BIN_FILES)
	@echo ========================================

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

# erl/hrl from dictionary file.
gen/diameter_gen_%.erl gen/diameter_gen_%.hrl: dict/%.dia
	../bin/diameterc -o gen -i $(EBIN) $<

# Generate the app file.
$(APP_TARGET): $(APP_SRC) ../vsn.mk modules.mk
	M=`echo $(notdir $(APP_MODULES)) | tr ' ' ,`; \
	sed -e 's;%VSN%;$(VSN);' \
	    -e "s;%MODULES%;$$M;" \
	  $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

app:  $(APP_TARGET) $(APPUP_TARGET)
dict: $(DICT_ERL_FILES)

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------

ifeq ($(ERL_TOP),)
include $(DIAMETER_TOP)/make/release_targets.mk
else
include $(ERL_TOP)/make/otp_release_targets.mk
endif

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/bin \
	               $(RELSYSDIR)/ebin \
	               $(RELSYSDIR)/src/dict \
	               $(TARGET_DIRS:%=$(RELSYSDIR)/src/%) \
	               $(RELSYSDIR)/include \
	               $(RELSYSDIR)/examples
	$(INSTALL_SCRIPT) $(BIN_FILES) $(RELSYSDIR)/bin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(DICT_FILES) $(RELSYSDIR)/src/dict
	$(INSTALL_DATA) $(EXTERNAL_HRL_FILES) $(DICT_HRL_FILES) \
	                $(RELSYSDIR)/include
	$(INSTALL_DATA) $(EXAMPLE_FILES) $(RELSYSDIR)/examples
	for dir in $(TARGET_DIRS); do \
	    $(MAKE) release_subdir SRCDIR=$$dir; \
	done

release_subdir:
	[ -d "$(SRCDIR)" ]
	$(INSTALL_DATA) $(filter $(SRCDIR)%,$(TARGET_MODULES:%=%.erl) \
	                                    $(INTERNAL_HRL_FILES)) \
	                $(RELSYSDIR)/src/$(SRCDIR)

release_docs_spec:

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------

gen/diameter_gen_base_accounting.erl gen/diameter_gen_relay.erl \
gen/diameter_gen_base_accounting.hrl gen/diameter_gen_relay.hrl: \
	$(EBIN)/diameter_gen_base_rfc3588.$(EMULATOR)

gen/diameter_gen_base_rfc3588.erl gen/diameter_gen_base_rfc3588.hrl: \
	$(COMPILER_MODULES:compiler/%=$(EBIN)/%.$(EMULATOR))

$(DICT_MODULES:%=$(EBIN)/%.$(EMULATOR)): \
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

.PRECIOUS: $(DICT_ERL_FILES) $(DICT_HRL_FILES)
.PHONY: app clean depend dict info release_subdir
.PHONY: debug opt release_docs_spec release_spec
.PHONY: $(TARGET_DIRS:%/=%)

.SECONDEXPANSION:

# Make the modules from a subdirectory.
$(TARGET_DIRS:%/=%): \
  $$(patsubst $$@/%,$(EBIN)/%.$(EMULATOR),$$(filter $$@/%,$(TARGET_MODULES)))

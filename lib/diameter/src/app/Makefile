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
#
#

ifneq ($(ERL_TOP),)
include $(ERL_TOP)/make/target.mk
EBIN = ../../ebin
include $(ERL_TOP)/make/$(TARGET)/otp.mk
else
include $(DIAMETER_TOP)/make/target.mk
EBIN = ../../ebin
include $(DIAMETER_TOP)/make/$(TARGET)/rules.mk
endif



# ----------------------------------------------------
# Application version
# ----------------------------------------------------

include ../../vsn.mk

VSN=$(DIAMETER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------

RELSYSDIR = $(RELEASE_PATH)/lib/diameter-$(VSN)

INCDIR = ../../include

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

diameter_gen_base_accounting.erl: \
	$(EBIN)/diameter_gen_base_rfc3588.beam
diameter_gen_relay.erl: \
	$(EBIN)/diameter_gen_base_rfc3588.beam

SPEC_MODULES = \
	$(SPEC_FILES:%.dia=%)

SPEC_ERL_FILES = \
	$(SPEC_FILES:%.dia=%.erl)

SPEC_HRL_FILES = \
	$(SPEC_FILES:%.dia=%.hrl)

MODULES = \
	$(RUNTIME_MODULES) \
	$(HELP_MODULES)

APP_MODULES = \
	$(RUNTIME_MODULES) \
	$(SPEC_MODULES)

TARGET_MODULES = \
	$(APP_MODULES) \
	$(HELP_MODULES)

TARGET_FILES = \
	$(TARGET_MODULES:%=$(EBIN)/%.$(EMULATOR)) \
	$(APP_TARGET) \
	$(APPUP_TARGET)

ESCRIPT_FILES = \
	../../bin/diameterc

APP_FILE   = diameter.app
APP_SRC    = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_FILE   = diameter.appup
APPUP_SRC    = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug
endif

include diameter.mk

ERL_COMPILE_FLAGS += \
    $(DIAMETER_ERL_COMPILE_FLAGS) \
    -I$(INCDIR)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug:
	@$(MAKE) TYPE=debug opt

opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES) $(SPEC_ERL_FILES) $(SPEC_HRL_FILES)
	rm -f $(APP_TARGET) $(APPUP_TARGET)
	rm -f errs core *~ diameter_gen_*.forms diameter_gen_*.spec
	rm -f depend.mk

docs:

info:
	@echo ""
	@echo "SPEC_FILES = $(FILES)"
	@echo "MODULES    = $(MODULES)"
	@echo ""
	@echo "EXTERNAL_HRL_FILES = $(EXTERNAL_HRL_FILES)"
	@echo "INTERNAL_HRL_FILES = $(INTERNAL_HRL_FILES)"
	@echo ""
	@echo "EXAMPLE_FILES = $(EXAMPLE_FILES)"
	@echo ""

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

# Generate the app file and then modules into in. This shouldn't know
# about ../transport but good enough for now.
$(APP_TARGET): $(APP_SRC) \
               ../../vsn.mk \
               modules.mk \
               ../transport/modules.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@
	M=`echo $(APP_MODULES) | sed -e 's/^ *//' -e 's/ *$$//' -e 'y/ /,/'`; \
	echo "/%APP_MODULES%/s//$$M/;w;q" | tr ';' '\n' \
	| ed -s $@
	$(MAKE) -C ../transport $(APP_TARGET) APP_TARGET=$(APP_TARGET)

$(APPUP_TARGET): $(APPUP_SRC) ../../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

compiler:
	$(MAKE) -C ../$@

app: $(APP_TARGET) $(APPUP_TARGET)

# erl/hrl from application spec
diameter_gen_%.erl diameter_gen_%.hrl: diameter_gen_%.dia
	../../bin/diameterc -i $(EBIN) -o $(@D) $<

$(SPEC_MODULES:%=$(EBIN)/%.$(EMULATOR)): $(EBIN)/diameter_exprecs.$(EMULATOR)

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------

ifneq ($(ERL_TOP),)
include $(ERL_TOP)/make/otp_release_targets.mk
else
include $(DIAMETER_TOP)/make/release_targets.mk
endif

release_spec: opt
	$(INSTALL_DIR)  $(RELSYSDIR)/bin
	$(INSTALL_DIR)  $(RELSYSDIR)/ebin
	$(INSTALL_DIR)  $(RELSYSDIR)/src/app
	$(INSTALL_DIR)  $(RELSYSDIR)/include
	$(INSTALL_DIR)  $(RELSYSDIR)/examples
	$(INSTALL_SCRIPT) $(ESCRIPT_FILES) $(RELSYSDIR)/bin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(MODULES:%=%.erl) $(SPEC_ERL_FILES) $(RELSYSDIR)/src/app
	$(INSTALL_DATA) $(SPEC_FILES) $(RELSYSDIR)/src/app
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) $(RELSYSDIR)/src/app
	$(INSTALL_DATA) $(EXTERNAL_HRL_FILES) $(SPEC_HRL_FILES) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(EXAMPLE_FILES) $(RELSYSDIR)/examples

release_docs_spec:

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------

$(SPEC_FILES:%.dia=$(EBIN)/%.$(EMULATOR)): \
	$(DIAMETER_TOP)/include/diameter.hrl \
	$(DIAMETER_TOP)/include/diameter_gen.hrl

depend: depend.mk

# Generate dependencies makefile. It's assumed that the compile target
# has already been made since it's currently not smart enough to not
# force a rebuild of those beams dependent on generated hrls, and this
# is a no-no at make release.
depend.mk: depend.sed $(MODULES:%=%.erl) Makefile
	(for f in $(MODULES); do \
	     sed -f $< $$f.erl | sed "s@/@/$$f@"; \
	 done) \
	> $@

-include depend.mk

.PRECIOUS: $(SPEC_ERL_FILES) $(SPEC_HRL_FILES)
.PHONY: app clean debug depend info opt compiler release_spec release_docs_spec

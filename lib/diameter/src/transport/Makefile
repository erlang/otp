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

ERL_FILES = \
	$(MODULES:%=%.erl) 

TARGET_FILES = \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug
endif

include ../app/diameter.mk

ERL_COMPILE_FLAGS += \
	$(DIAMETER_ERL_COMPILE_FLAGS) \
	-I$(INCDIR)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug:
	@${MAKE} TYPE=debug opt

opt: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES) 
	rm -f errs core *~
	rm -f depend.mk

docs:

info:
	@echo ""
	@echo "ERL_FILES = $(ERL_FILES)"
	@echo "HRL_FILES = $(HRL_FILES)"
	@echo ""
	@echo "TARGET_FILES = $(TARGET_FILES)"
	@echo ""

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

# Invoked from ../app to add modules to the app file.
$(APP_TARGET): force
	M=`echo $(MODULES) | sed -e 's/^ *//' -e 's/ *$$//' -e 'y/ /,/'`; \
	echo "/%TRANSPORT_MODULES%/s//$$M/;w;q" | tr ';' '\n' \
	| ed -s $@

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
ifneq ($(ERL_TOP),)
include $(ERL_TOP)/make/otp_release_targets.mk
else
include $(DIAMETER_TOP)/make/release_targets.mk
endif

release_spec: opt
	$(INSTALL_DIR)  $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DIR)  $(RELSYSDIR)/src/transport
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(RELSYSDIR)/src/transport

release_docs_spec:

force:

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------

depend: depend.mk

# Generate dependencies makefile.
depend.mk: ../app/depend.sed $(ERL_FILES) Makefile
	for f in $(MODULES); do \
	    sed -f $< $$f.erl | sed "s@/@/$$f@"; \
	done \
	> $@

-include depend.mk

.PHONY: clean debug depend docs force info opt release_docs_spec release_spec

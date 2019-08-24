#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2018. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%
#
#
# For an outline of how this all_SUITE_data stuff works, see the
# make file ../../ssl/test/Makefile.
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN = $(TFTP_VSN)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
INCLUDES = -I. \
	-I$(ERL_TOP)/lib/tftp/src

CP = cp

ifeq ($(TESTROOT_DIR),)
TESTROOT_DIR = /ldisk/tests/$(USER)/tftp
endif

ifeq ($(TFTP_DATA_DIR),)
TFTP_DATA_DIR = $(TESTROOT_DIR)/data_dir
endif

ifeq ($(TFTP_PRIV_DIR),)
TFTP_PRIV_DIR = $(TESTROOT_DIR)/priv_dir
endif

TFTP_FLAGS = -Dtftp__data_dir='"$(TFTP_DATA_DIR)"'       \
              -Dtftp_priv_dir='"$(TFTP_PRIV_DIR)"'


### 
### test suite debug flags
### 
ifeq ($(TFTP_DEBUG_CLIENT),)
  TFTP_DEBUG_CLIENT = y
endif

ifeq ($(TFTP_DEBUG_CLIENT),)
  TFTP_FLAGS += -Dtftp_debug_client
endif

ifeq ($(TFTP_TRACE_CLIENT),)
  TFTP_DEBUG_CLIENT = y
endif

ifeq ($(TFTP_TRACE_CLIENT),y)
  TFTP_FLAGS += -Dtftp_trace_client
endif

ifneq ($(TFTP_DEBUG),)
  TFTP_DEBUG = s
endif

ifeq ($(TFTP_DEBUG),l)
  TFTP_FLAGS += -Dtftp_log
endif

ifeq ($(TFTP_DEBUG),d)
  TFTP_FLAGS += -Dtftp_debug -Dtftp_log
endif


TFTP_FLAGS += -pa ../tftp/ebin

TFTP_ROOT = ../tftp

MODULES =                 		\
	tftp_SUITE 	  		\
	tftp_test_lib


EBIN = .

HRL_FILES = \
	../src/tftp.hrl \
	tftp_test_lib.hrl

ERL_FILES = $(MODULES:%=%.erl)

SOURCE = $(ERL_FILES) $(HRL_FILES) 

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

TFTP_SPECS = tftp.spec tftp_bench.spec
COVER_FILE = tftp.cover
TFTP_FILES = tftp.config $(TFTP_SPECS)


TFTP_DATADIRS   = tftp_SUITE_data  

DATADIRS     = $(TFTP_DATADIRS)

EMAKEFILE  = Emakefile
MAKE_EMAKE = $(wildcard $(ERL_TOP)/make/make_emakefile)

ifeq ($(MAKE_EMAKE),)
BUILDTARGET   = $(TARGET_FILES)
RELTEST_FILES = $(COVER_FILE) $(TFTP_SPECS) $(SOURCE)
else
BUILDTARGET   = emakebuild
RELTEST_FILES = $(EMAKEFILE) $(COVER_FILE) $(TFTP_SPECS) $(SOURCE)
endif


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------

RELTESTSYSDIR        = "$(RELEASE_PATH)/tftp_test"
RELTESTSYSALLDATADIR = $(RELTESTSYSDIR)/all_SUITE_data
RELTESTSYSBINDIR     = $(RELTESTSYSALLDATADIR)/bin


# ----------------------------------------------------
# FLAGS
# The path to the test_server ebin dir is needed when 
# running the target "targets".
# ----------------------------------------------------
ERL_COMPILE_FLAGS += \
	$(INCLUDES) \
	$(TFTP_FLAGS)

# ----------------------------------------------------
# Targets
# erl -sname kalle -pa ../ebin
# If you intend to run the test suite locally (private), then
# there is some requirements:
# 1) TFTP_PRIV_DIR must be created
# ----------------------------------------------------

tests debug opt: $(BUILDTARGET) 

targets: $(TARGET_FILES)

.PHONY: emakebuild

emakebuild: $(EMAKEFILE)

$(EMAKEFILE): 
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make' | grep -v Warning > $(EMAKEFILE)
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) | grep -v Warning >> $(EMAKEFILE)

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES)
	rm -f core *~

docs:


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR)  "$(RELSYSDIR)/test"
	$(INSTALL_DATA) $(HRL_FILES)   $(ERL_FILES) "$(RELSYSDIR)/test"
	$(INSTALL_DATA) $(TFTP_FILES) "$(RELSYSDIR)/test"
	@for d in $(DATADIRS); do \
            echo "installing data dir $$d"; \
            if test -f $$d/TAR.exclude; then \
                echo $$d/TAR.exclude2 > $$d/TAR.exclude2; \
                cat $$d/TAR.exclude >> $$d/TAR.exclude2; \
                find $$d -name '*.contrib*'     >> $$d/TAR.exclude2; \
                find $$d -name '*.keep*'        >> $$d/TAR.exclude2; \
                find $$d -name '*.mkelem*'      >> $$d/TAR.exclude2; \
                find $$d -name '*~'             >> $$d/TAR.exclude2; \
                find $$d -name 'erl_crash.dump' >> $$d/TAR.exclude2; \
                find $$d -name 'core'           >> $$d/TAR.exclude2; \
                find $$d -name '.cmake.state'   >> $$d/TAR.exclude2; \
                tar cfX - $$d/TAR.exclude2 $$d | (cd "$(RELSYSDIR)/test"; tar xf -); \
            else \
                tar cf - $$d | (cd "$(RELSYSDIR)/test"; tar xf -); \
            fi; \
        done

release_tests_spec: opt
	$(INSTALL_DIR) $(RELTESTSYSDIR)
	$(INSTALL_DATA) $(RELTEST_FILES) $(RELTESTSYSDIR)
	chmod -R u+w $(RELTESTSYSDIR)
	tar chf - $(DATADIRS) | (cd $(RELTESTSYSDIR); tar xf -)
	$(INSTALL_DIR) $(RELTESTSYSALLDATADIR)
	$(INSTALL_DIR) $(RELTESTSYSBINDIR)
	chmod -R +x $(RELTESTSYSBINDIR)
	$(INSTALL_DIR) $(RELTESTSYSALLDATADIR)/win32/lib	

release_docs_spec:

info:
	@echo "MAKE_EMAKE        = $(MAKE_EMAKE)"
	@echo "EMAKEFILE         = $(EMAKEFILE)"
	@echo "BUILDTARGET       = $(BUILDTARGET)"
	@echo ""
	@echo "MODULES           = $(MODULES)"
	@echo "ERL_FILES         = $(ERL_FILES)"
	@echo "SOURCE            = $(SOURCE)"
	@echo "TARGET_FILES      = $(TARGET_FILES)"
	@echo ""
	@echo "TFTP_SPECS       = $(TFTP_SPECS)"
	@echo "TFTP_FILES       = $(TFTP_FILES)"
	@echo ""
	@echo "RELEASE_PATH      = "$(RELEASE_PATH)""
	@echo "RELSYSDIR         = "$(RELSYSDIR)""
	@echo "RELTESTSYSDIR     = $(RELTESTSYSDIR)"
	@echo "RELTESTSYSALLDATADIR = $(RELTESTSYSALLDATADIR)"
	@echo "RELTESTSYSBINDIR     = $(RELTESTSYSBINDIR)"
	@echo ""
	@echo "DATADIRS          = $(DATADIRS)"
	@echo "REL_DATADIRS      = $(REL_DATADIRS)"
	@echo ""
	@echo "TFTP_DATA_DIR    = $(TFTP_DATA_DIR)"
	@echo "TFTP_PRIV_DIR    = $(TFTP_PRIV_DIR)"
	@echo "TFTP_ROOT        = $(TFTP_ROOT)"
	@echo "TFTP_FLAGS       = $(TFTP_FLAGS)"



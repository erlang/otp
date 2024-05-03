# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1999-2024. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include vsn.mk
VSN=$(MEGACO_VSN)

DIR_NAME = megaco_src-$(VSN)$(PRE_VSN)

nullstring :=
space := $(nullstring) # a space at the end

ifndef APP_RELEASE_DIR
  ifndef TESTROOT
    APP_RELEASE_DIR = /tmp
  else
    APP_RELEASE_DIR = $(subst $(space),\ ,$(TESTROOT))
  endif
endif

ifndef APP_TAR_FILE
  APP_TAR_FILE = $(APP_RELEASE_DIR)/$(DIR_NAME).tgz
endif

APP_DIR = $(APP_RELEASE_DIR)/$(DIR_NAME)

ifdef OTP_INSTALL_DIR
  APP_INSTALL_DIR = $(OTP_INSTALL_DIR)/lib/erlang
else
  # If installing into an OTP structure created
  # by installing an source OTP build, the '/tmp'
  # shall be replaced with the value of ERL_TOP
  APP_INSTALL_DIR = /tmp/lib/erlang
endif

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

include subdirs.mk

SUB_DIRECTORIES = $(SUB_DIRS) doc

SPECIAL_TARGETS = 

ifeq ($(FLEX_SCANNER_LINENO),disable)
  FLEX_SCANNER_LINENO_ENABLER = --disable-megaco-flex-scanner-lineno
else
  FLEX_SCANNER_LINENO_ENABLER = --enable-megaco-flex-scanner-lineno
endif

ifeq ($(FLEX_SCANNER_REENTRANT),disable)
  FLEX_SCANNER_REENTRANT_ENABLER = --disable-megaco-reentrant-flex-scanner
else
  FLEX_SCANNER_REENTRANT_ENABLER = --enable-megaco-reentrant-flex-scanner
endif

ifeq ($(ERL_DETERMINISTIC),yes)
  ERL_DETERMINISTIC_ENABLER = --enable-deterministic-build
else
  ERL_DETERMINISTIC_ENABLER = --disable-deterministic-build
endif

CONFIGURE_OPTS = $(FLEX_SCANNER_LINENO_ENABLER) $(FLEX_SCANNER_REENTRANT_ENABLER) $(ERL_DETERMINISTIC_ENABLER)


DIA_PLT      = ./priv/plt/$(APPLICATION).plt
DIA_ANALYSIS = $(basename $(DIA_PLT)).dialyzer_analysis


# ----------------------------------------------------
# Default Subdir Targets
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_subdir.mk

.PHONY: reconf conf dconf econf configure setup info_megaco version \
	app_install dialyzer

reconf:
	(cd $(ERL_TOP) && \
		./otp_build configure $(ERL_DETERMINISTIC_ENABLER) && \
		cd $(ERL_TOP)/../libraries/megaco)

conf: do_configure

dconf: 
	$(MAKE) conf FLEX_SCANNER_REENTRANT=disable

econf: 
	$(MAKE) conf FLEX_SCANNER_REENTRANT=enable

do_configure: configure 
	./configure $(CONFIGURE_OPTS)

configure: configure.in
	autoconf

setup:
	(cd src && $(MAKE) $@)

info_megaco:
	@echo "APP_RELEASE_DIR: $(APP_RELEASE_DIR)"
	@echo "APP_DIR:         $(APP_DIR)"
	@echo "APP_TAR_FILE:    $(APP_TAR_FILE)"
	@echo "OTP_INSTALL_DIR: $(OTP_INSTALL_DIR)"
	@echo "APP_INSTALL_DIR: $(APP_INSTALL_DIR)"
	@echo ""

info: info_megaco

version:
	@echo "$(VSN)"

DIA_PLT_APPS=asn1 runtime_tools et debugger
TEST_NEEDS_RELEASE=true

include $(ERL_TOP)/make/app_targets.mk

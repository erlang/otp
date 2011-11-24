# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1999-2011. All Rights Reserved.
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
# --------------------------------------------------------------------
# If downloading this app separately (not as part of 
# an OTP source release), then there is two ways to
# build and install this app.
# 1) Replace the megaco source dir in an existing
#    OTP source tree. Then re-configure, build and 
#    install OTP.
#    Note that the directory name has to be renamed
#    from megaco-<version> to megaco.
# 2) Unpack outside an OTP-source tree. 
#    a) Configure app (make ERL_TOP=</path/to/top/OTP-source/dir> conf)
#    b) make ERL_TOP=</path/to/top/OTP-source/dir>
#    c) make ERL_TOP=</path/to/top/OTP-source/dir> \
#            OTP_INSTALL_DIR=</path/to/top/OTP-install/dir> app_install
#       (e.g. if otp is installed in /usr/local/otp-r9b,
#        then OTP_INSTALL_DIR is /usr/local/otp-r9b and
#        the app will be installed in dir 
#        /usr/local/otp-r9b/lib/erlang/lib/megaco-<version>)
# -----------------------------------------------------------------------
#

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include vsn.mk
VSN=$(MEGACO_VSN)

DIR_NAME = megaco_src-$(VSN)$(PRE_VSN)

ifndef APP_RELEASE_DIR
  ifndef TESTROOT
    APP_RELEASE_DIR = /tmp
  else
    APP_RELEASE_DIR = $(TESTROOT)
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

SUB_DIRECTORIES = $(SUB_DIRS) doc/src

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

CONFIGURE_OPTS = $(FLEX_SCANNER_LINENO_ENABLER) $(FLEX_SCANNER_REENTRANT_ENABLER)


MEGACO_DIA_PLT     = ./priv/megaco.plt
MEGACO_DIA_PLT_LOG = $(basename $(MEGACO_DIA_PLT)).dialyzer_plt_log
MEGACO_DIA_LOG     = $(basename $(MEGACO_DIA_PLT)).dialyzer_log


# ----------------------------------------------------
# Default Subdir Targets
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_subdir.mk

.PHONY: reconf conf dconf econf configure setup info version \
	app_install dialyzer

reconf:
	(cd $(ERL_TOP) && \
		./otp_build autoconf && \
		./otp_build configure && \
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

info:
	@echo "APP_RELEASE_DIR: $(APP_RELEASE_DIR)"
	@echo "APP_DIR:         $(APP_DIR)"
	@echo "APP_TAR_FILE:    $(APP_TAR_FILE)"
	@echo "OTP_INSTALL_DIR: $(OTP_INSTALL_DIR)"
	@echo "APP_INSTALL_DIR: $(APP_INSTALL_DIR)"
	@echo ""
	@echo "MEGACO_PLT     = $(MEGACO_PLT)"
	@echo "MEGACO_DIA_LOG = $(MEGACO_DIA_LOG)"
	@echo ""

version:
	@echo "$(VSN)"


# ----------------------------------------------------
# Application install (of a app built from source) targets
# ----------------------------------------------------
app_install:
	$(MAKE) TESTROOT=$(APP_INSTALL_DIR) release 


# ----------------------------------------------------
# Application (source) release targets
# ----------------------------------------------------
app_release: app_doc tar

app_doc:
	cd doc/src; $(MAKE) html man

app_dir: $(APP_DIR)

tar_exclude: TAR.exclude2

TAR.exclude2: Makefile TAR.exclude
	cat TAR.exclude > TAR.exclude2; \
        echo "megaco/TAR.exclude2" >> TAR.exclude2; \
        echo "megaco/priv/lib/$(TARGET)" >> TAR.exclude2; \
        echo "megaco/src/flex/$(TARGET)" >> TAR.exclude2; \
	(cd ..; find megaco -name 'prebuild.skip' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name 'findmerge.*' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name '*.contrib*' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name '*.keep*' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name '*.mkelem*' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name '*~' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name 'erl_crash.dump' >> megaco/TAR.exclude2)
	(cd ..; find megaco/test -name '*.beam' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name '*.log' >> megaco/TAR.exclude2)
	(cd ..; find megaco/examples/meas -name '*.xls' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name 'core' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name '.cmake.state' >> megaco/TAR.exclude2)

$(APP_DIR): tar_exclude
	mkdir -p $(APP_DIR); \
        (cd ..; tar cfX - megaco/TAR.exclude2 megaco) | \
        (cd $(APP_DIR); tar xf -); \
        mv $(APP_DIR)/megaco/* $(APP_DIR)/; \
	mkdir $(APP_DIR)/autoconf; \
        cp autoconf/config.guess $(APP_DIR)/autoconf/; \
        cp autoconf/config.sub $(APP_DIR)/autoconf/; \
        cp autoconf/install-sh $(APP_DIR)/autoconf/; \
        rmdir $(APP_DIR)/megaco

tar: $(APP_TAR_FILE)

$(APP_TAR_FILE): $(APP_DIR)
	(cd $(APP_RELEASE_DIR); gtar zcf $(APP_TAR_FILE) $(DIR_NAME))

dialyzer_plt: $(MEGACO_DIA_PLT)

$(MEGACO_DIA_PLT): 
	@echo "Building megaco plt file"
	@dialyzer --build_plt \
                  --output_plt $@ \
                  -r ../megaco/ebin \
                  -o $(MEGACO_DIA_PLT_LOG) \
                  --verbose

dialyzer: $(MEGACO_DIA_PLT)
	(dialyzer --plt $< \
                  -o $(MEGACO_DIA_LOG) \
                  ../megaco/ebin \
                  && (shell cat $(MEGACO_DIA_LOG)))

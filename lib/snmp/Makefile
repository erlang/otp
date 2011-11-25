#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2011. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

include subdirs.mk

include vsn.mk

VSN = $(SNMP_VSN)

SPECIAL_TARGETS = 

DIR_NAME = snmp_src-$(VSN)$(PRE_VSN)

ifndef APP_RELEASE_DIR
  APP_RELEASE_DIR = /tmp
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


DIA_PLT      = ./priv/plt/$(APPLICATION).plt
DIA_ANALYSIS = $(basename $(DIA_PLT)).dialyzer_analysis

# ----------------------------------------------------
# Default Subdir Targets
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_subdir.mk

conf: do_configure

do_configure: configure 
	./configure 

configure: configure.in
	autoconf

.PHONY: info gclean

info: 
	@echo "OS:       $(OS)"
	@echo "DOCB:     $(DOCB)"
	@echo ""
	@echo "SNMP_VSN: $(SNMP_VSN)"
	@echo "APP_VSN:  $(APP_VSN)"
	@echo ""
	@echo "DIA_PLT:      $(DIA_PLT)"
	@echo "DIA_ANALYSIS: $(DIA_ANALYSIS)"
	@echo ""


gclean: 
	git clean -fXd


# ----------------------------------------------------
# Application (source) release targets
# ----------------------------------------------------
app_release: app_doc tar

app_clean:
	rm -rf $(APP_TAR_FILE) $(APP_DIR)

app_doc:
	cd doc/src; $(MAKE) html man

app_dir: $(APP_DIR)

$(APP_DIR):
	cat TAR.exclude | grep -v "snmp/doc" > TAR.exclude2; \
        echo "snmp/doc/src" >> TAR.exclude2; \
        echo "snmp/TAR.exclude2" >> TAR.exclude2; \
        echo "snmp/doc/internal" >> TAR.exclude2; \
        echo "snmp/mibs/prebuild.skip" >> TAR.exclude2
	(cd ..; find snmp -name 'findmerge.*' >> snmp/TAR.exclude2)
	(cd ..; find snmp -name '*.contrib*' >> snmp/TAR.exclude2)
	(cd ..; find snmp -name '*.keep*' >> snmp/TAR.exclude2)
	(cd ..; find snmp -name '*~' >> snmp/TAR.exclude2)
	(cd ..; find snmp -name '*.log' >> snmp/TAR.exclude2)
	(cd ..; find snmp -name 'erl_crash.dump' >> snmp/TAR.exclude2)
	mkdir $(APP_DIR); \
        (cd ..; tar cfX - snmp/TAR.exclude2 snmp) | \
        (cd $(APP_DIR); tar xf -); \
        mv $(APP_DIR)/snmp/* $(APP_DIR)/; \
        find $(APP_DIR)/snmp -name '.cmake.state' | xargs rm -f; \
	mkdir $(APP_DIR)/autoconf; \
        cp autoconf/config.guess $(APP_DIR)/autoconf/; \
        cp autoconf/config.sub $(APP_DIR)/autoconf/; \
        cp autoconf/install-sh $(APP_DIR)/autoconf/; \
        rmdir $(APP_DIR)/snmp

tar: $(APP_TAR_FILE)

$(APP_TAR_FILE): $(APP_DIR)
	(cd $(APP_RELEASE_DIR); gtar zcf $(APP_TAR_FILE) $(DIR_NAME))

dclean:
	rm -f $(DIA_PLT)
	rm -f $(DIA_ANALYSIS)

dialyzer_plt: $(DIA_PLT)

$(DIA_PLT): 
	@echo "Building $(APPLICATION) plt file"
	@dialyzer --build_plt \
                  --output_plt $@ \
                  -r ../$(APPLICATION)/ebin \
                  --output $(DIA_ANALYSIS) \
                  --verbose

dialyzer: $(DIA_PLT)
	@echo "Running dialyzer on $(APPLICATION)"
	@dialyzer --plt $< \
                  ../$(APPLICATION)/ebin \
                  --verbose
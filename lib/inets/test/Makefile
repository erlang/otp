#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
VSN = $(INETS_VSN)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
INCLUDES = -I. \
	-I$(ERL_TOP)/lib/inets/src/inets_app \
	-I$(ERL_TOP)/lib/inets/src/http_lib \
	-I$(ERL_TOP)/lib/inets/src/http_client \
	-I$(ERL_TOP)/lib/inets/src/ftp

CP = cp

ifeq ($(TESTROOT_DIR),)
TESTROOT_DIR = /ldisk/tests/$(USER)/inets
endif

ifeq ($(INETS_DATA_DIR),)
INETS_DATA_DIR = $(TESTROOT_DIR)/data_dir
endif

ifeq ($(INETS_PRIV_DIR),)
INETS_PRIV_DIR = $(TESTROOT_DIR)/priv_dir
endif

INETS_FLAGS = -Dinets_data_dir='"$(INETS_DATA_DIR)"'       \
              -Dinets_priv_dir='"$(INETS_PRIV_DIR)"'


### 
### test suite debug flags
### 
ifeq ($(FTP_DEBUG_CLIENT),)
  FTP_DEBUG_CLIENT = y
endif

ifeq ($(FTP_DEBUG_CLIENT),)
  FTP_FLAGS += -Dftp_debug_client
endif

ifeq ($(FTP_TRACE_CLIENT),)
  FTP_DEBUG_CLIENT = y
endif

ifeq ($(FTP_TRACE_CLIENT),y)
  FTP_FLAGS += -Dftp_trace_client
endif

ifneq ($(FTP_DEBUG),)
  FTP_DEBUG = s
endif

ifeq ($(FTP_DEBUG),l)
  FTP_FLAGS += -Dftp_log
endif

ifeq ($(FTP_DEBUG),d)
  FTP_FLAGS += -Dftp_debug -Dftp_log
endif

ifeq ($(INETS_DEBUG),)
  INETS_DEBUG = d
endif

ifeq ($(INETS_DEBUG),l)
  INETS_FLAGS += -Dinets_log
endif

ifeq ($(INETS_DEBUG),d)
  INETS_FLAGS += -Dinets_debug -Dinets_log
endif


### 
### HTTPD verbosity flags
### 

ifneq ($(MANV),)
  INETS_FLAGS += -Dhttpd_manager_verbosity=$(MANV)
else
  INETS_FLAGS += -Dhttpd_manager_verbosity=trace
endif

ifneq ($(REQV),)
  INETS_FLAGS += -Dhttpd_request_handler_verbosity=$(REQV)
else
  INETS_FLAGS += -Dhttpd_request_handler_verbosity=trace
endif

ifneq ($(ACCV),)
  INETS_FLAGS += -Dhttpd_acceptor_verbosity=$(ACCV)
else
  INETS_FLAGS += -Dhttpd_acceptor_verbosity=trace
endif

ifneq ($(AUTHV),)
  INETS_FLAGS += -Dhttpd_auth_verbosity=$(AUTHV)
else
  INETS_FLAGS += -Dhttpd_auth_verbosity=log
endif

ifneq ($(SECV),)
  INETS_FLAGS += -Dhttpd_security_verbosity=$(SECV)
else
  INETS_FLAGS += -Dhttpd_security_verbosity=log
endif

INETS_FLAGS += -pa ../../inets/ebin

INETS_ROOT = ../../inets

MODULES =                 		\
	inets_test_lib    		\
	erl_make_certs                  \
	ftp_SUITE 	  		\
	ftp_format_SUITE  		\
	http_format_SUITE 		\
	httpc_SUITE	  		\
	httpc_cookie_SUITE		\
	httpc_proxy_SUITE		\
	httpd_SUITE       		\
	old_httpd_SUITE       		\
	httpd_basic_SUITE		\
	httpd_mod 	  		\
	httpd_block	  		\
	httpd_load        		\
	httpd_time_test	  		\
	httpd_1_1         		\
	httpd_1_0                       \
	httpd_test_lib    		\
	inets_sup_SUITE   		\
	inets_SUITE       		\
        tftp_test_lib			\
        tftp_SUITE                      \
	uri_SUITE                       \
	inets_socketwrap_SUITE


EBIN = .

HRL_FILES = inets_test_lib.hrl \
	    inets_internal.hrl \
	    ftp_internal.hrl   \
            httpc_internal.hrl \
            http_internal.hrl \
            tftp_test_lib.hrl

ERL_FILES = $(MODULES:%=%.erl)

SOURCE = $(ERL_FILES) $(HRL_FILES) 

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

INETS_SPECS = inets.spec
COVER_FILE = inets.cover
INETS_FILES = inets.config $(INETS_SPECS)

# SUB_SUITES = \
# 	inets_sup_suite \
# 	inets_httpd_suite \
# 	inets_httpc_suite \
# 	inets_ftp_suite \
# 	inets_tftp_suite

INETS_DATADIRS = inets_SUITE_data inets_socketwrap_SUITE_data
HTTPD_DATADIRS = httpd_test_data httpd_SUITE_data httpd_basic_SUITE_data old_httpd_SUITE_data
HTTPC_DATADIRS = httpc_SUITE_data httpc_proxy_SUITE_data
FTP_DATADIRS   = ftp_SUITE_data  

DATADIRS     = $(INETS_DATADIRS) $(HTTPD_DATADIRS) $(HTTPC_DATADIRS) $(FTP_DATADIRS)

EMAKEFILE  = Emakefile
MAKE_EMAKE = $(wildcard $(ERL_TOP)/make/make_emakefile)

ifeq ($(MAKE_EMAKE),)
BUILDTARGET   = $(TARGET_FILES)
RELTEST_FILES = $(COVER_FILE) $(INETS_SPECS) $(SOURCE)
else
BUILDTARGET   = emakebuild
RELTEST_FILES = $(EMAKEFILE) $(COVER_FILE) $(INETS_SPECS) $(SOURCE)
endif


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------

RELTESTSYSDIR        = "$(RELEASE_PATH)/inets_test"
RELTESTSYSALLDATADIR = $(RELTESTSYSDIR)/all_SUITE_data
RELTESTSYSBINDIR     = $(RELTESTSYSALLDATADIR)/bin


# ----------------------------------------------------
# FLAGS
# The path to the test_server ebin dir is needed when 
# running the target "targets".
# ----------------------------------------------------
ERL_COMPILE_FLAGS += \
	$(INCLUDES) \
	$(FTP_FLAGS) \
	$(INETS_FLAGS)

# ----------------------------------------------------
# Targets
# erl -sname kalle -pa ../ebin
# If you intend to run the test suite locally (private), then
# there is some requirements:
# 1) INETS_PRIV_DIR must be created
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
	$(INSTALL_DATA) $(INETS_FILES) "$(RELSYSDIR)/test"
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
	@echo "INETS_SPECS       = $(INETS_SPECS)"
	@echo "INETS_FILES       = $(INETS_FILES)"
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
	@echo "INETS_DATA_DIR    = $(INETS_DATA_DIR)"
	@echo "INETS_PRIV_DIR    = $(INETS_PRIV_DIR)"
	@echo "INETS_ROOT        = $(INETS_ROOT)"
	@echo "INETS_FLAGS       = $(INETS_FLAGS)"
	@echo "FTP_FLAGS         = $(FTP_FLAGS)"

tftp:
	erlc $(ERL_COMPILE_FLAGS) tftp_test_lib.erl tftp_SUITE.erl && erl -pa ../../inets/ebin -s tftp_SUITE t -s erlang halt

tftp_work:
	echo "tftp_test_lib:t([{tftp_SUITE, all}])."
	erlc $(ERL_COMPILE_FLAGS) tftp_test_lib.erl tftp_SUITE.erl && erl -pa ../../inets/ebin

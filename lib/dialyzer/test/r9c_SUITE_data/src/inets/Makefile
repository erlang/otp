# ``Licensed under the Apache License, Version 2.0 (the "License");
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
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
#
#     $Id: Makefile,v 1.1 2008/12/17 09:53:33 mikpe Exp $
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk

VSN     = $(INETS_VSN)
APP_VSN = "inets-$(VSN)"


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/inets-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES = \
	ftp \
	http \
	http_lib \
	httpc_handler \
	httpc_manager \
	uri \
	httpd \
	httpd_acceptor \
	httpd_acceptor_sup \
	httpd_conf \
	httpd_example \
	httpd_manager \
	httpd_misc_sup \
	httpd_parse \
	httpd_request_handler \
	httpd_response \
	httpd_socket \
	httpd_sup \
	httpd_util \
	httpd_verbosity \
	inets_sup \
	mod_actions \
	mod_alias \
	mod_auth \
	mod_auth_plain \
	mod_auth_dets \
	mod_auth_mnesia \
	mod_auth_server \
	mod_browser \
	mod_cgi \
	mod_dir \
	mod_disk_log \
	mod_esi \
	mod_get \
	mod_head \
	mod_htaccess \
	mod_include \
	mod_log \
	mod_range \
	mod_responsecontrol \
	mod_trace \
	mod_security \
	mod_security_server

HRL_FILES = httpd.hrl httpd_verbosity.hrl mod_auth.hrl \
            http.hrl jnets_httpd.hrl

ERL_FILES = $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= inets.app
APPUP_FILE= inets.appup

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# INETS FLAGS
# ----------------------------------------------------
# DONT_USE_VERBOSITY = -Ddont_use_verbosity=true
INETS_FLAGS = -D'SERVER_SOFTWARE="inets/$(VSN)"' \
              -Ddefault_verbosity=silence \
              $(DONT_USE_VERBOSITY)

# INETS_DEBUG_DEFAULT = d
ifeq ($(INETS_DEBUG),)
  INETS_DEBUG = $(INETS_DEBUG_DEFAULT)
endif

ifeq ($(INETS_DEBUG),c)
  INETS_FLAGS += -Dinets_cdebug -Dinets_debug -Dinets_log -Dinets_error
endif
ifeq ($(INETS_DEBUG),d)
  INETS_FLAGS += -Dinets_debug -Dinets_log -Dinets_error
endif
ifeq ($(INETS_DEBUG),l)
  INETS_FLAGS += -Dinets_log -Dinets_error
endif
ifeq ($(INETS_DEBUG),e)
  INETS_FLAGS += -Dinets_error
endif


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS +=

ifeq ($(WARN_UNUSED_WARS),true)
ERL_COMPILE_FLAGS += +warn_unused_vars
endif

ERL_COMPILE_FLAGS += $(INETS_FLAGS) \
                     +'{parse_transform,sys_pre_attributes}' \
                     +'{attribute,insert,app_vsn,$(APP_VSN)}'


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(HRL_FILES) $(ERL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:

info:
	@echo "INETS_DEBUG       = $(INETS_DEBUG)"
	@echo "INETS_FLAGS       = $(INETS_FLAGS)"
	@echo "ERL_COMPILE_FLAGS = $(ERL_COMPILE_FLAGS)"

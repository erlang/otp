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
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

#
# Macros
#

SUB_DIRECTORIES = bin src include priv doc

include vsn.mk
VSN = $(EDOC_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/xmerl-$(VSN)

# ----------------------------------------------------
# Help application directory specification
# ----------------------------------------------------
DIR_NAME = edoc-$(VSN)
ERL_DIR = src

ifndef APP_RELEASE_DIR
  APP_RELEASE_DIR = /tmp
endif

# ----------------------------------------------------

EXTRA_FILES = \
	edoc-info overview.edoc stylesheet.css

EXTRA_HTML_FILES = \
	modules-frame.html overview-summary.html \
	packages-frame.html 

HTML_FILES = doc/*.html 

# ----------------------------------------------------
ifndef APP_TAR_FILE
  APP_TAR_FILE = $(APP_RELEASE_DIR)/$(DIR_NAME).tgz
endif

APP_DIR = $(APP_RELEASE_DIR)/$(DIR_NAME)


SPECIAL_TARGETS = 

#
# Default Subdir Targets
#

include $(ERL_TOP)/make/otp_subdir.mk


.PHONY: version


version:
	@echo "$(VSN)"


APPNAME=edoc
BINDIR=$(ERL_TOP)/lib/edoc/ebin
DOCDIR=$(ERL_TOP)/lib/edoc/doc
DOC_OPTS=[{def,{version,"$(VSN)"}},todo]

SYNTAX_TOOLS_DIR=$(ERL_TOP)/lib/syntax_tools
XMERL_DIR=$(ERL_TOP)/lib/xmerl
INCDIR=$(XMERL_DIR)/include

# The overriding docs target have been removed so the default make rules work properly.

edocs:
	erl -noshell -pa $(BINDIR) -pa $(SYNTAX_TOOLS_DIR)/ebin \
	    -pa $(XMERL_DIR)/ebin -run edoc_run application \
	    "'$(APPNAME)'" '"."' '$(DOC_OPTS)'

DIA_PLT_APPS=syntax_tools xmerl inets

include $(ERL_TOP)/make/app_targets.mk

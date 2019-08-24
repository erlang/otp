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

SUB_DIRECTORIES = src include priv doc/src

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


.PHONY: info version


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

info:
	@echo $(HTML_FILES)


app_release: tar

app_dir: $(APP_DIR)


$(APP_DIR):
	cat TAR.exclude > TAR.exclude2; \
        echo "edoc/TAR.exclude2" >> TAR.exclude2; \
	(cd ..; find edoc -name 'findmerge.*' >> edoc/TAR.exclude2)
	(cd ..; find edoc -name '*.contrib*' >> edoc/TAR.exclude2)
	(cd ..; find edoc -name '*.keep*' >> edoc/TAR.exclude2)
	(cd ..; find edoc -name '*~' >> edoc/TAR.exclude2)
	(cd ..; find edoc -name 'erl_crash.dump' >> edoc/TAR.exclude2)
	(cd ..; find edoc -name '*.log' >> edoc/TAR.exclude2)
	(cd ..; find edoc -name 'core' >> edoc/TAR.exclude2)
	(cd ..; find edoc -name '.cmake.state' >> edoc/TAR.exclude2)
	mkdir $(APP_DIR); \
        (cd ..; tar cfX - edoc/TAR.exclude2 edoc) | \
        (cd $(APP_DIR); tar xf -); \
        mv $(APP_DIR)/edoc/* $(APP_DIR)/; \
        rmdir $(APP_DIR)/edoc
	mkdir $(APP_DIR)/doc; \
	(cd doc; tar cf - man3 html) | (cd $(APP_DIR)/doc; tar xf -)

tar: $(APP_TAR_FILE)

$(APP_TAR_FILE): $(APP_DIR)
	(cd $(APP_RELEASE_DIR); gtar zcf $(APP_TAR_FILE) $(DIR_NAME))

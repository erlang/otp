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
#     $Id$
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

#
# Macros
#

SUB_DIRECTORIES = src examples doc/src

include vsn.mk
VSN = $(SYNTAX_TOOLS_VSN)

DIR_NAME = syntax_tools-$(VSN)

ifndef APP_RELEASE_DIR
  APP_RELEASE_DIR = /tmp
endif

ifndef APP_TAR_FILE
  APP_TAR_FILE = $(APP_RELEASE_DIR)/$(DIR_NAME).tgz
endif

APP_DIR = $(APP_RELEASE_DIR)/$(DIR_NAME)

APPNAME = syntax_tools
# BINDIR = ebin
DOC_OPTS = [{def,{version,"$(VSN)"}}]

SPECIAL_TARGETS = 

#
# Default Subdir Targets
#

include $(ERL_TOP)/make/otp_subdir.mk


.PHONY: info version


version:
	@echo "$(VSN)"

# The overriding "docs" target have been removed so the default make rules work properly.

edocs:
	erl -noshell -pa "$(BINDIR)" -run edoc_run application "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop

app_release: tar

app_dir: $(APP_DIR)

$(APP_DIR):
	cat TAR.exclude > TAR.exclude2; \
        echo "syntax_tools/TAR.exclude2" >> TAR.exclude2; \
	(cd ..; find syntax_tools -name 'findmerge.*' >> asn1/TAR.exclude2)
	(cd ..; find syntax_tools -name '*.contrib*' >> asn1/TAR.exclude2)
	(cd ..; find syntax_tools -name '*.keep*' >> asn1/TAR.exclude2)
	(cd ..; find syntax_tools -name '*~' >> asn1/TAR.exclude2)
	(cd ..; find syntax_tools -name 'erl_crash.dump' >> asn1/TAR.exclude2)
	(cd ..; find syntax_tools -name '*.log' >> asn1/TAR.exclude2)
	(cd ..; find syntax_tools -name 'core' >> asn1/TAR.exclude2)
	(cd ..; find syntax_tools -name '.cmake.state' >> asn1/TAR.exclude2)
	mkdir $(APP_DIR); \
        (cd ..; tar cfX - syntax_tools/TAR.exclude2 syntax_tools) | \
        (cd $(APP_DIR); tar xf -); \
        mv $(APP_DIR)/syntax_tools/* $(APP_DIR)/; \
        rmdir $(APP_DIR)/syntax_tools
	mkdir $(APP_DIR)/doc; \
	(cd doc; tar cf - man3 html) | (cd $(APP_DIR)/doc; tar xf -)

tar: $(APP_TAR_FILE)

$(APP_TAR_FILE): $(APP_DIR)
	(cd $(APP_RELEASE_DIR); gtar zcf $(APP_TAR_FILE) $(DIR_NAME))

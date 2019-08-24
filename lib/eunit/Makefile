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

SUB_DIRECTORIES = src examples doc/src

include vsn.mk
VSN = $(EUNIT_VSN)

DIR_NAME = eunit-$(VSN)

ifndef APP_RELEASE_DIR
  APP_RELEASE_DIR = /tmp
endif

ifndef APP_TAR_FILE
  APP_TAR_FILE = $(APP_RELEASE_DIR)/$(DIR_NAME).tgz
endif

APP_DIR = $(APP_RELEASE_DIR)/$(DIR_NAME)

APPNAME = eunit
DOC_OPTS = [{def,{version,"$(VSN)"}}]

SPECIAL_TARGETS = 

#
# Default Subdir Targets
#
include $(ERL_TOP)/make/otp_subdir.mk


.PHONY: info version

info:
	@echo "APP_RELEASE_DIR: $(APP_RELEASE_DIR)"
	@echo "APP_DIR:         $(APP_DIR)"
	@echo "APP_TAR_FILE:    $(APP_TAR_FILE)"

version:
	@echo "$(VSN)"

#docs:
#	erl -noshell -run edoc_run application "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop

#edocs: docs

# ----------------------------------------------------
# Application (source) release targets
# ----------------------------------------------------
app_release: tar

app_dir: $(APP_DIR)

$(APP_DIR):
	cat TAR.exclude > TAR.exclude2; \
        echo "eunit/TAR.exclude2" >> TAR.exclude2; \
	(cd ..; find eunit -name 'findmerge.*' >> eunit/TAR.exclude2)
	(cd ..; find eunit -name '*.contrib*' >> eunit/TAR.exclude2)
	(cd ..; find eunit -name '*.keep*' >> eunit/TAR.exclude2)
	(cd ..; find eunit -name '*~' >> eunit/TAR.exclude2)
	(cd ..; find eunit -name 'erl_crash.dump' >> eunit/TAR.exclude2)
	(cd ..; find eunit -name '*.log' >> eunit/TAR.exclude2)
	(cd ..; find eunit -name 'core' >> eunit/TAR.exclude2)
	(cd ..; find eunit -name '.cmake.state' >> eunit/TAR.exclude2)
	mkdir $(APP_DIR); \
        (cd ..; tar cfX - eunit/TAR.exclude2 eunit) | \
        (cd $(APP_DIR); tar xf -); \
        mv $(APP_DIR)/eunit/* $(APP_DIR)/; \
        rmdir $(APP_DIR)/eunit
	mkdir $(APP_DIR)/doc; \
	(cd doc; tar cf - man3 html) | (cd $(APP_DIR)/doc; tar xf -)

tar: $(APP_TAR_FILE)

$(APP_TAR_FILE): $(APP_DIR)
	(cd $(APP_RELEASE_DIR); gtar zcf $(APP_TAR_FILE) $(DIR_NAME))

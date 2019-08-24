#<copyright>
# <year>2003-2007</year>
# <holder>Ericsson AB, All Rights Reserved</holder>
#</copyright>
#<legalnotice>
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
# The Initial Developer of the Original Code is Ericsson AB.
#</legalnotice>
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include vsn.mk
VSN = $(XMERL_VSN)

SUB_DIRECTORIES = src doc/src

DIR_NAME = xmerl-$(VSN)

ifndef APP_RELEASE_DIR
  APP_RELEASE_DIR = /tmp
endif

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

info:
	@echo "APP_RELEASE_DIR: $(APP_RELEASE_DIR)"
	@echo "APP_DIR:         $(APP_DIR)"
	@echo "APP_TAR_FILE:    $(APP_TAR_FILE)"

version:
	@echo "$(VSN)"


# ----------------------------------------------------
# Application (source) release targets
# ----------------------------------------------------
#app_release: app_doc tar
app_release: tar

app_doc:
	cd doc/examles; $(MAKE) html man

app_dir: $(APP_DIR)

$(APP_DIR):
	cat TAR.exclude > TAR.exclude2; \
        echo "xmerl/TAR.exclude2" >> TAR.exclude2; \
	(cd ..; find xmerl -name 'findmerge.*' >> xmerl/TAR.exclude2)
	(cd ..; find xmerl -name '*.contrib*' >> xmerl/TAR.exclude2)
	(cd ..; find xmerl -name '*.keep*' >> xmerl/TAR.exclude2)
	(cd ..; find xmerl -name '*~' >> xmerl/TAR.exclude2)
	(cd ..; find xmerl -name 'erl_crash.dump' >> xmerl/TAR.exclude2)
	(cd ..; find xmerl -name '*.log' >> xmerl/TAR.exclude2)
	(cd ..; find xmerl -name 'core' >> xmerl/TAR.exclude2)
	(cd ..; find xmerl -name '.cmake.state' >> xmerl/TAR.exclude2)
	mkdir $(APP_DIR); \
        (cd ..; tar cfX - xmerl/TAR.exclude2 xmerl) | \
        (cd $(APP_DIR); tar xf -); \
        mv $(APP_DIR)/xmerl/* $(APP_DIR)/; \
        rmdir $(APP_DIR)/xmerl; \
	mkdir $(APP_DIR)/doc; \
	(cd doc; tar cf - html) | (cd $(APP_DIR)/doc; tar xf -)
#	(cd doc; tar cf - man3 html) | (cd $(APP_DIR)/doc; tar xf -)

tar: $(APP_TAR_FILE)

$(APP_TAR_FILE): $(APP_DIR)
	(cd $(APP_RELEASE_DIR); gtar zcf $(APP_TAR_FILE) $(DIR_NAME))


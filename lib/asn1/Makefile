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
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

#
# Macros
#

SUB_DIRECTORIES = src doc/src c_src

static_lib: SUB_DIRECTORIES = c_src


include vsn.mk
VSN = $(ASN1_VSN)

DIR_NAME = asn1-$(VSN)

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
app_release: app_doc tar

app_doc:
	cd doc/src; $(MAKE) html man

app_dir: $(APP_DIR)

$(APP_DIR):
	cat TAR.exclude > TAR.exclude2; \
        echo "asn1/TAR.exclude2" >> TAR.exclude2; \
        echo "asn1/priv/lib/$(TARGET)" >> TAR.exclude2; \
        echo "asn1/c_src/$(TARGET)" >> TAR.exclude2
	(cd ..; find asn1 -name 'findmerge.*' >> asn1/TAR.exclude2)
	(cd ..; find asn1 -name '*.contrib*' >> asn1/TAR.exclude2)
	(cd ..; find asn1 -name '*.keep*' >> asn1/TAR.exclude2)
	(cd ..; find asn1 -name '*~' >> asn1/TAR.exclude2)
	(cd ..; find asn1 -name 'erl_crash.dump' >> asn1/TAR.exclude2)
	(cd ..; find asn1 -name '*.log' >> asn1/TAR.exclude2)
	(cd ..; find asn1 -name 'core' >> asn1/TAR.exclude2)
	(cd ..; find asn1 -name '.cmake.state' >> asn1/TAR.exclude2)
	mkdir $(APP_DIR); \
        (cd ..; tar cfX - asn1/TAR.exclude2 asn1) | \
        (cd $(APP_DIR); tar xf -); \
        mv $(APP_DIR)/asn1/* $(APP_DIR)/; \
        rmdir $(APP_DIR)/asn1
	mkdir $(APP_DIR)/doc; \
	(cd doc; tar cf - man3 html) | (cd $(APP_DIR)/doc; tar xf -)

tar: $(APP_TAR_FILE)

$(APP_TAR_FILE): $(APP_DIR)
	(cd $(APP_RELEASE_DIR); gtar zcf $(APP_TAR_FILE) $(DIR_NAME))


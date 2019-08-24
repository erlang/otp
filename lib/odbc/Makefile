#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

include subdirs.mk

include vsn.mk

VSN = $(ODBC_VSN)

SPECIAL_TARGETS = 

DIR_NAME = odbc_c_src-$(VSN)

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


# ----------------------------------------------------
# Default Subdir Targets
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_subdir.mk

conf: do_configure

do_configure: configure 
	./configure 

configure: configure.in
	autoconf

.PHONY: info

info: 
	@echo "ODBC_VSN:  $(ODBC_VSN)"


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
	cat TAR.exclude | grep -v "odbc/doc" > TAR.exclude2; \
        echo "odbc/doc/src" >> TAR.exclude2; \
        echo "odbc/TAR.exclude2" >> TAR.exclude2; \
        echo "odbc/doc/internal" >> TAR.exclude2
	(cd ..; find odbc -name 'findmerge.*' >> odbc/TAR.exclude2)
	(cd ..; find odbc -name '*.contrib*' >> odbc/TAR.exclude2)
	(cd ..; find odbc -name '*.keep*' >> odbc/TAR.exclude2)
	(cd ..; find odbc -name '*~' >> odbc/TAR.exclude2)
	(cd ..; find odbc -name '*.log' >> odbc/TAR.exclude2)
	(cd ..; find odbc -name 'erl_crash.dump' >> odbc/TAR.exclude2)
	mkdir $(APP_DIR); \
        (cd ..; tar cfX - odbc/TAR.exclude2 odbc) | \
        (cd $(APP_DIR); tar xf -); \
        mv $(APP_DIR)/odbc/* $(APP_DIR)/; \
        find $(APP_DIR)/odbc -name '.cmake.state' | xargs rm -f; \
	mkdir $(APP_DIR)/autoconf; \
        cp autoconf/config.guess $(APP_DIR)/autoconf/; \
        cp autoconf/config.sub $(APP_DIR)/autoconf/; \
        cp autoconf/install-sh $(APP_DIR)/autoconf/; \
        rmdir $(APP_DIR)/odbc

tar: $(APP_TAR_FILE)

$(APP_TAR_FILE): $(APP_DIR)
	(cd $(APP_RELEASE_DIR); gtar zcf $(APP_TAR_FILE) $(DIR_NAME))

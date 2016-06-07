#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(PERCEPT_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/percept-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
CONF_FILES = \
	server_root/conf/mime.types 

HTDOCS_FILES = \
	server_root/htdocs/index.html

IMAGE_FILES = \
	server_root/images/nav.png \
	server_root/images/white.png

SCRIPT_FILES = \
	server_root/scripts/percept_area_select.js \
	server_root/scripts/percept_error_handler.js \
	server_root/scripts/percept_select_all.js

CSS_FILES = \
	server_root/css/percept.css	

FONT_FILES = \
	fonts/6x11_latin1.wingsfont

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: 

clean:

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	# Finished
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/logs"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/server_root"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/server_root/htdocs"
	$(INSTALL_DATA) $(HTDOCS_FILES) "$(RELSYSDIR)/priv/server_root/htdocs"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/server_root/conf"
	$(INSTALL_DATA) $(CONF_FILES) "$(RELSYSDIR)/priv/server_root/conf"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/server_root/scripts"
	$(INSTALL_DATA) $(SCRIPT_FILES) "$(RELSYSDIR)/priv/server_root/scripts"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/server_root/css"
	$(INSTALL_DATA) $(CSS_FILES) "$(RELSYSDIR)/priv/server_root/css"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/server_root/images"
	$(INSTALL_DATA) $(IMAGE_FILES) "$(RELSYSDIR)/priv/server_root/images"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/fonts"
	$(INSTALL_DATA) $(FONT_FILES) "$(RELSYSDIR)/priv/fonts"
	
release_docs_spec:


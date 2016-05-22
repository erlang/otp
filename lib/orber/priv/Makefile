#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

include ../vsn.mk
VSN = $(ORBER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/orber-$(VSN)

#
# Macros
#
HELP_FILES = \
	orber_help.txt

HTML_FILES = \
	orber.tool \
	blank.html \
	info_frames.html \
	main_frame.html \
	start_info.html

#
# Rules
#

debug opt:

docs:

clean:


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/priv"
	$(INSTALL_DATA) $(HELP_FILES) $(HTML_FILES) "$(RELSYSDIR)/priv"

release_docs_spec:



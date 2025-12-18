# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
# Copyright Ericsson AB 2004-2025. All Rights Reserved.
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
# Alternatively, you may use this file under the terms of the GNU Lesser
# General Public License (the "LGPL") as published by the Free Software
# Foundation; either version 2.1, or (at your option) any later version.
# If you wish to allow use of your version of this file only under the
# terms of the LGPL, you should delete the provisions above and replace
# them with the notice and other provisions required by the LGPL; see
# <http://www.gnu.org/licenses/>. If you do not delete the provisions
# above, a recipient may use your version of this file under the terms of
# either the Apache License or the LGPL.
#
# %CopyrightEnd%
# 
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

#
# Macros
#

SUB_DIRECTORIES = src doc

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

.PHONY: version

version:
	@echo "$(VSN)"

include $(ERL_TOP)/make/app_targets.mk

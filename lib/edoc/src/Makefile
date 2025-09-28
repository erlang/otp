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
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(EDOC_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/edoc-$(VSN)


#
# Common Macros
#

EBIN = ../ebin
XMERL = ../../xmerl
ERL_COMPILE_FLAGS += -pa $(XMERL) -I../include -I$(XMERL)/include -I${ERL_TOP}/lib
ERL_COMPILE_FLAGS += +warn_unused_vars +warn_unused_import +warn_deprecated_guard +no_docs +nowarn_missing_spec_documented +warn_deprecated_catch -Werror

include files.mk

OBJECTS=$(ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= edoc.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= edoc.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TYPES): $(OBJECTS)

all: $(OBJECTS)

$(OBJECTS): $(HRL_FILES) $(XMERL)/include/xmerl.hrl

docs: 

clean:
	rm -f $(OBJECTS) edoc_parser.erl
	rm -f core *~

distclean: clean

realclean: clean

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(OBJECTS) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(YRL_FILE) "$(RELSYSDIR)/src"

release_docs_spec:

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------
include $(ERL_TOP)/make/dep.mk

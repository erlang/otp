## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
##
## Copyright Ericsson AB 2004-2025. All Rights Reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
## Alternatively, you may use this file under the terms of the GNU Lesser
## General Public License (the "LGPL") as published by the Free Software
## Foundation; either version 2.1, or (at your option) any later version.
## If you wish to allow use of your version of this file only under the
## terms of the LGPL, you should delete the provisions above and replace
## them with the notice and other provisions required by the LGPL; see
## <http://www.gnu.org/licenses/>. If you do not delete the provisions
## above, a recipient may use your version of this file under the terms of
## either the Apache License or the LGPL.
##
## %CopyrightEnd%
# Authors: Richard Carlsson, Bertil Karlsson
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(SYNTAX_TOOLS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/syntax_tools-$(VSN)


#
# Common Macros
#

EBIN = ../ebin
INCLUDE=../include

ERL_COMPILE_FLAGS += -pa $(EBIN) -pa ./ -I$(INCLUDE)

ERL_COMPILE_FLAGS += +warn_deprecated_catch +warn_obsolete_bool_op +warn_unused_import -Werror # +warn_missing_spec +warn_untyped_record

ERL_FILES=erl_syntax.erl erl_prettypr.erl erl_syntax_lib.erl	\
	erl_comment_scan.erl erl_recomment.erl epp_dodger.erl	\
	prettypr.erl merl.erl merl_transform.erl

INCLUDE_FILES = merl.hrl

OBJECTS=$(ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

INCLUDE_DELIVERABLES = $(INCLUDE_FILES:%=$(INCLUDE)/%)

APP_FILE= syntax_tools.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= syntax_tools.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TYPES): $(OBJECTS)

all: $(OBJECTS)


clean:
	rm -f ./merl_transform.beam
	rm -f $(OBJECTS)
	rm -f core *~

docs: 

distclean: clean

realclean: clean

$(EBIN)/%.$(EMULATOR):%.erl
	$(erlc_verbose)erlc -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

# special rules and dependencies to apply the transform to itself
$(EBIN)/merl_transform.beam: $(EBIN)/merl.beam ./merl_transform.beam \
			../include/merl.hrl $(EBIN)/erl_comment_scan.beam \
			$(EBIN)/erl_syntax.beam $(EBIN)/erl_syntax_lib.beam
	$(V_ERLC) -pa $(ERL_TOP)/lib/syntax_tools/src $(ERL_COMPILE_FLAGS) -o $(EBIN) merl_transform.erl
./merl_transform.beam: ./merl_transform.erl $(EBIN)/merl.beam \
			../include/merl.hrl
	$(V_ERLC) -DMERL_NO_TRANSFORM $(ERL_COMPILE_FLAGS) -o ./ $<


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
	$(INSTALL_DATA) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(INCLUDE_DELIVERABLES) "$(RELSYSDIR)/include"

release_docs_spec:

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------
EXTRA_DEP_DEPENDENCIES=$(EBIN)/merl_transform.beam
include $(ERL_TOP)/make/dep.mk

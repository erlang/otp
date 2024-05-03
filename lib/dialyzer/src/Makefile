#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2006-2024. All Rights Reserved.
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
#=============================================================================
#
#    File:  lib/dialyzer/src/Makefile
# Authors:  Kostis Sagonas and Tobias Lindahl
#
#=============================================================================

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(DIALYZER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/dialyzer-$(VSN)

# ----------------------------------------------------
# Orientation information
# ----------------------------------------------------
DIALYZER_DIR = $(ERL_TOP)/lib/dialyzer

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES = \
	cerl_prettypr \
	dialyzer \
	dialyzer_analysis_callgraph \
	dialyzer_behaviours \
	dialyzer_callgraph \
	dialyzer_cl \
	dialyzer_cl_parse \
	dialyzer_clean_core \
	dialyzer_codeserver \
	dialyzer_contracts \
	dialyzer_dataflow \
	dialyzer_dep \
	dialyzer_dot \
	dialyzer_incremental \
	dialyzer_options \
	dialyzer_iplt \
	dialyzer_cplt \
	dialyzer_plt \
	dialyzer_succ_typings \
	dialyzer_timing \
	dialyzer_typesig \
	dialyzer_typegraph \
	dialyzer_coordinator \
	dialyzer_worker \
	dialyzer_utils \
	erl_bif_types \
	erl_types \
	typer \
	typer_core

HRL_FILES= dialyzer.hrl
ERL_FILES= $(MODULES:%=%.erl)
INSTALL_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)
TARGET_FILES= $(INSTALL_FILES)

APP_FILE= dialyzer.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= dialyzer.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_COMPILE_FLAGS += -Werror
ERL_COMPILE_FLAGS += +warn_export_vars +warn_unused_import +warn_missing_spec

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TYPES): $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)
	rm -f core

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EBIN)/dialyzer_cl_parse.$(EMULATOR): dialyzer_cl_parse.erl ../vsn.mk
	$(erlc_verbose)erlc -W  $(ERL_COMPILE_FLAGS) -DVSN="\"v$(VSN)\"" -o$(EBIN) dialyzer_cl_parse.erl

$(EBIN)/dialyzer_plt.$(EMULATOR): dialyzer_plt.erl ../vsn.mk
	$(erlc_verbose)erlc -W  $(ERL_COMPILE_FLAGS) -DVSN="\"v$(VSN)\"" -o$(EBIN) dialyzer_plt.erl

$(EBIN)/dialyzer_cplt.$(EMULATOR): dialyzer_cplt.erl ../vsn.mk
	$(erlc_verbose)erlc -W  $(ERL_COMPILE_FLAGS) -DVSN="\"v$(VSN)\"" -o$(EBIN) dialyzer_cplt.erl

$(EBIN)/dialyzer_iplt.$(EMULATOR): dialyzer_iplt.erl ../vsn.mk
	$(erlc_verbose)erlc -W  $(ERL_COMPILE_FLAGS) -DVSN="\"v$(VSN)\"" -o$(EBIN) dialyzer_iplt.erl

$(EBIN)/typer.$(EMULATOR): typer.erl ../vsn.mk
	$(erlc_verbose)erlc -W  $(ERL_COMPILE_FLAGS) -DVSN="\"v$(VSN)\"" -o$(EBIN) typer.erl

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

# ---------------------------------------------------------------------
# dependencies -- I wish they were somehow automatically generated
# ---------------------------------------------------------------------

$(EBIN)/dialyzer.beam: dialyzer.hrl
$(EBIN)/dialyzer_analysis_callgraph.beam: dialyzer.hrl
$(EBIN)/dialyzer_callgraph.beam: dialyzer.hrl
$(EBIN)/dialyzer_behaviours.beam: dialyzer.hrl
$(EBIN)/dialyzer_cl.beam: dialyzer.hrl ../../kernel/include/file.hrl
$(EBIN)/dialyzer_cl_parse.beam: dialyzer.hrl
$(EBIN)/dialyzer_codeserver.beam: dialyzer.hrl
$(EBIN)/dialyzer_contracts.beam: dialyzer.hrl
$(EBIN)/dialyzer_dataflow.beam: dialyzer.hrl
$(EBIN)/dialyzer_dep.beam: dialyzer.hrl
$(EBIN)/dialyzer_options.beam: dialyzer.hrl
$(EBIN)/dialyzer_plt.beam: dialyzer.hrl
$(EBIN)/dialyzer_race_data_server.beam: dialyzer.hrl
$(EBIN)/dialyzer_races.beam: dialyzer.hrl
$(EBIN)/dialyzer_succ_typings.beam: dialyzer.hrl
$(EBIN)/dialyzer_typesig.beam: dialyzer.hrl
$(EBIN)/dialyzer_typegraph.beam: dialyzer.hrl
$(EBIN)/dialyzer_utils.beam: dialyzer.hrl

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(EXTRA_FILES) \
		"$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(INSTALL_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

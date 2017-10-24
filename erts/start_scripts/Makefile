#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2017. All Rights Reserved.
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

.NOTPARALLEL:

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------

SS_ROOT = $(ERL_TOP)/erts/start_scripts
SS_TMP = $(SS_ROOT)/tmp
LIBPATH= $(ERL_TOP)/lib
SCRIPT_PATH = -I$(LIBPATH)/kernel/ebin -I$(LIBPATH)/stdlib/ebin -I$(LIBPATH)/sasl/ebin
BOOTSTRAP_LIBPATH=$(BOOTSTRAP_TOP)/lib
BOOTSTRAP_SCRIPT_PATH = -I$(BOOTSTRAP_LIBPATH)/kernel/ebin -I$(BOOTSTRAP_LIBPATH)/stdlib/ebin

INSTALL_SCRIPTS = \
	$(SS_ROOT)/start_clean.script \
	$(SS_ROOT)/start_clean.boot \
	$(SS_ROOT)/start_sasl.boot \
	$(SS_ROOT)/start_sasl.script \
	$(SS_ROOT)/no_dot_erlang.boot \
	$(SS_ROOT)/no_dot_erlang.script


REL_SCRIPTS = \
	$(SS_ROOT)/start_clean.rel \
	$(SS_ROOT)/start_sasl.rel \
	$(SS_ROOT)/start_all_example.rel \
	$(SS_ROOT)/no_dot_erlang.rel \

ifneq ($(findstring win32,$(TARGET)),win32)
RELEASES_SRC = RELEASES.src
endif

##############################################################################
# Get version numbers from the VSN files

# VSN
include ../vsn.mk
include $(LIBPATH)/kernel/vsn.mk
include $(LIBPATH)/stdlib/vsn.mk
-include $(LIBPATH)/sasl/vsn.mk
-include $(LIBPATH)/os_mon/vsn.mk
-include $(LIBPATH)/mnesia/vsn.mk
-include $(LIBPATH)/snmp/vsn.mk
-include $(LIBPATH)/inets/vsn.mk

##############################################################################

debug opt lcnt script: rel $(INSTALL_SCRIPTS) $(RELEASES_SRC)

rel:	$(REL_SCRIPTS)

RELEASES.src: $(SS_ROOT)/start_sasl.rel
	$(gen_verbose)$(INSTALL_DIR) $(SS_TMP)
	$(V_at)( cd $(SS_TMP) && \
	$(ERL) -noinput +B -eval 'release_handler:create_RELEASES("%ERL_ROOT%", "$(SS_ROOT)", "$(SS_ROOT)/start_sasl.rel", []), halt()')
	$(V_at)mv RELEASES RELEASES.src

$(SS_ROOT)/start_clean.script \
$(SS_ROOT)/start_clean.boot:	$(SS_ROOT)/start_clean.rel
	$(gen_verbose)$(INSTALL_DIR) $(SS_TMP)
	$(V_at)( cd $(SS_TMP) && \
	$(ERLC) $(SASL_FLAGS) $(SCRIPT_PATH) +no_warn_sasl -o $(SS_ROOT) $< )

$(SS_ROOT)/start_sasl.script \
$(SS_ROOT)/start_sasl.boot:	$(SS_ROOT)/start_sasl.rel
	$(gen_verbose)$(INSTALL_DIR) $(SS_TMP)
	$(V_at)( cd $(SS_TMP) && \
	$(ERLC) $(SASL_FLAGS) $(SCRIPT_PATH) -o $(SS_ROOT) $< )

$(SS_ROOT)/no_dot_erlang.script \
$(SS_ROOT)/no_dot_erlang.boot:	$(SS_ROOT)/no_dot_erlang.rel
	$(gen_verbose)$(INSTALL_DIR) $(SS_TMP)
	$(V_at)( cd $(SS_TMP) && \
	$(ERLC) $(SASL_FLAGS) $(SCRIPT_PATH) +no_warn_sasl +no_dot_erlang -o $(SS_ROOT) $< )


$(SS_ROOT)/start_clean.rel: $(SS_ROOT)/start_clean.rel.src \
			../vsn.mk \
			$(LIBPATH)/kernel/vsn.mk \
			$(LIBPATH)/stdlib/vsn.mk
	$(gen_verbose)sed -e 's;%SYS_VSN%;$(SYSTEM_VSN);' \
            -e 's;%ERTS_VSN%;$(VSN);' \
            -e 's;%KERNEL_VSN%;$(KERNEL_VSN);' \
            -e 's;%STDLIB_VSN%;$(STDLIB_VSN);' \
          $(SS_ROOT)/start_clean.rel.src > $(SS_ROOT)/start_clean.rel

$(SS_ROOT)/start_sasl.rel:	$(SS_ROOT)/start_sasl.rel.src \
			../vsn.mk \
			$(LIBPATH)/kernel/vsn.mk \
			$(LIBPATH)/stdlib/vsn.mk \
			$(LIBPATH)/sasl/vsn.mk
	$(gen_verbose)sed -e 's;%SYS_VSN%;$(SYSTEM_VSN);' \
            -e 's;%ERTS_VSN%;$(VSN);' \
            -e 's;%KERNEL_VSN%;$(KERNEL_VSN);' \
            -e 's;%STDLIB_VSN%;$(STDLIB_VSN);' \
            -e 's;%SASL_VSN%;$(SASL_VSN);' \
          $(SS_ROOT)/start_sasl.rel.src > $(SS_ROOT)/start_sasl.rel

$(SS_ROOT)/no_dot_erlang.rel: $(SS_ROOT)/no_dot_erlang.rel.src \
			../vsn.mk \
			$(LIBPATH)/kernel/vsn.mk \
			$(LIBPATH)/stdlib/vsn.mk
	$(gen_verbose)sed -e 's;%SYS_VSN%;$(SYSTEM_VSN);' \
            -e 's;%ERTS_VSN%;$(VSN);' \
            -e 's;%KERNEL_VSN%;$(KERNEL_VSN);' \
            -e 's;%STDLIB_VSN%;$(STDLIB_VSN);' \
          $(SS_ROOT)/no_dot_erlang.rel.src > $(SS_ROOT)/no_dot_erlang.rel

$(SS_ROOT)/start_all_example.rel:	$(SS_ROOT)/start_all_example.rel.src \
			../vsn.mk \
			$(LIBPATH)/kernel/vsn.mk \
			$(LIBPATH)/stdlib/vsn.mk \
			$(LIBPATH)/sasl/vsn.mk \
			$(LIBPATH)/os_mon/vsn.mk \
			$(LIBPATH)/mnesia/vsn.mk \
			$(LIBPATH)/snmp/vsn.mk \
			$(LIBPATH)/inets/vsn.mk
	$(gen_verbose)sed -e 's;%SYS_VSN%;$(SYSTEM_VSN);' \
            -e 's;%ERTS_VSN%;$(VSN);' \
            -e 's;%KERNEL_VSN%;$(KERNEL_VSN);' \
            -e 's;%STDLIB_VSN%;$(STDLIB_VSN);' \
            -e 's;%SASL_VSN%;$(SASL_VSN);' \
            -e 's;%OS_MON_VSN%;$(OS_MON_VSN);' \
            -e 's;%MNESIA_VSN%;$(MNESIA_VSN);' \
            -e 's;%SNMP_VSN%;$(SNMPEA_VSN);' \
            -e 's;%INETS_VSN%;$(INETS_VSN);' \
          $(SS_ROOT)/start_all_example.rel.src > $(SS_ROOT)/start_all_example.rel

## Special target used from $(ERL_TOP)/erts/Makefile.
$(ERL_TOP)/bin/start.script:
	$(gen_verbose)$(INSTALL_DIR) $(SS_TMP)
	$(V_at)( cd $(SS_TMP) && \
	  $(ERLC) $(SCRIPT_PATH) +no_warn_sasl +otp_build -o $@ $(SS_ROOT)/start_clean.rel )

$(ERL_TOP)/bin/start_sasl.script:
	$(gen_verbose)$(INSTALL_DIR) $(SS_TMP)
	$(V_at)( cd $(SS_TMP) && \
	  $(ERLC) $(SCRIPT_PATH) +otp_build -o $@ $(SS_ROOT)/start_sasl.rel )

$(ERL_TOP)/bin/start_clean.script:
	$(gen_verbose)$(INSTALL_DIR) $(SS_TMP)
	$(V_at)( cd $(SS_TMP) && \
	  $(ERLC) $(SCRIPT_PATH) +no_warn_sasl +otp_build -o $@ $(SS_ROOT)/start_clean.rel )

$(ERL_TOP)/bin/no_dot_erlang.script:
	$(gen_verbose)$(INSTALL_DIR) $(SS_TMP)
	$(V_at)( cd $(SS_TMP) && \
	  $(ERLC) $(SCRIPT_PATH) +no_warn_sasl +otp_build +no_dot_erlang -o $@ $(SS_ROOT)/no_dot_erlang.rel )

## Special target used from system/build/Makefile for source code release bootstrap.
bootstrap_scripts: $(SS_ROOT)/start_clean.rel $(SS_ROOT)/no_dot_erlang.rel
	$(V_at)$(INSTALL_DIR) $(TESTROOT)/bin
	$(V_at)$(INSTALL_DIR) $(SS_TMP)
	$(V_at)( cd $(SS_TMP) && \
	  $(ERLC) $(BOOTSTRAP_SCRIPT_PATH) +otp_build +no_module_tests \
	    -o $(TESTROOT)/bin/start.script $(SS_ROOT)/start_clean.rel )
	$(V_at)( cd $(SS_TMP) && \
	  $(ERLC) $(BOOTSTRAP_SCRIPT_PATH) +otp_build +no_module_tests \
	    -o $(TESTROOT)/bin/start_clean.script $(SS_ROOT)/start_clean.rel )
	$(V_at)( cd $(SS_TMP) && \
	  $(ERLC) $(BOOTSTRAP_SCRIPT_PATH) +otp_build +no_module_tests \
	    -o $(TESTROOT)/bin/no_dot_erlang.script $(SS_ROOT)/no_dot_erlang.rel )


clean:
	$(V_at)$(RM) $(REL_SCRIPTS) $(INSTALL_SCRIPTS)

docs:


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: script
	$(V_at)$(INSTALL_DIR) "$(RELEASE_PATH)/releases/$(SYSTEM_VSN)"
ifneq ($(findstring win32,$(TARGET)),win32)
	$(V_at)$(INSTALL_DATA) RELEASES.src "$(RELEASE_PATH)/releases"
endif
	$(V_at)$(INSTALL_DATA) $(INSTALL_SCRIPTS) $(REL_SCRIPTS) \
	   "$(RELEASE_PATH)/releases/$(SYSTEM_VSN)"
	$(V_at)$(INSTALL_DATA) start_clean.script "$(RELEASE_PATH)/releases/$(SYSTEM_VSN)/start.script"
	$(V_at)$(INSTALL_DATA) start_clean.boot "$(RELEASE_PATH)/releases/$(SYSTEM_VSN)/start.boot"


release_docs_spec:

TRUE:


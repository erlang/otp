#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
VSN=$(OBSERVER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/observer-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

MODULES= \
	crashdump_viewer \
	cdv_atom_cb \
	cdv_bin_cb \
	cdv_detail_wx \
	cdv_dist_cb \
	cdv_ets_cb \
	cdv_fun_cb \
	cdv_gen_cb \
	cdv_html_wx \
	cdv_info_wx \
	cdv_int_tab_cb \
	cdv_mem_cb \
	cdv_mod_cb \
	cdv_multi_wx \
	cdv_port_cb \
	cdv_proc_cb \
	cdv_sched_cb \
	cdv_table_wx \
	cdv_term_cb \
	cdv_timer_cb \
	cdv_virtual_list_wx \
	cdv_wx \
	etop \
	etop_tr \
	etop_txt \
	observer \
	observer_app_wx \
	observer_alloc_wx \
	observer_html_lib \
	observer_lib \
	observer_perf_wx \
	observer_port_wx \
	observer_pro_wx \
	observer_procinfo \
	observer_sys_wx \
	observer_trace_wx \
	observer_traceoptions_wx \
	observer_tv_table \
	observer_tv_wx \
	observer_wx \
	ttb \
	ttb_et

HRL_FILES= \
	../include/etop.hrl
INTERNAL_HRL_FILES= \
	observer_tv.hrl \
	observer_defs.hrl \
	crashdump_viewer.hrl \
	etop_defs.hrl
ERL_FILES= $(MODULES:%=%.erl)
EXAMPLE_FILES= multitrace.erl

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

PRIVDIR= ../priv
WEBTOOLFILES= $(PRIVDIR)/crashdump_viewer.tool $(PRIVDIR)/erlang_observer.png
BINDIR= $(PRIVDIR)/bin
ifeq ($(findstring win32,$(TARGET)),win32)
WIN32_EXECUTABLES= $(BINDIR)/etop.bat $(BINDIR)/cdv.bat
else
WIN32_EXECUTABLES=
endif
EXECUTABLES= \
	$(BINDIR)/etop \
	$(BINDIR)/cdv \
	$(WIN32_EXECUTABLES)
CDVDIR= $(PRIVDIR)/crashdump_viewer
GIF_FILES= \
	$(CDVDIR)/collapsd.gif \
	$(CDVDIR)/exploded.gif

APP_FILE= observer.app

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= observer.appup

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += \
	-I../include \
	-I ../../et/include \
	-I ../../../libraries/et/include \
	-Werror

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
opt debug: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(TARGET_FILES): $(INTERNAL_HRL_FILES)

docs:

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples"
	$(INSTALL_DATA) $(EXAMPLE_FILES) "$(RELSYSDIR)/examples"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/include"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/bin"
	$(INSTALL_SCRIPT) $(EXECUTABLES) "$(RELSYSDIR)/priv/bin"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/crashdump_viewer"
	$(INSTALL_DATA) $(WEBTOOLFILES) "$(RELSYSDIR)/priv"
	$(INSTALL_DATA) $(GIF_FILES) "$(RELSYSDIR)/priv/crashdump_viewer"

release_docs_spec:








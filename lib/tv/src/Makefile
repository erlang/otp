#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2012. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# %CopyrightEnd%
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(TV_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/tv-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

MODULES= \
	tv \
	tv_comm_func \
	tv_db \
	tv_db_search \
	tv_db_sort \
	tv_ets_rpc \
	tv_etsread \
	tv_info \
	tv_io_lib \
	tv_io_lib_format \
	tv_io_lib_pretty \
	tv_ip \
	tv_main \
	tv_mnesia_rpc \
	tv_new_table \
	tv_nodewin \
	tv_pb \
	tv_pb_funcs \
	tv_pc \
	tv_pc_graph_ctrl \
	tv_pc_menu_handling \
	tv_pd \
	tv_pd_display \
	tv_pd_frames \
	tv_pd_scale \
	tv_pg \
	tv_pg_gridfcns \
	tv_poll_dialog \
	tv_pw \
	tv_pw_window \
	tv_rec_edit \
	tv_table_owner \
	tv_utils



HRL_FILES= \
	tv_db_int_def.hrl \
	tv_int_def.hrl \
	tv_int_msg.hrl \
	tv_main.hrl \
	tv_pb_int_def.hrl \
	tv_pc_int_def.hrl \
	tv_pd_int_def.hrl \
	tv_pd_int_msg.hrl \
	tv_pg_int_def.hrl \
	tv_pw_int_def.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE = tv.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_FILE = tv.appup
APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)
# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += +warn_obsolete_guard

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

docs:

# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(HRL_FILES) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:


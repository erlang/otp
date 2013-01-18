#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1996-2012. All Rights Reserved.
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
#

ifdef BOOTSTRAP
EGEN=$(BOOTSTRAP_TOP)/lib/stdlib/egen
EBIN=$(BOOTSTRAP_TOP)/lib/stdlib/ebin
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(STDLIB_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/stdlib-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES= \
	array \
	base64 \
	beam_lib \
	binary \
	c \
	calendar \
	dets \
	dets_server \
	dets_sup \
	dets_utils \
	dets_v8 \
	dets_v9 \
	dict \
	digraph \
	digraph_utils \
	edlin \
	edlin_expand \
	epp \
	erl_bits \
	erl_compile \
	erl_eval \
	erl_expand_records \
	erl_internal \
	erl_lint \
	erl_parse \
	erl_posix_msg \
	erl_pp \
	erl_scan \
	erl_tar \
	error_logger_file_h \
	error_logger_tty_h \
	escript \
	ets \
	eval_bits \
	file_sorter \
	filelib \
	filename \
	gb_trees \
	gb_sets \
	gen \
	gen_event \
	gen_fsm \
	gen_server \
	io \
	io_lib \
	io_lib_format \
	io_lib_fread \
	io_lib_pretty \
	lib \
	lists \
	log_mf_h \
	math \
	ms_transform \
	otp_internal \
	orddict \
	ordsets \
	pg \
	re \
	pool \
	proc_lib \
	proplists \
	qlc \
	qlc_pt \
	queue \
	random \
	sets \
	shell \
	shell_default \
	slave \
	sofs \
	string \
	supervisor \
	supervisor_bridge \
	sys \
	timer \
	unicode \
	win32reg \
	zip

HRL_FILES= \
	../include/erl_compile.hrl \
	../include/erl_bits.hrl \
	../include/ms_transform.hrl \
	../include/qlc.hrl \
	../include/zip.hrl

INTERNAL_HRL_FILES= dets.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= stdlib.app

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= stdlib.appup

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(NATIVE_LIBS_ENABLED),yes)
ERL_COMPILE_FLAGS += +native
endif
ERL_COMPILE_FLAGS += -I../include -I../../kernel/include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES)
	rm -f core
	rm -f erl_parse.erl

docs:

# This is a trick so that the preloaded files will get the correct type
# specifications.
primary_bootstrap_compiler: \
  $(BOOTSTRAP_COMPILER)/ebin/epp.beam \
  $(BOOTSTRAP_COMPILER)/ebin/erl_scan.beam \
  $(BOOTSTRAP_COMPILER)/ebin/erl_parse.beam \
  $(BOOTSTRAP_COMPILER)/ebin/erl_lint.beam \
  $(BOOTSTRAP_COMPILER)/ebin/io.beam \
  $(BOOTSTRAP_COMPILER)/ebin/otp_internal.beam

$(BOOTSTRAP_COMPILER)/ebin/erl_parse.beam: erl_parse.yrl
	$(gen_verbose)
	$(V_at)$(ERLC) -o $(BOOTSTRAP_COMPILER)/egen erl_parse.yrl
	$(V_at)$(ERLC) -o $(BOOTSTRAP_COMPILER)/ebin $(BOOTSTRAP_COMPILER)/egen/erl_parse.erl

$(BOOTSTRAP_TOP)/lib/stdlib/egen/erl_parse.erl: erl_parse.yrl
	$(yecc_verbose)$(ERLC) $(YRL_FLAGS) -o$(BOOTSTRAP_TOP)/lib/stdlib/egen erl_parse.yrl

$(BOOTSTRAP_COMPILER)/ebin/%.beam: %.erl
	$(V_ERLC) -o $(BOOTSTRAP_COMPILER)/ebin $<

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
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) erl_parse.yrl "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/include"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

# ----------------------------------------------------
# Dependencies -- alphabetically, please
# ---------------------------------------------------- 

$(EBIN)/beam_lib.beam: ../include/erl_compile.hrl ../../kernel/include/file.hrl
$(EBIN)/dets.beam: dets.hrl ../../kernel/include/file.hrl
$(EBIN)/dets_server.beam: dets.hrl
$(EBIN)/dets_utils.beam: dets.hrl
$(EBIN)/dets_v8.beam: dets.hrl
$(EBIN)/dets_v9.beam: dets.hrl
$(EBIN)/erl_bits.beam: ../include/erl_bits.hrl
$(EBIN)/erl_compile.beam: ../include/erl_compile.hrl ../../kernel/include/file.hrl
$(EBIN)/erl_lint.beam: ../include/erl_bits.hrl
$(EBIN)/erl_tar.beam: ../../kernel/include/file.hrl
$(EBIN)/file_sorter.beam: ../../kernel/include/file.hrl
$(EBIN)/filelib.beam: ../../kernel/include/file.hrl
$(EBIN)/filename.beam: ../../kernel/include/file.hrl
$(EBIN)/qlc_pt.beam: ../include/ms_transform.hrl
$(EBIN)/shell.beam: ../../kernel/include/file.hrl
$(EBIN)/zip.beam: ../include/zip.hrl ../../kernel/include/file.hrl

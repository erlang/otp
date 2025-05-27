#
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

ifdef BOOTSTRAP
EGEN=$(BOOTSTRAP_TOP)/lib/kernel/egen
EBIN=$(BOOTSTRAP_TOP)/lib/kernel/ebin
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------

# Include erts/system/vsn.mk to port number for EPMD -- we will
# get an unwanted definition for VSN too.  Therefore,
# we'll use KERNEL_VSN directly instead of assigning it to
# VSN which is done in other Makefiles.

include ../vsn.mk
include $(ERL_TOP)/erts/vsn.mk

include $(ERL_TOP)/erts/epmd/epmd.mk


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/kernel-$(KERNEL_VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------


MODULES = \
	application \
	application_controller \
	application_master \
	application_starter \
	auth \
	code \
	code_server \
	disk_log \
	disk_log_1 \
	disk_log_server \
	disk_log_sup \
	dist_ac \
	dist_util \
	erl_boot_server \
	erl_compile_server \
	erl_ddll \
	erl_debugger \
	erl_distribution \
	erl_erts_errors \
	erl_epmd \
	erl_kernel_errors \
	erl_reply \
	erl_signal_handler \
	erpc \
	erts_debug \
	error_handler \
	error_logger \
	file \
	file_io_server \
	file_server \
	gen_tcp \
	gen_tcp_socket \
	gen_udp \
	gen_udp_socket \
	gen_sctp \
	global \
	global_group \
	global_search \
	group \
	group_history \
	heart \
	inet \
	inet6_tcp \
	inet6_tcp_dist \
	inet6_udp \
	inet6_sctp \
	inet_config \
	inet_db \
	inet_dns \
	inet_dns_tsig \
	inet_epmd_dist \
	inet_epmd_socket \
	inet_gethost_native \
	inet_hosts \
	inet_parse \
	inet_res \
	inet_tcp \
	inet_tcp_dist \
	inet_udp \
	inet_sctp \
	kernel \
	kernel_config \
	kernel_refc \
	local_udp \
	local_tcp \
        logger \
        logger_backend \
        logger_config \
	logger_handler \
	logger_handler_watcher \
	logger_std_h \
	logger_disk_log_h \
	logger_h_common \
	logger_filters \
        logger_formatter \
	logger_olp \
	logger_proxy \
        logger_server \
        logger_simple_h \
        logger_sup \
	net \
	net_adm \
	net_kernel \
	os \
	pg \
	pg2 \
	ram_file \
	rpc \
	seq_trace \
	socket \
	standard_error \
	trace \
	user_drv \
	user_sup \
	prim_tty \
	prim_tty_sighandler \
	raw_file_io \
	raw_file_io_compressed \
	raw_file_io_inflate \
	raw_file_io_deflate \
	raw_file_io_delayed \
	raw_file_io_list \
	wrap_log_reader

HRL_FILES= ../include/file.hrl ../include/inet.hrl ../include/inet_sctp.hrl \
	../include/dist.hrl ../include/dist_util.hrl \
	../include/net_address.hrl ../include/logger.hrl ../include/eep48.hrl


INTERNAL_HRL_FILES= application_master.hrl disk_log.hrl \
        erl_epmd.hrl file_int.hrl \
        inet_dns.hrl inet_res.hrl \
        inet_boot.hrl inet_config.hrl inet_int.hrl \
	inet_dns_record_adts.hrl \
	logger_internal.hrl logger_olp.hrl logger_h_common.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) \
		       $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= kernel.app

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= kernel.appup

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_COMPILE_FLAGS += -Werror +warn_deprecated_catch
ERL_COMPILE_FLAGS += -I../include


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TYPES): $(TARGET_FILES) 

# Note: In the open-source build clean must not destroyed the preloaded
# beam files.
clean:
	rm -f $(TARGET_FILES)
	rm -f core


docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(KERNEL_VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(KERNEL_VSN);' $< > $@


EPMD_FLAGS = -Depmd_port_no=$(EPMD_PORT_NO)	\
             -Depmd_node_type=$(EPMD_NODE_TYPE)	\
             -Depmd_dist_low=$(EPMD_DIST_LOW)	\
             -Depmd_dist_high=$(EPMD_DIST_HIGH)	\
             -Derlang_daemon_port=$(EPMD_PORT_NO)

$(ESRC)/inet_dns_record_adts.hrl: $(ESRC)/inet_dns_record_adts.pl
	$(gen_verbose)LANG=C $(PERL) $< > $@

$(EBIN)/erl_epmd.beam: $(ESRC)/erl_epmd.erl
	$(V_ERLC) $(ERL_COMPILE_FLAGS) $(EPMD_FLAGS) -o$(EBIN) $<

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/include"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:


# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------
include $(ERL_TOP)/make/dep.mk
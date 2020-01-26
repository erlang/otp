#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1996-2019. All Rights Reserved.
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
# VSN which is done in other Makefiles.  Same with HIPE_VSN.

include ../vsn.mk
include $(ERL_TOP)/erts/vsn.mk
include $(ERL_TOP)/lib/hipe/vsn.mk

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
	erl_distribution \
	erl_epmd \
	erl_reply \
	erl_signal_handler \
	erts_debug \
	error_handler \
	error_logger \
	file \
	file_io_server \
	file_server \
	gen_tcp \
	gen_udp \
	gen_sctp \
	global \
	global_group \
	global_search \
	group \
	group_history \
	heart \
	hipe_unified_loader \
	inet \
	inet6_tcp \
	inet6_tcp_dist \
	inet6_udp \
	inet6_sctp \
	inet_config \
	inet_db \
	inet_dns \
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
	standard_error \
	user \
	user_drv \
	user_sup \
	raw_file_io \
	raw_file_io_compressed \
	raw_file_io_inflate \
	raw_file_io_deflate \
	raw_file_io_delayed \
	raw_file_io_list \
	raw_file_io_raw \
	wrap_log_reader

HRL_FILES= ../include/file.hrl ../include/inet.hrl ../include/inet_sctp.hrl \
	../include/dist.hrl ../include/dist_util.hrl \
	../include/net_address.hrl ../include/logger.hrl

INTERNAL_HRL_FILES= application_master.hrl disk_log.hrl \
        erl_epmd.hrl file_int.hrl hipe_ext_format.hrl \
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

ifeq ($(NATIVE_LIBS_ENABLED),yes)
ERL_COMPILE_FLAGS += +native
else
ERL_COMPILE_FLAGS += -Werror
endif
ERL_COMPILE_FLAGS += -I../include


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) 

# Note: In the open-source build clean must not destroyed the preloaded
# beam files.
clean:
	rm -f $(TARGET_FILES)
	rm -f core


docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

../../hipe/main/hipe.hrl: ../../hipe/vsn.mk ../../hipe/main/hipe.hrl.src
	$(vsn_verbose)sed -e "s;%VSN%;$(HIPE_VSN);" ../../hipe/main/hipe.hrl.src > ../../hipe/main/hipe.hrl

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


# Include dependencies -- list below added by Kostis Sagonas
$(EBIN)/application_controller.beam: application_master.hrl ../include/logger.hrl
$(EBIN)/application_master.beam: application_master.hrl
$(EBIN)/auth.beam: ../include/file.hrl
$(EBIN)/code.beam: ../include/file.hrl
$(EBIN)/code_server.beam: ../include/file.hrl
$(EBIN)/disk_log.beam: disk_log.hrl
$(EBIN)/disk_log_1.beam: disk_log.hrl ../include/file.hrl
$(EBIN)/disk_log_server.beam: disk_log.hrl
$(EBIN)/dist_util.beam: ../include/dist_util.hrl ../include/dist.hrl
$(EBIN)/erl_boot_server.beam: inet_boot.hrl
$(EBIN)/erl_epmd.beam: inet_int.hrl erl_epmd.hrl
$(EBIN)/error_logger.beam: logger_internal.hrl ../include/logger.hrl
$(EBIN)/file.beam: ../include/file.hrl file_int.hrl
$(EBIN)/file_io_server.beam: ../include/file.hrl file_int.hrl
$(EBIN)/gen_tcp.beam: inet_int.hrl
$(EBIN)/gen_udp.beam: inet_int.hrl
$(EBIN)/gen_sctp.beam: ../include/inet_sctp.hrl
$(EBIN)/global.beam: ../../stdlib/include/ms_transform.hrl
$(EBIN)/hipe_unified_loader.beam: ../../hipe/main/hipe.hrl hipe_ext_format.hrl
$(EBIN)/inet.beam: ../include/inet.hrl inet_int.hrl ../include/inet_sctp.hrl
$(EBIN)/inet6_tcp.beam: inet_int.hrl
$(EBIN)/inet6_tcp_dist.beam: ../include/net_address.hrl ../include/dist.hrl ../include/dist_util.hrl
$(EBIN)/inet6_udp.beam: inet_int.hrl
$(EBIN)/inet6_sctp.beam: inet_int.hrl
$(EBIN)/inet_config.beam: inet_config.hrl ../include/inet.hrl
$(EBIN)/inet_db.beam: ../include/inet.hrl inet_int.hrl inet_res.hrl inet_dns.hrl inet_config.hrl
$(EBIN)/inet_dns.beam: inet_int.hrl inet_dns.hrl inet_dns_record_adts.hrl
$(EBIN)/inet_gethost_native.beam: ../include/inet.hrl
$(EBIN)/inet_hosts.beam: ../include/inet.hrl
$(EBIN)/inet_parse.beam: ../include/file.hrl
$(EBIN)/inet_res.beam: ../include/inet.hrl inet_res.hrl inet_dns.hrl inet_int.hrl
$(EBIN)/inet_tcp.beam: inet_int.hrl
$(EBIN)/inet_udp_dist.beam: ../include/net_address.hrl ../include/dist.hrl ../include/dist_util.hrl
$(EBIN)/inet_udp.beam: inet_int.hrl
$(EBIN)/inet_sctp.beam: inet_int.hrl ../include/inet_sctp.hrl
$(EBIN)/local_udp.beam: inet_int.hrl
$(EBIN)/local_tcp.beam: inet_int.hrl
$(EBIN)/logger.beam: logger_internal.hrl ../include/logger.hrl
$(EBIN)/logger_backend.beam: logger_internal.hrl ../include/logger.hrl
$(EBIN)/logger_config.beam: logger_internal.hrl ../include/logger.hrl
$(EBIN)/logger_disk_log_h.beam: logger_h_common.hrl logger_internal.hrl ../include/logger.hrl ../include/file.hrl
$(EBIN)/logger_filters.beam: logger_internal.hrl ../include/logger.hrl
$(EBIN)/logger_formatter.beam: logger_internal.hrl ../include/logger.hrl
$(EBIN)/logger_olp.beam: logger_olp.hrl logger_internal.hrl
$(EBIN)/logger_proxy.beam: logger_internal.hrl
$(EBIN)/logger_server.beam: logger_internal.hrl ../include/logger.hrl
$(EBIN)/logger_simple_h.beam: logger_internal.hrl ../include/logger.hrl
$(EBIN)/logger_std_h.beam: logger_h_common.hrl logger_internal.hrl ../include/logger.hrl ../include/file.hrl
$(EBIN)/logger_h_common.beam: logger_h_common.hrl logger_internal.hrl ../include/logger.hrl
$(EBIN)/net_kernel.beam: ../include/net_address.hrl
$(EBIN)/os.beam: ../include/file.hrl
$(EBIN)/ram_file.beam: ../include/file.hrl
$(EBIN)/wrap_log_reader.beam: disk_log.hrl ../include/file.hrl
$(EBIN)/raw_file_io.beam: ../include/file.hrl file_int.hrl
$(EBIN)/raw_file_io_compressed.beam: ../include/file.hrl file_int.hrl
$(EBIN)/raw_file_io_inflate.beam: ../include/file.hrl file_int.hrl
$(EBIN)/raw_file_io_deflate.beam: ../include/file.hrl file_int.hrl
$(EBIN)/raw_file_io_delayed.beam: ../include/file.hrl file_int.hrl
$(EBIN)/raw_file_io_list.beam: ../include/file.hrl file_int.hrl
$(EBIN)/raw_file_io_raw.beam: ../include/file.hrl file_int.hrl

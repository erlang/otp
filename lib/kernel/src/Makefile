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
	erl_ddll \
	erl_distribution \
	erl_epmd \
	erl_reply \
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
	net \
	net_adm \
	net_kernel \
	os \
	pg2 \
	ram_file \
	rpc \
	seq_trace \
	standard_error \
	user \
	user_drv \
	user_sup \
	wrap_log_reader

HRL_FILES= ../include/file.hrl ../include/inet.hrl ../include/inet_sctp.hrl \
	../include/dist.hrl ../include/dist_util.hrl \
	../include/net_address.hrl 

INTERNAL_HRL_FILES= application_master.hrl disk_log.hrl \
        inet_dns.hrl inet_res.hrl \
        inet_boot.hrl inet_config.hrl inet_int.hrl \
	inet_dns_record_adts.hrl

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
$(EBIN)/application_controller.beam: application_master.hrl
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
$(EBIN)/file.beam: ../include/file.hrl
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
$(EBIN)/net_kernel.beam: ../include/net_address.hrl
$(EBIN)/os.beam: ../include/file.hrl
$(EBIN)/ram_file.beam: ../include/file.hrl
$(EBIN)/wrap_log_reader.beam: disk_log.hrl ../include/file.hrl

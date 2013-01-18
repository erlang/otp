#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2012. All Rights Reserved.
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

#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(SSH_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/ssh-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

# Behaviour (api) modules are first so they are compiled when
# the compiler reaches a callback module using them.
BEHAVIOUR_MODULES= \
	ssh_sftpd_file_api \
        ssh_channel \
	ssh_subsystem \
	ssh_client_key_api \
	ssh_server_key_api

MODULES= \
	ssh \
	ssh_sup \
	sshc_sup \
	sshd_sup \
	ssh_connection_sup \
	ssh_connection \
	ssh_connection_handler \
	ssh_connection_manager \
	ssh_shell \
	ssh_system_sup \
	ssh_subsystem_sup \
	ssh_channel_sup \
	ssh_acceptor_sup \
	ssh_acceptor \
	ssh_app \
	ssh_auth\
	ssh_bits \
	ssh_cli \
	ssh_file \
	ssh_io \
	ssh_math \
	ssh_no_io \
	ssh_sftp \
	ssh_sftpd \
	ssh_sftpd_file\
	ssh_transport \
	ssh_userreg \
	ssh_xfer

PUBLIC_HRL_FILES= ssh.hrl ssh_userauth.hrl ssh_xfer.hrl

ERL_FILES= \
	$(MODULES:%=%.erl) \
	$(BEHAVIOUR_MODULES:%=%.erl)


TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

BEHAVIOUR_TARGET_FILES= $(BEHAVIOUR_MODULES:%=$(EBIN)/%.$(EMULATOR))

APP_FILE= ssh.app
APPUP_FILE= ssh.appup

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

INTERNAL_HRL_FILES = ssh_auth.hrl ssh_connect.hrl ssh_transport.hrl

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
EXTRA_ERLC_FLAGS = +warn_unused_vars
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/kernel/src \
	-pz $(EBIN) \
	-pz $(ERL_TOP)/lib/public_key/ebin \
	$(EXTRA_ERLC_FLAGS)


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TARGET_FILES): $(BEHAVIOUR_TARGET_FILES)

debug opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

clean:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)
	rm -f errs core *~

$(APP_TARGET):	$(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET):	$(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@


docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(BEHAVIOUR_TARGET_FILES) $(TARGET_FILES) $(APP_TARGET) \
	$(APPUP_TARGET) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(PUBLIC_HRL_FILES) "$(RELSYSDIR)/include"

release_docs_spec:


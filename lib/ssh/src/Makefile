#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2018. All Rights Reserved.
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
	ssh_client_key_api \
	ssh_daemon_channel \
	ssh_server_channel \
	ssh_server_key_api \
	ssh_sftpd_file_api \
        ssh_channel        \
        ssh_client_channel

MODULES= \
	ssh \
	ssh_acceptor \
	ssh_acceptor_sup \
	ssh_app \
	ssh_auth\
	ssh_bits \
	ssh_cli \
	ssh_connection \
	ssh_connection_handler \
	ssh_connection_sup \
	ssh_dbg \
	ssh_file \
	ssh_info \
	ssh_io \
	ssh_message \
	ssh_no_io \
	ssh_options \
	ssh_server_channel_sup \
	ssh_sftp \
	ssh_sftpd \
	ssh_sftpd_file\
	ssh_shell \
	ssh_subsystem_sup \
	ssh_sup \
	ssh_system_sup \
	ssh_transport \
	ssh_xfer \
	sshc_sup \
	sshd_sup

HRL_FILES =

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

INTERNAL_HRL_FILES = ssh_auth.hrl ssh_connect.hrl ssh_transport.hrl ssh.hrl ssh_xfer.hrl

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
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) $(BEHAVIOUR_TARGET_FILES)
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


release_docs_spec:


deps:
	erlc -M $(ERL_FILES) \
	| sed 's@$(ERL_TOP)/lib@../..@g' \
	| sed 's/\.$(EMULATOR)/\.$$\(EMULATOR\)/' \
	| sed 's@^ssh_@$$(EBIN)/ssh_@'

ssh.$(EMULATOR): ssh.erl ssh.hrl ssh_connect.hrl \
  ../../public_key/include/public_key.hrl \
  ../../public_key/include/OTP-PUB-KEY.hrl \
  ../../public_key/include/PKCS-FRAME.hrl \
  ../../kernel/include/file.hrl
$(EBIN)/ssh_sup.$(EMULATOR): ssh_sup.erl
sshc_sup.$(EMULATOR): sshc_sup.erl
sshd_sup.$(EMULATOR): sshd_sup.erl ssh.hrl
$(EBIN)/ssh_connection_sup.$(EMULATOR): ssh_connection_sup.erl
$(EBIN)/ssh_connection.$(EMULATOR): ssh_connection.erl ssh.hrl ssh_connect.hrl \
  ssh_transport.hrl
$(EBIN)/ssh_connection_handler.$(EMULATOR): ssh_connection_handler.erl ssh.hrl \
  ssh_transport.hrl ssh_auth.hrl ssh_connect.hrl
$(EBIN)/ssh_shell.$(EMULATOR): ssh_shell.erl ssh_connect.hrl
$(EBIN)/ssh_system_sup.$(EMULATOR): ssh_system_sup.erl ssh.hrl
$(EBIN)/ssh_subsystem_sup.$(EMULATOR): ssh_subsystem_sup.erl
$(EBIN)/ssh_server_channel_sup.$(EMULATOR): ssh_server_channel_sup.erl
$(EBIN)/ssh_acceptor_sup.$(EMULATOR): ssh_acceptor_sup.erl ssh.hrl
$(EBIN)/ssh_acceptor.$(EMULATOR): ssh_acceptor.erl ssh.hrl
$(EBIN)/ssh_app.$(EMULATOR): ssh_app.erl
$(EBIN)/ssh_auth.$(EMULATOR): ssh_auth.erl \
  ../../public_key/include/public_key.hrl \
  ../../public_key/include/OTP-PUB-KEY.hrl \
  ../../public_key/include/PKCS-FRAME.hrl \
  ssh.hrl ssh_auth.hrl ssh_transport.hrl
$(EBIN)/ssh_bits.$(EMULATOR): ssh_bits.erl ssh.hrl
$(EBIN)/ssh_cli.$(EMULATOR): ssh_cli.erl ssh.hrl ssh_connect.hrl
$(EBIN)/ssh_file.$(EMULATOR): ssh_file.erl \
  ../../public_key/include/public_key.hrl \
  ../../public_key/include/OTP-PUB-KEY.hrl \
  ../../public_key/include/PKCS-FRAME.hrl \
  ../../kernel/include/file.hrl ssh.hrl
$(EBIN)/ssh_io.$(EMULATOR): ssh_io.erl ssh.hrl
$(EBIN)/ssh_info.$(EMULATOR): ssh_info.erl
$(EBIN)/ssh_message.$(EMULATOR): ssh_message.erl \
  ../../public_key/include/public_key.hrl \
  ../../public_key/include/OTP-PUB-KEY.hrl \
  ../../public_key/include/PKCS-FRAME.hrl \
  ssh.hrl ssh_connect.hrl ssh_auth.hrl ssh_transport.hrl
$(EBIN)/ssh_no_io.$(EMULATOR): ssh_no_io.erl ssh_transport.hrl
$(EBIN)/ssh_sftp.$(EMULATOR): ssh_sftp.erl \
  ../../kernel/include/file.hrl ssh.hrl \
  ssh_xfer.hrl
$(EBIN)/ssh_sftpd.$(EMULATOR): ssh_sftpd.erl \
  ../../kernel/include/file.hrl ssh.hrl \
  ssh_xfer.hrl
$(EBIN)/ssh_sftpd_file.$(EMULATOR): ssh_sftpd_file.erl
$(EBIN)/ssh_transport.$(EMULATOR): ssh_transport.erl \
  ../../public_key/include/public_key.hrl \
  ../../public_key/include/OTP-PUB-KEY.hrl \
  ../../public_key/include/PKCS-FRAME.hrl \
  ../../kernel/include/inet.hrl \
  ssh_transport.hrl ssh.hrl
$(EBIN)/ssh_xfer.$(EMULATOR): ssh_xfer.erl ssh.hrl ssh_xfer.hrl
$(EBIN)/ssh_sftpd_file_api.$(EMULATOR): ssh_sftpd_file_api.erl
$(EBIN)/ssh_client_channel.$(EMULATOR): ssh_client_channel.erl ssh_connect.hrl
$(EBIN)/ssh_channel.$(EMULATOR): ssh_channel.erl ssh_connect.hrl
$(EBIN)/ssh_daemon_channel.$(EMULATOR): ssh_daemon_channel.erl
$(EBIN)/ssh_server_channel.$(EMULATOR): ssh_server_channel.erl
$(EBIN)/ssh_client_key_api.$(EMULATOR): ssh_client_key_api.erl \
  ../../public_key/include/public_key.hrl \
  ../../public_key/include/OTP-PUB-KEY.hrl \
  ../../public_key/include/PKCS-FRAME.hrl \
  ssh.hrl
$(EBIN)/ssh_server_key_api.$(EMULATOR): ssh_server_key_api.erl \
  ../../public_key/include/public_key.hrl \
  ../../public_key/include/OTP-PUB-KEY.hrl \
  ../../public_key/include/PKCS-FRAME.hrl \
  ssh.hrl


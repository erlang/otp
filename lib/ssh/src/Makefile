#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2024. All Rights Reserved.
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
# The $(BEHAVIOUR_MODULES_1) has a behaviour used in one or more
# of the $(BEHAVIOUR_MODULES_2)

BEHAVIOUR_MODULES_1= \
	ssh_dbg

BEHAVIOUR_MODULES_2= \
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
	ssh_agent \
	ssh_app \
	ssh_auth\
	ssh_bits \
	ssh_channel_sup \
	ssh_cli \
	ssh_connection \
	ssh_connection_handler \
	ssh_file \
	ssh_fsm_kexinit \
	ssh_fsm_userauth_client \
	ssh_fsm_userauth_server \
	ssh_info \
	ssh_io \
	ssh_lib \
	ssh_message \
	ssh_no_io \
	ssh_options \
	ssh_sftp \
	ssh_sftpd \
	ssh_sftpd_file\
	ssh_shell \
	ssh_subsystem_sup \
	ssh_system_sup \
	ssh_tcpip_forward_srv \
	ssh_tcpip_forward_client \
	ssh_tcpip_forward_acceptor_sup \
	ssh_tcpip_forward_acceptor \
	ssh_transport \
	ssh_xfer

HRL_FILES =

ERL_FILES= \
	$(MODULES:%=%.erl) \
	$(BEHAVIOUR_MODULES_1:%=%.erl) \
	$(BEHAVIOUR_MODULES_2:%=%.erl)


TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

BEHAVIOUR_TARGET_FILES_1= $(BEHAVIOUR_MODULES_1:%=$(EBIN)/%.$(EMULATOR))
BEHAVIOUR_TARGET_FILES_2= $(BEHAVIOUR_MODULES_2:%=$(EBIN)/%.$(EMULATOR))
BEHAVIOUR_TARGET_FILES= $(BEHAVIOUR_TARGET_FILES_1) $(BEHAVIOUR_TARGET_FILES_2)

APP_FILE= ssh.app
APPUP_FILE= ssh.appup

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

INTERNAL_HRL_FILES = \
	ssh.hrl \
	ssh_agent.hrl \
	ssh_auth.hrl \
	ssh_connect.hrl \
	ssh_fsm.hrl \
	ssh_transport.hrl \
	ssh_xfer.hrl

DEPDIR=$(ERL_TOP)/lib/ssh/src/deps
DEP_FILE=$(DEPDIR)/ssh.d
$(shell mkdir -p $(dir $(DEP_FILE)) >/dev/null)

ifeq ($(TARGET), win32)
  # Native path without C: ignore driveletter case
  ERL_TOP_NATIVE = $(shell w32_path.sh -m $(ERL_TOP) | sed "s@[a-zA-Z]:@:@")
else
  ERL_TOP_NATIVE = $(ERL_TOP)
endif

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
EXTRA_ERLC_FLAGS = +warn_unused_vars +nowarn_deprecated_callback
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/kernel/src \
	-pz $(EBIN) \
	-pz $(ERL_TOP)/lib/public_key/ebin \
	$(EXTRA_ERLC_FLAGS)


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TARGET_FILES): $(BEHAVIOUR_TARGET_FILES_2)
$(BEHAVIOUR_TARGET_FILES_2): $(BEHAVIOUR_TARGET_FILES_1)

$(DEP_FILE): $(ERL_FILES)
	@echo SED $(TARGET) $(ERL_TOP_NATIVE)
	$(gen_verbose)erlc -M $(ERL_FILES) \
	| perl -pe "s@ [a-zA-Z]?$(ERL_TOP_NATIVE)/(?:bootstrap/)?lib/([^/]+)@ ../../\1@g" 2> /dev/null \
	| sed "s/\.$(EMULATOR)/\.$$\(EMULATOR\)/" \
	| sed 's@^ssh_@$$(EBIN)/ssh_@' \
	| sed 's@^sshc_@$$(EBIN)/sshc_@' \
	| sed 's@^sshd_@$$(EBIN)/sshd_@' \
	> $(DEP_FILE)

$(TYPES): $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) $(DEP_FILE)

clean:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) $(BEHAVIOUR_TARGET_FILES)
	rm -f errs core *~
	rm -rf $(DEPDIR)

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

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------
-include $(DEP_FILE)

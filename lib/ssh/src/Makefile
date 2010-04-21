#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2010. All Rights Reserved.
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

MODULES= \
	ssh \
	ssh_sup \
	sshc_sup \
	sshd_sup \
        ssh_channel \
	ssh_connection \
	ssh_connection_handler \
	ssh_connection_manager \
	ssh_connection_controler \
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
	ssh_dsa \
	ssh_file \
	ssh_io \
	ssh_math \
	ssh_no_io \
	ssh_rsa \
	ssh_sftp \
	ssh_sftpd \
	ssh_sftpd_file\
	ssh_sftpd_file_api \
	ssh_transport \
	ssh_userreg \
	ssh_xfer

PUBLIC_HRL_FILES= ssh.hrl ssh_userauth.hrl ssh_xfer.hrl

ERL_FILES= $(MODULES:%=%.erl) $(ASN_ERLS)

ALL_MODULES= $(MODULES) $(ASN_MODULES)

TARGET_FILES= $(ALL_MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= ssh.app
APPUP_FILE= ssh.appup

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

ASN_MODULES = PKCS-1 DSS
ASN_ASNS = $(ASN_MODULES:%=%.asn1)
ASN_ERLS = $(ASN_MODULES:%=%.erl)
ASN_HRLS = $(ASN_MODULES:%=%.hrl)
ASN_DBS = $(ASN_MODULES:%=%.asn1db)
ASN_TABLES = $(ASN_MODULES:%=%.table)

ASN_FLAGS = -bber_bin +der +compact_bit_string +optimize +noobj +inline

INTERNAL_HRL_FILES = $(ASN_HRLS) ssh_auth.hrl ssh_connect.hrl ssh_transport.hrl

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -pa$(EBIN) 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

debug: ERLC_FLAGS += -Ddebug

clean:
	rm -f $(TARGET_FILES)
	rm -f errs core *~
	rm -f $(ASN_ERLS) $(ASN_HRLS) $(ASN_DBS)

$(TARGET_FILES): ssh.hrl

# $(EBIN)/ssh_sftpd_file.$(EMULATOR): ERLC_FLAGS += -pa$(EBIN)
# $(EBIN)/ssh_sftpd_file.$(EMULATOR): $(EBIN)/ssh_sftpd_file_api.$(EMULATOR)

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

%.hrl: %.asn1
	erlc $(ASN_FLAGS) $<

DSS.hrl DSS.erl: DSS.asn1
PKCS-1.hrl PKCS-1.erl: PKCS-1.asn1

$(EBIN)/ssh_file.$(EMULATOR): $(ASN_HRLS)

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) $(ERL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(PUBLIC_HRL_FILES) $(RELSYSDIR)/include

release_docs_spec:


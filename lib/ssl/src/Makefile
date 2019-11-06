#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
VSN=$(SSL_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/ssl-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

BEHAVIOUR_MODULES= \
	ssl_crl_cache_api \
	ssl_session_cache_api


MODULES= \
	dtls_connection \
	dtls_connection_sup \
	dtls_handshake \
	dtls_listener_sup \
	dtls_packet_demux \
	dtls_record \
	dtls_sup \
	dtls_socket \
	dtls_v1 \
	inet_tls_dist \
	inet6_tls_dist \
	ssl \
	ssl_admin_sup \
	ssl_alert \
	ssl_app \
	ssl_certificate \
	ssl_cipher \
	ssl_cipher_format \
	ssl_config \
	ssl_connection \
	ssl_connection_sup \
	ssl_crl \
	ssl_crl_cache \
	ssl_crl_hash_dir \
	ssl_dh_groups \
	ssl_dist_admin_sup \
	ssl_dist_connection_sup \
	ssl_dist_sup \
	ssl_handshake \
	ssl_listen_tracker_sup \
	ssl_logger \
	ssl_manager \
	ssl_pem_cache \
	ssl_pkix_db \
	ssl_record \
	ssl_session \
	ssl_session_cache \
	ssl_srp_primes \
	ssl_sup \
	ssl_v3 \
	tls_bloom_filter \
	tls_connection \
	tls_connection_sup \
	tls_connection_1_3 \
	tls_handshake \
	tls_handshake_1_3 \
	tls_record \
	tls_record_1_3 \
	tls_client_ticket_store \
	tls_sender \
	tls_server_session_ticket\
	tls_server_session_ticket_sup\
	tls_server_sup\
	tls_socket \
	tls_sup \
	tls_v1


INTERNAL_HRL_FILES = \
	dtls_connection.hrl \
	dtls_handshake.hrl \
	dtls_record.hrl \
	ssl_alert.hrl \
	ssl_api.hrl \
	ssl_cipher.hrl \
	ssl_connection.hrl \
	ssl_handshake.hrl \
	ssl_internal.hrl \
	ssl_record.hrl \
	ssl_srp.hrl \
	tls_connection.hrl \
	tls_handshake.hrl \
	tls_handshake_1_3.hrl \
	tls_record.hrl \
	tls_record_1_3.hrl


ERL_FILES= \
	$(MODULES:%=%.erl) \
	$(BEHAVIOUR_MODULES:%=%.erl)


TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

BEHAVIOUR_TARGET_FILES= $(BEHAVIOUR_MODULES:%=$(EBIN)/%.$(EMULATOR))

APP_FILE= ssl.app
APPUP_FILE= ssl.appup

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

DEPDIR=$(ERL_TOP)/lib/ssl/src/deps
DEP_FILE=$(DEPDIR)/ssl.d
$(shell mkdir -p $(dir $(DEP_FILE)) >/dev/null)

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

$(DEP_FILE): $(ERL_FILES)
	$(gen_verbose)erlc -M $(ERL_FILES) \
	| sed "s@$(ERL_TOP)@../../..@g" \
	| sed "s/\.$(EMULATOR)/\.$$\(EMULATOR\)/" \
	| sed 's@^dtls_@$$(EBIN)/dtls_@' \
	| sed 's@^inet_@$$(EBIN)/inet_@' \
	| sed 's@^ssl_@$$(EBIN)/ssl_@' \
	| sed 's@^tls_@$$(EBIN)/tls_@' \
	> $(DEP_FILE)

debug opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) $(DEP_FILE)

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
	$(INSTALL_DATA) $(ERL_FILES) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(BEHAVIOUR_TARGET_FILES) $(TARGET_FILES) $(APP_TARGET) \
	$(APPUP_TARGET) "$(RELSYSDIR)/ebin"

release_docs_spec:

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------
-include $(DEP_FILE)

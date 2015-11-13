#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1999-2015. All Rights Reserved.
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
        ssl_session_cache_api \
	ssl_crl_cache_api

MODULES= \
	ssl \
	tls \
	dtls \
	ssl_alert \
	ssl_app \
	ssl_dist_sup\
	ssl_sup \
	inet_tls_dist \
	inet6_tls_dist \
	ssl_certificate\
	ssl_pkix_db\
	ssl_cipher \
	ssl_srp_primes \
	tls_connection \
	dtls_connection \
	ssl_config \
	ssl_connection \
	tls_connection_sup \
	dtls_connection_sup \
	tls_handshake \
	dtls_handshake\
	ssl_handshake\
	ssl_manager \
	ssl_session \
	ssl_session_cache \
	ssl_crl\
	ssl_crl_cache \
	ssl_socket \
	ssl_listen_tracker_sup \
	tls_record \
	dtls_record \
	ssl_record \
	ssl_v2 \
	ssl_v3 \
	tls_v1 \
	dtls_v1 \
	ssl_tls_dist_proxy

INTERNAL_HRL_FILES = \
	ssl_alert.hrl ssl_cipher.hrl \
	tls_connection.hrl dtls_connection.hrl ssl_connection.hrl \
        ssl_handshake.hrl tls_handshake.hrl dtls_handshake.hrl ssl_api.hrl ssl_internal.hrl \
        ssl_record.hrl tls_record.hrl dtls_record.hrl ssl_srp.hrl

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

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
EXTRA_ERLC_FLAGS = +warn_unused_vars
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/kernel/src \
	-pz $(EBIN) \
	-pz $(ERL_TOP)/lib/public_key/ebin \
	$(EXTRA_ERLC_FLAGS) -DVSN=\"$(VSN)\"


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
	$(INSTALL_DATA) $(ERL_FILES) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(BEHAVIOUR_TARGET_FILES) $(TARGET_FILES) $(APP_TARGET) \
	$(APPUP_TARGET) "$(RELSYSDIR)/ebin"

release_docs_spec:

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------
$(EBIN)/inet_tls_dist.$(EMULATOR): ../../kernel/include/net_address.hrl  ../../kernel/include/dist.hrl  ../../kernel/include/dist_util.hrl
$(EBIN)/tls.$(EMULATOR): ssl_internal.hrl ssl_record.hrl ssl_cipher.hrl ssl_handshake.hrl ../../public_key/include/public_key.hrl
$(EBIN)/ssl_alert.$(EMULATOR): ssl_alert.hrl ssl_record.hrl  
$(EBIN)/ssl_certificate.$(EMULATOR): ssl_internal.hrl ssl_alert.hrl ssl_handshake.hrl ../../public_key/include/public_key.hrl
$(EBIN)/ssl_certificate_db.$(EMULATOR): ssl_internal.hrl ../../public_key/include/public_key.hrl ../../kernel/include/file.hrl
$(EBIN)/ssl_cipher.$(EMULATOR): ssl_internal.hrl ssl_record.hrl ssl_cipher.hrl ssl_handshake.hrl ssl_alert.hrl ../../public_key/include/public_key.hrl
$(EBIN)/tls_connection.$(EMULATOR): ssl_internal.hrl tls_connection.hrl tls_record.hrl ssl_cipher.hrl tls_handshake.hrl ssl_alert.hrl ../../public_key/include/public_key.hrl
$(EBIN)/dtls_connection.$(EMULATOR): ssl_internal.hrl dtls_connection.hrl dtls_record.hrl ssl_cipher.hrl dtls_handshake.hrl ssl_alert.hrl ../../public_key/include/public_key.hrl
$(EBIN)/tls_handshake.$(EMULATOR): ssl_internal.hrl tls_record.hrl ssl_cipher.hrl tls_handshake.hrl ssl_alert.hrl ../../public_key/include/public_key.hrl
$(EBIN)/tls_handshake.$(EMULATOR): ssl_internal.hrl ssl_connection.hrl ssl_record.hrl ssl_cipher.hrl ssl_handshake.hrl ssl_alert.hrl ../../public_key/include/public_key.hrl
$(EBIN)/ssl_manager.$(EMULATOR): ssl_internal.hrl ssl_handshake.hrl ../../kernel/include/file.hrl
$(EBIN)/ssl_record.$(EMULATOR):  ssl_internal.hrl ssl_record.hrl ssl_cipher.hrl ssl_handshake.hrl ssl_alert.hrl
$(EBIN)/ssl_session.$(EMULATOR):  ssl_internal.hrl ssl_handshake.hrl
$(EBIN)/ssl_session_cache.$(EMULATOR):  ssl_internal.hrl ssl_handshake.hrl
$(EBIN)/ssl_session_cache_api.$(EMULATOR):  ssl_internal.hrl ssl_handshake.hrl
$(EBIN)/ssl_ssl3.$(EMULATOR): ssl_internal.hrl ssl_record.hrl ssl_cipher.hrl
$(EBIN)/ssl_tls1.$(EMULATOR): ssl_internal.hrl ssl_record.hrl ssl_cipher.hrl
$(EBIN)/ssl_cache.$(EMULATOR): ssl_cache.erl ssl_internal.hrl ../../public_key/include/public_key.hrl


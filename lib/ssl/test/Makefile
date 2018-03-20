#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1999-2017. All Rights Reserved.
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

# SSL test suite Makefile
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(GS_VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	ssl_test_lib \
	ssl_bench_test_lib \
	ssl_dist_test_lib \
	ssl_alpn_handshake_SUITE \
	ssl_basic_SUITE \
	ssl_bench_SUITE \
	ssl_cipher_SUITE \
	ssl_certificate_verify_SUITE\
	ssl_crl_SUITE\
	ssl_dist_SUITE \
	ssl_dist_bench_SUITE \
	ssl_engine_SUITE\
	ssl_handshake_SUITE \
	ssl_npn_hello_SUITE \
	ssl_npn_handshake_SUITE \
	ssl_packet_SUITE \
	ssl_payload_SUITE \
	ssl_pem_cache_SUITE \
	ssl_session_cache_SUITE	\
	ssl_to_openssl_SUITE \
	ssl_ECC_SUITE \
	ssl_ECC_openssl_SUITE \
	ssl_ECC\
	ssl_upgrade_SUITE\
	ssl_sni_SUITE \
	make_certs\
        x509_test


ERL_FILES = $(MODULES:%=%.erl)

HRL_FILES = \
	ssl_dist_test_lib.hrl

HRL_FILES_SRC = \
	ssl_api.hrl\
	ssl_internal.hrl\
	ssl_alert.hrl \
	tls_handshake.hrl \
	ssl_handshake.hrl \
	ssl_cipher.hrl \
	tls_record.hrl \
	ssl_record.hrl

HRL_FILES_INC = 

HRL_FILES_NEEDED_IN_TEST = \
	$(HRL_FILES_SRC:%=../src/%) \
	$(HRL_FILES_INC:%=../include/%)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

INCLUDES = -I.

DATADIRS = ssl_basic_SUITE_data

EMAKEFILE=Emakefile
MAKE_EMAKE = $(wildcard $(ERL_TOP)/make/make_emakefile)

COVER_FILE = ssl.cover

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/ssl_test

# ----------------------------------------------------
# FLAGS
# The path to the test_server ebin dir is needed when 
# running the target "targets".
# ----------------------------------------------------
ERL_COMPILE_FLAGS += $(INCLUDES)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(BUILDTARGET)

targets: $(TARGET_FILES)

.PHONY: emakebuild

emakebuild: $(EMAKEFILE)

$(EMAKEFILE): 
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make' | grep -v Warning > $(EMAKEFILE)
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) | grep -v Warning >> $(EMAKEFILE)

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES)
	rm -f core *~

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA)  $(ERL_FILES) $(HRL_FILES) $(HRL_FILES_NEEDED_IN_TEST) $(COVER_FILE) "$(RELSYSDIR)"
	$(INSTALL_DATA) ssl.spec ssl_bench.spec ssl.cover "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:

# Dependencies

$(TARGET_FILES):	$(HRL_FILES)

#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2017. All Rights Reserved.
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
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

include ../vsn.mk
VSN = $(XMERL_VSN)

RM = rm -f

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

HRL_DIR= $(ERL_TOP)/lib/xmerl/include/
HRL_MODULES= $(HRL_DIR)xmerl $(HRL_DIR)xmerl_xsd
HRL_FILES= $(HRL_MODULES:%=%.hrl)


MODULES= \
	xmerl_test \
	xmerl_app_test \
	xmerl_appup_test \
	xmerl_test_lib \
	xmerl_xsd_lib

ERL_FILES= $(MODULES:%=%.erl)

SUITE_FILES= \
	xmerl_SUITE.erl \
	xmerl_std_SUITE.erl \
	xmerl_sax_SUITE.erl \
	xmerl_sax_std_SUITE.erl \
	xmerl_xsd_SUITE.erl \
	xmerl_xsd_MS2002-01-16_SUITE.erl \
	xmerl_xsd_NIST2002-01-16_SUITE.erl \
	xmerl_xsd_Sun2002-01-16_SUITE.erl \
        xmerl_sax_stream_SUITE.erl

XML_FILES= \
	testcases.dtd \
	xmlconformance.msxsl \
	xmlconformance.xsl


TARGET_FILES= $(MODULES:%=%.$(EMULATOR))


INSTALL_PROGS= $(TARGET_FILES)

ifndef XMERL_PRESERV
XMERL_PRESERV_TEST_DIRS=
else
XMERL_PRESERV_TEST_DIRS= -Ddont_rm_test_dirs
endif

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------

RELSYSDIR = $(RELEASE_PATH)/xmerl_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_COMPILE_FLAGS +=


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt:  xmerl_test.erl

info:
	@echo "HRL_FILES: $(HRL_FILES)"

clean:
	$(RM) *.beam
	$(RM) xmerl_test.erl
	$(RM) core *~

xmerl_test.erl: ../doc/examples/xmerl_test.erl
	cp $< $@
	chmod u+w $@


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA)  $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA)  $(SUITE_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) xmerl.spec xmerl.cover "$(RELSYSDIR)"
	cp $(XML_FILES) "$(RELSYSDIR)"
	@tar cfh - xmerl_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	@tar cfh - xmerl_std_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	@tar cfh - xmerl_sax_std_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	@tar cfh - xmerl_xsd_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	@tar cfh - xmerl_xsd_MS2002-01-16_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	@tar cfh - xmerl_xsd_NIST2002-01-16_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	@tar cfh - xmerl_xsd_Sun2002-01-16_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	@tar cfh - xmerl_sax_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	@tar cfh - xmerl_sax_stream_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	chmod -R u+w "$(RELSYSDIR)"

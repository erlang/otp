#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
VSN=$(MNESIA_VSN)

# ----------------------------------------------------
# Release Macros
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/mnesia-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------


MODULES = \
	company \
	company_o \
	bup \
	mnesia_meter \
	mnesia_tpcb

ERL_FILES= $(MODULES:=.erl)

HRL_FILES = \
	company.hrl \
	company_o.hrl

DATA_FILES = \
	DATA

# TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
TARGET_FILES = 

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -pa ../ebin
EBIN = .

# ----------------------------------------------------
# Make Rules
# ----------------------------------------------------
debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES) *~

docs:

# ----------------------------------------------------
# Release Targets
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/examples"
	$(INSTALL_DATA) $(ERL_FILES) $(DATA_FILES) $(HRL_FILES) "$(RELSYSDIR)/examples"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples/bench"
	(cd bench; $(INSTALL_DATA) \
          Makefile \
          README \
          bench.erl \
          bench.hrl \
          bench_generate.erl \
          bench_populate.erl \
          bench_trans.erl \
          bench.config1 \
          bench.config2 \
          bench.config3 \
          bench.config4 \
          bench.config5 \
          bench.config6 \
          bench.config7 \
          "$(RELSYSDIR)/examples/bench")
	(cd bench; $(INSTALL_SCRIPT) bench.sh "$(RELSYSDIR)/examples/bench")

release_docs_spec:


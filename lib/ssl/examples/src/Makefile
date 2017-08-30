#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
include ../../vsn.mk
VSN=$(SSL_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/ssl-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
EXTRA_ERLC_FLAGS = +warn_unused_vars
ERL_COMPILE_FLAGS += $(EXTRA_ERLC_FLAGS)


MODULES = client_server

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

clean:
	rm -fr $(TARGET_FILES) *~ *.beam 

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/examples/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples/ebin"
	(cd ..; tar cf - src ebin  | (cd "$(RELSYSDIR)/examples"; tar xf -))
	chmod -R ug+w "$(RELSYSDIR)/examples"

release_docs_spec:








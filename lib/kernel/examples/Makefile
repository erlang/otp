# ``Licensed under the Apache License, Version 2.0 (the "License");
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
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#
include $(ERL_TOP)/make/target.mk

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include ../vsn.mk

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt:

clean:

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

RELSYSDIR = $(RELEASE_PATH)/lib/kernel-$(KERNEL_VSN)/examples

# Pack and install the complete directory structure from 
# here (CWD) and down, for all examples.

EXAMPLES  = uds_dist gen_tcp_dist

release_spec:
	$(INSTALL_DIR) "$(RELSYSDIR)"
	tar cf - $(EXAMPLES) | \
	(cd "$(RELSYSDIR)"; tar xf - ; chmod -R ug+w $(EXAMPLES) )

release_docs_spec:

# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
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

EXAMPLES  = uds_dist

release_spec:
	$(INSTALL_DIR) "$(RELSYSDIR)"
	tar cf - $(EXAMPLES) | \
	(cd "$(RELSYSDIR)"; tar xf - ; chmod -R ug+w $(EXAMPLES) )

release_docs_spec:

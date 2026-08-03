# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
# Copyright Ericsson AB 2004-2025. All Rights Reserved.
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
# Alternatively, you may use this file under the terms of the GNU Lesser
# General Public License (the "LGPL") as published by the Free Software
# Foundation; either version 2.1, or (at your option) any later version.
# If you wish to allow use of your version of this file only under the
# terms of the LGPL, you should delete the provisions above and replace
# them with the notice and other provisions required by the LGPL; see
# <http://www.gnu.org/licenses/>. If you do not delete the provisions
# above, a recipient may use your version of this file under the terms of
# either the Apache License or the LGPL.
#
# %CopyrightEnd%
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/edoc-$(EDOC_VSN)


#
# Common Macros
#

GEN_SCRIPT = edoc_generate
PRIV_FILES = stylesheet.css erlang.png edoc.dtd

$(TYPES):

clean:
	rm -f core *~

docs:

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/priv"
	$(INSTALL_DATA) $(PRIV_FILES) "$(RELSYSDIR)/priv"
	$(INSTALL_SCRIPT) $(GEN_SCRIPT) "$(RELSYSDIR)/priv"


release_docs_spec:

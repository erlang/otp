#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2024. All Rights Reserved.
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
# Application version
# ----------------------------------------------------
include ../vsn.mk
APPLICATION = erts
INDEX_DIR = ../../../doc
RELSYSDIR = $(RELEASE_PATH)/$(APPLICATION)-$(VSN)

assets:
	$(V_at)mkdir assets
assets/%: ../emulator/internal_doc/assets/% | assets
	$(V_at)cp $< $@
assets/gen_tcp_dist.erl: ../../lib/kernel/examples/gen_tcp_dist/src/gen_tcp_dist.erl | assets
	$(V_at)cp $< $@
assets/erl_uds_dist.erl: ../../lib/kernel/examples/erl_uds_dist/src/erl_uds_dist.erl | assets
	$(V_at)cp $< $@
assets/time_compat.erl: ../example/time_compat.erl
	$(V_at)cp $< $@

$(HTMLDIR)/index.html: $(patsubst ../emulator/internal_doc/assets/%,assets/%,$(wildcard ../emulator/internal_doc/assets/*.png) $(wildcard ../emulator/internal_doc/assets/*.svg)) assets/gen_tcp_dist.erl assets/erl_uds_dist.erl assets/time_compat.erl

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/doc.mk

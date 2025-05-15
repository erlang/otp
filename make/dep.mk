#
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2025. All Rights Reserved.
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

ifeq ($(TARGET), win32)
  # Native path without C: ignore driveletter case
  ERL_TOP_NATIVE = $(shell w32_path.sh -m $(ERL_TOP) | sed "s@[a-zA-Z]:@:@")
else
  ERL_TOP_NATIVE = $(ERL_TOP)
endif

DEPDIR=deps
DEP_FILE=$(DEPDIR)/deps.mk
$(shell mkdir -p $(dir $(DEP_FILE)) >/dev/null)

DEP_REL_TOP?=../../

deps: $(DEP_FILE)

$(DEP_FILE): $(ERL_FILES) $(HRL_FILES) $(INTERNAL_HRL_FILES) $(EXTRA_DEP_DEPENDENCIES) $(ERL_TOP)/make/dep.mk Makefile
	$(gen_verbose)erlc -M $(ERL_COMPILE_FLAGS) -o $(EBIN) $(filter-out $(DEP_SKIP_FILES), $(ERL_FILES)) \
	| perl -pe "s@ [a-zA-Z]?$(ERL_TOP_NATIVE)/(?:bootstrap/)?lib/([^/]+)@ $(DEP_REL_TOP)\1@g" 2> /dev/null \
	> $(DEP_FILE)

-include $(DEP_FILE)
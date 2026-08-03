#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 1996-2026. All Rights Reserved.
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

.SUFFIXES: .hrl .erl .beam 


# ----------------------------------------------------
#	Common macros
# ----------------------------------------------------
DEFAULT_TARGETS =  opt debug instr release release_docs clean docs

# ----------------------------------------------------
#	Erlang language section
# ----------------------------------------------------
EMULATOR = beam

ERL_COMPILE_FLAGS += +debug_info
ERLC_WFLAGS = -W
ERLC = erlc $(ERLC_WFLAGS) $(ERLC_FLAGS)
ERL.beam =  erl.beam -boot start_clean
ERL = $(ERL.$(EMULATOR))

ifeq ($(EBIN),)
EBIN = .
endif

ESRC = .


$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) -bbeam $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

.erl.beam:
	$(ERLC) -bbeam $(ERL_COMPILE_FLAGS) -o$(dir $@) $<

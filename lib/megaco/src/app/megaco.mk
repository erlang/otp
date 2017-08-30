#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

ifeq ($(MEGACO_TRACE), io)
ERL_COMPILE_FLAGS += -Dmegaco_trace_io
endif

ifeq ($(MEGACO_EXTENDED_TRACE), true)
ERL_COMPILE_FLAGS += -Dmegaco_extended_trace
endif

ifeq ($(USE_MEGACO_TEST_CODE), true)
ERL_COMPILE_FLAGS += -DMEGACO_TEST_CODE=mona_lisa_spelar_doom
endif

ifeq ($(MEGACO_DEBUG), true)
ERL_COMPILE_FLAGS += -Dmegaco_debug
endif

ifneq ($(MEGACO_PARSER_INLINE), false)
ERL_COMPILE_FLAGS += -Dmegaco_parser_inline
endif

ifeq ($(USE_MEGACO_HIPE), true)
ERL_COMPILE_FLAGS += +native
endif

ifeq ($(WARN_UNUSED_WARS), true)
ERL_COMPILE_FLAGS += +warn_unused_vars
endif

MEGACO_APP_VSN_COMPILE_FLAGS = \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,app_vsn,$(APP_VSN)}'

MEGACO_ERL_COMPILE_FLAGS +=             \
	-pa $(ERL_TOP)/lib/et/ebin      \
	-pa $(ERL_TOP)/lib/megaco/ebin  \
	$(MEGACO_APP_VSN_COMPILE_FLAGS) 


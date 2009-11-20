#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2009. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
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


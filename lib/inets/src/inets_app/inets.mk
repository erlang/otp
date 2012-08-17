#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2012. All Rights Reserved.
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

ifeq ($(INETS_TRACE), io)
ERL_COMPILE_FLAGS += -Dinets_trace_io
endif

ifeq ($(INETS_DEBUG), true)
ERL_COMPILE_FLAGS += -Dinets_debug
endif

ifeq ($(USE_INETS_HIPE), true)
ERL_COMPILE_FLAGS += +native
endif

ifeq ($(WARN_UNUSED_WARS), true)
ERL_COMPILE_FLAGS += +warn_unused_vars
endif

INETS_APP_VSN_COMPILE_FLAGS = \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,app_vsn,$(APP_VSN)}'

INETS_ERL_COMPILE_FLAGS += \
	-pa $(ERL_TOP)/lib/inets/ebin  \
	$(INETS_APP_VSN_COMPILE_FLAGS) 


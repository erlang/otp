#-*-makefile-*-   ; force emacs to enter makefile-mode
# ----------------------------------------------------
# Make include file for Ryu
#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2011-2021. All Rights Reserved.
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
# ----------------------------------------------------

RYU_FILES = d2s

RYU_OBJDIR = $(ERL_TOP)/erts/emulator/ryu/obj/$(TARGET)/$(TYPE)
RYU_OBJS = $(RYU_FILES:%=$(RYU_OBJDIR)/%.o)
RYU_DIR =  $(ERL_TOP)/erts/emulator/ryu

RYU_SRC = $(RYU_FILES:%=ryu/%.c)

ifeq ($(TARGET), win32)
RYU_LIBRARY = $(RYU_OBJDIR)/ryu.lib
else
RYU_LIBRARY = $(RYU_OBJDIR)/libryu.a
endif

ifeq ($(TARGET), win32)
RYU_CFLAGS = $(CFLAGS)
else
RYU_CFLAGS = $(filter-out -Wdeclaration-after-statement,$(CFLAGS))
endif

ifeq ($(TARGET), win32)
$(RYU_LIBRARY): $(RYU_OBJS)
	$(V_AR) -out:$@ $(RYU_OBJS)
else
$(RYU_LIBRARY): $(RYU_OBJS)
	$(V_AR) $(ARFLAGS) $@ $(RYU_OBJS)
	-@ ($(RANLIB) $@ || true) 2>/dev/null
endif



$(RYU_OBJDIR)/%.o: ryu/%.c
	$(V_CC) -c $(RYU_CFLAGS) -o $@ $<

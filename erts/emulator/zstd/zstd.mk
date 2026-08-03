#-*-makefile-*-   ; force emacs to enter makefile-mode
# ----------------------------------------------------
# Make include file for zstd
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
#
# ----------------------------------------------------

ZSTD_SRC := $(sort $(wildcard zstd/common/*.c) \
                $(wildcard zstd/compress/*.c) \
                $(wildcard zstd/decompress/*.c) \
              )

ifneq ($(TARGET), win32)
ZSTD_SRC := $(ZSTD_SRC) $(wildcard zstd/decompress/*_amd64.S)
endif

# On windows we *need* a separate zstd during debug build
ZSTD_OBJDIR := $(ERL_TOP)/erts/emulator/zstd/obj/$(TARGET)/$(TYPE)

ZSTD_OBJS := $(patsubst %.S,%.o,$(patsubst %.c,%.o,\
		$(foreach file, $(ZSTD_SRC), $(ZSTD_OBJDIR)/$(notdir $(file)))))

ifeq ($(TARGET), win32)
ZSTD_LIBRARY = $(ZSTD_OBJDIR)/zstd.lib
else
ZSTD_LIBRARY = $(ZSTD_OBJDIR)/libzstd.a
endif

ifeq ($(TYPE),gcov)
ZSTD_CFLAGS = -O0 -fprofile-arcs -ftest-coverage $(DEBUG_CFLAGS) $(DEFS) $(THR_DEFS)
else  # !gcov
ifeq ($(TYPE),debug)
## DEBUGLEVEL=1 enables asserts, see common/debug.h for details
ZSTD_CFLAGS = -DDEBUGLEVEL=1 $(DEBUG_CFLAGS) $(DEFS) $(THR_DEFS)
else # !debug && !gcov

space:=$(subst ,, )
ZSTD_CFLAGS = $(subst $(space)-O2$(space), -O3 , $(CONFIGURE_CFLAGS) $(DEFS) $(THR_DEFS))
ifeq ($(TYPE), asan)
ZSTD_CFLAGS += -DZSTD_ADDRESS_SANITIZER
endif # asan

endif # debug
endif # gcov

ZSTD_CFLAGS += -MMD -MP

# Set VISIBLE's to empty to NOT export any symbols for dynamic linking
ZSTD_CFLAGS += -DZSTDLIB_VISIBLE= -DZSTDERRORLIB_VISIBLE=

ifeq ($(TARGET), win32)
$(ZSTD_LIBRARY): $(ZSTD_OBJS)
	$(V_AR) -out:$@ $(ZSTD_OBJS)
else
$(ZSTD_LIBRARY): $(ZSTD_OBJS)
	$(V_AR) $(ARFLAGS) $@ $(ZSTD_OBJS)
	-@ ($(RANLIB) $@ || true) 2>/dev/null
endif

DEPFILES := $(ZSTD_OBJS:.o=.d)
$(DEPFILES):

# The leading '-' means: do not fail is include fails (ex: directory does not exist yet)
-include $(wildcard $(DEPFILES))

$(ZSTD_OBJDIR)/%.o: zstd/common/%.c
	$(V_CC) -c $(ZSTD_CFLAGS) -o $@ $<
$(ZSTD_OBJDIR)/%.o: zstd/compress/%.c
	$(V_CC) -c $(ZSTD_CFLAGS) -o $@ $<
$(ZSTD_OBJDIR)/%.o: zstd/decompress/%.c
	$(V_CC) -c $(ZSTD_CFLAGS) -o $@ $<
$(ZSTD_OBJDIR)/%.o: zstd/decompress/%.S
	$(V_CC) -c $(ZSTD_CFLAGS) -o $@ $<

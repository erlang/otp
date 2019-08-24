#-*-makefile-*-   ; force emacs to enter makefile-mode
# ----------------------------------------------------
# Make include file for zlib
#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2011-2016. All Rights Reserved.
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
# Copyright for zlib itself see copyright notice in zlib.h

ZLIB_FILES = \
	adler32 \
	compress \
	crc32 \
	uncompr \
	deflate \
	trees \
	zutil \
	inflate \
	inftrees \
	inffast

# On windows we *need* a separate zlib during debug build
ZLIB_OBJDIR = $(ERL_TOP)/erts/emulator/zlib/obj/$(TARGET)/$(TYPE)

ZLIB_OBJS = $(ZLIB_FILES:%=$(ZLIB_OBJDIR)/%.o)
ZLIB_SRC = $(ZLIB_FILES:%=zlib/%.c)

ifeq ($(TARGET), win32)
ZLIB_LIBRARY = $(ZLIB_OBJDIR)/z.lib
else
ZLIB_LIBRARY = $(ZLIB_OBJDIR)/libz.a
endif


ifeq ($(TYPE),gcov)
ZLIB_CFLAGS = -O0 -fprofile-arcs -ftest-coverage $(DEBUG_CFLAGS) $(DEFS) $(THR_DEFS)
else  # gcov
ifeq ($(TYPE),debug)
ZLIB_CFLAGS = -DZLIB_DEBUG=1 $(DEBUG_CFLAGS) $(DEFS) $(THR_DEFS)
else # debug
ZLIB_CFLAGS = $(subst -O2, -O3, $(CONFIGURE_CFLAGS) $(DEFS) $(THR_DEFS))
#ZLIB_CFLAGS=-O -DMAX_WBITS=14 -DMAX_MEM_LEVEL=7
#ZLIB_CFLAGS=-g -DDEBUG
#ZLIB_CFLAGS=-O3 -Wall -Wwrite-strings -Wpointer-arith -Wconversion \
#	-Wstrict-prototypes -Wmissing-prototypes
endif # debug
endif # gcov

# Don't fail if _LFS64_LARGEFILE is undefined
ZLIB_CFLAGS := $(filter-out -Werror=undef,$(ZLIB_CFLAGS))

ifeq ($(TARGET), win32)
$(ZLIB_LIBRARY): $(ZLIB_OBJS)
	$(V_AR) -out:$@ $(ZLIB_OBJS)
else
$(ZLIB_LIBRARY): $(ZLIB_OBJS)
	$(V_AR) $(ARFLAGS) $@ $(ZLIB_OBJS)
	-@ ($(RANLIB) $@ || true) 2>/dev/null
endif

$(ZLIB_OBJDIR)/%.o: zlib/%.c
	$(V_CC) -c $(ZLIB_CFLAGS) -o $@ $<

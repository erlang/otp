#-*-makefile-*-   ; force emacs to enter makefile-mode
# ----------------------------------------------------
# Make include file for zlib
#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2011-2012. All Rights Reserved.
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
ZLIB_CFLAGS = $(DEBUG_CFLAGS) $(DEFS) $(THR_DEFS)
else # debug
ZLIB_CFLAGS = $(subst -O2, -O3, $(CONFIGURE_CFLAGS) $(DEFS) $(THR_DEFS))
#ZLIB_CFLAGS=-O -DMAX_WBITS=14 -DMAX_MEM_LEVEL=7
#ZLIB_CFLAGS=-g -DDEBUG
#ZLIB_CFLAGS=-O3 -Wall -Wwrite-strings -Wpointer-arith -Wconversion \
#	-Wstrict-prototypes -Wmissing-prototypes
endif # debug
endif # gcov

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

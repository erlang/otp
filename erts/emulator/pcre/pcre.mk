#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2012. All Rights Reserved.
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

PCRE_O = \
pcre_latin_1_table.o \
pcre_compile.o \
pcre_config.o \
pcre_dfa_exec.o \
pcre_exec.o \
pcre_fullinfo.o \
pcre_get.o \
pcre_globals.o \
pcre_info.o \
pcre_maketables.o \
pcre_newline.o \
pcre_ord2utf8.o \
pcre_refcount.o \
pcre_study.o \
pcre_tables.o \
pcre_try_flipped.o \
pcre_ucp_searchfuncs.o \
pcre_valid_utf8.o \
pcre_version.o \
pcre_xclass.o

PCRE_OBJS = $(PCRE_O:%=$(PCRE_OBJDIR)/%)

PCRE_GENINC = $(ERL_TOP)/erts/emulator/pcre/pcre_exec_loop_break_cases.inc

PCRE_OBJDIR = $(ERL_TOP)/erts/emulator/pcre/obj/$(TARGET)/$(TYPE)

PCRE_CFLAGS = $(filter-out -DDEBUG,$(CFLAGS)) -DERLANG_INTEGRATION

ifeq ($(TARGET), win32)
$(EPCRE_LIB): $(PCRE_OBJS)
	$(V_AR) -out:$@ $(PCRE_OBJS)
else
$(EPCRE_LIB): $(PCRE_OBJS)
	$(V_AR) $(ARFLAGS) $@ $(PCRE_OBJS)
	-@ ($(RANLIB) $@ || true) 2>/dev/null
endif

$(PCRE_OBJDIR)/%.o: pcre/%.c
	$(V_CC) -c $(PCRE_CFLAGS) -o $@ $<

$(PCRE_GENINC): pcre/pcre_exec.c
	$(gen_verbose)for x in `grep -n COST_CHK pcre/pcre_exec.c | grep -v 'COST_CHK(N)' | awk -F: '{print $$1}'`; \
	do \
		N=`expr $$x + 100`; \
		echo "case $$N: goto L_LOOP_COUNT_$${x};"; \
	done > $(PCRE_GENINC)

# Dependencies.

$(PCRE_OBJDIR)/pcre_chartables.o: pcre/pcre_chartables.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_compile.o: pcre/pcre_compile.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_config.o: pcre/pcre_config.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_dfa_exec.o: pcre/pcre_dfa_exec.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_exec.o: pcre/pcre_exec.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h $(PCRE_GENINC)
$(PCRE_OBJDIR)/pcre_fullinfo.o: pcre/pcre_fullinfo.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_get.o: pcre/pcre_get.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_globals.o: pcre/pcre_globals.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_info.o: pcre/pcre_info.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_maketables.o: pcre/pcre_maketables.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_newline.o: pcre/pcre_newline.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_ord2utf8.o: pcre/pcre_ord2utf8.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre/pcre_refcount.o: pcre/pcre_refcount.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_study.o: pcre/pcre_study.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_tables.o: pcre/pcre_tables.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_try_flipped.o: pcre/pcre_try_flipped.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre_ucp_searchfuncs.o: pcre/pcre_ucp_searchfuncs.c \
  pcre/pcre_internal.h pcre/local_config.h pcre/pcre.h pcre/ucp.h \
  pcre/ucpinternal.h pcre/ucptable.h
$(PCRE_OBJDIR)/pcre_valid_utf8.o: pcre/pcre_valid_utf8.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h
pcre_version.o: pcre/pcre_version.c pcre/pcre_internal.h pcre/local_config.h \
  pcre/pcre.h pcre/ucp.h
$(PCRE_OBJDIR)/pcre/pcre_xclass.o: pcre/pcre_xclass.c pcre/pcre_internal.h \
  pcre/local_config.h pcre/pcre.h pcre/ucp.h

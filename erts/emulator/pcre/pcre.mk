#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2012-2016. All Rights Reserved.
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

PCRE_O = \
pcre_latin_1_table.o \
pcre_compile.o \
pcre_config.o \
pcre_dfa_exec.o \
pcre_exec.o \
pcre_fullinfo.o \
pcre_get.o \
pcre_globals.o \
pcre_maketables.o \
pcre_newline.o \
pcre_ord2utf8.o \
pcre_refcount.o \
pcre_study.o \
pcre_tables.o \
pcre_valid_utf8.o \
pcre_version.o \
pcre_byte_order.o \
pcre_jit_compile.o \
pcre_string_utils.o \
pcre_ucd.o \
pcre_xclass.o

PCRE_OBJS = $(PCRE_O:%=$(PCRE_OBJDIR)/%)

PCRE_GENINC = $(ERL_TOP)/erts/emulator/pcre/pcre_exec_loop_break_cases.inc

PCRE_OBJDIR = $(ERL_TOP)/erts/emulator/pcre/obj/$(TARGET)/$(TYPE)

PCRE_DIR =  $(ERL_TOP)/erts/emulator/pcre

PCRE_CFLAGS = $(filter-out -DDEBUG,$(CFLAGS)) -DERLANG_INTEGRATION

ifeq ($(TARGET), win32)
$(EPCRE_LIB): $(PCRE_OBJS)
	$(V_AR) -out:$@ $(PCRE_OBJS)
else
$(EPCRE_LIB): $(PCRE_OBJS)
	$(V_AR) $(ARFLAGS) $@ $(PCRE_OBJS)
	-@ ($(RANLIB) $@ || true) 2>/dev/null
endif

$(PCRE_OBJDIR)/%.o: $(PCRE_DIR)/%.c
	$(V_CC) -c $(PCRE_CFLAGS) -o $@ $<

$(PCRE_GENINC): $(PCRE_DIR)/pcre_exec.c
	$(gen_verbose)for x in `grep -n COST_CHK $(PCRE_DIR)/pcre_exec.c | grep -v 'COST_CHK(N)' | awk -F: '{print $$1}'`; \
	do \
		N=`expr $$x + 100`; \
		echo "case $$N: goto L_LOOP_COUNT_$${x};"; \
	done > $(PCRE_GENINC)

# Dependencies.

$(PCRE_OBJDIR)/pcre_byte_order.o: $(PCRE_DIR)/pcre_byte_order.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h \
	$(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_compile.o: $(PCRE_DIR)/pcre_compile.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_config.o: $(PCRE_DIR)/pcre_config.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_dfa_exec.o: $(PCRE_DIR)/pcre_dfa_exec.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_exec.o: $(PCRE_DIR)/pcre_exec.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h $(PCRE_GENINC)
$(PCRE_OBJDIR)/pcre_fullinfo.o: $(PCRE_DIR)/pcre_fullinfo.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_get.o: $(PCRE_DIR)/pcre_get.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_globals.o: $(PCRE_DIR)/pcre_globals.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_jit_compile.o: $(PCRE_DIR)/pcre_jit_compile.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_latin_1_table.o: $(PCRE_DIR)/pcre_latin_1_table.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_maketables.o: $(PCRE_DIR)/pcre_maketables.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_newline.o: $(PCRE_DIR)/pcre_newline.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_ord2utf8.o: $(PCRE_DIR)/pcre_ord2utf8.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_refcount.o: $(PCRE_DIR)/pcre_refcount.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_string_utils.o: $(PCRE_DIR)/pcre_string_utils.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_study.o: $(PCRE_DIR)/pcre_study.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_tables.o: $(PCRE_DIR)/pcre_tables.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_ucd.o: $(PCRE_DIR)/pcre_ucd.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_valid_utf8.o: $(PCRE_DIR)/pcre_valid_utf8.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_version.o: $(PCRE_DIR)/pcre_version.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h
$(PCRE_OBJDIR)/pcre_xclass.o: $(PCRE_DIR)/pcre_xclass.c \
	$(PCRE_DIR)/pcre_internal.h $(PCRE_DIR)/local_config.h \
	$(PCRE_DIR)/pcre.h $(PCRE_DIR)/ucp.h

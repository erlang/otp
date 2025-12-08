#
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2012-2025. All Rights Reserved.
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
pcre2_auto_possess.o \
pcre2_chartables.o \
pcre2_chkdint.o \
pcre2_compile.o \
pcre2_compile_cgroup.o \
pcre2_compile_class.o \
pcre2_config.o \
pcre2_context.o \
pcre2_error.o \
pcre2_extuni.o \
pcre2_find_bracket.o \
pcre2_match.o \
pcre2_match_data.o \
pcre2_newline.o \
pcre2_ord2utf.o \
pcre2_pattern_info.o \
pcre2_script_run.o \
pcre2_serialize.o \
pcre2_string_utils.o \
pcre2_study.o \
pcre2_substring.o \
pcre2_tables.o \
pcre2_ucd.o \
pcre2_valid_utf.o \
pcre2_xclass.o

PCRE_OBJS = $(PCRE_O:%=$(PCRE_OBJDIR)/%)

PCRE_GENINC = $(ERL_TOP)/erts/emulator/pcre/pcre2_match_loop_break_cases.gen.h
PCRE_YIELD_COV = $(ERL_TOP)/erts/emulator/pcre/pcre2_match_yield_coverage.gen.h

PCRE_OBJDIR = $(ERL_TOP)/erts/emulator/pcre/obj/$(TARGET)/$(TYPE)

PCRE_DIR =  $(ERL_TOP)/erts/emulator/pcre

PCRE_CFLAGS = $(filter-out -Wimplicit-fallthrough,$(CFLAGS)) $(VISIBILITY_HIDDEN) -DERLANG_INTEGRATION

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

$(PCRE_GENINC): $(PCRE_DIR)/pcre2_match.c
	$(gen_verbose)for line in `grep -n 'COST_CHK(' $(PCRE_DIR)/pcre2_match.c | grep -E -v 'define|DBG_FAKE_' | awk -F: '{print $$1}'`; \
	do \
		echo "case $$line: goto L_LOOP_COUNT_$${line};"; \
	done > $(PCRE_GENINC)

$(PCRE_YIELD_COV): $(PCRE_DIR)/pcre2_match.c
	$(gen_verbose) INDEX=0; \
	for line in `grep -n 'COST_CHK(' $(PCRE_DIR)/pcre2_match.c | grep -v 'define' | awk -F: '{print $$1}'`; \
	do \
		echo "#define ERLANG_YIELD_POINT_$${line} $$INDEX"; \
		echo "$$line,"; \
		INDEX=`expr $$INDEX + 1`; \
	done > $@; \
	echo "#define ERLANG_YIELD_POINT_CNT $$INDEX" >> $@


# Target static_depend is currently only ment to be run manually.
# It works on Ubuntu Linux but not en FreeBSD for example.
# > make -f pcre.mk static_depend
# > git add depend.mk
static_depend:
	gcc -MM -MG -c -DERLANG_INTEGRATION -DDEBUG *.c \
		| sed -E 's/\S+\.o:/$$(PCRE_OBJDIR)\/&/' \
		| sed -E 's/\S+\.[ch]\b/$$(PCRE_DIR)\/&/g' > depend.mk

-include $(PCRE_DIR)/depend.mk

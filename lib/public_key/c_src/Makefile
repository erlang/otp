#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2022-2025. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk
include $(ERL_TOP)/make/$(TARGET)/otp_ded.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(PUBLIC_KEY_VSN)

# ----------------------------------------------------
# The following variables differ on different systems, we set
# reasonable defaults, if something different is needed it should
# be set for that system only.
# ----------------------------------------------------
CC = $(DED_CC)
CFLAGS = $(DED_CFLAGS) -I./
LD = $(DED_LD)
SHELL = /bin/sh
#LIBS = $(DED_LIBS) @LIBS@
LIBS = $(DED_LIBS)
LDFLAGS += $(DED_LDFLAGS)

PUBKEY_LIBNAME = public_key

SYSINCLUDE = $(DED_SYS_INCLUDE)

PUBKEY_INCLUDES = $(SYSINCLUDE)

ifeq ($(TYPE),debug)
TYPEMARKER = .debug
TYPE_FLAGS = $(subst -O3,,$(subst -O2,,$(CFLAGS))) -DDEBUG @DEBUG_FLAGS@
else
ifeq ($(TYPE),valgrind)
TYPEMARKER = .valgrind
TYPE_FLAGS = $(subst -O3,,$(subst -O2,,$(CFLAGS))) -DVALGRIND
else
TYPEMARKER =
TYPE_FLAGS = $(CFLAGS)
endif
endif

#DEFS = @DEFS@
#CONFIG_H_DIR = @ERTS_CONFIG_H_IDIR@

ALL_CFLAGS = $(DEFS) $(CONFIG_H_DIR) $(TYPE_FLAGS) $(PUBKEY_INCLUDES) \
	-I$(OBJDIR) -I$(ERL_TOP)/erts/emulator/$(TARGET)

ROOTDIR = $(ERL_TOP)/lib
PRIVDIR = ../priv
LIBDIR = $(PRIVDIR)/lib/$(TARGET)
OBJDIR = $(PRIVDIR)/obj/$(TARGET)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/public_key-$(VSN)

# ----------------------------------------------------
# Misc Macros
# ----------------------------------------------------

ifeq ($(TARGET),win32)
PUBKEY_LIB = $(LIBDIR)/$(PUBKEY_LIBNAME)$(TYPEMARKER).$(DED_EXT)
DIRS = $(OBJDIR) $(LIBDIR)
endif

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

_create_dirs := $(shell mkdir -p $(OBJDIR) $(LIBDIR))

debug opt valgrind lcnt asan gcov: $(DIRS) $(PUBKEY_LIB)

$(OBJDIR):
	-@mkdir -p $(OBJDIR)

$(LIBDIR):
	-@mkdir -p $(LIBDIR)

$(OBJDIR)/%$(TYPEMARKER).o: %.c
	$(V_CC) -c -o $@ $(ALL_CFLAGS) $<

$(LIBDIR)/%$(TYPEMARKER).$(DED_EXT): $(OBJDIR)/%$(TYPEMARKER).o
	$(V_LD) $(LDFLAGS) -o $@ $^ $(LIBS)

clean:
	rm -f $(PUBKEY_LIB)
	rm -f core *~

docs:

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

ifeq ($(TARGET),win32)
release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/lib"
	$(INSTALL_PROGRAM) $(PUBKEY_LIB) "$(RELSYSDIR)/priv/lib"
else
release_spec: opt

endif
release_docs_spec:

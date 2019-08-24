#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk
include $(ERL_TOP)/make/$(TARGET)/otp_ded.mk

CC = $(DED_CC)
LD = $(DED_LD)
LIBS = $(DED_LIBS)

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(ASN1_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/asn1-$(VSN)


# ----------------------------------------------------
# FLAGS misc
# ----------------------------------------------------
ifeq ($(TYPE),debug)
TYPEMARKER = .debug
else
TYPEMARKER =
endif

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
CFLAGS = $(DED_INCLUDES) $(EI_INCLUDES) $(DED_CFLAGS)
STATIC_CFLAGS = $(DED_INCLUDES) $(EI_INCLUDES) $(DED_STATIC_CFLAGS)
LDFLAGS += $(DED_LDFLAGS)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

NIF_OBJ_FILES = $(OBJDIR)/asn1_erl_nif.o
NIF_STATIC_OBJ_FILES = $(OBJDIR)/asn1_erl_nif_static.o

# Module and shared lib have to have same name of
# static nifs to work
ifeq ($(TARGET),win32)
NIF_SHARED_OBJ_FILE = $(LIBDIR)/asn1rt_nif.dll
NIF_LIB_FILE = $(LIBDIR)/asn1rt_nif.lib
CLIB_FLAGS =
LN=cp
else
NIF_SHARED_OBJ_FILE = $(LIBDIR)/asn1rt_nif.so
NIF_LIB_FILE = $(LIBDIR)/asn1rt_nif.a
CLIB_FLAGS = -lc
LN= ln -s
endif

ifeq ($(USING_VC),yes)
AR_OUT=-out:
AR_FLAGS=
else
AR_OUT=
ifeq ($(V),0)
AR_FLAGS=rc
else
AR_FLAGS=rcv
endif
endif

ifndef RANLIB
RANLIB=true
endif

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

_create_dirs := $(shell mkdir -p $(OBJDIR) $(LIBDIR))

opt: $(NIF_SHARED_OBJ_FILE)

debug: opt

static_lib: $(NIF_LIB_FILE)

clean:
	rm -f core *~ 
	rm -f $(LIBDIR)/*
	rm -f $(OBJDIR)/*

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------


$(OBJDIR)/%.o: %.c
	$(V_CC) -c $(CFLAGS) -O3 -o $@ $<

$(OBJDIR)/%_static.o: %.c
	$(V_CC) -c $(STATIC_CFLAGS) -O3 -o $@ $<

$(NIF_LIB_FILE): $(NIF_STATIC_OBJ_FILES)
	$(V_AR) $(AR_FLAGS) $(AR_OUT)$@ $(NIF_STATIC_OBJ_FILES)
	$(V_RANLIB) $@

$(NIF_SHARED_OBJ_FILE): $(NIF_OBJ_FILES)
	$(V_LD) $(LDFLAGS) -o $(NIF_SHARED_OBJ_FILE) $(NIF_OBJ_FILES) $(CLIB_FLAGS) $(LIBS)

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/lib"
	$(INSTALL_PROGRAM) $(NIF_SHARED_OBJ_FILE) "$(RELSYSDIR)/priv/lib"
	$(INSTALL_DIR) "$(RELSYSDIR)/c_src"
	$(INSTALL_DATA) *.c "$(RELSYSDIR)/c_src"

release_docs_spec:


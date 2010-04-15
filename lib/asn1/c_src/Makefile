#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2002-2010. All Rights Reserved.
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

EI_LIBDIR = $(ERL_TOP)/lib/erl_interface/obj$(TYPEMARKER)/$(TARGET)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
EI_INCLUDES = -I$(ERL_TOP)/lib/erl_interface/include
CFLAGS = $(DED_INCLUDES) $(EI_INCLUDES) $(DED_CFLAGS)
LDFLAGS += $(DED_LDFLAGS)

LD_INCL_EI = -L$(EI_LIBDIR)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

C_FILES = asn1_erl_driver.c


ifeq ($(TARGET),win32)
LD_EI = -lei_md 
SHARED_OBJ_FILES = $(LIBDIR)/asn1_erl_drv.dll
OBJ_FILES = $(OBJDIR)/asn1_erl_drv.o
CLIB_FLAGS =
LN=cp
else
LD_EI = -lei 
OBJ_FILES = $(OBJDIR)/asn1_erl_drv.o
ifeq ($(findstring vxworks,$(TARGET)),vxworks)
SHARED_OBJ_FILES = $(LIBDIR)/asn1_erl_drv.eld
CLIB_FLAGS =
else
SHARED_OBJ_FILES = $(LIBDIR)/asn1_erl_drv.so
CLIB_FLAGS = -lc
endif
LN= ln -s
endif

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt: $(OBJDIR) $(LIBDIR) $(SHARED_OBJ_FILES)

debug: opt

clean:
	rm -f core *~ 
	rm -f $(LIBDIR)/*
	rm -f $(OBJDIR)/*

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------


$(OBJ_FILES): $(C_FILES)
	$(CC) -c $(CFLAGS) -o $(OBJ_FILES) $(C_FILES) 

$(SHARED_OBJ_FILES): $(OBJ_FILES)
	$(LD) $(LDFLAGS) $(LD_INCL_EI) -o $(SHARED_OBJ_FILES) $(OBJ_FILES) $(LD_EI) $(CLIB_FLAGS) $(LIBS) 

$(LIBDIR):
	-mkdir -p $(LIBDIR)

$(OBJDIR):
	-mkdir -p $(OBJDIR)


 
# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/priv/lib
	$(INSTALL_PROGRAM) $(SHARED_OBJ_FILES) $(RELSYSDIR)/priv/lib
	$(INSTALL_DIR) $(RELSYSDIR)/c_src
	$(INSTALL_DATA) $(C_FILES) $(RELSYSDIR)/c_src

release_docs_spec:


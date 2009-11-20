# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
# Note, this makefile is not called during normal build process, it should
# be used when the preloaded modules actually are to be updated (i.e. the 
# beam files are to be recompiled, which is normally not done). 
# The beam files are placed in the current directory and should be copied 
# to the ../ebin directory by using the commit target (only works in 
# clearcase). 

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

EBIN=.

STATIC_EBIN=../ebin

include $(ERL_TOP)/erts/vsn.mk
include $(ERL_TOP)/lib/kernel/vsn.mk

PRE_LOADED_MODULES = \
	erl_prim_loader \
	init \
	prim_file \
	prim_inet \
	zlib \
	prim_zip \
	otp_ring0 \
	erlang

RELSYSDIR = $(RELEASE_PATH)/lib/erts-$(VSN)
# not $(RELEASE_PATH)/erts-$(VSN)/preloaded

ERL_FILES= $(PRE_LOADED_MODULES:%=%.erl) 

TARGET_FILES = $(PRE_LOADED_MODULES:%=$(EBIN)/%.$(EMULATOR))
STATIC_TARGET_FILES = $(PRE_LOADED_MODULES:%=$(STATIC_EBIN)/%.$(EMULATOR))

KERNEL_SRC=$(ERL_TOP)/lib/kernel/src
KERNEL_INCLUDE=$(ERL_TOP)/lib/kernel/include
STDLIB_INCLUDE=$(ERL_TOP)/lib/stdlib/include

ERL_COMPILE_FLAGS += +warn_obsolete_guard -I$(KERNEL_SRC) -I$(KERNEL_INCLUDE)

debug opt: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES)

prepare:
	cleartool co -nc $(STATIC_EBIN)/*
	cleartool co -nc $(STATIC_EBIN)

copy:
	for x in *.beam; do\
	  if test '!' -f $(STATIC_EBIN)/$$x; then\
	    cleartool mkelem -nc $$x;\
	  fi;\
	done
	cp *.beam $(STATIC_EBIN)

commit:
	cleartool ci -ident -nc $(STATIC_EBIN)/*.beam
	cleartool ci -ident -nc $(STATIC_EBIN)

cancel:
	-cleartool unco -rm $(STATIC_EBIN)
	-cleartool unco -rm $(STATIC_EBIN)/*.beam


include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: 
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(STATIC_TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:


list_preloaded:
	@echo $(PRE_LOADED_MODULES)

# Include dependencies -- list below added by PaN
$(EBIN)/erl_prim_loader.beam: $(KERNEL_SRC)/inet_boot.hrl $(KERNEL_INCLUDE)/file.hrl
$(EBIN)/prim_file.beam: $(KERNEL_INCLUDE)/file.hrl
$(EBIN)/prim_inet.beam: $(KERNEL_SRC)/inet_int.hrl $(KERNEL_INCLUDE)/inet_sctp.hrl
$(EBIN)/prim_zip.beam: zip_internal.hrl $(KERNEL_INCLUDE)/file.hrl $(STDLIB_INCLUDE)/zip.hrl
$(EBIN)/init.erl: $(KERNEL_INCLUDE)/file.hrl

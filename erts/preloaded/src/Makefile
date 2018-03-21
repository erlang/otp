# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
# Note, this makefile is not called during normal build process, it should
# be used when the preloaded modules actually are to be updated (i.e. the 
# beam files are to be recompiled, which is normally not done). 
# The beam files are placed in the current directory and should be copied 
# to the ../ebin directory by using the copy target.

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

EBIN=.

STATIC_EBIN=../ebin

include $(ERL_TOP)/erts/vsn.mk
include $(ERL_TOP)/lib/kernel/vsn.mk

PRE_LOADED_ERL_MODULES = \
	erl_prim_loader \
	init \
	prim_buffer \
	prim_file \
	prim_inet \
	zlib \
	prim_zip \
	otp_ring0 \
	erts_code_purger \
	erlang \
	erts_internal \
	erl_tracer \
	erts_literal_area_collector \
	erts_dirty_process_signal_handler

PRE_LOADED_BEAM_MODULES = \
	prim_eval

PRE_LOADED_MODULES = $(PRE_LOADED_ERL_MODULES) $(PRE_LOADED_BEAM_MODULES)

RELSYSDIR = $(RELEASE_PATH)/lib/erts-$(VSN)
# not $(RELEASE_PATH)/erts-$(VSN)/preloaded

ERL_FILES= $(PRE_LOADED_ERL_MODULES:%=%.erl)
BEAM_FILES= $(PRE_LOADED_BEAM_MODULES:%=%.S)
STUBS_FILES= $(PRE_LOADED_BEAM_MODULES:%=%.erl)

TARGET_FILES = $(PRE_LOADED_MODULES:%=$(EBIN)/%.$(EMULATOR)) \
    $(APP_TARGET)
STATIC_TARGET_FILES = $(PRE_LOADED_MODULES:%=$(STATIC_EBIN)/%.$(EMULATOR))

APP_FILE= erts.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(STATIC_EBIN)/$(APP_FILE)


KERNEL_SRC=$(ERL_TOP)/lib/kernel/src
KERNEL_INCLUDE=$(ERL_TOP)/lib/kernel/include
STDLIB_INCLUDE=$(ERL_TOP)/lib/stdlib/include

ERL_COMPILE_FLAGS += +debug_info -I$(KERNEL_SRC) -I$(KERNEL_INCLUDE)

debug opt: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES)

copy:
	cp *.beam $(STATIC_EBIN)

$(APP_TARGET): $(APP_SRC) $(ERL_TOP)/erts/vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@


include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: $(APP_TARGET)
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(BEAM_FILES) $(STUBS_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(STATIC_TARGET_FILES) $(APP_TARGET) "$(RELSYSDIR)/ebin"

release_docs_spec:


list_preloaded:
	@echo $(PRE_LOADED_MODULES)

#
# Combine a BEAM assembly script file a stub Erlang file into a BEAM file.
# See add_abstract_chunk script.
#

prim_eval.abstr: prim_eval.erl
	$(V_ERLC) $(ERL_COMPILE_FLAGS) -o$(dir $@) +dabstr $<

prim_eval.beam: prim_eval.S prim_eval.abstr
	$(gen_verbose)
	$(V_at)$(ERLC) $(ERL_COMPILE_FLAGS) $<
	$(V_at)escript add_abstract_code $@ prim_eval.abstr || (rm $@; exit 1)

# Include dependencies -- list below added by PaN
$(EBIN)/erl_prim_loader.beam: $(KERNEL_SRC)/inet_boot.hrl $(KERNEL_INCLUDE)/file.hrl
$(EBIN)/prim_file.beam: $(KERNEL_SRC)/file_int.hrl $(KERNEL_INCLUDE)/file.hrl
$(EBIN)/prim_inet.beam: $(KERNEL_SRC)/inet_int.hrl $(KERNEL_INCLUDE)/inet_sctp.hrl
$(EBIN)/prim_zip.beam: zip_internal.hrl $(KERNEL_INCLUDE)/file.hrl $(STDLIB_INCLUDE)/zip.hrl
$(EBIN)/init.erl: $(KERNEL_INCLUDE)/file.hrl

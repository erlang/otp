#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1999-2024. All Rights Reserved.
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

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
APPLICATION=Erlang/OTP
VSN=$(shell cat $(ERL_TOP)/OTP_VERSION)
APP_DIR=../../../lib/erl_interface
INDEX_DIR=.
HTMLDIR=../../../doc
RELSYS_HTMLDIR=$(RELEASE_PATH)/doc

CORE=compiler erts kernel sasl stdlib
DATABASE=mnesia odbc
OAM=os_mon snmp
INTERFACES=asn1 crypto diameter eldap erl_interface ftp inets jinterface megaco \
	public_key ssh ssl tftp wx xmerl
TOOLS=debugger dialyzer et observer parsetools reltool runtime_tools syntax_tools tools
TESTING=common_test eunit
DOCUMENTATION=edoc
SYSTEM:=$(shell awk -F: '{print $$1}' ../guides)

REDIRECTS=$(foreach guide,$(SYSTEM),system/$(guide).md) \
	$(foreach app,$(CORE),core/$(app).md) \
	$(foreach app,$(DATABASE),database/$(app).md) \
	$(foreach app,$(OAM),oam/$(app).md) \
	$(foreach app,$(INTERFACES),interfaces/$(app).md) \
	$(foreach app,$(TOOLS),tools/$(app).md) \
	$(foreach app,$(TESTING),testing/$(app).md) \
	$(foreach app,$(DOCUMENTATION),documentation/$(app).md)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

system core database oam interfaces tools testing documentation:
	$(V_at)mkdir $@
system/%.md: | system
	$(gen_verbose)GUIDE=$(basename $(notdir $@)) && \
		TITLE=$$(grep "$${GUIDE}" ../guides | awk -F: '{print $$2}') && \
		echo "# $${TITLE}\n\n<script>window.location.replace(\"../system/doc/html/$${GUIDE}.html\")</script>" > $@

## No nothing for README
%.md: %.md.in ../../../OTP_VERSION ../../../make/ex_doc_vsn
	$(gen_verbose)sed \
          -e 's:%EX_DOC_VSN%:$(EX_DOC_VSN):g' \
          -e 's:%OTP-VSN%:$(VSN):g' \
          -e 's:%OTP-REL%:$(OTP_REL):g' \
          $< > $@

#--------------------------------------------------------------------------

src/otp_man_index.beam: src/otp_man_index.erl
	$(V_ERLC) -o src +warn_unused_vars $<

man_index.md: src/otp_man_index.beam $(wildcard $(ERL_TOP)/erts/ebin/*.beam) $(wildcard $(ERL_TOP)/lib/*/ebin/*.beam)
	$(gen_verbose)$(ERL) +pc unicode -noshell -pa src -s otp_man_index gen "$@" -s erlang halt

core/erts.md: Makefile | core
	$(gen_verbose)echo "# erts\n\n<script>window.location.replace(\"../erts/doc/html/index.html\")</script>" > $@
%.md: Makefile | core database oam interfaces tools testing documentation
	$(gen_verbose)APP=$(basename $(notdir $@)) && \
		echo "# $${APP}\n\n<script>window.location.replace(\"../lib/$${APP}/doc/html/index.html\")</script>" > $@

HTML_DEPS=README.md man_index.md ../guides $(REDIRECTS)
EPUB=false

include $(ERL_TOP)/make/doc.mk

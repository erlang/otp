#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2022. All Rights Reserved.
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
include ../../vsn.mk
VSN=$(INETS_VSN)
APPLICATION=inets

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml

XML_CHAPTER_FILES = \
	introduction.xml \
	inets_services.xml \
	http_client.xml \
	http_server.xml \
	notes.xml

XML_REF3_FILES = \
	inets.xml \
	http_uri.xml\
	httpc.xml\
	httpd.xml \
	httpd_custom_api.xml \
	httpd_socket.xml \
	httpd_util.xml \
	mod_alias.xml \
	mod_auth.xml \
	mod_esi.xml \
	mod_security.xml

XML_PART_FILES = \
	part.xml

BOOK_FILES = book.xml

XML_FILES = \
	$(BOOK_FILES)        \
	$(XML_CHAPTER_FILES) \
	$(XML_PART_FILES)    \
	$(XML_REF3_FILES)    \
	$(XML_APPLICATION_FILES)

SPEC_FILES = $(XML_REF3_FILES:%.xml=$(SPECDIR)/specs_%.xml)
TOP_SPECS_FILE = specs.xml

NO_CHUNKS = httpd_custom_api.xml

# Generate file specs for files with type specs.
# These are needed because the standard script
# (otp/make/otp.mk.in) does not traverse folders
$(SPECDIR)/specs_%.xml: $(SPECS_ESRC)/http_client/%.erl $(TOP_SPECS_FILE)
	$(gen_verbose)escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) -o$(dir $@) $<
$(SPECDIR)/specs_%.xml: $(SPECS_ESRC)/inets_app/%.erl $(TOP_SPECS_FILE)
	$(gen_verbose)escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) -o$(dir $@) $<
$(SPECDIR)/specs_%.xml: $(SPECS_ESRC)/http_server/%.erl $(TOP_SPECS_FILE)
	$(gen_verbose)escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) -o$(dir $@) $<
$(SPECDIR)/specs_%.xml: $(SPECS_ESRC)/http_lib/%.erl $(TOP_SPECS_FILE)
	$(gen_verbose)escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) -o$(dir $@) $<

# ----------------------------------------------------

include $(ERL_TOP)/make/doc.mk
SPECS_FLAGS += -I../../src/inets_app -I../../include -I../../../kernel/include

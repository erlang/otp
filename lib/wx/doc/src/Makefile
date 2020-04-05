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

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
include ../../config.mk
APPLICATION=wx

ErlMods         = wx.erl wx_object.erl

GenMods         = $(shell ls ../../src/gen | grep 'wx[A-Z].*\.erl') \
		wx_misc.erl \
		glu.erl \
		gl.erl

ModsNoExt	= $(ErlMods:%.erl=%) $(GenMods:%.erl=%)

# Release directory specification

XML_APPLICATION_FILES = ref_man.xml
EDOC_REF3_FILES = $(ErlMods:%.erl=%.xml)
XML_REF3_FILES = $(GenMods:%.erl=%.xml)

XML_PART_FILES = part.xml
EDOC_CHAPTER_FILE = chapter.xml
XML_NOTES_FILES = notes.xml

BOOK_FILES = book.xml

XML_FILES = \
	$(BOOK_FILES) \
	$(XML_PART_FILES) $(XML_NOTES_FILES)

XML_GEN_FILES = \
	$(XML_APPLICATION_FILES:%=$(XMLDIR)/%) \
	$(XML_REF3_FILES:%=$(XMLDIR)/%)

EDOC_FLAGS=-i ../../src -preprocess true -sort_functions false

TOP_SPECS_FILE = specs.xml

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

include $(ERL_TOP)/make/doc.mk

$(XMLDIR)/%.xml: $(APP_SRC_DIR)/gen/%.erl
	$(gen_verbose)escript $(DOCGEN)/priv/bin/xml_from_edoc.escript \
	  -def vsn $(VSN) $(EDOC_FLAGS) -dir $(XMLDIR) $<

ref_man.xml: ref_man.xml.src Makefile
	@echo Preparing $@
	@cat ref_man.xml.src > $@
	@for d in $(ModsNoExt); do         \
		echo "  <xi:include href=\"$$d.xml\"/>" >> $@ ; \
	done
	@echo "</application>" >> $@

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

ErlMods         = wx.xml wx_object.xml

GenMods         = $(shell ls *.xml | grep 'wx[A-Z].*\.xml') \
		wx_misc.xml \
		gl.xml \
		glu.xml

ModsNoExt	= $(ErlMods:%.xml=%) $(GenMods:%.xml=%)

# Release directory specification

XML_APPLICATION_FILES = ref_man.xml
# EDOC_REF3_FILES = $(ErlMods:%.erl=%.xml)
XML_REF3_FILES = $(ErlMods) $(GenMods)

XML_PART_FILES = part.xml
XML_CHAPTER_FILE = chapter.xml
XML_NOTES_FILES = notes.xml

BOOK_FILES = book.xml

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILE) \
	$(XML_PART_FILES) $(XML_NOTES_FILES) $(XML_REF3_FILES) $(XML_REF6_FILES) $(XML_APPLICATION_FILES)

TOP_SPECS_FILE = specs.xml

# clean this generates as well
EXTRA_FILES = ref_man.xml specs.xml

include $(ERL_TOP)/make/doc.mk

.PRECIOUS: $(ModsNoExt:%=../specs/specs_%.xml)

ref_man.xml: ref_man.xml.src Makefile
	@echo Preparing $@
	@cat ref_man.xml.src > $@
	@for d in $(ModsNoExt); do         \
		echo "  <xi:include href=\"$$d.xml\"/>" >> $@ ; \
	done
	@echo "</application>" >> $@

specs.xml: specs.xml.src Makefile
	@echo Preparing $@
	@cat specs.xml.src > $@
	@for d in $(ModsNoExt); do         \
		echo "  <xi:include href=\"../specs/specs_$$d.xml\"/>" >> $@ ; \
	done
	@echo "</specs>" >> $@


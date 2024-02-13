#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2024. All Rights Reserved.
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

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(XMERL_VSN)
APPLICATION=xmerl

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XMERL_DIR = $(ERL_TOP)/lib/$(APPLICATION)/src

EDOC_REF3_FILES = \
	xmerl_scan.xml \
	xmerl.xml \
	xmerl_xs.xml \
	xmerl_eventp.xml \
	xmerl_xpath.xml \
	xmerl_xsd.xml

XML_APPLICATION_FILES = ref_man.xml

XML_REF3_FILES = xmerl_sax_parser.xml

XML_PART_FILES = \
	part.xml

XML_REF6_FILES =

XML_CHAPTER_FILES =  \
	notes.xml

XML_CHAPTER_GEN_FILES = \
	$(XMLDIR)/xmerl_ug.xml

HTML_EXAMPLE_FILES =  \
	xmerl_examples.html \
	xmerl_xs_examples.html

HTML_STYLESHEET_FILES = \
	../stylesheet.css

BOOK_FILES = book.xml

XML_HTML_FILES = \
	notes_history.xml

EXAMPLE_FILES = people2.txt people.txt motorcycles.txt motorcycles_dtd.txt \
	new_motorcycles.txt new_motorcycles2.txt result_export.html \
	motorcycles2.txt result_xs.html motorcycles2html.erl

HTML_EXTRA_FILES = $(EXAMPLE_FILES) $(HTML_EXAMPLE_FILES) $(HTML_STYLESHEET_FILES)

XML_FILES= \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) $(XML_APPLICATION_FILES)

XML_GEN_FILES = $(XML_CHAPTER_GEN_FILES)

TOP_SPECS_FILE = specs.xml

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/doc.mk

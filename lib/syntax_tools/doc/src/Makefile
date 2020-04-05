#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2006-2019. All Rights Reserved.
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
VSN=$(SYNTAX_TOOLS_VSN)
APPLICATION=syntax_tools

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
EDOC_REF3_FILES = \
	epp_dodger.xml \
	erl_comment_scan.xml \
	erl_prettypr.xml \
	erl_recomment.xml \
	erl_syntax.xml \
	erl_syntax_lib.xml \
	erl_tidy.xml \
	igor.xml \
	merl.xml \
	merl_transform.xml \
	prettypr.xml

XML_PART_FILES = part.xml
EDOC_CHAPTER_FILE = chapter.xml
XML_NOTES_FILES = notes.xml

BOOK_FILES = book.xml

XML_FILES=\
	$(BOOK_FILES) $(XML_PART_FILES)  $(XML_APPLICATION_FILES) \
	$(XML_NOTES_FILES)

HTML_EXTRA_FILES = ../../examples/demo.erl

XML_GEN_FILES = $(XML_CHAPTER_FILES:%=$(XMLDIR)/%)

TOP_SPECS_FILE = specs.xml

# ----------------------------------------------------
include $(ERL_TOP)/make/doc.mk

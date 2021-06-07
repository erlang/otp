#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
VSN=$(MNESIA_VSN)
APPLICATION=mnesia

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = \
	mnesia.xml \
	mnesia_frag_hash.xml \
	mnesia_registry.xml

XML_PART_FILES = \
	part.xml

XML_CHAPTER_FILES = \
	Mnesia_chap1.xml \
	Mnesia_overview.xml \
	Mnesia_chap8.xml \
	notes.xml

XML_CHAPTER_GEN_FILES = \
	Mnesia_chap2.xml \
	Mnesia_chap3.xml \
	Mnesia_chap4.xml \
	Mnesia_chap5.xml \
	Mnesia_chap7.xml \
	Mnesia_App_A.xml \
	Mnesia_App_B.xml \
	Mnesia_App_C.xml

BOOK_FILES = book.xml

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) $(XML_APPLICATION_FILES)

XML_GEN_FILES = $(XML_CHAPTER_GEN_FILES:%=$(XMLDIR)/%)

IMAGE_FILES = \
	company.gif

SPECS_FILES = $(XML_REF3_FILES:%.xml=$(SPECDIR)/specs_%.xml)
TOP_SPECS_FILE = specs.xml

include $(ERL_TOP)/make/doc.mk

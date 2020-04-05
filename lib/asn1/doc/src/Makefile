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
VSN=$(ASN1_VSN)
APPLICATION=asn1

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = asn1ct.xml

GEN_XML = \
	asn1_spec.xml

XML_PART_FILES = part.xml

XML_HTML_FILE =  \
	notes_history.xml


XML_CHAPTER_FILES = \
	asn1_introduction.xml \
	asn1_getting_started.xml \
	asn1_overview.xml \
	notes.xml

BOOK_FILES = book.xml

XML_FILES = $(BOOK_FILES) $(XML_APPLICATION_FILES) $(XML_REF3_FILES) \
            $(XML_PART_FILES) $(XML_CHAPTER_FILES)

XML_GEN_FILES = $(GEN_XML:%=$(XMLDIR)/%)

IMAGE_FILES = \
	exclusive_Win_But.gif \
	selective_Window2.gif \
	selective_TypeList.gif

include $(ERL_TOP)/make/doc.mk

.SECONDARY: $(XML_GEN_FILES)

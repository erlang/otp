#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1998-2020. All Rights Reserved.
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
VSN=$(EI_VSN)
APPLICATION=erl_interface

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

XML_REF1_FILES = erl_call_cmd.xml
XML_REF3_FILES = ei_global.xml \
	ei.xml \
	ei_connect.xml \
	registry.xml

BOOK_FILES = book.xml
XML_APPLICATION_FILES = ref_man.xml

XML_PART_FILES = \
	part.xml
XML_CHAPTER_FILES = ei_users_guide.xml notes.xml

XML_FILES = $(XML_REF1_FILES) $(XML_REF3_FILES) $(BOOK_FILES) \
	 $(XML_APPLICATION_FILES) $(XML_PART_FILES) $(XML_CHAPTER_FILES)

NO_CHUNKS=$(XML_REF3_FILES)

# ----------------------------------------------------

include $(ERL_TOP)/make/doc.mk

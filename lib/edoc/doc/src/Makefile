# ``Licensed under the Apache License, Version 2.0 (the "License");
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
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(EDOC_VSN)
APPLICATION=edoc

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
EDOC_REF3_FILES = edoc.xml \
	edoc_doclet.xml \
	edoc_extract.xml \
	edoc_layout.xml \
	edoc_lib.xml \
	edoc_run.xml

XML_PART_FILES = part.xml
EDOC_CHAPTER_FILE = chapter.xml
XML_NOTES_FILES = notes.xml

BOOK_FILES = book.xml

XML_FILES=\
	$(BOOK_FILES) $(XML_APPLICATION_FILES) \
	$(XML_PART_FILES) $(XML_NOTES_FILES)

INCLUDES_DIR = ../../include
INCLUDES = $(INCLUDES_DIR)/edoc_doclet.hrl

DTDS_DIR = ../../priv
DTDS = $(DTDS_DIR)/edoc.dtd

HTML_EXTRA_FILES = $(DTDS) $(INCLUDES)

include $(ERL_TOP)/make/doc.mk

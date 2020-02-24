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
#     $Id$
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(OBSERVER_VSN)
APPLICATION=observer

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF1_FILES = \
	cdv_cmd.xml
XML_REF3_FILES = \
	crashdump.xml \
	observer.xml \
	etop.xml \
	ttb.xml
XML_REF6_FILES = observer_app.xml

XML_PART_FILES = \
	part.xml

XML_CHAPTER_FILES = \
	introduction_ug.xml \
	crashdump_ug.xml \
	etop_ug.xml \
	observer_ug.xml \
	ttb_ug.xml \
	notes.xml

BOOK_FILES = book.xml

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF1_FILES) $(XML_REF3_FILES) \
	$(XML_APPLICATION_FILES) $(XML_REF6_FILES)

IMAGE_FILES = \
	et_processes.gif \
	et_modsprocs.gif

NO_CHUNKS = crashdump.xml
# ----------------------------------------------------

include $(ERL_TOP)/make/doc.mk

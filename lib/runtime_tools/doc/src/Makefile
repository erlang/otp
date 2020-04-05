#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1999-2018. All Rights Reserved.
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

# Make the docs target default.

default: docs

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(RUNTIME_TOOLS_VSN)
APPLICATION=runtime_tools

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = \
	dbg.xml \
	dyntrace.xml \
	erts_alloc_config.xml \
	system_information.xml \
	msacc.xml \
	scheduler.xml
XML_REF6_FILES = runtime_tools_app.xml

XML_PART_FILES = part.xml
XML_CHAPTER_FILES = notes.xml LTTng.xml

GENERATED_XML_FILES = DTRACE.xml SYSTEMTAP.xml

BOOK_FILES = book.xml

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) \
	$(XML_REF6_FILES) $(XML_APPLICATION_FILES)

XML_GEN_FILES = $(GENERATED_XML_FILES:%=$(XMLDIR)/%)

IMAGE_FILES =

TOP_SPECS_FILE = specs.xml

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(XMLDIR)/%.xml: $(ERL_TOP)/HOWTO/%.md $(ERL_TOP)/make/emd2exml
	$(ERL_TOP)/make/emd2exml $< $@

# ----------------------------------------------------

include $(ERL_TOP)/make/doc.mk

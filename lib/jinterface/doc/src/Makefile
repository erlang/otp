# -*-Makefile-*-

#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2000-2018. All Rights Reserved.
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
VSN=$(JINTERFACE_VSN)
APPLICATION=jinterface

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

# Renamed this var to fool otp.mk.in
XML_APP_FILES = ref_man.xml
XML_REF3_FILES = jinterface.xml

XML_PART_FILES = \
	part.xml
XML_CHAPTER_FILES = \
	notes.xml \
	jinterface_users_guide.xml

BOOK_FILES = book.xml

XML_FILES = $(BOOK_FILES) $(XML_APP_FILES) $(XML_REF3_FILES) \
            $(XML_PART_FILES) $(XML_CHAPTER_FILES)

NO_CHUNKS=$(XML_REF3_FILES)

#------------------------------------------------------

JAVADOC = javadoc
JAVADOC_PKGS = com.ericsson.otp.erlang
JAVA_PKG_PATH = com/ericsson/otp/erlang
JAVADOC_TITLE = 'Java-Erlang Interface Library'
JAVADOC_DEST = ../doc/html/java
JAVA_SRC_PATH = $(ERL_TOP)/lib/$(APPLICATION)/java_src/$(JAVA_PKG_PATH)

# don't add filenames to the Makefile!
# all java sourcefiles listed in common include file
include $(JAVA_SRC_PATH)/java_files

JAVA_SRC_FILES = $(JAVA_FILES:%=$(JAVA_SRC_PATH)/%.java)

# ----------------------------------------------------


../html/java/index.html: $(JAVA_SRC_FILES)
	(cd ../../java_src;$(JAVADOC) -sourcepath . -d $(JAVADOC_DEST) \
		-windowtitle $(JAVADOC_TITLE) $(JAVADOC_PKGS))

html: ../html/java/index.html

include $(ERL_TOP)/make/doc.mk

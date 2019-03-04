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

GIF_FILES =
#------------------------------------------------------


HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf


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

JAVA_DOC_FILES = \
	overview-tree.html \
	index-all.html \
	deprecated-list.html \
	allclasses-frame.html \
	index.html \
	serialized-form.html \
	package-list \
	stylesheet.css \
	help-doc.html

INFO_FILE = ../../info
JAVA_EXTRA_FILES = $(JAVA_DOC_FILES:%=$(HTMLDIR)/java/%)

JAVA_GEN_FILES = \
	$(JAVA_FILES:%=$(JAVADOC_DEST)/$(JAVA_PKG_PATH)/%.html) \
	$(JAVADOC_DEST)/$(JAVA_PKG_PATH)/package-summary.html \
	$(JAVADOC_DEST)/$(JAVA_PKG_PATH)/package-tree.html \
	$(JAVADOC_DEST)/$(JAVA_PKG_PATH)/package-frame.html


# ----------------------------------------------------


HTML_FILES = \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

TOP_HTML_FILES = $(INDEX_TARGET)

INDEX_FILE = index.html
INDEX_TARGET = $(DOCDIR)/$(INDEX_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
XML_FLAGS +=
DVIPS_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@

docs: pdf html jdoc man

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

clean clean_docs:
	rm -rf $(HTMLDIR)/*
	rm -rf $(XMLDIR)
	rm -f $(MAN3DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~

jdoc:$(JAVA_SRC_FILES)
	(cd ../../java_src;$(JAVADOC) -sourcepath . -d $(JAVADOC_DEST) \
		-windowtitle $(JAVADOC_TITLE) $(JAVADOC_PKGS))
	touch jdoc

man:

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

debug opt:

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_docs_spec: docs
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DIR_DATA) $(HTMLDIR) "$(RELSYSDIR)/doc/html"


release_spec:

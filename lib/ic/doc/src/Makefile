#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1998-2017. All Rights Reserved.
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
VSN=$(IC_VSN)
APPLICATION=ic

# ----------------------------------------------------
# Java specific
# ----------------------------------------------------
JAVADOC=javadoc
JAVA_INCL_ROOT = $(ERL_TOP)/lib/jinterface/priv/
JAVA_SRC_ROOT = $(ERL_TOP)/lib/ic/java_src/
JAVA_CLASS_SUBDIR = com/ericsson/otp/ic/

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = ic.xml \
	 	  ic_clib.xml \
		  ic_c_protocol.xml

XML_PART_FILES = part.xml

XML_CHAPTER_FILES = \
	ch_introduction.xml \
	ch_basic_idl.xml \
	ch_ic_protocol.xml \
	ch_erl_plain.xml \
	ch_erl_genserv.xml \
	ch_c_mapping.xml \
        ch_c_client.xml \
	ch_c_server.xml \
	ch_c_corba_env.xml \
	ch_java.xml \
	notes.xml

BOOK_FILES = book.xml

XML_FILES = $(BOOK_FILES) $(XML_APPLICATION_FILES) $(XML_REF3_FILES) \
            $(XML_PART_FILES) $(XML_CHAPTER_FILES)

GIF_FILES =

# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

INFO_FILE = ../../info
EXTRA_FILES = \
	$(DEFAULT_GIF_FILES) \
	$(DEFAULT_HTML_FILES) \
	$(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

JAVA_SOURCE_FILES = \
	Holder.java \
	BooleanHolder.java \
	ByteHolder.java \
	CharHolder.java \
	DoubleHolder.java \
	FloatHolder.java \
	IntHolder.java \
	LongHolder.java \
	ShortHolder.java \
	StringHolder.java \
	Environment.java \
	Any.java \
	AnyHelper.java \
	AnyHolder.java \
	TypeCode.java \
	TCKind.java \
	Pid.java \
	PidHolder.java \
	PidHelper.java \
	Ref.java \
	RefHolder.java \
	RefHelper.java \
	Port.java \
	PortHolder.java \
	PortHelper.java \
	Term.java \
	TermHolder.java \
	TermHelper.java


JD_INDEX_HTML_FILES = \
	allclasses-frame.html \
	allclasses-noframe.html \
	deprecated-list.html  \
	index-all.html \
	overview-tree.html \
	stylesheet.css \
	help-doc.html \
	index.html \
	package-list \
	serialized-form.html \
	constant-values.html

JD_GIF_FILES = \
	../html/java/resources/inherit.gif


PACK_DIR = com/ericsson/otp/ic
JAVA_SOURCE_DIR = ../../java_src/$(PACK_DIR)
JAVA_OUT_DIR = ../html/java

JD_PACK_HTML_FILES = \
	package-frame.html \
	package-summary.html \
	package-tree.html

JAVADOC_PACK_HTML_FILES = \
	$(JAVA_SOURCE_FILES:%.java=$(JAVA_OUT_DIR)/$(PACK_DIR)/%.html) \
	$(JD_PACK_HTML_FILES:%=$(JAVA_OUT_DIR)/$(PACK_DIR)/%)

JAVADOC_INDEX_HTML_FILES = $(JD_INDEX_HTML_FILES:%=$(JAVA_OUT_DIR)/%)

JAVADOC_GENERATED_FILES = $(JAVADOC_PACK_HTML_FILES) $(JAVADOC_INDEX_HTML_FILES)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
CLASSPATH = $(JAVA_SRC_ROOT):$(JAVA_INCL_ROOT)

XML_FLAGS +=
DVIPS_FLAGS +=
JAVADOCFLAGS = \
	-classpath $(CLASSPATH) \
	-d ../doc/html/java \
	-windowtitle "Package com.ericsson.otp.ic version $(IC_VSN)" \
	-public \
	-footer "<CENTER><FONT SIZE=-1>Copyright &copy; 1991-2007 Ericsson AB<BR> </FONT> </CENTER>"


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@

ifneq (,$(JAVA))
docs: pdf html man $(JAVADOC_GENERATED_FILES)
else
docs: pdf html man
endif

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: gifs $(HTML_REF_MAN_FILE)

clean clean_docs:
	rm -rf $(HTMLDIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~

$(JAVADOC_GENERATED_FILES): JAVADOC-GENERATED

JAVADOC-GENERATED: $(JAVA_SOURCE_FILES:%=$(JAVA_SOURCE_DIR)/%)
	@(cd ../../java_src; $(JAVADOC) $(JAVADOCFLAGS) com.ericsson.otp.ic)
	>JAVADOC-GENERATED

man: $(MAN3_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

$(INDEX_TARGET): $(INDEX_SRC) ../../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

debug opt:


# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_docs_spec: docs
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html"
	($(CP) -rf  $(HTMLDIR) "$(RELSYSDIR)/doc")
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3_FILES) "$(RELEASE_PATH)/man/man3"

release_spec:

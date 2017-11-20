#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2017. All Rights Reserved.
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
VSN=$(SSH_VSN)
APPLICATION=ssh

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = ssh.xml \
	ssh_channel.xml \
	ssh_connection.xml \
	ssh_client_key_api.xml \
	ssh_server_key_api.xml \
	ssh_sftp.xml \
	ssh_sftpd.xml \

XML_REF6_FILES = ssh_app.xml

XML_PART_FILES = \
		 usersguide.xml
XML_CHAPTER_FILES = notes.xml \
	introduction.xml \
	using_ssh.xml \
	configure_algos.xml
#	ssh_protocol.xml \

BOOK_FILES = book.xml

XML_FILES = $(BOOK_FILES) $(XML_APPLICATION_FILES) $(XML_REF3_FILES)  $(XML_REF6_FILES)\
            $(XML_PART_FILES) $(XML_CHAPTER_FILES)

IMAGE_FILES = SSH_protocols.png

# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

INFO_FILE = ../../info
EXTRA_FILES = \
	$(DEFAULT_GIF_FILES) \
	$(DEFAULT_HTML_FILES) \
	$(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html) \
        $(XML_REF6_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)


MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
XML_FLAGS +=
DVIPS_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
$(HTMLDIR)/%.png: %.png
	$(INSTALL_DATA) $< $@

docs: pdf html man

$(TOP_PDF_FILE): $(XML_FILES)

images: $(IMAGE_FILES:%=$(HTMLDIR)/%)

pdf: $(TOP_PDF_FILE)

html: images $(HTML_REF_MAN_FILE)


clean clean_docs:
	rm -rf $(HTMLDIR)/*
	rm -f $(MAN3DIR)/*
	rm -f $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f errs core *~

man: $(MAN3_FILES) $(MAN6_FILES)


debug opt:


# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_docs_spec: docs
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(HTMLDIR)/* \
		"$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3DIR)/* "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man6"
	$(INSTALL_DATA) $(MAN6_FILES) "$(RELEASE_PATH)/man/man6"


release_spec:

#-*-makefile-*-   ; force emacs to enter makefile-mode

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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN = $(SNMP_VSN)
APPLICATION=snmp

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MIBSDIR = ../../mibs
include files.mk

# ----------------------------------------------------

HTML_APP_FILES  = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html)
XML_PART_FILE2 = $(XML_PART_FILES) notes_history.xml
HTML_PART_FILES = \
	../html/notes_history.html \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

HTML_FILES = \
	$(HTML_APP_FILES) \
	$(HTML_PART_FILES)

XML_ERRS = $(XML_FILES:%.xml=%.latex.xmls_errs) \
            $(XML_FILES:%.xml=%.html.xmls_errs)

XML_OUTPUT = $(XML_FILES:%.xml=%.latex.xmls_output) \
              $(XML_FILES:%.xml=%.html.xmls_output)

NO_CHUNKS = snmpa_discovery_handler.xml \
	snmpa_error_report.xml \
	snmpa_mib_data.xml \
	snmpa_mib_storage.xml \
	snmpa_network_interface.xml \
	snmpa_network_interface_filter.xml \
	snmpa_notification_delivery_info_receiver.xml \
	snmpa_notification_filter.xml \
	snmpm_network_interface.xml \
	snmpm_network_interface_filter.xml \
	snmpm_user.xml

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(MAN7DIR)/%.7: $(MIBSDIR)/%.mib
	@echo "processing $*"
	@echo ".TH $* 7  \"SNMP\" \"Erlang/OTP\" \"MIB\"" > $@
	@echo ".nf" >> $@
	@cat $< >> $@
	@echo ".fi" >> $@
	@echo "" >> $@

$(MAN1DIR)/snmpc.1: snmpc_cmd.xml
	date=`date +"%B %e %Y"`; \
        xsltproc --output "$@" --stringparam company "Ericsson AB" --stringparam docgen "$(DOCGEN)" --stringparam gendate "$$date" --stringparam appname "$(APPLICATION)" --stringparam appver "$(VSN)" --xinclude -path $(DOCGEN)/priv/dtd  -path $(DOCGEN)/priv/dtd_man_entities $(DOCGEN)/priv/xsl/db_man.xsl $<

# ----------------------------------------------------

include $(ERL_TOP)/make/doc.mk

info: info_xml info_man info_html

info_man:
	@echo "man files:"
	@echo "MAN1_FILES = $(MAN1_FILES)"
	@echo "MAN3_FILES = $(MAN3_FILES)"
	@echo "MAN6_FILES = $(MAN6_FILES)"
	@echo "MAN7_FILES = $(MAN7_FILES)"
	@echo ""
	@echo "MIB_FILES  = $(MIB_FILES)"

info_xml:
	@echo "xml files:"
#	@echo "XML_REF1_FILES        = $(XML_REF1_FILES)"
	@echo "XML_REF3_FILES        = $(XML_REF3_FILES)"
	@echo "XML_REF6_FILES        = $(XML_REF6_FILES)"
	@echo "XML_PART_FILES        = $(XML_PART_FILES)"
	@echo "XML_CHAPTER_FILES     = $(XML_CHAPTER_FILES)"
	@echo "XML_APPLICATION_FILES = $(XML_APPLICATION_FILES)"
	@echo ""
	@echo "BOOK_FILES             = $(BOOK_FILES)"
	@echo ""
	@echo "XML_FILES  = $(XML_FILES)"
	@echo "XML_ERRS   = $(XML_ERRS)"
	@echo "XML_OUTPUT = $(XML_OUTPUT)"

info_html:
	@echo "html files:"
	@echo "DOCDIR = $(DOCDIR)"
	@echo "HTML_REF_MAN_FILE = $(HTML_REF_MAN_FILE)"
	@echo ""
	@echo "HTMLDIR = $(HTMLDIR)"
	@echo "HTML_APP_FILES  = $(HTML_APP_FILES)"
	@echo "HTML_PART_FILES = $(HTML_PART_FILES)"
	@echo "HTML_FILES  = $(HTML_FILES)"
	@echo ""
	@echo "EXTRA_FILES = $(EXTRA_FILES)"
	@echo ""
	@echo "DEFAULT_HTML_FILES = $(DEFAULT_HTML_FILES)"
	@echo ""
#	@echo "HTML_REF1_FILES = $(HTML_REF1_FILES)"
	@echo "HTML_REF3_FILES = $(HTML_REF3_FILES)"
	@echo "HTML_REF6_FILES = $(HTML_REF6_FILES)"
	@echo "HTML_CHAP_FILES = $(HTML_CHAP_FILES)"

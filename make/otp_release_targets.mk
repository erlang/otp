# 
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2013. All Rights Reserved.
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

include $(ERL_TOP)/make/otp_default_release_path.mk

# ----------------------------------------------------
# Targets for the new documentation support
# ----------------------------------------------------

ifneq ($(TOP_SPECS_FILE),)
TOP_SPECS_PARAM = --stringparam specs_file "`pwd`/$(TOP_SPECS_FILE)"
endif

MOD2APP = $(ERL_TOP)/make/$(TARGET)/mod2app.xml
ifneq ($(wildcard $(MOD2APP)),)
MOD2APP_PARAM = --stringparam mod2app_file "$(MOD2APP)"
endif

ifeq ($(TOPDOC),)
$(HTMLDIR)/index.html: $(XML_FILES) $(SPECS_FILES)
	date=`date +"%B %e, %Y"`; \
	$(XSLTPROC) --noout \
          --stringparam outdir $(HTMLDIR) \
          --stringparam docgen "$(DOCGEN)" \
          --stringparam topdocdir "$(TOPDOCDIR)" \
          --stringparam pdfdir "$(PDFDIR)" \
          --xinclude $(TOP_SPECS_PARAM) $(MOD2APP_PARAM) \
          --stringparam gendate "$$date" \
          --stringparam appname "$(APPLICATION)" \
          --stringparam appver "$(VSN)" \
	  --stringparam extra_front_page_info "$(DOC_EXTRA_FRONT_PAGE_INFO)" \
	  --stringparam stylesheet "$(CSS_FILE)" \
	  --stringparam winprefix "$(WINPREFIX)" \
	  --stringparam logo "$(HTMLLOGO_FILE)" \
	  --stringparam pdfname "$(PDFNAME)" \
          -path $(DOCGEN)/priv/dtd \
          -path $(DOCGEN)/priv/dtd_html_entities \
            $(DOCGEN)/priv/xsl/db_html.xsl book.xml

endif

$(HTMLDIR)/users_guide.html: $(XML_FILES)
	date=`date +"%B %e, %Y"`; \
	$(XSLTPROC) --noout  \
		--stringparam outdir  $(HTMLDIR)  \
		--stringparam docgen "$(DOCGEN)"  \
		--stringparam topdocdir "$(TOPDOCDIR)" \
		--stringparam pdfdir "$(PDFDIR)" \
		--stringparam gendate "$$date" \
	        --stringparam appname "$(APPLICATION)" \
	        --stringparam appver "$(VSN)" \
	        --stringparam extra_front_page_info "$(DOC_EXTRA_FRONT_PAGE_INFO)" \
		--stringparam stylesheet "$(CSS_FILE)" \
		--stringparam winprefix "$(WINPREFIX)" \
		--stringparam logo "$(HTMLLOGO_FILE)" \
		--stringparam pdfname "$(PDFNAME)" \
	        --xinclude  \
		-path $(DOCGEN)/priv/dtd \
	        -path $(DOCGEN)/priv/dtd_html_entities \
	        $(DOCGEN)/priv/xsl/db_html.xsl book.xml

%.fo: $(XML_FILES) $(SPECS_FILES)
	date=`date +"%B %e, %Y"`; \
	$(XSLTPROC) \
         --stringparam docgen "$(DOCGEN)" \
         --stringparam gendate "$$date" \
         --stringparam appname "$(APPLICATION)" \
         --stringparam appver "$(VSN)" \
	 --stringparam extra_front_page_info "$(DOC_EXTRA_FRONT_PAGE_INFO)" \
	 --stringparam logo "$(PDFLOGO_FILE)" \
	 --stringparam pdfcolor "$(PDFCOLOR)" \
         --xinclude $(TOP_SPECS_PARAM) \
         -path $(DOCGEN)/priv/dtd \
         -path $(DOCGEN)/priv/dtd_html_entities \
           $(DOCGEN)/priv/xsl/db_pdf.xsl book.xml > $@

# ------------------------------------------------------------------------
# The following targets just exist in the documentation directory
# ------------------------------------------------------------------------
ifneq ($(XML_FILES),)

# ----------------------------------------------------
# Generation of application index data
# ----------------------------------------------------
$(HTMLDIR)/$(APPLICATION).eix: $(XML_FILES) $(SPECS_FILES)
	date=`date +"%B %e, %Y"`; \
	$(XSLTPROC) --stringparam docgen "$(DOCGEN)" \
		--stringparam gendate "$$date" \
	        --stringparam appname "$(APPLICATION)" \
	        --stringparam appver "$(VSN)" \
	        -xinclude $(TOP_SPECS_PARAM)  \
		-path $(DOCGEN)/priv/dtd \
	        -path $(DOCGEN)/priv/dtd_html_entities \
	        $(DOCGEN)/priv/xsl/db_eix.xsl book.xml >  $@ 

docs: $(HTMLDIR)/$(APPLICATION).eix

xmllint: $(XML_FILES)
	@echo "Running xmllint"
	@BookFiles=`awk -F\" '/xi:include/ {print $$2}' book.xml`; \
	for i in $$BookFiles; do \
		if [ $$i = "notes.xml" ]; then \
			echo Checking $$i; \
			xmllint --noout --valid --nodefdtd --loaddtd --path $(DOCGEN)/priv/dtd:$(DOCGEN)/priv/dtd_html_entities $$i; \
		else\
			awk -F\" '/xi:include/ {print "echo Checking " $$2 ;print "xmllint --noout --valid --nodefdtd --loaddtd --path $(DOCGEN)/priv/dtd:$(DOCGEN)/priv/dtd_html_entities:$(XMLLINT_SRCDIRS) " $$2}' $$i |sh; \
		fi \
	done

# ----------------------------------------------------
# Local documentation target for testing 
# ----------------------------------------------------
local_docs: TOPDOCDIR=.
local_docs: local_copy_of_topdefs docs

local_html: TOPDOCDIR=.
local_html: local_copy_of_topdefs html

local_copy_of_topdefs:
	$(INSTALL) $(DOCGEN)/priv/css/otp_doc.css $(HTMLDIR)
	$(INSTALL) $(DOCGEN)/priv/images/erlang-logo.png $(HTMLDIR)
	$(INSTALL) $(DOCGEN)/priv/images/erlang-logo.gif $(HTMLDIR)
	$(INSTALL_DIR) $(HTMLDIR)/js/flipmenu
	$(INSTALL) $(DOCGEN)/priv/js/flipmenu/flip_closed.gif \
	 	$(DOCGEN)/priv/js/flipmenu/flip_open.gif \
		$(DOCGEN)/priv/js/flipmenu/flip_static.gif \
		$(DOCGEN)/priv/js/flipmenu/flipmenu.js $(HTMLDIR)/js/flipmenu

endif

# ----------------------------------------------------
# Standard release target
# ----------------------------------------------------

ifeq ($(TESTROOT),)

release release_docs release_tests release_html:
	$(MAKE) $(MFLAGS) RELEASE_PATH=$(OTP_DEFAULT_RELEASE_PATH) \
		$(TARGET_MAKEFILE)  $@_spec

else

release release_docs release_tests release_html:
	$(MAKE) $(MFLAGS) RELEASE_PATH="$(TESTROOT)" $(TARGET_MAKEFILE)  $@_spec 

endif

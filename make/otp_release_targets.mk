# 
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2013. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
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
	$(XMLLINT) --noout --valid --nodefdtd --loaddtd --path $(DOCGEN)/priv/dtd:$(DOCGEN)/priv/dtd_html_entities  $(XML_FILES)

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

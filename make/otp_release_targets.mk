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

# -------------------------------------------------------
# Take the XML files and add the github link info to them
# -------------------------------------------------------
ifneq ($(strip $(XMLDIR)),)
_create_xml_dirs := $(shell mkdir -p $(XMLDIR))
endif

XML_GEN_FILES+=$(patsubst %.xml,$(XMLDIR)/%.xml,$(XML_FILES))

ifeq ($(strip $(NO_GITHUB_DOC_LINKS)),)
$(XMLDIR)/%.xml: %.xml
	$(gen_verbose)escript $(DOCGEN)/priv/bin/github_link.escript $< \
	"$(subst $(ERL_TOP)/,,$(CURDIR)/$^)" "NA" $@

$(XMLDIR)/%.xmlsrc: %.xmlsrc
	$(gen_verbose)escript $(DOCGEN)/priv/bin/github_link.escript $< \
	"$(subst $(ERL_TOP)/,,$(CURDIR)/$^)" "NA" $@
else
## Just copy the files if the application does not want github edit links
$(XMLDIR)/%.xml: %.xml
	$(gen_verbose)$(CP) $< $@
$(XMLDIR)/%.xmlsrc: %.xmlsrc
	$(gen_verbose)$(CP) $< $@
endif

ifeq ($(TOPDOC),)

$(HTMLDIR)/index.html: $(XML_GEN_FILES) $(SPECS_FILES)
	$(gen_verbose)date=`date +"%B %e, %Y"`; \
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
          -path . \
          -path $(DOCGEN)/priv/dtd \
          -path $(DOCGEN)/priv/dtd_html_entities \
            $(DOCGEN)/priv/xsl/db_html.xsl $(XMLDIR)/book.xml

endif

$(HTMLDIR)/users_guide.html: $(XML_GEN_FILES)
	$(gen_verbose)date=`date +"%B %e, %Y"`; \
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
		-path . \
		-path $(DOCGEN)/priv/dtd \
	        -path $(DOCGEN)/priv/dtd_html_entities \
	        $(DOCGEN)/priv/xsl/db_html.xsl $(XMLDIR)/book.xml

%.fo: $(XML_GEN_FILES) $(SPECS_FILES)
	$(gen_verbose)date=`date +"%B %e, %Y"`; \
	$(XSLTPROC) \
         --stringparam docgen "$(DOCGEN)" \
         --stringparam gendate "$$date" \
         --stringparam appname "$(APPLICATION)" \
         --stringparam appver "$(VSN)" \
	 --stringparam extra_front_page_info "$(DOC_EXTRA_FRONT_PAGE_INFO)" \
	 --stringparam logo "$(PDFLOGO_FILE)" \
	 --stringparam pdfcolor "$(PDFCOLOR)" \
         --xinclude $(TOP_SPECS_PARAM) \
	 -path . \
         -path $(DOCGEN)/priv/dtd \
         -path $(DOCGEN)/priv/dtd_html_entities \
           $(DOCGEN)/priv/xsl/db_pdf.xsl $(XMLDIR)/book.xml > $@

# ------------------------------------------------------------------------
# The following targets just exist in the documentation directory
# ------------------------------------------------------------------------
.PHONY: xmllint

ifneq ($(XML_FILES),)

# ----------------------------------------------------
# Generation of application index data
# ----------------------------------------------------
$(HTMLDIR)/$(APPLICATION).eix: $(XML_GEN_FILES) $(SPECS_FILES)
	$(gen_verbose)date=`date +"%B %e, %Y"`; \
	$(XSLTPROC) --stringparam docgen "$(DOCGEN)" \
		--stringparam gendate "$$date" \
	        --stringparam appname "$(APPLICATION)" \
	        --stringparam appver "$(VSN)" \
	        -xinclude $(TOP_SPECS_PARAM)  \
	        -path . \
		-path $(DOCGEN)/priv/dtd \
	        -path $(DOCGEN)/priv/dtd_html_entities \
	        $(DOCGEN)/priv/xsl/db_eix.xsl $(XMLDIR)/book.xml >  $@

docs: $(HTMLDIR)/$(APPLICATION).eix

## Here awk is used to find all xi:include files in $(BOOK_FILES)
## Then we look into all those files check for xi:includes
BOOK_XI_INC_FILES:=$(foreach file,$(BOOK_FILES),$(shell awk -F\" '/xi:include/ {print $$2}' $(file))) $(BOOK_FILES)
ALL_XI_INC_FILES:=$(foreach file,$(BOOK_XI_INC_FILES),$(shell if [ -f $(file) ]; then awk -F\" '/xi:include/ {if ("$(dir $(file))" != "./") printf "$(dir $(file))"; print $$2}' $(file) ; fi)) $(BOOK_XI_INC_FILES)
ifeq ($(TOPDOC), true)
ALL_XI_INC_GEN_FILES:=$(filter-out book.xml,$(ALL_XI_INC_FILES)) $(BOOK_FILES:%=$(XMLDIR)/%)
else
ALL_XI_INC_GEN_FILES:=$(ALL_XI_INC_FILES:%=$(XMLDIR)/%)
endif


## These are the patterns of file names that xmllint cannot currently parse
XI_INC_FILES:=%user_man.xml %usersguide.xml %refman.xml %ref_man.xml %part.xml %book.xml %internal.xml

## These are the files that we should run the xmllint on
LINT_XI_INC_FILES := $(filter-out $(XI_INC_FILES), $(ALL_XI_INC_FILES))
LINT_XI_INC_GEN_FILES := $(filter-out $(XI_INC_FILES), $(ALL_XI_INC_GEN_FILES))

EMPTY :=
SPACE := $(EMPTY) $(EMPTY)
XMLLINT_SRCDIRS:=$(subst $(SPACE),:,$(sort $(foreach file,$(XML_FILES),$(dir $(file)))))

xmllint: $(ALL_XI_INC_GEN_FILES)
## We verify that the $(XML_GEN_FILES) variable in the Makefile have exactly
## the same files as we found out by following xi:include.
ifneq ($(filter-out $(filter %.xml,$(XML_GEN_FILES)),$(ALL_XI_INC_GEN_FILES)),)
	$(error "$(filter-out $(filter %.xml,$(XML_GEN_FILES)),$(ALL_XI_INC_GEN_FILES)) in $$ALL_XI_INC_FILES but not in $$XML_GEN_FILES");
endif
ifneq ($(filter-out $(ALL_XI_INC_GEN_FILES),$(filter %.xml,$(XML_GEN_FILES))),)
	$(error "$(filter-out $(ALL_XI_INC_GEN_FILES),$(filter %.xml,$(XML_GEN_FILES))) in $$XML_GEN_FILES but not in $$ALL_XI_INC_FILES");
endif
	@echo "xmllint $(LINT_XI_INC_GEN_FILES)"
	@xmllint --noout --valid --nodefdtd --loaddtd --path \
	$(DOCGEN)/priv/dtd:$(DOCGEN)/priv/dtd_html_entities:$(XMLLINT_SRCDIRS) \
	$(LINT_XI_INC_GEN_FILES)

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

else
xmllint:
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

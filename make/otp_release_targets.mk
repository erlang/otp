# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$

# ----------------------------------------------------
# Target for building only the files needed by the Book generation
# ----------------------------------------------------
#texmake: $(TEX_FILES) $(PSFIG_FILES)

# ----------------------------------------------------
# Targets for the new documentation support
# ----------------------------------------------------

ifeq ($(TOPDOC),)
$(HTMLDIR)/index.html: $(XML_FILES)
	date=`date +"%B %e %Y"`; \
	$(XSLTPROC) --noout --stringparam outdir $(HTMLDIR) --stringparam docgen "$(DOCGEN)" --stringparam topdocdir "$(TOPDOCDIR)" \
		--stringparam pdfdir "$(PDFDIR)" \
		--stringparam gendate "$$date" --stringparam appname "$(APPLICATION)" --stringparam appver "$(VSN)" --xinclude \
		-path $(DOCGEN)/priv/docbuilder_dtd -path $(DOCGEN)/priv/dtd_html_entities $(DOCGEN)/priv/xsl/db_html.xsl book.xml
endif

$(HTMLDIR)/users_guide.html: $(XML_FILES)
	date=`date +"%B %e %Y"`; \
	$(XSLTPROC) --noout --stringparam outdir $(HTMLDIR) --stringparam docgen "$(DOCGEN)" --stringparam topdocdir "$(TOPDOCDIR)" \
		--stringparam pdfdir "$(PDFDIR)" \
		--stringparam gendate "$$date" --stringparam appname "$(APPLICATION)" --stringparam appver "$(VSN)" --xinclude  \
		-path $(DOCGEN)/priv/docbuilder_dtd -path $(DOCGEN)/priv/dtd_html_entities $(DOCGEN)/priv/xsl/db_html.xsl book.xml


%.fo: $(XML_FILES)
	date=`date +"%B %e %Y"`; \
	$(XSLTPROC) --stringparam docgen "$(DOCGEN)" --stringparam gendate "$$date" --stringparam appname "$(APPLICATION)" \
		--stringparam appver "$(VSN)" --xinclude \
		-path $(DOCGEN)/priv/docbuilder_dtd -path $(DOCGEN)/priv/dtd_html_entities $(DOCGEN)/priv/xsl/db_pdf.xsl book.xml > $@


local_docs: TOPDOCDIR=.
local_docs: docs
	$(INSTALL) $(DOCGEN)/priv/css/otp_doc.css $(HTMLDIR)
	$(INSTALL) $(DOCGEN)/priv/images/erlang-logo.png $(HTMLDIR)
	$(INSTALL) $(DOCGEN)/priv/images/erlang-logo.gif $(HTMLDIR)
	$(INSTALL_DIR) $(HTMLDIR)/js/flipmenu
	$(INSTALL) $(DOCGEN)/priv/js/flipmenu/flip_closed.gif \
	 	$(DOCGEN)/priv/js/flipmenu/flip_open.gif \
		$(DOCGEN)/priv/js/flipmenu/flip_static.gif \
		$(DOCGEN)/priv/js/flipmenu/flipmenu.js $(HTMLDIR)/js/flipmenu


# ----------------------------------------------------
# Standard release target
# ----------------------------------------------------

ifeq ($(TESTROOT),)

release release_docs release_tests release_html:
	$(MAKE) $(MFLAGS) RELEASE_PATH=$(ERL_TOP)/release/$(TARGET) \
		$(TARGET_MAKEFILE)  $@_spec

else

release release_docs release_tests release_html:
	$(MAKE) $(MFLAGS) RELEASE_PATH=$(TESTROOT) $(TARGET_MAKEFILE)  $@_spec

endif

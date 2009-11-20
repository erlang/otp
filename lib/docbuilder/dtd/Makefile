# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999-2000, Ericsson 
# Utvecklings AB. All Rights Reserved.''
# 
#     $Id$
#

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(DOCB_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/docbuilder-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

DTD_FILES = \
	application.dtd \
	appref.dtd \
	book.dtd \
	bookinsidecover.dtd \
	chapter.dtd \
	cites.dtd \
	common.dtd \
	common.entities.dtd \
	common.header.dtd \
	common.image.dtd \
	common.refs.dtd \
	common.table.dtd \
	comref.dtd \
	cref.dtd \
	erlref.dtd \
	fascicules.dtd \
	fileref.dtd \
	part.dtd \
	report.dtd \
	terms.dtd \
	xhtml-lat1.ent \
	xhtml-special.ent \
	xhtml-symbol.ent \
	xhtml1-frameset.dtd \
	xhtml1-strict.dtd \
	xhtml1-transitional.dtd

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: 

clean:

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec:
	$(INSTALL_DIR) $(RELSYSDIR)/dtd
	$(INSTALL_DATA) $(DTD_FILES) $(RELSYSDIR)/dtd

release_docs_spec:








#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2012. All Rights Reserved.
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

#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(GS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/gs-$(VSN)

ERL = erl

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES=    	gs gs_frontend gs_make gs_widgets gstk gstk_arc gstk_button\
                gstk_canvas gstk_checkbutton gstk_db gstk_editor gstk_entry \
                gstk_font gstk_frame gstk_grid gstk_gridline gs_packer \
                gstk_gs gstk_image gstk_label gstk_line gstk_listbox gstk_menu\
                gstk_menubar gstk_menubutton gstk_menuitem gstk_oval gstk_polygon \
                gstk_port_handler gstk_radiobutton gstk_rectangle gstk_scale \
                gstk_text gstk_widgets gstk_window tcl2erl tool_utils \
		tool_file_dialog gse

GSTK_GENERIC = gstk_generic.erl

HRL_FILES = gstk.hrl
GEN_HRL_FILES = gstk_generic.hrl
GSTK_GENERIC_TARGET = $(EBIN)/gstk_generic.$(EMULATOR)

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR)) $(GEN_HRL_FILES) \
	$(GSTK_GENERIC_TARGET) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= gs.app
APPUP_FILE= gs.appup

APP_SRC= $(APP_FILE).src
APPUP_SRC= $(APPUP_FILE).src

APP_TARGET= ../ebin/$(APP_FILE)
APPUP_TARGET= ../ebin/$(APPUP_FILE)

IMAGES=../priv/bitmap/fup.bm


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += +warn_obsolete_guard

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)
	rm -f core *~

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

gstk_generic.hrl: gs_make.erl ../ebin/gs_make.$(EMULATOR) ../ebin/gs.$(EMULATOR)
	$(gen_verbose)$(ERL) -pa $(EBIN) -s gs_make -s erlang halt -noshell

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(GSTK_GENERIC_TARGET): gstk_generic.hrl

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(APP_SRC) $(ERL_FILES) $(HRL_FILES) $(GEN_HRL_FILES) \
		$(GSTK_GENERIC) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/bitmap"
	$(INSTALL_DATA) $(IMAGES) "$(RELSYSDIR)/priv/bitmap"


release_docs_spec:


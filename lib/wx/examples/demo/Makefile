#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2009-2012. All Rights Reserved.
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

include ../../vsn.mk
include ../../config.mk

TOPDIR   = ../..
SRC = .
BIN = .
ERLINC = $(TOPDIR)/include
ERLC = erlc
TESTMODS = \
	demo \
	demo_html_tagger \
	ex_aui \
	ex_button \
	ex_canvas \
	ex_canvas_paint \
	ex_choices \
	ex_cursor \
	ex_dialogs \
	ex_frame_utils \
	ex_gauge \
	ex_gl \
	ex_grid \
	ex_htmlWindow \
	ex_listCtrl \
	ex_notebook \
	ex_pickers \
	ex_popupMenu \
	ex_radioBox \
	ex_sashWindow \
	ex_sizers \
	ex_slider \
	ex_splitterWindow \
	ex_static \
	ex_textCtrl \
	ex_treeCtrl \
	ex_graphicsContext


TESTTARGETS = $(TESTMODS:%=%.beam)
TESTSRC = $(TESTMODS:%=%.erl)

# Targets
opt debug:	$(TESTTARGETS)
clean:
	rm -f $(TESTTARGETS)
	rm -f *~ core erl_crash.dump

docs:

run: opt
	erl -smp -detached -pa $(TOPDIR)/ebin -s demo

EXRELSYSDIR = "$(RELSYSDIR)/examples/demo"
include $(ERL_TOP)/make/otp_release_targets.mk

docs:

release_spec: opt
	$(INSTALL_DIR) $(EXRELSYSDIR)
	$(INSTALL_DATA) $(TESTSRC)  $(EXRELSYSDIR)
	$(INSTALL_DATA) $(TESTTARGETS) $(EXRELSYSDIR)
	$(INSTALL_DATA) image.jpg erlang.png ex_htmlWindow.html $(EXRELSYSDIR)

release_tests_spec:

release_docs_spec:

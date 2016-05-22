#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

include ../../vsn.mk
include ../../config.mk

TOPDIR = ../..
EBIN = .
ERLINC = $(TOPDIR)/include
ERLC = erlc
TESTMODS = xrc
TESTTARGETS = $(TESTMODS:%=%.beam)
TESTSRC = $(TESTMODS:%=%.erl)

RESOURCEFILES = \
	appicon.ico basicdlg.xpm custclas.xpm fileopen.gif menu.xrc \
	resource.xrc uncenter.xpm  variable.xrc appicon.xpm basicdlg.xrc \
	custclas.xrc filesave.gif platform.xpm stop.xpm uncenter.xrc \
	artprov.xpm controls.xpm derivdlg.xpm frame.xrc platform.xrc \
	throbber.gif update.gif artprov.xrc controls.xrc derivdlg.xrc \
	fuzzy.gif quotes.gif toolbar.xrc variable.xpm

TESTRESOURCE = $(RESOURCEFILES:%=rc/%)

# Targets
opt debug:	$(TESTTARGETS)
clean:
	rm -f $(TESTTARGETS)
	rm -f *~ core erl_crash.dump

docs: 

run: opt
	erl -smp -detached -pa $(TOPDIR)/ebin -s xrc 

EXRELSYSDIR = "$(RELSYSDIR)/examples/xrc"
include $(ERL_TOP)/make/otp_release_targets.mk

docs: 

release_spec: opt
	$(INSTALL_DIR) $(EXRELSYSDIR)
	$(INSTALL_DATA) $(TESTSRC) $(EXRELSYSDIR)
	$(INSTALL_DATA) $(TESTTARGETS) $(EXRELSYSDIR)
	$(INSTALL_DIR) $(EXRELSYSDIR)/rc
	$(INSTALL_DATA) $(TESTRESOURCE) $(EXRELSYSDIR)/rc

release_tests_spec:

release_docs_spec:




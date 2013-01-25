#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 1999-2013. All Rights Reserved.
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

.SUFFIXES: .erl .jam .beam .yrl .hrl .sgml .html .so .c .flex .flex.src


CC     = gcc
CFLAGS = -g -O2 -funroll-loops -Wall -fPIC
FLEX   = flex
PERL   = perl

# ----------------------------------------------------
#	Erlang language section
# ----------------------------------------------------
EMULATOR = beam
ERLC_WFLAGS = -W
ERLC = erlc $(ERLC_WFLAGS) $(ERLC_FLAGS)
ERL.beam =  erl.beam -boot start_clean
ERL.jam = erl -boot start_clean
ERL = $(ERL.$(EMULATOR))

ifndef EBIN
EBIN = ../ebin
endif

ifndef ESRC
ESRC = .
endif

$(EBIN)/%.jam: $(ESRC)/%.erl
	$(ERLC) -bjam $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) -bbeam $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

.erl.jam:
	$(ERLC) -bjam $(ERL_COMPILE_FLAGS) -o$(dir $@) $<

.erl.beam:
	$(ERLC) -bbeam $(ERL_COMPILE_FLAGS) -o$(dir $@) $<

#
# When .erl files are automatically created GNU make removes them if
# they were the result of a chain of implicit rules. To prevent this
# we say that all .erl files are "precious".
#
.PRECIOUS: %.erl

## Uncomment these lines and add .idl to suffixes above to have erlc 
## eat IDL files
##.idl.erl:
##      $(ERLC) $(IDL_FLAGS) $<

.yrl.erl:
	$(ERLC) $(YRL_FLAGS) $<

.xrl.erl:
	$(ERLC) $(XRL_FLAGS) $<

##  generating app files

$(EBIN)/%.app: $(ESRC)/%.app.src $(VSN_FILE)
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(EBIN)/%.appup: $(ESRC)/%.appup.src $(VSN_FILE)
	sed -e 's;%VSN%;$(VSN);' $< > $@


.c.so:
	$(CC) $(CFLAGS) -fpic -shared -o $*.so $< -lfl


# ----------------------------------------------------
#       Command macros
# ----------------------------------------------------
INSTALL = /usr/ucb/install -c
INSTALL_DIR = /usr/ucb/install -c -d
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644


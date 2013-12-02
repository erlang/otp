#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2013. All Rights Reserved.
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
# Build example dictionaries. Assumes erlc and diameterc are on PATH.
#

DICTS = rfc4004_mip \
        rfc4005_nas \
        rfc4006_cc \
        rfc4072_eap \
        rfc4590_digest \
        rfc4740_sip

FILES = $(DICTS:%=%.dia)
BEAMS = $(DICTS:%=%.beam)

COMMON = diameter_gen_base_rfc6733

%.erl: %.dia
	diameterc --name $(basename $@) \
	          --prefix $(basename $@) \
	          --inherits common/$(COMMON) \
	  $<

%.beam: %.erl
	erlc -Wall +debug_info $<

all: $(BEAMS)

clean:
	rm -f $(DICTS:%=%.erl) $(DICTS:%=%.hrl) $(BEAMS) depend.mk

-include depend.mk

depend.mk: depend.sed $(FILES) GNUmakefile
	(for f in $(FILES); do \
	     (echo $$f; cat $$f) | sed -f depend.sed; \
	 done) \
	> $@

.PHONY: all clean

.SECONDARY: $(DICTS:%=%.erl)

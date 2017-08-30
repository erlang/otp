#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

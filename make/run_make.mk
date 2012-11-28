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
#
# ----------------------------------------------------
# This make include file runs make recursively on the Makefile
# in the architecture dependent subdirectory.
#
# Typical use from Makefile:
#
#    include $(ERL_TOP)/make/run_make.mk
#
# ----------------------------------------------------

include $(ERL_TOP)/make/output.mk
include $(ERL_TOP)/make/target.mk

.PHONY: valgrind

opt debug purify quantify purecov valgrind gcov gprof lcnt:
	$(make_verbose)$(MAKE) -f $(TARGET)/Makefile TYPE=$@

plain smp frag smp_frag:
	$(make_verbose)$(MAKE) -f $(TARGET)/Makefile FLAVOR=$@

clean generate depend docs release release_spec release_docs release_docs_spec \
  tests release_tests release_tests_spec:
	$(make_verbose)$(MAKE) -f $(TARGET)/Makefile $@





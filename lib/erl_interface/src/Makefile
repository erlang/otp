# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2009. All Rights Reserved.
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

# Invoke with GNU make or clearmake -C gnu.
#

# FIXME let configure put in this last part TARGET

include $(ERL_TOP)/make/output.mk
include $(ERL_TOP)/make/target.mk

debug opt shared purify quantify purecov gcov:
	$(make_verbose)$(MAKE) -f $(TARGET)/Makefile TYPE=$@

clean depend docs release release_docs tests release_tests check:
	$(make_verbose)$(MAKE) -f $(TARGET)/Makefile $@

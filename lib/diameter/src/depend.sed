#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
# Extract include dependencies from .erl files. First line of input
# is the path to the module in question (minus the .erl extension),
# the rest is the contents of the module.
#

1{
  s@^[^/]*/@@
  h
  d
}

# Only interested in includes of diameter hrls.
/^-include/!d
/"diameter/!d

# Extract the name of the included files in one of two forms:
#
#   $(INCDIR)/diameter.hrl
#   diameter_internal.hrl

s@^-include_lib(".*/@$(INCDIR)/@
s@^-include("@@
s@".*@@

# Retrieve the path to our module from the hold space, morph it
# into a beam path and turn it into a dependency like this:
#
#   $(EBIN)/diameter_service.$(EMULATOR): $(INCDIR)/diameter.hrl

G
s@^\(.*\)\n\(.*\)@$(EBIN)/\2.$(EMULATOR): \1@

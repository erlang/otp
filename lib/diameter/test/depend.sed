#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2013. All Rights Reserved.
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
# Extract local include dependencies from an .erl file. The first
# input line is the module name.
#

# Store the module name in the hold space.
1{
  h
  d
}

# Throw away everything but local includes.
/^-include_lib/d
/^-include/!d
/diameter_gen/d
/diameter\./d

# Output a dependency of the beam on the included file.
s@^-include("@@
s@".*@@
G
s@^\(.*\)\n\(.*\)@\2.$(EMULATOR): \1@

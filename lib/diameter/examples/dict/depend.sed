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
# Extract dependencies from .dia files. First line of input is the
# dictionary's filename, the rest is its contents.
#

1{
  s@\.[^.]*$@@
  h
  d
}

# Only interested in @inherits.
/^@inherits  */!d

s///
s/ .*//

# Ignore the common application.
/^common$/d

# Retrieve the dictionary name from the hold space and output
# a dependency.
G
s@^\(.*\)\n\(.*\)@\2.erl: \1.beam@

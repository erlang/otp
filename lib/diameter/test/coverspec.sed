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
# Morph diameter.cover into a legitimate cover spec. All that's being
# retained is the list of excluded modules. This is used by Makefile
# when running cover locally.
#

/^{incl_app,/{
  i\
{level, details}.\
{incl_dirs, ["../ebin"]}.
  d
}

/^{excl_mods,/s@ .*@@

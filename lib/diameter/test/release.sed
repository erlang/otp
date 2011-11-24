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
# This bit of gymnastics is to replace the include of diameter's
# public hrls by include_lib when releasing testsuites, so that they
# compile both in the development filesystem (where generated hrls
# aren't in diameter/include) and with common_test's autocompilation
# on an installed release. Solving the problem by installing generated
# hrls to ../include is anathema: that directory is for handwritten
# source.)
#

/^-include("/!b
/"diameter_gen_/b s
/"diameter\./!b

:s
s@("@_lib&diameter/include/@

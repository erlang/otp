#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2014. All Rights Reserved.
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
# Generate runtime_dependencies from applications to avoid having to
# specify the same application more than once.
#

/{runtime_dependencies,/b v
/{[-a-z]*, "[0-9.]*"}/!b
/{vsn,/b

/%%/!H
s/{\([^,]*\)[^}]*}/\1/g
s/%%/%,/
b

:v

p
x
s/\n//
s/%//g
s/\n */ /g
s/{\([^,]*\), "\([^"]*"\)}/"\1-\2/g

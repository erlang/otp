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

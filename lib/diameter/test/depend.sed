#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

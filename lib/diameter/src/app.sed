#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

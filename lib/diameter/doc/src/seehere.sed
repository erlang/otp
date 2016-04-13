#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
# Map entities for inter-document references to ones for
# intra-document references like this:
#
#   <!ENTITY aaa_xxx '<seealso marker="bbb#yyy">ccc:zzz</seealso>'>
#
#   ===>
#
#   <!ENTITY xxx '<seealso marker="#yyy">zzz</seealso>'>
#

/<!ENTITY/!d
/#/!d
/"#/d
s@ [^_]*_@ @
s@"[^#]*#@"#@
s@>[^:]*:@>@

#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2011-2016. All Rights Reserved.
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
# Generate dependencies for html output. The output is further
# massaged in Makefile.
#

/^<com>/b c
/^<module>/b c

/^<chapter>/!d

# Chapter: html basename is same as xml.
s@@$(HTMLDIR)/%FILE%.html: %FILE%.xml@
q

# Reference: html basename is from contents of com/module element.
:c
s@^[^>]*>@@
s@<.*@@
s@.*@$(HTMLDIR)/&.html: %FILE%.xml@
q

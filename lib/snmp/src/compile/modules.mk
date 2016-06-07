#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

PARSER_SRC = snmpc_mib_gram.yrl

PARSER_MODULE = $(PARSER_SRC:%.yrl=%)

ESCRIPT_SRC = \
	snmpc.src

MODULES = \
	$(PARSER_MODULE) \
	snmpc \
	snmpc_lib \
	snmpc_mib_to_hrl \
	snmpc_misc \
	snmpc_tok


INTERNAL_HRL_FILES = \
	snmpc.hrl \
	snmpc_lib.hrl \
	snmpc_misc.hrl

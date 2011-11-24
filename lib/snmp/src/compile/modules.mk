#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2004-2011. All Rights Reserved.
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

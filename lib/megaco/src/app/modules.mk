#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2009. All Rights Reserved.
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

MODULES = \
	megaco

EXTERNAL_HRL_FILES = \
        ../../include/megaco.hrl \
        ../../include/megaco_message_v1.hrl \
        ../../include/megaco_message_v2.hrl \
        ../../include/megaco_message_prev3a.hrl \
        ../../include/megaco_message_prev3b.hrl \
        ../../include/megaco_message_prev3c.hrl \
        ../../include/megaco_message_v3.hrl \
        ../../include/megaco_sdp.hrl

INTERNAL_HRL_FILES = \
	megaco_internal.hrl



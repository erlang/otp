#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2010. All Rights Reserved.
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

SYS_CONFIG_FILES = \
	sys.config \
	sys-agent.config \
	sys-manager.config

AGENT_CONFIG_FILES = \
	agent/agent.conf \
	agent/community.conf \
	agent/context.conf \
	agent/notify.conf \
	agent/standard.conf \
	agent/target_addr.conf \
	agent/target_params.conf \
	agent/usm.conf \
	agent/vacm.conf

MANAGER_CONFIG_FILES = \
	manager/manager.conf \
	manager/usm.conf

MODULES = \
	snmp_test_config

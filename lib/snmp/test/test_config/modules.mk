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

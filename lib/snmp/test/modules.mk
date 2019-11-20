#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2019. All Rights Reserved.
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

SUITE_MODULES = \
	snmp_agent_SUITE \
	snmp_agent_conf_SUITE \
	snmp_agent_mibs_SUITE \
	snmp_agent_nfilter_SUITE \
	snmp_app_SUITE \
	snmp_compiler_SUITE \
	snmp_conf_SUITE \
	snmp_log_SUITE \
	snmp_manager_config_SUITE \
	snmp_manager_user_SUITE \
	snmp_manager_SUITE \
	snmp_note_store_SUITE \
	snmp_pdus_SUITE \
        snmp_to_snmpnet_SUITE

TEST_UTIL_MODULES = \
	snmp_agent_test_get \
	snmp_agent_test_lib \
	snmp_manager_user \
	snmp_manager_user_old \
	snmp_manager_user_test_lib \
	snmp_test_global_sys_monitor \
	snmp_test_sys_monitor \
	snmp_test_lib \
	snmp_test_manager \
	snmp_test_mgr \
	snmp_test_mgr_misc \
	snmp_test_mgr_counter_server \
	sa \
	klas3 \
	test1 \
	test2

TEST_SERVER_MODULES = \
	snmp_test_server \
	snmp_test_suite

MODULES = \
	$(TEST_UTIL_MODULES) \
	$(SUITE_MODULES)

HRL_FILES = snmp_test_lib.hrl

# These are MIBs that aure used by the compiler test-suite.
COMPILER_MIB_FILES = \
	OTP8574-MIB

MIB_FILES = \
	AC-TEST-MIB.mib \
	MC-TEST-MIB.mib \
	OLD-SNMPEA-MIB.mib \
	OLD-SNMPEA-MIB-v2.mib \
	Klas1.mib \
	Klas1-v2.mib \
	Klas2.mib \
	Klas3.mib \
	Klas4.mib \
	SA-MIB.mib \
	EX1-MIB.mib \
	TestTrap.mib \
	TestTrapv2.mib \
	Test1.mib \
	Test2.mib \
	Test3.mib

SPECS = snmp.spec


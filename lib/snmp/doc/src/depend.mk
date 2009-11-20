#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2004-2009. All Rights Reserved.
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

$(HTMLDIR)/part_notes.html: \
	part_notes_history.xml \
	part_notes.xml \
	notes_history.xml \
	notes.xml

$(HTMLDIR)/part.html: \
	part.xml \
	snmp_intro.xml \
	snmp_agent_funct_descr.xml \
	snmp_manager_funct_descr.xml \
	snmp_mib_compiler.xml \
	snmp_config.xml \
	snmp_agent_config_files.xml \
	snmp_manager_config_files.xml \
	snmp_impl_example_agent.xml \
	snmp_impl_example_manager.xml \
	snmp_instr_functions.xml \
	snmp_def_instr_functions.xml \
	snmp_agent_netif.xml \
	snmp_manager_netif.xml \
	snmp_audit_trail_log.xml \
	snmp_advanced_agent.xml \
	snmp_app_a.xml \
	snmp_app_b.xml

$(HTMLDIR)/ref_man.html: \
	ref_man.xml \
	snmp_app.xml \
	snmp.xml \
	snmpc.xml \
	snmpa.xml \
	snmpa_conf.xml \
	snmpa_discovery_handler.xml \
	snmpa_error_report.xml \
	snmpa_error.xml \
	snmpa_error_io.xml \
	snmpa_error_logger.xml \
	snmpa_local_db.xml \
	snmpa_mpd.xml \
	snmpa_network_interface.xml \
	snmpa_network_interface_filter.xml \
	snmpa_notification_delivery_info_receiver.xml \
	snmpa_notification_filter.xml \
	snmpa_supervisor.xml \
	snmp_community_mib.xml \
	snmp_framework_mib.xml \
	snmp_generic.xml \
	snmp_index.xml \
	snmp_notification_mib.xml \
	snmp_pdus.xml \
	snmp_standard_mib.xml \
	snmp_target_mib.xml \
	snmp_user_based_sm_mib.xml \
	snmp_view_based_acm_mib.xml \
	snmpm.xml \
	snmpm_conf.xml \
	snmpm_mpd.xml \
	snmpm_network_interface.xml \
	snmpm_network_interface_filter.xml \
	snmpm_user.xml



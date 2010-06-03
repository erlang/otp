%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-define(attr_table,ct_attributes).
-define(conn_table,ct_connections).
-define(board_table,ct_boards).
-define(suite_table,ct_suite_data).

-record(conn, {handle,
	       targetref,
	       address,
	       callback}).

-record(testspec, {spec_dir,
	           nodes=[],
		   init=[],
		   logdir=["."],
		   cover=[],
		   config=[],
		   userconfig=[],
		   event_handler=[],
		   include=[],
		   multiply_timetraps=[],
		   scale_timetraps=[],
		   alias=[],
		   tests=[]}).

-record(cover, {app=none,
		level=details,
		excl_mods=[],
		incl_mods=[],
		cross=[],
		src=[]}).

-define(CT_EVMGR, ct_event).
-define(CT_EVMGR_REF, ct_event).
-define(CT_MEVMGR, ct_master_event).
-define(CT_MEVMGR_REF, ct_master_event).

-define(missing_suites_info, "missing_suites.info").
-define(ct_config_txt, ct_config_plain).

%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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

-define(ERROR_MSG_MODE, normal).

-define(WIN_WIDTH, 745).  % 779
-define(WIN_HEIGHT, 380).
-define(MIN_WIN_WIDTH, 524).
-define(MIN_WIN_HEIGHT, 150).

-define(FONT, {screen, 12}).
-define(HEADER_FONT, {screen, [bold,italic], 12}).

-define(GRID_XPOS, 3).
-define(GRID_YPOS, 68).


%% Unreadable tables are indicated by the background color.
%% Unnamed tables are indicated by the foreground color.

-define(NORMAL_FG_COLOR, {0,0,0}).
-define(READABLE_BG_COLOR, {255,255,255}).
-define(UNREADABLE_FG_COLOR, ?NORMAL_FG_COLOR).
-define(UNREADABLE_BG_COLOR, {240,240,240}).
%-define(UNREADABLE_BG_COLOR, {255,250,230}).
%-define(UNREADABLE_BG_COLOR, {242,242,242}).
-define(UNNAMED_FG_COLOR, {175,175,175}).
%-define(UNNAMED_FG_COLOR, {140,35,35}).


-define(DISABLED_COLOR, {160,160,160}).

-define(NAME_ELEM, 1).
-define(NAMED_TABLE_ELEM, 2).
-define(ID_ELEM, 3).
-define(READABLE_ELEM, 4).
-define(PID_ELEM, 5).
-define(PROCNAME_ELEM, 6).
-define(INFO_ELEM, 7).

-define(NAME_COL, 1).
-define(ID_COL, 2).
-define(PID_COL, 3).
-define(PROCNAME_COL, 4).
-define(INFO_COL, 5).

-define(POSSIBLE_MARK_COLS, [?NAME_COL, ?ID_COL, ?PID_COL, ?PROCNAME_COL, ?INFO_COL]).
-define(COL_WIDTHS, [205,131,91,197,90]).   % [140,95,125,75,85,140,90]).
-define(FIX_WIDTH_COLS, [2,3,5]).


-define(HEADER_LABELS, [{label1, " Table Name"}, 
			% {label2, " Named Table"}, 
			{label2, " Table Id"}, 
			% {label4, " Readable"}, 
			{label3, " Owner Pid"}, 
			{label4, " Owner Name"}, 
			{label5, " Table Size"}
		       ]).



%% TABLES_TO_HIDE shall contain both Mnesia and ETS tables that we want to hide.  :-)

-define(SYSTEM_TABLES, [ac_tab,
			asn1,
			cdv_dump_index_table,
			cdv_menu_table,
			cdv_decode_heap_table,
			cell_id,
			cell_pos,
			clist, 
			cover_internal_data_table,
			cover_collected_remote_data_table,
			cover_binary_code_table,
			code,
			code_names,
			cookies,
			corba_policy,
			corba_policy_associations,
                        dets,
                        dets_owners,
			dets_registry,
			disk_log_names,
                        disk_log_pids,
			eprof,
			erl_atom_cache,
			erl_epmd_nodes,
			etop_accum_tab,
			etop_tr,
			ets_coverage_data,
			file_io_servers,
			global,
			global_locks,
			global_names,
			global_names_ext,
			gs_mapping,
			gs_names,
			gstk_db,
			gstk_grid_cellid,
			gstk_grid_cellpos,
			gstk_grid_id,
			gvar,
			httpd,
			id,
			ig,
			ign_req_index,
			ign_requests,
			index,
			inet_cache,
			inet_db,
			inet_hosts,
			'InitialReferences',
			int_db,
			interpreter_includedirs_macros,
			ir_WstringDef,
			lmcounter,
			locks, 
			pg2_table,
			queue,
			snmp_agent_table,
			snmp_local_db2,
			snmp_mib_data,
			snmp_note_store,
			snmp_symbolic_ets,
			sticky,
			sys_dist,			
			tid_locks,
			tkFun,
			tkLink,
			tkPriv,
			ttb,
			ttb_history_table,
			udp_fds,
			udp_pids
		       ]).


-define(MNESIA_TABLES, [alarm,
			alarmTable,
			evaLogDiscriminatorTable,
			eva_snmp_map,
			eventTable,
			group,
			imprec,
			ir_AliasDef,
			ir_ArrayDef,
			ir_AttributeDef,
			ir_ConstantDef,
			ir_Contained,
			ir_Container,
			ir_EnumDef,
			ir_ExceptionDef,
			ir_IDLType,
			ir_IRObject,
			ir_InterfaceDef,
			ir_ModuleDef,
			ir_ORB,
			ir_OperationDef,
			ir_PrimitiveDef,
			ir_Repository,
			ir_SequenceDef,
			ir_StringDef,
			ir_StructDef,
			ir_TypedefDef,
			ir_UnionDef,
			logTable,
			logTransferTable,
			mesh_meas,
			mesh_type,
			mnesia_clist,
			mnesia_decision,
			mnesia_transient_decision,
			orber_CosNaming,
			orber_objkeys,
			schema,
			user
		       ]).


-define(UNREADABLE_MNESIA_TABLES, [schema]).


-define(SYSTEM_OWNERS, [alarm_handler,
			application_controller,
			auth,
			coast_server,
			code_server,
			cover_server_001,
			dbg,
			dets,
                        dets_sup,
			disk_log_server,
			disk_log_sup,
			erl_epmd,
			erl_prim_loader,
			error_logger,
			eva_log_sup,
			eva_server,
			eva_sup,
			file_server,
			file_server_2,
			global_group,
			global_group_check,
			global_name_server,
			gs_frontend,
			heart,
			help_main,
			inet_db,
			inet_gethost_native,
			init,
			int_db,
			interpret,
			jive_server,
			kernel_safe_sup,
			kernel_sup,
			log_server,
			mandel_server,
			mesh_sup,
			mesh_server,
			mnesia_checkpoint_sup,
			mnesia_dumper,
			mnesia_event,
			mnesia_fallback,
			mnesia_init,
			mnesia_kernel_sup,
			mnesia_late_loader,
			mnesia_locker,
			mnesia_monitor,
			mnesia_recover,
			mnesia_snmp_sup,
			mnesia_subscr,
			mnesia_sup,
			mnesia_tm,
			net_kernel,
			net_sup,
			overload,
			perfmon_sampler,
			pxw_server,
			release_handler,
			%% rex,    %% Otherwise we won't see tables we've created on other nodes!
			rsh_starter,
			sasl_safe_sup,
			sasl_sup,
			snmp_agent_sup,
			snmp_local_db,
			snmp_master_agent,
			snmp_misc_sup,
			snmp_note_store,
			snmp_supervisor,
			snmp_symbolic_store,
			socket,
			sounder,
			ssl_socket,
			take_over_monitor,
			timer_server,
			tk,
			udp_server,
			user,
			winshell_controller,
			xerl_copy,
			xerl_monitor
		       ]).








%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

-module(snmp_config).

-include_lib("kernel/include/file.hrl").
-include("snmp_types.hrl").

-export([config/0]).

-export([write_config_file/4, append_config_file/4, read_config_file/3]).

-export([write_agent_snmp_files/7, write_agent_snmp_files/12,

	 write_agent_snmp_conf/5, 
	 write_agent_snmp_context_conf/1, 
	 write_agent_snmp_community_conf/1, 
	 write_agent_snmp_standard_conf/2, 
	 write_agent_snmp_target_addr_conf/4, 
	 write_agent_snmp_target_addr_conf/6, 
	 write_agent_snmp_target_params_conf/2, 
	 write_agent_snmp_notify_conf/2, 
	 write_agent_snmp_usm_conf/5, 
	 write_agent_snmp_vacm_conf/3, 

	 write_manager_snmp_files/8,
	 write_manager_snmp_conf/5, 
	 write_manager_snmp_users_conf/2, 
	 write_manager_snmp_agents_conf/2, 
	 write_manager_snmp_usm_conf/2
 	 
	]).

-export([write_agent_config/3, 
	 update_agent_config/2, 
	 
	 write_agent_context_config/3, 
	 update_agent_context_config/2, 
	 
	 write_agent_community_config/3, 
	 update_agent_community_config/2, 

	 write_agent_standard_config/3, 
	 update_agent_standard_config/2, 

	 write_agent_target_addr_config/3, 
	 update_agent_target_addr_config/2, 

	 write_agent_target_params_config/3, 
	 update_agent_target_params_config/2, 

	 write_agent_notify_config/3, 
	 update_agent_notify_config/2, 

	 write_agent_vacm_config/3, 
	 update_agent_vacm_config/2, 

	 write_agent_usm_config/3, 
	 update_agent_usm_config/2, 

	 write_manager_config/3, 
	 update_manager_config/2,

	 write_manager_users_config/3, 
	 update_manager_users_config/2,

	 write_manager_agents_config/3, 
	 update_manager_agents_config/2,

	 write_manager_usm_config/3, 
	 update_manager_usm_config/2
	]).


%%----------------------------------------------------------------------
%% Handy SNMP configuration
%%----------------------------------------------------------------------

config() ->
    case (catch config2()) of
	ok ->
	    ok;
	{error, Reason} ->
	    {error, Reason};
	E ->
	    {error, {failed, E}}
    end.


config2() ->
    intro(),
    SysAgentConfig = 
	case config_agent() of
	    [] ->
		[];
	    SAC ->
		[{agent, SAC}]
	end,
    SysMgrConfig   = 
	case config_manager() of
	    [] ->
		[];
	    SMC ->
		[{manager, SMC}]
	end,
    config_sys(SysAgentConfig ++ SysMgrConfig),
    ok.


intro() ->
    i("~nSimple SNMP configuration tool (version ~s)", [?version]),
    i("------------------------------------------------"),
    i("Note: Non-trivial configurations still has to be"),
    i("      done manually. IP addresses may be entered "),
    i("      as dront.ericsson.se (UNIX only) or"),
    i("      123.12.13.23"),
    i("------------------------------------------------"),
    ok.


config_agent() ->
    case (catch snmp_agent2()) of
	ok ->
	    [];
	{ok, SysConf} ->
	    SysConf;
	{error, Reason} ->
	    error(Reason);
	{'EXIT', Reason} ->
	    error(Reason);
	E ->
	    error({agent_config_failed, E})
    end.

snmp_agent2() ->
    case ask("~nConfigure an agent (y/n)?", "y", fun verify_yes_or_no/1) of
	yes ->
	    {Vsns, ConfigDir, SysConf} = config_agent_sys(),
	    config_agent_snmp(ConfigDir, Vsns),
	    {ok, SysConf};
	no ->
	    ok
    end.


config_manager() ->
    case (catch config_manager2()) of
	ok ->
	    [];
	{ok, SysConf} ->
	    SysConf;
	{error, Reason} ->
	    error(Reason);
	{'EXIT', Reason} ->
	    error(Reason);
	E ->
	    error({manager_config_failed, E})
    end.

config_manager2() ->
    case ask("~nConfigure a manager (y/n)?", "y", fun verify_yes_or_no/1) of
	yes ->
	    {Vsns, ConfigDir, SysConf} = config_manager_sys(),
 	    config_manager_snmp(ConfigDir, Vsns),
	    {ok, SysConf};
	no ->
	    ok
    end.


config_sys(SysConfig) ->
    i("~n--------------------"),
    {ok, DefDir} = file:get_cwd(),
    ConfigDir = ask("Configuration directory for system file (absolute path)?",
		    DefDir, fun verify_dir/1),    
    write_sys_config_file(ConfigDir, SysConfig).


%% -------------------

config_agent_sys() ->
    i("~nAgent system config: "
      "~n--------------------"),
    Prio = ask("1. Agent process priority (low/normal/high)", 
	       "normal", fun verify_prio/1),
    Vsns = ask("2. What SNMP version(s) should be used "
	       "(1,2,3,1&2,1&2&3,2&3)?", "3", fun verify_versions/1),
    %% d("Vsns: ~p", [Vsns]),
    {ok, DefDir} = file:get_cwd(),
    {DefConfDir, Warning} = default_agent_dir(DefDir),
    ConfQ = 
	if
	    Warning == "" ->
		"3. Configuration directory (absolute path)?";
	    true ->
		lists:flatten(
		  io_lib:format("3. Configuration directory (absolute path)?" 
				"~n   ~s", [Warning]))
	end,
    ConfigDir = ask(ConfQ, DefConfDir, fun verify_dir/1),
    ConfigVerb = ask("4. Config verbosity "
		     "(silence/info/log/debug/trace)?", 
		     "silence",
		     fun verify_verbosity/1),
    DbDir     = ask("5. Database directory (absolute path)?", DefDir, 
		    fun verify_dir/1),
    MibStorageType = ask("6. Mib storage type (ets/dets/mnesia)?", "ets",
			 fun verify_mib_storage_type/1),
    MibStorage = 
	case MibStorageType of
	    ets ->
		ets;
	    dets ->
		DetsDir = ask("6b. Mib storage directory (absolute path)?",
			      DbDir, fun verify_dir/1),
		DetsAction = ask("6c. Mib storage [dets] database start "
				 "action "
				 "(default/clear/keep)?", 
				 "default", fun verify_mib_storage_action/1),
		case DetsAction of
		    default ->
			{dets, DetsDir};
		    _ ->
			{dets, DetsDir, DetsAction}
		end;
	    mnesia ->
% 		Nodes = ask("Mib storage nodes?", "none", 
% 			    fun verify_mib_storage_nodes/1),
		Nodes = [],
		MnesiaAction = ask("6b. Mib storage [mnesia] database start "
				   "action "
				   "(default/clear/keep)?", 
				   "default", fun verify_mib_storage_action/1),
		case MnesiaAction of
		    default ->
			{mnesia, Nodes};
		    _ ->
			{mnesia, Nodes, MnesiaAction}
		end
	end,
    TargetCacheVerb = ask("7. Target cache verbosity "
			  "(silence/info/log/debug/trace)?", "silence",
			  fun verify_verbosity/1),
    SymStoreVerb = ask("8. Symbolic store verbosity "
		       "(silence/info/log/debug/trace)?", "silence",
		       fun verify_verbosity/1),
    LocalDbVerb = ask("9. Local DB verbosity "
		       "(silence/info/log/debug/trace)?", "silence",
		       fun verify_verbosity/1),
    LocalDbRepair = ask("10. Local DB repair (true/false/force)?", "true",
			fun verify_dets_repair/1),
    LocalDbAutoSave = ask("11. Local DB auto save (infinity/milli seconds)?", 
			  "5000", fun verify_dets_auto_save/1),
    ErrorMod = ask("12. Error report module?", "snmpa_error_logger", fun verify_module/1),
    Type = ask("13. Agent type (master/sub)?", "master", 
	       fun verify_agent_type/1),
    AgentConfig = 
	case Type of 
	    master ->
		MasterAgentVerb = ask("14. Master-agent verbosity "
				      "(silence/info/log/debug/trace)?", 
				      "silence",
				      fun verify_verbosity/1),
		ForceLoad = ask("15. Shall the agent re-read the "
				"configuration files during startup ~n"
				"    (and ignore the configuration "
				"database) (true/false)?", "true", 
				fun verify_bool/1),
		MultiThreaded = ask("16. Multi threaded agent (true/false)?", 
				    "false",
				    fun verify_bool/1),
		MeOverride = ask("17. Check for duplicate mib entries when "
				 "installing a mib (true/false)?", "false",
				 fun verify_bool/1),
		TrapOverride = ask("18. Check for duplicate trap names when "
				   "installing a mib (true/false)?", "false",
				   fun verify_bool/1),
		MibServerVerb = ask("19. Mib server verbosity "
				    "(silence/info/log/debug/trace)?", 
				    "silence",
				    fun verify_verbosity/1),
		MibServerCache = ask("20. Mib server cache "
				    "(true/false)?", 
				    "true",
				    fun verify_bool/1),
		NoteStoreVerb = ask("21. Note store verbosity "
				    "(silence/info/log/debug/trace)?", 
				    "silence",
				    fun verify_verbosity/1),
		NoteStoreTimeout = ask("22. Note store GC timeout?", "30000",
				       fun verify_timeout/1),
		ATL = 
		    case ask("23. Shall the agent use an audit trail log "
			     "(y/n)?",
			     "n", fun verify_yes_or_no/1) of
			yes ->
			    ATLType = ask("23b. Audit trail log type "
					  "(write/read_write)?",
					  "read_write", fun verify_atl_type/1),
			    ATLDir = ask("23c. Where to store the "
					 "audit trail log?",
					 DefDir, fun verify_dir/1),
			    ATLMaxFiles = ask("23d. Max number of files?", 
					      "10", 
					      fun verify_pos_integer/1),
			    ATLMaxBytes = ask("23e. Max size (in bytes) "
					      "of each file?", 
					      "10240", 
					      fun verify_pos_integer/1),
			    ATLSize = {ATLMaxBytes, ATLMaxFiles},
			    ATLRepair = ask("23f. Audit trail log repair "
					    "(true/false/truncate/snmp_repair)?", "true",
					    fun verify_atl_repair/1),
			    ATLSeqNo = ask("23g. Audit trail log "
					   "sequence-numbering (true/false)?", 
					   "false",
					   fun verify_atl_seqno/1),
			    [{audit_trail_log, [{type,   ATLType},
						{dir,    ATLDir},
						{size,   ATLSize},
						{repair, ATLRepair},
						{seqno, ATLSeqNo}]}];
			no ->
			    []
		    end,
		NetIfVerb = ask("24. Network interface verbosity "
				"(silence/info/log/debug/trace)?", 
				"silence",
				fun verify_verbosity/1),
		NetIfMod = ask("25. Which network interface module shall be used?",
			       "snmpa_net_if", fun verify_module/1),
		NetIfOpts = 
		    case NetIfMod of
			snmpa_net_if ->
			    NetIfBindTo = 
				ask("25a. Bind the agent IP address "
				    "(true/false)?",
				    "false", fun verify_bool/1),
			    NetIfNoReuse = 
				ask("25b. Shall the agents "
				    "IP address "
				    "and port be not reusable "
				    "(true/false)?",
				    "false", fun verify_bool/1),
			    NetIfReqLimit = 
				ask("25c. Agent request limit "
				    "(used for flow control) "
				    "(infinity/pos integer)?", 
				    "infinity",
				    fun verify_netif_req_limit/1),
			    NetIfRecbuf = 
				case ask("25d. Receive buffer size of the "
					 "agent (in bytes) "
					 "(default/pos integer)?", 
					 "default", 
					 fun verify_netif_recbuf/1) of
				    default ->
					[];
				    RecBufSz ->
					[{recbuf, RecBufSz}]
				end,
			    NetIfSndbuf = 
				case ask("25e. Send buffer size of the agent "
					 "(in bytes) (default/pos integer)?", 
					 "default", 
					 fun verify_netif_sndbuf/1) of
				    default ->
					[];
				    SndBufSz ->
					[{sndbuf, SndBufSz}]
				end,
			    NetIfFilter = 
				case ask("25f. Do you wish to specify a "
					 "network interface filter module "
					 "(or use default)",
					 "default", fun verify_module/1) of
				    default ->
					[];
				    NetIfFilterMod ->
					[{filter, [{module, NetIfFilterMod}]}]
				end,
			    [{bind_to,   NetIfBindTo},
			     {no_reuse,  NetIfNoReuse},
			     {req_limit, NetIfReqLimit}] ++ 
				NetIfRecbuf ++ NetIfSndbuf ++ NetIfFilter;
			_ ->
			    []
		    end,
		NetIf = [{module,    NetIfMod},
			 {verbosity, NetIfVerb},
			 {options,   NetIfOpts}],
		TermDiscoEnable = ask("26a. Allow terminating discovery "
				      "(true/false)?", "true",
				      fun verify_bool/1),
		TermDiscoConf = 
		    case TermDiscoEnable of
			true ->
			    TermDiscoStage2 = 
				ask("26b. Second stage behaviour "
				    "(discovery/plain)?", "discovery",
				    fun verify_term_disco_behaviour/1),
			    TermDiscoTrigger = 
				ask("26c. Trigger username "
				    "(default/a string)?", "default",
				    fun verify_term_disco_trigger_username/1),
			    [{enable, TermDiscoEnable},
			     {stage2, TermDiscoStage2},
			     {trigger_username, TermDiscoTrigger}];
			false ->
			    [{enable, TermDiscoEnable},
			     {stage2, discovery},
			     {trigger_username, ""}]
		    end,
		OrigDiscoEnable = ask("27a. Allow originating discovery "
				      "(true/false)?", "true",
				      fun verify_bool/1),
		OrigDiscoConf = 
		    [{enable, OrigDiscoEnable}], 
		DiscoveryConfig = 
		    [{terminating, TermDiscoConf},
		     {originating, OrigDiscoConf}], 
		[{agent_type,      master},
		 {agent_verbosity, MasterAgentVerb},
		 {discovery,       DiscoveryConfig}, 
		 {config,          [{dir,        ConfigDir}, 
				    {force_load, ForceLoad},
				    {verbosity,  ConfigVerb}]},
		 {multi_threaded,  MultiThreaded},
		 {mib_server,      [{mibentry_override,  MeOverride},
				    {trapentry_override, TrapOverride},
				    {verbosity,          MibServerVerb},
				    {cache,              MibServerCache}]},
		 {note_store,      [{timeout,   NoteStoreTimeout},
				    {verbosity, NoteStoreVerb}]},
		 {net_if, NetIf}] ++ ATL;
	    sub ->
		SubAgentVerb = ask("14. Sub-agent verbosity "
				   "(silence/info/log/debug/trace)?", 
				   "silence",
				   fun verify_verbosity/1),
		[{agent_type,      sub},
		 {agent_verbosity, SubAgentVerb},
		 {config,          [{dir, ConfigDir}]}]
	end,
    SysConfig = 
	[{priority,    Prio},
	 {versions,    Vsns},
	 {db_dir,      DbDir},
	 {mib_storage, MibStorage},
	 {target_cache, [{verbosity, TargetCacheVerb}]},
	 {symbolic_store, [{verbosity, SymStoreVerb}]},
	 {local_db, [{repair,    LocalDbRepair},
		     {auto_save, LocalDbAutoSave},
		     {verbosity, LocalDbVerb}]},
	 {error_report_module, ErrorMod}] ++ AgentConfig,
    {Vsns, ConfigDir, SysConfig}.


config_agent_snmp(Dir, Vsns) ->
    i("~nAgent snmp config: "
      "~n------------------"),
    AgentName  = guess_agent_name(),
    EngineName = guess_engine_name(),
    SysName    = ask("1. System name (sysName standard variable)", 
		     AgentName, fun verify_system_name/1),
    EngineID   = ask("2. Engine ID (snmpEngineID standard variable)", 
		      EngineName, fun verify_engine_id/1),
    MMS        = ask("3. Max message size?", "484", 
		     fun verify_max_message_size/1),
    AgentUDP   = ask("4. The UDP port the agent listens to. "
		     "(standard 161)",
		     "4000", fun verify_port_number/1),
    Host       = host(),
    AgentIP    = ask("5. IP address for the agent (only used as id ~n"
		     "   when sending traps)", Host, fun verify_address/1),
    ManagerIP  = ask("6. IP address for the manager (only this manager ~n"
		     "   will have access to the agent, traps are sent ~n"
		     "   to this one)", Host, fun verify_address/1),
    TrapUdp    = ask("7. To what UDP port at the manager should traps ~n"
		     "   be sent (standard 162)?", "5000", 
		     fun verify_port_number/1),
    SecType    = ask("8. Do you want a none- minimum- or semi-secure"
		     " configuration? ~n"
		     "   Note that if you chose v1 or v2, you won't get any"
		     " security for these~n"
		     "   requests (none, minimum, semi_des, semi_aes)", 
		     "minimum", 
		    fun verify_sec_type/1),
    Passwd = 
	case lists:member(v3, Vsns) and (SecType /= none) of
	    true ->
		ensure_crypto_started(),
		ask("8b. Give a password of at least length 8. It is used to "
		    "generate ~n"
		    "    private keys for the configuration: ",
		    mandatory, fun verify_passwd/1);
	    false ->
		""
	end,
    NotifType  =
	case lists:member(v1, Vsns) of
	    true ->
		Overwrite = ask("9. Current configuration files will "
				"now be overwritten. "
				"Ok (y/n)?", "y", fun verify_yes_or_no/1),
		case Overwrite of
		    no ->
			error(overwrite_not_allowed);
		    yes ->
			ok
		end,
		trap;
	    false ->
		NT = ask("9. Should notifications be sent as traps or informs "
			 "(trap/inform)?", "trap", fun verify_notif_type/1),
		Overwrite = ask("10. Current configuration files will "
				"now be overwritten. "
				"Ok (y/n)?", "y", fun verify_yes_or_no/1),
		case Overwrite of
		    no ->
			error(overwrite_not_allowed);
		    yes ->
			ok
		end,
		NT
	end,
    case (catch write_agent_snmp_files(Dir, 
				       Vsns, ManagerIP, TrapUdp, 
				       AgentIP, AgentUDP,
				       SysName, NotifType, SecType, 
				       Passwd, EngineID, MMS)) of
	ok ->
	   i("~n- - - - - - - - - - - - -"),
	   i("Info: 1. SecurityName \"initial\" has noAuthNoPriv read access~n"
	     "         and authenticated write access to the \"restricted\"~n"
	     "         subtree."),
	   i("      2. SecurityName \"all-rights\" has noAuthNoPriv "
	     "read/write~n"
	     "         access to the \"internet\" subtree."),
	   i("      3. Standard traps are sent to the manager."),
	   case lists:member(v1, Vsns) or lists:member(v2, Vsns) of
	       true ->
		   i("      4. Community \"public\" is mapped to security name"
		     " \"initial\"."),
		   i("      5. Community \"all-rights\" is mapped to security"
		     " name \"all-rights\".");
	       false ->
		   ok
	   end,
	   i("The following agent files were written: agent.conf, "
	     "community.conf,~n"
	     "standard.conf, target_addr.conf, "
	     "target_params.conf, ~n"
	     "notify.conf" ++
	     case lists:member(v3, Vsns) of
		 true -> ", vacm.conf and usm.conf";
		 false -> " and vacm.conf"
	     end),
	   i("- - - - - - - - - - - - -"),
	   ok;
	E -> 
	    error({failed_writing_files, E})
    end.


%% -------------------

config_manager_sys() ->
    i("~nManager system config: "
      "~n----------------------"),
    Prio = ask("1. Manager process priority (low/normal/high)", 
	       "normal", fun verify_prio/1),
    Vsns = ask("2. What SNMP version(s) should be used "
	       "(1,2,3,1&2,1&2&3,2&3)?", "3", fun verify_versions/1),
    {ok, DefDir} = file:get_cwd(),
    {DefConfDir, Warning} = default_manager_dir(DefDir),
    ConfQ = 
	if
	    Warning == "" ->
		"3. Configuration directory (absolute path)?";
	    true ->
		lists:flatten(
		  io_lib:format("3. Configuration directory (absolute path)?" 
				"~n   ~s", [Warning]))
	end,
    ConfigDir = ask(ConfQ, DefConfDir, fun verify_dir/1),
    ConfigVerb = ask("4. Config verbosity "
			"(silence/info/log/debug/trace)?", 
			"silence",
			fun verify_verbosity/1),
    ConfigDbDir = ask("5. Database directory (absolute path)?", 
		      DefDir, fun verify_dir/1),
    ConfigDbRepair = ask("6. Database repair "
			 "(true/false/force)?", "true",
			 fun verify_dets_repair/1),
    ConfigDbAutoSave = ask("7. Database auto save "
			   "(infinity/milli seconds)?", 
			   "5000", fun verify_dets_auto_save/1),
    IRB = 
	case ask("8. Inform request behaviour (auto/user)?", 
		 "auto", fun verify_irb/1) of
	    auto ->
		auto;
	    user ->
		case ask("8b. Use default GC timeout"
			 "(default/seconds)?",
			 "default", fun verify_irb_user/1) of
		    default ->
			user;
		    IrbGcTo ->
			{user, IrbGcTo}
		end
	end,
    ServerVerb = ask("9. Server verbosity "
			"(silence/info/log/debug/trace)?", 
			"silence",
			fun verify_verbosity/1),
    ServerTimeout = ask("10. Server GC timeout?", "30000",
			   fun verify_timeout/1),    
    NoteStoreVerb = ask("11. Note store verbosity "
			"(silence/info/log/debug/trace)?", 
			"silence",
			fun verify_verbosity/1),
    NoteStoreTimeout = ask("12. Note store GC timeout?", "30000",
			   fun verify_timeout/1),    
    NetIfMod = ask("13. Which network interface module shall be used?",
		   "snmpm_net_if", fun verify_module/1),
    NetIfVerb = ask("14. Network interface verbosity "
		    "(silence/info/log/debug/trace)?", "silence",
		    fun verify_verbosity/1),
    NetIfBindTo = ask("15. Bind the manager IP address "
		      "(true/false)?",
		      "false", fun verify_bool/1),
    NetIfNoReuse = ask("16. Shall the manager IP address and port "
		       "be not reusable (true/false)?",
		       "false", fun verify_bool/1),
    NetIfRecbuf = 
	case ask("17. Receive buffer size of the manager (in bytes) "
		 "(default/pos integer)?", "default", 
		 fun verify_netif_recbuf/1) of
	    default ->
		[];
	    RecBufSz ->
		[{recbuf, RecBufSz}]
	end,
    NetIfSndbuf = 
	case ask("18. Send buffer size of the manager (in bytes) "
		 "(default/pos integer)?", "default", 
		 fun verify_netif_sndbuf/1) of
	    default ->
		[];
	    SndBufSz ->
		[{sndbuf, SndBufSz}]
	end,
    NetIfOpts = 
	[{bind_to,   NetIfBindTo},
	 {no_reuse,  NetIfNoReuse}] ++ NetIfRecbuf ++ NetIfSndbuf,
    NetIf = 
	[{module,    NetIfMod},
	 {verbosity, NetIfVerb},
	 {options,   NetIfOpts}], 
    ATL = 
	case ask("19. Shall the manager use an audit trail log "
		 "(y/n)?",
		 "n", fun verify_yes_or_no/1) of
	    yes ->
		ATLType = ask("19b. Audit trail log type "
			      "(write/read_write)?",
			      "read_write", fun verify_atl_type/1),
		ATLDir = ask("19c. Where to store the "
			     "audit trail log?",
			     DefDir, fun verify_dir/1),
		ATLMaxFiles = ask("19d. Max number of files?", 
				  "10", 
				  fun verify_pos_integer/1),
		ATLMaxBytes = ask("19e. Max size (in bytes) "
				  "of each file?", 
				  "10240", 
				  fun verify_pos_integer/1),
		ATLSize = {ATLMaxBytes, ATLMaxFiles},
		ATLRepair = ask("19f. Audit trail log repair "
				"(true/false/truncate/snmp_repair)?", "true",
				fun verify_atl_repair/1),
		ATLSeqNo = ask("19g. Audit trail log sequence-numbering "
			       "(true/false)?", "false",
			       fun verify_atl_seqno/1),
		[{audit_trail_log, [{type,   ATLType},
				    {dir,    ATLDir},
				    {size,   ATLSize},
				    {repair, ATLRepair},
				    {seqno,  ATLSeqNo}]}];
	    no ->
		[]
	end,
    DefUser = 
	case ask("20. Do you wish to assign a default user [yes] or use~n"
		 "    the default settings [no] (y/n)?", "n", 
		 fun verify_yes_or_no/1) of
	    yes ->
		DefUserMod = ask("20b. Default user module?", 
				 "snmpm_user_default",
				 fun verify_module/1),
		DefUserData = ask("20c. Default user data?", "undefined",
				  fun verify_user_data/1),
		[{def_user_mod,  DefUserMod},
		 {def_user_data, DefUserData}];
	    no ->
		[]
	end,
    SysConfig = 
	[{priority,   Prio},
	 {versions,   Vsns},
	 {config,     [{dir,       ConfigDir}, 
		       {verbosity, ConfigVerb},
		       {db_dir,    ConfigDbDir},
		       {repair,    ConfigDbRepair},
		       {auto_save, ConfigDbAutoSave}]},
	 {inform_request_behaviour, IRB},
	 {mibs,       []},
	 {server,     [{timeout,   ServerTimeout},
		       {verbosity, ServerVerb}]},
	 {note_store, [{timeout,   NoteStoreTimeout},
		       {verbosity, NoteStoreVerb}]},
	 {net_if,     NetIf}] ++ ATL ++ DefUser,
    {Vsns, ConfigDir, SysConfig}.


config_manager_snmp(Dir, Vsns) ->
    i("~nManager snmp config: "
      "~n--------------------"),
    EngineName = guess_engine_name(),
    EngineID   = ask("1. Engine ID (snmpEngineID standard variable)", 
		      EngineName, fun verify_engine_id/1),
    MMS        = ask("2. Max message size?", "484", 
		     fun verify_max_message_size/1),
    Host       = host(),
    IP         = ask("3. IP address for the manager (only used as id ~n"
		     "   when sending requests)",
		     Host, fun verify_address/1),
    Port       = ask("4. Port number (standard 162)?", "5000", 
		     fun verify_port_number/1),
    Users      = config_manager_snmp_users([]),
    Agents     = config_manager_snmp_agents([]),
    Usms       = config_manager_snmp_usms([]),
    Overwrite = ask("8. Current configuration files will now be overwritten. "
		    "Ok (y/n)?", "y", fun verify_yes_or_no/1),
    case Overwrite of
	no ->
	    error(overwrite_not_allowed);
	yes ->
	    ok
    end,
    case (catch write_manager_snmp_files(Dir, 
					 IP, Port, MMS, EngineID, 
					 Users, Agents, Usms)) of
	ok ->
	   i("~n- - - - - - - - - - - - -"),
	   i("The following manager files were written: "
	     "manager.conf, agents.conf " ++ 
	     case lists:member(v3, Vsns) of
		 true ->
		     ", users.conf and usm.conf";
		 false ->
		     " and users.conf"
	     end),
	   i("- - - - - - - - - - - - -"),
	    ok;
	E ->
	    error({failed_writing_files, E})
    end.
	     

config_manager_snmp_users(Users) ->
    case ask("5. Configure a user of this manager (y/n)?",
	     "y", fun verify_yes_or_no/1) of
	yes ->
	    User = config_manager_snmp_user(),
	    config_manager_snmp_users([User|Users]);
	no ->
	    lists:reverse(Users)
    end.

config_manager_snmp_user() ->
    UserId   = ask("5b. User id?", mandatory, 
		   fun verify_user_id/1),
    UserMod  = ask("5c. User callback module?", mandatory, 
		   fun verify_module/1),
    UserData = ask("5d. User (callback) data?", "undefined",
		   fun verify_user_data/1),
    {UserId, UserMod, UserData}.
    

config_manager_snmp_agents(Agents) ->
    case ask("6. Configure an agent handled by this manager (y/n)?",
	     "y", fun verify_yes_or_no/1) of
	yes ->
	    Agent = config_manager_snmp_agent(),
	    config_manager_snmp_agents([Agent|Agents]);
	no ->
	    lists:reverse(Agents)
    end.

config_manager_snmp_agent() ->
    UserId     = ask("6b. User id?", mandatory, 
		     fun verify_user_id/1),
    TargetName = ask("6c. Target name?", guess_agent_name(),
		     fun verify_system_name/1),
    Version    = ask("6d. Version (1/2/3)?", "1",
	             fun verify_version/1),
    Comm       = ask("6e. Community string ?", "public",
	             fun verify_community/1),
    EngineID   = ask("6f. Engine ID (snmpEngineID standard variable)", 
	             guess_engine_name(), fun verify_engine_id/1),
    IP         = ask("6g. IP address for the agent", host(), 
	             fun verify_address/1),
    Port       = ask("6h. The UDP port the agent listens to. "
	             "(standard 161)", "4000", fun verify_port_number/1),
    Timeout    = ask("6i. Retransmission timeout (infinity/pos integer)?",
	             "infinity", fun verify_retransmission_timeout/1),    
    MMS        = ask("6j. Max message size?", "484", 
	             fun verify_max_message_size/1),
    SecModel   = ask("6k. Security model (any/v1/v2c/usm)?", "any", 
	             fun verify_sec_model/1),
    SecName    = ask("6l. Security name?", "\"initial\"", 
	             fun verify_sec_name/1),
    SecLevel   = ask("6m. Security level (noAuthNoPriv/authNoPriv/authPriv)?",
	             "noAuthNoPriv", fun verify_sec_level/1),
    {UserId,
     TargetName, Comm, IP, Port, EngineID, Timeout, MMS, 
     Version, SecModel, SecName, SecLevel}.


config_manager_snmp_usms(Usms) ->
    case ask("7. Configure an usm user handled by this manager (y/n)?",
	     "y", fun verify_yes_or_no/1) of
	yes ->
	    Usm = config_manager_snmp_usm(),
	    config_manager_snmp_usms([Usm|Usms]);
	no ->
	    lists:reverse(Usms)
    end.

config_manager_snmp_usm() ->
    EngineID = ask("7a. Engine ID", guess_engine_name(), 
		   fun verify_engine_id/1),
    UserName = ask("7b. User name?", mandatory, fun verify_usm_name/1),
    SecName  = ask("7c. Security name?", UserName,
		   fun verify_usm_sec_name/1),
    AuthP    = ask("7d. Authentication protocol (no/sha/md5)?", "no",
		   fun verify_usm_auth_protocol/1),
    AuthKey  = ask_auth_key("7e", AuthP), 
    PrivP    = ask("7e. Priv protocol (no/des/aes)?", "no",
		   fun verify_usm_priv_protocol/1),
    PrivKey  = ask_priv_key("7f", PrivP), 
    {EngineID, UserName,
     SecName, AuthP, AuthKey, PrivP, PrivKey}.


%% ------------------------------------------------------------------

is_members([], _Files) ->
    true;
is_members([H|T], Files) ->
    lists:member(H, Files) andalso is_members(T, Files).

default_agent_dir(DefDir) ->
    default_dir("agent", DefDir).

default_manager_dir(DefDir) ->
    default_dir("manager", DefDir).

default_dir(Component, DefDir) ->
    %% Look for the component dir, if found use that as default
    {ok, Files} = file:list_dir(DefDir),
    case lists:member(Component, Files) of
	true ->
	    {filename:join(DefDir, Component), ""};
	false ->
	    %% No luck, 
	    %% so check if cwd contains either agent and/or 
	    %% manager config files. If it does, issue a warning

	    %% Check for presence of agent config files
	    %% If all the agent config files are present,
	    %% issue a warning
	    AgentConfs = 
		[
		 "agent.conf",
		 "context.conf",
		 "community.conf",
		 "notify.conf",
		 "standard.conf",
		 "target_params.conf",
		 "target_addr.conf",
		 "usm.conf",	 
		 "vacm.conf"
		],
	    IsAgentDir = is_members(AgentConfs, Files),

	    %% Check for presence of manager config files
	    %% If all the manager config files are present,
	    %% issue a warning
	    ManagerConfs = 
		[
		 "agents.conf",
		 "manager.conf",
		 "users.conf",
		 "usm.conf"
		],
	    IsManagerDir = is_members(ManagerConfs, Files),
	    Warning = 
		if
		    IsAgentDir and IsManagerDir ->
			"Note that the default directory contains both agent and manager config files";
		    IsAgentDir ->
			"Note that the default directory contains agent config files";
		    IsManagerDir ->
			"Note that the default directory contains manager config files";
		    true ->
			""
		end,
	    {DefDir, Warning}
    end.


%% ------------------------------------------------------------------

ask_auth_key(_Prefix, usmNoAuthProtocol) ->
    "";
ask_auth_key(Prefix, usmHMACSHAAuthProtocol) ->
    ask(Prefix ++ "  Authentication [sha] key (length 0 or 20)?", "\"\"",
	fun verify_usm_auth_sha_key/1);
ask_auth_key(Prefix, usmHMACMD5AuthProtocol) ->
    ask(Prefix ++ "  Authentication [md5] key (length 0 or 16)?", "\"\"",
	fun verify_usm_auth_md5_key/1).

ask_priv_key(_Prefix, usmNoPrivProtocol) ->
    "";
ask_priv_key(Prefix, usmDESPrivProtocol) ->
    ask(Prefix ++ "  Priv [des] key (length 0 or 16)?", "\"\"",
	fun verify_usm_priv_des_key/1);
ask_priv_key(Prefix, usmAesCfb128Protocol) ->
    ask(Prefix ++ "  Priv [aes] key (length 0 or 16)?", "\"\"",
	fun verify_usm_priv_aes_key/1).


%% ------------------------------------------------------------------

verify_yes_or_no("y") ->
    {ok, yes};
verify_yes_or_no("yes") ->
    {ok, yes};
verify_yes_or_no("n") ->
    {ok, no};
verify_yes_or_no("no") ->
    {ok, no};
verify_yes_or_no(YON) ->
    {error, "invalid yes or no: " ++ YON}.


verify_prio("low") ->
    {ok, low};
verify_prio("normal") ->
    {ok, normal};
verify_prio("high") ->
    {ok, high};
verify_prio(Prio) ->
    {error, "invalid process priority: " ++ Prio}.


verify_system_name(Name) -> {ok, Name}.


verify_engine_id(Name) -> {ok, Name}.


verify_max_message_size(MMS) ->
    case (catch list_to_integer(MMS)) of
	I when is_integer(I) andalso (I >= 484) ->
	    {ok, I};
	I when is_integer(I) ->
	    {error, "invalid max message size (must be atleast 484): " ++ MMS};
	_ ->
	    {error, "invalid max message size: " ++ MMS}
    end.
	
 
verify_port_number(P) ->
    case (catch list_to_integer(P)) of
	N when is_integer(N) andalso (N > 0) ->
	    {ok, N};
	_ ->
	    {error, "invalid port number: " ++ P}
    end.


verify_versions("1")     -> {ok, [v1]};
verify_versions("2")     -> {ok, [v2]};
verify_versions("3")     -> {ok, [v3]};
verify_versions("1&2")   -> {ok, [v1,v2]};
verify_versions("1&3")   -> {ok, [v1,v3]};
verify_versions("2&3")   -> {ok, [v2,v3]};
verify_versions("1&2&3") -> {ok, [v1,v2,v3]};
verify_versions(V)       -> {error, "incorrect version(s): " ++ V}.

verify_version("1")     -> {ok, v1};
verify_version("2")     -> {ok, v2};
verify_version("3")     -> {ok, v3};
verify_version(V)       -> {error, "incorrect version: " ++ V}.

    
verify_passwd(Passwd) when length(Passwd) >= 8 ->
    {ok, Passwd};
verify_passwd(_P) ->
    {error, "invalid password"}.


verify_dir(Dir) ->
    case filename:pathtype(Dir) of
	absolute -> 
	    case file:read_file_info(Dir) of
		{ok, #file_info{type = directory}} ->
		    {ok, snmp_misc:ensure_trailing_dir_delimiter(Dir)};
		{ok, _FileInfo} ->
		    {error, Dir ++ " is not a directory"};
		_ ->
		    {error, "invalid directory: " ++ Dir}
	    end;
	_E -> 
	    {error, "invalid directory (not absolute): " ++ Dir}
    end.
	    

verify_notif_type("trap")   -> {ok, trap};
verify_notif_type("inform") -> {ok, inform};
verify_notif_type(NT)       -> {error, "invalid notifcation type: " ++ NT}.


verify_sec_type("none")     -> {ok, none};
verify_sec_type("minimum")  -> {ok, minimum};
verify_sec_type("semi_des") -> {ok, {semi, des}};
verify_sec_type("semi_aes") -> {ok, {semi, aes}};
verify_sec_type(ST)         -> {error, "invalid security type: " ++ ST}.

    
verify_address(A) ->
    case (catch snmp_misc:ip(A)) of
	{ok, IP} ->
	     {ok, tuple_to_list(IP)};
	{error, _} ->
	    {error, "invalid address: " ++ A};
	_E ->
	    {error, "invalid address: " ++ A}
    end.


verify_mib_storage_type("m") ->
    {ok, mnesia};
verify_mib_storage_type("mnesia") ->
    {ok, mnesia};
verify_mib_storage_type("d") ->
    {ok, dets};
verify_mib_storage_type("dets") ->
    {ok, dets};
verify_mib_storage_type("e") ->
    {ok, ets};
verify_mib_storage_type("ets") ->
    {ok, ets};
verify_mib_storage_type(T) ->
    {error, "invalid mib storage type: " ++ T}.

verify_mib_storage_action("default") ->
    {ok, default};
verify_mib_storage_action("clear") ->
    {ok, clear};
verify_mib_storage_action("keep") ->
    {ok, keep};
verify_mib_storage_action(A) ->
    {error, "invalid mib storage action: " ++ A}.


verify_verbosity("silence") ->
    {ok, silence};
verify_verbosity("info") ->
    {ok, info};
verify_verbosity("log") ->
    {ok, log};
verify_verbosity("debug") ->
    {ok, debug};
verify_verbosity("trace") ->
    {ok, trace};
verify_verbosity(V) ->
    {error, "invalid verbosity: " ++ V}.


verify_dets_repair("true") ->
    {ok, true};
verify_dets_repair("false") ->
    {ok, false};
verify_dets_repair("force") ->
    {ok, force};
verify_dets_repair(R) ->
    {error, "invalid repair: " ++ R}.

verify_dets_auto_save("infinity") ->
    {ok, infinity};
verify_dets_auto_save(I0) ->
    case (catch list_to_integer(I0)) of
	I when is_integer(I) andalso (I > 0) ->
	    {ok, I};
	_ -> 
	    {error, "invalid auto save timeout time: " ++ I0}
    end.


%% I know that this is a little of the edge, but...
verify_module(M0) ->
    case (catch list_to_atom(M0)) of
	M when is_atom(M) ->
	    {ok, M};
	_ ->
	    {error, "invalid module: " ++ M0}
    end.
	 

verify_agent_type("master") ->
    {ok, master};
verify_agent_type("sub") ->
    {ok, sub};
verify_agent_type(AT) ->
    {error, "invalid agent type: " ++ AT}.


verify_bool("true") ->
    {ok, true};
verify_bool("false") ->
    {ok, false};
verify_bool(B) ->
    {error, "invalid boolean: " ++ B}.


verify_timeout(T0) ->
    case (catch list_to_integer(T0)) of
	T when is_integer(T) andalso (T > 0) ->
	    {ok, T};
	_ ->
	    {error, "invalid timeout time: '" ++ T0 ++ "'"}
    end.


verify_retransmission_timeout("infinity") ->
    {ok, infinity};
verify_retransmission_timeout([${|R] = Timer) ->
    case lists:reverse(R) of
	[$}|R2] ->
	    case string:tokens(lists:reverse(R2), ", ") of
		[WaitForStr, FactorStr, IncrStr, RetryStr] ->
		    WaitFor = incr_timer_value(WaitForStr, 1),
		    Factor  = incr_timer_value(FactorStr,  1),
		    Incr    = incr_timer_value(IncrStr,    0),
		    Retry   = incr_timer_value(RetryStr,   0),
		    {ok, {WaitFor, Factor, Incr, Retry}};
		_ ->
		    {error, "invalid retransmission timer: '" ++ Timer ++ "'"}
	    end;
	_ ->
	    {error, "invalid retransmission timer: '" ++ Timer ++ "'"}
    end;
verify_retransmission_timeout(T0) ->
    case (catch list_to_integer(T0)) of
	T when is_integer(T) andalso (T > 0) ->
	    {ok, T};
	_ ->
	    {error, "invalid timeout time: '" ++ T0 ++ "'"}
    end.

incr_timer_value(Str, Min) ->
    case (catch list_to_integer(Str)) of
	I when is_integer(I) andalso (I >= Min) ->
	    I;
	I when is_integer(I) ->
	    E = lists:flatten(io_lib:format("invalid incremental timer value "
					    "(min value is ~w): " ++ Str, 
					    [Min])),
	    error(E);
	_ ->
	    error("invalid incremental timer value: " ++ Str)
    end.
	 

%% verify_atl_type("read") ->
%%     {ok, read};
verify_atl_type("write") ->
    {ok, write};
verify_atl_type("read_write") ->
    {ok, read_write};
verify_atl_type(T) ->
    {error, "invalid log type: " ++ T}.

verify_atl_repair("true") ->
    {ok, true};
verify_atl_repair("false") ->
    {ok, false};
verify_atl_repair("truncate") ->
    {ok, truncate};
verify_atl_repair("snmp_repair") ->
    {ok, snmp_repair};
verify_atl_repair(R) ->
    {error, "invalid audit trail log repair: " ++ R}.

verify_atl_seqno("true") ->
    {ok, true};
verify_atl_seqno("false") ->
    {ok, false};
verify_atl_seqno(SN) ->
    {error, "invalid audit trail log seqno: " ++ SN}.


verify_pos_integer(I0) ->
    case (catch list_to_integer(I0)) of
	I when is_integer(I) andalso (I > 0) ->
	    {ok, I};
	_ ->
	    {error, "invalid integer value: " ++ I0}
    end.


verify_netif_req_limit("infinity") ->
    {ok, infinity};
verify_netif_req_limit(I0) ->
    case (catch list_to_integer(I0)) of
	I when is_integer(I) andalso (I > 0) ->
	    {ok, I};
	_ ->
	    {error, "invalid network interface request limit: " ++ I0}
    end.

verify_netif_recbuf(Val) ->
    verify_netif_recbuf_or_sndbuf(Val, "recbuf").

verify_netif_sndbuf(Val) ->
    verify_netif_recbuf_or_sndbuf(Val, "sndbuf").

verify_netif_recbuf_or_sndbuf("default", _) ->
    {ok, default};
verify_netif_recbuf_or_sndbuf(I0, Buf) ->
    case (catch list_to_integer(I0)) of
	I when is_integer(I) andalso (I > 0) ->
	    {ok, I};
	_ ->
	    {error, "invalid network interface " ++ Buf ++ " size: " ++ I0}
    end.


verify_irb("auto") ->
    {ok, auto};
verify_irb("user") ->
    {ok, user};
verify_irb(IRB) ->
    E = lists:flatten(io_lib:format("invalid irb: ~p", [IRB])),
    {error, E}.


verify_irb_user("default") ->
    {ok, default};
verify_irb_user(TO) ->
    case (catch list_to_integer(TO)) of
	I when is_integer(I) andalso (I > 0) ->
	    {ok, I*1000}; % Time is given in seconds
	_ ->
	    {error, "invalid IRB GC time: " ++ TO}
    end.
    

verify_term_disco_behaviour("discovery") ->
    {ok, discovery};
verify_term_disco_behaviour("plain") ->
    {ok, plain};
verify_term_disco_behaviour(B) ->
    {error, "invalid terminating discovery behaviour: " ++ B}.

verify_term_disco_trigger_username("default") ->
    {ok, ""};
verify_term_disco_trigger_username(Trigger) ->
    {ok, Trigger}.


verify_user_id(UserId) when is_list(UserId) ->
    case (catch list_to_atom(UserId)) of
	A when is_atom(A) ->
	    {ok, A};
	_ ->
	    {error, "invalid user id: " ++ UserId}
    end;
verify_user_id(UserId) when is_atom(UserId) ->
    {ok, UserId};
verify_user_id(UserId) ->
    E = lists:flatten(io_lib:format("invalid user id: ~p", [UserId])),
    {error, E}.

verify_user_data("undefined") ->
    {ok, undefined};
verify_user_data(UserData) ->
    {ok, UserData}.


verify_community("\"\"") ->
    {ok, ""};
verify_community(Comm) ->
    {ok, Comm}.


% verify_context_name("\"\"") ->
%     {ok, ""};
% verify_context_name(Ctx) ->
%     {ok, Ctx}.


% verify_mp_model("v1") ->
%     {ok, v1};
% verify_mp_model("v2c") ->
%     {ok, v2c};
% verify_mp_model("v3") ->
%     {ok, v3};
% verify_mp_model(M) ->
%     {error, "invalid mp model: " ++ M}.


verify_sec_model("any") ->
    {ok, any};
verify_sec_model("v1") ->
    {ok, v1};
verify_sec_model("v2c") ->
    {ok, v2c};
verify_sec_model("usm") ->
    {ok, usm};
verify_sec_model(M) ->
    {error, "invalid sec model: " ++ M}.

verify_sec_name("\"initial\"") ->
    {ok, "initial"};
verify_sec_name(N) ->
    {ok, N}.


verify_sec_level("noAuthNoPriv") ->
    {ok, noAuthNoPriv};
verify_sec_level("authNoPriv") ->
    {ok, authNoPriv};
verify_sec_level("authPriv") ->
    {ok, authPriv};
verify_sec_level(L) ->
    {error, "invalid sec level: " ++ L}.


verify_usm_name(Name) ->
    {ok, Name}.

verify_usm_sec_name(Name) ->
    {ok, Name}.


verify_usm_auth_protocol("no") ->
    {ok, usmNoAuthProtocol};
verify_usm_auth_protocol("sha") ->
    {ok, usmHMACSHAAuthProtocol};
verify_usm_auth_protocol("md5") ->
    {ok, usmHMACMD5AuthProtocol};
verify_usm_auth_protocol(AuthP) ->
    {error, "invalid auth protocol: " ++ AuthP}.

verify_usm_auth_sha_key(Key) ->
    verify_usm_key("auth sha", Key, 20).

verify_usm_auth_md5_key(Key) ->
    verify_usm_key("auth md5", Key, 16).

verify_usm_priv_protocol("no") ->
    {ok, usmNoPrivProtocol};
verify_usm_priv_protocol("des") ->
    {ok, usmDESPrivProtocol};
verify_usm_priv_protocol("aes") ->
    {ok, usmAesCfb128Protocol};
verify_usm_priv_protocol(AuthP) ->
    {error, "invalid priv protocol: " ++ AuthP}.

verify_usm_priv_des_key(Key) ->
    verify_usm_key("priv des", Key, 16).

verify_usm_priv_aes_key(Key) ->
    verify_usm_key("priv aes", Key, 16).

verify_usm_key(_What, "\"\"", _ExpectLength) ->
    {ok, ""};
verify_usm_key(_What, Key, ExpectLength) when length(Key) =:= ExpectLength ->
    {ok, Key};
verify_usm_key(What, [$[|RestKey] = Key0, ExpectLength) ->
    case lists:reverse(RestKey) of
	[$]|RevRestKey] ->
	    Key1 = lists:reverse(RevRestKey),
	    verify_usm_key2(What, Key1, ExpectLength);
	_ ->
	    %% Its not a list ([...]) and its not the correct length, ...
	    {error, "invalid " ++ What ++ " key length: " ++ Key0}
    end;
verify_usm_key(What, Key, ExpectLength) ->
    verify_usm_key2(What, Key, ExpectLength).
    
verify_usm_key2(What, Key0, ExpectLength) ->
    case string:tokens(Key0, [$,]) of
	Key when length(Key) =:= ExpectLength ->
	    convert_usm_key(Key, []);
	_ ->
	    {error, "invalid " ++ What ++ " key length: " ++ Key0}
    end.
    
convert_usm_key([], Acc) ->
    {ok, lists:reverse(Acc)};
convert_usm_key([I|Is], Acc) ->
    case (catch list_to_integer(I)) of
	Int when is_integer(Int) ->
	    convert_usm_key(Is, [Int|Acc]);
	_Err ->
	    {error, "invalid key number: " ++ I}
    end.

	     
% ip(Host) ->
%     case catch snmp_misc:ip(Host) of
% 	{ok, IPtuple} -> tuple_to_list(IPtuple);
% 	{error, Reason} -> throw({error, Reason});
% 	_Q -> throw({error, {"ip conversion failed", Host}})
%     end.

% make_ip(Str) ->
%     case catch snmp_misc:ip(Str) of
% 	{ok, IPtuple} -> tuple_to_list(IPtuple);
% 	_Q -> ip(Str)
%     end.


print_q(Q, mandatory) ->
    io:format(Q ++ " ",[]);
print_q(Q, Default) when is_list(Default) ->
    io:format(Q ++ " [~s] ",[Default]).

%% Defval = string() | mandatory
ask(Q, Default, Verify) when is_list(Q) andalso is_function(Verify) ->
    print_q(Q, Default),
    PrelAnsw = io:get_line(''),
    Answer = 
	case remove_newline(PrelAnsw) of
	    "" when Default =/= mandatory -> Default;
	    "" -> ask(Q, Default, Verify);
	    A -> A
    end,
    case (catch Verify(Answer)) of
	{ok, Answer2} ->
	    Answer2;
	{error, ReasonStr} ->
	    i("ERROR: " ++ ReasonStr),
	    ask(Q, Default, Verify)
    end.


host() ->
    case (catch inet:gethostname()) of
	{ok, Name} ->
	    case (catch inet:getaddr(Name, inet)) of
		{ok, Addr} when is_tuple(Addr) ->
		    lists:flatten(
		      io_lib:format("~w.~w.~w.~w", tuple_to_list(Addr)));
		_ ->
		    "127.0.0.1"
	    end;
	_ -> 
	    "127.0.0.1"
    end.

guess_agent_name() ->
    case os:type() of
	{unix, _} ->
	    lists:append(remove_newline(os:cmd("echo $USER")), "'s agent");
	{_,_} -> "my agent"
    end.

guess_engine_name() ->
    case os:type() of
	{unix, _} ->
	    lists:append(remove_newline(os:cmd("echo $USER")), "'s engine");
	{_,_} -> "my engine"
    end.

% guess_user_id() ->
%     case os:type() of
% 	{unix, _} ->
% 	    lists:append(remove_newline(os:cmd("echo $USER")), "'s user");
% 	{_,_} -> "user_id"
%     end.

    
remove_newline(Str) -> 
    lists:delete($\n, Str).


%%======================================================================
%% File generation
%%======================================================================

%%----------------------------------------------------------------------
%% Dir: string()  (ex: "../conf/")
%% ManagerIP, AgentIP: [int(),int(),int(),int()]
%% TrapUdp, AgentUDP: integer()
%% SysName: string()
%%----------------------------------------------------------------------
write_agent_snmp_files(Dir, Vsns, ManagerIP, TrapUdp, 
		       AgentIP, AgentUDP, SysName) 
  when is_list(Dir) andalso 
       is_list(Vsns) andalso 
       is_list(ManagerIP) andalso 
       is_integer(TrapUdp) andalso 
       is_list(AgentIP) andalso 
       is_integer(AgentUDP) andalso 
       is_list(SysName) ->
    write_agent_snmp_files(Dir, Vsns, ManagerIP, TrapUdp, AgentIP, AgentUDP,
			   SysName, "trap", none, "", "agentEngine", 484).

%% 
%% ----- Agent config files generator functions -----
%% 

write_agent_snmp_files(Dir, Vsns, ManagerIP, TrapUdp, AgentIP, AgentUDP, 
		       SysName, NotifType, SecType, Passwd, EngineID, MMS) ->
    write_agent_snmp_conf(Dir, AgentIP, AgentUDP, EngineID, MMS),
    write_agent_snmp_context_conf(Dir),
    write_agent_snmp_community_conf(Dir),
    write_agent_snmp_standard_conf(Dir, SysName),
    write_agent_snmp_target_addr_conf(Dir, ManagerIP, TrapUdp, Vsns),
    write_agent_snmp_target_params_conf(Dir, Vsns),
    write_agent_snmp_notify_conf(Dir, NotifType),
    write_agent_snmp_usm_conf(Dir, Vsns, EngineID, SecType, Passwd),
    write_agent_snmp_vacm_conf(Dir, Vsns, SecType),
    ok.


%% 
%% ------ [agent] agent.conf ------
%% 

write_agent_snmp_conf(Dir, AgentIP, AgentUDP, EngineID, MMS) -> 
    Comment = 
"%% This file defines the Agent local configuration info\n"
"%% The data is inserted into the snmpEngine* variables defined\n"
"%% in SNMP-FRAMEWORK-MIB, and the intAgent* variables defined\n"
"%% in OTP-SNMPEA-MIB.\n"
"%% Each row is a 2-tuple:\n"
"%% {AgentVariable, Value}.\n"
"%% For example\n"
"%% {intAgentUDPPort, 4000}.\n"
"%% The ip address for the agent is sent as id in traps.\n"
"%% {intAgentIpAddress, [127,42,17,5]}.\n"
"%% {snmpEngineID, \"agentEngine\"}.\n"
"%% {snmpEngineMaxMessageSize, 484}.\n"
"%%\n\n",
    Hdr = header() ++ Comment, 
    Conf = [{intAgentUDPPort,          AgentUDP}, 
	    {intAgentIpAddress,        AgentIP},
	    {snmpEngineID,             EngineID},
	    {snmpEngineMaxMessageSize, MMS}],
    write_agent_config(Dir, Hdr, Conf).

write_agent_config(Dir, Hdr, Conf) ->
    snmpa_conf:write_agent_config(Dir, Hdr, Conf).
    
update_agent_config(Dir, Conf) ->
    snmpa_conf:append_agent_config(Dir, Conf).
    

%% 
%% ------ [agent] context.conf ------
%% 

write_agent_snmp_context_conf(Dir) ->
    Comment = 
"%% This file defines the contexts known to the agent.\n"
"%% The data is inserted into the vacmContextTable defined\n"
"%% in SNMP-VIEW-BASED-ACM-MIB.\n"
"%% Each row is a string:\n"
"%% ContextName.\n"
"%%\n"
"%% The empty string is the default context.\n"
"%% For example\n"
"%% \"bridge1\".\n"
"%% \"bridge2\".\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    Conf = [""],
    write_agent_context_config(Dir, Hdr, Conf).

write_agent_context_config(Dir, Hdr, Conf) ->
    snmpa_conf:write_context_config(Dir, Hdr, Conf).

update_agent_context_config(Dir, Conf) ->
    snmpa_conf:append_context_config(Dir, Conf).

    
%% 
%% ------ community.conf ------
%% 

write_agent_snmp_community_conf(Dir) ->
    Comment = 
"%% This file defines the community info which maps to VACM parameters.\n"
"%% The data is inserted into the snmpCommunityTable defined\n"
"%% in SNMP-COMMUNITY-MIB.\n"
"%% Each row is a 5-tuple:\n"
"%% {CommunityIndex, CommunityName, SecurityName, ContextName, TransportTag}.\n"
"%% For example\n"
"%% {\"1\", \"public\", \"initial\", \"\", \"\"}.\n"
"%% {\"2\", \"secret\", \"secret_name\", \"\", \"tag\"}.\n"
"%% {\"3\", \"bridge1\", \"initial\", \"bridge1\", \"\"}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    Conf = [{"public", "public", "initial", "", ""}, 
	    {"all-rights", "all-rights", "all-rights", "", ""}, 
	    {"standard trap", "standard trap", "initial", "", ""}], 
    write_agent_community_config(Dir, Hdr, Conf).

write_agent_community_config(Dir, Hdr, Conf) ->
    snmpa_conf:write_community_config(Dir, Hdr, Conf).

update_agent_community_config(Dir, Conf) ->
    snmpa_conf:append_community_config(Dir, Conf).
    

%% 
%% ------ standard.conf ------
%% 

write_agent_snmp_standard_conf(Dir, SysName) ->
    Comment = 
"%% This file defines the STANDARD-MIB info.\n"
"%% Each row is a 2-tuple:\n"
"%% {StandardVariable, Value}.\n"
"%% For example\n"
"%% {sysDescr, \"Erlang SNMP agent\"}.\n"
"%% {sysObjectID, [1,2,3]}.\n"
"%% {sysContact, \"{mbj,eklas}@erlang.ericsson.se\"}.\n"
"%% {sysName, \"test\"}.\n"
"%% {sysLocation, \"erlang\"}.\n"
"%% {sysServices, 72}.\n"
"%% {snmpEnableAuthenTraps, enabled}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    Conf = [{sysDescr,              "Erlang SNMP agent"},
	    {sysObjectID,           [1,2,3]},
	    {sysContact,            "{mbj,eklas}@erlang.ericsson.se"},
	    {sysLocation,           "erlang"}, 
	    {sysServices,           72}, 
	    {snmpEnableAuthenTraps, enabled},
	    {sysName,               SysName}],
    write_agent_standard_config(Dir, Hdr, Conf).

write_agent_standard_config(Dir, Hdr, Conf) ->
    snmpa_conf:write_standard_config(Dir, Hdr, Conf).

update_agent_standard_config(Dir, Conf) ->
    snmpa_conf:append_standard_config(Dir, Conf).


%% 
%% ------ target_addr.conf ------
%% 

write_agent_snmp_target_addr_conf(Dir, ManagerIp, UDP, Vsns) -> 
    Timeout    = 1500, 
    RetryCount = 3, 
    write_agent_snmp_target_addr_conf(Dir, ManagerIp, UDP, 
				      Timeout, RetryCount, 
				      Vsns).

write_agent_snmp_target_addr_conf(Dir, ManagerIp, UDP, 
				  Timeout, RetryCount, 
				  Vsns) -> 
    Comment = 
"%% This file defines the target address parameters.\n"
"%% The data is inserted into the snmpTargetAddrTable defined\n"
"%% in SNMP-TARGET-MIB, and in the snmpTargetAddrExtTable defined\n"
"%% in SNMP-COMMUNITY-MIB.\n"
"%% Each row is a 10-tuple:\n"
"%% {Name, Ip, Udp, Timeout, RetryCount, TagList, ParamsName, EngineId,\n"
"%%        TMask, MaxMessageSize}.\n"
"%% The EngineId value is only used if Inform-Requests are sent to this\n"
"%% target.  If Informs are not sent, this value is ignored, and can be\n"
"%% e.g. an empty string.  However, if Informs are sent, it is essential\n"
"%% that the value of EngineId matches the value of the target's\n"
"%% actual snmpEngineID.\n"
"%% For example\n"
"%% {\"1.2.3.4 v1\", [1,2,3,4], 162, \n"
"%%  1500, 3, \"std_inform\", \"otp_v2\", \"\",\n"
"%%  [127,0,0,0],  2048}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    F = fun(v1 = Vsn, Acc) ->
		[{mk_ip(ManagerIp, Vsn), 
		  ManagerIp, UDP, Timeout, RetryCount, 
		  "std_trap", mk_param(Vsn), "", [], 2048}| Acc];
	   (v2 = Vsn, Acc) ->
		[{mk_ip(ManagerIp, Vsn), 
		  ManagerIp, UDP, Timeout, RetryCount, 
		  "std_trap", mk_param(Vsn), "", [], 2048},
		 {lists:flatten(io_lib:format("~s.2",[mk_ip(ManagerIp, Vsn)])),
		  ManagerIp, UDP, Timeout, RetryCount, 
		  "std_inform", mk_param(Vsn), "", [], 2048}| Acc];
	   (v3 = Vsn, Acc) ->
		[{mk_ip(ManagerIp, Vsn), 
		  ManagerIp, UDP, Timeout, RetryCount, 
		  "std_trap", mk_param(Vsn), "", [], 2048},
		 {lists:flatten(io_lib:format("~s.3",[mk_ip(ManagerIp, Vsn)])),
		  ManagerIp, UDP, Timeout, RetryCount, 
		  "std_inform", mk_param(Vsn), "mgrEngine", [], 2048}| Acc]
	end,
    Conf = lists:foldl(F, [], Vsns),
    write_agent_target_addr_config(Dir, Hdr, Conf).

mk_param(Vsn) ->
    lists:flatten(io_lib:format("target_~w", [Vsn])).

mk_ip([A,B,C,D], Vsn) ->
    lists:flatten(io_lib:format("~w.~w.~w.~w ~w", [A,B,C,D,Vsn])).

write_agent_target_addr_config(Dir, Hdr, Conf) ->
    snmpa_conf:write_target_addr_config(Dir, Hdr, Conf).

update_agent_target_addr_config(Dir, Conf) ->
    snmpa_conf:append_target_addr_config(Dir, Conf).


%% 
%% ------ target_params.conf ------
%% 

write_agent_snmp_target_params_conf(Dir, Vsns) -> 
    Comment = 
"%% This file defines the target parameters.\n"
"%% The data is inserted into the snmpTargetParamsTable defined\n"
"%% in SNMP-TARGET-MIB.\n"
"%% Each row is a 5-tuple:\n"
"%% {Name, MPModel, SecurityModel, SecurityName, SecurityLevel}.\n"
"%% For example\n"
"%% {\"target_v3\", v3, usm, \"\", noAuthNoPriv}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    Conf = [fun(V) ->
		    MP = if V == v1 -> v1;
			    V == v2 -> v2c;
			    V == v3 -> v3
			 end,
		    SM = if V == v1 -> v1;
			    V == v2 -> v2c;
			    V == v3 -> usm
			 end,
		    Name = lists:flatten(
			     io_lib:format("target_~w", [V])),
		    {Name, MP, SM, "initial", noAuthNoPriv}
	    end(Vsn) || Vsn <- Vsns],
    write_agent_target_params_config(Dir, Hdr, Conf).

write_agent_target_params_config(Dir, Hdr, Conf) ->
    snmpa_conf:write_target_params_config(Dir, Hdr, Conf).

update_agent_target_params_config(Dir, Conf) ->
    snmpa_conf:append_target_params_config(Dir, Conf).


%% 
%% ------ notify.conf ------
%% 

write_agent_snmp_notify_conf(Dir, NotifyType) -> 
    Comment = 
"%% This file defines the notification parameters.\n"
"%% The data is inserted into the snmpNotifyTable defined\n"
"%% in SNMP-NOTIFICATION-MIB.\n"
"%% The Name is used as CommunityString for v1 and v2c.\n"
"%% Each row is a 3-tuple:\n"
"%% {Name, Tag, Type}.\n"
"%% For example\n"
"%% {\"standard trap\", \"std_trap\", trap}.\n"
"%% {\"standard inform\", \"std_inform\", inform}.\n"
"%%\n\n",
    Hdr = header() ++ Comment, 
    Conf = [{"standard trap", "std_trap", NotifyType}],
    write_agent_notify_config(Dir, Hdr, Conf).

write_agent_notify_config(Dir, Hdr, Conf) ->
    snmpa_conf:write_notify_config(Dir, Hdr, Conf).

update_agent_notify_config(Dir, Conf) ->
    snmpa_conf:append_notify_config(Dir, Conf).


%% 
%% ------ usm.conf ------
%% 

write_agent_snmp_usm_conf(Dir, Vsns, EngineID, SecType, Passwd) -> 
    case lists:member(v3, Vsns) of
	false -> ok;
	true -> write_agent_snmp_usm_conf(Dir, EngineID, SecType, Passwd)
    end.

write_agent_snmp_usm_conf(Dir, EngineID, SecType, Passwd) -> 
    Comment = 
"%% This file defines the security parameters for the user-based\n"
"%% security model.\n"
"%% The data is inserted into the usmUserTable defined\n"
"%% in SNMP-USER-BASED-SM-MIB.\n"
"%% Each row is a 14-tuple:\n"
"%% {EngineID, UserName, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC,\n"
"%%  PrivP, PrivKeyC, OwnPrivKeyC, Public, AuthKey, PrivKey}.\n"
"%% For example\n"
"%% {\"agentEngine\", \"initial\", \"initial\", zeroDotZero,\n"
"%%  usmNoAuthProtocol, \"\", \"\", usmNoPrivProtocol, \"\", \"\", \"\",\n"
"%%  \"\", \"\"}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    Conf = write_agent_snmp_usm_conf2(EngineID, SecType, Passwd),
    write_agent_usm_config(Dir, Hdr, Conf).

write_agent_snmp_usm_conf2(EngineID, none, _Passwd) ->
    [{EngineID, "initial", "initial", zeroDotZero, 
      usmNoAuthProtocol, "", "", 
      usmNoPrivProtocol, "", "", 
      "", "", ""}];
write_agent_snmp_usm_conf2(EngineID, SecType, Passwd) ->
    Secret16 = agent_snmp_mk_secret(md5, Passwd, EngineID),
    Secret20 = agent_snmp_mk_secret(sha, Passwd, EngineID),
    {PrivProt, PrivSecret} = 
	case SecType of
	    minimum ->
		{usmNoPrivProtocol,    ""};
	    {semi, des} ->
		{usmDESPrivProtocol,   Secret16};
	    {semi, aes} ->
		{usmAesCfb128Protocol, Secret16}
	end,
    [{EngineID, "initial", "initial", zeroDotZero, 
      usmHMACMD5AuthProtocol, "", "", 
      PrivProt, "", "", 
      "", Secret16, PrivSecret},
     
     {EngineID, "templateMD5", "templateMD5", zeroDotZero, 
      usmHMACMD5AuthProtocol, "", "", 
      PrivProt, "", "", 
      "", Secret16, PrivSecret}, 

     {EngineID, "templateSHA", "templateSHA", zeroDotZero, 
      usmHMACSHAAuthProtocol, "", "", 
      PrivProt, "", "", 
      "", Secret20, PrivSecret}].

write_agent_usm_config(Dir, Hdr, Conf) ->
    snmpa_conf:write_usm_config(Dir, Hdr, Conf).

update_agent_usm_config(Dir, Conf) ->
    snmpa_conf:append_usm_config(Dir, Conf).


%% 
%% ------ vacm.conf ------
%% 

write_agent_snmp_vacm_conf(Dir, Vsns, SecType) ->
    Comment = 
"%% This file defines the Mib Views.\n"
"%% The data is inserted into the vacm* tables defined\n"
"%% in SNMP-VIEW-BASED-ACM-MIB.\n"
"%% Each row is one of 3 tuples; one for each table in the MIB:\n"
"%% {vacmSecurityToGroup, SecModel, SecName, GroupName}.\n"
"%% {vacmAccess, GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV}.\n"
"%% {vacmViewTreeFamily, ViewIndex, ViewSubtree, ViewStatus, ViewMask}.\n"
"%% For example\n"
"%% {vacmSecurityToGroup, v2c, \"initial\", \"initial\"}.\n"
"%% {vacmSecurityToGroup, usm, \"initial\", \"initial\"}.\n"
"%%  read/notify access to system\n"
"%% {vacmAccess, \"initial\", \"\", any, noAuthNoPriv, exact,\n"
"%%              \"system\", \"\", \"system\"}.\n"
"%% {vacmViewTreeFamily, \"system\", [1,3,6,1,2,1,1], included, null}.\n"
"%% {vacmViewTreeFamily, \"exmib\", [1,3,6,1,3], included, null}."
" % for EX1-MIB\n"
"%% {vacmViewTreeFamily, \"internet\", [1,3,6,1], included, null}.\n"
"%%\n\n",
    Hdr = lists:flatten(header()) ++ Comment,
    Groups = 
	lists:foldl(
	  fun(V, Acc) ->
		  [{vacmSecurityToGroup, vacm_ver(V), 
		    "initial",    "initial"},
		   {vacmSecurityToGroup, vacm_ver(V), 
		    "all-rights", "all-rights"}|
		   Acc]
	  end, [], Vsns),
    Acc = 
	[{vacmAccess, "initial", "", any, noAuthNoPriv, exact, 
	  "restricted", "", "restricted"}, 
	 {vacmAccess, "initial", "", usm, authNoPriv, exact, 
	  "internet", "internet", "internet"}, 
	 {vacmAccess, "initial", "", usm, authPriv, exact, 
	  "internet", "internet", "internet"}, 
	 {vacmAccess, "all-rights", "", any, noAuthNoPriv, exact, 
	  "internet", "internet", "internet"}],
    VTF0 = 
	case SecType of
	    none ->
		[{vacmViewTreeFamily, 
		  "restricted", [1,3,6,1], included, null}];
	    minimum ->
		[{vacmViewTreeFamily, 
		  "restricted", [1,3,6,1], included, null}];
	    {semi, _} ->
		[{vacmViewTreeFamily, 
		  "restricted", [1,3,6,1,2,1,1], included, null},
		 {vacmViewTreeFamily, 
		  "restricted", [1,3,6,1,2,1,11], included, null},
		 {vacmViewTreeFamily, 
		  "restricted", [1,3,6,1,6,3,10,2,1], included, null},
		 {vacmViewTreeFamily, 
		  "restricted", [1,3,6,1,6,3,11,2,1], included, null},
		 {vacmViewTreeFamily, 
		  "restricted", [1,3,6,1,6,3,15,1,1], included, null}]
	end,
    VTF = VTF0 ++ [{vacmViewTreeFamily,"internet",[1,3,6,1],included,null}],
    write_agent_vacm_config(Dir, Hdr, Groups ++ Acc ++ VTF).

vacm_ver(v1) -> v1;
vacm_ver(v2) -> v2c;
vacm_ver(v3) -> usm.
     
write_agent_vacm_config(Dir, Hdr, Conf) ->
    snmpa_conf:write_vacm_config(Dir, Hdr, Conf).

update_agent_vacm_config(Dir, Conf) ->
    snmpa_conf:append_vacm_config(Dir, Conf).


%% 
%% ----- Manager config files generator functions -----
%% 

write_manager_snmp_files(Dir, IP, Port, MMS, EngineID, 
			 Users, Agents, Usms) ->
    write_manager_snmp_conf(Dir, IP, Port, MMS, EngineID),
    write_manager_snmp_users_conf(Dir, Users),
    write_manager_snmp_agents_conf(Dir, Agents),
    write_manager_snmp_usm_conf(Dir, Usms),  
    ok.


%% 
%% ------ manager.conf ------
%% 

write_manager_snmp_conf(Dir, IP, Port, MMS, EngineID) -> 
    Comment = 
"%% This file defines the Manager local configuration info\n"
"%% Each row is a 2-tuple:\n"
"%% {Variable, Value}.\n"
"%% For example\n"
"%% {port,             5000}.\n"
"%% {address,          [127,42,17,5]}.\n"
"%% {engine_id,        \"managerEngine\"}.\n"
"%% {max_message_size, 484}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    Conf = [{port,             Port}, 
            {address,          IP}, 
	    {engine_id,        EngineID}, 
	    {max_message_size, MMS}], 
    write_manager_config(Dir, Hdr, Conf).

write_manager_config(Dir, Hdr, Conf) ->
    snmpm_conf:write_manager_config(Dir, Hdr, Conf).
    
update_manager_config(Dir, Conf) ->
    snmpm_conf:append_manager_config(Dir, Conf).
    

%% 
%% ------ users.conf ------
%% 

write_manager_snmp_users_conf(Dir, Users) ->
    Comment = 
"%% This file defines the users the manager handles\n"
"%% Each row is a 3-tuple:\n"
"%% {UserId, UserMod, UserData}.\n"
"%% For example\n"
"%% {kalle, kalle_callback_user_mod, \"dummy\"}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_manager_users_config(Dir, Hdr, Users).

write_manager_users_config(Dir, Hdr, Users) ->
    snmpm_conf:write_users_config(Dir, Hdr, Users).

update_manager_users_config(Dir, Users) ->
    snmpm_conf:append_users_config(Dir, Users).


%% 
%% ------ agents.conf ------
%% 

write_manager_snmp_agents_conf(Dir, Agents) ->
    Comment = 
"%% This file defines the agents the manager handles\n"
"%% Each row is a 12-tuple:\n"
"%% {UserId, \n"
"%%  TargetName, Comm, Ip, Port, EngineID, Timeout, \n"
"%%  MaxMessageSize, Version, SecModel, SecName, SecLevel}\n"
"%%\n\n",
    Hdr = header() ++ Comment, 
    write_manager_agents_config(Dir, Hdr, Agents).

write_manager_agents_config(Dir, Hdr, Agents) ->
    snmpm_conf:write_agents_config(Dir, Hdr, Agents).

update_manager_agents_config(Dir, Agents) ->
    snmpm_conf:append_agents_config(Dir, Agents).


%% 
%% ------ usm.conf -----
%% 

write_manager_snmp_usm_conf(Dir, Usms) ->
    Comment = 
"%% This file defines the usm users the manager handles\n"
"%% Each row is a 6 or 7-tuple:\n"
"%% {EngineID, UserName, AuthP, AuthKey, PrivP, PrivKey}\n"
"%% {EngineID, UserName, SecName, AuthP, AuthKey, PrivP, PrivKey}\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_manager_usm_config(Dir, Hdr, Usms).

write_manager_usm_config(Dir, Hdr, Usms) ->
    snmpm_conf:write_usm_config(Dir, Hdr, Usms).

update_manager_usm_config(Dir, Usms) ->
    snmpm_conf:append_usm_config(Dir, Usms).


%% 
%% -------------------------------------------------------------------------
%% 

write_sys_config_file(Dir, Services) ->
    {ok, Fid} = file:open(filename:join(Dir,"sys.config"), [write]),
    ok = io:format(Fid, "~s", [header()]),
    ok = io:format(Fid, "[{snmp, ~n", []),
    ok = io:format(Fid, "  [~n", []),
    write_sys_config_file_services(Fid, Services),
    ok = io:format(Fid, "  ]~n", []),
    ok = io:format(Fid, " }~n", []),
    ok = io:format(Fid, "].~n", []),
    ok.

write_sys_config_file_services(Fid, [Service]) ->
    write_sys_config_file_service(Fid, Service),
    ok = io:format(Fid, "~n", []),
    ok;
write_sys_config_file_services(Fid, [Service|Services]) ->
    write_sys_config_file_service(Fid, Service),
    ok = io:format(Fid, ", ~n", []),
    write_sys_config_file_services(Fid, Services).

write_sys_config_file_service(Fid, {Service, Opts}) ->
    ok = io:format(Fid, "   {~w,~n", [Service]),
    ok = io:format(Fid, "    [~n", []),
    write_sys_config_file_service_opts(Fid, Service, Opts),
    ok = io:format(Fid, "    ]~n", []),
    ok = io:format(Fid, "   }", []),
    true.

write_sys_config_file_service_opts(Fid, agent, Opts) ->
    write_sys_config_file_agent_opts(Fid, Opts);
write_sys_config_file_service_opts(Fid, manager, Opts) ->
    write_sys_config_file_manager_opts(Fid, Opts).


write_sys_config_file_agent_opts(Fid, [Opt]) ->
    write_sys_config_file_agent_opt(Fid, Opt),
    ok = io:format(Fid, "~n", []),
    ok;
write_sys_config_file_agent_opts(Fid, [Opt|Opts]) ->
    write_sys_config_file_agent_opt(Fid, Opt),
    ok = io:format(Fid, ", ~n", []),
    write_sys_config_file_agent_opts(Fid, Opts).


write_sys_config_file_agent_opt(Fid, {mibs, []}) ->
    ok = io:format(Fid, "     {mibs, []}", []);
write_sys_config_file_agent_opt(Fid, {priority, Prio}) ->
    ok = io:format(Fid, "     {priority, ~w}", [Prio]);
write_sys_config_file_agent_opt(Fid, {error_report_mod, Mod}) ->
    ok = io:format(Fid, "     {error_report_mod, ~w}", [Mod]);
write_sys_config_file_agent_opt(Fid, {versions, Vsns}) ->
    ok = io:format(Fid, "     {versions, ~w}", [Vsns]);
write_sys_config_file_agent_opt(Fid, {multi_threaded, B}) ->
    ok = io:format(Fid, "     {multi_threaded, ~w}", [B]);
write_sys_config_file_agent_opt(Fid, {config, Opts}) ->
    ok = io:format(Fid, "     {config, [", []),
    write_sys_config_file_agent_config_opts(Fid, Opts),
    ok = io:format(Fid, "}", []);
write_sys_config_file_agent_opt(Fid, {db_dir, Dir}) ->
    ok = io:format(Fid, "     {db_dir, \"~s\"}", [Dir]);
write_sys_config_file_agent_opt(Fid, {mib_storage, ets}) ->
    ok = io:format(Fid, "     {mib_storage, ets}", []);
write_sys_config_file_agent_opt(Fid, {mib_storage, {dets, Dir}}) ->
    ok = io:format(Fid, "     {mib_storage, {dets, \"~s\"}}", [Dir]);
write_sys_config_file_agent_opt(Fid, {mib_storage, {dets, Dir, Act}}) ->
    ok = io:format(Fid, "     {mib_storage, {dets, \"~s\", ~w}}", 
		   [Dir, Act]);
write_sys_config_file_agent_opt(Fid, {mib_storage, {mnesia, Nodes}}) ->
    ok = io:format(Fid, "     {mib_storage, {mnesia, ~w}}", [Nodes]);
write_sys_config_file_agent_opt(Fid, {mib_storage, {mnesia, Nodes, Act}}) ->
    ok = io:format(Fid, "     {mib_storage, {mnesia, ~w, ~w}}", 
		   [Nodes, Act]);
write_sys_config_file_agent_opt(Fid, {target_cache, Opts}) ->
    ok = io:format(Fid, "     {target_cache, ~w}", [Opts]);
write_sys_config_file_agent_opt(Fid, {local_db, Opts}) ->
    ok = io:format(Fid, "     {local_db, ~w}", [Opts]);
write_sys_config_file_agent_opt(Fid, {note_store, Opts}) ->
    ok = io:format(Fid, "     {note_store, ~w}", [Opts]);
write_sys_config_file_agent_opt(Fid, {symbolic_store, Opts}) ->
    ok = io:format(Fid, "     {symbolic_store, ~w}", [Opts]);
write_sys_config_file_agent_opt(Fid, {agent_type, Type}) ->
    ok = io:format(Fid, "     {agent_type, ~w}", [Type]);
write_sys_config_file_agent_opt(Fid, {agent_verbosity, Verb}) ->
    ok = io:format(Fid, "     {agent_verbosity, ~w}", [Verb]);
write_sys_config_file_agent_opt(Fid, {audit_trail_log, Opts}) ->
    ok = io:format(Fid, "     {audit_trail_log, [", []),
    write_sys_config_file_agent_atl_opts(Fid, Opts),
    ok = io:format(Fid, "}", []);
write_sys_config_file_agent_opt(Fid, {discovery, Opts}) ->
    ok = io:format(Fid, "     {discovery, [", []),
    write_sys_config_file_agent_disco_opts(Fid, Opts),
    ok = io:format(Fid, "}", []);
write_sys_config_file_agent_opt(Fid, {net_if, Opts}) ->
    ok = io:format(Fid, "     {net_if, ~w}", [Opts]);
write_sys_config_file_agent_opt(Fid, {mib_server, Opts}) ->
    ok = io:format(Fid, "     {mib_server, ~w}", [Opts]);
write_sys_config_file_agent_opt(Fid, {Key, Val}) ->
    ok = io:format(Fid, "     {~w, ~w}", [Key, Val]).
    

%% Mandatory option dir, means that this is never empty:
write_sys_config_file_agent_config_opts(Fid, [Opt]) ->
    write_sys_config_file_agent_config_opt(Fid, Opt),
    ok = io:format(Fid, "]", []),
    ok;
write_sys_config_file_agent_config_opts(Fid, [Opt|Opts]) ->
    write_sys_config_file_agent_config_opt(Fid, Opt),
    ok = io:format(Fid, ", ", []),
    write_sys_config_file_agent_config_opts(Fid, Opts).
    
write_sys_config_file_agent_config_opt(Fid, {dir, Dir}) ->
    ok = io:format(Fid, "{dir, \"~s\"}", [Dir]);
write_sys_config_file_agent_config_opt(Fid, {force_load, Bool}) ->
    ok = io:format(Fid, "{force_load, ~w}", [Bool]);
write_sys_config_file_agent_config_opt(Fid, {verbosity, Verb}) ->
    ok = io:format(Fid, "{verbosity, ~w}", [Verb]).


%% This is only present if there is atleast one option
write_sys_config_file_agent_atl_opts(Fid, [Opt]) ->
    write_sys_config_file_agent_atl_opt(Fid, Opt),
    ok = io:format(Fid, "]", []),
    ok;
write_sys_config_file_agent_atl_opts(Fid, [Opt|Opts]) ->
    write_sys_config_file_agent_atl_opt(Fid, Opt),
    ok = io:format(Fid, ", ", []),
    write_sys_config_file_agent_atl_opts(Fid, Opts).
    
write_sys_config_file_agent_atl_opt(Fid, {dir, Dir}) ->
    ok = io:format(Fid, "{dir, \"~s\"}", [Dir]);
write_sys_config_file_agent_atl_opt(Fid, {type, Type}) ->
    ok = io:format(Fid, "{type, ~w}", [Type]);
write_sys_config_file_agent_atl_opt(Fid, {size, Size}) ->
    ok = io:format(Fid, "{size, ~w}", [Size]);
write_sys_config_file_agent_atl_opt(Fid, {repair, Rep}) ->
    ok = io:format(Fid, "{repair, ~w}", [Rep]);
write_sys_config_file_agent_atl_opt(Fid, {seqno, SeqNo}) ->
    ok = io:format(Fid, "{seqno, ~w}", [SeqNo]).


%% These options are allways there
write_sys_config_file_agent_disco_opts(Fid, [Opt]) ->
    write_sys_config_file_agent_disco_opt(Fid, Opt),
    ok = io:format(Fid, "]", []),
    ok;
write_sys_config_file_agent_disco_opts(Fid, [Opt|Opts]) ->
    write_sys_config_file_agent_disco_opt(Fid, Opt),
    ok = io:format(Fid, ", ", []),
    write_sys_config_file_agent_disco_opts(Fid, Opts).
    
write_sys_config_file_agent_disco_opt(Fid, {terminating, Opts}) ->
    ok = io:format(Fid, "{terminating, [", []),
    write_sys_config_file_agent_term_disco_opts(Fid, Opts),
    ok = io:format(Fid, "}", []);
write_sys_config_file_agent_disco_opt(Fid, {originating, Opts}) ->
    ok = io:format(Fid, "{originating, [", []),
    write_sys_config_file_agent_orig_disco_opts(Fid, Opts),
    ok = io:format(Fid, "}", []).

write_sys_config_file_agent_term_disco_opts(Fid, [Opt]) ->
    write_sys_config_file_agent_term_disco_opt(Fid, Opt),
    ok = io:format(Fid, "]", []),
    ok;
write_sys_config_file_agent_term_disco_opts(Fid, [Opt|Opts]) ->
    write_sys_config_file_agent_term_disco_opt(Fid, Opt),
    ok = io:format(Fid, ", ", []),
    write_sys_config_file_agent_term_disco_opts(Fid, Opts).
    
write_sys_config_file_agent_term_disco_opt(Fid, {enable, Enable}) ->
    ok = io:format(Fid, "{enable, ~w}", [Enable]);
write_sys_config_file_agent_term_disco_opt(Fid, {stage2, Stage2}) ->
    ok = io:format(Fid, "{stage2, ~w}", [Stage2]);
write_sys_config_file_agent_term_disco_opt(Fid, {trigger_username, Trigger}) ->
    ok = io:format(Fid, "{trigger_username, \"~s\"}", [Trigger]).

write_sys_config_file_agent_orig_disco_opts(Fid, [Opt]) ->
    write_sys_config_file_agent_orig_disco_opt(Fid, Opt),
    ok = io:format(Fid, "]", []),
    ok;
write_sys_config_file_agent_orig_disco_opts(Fid, [Opt|Opts]) ->
    write_sys_config_file_agent_orig_disco_opt(Fid, Opt),
    ok = io:format(Fid, ", ", []),
    write_sys_config_file_agent_orig_disco_opts(Fid, Opts).
    
write_sys_config_file_agent_orig_disco_opt(Fid, {enable, Enable}) ->
    ok = io:format(Fid, "{enable, ~w}", [Enable]).



write_sys_config_file_manager_opts(Fid, [Opt]) ->
    write_sys_config_file_manager_opt(Fid, Opt),
    ok = io:format(Fid, "~n", []),
    ok;
write_sys_config_file_manager_opts(Fid, [Opt|Opts]) ->
    write_sys_config_file_manager_opt(Fid, Opt),
    ok = io:format(Fid, ", ~n", []),
    write_sys_config_file_manager_opts(Fid, Opts).


write_sys_config_file_manager_opt(Fid, {mibs, []}) ->
    ok = io:format(Fid, "     {mibs, []}", []);
write_sys_config_file_manager_opt(Fid, {priority, Prio}) ->
    ok = io:format(Fid, "     {priority, ~w}", [Prio]);
write_sys_config_file_manager_opt(Fid, {versions, Vsns}) ->
    ok = io:format(Fid, "     {versions, ~w}", [Vsns]);
write_sys_config_file_manager_opt(Fid, {config, Opts}) ->
    ok = io:format(Fid, "     {config, [", []),
    write_sys_config_file_manager_config_opts(Fid, Opts),
    ok = io:format(Fid, "}", []);
write_sys_config_file_manager_opt(Fid, {server, Opts}) ->
    ok = io:format(Fid, "     {server, ~w}", [Opts]);
write_sys_config_file_manager_opt(Fid, {note_store, Opts}) ->
    ok = io:format(Fid, "     {note_store, ~w}", [Opts]);
write_sys_config_file_manager_opt(Fid, {audit_trail_log, Opts}) ->
    ok = io:format(Fid, "     {audit_trail_log, [", []),
    write_sys_config_file_manager_atl_opts(Fid, Opts),
    ok = io:format(Fid, "}", []);
write_sys_config_file_manager_opt(Fid, {net_if, Opts}) ->
    ok = io:format(Fid, "     {net_if, ~w}", [Opts]);
write_sys_config_file_manager_opt(Fid, {Key, Val}) ->
    ok = io:format(Fid, "     {~w, ~w}", [Key, Val]).
    
%% Mandatory option dir, means that this is never empty:
write_sys_config_file_manager_config_opts(Fid, [Opt]) ->
    write_sys_config_file_manager_config_opt(Fid, Opt),
    ok = io:format(Fid, "]", []),
    ok;
write_sys_config_file_manager_config_opts(Fid, [Opt|Opts]) ->
    write_sys_config_file_manager_config_opt(Fid, Opt),
    ok = io:format(Fid, ", ", []),
    write_sys_config_file_manager_config_opts(Fid, Opts).
    
write_sys_config_file_manager_config_opt(Fid, {dir, Dir}) ->
    ok = io:format(Fid, "{dir, \"~s\"}", [Dir]);
write_sys_config_file_manager_config_opt(Fid, {db_dir, Dir}) ->
    ok = io:format(Fid, "{db_dir, \"~s\"}", [Dir]);
write_sys_config_file_manager_config_opt(Fid, {repair, Rep}) ->
    ok = io:format(Fid, "{repair, ~w}", [Rep]);
write_sys_config_file_manager_config_opt(Fid, {auto_save, As}) ->
    ok = io:format(Fid, "{auto_save, ~w}", [As]);
write_sys_config_file_manager_config_opt(Fid, {verbosity, Verb}) ->
    ok = io:format(Fid, "{verbosity, ~w}", [Verb]).


%% This is only present if there is atleast one option
write_sys_config_file_manager_atl_opts(Fid, [Opt]) ->
    write_sys_config_file_manager_atl_opt(Fid, Opt),
    ok = io:format(Fid, "]", []),
    ok;
write_sys_config_file_manager_atl_opts(Fid, [Opt|Opts]) ->
    write_sys_config_file_manager_atl_opt(Fid, Opt),
    ok = io:format(Fid, ", ", []),
    write_sys_config_file_manager_atl_opts(Fid, Opts).
    
write_sys_config_file_manager_atl_opt(Fid, {dir, Dir}) ->
    ok = io:format(Fid, "{dir, \"~s\"}", [Dir]);
write_sys_config_file_manager_atl_opt(Fid, {type, Type}) ->
    ok = io:format(Fid, "{type, ~w}", [Type]);
write_sys_config_file_manager_atl_opt(Fid, {size, Size}) ->
    ok = io:format(Fid, "{size, ~w}", [Size]);
write_sys_config_file_manager_atl_opt(Fid, {repair, Rep}) ->
    ok = io:format(Fid, "{repair, ~w}", [Rep]).


header() ->
    {Y,Mo,D} = date(),
    {H,Mi,S} = time(),
    io_lib:format("%% This file was generated by "
		  "snmp_config (version-~s) ~w-~2.2.0w-~2.2.0w "
		  "~2.2.0w:~2.2.0w:~2.2.0w\n",
		  [?version,Y,Mo,D,H,Mi,S]).


write_config_file(Dir, FileName, Verify, Write) 
  when (is_list(Dir) andalso 
	is_list(FileName) andalso 
	is_function(Verify) andalso 
	is_function(Write)) ->
    (catch do_write_config_file(Dir, FileName, Verify, Write)).

do_write_config_file(Dir, FileName, Verify, Write) ->
    Verify(),
    case file:open(filename:join(Dir, FileName), [write]) of
	{ok, Fd} ->
	    (catch Write(Fd)),
	    file:close(Fd),
	    ok;
	Error ->
	    Error
    end.


append_config_file(Dir, FileName, Verify, Write) 
  when (is_list(Dir) andalso 
	is_list(FileName) andalso 
	is_function(Verify) andalso 
	is_function(Write)) ->
    (catch do_append_config_file(Dir, FileName, Verify, Write)).

do_append_config_file(Dir, FileName, Verify, Write) ->
    Verify(),
    case file:open(filename:join(Dir, FileName), [read, write]) of
	{ok, Fd} ->
	    file:position(Fd, eof),
	    (catch Write(Fd)),
	    file:close(Fd),
	    ok;
	Error ->
	    Error
    end.


read_config_file(Dir, FileName, Verify) 
  when is_list(Dir) andalso is_list(FileName) andalso is_function(Verify) ->
    (catch do_read_config_file(Dir, FileName, Verify)).

do_read_config_file(Dir, FileName, Verify) ->
    case file:open(filename:join(Dir, FileName), [read]) of
	{ok, Fd} ->
	    Result = read_loop(Fd, [], Verify, 1),
	    file:close(Fd),
	    Result;
	{error, Reason} ->
	    {error, {Reason, FileName}}
    end.

read_loop(Fd, Acc, Check, StartLine) ->
    case read_term(Fd, StartLine) of
	{ok, Term, EndLine} ->
	    case (catch Check(Term)) of
		ok ->
		    read_loop(Fd, [Term | Acc], Check, EndLine);
		{error, Reason} ->
		    {error, {failed_check, StartLine, EndLine, Reason}};
		Error ->
		    {error, {failed_check, StartLine, EndLine, Error}}
	    end;
	{error, EndLine, Error} ->
            {error, {failed_reading, StartLine, EndLine, Error}};
        eof ->
            {ok, lists:reverse(Acc)}
    end.
	    
read_term(Fd, StartLine) ->
    case io:request(Fd, {get_until, "", erl_scan, tokens, [StartLine]}) of
	{ok, Tokens, EndLine} ->
	    case erl_parse:parse_term(Tokens) of
                {ok, Term} ->
                    {ok, Term, EndLine};
                {error, {Line, erl_parse, Error}} ->
                    {error, Line, {parse_error, Error}}
            end;
        {error, E, EndLine} ->
            {error, EndLine, E};
        {eof, _EndLine} ->
            eof;
        Other ->
            Other
    end.


agent_snmp_mk_secret(Alg, Passwd, EngineID) ->
    snmp_usm:passwd2localized_key(Alg, Passwd, EngineID).


ensure_crypto_started() ->
    i("making sure crypto server is started..."),
    ensure_started(crypto).

ensure_started(App) ->
    case (catch App:start()) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok;
	E ->
	    error({failed_starting, App, E})
    end.


%% -------------------------------------------------------------------------

% d(F, A) ->
%     i("DBG: " ++ F, A).

i(F) ->
    i(F, []).

i(F, A) ->
    io:format(F ++ "~n", A).

error(R) ->
    throw({error, R}).

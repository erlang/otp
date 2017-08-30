%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
%%
%%-----------------------------------------------------------------
%% File: iop_ior.erl
%% Description:
%%    This file contains the IOP::IOR handling
%%
%%-----------------------------------------------------------------
-module(iop_ior).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([code/4, decode/4, string_decode/1,
	 string_code/1, string_code/2, string_code/3, string_code/4, 
	 get_key/1, get_key/2, get_typeID/1, create/9,
	 get_objkey/1, check_nil/1, get_privfield/1, set_privfield/2, 
	 get_orbfield/1, set_orbfield/2, 
	 get_flagfield/1, set_flagfield/2, 
	 create_external/5, create_external/6, print/1, print/2,
	 get_alt_addr/1, add_component/2, get_peerdata/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 6).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: create/5/6
%%-----------------------------------------------------------------
%% There are a few restrictions if a certain IIOP-version may contain certain components
%% and contexts The ones we currently, and the ones we perhaps will, support is:
%% 
%%    Feature                          1.0  1.1  1.2
%% TransactionService Service Context  yes  yes  yes
%% CodeSets Service Context                 yes  yes
%% Object by Value Service Context               yes
%% Bi-Directional IIOP Service Context           yes
%% IOR components in IIOP profile           yes  yes
%% TAG_ORB_TYPE                             yes  yes
%% TAG_CODE_SETS                            yes  yes
%% TAG_ALTERNATE_IIOP_ADDRESS                    yes
%% TAG_SSL_SEC_TRANS                        yes  yes
%% Extended IDL data types                  yes  yes
%% Bi-Directional GIOP Features                  yes
%% Value types and Abstract Interfaces           yes
%%
%% CSIv2:
%% A target that supports unprotected IIOP invocations shall specify in the 
%% corresponding TAG_INTERNET_IOP profile a nonzero port number at which the 
%% target will accept unprotected invocations.9 A target that supports only
%% protected IIOP invocations shall specify a port number of 0 (zero) in the
%% corresponding TAG_INTERNET_IOP profile.
%%-----------------------------------------------------------------
create({1, 0}, TypeID, Hosts, IIOPPort, _, Objkey, _, _, _) ->
    Template = #'IIOP_ProfileBody_1_0'{iiop_version = 
				       #'IIOP_Version'{major=1, minor=0},
				       port = IIOPPort,
				       object_key = Objkey},
    #'IOP_IOR'{type_id=TypeID, 
	       profiles=duplicate_1_0_profiles(Hosts, Template, [])};
create({1, Minor}, TypeID, Hosts, IIOPPort, -1, Objkey, MC, _, _) ->
    Template = #'IIOP_ProfileBody_1_1'{iiop_version = 
				       #'IIOP_Version'{major=1, minor=Minor},
				       port = IIOPPort,
				       object_key = Objkey,
				       components = MC},
    #'IOP_IOR'{type_id=TypeID, 
	       profiles=duplicate_1_1_profiles(Hosts, Template, [])};
			     
create({1, Minor}, TypeID, Hosts, IIOPPort, SSLPort, Objkey, MC, Flags, EnvFlags) ->
    V=#'IIOP_Version'{major=1, minor=Minor},
    UseCSIv2 = ?ORB_FLAG_TEST(EnvFlags, ?ORB_ENV_USE_CSIV2),
    Template = 
	case ?ORB_FLAG_TEST(Flags, ?ORB_NO_SECURITY) of
	    true ->
		#'IIOP_ProfileBody_1_1'{iiop_version = V,
					port = IIOPPort,
					object_key = Objkey,
					components = MC};
	    false when UseCSIv2 == false ->
		#'IIOP_ProfileBody_1_1'{iiop_version=V,
					port=IIOPPort,
					object_key=Objkey,
					components= [#'IOP_TaggedComponent'
						     {tag=?TAG_SSL_SEC_TRANS, 
						      component_data=#'SSLIOP_SSL'{target_supports = 2, 
										   target_requires = 2, 
										   port = SSLPort}}|MC]};
	    false when UseCSIv2 == true ->
		#'IIOP_ProfileBody_1_1'
                  {iiop_version=V,
		   port=0,
		   object_key=Objkey,
		   components= [#'IOP_TaggedComponent'
				{tag = ?TAG_CSI_SEC_MECH_LIST, 
				 component_data = 
				 #'CSIIOP_CompoundSecMechList'
				 {stateful = false,
				  mechanism_list = 
				  [#'CSIIOP_CompoundSecMech'
				   {target_requires = 6, 
				    transport_mech = 
				    #'IOP_TaggedComponent'
				    {tag=?TAG_TLS_SEC_TRANS,
				     component_data=#'CSIIOP_TLS_SEC_TRANS'
				     {target_supports = 7, 
				      target_requires = 8, 
				      addresses = 
				      [#'CSIIOP_TransportAddress'{host_name = "Host", 
								  port = SSLPort}]}},
				    as_context_mech = 
				    #'CSIIOP_AS_ContextSec'
				    {target_supports = 9, target_requires = 10,
				     client_authentication_mech = [1, 255], 
				     target_name = [2,255]}, 
				    sas_context_mech = 
				    #'CSIIOP_SAS_ContextSec'
				    {target_supports = 11, target_requires = 12,
				     privilege_authorities = 
				     [#'CSIIOP_ServiceConfiguration'
				      {syntax = ?ULONGMAX, 
				       name = [3,255]}], 
				     supported_naming_mechanisms = [[4,255],[5,255]],
				     supported_identity_types = ?ULONGMAX}}]}}|MC]}
	 end,
    #'IOP_IOR'{type_id=TypeID, 
	       profiles=duplicate_1_1_profiles(Hosts, Template, [])};
create(Version, TypeID, Host, IIOPPort, SSLPort, Objkey, MC, _, _) ->
    orber:dbg("[~p] iop_ior:create(~p, ~p, ~p, ~p, ~p, ~p, ~p);~n"
	      "Unsupported IIOP-version.", 
	      [?LINE, Version, TypeID, Host, IIOPPort, SSLPort, Objkey, MC], 
	      ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).




duplicate_1_1_profiles([], _, Profiles) ->
    Profiles;
duplicate_1_1_profiles([H|T], Template, Profiles) ->
    duplicate_1_1_profiles(T, Template, 
			   [#'IOP_TaggedProfile'
			    {tag=?TAG_INTERNET_IOP,
			     profile_data = 
			     Template#'IIOP_ProfileBody_1_1'{host = H}}|Profiles]).
			     
duplicate_1_0_profiles([], _, Profiles) ->
    Profiles;
duplicate_1_0_profiles([H|T], Template, Profiles) ->
    duplicate_1_0_profiles(T, Template, 
			   [#'IOP_TaggedProfile'
			    {tag=?TAG_INTERNET_IOP,
			     profile_data = 
			     Template#'IIOP_ProfileBody_1_0'{host = H}}|Profiles]).

   
%%-----------------------------------------------------------------
%% Func: create_external/5/6
%%-----------------------------------------------------------------
create_external(Version, TypeID, Host, IIOP_port, Objkey) ->
    create_external(Version, TypeID, Host, IIOP_port, Objkey, []).
create_external({1, 0}, TypeID, Host, IIOP_port, Objkey, _MC) ->
    V=#'IIOP_Version'{major=1,
		      minor=0},
    PB=#'IIOP_ProfileBody_1_0'{iiop_version=V,
			       host=Host,
			       port=IIOP_port,
			       object_key=Objkey},
    #'IOP_IOR'{type_id=TypeID, profiles=[#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
							      profile_data=PB}]};
create_external({1, 1}, TypeID, Host, IIOP_port, Objkey, Components) ->
    V=#'IIOP_Version'{major=1,
		      minor=1},
    PB=#'IIOP_ProfileBody_1_1'{iiop_version=V,
			       host=Host,
			       port=IIOP_port,
			       object_key=Objkey,
			       components=Components},
    #'IOP_IOR'{type_id=TypeID, 
	       profiles=[#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
					      profile_data=PB}]};
create_external({1, 2}, TypeID, Host, IIOP_port, Objkey, Components) ->
    V=#'IIOP_Version'{major=1,
		      minor=2},
    PB=#'IIOP_ProfileBody_1_1'{iiop_version=V,
			       host=Host,
			       port=IIOP_port,
			       object_key=Objkey,
			       components=Components},
    #'IOP_IOR'{type_id=TypeID, 
	       profiles=[#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
					      profile_data=PB}]};
create_external(Version, TypeID, Host, IIOP_port, Objkey, MC) ->
    orber:dbg("[~p] iop_ior:create_external(~p, ~p, ~p, ~p, ~p, ~p);~n" 
	      "Unsupported IIOP-version.", 
	      [?LINE, Version, TypeID, Host, IIOP_port, Objkey, MC], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).
   
%%-----------------------------------------------------------------
%% Func: get_peerdata/1
%%-----------------------------------------------------------------
%% Probably an external IOR.
get_peerdata(#'IOP_IOR'{} = IOR)  ->
    get_peerdata(get_key(IOR), IOR, [], []);
%% Local object reference.
get_peerdata(_) ->
    [].

%% "Plain" TCP/IP.
get_peerdata({'external', {Host, Port, _InitObjkey, Index, TaggedProfile, 
			   #host_data{protocol = normal, 
				      csiv2_mech = undefined}}}, 
	     IOR, Acc, Indexes) ->
    Alts = get_alt_addr(TaggedProfile),
    get_peerdata(get_key(IOR, [Index|Indexes]), IOR, [{Host, Port}|Alts] ++ Acc, 
		 [Index|Indexes]);
%% "Plain" SSL
get_peerdata({'external', {Host, _Port, _InitObjkey, Index, TaggedProfile,
			   #host_data{protocol = ssl, 
				      ssl_data = #'SSLIOP_SSL'{port = Port}, 
				      csiv2_mech = undefined}}}, 
	     IOR, Acc, Indexes) ->
    Alts = get_alt_addr(TaggedProfile),
    get_peerdata(get_key(IOR, [Index|Indexes]), IOR, [{Host, Port}|Alts] ++ Acc, 
		 [Index|Indexes]);
%% TEMPORARY FIX TO SKIP CSIv2 DATA.
get_peerdata({'external', {Host, _Port, _InitObjkey, Index, TaggedProfile,
			   #host_data{protocol = ssl, 
				      ssl_data = #'SSLIOP_SSL'{port = Port}}}}, 
	     IOR, Acc, Indexes) ->
    Alts = get_alt_addr(TaggedProfile),
    get_peerdata(get_key(IOR, [Index|Indexes]), IOR, [{Host, Port}|Alts] ++ Acc, 
		 [Index|Indexes]);
%% CSIv2 over SSL (TAG_TLS_SEC_TRANS) using the SAS protocol. Note port must equal 0.
get_peerdata({'external', 
	      {_Host, 0, _InitObjkey, Index, TaggedProfile, 
	       #host_data{protocol = ssl, 
			  csiv2_mech = 
			  #'CSIIOP_CompoundSecMech'{target_requires = _TR} = _Mech,
			  csiv2_addresses = Addresses}}}, 
	     IOR, Acc, Indexes) ->
    Alts = get_alt_addr(TaggedProfile),
    get_peerdata(get_key(IOR, [Index|Indexes]), IOR, Addresses ++ Alts ++ Acc, 
		 [Index|Indexes]);
%% CSIv2 over SSL (TAG_NULL_TAG) using the SAS protocol.
get_peerdata({'external', 
	      {Host, _Port, _InitObjkey, Index, TaggedProfile, 
	       #host_data{protocol = ssl, 
			  ssl_data = #'SSLIOP_SSL'{port = Port}, 
			  csiv2_mech = Mech}}},
	     IOR, Acc, Indexes) when is_record(Mech, 'CSIIOP_CompoundSecMech') ->
    Alts = get_alt_addr(TaggedProfile),
    get_peerdata(get_key(IOR, [Index|Indexes]), IOR, [{Host, Port}|Alts] ++ Acc, 
		 [Index|Indexes]);
%% CSIv2 over TCP (TAG_NULL_TAG) using the SAS protocol.
get_peerdata({'external', 
	      {Host, Port, _InitObjkey, Index, TaggedProfile, 
	       #host_data{protocol = normal, 
			  csiv2_mech = Mech}}},
	     IOR, Acc, Indexes) when is_record(Mech, 'CSIIOP_CompoundSecMech') ->
    Alts = get_alt_addr(TaggedProfile),
    get_peerdata(get_key(IOR, [Index|Indexes]), IOR, [{Host, Port}|Alts] ++ Acc, 
		 [Index|Indexes]);
get_peerdata(undefined, _IOR, Acc, _Indexes) ->
    Acc;
%% Local object reference.
get_peerdata(_, _, _, _) ->
    [].

%%-----------------------------------------------------------------
%% Func: get_key/1
%%-----------------------------------------------------------------
get_key(#'IOP_IOR'{profiles=P})  ->
    get_key_1(P, false, 0, undefined, #host_data{});
get_key({Module, Type, Key, _UserDef, OrberDef, Flags}) ->
    if
	is_binary(Key) ->
	    {'internal', Key, OrberDef, Flags, Module};
	Type == pseudo ->
	    {'internal_registered', {pseudo, Key}, OrberDef, Flags, Module};
	is_atom(Key) ->
	    {'internal_registered', Key, OrberDef, Flags, Module}
    end;
get_key(What) ->
    orber:dbg("[~p] iop_ior:get_key(~p); Invalid IOR", 
	      [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).
    

get_key(#'IOP_IOR'{profiles=P}, Exclude)  ->
    get_key_1(P, true, 0, Exclude, #host_data{});
get_key(What, _Exclude) ->
    orber:dbg("[~p] iop_ior:get_key(~p); Invalid IOR", 
	      [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).


get_key_1([], false, _, _, _)  ->
    orber:dbg("[~p] iop_ior:get_key_1([]); bad object reference, profile not found.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
get_key_1([], true, _, _, _)  ->
    undefined;
%%--------- Local IIOP-1.0 Profile ---------
get_key_1([#'IOP_TaggedProfile'
	   {tag=?TAG_INTERNET_IOP, 
	    profile_data=#'IIOP_ProfileBody_1_0'
	    {object_key={Module, Type, Key, _UserDef, OrberDef, Flags}}}|_], 
	  _Retry, _Counter, _Exclude, _HD) ->
    if
	is_binary(Key) ->
	    {'internal', Key, OrberDef, Flags, Module};
	Type == pseudo ->
	    {'internal_registered', {pseudo, Key}, OrberDef, Flags, Module};
	is_atom(Key) ->
	    {'internal_registered', Key, OrberDef, Flags, Module}
    end;
%%--------- Local IIOP-1.1 & IIOP-1.2 Profiles ---------
get_key_1([#'IOP_TaggedProfile'
	   {tag=?TAG_INTERNET_IOP, 
	    profile_data=#'IIOP_ProfileBody_1_1'
	    {object_key={Module, Type, Key, _UserDef, OrberDef, Flags}}}|_], 
	  _Retry, _Counter, _Exclude, _HD) ->
    if
	is_binary(Key) ->
	    {'internal', Key, OrberDef, Flags, Module};
	Type == pseudo ->
	    {'internal_registered', {pseudo, Key}, OrberDef, Flags, Module};
	Type == passive ->
	    %% CHECK FOR PRIMARY COMPONENT & GROUPID! Better yet, do not.
	    %% This is internal key and is supposed to be well formed.
	    %% Also, internal keys are not searched for primary member or
	    %% groupid in the component-section of IOR. ObjectKey will tell
	    %% GroupID and database read transaction will tell primary member.
	    {'internal_registered', {passive, Key}, OrberDef, Flags, Module};
	is_atom(Key) ->
	    {'internal_registered', Key, OrberDef, Flags, Module}
    end;
%%--------- External IIOP-1.0 Profile ---------
get_key_1([#'IOP_TaggedProfile'
	   {tag=?TAG_INTERNET_IOP, 
	    profile_data=#'IIOP_ProfileBody_1_0'
	    {host = Host, port = Port, object_key= ObjectKey}} = TP|P], 
	  _Retry, Counter, Exclude, HD) when Exclude == undefined ->
	    %% This case is "necessary" if an ORB adds several IIOP-profiles since,
	    %% for example, wchar isn't supported for 1.0.
    case get_key_1(P, true, Counter+1, Exclude, HD) of
	undefined ->
	    %% We now it's IIOP-1.0 and it doesn't contain any
	    %% components. Hence, no need to check for it.
	    {'external', {Host, Port, ObjectKey, Counter, TP, 
			  HD#host_data{version = {1,0}}}};
	LaterVersion ->
	    LaterVersion
    end;
get_key_1([#'IOP_TaggedProfile'
	   {tag=?TAG_INTERNET_IOP, 
	    profile_data=#'IIOP_ProfileBody_1_0'
	    {host = Host, port = Port, object_key= ObjectKey}} = TP|P], 
	  Retry, Counter, Exclude, HD) ->
    case lists:member(Counter, Exclude) of
	true ->
	    get_key_1(P, Retry, Counter+1, Exclude, HD);
	false ->
	    %% This case is "necessary" if an ORB adds several IIOP-profiles since,
	    %% for example, wchar isn't supported for 1.0.
	    case get_key_1(P, true, Counter+1, Exclude, HD) of
		undefined ->
		    {'external', {Host, Port, ObjectKey, Counter, TP, 
				  HD#host_data{version = {1,0}}}};
		LaterVersion ->
		    LaterVersion
	    end
    end;
%%--------- External IIOP-1.1 & IIOP-1.2 Profiles ---------
get_key_1([#'IOP_TaggedProfile'
	   {tag=?TAG_INTERNET_IOP, 
	    profile_data=#'IIOP_ProfileBody_1_1'
	    {iiop_version = #'IIOP_Version'{major=Major, minor=Minor}, 
	     host = Host, port = Port, object_key= ObjectKey, 
	     components = Components}} = TP|P], 
	      Retry, Counter, Exclude, HD) when Exclude == undefined ->
    case check_components(Components, Port, HD#host_data{version = {Major,Minor}}) of
	#host_data{csiv2_mech = undefined} when Port == 0 ->
	    get_key_1(P, Retry, Counter+1, Exclude, HD);
	NewHD ->
	    {'external', {Host, Port, ObjectKey, Counter, TP, NewHD}}
    end;
get_key_1([#'IOP_TaggedProfile'
	   {tag=?TAG_INTERNET_IOP, 
	    profile_data=#'IIOP_ProfileBody_1_1'
	    {iiop_version = #'IIOP_Version'{major=Major, minor=Minor}, 
	     host = Host, port = Port, object_key= ObjectKey, 
	     components = Components}} = TP|P], 
	  Retry, Counter, Exclude, HD) ->
    case lists:member(Counter, Exclude) of
	true ->
	    get_key_1(P, Retry, Counter+1, Exclude, HD);
	false ->
	    case check_components(Components, Port,
				  HD#host_data{version = {Major,Minor}}) of
		#host_data{csiv2_mech = undefined} when Port == 0 ->
		    get_key_1(P, Retry, Counter+1, Exclude, HD);
		NewHD ->
		    {'external', {Host, Port, ObjectKey, Counter, TP, NewHD}}
	    end
    end;
get_key_1([_ | P], Retry, Counter, Exclude, HD) ->
    get_key_1(P, Retry, Counter+1, Exclude, HD).

check_components([], _, HostData) ->
    HostData;
check_components([#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
					 component_data=SSLStruct}|Rest], 
		       Port, HostData) when is_record(SSLStruct, 'SSLIOP_SSL') ->
    check_components(Rest, Port, HostData#host_data{protocol = ssl,
						    ssl_data = SSLStruct});
%% CSIv2 Components
check_components([#'IOP_TaggedComponent'{tag=?TAG_CSI_SEC_MECH_LIST, 
					 component_data=Data}|Rest], 
		 Port, HostData) when is_record(Data, 'CSIIOP_CompoundSecMechList') ->
    case check_sec_mech(Data#'CSIIOP_CompoundSecMechList'.mechanism_list, Port) of
	undefined ->
	    check_components(Rest, Port, HostData);
	{ok, Protocol, Mech, Addresses} ->
	    check_components(Rest, Port, 
			     HostData#host_data
			     {protocol = Protocol,
			      csiv2_mech = Mech,
			      csiv2_statefull = Data#'CSIIOP_CompoundSecMechList'.stateful,
			      csiv2_addresses = Addresses});
	{ok, Mech} ->
	    check_components(Rest, Port, 
			     HostData#host_data
			     {csiv2_mech = Mech,
			      csiv2_statefull = Data#'CSIIOP_CompoundSecMechList'.stateful})
    end;
%% FT Components
check_components([#'IOP_TaggedComponent'
		  {tag=?TAG_FT_HEARTBEAT_ENABLED, 
		   component_data= 
		   #'FT_TagFTHeartbeatEnabledTaggedComponent'
		   {heartbeat_enabled = Boolean}}|Rest], 
		 Port, HostData) ->
    check_components(Rest, Port, HostData#host_data{ft_heartbeat = Boolean});
check_components([#'IOP_TaggedComponent'
		  {tag=?TAG_FT_PRIMARY,
		   component_data=
		   #'FT_TagFTPrimaryTaggedComponent'{primary = Boolean}}|Rest], 
		 Port, HostData) ->
    check_components(Rest, Port, HostData#host_data{ft_primary = Boolean});
check_components([#'IOP_TaggedComponent'
		  {tag=?TAG_FT_GROUP, 
		   component_data=#'FT_TagFTGroupTaggedComponent'
		   {version = #'GIOP_Version'{major = 1, minor = 0},
		    ft_domain_id = FTDomain,
		    object_group_id = GroupID,
		    object_group_ref_version = GroupVer}}|Rest], 
		 Port, HostData) ->
    check_components(Rest, Port, HostData#host_data{ft_domain = FTDomain,
						    ft_group = GroupID,
						    ft_ref_version = GroupVer});
%% CodeSets Component
check_components([#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					 component_data=#'CONV_FRAME_CodeSetComponentInfo'
					 {'ForCharData' = Char,
					  'ForWcharData' = Wchar}}|Rest], 
		 Port, HostData) ->
    CharData = check_char_codeset(Char),
    WcharData = check_wchar_codeset(Wchar),
    check_components(Rest, Port, HostData#host_data{charset = CharData,
						    wcharset = WcharData});
%% Not used
check_components([_ | Rest], Port, HostData) ->
    check_components(Rest, Port, HostData).

check_sec_mech([], _) ->
    undefined;
%% Not supported yet.
%check_sec_mech([#'CSIIOP_CompoundSecMech'
%		{target_requires = TR,
%		 transport_mech=
%		 #'IOP_TaggedComponent'{tag=?TAG_SECIOP_SEC_TRANS}} = Mech|_], 
%		Port) ->
%    {ok, seciop, Mech};
check_sec_mech([#'CSIIOP_CompoundSecMech'
		{target_requires = TR,
		 transport_mech= 
		 #'IOP_TaggedComponent'{tag = ?TAG_TLS_SEC_TRANS,
					component_data = CD}} = Mech|_], _Port)
  when TR =< ?CSIv2_MAX_TARGET_REQUIRES ->
    {ok, ssl, Mech, extract_host_port(CD#'CSIIOP_TLS_SEC_TRANS'.addresses, [])};
%% The TAG_NULL_TAG component shall be used in the 'transport_mech' field to
%% indicate that a mechanism does not implement security functionality at the
%% transport layer.
%% If the port field in TAG_INTERNET_IOP equals 0 we must find a TAG_TLS_SEC_TRANS
%% or TAG_SECIOP_SEC_TRANS mechanism.
check_sec_mech([#'CSIIOP_CompoundSecMech'
		{transport_mech= 
		 #'IOP_TaggedComponent'{tag = ?TAG_NULL_TAG}}|Rest], 0) ->
    check_sec_mech(Rest, 0);
check_sec_mech([#'CSIIOP_CompoundSecMech'
		{target_requires = TR,
		 transport_mech= 
		 #'IOP_TaggedComponent'{tag = ?TAG_NULL_TAG}} = Mech|_], _Port)
  when TR =< ?CSIv2_MAX_TARGET_REQUIRES ->
    {ok, Mech};
%% Unrecognized or the peer requires more than we support.
check_sec_mech([_ | Rest], Port) ->
    check_sec_mech(Rest, Port).

extract_host_port([], Acc) ->
    Acc;
extract_host_port([#'CSIIOP_TransportAddress'{host_name = Host, 
					      port = Port}|Rest], Acc) ->
    extract_host_port(Rest, [{Host, Port}|Acc]).


check_char_codeset(#'CONV_FRAME_CodeSetComponent'{native_code_set=?ISO8859_1_ID}) ->
    ?ISO8859_1_ID;
check_char_codeset(#'CONV_FRAME_CodeSetComponent'{native_code_set=?ISO646_IRV_ID}) ->
    ?ISO646_IRV_ID;
check_char_codeset(#'CONV_FRAME_CodeSetComponent'{conversion_code_sets=Converters}) ->
    %% Since the list of Converters usually is very short (0 or 1 element) we
    %% can use lists:member.
    case lists:member(?ISO8859_1_ID, Converters) of
	true ->
	    ?ISO8859_1_ID;
	false ->
	    %% Since we are 100% sure strings will be (e.g. IFR-ids) used we
	    %% can raise an exception at this point.
	    orber:dbg("[~p] iop_ior:check_char_codeset(~p);~n"
		      "Orber cannot communicate with this ORB.~n"
		      "It doesn't support a Char CodeSet known to Orber.", 
		      [?LINE, Converters], ?DEBUG_LEVEL),
	    corba:raise(#'INV_OBJREF'{completion_status = ?COMPLETED_NO})
    end.

check_wchar_codeset(#'CONV_FRAME_CodeSetComponent'{native_code_set=?UTF_16_ID}) ->
    ?UTF_16_ID;
check_wchar_codeset(#'CONV_FRAME_CodeSetComponent'{native_code_set=?UCS_2_ID}) ->
    ?UCS_2_ID;
check_wchar_codeset(#'CONV_FRAME_CodeSetComponent'{conversion_code_sets=Converters}) ->
    case lists:member(?UTF_16_ID, Converters) of
	true ->
	    ?UTF_16_ID;
	false ->
	    %% We should not raise an exception here since we do not know if
	    %% wchar/wstring is used.
	    ?UTF_16_ID
%	    ?UNSUPPORTED_WCHAR
    end.


%%-----------------------------------------------------------------
%% Func: add_component/2
%%-----------------------------------------------------------------
add_component(Objref, Component) when is_record(Objref, 'IOP_IOR') ->
    add_component_ior(Objref, Component);
add_component(Objref, Component) ->
    add_component_local(Objref, Component, orber:giop_version()).

add_component_local(_, Component, {1,0}) ->
    orber:dbg("[~p] iop_ior:add_component(~p);~n"
	      "IIOP-1.0 objects cannot contain any components.", 
	      [?LINE, Component], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO});
add_component_local(_, #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS} 
		    = Component, {1,1}) ->
    orber:dbg("[~p] iop_ior:add_component(~p);~n"
	      "IIOP-1.1 objects may not contain ALTERNATE_IIOP_ADDRESS components.", 
	      [?LINE, Component], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO});
add_component_local({Mod, Type, Key, UserDef, OrberDef, Flags}, Component, Version) ->
    EnvFlags = orber:get_flags(),
    MC = case ?ORB_FLAG_TEST(EnvFlags, ?ORB_ENV_EXCLUDE_CODESET_COMPONENT) of
	     true ->
		 [Component];
	     false ->
		 [#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					 component_data=?DEFAULT_CODESETS},
		  Component]
	 end,
    case ?ORB_FLAG_TEST(EnvFlags, ?ORB_ENV_ENABLE_NAT) of
	false ->
	    create(Version, Mod:typeID(), orber:host(), orber:iiop_port(),
		   orber:iiop_ssl_port(),
		   {Mod, Type, Key, UserDef, OrberDef, Flags}, 
		   MC, Flags, EnvFlags);
	true ->
	    create(Version, Mod:typeID(), orber:nat_host(), 
		   orber:nat_iiop_port(), orber:nat_iiop_ssl_port(),
		   {Mod, Type, Key, UserDef, OrberDef, Flags}, 
		   MC, Flags, EnvFlags)
    
    end.

add_component_ior(#'IOP_IOR'{profiles=P} = IOR, Component) ->
    case add_component_ior_helper(P, Component, false, []) of
	{false, _} ->
	    orber:dbg("[~p] iop_ior:add_component_ior(~p);~n"
		      "The IOR do not contain a valid IIOP-version for the supplied component.", 
		      [?LINE, Component], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO});
	{_, NewProfiles} ->
	    IOR#'IOP_IOR'{profiles=NewProfiles}
    end.

add_component_ior_helper([], _Component, Status, Acc) ->
    {Status, Acc};
add_component_ior_helper([#'IOP_TaggedProfile'
			  {tag=?TAG_INTERNET_IOP, 
			   profile_data=#'IIOP_ProfileBody_1_1'
			   {iiop_version= #'IIOP_Version'{minor=1}}}|T], 
			 #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS} 
			 = Component, Status, Acc) ->
    %% 'ALTERNATE_IIOP_ADDRESS' may only be added to IIOP-1.2 IOR's.
    add_component_ior_helper(T, Component, Status, Acc);
add_component_ior_helper([#'IOP_TaggedProfile'
			  {tag=?TAG_INTERNET_IOP, 
			   profile_data=#'IIOP_ProfileBody_1_1'
			   {object_key=Objkey,
			    components=Components} = PB} = H|T], 
			 Component, _Status, Acc) when is_tuple(Objkey) ->
    %% The objectkey must be a tuple if it's a local object. We cannot(!!) add components
    %% to an external IOR.
    add_component_ior_helper(T, Component, true, 
			     [H#'IOP_TaggedProfile'
			      {profile_data=PB#'IIOP_ProfileBody_1_1'
			       {components = [Component|Components]}}|Acc]);
add_component_ior_helper([_|T], Component, Status, Acc) ->
    add_component_ior_helper(T, Component, Status, Acc).

%%-----------------------------------------------------------------
%% Func: get_alt_addr/1
%%-----------------------------------------------------------------
%% TAG_ALTERNATE_IIOP_ADDRESS may only occur in IIOP-1.2 IOR's.
get_alt_addr(#'IOP_TaggedProfile'
	     {tag=?TAG_INTERNET_IOP, 
	      profile_data=#'IIOP_ProfileBody_1_1'{iiop_version=
						   #'IIOP_Version'{minor=2},
						   components=Components}}) ->
    get_alt_addr_helper(Components, []);
get_alt_addr(_) ->
    [].

get_alt_addr_helper([], Acc) -> Acc;
get_alt_addr_helper([#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
					    component_data=#'ALTERNATE_IIOP_ADDRESS'
					    {'HostID'=Host, 'Port'=Port}}|T], Acc) ->
    get_alt_addr_helper(T, [{Host, Port}|Acc]);
get_alt_addr_helper([_|T], Acc) ->
    get_alt_addr_helper(T, Acc).

%%-----------------------------------------------------------------
%% Func: get_typeID/1
%%-----------------------------------------------------------------
get_typeID(#'IOP_IOR'{type_id=TypeID}) ->
    TypeID;
get_typeID({Mod, _Type, _Key, _UserDef, _OrberDef, _Flags}) ->
    Mod:typeID().

%%-----------------------------------------------------------------
%% Func: get_objkey/1
%%-----------------------------------------------------------------
get_objkey(#'IOP_IOR'{profiles=P}) ->
    get_objkey_1(P);
get_objkey({Id, Type, Key, UserDef, OrberDef, Flags}) ->
    {Id, Type, Key, UserDef, OrberDef, Flags}.

get_objkey_1([]) ->
    orber:dbg("[~p] iop_ior:get_objkey_1([]); bad object key, profile not found.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
get_objkey_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB} |_]) ->
    [_, _, _, _, ObjectKey | _] = tuple_to_list(PB),
    ObjectKey;
get_objkey_1([_ | P]) ->
    get_objkey_1(P).

%%-----------------------------------------------------------------
%% Func: get_privfield/1
%%-----------------------------------------------------------------
get_privfield(#'IOP_IOR'{profiles=P}) ->
    get_privfield_1(P);
get_privfield({_Id, _Type, _Key, UserDef, _OrberDef, _Flags}) ->
    UserDef.

get_privfield_1([]) ->
    orber:dbg("[~p] iop_ior:get_privfield_1([]); bad object key, profile not found.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
get_privfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}|_]) ->
    [_, _, _, _, ObjectKey | _] = tuple_to_list(PB),
    case ObjectKey of
	{_Id, _Type, _Key, UserDef, _OrberDef, _Flags} ->
	    UserDef;
	_ ->
	    orber:dbg("[~p] iop_ior:get_privfield_1(~p); bad object key.", 
				    [?LINE, ObjectKey], ?DEBUG_LEVEL),
	    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end;
get_privfield_1([_| P]) ->
    get_privfield_1(P).
    
%%-----------------------------------------------------------------
%% Func: set_privfield/2
%%-----------------------------------------------------------------
set_privfield(#'IOP_IOR'{type_id=Id, profiles=P}, UserData) ->
    #'IOP_IOR'{type_id=Id, profiles=set_privfield_1(P, UserData)};
set_privfield({Id, Type, Key, _, OrberDef, Flags}, UserData) ->
	    {Id, Type, Key, UserData, OrberDef, Flags}.

set_privfield_1([], _) ->
    orber:dbg("[~p] iop_ior:set_privfield_1([]); bad object key, profile not found or external object.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
set_privfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}|P], UserData) ->
    [RecName, Version, Host, IIOP_port, ObjectKey | Rest] = tuple_to_list(PB),
    case ObjectKey of
	{Id, Type, Key, _, OrberDef, Flags} ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
				  profile_data=list_to_tuple([RecName,
							      Version, Host,
							      IIOP_port,
							      {Id, Type, Key, UserData, OrberDef, Flags}|
							      Rest])} |
	     set_privfield_1(P, UserData)];
	_ ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB} | set_privfield_1(P, UserData)]
    end;
set_privfield_1([PB| P], UserData) ->
    [PB | set_privfield_1(P, UserData)].

%%-----------------------------------------------------------------
%% Func: get_orbfield/1
%%-----------------------------------------------------------------
get_orbfield(#'IOP_IOR'{profiles=P}) ->
    get_orbfield_1(P);
get_orbfield({_Id, _Type, _Key, _UserDef, OrberDef, _Flags}) ->
    OrberDef.

get_orbfield_1([]) ->
    orber:dbg("[~p] iop_ior:get_orbfield_1([]);~n"
	      "bad object key, profile not found.", [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
get_orbfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}|_]) ->
    [_, _, _, _, ObjectKey | _] = tuple_to_list(PB),
    case ObjectKey of
	{_Id, _Type, _Key, _UserDef, OrberDef, _Flags} ->
	    OrberDef;
	_ ->
	    orber:dbg("[~p] iop_ior:get_orbfield_1(~p);~n"
		      "bad object key.", [?LINE, ObjectKey], ?DEBUG_LEVEL),
	    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end;
get_orbfield_1([_| P]) ->
    get_orbfield_1(P).
    
%%-----------------------------------------------------------------
%% Func: set_orbfield/2
%%-----------------------------------------------------------------
set_orbfield(#'IOP_IOR'{type_id=Id, profiles=P}, OrberDef) ->
    #'IOP_IOR'{type_id=Id, profiles=set_orbfield_1(P, OrberDef)};
set_orbfield({Id, Type, Key, Priv, _, Flags}, OrberDef) ->
	    {Id, Type, Key, Priv, OrberDef, Flags}.

set_orbfield_1([], _) ->
    orber:dbg("[~p] iop_ior:set_orbfield_1([]);~n"
	      "bad object key, profile not found or external object.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
set_orbfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}| P], OrberDef) ->
    [RecName, Version, Host, IIOP_port, ObjectKey | Rest] = tuple_to_list(PB),
    case ObjectKey of
	{Id, Type, Key, Priv, _, Flags} ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
				  profile_data=list_to_tuple([RecName,
							      Version, Host,
							      IIOP_port,
							      {Id, Type, Key, Priv, OrberDef, Flags}|
							      Rest])} |
	     set_orbfield_1(P, OrberDef)];
	_ ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB} | set_orbfield_1(P, OrberDef)]
    end;
set_orbfield_1([PB| P], OrberDef) ->
    [PB | set_orbfield_1(P, OrberDef)].

%%-----------------------------------------------------------------
%% Func: get_flagfield/1
%%-----------------------------------------------------------------
get_flagfield(#'IOP_IOR'{profiles=P}) ->
    get_flagfield_1(P);
get_flagfield({_Id, _Type, _Key, _UserDef, _OrberDef, Flags}) ->
    Flags.

get_flagfield_1([]) ->
    orber:dbg("[~p] iop_ior:get_flagfield_1([]); bad object key, profile not found.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
get_flagfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}|_]) ->
    [_, _, _, _, ObjectKey | _] = tuple_to_list(PB),
    case ObjectKey of
	{_Id, _Type, _Key, _UserDef, _OrberDef, Flags} ->
	    Flags;
	_ ->
	    orber:dbg("[~p] iop_ior:get_flagfield_1(~p); bad object key.", 
		      [?LINE, ObjectKey], ?DEBUG_LEVEL),
	    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end;
get_flagfield_1([_| P]) ->
    get_flagfield_1(P).
    
%%-----------------------------------------------------------------
%% Func: set_flagfield/2
%%-----------------------------------------------------------------
set_flagfield(#'IOP_IOR'{type_id=Id, profiles=P}, Flags) ->
    #'IOP_IOR'{type_id=Id, profiles=set_flagfield_1(P, Flags)};
set_flagfield({Id, Type, Key, Priv, OrberDef, _}, Flags) ->
	    {Id, Type, Key, Priv, OrberDef, Flags}.

set_flagfield_1([], _) ->
    orber:dbg("[~p] iop_ior:set_flagfield_1([]); bad object key, profile not found or external object.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
set_flagfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}| P], Flags) ->
    [RecName, Version, Host, IIOP_port, ObjectKey | Rest] = tuple_to_list(PB),
    case ObjectKey of
	{Id, Type, Key, Priv, OrberDef, _} ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
				  profile_data=list_to_tuple([RecName,
							      Version, Host,
							      IIOP_port,
							      {Id, Type, Key, Priv, OrberDef, Flags}|
							      Rest])} |
	     set_flagfield_1(P, Flags)];
	_ ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB} | set_flagfield_1(P, Flags)]
    end;
set_flagfield_1([PB| P], Flags) ->
    [PB | set_flagfield_1(P, Flags)].

%%-----------------------------------------------------------------
%% Func: check_nil/1
%%-----------------------------------------------------------------
check_nil(#'IOP_IOR'{type_id="", profiles=[]}) ->
    true;
check_nil({Id, _, _, _, _, _}) when is_atom(Id) ->
    false;
check_nil({Id, _, _, _, _, _}) ->  
    case binary_to_list(Id) of
	"" ->
	    true; 
	_ ->
	    false
    end;
check_nil(_) ->
    false.



%%----------------------------------------------------------------------
%% Function   : print
%% Arguments  : An object represented as one of the following:
%%               - local (tuple)
%%               - IOR
%%               - stringified IOR
%%               - corbaloc- or corbaname-schema
%%              IoDevice - the same as the io-module defines.
%% Returns    : 
%% Description: Prints the object's components.
%%----------------------------------------------------------------------
print(Object) ->
    print(undefined, Object).
print(IoDevice, #'IOP_IOR'{type_id="", profiles=[]}) ->
    print_it(IoDevice, 
	     "================== IOR ====================~n"
	     "NIL Object Reference.~n"
	     "================== END ====================~n");
print(IoDevice, IORStr) when is_list(IORStr) ->
    IOR = corba:string_to_object(IORStr),
    print_helper(IoDevice, IOR);
print(IoDevice, IOR) when is_record(IOR, 'IOP_IOR') ->
    print_helper(IoDevice, IOR);
print(IoDevice, {Mod, Type, Key, UserDef, OrberDef, Flags}) ->
    EnvFlags = orber:get_flags(),
    MC = case ?ORB_FLAG_TEST(EnvFlags, ?ORB_ENV_EXCLUDE_CODESET_COMPONENT) of
	     true ->
		 [];
	     false ->
		 [#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					 component_data=?DEFAULT_CODESETS}]
	 end,
    IOR = case ?ORB_FLAG_TEST(EnvFlags, ?ORB_ENV_ENABLE_NAT) of
	      false ->
		  create(orber:giop_version(), Mod:typeID(), orber:host(), 
			 orber:iiop_port(), orber:iiop_ssl_port(),
			 {Mod, Type, Key, UserDef, OrberDef, Flags}, 
			 MC, Flags, EnvFlags);
	      true ->
		  create(orber:giop_version(), Mod:typeID(), orber:nat_host(), 
			 orber:nat_iiop_port(), orber:nat_iiop_ssl_port(),
			 {Mod, Type, Key, UserDef, OrberDef, Flags}, 
			 MC, Flags, EnvFlags)
	  
	  end,
    print_helper(IoDevice, IOR);
print(_, _) ->
    exit("Bad parameter").

print_helper(IoDevice, #'IOP_IOR'{type_id=TypeID, profiles=Profs}) ->
    Data = io_lib:format("================== IOR ====================~n"
			 "------------------ IFR ID -----------------~n~s~n", 
			 [TypeID]),
    NewData = print_profiles(Profs, []),
    print_it(IoDevice, lists:flatten([Data|NewData])).

print_profiles([], Acc) ->
   lists:flatten([Acc | io_lib:format("================== END ====================~n", [])]);
print_profiles([#'IOP_TaggedProfile'
		{tag=?TAG_INTERNET_IOP,
		 profile_data = #'IIOP_ProfileBody_1_0'{iiop_version=
							#'IIOP_Version'{major=Major,
									minor=Minor},
							host=Host, port=Port,
							object_key=Objkey}}|T], Acc) ->
    Profile = io_lib:format("~n------------------ IIOP Profile -----------~n"
			    "Version.............: ~p.~p~n"
			    "Host................: ~s~n"
			    "Port................: ~p~n", 
			    [Major, Minor, Host, Port]),
    ObjKeyStr = print_objkey(Objkey),
    print_profiles(T, [Profile, ObjKeyStr | Acc]);
print_profiles([#'IOP_TaggedProfile'
		{tag=?TAG_INTERNET_IOP,
		 profile_data = #'IIOP_ProfileBody_1_1'{iiop_version=
							#'IIOP_Version'{major=Major,
									minor=Minor},
							host=Host,
							port=Port,
							object_key=Objkey,
							components=Components}}|T], Acc) ->
    Profile = io_lib:format("~n------------------ IIOP Profile -----------~n"
			    "Version.............: ~p.~p~n"
			    "Host................: ~s~n"
			    "Port................: ~p~n", 
			    [Major, Minor, Host, Port]),
    ComponentsStr = print_components(Components, []),
    ObjKeyStr = print_objkey(Objkey),
    print_profiles(T, [Profile, ObjKeyStr, ComponentsStr |Acc]);
print_profiles([#'IOP_TaggedProfile'{tag=?TAG_MULTIPLE_COMPONENTS,
				     profile_data = Components}|T], Acc) ->
    MComp = io_lib:format("~n------------------ Multiple Components ----~n", []),
    ComponentsStr = print_components(Components, []),
    print_profiles(T, [MComp, ComponentsStr | Acc]);
print_profiles([#'IOP_TaggedProfile'{tag=?TAG_SCCP_IOP,
				     profile_data = _Data}|T], Acc) ->
    SCCP = io_lib:format("~n------------------ SCCP IOP ---------------~n", []),
    print_profiles(T, [SCCP | Acc]);
print_profiles([#'IOP_TaggedProfile'{tag=Tag,
				     profile_data = Data}|T], Acc) ->
    TAG = io_lib:format("~n------------------ TAG ~p -----------------~n"
			"Data................: ~p~n", [Tag, Data]),
    print_profiles(T, [TAG|Acc]).

print_components([], Data) -> lists:flatten(lists:reverse(Data));
print_components([#'IOP_TaggedComponent'{tag=?TAG_ORB_TYPE, 
					 component_data=ORB}|T], Data) ->
    OType = io_lib:format("   TAG_ORB_TYPE~n"
			  "ORB Type............: ~p~n", [ORB]),
    print_components(T, [OType | Data]);
print_components([#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					 component_data=
					 #'CONV_FRAME_CodeSetComponentInfo'
					 {'ForCharData' = Char,
					  'ForWcharData' = Wchar}}|T], Data) ->
    CharSet = io_lib:format("   TAG_CODE_SETS~n"
			    "Native Char.........: ~p~n"
			    "Char Conversion.....: ~p~n"
			    "Native Wchar........: ~p~n"
			    "Wchar Conversion....: ~p~n", 
			    [Char#'CONV_FRAME_CodeSetComponent'.native_code_set,
			     Char#'CONV_FRAME_CodeSetComponent'.conversion_code_sets,
			     Wchar#'CONV_FRAME_CodeSetComponent'.native_code_set,
			     Wchar#'CONV_FRAME_CodeSetComponent'.conversion_code_sets]),
    print_components(T, [CharSet | Data]);
print_components([#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
					 component_data=#'ALTERNATE_IIOP_ADDRESS'
					 {'HostID'=Host, 'Port'=Port}}|T], Data) ->
    AltAddr = io_lib:format("   TAG_ALTERNATE_IIOP_ADDRESS~n"
			    "Alternate Address...: ~s:~p~n", [Host, Port]),
    print_components(T, [AltAddr | Data]);
print_components([#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
					 component_data=#'SSLIOP_SSL'
					 {target_supports=Supports, 
					  target_requires=Requires,
					  port=Port}}|T], Data) ->
    SSL = io_lib:format("   TAG_SSL_SEC_TRANS~n"
			"SSL Port............: ~p~n"
			"SSL Requires........: ~p~n"
			"SSL Supports........: ~p~n", [Port, Requires, Supports]),
    print_components(T, [SSL | Data]);
%% Fault Tolerant Components
print_components([#'IOP_TaggedComponent'{tag=?TAG_FT_GROUP, 
					 component_data=#'FT_TagFTGroupTaggedComponent'
					 {version = Version,
					  ft_domain_id = DomainId,
					  object_group_id = ObjectGroupId,
					  object_group_ref_version = ObjGrRefVer}}|T], Data) ->
    Comp = io_lib:format("   TAG_FT_GROUP~n"
			 "Version.............: ~p~n"
			 "Domain Id...........: ~p~n"
			 "Obj Group Id........: ~p~n"
			 "Obj Group Ref Ver...: ~p~n", 
			 [Version, DomainId, ObjectGroupId, ObjGrRefVer]),
    print_components(T, [Comp | Data]);
print_components([#'IOP_TaggedComponent'{tag=?TAG_FT_PRIMARY, 
					 component_data=#'FT_TagFTPrimaryTaggedComponent'
					 {primary = Primary}}|T], Data) ->
    Comp = io_lib:format("   TAG_FT_PRIMARY~n"
			 "Primary.............: ~p~n", [Primary]),
    print_components(T, [Comp | Data]);
print_components([#'IOP_TaggedComponent'{tag=?TAG_FT_HEARTBEAT_ENABLED, 
					 component_data=#'FT_TagFTHeartbeatEnabledTaggedComponent'
					 {heartbeat_enabled = HBE}}|T], Data) ->
    Comp = io_lib:format("   TAG_FT_HEARTBEAT_ENABLED~n"
			 "Heart Beat Enabled..: ~p~n", [HBE]),
    print_components(T, [Comp | Data]);
%% Security - CSIIOP
print_components([#'IOP_TaggedComponent'{tag=?TAG_CSI_SEC_MECH_LIST, 
					 component_data=#'CSIIOP_CompoundSecMechList'
					 {stateful=Stateful,
					  mechanism_list = MechList}}|T], Data) ->
    Comp = io_lib:format("   TAG_CSI_SEC_MECH_LIST~n"
			 "Stateful............: ~p~n"
			 "Mechanisms..........: ~p~n", [Stateful, MechList]),
    print_components(T, [Comp | Data]);
print_components([#'IOP_TaggedComponent'{tag=?TAG_TLS_SEC_TRANS, 
					 component_data=#'CSIIOP_TLS_SEC_TRANS'
					 {target_supports = TargetS,
					  target_requires = TargetR,
					  addresses = Addresses}}|T], Data) ->
    Comp = io_lib:format("   TAG_TLS_SEC_TRANS~n"
			 "Target Supports.....: ~p~n"
			 "Target Requires.....: ~p~n"
			 "Addresses...........: ~p~n", 
			 [TargetS, TargetR, Addresses]),
    print_components(T, [Comp | Data]);
print_components([#'IOP_TaggedComponent'{tag=?TAG_SECIOP_SEC_TRANS, 
					 component_data=#'CSIIOP_SECIOP_SEC_TRANS'
					 {target_supports = TargetS,
					  target_requires = TargetR,
					  mech_oid = MechOID,
					  target_name = TargetName,
					  addresses = Addresses}}|T], Data) ->
    Comp = io_lib:format("   TAG_SECIOP_SEC_TRANS~n"
			 "Target Supports.....: ~p~n"
			 "Target Requires.....: ~p~n"
			 "Mechanism OID.......: ~p~n"
			 "Target Name.........: ~p~n"
			 "Addresses...........: ~p~n", 
			 [TargetS, TargetR, MechOID, TargetName, Addresses]),
    print_components(T, [Comp | Data]);
%% Unused components.
print_components([#'IOP_TaggedComponent'{tag=TAG, 
					 component_data=CData}|T], Data) ->
    Unused = io_lib:format("Unused Component....: ~s~n", [match_tag(TAG)]),
    Octets = print_octets(CData, [], 1, []),
    print_components(T, [lists:flatten([Unused | Octets])| Data]).


print_objkey(Objkey) when is_tuple(Objkey) ->
    io_lib:format("Local Object........:~n~p~n", [Objkey]);
print_objkey(Objkey) ->
    Hdr = io_lib:format("External Object.....: ~n", []),
    Octets = print_octets(Objkey, [], 1, []),
    lists:flatten([Hdr | Octets]).

print_octets([], [], _, Data) ->
    lists:reverse(Data);
print_octets([], Acc, C, Data) ->
    Filling = lists:duplicate((4*(9-C)), 32),
    FData = io_lib:format("~s", [Filling]),
    Rest = io_lib:format("  ~p~n", [lists:reverse(Acc)]),
    [lists:reverse(Data), FData | Rest];
print_octets([H|T], Acc, 8, Data) when H > 31 , H < 127 ->
    D1 = io_lib:format("~4w", [H]),
    D2 = io_lib:format("  ~p~n", [lists:reverse([H|Acc])]),
    print_octets(T, [], 1, [D2, D1 | Data]);
print_octets([H|T], Acc, 1, Data) when H > 31 , H < 127 ->
    D1 = io_lib:format("~3w", [H]),
    print_octets(T, [H|Acc], 2, [D1 | Data]);
print_octets([H|T], Acc, C, Data) when H > 31 , H < 127 ->
    D1 = io_lib:format("~4w", [H]),
    print_octets(T, [H|Acc], C+1, [D1 | Data]);
print_octets([H|T], Acc, 8, Data) ->
    D1 = io_lib:format("~4w", [H]),
    D2 = io_lib:format("  ~p~n", [lists:reverse([$.|Acc])]),
    print_octets(T, [], 1, [D2, D1 | Data]);
print_octets([H|T], Acc, 1, Data) ->
    D1 = io_lib:format("~3w", [H]),
    print_octets(T, [$.|Acc], 2, [D1|Data]);
print_octets([H|T], Acc, C, Data) ->
    D1 = io_lib:format("~4w", [H]),
    print_octets(T, [$.|Acc], C+1, [D1|Data]).

print_it(undefined, Data) ->
    io:format(Data);
print_it(error_report, Data) ->
    error_logger:error_report(Data);
print_it(info_msg, Data) ->
    error_logger:info_msg(Data);
print_it(string, Data) ->
    lists:flatten(Data);
print_it({error_report, Msg}, Data) ->
    error_logger:error_report(io_lib:format("================== Reason =================~n~s~n~s", 
					    [Msg, Data]));
print_it({info_msg, Msg}, Data) ->
    error_logger:info_msg(io_lib:format("================== Comment ================~n~s~n~s", 
					[Msg, Data]));
print_it(IoDevice, Data) ->
    io:format(IoDevice, Data, []).

match_tag(?TAG_ORB_TYPE) -> ?TAG_ORB_TYPE_STR;
match_tag(?TAG_CODE_SETS) -> ?TAG_CODE_SETS_STR;
match_tag(?TAG_POLICIES) -> ?TAG_POLICIES_STR;
match_tag(?TAG_ALTERNATE_IIOP_ADDRESS) -> ?TAG_ALTERNATE_IIOP_ADDRESS_STR;
match_tag(?TAG_COMPLETE_OBJECT_KEY) -> ?TAG_COMPLETE_OBJECT_KEY_STR;
match_tag(?TAG_ENDPOINT_ID_POSITION) -> ?TAG_ENDPOINT_ID_POSITION_STR;
match_tag(?TAG_LOCATION_POLICY) -> ?TAG_LOCATION_POLICY_STR;
match_tag(?TAG_ASSOCIATION_OPTIONS) -> ?TAG_ASSOCIATION_OPTIONS_STR;
match_tag(?TAG_SEC_NAME) -> ?TAG_SEC_NAME_STR;
match_tag(?TAG_SPKM_1_SEC_MECH) -> ?TAG_SPKM_1_SEC_MECH_STR;
match_tag(?TAG_SPKM_2_SEC_MECH) -> ?TAG_SPKM_2_SEC_MECH_STR;
match_tag(?TAG_KerberosV5_SEC_MECH) -> ?TAG_KerberosV5_SEC_MECH_STR;
match_tag(?TAG_CSI_ECMA_Secret_SEC_MECH) -> ?TAG_CSI_ECMA_Secret_SEC_MECH_STR;
match_tag(?TAG_CSI_ECMA_Hybrid_SEC_MECH) -> ?TAG_CSI_ECMA_Hybrid_SEC_MECH_STR;
match_tag(?TAG_SSL_SEC_TRANS) -> ?TAG_SSL_SEC_TRANS_STR;
match_tag(?TAG_CSI_ECMA_Public_SEC_MECH) -> ?TAG_CSI_ECMA_Public_SEC_MECH_STR;
match_tag(?TAG_GENERIC_SEC_MECH) -> ?TAG_GENERIC_SEC_MECH_STR;
match_tag(?TAG_FIREWALL_TRANS) -> ?TAG_FIREWALL_TRANS_STR;
match_tag(?TAG_SCCP_CONTACT_INFO) -> ?TAG_SCCP_CONTACT_INFO_STR;
match_tag(?TAG_JAVA_CODEBASE) -> ?TAG_JAVA_CODEBASE_STR;
match_tag(?TAG_TRANSACTION_POLICY) -> ?TAG_TRANSACTION_POLICY_STR;
match_tag(?TAG_FT_GROUP) -> ?TAG_FT_GROUP_STR;
match_tag(?TAG_FT_PRIMARY) -> ?TAG_FT_PRIMARY_STR;
match_tag(?TAG_FT_HEARTBEAT_ENABLED) -> ?TAG_FT_HEARTBEAT_ENABLED_STR;
match_tag(?TAG_MESSAGE_ROUTERS) -> ?TAG_MESSAGE_ROUTERS_STR;
match_tag(?TAG_OTS_POLICY) -> ?TAG_OTS_POLICY_STR;
match_tag(?TAG_INV_POLICY) -> ?TAG_INV_POLICY_STR;
match_tag(?TAG_CSI_SEC_MECH_LIST) -> ?TAG_CSI_SEC_MECH_LIST_STR;
match_tag(?TAG_NULL_TAG) -> ?TAG_NULL_TAG_STR;
match_tag(?TAG_SECIOP_SEC_TRANS) -> ?TAG_SECIOP_SEC_TRANS_STR;
match_tag(?TAG_TLS_SEC_TRANS) -> ?TAG_TLS_SEC_TRANS_STR;
match_tag(?TAG_DCE_STRING_BINDING) -> ?TAG_DCE_STRING_BINDING_STR;
match_tag(?TAG_DCE_BINDING_NAME) -> ?TAG_DCE_BINDING_NAME_STR;
match_tag(?TAG_DCE_NO_PIPES) -> ?TAG_DCE_NO_PIPES_STR;
match_tag(?TAG_DCE_SEC_MECH) -> ?TAG_DCE_SEC_MECH_STR;
match_tag(?TAG_INET_SEC_TRANS) -> ?TAG_INET_SEC_TRANS_STR;
match_tag(Tag) -> integer_to_list(Tag).

%%-----------------------------------------------------------------
%% Func: string_code/1
%%-----------------------------------------------------------------
string_code(IOR) ->
    Flags = orber:get_flags(),
    case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_ENABLE_NAT) of
	false ->
	    string_code(IOR, Flags, orber:host(), 
			orber:iiop_port(), orber:iiop_ssl_port());
	true ->
	    string_code(IOR, Flags, orber:nat_host(), 
			orber:nat_iiop_port(), orber:nat_iiop_ssl_port())
    end.

string_code(IOR, Host) ->
    Flags = orber:get_flags(),
    case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_ENABLE_NAT) of
	false ->
	    string_code(IOR, Flags, Host, 
			orber:iiop_port(), orber:iiop_ssl_port());
	true ->
	    string_code(IOR, Flags, Host, 
			orber:nat_iiop_port(), orber:nat_iiop_ssl_port())
    end.

string_code(IOR, Host, Port) ->
    Flags = orber:get_flags(),
    case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_ENABLE_NAT) of
	false ->
	    string_code(IOR, Flags, Host, Port, orber:iiop_ssl_port());
	true ->
	    string_code(IOR, Flags, Host, Port, orber:nat_iiop_ssl_port())
    end.

string_code(IOR, Host, Port, SSLPort) ->
    string_code(IOR, orber:get_flags(), Host, Port, SSLPort).

string_code(IOR, Flags, IP, Port, SSLPort) ->
    Env = #giop_env{version = orber:giop_version(),
		    flags = Flags, host = IP, iiop_port = Port, 
		    iiop_ssl_port = SSLPort, domain = orber:domain(),
		    partial_security = orber:partial_security()},
    {IorByteSeq0, Length0} = cdr_encode:enc_type('tk_octet', Env, 0, [], 0),
    {IorByteSeq, _} = code(Env, IOR, IorByteSeq0, Length0),
    IorByteSeq1 = binary_to_list(list_to_binary(lists:reverse(IorByteSeq))),
    IorHexSeq = bytestring_to_hexstring(IorByteSeq1),
    [$I,$O,$R,$: | IorHexSeq].
    
%%-----------------------------------------------------------------
%% Func: code/3
%%-----------------------------------------------------------------
code(#giop_env{version = Version} = Env, #'IOP_IOR'{type_id=TypeId, profiles=Profiles}, Bytes, Len) ->
    ProfileSeq =code_profile_datas(Version, Profiles),
    %% Byte order
    cdr_encode:enc_type(?IOR_TYPEDEF,
			Env, 
			#'IOP_IOR'{type_id=TypeId, profiles=ProfileSeq},
			Bytes, Len);
%% No Local Interface supplied. Use configuration parameters.
code(#giop_env{version = Version, host = 0, flags = EnvFlags} = Env, 
     {Mod, Type, Key, UserDef, OrberDef, Flags}, Bytes, Len) ->
    MC = case ?ORB_FLAG_TEST(EnvFlags, ?ORB_ENV_EXCLUDE_CODESET_COMPONENT) of
	     true ->
		 [];
	     false ->
		 [#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					 component_data=?DEFAULT_CODESETS}]
	 end,
    IOR = case ?ORB_FLAG_TEST(EnvFlags, ?ORB_ENV_ENABLE_NAT) of
	      false ->
		  create(Version, Mod:typeID(), orber_env:host(), 
			 orber_env:iiop_port(), orber_env:iiop_ssl_port(),
			 {Mod, Type, Key, UserDef, OrberDef, Flags}, 
			 MC, Flags, EnvFlags);
	      true ->
		  create(Version, Mod:typeID(), orber_env:nat_host(), 
			 orber_env:nat_iiop_port(), orber_env:nat_iiop_ssl_port(),
			 {Mod, Type, Key, UserDef, OrberDef, Flags}, 
			 MC, Flags, EnvFlags)
	  
	  end,
    code(Env, IOR, Bytes, Len);
code(#giop_env{version = Version, host = Host, iiop_port = IIOPort, 
	       iiop_ssl_port = SSLPort, flags = EnvFlags} = Env, 
     {Mod, Type, Key, UserDef, OrberDef, Flags}, Bytes, Len) ->
    MC = case ?ORB_FLAG_TEST(EnvFlags, ?ORB_ENV_EXCLUDE_CODESET_COMPONENT) of
	     true ->
		 [];
	     false ->
		 [#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					 component_data=?DEFAULT_CODESETS}]
	 end,
    IOR = case ?ORB_FLAG_TEST(EnvFlags, ?ORB_ENV_ENABLE_NAT) of
	      false ->
		  create(Version, Mod:typeID(), Host, check_port(IIOPort, normal), 
			 check_port(SSLPort, ssl), 
			 {Mod, Type, Key, UserDef, OrberDef, Flags}, 
			 MC, Flags, EnvFlags);
	      true ->
		  create(Version, Mod:typeID(), orber_env:nat_host(Host), 
			 orber_env:nat_iiop_port(check_port(IIOPort, normal)), 
			 orber_env:nat_iiop_ssl_port(check_port(SSLPort, ssl)),
			 {Mod, Type, Key, UserDef, OrberDef, Flags}, 
			 MC, Flags, EnvFlags)
	  end,
    code(Env, IOR, Bytes, Len).

check_port(Port, _Type) when is_integer(Port) ->
    Port;
check_port(_, normal) ->
    orber:iiop_port();
check_port(_, ssl) ->
    orber:iiop_ssl_port().

code_profile_datas(_, []) ->
    [];
code_profile_datas(Version, [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=P} | Profiles]) ->
    NewBytes = list_to_binary(code_profile_data(Version, P)),
    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=NewBytes} | 
     code_profile_datas(Version, Profiles)];
%% Multiple Components
code_profile_datas(Version, [#'IOP_TaggedProfile'{tag=?TAG_MULTIPLE_COMPONENTS, 
						  profile_data=P} | Profiles]) ->
    Comps= code_comp(Version, P, []),
    {Bytes, Length} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Length1} =  cdr_encode:enc_type(?IOP_TAGGEDCOMPONENT_SEQ, Version, Comps, Bytes, Length),
    Profs = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    [#'IOP_TaggedProfile'{tag=?TAG_MULTIPLE_COMPONENTS, 
			  profile_data=Profs}| code_profile_datas(Version, Profiles)];
code_profile_datas(Version, [#'IOP_TaggedProfile'{tag=N, profile_data=P} | Profiles]) ->
    [#'IOP_TaggedProfile'{tag=N, profile_data=P} | code_profile_datas(Version, Profiles)];
code_profile_datas(_, Data) ->
    orber:dbg("[~p] iop_ior:code_profile_datas(~p); unsupported TaggedProfile.", 
	      [?LINE, Data], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).

code_profile_data(Version, ProfileData) ->
    [RecTag, V, H, P, O |Rest] = tuple_to_list(ProfileData),
    {Bytes, Length} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, Length1} = cdr_encode:enc_type(?IIOP_VERSION, Version, V, Bytes, Length),
    {Bytes2, Length2} = cdr_encode:enc_type({'tk_string', 0}, Version,
					    H, Bytes1, Length1),
    {Bytes3, Length3} = cdr_encode:enc_type('tk_ushort', Version, P, Bytes2, Length2),
    {Bytes4, Length4} = cdr_encode:enc_type({'tk_sequence', 'tk_octet', 0}, Version,
					    corba:objkey_to_string(O), Bytes3, Length3),
    {Bytes5, _Length5} = code_profile_data_1(Version, RecTag, Rest, Bytes4, Length4), 
    Bytes6 = lists:reverse(Bytes5),
    lists:flatten(Bytes6).

code_profile_data_1(_Version, 'IIOP_ProfileBody_1_0', [], Bytes, Length) ->
    {Bytes, Length};
code_profile_data_1(Version, 'IIOP_ProfileBody_1_1', [TaggedComponentSeq], Bytes, Length) ->
    Comps = code_comp(Version, TaggedComponentSeq, []),
    cdr_encode:enc_type(?IOP_TAGGEDCOMPONENT_SEQ, Version, Comps, Bytes, Length);
code_profile_data_1(_,V,S,_,_) ->
    orber:dbg("[~p] iop_ior:code_profile_datas(~p, ~p); probably unsupported IIOP-version", 
	      [?LINE, V, S], ?DEBUG_LEVEL),
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO}).

code_comp(_Version, [], CompData) ->
    CompData;
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					   component_data=CodeSet}|Comps], CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Len1} = cdr_encode:enc_type(?CONV_FRAME_CODESETCOMPONENTINFO, Version, 
					  CodeSet, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_ORB_TYPE, 
					   component_data=ORBType}|Comps], CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Len1} = cdr_encode:enc_type(?ORB_TYPE, Version, 
					  ORBType, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_ORB_TYPE, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
					   component_data=AltAddr}|Comps], CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Len1} = cdr_encode:enc_type(?ALTERNATE_IIOP_ADDRESS, Version, 
					  AltAddr, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
					   component_data=SSLStruct}|Comps], CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Len1} = cdr_encode:enc_type(?SSLIOP_SSL, Version, 
					  SSLStruct, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
						      component_data=Bytes}|CompData]);
%% Fault Tolerant Components
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_FT_GROUP, 
					   component_data=Data}|Comps], CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Len1} = cdr_encode:enc_type(?FT_TagFTGroupTaggedComponent, Version, 
					  Data, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_FT_GROUP, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_FT_PRIMARY, 
					   component_data=Data}|Comps], CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Len1} = cdr_encode:enc_type(?FT_TagFTPrimaryTaggedComponent, Version, 
					  Data, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_FT_PRIMARY, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_FT_HEARTBEAT_ENABLED, 
					   component_data=Data}|Comps], CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Len1} = cdr_encode:enc_type(?FT_TagFTHeartbeatEnabledTaggedComponent, Version, 
					  Data, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_FT_HEARTBEAT_ENABLED, 
						      component_data=Bytes}|CompData]);
%% Security - CSIIOP
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_CSI_SEC_MECH_LIST, 
					   component_data=Data}|Comps], CompData) ->
    NewData = Data#'CSIIOP_CompoundSecMechList'
		{mechanism_list = code_sec_mech(Version, 
						Data#'CSIIOP_CompoundSecMechList'.mechanism_list, 
						[])},
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Len1} = cdr_encode:enc_type(?CSIIOP_CompoundSecMechList, Version, 
					  NewData, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_CSI_SEC_MECH_LIST, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_TLS_SEC_TRANS, 
					   component_data=Data}|Comps], 
	  CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Len1} = cdr_encode:enc_type(?CSIIOP_TLS_SEC_TRANS, Version, 
					  Data, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_TLS_SEC_TRANS, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_SECIOP_SEC_TRANS, 
					   component_data=Data}|Comps], CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Len1} = cdr_encode:enc_type(?CSIIOP_SECIOP_SEC_TRANS, Version, 
					  Data, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_SECIOP_SEC_TRANS, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_NULL_TAG}|Comps], CompData) ->
    %% The body of the TAG_NULL_TAG component is a sequence of octets of
    %% length 0.
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, _Len1} = cdr_encode:enc_type({'tk_sequence', 'tk_octet', 0}, Version, 
					  [], Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_NULL_TAG, 
						      component_data=Bytes}|CompData]);
%% Unsupported/not used component.
code_comp(Version, [C|Comps], CompData) -> 
    code_comp(Version, Comps, [C|CompData]).


code_sec_mech(_, [], Acc) ->
    %% We must preserver the order!!
    lists:reverse(Acc);
code_sec_mech(Version, [#'CSIIOP_CompoundSecMech'{transport_mech = TagComp} = CSM|T],
	      Acc) ->
    [EncTagComp] = code_comp(Version, [TagComp], []),
    code_sec_mech(Version, T, [CSM#'CSIIOP_CompoundSecMech'
			       {transport_mech = EncTagComp}|Acc]).


%%-----------------------------------------------------------------
%% Func: string_decode/1
%%-----------------------------------------------------------------
string_decode([$I,$O,$R,$: | IorHexSeq]) ->
    Version = orber:giop_version(),
    IorByteSeq = list_to_binary(hexstring_to_bytestring(IorHexSeq)),
    {ByteOrder, IorRest} = cdr_decode:dec_byte_order(IorByteSeq),
    decode(Version, IorRest, 1, ByteOrder);
string_decode([$i,$o,$r,$: | IorHexSeq]) ->
    Version = orber:giop_version(),
    IorByteSeq = list_to_binary(hexstring_to_bytestring(IorHexSeq)),
    {ByteOrder, IorRest} = cdr_decode:dec_byte_order(IorByteSeq),
    decode(Version, IorRest, 1, ByteOrder);
string_decode(What) ->
    orber:dbg("[~p] iop_ior:string_decode(~p); Should be IOR:.. or ior:..", 
	      [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: decode/3
%%-----------------------------------------------------------------
decode(Version, IorByteSeq, Len, ByteOrder) ->
    {#'IOP_IOR'{type_id=TypeId, profiles=Profiles}, Rest, Length} =
	cdr_decode:dec_type(?IOR_TYPEDEF, Version, IorByteSeq, Len, ByteOrder),
    L = decode_profiles(Version, Profiles),
    {#'IOP_IOR'{type_id=TypeId, profiles=L}, Rest, Length}.

decode_profiles(_, []) ->
    [];
decode_profiles(Version, [P | Profiles]) ->
    Struct = decode_profile(Version, P),
    L = decode_profiles(Version, Profiles),
    [Struct | L].

decode_profile(Version, #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=ProfileData}) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(list_to_binary(ProfileData)),
    Length = 1,
    {V, Rest1, Length1} = cdr_decode:dec_type(?IIOP_VERSION, Version, Rest, Length,
					      ByteOrder),
    {H, Rest2, Length2} = cdr_decode:dec_type({'tk_string', 0}, Version, Rest1, Length1,
					      ByteOrder),
    {P, Rest3, Length3} = cdr_decode:dec_type('tk_ushort', Version, Rest2, Length2,
					      ByteOrder),
    {ObjKey, Rest4, Length4} = cdr_decode:dec_type({'tk_sequence', 'tk_octet', 0},
						   Version, Rest3, Length3, 
						   ByteOrder),
    Struct = decode_profile_1(V, H, P, ObjKey, Version, Rest4, Length4, ByteOrder),
    #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=Struct};
%% Multiple Components
decode_profile(Version, #'IOP_TaggedProfile'{tag=?TAG_MULTIPLE_COMPONENTS, 
					     profile_data=ProfileData}) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(list_to_binary(ProfileData)),
    {Components, <<>>, _Length1} =cdr_decode:dec_type(?IOP_TAGGEDCOMPONENT_SEQ, Version, Rest, 1, ByteOrder),
    CompData = decode_comp(Version, Components, []),
    #'IOP_TaggedProfile'{tag=?TAG_MULTIPLE_COMPONENTS, profile_data=CompData};
decode_profile(_, #'IOP_TaggedProfile'{tag=N, profile_data=ProfileData}) ->
    #'IOP_TaggedProfile'{tag=N, profile_data=ProfileData};
decode_profile(_, Data) ->
    orber:dbg("[~p] iop_ior:decode_profile(~p); unsupported TaggedProfile.", 
	      [?LINE, Data], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).

decode_profile_1(#'IIOP_Version'{major=1, minor=0}, H, P, ObjKey, _Version, _Rest, _Length, _ByteOrder) ->
    #'IIOP_ProfileBody_1_0'{iiop_version=#'IIOP_Version'{major=1,
							 minor=0}, 
			    host=H, port=P,
			    object_key=corba:string_to_objkey(ObjKey)};
decode_profile_1(#'IIOP_Version'{major=1, minor=1}, H, P, ObjKey, Version, Rest, Length, ByteOrder) ->
    {Components, <<>>, _Length1} =cdr_decode:dec_type(?IOP_TAGGEDCOMPONENT_SEQ, Version, Rest, Length, ByteOrder),
    CompData = decode_comp(Version, Components, []),
    #'IIOP_ProfileBody_1_1'{iiop_version=#'IIOP_Version'{major=1,
							 minor=1}, 
			    host=H, port=P,
			    object_key=corba:string_to_objkey(ObjKey),
			    components=CompData};
decode_profile_1(#'IIOP_Version'{major=1, minor=2}, H, P, ObjKey, Version, Rest, Length, ByteOrder) ->
    {Components, <<>>, _Length1} =cdr_decode:dec_type(?IOP_TAGGEDCOMPONENT_SEQ, Version, Rest, Length, ByteOrder),
    CompData = decode_comp(Version, Components, []),
    #'IIOP_ProfileBody_1_1'{iiop_version=#'IIOP_Version'{major=1,
							 minor=2}, 
			    host=H, port=P,
			    object_key=corba:string_to_objkey(ObjKey),
			    components=CompData};
decode_profile_1(V, _, _, _, _, _, _,_) ->
    orber:dbg("[~p] iop_ior:decode_profile_1(~p); probably unsupported IIOP-version.", 
	      [?LINE, V], ?DEBUG_LEVEL),
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO}).

decode_comp(_Version, [], Components) ->
    Components;
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					     component_data=Bytes}|Comps],
	    Components) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(list_to_binary(Bytes)),
    {CodeSet, _, _} = cdr_decode:dec_type(?CONV_FRAME_CODESETCOMPONENTINFO, 
					  Version, Rest, 1, ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					component_data=CodeSet}|Components]);
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_ORB_TYPE, 
					     component_data=Bytes}|Comps],
	    Components) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(list_to_binary(Bytes)),
    {ORBType, _, _} = cdr_decode:dec_type(?ORB_TYPE, 
					  Version, Rest, 1, ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_ORB_TYPE, 
					component_data=ORBType}|Components]);
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
					     component_data=Bytes}|Comps],
	    Components) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(list_to_binary(Bytes)),
    {AltIIOP, _, _} = cdr_decode:dec_type(?ALTERNATE_IIOP_ADDRESS, 
					  Version, Rest, 1, ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
					component_data=AltIIOP}|Components]);
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
					     component_data=Data}|Comps], Components) ->
    {ByteOrder, R} = cdr_decode:dec_byte_order(list_to_binary(Data)),
    {SSLStruct, _Rest1, _Length1} = cdr_decode:dec_type(?SSLIOP_SSL, Version, R, 1,
							ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
					component_data=SSLStruct}|Components]);
%% Fault Tolerant Components
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_FT_GROUP, 
					     component_data=Data}|Comps], Components) ->
    {ByteOrder, R} = cdr_decode:dec_byte_order(list_to_binary(Data)),
    {DecodedData, _Rest1, _Length1} = cdr_decode:dec_type(?FT_TagFTGroupTaggedComponent, Version, R, 1,
							  ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_FT_GROUP, 
					component_data=DecodedData}|Components]);
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_FT_PRIMARY, 
					     component_data=Data}|Comps], Components) ->
    {ByteOrder, R} = cdr_decode:dec_byte_order(list_to_binary(Data)),
    {DecodedData, _Rest1, _Length1} = cdr_decode:dec_type(?FT_TagFTPrimaryTaggedComponent, Version, R, 1,
							  ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_FT_PRIMARY, 
					component_data=DecodedData}|Components]);
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_FT_HEARTBEAT_ENABLED, 
					     component_data=Data}|Comps], Components) ->
    {ByteOrder, R} = cdr_decode:dec_byte_order(list_to_binary(Data)),
    {DecodedData, _Rest1, _Length1} = cdr_decode:dec_type(?FT_TagFTHeartbeatEnabledTaggedComponent, Version, R, 1,
							  ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_FT_HEARTBEAT_ENABLED, 
					component_data=DecodedData}|Components]);
%% Security - CSIIOP
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_CSI_SEC_MECH_LIST, 
					     component_data=Data}|Comps], Components) ->
    {ByteOrder, R} = cdr_decode:dec_byte_order(list_to_binary(Data)),
    {DecodedData, _Rest1, _Length1} = cdr_decode:dec_type(?CSIIOP_CompoundSecMechList, Version, R, 1,
							  ByteOrder),
    NewDecodedData = DecodedData#'CSIIOP_CompoundSecMechList'
      {mechanism_list = decode_sec_mech(Version, 
					DecodedData#'CSIIOP_CompoundSecMechList'.mechanism_list, 
					[])},
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_CSI_SEC_MECH_LIST, 
					component_data=NewDecodedData}|Components]);
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_TLS_SEC_TRANS, 
					     component_data=Data}|Comps], Components) ->
    {ByteOrder, R} = cdr_decode:dec_byte_order(list_to_binary(Data)),
    {DecodedData, _Rest1, _Length1} = cdr_decode:dec_type(?CSIIOP_TLS_SEC_TRANS, Version, R, 1,
							  ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_TLS_SEC_TRANS, 
					component_data=DecodedData}|Components]);
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_SECIOP_SEC_TRANS, 
					     component_data=Data}|Comps], Components) ->
    {ByteOrder, R} = cdr_decode:dec_byte_order(list_to_binary(Data)),
    {DecodedData, _Rest1, _Length1} = cdr_decode:dec_type(?CSIIOP_SECIOP_SEC_TRANS, Version, R, 1,
							  ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_SECIOP_SEC_TRANS, 
					component_data=DecodedData}|Components]);
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_NULL_TAG, 
					     component_data=_Data}|Comps], Components) ->
    %% The body of the TAG_NULL_TAG component is a sequence of octets of
    %% length 0.
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_NULL_TAG, 
					component_data=[]}|Components]);

decode_comp(Version, [C|Comps], Components) ->
    %% Not used but we cannot discard it.
    decode_comp(Version, Comps, [C|Components]).


decode_sec_mech(_Version, [], Acc) ->
    %% We must preserver the order!!
    lists:reverse(Acc);
decode_sec_mech(Version, [#'CSIIOP_CompoundSecMech'{transport_mech = TagComp} = CSM|T],
		Acc) ->
    [DecTagComp] = decode_comp(Version, [TagComp], []),
    decode_sec_mech(Version, T, [CSM#'CSIIOP_CompoundSecMech'
			       {transport_mech = DecTagComp}|Acc]).


%%-----------------------------------------------------------------
%% Func: hexstring_to_bytestring/1
%%-----------------------------------------------------------------
hexstring_to_bytestring(HexString) ->
    ByteString = hexstring_to_bytestring(HexString, []),
    lists:reverse(ByteString).

hexstring_to_bytestring([], Acc) ->
    Acc;
hexstring_to_bytestring([H1, H2 |Rest], Acc) ->
    I1 = hex_to_int(H1),
    I2 = hex_to_int(H2),
    I = I1 * 16 + I2,
    Acc2 = cdrlib:enc_octet(I, Acc),
    hexstring_to_bytestring(Rest, Acc2).


hex_to_int(H) when H >= $a ->
    10 + H - $a;
hex_to_int(H) when H >= $A ->
    10 + H -$A;
hex_to_int(H) ->
    H - $0.
%%-----------------------------------------------------------------
%% Func: bytestring_to_hexstring/1
%% Args: A byte string
%% Returns: A list of hexadecimal digits (onebyte will be represented as 
%%          two hexadecimal digits).
%%-----------------------------------------------------------------
bytestring_to_hexstring(ByteString) ->
    HexString = bytestring_to_hexstring(ByteString, []),
    lists:reverse(HexString).

bytestring_to_hexstring([], Acc) ->
    Acc;
bytestring_to_hexstring([B |Rest], Acc) ->
    [C1, C2] = int_to_hex(B),
    bytestring_to_hexstring(Rest,[C2, C1| Acc]).

int_to_hex(B) when B < 256, B >= 0 ->
    N1 = B div 16,
    N2 = B rem 16,
    [code_character(N1),
     code_character(N2)].

code_character(N) when N < 10 ->
    $0 + N;
code_character(N) ->
    $a + (N - 10).


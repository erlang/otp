%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File: orber_iiop.hrl
%% 
%%----------------------------------------------------------------------
-ifndef(orber_iiop_hrl).
-define(orber_iiop_hrl, true).

-include_lib("orber/include/corba.hrl").

%% The identifiers which indicates if a fixed value has a negative or
%% positive scale.
-define(FIXED_NEGATIVE, 13).
-define(FIXED_POSITIVE, 12).

%% Used instead of IFR-id's in TypeCode definitions for internal data types.
-define(SYSTEM_TYPE, 0).

%% Major version of GIOP protocol which are supported
-define(GIOP_MAJOR, 1).

%% Minor version of GIOP protocol which are supported
-define(GIOP_MINOR, 0).

%% Major version of IIOP protocol which are supported
-define(IIOP_MAJOR, 1).

%% Minor version of IIOP protocol which are supported
-define(IIOP_MINOR, 0).

%% Fragment flags for the flags bitfield in GIOP message headers
-define(GIOP_BYTE_ORDER_MSB, 0).
-define(GIOP_BYTE_ORDER_LSB, 1).

%% Fragment flags for the flags bitfield in GIOP message headers
-define(GIOP_MORE_FRAGMENTS_FALSE, 0).
-define(GIOP_MORE_FRAGMENTS_TRUE,  1).

%% GIOP Message Types
-define(GIOP_MSG_REQUEST,          0).
-define(GIOP_MSG_REPLY,            1).
-define(GIOP_MSG_CANCEL_REQUEST,   2).
-define(GIOP_MSG_LOCATE_REQUEST,   3).
-define(GIOP_MSG_LOCATE_REPLY,     4).
-define(GIOP_MSG_CLOSE_CONNECTION, 5).
-define(GIOP_MSG_MESSAGE_ERROR,    6).
-define(GIOP_MSG_FRAGMENT,         7).

%% PROFILE_ID's
-define(TAG_INTERNET_IOP,        0).
-define(TAG_MULTIPLE_COMPONENTS, 1).
-define(TAG_SCCP_IOP,            2).


%% COMPONENT_ID's
-define(TAG_ORB_TYPE,                  0).
-define(TAG_CODE_SETS,                 1).
-define(TAG_POLICIES,                  2).
-define(TAG_ALTERNATE_IIOP_ADDRESS,    3).
-define(TAG_COMPLETE_OBJECT_KEY,       5).
-define(TAG_ENDPOINT_ID_POSITION,      6).
-define(TAG_LOCATION_POLICY,          12).
-define(TAG_ASSOCIATION_OPTIONS,      13).
-define(TAG_SEC_NAME,                 14).
-define(TAG_SPKM_1_SEC_MECH,          15).
-define(TAG_SPKM_2_SEC_MECH,          16).
-define(TAG_KerberosV5_SEC_MECH,      17).
-define(TAG_CSI_ECMA_Secret_SEC_MECH, 18).
-define(TAG_CSI_ECMA_Hybrid_SEC_MECH, 19).
-define(TAG_SSL_SEC_TRANS,            20).
-define(TAG_CSI_ECMA_Public_SEC_MECH, 21).
-define(TAG_GENERIC_SEC_MECH,         22).
-define(TAG_FIREWALL_TRANS,           23).
-define(TAG_SCCP_CONTACT_INFO,        24).
-define(TAG_JAVA_CODEBASE,            25).
-define(TAG_TRANSACTION_POLICY,       26).
-define(TAG_FT_GROUP,                 27).
-define(TAG_FT_PRIMARY,               28).
-define(TAG_FT_HEARTBEAT_ENABLED,     29).
-define(TAG_MESSAGE_ROUTERS,          30).
-define(TAG_OTS_POLICY,               31).
-define(TAG_INV_POLICY,               32).
-define(TAG_CSI_SEC_MECH_LIST,        33).
-define(TAG_NULL_TAG,                 34).
-define(TAG_SECIOP_SEC_TRANS,         35).
-define(TAG_TLS_SEC_TRANS,            36).
-define(TAG_DCE_STRING_BINDING,      100).
-define(TAG_DCE_BINDING_NAME,        101).
-define(TAG_DCE_NO_PIPES,            102).
-define(TAG_DCE_SEC_MECH,            103).
-define(TAG_INET_SEC_TRANS,          123).

%% COMPONENT_ID strings
-define(TAG_ORB_TYPE_STR,                  "TAG_ORB_TYPE").
-define(TAG_CODE_SETS_STR,                 "TAG_CODE_SETS").
-define(TAG_POLICIES_STR,                  "TAG_POLICIES").
-define(TAG_ALTERNATE_IIOP_ADDRESS_STR,    "TAG_ALTERNATE_IIOP_ADDRESS").
-define(TAG_COMPLETE_OBJECT_KEY_STR,       "TAG_COMPLETE_OBJECT_KEY").
-define(TAG_ENDPOINT_ID_POSITION_STR,      "TAG_ENDPOINT_ID_POSITION").
-define(TAG_LOCATION_POLICY_STR,           "TAG_LOCATION_POLICY").
-define(TAG_ASSOCIATION_OPTIONS_STR,       "TAG_ASSOCIATION_OPTIONS").
-define(TAG_SEC_NAME_STR,                  "TAG_SEC_NAME").
-define(TAG_SPKM_1_SEC_MECH_STR,           "TAG_SPKM_1_SEC_MECH").
-define(TAG_SPKM_2_SEC_MECH_STR,           "TAG_SPKM_2_SEC_MECH").
-define(TAG_KerberosV5_SEC_MECH_STR,       "TAG_KerberosV5_SEC_MECH").
-define(TAG_CSI_ECMA_Secret_SEC_MECH_STR,  "TAG_CSI_ECMA_Secret_SEC_MECH").
-define(TAG_CSI_ECMA_Hybrid_SEC_MECH_STR,  "TAG_CSI_ECMA_Hybrid_SEC_MECH").
-define(TAG_SSL_SEC_TRANS_STR,             "TAG_SSL_SEC_TRANS").
-define(TAG_CSI_ECMA_Public_SEC_MECH_STR,  "(TAG_CSI_ECMA_Public_SEC_MECH").
-define(TAG_GENERIC_SEC_MECH_STR,          "TAG_GENERIC_SEC_MECH").
-define(TAG_FIREWALL_TRANS_STR,            "TAG_FIREWALL_TRANS").
-define(TAG_SCCP_CONTACT_INFO_STR,         "TAG_SCCP_CONTACT_INFO").
-define(TAG_JAVA_CODEBASE_STR,             "TAG_JAVA_CODEBASE").
-define(TAG_TRANSACTION_POLICY_STR,        "TAG_TRANSACTION_POLICY").
-define(TAG_FT_GROUP_STR,                  "TAG_FT_GROUP").
-define(TAG_FT_PRIMARY_STR,                "TAG_FT_PRIMARY").
-define(TAG_FT_HEARTBEAT_ENABLED_STR,      "TAG_FT_HEARTBEAT_ENABLED").
-define(TAG_MESSAGE_ROUTERS_STR,           "TAG_MESSAGE_ROUTERS").
-define(TAG_OTS_POLICY_STR,                "TAG_OTS_POLICY").
-define(TAG_INV_POLICY_STR,                "TAG_INV_POLICY").
-define(TAG_CSI_SEC_MECH_LIST_STR,         "TAG_CSI_SEC_MECH_LIST").
-define(TAG_NULL_TAG_STR,                  "TAG_NULL_TAG").
-define(TAG_SECIOP_SEC_TRANS_STR,          "TAG_SECIOP_SEC_TRANS").
-define(TAG_TLS_SEC_TRANS_STR,             "TAG_TLS_SEC_TRANS").
-define(TAG_DCE_STRING_BINDING_STR,        "TAG_DCE_STRING_BINDING").
-define(TAG_DCE_BINDING_NAME_STR,          "TAG_DCE_BINDING_NAME").
-define(TAG_DCE_NO_PIPES_STR,              "TAG_DCE_NO_PIPES").
-define(TAG_DCE_SEC_MECH_STR,              "TAG_DCE_SEC_MECH").
-define(TAG_INET_SEC_TRANS_STR,            "TAG_INET_SEC_TRANS").

%% GIOP header size
-define(GIOP_HEADER_SIZE, 12).

%% CODESET's we support.
%% Latin-1. This CodeSet is default if no information exists in the IOR.
-define(ISO8859_1_ID, 16#00010001).

%% UTF-16, UCS Transformation Format 16-bit form
-define(UTF_16_ID, 16#00010109).

%% X/Open UTF-8; UCS Transformation Format 8 (UTF-8)
-define(UTF_8_ID, 16#05010001).

%% The limited UTF-16 without the surrogate mechanism is called UCS-2.
%% The two-byte subset which is identical with the original Unicode. 
%% UCS-2, Level 1. Used by JDK-1.3 as native wchar.
-define(UCS_2_ID, 16#00010100).

%% ISO 646:1991 IRV (International Reference Version).
%% Used by JavaIDL as Native Char (JDK-1.3). A.k.a PCS.
-define(ISO646_IRV_ID, 16#00010020).

%% Fallback is *not* the same thing as default!!
-define(FALLBACK_CHAR,  16#05010001).
-define(FALLBACK_WCHAR, 16#00010109).

%% This is used when the wchar codeset is unknown.
-define(UNSUPPORTED_WCHAR, 0).

%% Integer limits
-define(SHORTMIN, -32768).
-define(SHORTMAX, 32767).
-define(USHORTMIN, 0).
-define(USHORTMAX, 65535).
-define(LONGMIN, -2147483648).
-define(LONGMAX, 2147483647).
-define(ULONGMIN, 0).
-define(ULONGMAX, 4294967295).
-define(LONGLONGMIN, -9223372036854775808).
-define(LONGLONGMAX, 9223372036854775807).
-define(ULONGLONGMIN, 0).
-define(ULONGLONGMAX, 18446744073709551615).


-define(ORBER_GENERIC_CTX, {'tk_sequence', 'tk_octet', 0}).


%%----------------------------------------------------------------------
%% GIOP Message Header
%% 
%% magic: identifies the GIOP message headers, array of four characters.
%% giop_version: contains the version number of the giop protocol being 
%%		used in the message.
%% byte_order: indicating the byte order being used in subsequent 
%%		elements of the message.
%%	0 - big-endian byte ordering, 1 - little-endian byte ordering
%% fragments: true if more fragments follow, otherwise false.
%% message_type: indicating the type of the message 
%% message_size: gives the length of the message following the message 
%%		headerin octets.
%%----------------------------------------------------------------------
-record(giop_message, {magic, 
		       giop_version, 
		       byte_order,
		       fragments = false,
		       message_type, 
		       message_size, 
		       message}).



%%----------------------------------------------------------------------
%% Request Message Header
%%
%% service_context: contains ORB service data being passed from client to server.
%%	(IOP::ServiceContextList)
%% request_id: id used to assosciate reply messages with request messages.
%% response_expected: true if the request is expected to have a reply message.
%% object_key: identifies the object wich is the target of the invocation.
%% operation: contains the name of the operation being invoked.
%% requesting_principal: contains a value that identifying the requesting 
%%		principal.
%%----------------------------------------------------------------------
-record(request_header, {service_context, request_id, response_expected, object_key, operation, requesting_principal}).



%%----------------------------------------------------------------------
%% Reply Message Header
%%
%% service_context: contains ORB service data being passed from client to server.
%%	(IOP::ServiceContextList)
%% request_id: id used to assosciate reply messages with request messages.
%% reply_status: indicates the completion status of the request
%%----------------------------------------------------------------------
-record(reply_header, {service_context, request_id, reply_status}).



%%----------------------------------------------------------------------
%% Cancel Request Message Header
%%
%% request_id: id used to assosciate reply messages with request messages.
%%----------------------------------------------------------------------
-record(cancel_request_header, {request_id}).



%%----------------------------------------------------------------------
%% Locate Request Message Header
%%
%% request_id: id used to assosciate reply messages with request messages.
%% object_key: identifies the object being located (octet sequence).
%%----------------------------------------------------------------------
-record(locate_request_header, {request_id, object_key}).



%%----------------------------------------------------------------------
%% Locate Reply Message Header
%%
%% request_id: id used to assosciate reply messages with request messages.
%% locate_status: indicates the completion status of the locate request
%%----------------------------------------------------------------------
-record(locate_reply_header, {request_id, locate_status}).



%%----------------------------------------------------------------------
%% Profile Body
%%
%% iiop_version: describes the version of IIOP that the agent at the
%%		specified address is prepared to receive.
%% host: identifies the internet host to which the GIOP messages
%%		for the specified object may be sent.
%% port: contains the TCP?IP port number where the target agnet is listening
%%		for connection requests.
%% object_key: is an opaque value supplied by the agent producing the IOR.
%%----------------------------------------------------------------------
-record(profile_body, {iiop_version,host,port,object_key}).

%%----------------------------------------------------------------------
%% Version
%% 
%% major: major version number of iiop protocol 
%% minor: minor version number of iiop protocol. 
%%
%% When an agnet generates profiles specifying a particular version,
%% it must be able to accept messages complying with the specified 
%% version or any porevious minor version.
%%----------------------------------------------------------------------
-record(version, {major,minor}).

%%----------------------------------------------------------------------
%% Fragment Message Header
%%
%% request_id:
%%----------------------------------------------------------------------
-record(fragment_header, {request_id}).


%%----------------------------------------------------------------------
%% ORB_FLAGS macros. Used in the local object references {_,_,_,_,_,Flags}.
%% 
%%----------------------------------------------------------------------

%% Definition of flag positions:
-define(ORB_SEC_ATTRIBUTES, 16#01).
-define(ORB_CONTEXT,        16#02).
-define(ORB_TYPECHECK,      16#04).
-define(ORB_NO_SECURITY,    16#08).
-define(ORB_SURVIVE_EXIT,   16#10).
-define(ORB_USE_PI,         16#20).

-define(ORB_INIT_FLAGS, 16#00).

%%----------------------------------------------------------------------
%% Flags used as configuration parameters (application env).
%% 
%%----------------------------------------------------------------------
-define(ORB_ENV_EXCLUDE_CODESET_COMPONENT, 16#01). %% FIXED!!
-define(ORB_ENV_LOCAL_TYPECHECKING,        16#02). %% FIXED!!
-define(ORB_ENV_HOSTNAME_IN_IOR,           16#04). %% FIXED!!
-define(ORB_ENV_ENABLE_NAT,                16#08). %% FIXED!!
-define(ORB_ENV_PARTIAL_SECURITY,          16#10). %% FIXED FOR NOW!! INTERNAL
-define(ORB_ENV_USE_PI,                    16#20). %% FIXED!!
-define(ORB_ENV_USE_FT,                    16#40). %% WILL PROBABLY BE FIXED!!
-define(ORB_ENV_LIGHT_IFR,                 16#80). %% FIXED!!
-define(ORB_ENV_USE_IPV6,                  16#100). %% FIXED!!
-define(ORB_ENV_SURVIVE_EXIT,              16#200). %% FIXED!!
-define(ORB_ENV_USE_ACL_INCOMING,          16#400). %% FIXED!!
-define(ORB_ENV_USE_ACL_OUTGOING,          16#800). %% FIXED!!
-define(ORB_ENV_LOCAL_INTERFACE,           16#1000). %% FIXED!!

-define(ORB_ENV_USE_BI_DIR_IIOP,           16#2000). %% CAN BE CHANGED
-define(ORB_ENV_USE_CSIV2,                 16#4000). %% CAN BE CHANGED
-define(ORB_ENV_EXCLUDE_CODESET_CTX,       16#8000). %% CAN BE CHANGED


-define(ORB_ENV_INIT_FLAGS,      16#00).

-define(ORB_ENV_FLAGS, 
	[{?ORB_ENV_EXCLUDE_CODESET_CTX, "Exclude CodeSet Ctx"},
	 {?ORB_ENV_LOCAL_TYPECHECKING, "Local Typechecking"},
	 {?ORB_ENV_HOSTNAME_IN_IOR, "Use Hostname in IOR"},
	 {?ORB_ENV_EXCLUDE_CODESET_COMPONENT, "Exclude CodeSet Component"},
	 {?ORB_ENV_ENABLE_NAT, "NAT Enabled"},
	 {?ORB_ENV_USE_CSIV2, "CSIv2 Activated"},
	 {?ORB_ENV_USE_FT, "Fault Tolerance Activated"},
	 {?ORB_ENV_USE_IPV6, "IPv6 Activated"},
	 {?ORB_ENV_SURVIVE_EXIT, "EXIT Tolerance Activated"},
	 {?ORB_ENV_USE_PI, "Local Interceptors"},
	 {?ORB_ENV_LIGHT_IFR, "Light IFR"},
	 {?ORB_ENV_USE_BI_DIR_IIOP, "Use BiDirIIOP"},
	 {?ORB_ENV_USE_ACL_INCOMING, "Use ACL for Incoming Connections"},
	 {?ORB_ENV_USE_ACL_OUTGOING, "Use ACL for Outgoing Connections"},
	 {?ORB_ENV_LOCAL_INTERFACE, "Use the Proxy Interface in Exported IOR:s"}]).


%%----------------------------------------------------------------------
%% Definition of flag operations
%% 
%%----------------------------------------------------------------------
%% USAGE: Boolean = ?ORB_FLAG_TEST(Flags, ?ORB_SEC_ATTRIBUTES)
-define(ORB_FLAG_TEST(_F1, _I1),   ((_F1 band _I1) == _I1)).

%% USAGE: NewFlags = ?ORB_SET_TRUE(Flags, ?ORB_CONTEXT)
-define(ORB_SET_TRUE(_F2, _I2),    (_I2 bor _F2)).

%% USAGE: NewFlags = ?ORB_SET_FALSE(Flags, ?ORB_CONTEXT)
-define(ORB_SET_FALSE(_F3, _I3),   ((_I3 bxor 16#ff) band _F3)).

%% USAGE: NewFlags = ?ORB_SET_FALSE_LIST(Flags, [?ORB_SEC_ATTRIBUTES, ?ORB_SOME])
-define(ORB_SET_FALSE_LIST(_F4, _IList1),
	lists:foldl(fun(_I4, _F5) ->
			    ((_I4 bxor 16#ff) band _F5)
		    end, 
		    _F4, _IList1)).

%% USAGE: NewFlags = ?ORB_SET_TRUE_LIST(Flags, [?ORB_SEC_ATTRIBUTES, ?ORB_SOME])
-define(ORB_SET_TRUE_LIST(_F6, _IList2),
	lists:foldl(fun(_I6, _F7) ->
			    (_I6 bor _F7)
		    end, 
		    _F6, _IList2)).

%% USAGE: Boolean = ?ORB_FLAG_TEST_LIST(Flags, [?ORB_CONTEXT, ?ORB_THING])
-define(ORB_FLAG_TEST_LIST(_F8, _IList3),
	lists:all(fun(_I7) ->
			  ((_F8 band _I7) == _I7)
		  end,
		  _IList3)).

%%----------------------------------------------------------------------
%% IOR
%% 
%%----------------------------------------------------------------------
-record('IOP_IOR', {type_id, profiles}).
-record('IOP_TaggedProfile', {tag, profile_data}).
-record('IIOP_ProfileBody_1_0', {iiop_version,
				 host,
				 port,
				 object_key}).
-record('IIOP_ProfileBody_1_1', {iiop_version,
				 host,
				 port,
				 object_key,
				 components}).

-record('GIOP_Version', {major, minor}).

-record('IIOP_Version', {major, minor}).

-record('SSLIOP_SSL', {target_supports, target_requires, port}).

-record('IOP_TaggedComponent', {tag, component_data}).

-record('GIOP_TargetAddress', {label, value}).

-record('GIOP_IORAddressingInfo', {selected_profile_index, ior}).


%%
%% Nil object reference
%%
-define(ORBER_NIL_OBJREF, #'IOP_IOR' {type_id = "", profiles = []}).

-define(IOR_TYPEDEF, {'tk_struct', ?SYSTEM_TYPE, 'IOP_IOR',
		      [{"type_id", {'tk_string', 0}},
		       {"profiles", {'tk_sequence', {'tk_struct', ?SYSTEM_TYPE,
					'IOP_TaggedProfile',
					[{"tag", 'tk_ulong'},
					 {"profile_data",
					  {'tk_sequence', 'tk_octet', 0}}]}, 0}}]}).

-define(GIOP_VERSION, {'tk_struct', ?SYSTEM_TYPE, 'GIOP_Version',
				 [{"major", 'tk_octet'},
				  {"minor", 'tk_octet'}]}).

-define(IIOP_VERSION, {'tk_struct', ?SYSTEM_TYPE, 'IIOP_Version',
				 [{"major vsn", 'tk_octet'},
				  {"minor vsn", 'tk_octet'}]}).
-define(IOP_TAGGEDCOMPONENT, {'tk_struct', ?SYSTEM_TYPE,
			      'IOP_TaggedComponent',
			      [{"tag", 'tk_ulong'},
			       {"component_data",
				{'tk_sequence',
				 'tk_octet', 0}}]}).
-define(IOP_TAGGEDCOMPONENT_SEQ, {'tk_sequence', ?IOP_TAGGEDCOMPONENT, 0}).

-define(PROFILEBODY_1_0_TYPEDEF, {'tk_struct', ?SYSTEM_TYPE, 'IIOP_ProfileBody_1_0',
			      [{"iiop_version", ?IIOP_VERSION },
			       {"host", {'tk_string', 0}},
			       {"port", 'tk_ushort'},
			       {"object_key", {'tk_sequence', 'tk_octet', 0}}]}).
			       
-define(PROFILEBODY_1_1_TYPEDEF, {'tk_struct', ?SYSTEM_TYPE, 'IIOP_ProfileBody_1_1',
			      [{"iiop_version",?IIOP_VERSION },
			       {"host", {'tk_string', 0}},
			       {"port", 'tk_ushort'},
			       {"object_key", {'tk_sequence', 'tk_octet', 0}},
			       {"components", ?IOP_TAGGEDCOMPONENT_SEQ}]}).

-define(PROFILEBODY_1_2_TYPEDEF, {'tk_struct', ?SYSTEM_TYPE, 'IIOP_ProfileBody_1_1',
			      [{"iiop_version",?IIOP_VERSION },
			       {"host", {'tk_string', 0}},
			       {"port", 'tk_ushort'},
			       {"object_key", {'tk_sequence', 'tk_octet', 0}},
			       {"components", ?IOP_TAGGEDCOMPONENT_SEQ}]}).
			       
-define(SSLIOP_SSL, {'tk_struct', ?SYSTEM_TYPE, 'SSLIOP_SSL',
				 [{"target_supports", 'tk_ushort'},
				  {"target_requires", 'tk_ushort'},
				  {"port", 'tk_ushort'}]}).

-define(GIOP_KeyAddr, 0).
-define(GIOP_ProfileAddr, 1).
-define(GIOP_ReferenceAddr, 2).

-define(TARGETADDRESS, {'tk_union', ?SYSTEM_TYPE, 'GIOP_TargetAddress', 'tk_short', -1,
			[{?GIOP_KeyAddr, "object_key", {'tk_sequence', 'tk_octet', 0}},
			 {?GIOP_ProfileAddr, "profile", {'tk_struct', ?SYSTEM_TYPE,
					 'IOP_TaggedProfile',
					 [{"tag", 'tk_ulong'},
					  {"profile_data",
					   {'tk_sequence', 'tk_octet', 0}}]}},
			 {?GIOP_ReferenceAddr, "ior", {'tk_struct', ?SYSTEM_TYPE, 
				     'GIOP_IORAddressingInfo',
				     [{"selected_profile_index", 'tk_ulong'},
				      {"ior", ?IOR_TYPEDEF}]}}]}).

% Zero or more instances of the TAG_ALTERNATE_IIOP_ADDRESS component type
% may be included in a version 1.2 TAG_INTERNET_IOP Profile.
-record('ALTERNATE_IIOP_ADDRESS', {'HostID', 'Port'}).
-define(ALTERNATE_IIOP_ADDRESS, {'tk_struct', ?SYSTEM_TYPE,
				 'ALTERNATE_IIOP_ADDRESS',
				 [{"HostID", {'tk_string', 0}},
				  {"Port", 'tk_ushort'}]}).
% The TAG_ORB_TYPE component can appear at most once in any IOR profile. For
% profiles supporting IIOP 1.1 or greater, it is optionally present.
-define(ORB_TYPE, 'tk_ulong').

-record('CONV_FRAME_CodeSetComponent', {native_code_set, conversion_code_sets}).
-record('CONV_FRAME_CodeSetComponentInfo', {'ForCharData', 'ForWcharData'}).
-define(CONV_FRAME_CODESETCOMPONENT, {'tk_struct', ?SYSTEM_TYPE, 
				      'CONV_FRAME_CodeSetComponent',
				      [{"native_code_set", 'tk_ulong'},
				       {"conversion_code_sets", 
					{'tk_sequence', 'tk_ulong', 0}}]}).
-define(CONV_FRAME_CODESETCOMPONENTINFO, {'tk_struct', ?SYSTEM_TYPE, 
					  'CONV_FRAME_CodeSetComponentInfo',
					  [{"ForCharData", 
					    ?CONV_FRAME_CODESETCOMPONENT},
					   {"ForWcharData", 
					    ?CONV_FRAME_CODESETCOMPONENT}]}).




-define(DEFAULT_FOR_CHAR,  #'CONV_FRAME_CodeSetComponent'{native_code_set=?ISO8859_1_ID, 
							  conversion_code_sets=[]}).
-define(DEFAULT_FOR_WCHAR, #'CONV_FRAME_CodeSetComponent'{native_code_set=?UTF_16_ID, 
							  conversion_code_sets=[]}).
-define(DEFAULT_CODESETS, 
	#'CONV_FRAME_CodeSetComponentInfo'{'ForCharData' = ?DEFAULT_FOR_CHAR, 
					   'ForWcharData' = ?DEFAULT_FOR_WCHAR}).

%% Fragmentation - IIOP-1.1 & 1.2	  
-record('GIOP_FragmentHeader_1_2', {request_id}).

-define(GIOP_FragmentHeader_1_2, {'tk_struct', ?SYSTEM_TYPE, 
				  'GIOP_FragmentHeader_1_2',
				  [{"request_id", 'tk_ulong'}]}).

%%------ MISC Definitions -------
%% TimeBase::TimeT (TimeBase.idl) is defined as
%%    typedef unsigned long long TimeT; 
-define(TimeBase_TimeT, 'tk_ulonglong').

%%------ Fault Tolerant Definitions -------

%% Specification for Interoperable Object Group References
-define(FT_FTDomainId, {'tk_string', 0}).
-define(FT_ObjectGroupId, 'tk_ulonglong').
-define(FT_ObjectGroupRefVersion, 'tk_ulong').
%% A GIOP::Version of 1.0 indicates that the implementation is compliant
%% with the CORBA-2.6 specification.
%%  tag = TAG_FT_GROUP
-record('FT_TagFTGroupTaggedComponent', {version = #'GIOP_Version'{major = 1,
								   minor = 0}, 
					 ft_domain_id, object_group_id,
					 object_group_ref_version}). 
-define(FT_TagFTGroupTaggedComponent, {'tk_struct', ?SYSTEM_TYPE, 'FT_TagFTGroupTaggedComponent',
				       [{"version", ?GIOP_VERSION},
					{"ft_domain_id", ?FT_FTDomainId},
					{"object_group_id", ?FT_ObjectGroupId},
					{"object_group_ref_version", ?FT_ObjectGroupRefVersion}]}).

%% tag = TAG_FT_PRIMARY;
-record('FT_TagFTPrimaryTaggedComponent', {primary}).
-define(FT_TagFTPrimaryTaggedComponent, {'tk_struct', ?SYSTEM_TYPE, 'FT_TagFTPrimaryTaggedComponent',
				       [{"primary", 'tk_boolean'}]}).


%% Specification for Most Recent Object Group Reference
%% context_id = FT_GROUP_VERSION;
-record('FT_FTGroupVersionServiceContext', {object_group_ref_version}).
-define(FT_FTGroupVersionServiceContext, {'tk_struct', ?SYSTEM_TYPE, 'FT_FTGroupVersionServiceContext',
					  [{"object_group_ref_version", ?FT_ObjectGroupRefVersion}]}).

%% Specification for Transparent Reinvocation
-define(FT_PolicyType_REQUEST_DURATION_POLICY, 47).

%% context_id = FT_REQUEST
-record('FT_FTRequestServiceContext', {client_id, retention_id, expiration_time}).
-define(FT_FTRequestServiceContext, {'tk_struct', ?SYSTEM_TYPE, 'FT_FTRequestServiceContext',
				     [{"client_id", {'tk_string', 0}},
				      {"retention_id", 'tk_long'},
				      {"expiration_time", ?TimeBase_TimeT}]}).

%% Specification for Transport Heartbeats
-define(FT_PolicyType_HEARTBEAT_POLICY,         48).
-define(FT_PolicyType_HEARTBEAT_ENABLED_POLICY, 49).

%% tag = TAG_FT_HEARTBEAT_ENABLED;
-record('FT_TagFTHeartbeatEnabledTaggedComponent', {heartbeat_enabled}).
-define(FT_TagFTHeartbeatEnabledTaggedComponent, {'tk_struct', ?SYSTEM_TYPE, 'FT_TagFTHeartbeatEnabledTaggedComponent',
						  [{"heartbeat_enabled", 'tk_boolean'}]}).


%%------ CSI stuff - required by the SAS protocol. -------
%% This constant defines the current level we support.
-define(CSIv2_MAX_TARGET_REQUIRES, 16#488).

%% NOTE! The OMG VMCID is incorrect in the SAS specification, should be
%% OMGVMCID = 0x4f4d0000;
-define(CSI_OMGVMCID, ?CORBA_OMGVMCID).

%% ASN.1 Encoding of an OBJECT IDENTIFIER
-define(CSI_OID, {'tk_sequence', 'tk_octet', 0}).
-define(CSI_OIDList, {'tk_sequence', ?CSI_OID, 0}).

%% An X509CertificateChain contains an ASN.1 BER encoded SEQUENCE
%% [1..MAX] OF X.509 certificates encapsulated in a sequence of octets. The
%% subject:s certificate shall come first in the list. Each following
%% certificate shall directly certify the one preceding it. The ASN.1
%% representation of Certificate is as defined in [IETF RFC 2459].
-define(CSI_X509CertificateChain, {'tk_sequence', 'tk_octet', 0}).

%% an X.501 type name or Distinguished Name encapsulated in a sequence of
%% octets containing the ASN.1 encoding.
-define(CSI_X501DistinguishedName, {'tk_sequence', 'tk_octet', 0}).

%% UTF-8 Encoding of String
-define(CSI_UTF8String, {'tk_sequence', 'tk_octet', 0}).

%% A sequence of octets containing a GSStoken. Initial context tokens are
%% ASN.1 encoded as defined in [IETF RFC 2743] Section 3.1,
%% "Mechanism-Independent token Format", pp. 81-82. Initial context tokens
%% contain an ASN.1 tag followed by a token length, a mechanism identifier,
%% and a mechanism-specific token (i.e. a GSSUP::InitialContextToken). The
%% encoding of all other GSS tokens (e.g. error tokens and final context
%% tokens) is mechanism dependent.
-define(CSI_GSSToken, {'tk_sequence', 'tk_octet', 0}).

%% An encoding of a GSS Mechanism-Independent Exported Name Object as
%% defined in [IETF RFC 2743] Section 3.2, "GSS Mechanism-Independent
%% Exported Name Object Format," p. 84.
-define(CSI_GSS_NT_ExportedName, {'tk_sequence', 'tk_octet', 0}).
-define(CSI_GSS_NT_ExportedNameList, {'tk_sequence', ?CSI_GSS_NT_ExportedName, 0}).

%% The MsgType enumeration defines the complete set of service context
%% message types used by the CSI context management protocols, including
%% those message types pertaining only to the stateful application of the
%% protocols (to insure proper alignment of the identifiers between
%% stateless and stateful implementations). Specifically, the
%% MTMessageInContext is not sent by stateless clients (although it may
%% be received by stateless targets).
-define(CSI_MsgType, 'tk_short').
-define(CSI_MsgType_MTEstablishContext,         0).
-define(CSI_MsgType_MTCompleteEstablishContext, 1).
-define(CSI_MsgType_MTContextError,             4).
-define(CSI_MsgType_MTMessageInContext,         5).

%% The ContextId type is used carry session identifiers. A stateless
%% application of the service context protocol is indicated by a session
%% identifier value of 0.
-define(CSI_ContextId, 'tk_ulonglong').

%% The AuthorizationElementType defines the contents and encoding of
%% the_element field of the AuthorizationElement.
%% The high order 20-bits of each AuthorizationElementType constant
%% shall contain the Vendor Minor Codeset ID (VMCID) of the
%% organization that defined the element type. The low order 12 bits
%% shall contain the organization-scoped element type identifier. The
%% high-order 20 bits of all element types defined by the OMG shall
%% contain the VMCID allocated to the OMG (that is, 0x4F4D0).
-define(CSI_AuthorizationElementType, 'tk_ulong').

%% An AuthorizationElementType of X509AttributeCertChain indicates that
%% the_element field of the AuthorizationElement contains an ASN.1 BER
%% SEQUENCE composed of an (X.509) AttributeCertificate followed by a
%% SEQUENCE OF (X.509) Certificate. The two-part SEQUENCE is encapsulated
%% in an octet stream. The chain of identity certificates is provided
%% to certify the attribute certificate. Each certificate in the chain
%% shall directly certify the one preceding it. The first certificate
%% in the chain shall certify the attribute certificate. The ASN.1
%% representation of (X.509) Certificate is as defined in [IETF RFC 2459].
%% The ASN.1 representation of (X.509) AtributeCertificate is as defined
%% in [IETF ID PKIXAC].
-define(CSI_X509AttributeCertChain, (?CSI_OMGVMCID bor 1)).
-define(CSI_AuthorizationElementContents, {'tk_sequence', 'tk_octet', 0}).

%% The AuthorizationElement contains one element of an authorization token.
%% Each element of an authorization token is logically a PAC.
%% The AuthorizationToken is made up of a sequence of AuthorizationElements
%% --- NOTE --- 
%% OMG only defines 'CSI_X509AttributeCertChain' so we use it as default value.
-record('CSI_AuthorizationElement', {the_type = ?CSI_X509AttributeCertChain, 
				     the_element = []}).
-define(CSIIOP_AuthorizationElement, {'tk_struct', ?SYSTEM_TYPE, 'CSI_AuthorizationElement',
				      [{"the_type", ?CSI_AuthorizationElementType},
				       {"the_element", ?CSI_AuthorizationElementContents}]}).
-define(CSI_AuthorizationToken, {'tk_sequence', ?CSIIOP_AuthorizationElement, 0}).

%% Additional standard identity token types shall only be defined by the
%% OMG. All IdentityTokenType constants shall be a power of 2.
-define(CSI_IdentityTokenType, 'tk_ulong').
-define(CSI_IdentityTokenType_ITTAbsent,            0).
-define(CSI_IdentityTokenType_ITTAnonymous,         1).
-define(CSI_IdentityTokenType_ITTPrincipalName,     2).
-define(CSI_IdentityTokenType_ITTX509CertChain,     4).
-define(CSI_IdentityTokenType_ITTDistinguishedName, 8).

-define(CSI_IdentityExtension, {'tk_sequence', 'tk_octet', 0}).
-record('CSI_IdentityToken', {label, value}).
-define(CSI_IdentityToken, 
	{'tk_union', ?SYSTEM_TYPE, 'CSI_IdentityToken', 
	 ?CSI_IdentityTokenType, 5,
	 [{?CSI_IdentityTokenType_ITTAbsent, "absent", 'tk_boolean'},	
	  {?CSI_IdentityTokenType_ITTAnonymous, "anonymous", 'tk_boolean'},
	  {?CSI_IdentityTokenType_ITTPrincipalName, "principal_name", ?CSI_GSS_NT_ExportedName},
	  {?CSI_IdentityTokenType_ITTX509CertChain, "certificate_chain", ?CSI_X509CertificateChain},
	  {?CSI_IdentityTokenType_ITTDistinguishedName, "dn",  ?CSI_X501DistinguishedName},
	  {default, "id", ?CSI_IdentityExtension}]}).

-record('CSI_EstablishContext', {client_context_id, authorization_token, 
				 identity_token, client_authentication_token}).
-define(CSI_EstablishContext, {'tk_struct', ?SYSTEM_TYPE, 'CSI_EstablishContext',
			       [{"client_context_id", ?CSI_ContextId},
				{"authorization_token", ?CSI_AuthorizationToken},
				{"identity_token", ?CSI_IdentityToken},
				{"client_authentication_token", ?CSI_GSSToken}]}).

-record('CSI_CompleteEstablishContext', {client_context_id, context_stateful,
					 final_context_token}).
-define(CSI_CompleteEstablishContext, {'tk_struct', ?SYSTEM_TYPE, 'CSI_CompleteEstablishContext',
				       [{"client_context_id", ?CSI_ContextId},
					{"context_stateful", 'tk_boolean'},
					{"final_context_token", ?CSI_GSSToken}]}).

-record('CSI_ContextError', {client_context_id, major_status, 
			     minor_status, error_token}).
-define(CSI_ContextError, {'tk_struct', ?SYSTEM_TYPE, 'CSI_ContextError',
			   [{"client_context_id", ?CSI_ContextId},
			    {"major_status", 'tk_long'},
			    {"minor_status", 'tk_long'},
			    {"error_token", ?CSI_GSSToken}]}).
	
% Not sent by stateless clients. If received by a stateless server, a
% ContextError message should be returned, indicating the session does
% not exist.
-record('CSI_MessageInContext', {client_context_id, discard_context}).
-define(CSI_MessageInContext, {'tk_struct', ?SYSTEM_TYPE, 'CSI_MessageInContext',
			       [{"client_context_id", ?CSI_ContextId},
				{"discard_context", 'tk_boolean'}]}).

-record('CSI_SASContextBody', {label, value}).
-define(CSI_SASContextBody, 
	{'tk_union', ?SYSTEM_TYPE, 'CSI_SASContextBody', ?CSI_MsgType, -1,
	 [{?CSI_MsgType_MTEstablishContext, "establish_msg", ?CSI_EstablishContext},
	  {?CSI_MsgType_MTCompleteEstablishContext, "complete_msg", ?CSI_CompleteEstablishContext},
	  {?CSI_MsgType_MTContextError, "error_msg", ?CSI_ContextError},
	  {?CSI_MsgType_MTMessageInContext, "in_context_msg", ?CSI_MessageInContext}]}).

%% The following type represents the string representation of an ASN.1
%% OBJECT IDENTIFIER (OID). OIDs are represented by the string "oid:"
%% followed by the integer base 10 representation of the OID separated
%% by dots. For example, the OID corresponding to the OMG is represented
%% as: "oid:2.23.130"
-define(CSI_StringOID,  {'tk_string', 0}).


%% The GSS Object Identifier for the KRB5 mechanism is:
%% { iso(1) member-body(2) United States(840) mit(113554) infosys(1)
%% gssapi(2) krb5(2) }
%% Type ?CSI_StringOID
-define(CSI_KRB5MechOID, "oid:1.2.840.113554.1.2.2").

%% The GSS Object Identifier for name objects of the Mechanism-independent
%% Exported Name Object type is:
%% { iso(1) org(3) dod(6) internet(1) security(5) nametypes(6)
%% gss-api-exported-name(4) }
%% Type ?CSI_StringOID
-define(CSI_GSS_NT_Export_Name_OID, "oid:1.3.6.1.5.6.4").

%% The GSS Object Identifier for the scoped-username name form is:
%% { iso-itu-t (2) international-organization (23) omg (130) security (1)
%% naming (2) scoped-username(1) }
%% Type ?CSI_StringOID
-define(CSI_GSS_NT_Scoped_Username_OID, "oid:2.23.130.1.2.1").

%%------ GSSUP stuff - required by the SAS protocol. -------
%% The GSS Object Identifier allocated for the username/password mechanism is defined
%% below.
%% { iso-itu-t (2) international-organization (23) omg (130)
%% security (1) authentication (1) gssup-mechanism (1) }
%% Type ?CSI_StringOID
-define(GSSUP_GSSUPMechOID, "oid:2.23.130.1.1.1").

%% The following structure defines the inner contents of the
%% username password initial context token. This structure is
%% CDR encapsulated and appended at the end of the
%% username/password GSS (initial context) Token.
-record('GSSUP_InitialContextToken', {username, password, target_name}).
-define(GSSUP_InitialContextToken, {'tk_struct', ?SYSTEM_TYPE, 'GSSUP_InitialContextToken',
				    [{"username", ?CSI_UTF8String},
				     {"password", ?CSI_UTF8String},
				     {"target_name", ?CSI_GSS_NT_ExportedName}]}).

-define(GSSUP_ErrorCode, 'tk_ulong').

%% GSSUP Mechanism-Specific Error Token
-record('GSSUP_ErrorToken', {error_code}).
-define(GSSUP_ErrorToken, {'tk_struct', ?SYSTEM_TYPE, 'GSSUP_ErrorToken',
			   [{"error_code", ?GSSUP_ErrorCode}]}).

%% The context validator has chosen not to reveal the GSSUP
%% specific cause of the failure.
%% Type ?GSSUP_ErrorCode
-define(GSSUP_GSS_UP_S_G_UNSPECIFIED, 1).

%% The user identified in the username field of the
%% GSSUP::InitialContextToken is unknown to the target.
%% Type ?GSSUP_ErrorCode
-define(GSSUP_GSS_UP_S_G_NOUSER, 2).

%% The password supplied in the GSSUP::InitialContextToken was
%% incorrect.
%% Type ?GSSUP_ErrorCode
-define(GSSUP_GSS_UP_S_G_BAD_PASSWORD, 3).

%% The target_name supplied in the GSSUP::InitialContextToken does
%% not match a target_name in a mechanism definition of the target.
%% Type ?GSSUP_ErrorCode
-define(GSSUP_GSS_UP_S_G_BAD_TARGET, 4).


%%----- CSIIOP stuff - required by the SAS protocol. -----

% AssociationOptions
-define(CSIIOP_AssociationOptions, 'tk_ushort').
%% AssociationOptions - constant definitions
-define(CSIIOP_AssociationOptions_NoProtection,              1).
-define(CSIIOP_AssociationOptions_Integrity,                 2).
-define(CSIIOP_AssociationOptions_Confidentiality,           4).
-define(CSIIOP_AssociationOptions_DetectReplay,              8).
-define(CSIIOP_AssociationOptions_DetectMisordering,        16).
-define(CSIIOP_AssociationOptions_EstablishTrustInTarget,   32).
-define(CSIIOP_AssociationOptions_EstablishTrustInClient,   64).
-define(CSIIOP_AssociationOptions_NoDelegation,            128).
-define(CSIIOP_AssociationOptions_SimpleDelegation,        256).
-define(CSIIOP_AssociationOptions_CompositeDelegation,     512).
-define(CSIIOP_AssociationOptions_IdentityAssertion,      1024).
-define(CSIIOP_AssociationOptions_DelegationByClient,     2048).

%% The high order 20-bits of each ServiceConfigurationSyntax constant
%% shall contain the Vendor Minor Codeset ID (VMCID) of the
%% organization that defined the syntax. The low order 12 bits shall
%% contain the organization-scoped syntax identifier. The high-order 20
%% bits of all syntaxes defined by the OMG shall contain the VMCID
%% allocated to the OMG (that is, 0x4F4D0).
%% NOTE! The OMG VMCID is incorrect in the SAS specification, should be
%% OMGVMCID = 0x4f4d0000;
-define(CSIIOP_ServiceConfigurationSyntax, 'tk_ulong').
-define(CSIIOP_ServiceConfigurationSyntax_SCS_GeneralNames, (?CSI_OMGVMCID bor 0)).
-define(CSIIOP_ServiceConfigurationSyntax_SCS_GSSExportedName, (?CSI_OMGVMCID bor 1)).

-define(CSIIOP_ServiceSpecificName, {'tk_sequence', 'tk_octet', 0}).

%% The name field of the ServiceConfiguration structure identifies a
%% privilege authority in the format identified in the syntax field. If the
%% syntax is SCS_GeneralNames, the name field contains an ASN.1 (BER)
%% SEQUENCE [1..MAX] OF GeneralName, as defined by the type GeneralNames in
%% [IETF RFC 2459]. If the syntax is SCS_GSSExportedName, the name field
%% contains a GSS exported name encoded according to the rules in
%% [IETF RFC 2743] Section 3.2, "Mechanism-Independent Exported Name
%% Object Format," p. 84 (CORBA-2.6)
-record('CSIIOP_ServiceConfiguration', {syntax, name}).
-define(CSIIOP_ServiceConfiguration, {'tk_struct', ?SYSTEM_TYPE, 'CSIIOP_ServiceConfiguration',
				      [{"syntax", ?CSIIOP_ServiceConfigurationSyntax},
				       {"name", ?CSIIOP_ServiceSpecificName}]}).
-define(CSIIOP_ServiceConfigurationList, {'tk_sequence', ?CSIIOP_ServiceConfiguration, 0}).

%% The body of the TAG_NULL_TAG component is a sequence of octets of
%% length 0.

%% type used to define AS layer functionality within a compound mechanism
%% definition
-record('CSIIOP_AS_ContextSec', {target_supports = 0, target_requires = 0,
				 client_authentication_mech, target_name}).
-define(CSIIOP_AS_ContextSec, {'tk_struct', ?SYSTEM_TYPE, 'CSIIOP_AS_ContextSec',
			       [{"target_supports", ?CSIIOP_AssociationOptions},
				{"target_requires", ?CSIIOP_AssociationOptions},
				{"client_authentication_mech", ?CSI_OID},
				{"target_name", ?CSI_GSS_NT_ExportedName}]}).

%% type used to define SAS layer functionality within a compound mechanism
%% definition
-record('CSIIOP_SAS_ContextSec', {target_supports = 0, target_requires = 0,
				  privilege_authorities, 
				  supported_naming_mechanisms,
				  supported_identity_types}).
-define(CSIIOP_SAS_ContextSec, {'tk_struct', ?SYSTEM_TYPE, 'CSIIOP_SAS_ContextSec',
				[{"target_supports", ?CSIIOP_AssociationOptions},
				 {"target_requires", ?CSIIOP_AssociationOptions},
				 {"privilege_authorities", ?CSIIOP_ServiceConfigurationList},
				 {"supported_naming_mechanisms", ?CSI_OIDList},
				 {"supported_identity_types", ?CSI_IdentityTokenType}]}).

%% Type used in the body of a TAG_CSI_SEC_MECH_LIST component to describe a
%% compound mechanism
-record('CSIIOP_CompoundSecMech', {target_requires = 0, transport_mech,
				   as_context_mech, sas_context_mech}).
-define(CSIIOP_CompoundSecMech, {'tk_struct', ?SYSTEM_TYPE, 'CSIIOP_CompoundSecMech',
				 [{"target_requires", ?CSIIOP_AssociationOptions},
				  {"transport_mech", ?IOP_TAGGEDCOMPONENT},
				  {"as_context_mech", ?CSIIOP_AS_ContextSec},
				  {"sas_context_mech", ?CSIIOP_SAS_ContextSec}]}).
-define(CSIIOP_CompoundSecMechanisms, {'tk_sequence', ?CSIIOP_CompoundSecMech, 0}).

%% type corresponding to the body of a TAG_CSI_SEC_MECH_LIST component
-record('CSIIOP_CompoundSecMechList', {stateful = false, mechanism_list}).
-define(CSIIOP_CompoundSecMechList, {'tk_struct', ?SYSTEM_TYPE, 'CSIIOP_CompoundSecMechList',
				     [{"stateful", 'tk_boolean'},
				      {"mechanism_list", ?CSIIOP_CompoundSecMechanisms}]}).
%% CSIIOP::TransportAddress
-record('CSIIOP_TransportAddress', {host_name, port}).
-define(CSIIOP_TransportAddress, {'tk_struct', ?SYSTEM_TYPE, 'CSIIOP_TransportAddress',
				  [{"host_name", {'tk_string', 0}},
				   {"port", 'tk_ushort'}]}).
-define(CSIIOP_TransportAddressList, {'tk_sequence', ?CSIIOP_TransportAddress, 0}).

%% Tagged component (TAG_TLS_SEC_TRANS) for configuring TLS/SSL as a CSIv2
%% transport mechanism.
-record('CSIIOP_TLS_SEC_TRANS', {target_supports, target_requires, addresses}).
-define(CSIIOP_TLS_SEC_TRANS, {'tk_struct', ?SYSTEM_TYPE, 'CSIIOP_TLS_SEC_TRANS',
			       [{"target_supports", ?CSIIOP_AssociationOptions},
				{"target_requires", ?CSIIOP_AssociationOptions},
				{"addresses", ?CSIIOP_TransportAddressList}]}).

%% Tagged component (TAG_SECIOP_SEC_TRANS) for configuring SECIOP as a CSIv2
%% transport mechanism
-record('CSIIOP_SECIOP_SEC_TRANS', {target_supports = 0, target_requires = 0, mech_oid,
				    target_name, addresses}).
-define(CSIIOP_SECIOP_SEC_TRANS, {'tk_struct', ?SYSTEM_TYPE, 'CSIIOP_SECIOP_SEC_TRANS',
				  [{"target_supports", ?CSIIOP_AssociationOptions},
				   {"target_requires", ?CSIIOP_AssociationOptions},
				   {"mech_oid", ?CSI_OID},
				   {"target_name", ?CSI_GSS_NT_ExportedName},
				   {"addresses", ?CSIIOP_TransportAddressList}]}).


%%-- ServiceContext ID's ------------
%% Describes what type of context included, i.e.,
%% typedef unsigned long ServiceId; 
%% struct ServiceContext { 
%%	  ServiceId context_id; 
%%	  sequence <octet> context_data; 
%%	 };

%% The record is defined in include/corba.hrl.
%%-record('IOP_ServiceContext', {context_id, context_data}).
-define(IOP_SERVICECONTEXT, {'tk_sequence',
			     {'tk_struct', ?SYSTEM_TYPE, 'IOP_ServiceContext',
			      [{"context_id", 'tk_ulong'},
			       {"context_data",
				{'tk_sequence', 'tk_octet', 0}}]}, 0}).

-record('CONV_FRAME_CodeSetContext', {char_data, wchar_data}).
-define(CONV_FRAME_CODESETCONTEXT, {'tk_struct', ?SYSTEM_TYPE, 'CONV_FRAME_CodeSetContext',
				    [{"char_data", 'tk_ulong'},
				     {"wchar_data", 'tk_ulong'}]}).


-record('IIOP_ListenPoint', {host, port}).
-define(IIOP_LISTENPOINT, {'tk_struct', ?SYSTEM_TYPE, 'IIOP_ListenPoint',
			   [{"host", {'tk_string', 0}},
			    {"port", 'tk_ushort'}]}).

-record('IIOP_BiDirIIOPServiceContext', {listen_points}).
-define(IIOP_BIDIRIIOPSERVICECONTEXT, 
	{'tk_struct', ?SYSTEM_TYPE, 'IIOP_BiDirIIOPServiceContext',
	 [{"listen_points", {'tk_sequence', ?IIOP_LISTENPOINT, 0}}]}).

-define(IOP_TransactionService,        0).
-define(IOP_CodeSets,                  1).
-define(IOP_ChainBypassCheck,          2).
-define(IOP_ChainBypassInfo,           3).
-define(IOP_LogicalThreadId,           4).
-define(IOP_BI_DIR_IIOP,               5).
-define(IOP_SendingContextRunTime,     6).
-define(IOP_INVOCATION_POLICIES,       7).
-define(IOP_FORWARDED_IDENTITY,        8).
-define(IOP_UnknownExceptionInfo,      9).
-define(IOP_RTCorbaPriority,          10).
-define(IOP_RTCorbaPriorityRange,     11).
-define(IOP_FT_GROUP_VERSION,         12).
-define(IOP_FT_REQUEST,               13).
-define(IOP_ExceptionDetailMessage,   14).
-define(IOP_SecurityAttributeService, 15).



%%----------------------------------------------------------------------
%% host_data
%%----------------------------------------------------------------------
-record(host_data, {protocol = normal, ssl_data, version, csiv2_mech,
		    csiv2_statefull = false, csiv2_addresses = [],
		    charset = ?ISO8859_1_ID, wcharset = ?UTF_16_ID,
		    ft_heartbeat = false, ft_primary = false, ft_domain,
		    ft_group, ft_ref_version}).

%%----------------------------------------------------------------------
%% giop_env
%%----------------------------------------------------------------------
-record(giop_env, {interceptors, type, version, bytes, ctx = [], 
		   request_id, op, parameters = [], tc, response_expected, 
		   objkey, reply_status, result, flags, host, iiop_port,
		   iiop_ssl_port, domain, partial_security}).

-endif.

%%----------------------------------------------------------------------
%% END OF MODULE
%%----------------------------------------------------------------------

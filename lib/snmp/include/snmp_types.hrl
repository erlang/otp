%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Note: All internal representations may be changed without notice.
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Use this 'value' when sending a notification if wanting to exclude
%% a varbind form the (notification) message.
%%----------------------------------------------------------------------

-define(NOTIFICATION_IGNORE_VB_VALUE, '$ignore-oid').


%%----------------------------------------------------------------------
%% Variablebinding
%% oid: a list of integers (see snmp_misc:is_oid)
%% variabletype corresponds to type in the asn1_type.
%% variabletype=='NULL' =>
%%     value== 'NULL' | noSuchObject | noSuchInstance | endOfMibView
%% else: variabletype == <one of the types defined in rfc1903>;
%%                       'INTEGER' | 'Integer32' | 'OCTET STRING' |
%%                       'OBJECT IDENTIFIER' | 'IpAddress' | 'Counter32' |
%%                       'TimeTicks' | 'Opaque' | 'Counter64' | 'Unsigned32'
%% value: a value.
%% org_index: an integer. Its position in the original varbindlist (the one
%%            from the get- or set-request).
%%----------------------------------------------------------------------
-record(varbind, 
	{oid, 
	 variabletype, 
	 value, 
	 org_index
	}
       ).

%%-----------------------------------------------------------------
%% Internal Variablebinding
%% status = noError | ErrorStatus
%% mibentry = A mibentry if status == noError
%% varbind = a varbind-record
%%-----------------------------------------------------------------
-record(ivarbind, 
	{status = noError, 
	 mibentry, 
	 varbind
	}
       ).

%%----------------------------------------------------------------------
%% ASN1_type. Everything that is needed to represent a typed variable.
%% BERtype: Type used during Basic Encoding/Decoding Rules
%% aliasname: The name of the derived type as defined in the MIB
%% assocList can be list of:
%% {enums, [{up, 1}, {down, 2}, {right, 3}, {left, 4}]}
%%----------------------------------------------------------------------
-record(asn1_type, 
	{bertype, 
	 lo, 
	 hi, 
	 assocList = [], 
	 imported = false,
	 aliasname, 
	 implied = false,
	 display_hint
	}
       ).


%%-----------------------------------------------------------------
%% TableInfo - stored in snmp_symbolic_store for use by the
%% generic table functions. For an ordinary table, the
%% types will be the following: 
%%   nbr_of_cols      is an integer
%%                    pos_integer()
%%   defvals          is a list of {Col, Defval}, ordered by column
%%                    number
%%                    [{Col :: integer(), Defval :: term()}]
%%   status_col       is an integer
%%                    pos_integer()
%%   not_accessible   a sorted list of columns (> first_accessible) 
%%                    that are 'not-accessible'
%%                    [pos_integer()]
%%   index_types      is a list of #asn1_type for the index-columns,
%%                    ordered by column number or an "augment"-tuple
%%                    [asn1_type()]
%%   first_accessible is an integer, the first accessible column
%%                    pos_integer()
%%   first_own_index  is an integer. 0 if there is no such index for 
%%                    this table.
%%                    This is not the same as the last integer in the oid!
%%                    Example: If a table has one own index (oid.1), one 
%%                    column (oid.2) and one imported index then 
%%                    first_own_index will be 2.
%%                    non_neg_integer()
%% For a augmented table, it will instead look like this:
%%   index_types      {augments, {atom(), asn1_type()}}
%%   nbr_of_cols      pos_integer()
%%   not_accessible   [pos_integer()]
%%   first_accessible pos_integer()
%%   
%%-----------------------------------------------------------------

-record(table_info, 
	{nbr_of_cols, 
	 defvals = [], 
	 status_col, 
	 not_accessible,
	 index_types, 
	 first_accessible = 1, 
	 first_own_index
	}
       ).


%%-----------------------------------------------------------------
%% TableInfo - stored in snmp_symbolic_store for use by the
%% generic variable functions.
%%   defval           is a default value for the variable
%%-----------------------------------------------------------------
-record(variable_info, 
	{
	  defval
	}
       ).


%%----------------------------------------------------------------------
%% MibEntry
%% aliasname is the name for the oid.
%% asn1_type is a record of asn1_type.
%% entrytype: variable | table | table_column | internal
%% access: notAccessible | readOnly | readWrite | readCreate     (see rfc 1142)
%% assocList: list of
%%   {table_info, #table_info}      when entrytype == table
%%   {varable_info, #variable_info} when entrytype == variable
%%   {table_name, TableName}        when entrytype == table_column
%%   {table_entry_with_sequence, NameOfSequence} when entrytype == table_entry
%% description: DESCRIPTIONS field
%%----------------------------------------------------------------------
-record(me, 
	{
	  oid, 
	  entrytype, 
	  aliasname, 
	  asn1_type,
	  access, 
	  mfa, 
	  imported = false,
	  assocList = [], 
	  description = undefined,
	  units
	 }
       ).


%% oidobjects is a list of {oid, asn1_type} to be sent in the trap
%% with the description field included.
-record(trap, 
	{trapname, 
	 enterpriseoid,  
	 specificcode, 
	 oidobjects, 
	 description = undefined
	}
       ).

%% oidobjects is a list of {oid, asn1_type} to be sent in the trap
%% with the description field included.
-record(notification, 
	{trapname, 
	 oid, 
	 oidobjects, 
	 description = undefined
	}
       ).

%%----------------------------------------------------------------------
%% This is how a mib is represented on disk (as a binary)
%% types is: [asn1_type()]
%% variable_infos is a list of {Name,  variable_info-record}
%% table_infos is a list of {Name,  table_info-record}
%%
%% The mib format version is a string with the following 
%% structure: Major.Minor
%% The Major number is changed when the mib format is changed so 
%% that it is incompatible with previous versions. It still _might_ 
%% be possible to convert (off-line or in run-time), but don't count 
%% on it.
%% The Minor number is changed when a minor change has been made that
%% does not effect the backward compatibillity.
%% Both Major and Minor are integers. 
%%
%% So, "2.0" is compatible with "2.1", but not with "3.0".
%%    
%%----------------------------------------------------------------------
-record(mib, 
	{misc = [], 
	 mib_format_version = "3.3", 
	 name = "",
	 module_identity,  %% Not in SMIv1, and only with +module_identity
	 mes = [], 
	 asn1_types = [], 
	 traps = [], 
	 variable_infos = [],
	 table_infos = [],
	 imports           %% only with +imports
	}
       ).

-record(module_identity,
	{last_updated,
         organization,
         contact_info,
         description,
         revisions
	}
       ).

%%----------------------------------------------------------------------
%% version = 'version-1' | 'version-2' | 'version-3'
%% vsn_hdr is dependent on version.  If v1 | v2 it's the community string,
%% if v3 its a v3_hdr record
%% data is a PDU (v1 & v2c) or a (possibly encrypted) ScopedPDU (v3)
%%
%% The constant SNMP_USE_V3 is used for compatibility reasons.  In earlier
%% versions, the vsn_hdr field was called 'community'.  This only worked
%% for v1 and v2c.  Thus, the field is renamed to vsn_hdr, and the
%% content depend on the version as described above.  An application
%% that handles not only v1 and v2c, but also v3, *must* define the
%% constant SNMP_USE_V3 before including this header file.  This 
%% ensures that the application can refer to the field as 'vsn_hdr'.
%% An old application, that doesn't handle v3, doesn't define
%% the constant, can still refer to the field as 'coomunity'.
%%----------------------------------------------------------------------
-ifdef(SNMP_USE_V3).
-record(message, {version, vsn_hdr, data}).
-else.
-record(message, {version, community, data}).
-endif.

-record(v3_hdr, 
	{msgID, 
	 msgMaxSize, 
	 msgFlags,
	 msgSecurityModel, 
	 msgSecurityParameters, 
	 hdr_size
	}
       ).

-record(scopedPdu, 
	{contextEngineID, 
	 contextName, 
	 data
	}
       ).

%%-----------------------------------------------------------------
%% USM Security Model
%%-----------------------------------------------------------------
-record(usmSecurityParameters, 
	{msgAuthoritativeEngineID,
	 msgAuthoritativeEngineBoots,
	 msgAuthoritativeEngineTime,
	 msgUserName,
	 msgAuthenticationParameters,
	 msgPrivacyParameters
	}
       ).

%%----------------------------------------------------------------------
%% type: 'get-request' | 'get-next-request' | 'get-bulk-request' |
%% 'get-response' | 'set-request' | 'inform-request' | 'snmpv2-trap' | report
%% (see rfc 1905)
%% request_id, error_status and error_index are integers.
%% varbinds: a list of varbinds.
%%----------------------------------------------------------------------
%%               if bulk        non-repeaters max-repetitions  resp
-record(pdu, 
	{type, 
	 request_id, 
	 error_status, 
	 error_index, 
	 varbinds
	}
       ).

-record(trappdu, 
	{enterprise, 
	 agent_addr, 
	 generic_trap, 
	 specific_trap,
	 time_stamp, 
	 varbinds
	}
       ).


%%-----------------------------------------------------------------
%% This record should be used when a Mnesia table for variables
%% is created.
%%-----------------------------------------------------------------
-record(snmp_variables, 
	{name, 
	 value
	}
       ).


%%-----------------------------------------------------------------
%% STD security models (from rfc2271)
%%-----------------------------------------------------------------
-define(SEC_ANY, 0).
-define(SEC_V1,  1).
-define(SEC_V2C, 2).
-define(SEC_USM, 3).


%%-----------------------------------------------------------------
%% The OTP Security Model (ericsson * 256 + otp)
%% (works for Community based SNMP i.e. v1 and v2c)
%%-----------------------------------------------------------------
-define(SEC_OTP, 49427).


%%-----------------------------------------------------------------
%% STD message processing models (from rfc2271)
%%-----------------------------------------------------------------
-define(MP_V1,     0).
-define(MP_V2C,    1).
-define('MP_V2U*', 2).
-define(MP_V3,     3).


%%-----------------------------------------------------------------
%% Mib Views
%%-----------------------------------------------------------------
-define(view_included, 1).
-define(view_excluded, 2).

-define(view_wildcard, 0).
-define(view_exact,    1).


%%-----------------------------------------------------------------
%% From SNMPv2-SMI
%%-----------------------------------------------------------------
-define(zeroDotZero, [0,0]).


%%-----------------------------------------------------------------
%% Incremental timer
%%
%% The timer sleeps in WaitFor milli seconds and when it
%% times out, a new time out value is computed:
%%
%%   WaitFor2 = WaitFor * Factor + Incr
%%
%% And the timer starts all over again with the new WaitFor value.
%% The procedure is repeated at most MaxRetries.
%%-----------------------------------------------------------------

-record(snmp_incr_timer, 
	{wait_for    = timer:seconds(5),
	 factor      = 2,
	 incr        = 0,
	 max_retries = infinity
	}
       ).

%%-----------------------------------------------------------------
%% Inform delivery information
%%
%% This record defines the info related to inform delivery info.
%% That is, when sending an inform, info about the delivery (such
%% if it was acknowledged) will be delivered using the info in
%% this record.
%%
%% The delivery will be performed according to:
%%
%%     Mod:inform_delivery_targets(Tag, Addresses, Extra)
%%     Mod:inform_delivery_info(Tag, Address, DeliveryResult, Extra)
%%
%% The Extra is any term, provided by the user.
%% 
%% The fields of this record has the following meaning:
%% tag - term()   - Value selected by the user to identify this 
%%                  sending
%% mod - module() - The callback module implementing the 
%%                  snmpa_notification_delivery_info_receiver 
%%                  behaviour
%% extra - term() - Any extra info the user wants passed along
%% 
%%-----------------------------------------------------------------

-record(snmpa_notification_delivery_info,
        {
          tag, 
          mod, 
          extra 
       }).

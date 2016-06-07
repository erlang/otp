%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(snmpm_user).

-export_type([
	      snmp_gen_info/0, 
	      snmp_v1_trap_info/0 
	     ]).

-type snmp_gen_info() :: {ErrorStatus :: atom(), 
			  ErrorIndex :: pos_integer(), 
			  Varbinds :: [snmp:varbind()]}.
-type snmp_v1_trap_info() :: {Enteprise :: snmp:oid(), 
			      Generic   :: integer(), 
			      Spec      :: integer(), 
			      Timestamp :: integer(), 
			      Varbinds  :: [snmp:varbind()]}.
-type ip_address()  :: inet:ip_address().
-type port_number() :: inet:port_number().


%% *** handle_error ***
%% An "asynchronous" error has been detected 

-callback handle_error(
	    ReqId :: integer(), 
	    Reason :: {unexpected_pdu, SnmpInfo :: snmp_gen_info()} |
		      {invalid_sec_info,
		       SecInfo :: term(),
		       SnmpInfo :: snmp_gen_info()} | 
		      {empty_message,
		       TransportDomain :: atom(),
		       {Addr :: ip_address(), Port :: port_number()}} | 
		      term(), 
	    UserData :: term()) ->
    snmp:void().


%% *** handle_agent ***
%% A message was received from an unknown agent

-callback handle_agent(Domain   :: atom(),
		       Address  :: term(),
		       Type     :: pdu | trap | inform | report, 
		       SnmpInfo :: snmp_gen_info() | snmp_v1_trap_info(), 
		       UserData :: term()) ->
    Reply :: ignore | 
	     {register, 
	      UserId      :: term(), 
	      RTargetName :: snmpm:target_name(), 
	      AgentConfig :: [snmpm:agent_config()]}. 


%% *** handle_pdu ***
%% Handle the reply to an async request (such as get, get-next and set).

-callback handle_pdu(TargetName   :: snmpm:target_name(), 
		     ReqId        :: term(), 
		     SnmpResponse :: snmp_gen_info(), 
		     UserData     :: term()) ->
    snmp:void().


%% *** handle_trap ***
%% Handle a trap/notification message received from an agent

-callback handle_trap(TargetName   :: snmpm:target_name(), 
		      SnmpTrapInfo :: snmp_gen_info() | snmp_v1_trap_info(), 
		      UserData     :: term()) ->
    Reply :: ignore | 
	     unregister | 
	     {register, 
	      UserId      :: term(), 
	      RTargetName :: snmpm:target_name(), 
	      AgentConfig :: [snmpm:agent_config()]}.


%% *** handle_inform ***
%% Handle a inform message received from an agent

-callback handle_inform(TargetName :: snmpm:target_name(), 
			SnmpInform :: snmp_gen_info(), 
			UserData   :: term()) ->
    Reply :: ignore | no_reply | 
	     unregister | 
	     {register, 
	      UserId      :: term(), 
	      RTargetName :: snmpm:target_name(), 
	      AgentConfig :: [snmpm:agent_config()]}.


%% *** handle_report *** 
%% Handle a report message received from an agent

-callback handle_report(TargetName :: snmpm:target_name(), 
			SnmpReport :: snmp_gen_info(), 
			UserData   :: term()) ->
    Reply :: ignore | 
	     unregister | 
	     {register, 
	      UserId      :: term(), 
	      RTargetName :: snmpm:target_name(), 
	      AgentConfig :: [snmpm:agent_config()]}.




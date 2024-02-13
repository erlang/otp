%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2024. All Rights Reserved.
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
-moduledoc """
Behaviour module for the SNMP manager user.

This module defines the behaviour of the manager user. A `snmpm_user` compliant
module must export the following functions:

- handle_error/3
- handle_agent/4
- handle_pdu/4
- handle_trap/3
- handle_inform/3
- handle_report/3
- handle_invalid_result/2

The semantics of them and their exact signatures are explained below.

Some of the function has no defined return value (`void()`), they can of course
return anything. But the functions that do have specified return value(s) _must_
adhere to this. None of the functions can use exit of throw to return.

If the manager is not configured to use any particular transport domain, the
behaviour `handle_agent/4` will for backwards copmpatibility reasons be called
with the old `IpAddr` and `PortNumber` arguments

## DATA TYPES

```erlang
snmp_gen_info() = {ErrorStatus :: atom(),
                   ErrorIndex  :: pos_integer(),
                   Varbinds    :: [snmp:varbind()]}
snmp_v1_trap_info() :: {Enteprise :: snmp:oid(),
                        Generic   :: integer(),
                        Spec      :: integer(),
                        Timestamp :: integer(),
                        Varbinds  :: [snmp:varbind()]}
```

[](){: #handle_error }
""".

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

-doc """
This function is called when the manager needs to communicate an "asynchronous"
error to the user: e.g. failure to send an asynchronous message (i.e. encoding
error), a received message was discarded due to security error, the manager
failed to generate a response message to a received inform-request, or when
receiving an unexpected PDU from an agent (could be an expired async request).

If `ReqId` is less then 0, it means that this information was not available to
the manager (that info was never retrieved before the message was discarded).

For `SnmpInfo` see handle_agent below.

Note that there is a special case when the value of `ReqId` has the value of the
atom `netif`. This means that the NetIF process has suffered a "fatal" error and
been restarted. With possible loss of traffic\!

[](){: #handle_agent }
""".
-callback handle_error(
	    ReqId :: netif | integer(), 
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

-doc """
This function is called when a message is received from an unknown agent.

Note that this will always be the default user that is called.

For more info about the `agent_config()`, see
[register_agent](`m:snmpm#register_agent`).

The arguments `Type` and `SnmpInfo` relates in the following way:

- `pdu` \- `SnmpPduInfo` (see [handle_pdu](`m:snmpm_user#handle_pdu`) for more
  info).
- `trap` \- `SnmpTrapInfo` (see [handle_trap](`m:snmpm_user#handle_trap`) for
  more info).
- `report` \- `SnmpReportInfo` (see
  [handle_report](`m:snmpm_user#handle_report`) for more info).
- `inform` \- `SnmpInformInfo` (see
  [handle_inform](`m:snmpm_user#handle_inform`) for more info).

The only user which would return `{register, UserId, TargetName, AgentConfig}`
is the _default user_.

[](){: #handle_pdu }
""".
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

-doc """
Handle the reply to an asynchronous request, such as
[async_get](`m:snmpm#async_get2`), [async_get_next](`m:snmpm#async_get_next2`)
or [async_set](`m:snmpm#async_set2`).

It could also be a late reply to a synchronous request.

`ReqId` is returned by the asynchronous request function.

[](){: #handle_trap }
""".
-callback handle_pdu(TargetName   :: snmpm:target_name(), 
		     ReqId        :: term(), 
		     SnmpResponse :: snmp_gen_info(), 
		     UserData     :: term()) ->
    snmp:void().


%% *** handle_trap ***
%% Handle a trap/notification message received from an agent

-doc """
Handle a trap/notification message from an agent.

For more info about the `agent_config()`, see
[register_agent](`m:snmpm#register_agent`)

The only user which would return `{register, UserId, TargetName2, agent_info()}`
is the _default user_.

[](){: #handle_inform }
""".
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

-doc """
Handle a inform message.

For more info about the `agent_config()`, see
[register_agent](`m:snmpm#register_agent`)

The only user which would return `{register, UserId, TargetName2, AgentConfig}`
is the _default user_.

If the [inform request behaviour](snmp_config.md#manager_irb) configuration
option is set to `user` or `{user, integer()}`, the response (acknowledgment) to
this inform-request will be sent when this function returns.

[](){: #handle_report }
""".
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

-doc """
Handle a report message.

For more info about the `agent_config()`, see
[register_agent](`m:snmpm#register_agent`)

The only user which would return `{register, UserId, TargetName2, AgentConfig}`
is the _default user_.

[](){: #handle_invalid_result }
""".
-callback handle_report(TargetName :: snmpm:target_name(), 
			SnmpReport :: snmp_gen_info(), 
			UserData   :: term()) ->
    Reply :: ignore | 
	     unregister | 
	     {register, 
	      UserId      :: term(), 
	      RTargetName :: snmpm:target_name(), 
	      AgentConfig :: [snmpm:agent_config()]}.

-doc """
If _any_ of the _other_ callback functions crashes (exit, throw or a plain
crash) or return an invalid result (if a valid return has been specified), this
function is called. The purpose is to allow the user handle this error (for
instance to issue an error report).

`IN` reprecents the function called (and its arguments). `OUT` represents the
unexpected/invalid result.
""".
-doc(#{since => <<"OTP R16B03">>}).
-callback handle_invalid_result(In, Out) -> no_return() when
      In :: {Fun :: atom(), Args :: list()},
      Out :: {crash, CrashInfo} | {result, InvalidResult :: term()},
      CrashInfo :: {ErrorType :: atom(), Error :: term(), Stacktrace :: erlang:stacktrace()}.

-optional_callbacks([handle_invalid_result/2]).

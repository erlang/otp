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
-module(snmpa_network_interface).
-moduledoc """
Behaviour module for the SNMP agent network interface.

This module defines the behaviour of the agent network interface. A
`snmpa_network_interface` compliant module must export the following functions:

- [start_link/4](`m:snmpa_network_interface#start_link`)
- [info/1](`m:snmpa_network_interface#info`)
- [get_log_type/1](`m:snmpa_network_interface#get_log_type`)
- [set_log_type/2](`m:snmpa_network_interface#set_log_type`)
- [verbosity/2](`m:snmpa_network_interface#verbosity`)

The semantics of them and their exact signatures are explained below.

But this is not enough. There is also a set of _mandatory_ messages which the
network interface entity must be able to receive and be able to send. This is
described in chapter [snmp_agent_netif](snmp_agent_netif.md).

[](){: #start_link }
""".

%% Note that this behaviour is not enough!
%% There is also a set of mandatory messages which the
%% network interface entity must be able to receive and
%% be able to send. See the documentation for more info.

-doc """
Start-link the network interface process.

`NoteStore` is the pid of the note-store process and `MasterAgent` is the pid of
the master-agent process.

`Opts` is an (basically) implementation dependent list of options to the network
interface process. There are however a number of options which _must_ be
handled: `versions` and `verbosity`.

[](){: #info }
""".
-callback start_link(Prio, NoteStore, MasterAgent, Opts) ->
    {ok, Pid} | {error, Reason} when
      Prio        :: low | normal | high, % priority_level(),
      NoteStore   :: pid(),
      MasterAgent :: pid(),
      Opts        :: [Option],
      Option      :: {verbosity, snmp:verbosity()} |
                     {versions,  [snmp:version()]} |
                     term(),
      Pid         :: pid(),
      Reason      :: term().

-doc """
The info returned is basically up to the implementer to decide. This
implementation provided by the application provides info about memory allocation
and various socket information.

The info returned by this function is returned together with other info
collected by the agent when the [info](`m:snmpa#info`) function is called
(tagged with with the key `net_if`).

[](){: #verbosity }
""".
-callback info(Pid) ->
    Info when
      Pid   :: pid(),
      Info  :: [{Key, Value}],
      Key   :: term(),
      Value :: term().

-doc """
Change the verbosity of a running network interface process.

[](){: #get_log_type }
""".
-callback verbosity(Pid, Verbosity) ->
    snmp:void() when
      Pid       :: pid(),
      Verbosity :: snmp:verbosity().

-doc """
The Audit Trail Log is managed by the network interface process. So, it is this
process that has to retrieve the actual log-type.

[](){: #set_log_type }
""".
-callback get_log_type(Pid) ->
    {ok, LogType} | {error, Reason} when
      Pid     :: pid(),
      LogType :: snmp:atl_type(),
      Reason  :: term().

-doc """
The Audit Trail Log is managed by the network interface process. So, it is this
process that has to do the actual changing of the type.

See [set_log_type](`m:snmpa#set_log_type`) for more info.
""".
-callback set_log_type(Pid, NewType) ->
    {ok, OldType} | {error, Reason} when
      Pid     :: pid(),
      NewType :: snmp:atl_type(),
      OldType :: snmp:atl_type(),
      Reason  :: term().

-callback get_request_limit(Pid) ->
    {ok, Limit} when
      Pid   :: pid(),
      Limit :: non_neg_integer() | infinity.

-callback set_request_limit(Pid, NewLimit) ->
    {ok, OldLimit} when
      Pid      :: pid(),
      NewLimit :: non_neg_integer() | infinity,
      OldLimit :: non_neg_integer() | infinity.


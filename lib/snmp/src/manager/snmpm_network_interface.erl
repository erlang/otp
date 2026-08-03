%% 
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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

-module(snmpm_network_interface).
-moduledoc """
Behaviour module for the SNMP manager network interface.

This module defines the behaviour of the manager network interface. A
`snmpm_network_interface` compliant module must export the following functions:

- `c:start_link/2`
- `c:stop/1`
- `c:send_pdu/7`
- `c:inform_response/4`
- `c:note_store/2`
- `c:info/1`
- `c:get_log_type/1`
- `c:set_log_type/2`
- `c:verbosity/2`

The semantics of them and their exact signatures are explained below.

Legacy API function `c:send_pdu/7` that has got separate
`IpAddr` and `PortNumber` arguments still works as before for backwards
compatibility reasons.
""".

-doc """
Start-link the network interface process.

`Server` is the pid of the managing process.

`NoteStore` is the pid of the `note-store` process.
""".
-callback start_link(Server, NoteStore) ->
    {ok, Pid} | {error, Reason} when
      Server    :: pid(),
      NoteStore :: pid(),
      Pid       :: pid(),
      Reason    :: term().

-doc """
Stop the network interface process.
""".
-callback stop(Pid) ->
    snmp:void() when
      Pid :: pid().

-doc """
Request the network interface process (`Pid`) to send this pdu (`Pdu`).

`ExtraInfo` is some opaque data that is passed to the `net-if` process. It
originates from the `ExtraInfo` parameter in the calls to the
[`synchronous get-request`](`snmpm:sync_get2/4`),
[`asynchronous get-request`](`snmpm:async_get2/4`),
[`synchronous get-next-request`](`snmpm:sync_get_next2/4`),
[`asynchronous get-next-request`](`snmpm:async_get_next2/4`),
[`synchronous set-request`](`snmpm:sync_set2/4`) and
[`asynchronous set-request`](`snmpm:async_set2/4`) functions.
Whether the `net-if` process chooses to use this is implementation dependent.
The `net-if` process included in this application ignores it.
""".
-callback send_pdu(Pid, Pdu, Vsn, MsgData, Domain, Addr, ExtraInfo) ->
    snmp:void() when
      Pid       :: pid(),
      Pdu       :: snmp:pdu(),
      Vsn       :: 'version-1' | 'version-2' | 'version-3',
      MsgData   :: term(),
      Domain    :: snmp:tdomain(),
      Addr      :: {inet:ip_address(), inet:port_number()},
      ExtraInfo :: term().

-doc """
Instruct the network interface process to send the response (acknowledgment) to
an inform-request.

`Ref` is something that can be used to identify the inform-request, e.g.
request-id of the inform-request.

`Addr` and `Port` identifies the agent, from which the inform-request
originated.
""".
-callback inform_response(Pid, Ref, Addr, Port) ->
    snmp:void() when
      Pid  :: pid(),
      Ref  :: term(),
      Addr :: inet:ip_address(),
      Port :: inet:port_number().

-doc """
Change the pid of the note-store process. This is used when the server restarts
the `note-store` (e.g. after a crach).
""".
-callback note_store(Pid, NoteStore) ->
    snmp:void() when
      Pid       :: pid(),
      NoteStore :: pid().

-doc """
The info returned is basically up to the implementer to decide. The
implementation provided by this application provides info about memory
allocation and various socket information.

The info returned by this function is returned together with other info
collected by the manager when the [`snmpm:info()`](`snmpm:info/0`)
function is called (tagged with the key `net_if`).
""".
-callback info(Pid) ->
    Info when
      Pid   :: pid(),
      Info  :: [{Key, Value}],
      Key   :: term(),
      Value :: term().

-doc """
Change the verbosity of the network interface process.
""".
-callback verbosity(Pid, Verbosity) ->
    snmp:void() when
      Pid       :: pid(),
      Verbosity :: snmp:verbosity().

-doc """
The Audit Trail Log is managed by the network interface process. So, it is this
process that has to return the actual log-type.
""".
-callback get_log_type(Pid) ->
    {ok, LogType} | {error, Reason} when
      Pid     :: pid(),
      LogType :: snmp:atl_type(),
      Reason  :: term().

-doc """
The Audit Trail Log is managed by the network interface process. So, it is this
process that has to do the actual changing of the type.

See `snmpm:set_log_type/1` for more info.
""".
-callback set_log_type(Pid, NewType) ->
    {ok, OldType} | {error, Reason} when
      Pid     :: pid(),
      NewType :: snmp:atl_type(),
      OldType :: snmp:atl_type(),
      Reason  :: term().


%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2019. All Rights Reserved.
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

-callback start_link(Server, NoteStore) ->
    {ok, Pid} | {error, Reason} when
      Server    :: pid(),
      NoteStore :: pid(),
      Pid       :: pid(),
      Reason    :: term().

-callback stop(Pid) ->
    snmp:void() when
      Pid :: pid().

-callback send_pdu(Pid, Pdu, Vsn, MsgData, Domain, Addr, ExtraInfo) ->
    snmp:void() when
      Pid       :: pid(),
      Pdu       :: snmp:pdu(),
      Vsn       :: 'version-1' | 'version-2' | 'version-3',
      MsgData   :: term(),
      Domain    :: snmp:tdomain(),
      Addr      :: {inet:ip_address(), inet:port_number()},
      ExtraInfo :: term().

-callback inform_response(Pid, Ref, Addr, Port) ->
    snmp:void() when
      Pid  :: pid(),
      Ref  :: term(),
      Addr :: inet:ip_address(),
      Port :: inet:port_number().

-callback note_store(Pid, NoteStore) ->
    snmp:void() when
      Pid       :: pid(),
      NoteStore :: pid().

-callback info(Pid) ->
    Info when
      Pid   :: pid(),
      Info  :: [{Key, Value}],
      Key   :: term(),
      Value :: term().

-callback verbosity(Pid, Verbosity) ->
    snmp:void() when
      Pid       :: pid(),
      Verbosity :: snmp:verbosity().

-callback get_log_type(Pid) ->
    {ok, LogType} | {error, Reason} when
      Pid     :: pid(),
      LogType :: snmp:atl_type(),
      Reason  :: term().

-callback set_log_type(Pid, NewType) ->
    {ok, OldType} | {error, Reason} when
      Pid     :: pid(),
      NewType :: snmp:atl_type(),
      OldType :: snmp:atl_type(),
      Reason  :: term().


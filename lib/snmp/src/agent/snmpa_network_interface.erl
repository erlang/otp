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
-module(snmpa_network_interface).

%% Note that this behaviour is not enough!
%% There is also a set of mandatory messages which the
%% network interface entity must be able to receive and
%% be able to send. See the documentation for more info.

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

-callback get_request_limit(Pid) ->
    {ok, Limit} when
      Pid   :: pid(),
      Limit :: non_neg_integer() | infinity.

-callback set_request_limit(Pid, NewLimit) ->
    {ok, OldLimit} when
      Pid      :: pid(),
      NewLimit :: non_neg_integer() | infinity,
      OldLimit :: non_neg_integer() | infinity.


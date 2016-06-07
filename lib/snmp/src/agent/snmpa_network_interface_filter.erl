%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
-module(snmpa_network_interface_filter).

-export([behaviour_info/1]).
-export([verify/1]).


behaviour_info(callbacks) ->
    [{accept_recv,     2},
     {accept_send,     2},
     {accept_recv_pdu, 3},
     {accept_send_pdu, 2}];
behaviour_info(_) ->
    undefined.


%% accept_recv({domain(), address()}) -> boolean()
%% Called at the receiption of a message 
%% (before *any* processing has been done).
%% 
%% accept_send({domain(), address()}) -> boolean()
%% Called before the sending of a message 
%% (after *all* processing has been done).
%% 
%% accept_recv_pdu({domain(), address()}, pdu_type()) -> boolean()
%% Called after the basic message processing (MPD) has been done, 
%% but before the pdu is handed over to the master-agent for 
%% primary processing.
%% 
%% accept_send_pdu([{domain(), address()}, ...] = Targets, pdu_type()) ->
%%     boolean() | NewTargets
%% Called before the basic message processing (MPD) is done, 
%% when a pdu has been received from the master-agent.
%% 


verify(Module) ->
    snmp_misc:verify_behaviour(?MODULE, Module).

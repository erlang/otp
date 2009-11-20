%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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


%% accept_recv(address(), port()) -> boolean() 
%% Called at the receiption of a message 
%% (before *any* processing has been done).
%% 
%% accept_send(address(), port()) -> boolean()
%% Called before the sending of a message 
%% (after *all* processing has been done).
%% 
%% accept_recv_pdu(Addr, Port, pdu_type()) -> boolean()
%% Called after the basic message processing (MPD) has been done, 
%% but before the pdu is handed over to the master-agent for 
%% primary processing.
%% 
%% accept_send_pdu(Targets, pdu_type()) -> boolean() | NewTargets
%% Called before the basic message processing (MPD) is done, 
%% when a pdu has been received from the master-agent.
%% 


verify(Module) ->
    snmp_misc:verify_behaviour(?MODULE, Module).

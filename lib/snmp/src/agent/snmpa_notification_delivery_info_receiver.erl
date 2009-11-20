%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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

-module(snmpa_notification_delivery_info_receiver).

-export([behaviour_info/1]).
-export([verify/1]).

behaviour_info(callbacks) ->
    [
     {delivery_targets, 3},
     {delivery_info,    4}
    ];
behaviour_info(_) ->
    undefined.


verify(Module) ->
    snmp_misc:verify_behaviour(?MODULE, Module).

%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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

-module(snmpm_network_interface).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start_link,          2}, 
     {stop,                1},
     {send_pdu,            7},
     {inform_response,     4},
     {note_store,          2},
     {info,                1},
     {verbosity,           2},
     %% {system_info_updated, 2},
     {get_log_type,        1},
     {set_log_type,        2}];
behaviour_info(_) ->
    undefined.


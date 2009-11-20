%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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

-module(snmpm_user_old).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{handle_error,    3}, 
     {handle_agent,    4}, 
     {handle_pdu,      5},
     {handle_trap,     4},
     {handle_inform,   4},
     {handle_report,   4}];
behaviour_info(_) ->
    undefined.


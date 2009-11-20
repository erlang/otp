%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Megaco encoder behaviour module
%%----------------------------------------------------------------------

-module(megaco_encoder).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{encode_message,         3}, 
     {decode_message,         3},
     {decode_mini_message,    3},
     {encode_transaction,     3},
     {encode_action_requests, 3},
     {encode_action_reply,    3}];
behaviour_info(_) ->
    undefined.

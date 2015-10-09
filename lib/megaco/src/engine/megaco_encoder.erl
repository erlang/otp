%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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

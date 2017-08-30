%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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


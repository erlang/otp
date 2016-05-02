%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start_link,        4}, 
     {get_log_type,      1},
     {set_log_type,      2},
     {get_request_limit, 1},
     {set_request_limit, 2},
     {info,              1}, 
     {verbosity,         2}];
behaviour_info(_) ->
    undefined.


%% behaviour_info(callbacks) ->
%%     [{start_link,        4}, 
%%      {send_pdu,          5},
%%      {send_response_pdu, 6},
%%      {discard_pdu,       6},
%%      {send_pdu_req,      6},
%%      {verbosity,         2}, 
%%      {change_log_type, 2}];
%% behaviour_info(_) ->
%%     undefined.


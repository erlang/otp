-module(record_inf).

-export([type_opts/1]).

type_opts(_Config) ->
    r_list([]),
    r_list([1,2,3]).

-record #r_cons{hd, tl}.
-record #r_nil{hd, tl}.


r_list([H|T]) ->
    #r_cons{hd=H, tl=T};
r_list([]) ->
    #r_nil{hd=h, tl=t}.

%% %CopyrightBegin%
%% 
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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
-module(native_record_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
         term_order/1]).

-record #a{x=1, y=2}.
-record #b{a=1, b=2}.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [term_order].

groups() ->
    [].

init_per_suite(Config) ->
    id(Config),
    %% {module,ext_records} = code:ensure_loaded(ext_records),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

term_order(_Config) ->
    true = id(#a{}) < id(#b{}),
    true = id(#a{}) =:= id(#a{}),
    true = id(#a{x=1}) < id(#a{x=2}),
    true = id(#a{x=10}) > id(#a{x=0}),

    true = id(#a{}) =/= id(#b{}),

    ok.

%%% Common utilities.

id(I) ->
    I.

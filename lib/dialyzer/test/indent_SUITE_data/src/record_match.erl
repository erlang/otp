-module(record_match).

-export([select/0, n_select/0]).

-record(b_literal, {val}).
-record(b_remote, {mod,name,arity}).
-record(b_local, {name,arity}).

-type b_remote()   :: #b_remote{}.
-type b_local()    :: #b_local{}.

-type argument()   :: b_remote() | b_local().

-record(b_set, {args=[] :: [argument()]}).

select() ->
    #b_set{args=[#b_remote{},#b_literal{}]}.

-record #n_literal{val=0}.
-record #n_remote{mod=m,name=n,arity=0}.
-record #n_local{name,arity}.

-type n_remote()   :: #n_remote{}.
-type n_local()    :: #n_local{}.

-type n_argument()   :: n_remote() | n_local().

-record #n_set{args=[] :: [n_argument()]}.

n_select() ->
    #n_set{args=[#n_remote{},#n_literal{}]}.

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

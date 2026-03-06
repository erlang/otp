%%%-------------------------------------------------------------------
%%% File    : record_pat.erl
%%% Author  : Tobias Lindahl <>
%%% Description : Emit warning if a pattern violates the record type
%%%
%%% Created : 21 Oct 2008 by Tobias Lindahl <>
%%%-------------------------------------------------------------------
-module(record_pat).

-export([t/1, n_t/1]).

-record(foo, {bar :: integer()}).

t(#foo{bar=baz}) -> no_way;
t(#foo{bar=1}) -> ok.

-record #n_foo{bar :: integer()}.

n_t(#n_foo{bar=baz}) -> no_way;
n_t(#n_foo{bar=1}) -> ok.

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

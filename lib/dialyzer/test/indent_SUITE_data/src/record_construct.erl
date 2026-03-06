-module(record_construct).
-export([t_loc/0, t_opa/0, t_rem/0, n_loc/0, n_opa/0, n_rem/0, foo2/1]).

-record(r_loc, {a = gazonk :: integer(), b = 42 :: atom()}).

t_loc() ->
  #r_loc{}.

-record(r_opa, {a                 :: atom(),
		b = gb_sets:new() :: gb_sets:set(),
		c = 42            :: boolean(),
		d,	% untyped on purpose
		e = false         :: boolean()}).

t_opa() ->
  #r_opa{}.

-record(r_rem, {a = gazonk :: string()}).

t_rem() ->
  #r_rem{}.

-record #n_loc{a = gazonk :: integer(), b = 42 :: atom()}.

n_loc() ->
  #n_loc{}.

-record #n_opa{a                 :: atom(),
		c = 42            :: boolean(),
		d,	% untyped on purpose
		e = false         :: boolean()}.

n_opa() ->
  #n_opa{a=atom,d=0}.

-record(n_rem, {a = gazonk :: string()}).

n_rem() ->
  #n_rem{}.

-import_record(record_creation_diffs, [n]).

foo2(Input) ->
    #n{some_atom = Input, some_list = {this,is,a,tuple}}.

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

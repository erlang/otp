-module(record_update).

-export([quux/2, n_quux/2, update/1]).

-record(foo, {bar :: atom()}).

-spec quux(#foo{}, string()) -> #foo{}.

quux(Foo, NotBar) ->
  Foo#foo{ bar = NotBar }.

-record #n_foo{bar :: atom()}.

update(NFoo) ->
  NFoo#n_foo{bar = 1}.

-spec n_quux(#n_foo{}, string()) -> #n_foo{}.

n_quux(NFoo, NotBar) ->
  NFoo#n_foo{bar = NotBar}.

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

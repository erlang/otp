%%%-------------------------------------------------------------------
%%% File    : record_test.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description :
%%%
%%% Created : 22 Oct 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(record_test).

-export([t/0, n_t/0]).

-record(foo, {bar}).

t() ->
  doit(foo).

doit(X) ->
  case X of
    #foo{} -> error1;
    foo -> ok;
    _ -> error2
  end.

-record #n_foo{bar}.

n_t() ->
  do(n_foo).

do(X) ->
  case X of
    #n_foo{} -> error1;
    n_foo -> ok;
    _ -> error2
  end.

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

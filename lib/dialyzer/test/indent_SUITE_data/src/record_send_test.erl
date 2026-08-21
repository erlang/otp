%%-------------------------------------------------------------------
%% File    : record_send_test.erl
%% Author  : Kostis Sagonas <kostis@it.uu.se>
%% Description : A test inspired by a post of Mkcael Remond to the
%%		 Erlang mailing list suggesting thst Dialyzer should
%%		 be reporting sends to records rather than to pids.
%%		 Dialyzer v1.3.0 indeed reports one of the discrepancies
%%		 (the one with the 4-tuple) but not the one where the
%%		 message is sent to a pair which is a record.
%%		 This should be fixed.
%%
%% Created : 10 Apr 2005 by Kostis Sagonas <kostis@it.uu.se>
%%-------------------------------------------------------------------
-module(record_send_test).

-export([t/0, n_t/0]).

-record(rec1, {a=a, b=b, c=c}).
-record(rec2, {a}).

t() ->
  t(#rec1{}).

t(Rec1 = #rec1{b=B}) ->
  Rec2 = some_mod:some_function(),
  if
    is_record(Rec2, rec2) ->
      Rec2 ! hello;	%% currently this one is not found
    true ->
      Rec1 ! hello_again
  end,
  B.

-record #n_rec1{a=a, b=b, c=c}.
-record #n_rec2{a}.

n_t() ->
  n_t(#n_rec1{}).

n_t(Rec1 = #n_rec1{b=B}) ->
  Rec2 = some_mod:some_function(),
  if
    is_record(Rec2, n_rec2) ->
      Rec2 ! hello;	%% currently this one is not found
    true ->
      Rec1 ! hello_again
  end,
  B.

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

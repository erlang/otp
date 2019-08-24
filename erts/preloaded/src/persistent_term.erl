%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-module(persistent_term).

-export([erase/1,get/0,get/1,get/2,info/0,put/2]).

-type key() :: term().
-type value() :: term().

-spec erase(Key) -> Result when
      Key :: key(),
      Result :: boolean().
erase(_Key) ->
    erlang:nif_error(undef).

-spec get() -> List when
      List :: [{key(),value()}].
get() ->
    erlang:nif_error(undef).

-spec get(Key) -> Value when
      Key :: key(),
      Value :: value().
get(_Key) ->
    erlang:nif_error(undef).

-spec get(Key, Default) -> Value when
      Key :: key(),
      Default :: value(),
      Value :: value().
get(_Key, _Default) ->
    erlang:nif_error(undef).

-spec info() -> Info when
      Info :: #{'count':=Count,'memory':=Memory},
      Count :: non_neg_integer(),
      Memory :: non_neg_integer().
info() ->
    erlang:nif_error(undef).

-spec put(Key, Value) -> 'ok' when
      Key :: key(),
      Value :: value().
put(_Key, _Value) ->
    erlang:nif_error(undef).

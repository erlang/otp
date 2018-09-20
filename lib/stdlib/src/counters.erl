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

%% Purpose : Main counters API module.

-module(counters).

-export([new/2, add/3, add_get/3, sub/3, sub_get/3, put/3, get/2,
         size/1, max/1, min/1]).

-export_type([counters_ref/0]).

-opaque counters_ref() :: reference().


-spec new(Arity, Opts) -> counters_ref() when
      Arity :: pos_integer(),
      Opts :: [].
new(_Arity, _Opts) ->
    erlang:nif_error(undef).

-spec put(Ref, Ix, Value) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Value :: integer().
put(_Ref, _Ix, _Value) ->
    erlang:nif_error(undef).

-spec get(Ref, Ix) -> integer() when
      Ref  :: counters_ref(),
      Ix :: integer().
get(_Ref, _Ix) ->
    erlang:nif_error(undef).

-spec add(Ref, Ix, Incr) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Incr :: integer().
add(_Ref, _Ix, _Incr) ->
    erlang:nif_error(undef).

-spec add_get(Ref, Ix, Incr) -> integer() when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Incr :: integer().
add_get(_Ref, _Ix, _Incr) ->
    erlang:nif_error(undef).

-spec sub(Ref, Ix, Decr) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Decr :: integer().
sub(Ref, Ix, Decr) ->
    ?MODULE:add(Ref, Ix, -Decr).

-spec sub_get(Ref, Ix, Decr) -> integer() when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Decr :: integer().
sub_get(Ref, Ix, Decr) ->
    ?MODULE:add_get(Ref, Ix, -Decr).

-spec size(Ref) -> integer() when
      Ref  :: counters_ref().
size(_Ref) ->
    erlang:nif_error(undef).

-spec max(Ref) -> integer() when
      Ref  :: counters_ref().
max(_Ref) ->
    erlang:nif_error(undef).

-spec min(Ref) -> integer() when
      Ref  :: counters_ref().
min(_Ref) ->
    erlang:nif_error(undef).

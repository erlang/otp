%% %CopyrightBegin%
%% 
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2022-2026. All Rights Reserved.
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

-module(redefine_builtin_type).
-export([lookup/2, verify_mfa/1, verify_pid/1, foo/1]).
-export_type([my_map/0, my_tuple/0]).

-type map() :: {atom(), erlang:map()}.

-spec lookup(atom(), map()) -> {'ok', term()} | 'error'.

lookup(Key, {Key, Map}) when is_atom(Key), is_map(Map) ->
    {ok, Map};
lookup(Key1, {Key2, Map}) when is_atom(Key1), is_atom(Key2), is_map(Map) ->
    error.

%% Type `mfa()` depends on `erlang::module()`. Make sure that `mfa()`
%% does not attempt to use our local definition of `module()`.

-type module() :: pid().

-spec verify_mfa(mfa()) -> 'ok'.
verify_mfa({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    ok.

-spec verify_pid(module()) -> 'ok'.
verify_pid(Pid) when is_pid(Pid) ->
    ok.

-type map(_Type) :: map().            %% should not override the builtin map/0
-type my_map() :: undefined | #{atom() => string()} | '_'.

-type tuple(_Type) :: integer().
-type my_tuple() :: {atom()}.

%% Should emit Dialyzer warning.
-spec foo(my_tuple()) -> ok.
foo(1) -> ok.

-module(callback).

%% -doc "
%% This should be ignored
%% ".
%% -behaviour(gen_server).

-export([all_ok/0, main/0, main2/0, foo/1]).

-doc "
Callback fn that always returns ok.
".
-callback all_ok() -> ok.

-doc "
Test changing order
".
-doc #{equiv => ok()}.
-callback change_order() -> Order :: boolean().

-callback param(X) -> X.
-callback ann(X :: integer()) -> Y :: integer().

-callback multi_no_slogan(X :: integer()) -> X :: integer();
                         (X :: atom()) -> X :: atom().


-doc "multi(Argument)

A multiclause callback with slogan docs".
-callback multi(X :: integer()) -> X :: integer();
               (X :: atom()) -> X :: atom().

-callback bounded(X) -> integer() when X :: integer().

-doc hidden.
-callback warn() -> ok.

-doc hidden.
-compile({nowarn_hidden_doc, nowarn/1}).
-callback nowarn(Arg :: atom()) -> ok.

-doc "Foos the module foo".
-callback foo(CallbackParam :: term()) -> term().


-doc #{equiv => ok/0}.
-doc "
This is a test
".
all_ok() ->
    all_ok().

-doc #{equiv => main()}.
-spec main() -> ok.
-doc "
all_ok()

Calls all_ok/0
".
main() ->
    all_ok().

-doc #{equiv => main()}.
-doc "
main2()

Second main
".
-spec main2() -> ok.
main2() ->
    ok.

-doc "Foos a generic foo".
-spec foo(FuncParam :: term()) -> term().
foo(_FuncParam) -> ok.

%%
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
%%

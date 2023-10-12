%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022. All Rights Reserved.
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

-module(bs_init_writable).

-export([do/0]).

do() ->
    Val = ex:foo(),
    X = << <<B:1>> || B <- Val >>,
    should_not_have_bitstring_test(X),
    Y = << <<B:8>> || B <- Val >>,
    should_not_have_binary_test(Y).


%% If the beam_ssa_type pass does its job,
%% should_not_have_bitstring_test/1 should not contain a is_bitstr test.
should_not_have_bitstring_test(X) when is_bitstring(X) ->
    bitstring;
should_not_have_bitstring_test(_) ->
    something_else.

%% If the beam_ssa_type pass does its job,
%% should_not_have_binary_test/1 should not contain a is_binary test.
should_not_have_binary_test(X) when is_binary(X) ->
    binary;
should_not_have_binary_test(_) ->
    something_else.

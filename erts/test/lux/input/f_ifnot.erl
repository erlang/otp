%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2022. All Rights Reserved.
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

-module(f_ifnot).

-feature(ifnot_expr, enable).

-export([f0/0,
	 f1/0,
	 f2/0,
	 f3/0,
	 f4/1]).

f0() ->
    ifnot false -> 40 + 2 end.

f1() ->
    ifnot true -> 42 end.

f2() ->
    ifnot begin X = 2, X > 2 end -> true end.

f3() ->
    Z = ifnot begin X = 3, X > 4 end -> Y=7+X, {X, Y} end,
    Z.

f4(X) ->
    ifnot X ->
	      foo
      end.

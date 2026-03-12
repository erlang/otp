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

-module(ct_doctest_parse_rewrite_mod).
-moduledoc false.
-export([f/0, g/0, h/0]).

-doc """
```
1> {lists:seq(1,100), <<1,2,0:1024>>, foo}.
{[1, 2, 3, ...], <<1,2,...>>, ...}
2> #{ a => #{ b => c } }.
#{ a => #{ b => c } }
3> <<1,2,0:1024>>
<<1,2, ...>>
4> <0.1.0>.
<0.1.0>
5> #Port<0.1>.
#Port<0.1>
6> #Ref<0.932613086.1026293762.106328>.
#Ref<0.932613086.1026293762.106328>
7> #{ ref => #Ref<0.932613086.1026293762.106328> }.
#{ ref => #Ref<0.932613086.1026293762.106328> }
8> fun ct_doctest_parse_rewrite_mod:f/0.
fun ct_doctest_parse_rewrite_mod:f/0
9> #{ function => #Fun<lists.foreach.2> }.
#{ function => #Fun<lists.foreach.2> }
10> {ok, <0.1.0>}.
{ok, <0.1.0>}
11> [<0.1.0>, #Ref<0.932613086.1026293762.106328>].
[<0.1.0>, #Ref<0.932613086.1026293762.106328>]
12> {<0.1.0>, #Port<0.1>, #Ref<0.932613086.1026293762.106328>}.
{<0.1.0>, #Port<0.1>, #Ref<0.932613086.1026293762.106328>}
```
""".
f() ->
    ok.

-doc """
Remote function calls cannot be used as match patterns alongside literals.
```
1> {[1,2,3], <0.1.0>}.
{lists:seq(1,3), <0.1.0>}
```
""".
g() ->
    ok.

-doc """
Local function calls cannot be used as match patterns alongside literals.
```
1> {ok, <0.1.0>}.
{foo(1), <0.1.0>}
```
""".
h() ->
    ok.

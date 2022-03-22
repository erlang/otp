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

-module(f_macros).

-export([has_ifn/0,
	 has_hindly_milner/0,
         with_hm/0,
	 uses_ifn/0,
	 uses_maybe/0
	]).

%% This test feature exists
-if(?FEATURE_AVAILABLE(ifn_expr)).
has_ifn() ->
    true.
-else.
has_ifn() ->
    false.
-endif.

%% This feature will probably never exist :-(
-if(?FEATURE_AVAILABLE(hindley_milner)).
has_hindly_milner() ->
    true.
-else.
has_hindly_milner() ->
    false.
-endif.

with_hm() ->
    ?FEATURE_ENABLED(hindley_milner).

-if(?FEATURE_ENABLED(ifn_expr)).
uses_ifn() ->
    true.
-else.
uses_ifn() ->
    false.
-endif.

-if(?FEATURE_ENABLED(while_expr)).
uses_maybe() ->
    true.
-else.
uses_maybe() ->
    false.
-endif.

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

-export([has_experimental/0,
	 has_hindley_milner/0,
         with_hm/0,
	 uses_experimental/0,
	 uses_exp2/0
	]).

%% This test feature exists
-if(?FEATURE_AVAILABLE(experimental_ftr_1)).
has_experimental() ->
    true.
-else.
has_experimental() ->
    false.
-endif.

%% This feature will probably never exist :-(
-if(?FEATURE_AVAILABLE(hindley_milner)).
has_hindley_milner() ->
    true.
-else.
has_hindley_milner() ->
    false.
-endif.

with_hm() ->
    ?FEATURE_ENABLED(hindley_milner).

-if(?FEATURE_ENABLED(experimental_ftr_1)).
uses_experimental() ->
    true.
-else.
uses_experimental() ->
    false.
-endif.

-if(?FEATURE_ENABLED(experimental_ftr_2)).
uses_exp2() ->
    true.
-else.
uses_exp2() ->
    false.
-endif.

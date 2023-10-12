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

-module(f_disable).

-feature(experimental_ftr_1, disable).
-feature(experimental_ftr_2, disable).
-feature(approved_ftr_1, disable).

-export([no_experimental/0,
	 no_ftrs/0]).

-if(?FEATURE_ENABLED(experimental_ftr_1)).
-define(FOO, has_experimental).
-else.
-define(FOO, no_experimental).
-endif.

no_experimental() ->
    ?FOO.

-if(?FEATURE_ENABLED(experimental_ftr_2)).
-define(BAR, has_exp2).
-else.
-define(BAR, no_exp2).
-endif.

no_ftrs() ->
    ?BAR.

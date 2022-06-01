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

-feature(ifn_expr, disable).
-feature(while_expr, disable).

-export([no_ifn/0,
	 no_ftrs/0]).

-if(?FEATURE_ENABLED(ifn_expr)).
-define(FOO, has_ifn).
-else.
-define(FOO, no_ifn).
-endif.

no_ifn() ->
    [ifn, 'while', ?FOO].

-if(?FEATURE_ENABLED(while_expr)).
-define(BAR, has_maybe).
-else.
-define(BAR, no_maybe).
-endif.

no_ftrs() ->
    [ifn, while, until, ?BAR].

%% -compile({feature, ifn_expr, enable}).

%% do(X) ->
%%     ifn X > 10 ->
%% 	    maybe
%%     end.

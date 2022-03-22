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

%% FIXME This file contains too much in erlation to what it tests

-module(f_incorrect_disable).

-feature(disable, unless).

-export([do/1,
	 no_ifn/0,
	 no_ftrs/0]).


-if(?FEATURE_ENABLED(ifn_expr)).
-define(FOO, has_ifn).
-else.
-define(FOO, no_ifn).
-endif.

no_ifn() ->
    [ifn, 'maybe', ?FOO].

-feature(disable, maybe_expr).

-if(?FEATURE_ENABLED(maybe_expr)).
-define(BAR, has_maybe).
-else.
-define(BAR, no_maybe).
-endif.

no_ftrs() ->
    [ifn, maybe, then, ?BAR].

-feature(enable, ifn_expr).

do(X) ->
    ifn X > 10 ->
	    maybe
    end.

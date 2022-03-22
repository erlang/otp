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

-module(f_directives_2).

%% This module uses the feature ifn_expr, so atoms belonging to that
%% featurfe need to be quoted.

-feature(enable, while_expr).
-feature(enable, ifn_expr).
%% Disable feature so atoms beonging to maybe_expr can be unquoted
-feature(disable, while_expr).

-export([foo/0,
	 bar/0,
         baz/1
	]).

foo() ->
    %% Note: xmaybe_expr not active here
    ['ifn', while, until, 'if'].

bar() ->
    ['until', 'while'].

baz(0) ->
    [while];
baz(1) ->
    [until].

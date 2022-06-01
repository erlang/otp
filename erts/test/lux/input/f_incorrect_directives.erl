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

%% This module knows about the both features ifn and maybe
%% These atoms are thus quoted

-module(f_incorrect_directives).

-export([foo/0,
	 bar/0
	]).

%% FIXME This is out of place, but not check in any test.
-feature(ifn, enable).

foo() ->
    %% Note: maybe_expr not active here
    ['ifn', maybe, then, 'if'].

-feature(maybe, enable).

bar() ->
    ['then', 'maybe'].

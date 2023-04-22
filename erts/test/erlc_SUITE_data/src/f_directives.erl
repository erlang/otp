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

-module(f_directives).

-ifdef(disable_unknown).
-feature(unlesser, disable).
-endif.

-ifdef(enable_unknown).
-feature(unlesser, enable).
-endif.

%% This module uses both features experimental_ftr_1 and
%% experimental_ftr_2, so atoms belonging to these need to be quoted.

-feature(experimental_ftr_1, enable).

-ifndef(no_dir_enable_exp2).
-feature(experimental_ftr_2, enable).
-endif.

-ifdef(disable_exp2).
-feature(experimental_ftr_2, disable).
-endif.

-export([bar/0,
	foo/0]).

-ifdef(misplaced_enable).
%% This is out of place and will result in an error
-feature(misplaced, enable).
-endif.

-ifdef(misplaced_disable).
%% This is out of place and will result in an error
-feature(misplaced, disable).
-endif.

bar() ->
    ['ifn'].

-ifdef(disable_exp2).
foo() ->
    %% Note: experimental_ftr_2 not active here
    ['ifn', while, until, 'if'].
-else.
foo() ->
    ['ifn', 'while', 'until', 'if'].
-endif.

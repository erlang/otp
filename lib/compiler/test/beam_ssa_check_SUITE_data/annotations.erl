%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2022. All Rights Reserved.
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
-module(annotations).

-export([t0/0]).

%% Check annotations, do not add lines before this function without
%% changing the location annotation.
t0() ->
%ssa% () when post_ssa_opt ->
%ssa% _ = call(fun return_int/0) { result_type => {t_integer,{17,17}},
%ssa%                              location => {_,32} },
%ssa% _ = call(fun return_tuple/0) {
%ssa%    result_type => {t_tuple,2,true,#{1 => {t_integer,{1,1}},
%ssa%                                     2 => {t_integer,{2,2}}}}
%ssa% }.
    X = return_int(),
    Y = return_tuple(),
    {X, Y}.

return_int() ->
    17.

return_tuple() ->
    {1,2}.

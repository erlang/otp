%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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
-module(ret_annotation).

-export([return_atom/0, return_int/0, return_tuple/0, return_unknown/0]).

%%%
%%% Check that a type annotation is added to return instructions when
%%% the type is known.
%%%

return_atom() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_atom,_} }.
    foo.

return_int() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_integer,_} }.
    17.

return_tuple() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_tuple,2,true,_} }.
    {1,2}.

return_unknown() ->
%ssa% fail () when post_ssa_opt ->
%ssa% ret(_) { result_type => _ },
%ssa% label 1.
    e:f().

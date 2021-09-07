%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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

-module(get_two_tuple_elements).
-export([?MODULE/0]).

?MODULE() ->
    3 = xx(id(x), {id(1),{x,y,id(2)}}),
    ok.

xx(_Arg1, {A,T}) ->
    {_,_,B} = T,
    %% Test that the overwriting of the tuple is noticed:
    %%
    %% {get_tuple_element,{x,1},0,{x,0}}.
    %% {get_tuple_element,{x,1},1,{x,1}}.  %% Tuple overwritten!
    %% {get_tuple_element,{x,1},2,{x,1}}.  %% The tuple pointer must be reloaded.
    yy(A, B, []).

yy(A, B, _) ->
    A + B.

id(I) ->
    I.

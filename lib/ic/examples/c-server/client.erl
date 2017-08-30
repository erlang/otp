%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%%
%%%----------------------------------------------------------------------
%%% File    : client.erl
%%% Author  : Babbis Xagorarakis <babbis@balin>
%%% Purpose : 
%%% Created : 22 Oct 1998 by Babbis Xagorarakis <babbis@balin>
%%%----------------------------------------------------------------------

-module(client).
-author('babbis@balin').

-export([produce/0,init/3]).

-define(SERVER,{rmod_random_impl,'babbis@balin'}).
-define(CLIENTMOD,'rmod_random').

produce() ->
    ?CLIENTMOD:produce(?SERVER).


init(Seed1, Seed2, Seed3) ->
    ?CLIENTMOD:init(?SERVER, Seed1, Seed2, Seed3).





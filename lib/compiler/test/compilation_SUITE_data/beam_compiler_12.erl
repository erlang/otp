%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
-module(beam_compiler_12).

-export([?MODULE/0,t/1]).

?MODULE() ->
    ok.

t(Name) ->
    {ok = {file_info,_,regular,_,AccTime1,ModTime1,_,_,_,_,_,_,_,_}} =
	prim_file:read_file_info(Name).


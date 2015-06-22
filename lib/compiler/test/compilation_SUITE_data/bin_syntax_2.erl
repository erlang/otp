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
-module(bin_syntax_2).

-export([?MODULE/0]).

%% This module tests that constant propagation is done properly.

?MODULE() ->
    258 = b(<<1,2>>),
    F = c(),
    259 = F(<<1,3>>),
    ok.

b(B) ->
    Sz = 16,
    <<X:Sz/integer>> = B,
    X.

c() ->
    Size = 16,
    fun(Bin) ->
	    <<X:Size/integer>> = Bin,
	    X
    end.

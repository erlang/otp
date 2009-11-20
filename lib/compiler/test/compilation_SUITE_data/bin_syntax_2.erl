%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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

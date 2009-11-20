%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
-module(otp_5092).
-export([?MODULE/0]).

?MODULE() ->
    [] = t(),
    [] = t2(),
    [t] = t4(),
    [] = t5(),
    ok.

t() ->
    [t || {C=D}={_,_} <- []].
    
t2() ->
    [X || {X,{Y}={X,X}} <- []].

t4() ->
    [t || "a"++"b" = "ab" <- ["ab"]].

t5() ->
    [{X,Y} || {X} <- [], begin Y = X, Y =:= X end].

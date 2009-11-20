%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(compiler_5).           
-export([compiler_5/0]).

compiler_5() ->
    f0(),
    f1(),
    f2(),
    ok.

%% compiler treats records with 1 and 2 fields differently... 
-record(nil, {}).
-record(foo, {hello=1}).
-record(bar, {hello=2,there=3}).

f0() ->
    R1 = #nil{},
    R2 = R1#nil{}, %% stupid code, but compiler shouldn't crash
    R1 = R2,
    ok.

f1() ->
    R1 = #foo{},
    R2 = R1#foo{}, %% stupid code, but compiler shouldn't crash
    R1 = R2,
    ok.

f2() ->
    R1 = #bar{},
    R2 = R1#bar{}, %% stupid code, but compiler shouldn't crash
    R1 = R2,
    ok.

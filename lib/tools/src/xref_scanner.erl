%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2015. All Rights Reserved.
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

-module(xref_scanner).

-include("xref.hrl").

-export([scan/1]).

scan(Chars) ->
    case erl_scan:string(Chars) of
	{ok, Tokens, _Line}  ->
	    {ok, lex(a1(Tokens))};
	{error, {Line,Module,Info}, _EndLine} ->
	    {error, Module:format_error(Info), Line}
    end.

a1([{'-',N},{integer,N,1} | L]) ->
    [{integer,N,-1} | a1(L)];
a1([T | L]) ->
    [T | a1(L)];
a1([]) ->
    [].

-define(MFA(M,F,A,N), {atom,N,M}, {':',_}, {atom,_,F}, {'/',_}, {integer,_,A}).
-define(MFA2(M,F,A,N), 
	{'{',N},{atom,_,M},{',',_},{atom,_,F},{',',_},{integer,_,A},{'}',_}).
-define(DECL(N1,N2,T), {':',N1},{var,N2,T}).

lex([{atom,N,V1},{'->',_},{atom,_,V2} | L]) ->
    Constant = {constant, unknown, edge, {V1,V2}},
    [{edge,N,Constant} | lex(L)];
lex([{'{',N},{atom,_,V1},{',',_},{atom,_,V2},{'}',_} | L]) ->
    Constant = {constant, unknown, edge, {V1,V2}},
    [{edge,N,Constant} | lex(L)];
lex([?MFA(M,F,A,N),{'->',_},?MFA(M2,F2,A2,_) | L]) ->
    Constant = {constant, 'Fun', edge, {{M,F,A},{M2,F2,A2}}},
    [{edge,N,Constant} | lex(L)];
lex([?MFA(M,F,A,N) | L]) ->
    Constant = {constant, 'Fun', vertex, {M,F,A}},
    [{vertex,N,Constant} | lex(L)];
lex([{'{',N},?MFA2(M,F,A,_),{',',_},?MFA2(M2,F2,A2,_),{'}',_} | L]) ->
    Constant = {constant, 'Fun', edge, {{M,F,A},{M2,F2,A2}}},
    [{edge,N,Constant} | lex(L)];
lex([?MFA2(M,F,A,N) | L]) ->
    Constant = {constant, 'Fun', vertex, {M,F,A}},
    [{vertex,N,Constant} | lex(L)];
lex([?DECL(N1,N2,Decl) | L]) ->
    case is_type(Decl) of
	false -> [?DECL(N1, N2, Decl) | lex(L)];
	true -> [{decl,N1,Decl} | lex(L)]
    end;
lex([{':',N},{'=',_} | L]) ->
    [{':=',N} | lex(L)];
lex([{'||',N},{'|',_} | L]) ->
    [{'|||',N} | lex(L)];
lex([V={var,N,Var} | L]) ->
    T = case is_type(Var) of
	    false -> V;
	    true -> {cast,N,Var}
	end,
    [T | lex(L)];
lex([T | Ts]) ->
    [T | lex(Ts)];
lex([]) ->
    [{'$end', erl_anno:new(?XREF_END_LINE)}].

is_type('Rel')         -> true;
is_type('App')         -> true;
is_type('Mod')         -> true;
is_type('Fun')         -> true;
is_type('Lin')         -> true;
is_type('LLin')        -> true;
is_type('XLin')        -> true;
is_type('ELin')        -> true;
is_type('XXL')         -> true;
is_type(_)             -> false.

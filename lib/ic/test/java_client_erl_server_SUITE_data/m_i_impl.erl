%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
%%--------------------------------------------------------------------
-module(m_i_impl).

-export([marshal_ll/3,marshal_ull/3,
	 marshal_l/3,marshal_ul/3,
	 marshal_s/3,marshal_us/3,
	 marshal_c/3,marshal_wc/3,
	 strcat/3,
	 marshal_any_3/4,marshal_any_2/3]).
-export([init/1,terminate/2,code_change/3]).

-include("m.hrl").

-define(TK_M_S, {tk_struct,
		 "IDL:m/s:1.0",
		 "s",
		 [{"ll_x",tk_longlong},
		  {"ull_x",tk_ulonglong},
		  {"ll_y",tk_longlong},
		  {"ll_z",tk_longlong},
		  {"ull_z",tk_ulonglong},
		  {"l_x",tk_long},
		  {"ul_x",tk_ulong},
		  {"l_y",tk_long},
		  {"l_z",tk_long},
		  {"ul_z",tk_ulong},
		  {"s_x",tk_short},
		  {"us_x",tk_ushort},
		  {"s_y",tk_short},
		  {"s_z",tk_short},
		  {"us_z",tk_ushort},
		  {"c_x",tk_char},
		  {"c_y",tk_char},
		  {"c_z",tk_char},
		  {"wc_x",tk_wchar},
		  {"wc_y",tk_wchar},
		  {"wc_z",tk_wchar}|_]}).



marshal_ll(State, #m_s{ll_x = X, ll_y = Y}=_A, B) when integer(B) ->
    R = (X - Y)*B,
    io:format("~p", [{?MODULE,?LINE,[X,Y,B,R]}]),
    {reply, R, State}.

marshal_ull(State, #m_s{ull_x = X, ll_y = Y}=_A, B) when integer(B) ->
    R = (X - Y)*B,
    io:format("~p", [{?MODULE,?LINE,[X,Y,B,R]}]),
    {reply, R, State}.


marshal_l(State, #m_s{l_x = X, l_y = Y}=_A, B) when integer(B) ->
    R = (X - Y)*B,
    io:format("~p", [{?MODULE,?LINE,[X,Y,B,R]}]),
    {reply, R, State}.

marshal_ul(State, #m_s{ul_x = X, l_y = Y}=_A, B) when integer(B) ->
    R = (X - Y)*B,
    io:format("~p", [{?MODULE,?LINE,[X,Y,B,R]}]),
    {reply, R, State}.


marshal_s(State, #m_s{s_x = X, s_y = Y}=_A, B) when integer(B) ->
    R = (X - Y)*B,
    io:format("~p", [{?MODULE,?LINE,[X,Y,B,R]}]),
    {reply, R, State}.

marshal_us(State, #m_s{us_x = X, s_y = Y}=_A, B) when integer(B) ->
    R = (X - Y)*B,
    io:format("~p", [{?MODULE,?LINE,[X,Y,B,R]}]),
    {reply, R, State}.


marshal_c(State, #m_s{c_x = X, c_y = Y}=_A, B) when integer(B) ->
    R = (X - Y)*B,
    io:format("~p", [{?MODULE,?LINE,[X,Y,B,R]}]),
    {reply, R, State}.

marshal_wc(State, #m_s{wc_x = X, wc_y = Y}=_A, B) when integer(B) ->
    R = (X - Y)*B,
    io:format("~p", [{?MODULE,?LINE,[X,Y,B,R]}]),
    {reply, R, State}.

strcat(State, A, B) when list(A), list(B) ->
    R = A++B,
    io:format("~p", [{?MODULE,?LINE,[length(A),length(B),A,B,R]}]),
    {reply, R, State};
strcat(State, A, B) ->
    io:format("~p", [{?MODULE,?LINE,[A,B]}]),
    {reply, [], State}.

marshal_any_3(State, {any,TkX,_}=X, {any,_,_}=Y, B) when integer(B) ->
    R = any(mul(sub(any(X), any(Y)), B), TkX),
    io:format("~p", [{?MODULE,?LINE,[X,Y,B,R]}]),
    {reply, R, State}.

marshal_any_2(State, 
	      {any,TkA,#m_s{ll_x=LL_X, ull_x=ULL_X, ll_y=LL_Y,
			    l_x=L_X, ul_x=UL_X, l_y=L_Y,
			    s_x=S_X, us_x=US_X, s_y=S_Y,
			    c_x=C_X, c_y=C_Y,
			    wc_x=WC_X, wc_y=WC_Y} = A},
	      B) when integer(B) ->
    {check_type_code,?TK_M_S} = {check_type_code,TkA},
    ULL_Z = (ULL_X - LL_Y) * B,
    LL_Z = (LL_X - LL_Y) * B,
    UL_Z = (UL_X - L_Y) * B,
    L_Z = (L_X - L_Y) * B,
    US_Z = (US_X - S_Y) * B,
    S_Z = (S_X - S_Y) * B,
    C_Z = (C_X - C_Y) * B,
    WC_Z = (WC_X - WC_Y) * B,
    R = A#m_s{ll_z=LL_Z, ull_z=ULL_Z,
	      l_z=L_Z, ul_z=UL_Z,
	      s_z=S_Z, us_z=US_Z,
	      c_z=C_Z, wc_z=WC_Z},
    io:format("~p", [{?MODULE,?LINE,[A,B,R]}]),
    {reply, {any,TkA,R}, State}.



init(_Env) ->
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


any({any,tk_longlong,X}) -> X;
any({any,tk_long,X}) -> X;
any({any,tk_short,X}) -> X;
any({any,tk_ulonglong,X}) -> X;
any({any,tk_ulong,X}) -> X;
any({any,tk_ushort,X}) -> X;
any({any,tk_char,X}) -> X;
any({any,tk_wchar,X}) -> X.

any(X, Tk) when integer(X) -> {any,Tk,X}.

sub(X, Y) when integer(X), integer(Y) ->
    X - Y.

mul(X, Y) when integer(X), integer(Y) ->
    X * Y.

napp(0, L) -> L;
napp(N, L) when integer(N), N >= 1 -> napp(N-1, L)++L.

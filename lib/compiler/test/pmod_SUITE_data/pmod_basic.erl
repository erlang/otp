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
-module(pmod_basic, [Props]).

-export([lookup/1,or_props/1,prepend/1,append/1,stupid_sum/0]).
-export([bar/1,bar_bar/1]).

lookup(Key) ->
    proplists:lookup(Key, Props).

or_props(Keys) ->    
    Res = or_props_1(Keys, false),
    true = is_bool(Res),			%is_bool/1 does not use Props.
    Res.

prepend(Term) ->
    new([Term|Props]).

append(Term) ->
    pmod_basic:new(Props++[Term]).

or_props_1([K|Ks], Acc) ->
    or_props_1(Ks, proplists:get_bool(K, Props) or Acc);
or_props_1([], Acc) -> Acc.

is_bool(true) -> true;
is_bool(false) -> true;
is_bool(_) -> false.

stupid_sum() ->
    put(counter, 0),
    Res = stupid_sum_1(Props, 0),
    {Res,get(counter)}.

stupid_sum_1([H|T], Sum0) ->
    try add(Sum0, H) of
	Sum -> stupid_sum_1(T, Sum)
    catch
	error:_ -> stupid_sum_1(T, Sum0)
    after
	bump()
    end;
stupid_sum_1([], Sum) -> Sum.

bump() ->
    put(counter, get(counter)+1).

add(A, B) ->
    A+B.

-record(s, {a}).

bar(S) when S#s.a == 0 -> ok.

bar_bar(S) when is_record(S, s) -> ok;
bar_bar(_) -> error.

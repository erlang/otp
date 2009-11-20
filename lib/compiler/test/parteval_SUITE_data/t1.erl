%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
-module(?M).

-compile(export_all).

%%% The arity-0 functions are all called from the test suite.

f2() ->
    size({1,2}).

i() ->
    case [] of
	[] ->
	    ok;
	X ->
	    hopp
    end.

e() ->
    case 4+5 of
%	X when X>10 -> kvock;	% not removed by BEAM opt.
	{X,X} when list(X) ->
	    kvack;
	9 ->
	    ok;
	_ ->
	    ko
    end.

f() ->
    element(2,{a,b,c,d}),
    erlang:element(2,{a,b,c,d}),
    "hej" ++ "hopp".

g(X) ->
    if
	float(3.4) ->
	    hej;
	X == 5, 4==4 ->
	    japp;
	4 == 4, size({1,2}) == 1 ->
	    ok
    end.

g() ->
    {g(3),g(5)}.

bliff() ->
    if
	3==4 ->
	    himm
    end.

fi() ->
    case 4 of
	X when 4==3 ->
	    {X};
	4 ->
	    4;
	_ ->
	    ok
    end.

iff() when 3==2 ->
    if
	3 == 4 ->
	    baff;
	3 == 3 ->
	    nipp
    end.

sleep(I) -> receive after I -> ok end.

sleep() ->
    sleep(45).

s() ->
    case 4 of
	3 ->
	    ok
    end.

error_reason(R) when atom(R) ->
    R;
error_reason(R) when tuple(R) ->
    error_reason(element(1, R)).

plusplus() ->
    ?MODULE ++ " -> mindre snygg felhantering".

call_it(F) ->
    case (catch apply(?MODULE, F, [])) of
	{'EXIT', R0} ->
	    {'EXIT', error_reason(R0)};
	V ->
	    V
    end.

run() ->
    L = [{f2, 2},
	 {i, ok},
	 {e, ok},
	 {f, "hejhopp"},
	 {g, {hej, hej}},
	 {bliff, {'EXIT', if_clause}},
	 {fi, 4},
	 {iff, {'EXIT', function_clause}},
	 {sleep, ok},
	 {s, {'EXIT', case_clause},
	 {plusplus, {'EXIT', badarg}}}],
    Actual = [call_it(F) || {F, _} <- L],
    Correct = [C || {_, C} <- L],
    {Correct, Actual}.


%%% Don't call, only compile.
t(A) ->
    receive
	A when 1==2 ->
	    ok;
	B ->
	    B
    end.

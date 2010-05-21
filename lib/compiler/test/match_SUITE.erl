%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
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
-module(match_SUITE).

-export([all/1,
	 pmatch/1,mixed/1,aliases/1,match_in_call/1,
	 untuplify/1,shortcut_boolean/1,letify_guard/1,
	 selectify/1,underscore/1]).
	 
-include("test_server.hrl").

all(suite) ->
    test_lib:recompile(?MODULE),
    [pmatch,mixed,aliases,match_in_call,untuplify,shortcut_boolean,
     letify_guard,selectify,underscore].

pmatch(Config) when is_list(Config) ->
    ?line ok = doit(1),
    ?line ok = doit(2),
    ?line {error,baz} = doit(3),
    ?line {error,foobar} = doit(4),
    ok.

%% Thanks to Tobias Lindahl (HiPE).
-define(FOO(X),
	case X of
	    1 -> foo;
	    2 -> bar;
	    3 -> baz;
	    4 -> foobar
	end).

doit(X) ->
    case ?FOO(X) of
	foo -> ok;
	bar -> ok;
	Other -> {error, Other}
    end.

mixed(Config) when is_list(Config) ->
    ?line glufs = mixit(1),
    ?line klafs = mixit(2),
    ?line fnurra = mixit(3),
    ?line usch = mixit(4),
    ?line {error,blurf} = mixit(5),
    ?line {error,87987987} = mixit(6),
    ?line {error,{a,b,c}} = mixit(7),
    ok.

mixit(X) ->
    case case X of
	     1 -> a;
	     2 -> b;
	     3 -> 42;
	     4 -> 77;
	     5 -> blurf;
	     6 -> 87987987;
	     7 -> {a,b,c}
	 end of
	a -> glufs;
	b -> klafs;
	42 -> fnurra;
	77 -> usch;
	Other -> {error,Other}
    end.

aliases(Config) when is_list(Config) ->
    %% Lists/strings.
    ?line ok = str_alias("abc"),
    ?line ok = str_alias("def"),
    ?line ok = str_alias("ghi"),
    ?line ok = str_alias("klm"),
    ?line ok = str_alias("qrs"),
    ?line ok = str_alias("xy"),
    ?line ok = str_alias(""),
    ?line ok = str_alias([]),
    ?line error = str_alias("blurf"),

    %% Characters/integers.
    ?line ok = char_alias($v),
    ?line ok = char_alias(118),
    ?line ok = char_alias($w),
    ?line ok = char_alias(119),
    ?line ok = char_alias(42),
    ?line ok = char_alias(3.0),
    ?line error = char_alias($_),
    ?line error = char_alias(0),

    ?line {42,42,42} = three(42),

    ?line {1,42,99,1,42,99} = tuple_alias({1,42,99}),
    ?line {-10,20,-10,20,-10,20} = tuple_alias({-10,20}),
    ?line 6 = tup_lit_alias({1,2,3}),
    ?line 6 = tup_lit_alias_rev({1,2,3}),

    ?line {42,42,42,42} = multiple_aliases_1(42),
    ?line {7,7,7} = multiple_aliases_2(7),
    ?line {{a,b},{a,b},{a,b}} = multiple_aliases_3({a,b}),

    %% Lists/literals.
    ?line {a,b} = list_alias1([a,b]),
    ?line {a,b} = list_alias2([a,b]),
    ?line {a,b} = list_alias3([a,b]),

    ok.

str_alias(V) ->
    Res = str_alias_1(V),
    Res = str_alias_2(V).

str_alias_1([$a,$b,$c]="abc"="a"++[$b,$c]=[97,98,99]) -> ok;
str_alias_1([$d|"ef"]="def") -> ok;
str_alias_1([$g|"hi"]="g"++"hi"="gh"++"i"="ghi"++"") -> ok;
str_alias_1("k"++"lm"=[$k|"lm"]) -> ok;
str_alias_1([113,114,115]="qrs"=[$q,$r,$s]="q"++"r"++"s") -> ok;
str_alias_1([$x,$y]="xy") -> ok;
str_alias_1(""=[]) -> ok;
str_alias_1(_) -> error.

%% Make sure that different line numbers do not matter.

str_alias_2([$a,$b,$c]=
	    "abc"=
	    "a"++[$b,$c
	       ]=
	    [97,98,99
	  ]) -> ok;
str_alias_2([$d|"ef"]=
	  "def") -> ok;
str_alias_2([$g|"hi"]=
	  "g"++"hi"=
	  "gh"++"i"=
	  "ghi"++"") -> ok;
str_alias_2("k"++"lm"=
	  [$k|"lm"
	  ]) -> ok;
str_alias_2([113,114,115]=
	  "qrs"=[$q,$r,$s
		]=
	  "q"++"r"++"s") -> ok;
str_alias_2([$x,$y]=
	  "xy") -> ok;
str_alias_2(""=
	  []) -> ok;
str_alias_2(_) -> error.

char_alias(V) ->
    Res = char_alias_1(V),
    Res = char_alias_2(V).

char_alias_1(118=$v) -> ok;
char_alias_1($w=119) -> ok;
char_alias_1(42=42) -> ok;
char_alias_1(3.0=3.0) -> ok;
char_alias_1(_) -> error.

char_alias_2(118=
	     $v) -> ok;
char_alias_2($w=
	     119) -> ok;
char_alias_2(42=
	     42) -> ok;
char_alias_2(3.0=
	     3.0) -> ok;
char_alias_2(_) -> error.

three(V) ->
    Res = three_1(V),
    Res = three_2(V).

three_1(A=B=C) ->
    {A,B,C}.

three_2(A=
	B=
	C) ->
    {A,B,C}.

tuple_alias({A,B,C}={X,Y,Z}) ->
    {A,B,C,X,Y,Z};
tuple_alias({A,B}={C,D}={E,F}) ->
    {A,B,C,D,E,F}.

tup_lit_alias({A,B,C}={1,2,3}) ->
    A+B+C.

tup_lit_alias_rev({1,2,3}={A,B,C}) ->
    A+B+C.

multiple_aliases_1((A=B)=(C=D)) ->
    {A,B,C,D}.

multiple_aliases_2((A=B)=(A=C)) ->
    {A,B,C}.

multiple_aliases_3((A={_,_}=B)={_,_}=C) ->
    {A,B,C}.

list_alias1([a,b]=[X,Y]) ->
    {X,Y}.

list_alias2([X,Y]=[a,b]) ->
    {X,Y}.

list_alias3([X,b]=[a,Y]) ->
    {X,Y}.

%% OTP-7018.

match_in_call(Config) when is_list(Config) ->
    ?line mac_a(0),
    ?line mac_b(1),
    ?line mac_c(42),
    ?line mac_d(42),
    ?line mac_e({gurka,42}),

    ?line [{2,2},{2,2}] = mac_lc([{2,any},{2,2}]),
    ?line {'EXIT',_} = (catch mac_lc([{1,1}])),

    ok.

mac_a(X) ->
    id(_Gurka = {gurka,X}),
    ok.

mac_b(X) ->
    id(Gurka = {gurka,X}),
    gurka(Gurka, X),
    ok.

mac_c(X) ->
    id(Gurka = Yxa = {gurka,X}),
    id({Gurka,Yxa}),
    ok.

mac_d(X) ->
    id({gurka,42} = {gurka,X}),
    ok.

mac_e(X) ->
    id({gurka,42} = X),
    ok.

mac_lc(E) ->
    Res = mac_lc1(E),
    Res = mac_lc2(E).

mac_lc1(E) ->
    [{X,Y} ||
	{X,_} <- E,
	(Y = X) =:= (Y = 1 + 1)].

mac_lc2(E) ->
    [{X,Y} ||
	{X,_} <- E,
	(Y = X) =:= (Y = 2)].

gurka({gurka,X}, X) -> ok.


untuplify(Config) when is_list(Config) ->
    %% We do this to cover sys_core_fold:unalias_pat/1.
    ?line {1,2,3,4,alias,{[1,2],{3,4},alias}} = untuplify_1([1,2], {3,4}, alias),
    ?line error = untuplify_1([1,2], {3,4}, 42),
    ok.

untuplify_1(A, B, C) ->
    case {A,B,C} of
	{[X,Y],{Z,W},alias=Alias}=Top ->
	    {X,Y,Z,W,Alias,Top};
	[_,_]=CantMatch ->
	    CantMatch;
	_ ->
	    error
    end.

%% Coverage of beam_dead:shortcut_boolean_label/4.
shortcut_boolean(Config) when is_list(Config) ->
    ?line false = shortcut_boolean_1([0]),
    ?line true = shortcut_boolean_1({42}),
    ?line maybe = shortcut_boolean_1(self()),
    ?line {'EXIT',_} = (catch shortcut_boolean_1([a,b])),
    ?line {'EXIT',_} = (catch shortcut_boolean_1({a,b})),
    ok.

shortcut_boolean_1(X) ->
    Outer = case not is_pid(X) of
		true ->
		    V = case X of
			    [_] -> true;
			    {_} -> false
			end,
		    not V;
		false ->
		    maybe
	    end,
    id(Outer).


%% Test sys_core_fold:letify_guard/3.
letify_guard(Config) when is_list(Config) ->
    ?line {-15,a} = letify_guard(-15, a),
    ?line 5 = letify_guard(2, 3),
    ok.

letify_guard(A, B) ->
    case {A,B} of
	%% The tuple will be built in the guard...
	Z when tuple_size(Z) =:= 2, element(1, Z) < 0 ->
	    %% ... and again here.
	    Z;
	{X,Y} -> X+Y
    end.

%% Test combining of is_eq_exact instructions to select_val
%% instructions in beam_dead and beam_peep.

selectify(Config) when is_list(Config) ->
    ?line integer = sel_different_types({r,42}),
    ?line atom = sel_different_types({r,forty_two}),
    ?line none = sel_different_types({r,18}),
    ?line {'EXIT',_} = (catch sel_different_types([a,b,c])),

    ?line integer = sel_same_value({r,42}),
    ?line error = sel_same_value({r,100}),
    ?line error = sel_same_value(a),

    ?line integer42 = sel_same_value2(42),
    ?line integer43 = sel_same_value2(43),
    ?line error = sel_same_value2(44),
    ok.

sel_different_types({r,_}=T) when element(2, T) =:= forty_two ->
    atom;
sel_different_types({r,_}=T) when element(2, T) =:= 42 ->
    integer;
sel_different_types({r,_}) ->
    none.

sel_same_value({r,V}) when V =:= 42 ->
    integer;
sel_same_value({r,V}) when V =:= 42 ->
    integer42;
sel_same_value(_) ->
    error.

sel_same_value2(V) when V =:= 42 ->
    integer42;
sel_same_value2(V) when V =:= 42; V =:= 43 ->
    integer43;
sel_same_value2(_) ->
    error.

underscore(Config) when is_list(Config) ->
    case Config of
	[] ->
	    %% Assignment to _ at the end of a construct.
	    _ = length(Config);
	[_|_] ->
	    %% Assignment to _ at the end of a construct.
	    _ = list_to_tuple(Config)
    end,
    _ = is_list(Config),
    ok.

id(I) -> I.

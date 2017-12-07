%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-module(match_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 pmatch/1,mixed/1,aliases/1,non_matching_aliases/1,
	 match_in_call/1,untuplify/1,shortcut_boolean/1,letify_guard/1,
	 selectify/1,deselectify/1,underscore/1,match_map/1,map_vars_used/1,
	 coverage/1,grab_bag/1,literal_binary/1,
         unary_op/1]).
	 
-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,[parallel],
      [pmatch,mixed,aliases,non_matching_aliases,
       match_in_call,untuplify,
       shortcut_boolean,letify_guard,selectify,deselectify,
       underscore,match_map,map_vars_used,coverage,
       grab_bag,literal_binary,unary_op]}].


init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


pmatch(Config) when is_list(Config) ->
    ok = doit(1),
    ok = doit(2),
    {error,baz} = doit(3),
    {error,foobar} = doit(4),
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
    glufs = mixit(1),
    klafs = mixit(2),
    fnurra = mixit(3),
    usch = mixit(4),
    {error,blurf} = mixit(5),
    {error,87987987} = mixit(6),
    {error,{a,b,c}} = mixit(7),
    ok.

mixit(X) ->
    case case X of
	     1 -> a;
	     2 -> b;
	     3 -> 42;
	     4 -> 77;
	     4+1 -> blurf;
	     5+1 -> 87987987;
	     6+1 -> {a,b,c}
	 end of
	a -> glufs;
	b -> klafs;
	42 -> fnurra;
	77 -> usch;
	Other -> {error,Other}
    end.

aliases(Config) when is_list(Config) ->
    %% Lists/strings.
    ok = str_alias("abc"),
    ok = str_alias("def"),
    ok = str_alias("ghi"),
    ok = str_alias("klm"),
    ok = str_alias("qrs"),
    ok = str_alias("xy"),
    ok = str_alias(""),
    ok = str_alias([]),
    error = str_alias("blurf"),

    %% Characters/integers.
    ok = char_alias($v),
    ok = char_alias(118),
    ok = char_alias($w),
    ok = char_alias(119),
    ok = char_alias(42),
    ok = char_alias(3.0),
    error = char_alias($_),
    error = char_alias(0),

    {42,42,42} = three(42),

    {1,42,99,1,42,99} = tuple_alias({1,42,99}),
    {-10,20,-10,20,-10,20} = tuple_alias({-10,20}),
    6 = tup_lit_alias({1,2,3}),
    6 = tup_lit_alias_rev({1,2,3}),

    {42,42,42,42} = multiple_aliases_1(42),
    {7,7,7} = multiple_aliases_2(7),
    {{a,b},{a,b},{a,b}} = multiple_aliases_3({a,b}),

    %% Lists/literals.
    {a,b} = list_alias1([a,b]),
    {a,b} = list_alias2([a,b]),
    {a,b} = list_alias3([a,b]),

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

non_matching_aliases(_Config) ->
    none = mixed_aliases(<<42>>),
    none = mixed_aliases([b]),
    none = mixed_aliases([d]),
    none = mixed_aliases({a,42}),
    none = mixed_aliases(42),

    {'EXIT',{{badmatch,42},_}} = (catch nomatch_alias(42)),
    {'EXIT',{{badmatch,job},_}} = (catch entirely()),
    {'EXIT',{{badmatch,associates},_}} = (catch printer()),
    {'EXIT',{{badmatch,borogoves},_}} = (catch tench()),

    put(perch, 0),
    {'EXIT',{{badmatch,{spine,42}},_}} = (catch perch(42)),
    1 = erase(perch),

    put(salmon, 0),
    {'EXIT',{{badmatch,mimsy},_}} = (catch salmon()),
    1 = erase(salmon),

    put(shark, 0),
    {'EXIT',{{badmatch,_},_}} = (catch shark()),
    1 = erase(shark),

    {'EXIT',{{badmatch,_},_}} = (catch radio(research)),
    ok.

mixed_aliases(<<X:8>> = x) -> {a,X};
mixed_aliases([b] = <<X:8>>) -> {b,X};
mixed_aliases(<<X:8>> = {a,X}) -> {c,X};
mixed_aliases([X] = <<X:8>>) -> {d,X};
mixed_aliases(_) -> none.

nomatch_alias(I) ->
    {ok={A,B}} = id(I),
    {A,B}.

entirely() ->
    0(((Voice = true) = cool) = job),
    [receive _ -> Voice end || banking <- printer].

printer() ->
    {[Indoor] = [] = associates},
    [ireland || Indoor <- Indoor].

tench() ->
    E = begin
	    [A] = [] = borogoves,
	    A + 1
	end,
    E + 7 * A.

perch(X) ->
    begin
	put(perch, get(perch)+1),
	[A] = [] = {spine,X}
    end.

salmon() ->
    {put(salmon, get(salmon)+1),#{key:=([A]=[])}=mimsy,exit(fail)},
    A + 10.

shark() ->
    (hello = there) = (catch shark(put(shark, get(shark)+1), a = b)).

shark(_, _) ->
    ok.

radio(research) ->
    (connection = proof) =
	(catch erlang:trace_pattern(catch mechanisms + assist,
				    summary = mechanisms)).

%% OTP-7018.

match_in_call(Config) when is_list(Config) ->
    mac_a(0),
    mac_b(1),
    mac_c(42),
    mac_d(42),
    mac_e({gurka,42}),

    [{2,2},{2,2}] = mac_lc([{2,any},{2,2}]),
    {'EXIT',_} = (catch mac_lc([{1,1}])),

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
    {1,2,3,4,alias,{[1,2],{3,4},alias}} = untuplify_1([1,2], {3,4}, alias),
    error = untuplify_1([1,2], {3,4}, 42),
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
    false = shortcut_boolean_1([0]),
    true = shortcut_boolean_1({42}),
    maybe = shortcut_boolean_1(self()),
    {'EXIT',_} = (catch shortcut_boolean_1([a,b])),
    {'EXIT',_} = (catch shortcut_boolean_1({a,b})),
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
    {-15,a} = letify_guard(-15, a),
    5 = letify_guard(2, 3),
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
    integer = sel_different_types({r,42}),
    atom = sel_different_types({r,forty_two}),
    none = sel_different_types({r,18}),
    {'EXIT',_} = (catch sel_different_types([a,b,c])),

    integer = sel_same_value({r,42}),
    error = sel_same_value({r,100}),
    error = sel_same_value(a),

    integer42 = sel_same_value2(42),
    integer43 = sel_same_value2(43),
    error = sel_same_value2(44),
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

%% Test deconstruction of select_val instructions in beam_peep into
%% regular tests with just one possible value left. Hitting proper cases
%% in beam_peep relies on unification of labels by beam_jump.

deselectify(Config) when is_list(Config) ->
    one_or_other = desel_tuple_arity({1}),
    two = desel_tuple_arity({1,1}),
    one_or_other = desel_tuple_arity({1,1,1}),

    one_or_other = dsel_integer(1),
    two = dsel_integer(2),
    one_or_other = dsel_integer(3),

    one_or_other = dsel_integer_typecheck(1),
    two = dsel_integer_typecheck(2),
    one_or_other = dsel_integer_typecheck(3),

    one_or_other = dsel_atom(one),
    two = dsel_atom(two),
    one_or_other = dsel_atom(three),

    one_or_other = dsel_atom_typecheck(one),
    two = dsel_atom_typecheck(two),
    one_or_other = dsel_atom_typecheck(three).

desel_tuple_arity(Tuple) when is_tuple(Tuple) ->
    case Tuple of
        {_} -> one_or_other;
        {_,_} -> two;
        _ -> one_or_other
    end.

dsel_integer(Val) ->
    case Val of
        1 -> one_or_other;
        2 -> two;
        _ -> one_or_other
    end.

dsel_integer_typecheck(Val) when is_integer(Val) ->
    case Val of
        1 -> one_or_other;
        2 -> two;
        _ -> one_or_other
    end.

dsel_atom(Val) ->
    case Val of
        one -> one_or_other;
        two -> two;
        _ -> one_or_other
    end.

dsel_atom_typecheck(Val) when is_atom(Val) ->
    case Val of
        one -> one_or_other;
        two -> two;
        _ -> one_or_other
    end.

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

-record(s, {map,t}).

match_map(Config) when is_list(Config) ->
    Map = #{key=>{x,y},ignore=>anything},
    #s{map=Map,t={x,y}} = do_match_map(#s{map=Map}),
    {a,#{k:={a,b,c}}} = do_match_map_2(#{k=>{a,b,c}}),
    ok.

do_match_map(#s{map=#{key:=Val}}=S) ->
    %% Would crash with a 'badarg' exception.
    S#s{t=Val}.

do_match_map_2(Map) ->
    case {a,Map} of
	{a,#{k:=_}}=Tuple ->
	    Tuple
    end.

map_vars_used(Config) when is_list(Config) ->
    {some,value} = do_map_vars_used(a, b, #{{a,b}=>42,v=>{some,value}}),
    ok.

do_map_vars_used(X, Y, Map) ->
    case {X,Y} of
	T ->
	    %% core_lib:is_var_used/2 would not consider T used.
	    #{T:=42,v:=Val} = Map,
	    Val
    end.

coverage(Config) when is_list(Config) ->
    %% Cover beam_dead.
    ok = coverage_1(x, a),
    ok = coverage_1(x, b),

    %% Cover sys_pre_expand.
    ok = coverage_3("abc").

coverage_1(B, Tag) ->
    case Tag of
	a -> coverage_2(1, a, B);
	b -> coverage_2(2, b, B)
    end.

coverage_2(1, a, x) -> ok;
coverage_2(2, b, x) -> ok.

coverage_3([$a]++[]++"bc") -> ok.

grab_bag(_Config) ->
    [_|T] = id([a,b,c]),
    [b,c] = id(T),

    T1 = fun() ->
		 [_|_] = x
	 end,
    {'EXIT',_} = (catch T1()),

    T2 = fun(A, B) ->
		 case {{element(1, A),element(2, B)},
		       {element(2, A),element(2, B)}} of
		     {Same,Same} -> ok;
		     {{0,1},{up,X}} -> id(X);
		     {_,{X,_}} -> id(X)
		 end
	 end,
    ok = T2({a,a,z,z}, {z,a,z}),
    1 = T2({0,up}, {zzz,1}),
    y = T2({x,y}, {a,z,z}),

    %% OTP-5244.
    L = [{stretch,0,0},
	 {bad,[]},
	 {bad,atom},
	 {bad,0},
	 {bad,16#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA},
	 {bad,16#555555555555555555555555555555555555555555555555555}],
    ok = grab_bag_remove_failure(L, unit, 0),

    {42,<<43,44>>} = grab_bag_single_valued(<<42,43,44>>),
    empty_list = grab_bag_single_valued([]),
    empty_tuple = grab_bag_single_valued({}),

    ok.

grab_bag_remove_failure([], _Unit, _MaxFailure) ->
    ok;
grab_bag_remove_failure([{bad,Bad}|_], _Unit, _MaxFailure) ->
    Bad;
grab_bag_remove_failure([{stretch,_,Mi}=Stretch | Specs], Unit, _MaxFailure) ->
    {MinMax,NewMaxFailure} = id({min,1}),
    case {MinMax,grab_bag_remove_failure(Specs, Unit, NewMaxFailure)} of
	{min,{NewMaxFailure,Rest}} ->
	    {done,[{fixed,Mi} | Rest]};
	{min,_} when Specs =/= [] ->
	    grab_bag_remove_failure([Stretch|tl(Specs)], Unit, NewMaxFailure);
	{min,_} ->
	    ok
    end.

%% Cover a line v3_kernel that places binary matching first.
grab_bag_single_valued(<<H,T/bytes>>) -> {H,T};
grab_bag_single_valued([]) -> empty_list;
grab_bag_single_valued({}) -> empty_tuple.


%% Regression in 19.0, reported by Alexei Sholik
literal_binary(_Config) ->
    3 = literal_binary_match(bar, <<"y">>),

    %% While we are at it, also test the remaining code paths
    %% in literal_binary_match/2.
    1 = literal_binary_match(bar, <<"x">>),
    2 = literal_binary_match(foo, <<"x">>),
    3 = literal_binary_match(foo, <<"y">>),
    fail = literal_binary_match(bar, <<"z">>),
    fail = literal_binary_match(foo, <<"z">>),
    ok.

literal_binary_match(bar, <<"x">>) -> 1;
literal_binary_match(_, <<"x">>) -> 2;
literal_binary_match(_, <<"y">>) -> 3;
literal_binary_match(_, _) -> fail.

unary_op(Config) ->
    %% ERL-514. This test case only verifies that the code
    %% calculates the correct result, not that the generated
    %% code is optimial.

    {non_associative,30} = unary_op_1('&'),
    {non_associative,300} = unary_op_1('^'),
    {non_associative,300} = unary_op_1('not'),
    {non_associative,300} = unary_op_1('+'),
    {non_associative,300} = unary_op_1('-'),
    {non_associative,300} = unary_op_1('~~~'),
    {non_associative,300} = unary_op_1('!'),
    {non_associative,320} = unary_op_1('@'),

    error = unary_op_1(Config),
    error = unary_op_1(abc),
    error = unary_op_1(42),

    ok.

unary_op_1(Vop@1) ->
    %% If all optimizations are working as they should, there should
    %% be no stack frame and all '=:=' tests should be coalesced into
    %% a single select_val instruction.

    case Vop@1 =:= '&' of
        true ->
            {non_associative,30};
        false ->
            case
                case Vop@1 =:= '^' of
                    true ->
                        true;
                    false ->
                        case Vop@1 =:= 'not' of
                            true ->
                                true;
                            false ->
                                case Vop@1 =:= '+' of
                                    true ->
                                        true;
                                    false ->
                                        case Vop@1 =:= '-' of
                                            true ->
                                                true;
                                            false ->
                                                case Vop@1 =:= '~~~' of
                                                    true ->
                                                        true;
                                                    false ->
                                                        Vop@1 =:= '!'
                                                end
                                        end
                                end
                        end
                end
            of
                true ->
                    {non_associative,300};
                false ->
                    case Vop@1 =:= '@' of
                        true ->
                            {non_associative,320};
                        false ->
                            error
                    end
            end
    end.


id(I) -> I.

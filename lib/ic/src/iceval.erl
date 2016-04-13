%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%
-module(iceval).

-include("icforms.hrl").

-export([eval_const/5, eval_e/5]).

-export([check_tk/3, get_val/1, mk_val/1]).

-define(get_max(__X, __Y), if __X > __Y -> __X; true -> __Y end).
-define(get_min(__X, __Y), if __X > __Y -> __Y; true -> __X end).

-define(BASE, 100000000000000000000000000000000).
-define(FIXED_MAX, 9999999999999999999999999999999).

%% Called fr: ictype 99, 522, 533
%% Fixed constants can be declared as:
%% (1)  const fixed pi = 3.14D; or
%% (2)  typedef fixed<3,2> f32;
%%      const f32 pi = 3.14D;
%% Hence, if fixed is declared as (1) we must handle it especially.
eval_const(G, S, N, tk_fixed, Expr) ->
    case catch eval_e(G, S, N, tk_fixed, Expr) of
	T when element(1, T) == error -> 0;
	V when is_record(V, fixed) -> 
	    {ok, {tk_fixed, V#fixed.digits, V#fixed.scale}, V};
	V ->
	    ic_error:error(G, {bad_tk_match, Expr, tk_fixed, get_val(V)})		      
    end;
eval_const(G, S, N, TK, Expr) ->
    case catch eval_e(G, S, N, TK, Expr) of
	T when element(1, T) == error -> 0;
	V -> 
	    case check_tk(G, TK, V) of
		true -> ok;
		false ->
		    ic_error:error(G, {bad_tk_match, Expr, TK, get_val(V)})
	    end,
	    get_val(V)
    end.


check_op(G, S, N, Tk, Types, Op, E1, E2) ->
    V1 = eval_e(G, S, N, Tk, E1),
    V2 = eval_e(G, S, N, Tk, E2),
    check_types(G, Op, E1, Types, V1),
    check_types(G, Op, E2, Types, V2),
    case check_comb(V1, V2) of
	true ->
	    {V1, V2};
	false ->
	    Err = {bad_type_combination, E1, get_val(V1), get_val(V2)},
	    ic_error:error(G, Err),
	    throw({error, Err})
    end.

check_op(G, S, N, Tk, Types, Op, E1) ->
    V1 = eval_e(G, S, N, Tk, E1),
    check_types(G, Op, E1, Types, V1),
    V1.

%% Match the declared type TK against the factual value of an constant
%%
check_tk(_G, _Any, default) -> true;		% Default case in union
check_tk(_G, positive_int, V) when is_integer(V) andalso V >= 0 -> true;
check_tk(_G, tk_long, V) when is_integer(V) -> true;
check_tk(_G, tk_longlong, V) when is_integer(V) -> true;  %% LLON_G
check_tk(_G, tk_short, V) when is_integer(V) -> true;
check_tk(_G, tk_ushort, V) when is_integer(V) andalso V >= 0 -> true;
check_tk(_G, tk_ulong, V) when is_integer(V) andalso V >= 0 -> true;
check_tk(_G, tk_ulonglong, V) when is_integer(V) andalso V >= 0 -> true;  %% ULLON_G
check_tk(_G, tk_float, V) when is_float(V) -> true;
check_tk(_G, tk_double, V) when is_float(V) -> true;
check_tk(_G, tk_boolean, V) -> is_bool(V);
check_tk(_G, tk_char, {char, _V}) -> true;
check_tk(_G, tk_wchar, {wchar, _V}) -> true; %% WCHAR
check_tk(_G, {tk_string, _Len}, {string, _V}) -> true;
check_tk(_G, {tk_wstring, _Len}, {wstring, _V}) -> true;  %% WSTRING
check_tk(_G, {tk_fixed, Digits, Scale}, {fixed, Digits, Scale, _V}) -> true;
check_tk(_G, tk_octet, V) when is_integer(V) -> true;
%%check_tk(_G, tk_null, V) when integer(V) -> true;
%%check_tk(_G, tk_void, V) when integer(V) -> true;
%%check_tk(_G, tk_any, V) when integer(V) -> true;
%%check_tk(_G, {tk_objref, "", "Object"}, V) when integer(V) -> true.
check_tk(_G, {tk_enum, _, _, Body}, {enum_id, Id}) -> 
    until(fun(X) when X == Id -> true;
	     (_X) -> 
		  false 
	  end, Body);
check_tk(_G, _TK, _V) ->
    false.

get_val({string, X}) -> X;
get_val({wstring, X}) -> X;  %% WCHAR
get_val({char, X}) -> X;
get_val({wchar, X}) -> X;  %% WSTRING
get_val({enum_id, X}) -> X;
get_val(X) -> X.

check_types(G, Op, Expr, TypeList, V) ->
    case until(fun(int) when is_integer(V) -> true;
		  (float) when is_float(V) -> true;
		  (bool) when V==true -> true;
		  (bool) when V==false -> true;
		  (fixed) when is_record(V, fixed) -> true;
		  (_) -> false end,
	       TypeList) of
	true -> true;
	false ->
	    Err = {bad_type, Expr, Op, TypeList, V},
	    ic_error:error(G, Err),
	    throw({error, Err})
    end.

%%get_op(T) when tuple(T) -> element(1, T).

%% Should be in lists
until(F, [H|T]) ->
    case F(H) of
	true -> true;
	false -> until(F, T)
    end;
until(_F, []) -> false.

%% Section of all the boolean operators (because Erlang ops don't like
%% boolean values.
e_or(X, Y) when is_integer(X) andalso is_integer(Y) -> X bor Y;
e_or(true, _) -> true;
e_or(_, true) -> true;
e_or(_, _) -> false.

e_and(X, Y) when is_integer(X) andalso is_integer(Y) -> X band Y;
e_and(true, true) -> true;
e_and(_, _) -> false.

e_xor(X, Y) when is_integer(X) andalso is_integer(Y) -> X bxor Y;
e_xor(X, X) -> false;
e_xor(_, _) -> true.

%% Handling infix operators (+,-,*,/) for fixed type.
%% Boundries determined as fixed<max(d1-s1,d2-s2) + max(s1,s2) + 1, max(s1,s2)>
e_fixed_add(#fixed{digits = D1, scale = S1, value = V1}, 
	    #fixed{digits = D2, scale = S2, value = V2}) ->
    Scale = ?get_max(S1, S2),
    Digits = ?get_max((D1-S1), (D2-S2)) + Scale +1,
    %% We must normalize the values before adding. Why?
    %% 4.23 and 5.2 are represented as 423 and 52. To be able to get the 
    %% correct result we must add 4230 and 5200 == 9430.
    {PV1, PV2} = normalize(S1, V1, S2, V2),
    check_fixed_overflow(#fixed{digits = Digits, 
				scale = Scale, 
				value = (PV1 + PV2)}).

%% Boundries determined as fixed<max(d1-s1,d2-s2) + max(s1,s2) + 1, max(s1,s2)>
e_fixed_sub(#fixed{digits = D1, scale = S1, value = V1}, 
	    #fixed{digits = D2, scale = S2, value = V2}) ->
    Scale = ?get_max(S1, S2),
    Digits = ?get_max((D1-S1), (D2-S2)) + Scale +1,
    {PV1, PV2} = normalize(S1, V1, S2, V2),
    check_fixed_overflow(#fixed{digits = Digits, 
				scale = Scale, 
				value = (PV1 - PV2)}).

%% Boundries determined as fixed<d1+d2, s1+s2>
e_fixed_mul(#fixed{digits = D1, scale = S1, value = V1}, 
	    #fixed{digits = D2, scale = S2, value = V2}) ->
    check_fixed_overflow(#fixed{digits = (D1+D2), 
				scale = (S1+S2), 
				value = V1*V2}).

%% Boundries determined as fixed<(d1-s1+s2) + s inf ,s inf>
e_fixed_div(#fixed{digits = D1, scale = S1, value = V1}, 
	    #fixed{digits = _D2, scale = S2, value = V2}) ->
    {PV1, PV2} = normalize(S1, V1, S2, V2),
    DigitsMin = (D1-S1+S2),
    R1 = (PV1 div PV2),
    R2 = (R1*?BASE + (PV1 rem PV2) * (?BASE div PV2)),
    {Result2, Sinf} = delete_zeros_value(R2, 0, R1),
    check_fixed_overflow(#fixed{digits = DigitsMin + Sinf, scale = Sinf, 
				value = Result2}).


%% Checks combination of argument types, basically floats and ints are
%% interchangeable, and all types are allowed with themselves. No
%% other combinations are allowed
%%
check_comb(X, Y) when is_integer(X) andalso is_integer(Y) -> true;
check_comb(X, Y) when is_float(X) andalso is_integer(Y) -> true;
check_comb(X, Y) when is_integer(X) andalso is_float(Y) -> true;
check_comb(X, Y) when is_float(X) andalso is_float(Y) -> true;
check_comb({X, _}, {X, _}) -> true;		% Strings and chars are tuples
check_comb({fixed, _, _, _}, {fixed, _, _, _}) -> true;
check_comb(X, Y) ->
    case {is_bool(X), is_bool(Y)} of
	{true, true} -> 
	    true;
	_ -> 
	    false
    end.

is_bool(true) -> true;
is_bool(false) -> true;
is_bool(_) -> false.


%%%% (15)
eval_e(G, S, N, Tk, {'or', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, Tk, [int, bool], 'or', T1, T2),
    e_or(E1, E2);

%%%% (16)
eval_e(G, S, N, Tk, {'xor', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, Tk, [int, bool], 'xor', T1, T2),
    e_xor(E1, E2);

%%%% (17)
eval_e(G, S, N, Tk, {'and', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, Tk, [int, bool], 'and', T1, T2),
    e_and(E1, E2);

%%%% (18)
eval_e(G, S, N, Tk, {'rshift', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, Tk,  [int], 'rshift', T1, T2),
    E1 bsr E2;
eval_e(G, S, N, Tk, {'lshift', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, Tk, [int], 'lshift', T1, T2),
    E1 bsl E2;

%%%% (19)
eval_e(G, S, N, Tk, {'+', T1, T2}) ->
    case check_op(G, S, N, Tk, [int, float, fixed], '+', T1, T2) of
	{F1, F2} when is_record(F1,fixed) andalso is_record(F2,fixed) ->
	    e_fixed_add(F1, F2);
	{E1, E2} ->
	    E1 + E2
    end;
eval_e(G, S, N, Tk, {'-', T1, T2}) ->
    case check_op(G, S, N, Tk, [int, float, fixed], '-', T1, T2) of
	{F1, F2} when is_record(F1,fixed) andalso is_record(F2,fixed) ->
	    e_fixed_sub(F1, F2);
	{E1, E2} ->
	    E1 - E2
    end;

%%%% (20)
eval_e(G, S, N, Tk, {'*', T1, T2}) ->
    case check_op(G, S, N, Tk, [int, float, fixed], '*', T1, T2) of
	{F1, F2} when is_record(F1,fixed) andalso is_record(F2,fixed) ->
	    e_fixed_mul(F1, F2);
	{E1, E2} ->
	    E1 * E2
    end;
eval_e(G, S, N, Tk, {'/', T1, T2}) ->
    case check_op(G, S, N, Tk, [int, float, fixed], '/', T1, T2) of
	{F1, F2} when is_record(F1,fixed) andalso is_record(F2,fixed) ->
	    e_fixed_div(F1, F2);
	{E1, E2} ->
	    E1 / E2
    end;
eval_e(G, S, N, Tk, {'%', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, Tk, [int], '%', T1, T2),
    E1 rem E2;

%%%% (21)
eval_e(G, S, N, Tk, {{'-', _Line}, T}) ->
    case check_op(G, S, N, Tk, [int, float, fixed], '-', T) of
	F when is_record(F,fixed) ->
	    F#fixed{value = -(F#fixed.value)};
	Number ->
	    -Number
    end;
eval_e(G, S, N, Tk, {{'+', _Line}, T}) ->
    check_op(G, S, N, Tk, [int, float, fixed], '+', T);
eval_e(G, S, N, Tk, {{'~', Line}, T}) ->
    ic_error:error(G, {unsupported_op, {'~', Line}}),
    eval_e(G, S, N, Tk, T);


%% Ints are repr. by an Erlang integer val, floats and doubles by
%% Erlang floats, chars and strings must be tuplerized for type
%% checking. These tuples are removed just before returning from top
%% function.
%%
eval_e(_G, _S, _N, tk_fixed, {'<fixed_pt_literal>', _Line, X}) ->
    create_fixed(X);
eval_e(G, _S, _N, {tk_fixed, Digits, Scale}, {'<fixed_pt_literal>', Line, X})
  when Digits < 32, Digits >= Scale ->
    case convert_fixed(X, [], Digits, Digits-Scale) of
	{error, Format, Args} ->
	    ic_error:error(G, {bad_fixed, Format, Args, Line});
	FixedData ->
	    {fixed, Digits, Scale, FixedData}
    end;
eval_e(_G, _S, _N, _Tk, {'<integer_literal>', _Line, X}) -> list_to_integer(X);
eval_e(_G, _S, _N, {tk_string,_}, {'<string_literal>', _Line, X}) -> {string, X};
eval_e(_G, _S, _N, {tk_wstring,_}, {'<wstring_literal>', _Line, X}) -> {wstring, X}; %% WSTRING
eval_e(_G, _S, _N, tk_char, {'<character_literal>', _Line, X}) -> {char, hd(X)};
eval_e(_G, _S, _N, tk_wchar, {'<wcharacter_literal>', _Line, X}) -> {wchar, hd(X)}; %% WCHAR
eval_e(_G, _S, _N, _Tk, {'TRUE', _Line}) -> true;
eval_e(_G, _S, _N, _Tk, {'FALSE', _Line}) -> false;
eval_e(_G, _S, _N, _Tk, {'<floating_pt_literal>', _Line, X}) -> to_float(X);
%% Some possible error conditions
eval_e(_G, _S, _N, _Tk, {'<character_literal>', _Line, X}) -> {char, hd(X)}; %% ERROR?
%%
eval_e(G, S, N, _Tk, X) when element(1, X) == scoped_id ->
    mk_val(ictype:scoped_lookup(G, S, N, X));
eval_e(_G, _S, _N, _Tk, {default, _}) -> default;	% Default case in union
eval_e(G, _S, _N, Tk, Val) ->
    ic_error:error(G, {plain_error_string, Val, 
		       io_lib:format("value and declared type ~p differ", [Tk])}).

%% A fixed type can be 123.45 or 123 but we represent it as integers (i.e. 12345 or 123).
convert_fixed([], Acc, 0, _) ->
    list_to_integer(lists:reverse(Acc));
convert_fixed([], _Acc, _, _) ->
    {error, "Fixed type do not match the digits field", []};
convert_fixed([$.|Rest], Acc, Digits, 0) ->
    convert_fixed(Rest, Acc, Digits, -1);
convert_fixed([$.|_Rest], _Acc, _, _) ->
    {error, "Fixed decimal point placed incorrectly", []};
convert_fixed([X|Rest], Acc, Digits, Position) ->
    convert_fixed(Rest, [X|Acc], Digits-1, Position-1).


create_fixed([$0|Rest]) ->
    %% Leading zeros shall be ignored.
    create_fixed(Rest);
create_fixed(Fixed) ->
    create_fixed(Fixed, [], 0, 0, false).

create_fixed([], Acc, Total, Frac, true) ->
    {Fixed, N} = remove_trailing_zeros(Acc, 0),
    Digits = Total-N,
    Scale = Frac-N,
    #fixed{digits = Digits, scale = Scale, value = list_to_integer(Fixed)};
create_fixed([], Acc, Total, _Frac, false) ->
    %% A '.' never found. Hence, must be 2000D
    #fixed{digits = Total, scale = 0, value = list_to_integer(lists:reverse(Acc))};
create_fixed([$.|Rest], Acc, Total, _, _) ->
    create_fixed(Rest, Acc, Total, 0, true);
create_fixed([X|Rest], Acc, Total, Frac, FoundDot) ->
    create_fixed(Rest, [X|Acc], Total+1, Frac+1, FoundDot).

remove_trailing_zeros([$0|Rest], N) ->
    remove_trailing_zeros(Rest, N+1);
remove_trailing_zeros(Fixed, N) ->
    {lists:reverse(Fixed), N}.

%% Make the newly looked up value a value that can be type checked.
mk_val({_, _, {tk_string, _}, V}) -> {string, V};
mk_val({_, _, {tk_wstring, _}, V}) -> {wstring, V};  %% WSTRING
mk_val({_, _, tk_char, V}) -> {char, V};
mk_val({_, _, tk_wchar, V}) -> {wchar, V}; %% WCHAR
mk_val({_, _, enum_val, V}) -> 
    {enum_id, ic_forms:get_id2(V)};
mk_val(X) when element(1, X) == error -> X;
mk_val({_, _, _TK, V}) -> 
    V;
mk_val(V) -> V.



%% Floating point numbers
%%
%%	Conversion to Erlang floating points is neccessary because
%%	list_to_float BIF differs from IDL floats. "1e2" ".4e2" is
%%	allowed in IDL and must be translated to "1.0e2" and "0.4e2"

to_float(X) ->
    list_to_float(erlangify(X)).

erlangify([$. | R]) ->
    [$0, $. | R];
erlangify(R) ->
    look_for_dot(R).

look_for_dot([$. | R]) -> [$. | dot_pending(R)];
look_for_dot([$e | R]) -> [$., $0, $e | R];
look_for_dot([$E | R]) -> [$., $0, $E | R];
look_for_dot([X | R]) -> [X | look_for_dot(R)].

dot_pending([$e | R]) -> [$0, $e | R];
dot_pending([$E | R]) -> [$0, $E | R];
dot_pending([]) -> [$0];
dot_pending(R) -> R.


%%------------------------------------------------------------------
%%--------------- Fixed Datatype Helper Functions ------------------
%%------------------------------------------------------------------
%% Pretty?! No, but since we now the upper-limit this is the fastest way
%% to calculate 10^x
power(0) ->  1;
power(1) ->  10;
power(2) ->  100;
power(3) ->  1000;
power(4) ->  10000;
power(5) ->  100000;
power(6) ->  1000000;
power(7) ->  10000000;
power(8) ->  100000000;
power(9) ->  1000000000;
power(10) -> 10000000000;
power(11) -> 100000000000;
power(12) -> 1000000000000;
power(13) -> 10000000000000;
power(14) -> 100000000000000;
power(15) -> 1000000000000000;
power(16) -> 10000000000000000;
power(17) -> 100000000000000000;
power(18) -> 1000000000000000000;
power(19) -> 10000000000000000000;
power(20) -> 100000000000000000000;
power(21) -> 1000000000000000000000;
power(22) -> 10000000000000000000000;
power(23) -> 100000000000000000000000;
power(24) -> 1000000000000000000000000;
power(25) -> 10000000000000000000000000;
power(26) -> 100000000000000000000000000;
power(27) -> 1000000000000000000000000000;
power(28) -> 10000000000000000000000000000;
power(29) -> 100000000000000000000000000000;
power(30) -> 1000000000000000000000000000000;
power(31) -> 10000000000000000000000000000000;
power(_) ->  10000000000000000000000000000000.



%% If the result of an operation (+, -, * or /) causes overflow we use this 
%% operation. However, since these calculations are performed during compiletime,
%% shouldn't the IDL-specification be changed to not cause overflow?! But, since
%% the OMG standard allows this we must support it.
check_fixed_overflow(#fixed{digits = Digits, scale = Scale, value = Value}) ->
    case count_digits(abs(Value)) of
	overflow ->
	    {N, NewVal} = cut_overflow(0, Value),
%	    NewDigits = Digits - N,
	    if
		N > Scale ->
		    #fixed{digits = 31, scale = 0, value = NewVal};
		true ->
		    NewScale = Scale - N,
		    {NewVal2, Removed} = delete_zeros(NewVal, NewScale),
		    #fixed{digits = 31, scale = NewScale-Removed, value = NewVal2}
	    end;
	Count when Count > Digits ->
	    Diff = Count-Digits,
	    if
		Diff > Scale ->
		    #fixed{digits = Digits, scale = 0, 
			   value = (Value div power(Diff))};
		true ->
		    NewScale = Scale-Diff,
		    {NewVal, Removed} = delete_zeros((Value div power(Diff)), NewScale),
		    #fixed{digits = Digits-Removed, 
			   scale = NewScale-Removed, 
			   value = NewVal}
	    end;
	Count ->
	    {NewVal, Removed} = delete_zeros(Value, Scale),
	    #fixed{digits = Count-Removed, scale = Scale-Removed, value = NewVal}
    end.

%% This function see to that the values are of the same baase.
normalize(S, V1, S, V2) ->
    {V1, V2};
normalize(S1, V1, S2, V2) when S1 > S2 ->
    {V1, V2*power(S1-S2)};
normalize(S1, V1, S2, V2) ->
    {V1*power(S2-S1), V2}.

%% If we have access to the integer part of the fixed type we use this
%% operation to remove all trailing zeros. If we know the scale, length of
%% fraction part, we can use delete_zeros as well. But, after a division
%% it's hard to know the scale and we don't need to calcluate the integer part.
delete_zeros_value(0, N, _) ->
    {0, 32-N};
delete_zeros_value(X, N, M) when X > M, (X rem 10) == 0 ->
    delete_zeros_value((X div 10), N+1, M);
delete_zeros_value(X, N, _) ->
    {X, 32-N}.

%% If we know the exact scale of a fixed type we can use this operation to
%% remove all trailing zeros.
delete_zeros(0, _) ->
    {0,0};
delete_zeros(X, Max) ->
    delete_zeros(X, 0, Max).
delete_zeros(X, Max, Max) ->
    {X, Max};
delete_zeros(X, N, Max) when (X rem 10) == 0 ->
    delete_zeros((X div 10), N+1, Max);
delete_zeros(X, N, _) ->
    {X, N}.
                            
cut_overflow(N, X) when X > ?FIXED_MAX ->
    cut_overflow(N+1, (X div 10));
cut_overflow(N, X) ->
    {N, X}.
    
%% A fast way to check the size of a fixed data type.
count_digits(X) when X >  ?FIXED_MAX -> overflow;
count_digits(X) when X >= 1000000000000000000000000000000 -> 31;
count_digits(X) when X >= 100000000000000000000000000000 -> 30;
count_digits(X) when X >= 10000000000000000000000000000 -> 29;
count_digits(X) when X >= 1000000000000000000000000000 -> 28;
count_digits(X) when X >= 100000000000000000000000000 -> 27;
count_digits(X) when X >= 10000000000000000000000000 -> 26;
count_digits(X) when X >= 1000000000000000000000000 -> 25;
count_digits(X) when X >= 100000000000000000000000 -> 24;
count_digits(X) when X >= 10000000000000000000000 -> 23;
count_digits(X) when X >= 1000000000000000000000 -> 22;
count_digits(X) when X >= 100000000000000000000 -> 21;
count_digits(X) when X >= 10000000000000000000 -> 20;
count_digits(X) when X >= 1000000000000000000 -> 19;
count_digits(X) when X >= 100000000000000000 -> 18;
count_digits(X) when X >= 10000000000000000 -> 17;
count_digits(X) when X >= 1000000000000000 -> 16;
count_digits(X) when X >= 100000000000000 -> 15;
count_digits(X) when X >= 10000000000000 -> 14;
count_digits(X) when X >= 1000000000000 -> 13;
count_digits(X) when X >= 100000000000 -> 12;
count_digits(X) when X >= 10000000000 -> 11;
count_digits(X) when X >= 1000000000 -> 10;
count_digits(X) when X >= 100000000 -> 9;
count_digits(X) when X >= 10000000 -> 8;
count_digits(X) when X >= 1000000 -> 7;
count_digits(X) when X >= 100000 -> 6;
count_digits(X) when X >= 10000 -> 5;
count_digits(X) when X >= 1000 -> 4;
count_digits(X) when X >= 100 -> 3;
count_digits(X) when X >= 10 -> 2;
count_digits(_X) -> 1.
    
%%------------------------------------------------------------------
%%--------------- END Fixed Datatype Helper Functions --------------
%%------------------------------------------------------------------

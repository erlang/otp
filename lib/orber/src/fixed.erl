%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
%%--------------------------------------------------------------------
%% File    : fixed.erl
%% Purpose : 
%%--------------------------------------------------------------------

-module(fixed).

-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([create/3, add/2, subtract/2, divide/2, multiply/2, unary_minus/1,
	 get_typecode/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% Definitions
%%-----------------------------------------------------------------
-define(get_max(__X, __Y), if __X > __Y -> __X; true -> __Y end).
-define(get_min(__X, __Y), if __X > __Y -> __Y; true -> __X end).

-define(BASE, 100000000000000000000000000000000).
-define(FIXED_MAX,  9999999999999999999999999999999).
-define(FIXED_MIN, -9999999999999999999999999999999).

-define(DEBUG_LEVEL, 5).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
create(Digits, Scale, Value) when is_integer(Digits) andalso Digits >= 0 andalso Digits < 32 andalso
				  is_integer(Scale) andalso Scale >= 0 andalso Digits >= Scale andalso
				  is_integer(Value) andalso Value =< ?FIXED_MAX andalso
				  Value >= ?FIXED_MIN ->
    case count_digits(abs(Value)) of
	Dig when Dig =< Digits ->
	    #fixed{digits = Digits, scale = Scale, value = Value};
	Overflow ->
	    orber:dbg("[~p] fixed:create(~p, ~p, ~p).~n"
		      "The Value exceeds the Digits limit: ~p, ~p", 
		      [?LINE, Digits, Scale, Value, Digits, Overflow], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end;
create(Digits, Scale, Value) ->
    orber:dbg("[~p] fixed:add(~p, ~p, ~p).~n"
	      "At least one of the supplied arguments is incorrect.~n"
	      "Digits and Scale must be a positive integer with the following~n"
	      "limits:~n"
	      " * 0 =< Digits < 32~n"
	      " * Digits >= Scale~n"
	      " * Value range +/- 9999999999999999999999999999999", 
	      [?LINE, Digits, Scale, Value], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


get_typecode(#fixed{digits = Digits, scale = Scale}) ->
    {tk_fixed, Digits, Scale};
get_typecode(Other) ->
    orber:dbg("[~p] fixed:get_typecode(~p).
The supplied argument is not a Fixed Type.", [?LINE, Other], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

add(#fixed{digits = D1, scale = S1, value = V1}, 
    #fixed{digits = D2, scale = S2, value = V2}) ->
    Scale = ?get_max(S1, S2),
    Digits = ?get_max((D1-S1), (D2-S2)) + Scale +1,
    %% We must normalize the values before adding. Why?
    %% 4.23 and 5.2 are represented as 423 and 52. To be able to get the 
    %% correct result we must add 4230 and 5200 == 9430.
    {PV1, PV2} = normalize(S1, V1, S2, V2),
    check_fixed_overflow(#fixed{digits = Digits, 
				scale = Scale, 
				value = (PV1 + PV2)});
add(F1, F2) ->
    orber:dbg("[~p] fixed:add(~p, ~p).~n"
	      "At least one of the supplied arguments is not a Fixed Type.", 
	      [?LINE, F1, F2], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


subtract(#fixed{digits = D1, scale = S1, value = V1}, 
	 #fixed{digits = D2, scale = S2, value = V2}) ->
    Scale = ?get_max(S1, S2),
    Digits = ?get_max((D1-S1), (D2-S2)) + Scale +1,
    {PV1, PV2} = normalize(S1, V1, S2, V2),
    check_fixed_overflow(#fixed{digits = Digits, 
				scale = Scale, 
				value = (PV1 - PV2)});
subtract(F1, F2) ->
    orber:dbg("[~p] fixed:subtract(~p, ~p).~n"
	      "At least one of the supplied arguments is not a Fixed Type.", 
	      [?LINE, F1, F2], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

divide(#fixed{digits = D1, scale = S1, value = V1}, 
	    #fixed{digits = _D2, scale = S2, value = V2}) ->
    {PV1, PV2} = normalize(S1, V1, S2, V2),
    DigitsMin = (D1-S1+S2),
    R1 = (PV1 div PV2),
    R2 = (R1*?BASE + (PV1 rem PV2) * (?BASE div PV2)),
    {Result2, Sinf} = delete_zeros_value(R2, 0, R1),
    check_fixed_overflow(#fixed{digits = DigitsMin + Sinf, scale = Sinf, 
				value = Result2});
divide(F1, F2) ->
    orber:dbg("[~p] fixed:divide(~p, ~p).~n"
	      "At least one of the supplied arguments is not a Fixed Type.", 
	      [?LINE, F1, F2], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


multiply(#fixed{digits = D1, scale = S1, value = V1}, 
	 #fixed{digits = D2, scale = S2, value = V2}) ->
    check_fixed_overflow(#fixed{digits = (D1+D2), 
				scale = (S1+S2), 
				value = V1*V2});
multiply(F1, F2) ->
    orber:dbg("[~p] fixed:multiply(~p, ~p).~n"
	      "At least one of the supplied arguments is not a Fixed Type.", 
	      [?LINE, F1, F2], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

unary_minus(Fixed) when is_record(Fixed, fixed) ->
    Fixed#fixed{value = -(Fixed#fixed.value)};
unary_minus(Fixed) ->
    orber:dbg("[~p] fixed:unary_minus(~p).~n"
	      "The supplied argument is not a Fixed Type.", 
	      [?LINE, Fixed], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).



%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
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

%%-----------------------------------------------------------------
%%------------- END OF MODULE -------------------------------------
%%-----------------------------------------------------------------

%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

-module(xmerl_xsd_type).

-export([check_simpleType/3,facet_fun/2,compare_floats/2,
	 replace_ws/2,collapse_ws/1]).

-export([fQuotient/2,fQuotient/3,modulo/2,modulo/3,maximumDayInMonthFor/2,
	 add_duration2dateTime/2,duration_atoms/1,dateTime_atoms/1,
	 normalize_dateTime/1]).

-export([compare_durations/2,compare_dateTime/2]).

-include("xmerl.hrl").
-include("xmerl_internal.hrl").
-include("xmerl_xsd.hrl").


-define(catch_exit(_Call_,_Value_,_ErrorCause_),
	case catch (_Call_) of
	    {'EXIT',_} ->
		{error,{type,_ErrorCause_,_Value_}};
	    {error,_} ->
		{error,{_ErrorCause_,_Value_}};
	    _ ->
		{ok,_Value_}
	end).

-define(is_whitespace(__WS__),
	__WS__==16#20; __WS__==16#9;__WS__==16#a; __WS__==16#d).

check_simpleType(Name,Value,S) when is_list(Name) ->
    ?debug("simpleType name a list: "++Name++"~n",[]),
    check_simpleType(list_to_atom(Name),Value,S);
check_simpleType(string,Value,_S) ->
    case [X||X <- Value,
	     xmerl_lib:is_char(X)] of
	Value ->
	    {ok,Value};
	_ ->
	    {error,{value_not_string,Value}}
    end;
check_simpleType(normalizedString,Value,_S) ->
    case [X||X <- Value,xmerl_lib:is_char(X),
	     ns_whitespace(X)==false] of
	Value ->
	    {ok,Value};
	_ ->
	    {error,{value_not_normalizedString,Value}}
    end;
check_simpleType(boolean,"true",_S) -> {ok,"true"};
check_simpleType(boolean,"false",_S) -> {ok,"false"};
check_simpleType(boolean,"1",_S) -> {ok,"1"};
check_simpleType(boolean,"0",_S) -> {ok,"0"};
check_simpleType(boolean,Other,_S) -> {error,{value_not_boolean,Other}};
check_simpleType(decimal,Value,_S) ->
    ?catch_exit(check_decimal(Value),Value,invalid_decimal);
check_simpleType(integer,Value,_S) ->
    ?catch_exit(check_integer(Value),Value,invalid_integer);

% float values: m * 2^e, where m is an integer whose absolute value is
% less than 2^24, and e is an integer between -149 and 104, inclusive.
check_simpleType(float,Value,_S) ->
    ?catch_exit(check_float(Value),Value,invalid_float);
% double values: m * 2^e, where m is an integer whose absolute value
% is less than 2^53, and e is an integer between -1075 and 970,
% inclusive.
check_simpleType(double,Value,_S) ->
    ?catch_exit(check_double(Value),Value,invalid_double);
% extended format PnYnMnDTnHnMnS where n is an integer. The n value
% before S may include decimal fraction.
check_simpleType(duration,Value,_S) ->
    ?catch_exit(check_duration(Value),Value,invalid_duration);	
check_simpleType(dateTime,Value,_S) ->
    ?catch_exit(check_dateTime(Value),Value,invalid_dateTime);
check_simpleType(time,Value,_S) ->
    ?catch_exit(check_time(Value),Value,invalid_time);
check_simpleType(date,Value,_S) ->
    ?catch_exit(check_date(Value),Value,invalid_date);
check_simpleType(gYearMonth,Value,_S) ->
    ?catch_exit(check_gYearMonth(Value),Value,invalid_gYearMonth);
check_simpleType(gYear,Value,_S) ->
    ?catch_exit(check_gYear(Value),Value,invalid_gYear);
check_simpleType(gMonthDay,Value,_S) ->
    ?catch_exit(check_gMonthDay(Value),Value,invalid_gMonthDay);
check_simpleType(gDay,Value,_S) ->
    ?catch_exit(check_gDay(Value),Value,invalid_gDay);
check_simpleType(gMonth,Value,_S) ->
    ?catch_exit(check_gMonth(Value),Value,invalid_gMonth);
check_simpleType(hexBinary,Value,_S) ->
    IsEven = fun(X) ->
		   case X rem 2 of
		       0 -> true;
		       _ -> false
		   end
	   end,
    IsHex = fun(X) when X >= $A, X =< $F -> true;
	       (X) when X >= $a, X =< $f -> true;
	       (X) when X >= $0, X =< $9 -> true;
	       (_) -> false
	    end,
    case [X|| X<-Value,
	      IsEven(length(Value)),
	      IsHex(X)] of
	Value ->
	    {ok,Value};
	_ -> {error,{value_not_hexBinary,Value}}
    end;
check_simpleType(base64Binary,Value,_S) ->
    check_base64Binary(Value);
check_simpleType(anyURI,Value,S) ->
    case xmerl_uri:parse(Value) of
	{error,_} ->
	    %% might be a relative uri, then it has to be a path in the context
	    case catch file:read_file_info(filename:join(S#xsd_state.xsd_base,Value)) of
		{ok,_} ->
		    {ok,Value};
		_ ->
		    {error,{value_not_anyURI,Value}}
	    end;
	_ ->
	    {ok,Value}
    end;
check_simpleType('QName',Value,_S) ->
    case xmerl_lib:is_name(Value) of
	true ->
	    {ok,Value};
	_ ->
	    {error,{value_not_QName,Value}}
    end;
check_simpleType('NOTATION',Value,_S) ->
    {ok,Value}; %% Must provide for check of all QNames in schema.
check_simpleType(token,Value,_S) ->
    ?catch_exit(check_token(Value),Value,invalid_token);
%% conform to the pattern [a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*
check_simpleType(language,Value,_S) ->
    ?catch_exit(check_language(Value),Value,illegal_language);
check_simpleType('NMTOKEN',Value,_S) ->
    ?catch_exit(check_NMTOKEN(Value),Value,illegal_NMTOKEN);
check_simpleType('NMTOKENS',Value,_S) ->
    ?catch_exit(check_NMTOKENS(Value),Value,illegal_NMTOKENS);
check_simpleType('Name',Value,_S) ->
    ?catch_exit(check_Name(Value),Value,illegal_name);
check_simpleType('NCName',Value,_S) ->
    ?catch_exit(check_NCName(Value),Value,illegal_name);
check_simpleType('ID',Value,_S) ->
    ?catch_exit(check_ID(Value),Value,illegal_ID);
check_simpleType('IDREF',Value,_S) ->
    ?catch_exit(check_IDREF(Value),Value,illegal_IDREF);
check_simpleType('IDREFS',Value,_S) ->
    ?catch_exit(check_IDREFS(Value),Value,illegal_IDREFS);
check_simpleType('ENTITY',Value,_S) ->
    ?catch_exit(check_ENTITY(Value),Value,illegal_ENTITY);
check_simpleType('ENTITIES',Value,_S) ->
    ?catch_exit(check_ENTITIES(Value),Value,illegal_ENTITIES);
check_simpleType(nonPositiveInteger,Value,_S) ->
    ?catch_exit(check_nonPositiveInteger(Value),Value,
		illegal_nonPositiveInteger);
check_simpleType(negativeInteger,Value,_S) ->
    ?catch_exit(check_negativeInteger(Value),Value,
		illegal_negativeInteger);
check_simpleType(long,Value,_S) ->
    ?catch_exit(check_long(Value),Value,illegal_long);
check_simpleType(int,Value,_S) ->
    ?catch_exit(check_int(Value),Value,illegal_int);
check_simpleType(short,Value,_S) ->
    ?catch_exit(check_short(Value),Value,illegal_short);
check_simpleType(byte,Value,_S) ->
    ?catch_exit(check_byte(Value),Value,illegal_byte);
check_simpleType(nonNegativeInteger,Value,_S) ->
    ?catch_exit(check_nonNegativeInteger(Value),Value,
		illegal_nonNegativeInteger);
check_simpleType(unsignedLong,Value,_S) ->
    ?catch_exit(check_unsignedLong(Value),Value,illegal_unsignedLong);
check_simpleType(unsignedInt,Value,_S) ->
    ?catch_exit(check_unsignedInt(Value),Value,illegal_unsignedInt);
check_simpleType(unsignedShort,Value,_S) ->
    ?catch_exit(check_unsignedShort(Value),Value,illegal_unsignedShort);
check_simpleType(unsignedByte,Value,_S) ->
    ?catch_exit(check_unsignedByte(Value),Value,illegal_unsignedByte);
check_simpleType(positiveInteger,Value,_S) ->
    ?catch_exit(check_positiveInteger(Value),Value,illegal_positiveInteger);
check_simpleType(Unknown,Value,_S) ->
    {error,{unknown_type,Unknown,Value}}.

check_decimal(Value) ->
    case string:tokens(Value,".") of
	L when length(L) == 1; length(L) == 2 ->
	    _ = [list_to_integer(X)||X <- L],
	    {ok,Value};
	_ ->
	    {error,{value_not_decimal,Value}}
    end.
%%     I=string:chr(Value,$.),
%%     {NumberDot,Decimal}=lists:split(I,Value),
%%     Number=string:strip(NumberDot,right,$.),
%%     case catch {list_to_integer(Number),list_to_integer(Decimal)} of
%% 	{'EXIT',_} ->
%% 	    {error,{value_not_decimal,Value}};
%% 	_ -> {ok,Value}
%%     end.

check_float(V="-INF") ->
    {ok,V};
check_float(V="INF") ->
    {ok,V};
check_float(V="NaN") ->
    {ok,V};
check_float(Value) ->
%%     Pred = fun(X) when X==$e;X==$E -> false;(_) -> true end,
%%     {Mantissa,Exponent}=lists:splitwith(Pred,Value),
%%     SkipEe = fun([]) -> [];(L) -> tl(L) end,
    case string:tokens(Value,"eE") of
        [Mantissa,Exponent] ->
            {ok,_} = check_decimal(Mantissa),
            {ok,_} = check_integer(Exponent),
            ok;
        [Mantissa] ->
            {ok,_} = check_decimal(Mantissa),
            ok
    end,
    {ok,Value}.
%%     case {check_decimal(Mantissa),
%% 	  check_simpleType(integer,SkipEe(Exponent))} of
%% 	{{ok,_},{ok,_}} ->
%% 	    {ok,Value};
%% 	_ ->
%% 	    {error,{value_not_float,Value}}
%%     end.

check_double(Value) ->
    check_float(Value).


%% format PnYnMnDTnHnMnS
%% P is always present
%% T is absent iff all time items are absent
%% At least one duration item must be present
check_duration("-"++Value) ->
    check_duration(Value);
check_duration("P"++Value) ->
    {Date,Time}=lists:splitwith(fun($T) -> false;(_) -> true end,Value),
    {ok,_} = check_duration_date(Date,["Y","M","D"]),
    {ok,_} = check_duration_time(Time,["T","H","M","S"]).
	    
check_duration_date("",_) ->
    {ok,""};
check_duration_date(Date,[H|T]) ->
    case string:tokens(Date,H) of
	[Date] ->
	    check_duration_date(Date,T);
	[DateItem] ->
	    {ok,_} = check_positive_integer(DateItem);
	[DateItem,Rest] ->
	    {ok,_} = check_positive_integer(DateItem),
	    check_duration_date(Rest,T)
    end.
%% Time any combination of TnHnMfS
%% n unsigned integers and f unsigned decimal
%%check_duration_time(Time,["T","H","M","S"]) 
check_duration_time("",[_H|_T]) ->
    {ok,""};
check_duration_time(Time,[S]) ->
    [Sec] = string:tokens(Time,S),
    {ok,_} = check_decimal(Sec);
check_duration_time("T"++Time,TTokens) ->
    [_H|_] = Time,
    check_duration_time(Time,tl(TTokens));
check_duration_time(Time,[H|T]) ->
    case string:tokens(Time,H) of
	[Time] ->
	    check_duration_time(Time,T);
	[TimeItem] ->
	    {ok,_} = check_positive_integer(TimeItem);
	[TimeItem,Rest] ->
	    {ok,_} = check_positive_integer(TimeItem),
	    check_duration_time(Rest,T)
    end.

check_positive_integer(Value) ->
    case catch list_to_integer(Value) of
	Int when is_integer(Int),Int>=0 ->
	    {ok,Int};
	_ ->
	    {error,{value_not_integer,Value}}
    end.


%% check_integer and thereof derived types
check_integer(Value) ->
    {ok,list_to_integer(Value)}.
    
check_nonPositiveInteger(Value) ->
    check_constr_int(Value,undefined,0,illegal_nonPositiveInteger).

check_negativeInteger(Value) ->
    check_constr_int(Value,undefined,-1,illegal_negativeInteger).

check_long(Value) ->
    check_constr_int(Value,-9223372036854775808,
		     9223372036854775807,illegal_long).

check_int(Value) ->
    check_constr_int(Value,-2147483648,2147483647,illegal_int).

check_short(Value) ->
    check_constr_int(Value,-32768,32767,illegal_short).

check_byte(Value) ->
    check_constr_int(Value,-128,127,illegal_byte).

check_nonNegativeInteger(Value) ->
    check_constr_int(Value,0,undefined,illegal_nonNegativeInteger).

check_unsignedLong(Value) ->
    check_constr_int(Value,0,18446744073709551615,illegal_unsignedLong).

check_unsignedInt(Value) ->
    check_constr_int(Value,0,4294967295,illegal_unsignedInt).

check_unsignedShort(Value) ->
    check_constr_int(Value,0,65535,illegal_unsignedShort).

check_unsignedByte(Value) ->
    check_constr_int(Value,0,255,illegal_unsignedByte).

check_positiveInteger(Value) ->
    check_constr_int(Value,1,undefined,illegal_positiveInteger).

check_constr_int(Value,undefined,Max,ErrMsg) ->
    case check_integer(Value) of
	{ok,Int} when Int =< Max ->
	    {ok,Int};
	_ ->
	    {error,{ErrMsg}}
    end;
check_constr_int(Value,Min,Max,ErrMsg) ->
    case check_integer(Value) of
	{ok,Int} when Int >= Min, Int =< Max ->
	    {ok,Int};
	_ ->
	    {error,{ErrMsg}}
    end.

%% DateTime on form: '-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss 
%% ('.' s+)? (zzzzzz)?
check_dateTime("-"++DateTime) ->
    check_dateTime(DateTime);
check_dateTime("+"++_DateTime) ->
    {error,{invalid_dateTime,plus_sign}};
check_dateTime(DateTime) ->
    [Date,Time] = string:tokens(DateTime,"T"),
    [Y,M,D] = string:tokens(Date,"-"),
    {ok,_} = check_year(Y),
    {ok,_} = check_positive_integer(M),
    {ok,_} = check_positive_integer(D),
    check_time(Time).

check_year(Y) when length(Y)>4 ->
    Y = string:strip(Y,left,$0),
    {ok,list_to_integer(Y)};
check_year(Y) ->
    case list_to_integer(Y) of
	Year when Year =/= 0 ->
	    {ok,Year};
	_ ->
	    {error,{invalid_year,Y}}
    end.

check_month(Str) ->
    case check_positive_integer(Str) of
	{ok,Int} when Int >= 1,Int =< 12 ->
	    {ok,Int};
	_ -> 
	    {error,{invalid_month,Str}}
    end.
check_day(Str) ->
    case check_positive_integer(Str) of
	{ok,Int} when Int >= 1,Int =< 31 ->
	    {ok,Int};
	_ -> 
	    {error,{invalid_day,Str}}
    end.


check_time(Time) ->
    %% hh:mm:ss (.s+)? TZ
    {HMS,TZ} =
    case lists:split(8,Time) of
	{T,"."++SecFractionsTZ} ->
	    OnlyDigits = fun(X) when X>=$0,X=<$9 ->true;(_)->false end,
	    {SecFrac,TZone} = lists:splitwith(OnlyDigits,SecFractionsTZ),
	    {ok,_} = check_positive_integer(SecFrac),
	    {T,TZone};
	{T,TZone} ->
	    {T,TZone}
    end,
    [H,M,S] = string:tokens(HMS,":"),
    {ok,_} = check_hour(H),
    {ok,_} = check_minute(M),
    {ok,_} = check_second(S),
    case TZ of
	[] ->
	    {ok,Time}; %% timezone optional
	_ ->
	    check_timezone(TZ)
    end.

check_hour(Str) ->
    case check_positive_integer(Str) of
	{ok,H} when H >= 0,H =< 24 ->
	    {ok,H};
	_ ->
	    {error,{invalid_hour,Str}}
    end.
check_minute(Str) ->
    case check_positive_integer(Str) of
	{ok,H} when H >= 0,H =< 60 ->
	    {ok,H};
	_ ->
	    {error,{invalid_minute,Str}}
    end.
check_second(Str) ->
    case check_positive_integer(Str) of
	{ok,H} when H >= 0,H =< 60 ->
	    {ok,H};
	_ ->
	    {error,{invalid_second,Str}}
    end.

check_timezone("Z") ->
    {ok,"Z"};
check_timezone(TZ) ->
    [H,M] = string:tokens(TZ,":"),
    case check_integer(H) of
	{ok,H2} when H2 >= -13, H2 =< 13 ->
	    case check_positive_integer(M) of
		{ok,M2} when M2 >= 0, M2 =< 59 ->
		    {ok,{H2,M2}};
		_ ->
		    {error,{invalid_timezone,TZ,M}}
	    end;
	{ok,H2} when H2==14;H2==-14 ->
	    case check_positive_integer(M) of
		{ok,0} ->
		    {ok,{H2,0}};
		_ ->
		    {error,{invalid_timezone,TZ}}
	    end;
	_ ->
	    {error,{invalid_timezone,TZ}}
    end.


%%  the form: '-'? yyyy '-' mm '-' dd zzzzzz?
check_date("-"++Date) ->
    check_date(Date);
check_date("+"++_Date) ->
    {error,{invalid_date,plus_sign}};
check_date(Date) ->
    {Year,Month,Day} =
    case string:tokens(Date,"-+Z") of
	[Y,M,D,TZ] ->
	    {ok,_}=check_timezone(TZ),
	    {Y,M,D};
	[Y,M,D] ->
	    {Y,M,D}
    end,
    {ok,_}=check_year(Year),
    {ok,_}=check_month(Month),
    {ok,_}=check_day(Day).
    
%% gYearMonth on the form: '-'? ccyy '-' mm zzzzzz?
check_gYearMonth("-"++Value) ->
    check_gYearMonth(Value);
check_gYearMonth("+"++_Value) ->
    {error,{invalid_gYearMonth,plus_sign}};
check_gYearMonth(Value) ->
    {Year,Month} =
    case string:tokens(Value,"-+Z") of
	[Y,M,TZ] ->
	    {ok,_} = check_timezone(TZ),
	    {Y,M};
	[Y,M] ->
	    {Y,M}
    end,
    {ok,_} = check_year(Year),
    {ok,_} = check_month(Month).

%% gYear on the form: '-'? ccyy zzzzzz?
check_gYear("-"++Value) ->
    check_gYear(Value);
check_gYear("+"++_Value) ->
    {error,{invalid_gYear,plus_sign}};
check_gYear(Value) ->
    Year =
	case string:tokens(Value,"-+Z") of
	    [Y,TZ] ->
		{ok,_} = check_timezone(TZ),
		Y;
	    [Y] ->
		Y
	end,
    {ok,_} = check_year(Year).
    
%% gMonthDay on the form: mm dd zzzzzz?
check_gMonthDay("--"++Value) ->
    {M,"-"++DTZ} = lists:split(2,Value),
    {ok,_} = check_month(M),
    {ok,_} = check_gDay2(DTZ).

%% dDay on the form dd zzzzzz?
check_gDay("---"++Value) ->
    check_gDay2(Value).
check_gDay2(Value) ->
    {D,TZ} = lists:split(2,Value),
    {ok,_} = check_day(D),
    case TZ of
	[] ->
	    {ok,Value};
	_ ->
	    {ok,_} = check_timezone(TZ)
    end.
%% dMonth on the form mm zzzzzz?
check_gMonth("--"++Value) ->
    {M,TZ} = lists:split(2,Value),
    {ok,_} = check_month(M),
    case TZ of
	[] ->
	    {ok,Value};
	_ ->
	    {ok,_} = check_timezone(TZ)
    end.

check_base64Binary(Value) ->
    case catch xmerl_b64Bin:parse(xmerl_b64Bin_scan:scan(Value)) of
	{ok,_} ->
	    {ok,Value};
	Err = {error,_} ->
	    Err;
	{'EXIT',{error,Reason}} -> %% scanner failed on character
	    {error,Reason};
	{'EXIT',Reason} ->
	    {error,{internal_error,Reason}}
    end.

%% tokens may not contain the carriage return (#xD), line feed (#xA)
%% nor tab (#x9) characters, that have no leading or trailing spaces
%% (#x20) and that have no internal sequences of two or more spaces.
check_token(V=[32|_]) ->
    {error,{invalid_token,leading_space,V}};
check_token(Value) ->
    check_token(Value,Value).
check_token([],Value) ->
    {ok,Value};
check_token([32],V) ->
    {error,{invalid_token,trailing_space,V}};
check_token([9|_T],V) ->
    {error,{invalid_token,tab,V}};
check_token([10|_T],V) ->
    {error,{invalid_token,line_feed,V}};
check_token([13|_T],V) ->
    {error,{invalid_token,carriage_return,V}};
check_token([32,32|_T],V) ->
    {error,{invalid_token,double_space,V}};
check_token([_H|T],V) ->
    check_token(T,V).

%% conform to the pattern [a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*
check_language(Value) ->
    check_language(Value,0).
check_language([H|T],N) when H>=$A,H=<$Z ->
    check_language(T,N+1);
check_language([H|T],N) when H>=$a,H=<$z ->
    check_language(T,N+1);
check_language([$-|T],N) when N>=1,N=<8 ->
    check_language2(T,0);
check_language([],N) when N>=1,N=<8 ->
    {ok,[]}.
check_language2([H|T],N) when H>=$A,H=<$Z ->
    check_language2(T,N+1);
check_language2([H|T],N) when H>=$a,H=<$z ->
    check_language2(T,N+1);
check_language2([H|T],N) when H>=$0,H=<$9 ->
    check_language2(T,N+1);
check_language2([$-|T],N) when N>=1,N=<8 ->
    check_language2(T,0);
check_language2([],N) when N>=1,N=<8 ->
    {ok,[]}.

check_NMTOKEN([H|T]) ->
    true = xmerl_lib:is_namechar(H),
    check_NMTOKEN2(T).
check_NMTOKEN2([]) ->
    {ok,[]};
check_NMTOKEN2([H|T]) ->
    true = xmerl_lib:is_namechar(H),
    check_NMTOKEN2(T).

check_NMTOKENS(Value) ->
    TokList = string:tokens(Value," \n\t\r"),
    lists:foreach(fun check_NMTOKEN/1,TokList),
    {ok,Value}.

check_Name(Value) ->
    true = xmerl_lib:is_name(Value),
    {ok,Value}.

check_NCName(Value) ->
    true = xmerl_lib:is_ncname(Value),
    {ok,Value}.

check_ID(Value) ->
    %% ID must be a NCName and uniquely identify the element which
    %% bear it. Only one ID per element.
    true = xmerl_lib:is_ncname(Value),
    {ok,Value}.

check_IDREF(Value) ->
    true = xmerl_lib:is_name(Value),
    {ok,Value}.

check_IDREFS(Value) ->
    check_list_type(Value,fun check_IDREF/1).
    
check_ENTITY(Value) ->
    true = xmerl_lib:is_ncname(Value),
    {ok,Value}.

check_ENTITIES(Value) ->
    check_list_type(Value,fun check_ENTITY/1).

check_list_type(Value,BaseTypeFun) ->
    Tokens = string:tokens(Value," \n\t\r"),
    lists:foreach(BaseTypeFun,Tokens),
    {ok,Value}.

ns_whitespace(WS) when WS==16#9;WS==16#A;WS==16#D -> 
    true;
ns_whitespace(_) ->
    false.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  facet functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

facet_fun(Type,{length,V}) ->
    length_fun(Type,list_to_integer(V));
facet_fun(Type,{minLength,V}) ->
    minLength_fun(Type,list_to_integer(V));
facet_fun(Type,{maxLength,V}) ->
    maxLength_fun(Type,list_to_integer(V));
facet_fun(Type,{pattern,V}) ->
%%      fun(Val) ->
%%  	    {ok,Val}
%%      end;
    pattern_fun(Type,V);
facet_fun(Type,{enumeration,V}) ->
    enumeration_fun(Type,V);
facet_fun(Type,{whiteSpace,V}) ->
    whiteSpace_fun(Type,V);
facet_fun(Type,{maxInclusive,V}) ->
    maxInclusive_fun(Type,V);
facet_fun(Type,{maxExclusive,V}) ->
    maxExclusive_fun(Type,V);
facet_fun(Type,{minExclusive,V}) ->
    minExclusive_fun(Type,V);
facet_fun(Type,{minInclusive,V}) ->
    minInclusive_fun(Type,V);
facet_fun(Type,{totalDigits,V}) ->
    totalDigits_fun(Type,list_to_integer(V));
facet_fun(Type,{fractionDigits,V}) ->
    fractionDigits_fun(Type,list_to_integer(V));
facet_fun(Type,F) ->
    fun(_X_) ->
	    error_logger:warning_msg("~w: not valid facet on ~p ~p~n",
                                     [?MODULE,Type,F])
    end.


length_fun(T,V) 
  when T==string;T==normalizedString;T==token;
       T=='Name';T=='NCName';T==language;T=='ID';
       T=='IDREF';T=='IDREFS';T=='ENTITY';T=='ENTITIES';
       T=='NMTOKEN';T=='NMTOKENS';T==anyURI ->
    fun(Val) ->
	    case string:len(Val) == V of
		true -> {ok,Val};
		false -> {error,{length,string:len(Val),should_be,V}}
	    end
    end;
length_fun(T,_V) when T=='NOTATION';T=='QName' ->
    fun(Val) ->
	    {ok,Val}
    end;
length_fun(T,V) when T==base64Binary;T==hexBinary ->
    fun(Val) ->
	    case length(Val)==V of
		true -> {ok,Val};
		false -> {error,{length,length(Val),xhould_be,V}}
	    end
    end;
length_fun(T,_V) ->
    fun(_Val) ->
	    {error,{length_not_applicable_on,T}}
    end.

minLength_fun(T,V) 
  when T==string;T==normalizedString;T==token;
       T=='Name';T=='NCName';T==language;T=='ID';
       T=='IDREF';T=='IDREFS';T=='ENTITY';T=='ENTITIES';
       T=='NMTOKEN';T=='NMTOKENS';T==anyURI ->
    fun(Val) ->
	    case string:len(Val) >= V of
		true -> {ok,Val};
		false -> {error,{minLength,string:len(Val),should_at_least_be,V}}
	    end
    end;
minLength_fun(T,_V) when T=='NOTATION';T=='QName' ->
    fun(Val) ->
	    {ok,Val}
    end;
minLength_fun(T,V) when T==base64Binary;T==hexBinary ->
    fun(Val) ->
	    case length(Val) >= V of
		true -> {ok,Val};
		false -> {error,{minLength,length(Val),should_at_least_be,V}}
	    end
    end;
minLength_fun(T,_V) ->
    fun(_Val) ->
	    {error,{minLength_not_applicable_on,T}}
    end.

maxLength_fun(T,V) 
  when T==string;T==normalizedString;T==token;
       T=='Name';T=='NCName';T==language;T=='ID';
       T=='IDREF';T=='IDREFS';T=='ENTITY';T=='ENTITIES';
       T=='NMTOKEN';T=='NMTOKENS';T==anyURI ->
    fun(Val) ->
	    case length(Val) of
		Len when Len =< V -> {ok,Val};
		_ -> {error,{maxLength,string:len(Val),should_not_be_more_than,V}}
	    end
    end;
maxLength_fun(T,_V) when T=='NOTATION';T=='QName' ->
    fun(Val) ->
	    {ok,Val}
    end;
maxLength_fun(T,V) when T==base64Binary;T==hexBinary ->
    fun(Val) ->
	    case length(Val) =< V of
		true -> {ok,Val};
		false -> {error,{maxLength,length(Val),should_not_be_more_than,V}}
	    end
    end;
maxLength_fun(T,_V) ->
    fun(_Val) ->
	    {error,{maxLength_not_applicable_on,T}}
    end.

pattern_fun(_Type,RegExp) ->
    case xmerl_regexp:setup(RegExp) of
	{ok,RE} ->
	    fun(Val) ->
		    case xmerl_regexp:first_match(Val,RE) of
			{match,_,_} -> {ok,Val};
			_ -> {error,{pattern_mismatch,Val,RegExp}}
		    end
	    end;
	_ ->
	    fun(Val) ->
		    {error,{unsupported_pattern,Val,RegExp}}
	    end
    end.

enumeration_fun(_Type,V) ->
    fun(Val) ->
	    case lists:member(Val,V) of
		true -> {ok,Val};
		false -> {error,{enumeration,Val,should_be_one_of,V}}
	    end
    end.

whiteSpace_fun(_Type,"preserve") ->
    fun(Val) ->
	    {ok,Val}
    end;
whiteSpace_fun(_Type,"replace") ->
    fun(Val) ->
	    {ok,?MODULE:replace_ws(Val,[])}
    end;
whiteSpace_fun(_Type,"collapse") ->
    fun(Val) ->
	    {ok,?MODULE:collapse_ws(Val)}
    end.

replace_ws([16#9|T],Acc) ->
    replace_ws(T,[16#20|Acc]);
replace_ws([16#a|T],Acc) ->
    replace_ws(T,[16#20|Acc]);
replace_ws([16#d|T],Acc) ->
    replace_ws(T,[16#20|Acc]);
replace_ws([H|T],Acc) ->
    replace_ws(T,[H|Acc]);
replace_ws([],Acc) ->
    lists:reverse(Acc).

collapse_ws(Val) ->
    collapse_ws(lists:dropwhile(fun(WS) when ?is_whitespace(WS) ->true;(_) -> false end,
				replace_ws(Val,[])),[]).
collapse_ws([16#20,16#20|T],Acc) ->
    collapse_ws([16#20|T],Acc);
collapse_ws([H|T],Acc) ->
    collapse_ws(T,[H|Acc]);
collapse_ws([],Acc) ->
    lists:reverse(lists:dropwhile(fun($ ) ->true;(_) -> false end,Acc)).
    
maxInclusive_fun(T,V) 
  when T==integer;T==positiveInteger;T==negativeInteger;
       T==nonNegativeInteger;T==nonPositiveInteger;T==long;
       T==unsignedLong;T==int;T==unsignedInt;T==short;
       T==unsignedShort;T==byte;T==unsignedByte ->
    fun(Val) ->
	    case (catch list_to_integer(Val) =< list_to_integer(V)) of
		true ->
		    {ok,Val};
		_ -> 
		    {error,{maxInclusive,Val,should_be_less_than_or_equal_with,V}}
	    end
    end;
maxInclusive_fun(T,V) when T==decimal;T==float;T==double ->
    fun(Val) ->
	    case ?MODULE:compare_floats(Val,V) of
		gt ->
		    {error,{maxInclusive,Val,should_be_less_than_or_equal_with,V}};
		Err={error,_} -> Err;
		_ ->
		    {ok,Val}
	    end
    end;
maxInclusive_fun(T,V) when T==duration ->
    fun(Val) ->
	    case ?MODULE:compare_durations(Val,V) of
		GT when GT==gt;GT==indefinite ->
		    {error,{maxInclusive,Val,should_be_less_than_or_equal_with,V}};
		_ ->
		    {ok,Val}
	    end
    end;
maxInclusive_fun(T,V) when T==dateTime ->
    fun(Val) ->
	    case ?MODULE:compare_dateTime(Val,V) of
		GT when GT==gt;GT==indefinite ->
		    {error,{maxInclusive,Val,should_be_less_than_or_equal_with,V}};
		_ ->
		    {ok,Val}
	    end
    end;
maxInclusive_fun(T,_V) ->
%%   when T==duration;T==dateTime;T==date;T==time;T==gYear;T==gYearMonth;
%%        T==gMonth;T==gMonthDay;T==gDay ->
    fun(_) -> {error,{maxInclusive,not_implemented_for,T}} end.

maxExclusive_fun(T,V) 
  when T==integer;T==positiveInteger;T==negativeInteger;
       T==nonNegativeInteger;T==nonPositiveInteger;T==long;
       T==unsignedLong;T==int;T==unsignedInt;T==short;
       T==unsignedShort;T==byte;T==unsignedByte ->
    fun(Val) ->
	    case (catch list_to_integer(Val) < list_to_integer(V)) of
		true ->
		    {ok,Val};
		_ -> 
		    {error,{maxExclusive,Val,not_less_than,V}}
	    end
    end;
maxExclusive_fun(T,V) when T==decimal;T==float;T==double ->
    fun(Val) ->
	    case ?MODULE:compare_floats(Val,V) of
		lt ->
		    {ok,Val};
		Err={error,_} -> Err;
		_ ->
		    {error,{maxExclusive,Val,not_less_than,V}}
	    end
    end;
maxExclusive_fun(T,V) when T==duration ->
    fun(Val) ->
	    case ?MODULE:compare_durations(Val,V) of
		lt ->
		    {ok,Val};
		_ ->
		    {error,{maxExclusive,Val,not_less_than,V}}
	    end
    end;
maxExclusive_fun(T,V) when T==dateTime ->
    fun(Val) ->
	    case ?MODULE:compare_dateTime(Val,V) of
		lt ->
		    {ok,Val};
		_ ->
		    {error,{maxExclusive,Val,not_less_than,V}}
	    end
    end;
maxExclusive_fun(T,_V) ->
    fun(_) -> {error,{maxExclusive,not_implemented_for,T}} end.

minExclusive_fun(T,V) 
  when T==integer;T==positiveInteger;T==negativeInteger;
       T==nonNegativeInteger;T==nonPositiveInteger;T==long;
       T==unsignedLong;T==int;T==unsignedInt;T==short;
       T==unsignedShort;T==byte;T==unsignedByte ->
    fun(Val) ->
	    case (catch list_to_integer(Val) > list_to_integer(V)) of
		true ->
		    {ok,Val};
		_ -> 
		    {error,{minExclusive,Val,not_greater_than,V}}
	    end
    end;
minExclusive_fun(T,V) when T==decimal;T==float;T==double ->
    fun(Val) ->
	    case ?MODULE:compare_floats(Val,V) of
		gt ->
		    {ok,Val};
		Err={error,_} -> Err;
		_ ->
		    {error,{minExclusive,Val,not_greater_than,V}}
	    end
    end;
minExclusive_fun(T,V) when T==duration ->
    fun(Val) ->
	    case ?MODULE:compare_durations(Val,V) of
		gt ->
		    {ok,Val};
		_ ->
		    {error,{minExclusive,Val,not_greater_than,V}}
	    end
    end;
minExclusive_fun(T,V) when T==dateTime ->
    fun(Val) ->
	    case ?MODULE:compare_dateTime(Val,V) of
		gt ->
		    {ok,Val};
		_ ->
		    {error,{minExclusive,Val,not_greater_than,V}}
	    end
    end;
minExclusive_fun(T,_V) ->
    fun(_) -> {error,{minExclusive,not_implemented_for,T}} end.

minInclusive_fun(T,V) 
  when T==integer;T==positiveInteger;T==negativeInteger;
       T==nonNegativeInteger;T==nonPositiveInteger;T==long;
       T==unsignedLong;T==int;T==unsignedInt;T==short;
       T==unsignedShort;T==byte;T==unsignedByte ->
    fun(Val) ->
	    case (catch list_to_integer(Val) >= list_to_integer(V)) of
		true ->
		    {ok,Val};
		_ -> 
		    {error,{minInclusive,Val,not_greater_than_or_equal_with,V}}
	    end
    end;
minInclusive_fun(T,V) when T==decimal;T==float;T==double ->
    fun(Val) ->
	    case ?MODULE:compare_floats(Val,V) of
		lt ->
		    {error,{minInclusive,Val,not_greater_than_or_equal_with,V}};
		Err={error,_} -> Err;
		_ ->
		    {ok,Val}
	    end
    end;
minInclusive_fun(T,V) when T==duration ->
    fun(Val) ->
	    case ?MODULE:compare_durations(Val,V) of
		lt ->
		    {error,{minInclusive,Val,not_greater_than_or_equal_with,V}};
		_ ->
		    {ok,Val}
	    end
    end;
minInclusive_fun(T,V) when T==dateTime ->
    fun(Val) ->
	    case ?MODULE:compare_dateTime(Val,V) of
		lt ->
		    {error,{minInclusive,Val,not_greater_than_or_equal_with,V}};
		_ ->
		    {ok,Val}
	    end
    end;
minInclusive_fun(T,_V) ->
    fun(_) -> {error,{minInclusive,not_implemented_for,T}} end.
    
totalDigits_fun(T,V)
  when T==integer;T==positiveInteger;T==negativeInteger;T==nonNegativeInteger;
       T==nonPositiveInteger;T==long;T==unsignedLong;T==int;T==unsignedInt;
       T==short;T==unsignedShort;T==byte;T==unsignedByte;T==decimal ->
    %% Val is expressible as i * 10^-n where i and n are integers
    %% such that |i| < 10^Val and 0 =< n =< Val.
    fun(Val) ->
	    Pred = fun($0)-> true;
		      (_) -> false
		   end,
	    Val2 = lists:dropwhile(Pred,Val),
	    Length =
		case lists:member($.,Val2) of
		    true ->
			length(lists:dropwhile(Pred,lists:reverse(Val2))) -1;
		    _ -> 
			length(Val2)
		end,
	    if
		Length =< V ->
		    {ok,Val};
		true ->
		    {error,{totalDigits,Length,to_many_digits}}
	    end
    end;
totalDigits_fun(T,_V) ->
    fun(_) -> {error,{totalDigits,not_applicable,T}} end.
		     
fractionDigits_fun(T,V)
  when T==integer;T==positiveInteger;T==negativeInteger;T==nonNegativeInteger;
       T==nonPositiveInteger;T==long;T==unsignedLong;T==int;T==unsignedInt;
       T==short;T==unsignedShort;T==byte;T==unsignedByte;T==decimal ->
    fun(Val) ->	
	    Len =
		case string:tokens(Val,".") of
		    [_I,Frc] when T==decimal ->
			Pred = fun($0)-> true;
				  (_) -> false
			       end,
			length(lists:dropwhile(Pred,lists:reverse(Frc)));
		    _ ->
			0
		end,
	    if 
		Len =< V ->
		    {ok,Val};
		true ->
		    {error,{fractionDigits,Len,to_many_digits_in,Val}}
	    end
    end;
fractionDigits_fun(T,_V) ->
    fun(_) -> {error,{fractionDigits,not_applicable,T}} end.
    

%% The relation between F1 and F2 may be eq,lt or gt.
%% lt: F1 < F2
%% gt: F1 > F2
compare_floats(F1,F2) when F1=="NaN";F2=="NaN" ->
    {error,{not_comparable}};
compare_floats(F1,F1) ->
    eq;
compare_floats(F1,F2) when F1=="INF";F2=="-INF" ->
    gt;
compare_floats(F1,F2) when F1=="-INF";F2=="INF" ->
    lt;
compare_floats(Str1,Str2) ->
    F1={S1,_B1,_D1,_E1} = str_to_float(Str1),
    F2={S2,_B2,_D2,_E2} = str_to_float(Str2),
%    ?dbg("F1: ~p~nF2: ~p~n",[F1,F2]),
    if
	S1=='-',S2=='+' -> lt;
	S1=='+',S2=='-' -> gt;
%	B1<0 -> compare_floats2(F2,F1);
	true -> compare_floats2(F1,F2)
    end.
compare_floats2({S1,B1,D1,E1},{_S2,B2,D2,E2}) when B1==0;B2==0 ->
    I1 = pow(B1,D1,E1),
    I2 = pow(B2,D2,E2),
    if I1 > I2 ->
	    sign(S1,gt);
       I1 < I2 ->
	    sign(S1,lt);
       true ->
	    eq
    end;
compare_floats2({S1,B1,D1,E1},{_S2,B2,D2,E2}) ->
    %% S1 and S2 have same sign.
    I1 = pow(B1,E1),% B1 * round(math:pow(10,E1)),
    I2 = pow(B2,E2),%B2 * round(math:pow(10,E2)),
    if
	I1 > I2 -> sign(S1,gt);
	I1 < I2 -> sign(S1,lt);
	true ->
	    %% fractions are compared in lexicographic order
	    if 
		D1 == D2 -> eq;
		D1 < D2 -> sign(S1,lt);
		D1 > D2 -> sign(S1,gt)
	    end
    end.
    
str_to_float(String) ->
    {Sign,Str} =
	case String of
	    "-"++Str1 -> {'-',Str1};
	    _ -> {'+',String}
	end,
    case string:tokens(Str,".") of
	[B,DE] ->
	    case string:tokens(DE,"Ee") of
		[D,E] ->
		    %% round(math:pow(10,list_to_integer(E)))
		    {Sign,list_to_integer(B),
		     remove_trailing_zeros(D),
		     list_to_integer(E)};
		[D] ->
		    {Sign,list_to_integer(B),
		     remove_trailing_zeros(D),0}
	    end;
	[B] -> %% could also be 1E4, but no fraction
	    case string:tokens(Str,"Ee") of
		[I,E] ->
		    {Sign,list_to_integer(I),"0",list_to_integer(E)};
		_ ->
		    {Sign,list_to_integer(B),"0",0}
	    end
    end.

pow(Mantissa,Exponent) ->
    case (Mantissa * math:pow(10,Exponent)) of
	I when I<1 ->
	    I;
	I -> round(I)
    end.

pow(Mantissa,Fraction,Exponent) ->
    (Mantissa * math:pow(10,Exponent)) + 
	(list_to_integer(Fraction) * math:pow(10,Exponent-length(Fraction))).

sign('-',gt) ->
    lt;
sign('-',lt) ->
    gt;
sign(_,Rel) ->
    Rel.

remove_trailing_zeros(Str) ->
    Pred = fun($0) ->true;(_) ->false end,
    case lists:reverse(lists:dropwhile(Pred,lists:reverse(Str))) of
	[] ->
	    "0";
	Fr -> Fr
    end.


%%   when T==duration;T==dateTime;T==date;T==time;T==gYear;T==gYearMonth;
%%        T==gMonth;T==gMonthDay;T==gDay ->

%% compare_duration(V1,V2) compares V1 to V2
%% returns gt | lt | eq | indefinite 
%% ex: V1 > V2 -> gt
%%
%% V1, V2 on format PnYnMnDTnHnMnS
%% P is always present
%% T is absent iff all time items are absent
%% compare_duration(V1,V2) ->
%%     {Y1,M1,D1,H1,M1,S1} = duration_atoms(V1),
%%     {Y2,M2,D2,H2,M2,S2} = duration_atoms(V2),
%%     YearDiff = Y1 - Y2,
%%     MonthsDiff = M1 - M2,
%%     DaysDiff = D1 - D2,
compare_durations(V1,V2) ->
    %% Four reference dateTimes are used, see XMLSchema part 2,
    %% 3.2.6.2.
    %% "The order-relation of two duration values x and y is x < y iff
    %% s+x < s+y for each qualified dateTime s in the list below."
    Ref1_dateTime = {1696,9,1,0,0,0,{pos,0,0}},%1696-09-01T00:00:00Z
    Ref2_dateTime = {1697,2,1,0,0,0,{pos,0,0}},%1697-02-01T00:00:00Z
    Ref3_dateTime = {1903,3,1,0,0,0,{pos,0,0}},%1903-03-01T00:00:00Z
    Ref4_dateTime = {1903,7,1,0,0,0,{pos,0,0}},%1903-07-01T00:00:00Z
    CmpRes1=compare_dateTime(normalize_dateTime(add_duration2dateTime(Ref1_dateTime,V1)),
			     normalize_dateTime(add_duration2dateTime(Ref1_dateTime,V2))),
    CmpRes2=compare_dateTime(normalize_dateTime(add_duration2dateTime(Ref2_dateTime,V1)),
			     normalize_dateTime(add_duration2dateTime(Ref2_dateTime,V2))),
    CmpRes3=compare_dateTime(normalize_dateTime(add_duration2dateTime(Ref3_dateTime,V1)),
			     normalize_dateTime(add_duration2dateTime(Ref3_dateTime,V2))),
    CmpRes4=compare_dateTime(normalize_dateTime(add_duration2dateTime(Ref4_dateTime,V1)),
			     normalize_dateTime(add_duration2dateTime(Ref4_dateTime,V2))),
    if
	CmpRes1==CmpRes2,
	CmpRes1==CmpRes3,
	CmpRes1==CmpRes4 ->
	    CmpRes1;
	true ->  indefinite
    end.


compare_dateTime(DT1={_,_,_,_,_,_,Z},DT2={_,_,_,_,_,_,Z}) ->
    case DT1<DT2 of
	true -> lt;
	_ ->
	    case DT1>DT2 of
		true ->
		    gt;
		_ -> eq
	    end
    end;
%% If P contains a time zone and Q does not, compare as follows:
%%    1. P < Q if P < (Q with time zone +14:00)
%%    2. P > Q if P > (Q with time zone -14:00)
%%    3. P <> Q otherwise, that is, if (Q with time zone +14:00) < P <
%%    (Q with time zone -14:00)
compare_dateTime(P={_,_,_,_,_,_,{_,_,_}},_Q={Y,M,D,H,Min,S,none}) ->
    case compare_dateTime(P,normalize_dateTime({Y,M,D,H,Min,S,{pos,14,0}})) of
	lt ->
	    lt;
	_ ->
	    case compare_dateTime(P,normalize_dateTime({Y,M,D,H,Min,S,{neg,14,0}})) of
		gt ->
		    gt;
		_ ->
		    indefinite
	    end
    end;
%% If P does not contain a time zone and Q does, compare as follows:
%%    1. P < Q if (P with time zone -14:00) < Q.
%%    2. P > Q if (P with time zone +14:00) > Q.
%%    3. P <> Q otherwise, that is, if (P with time zone +14:00) < Q <
%%    (P with time zone -14:00)
compare_dateTime(_P={Y,M,D,H,Min,S,none},Q={_,_,_,_,_,_,{_,_,_}}) ->
    case compare_dateTime(normalize_dateTime({Y,M,D,H,Min,S,{neg,14,0}}),Q) of
	lt ->
	    lt;
	_ ->
	    case compare_dateTime(normalize_dateTime({Y,M,D,H,Min,S,{pos,14,0}}),Q) of
		gt ->
		    gt;
		_ ->
		    indefinite
	    end
    end;
compare_dateTime(P,Q) when is_list(P) ->
    compare_dateTime(normalize_dateTime(dateTime_atoms(P)),Q);
compare_dateTime(P,Q) when is_list(Q) ->
    compare_dateTime(P,normalize_dateTime(dateTime_atoms(Q)));
compare_dateTime(_P,_Q) ->
    indefinite.
    
fQuotient(A,B) when is_float(A) ->
    fQuotient(floor(A),B);
fQuotient(A,B) when is_float(B) ->
    fQuotient(A,floor(B));
fQuotient(A,B) when A >= 0, B >= 0 ->
    A div B;
fQuotient(A,B) when A < 0, B < 0 ->
    A div B;
fQuotient(A,B) ->
    case A rem B of
	0 ->
	    A div B;
	_ ->
	    (A div B) -1
    end.

fQuotient(A, Low, High) ->
    fQuotient(A - Low, High - Low).

floor(A) ->
    case round(A) of
	I when I > A ->
	    I - 1;
	I -> I
    end.

modulo(A,B) ->
    A - (fQuotient(A,B) * B).

modulo(A, Low, High) ->
    modulo(A - Low, High - Low) + Low.
    
maximumDayInMonthFor(YearValue, MonthValue) ->
    M = modulo(MonthValue, 1, 13),
    Y = YearValue + fQuotient(MonthValue, 1, 13),
    monthValue(M,Y).

monthValue(M,_Y) when M==1;M==3;M==5;M==7;M==8;M==10;M==12 ->
    31;
monthValue(M,_Y) when M==4;M==6;M==9;M==11 ->
    30;
monthValue(_M,Y) ->
    case modulo(Y,400) of
	0 ->
	    29;
	_ ->
	    case {modulo(Y,100) /= 0,modulo(Y,4)} of
		{true,0} ->
		    29;
		_ ->
		    28
	    end
    end.
		
%% S dateTime, D duration
%% result is E dateTime, end of time period with start S and duration
%% D. E = S + D.
add_duration2dateTime(S,D) when is_list(S),is_list(D) ->
    Satoms = dateTime_atoms(S),
    case duration_atoms(D) of
	Datoms = {_,_,_,_,_,_} ->
	    add_duration2dateTime2(Satoms,Datoms);
	Err ->
	    {error,Err}
    end;
add_duration2dateTime(S={_,_,_,_,_,_,_},D) ->
    case duration_atoms(D) of
	Datoms = {_,_,_,_,_,_} ->
	    add_duration2dateTime2(S,Datoms);
	Err ->
	    {error,Err}
    end.

add_duration2dateTime2({Syear,Smonth,Sday,Shour,Sminute,Ssec,Szone},
		       {Dyears,Dmonths,Ddays,Dhours,Dminutes,Dsecs}) ->

    %% months
    Temp1 = Smonth + Dmonths,
    Emonth = modulo(Temp1,1,13),
    Carry1 = fQuotient(Temp1,1,13),
    
    %% years
    Eyear = Syear + Dyears + Carry1,
    
    %% seconds
    Temp2 = Ssec + Dsecs,
    Esecs = modulo(Temp2,60),
    Carry2 = fQuotient(Temp2,60),
    
    %% minutes
    Temp3 = Sminute + Dminutes + Carry2,
    Eminute = modulo(Temp3,60),
    Carry3 = fQuotient(Temp3,60),
    
    %% hours
    Temp4 = Shour + Dhours + Carry3,
    Ehour = modulo(Temp4,24),
    Carry4 = fQuotient(Temp4,24),
    
    %% days
    TempDays =
	case maximumDayInMonthFor(Eyear,Emonth) of
	    MaxDay when Sday > MaxDay ->
		MaxDay;
	    _ ->
		case Sday < 1 of
		    true ->
			1;
		    _ ->
			Sday
		end
	end,
    {Eyear2,Emonth2,Eday} =
	carry_loop(TempDays+Ddays+Carry4,Emonth,Eyear),
    {Eyear2,Emonth2,Eday,Ehour,Eminute,Esecs,Szone}.

carry_loop(Eday,Emonth,Eyear) when Eday < 1 ->
    carry_loop(Eday + maximumDayInMonthFor(Eyear,Emonth - 1),
	       modulo(Emonth - 1,1,13),
	       Eyear + fQuotient(Emonth - 1,1,13));
carry_loop(Eday,Emonth,Eyear) ->
    case maximumDayInMonthFor(Eyear,Emonth) of
	MaxD when Eday > MaxD ->
	    carry_loop(Eday - maximumDayInMonthFor(Eyear,Emonth),
		       modulo(Emonth + 1,1,13),
		       Eyear + fQuotient(Emonth+1,1,13));
	_ ->
	    {Eyear,Emonth,Eday}
    end.

%% Format: '-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
dateTime_atoms("-" ++ DT) ->
    dateTime_atoms(DT,neg);
dateTime_atoms(DT) ->
    dateTime_atoms(DT,pos).
dateTime_atoms(S,Sign) ->
    [Date,TimeZone] = string:tokens(S,"T"),
    [YY,MM,DD] = string:tokens(Date,"-"),
    {Zone,ZoneSign,[Hour,Min,Sec]} =
	case lists:reverse(TimeZone) of
	    "Z"++_ ->
		{"Z",pos,string:tokens(TimeZone,"Z:")};
	    _ ->
		ZS = zone_sign(TimeZone),
		case string:tokens(TimeZone,"-+") of
		    [Time,Z] ->
			{Z,ZS,string:tokens(Time,":")};
		    [Time] ->
			{none,ZS,string:tokens(Time,":")}
		end
	end,
    {set_sign(Sign,YY),list_to_integer(MM),list_to_integer(DD),
     list_to_integer(Hour),list_to_integer(Min),sign_sec(pos,Sec),
     zone_atoms(ZoneSign,Zone)}.

zone_sign(TimeZone) ->
    case lists:member($-,TimeZone) of
	true ->
	    neg;
	_ ->
	    pos
    end.

zone_atoms(_Sign,"Z") ->
    {pos,0,0};
zone_atoms(Sign,Zone) when is_list(Zone) ->
    case string:tokens(Zone,":") of
	[H,M] ->
	    {Sign,list_to_integer(H),list_to_integer(M)};
	_ -> none
    end;
zone_atoms(_Sign,Zone) ->
    Zone.

    
%% Format: '-'? PnYnMnDTnHnMnS
duration_atoms("-P"++Dur) ->
    duration_atoms2(Dur,neg);
duration_atoms("P"++Dur) ->
    duration_atoms2(Dur,pos);
duration_atoms(Dur) ->
    {illegal_duration,Dur}.
duration_atoms2(Dur,Sign) ->
    case lists:member($T,Dur) of
	true -> %% time atoms exists
	    case string:tokens(Dur,"T") of
		[Date,Time] ->
		    case duration_atoms_date(Date) of
			{Y,M,D} ->
			    case duration_atoms_time(Time) of
				{Hour,Min,Sec} ->
				    {set_sign(Sign,Y),set_sign(Sign,M),
				     set_sign(Sign,D),set_sign(Sign,Hour),
				     set_sign(Sign,Min),sign_sec(Sign,Sec)};
				Err ->
				    Err
			    end;
			Err ->
			    Err
		    end;
		[Time] ->
		    case duration_atoms_time(Time) of
			{Hour,Min,Sec} ->
			    {0,0,0,set_sign(Sign,Hour),set_sign(Sign,Min),
			     sign_sec(Sign,Sec)};
			Err ->
			    Err
		    end;
		Err ->
		    {illegal_duration,Err}
	    end;
	_ -> %% only date coomponents
	    {Y,M,D} = duration_atoms_date(Dur),
	    {set_sign(Sign,Y),set_sign(Sign,M),set_sign(Sign,D),0,0,0}
    end.

duration_atoms_date(Date) ->
    {Y,Date2} = get_digit(Date,$Y),
    {M,Date3} = get_digit(Date2,$M),
    {D,Rest}  = get_digit(Date3,$D),
    case Rest of
	"" -> {Y,M,D};
	Err -> {illegal_duration,Err}
    end.
duration_atoms_time(Time) ->
    {H,Time2} = get_digit(Time,$H),
    {M,Time3} = get_digit(Time2,$M),
    {S,Rest} = get_sec(Time3),
    case Rest of
	"" ->
	    {H,M,S};
	Err ->
	    {illegal_duration,Err}
    end.

get_digit(Str,Delim) ->
    get_digit(Str,Delim,[],Str).
get_digit([Delim|T],Delim,Acc,_Str) ->
    {lists:reverse(Acc),T};
get_digit([H|T],Delim,Acc,Str) when H>=$0,H=<$9 ->
    get_digit(T,Delim,[H|Acc],Str);
get_digit([],_,[],_Str) ->
    {"0",[]};
get_digit([],_,_,Str) ->
    {"0",Str};
get_digit(_,_,_,Str) ->
    %% this matches both the case when reaching another delimeter and
    %% when the string already are emptied.
    {"0",Str}.

get_sec([]) ->
    {"0",[]};
get_sec(Str) ->
    get_sec(Str,[],Str).
get_sec([H|T],Acc,Str) when H>=$0,H=<$9 ->
    get_sec(T,[H|Acc],Str);
get_sec([$.|T],Acc,Str) ->
    get_sec(T,[$.|Acc],Str);
get_sec([$S|T],Acc,_) ->
    {lists:reverse(Acc),T};
get_sec(_,_,Str) ->
    {"0",Str}.
    
	    
set_sign(pos,Istr) ->
    list_to_integer(Istr);
set_sign(_,Istr) ->
    list_to_integer("-"++Istr).
sign_sec(pos,Sec) ->
    case lists:member($.,Sec) of
	true ->
	    list_to_float(Sec);
	_ ->
	    list_to_integer(Sec)
    end;
sign_sec(_,Sec) ->
    sign_sec(pos,"-"++Sec).

invert_sign(pos) ->
    neg;
invert_sign(neg) ->
    pos;
invert_sign(S) ->
    S.

normalize_dateTime({Y,M,D,Hour,Min,Sec,{Sign,ZH,ZM}}) ->
    %% minutes
    TmpMin = Min + set_sign(invert_sign(Sign),integer_to_list(ZM)),
    NMin = modulo(TmpMin,60),
    Carry1 = fQuotient(TmpMin,60),

    %% hours
    TmpHour = Hour + set_sign(invert_sign(Sign),integer_to_list(ZH)) + Carry1,
    NHour = modulo(TmpHour,24),
    Carry2 = fQuotient(TmpHour,24),
    
    {NY,NM,ND} =
	carry_loop(D+Carry2,M,Y),
    {NY,NM,ND,NHour,NMin,Sec,{pos,0,0}};
normalize_dateTime(DT) ->
    DT.

%%--------------------------------------------------------------------
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
%%-----------------------------------------------------------------
%% File: cdrlib.erl
%% 
%% Description:
%%    CDR basic type encode/decode functions
%% 
%%-----------------------------------------------------------------
-module(cdrlib).

-include_lib("orber/include/corba.hrl").
-include("orber_iiop.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([ %% IIOP 1.0 -
	  enc_short/2, dec_short/2,
	  enc_unsigned_short/2, dec_unsigned_short/2,
	  enc_long/2, dec_long/2,
	  enc_unsigned_long/2, dec_unsigned_long/2,
	  enc_bool/2, dec_bool/1,
	  enc_float/2, dec_float/2,
	  enc_double/2, dec_double/2,
	  enc_char/2, dec_char/1,
	  enc_octet/2, dec_octet/1,
	  enc_enum/3, dec_enum/3,
	  %% IIOP 1.1 - 
	  enc_longlong/2, dec_longlong/2,
	  enc_unsigned_longlong/2, dec_unsigned_longlong/2
	  %%enc_longdouble/2, dec_longdouble/2
	  %%enc_fixed/4, dec_fixed/2
	 ]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 10).

%%-----------------------------------------------------------------
%% short
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_short/2
%%-----------------------------------------------------------------
enc_short(X, Message) when is_integer(X) andalso X >= ?SHORTMIN andalso X =< ?SHORTMAX -> 
    [<<X:16/big-signed-integer>> | Message];
enc_short(X, _Message) when is_integer(X) -> 
    orber:dbg("[~p] cdrlib:enc_short(~p); Out of range.", [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 1), completion_status=?COMPLETED_NO});
enc_short(X, _Message) ->
    orber:dbg("[~p] cdrlib:enc_short(~p); not integer.", [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_short/2
%%-----------------------------------------------------------------
dec_short(big, <<Short:16/big-signed-integer,Rest/binary>>) ->
    {Short, Rest};
dec_short(little, <<Short:16/little-signed-integer,Rest/binary>>) ->
    {Short, Rest}.

%%-----------------------------------------------------------------
%% unsigned short
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_unsigned_short/2
%%-----------------------------------------------------------------
enc_unsigned_short(X, Message) when is_integer(X) andalso X >= ?USHORTMIN andalso X =< ?USHORTMAX -> 
    [<<X:16/big-unsigned-integer>> | Message];
enc_unsigned_short(X, _Message) when is_integer(X) -> 
    orber:dbg("[~p] cdrlib:enc_unsigned_short(~p); Out of range.", 
	      [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 1), completion_status=?COMPLETED_NO});
enc_unsigned_short(X, _Message) ->
    orber:dbg("[~p] cdrlib:enc_unsigned_short(~p); not integer >= 0", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_unsigned_short/2
%%-----------------------------------------------------------------
dec_unsigned_short(big, <<UShort:16/big-unsigned-integer,Rest/binary>>) ->
    {UShort, Rest};
dec_unsigned_short(little, <<UShort:16/little-unsigned-integer,Rest/binary>>) ->
    {UShort, Rest}.

%%-----------------------------------------------------------------
%% long
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_long/2
%%-----------------------------------------------------------------
enc_long(X, Message) when is_integer(X) andalso X >= ?LONGMIN andalso X =< ?LONGMAX -> 
    [<<X:32/big-signed-integer>> | Message];
enc_long(X, _Message) when is_integer(X) -> 
    orber:dbg("[~p] cdrlib:enc_long(~p); Out of range.",[?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 1), completion_status=?COMPLETED_NO});
enc_long(X, _Message) ->
    orber:dbg("[~p] cdrlib:enc_long(~p); not integer.", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_long/2
%%-----------------------------------------------------------------
dec_long(big, <<Long:32/big-signed-integer,Rest/binary>>) ->
    {Long, Rest};
dec_long(little, <<Long:32/little-signed-integer,Rest/binary>>) ->
    {Long, Rest}.

%%-----------------------------------------------------------------
%% unsigned_long
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_unsigned_long/2
%%-----------------------------------------------------------------
enc_unsigned_long(X, Message) when is_integer(X) andalso X >= ?ULONGMIN andalso X =< ?ULONGMAX ->
    [<<X:32/big-unsigned-integer>> | Message];
enc_unsigned_long(X, _Message) when is_integer(X) ->
    orber:dbg("[~p] cdrlib:enc_unsigned_long(~p); Out of range.", 
	      [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 1), completion_status=?COMPLETED_NO});
enc_unsigned_long(X, _Message) ->
    orber:dbg("[~p] cdrlib:enc_unsigned_long(~p); not integer >=0 ", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_unsigned_long/2
%%-----------------------------------------------------------------
dec_unsigned_long(big, <<ULong:32/big-unsigned-integer,Rest/binary>>) ->
    {ULong, Rest};
dec_unsigned_long(little, <<ULong:32/little-unsigned-integer,Rest/binary>>) ->
    {ULong, Rest}.

%%-----------------------------------------------------------------
%% boolean
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_bool/2
%%-----------------------------------------------------------------
enc_bool(true, Message) -> [<<1:8>>| Message];
enc_bool(false, Message) -> [<<0:8>>| Message];
enc_bool(X, _Message) ->
    orber:dbg("[~p] cdrlib:enc_bool(~p); Must be 'true' or 'false'", 
	      [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 3), completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_bool/1
%%-----------------------------------------------------------------
dec_bool(<<1:8,Rest/binary>>) -> {true, Rest};
dec_bool(<<0:8,Rest/binary>>) -> {false, Rest};
dec_bool(<<X:8,_Rest/binary>>) ->
    orber:dbg("[~p] cdrlib:dec_bool(~p); Not a boolean (1 or 0).", 
	      [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 3), completion_status=?COMPLETED_NO}).


%%-----------------------------------------------------------------
%% Func: enc_float/2
%%-----------------------------------------------------------------
enc_float(X, Message) when is_number(X) ->
    [<<X:32/big-float>> | Message];
enc_float(X, _Message) ->
    orber:dbg("[~p] cdrlib:enc_float(~p); not a number.", [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 4), completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_float/2
%%-----------------------------------------------------------------
dec_float(big, <<Float:32/big-float,Rest/binary>>) ->
    {Float, Rest};
dec_float(little, <<Float:32/little-float,Rest/binary>>) ->
    {Float, Rest}.

%%-----------------------------------------------------------------
%% Func: enc_double/2
%%-----------------------------------------------------------------
enc_double(X, Message) when is_number(X) ->
    [<<X:64/big-float>> | Message];
enc_double(X, _Message) ->
    orber:dbg("[~p] cdrlib:enc_double(~p); not a number.", [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 4), completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_double/2
%%-----------------------------------------------------------------
dec_double(big, <<Double:64/big-float,Rest/binary>>) ->
    {Double, Rest};
dec_double(little, <<Double:64/little-float,Rest/binary>>) ->
    {Double, Rest}.

%%-----------------------------------------------------------------
%% char
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_char/2
%%-----------------------------------------------------------------
enc_char(X, Message) when is_integer(X) andalso X =< 255, X >= 0 -> 
    [<<X:8>> | Message];
enc_char(X,_) -> 
    orber:dbg("[~p] cdrlib:enc_char(~p); not a char.", [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 6),completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_char/1
%%-----------------------------------------------------------------
dec_char(<<Char:8,Rest/binary>>) ->
    {Char, Rest}.

%%-----------------------------------------------------------------
%% octet
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_octet/2
%%-----------------------------------------------------------------
enc_octet(X, Message) when is_integer(X) andalso X =< 255, X >= 0 -> 
    [<<X:8/big-unsigned-integer>> | Message];
enc_octet(X, _Message) ->
    orber:dbg("[~p] cdrlib:enc_octet(~p); not an octet.", [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 6),completion_status=?COMPLETED_NO}).
   
%%-----------------------------------------------------------------
%% Func: dec_octet/1
%%-----------------------------------------------------------------
dec_octet(<<Octet:8/big-unsigned-integer,Rest/binary>>) ->
    {Octet, Rest}.

%%-----------------------------------------------------------------
%% enum
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_enum/3
%%-----------------------------------------------------------------
enc_enum(Enum, ElemList, Message) ->
    Val = getEnumValue(ElemList,Enum, 0),
    enc_unsigned_long(Val, Message).

getEnumValue([],Enum, _) -> 
    orber:dbg("[~p] cdrlib:enc_enum/enc_r_enum(~p); not exist.", 
			    [?LINE, Enum], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 5),
			   completion_status=?COMPLETED_NO});
getEnumValue([Enum|_List], Enum, N) ->
    N;
getEnumValue([_ |List], Enum, N) ->
    getEnumValue(List, Enum, N + 1).

%%-----------------------------------------------------------------
%% Func: dec_enum/2
%%-----------------------------------------------------------------
dec_enum(ByteOrder, ElemList, Message) ->
    {N, Rest}  = dec_unsigned_long(ByteOrder, Message),
    case catch lists:nth(N + 1, ElemList) of
	{'EXIT', _} ->
	    orber:dbg("[~p] cdrlib:dec_enum(~p, ~p); not defined.", 
				    [?LINE, N, ElemList], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 5),
				   completion_status=?COMPLETED_NO});
	X ->
	    {list_to_atom(X), Rest}
    end.


%%-----------------------------------------------------------------
%% IIOP 1.1 - 
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% longlong 
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_longlong/2
%%-----------------------------------------------------------------
enc_longlong(X, Message) when is_integer(X) andalso X >= ?LONGLONGMIN andalso X =< ?LONGLONGMAX -> 
    [<<X:64/big-signed-integer>> | Message];
enc_longlong(X, _Message) when is_integer(X) -> 
    orber:dbg("[~p] cdrlib:enc_longlong(~p); Out of range.", 
	      [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 1), completion_status=?COMPLETED_NO});
enc_longlong(X, _Message) ->
    orber:dbg("[~p] cdrlib:enc_longlong(~p); not integer.", 
	      [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_longlong/2
%%-----------------------------------------------------------------
dec_longlong(big, <<LongLong:64/big-signed-integer,Rest/binary>>) ->
    {LongLong, Rest};
dec_longlong(little, <<LongLong:64/little-signed-integer,Rest/binary>>) ->
    {LongLong, Rest}.

%%-----------------------------------------------------------------
%% Func: enc_unsigned_longlong/2
%%-----------------------------------------------------------------
enc_unsigned_longlong(X, Message) when is_integer(X) andalso X >= ?ULONGLONGMIN andalso X =< ?ULONGLONGMAX ->
    [<<X:64/big-unsigned-integer>> | Message];
enc_unsigned_longlong(X, _Message) when is_integer(X) ->
    orber:dbg("[~p] cdrlib:enc_unsigned_longlong(~p); Out of range.", 
	      [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 1), completion_status=?COMPLETED_NO});
enc_unsigned_longlong(X, _Message) ->
    orber:dbg("[~p] cdrlib:enc_unsigned_longlong(~p); not integer >= 0.", 
	      [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_unsigned_longlong/2
%%-----------------------------------------------------------------
dec_unsigned_longlong(big, <<ULongLong:64/big-unsigned-integer,Rest/binary>>) ->
    {ULongLong, Rest};
dec_unsigned_longlong(little, <<ULongLong:64/little-unsigned-integer,Rest/binary>>) ->
    {ULongLong, Rest}.

%%%-----------------------------------------------------------------
%%% long double [S=1 | E=15 | F=112]
%%% X = (-1)^S * 2^(E-16383) * 1.F
%%%-----------------------------------------------------------------
%-define(LONGDOUBLE_BASE, 16#10000000000000000000000000000).
%-define(LONGDOUBLE_BIAS, 16383).
%%%-----------------------------------------------------------------
%%% Func: enc_longdouble/2
%%%-----------------------------------------------------------------
%enc_longdouble(X, Message) when number(X) ->
%    {S, E, F} = enc_ieee(X, ?LONGDOUBLE_BASE, ?LONGDOUBLE_BIAS),
%    [ (S bsl 7) bor ((E bsr 8) band 16#7f),
%     E band 16#ff,
%     (F bsr 104) band 16#ff,
%     (F bsr 96) band 16#ff,
%     (F bsr 88) band 16#ff,
%     (F bsr 80) band 16#ff,
%     (F bsr 72) band 16#ff,
%     (F bsr 64) band 16#ff,
%     (F bsr 56) band 16#ff,
%     (F bsr 48) band 16#ff,
%     (F bsr 40) band 16#ff,
%     (F bsr 32) band 16#ff,
%     (F bsr 24) band 16#ff,
%     (F bsr 16) band 16#ff,
%     (F bsr 8) band 16#ff,
%     F band 16#ff | Message];
%enc_longdouble(X, Message) ->
%    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 4), completion_status=?COMPLETED_NO}).

%%%-----------------------------------------------------------------
%%% Func: dec_longdouble/2
%%%-----------------------------------------------------------------
%dec_longdouble([X15,X14,X13,X12,X11,X10,X9,X8,X7,X6,X5,X4,X3,X2,X1,X0 | R], big) ->

%    E = (X15 band 16#7f) bsl 8 + X14,

%    F = (X13 bsl 104) + (X12 bsl 96) + 
%	(X11 bsl 88) + (X10 bsl 80) + (X9 bsl 72) + 
%	(X8 bsl 64) + (X7 bsl 56) + (X6 bsl 48) + 
%	(X5 bsl 40) + (X4 bsl 32) + (X3 bsl 24) + 
%	(X2 bsl 16) + (X1 bsl 8) + X0,

%    if
%	E == 0, F == 0 -> 
%	    { 0.0, R};
%	X15 >= 16#80 ->
%	    { - math:pow(2, E-?LONGDOUBLE_BIAS) * (1 + F / ?LONGDOUBLE_BASE), R};
%	true ->
%	    { math:pow(2, E-?LONGDOUBLE_BIAS) * (1 + F / ?LONGDOUBLE_BASE), R}
%    end;
%dec_longdouble([X15,X14,X13,X12,X11,X10,X9,X8,X7,X6,X5,X4,X3,X2,X1,X0  | R], little) ->

%    E = (X0 band 16#7f) bsl 8 + X1,

%    F = 
%	(X2 bsl 104) + (X3 bsl 96) + 
%	(X4 bsl 88) + (X5 bsl 80) + (X6 bsl 72) +
%	(X7 bsl 64) + (X8 bsl 56) + (X9 bsl 48) +
%	(X10 bsl 40) + (X11 bsl 32) + (X12 bsl 24) +
%	(X13 bsl 16) + (X14 bsl 8) + X15,

%    if
%	E == 0, F == 0 -> 
%	    { 0.0, R};
%	X0 >= 16#80 ->
%	    { - math:pow(2, E-?DOUBLE_BIAS) * (1 + F / ?DOUBLE_BASE), R};
%	true ->
%	    { math:pow(2, E-?DOUBLE_BIAS) * (1 + F / ?DOUBLE_BASE), R}
%    end.

%%------------------ END OF MODULE -----------------------------------


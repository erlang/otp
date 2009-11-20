%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
%% Description: Implements base 64 encode and decode. See RFC4648.

-module(base64).

-export([encode/1, decode/1, mime_decode/1,
	 encode_to_string/1, decode_to_string/1, mime_decode_to_string/1]).

%%-------------------------------------------------------------------------
%% The following type is a subtype of string() for return values
%% of (some) functions of this module.
%%-------------------------------------------------------------------------

-type ascii_string() :: [1..255].

%%-------------------------------------------------------------------------
%% encode_to_string(ASCII) -> Base64String
%%	ASCII - string() | binary()
%%	Base64String - string()
%%                                   
%% Description: Encodes a plain ASCII string (or binary) into base64.
%%-------------------------------------------------------------------------

-spec encode_to_string(string() | binary()) -> ascii_string().

encode_to_string(Bin) when is_binary(Bin) ->
    encode_to_string(binary_to_list(Bin));
encode_to_string(List) when is_list(List) ->
    encode_l(List).

%%-------------------------------------------------------------------------
%% encode(ASCII) -> Base64
%%	ASCII - string() | binary()
%%	Base64 - binary()
%%                                   
%% Description: Encodes a plain ASCII string (or binary) into base64.
%%-------------------------------------------------------------------------

-spec encode(string() | binary()) -> binary().

encode(Bin) when is_binary(Bin) ->
    encode_binary(Bin);
encode(List) when is_list(List) ->
    list_to_binary(encode_l(List)).

-spec encode_l(string()) -> ascii_string().

encode_l([]) ->
    [];
encode_l([A]) ->
    [b64e(A bsr 2),
     b64e((A band 3) bsl 4), $=, $=];
encode_l([A,B]) ->
    [b64e(A bsr 2),
     b64e(((A band 3) bsl 4) bor (B bsr 4)), 
     b64e((B band 15) bsl 2), $=];
encode_l([A,B,C|Ls]) ->
    BB = (A bsl 16) bor (B bsl 8) bor C,
    [b64e(BB bsr 18),
     b64e((BB bsr 12) band 63), 
     b64e((BB bsr 6) band 63),
     b64e(BB band 63) | encode_l(Ls)].

encode_binary(Bin) ->
    Split = 3*(byte_size(Bin) div 3),
    <<Main0:Split/binary,Rest/binary>> = Bin,
    Main = << <<(b64e(C)):8>> || <<C:6>> <= Main0 >>,
    case Rest of
	<<A:6,B:6,C:4>> ->
	    <<Main/binary,(b64e(A)):8,(b64e(B)):8,(b64e(C bsl 2)):8,$=:8>>;
	<<A:6,B:2>> ->
	    <<Main/binary,(b64e(A)):8,(b64e(B bsl 4)):8,$=:8,$=:8>>;
	<<>> ->
	    Main
    end.

%%-------------------------------------------------------------------------
%% mime_decode(Base64) -> ASCII
%% decode(Base64) -> ASCII
%%	Base64 - string() | binary()
%%	ASCII - binary()
%%                                    
%% Description: Decodes an base64 encoded string to plain ASCII.
%% mime_decode strips away all characters not Base64 before converting,
%% whereas decode crashes if an illegal character is found
%%-------------------------------------------------------------------------

-spec decode(string() | binary()) -> binary().

decode(Bin) when is_binary(Bin) ->
    decode_binary(<<>>, Bin);
decode(List) when is_list(List) ->
    list_to_binary(decode_l(List)).

-spec mime_decode(string() | binary()) -> binary().

mime_decode(Bin) when is_binary(Bin) ->
    mime_decode_binary(<<>>, Bin);
mime_decode(List) when is_list(List) ->
    list_to_binary(mime_decode_l(List)).

-spec decode_l(string()) -> string().

decode_l(List) ->
    L = strip_spaces(List, []),
    decode(L, []).

-spec mime_decode_l(string()) -> string().

mime_decode_l(List) ->
    L = strip_illegal(List, []),
    decode(L, []).

%%-------------------------------------------------------------------------
%% mime_decode_to_string(Base64) -> ASCII
%% decode_to_string(Base64) -> ASCII
%%	Base64 - string() | binary()
%%	ASCII - binary()
%%
%% Description: Decodes an base64 encoded string to plain ASCII.
%% mime_decode strips away all characters not Base64 before converting,
%% whereas decode crashes if an illegal character is found
%%-------------------------------------------------------------------------

-spec decode_to_string(string() | binary()) -> string().

decode_to_string(Bin) when is_binary(Bin) ->
    decode_to_string(binary_to_list(Bin));
decode_to_string(List) when is_list(List) ->
    decode_l(List).

-spec mime_decode_to_string(string() | binary()) -> string().

mime_decode_to_string(Bin) when is_binary(Bin) ->
    mime_decode_to_string(binary_to_list(Bin));
mime_decode_to_string(List) when is_list(List) ->
    mime_decode_l(List).

%% One-based decode map.
-define(DECODE_MAP,
	{bad,bad,bad,bad,bad,bad,bad,bad,ws,ws,bad,bad,ws,bad,bad, %1-15
	 bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad, %16-31
	 ws,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,62,bad,bad,bad,63, %32-47
	 52,53,54,55,56,57,58,59,60,61,bad,bad,bad,eq,bad,bad, %48-63
	 bad,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,
	 15,16,17,18,19,20,21,22,23,24,25,bad,bad,bad,bad,bad,
	 bad,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
	 41,42,43,44,45,46,47,48,49,50,51,bad,bad,bad,bad,bad,
	 bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
	 bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
	 bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
	 bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
	 bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
	 bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
	 bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
	 bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad}).

decode_binary(Result0, <<C:8,T0/bits>>) ->
    case element(C, ?DECODE_MAP) of
	bad ->
	    erlang:error({badarg,C});
	ws ->
	    decode_binary(Result0, T0);
	eq ->
	    case strip_ws(T0) of
		<<$=:8,T/binary>> ->
		    <<>> = strip_ws(T),
		    Split = byte_size(Result0) - 1,
		    <<Result:Split/bytes,_:4>> = Result0,
		    Result;
		T ->
		    <<>> = strip_ws(T),
		    Split = byte_size(Result0) - 1,
		    <<Result:Split/bytes,_:2>> = Result0,
		    Result
	    end;
	Bits ->
	    decode_binary(<<Result0/bits,Bits:6>>, T0)
    end;
decode_binary(Result, <<>>) ->
    true = is_binary(Result),
    Result.

mime_decode_binary(Result, <<0:8,T/bits>>) ->
    mime_decode_binary(Result, T);
mime_decode_binary(Result0, <<C:8,T/bits>>) ->
    case element(C, ?DECODE_MAP) of
	Bits when is_integer(Bits) ->
	    mime_decode_binary(<<Result0/bits,Bits:6>>, T);
	eq ->
	    case tail_contains_equal(T) of
		true ->
		    Split = byte_size(Result0) - 1,
		    <<Result:Split/bytes,_:4>> = Result0,
		    Result;
		false ->
		    Split = byte_size(Result0) - 1,
		    <<Result:Split/bytes,_:2>> = Result0,
		    Result
	    end;
	_ ->
	    mime_decode_binary(Result0, T)
    end;
mime_decode_binary(Result, <<>>) ->
    true = is_binary(Result),
    Result.

decode([], A) -> A;
decode([$=,$=,C2,C1|Cs], A) ->
    Bits2x6 = (b64d(C1) bsl 18) bor (b64d(C2) bsl 12),
    Octet1 = Bits2x6 bsr 16,
    decode(Cs, [Octet1|A]);
decode([$=,C3,C2,C1|Cs], A) ->
    Bits3x6 = (b64d(C1) bsl 18) bor (b64d(C2) bsl 12)
	bor (b64d(C3) bsl 6),
    Octet1 = Bits3x6 bsr 16,
    Octet2 = (Bits3x6 bsr 8) band 16#ff,
    decode(Cs, [Octet1,Octet2|A]);
decode([C4,C3,C2,C1| Cs], A) ->
    Bits4x6 = (b64d(C1) bsl 18) bor (b64d(C2) bsl 12)
	bor (b64d(C3) bsl 6) bor b64d(C4),
    Octet1 = Bits4x6 bsr 16,
    Octet2 = (Bits4x6 bsr 8) band 16#ff,
    Octet3 = Bits4x6 band 16#ff,
    decode(Cs, [Octet1,Octet2,Octet3|A]).

%%%========================================================================
%%% Internal functions
%%%========================================================================

strip_spaces([], A) -> A;
strip_spaces([$\s|Cs], A) -> strip_spaces(Cs, A);
strip_spaces([$\t|Cs], A) -> strip_spaces(Cs, A);
strip_spaces([$\r|Cs], A) -> strip_spaces(Cs, A);
strip_spaces([$\n|Cs], A) -> strip_spaces(Cs, A);
strip_spaces([C|Cs], A) -> strip_spaces(Cs, [C | A]).

strip_ws(<<$\t,T/binary>>) ->
    strip_ws(T);
strip_ws(<<$\n,T/binary>>) ->
    strip_ws(T);
strip_ws(<<$\r,T/binary>>) ->
    strip_ws(T);
strip_ws(<<$\s,T/binary>>) ->
    strip_ws(T);
strip_ws(T) -> T.

strip_illegal([0|Cs], A) ->
    strip_illegal(Cs, A);
strip_illegal([C|Cs], A) ->
    case element(C, ?DECODE_MAP) of
	bad -> strip_illegal(Cs, A);
	ws -> strip_illegal(Cs, A);
	eq -> strip_illegal_end(Cs, [$=|A]);
	_ -> strip_illegal(Cs, [C|A])
    end;
strip_illegal([], A) -> A.

strip_illegal_end([0|Cs], A) ->
    strip_illegal_end(Cs, A);
strip_illegal_end([C|Cs], A) ->
    case element(C, ?DECODE_MAP) of
	bad -> strip_illegal(Cs, A);
	ws -> strip_illegal(Cs, A);
	eq -> [C|A];
	_ -> strip_illegal(Cs, [C|A])
    end;
strip_illegal_end([], A) -> A.

tail_contains_equal(<<$=,_/binary>>) -> true;
tail_contains_equal(<<_,T/binary>>) -> tail_contains_equal(T);
tail_contains_equal(<<>>) -> false.
    
%% accessors 
b64e(X) ->
    element(X+1,
	    {$A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
	     $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
	     $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
	     $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
	     $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $+, $/}).


b64d(X) ->
    b64d_ok(element(X, ?DECODE_MAP)).

b64d_ok(I) when is_integer(I) -> I.

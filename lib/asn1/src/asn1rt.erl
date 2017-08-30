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
-module(asn1rt).
-deprecated(module).

%% Runtime functions for ASN.1 (i.e encode, decode)

-export([encode/2,encode/3,decode/3,load_driver/0,unload_driver/0,info/1]).

-export([utf8_binary_to_list/1,utf8_list_to_binary/1]).

encode(Module,{Type,Term}) ->
    encode(Module,Type,Term).

encode(Module,Type,Term) ->
    case catch apply(Module,encode,[Type,Term]) of
	{'EXIT',undef} ->
	    {error,{asn1,{undef,Module,Type}}};
	Result ->
	    Result
    end.

decode(Module,Type,Bytes) ->
    case catch apply(Module,decode,[Type,Bytes]) of
	{'EXIT',undef} ->
	    {error,{asn1,{undef,Module,Type}}};
	Result ->
	    Result
    end.

%% Remove in R16A
load_driver() ->
    ok.

unload_driver() ->
    ok.

info(Module) ->
    case catch apply(Module,info,[]) of
	{'EXIT',{undef,_Reason}} ->
	    {error,{asn1,{undef,Module,info}}};
	Result ->
	    {ok,Result}
    end.

%% utf8_binary_to_list/1 transforms a utf8 encoded binary to a list of
%% unicode elements, where each element is the unicode integer value
%% of a utf8 character.
%% Bin is a utf8 encoded value. The return value is either {ok,Val} or
%% {error,Reason}. Val is a list of integers, where each integer is a
%% unicode character value.
utf8_binary_to_list(Bin) when is_binary(Bin) ->
    utf8_binary_to_list(Bin,[]).

utf8_binary_to_list(<<>>,Acc) ->
    {ok,lists:reverse(Acc)};
utf8_binary_to_list(Bin,Acc) ->
    Len = utf8_binary_len(Bin),
    case catch split_binary(Bin,Len) of
	{CharBin,RestBin} -> 
	    case utf8_binary_char(CharBin) of
		C when is_integer(C) -> 
		    utf8_binary_to_list(RestBin,[C|Acc]);
		Err -> Err
	    end;
	Err -> {error,{asn1,{bad_encoded_utf8string,Err}}}
    end.
	    
utf8_binary_len(<<0:1,_:7,_/binary>>) ->
    1;
utf8_binary_len(<<1:1,1:1,0:1,_:5,_/binary>>) ->
    2;
utf8_binary_len(<<1:1,1:1,1:1,0:1,_:4,_/binary>>) ->
    3;
utf8_binary_len(<<1:1,1:1,1:1,1:1,0:1,_:3,_/binary>>) ->
    4;
utf8_binary_len(<<1:1,1:1,1:1,1:1,1:1,0:1,_:2,_/binary>>) ->
    5;
utf8_binary_len(<<1:1,1:1,1:1,1:1,1:1,1:1,0:1,_:1,_/binary>>) ->
    6;
utf8_binary_len(Bin) ->
    {error,{asn1,{bad_utf8_length,Bin}}}.

utf8_binary_char(<<0:1,Int:7>>) ->
    Int;
utf8_binary_char(<<_:2,0:1,Int1:5,1:1,0:1,Int2:6>>) ->
    (Int1 bsl 6) bor Int2;
utf8_binary_char(<<_:3,0:1,Int1:4,1:1,0:1,Int2:6,1:1,0:1,Int3:6>>) ->
    <<Res:16>> = <<Int1:4,Int2:6,Int3:6>>,
    Res;
utf8_binary_char(<<_:4,0:1,Int1:3,Rest/binary>>) ->
    <<1:1,0:1,Int2:6,1:1,0:1,Int3:6,1:1,0:1,Int4:6>> = Rest,
    <<Res:24>> = <<0:3,Int1:3,Int2:6,Int3:6,Int4:6>>,
    Res;
utf8_binary_char(<<_:5,0:1,Int1:2,Rest/binary>>) ->
    <<1:1,0:1,Int2:6,1:1,0:1,Int3:6,1:1,0:1,Int4:6,1:1,0:1,Int5:6>> = Rest,
    <<Res:32>> = <<0:6,Int1:2,Int2:6,Int3:6,Int4:6,Int5:6>>,
    Res;
utf8_binary_char(<<_:6,0:1,I:1,Rest/binary>>) ->
    <<1:1,0:1,Int2:6,1:1,0:1,Int3:6,1:1,0:1,Int4:6,1:1,0:1,
     Int5:6,1:1,0:1,Int6:6>> = Rest,
    <<Res:32>> = <<0:1,I:1,Int2:6,Int3:6,Int4:6,Int5:6,Int6:6>>,
    Res;
utf8_binary_char(Err) ->
    {error,{asn1,{bad_utf8_character_encoding,Err}}}.


%% macros used for utf8 encoding
-define(bit1to6_into_utf8byte(I),16#80 bor (I band 16#3f)).
-define(bit7to12_into_utf8byte(I),16#80 bor ((I band 16#fc0) bsr 6)).
-define(bit13to18_into_utf8byte(I),16#80 bor ((I band 16#3f000) bsr 12)).
-define(bit19to24_into_utf8byte(I),16#80 bor ((Int band 16#fc0000) bsr 18)).
-define(bit25to30_into_utf8byte(I),16#80 bor ((Int band 16#3f000000) bsr 24)).

%% utf8_list_to_binary/1 transforms a list of integers to a
%% binary. Each element in the input list has the unicode (integer)
%% value of an utf8 character. 
%% The return value is either {ok,Bin} or {error,Reason}. The
%% resulting binary is utf8 encoded.
utf8_list_to_binary(List) ->
    utf8_list_to_binary(List,[]).

utf8_list_to_binary([],Acc) when is_list(Acc) ->
    {ok,list_to_binary(lists:reverse(Acc))};
utf8_list_to_binary([],Acc) ->
    {error,{asn1,Acc}};
utf8_list_to_binary([H|T],Acc) ->
    case catch utf8_encode(H,Acc) of
	NewAcc when is_list(NewAcc) -> 
	    utf8_list_to_binary(T,NewAcc);
	Err -> Err
    end.


utf8_encode(Int,Acc) when Int < 128 ->
    %% range 16#00000000 - 16#0000007f
    %% utf8 encoding: 0xxxxxxx
    [Int|Acc];
utf8_encode(Int,Acc) when Int < 16#800 ->
    %% range 16#00000080 - 16#000007ff
    %% utf8 encoding: 110xxxxx 10xxxxxx
    [?bit1to6_into_utf8byte(Int),16#c0 bor (Int bsr 6)|Acc];
utf8_encode(Int,Acc) when Int < 16#10000 ->
    %% range 16#00000800 - 16#0000ffff
    %% utf8 encoding: 1110xxxx 10xxxxxx 10xxxxxx
    [?bit1to6_into_utf8byte(Int),?bit7to12_into_utf8byte(Int),
     16#e0 bor ((Int band 16#f000) bsr 12)|Acc];
utf8_encode(Int,Acc) when Int < 16#200000 ->
    %% range 16#00010000 - 16#001fffff
    %% utf8 encoding: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    [?bit1to6_into_utf8byte(Int),?bit7to12_into_utf8byte(Int),
     ?bit13to18_into_utf8byte(Int),
     16#f0 bor ((Int band 16#1c0000) bsr 18)|Acc];
utf8_encode(Int,Acc) when Int < 16#4000000 ->
    %% range 16#00200000 - 16#03ffffff
    %% utf8 encoding: 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
    [?bit1to6_into_utf8byte(Int),?bit7to12_into_utf8byte(Int),
     ?bit13to18_into_utf8byte(Int),?bit19to24_into_utf8byte(Int),
     16#f8 bor ((Int band 16#3000000) bsr 24)|Acc];
utf8_encode(Int,Acc) ->
    %% range 16#04000000 - 16#7fffffff
    %% utf8 encoding: 1111110x 10xxxxxx ...(total 6 bytes) 10xxxxxx
    [?bit1to6_into_utf8byte(Int),?bit7to12_into_utf8byte(Int),
     ?bit13to18_into_utf8byte(Int),?bit19to24_into_utf8byte(Int),
     ?bit25to30_into_utf8byte(Int),
     16#fc bor ((Int band 16#40000000) bsr 30)|Acc].

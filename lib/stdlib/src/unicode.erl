%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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
-module(unicode).

-export([characters_to_list/1, characters_to_list_int/2,
	 characters_to_binary/1, characters_to_binary_int/2,
	 characters_to_binary/3,
	 bom_to_encoding/1, encoding_to_bom/1,
         characters_to_nfd_list/1, characters_to_nfd_binary/1,
         characters_to_nfc_list/1, characters_to_nfc_binary/1,
         characters_to_nfkd_list/1, characters_to_nfkd_binary/1,
         characters_to_nfkc_list/1, characters_to_nfkc_binary/1
        ]).

-export_type([chardata/0, charlist/0, encoding/0, external_chardata/0,
              external_charlist/0, latin1_char/0, latin1_chardata/0,
              latin1_charlist/0, latin1_binary/0, unicode_binary/0]).

-type encoding()  :: 'latin1' | 'unicode' | 'utf8'
                   | 'utf16' | {'utf16', endian()}
                   | 'utf32' | {'utf32', endian()}.
-type endian()    :: 'big' | 'little'.
-type unicode_binary() :: binary().
-type charlist() ::
        maybe_improper_list(char() | unicode_binary() | charlist(),
                            unicode_binary() | nil()).
-type chardata() :: charlist() | unicode_binary().
-type external_unicode_binary() :: binary().
-type external_chardata() :: external_charlist() | external_unicode_binary().
-type external_charlist() ::
        maybe_improper_list(char() |
                              external_unicode_binary() |
                              external_charlist(),
                            external_unicode_binary() | nil()).
-type latin1_binary() :: binary().
-type latin1_char() :: byte().
-type latin1_chardata() :: latin1_charlist() | latin1_binary().
-type latin1_charlist() ::
        maybe_improper_list(latin1_char() |
                              latin1_binary() |
                              latin1_charlist(),
                            latin1_binary() | nil()).

%%% BIFs
%%%
%%% characters_to_binary/2 (will trap to characters_to_binary_int/2
%%%                         if InEncoding is not {latin1 | unicode | utf8})
%%% characters_to_list/2   (will trap to characters_to_list_int/2 if
%%%                         InEncoding is not {latin1 | unicode | utf8})

-export([bin_is_7bit/1, characters_to_binary/2, characters_to_list/2]).

-spec bin_is_7bit(Binary) -> boolean() when
      Binary :: binary().

bin_is_7bit(_) ->
    erlang:nif_error(undef).

-spec characters_to_binary(Data, InEncoding) -> Result when
      Data :: latin1_chardata() | chardata() | external_chardata(),
      InEncoding :: encoding(),
      Result :: binary()
              | {error, binary(), RestData}
              | {incomplete, binary(), binary()},
      RestData :: latin1_chardata() | chardata() | external_chardata().

characters_to_binary(_, _) ->
    erlang:nif_error(undef).

-spec characters_to_list(Data,  InEncoding) -> Result when
      Data :: latin1_chardata() | chardata() | external_chardata(),
      InEncoding :: encoding(),
      Result :: list()
              | {error, list(), RestData}
              | {incomplete, list(), binary()},
      RestData :: latin1_chardata() | chardata() | external_chardata().

characters_to_list(_, _) ->
    erlang:nif_error(undef).

%%% End of BIFs

-spec characters_to_list(Data) -> Result when
      Data :: latin1_chardata() | chardata() | external_chardata(),
      Result :: list()
              | {error, list(), RestData}
              | {incomplete, list(), binary()},
      RestData :: latin1_chardata() | chardata() | external_chardata().

characters_to_list(ML) ->
    unicode:characters_to_list(ML,unicode).

-spec characters_to_binary(Data) -> Result when
      Data :: latin1_chardata() | chardata() | external_chardata(),
      Result :: binary()
              | {error, binary(), RestData}
              | {incomplete, binary(), binary()},
      RestData :: latin1_chardata() | chardata() | external_chardata().

characters_to_binary(ML) ->
    try
	unicode:characters_to_binary(ML,unicode)
    catch
	error:AnyError ->
	    TheError = case AnyError of
			   system_limit ->
			       system_limit;
			   _ ->
			       badarg
		       end,
	    {'EXIT',{new_stacktrace,[{Mod,_,L,_}|Rest]}} =
		(catch erlang:error(new_stacktrace,
				    [ML])),
	    erlang:raise(error,TheError,[{Mod,characters_to_binary,L}|Rest])
    end.

-spec characters_to_binary(Data, InEncoding, OutEncoding) -> Result when
      Data :: latin1_chardata() | chardata() | external_chardata(),
      InEncoding :: encoding(),
      OutEncoding :: encoding(),
      Result :: binary()
              | {error, binary(), RestData}
              | {incomplete, binary(), binary()},
      RestData :: latin1_chardata() | chardata() | external_chardata().

characters_to_binary(ML, latin1, latin1) when is_binary(ML) ->
    ML;
characters_to_binary(ML, latin1, Uni) when is_binary(ML) and ((Uni =:= utf8) or   (Uni =:= unicode)) ->
    case unicode:bin_is_7bit(ML) of
	true ->
	    ML;
	false ->
	        try
		    characters_to_binary_int(ML,latin1,utf8)
		catch
		    error:AnyError ->
			TheError = case AnyError of
				       system_limit ->
					   system_limit;
				       _ ->
					   badarg
				   end,
			{'EXIT',{new_stacktrace,[{Mod,_,L,_}|Rest]}} =
			    (catch erlang:error(new_stacktrace,
						[ML,latin1,Uni])),
			erlang:raise(error,TheError,
				     [{Mod,characters_to_binary,L}|Rest])
		end
    end;
characters_to_binary(ML,Uni,latin1) when is_binary(ML) and ((Uni =:= utf8) or   (Uni =:= unicode)) ->
    case unicode:bin_is_7bit(ML) of
	true ->
	    ML;
	false ->
	        try
		    characters_to_binary_int(ML,utf8,latin1)
		catch
		    error:AnyError ->
			TheError = case AnyError of
				       system_limit ->
					   system_limit;
				       _ ->
					   badarg
				   end,
			{'EXIT',{new_stacktrace,[{Mod,_,L,_}|Rest]}} =
			    (catch erlang:error(new_stacktrace,
						[ML,Uni,latin1])),
			erlang:raise(error,TheError,
				     [{Mod,characters_to_binary,L}|Rest])
		end
    end;

characters_to_binary(ML, InEncoding, OutEncoding) ->
    try
	characters_to_binary_int(ML,InEncoding,OutEncoding)
    catch
	error:AnyError ->
	    TheError = case AnyError of
			   system_limit ->
			       system_limit;
			   _ ->
			       badarg
		       end,
	    {'EXIT',{new_stacktrace,[{Mod,_,L,_}|Rest]}} =
		(catch erlang:error(new_stacktrace,
				    [ML,InEncoding,OutEncoding])),
	    erlang:raise(error,TheError,[{Mod,characters_to_binary,L}|Rest])
    end.

-spec bom_to_encoding(Bin) -> {Encoding, Length} when
      Bin :: binary(),
      Encoding ::  'latin1' | 'utf8'
                 | {'utf16', endian()}
                 | {'utf32', endian()},
      Length :: non_neg_integer().

bom_to_encoding(<<239,187,191,_/binary>>) ->
    {utf8,3};
bom_to_encoding(<<0,0,254,255,_/binary>>) ->
    {{utf32,big},4};
bom_to_encoding(<<255,254,0,0,_/binary>>) ->
    {{utf32,little},4};
bom_to_encoding(<<254,255,_/binary>>) ->
    {{utf16,big},2};
bom_to_encoding(<<255,254,_/binary>>) ->
    {{utf16,little},2};
bom_to_encoding(Bin) when is_binary(Bin) ->
    {latin1,0}.

-spec encoding_to_bom(InEncoding) -> Bin when
      Bin :: binary(),
      InEncoding :: encoding().

encoding_to_bom(unicode) ->
    <<239,187,191>>;
encoding_to_bom(utf8) ->
    <<239,187,191>>;
encoding_to_bom(utf16) ->
    <<254,255>>;
encoding_to_bom({utf16,big}) ->
    <<254,255>>;
encoding_to_bom({utf16,little}) ->
    <<255,254>>;
encoding_to_bom(utf32) ->
    <<0,0,254,255>>;
encoding_to_bom({utf32,big}) ->
    <<0,0,254,255>>;
encoding_to_bom({utf32,little}) ->
    <<255,254,0,0>>;
encoding_to_bom(latin1) ->
    <<>>.

-define(GC_N, 200). %% arbitrary number

%% Canonical decompose string to list of chars
-spec characters_to_nfd_list(chardata()) -> [char()] | {error, [char()], chardata()}.
characters_to_nfd_list(CD) ->
    characters_to_nfd_list(CD, []).
characters_to_nfd_list(CD, Acc) ->
    case unicode_util:nfd(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfd_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfd_list(Str, [CP | Acc]);
        [] -> lists:reverse(Acc);
        {error,Error} -> {error, lists:reverse(Acc), Error}
    end.

-spec characters_to_nfd_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
characters_to_nfd_binary(CD) ->
    characters_to_nfd_binary(CD, ?GC_N, [], []).

characters_to_nfd_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfd(CD) of
        [GC|Str] -> characters_to_nfd_binary(Str, N-1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfd_binary(CD, _, Row, Acc) ->
    characters_to_nfd_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).

%% Compability Canonical decompose string to list of chars.
-spec characters_to_nfkd_list(chardata()) -> [char()] | {error, [char()], chardata()}.
characters_to_nfkd_list(CD) ->
    characters_to_nfkd_list(CD, []).
characters_to_nfkd_list(CD, Acc) ->
    case unicode_util:nfkd(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfkd_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfkd_list(Str, [CP | Acc]);
        [] -> lists:reverse(Acc);
        {error,Error} -> {error, lists:reverse(Acc), Error}
    end.

-spec characters_to_nfkd_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
characters_to_nfkd_binary(CD) ->
    characters_to_nfkd_binary(CD, ?GC_N, [], []).

characters_to_nfkd_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfkd(CD) of
        [GC|Str] -> characters_to_nfkd_binary(Str, N-1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfkd_binary(CD, _, Row, Acc) ->
    characters_to_nfkd_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).


%% Canonical compose string to list of chars
-spec characters_to_nfc_list(chardata()) -> [char()] | {error, [char()], chardata()}.
characters_to_nfc_list(CD) ->
    characters_to_nfc_list(CD, []).
characters_to_nfc_list(CD, Acc) ->
    case unicode_util:nfc(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfc_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfc_list(Str, [CP | Acc]);
        [] -> lists:reverse(Acc);
        {error,Error} -> {error, lists:reverse(Acc), Error}
    end.

-spec characters_to_nfc_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
characters_to_nfc_binary(CD) ->
    characters_to_nfc_binary(CD, ?GC_N, [], []).

characters_to_nfc_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfc(CD) of
        [GC|Str] -> characters_to_nfc_binary(Str, N-1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfc_binary(CD, _, Row, Acc) ->
    characters_to_nfc_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).

%% Compability Canonical compose string to list of chars
-spec characters_to_nfkc_list(chardata()) -> [char()] | {error, [char()], chardata()}.
characters_to_nfkc_list(CD) ->
    characters_to_nfkc_list(CD, []).
characters_to_nfkc_list(CD, Acc) ->
    case unicode_util:nfkc(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfkc_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfkc_list(Str, [CP | Acc]);
        [] -> lists:reverse(Acc);
        {error,Error} -> {error, lists:reverse(Acc), Error}
    end.

-spec characters_to_nfkc_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
characters_to_nfkc_binary(CD) ->
    characters_to_nfkc_binary(CD, ?GC_N, [], []).

characters_to_nfkc_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfkc(CD) of
        [GC|Str] -> characters_to_nfkc_binary(Str, N-1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfkc_binary(CD, _, Row, Acc) ->
    characters_to_nfkc_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).

acc_to_binary(Acc) ->
    list_to_binary(lists:reverse(Acc)).
prepend_row_to_acc(Row, Acc) ->
    [characters_to_binary(lists:reverse(Row))|Acc].

%% internals

characters_to_list_int(ML, Encoding) ->
    try
	do_characters_to_list(ML,Encoding)
    catch
	error:AnyError ->
	    TheError = case AnyError of
			   system_limit ->
			       system_limit;
			   _ ->
			       badarg
		       end,
	    {'EXIT',{new_stacktrace,[{Mod,_,L,_}|Rest]}} =
		(catch erlang:error(new_stacktrace,
				    [ML,Encoding])),
	    erlang:raise(error,TheError,[{Mod,characters_to_list,L}|Rest])
    end.

                                                % XXX: Optimize me!
do_characters_to_list(ML, Encoding) ->
    case unicode:characters_to_binary(ML,Encoding) of
	Bin when is_binary(Bin) ->
	    unicode:characters_to_list(Bin,utf8);
	{error,Encoded,Rest} ->
	    {error,unicode:characters_to_list(Encoded,utf8),Rest};
	{incomplete, Encoded2, Rest2} ->
	    {incomplete,unicode:characters_to_list(Encoded2,utf8),Rest2}
    end.


characters_to_binary_int(ML,InEncoding) ->
    try
	characters_to_binary_int(ML,InEncoding,unicode)
    catch
	error:AnyError ->
	    TheError = case AnyError of
			   system_limit ->
			       system_limit;
			   _ ->
			       badarg
		       end,
	    {'EXIT',{new_stacktrace,[{Mod,_,L,_}|Rest]}} =
		(catch erlang:error(new_stacktrace,
				    [ML,InEncoding])),
	    erlang:raise(error,TheError,[{Mod,characters_to_binary,L}|Rest])
    end.


characters_to_binary_int(ML, InEncoding, OutEncoding) when
      InEncoding =:= latin1, OutEncoding =:= unicode;
      InEncoding =:= latin1, OutEncoding =:= utf8;
      InEncoding =:= unicode, OutEncoding =:= unicode;
      InEncoding =:= unicode, OutEncoding =:= utf8;
      InEncoding =:= utf8, OutEncoding =:= unicode;
      InEncoding =:= utf8, OutEncoding =:= utf8 ->
    unicode:characters_to_binary(ML,InEncoding);

characters_to_binary_int(ML, InEncoding, OutEncoding) ->
    {InTrans,Limit} = case OutEncoding of
                          latin1 -> {i_trans_chk(InEncoding),255};
                          _ -> {i_trans(InEncoding),case InEncoding of latin1 -> 255; _ -> 16#10FFFF end}
                      end,
    OutTrans = o_trans(OutEncoding),
    Res =
	ml_map(ML,
	       fun(Part,Accum) when is_binary(Part) ->
		       case InTrans(Part) of
			   List when is_list(List) ->
			       Tail = OutTrans(List),
			       <<Accum/binary, Tail/binary>>;
			   {error, Translated, Rest} ->
			       Tail = OutTrans(Translated),
			       {error, <<Accum/binary,Tail/binary>>, Rest};
			   {incomplete, Translated, Rest, Missing}  ->
			       Tail = OutTrans(Translated),
			       {incomplete, <<Accum/binary,Tail/binary>>, Rest,
				Missing}
		       end;
		  (Part, Accum) when is_integer(Part), Part =< Limit ->
		       case OutTrans([Part]) of
			   Binary when is_binary(Binary) ->
			       <<Accum/binary, Binary/binary>>;
			   {error, _, [Part]} ->
			       {error,Accum,[Part]}
		       end;
		  (Part, Accum) ->
		       {error, Accum, [Part]}
	       end,<<>>),
    case Res of
	{incomplete,A,B,_} ->
	    {incomplete,A,B};
	_ ->
	    Res
    end.


cbv(utf8,<<1:1,1:1,0:1,_:5>>) ->
    1;
cbv(utf8,<<1:1,1:1,1:1,0:1,_:4,R/binary>>) ->
    case R of
	<<>> ->
	    2;
	<<1:1,0:1,_:6>> ->
	    1;
	_ ->
	    false
    end;
cbv(utf8,<<1:1,1:1,1:1,1:1,0:1,_:3,R/binary>>) ->
    case R of
	<<>> ->
	    3;
	<<1:1,0:1,_:6>> ->
	    2;
	<<1:1,0:1,_:6,1:1,0:1,_:6>> ->
	    1;
	_ ->
	    false
    end;
cbv(utf8,_) ->
    false;

cbv({utf16,big},<<A:8>>) when A =< 215; A >= 224 ->
    1;
cbv({utf16,big},<<54:6,_:2>>) ->
    3;
cbv({utf16,big},<<54:6,_:10>>) ->
    2;
cbv({utf16,big},<<54:6,_:10,55:6,_:2>>) ->
    1;
cbv({utf16,big},_) ->
    false;
cbv({utf16,little},<<_:8>>) ->
    1; % or 3, we'll see
cbv({utf16,little},<<_:8,54:6,_:2>>) ->
    2;
cbv({utf16,little},<<_:8,54:6,_:2,_:8>>) ->
    1;
cbv({utf16,little},_) ->
    false;


cbv({utf32,big}, <<0:8>>) ->
    3;
cbv({utf32,big}, <<0:8,X:8>>) when X =< 16 ->
    2;
cbv({utf32,big}, <<0:8,X:8,Y:8>>)
  when X =< 16, ((X > 0) or ((Y =< 215) or (Y >= 224))) ->
    1;
cbv({utf32,big},_) ->
    false;
cbv({utf32,little},<<_:8>>) ->
    3;
cbv({utf32,little},<<_:8,_:8>>) ->
    2;
cbv({utf32,little},<<X:8,255:8,0:8>>) when X =:= 254; X =:= 255 ->
    false;
cbv({utf32,little},<<_:8,Y:8,X:8>>)
  when X =< 16, ((X > 0) or ((Y =< 215) or (Y >= 224))) ->
    1;
cbv({utf32,little},_) ->
    false.


ml_map([],_,{{Inc,X},Accum}) ->
    {incomplete, Accum, Inc, X};
ml_map([],_Fun,Accum) ->
    Accum;
ml_map([Part|_] = Whole,_,{{Incomplete, _}, Accum}) when is_integer(Part) ->
    {error, Accum, [Incomplete | Whole]};
ml_map([Part|T],Fun,Accum) when is_integer(Part) ->
    case Fun(Part,Accum) of
	Bin when is_binary(Bin) ->
	    case ml_map(T,Fun,Bin) of
		Bin2 when is_binary(Bin2) ->
		    Bin2;
		{error, Converted, Rest} ->
		    {error, Converted, Rest};
		{incomplete, Converted, Rest,X} ->
		    {incomplete, Converted, Rest,X}
	    end;
	% Can not be incomplete - it's an integer
	{error, Converted, Rest} ->
	    {error, Converted, [Rest|T]}
    end;
ml_map([Part|T],Fun,{{Incomplete,Missing}, Accum}) when is_binary(Part) ->
    % Ok, how much is needed to fill in the incomplete part?
    case byte_size(Part) of
	N when N >= Missing ->
	    <<FillIn:Missing/binary,Trailing/binary>> = Part,
	    NewPart = <<Incomplete/binary,FillIn/binary>>,
	    ml_map([NewPart,Trailing|T], Fun, Accum);
	M ->
	    NewIncomplete = <<Incomplete/binary, Part/binary>>,
	    NewMissing = Missing - M,
	    ml_map(T,Fun,{{NewIncomplete, NewMissing}, Accum})
    end;
ml_map([Part|T],Fun,Accum) when is_binary(Part), byte_size(Part) > 8192 ->
    <<Part1:8192/binary,Part2/binary>> = Part,
    ml_map([Part1,Part2|T],Fun,Accum);
ml_map([Part|T],Fun,Accum) when is_binary(Part) ->
    case Fun(Part,Accum) of
	Bin when is_binary(Bin) ->
	    ml_map(T,Fun,Bin);
	{incomplete, Converted, Rest, Missing} ->
	    ml_map(T,Fun,{{Rest, Missing},Converted});
	{error, Converted, Rest} ->
	    {error, Converted, [Rest|T]}
    end;
ml_map([List|T],Fun,Accum) when is_list(List) ->
    case ml_map(List,Fun,Accum) of
	Bin when is_binary(Bin) ->
	    ml_map(T,Fun,Bin);
	{error, Converted,Rest} ->
	    {error, Converted, [Rest | T]};
	{incomplete, Converted,Rest,N} ->
	    ml_map(T,Fun,{{Rest,N},Converted})
    end;
ml_map(Bin,Fun,{{Incomplete,Missing},Accum}) when is_binary(Bin) ->
    case byte_size(Bin) of
	N when N >= Missing ->
	    ml_map([Incomplete,Bin],Fun,Accum);
	M ->
	    {incomplete, Accum, <<Incomplete/binary, Bin/binary>>, Missing - M}
    end;
ml_map(Part,Fun,Accum) when is_binary(Part), byte_size(Part) > 8192 ->
     <<Part1:8192/binary,Part2/binary>> = Part,
    ml_map([Part1,Part2],Fun,Accum);
ml_map(Bin,Fun,Accum) when is_binary(Bin) ->
    Fun(Bin,Accum).





i_trans(latin1) ->
    fun(Bin) -> binary_to_list(Bin) end;
i_trans(unicode) ->
    i_trans(utf8);
i_trans(utf8) ->
    fun do_i_utf8/1;
i_trans(utf16) ->
    fun do_i_utf16_big/1;
i_trans({utf16,big}) ->
    fun do_i_utf16_big/1;
i_trans({utf16,little}) ->
    fun do_i_utf16_little/1;
i_trans(utf32) ->
    fun do_i_utf32_big/1;
i_trans({utf32,big}) ->
    fun do_i_utf32_big/1;
i_trans({utf32,little}) ->
    fun do_i_utf32_little/1.

i_trans_chk(latin1) ->
    fun(Bin) -> binary_to_list(Bin) end;
i_trans_chk(unicode) ->
    i_trans_chk(utf8);
i_trans_chk(utf8) ->
    fun do_i_utf8_chk/1;
i_trans_chk(utf16) ->
    fun do_i_utf16_big_chk/1;
i_trans_chk({utf16,big}) ->
    fun do_i_utf16_big_chk/1;
i_trans_chk({utf16,little}) ->
    fun do_i_utf16_little_chk/1;
i_trans_chk(utf32) ->
    fun do_i_utf32_big_chk/1;
i_trans_chk({utf32,big}) ->
    fun do_i_utf32_big_chk/1;
i_trans_chk({utf32,little}) ->
    fun do_i_utf32_little_chk/1.

o_trans(latin1) ->
    fun(L) -> list_to_binary(L) end;
o_trans(unicode) ->
    o_trans(utf8);
o_trans(utf8) ->
    fun(L) ->
	    do_o_binary(fun(One) ->
				<<One/utf8>>
			end, L)
    end;

o_trans(utf16) ->
    fun(L) ->
	    do_o_binary(fun(One) ->
				<<One/utf16>>
			end, L)
    end;
o_trans({utf16,big}) ->
    o_trans(utf16);
o_trans({utf16,little}) ->
    fun(L) ->
	    do_o_binary(fun(One) ->
				<<One/utf16-little>>
			end, L)
    end;
o_trans(utf32) ->
    fun(L) ->
	    do_o_binary(fun(One) ->
				<<One/utf32>>
			end, L)
    end;
o_trans({utf32,big}) ->
    o_trans(utf32);
o_trans({utf32,little}) ->
    fun(L) ->
	    do_o_binary(fun(One) ->
				<<One/utf32-little>>
			end, L)
    end.

do_o_binary(F,L) ->
    case do_o_binary2(F,L) of
	{Tag,List,R} ->
	    {Tag,erlang:iolist_to_binary(List),R};
	List ->
	    erlang:iolist_to_binary(List)
    end.

-dialyzer({no_improper_lists, do_o_binary2/2}).

do_o_binary2(_F,[]) ->
    <<>>;
do_o_binary2(F,[H|T]) ->
    case (catch F(H)) of
	{'EXIT',_} ->
	    {error,<<>>,[H|T]};
	Bin when is_binary(Bin) ->
	    case do_o_binary2(F,T) of
		{error,Bin2,Rest} ->
		    {error,[Bin|Bin2],Rest};
		Bin3 ->
		    [Bin|Bin3]
	    end
    end.

%% Specific functions only allowing codepoints in latin1 range

do_i_utf8_chk(<<>>) ->
    [];
do_i_utf8_chk(<<U/utf8,R/binary>>) when U =< 255 ->
    case do_i_utf8_chk(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf8_chk(<<_/utf8,_/binary>> = Bin) ->
    {error, [], Bin};
do_i_utf8_chk(Bin) when is_binary(Bin) ->
    case cbv(utf8,Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin,N};
	false ->
	    {error, [], Bin}
    end.
do_i_utf16_big_chk(<<>>) ->
    [];
do_i_utf16_big_chk(<<U/utf16,R/binary>>) when U =< 255 ->
    case do_i_utf16_big_chk(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf16_big_chk(<<_/utf16,_/binary>> = Bin) ->
    {error, [], Bin};
do_i_utf16_big_chk(Bin) when is_binary(Bin) ->
    case cbv({utf16,big},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.
do_i_utf16_little_chk(<<>>) ->
    [];
do_i_utf16_little_chk(<<U/utf16-little,R/binary>>) when U =< 255 ->
    case do_i_utf16_little_chk(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf16_little_chk(<<_/utf16-little,_/binary>> = Bin) ->
    {error, [], Bin};
do_i_utf16_little_chk(Bin) when is_binary(Bin) ->
    case cbv({utf16,little},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.


do_i_utf32_big_chk(<<>>) ->
    [];
do_i_utf32_big_chk(<<U/utf32,R/binary>>) when U =< 255 ->
    case do_i_utf32_big_chk(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf32_big_chk(<<_/utf32,_/binary>> = Bin) ->
    {error, [], Bin};
do_i_utf32_big_chk(Bin) when is_binary(Bin) ->
    case cbv({utf32,big},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.
do_i_utf32_little_chk(<<>>) ->
    [];
do_i_utf32_little_chk(<<U/utf32-little,R/binary>>) when U =< 255 ->
    case do_i_utf32_little_chk(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf32_little_chk(<<_/utf32-little,_/binary>> = Bin) ->
    {error, [], Bin};
do_i_utf32_little_chk(Bin) when is_binary(Bin) ->
    case cbv({utf32,little},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.


%% General versions

do_i_utf8(<<>>) ->
    [];
do_i_utf8(<<U/utf8,R/binary>>) ->
    case do_i_utf8(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf8(Bin) when is_binary(Bin) ->
    case cbv(utf8,Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin,N};
	false ->
	    {error, [], Bin}
    end.

do_i_utf16_big(<<>>) ->
    [];
do_i_utf16_big(<<U/utf16,R/binary>>) ->
    case do_i_utf16_big(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf16_big(Bin) when is_binary(Bin) ->
    case cbv({utf16,big},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.
do_i_utf16_little(<<>>) ->
    [];
do_i_utf16_little(<<U/utf16-little,R/binary>>) ->
    case do_i_utf16_little(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf16_little(Bin) when is_binary(Bin) ->
    case cbv({utf16,little},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.


do_i_utf32_big(<<>>) ->
    [];
do_i_utf32_big(<<U/utf32,R/binary>>) ->
    case do_i_utf32_big(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf32_big(Bin) when is_binary(Bin) ->
    case cbv({utf32,big},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.
do_i_utf32_little(<<>>) ->
    [];
do_i_utf32_little(<<U/utf32-little,R/binary>>) ->
    case do_i_utf32_little(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf32_little(Bin) when is_binary(Bin) ->
    case cbv({utf32,little},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.

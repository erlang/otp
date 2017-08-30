%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(string).

-export([len/1,equal/2,concat/2,chr/2,rchr/2,str/2,rstr/2,
	 span/2,cspan/2,substr/2,substr/3,tokens/2,chars/2,chars/3]).
-export([copies/2,words/1,words/2,strip/1,strip/2,strip/3,
	 sub_word/2,sub_word/3,left/2,left/3,right/2,right/3,
	 sub_string/2,sub_string/3,centre/2,centre/3, join/2]).
-export([to_upper/1, to_lower/1]).

-import(lists,[reverse/1,member/2]).

%%---------------------------------------------------------------------------

%%% BIFs

-export([to_float/1, to_integer/1]).

-spec to_float(String) -> {Float, Rest} | {error, Reason} when
      String :: string(),
      Float :: float(),
      Rest :: string(),
      Reason :: no_float | not_a_list.

to_float(_) ->
    erlang:nif_error(undef).

-spec to_integer(String) -> {Int, Rest} | {error, Reason} when
      String :: string(),
      Int :: integer(),
      Rest :: string(),
      Reason :: no_integer | not_a_list.

to_integer(_) ->
    erlang:nif_error(undef).

%%% End of BIFs

%% Robert's bit

%% len(String)
%%  Return the length of a string.

-spec len(String) -> Length when
      String :: string(),
      Length :: non_neg_integer().

len(S) -> length(S).

%% equal(String1, String2)
%%  Test if 2 strings are equal.

-spec equal(String1, String2) -> boolean() when
      String1 :: string(),
      String2 :: string().

equal(S, S) -> true;
equal(_, _) -> false.

%% concat(String1, String2)
%%  Concatenate 2 strings.

-spec concat(String1, String2) -> String3 when
      String1 :: string(),
      String2 :: string(),
      String3 :: string().

concat(S1, S2) -> S1 ++ S2.

%% chr(String, Char)
%% rchr(String, Char)
%%  Return the first/last index of the character in a string.

-spec chr(String, Character) -> Index when
      String :: string(),
      Character :: char(),
      Index :: non_neg_integer().

chr(S, C) when is_integer(C) -> chr(S, C, 1).

chr([C|_Cs], C, I) -> I;
chr([_|Cs], C, I) -> chr(Cs, C, I+1);
chr([], _C, _I) -> 0.

-spec rchr(String, Character) -> Index when
      String :: string(),
      Character :: char(),
      Index :: non_neg_integer().

rchr(S, C) when is_integer(C) -> rchr(S, C, 1, 0).

rchr([C|Cs], C, I, _L) ->			%Found one, now find next!
    rchr(Cs, C, I+1, I);
rchr([_|Cs], C, I, L) ->
    rchr(Cs, C, I+1, L);
rchr([], _C, _I, L) -> L.

%% str(String, SubString)
%% rstr(String, SubString)
%% index(String, SubString)
%%  Return the first/last index of the sub-string in a string.
%%  index/2 is kept for backwards compatibility.

-spec str(String, SubString) -> Index when
      String :: string(),
      SubString :: string(),
      Index :: non_neg_integer().

str(S, Sub) when is_list(Sub) -> str(S, Sub, 1).

str([C|S], [C|Sub], I) ->
    case prefix(Sub, S) of
	true -> I;
	false -> str(S, [C|Sub], I+1)
    end;
str([_|S], Sub, I) -> str(S, Sub, I+1);
str([], _Sub, _I) -> 0.

-spec rstr(String, SubString) -> Index when
      String :: string(),
      SubString :: string(),
      Index :: non_neg_integer().

rstr(S, Sub) when is_list(Sub) -> rstr(S, Sub, 1, 0).

rstr([C|S], [C|Sub], I, L) ->
    case prefix(Sub, S) of
	true -> rstr(S, [C|Sub], I+1, I);
	false -> rstr(S, [C|Sub], I+1, L)
    end;
rstr([_|S], Sub, I, L) -> rstr(S, Sub, I+1, L);
rstr([], _Sub, _I, L) -> L.

prefix([C|Pre], [C|String]) -> prefix(Pre, String);
prefix([], String) when is_list(String) -> true;
prefix(Pre, String) when is_list(Pre), is_list(String) -> false.

%% span(String, Chars) -> Length.
%% cspan(String, Chars) -> Length.

-spec span(String, Chars) -> Length when
      String :: string(),
      Chars :: string(),
      Length :: non_neg_integer().

span(S, Cs) when is_list(Cs) -> span(S, Cs, 0).

span([C|S], Cs, I) ->
    case member(C, Cs) of
	true -> span(S, Cs, I+1);
	false -> I
    end;
span([], _Cs, I) -> I.

-spec cspan(String, Chars) -> Length when
      String :: string(),
      Chars :: string(),
      Length :: non_neg_integer().

cspan(S, Cs) when is_list(Cs) -> cspan(S, Cs, 0).

cspan([C|S], Cs, I) ->
    case member(C, Cs) of
	true -> I;
	false -> cspan(S, Cs, I+1)
    end;
cspan([], _Cs, I) -> I.

%% substr(String, Start)
%% substr(String, Start, Length)
%%  Extract a sub-string from String.

-spec substr(String, Start) -> SubString when
      String :: string(),
      SubString :: string(),
      Start :: pos_integer().

substr(String, 1) when is_list(String) -> 
    String;
substr(String, S) when is_integer(S), S > 1 ->
    substr2(String, S).

-spec substr(String, Start, Length) -> SubString when
      String :: string(),
      SubString :: string(),
      Start :: pos_integer(),
      Length :: non_neg_integer().

substr(String, S, L) when is_integer(S), S >= 1, is_integer(L), L >= 0 ->
    substr1(substr2(String, S), L).

substr1([C|String], L) when L > 0 -> [C|substr1(String, L-1)];
substr1(String, _L) when is_list(String) -> [].	     %Be nice!

substr2(String, 1) when is_list(String) -> String;
substr2([_|String], S) -> substr2(String, S-1).

%% tokens(String, Seperators).
%%  Return a list of tokens seperated by characters in Seperators.

-spec tokens(String, SeparatorList) -> Tokens when
      String :: string(),
      SeparatorList :: string(),
      Tokens :: [Token :: nonempty_string()].

tokens(S, Seps) ->
    case Seps of
	[] ->
	    case S of
		[] -> [];
		[_|_] -> [S]
	    end;
	[C] ->
	    tokens_single_1(reverse(S), C, []);
	[_|_] ->
	    tokens_multiple_1(reverse(S), Seps, [])
    end.

tokens_single_1([Sep|S], Sep, Toks) ->
    tokens_single_1(S, Sep, Toks);
tokens_single_1([C|S], Sep, Toks) ->
    tokens_single_2(S, Sep, Toks, [C]);
tokens_single_1([], _, Toks) ->
    Toks.

tokens_single_2([Sep|S], Sep, Toks, Tok) ->
    tokens_single_1(S, Sep, [Tok|Toks]);
tokens_single_2([C|S], Sep, Toks, Tok) ->
    tokens_single_2(S, Sep, Toks, [C|Tok]);
tokens_single_2([], _Sep, Toks, Tok) ->
    [Tok|Toks].

tokens_multiple_1([C|S], Seps, Toks) ->
    case member(C, Seps) of
	true -> tokens_multiple_1(S, Seps, Toks);
	false -> tokens_multiple_2(S, Seps, Toks, [C])
    end;
tokens_multiple_1([], _Seps, Toks) ->
    Toks.

tokens_multiple_2([C|S], Seps, Toks, Tok) ->
    case member(C, Seps) of
	true -> tokens_multiple_1(S, Seps, [Tok|Toks]);
	false -> tokens_multiple_2(S, Seps, Toks, [C|Tok])
    end;
tokens_multiple_2([], _Seps, Toks, Tok) ->
    [Tok|Toks].

-spec chars(Character, Number) -> String when
      Character :: char(),
      Number :: non_neg_integer(),
      String :: string().

chars(C, N) -> chars(C, N, []).

-spec chars(Character, Number, Tail) -> String when
      Character :: char(),
      Number :: non_neg_integer(),
      Tail :: string(),
      String :: string().

chars(C, N, Tail) when N > 0 ->
    chars(C, N-1, [C|Tail]);
chars(C, 0, Tail) when is_integer(C) ->
    Tail.

%% Torbjörn's bit.

%%% COPIES %%%

-spec copies(String, Number) -> Copies when
      String :: string(),
      Copies :: string(),
      Number :: non_neg_integer().

copies(CharList, Num) when is_list(CharList), is_integer(Num), Num >= 0 ->
    copies(CharList, Num, []).

copies(_CharList, 0, R) ->
    R;
copies(CharList, Num, R) ->
    copies(CharList, Num-1, CharList++R).

%%% WORDS %%%

-spec words(String) -> Count when
      String :: string(),
      Count :: pos_integer().

words(String) -> words(String, $\s).

-spec words(String, Character) -> Count when
      String :: string(),
      Character :: char(),
      Count :: pos_integer().

words(String, Char) when is_integer(Char) ->
    w_count(strip(String, both, Char), Char, 0).

w_count([], _, Num) -> Num+1;
w_count([H|T], H, Num) -> w_count(strip(T, left, H), H, Num+1);
w_count([_H|T], Char, Num) -> w_count(T, Char, Num).

%%% SUB_WORDS %%%

-spec sub_word(String, Number) -> Word when
      String :: string(),
      Word :: string(),
      Number :: integer().

sub_word(String, Index) -> sub_word(String, Index, $\s).

-spec sub_word(String, Number, Character) -> Word when
      String :: string(),
      Word :: string(),
      Number :: integer(),
      Character :: char().

sub_word(String, Index, Char) when is_integer(Index), is_integer(Char) ->
    case words(String, Char) of
	Num when Num < Index ->
	    [];
	_Num ->
	    s_word(strip(String, left, Char), Index, Char, 1, [])
    end.

s_word([], _, _, _,Res) -> reverse(Res);
s_word([Char|_],Index,Char,Index,Res) -> reverse(Res);
s_word([H|T],Index,Char,Index,Res) -> s_word(T,Index,Char,Index,[H|Res]);
s_word([Char|T],Stop,Char,Index,Res) when Index < Stop -> 
    s_word(strip(T,left,Char),Stop,Char,Index+1,Res);
s_word([_|T],Stop,Char,Index,Res) when Index < Stop -> 
    s_word(T,Stop,Char,Index,Res).

%%% STRIP %%%

-spec strip(string()) -> string().

strip(String) -> strip(String, both).

-spec strip(String, Direction) -> Stripped when
      String :: string(),
      Stripped :: string(),
      Direction :: left | right | both.

strip(String, left) -> strip_left(String, $\s);
strip(String, right) -> strip_right(String, $\s);
strip(String, both) ->
    strip_right(strip_left(String, $\s), $\s).

-spec strip(String, Direction, Character) -> Stripped when
      String :: string(),
      Stripped :: string(),
      Direction :: left | right | both,
      Character :: char().

strip(String, right, Char) -> strip_right(String, Char);
strip(String, left, Char) -> strip_left(String, Char);
strip(String, both, Char) ->
    strip_right(strip_left(String, Char), Char).

strip_left([Sc|S], Sc) ->
    strip_left(S, Sc);
strip_left([_|_]=S, Sc) when is_integer(Sc) -> S;
strip_left([], Sc) when is_integer(Sc) -> [].

strip_right([Sc|S], Sc) ->
    case strip_right(S, Sc) of
	[] -> [];
	T  -> [Sc|T]
    end;
strip_right([C|S], Sc) ->
    [C|strip_right(S, Sc)];
strip_right([], Sc) when is_integer(Sc) ->
    [].

%%% LEFT %%%

-spec left(String, Number) -> Left when
      String :: string(),
      Left :: string(),
      Number :: non_neg_integer().

left(String, Len) when is_integer(Len) -> left(String, Len, $\s).

-spec left(String, Number, Character) -> Left when
      String :: string(),
      Left :: string(),
      Number :: non_neg_integer(),
      Character :: char().

left(String, Len, Char) when is_integer(Char) ->
    Slen = length(String),
    if
	Slen > Len -> substr(String, 1, Len);
	Slen < Len -> l_pad(String, Len-Slen, Char);
	Slen =:= Len -> String
    end.

l_pad(String, Num, Char) -> String ++ chars(Char, Num).

%%% RIGHT %%%

-spec right(String, Number) -> Right when
      String :: string(),
      Right :: string(),
      Number :: non_neg_integer().

right(String, Len) when is_integer(Len) -> right(String, Len, $\s).

-spec right(String, Number, Character) -> Right when
      String :: string(),
      Right :: string(),
      Number :: non_neg_integer(),
      Character :: char().

right(String, Len, Char) when is_integer(Char) ->
    Slen = length(String),
    if
	Slen > Len -> substr(String, Slen-Len+1);
	Slen < Len -> r_pad(String, Len-Slen, Char);
	Slen =:= Len -> String
    end.

r_pad(String, Num, Char) -> chars(Char, Num, String).

%%% CENTRE %%%

-spec centre(String, Number) -> Centered when
      String :: string(),
      Centered :: string(),
      Number :: non_neg_integer().

centre(String, Len) when is_integer(Len) -> centre(String, Len, $\s).

-spec centre(String, Number, Character) -> Centered when
      String :: string(),
      Centered :: string(),
      Number :: non_neg_integer(),
      Character :: char().

centre(String, 0, Char) when is_list(String), is_integer(Char) ->
    [];                       % Strange cases to centre string
centre(String, Len, Char) when is_integer(Char) ->
    Slen = length(String),
    if
	Slen > Len -> substr(String, (Slen-Len) div 2 + 1, Len);
	Slen < Len ->
	    N = (Len-Slen) div 2,
	    r_pad(l_pad(String, Len-(Slen+N), Char), N, Char);
	Slen =:= Len -> String
    end.

%%% SUB_STRING %%%

-spec sub_string(String, Start) -> SubString when
      String :: string(),
      SubString :: string(),
      Start :: pos_integer().

sub_string(String, Start) -> substr(String, Start).

-spec sub_string(String, Start, Stop) -> SubString when
      String :: string(),
      SubString :: string(),
      Start :: pos_integer(),
      Stop :: pos_integer().

sub_string(String, Start, Stop) -> substr(String, Start, Stop - Start + 1).

%% ISO/IEC 8859-1 (latin1) letters are converted, others are ignored
%%

to_lower_char(C) when is_integer(C), $A =< C, C =< $Z ->
    C + 32;
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 ->
    C + 32;
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE ->
    C + 32;
to_lower_char(C) ->
    C.

to_upper_char(C) when is_integer(C), $a =< C, C =< $z ->
    C - 32;
to_upper_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 ->
    C - 32;
to_upper_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE ->
    C - 32;
to_upper_char(C) ->
    C.

-spec to_lower(String) -> Result when
                  String :: io_lib:latin1_string(),
                  Result :: io_lib:latin1_string()
	    ; (Char) -> CharResult when
                  Char :: char(),
                  CharResult :: char().

to_lower(S) when is_list(S) ->
    [to_lower_char(C) || C <- S];
to_lower(C) when is_integer(C) ->
    to_lower_char(C).

-spec to_upper(String) -> Result when
                  String :: io_lib:latin1_string(),
                  Result :: io_lib:latin1_string()
	    ; (Char) -> CharResult when
                  Char :: char(),
                  CharResult :: char().

to_upper(S) when is_list(S) ->
    [to_upper_char(C) || C <- S];
to_upper(C) when is_integer(C) ->
    to_upper_char(C).

-spec join(StringList, Separator) -> String when
      StringList :: [string()],
      Separator :: string(),
      String :: string().

join([], Sep) when is_list(Sep) ->
    [];
join([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).

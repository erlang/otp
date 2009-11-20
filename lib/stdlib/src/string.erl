%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(string).

-export([len/1,equal/2,concat/2,chr/2,rchr/2,str/2,rstr/2,
	 span/2,cspan/2,substr/2,substr/3,tokens/2,chars/2,chars/3]).
-export([copies/2,words/1,words/2,strip/1,strip/2,strip/3,
	 sub_word/2,sub_word/3,left/2,left/3,right/2,right/3,
	 sub_string/2,sub_string/3,centre/2,centre/3, join/2]).
-export([to_upper/1, to_lower/1]).

-import(lists,[reverse/1,member/2]).

%%---------------------------------------------------------------------------

-type direction() :: 'left' | 'right' | 'both'.

%%---------------------------------------------------------------------------

%% Robert's bit

%% len(String)
%%  Return the length of a string.

-spec len(string()) -> non_neg_integer().

len(S) -> length(S).

%% equal(String1, String2)
%%  Test if 2 strings are equal.

-spec equal(string(), string()) -> boolean().

equal(S, S) -> true;
equal(_, _) -> false.

%% concat(String1, String2)
%%  Concatenate 2 strings.

-spec concat(string(), string()) -> string().

concat(S1, S2) -> S1 ++ S2.

%% chr(String, Char)
%% rchr(String, Char)
%%  Return the first/last index of the character in a string.

-spec chr(string(), char()) -> non_neg_integer().

chr(S, C) when is_integer(C) -> chr(S, C, 1).

chr([C|_Cs], C, I) -> I;
chr([_|Cs], C, I) -> chr(Cs, C, I+1);
chr([], _C, _I) -> 0.

-spec rchr(string(), char()) -> non_neg_integer().

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

-spec str(string(), string()) -> non_neg_integer().

str(S, Sub) when is_list(Sub) -> str(S, Sub, 1).

str([C|S], [C|Sub], I) ->
    case prefix(Sub, S) of
	true -> I;
	false -> str(S, [C|Sub], I+1)
    end;
str([_|S], Sub, I) -> str(S, Sub, I+1);
str([], _Sub, _I) -> 0.

-spec rstr(string(), string()) -> non_neg_integer().

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

-spec span(string(), string()) -> non_neg_integer().

span(S, Cs) when is_list(Cs) -> span(S, Cs, 0).

span([C|S], Cs, I) ->
    case member(C, Cs) of
	true -> span(S, Cs, I+1);
	false -> I
    end;
span([], _Cs, I) -> I.

-spec cspan(string(), string()) -> non_neg_integer().

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

-spec substr(string(), pos_integer()) -> string().

substr(String, 1) when is_list(String) -> 
    String;
substr(String, S) when is_integer(S), S > 1 ->
    substr2(String, S).

-spec substr(string(), pos_integer(), non_neg_integer()) -> string().

substr(String, S, L) when is_integer(S), S >= 1, is_integer(L), L >= 0 ->
    substr1(substr2(String, S), L).

substr1([C|String], L) when L > 0 -> [C|substr1(String, L-1)];
substr1(String, _L) when is_list(String) -> [].	     %Be nice!

substr2(String, 1) when is_list(String) -> String;
substr2([_|String], S) -> substr2(String, S-1).

%% tokens(String, Seperators).
%%  Return a list of tokens seperated by characters in Seperators.

-spec tokens(string(), string()) -> [[char(),...]].

tokens(S, Seps) ->
    tokens1(S, Seps, []).

tokens1([C|S], Seps, Toks) ->
    case member(C, Seps) of
	true -> tokens1(S, Seps, Toks);
	false -> tokens2(S, Seps, Toks, [C])
    end;
tokens1([], _Seps, Toks) ->
    reverse(Toks).

tokens2([C|S], Seps, Toks, Cs) ->
    case member(C, Seps) of
	true -> tokens1(S, Seps, [reverse(Cs)|Toks]);
	false -> tokens2(S, Seps, Toks, [C|Cs])
    end;
tokens2([], _Seps, Toks, Cs) ->
    reverse([reverse(Cs)|Toks]).

-spec chars(char(), non_neg_integer()) -> string().

chars(C, N) -> chars(C, N, []).

-spec chars(char(), non_neg_integer(), string()) -> string().

chars(C, N, Tail) when N > 0 ->
    chars(C, N-1, [C|Tail]);
chars(C, 0, Tail) when is_integer(C) ->
    Tail.

%% Torbjörn's bit.

%%% COPIES %%%

-spec copies(string(), non_neg_integer()) -> string().

copies(CharList, Num) when is_list(CharList), Num >= 0 ->
    copies(CharList, Num, []).

copies(_CharList, 0, R) ->
    R;
copies(CharList, Num, R) ->
    copies(CharList, Num-1, CharList++R).

%%% WORDS %%%

-spec words(string()) -> pos_integer().

words(String) -> words(String, $\s).

-spec words(string(), char()) -> pos_integer().

words(String, Char) when is_integer(Char) ->
    w_count(strip(String, both, Char), Char, 0).

w_count([], _, Num) -> Num+1;
w_count([H|T], H, Num) -> w_count(strip(T, left, H), H, Num+1);
w_count([_H|T], Char, Num) -> w_count(T, Char, Num).

%%% SUB_WORDS %%%

-spec sub_word(string(), integer()) -> string().

sub_word(String, Index) -> sub_word(String, Index, $\s).

-spec sub_word(string(), integer(), char()) -> string().

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

-spec strip(string(), direction()) -> string().

strip(String, left) -> strip_left(String, $\s);
strip(String, right) -> strip_right(String, $\s);
strip(String, both) ->
    strip_right(strip_left(String, $\s), $\s).

-spec strip(string(), direction(), char()) -> string().

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

-spec left(string(), non_neg_integer()) -> string().

left(String, Len) when is_integer(Len) -> left(String, Len, $\s).

-spec left(string(), non_neg_integer(), char()) -> string().

left(String, Len, Char) when is_integer(Char) ->
    Slen = length(String),
    if
	Slen > Len -> substr(String, 1, Len);
	Slen < Len -> l_pad(String, Len-Slen, Char);
	Slen =:= Len -> String
    end.

l_pad(String, Num, Char) -> String ++ chars(Char, Num).

%%% RIGHT %%%

-spec right(string(), non_neg_integer()) -> string().

right(String, Len) when is_integer(Len) -> right(String, Len, $\s).

-spec right(string(), non_neg_integer(), char()) -> string().

right(String, Len, Char) when is_integer(Char) ->
    Slen = length(String),
    if
	Slen > Len -> substr(String, Slen-Len+1);
	Slen < Len -> r_pad(String, Len-Slen, Char);
	Slen =:= Len -> String
    end.

r_pad(String, Num, Char) -> chars(Char, Num, String).

%%% CENTRE %%%

-spec centre(string(), non_neg_integer()) -> string().

centre(String, Len) when is_integer(Len) -> centre(String, Len, $\s).

-spec centre(string(), non_neg_integer(), char()) -> string().

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

-spec sub_string(string(), pos_integer()) -> string().

sub_string(String, Start) -> substr(String, Start).

-spec sub_string(string(), pos_integer(), pos_integer()) -> string().

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

-spec to_lower(string()) -> string()
	    ; (char()) -> char().

to_lower(S) when is_list(S) ->
    [to_lower_char(C) || C <- S];
to_lower(C) when is_integer(C) ->
    to_lower_char(C).

-spec to_upper(string()) -> string()
	    ; (char()) -> char().

to_upper(S) when is_list(S) ->
    [to_upper_char(C) || C <- S];
to_upper(C) when is_integer(C) ->
    to_upper_char(C).

-spec join([string()], string()) -> string().

join([], Sep) when is_list(Sep) ->
    [];
join([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).

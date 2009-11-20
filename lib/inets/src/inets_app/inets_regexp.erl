%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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

-module(inets_regexp).

-export([parse/1, match/2, first_match/2, split/2, sub/3, gsub/3]).


%%%=========================================================================
%%%  API
%%%=========================================================================

%% parse(RegExp) -> {ok, RE} | {error, E}.
%% Parse the regexp described in the string RegExp.

parse(S) ->
    case (catch reg(S)) of
	{R, []} -> 
	    {ok, R};
	{_R, [C|_]} -> 
	    {error, {illegal, [C]}};
	{error, E} -> 
	    {error, E}
    end.


%% Find the longest match of RegExp in String.

match(S, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> match(S, RE);
	{error,E} -> {error,E}
    end;
match(S, RE) ->
    case match(RE, S, 1, 0, -1) of
	{Start,Len} when Len >= 0 ->
	    {match, Start, Len};
	{_Start,_Len} -> 
	    nomatch
    end.

%% Find the first match of RegExp in String.

first_match(S, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok, RE} -> 
	    first_match(S, RE);
	{error, E} -> 
	    {error, E}
    end;
first_match(S, RE) ->
    case first_match(RE, S, 1) of
	{Start,Len} when Len >= 0 ->
	    {match, Start,Len};
	nomatch -> 
	    nomatch
    end.

first_match(RE, S, St) when S =/= [] ->
    case re_apply(S, St, RE) of
	{match, P, _Rest} -> 
	    {St, P-St};
	nomatch -> 
	    first_match(RE, tl(S), St+1)
    end;
first_match(_RE, [], _St) -> 
    nomatch.


match(RE, S, St, Pos, L) ->
    case first_match(RE, S, St) of
	{St1, L1} ->
	    Nst = St1 + 1,
	    if L1 > L -> 
		    match(RE, lists:nthtail(Nst-St, S), Nst, St1, L1);
	       true -> 
		    match(RE, lists:nthtail(Nst-St, S), Nst, Pos, L)
	    end;
	nomatch -> 
	    {Pos, L}
    end.


%% Split a string into substrings where the RegExp describes the
%% field seperator. The RegExp " " is specially treated.

split(String, " ") ->				%This is really special
    {ok, RE} = parse("[ \t]+"),
    case split_apply(String, RE, true) of
	[[]|Ss] -> 
	    {ok,Ss};
	Ss -> 
	    {ok,Ss}
    end;
split(String, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok, RE} -> 
	    {ok, split_apply(String, RE, false)};
	{error, E} -> 
	    {error,E}
    end;
split(String, RE) -> 
    {ok, split_apply(String, RE, false)}.


%% Substitute the first match of the regular expression RegExp 
%% with the string Replace in String. Accept pre-parsed regular
%% expressions.

sub(String, RegExp, Rep) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok, RE} -> 
	    sub(String, RE, Rep);
	{error, E} -> 
	    {error, E}
    end;
sub(String, RE, Rep) ->
    Ss = sub_match(String, RE, 1),
    {ok, sub_repl(Ss, Rep, String, 1), length(Ss)}.


%% Substitute every match of the regular expression RegExp with 
%% the string New in String. Accept pre-parsed regular expressions.

gsub(String, RegExp, Rep) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok, RE} -> 
	    gsub(String, RE, Rep);
	{error, E} -> 
	    {error, E}
    end;
gsub(String, RE, Rep) ->
    Ss = matches(String, RE, 1),
    {ok, sub_repl(Ss, Rep, String, 1), length(Ss)}.


%%%========================================================================
%%% Internal functions
%%%========================================================================

%% This is the regular expression grammar used. It is equivalent to the
%% one used in AWK, except that we allow ^ $ to be used anywhere and fail
%% in the matching.
%%
%% reg -> reg1 : '$1'.
%% reg1 -> reg1 "|" reg2 : {'or','$1','$2'}.
%% reg1 -> reg2 : '$1'.
%% reg2 -> reg2 reg3 : {concat,'$1','$2'}.
%% reg2 -> reg3 : '$1'.
%% reg3 -> reg3 "*" : {kclosure,'$1'}.
%% reg3 -> reg3 "+" : {pclosure,'$1'}.
%% reg3 -> reg3 "?" : {optional,'$1'}.
%% reg3 -> reg4 : '$1'.
%% reg4 -> "(" reg ")" : '$2'.
%% reg4 -> "\\" char : '$2'.
%% reg4 -> "^" : bos.
%% reg4 -> "$" : eos.
%% reg4 -> "." : char.
%% reg4 -> "[" class "]" : {char_class,char_class('$2')}
%% reg4 -> "[" "^" class "]" : {comp_class,char_class('$3')}
%% reg4 -> "\"" chars "\"" : char_string('$2')
%% reg4 -> char : '$1'.
%% reg4 -> empty : epsilon.
%%  The grammar of the current regular expressions. The actual parser
%%  is a recursive descent implementation of the grammar.

reg(S) -> reg1(S).

%% reg1 -> reg2 reg1'
%% reg1' -> "|" reg2
%% reg1' -> empty

reg1(S0) ->
    {L,S1} = reg2(S0),
    reg1p(S1, L).

reg1p([$||S0], L) ->
    {R,S1} = reg2(S0),
    reg1p(S1, {'or',L,R});
reg1p(S, L) -> {L,S}.

%% reg2 -> reg3 reg2'
%% reg2' -> reg3
%% reg2' -> empty

reg2(S0) ->
    {L,S1} = reg3(S0),
    reg2p(S1, L).

reg2p([C|S0], L) when (C =/= $|) andalso (C =/= $)) ->
    {R,S1} = reg3([C|S0]),
    reg2p(S1, {concat,L,R});
reg2p(S, L) -> {L,S}.

%% reg3 -> reg4 reg3'
%% reg3' -> "*" reg3'
%% reg3' -> "+" reg3'
%% reg3' -> "?" reg3'
%% reg3' -> empty

reg3(S0) ->
    {L,S1} = reg4(S0),
    reg3p(S1, L).

reg3p([$*|S], L) -> reg3p(S, {kclosure,L});
reg3p([$+|S], L) -> reg3p(S, {pclosure,L});
reg3p([$?|S], L) -> reg3p(S, {optional,L});
reg3p(S, L) -> {L,S}.

reg4([$(|S0]) ->
    case reg(S0) of
	{R,[$)|S1]} -> {R,S1};
	{_R,_S} -> throw({error,{unterminated,"("}})
    end;
reg4([$\\,O1,O2,O3|S]) 
  when ((O1 >= $0) andalso 
	(O1 =< $7) andalso 
	(O2 >= $0) andalso 
	(O2 =< $7) andalso 
	(O3 >= $0) andalso 
	(O3 =< $7)) ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
reg4([$\\,C|S]) -> 
    {escape_char(C),S};
reg4([$\\]) -> 
    throw({error, {unterminated,"\\"}});
reg4([$^|S]) -> 
    {bos,S};
reg4([$$|S]) -> 
    {eos,S};
reg4([$.|S]) -> 
    {{comp_class,"\n"},S};
reg4("[^" ++ S0) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{comp_class,Cc},S1};
	{_Cc,_S} -> throw({error,{unterminated,"["}})
    end;
reg4([$[|S0]) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{char_class,Cc},S1};
	{_Cc,_S1} -> throw({error,{unterminated,"["}})
    end;
reg4([C|S]) 
  when (C =/= $*) andalso (C =/= $+) andalso (C =/= $?) andalso (C =/= $]) -> 
    {C, S};
reg4([C|_S]) -> 
    throw({error,{illegal,[C]}});
reg4([]) -> 
    {epsilon,[]}.

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPACE
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.

char_class([$]|S]) -> char_class(S, [$]]);
char_class(S) -> char_class(S, []).

char($\\, [O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
char($\\, [C|S]) -> {escape_char(C),S};
char(C, S) -> {C,S}.

char_class([C1|S0], Cc) when C1 =/= $] ->
    case char(C1, S0) of
	{Cf,[$-,C2|S1]} when C2 =/= $] ->
	    case char(C2, S1) of
		{Cl,S2} when Cf < Cl -> char_class(S2, [{Cf,Cl}|Cc]); 
		{Cl,_S2} -> throw({error,{char_class,[Cf,$-,Cl]}})
	    end;
	{C,S1} -> char_class(S1, [C|Cc])
    end;
char_class(S, Cc) -> {Cc,S}.


%% re_apply(String, StartPos, RegExp) -> re_app_res().
%%
%%  Apply the (parse of the) regular expression RegExp to String.  If
%%  there is a match return the position of the remaining string and
%%  the string if else return 'nomatch'. BestMatch specifies if we want
%%  the longest match, or just a match.
%%
%%  StartPos should be the real start position as it is used to decide
%%  if we ae at the beginning of the string.
%%
%%  Pass two functions to re_apply_or so it can decide, on the basis
%%  of BestMatch, whether to just any take any match or try both to
%%  find the longest. This is slower but saves duplicatng code.

re_apply(S, St, RE) -> re_apply(RE, [], S, St).

re_apply(epsilon, More, S, P) ->		%This always matches
    re_apply_more(More, S, P);
re_apply({'or',RE1,RE2}, More, S, P) ->
    re_apply_or(re_apply(RE1, More, S, P),
		re_apply(RE2, More, S, P));
re_apply({concat,RE1,RE2}, More, S0, P) ->
    re_apply(RE1, [RE2|More], S0, P);
re_apply({kclosure,CE}, More, S, P) ->
    %% Be careful with the recursion, explicitly do one call before
    %% looping.
    re_apply_or(re_apply_more(More, S, P),
		re_apply(CE, [{kclosure,CE}|More], S, P));
re_apply({pclosure,CE}, More, S, P) ->
    re_apply(CE, [{kclosure,CE}|More], S, P);
re_apply({optional,CE}, More, S, P) ->
    re_apply_or(re_apply_more(More, S, P),
		re_apply(CE, More, S, P));
re_apply(bos, More, S, 1) -> re_apply_more(More, S, 1);
re_apply(eos, More, [$\n|S], P) -> re_apply_more(More, S, P);
re_apply(eos, More, [], P) -> re_apply_more(More, [], P);
re_apply({char_class,Cc}, More, [C|S], P) ->
    case in_char_class(C, Cc) of
	true -> re_apply_more(More, S, P+1);
	false -> nomatch
    end;
re_apply({comp_class,Cc}, More, [C|S], P) ->
    case in_char_class(C, Cc) of
	true -> nomatch;
	false -> re_apply_more(More, S, P+1)
    end;
re_apply(C, More, [C|S], P) when is_integer(C) ->
    re_apply_more(More, S, P+1);
re_apply(_RE, _More, _S, _P) -> nomatch.

%% re_apply_more([RegExp], String, Length) -> re_app_res().

re_apply_more([RE|More], S, P) -> re_apply(RE, More, S, P);
re_apply_more([], S, P) -> {match,P,S}.

%% in_char_class(Char, Class) -> bool().

in_char_class(C, [{C1,C2}|_Cc]) when C >= C1, C =< C2 -> true;
in_char_class(C, [C|_Cc]) -> true;
in_char_class(C, [_|Cc]) -> in_char_class(C, Cc);
in_char_class(_C, []) -> false.

%% re_apply_or(Match1, Match2) -> re_app_res().
%%  If we want the best match then choose the longest match, else just
%%  choose one by trying sequentially.

re_apply_or({match,P1,S1},   {match,P2,_S2}) when P1 >= P2 -> {match,P1,S1};
re_apply_or({match,_P1,_S1}, {match,P2,S2}) -> {match,P2,S2};
re_apply_or(nomatch, R2) -> R2;
re_apply_or(R1, nomatch) -> R1.


matches(S, RE, St) ->
    case first_match(RE, S, St) of
	{St1,0} -> 
	    [{St1,0}|matches(string:substr(S, St1+2-St), RE, St1+1)];
	{St1,L1} -> 
	    [{St1,L1}|matches(string:substr(S, St1+L1+1-St), RE, St1+L1)];
	nomatch -> 
	    []
    end.

sub_match(S, RE, St) ->
    case first_match(RE, S, St) of
	{St1,L1} -> [{St1,L1}];
	nomatch -> []
    end.

sub_repl([{St,L}|Ss], Rep, S, Pos) ->
    Rs = sub_repl(Ss, Rep, S, St+L),
    string:substr(S, Pos, St-Pos) ++ 
	sub_repl(Rep, string:substr(S, St, L), Rs);
sub_repl([], _Rep, S, Pos) -> 
    string:substr(S, Pos).

sub_repl([$&|Rep], M, Rest) -> M ++ sub_repl(Rep, M, Rest);
sub_repl("\\&" ++ Rep, M, Rest) -> [$&|sub_repl(Rep, M, Rest)];
sub_repl([C|Rep], M, Rest) -> [C|sub_repl(Rep, M, Rest)];
sub_repl([], _M, Rest) -> Rest.

split_apply(S, RE, Trim) -> split_apply(S, 1, RE, Trim, []).

split_apply([], _P, _RE, true, []) -> 
    [];
split_apply([], _P, _RE, _T, Sub) -> 
    [lists:reverse(Sub)];
split_apply(S, P, RE, T, Sub) ->
    case re_apply(S, P, RE) of
	{match,P,_Rest} ->
	    split_apply(tl(S), P+1, RE, T, [hd(S)|Sub]);
	{match,P1,Rest} ->
	    [lists:reverse(Sub)|split_apply(Rest, P1, RE, T, [])];
	nomatch ->
	    split_apply(tl(S), P+1, RE, T, [hd(S)|Sub])
    end.

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

%%
-module(xmerl_regexp).

%% This module provides a basic set of regular expression functions
%% for strings. The functions provided are taken from AWK.
%%
%% Note that we interpret the syntax tree of a regular expression
%% directly instead of converting it to an NFA and then interpreting
%% that. This method seems to go significantly faster.

-export([sh_to_awk/1,parse/1,format_error/1,match/2,first_match/2,matches/2]).
-export([sub/3,gsub/3,split/2,sub_match/2,sub_first_match/2]).

-export([make_nfa/1,make_dfa/1,make_dfa/2,compile/1]).

-import(string, [substr/2,substr/3]).
-import(lists, [reverse/1,reverse/2,last/1,duplicate/2,seq/2]).
-import(lists, [member/2,keysearch/3,keysort/2,map/2,foldl/3]).
-import(ordsets, [is_element/2,add_element/2,union/2,subtract/2]).

%%-compile([export_all]).

-export([setup/1,compile_proc/2]).

-include("xmerl_internal.hrl").

setup(RE0) ->
    RE = setup(RE0, [$^]),
    Pid = spawn(?MODULE,compile_proc,[self(),RE]),
    receive
	{ok,Result} ->
	    Result
    after 2000 ->
	    exit(Pid,force),
	    parse(RE)
    end.
    %% compile(RE).
%%RE.
compile_proc(From,RE) ->
    Res = compile(RE),
    From ! {ok,Res}.


setup([$\\,$d|S],Acc) -> setup(S,"]9-0[" ++Acc);
setup([$\\,$D|S],Acc) -> setup(S,"]9-0^[" ++Acc);
setup([$\\,$s|S],Acc) -> setup(S,"]s\\t\\n\\r\\[" ++Acc);
setup([$\\,$S|S],Acc) -> setup(S,"]\\s\\t\\n\\r^[" ++Acc);
setup([$\\,$i|S],Acc) -> setup(S,"]z-aZ-A_:[" ++Acc);   %% Only Latin-1 now
setup([$\\,$I|S],Acc) -> setup(S,"]z-aZ-A_:^[" ++Acc);
setup([$\\,$c|S],Acc) -> setup(S,"]9-0z-aZ-A_:."++[183]++"-[" ++Acc); 
setup([$\\,$C|S],Acc) -> setup(S,"]9-0z-aZ-A_:."++[183]++"-^[" ++Acc);
%% fixme setup([$\\,$w|S]) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup([$\\,$W|S]) -> {{comp_class,"\s\t\n\r"},S};
%% Letter, Any
%% fixme setup(["\\p{L}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{L}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Letter, Uppercase
%% fixme setup(["\\p{Lu}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Lu}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Letter, Lowercase
%% fixme setup(["\\p{Ll}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Ll}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Letter, Titlecase
%% fixme setup(["\\p{Lt}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Lt}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Letter, Modifier
%% fixme setup(["\\p{Lm}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Lm}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Letter, Other
%% fixme setup(["\\p{Lo}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Lo}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Mark, Any
%% fixme setup(["\\p{M}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{M}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Mark, Nonspacing
%% fixme setup(["\\p{Mn}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Mn}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Mark, Spacing Combining
%% fixme setup(["\\p{Mc}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Mc}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Mark, Enclosing
%% fixme setup(["\\p{Me}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Me}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Number, Any
%% fixme setup(["\\p{N}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{N}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Number, Decimal Digit
%% fixme setup(["\\p{Nd}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Nd}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Number, Letter
%% fixme setup(["\\p{Nl}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Nl}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Number, Other
%% fixme setup(["\\p{No}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{No}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Punctuation, Any
%% fixme setup(["\\p{P}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{P}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Punctuation, Connector
%% fixme setup(["\\p{Pc}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Pc}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Punctuation, Dash
%% fixme setup(["\\p{Pd}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Pd}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Punctuation, Open
%% fixme setup(["\\p{Ps}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Ps}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Punctuation, Close
%% fixme setup(["\\p{Pe}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Pe}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Punctuation, Initial quote (may behave like Ps or Pe, depending on usage)
%% fixme setup(["\\p{Pi}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Pi}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Punctuation, Final quote (may behave like Ps or Pe, depending on usage)
%% fixme setup(["\\p{Pf}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Pf}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Punctuation, Other
%% fixme setup(["\\p{Po}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Po}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Symbol, Any
%% fixme setup(["\\p{S}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{S}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Symbol, Math
%% fixme setup(["\\p{Sm}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Sm}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Symbol, Currency
%% fixme setup(["\\p{Sc}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Sc}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Symbol, Modifier
%% fixme setup(["\\p{Sk}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Sk}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Symbol, Other
%% fixme setup(["\\p{So}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{So}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Separator, Any
%% fixme setup(["\\p{Z}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Z}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Separator, Space
%% fixme setup(["\\p{Zs}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Zs}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Separator, Line
%% fixme setup(["\\p{Zl}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Zl}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Separator, Paragraph
%% fixme setup(["\\p{Zp}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Zp}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Other, Any
%% fixme setup(["\\p{C}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{C}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Other, Control
%% fixme setup(["\\p{Cc}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Cc}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Other, Format
%% fixme setup(["\\p{Cf}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Cf}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Other, Surrogate not supported by schema recommendation
%% fixme setup(["\\p{Cs}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Cs}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Other, Private Use
%% fixme setup(["\\p{Co}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Co}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
%% Other, Not assigned (no characters in the file have this property)
%% fixme setup(["\\p{Cn}" ++ S) -> {{char_class,"\s\t\n\r"},S};
%% fixme setup(["\\P{Cn}" ++ S) -> {{comp_class,"\s\t\n\r"},S};
setup([A|S], Acc) -> setup(S, [A|Acc]);
setup([],Acc) ->  reverse([$$|Acc]).

%% sh_to_awk(ShellRegExp)
%%  Convert a sh style regexp into a full AWK one. The main difficulty is
%%  getting character sets right as the conventions are different.

sh_to_awk(Sh) -> "^(" ++ sh_to_awk_1(Sh).	%Fix the beginning

sh_to_awk_1([$*|Sh]) ->				%This matches any string
    ".*" ++ sh_to_awk_1(Sh);
sh_to_awk_1([$?|Sh]) ->				%This matches any character
    [$.|sh_to_awk_1(Sh)];
sh_to_awk_1([$[,$^,$]|Sh]) ->			%This takes careful handling
    "\\^" ++ sh_to_awk_1(Sh);
%% Must move '^' to end.
sh_to_awk_1("[^" ++ Sh) -> [$[|sh_to_awk_2(Sh, true)];
sh_to_awk_1("[!" ++ Sh) -> "[^" ++ sh_to_awk_2(Sh, false);
sh_to_awk_1([$[|Sh]) -> [$[|sh_to_awk_2(Sh, false)];
sh_to_awk_1([C|Sh]) ->
    %% Unspecialise everything else which is not an escape character.
    case sh_special_char(C) of
	true -> [$\\,C|sh_to_awk_1(Sh)];
	false -> [C|sh_to_awk_1(Sh)]
    end;
sh_to_awk_1([]) -> ")$".			%Fix the end

sh_to_awk_2([$]|Sh], UpArrow) -> [$]|sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_2(Sh, UpArrow) -> sh_to_awk_3(Sh, UpArrow).

sh_to_awk_3([$]|Sh], true) -> "^]" ++ sh_to_awk_1(Sh);
sh_to_awk_3([$]|Sh], false) -> [$]|sh_to_awk_1(Sh)];
sh_to_awk_3([C|Sh], UpArrow) -> [C|sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_3([], true) -> [$^|sh_to_awk_1([])];
sh_to_awk_3([], false) -> sh_to_awk_1([]).

%% -type sh_special_char(char()) -> bool().
%%  Test if a character is a special character.

sh_special_char($|) -> true;
sh_special_char($*) -> true;
sh_special_char($+) -> true;
sh_special_char($?) -> true;
sh_special_char($() -> true;
sh_special_char($)) -> true;
sh_special_char($\\) -> true;
sh_special_char($^) -> true;
sh_special_char($$) -> true;
sh_special_char($.) -> true;
sh_special_char($[) -> true;
sh_special_char($]) -> true;
sh_special_char($") -> true;
sh_special_char(_C) -> false.

%% parse(RegExp) -> {ok,RE} | {error,E}.
%%  Parse the regexp described in the string RegExp.

parse(S) ->
    case catch reg(S, 0) of
	{R,Sc,[]} -> {ok,{regexp,{R,Sc}}};
	{_R,_Sc,[C|_]} -> {error,{illegal,[C]}};
	{error,E} -> {error,E}
    end.

%% format_error(Error) -> String.

format_error({interval_range,What}) ->
    ["illegal interval range",io_lib:write_string(What)];
format_error({illegal,What}) -> ["illegal character `",What,"'"];
format_error({unterminated,What}) -> ["unterminated `",What,"'"];
format_error({posix_cc,What}) ->
    ["illegal POSIX character class ",io_lib:write_string(What)];
format_error({char_class,What}) ->
    ["illegal character class ",io_lib:write_string(What)].

%% match(String, RegExp) -> {match,Start,Length} | nomatch | {error,E}.
%%  Find the longest match of RegExp in String.

match(S, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> match(S, RE);
	{error,E} -> {error,E}
    end;
match(S, {regexp,RE}) ->
    case match_re(RE, S, 1, 0, -1) of
	{Start,Len} when Len >= 0 ->
	    {match,Start,Len};
	{_Start,_Len} -> nomatch
    end;
match(S, {comp_regexp,RE}) ->
    case match_comp(RE, S, 1, 0, -1) of
	{Start,Len} when Len >= 0 ->
	    {match,Start,Len};
	{_Start,_Len} -> nomatch
    end.

match_re(RE, [_|Cs]=S0, P0, Mst, Mlen) ->
    case re_apply(S0, P0, RE) of
	{match,P1,_S1,_Subs} ->
	    Len = P1-P0,
	    if Len > Mlen -> match_re(RE, Cs, P0+1, P0, Len);
	       true -> match_re(RE, Cs, P0+1, Mst, Mlen)
	    end;
	nomatch -> match_re(RE, Cs, P0+1, Mst, Mlen);
	never_match -> {Mst,Mlen}		%No need to go on
    end;
match_re(_RE, _S, _P, Mst, Mlen) -> {Mst,Mlen}.

match_comp(RE, [_|Cs]=S0, P0, Mst, Mlen) ->
    case comp_apply(S0, P0, RE) of
	{match,P1,_S1} ->
	    Len = P1-P0,
	    if Len > Mlen -> match_comp(RE, Cs, P0+1, P0, Len);
	       true -> match_comp(RE, Cs, P0+1, Mst, Mlen)
	    end;
	nomatch -> match_comp(RE, Cs, P0+1, Mst, Mlen)
    end;
match_comp(_RE, _S, _P, Mst, Mlen) -> {Mst,Mlen}.

%% match_re(RE, S0, Pos0, Mst, Mlen) ->
%%     case first_match_re(RE, S0, Pos0) of
%% 	{St,Len,_} ->				%Found a match
%% 	    Pos1 = St + 1,			%Where to start next match
%% 	    S1 = lists:nthtail(Pos1-Pos0, S0),
%% 	    if Len > Mlen -> match_re(RE, S1, Pos1, St, Len);
%% 	       true -> match_re(RE, S1, Pos1, Mst, Mlen)
%% 	    end;
%% 	nomatch -> {Mst,Mlen}
%%     end.

%% match_comp(RE, S0, Pos0, Mst, Mlen) ->
%%     case first_match_comp(RE, S0, Pos0) of
%% 	{St,Len} ->				%Found a match
%% 	    Pos1 = St + 1,			%Where to start next match
%% 	    S1 = lists:nthtail(Pos1-Pos0, S0),
%% 	    if Len > Mlen -> match_comp(RE, S1, Pos1, St, Len);
%% 	       true -> match_comp(RE, S1, Pos1, Mst, Mlen)
%% 	    end;
%% 	nomatch -> {Mst,Mlen}
%%     end.

%% first_match(String, RegExp) -> {match,Start,Length} | nomatch | {error,E}.
%%  Find the first match of RegExp in String.

first_match(S, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> first_match(S, RE);
	{error,E} -> {error,E}
    end;
first_match(S, {regexp,RE}) ->
    case first_match_re(RE, S, 1) of
	{Start,Len,_} -> {match,Start,Len};
	nomatch -> nomatch
    end;
first_match(S, {comp_regexp,RE}) ->
    case first_match_comp(RE, S, 1) of
	{Start,Len} -> {match,Start,Len};
	nomatch -> nomatch
    end.

first_match_re(RE, S, St) when S /= [] ->
    case re_apply(S, St, RE) of
	{match,P,_Rest,Subs} -> {St,P-St,Subs};
	nomatch -> first_match_re(RE, tl(S), St+1);
	never_match -> nomatch
    end;
first_match_re(_RE, [], _St) -> nomatch.

first_match_comp(RE, S, St) when S /= [] ->
    case comp_apply(S, St, RE) of
	{match,P,_Rest} -> {St,P-St};
	nomatch -> first_match_comp(RE, tl(S), St+1)
    end;
first_match_comp(_RE, [], _St) -> nomatch.

%% matches(String, RegExp) -> {match,[{Start,Length}]} | {error,E}.
%%  Return the all the non-overlapping matches of RegExp in String.

matches(S, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> matches(S, RE);
	{error,E} -> {error,E}
    end;
matches(S, {regexp,RE}) -> {match,matches_re(S, RE, 1)};
matches(S, {comp_regexp,RE}) -> {match,matches_comp(S, RE, 1)}.

matches_re([_|Cs]=S0, RE, P0) ->
    case re_apply(S0, P0, RE) of
	{match,P0,S1,_Subs} ->			%0 length match
	    [{P0,0}|matches_re(tl(S1), RE, P0+1)];
	{match,P1,S1,_Subs} ->
	    [{P0,P1-P0}|matches_re(S1, RE, P1)];
	nomatch -> matches_re(Cs, RE, P0+1);
	never_match -> []
    end;
matches_re([], _RE, _P) -> [].

matches_comp([_|Cs]=S0, RE, P0) ->
    case comp_apply(S0, P0, RE) of
	{match,P0,S1} ->			%0 length match
	    [{P0,0}|matches_comp(tl(S1), RE, P0+1)];
	{match,P1,S1} ->
	    [{P0,P1-P0}|matches_comp(S1, RE, P1)];
	nomatch -> matches_comp(Cs, RE, P0+1)
    end;
matches_comp([], _RE, _P) -> [].

%% sub(String, RegExp, Replace) -> {ok,RepString,RepCount} | {error,E}.
%%  Substitute the first match of the regular expression RegExp with
%%  the string Replace in String. Accept pre-parsed regular
%%  expressions.

sub(String, RegExp, Rep) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> sub(String, RE, Rep);
	{error,E} -> {error,E}
    end;
sub(String, {regexp,RE}, Rep) ->
    case sub_re(String, 1, RE, [], Rep) of
	{yes,NewStr} -> {ok,NewStr,1};
	no -> {ok,String,0}
    end;
sub(String, {comp_regexp,RE}, Rep) ->
    case sub_comp(String, 1, RE, [], Rep) of
	{yes,NewStr} -> {ok,NewStr,1};
	no -> {ok,String,0}
    end.

%% sub_re(String, Position, Regexp, Before, Replacement) ->
%%      {NewString,Count}.
%% sub_comp(String, Position, Regexp, Before, Replacement) ->
%%      {NewString,Count}.
%% Step forward over String until a match is found saving stepped over
%% chars in Before. Return reversed Before prepended to replacement
%% and rest of string.

sub_re([C|Cs]=S0, P0, RE, Bef, Rep) ->
    case re_apply(S0, P0, RE) of
	{match,P0,_S1,_} ->			%Ignore 0 length match
	    sub_re(Cs, P0+1, RE, [C|Bef], Rep);
	{match,P1,Rest,_Gps} ->
	    {yes,reverse(Bef, sub_repl(Rep, substr(S0, 1, P1-P0), Rest))};
	nomatch -> sub_re(Cs, P0+1, RE, [C|Bef], Rep);
	never_match -> no			%No need to go on
    end;
sub_re([], _P, _RE, _Bef, _Rep) -> no.

sub_comp([C|Cs]=S0, P0, RE, Bef, Rep) ->
    case comp_apply(S0, P0, RE) of
	{match,P0,_S1} ->			%Ignore 0 length match
	    sub_comp(Cs, P0+1, RE, [C|Bef], Rep);
	{match,P1,Rest} ->
	    {yes,reverse(Bef, sub_repl(Rep, substr(S0, 1, P1-P0), Rest))};
	nomatch -> sub_comp(Cs, P0+1, RE, [C|Bef], Rep)
    end;
sub_comp([], _P, _RE, _Bef, _Rep) -> no.

sub_repl([$&|Rep], M, Rest) -> M ++ sub_repl(Rep, M, Rest);
sub_repl("\\&" ++ Rep, M, Rest) -> [$&|sub_repl(Rep, M, Rest)];
sub_repl([C|Rep], M, Rest) -> [C|sub_repl(Rep, M, Rest)];
sub_repl([], _M, Rest) -> Rest.

%%  gsub(String, RegExp, Replace) -> {ok,RepString,RepCount} | {error,E}.
%%  Substitute every match of the regular expression RegExp with the
%%  string New in String. Accept pre-parsed regular expressions.

gsub(String, RegExp, Rep) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> gsub(String, RE, Rep);
	{error,E} -> {error,E}
    end;
gsub(String, {regexp,RE}, Rep) ->
    case gsub_re(String, 1, RE, [], Rep) of
	{NewStr,N} -> {ok,NewStr,N};
	no -> {ok,String,0}			%No substitutions
    end;
gsub(String, {comp_regexp,RE}, Rep) ->
    case gsub_comp(String, 1, RE, [], Rep) of
	{NewStr,N} -> {ok,NewStr,N};
	no -> {ok,String,0}			%No substitutions
    end.

%% gsub_re(String, Position, Regexp, Before, Replacement) ->
%%      {NewString,Count}.
%% gsub_comp(String, Position, Regexp, Before, Replacement) ->
%%      {NewString,Count}.
%% Step forward over String until a match is found saving stepped over
%% chars in Before. Call recursively to do rest of string after
%% match. Return reversed Before prepended to return from recursive
%% call.

gsub_re([C|Cs]=S0, P0, RE, Bef, Rep) ->
    case re_apply(S0, P0, RE) of
	{match,P0,_S1,_} ->			%Ignore 0 length match
	    gsub_re(Cs, P0+1, RE, [C|Bef], Rep);
	{match,P1,S1,_Gps} ->
	    case gsub_re(S1, P1, RE, [], Rep) of
		{NewStr,N0} ->			%Substituitions
		    {reverse(Bef, sub_repl(Rep, substr(S0, 1, P1-P0), NewStr)),
		     N0+1};
		no ->				%No substituitions.
		    {reverse(Bef, sub_repl(Rep, substr(S0, 1, P1-P0), S1)),1}
	    end;
	%%No match so step forward saving C on Bef.
	nomatch -> gsub_re(Cs, P0+1, RE, [C|Bef], Rep);
	never_match -> no			%No need to go on
    end;
gsub_re([], _P, _RE, _Bef, _Rep) -> no.

gsub_comp([C|Cs]=S0, P0, RE, Bef, Rep) ->
    case comp_apply(S0, P0, RE) of
	{match,P0,_S1} ->			%Ignore 0 length match
	    gsub_comp(Cs, P0+1, RE, [C|Bef], Rep);
	{match,P1,S1} ->
	    case gsub_comp(S1, P1, RE, [], Rep) of
		{NewStr,N0} ->			%Substituitions
		    {reverse(Bef, sub_repl(Rep, substr(S0, 1, P1-P0), NewStr)),
		     N0+1};
		no ->				%No substituitions.
		    {reverse(Bef, sub_repl(Rep, substr(S0, 1, P1-P0), S1)),1}
	    end;
	%%No match so step forward saving C on Bef.
	nomatch -> gsub_comp(Cs, P0+1, RE, [C|Bef], Rep)
    end;
gsub_comp([], _P, _RE, _Bef, _Rep) -> no.

%% split(String, RegExp) -> {ok,[SubString]} | {error,E}.
%%  Split a string into substrings where the RegExp describes the
%%  field seperator. The RegExp " " is specially treated.

split(String, " ") ->				%This is really special
    {ok,{regexp,RE}} = parse("[ \t]+"),
    case split_apply_re(String, RE, true) of
	[[]|Ss] -> {ok,Ss};
	Ss -> {ok,Ss}
    end;
split(String, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,{regexp,RE}} -> {ok,split_apply_re(String, RE, false)};
	{error,E} -> {error,E}
    end;
split(String, {regexp,RE}) -> {ok,split_apply_re(String, RE, false)};
split(String, {comp_regexp,RE}) -> {ok,split_apply_comp(String, RE, false)}.

split_apply_re(S, RE, Trim) -> split_apply_re(S, 1, RE, Trim, []).

split_apply_re([], _P, _RE, true, []) -> [];
split_apply_re([], _P, _RE, _T, Sub) -> [reverse(Sub)];
split_apply_re([C|Cs]=S, P0, RE, T, Sub) ->
    case re_apply(S, P0, RE) of
	{match,P0,_S1,_} ->			%Ignore 0 length match
	    split_apply_re(Cs, P0+1, RE, T, [C|Sub]);
	{match,P1,S1,_} ->
	    [reverse(Sub)|split_apply_re(S1, P1, RE, T, [])];
	nomatch ->
	    split_apply_re(Cs, P0+1, RE, T, [C|Sub]);
	never_match -> [reverse(Sub, S)]	%No need to go on
    end.

split_apply_comp(S, RE, Trim) -> split_apply_comp(S, 1, RE, Trim, []).

%%split_apply_comp([], _P, _RE, true, []) -> [];
split_apply_comp([], _P, _RE, _T, Sub) -> [reverse(Sub)];
split_apply_comp([C|Cs]=S, P0, RE, T, Sub) ->
    case comp_apply(S, P0, RE) of
	{match,P0,_S1} ->			%Ignore 0 length match
	    split_apply_comp(Cs, P0+1, RE, T, [C|Sub]);
	{match,P1,S1} ->
	    [reverse(Sub)|split_apply_comp(S1, P1, RE, T, [])];
	nomatch ->
	    split_apply_comp(Cs, P0+1, RE, T, [C|Sub])
    end.

%% sub_match(String, RegExp) ->
%%      {match,Start,Length,SubExprs} | nomatch | {error,E}.
%%  Find the longest match of RegExp in String.

sub_match(S, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> sub_match(S, RE);
	{error,E} -> {error,E}
    end;
sub_match(S, {regexp,RE}) ->
    case sub_match_re(RE, S, 1, 0, -1, none) of
	{Start,Len,Subs} when Len >= 0 ->
	    {match,Start,Len,Subs};
	{_Start,_Len,_Subs} -> nomatch
    end.

sub_match_re(RE, S0, Pos0, Mst, Mlen, Msubs) ->
    case first_match_re(RE, S0, Pos0) of
	{St,Len,Subs} ->			%Found a match
	    Pos1 = St + 1,			%Where to start next match
	    S1 = lists:nthtail(Pos1-Pos0, S0),
	    if Len > Mlen -> sub_match_re(RE, S1, Pos1, St, Len, Subs);
	       true -> sub_match_re(RE, S1, Pos1, Mst, Mlen, Msubs)
	    end;
	nomatch -> {Mst,Mlen,Msubs}
    end.

%% sub_first_match(String, RegExp) ->
%%       {match,Start,Length,SubExprs} | nomatch | {error,E}.
%%  Find the longest match of RegExp in String, return Start and Length
%%  as well as tuple of sub-expression matches.

sub_first_match(S, RegExp) when is_list(RegExp) ->
    {ok,RE} = parse(RegExp),
    sub_first_match(S, RE);
sub_first_match(S, {regexp,RE}) ->
    case first_match_re(RE, S, 1) of
	{St,Len,Subs} -> {match,St,Len,Subs};
	nomatch -> nomatch
    end.


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
%% reg3 -> reg3 "{" [Min],[Max] "}" : {closure_range, Num, '$1'} see below
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

reg(S, Sc) -> reg1(S, Sc).

%% reg1 -> reg2 reg1'
%% reg1' -> "|" reg2
%% reg1' -> empty

reg1(S0, Sc0) ->
    {L,Sc1,S1} = reg2(S0, Sc0),
    reg1p(S1, L, Sc1).

reg1p([$||S0], L, Sc0) ->
    {R,Sc1,S1} = reg2(S0, Sc0),
    reg1p(S1, {'or',L,R}, Sc1);
reg1p(S, L, Sc) -> {L,Sc,S}.

%% reg2 -> reg3 reg2'
%% reg2' -> reg3
%% reg2' -> empty

reg2(S0, Sc0) ->
    {L,Sc1,S1} = reg3(S0, Sc0),
    reg2p(S1, L, Sc1).

reg2p([C|S0], L, Sc0) when C /= $|, C /= $) ->
    {R,Sc1,S1} = reg3([C|S0], Sc0),
    %% reg2p(S1, {concat,L,R}, Sc1);
    case is_integer(R) of
 	true -> 
 	    case L of
 		{literal,Lit} ->
 		    reg2p(S1, {literal,Lit ++[R]}, Sc1);
 		{concat,S2,Char} when is_integer(Char) ->
 		    reg2p(S1, {concat,S2,{literal,[Char,R]}}, Sc1);
 		{concat,S2,{literal,Lit}}  ->
 		    reg2p(S1, {concat,S2,{literal,Lit ++ [R]}}, Sc1);
 		Char when is_integer(Char) -> 
 		    reg2p(S1, {literal,[Char,R]}, Sc1);
 		_ ->
 		    reg2p(S1, {concat,L,R}, Sc1)
 	    end;
 	false ->
 	    reg2p(S1, {concat,L,R}, Sc1)
    end;
reg2p(S, L, Sc) -> {L,Sc,S}.

%% reg3 -> reg4 reg3'
%% reg3' -> "*" reg3'
%% reg3' -> "+" reg3'
%% reg3' -> "?" reg3'
%% reg3' -> "{" [Min],[Max] "}" reg3'
%% reg3' -> empty

reg3(S0, Sc0) ->
    {L,Sc1,S1} = reg4(S0, Sc0),
    reg3p(S1, L, Sc1).

reg3p([$*|S], L, Sc) -> reg3p(S, {kclosure,L}, Sc);
reg3p([$+|S], L, Sc) -> reg3p(S, {pclosure,L}, Sc);
reg3p([$?|S], L, Sc) -> reg3p(S, {optional,L}, Sc);
reg3p([${|Cs0], L, Sc) ->			% $}
    case interval_range(Cs0) of
	{none,none,_Cs1} -> parse_error({interval_range,[${|Cs0]});
	{N,M,[$}|Cs1]} -> reg3p(Cs1, {iclosure,L,N,M}, Sc);
	{_N,_M,_Cs1} -> parse_error({unterminated,"{"})
    end;
reg3p(S, L, Sc) -> {L,Sc,S}.

reg4([$(|S0], Sc0) ->
    Sc1 = Sc0+1,
    case reg(S0, Sc1) of
	{R,Sc2,[$)|S1]} -> {{subexpr,Sc1,R},Sc2,S1};
	{_R,_Sc,_S} -> parse_error({unterminated,"("})
    end;
reg4([$^|S], Sc) -> {bos,Sc,S};
reg4([$$|S], Sc) -> {eos,Sc,S};
reg4([$.|S], Sc) -> {{comp_class,"\n"},Sc,S};
reg4("[^" ++ S0, Sc) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{comp_class,Cc},Sc,S1};
	{_Cc,_S} -> parse_error({unterminated,"["})
    end;
reg4([$[|S0], Sc) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{char_class,Cc},Sc,S1};
	{_Cc,_S1} -> parse_error({unterminated,"["})
    end;
%reg4([$"|S0], Sc) ->
%    case char_string(S0) of
%	{St,[$"|S1]} -> {St,Sc,S1};
%	{St,S1} -> parse_error({unterminated,"\""})
%    end;
reg4([C0|S0], Sc) when
  is_integer(C0), C0 /= $*, C0 /= $+, C0 /= $?, C0 /= $], C0 /= $), C0 /= $} ->
    %% Handle \ quoted characters as well, at least those we see.
    {C1,S1} = char(C0, S0),
    {C1,Sc,S1};
reg4(S=[$)|_], Sc) -> {epsilon,Sc,S};
reg4([C|_S], _Sc) -> parse_error({illegal,[C]});
reg4([], Sc) -> {epsilon,Sc,[]}.

char($\\, [O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
char($\\, [C|S]) -> {escape_char(C),S};
char($\\, []) -> parse_error({unterminated,"\\"});
char(C, S) -> {C,S}.

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

char_class([$]|S0]) ->
    {Cc,S1} = char_class(S0, [$]]),
    {pack_cc(Cc),S1};
char_class(S0) ->
    {Cc,S1} = char_class(S0, []),
    {pack_cc(Cc),S1}.

pack_cc(Cc0) ->
    %% First sort the list.
    Cc1 = lists:usort(fun ({Cf1,_}, {Cf2,_}) -> Cf1 < Cf2;
			  ({Cf1,_}, C) -> Cf1 < C;
			  (C, {Cf,_}) -> C < Cf;
			  (C1, C2) -> C1 =< C2
		      end, Cc0),
    pack_cc1(Cc1).

pack_cc1([{Cf1,Cl1},{Cf2,Cl2}|Cc]) when Cl1 >= Cf2, Cl1 =< Cl2 ->
    pack_cc1([{Cf1,Cl2}|Cc]);
pack_cc1([{Cf1,Cl1},{Cf2,Cl2}|Cc]) when Cl1 >= Cf2, Cl1 >= Cl2 ->
    pack_cc1([{Cf1,Cl1}|Cc]);
pack_cc1([{Cf1,Cl1},{Cf2,Cl2}|Cc]) when Cl1+1 == Cf2 ->
    pack_cc1([{Cf1,Cl2}|Cc]);
pack_cc1([{Cf,Cl},C|Cc]) when Cl >= C -> pack_cc1([{Cf,Cl}|Cc]);
pack_cc1([{Cf,Cl},C|Cc]) when Cl+1 == C -> pack_cc1([{Cf,C}|Cc]);
pack_cc1([C,{Cf,Cl}|Cc]) when C == Cf-1 -> pack_cc1([{C,Cl}|Cc]);
pack_cc1([C1,C2|Cc]) when C1+1 == C2 -> pack_cc1([{C1,C2}|Cc]);
pack_cc1([C|Cc]) -> [C|pack_cc1(Cc)];
pack_cc1([]) -> [].

char_class("[:" ++ S0, Cc0) ->			%Start of POSIX char class
    case posix_cc(S0, Cc0) of
	{Cc1,":]" ++ S1} -> char_class(S1, Cc1);
	{_,_S1} -> parse_error({posix_cc,"[:" ++ S0})
    end;
char_class([C1|S0], Cc) when C1 /= $] ->
    case char(C1, S0) of
	{Cf,[$-,C2|S1]} when C2 /= $] ->
	    case char(C2, S1) of
		{Cl,S2} when Cf < Cl -> char_class(S2, [{Cf,Cl}|Cc]); 
		{_Cl,_S2} -> parse_error({char_class,[C1|S0]})
	    end;
	{C,S1} -> char_class(S1, [C|Cc])
    end;
char_class(S, Cc) -> {Cc,S}.

%% posix_cc(String, CharClass) -> {NewCharClass,RestString}.
%%  Handle POSIX character classes, use Latin-1 character set.

posix_cc("alnum" ++ S, Cc) ->
    {[{$0,$9},{$A,$Z},{192,214},{216,223},{$a,$z},{224,246},{248,255}|Cc],S};
posix_cc("alpha" ++ S, Cc) ->
    {[{$A,$Z},{192,214},{216,223},{$a,$z},{224,246},{248,255}|Cc],S};
posix_cc("blank" ++ S, Cc) -> {[$\s,$\t,160|Cc],S};
posix_cc("cntrl" ++ S, Cc) -> {[{0,31},{127,159}|Cc],S};
posix_cc("digit" ++ S, Cc) -> {[{$0,$9}|Cc],S};
posix_cc("graph" ++ S, Cc) -> {[{33,126},{161,255}|Cc],S};
posix_cc("lower" ++ S, Cc) -> {[{$a,$z},{224,246},{248,255}|Cc],S};
posix_cc("print" ++ S, Cc) -> {[{32,126},{160,255}|Cc],S};
posix_cc("punct" ++ S, Cc) -> {[{$!,$/},{$:,$?},{${,$~},{161,191}|Cc],S};
posix_cc("space" ++ S, Cc) -> {[$\s,$\t,$\f,$\r,$\v,160|Cc],S};
posix_cc("upper" ++ S, Cc) -> {[{$A,$Z},{192,214},{216,223}|Cc],S};
posix_cc("xdigit" ++ S, Cc) -> {[{$a,$f},{$A,$F},{$0,$9}|Cc],S};
posix_cc(S, _Cc) -> parse_error({posix_cc,"[:" ++ S}).

interval_range(Cs0) ->
    case number(Cs0) of
	{none,Cs1} -> {none,none,Cs1};
	{N,[$,|Cs1]} ->
	    case number(Cs1) of
		{none,Cs2} -> {N,any,Cs2};
		{M,Cs2} -> {N,M,Cs2}
	    end;
	{N,Cs1} -> {N,none,Cs1}
    end.

number([C|Cs]) when C >= $0, C =< $9 ->
    number(Cs, C - $0);
number(Cs) -> {none,Cs}.

number([C|Cs], Acc) when C >= $0, C =< $9 ->
    number(Cs, 10*Acc + (C - $0));
number(Cs, Acc) -> {Acc,Cs}.

parse_error(E) -> throw({error,E}).

%char_string([C|S]) when C /= $" -> char_string(S, C);
%char_string(S) -> {epsilon,S}.

%char_string([C|S0], L) when C /= $" ->
%    char_string(S0, {concat,L,C});
%char_string(S, L) -> {L,S}.

%% re_apply(String, StartPos, RegExp) ->
%%      {match,RestPos,Rest,SubExprs} | nomatch.
%%
%%  Apply the (parse of the) regular expression RegExp to String.  If
%%  there is a match return the position of the remaining string and
%%  the string if else return 'nomatch'.
%%
%%  StartPos should be the real start position as it is used to decide
%%  if we are at the beginning of the string.

re_apply(S, St, {RE,Sc}) ->
    Subs = erlang:make_tuple(Sc, none),		%Make a sub-regexp table.
    Res = re_apply(RE, [], S, St, Subs),
    %% ?dbg("~p x ~p -> ~p\n", [RE,S,Res]),
    Res.

re_apply(epsilon, More, S, P, Subs) ->		%This always matches
    re_apply_more(More, S, P, Subs);
re_apply({'or',RE1,RE2}, More, S, P, Subs) ->
    re_apply_or(re_apply(RE1, More, S, P, Subs),
		re_apply(RE2, More, S, P, Subs));
re_apply({concat,RE1,RE2}, More, S0, P, Subs) ->
    re_apply(RE1, [RE2|More], S0, P, Subs);
re_apply({literal,[C|Lcs]}, More, [C|S], P, Subs) ->
    re_apply_lit(Lcs, More, S, P+1, Subs);	%Have matched first char
re_apply({kclosure,RE}, More, S0, P0, Subs0) ->
    %% Greedy so try RE first, no difference here actually.
    Loop = case re_apply(RE, [], S0, P0, Subs0) of
	       {match,P0,_S1,_Subs1} ->		%0 length match, don't loop!
		   nomatch;
	       {match,P1,S1,Subs1} ->
		   re_apply_more([{kclosure,RE}|More], S1, P1, Subs1);
	       nomatch -> nomatch;
	       never_match -> never_match
	   end,
    re_apply_or(Loop, re_apply_more(More, S0, P0, Subs0));
re_apply({pclosure,RE}, More, S, P, Subs) ->
    re_apply(RE, [{kclosure,RE}|More], S, P, Subs);
re_apply({optional,RE}, More, S, P, Subs) ->
    %% Greedy so try RE first, no difference here actually.
    re_apply_or(re_apply(RE, More, S, P, Subs),
		re_apply_more(More, S, P, Subs));
re_apply({iclosure,RE,N,M}, More, S, P, Subs) when N > 0 ->
    re_apply(RE, [{iclosure,RE,N-1,M}|More], S, P, Subs);
re_apply({iclosure,RE,0,M}, More, S, P, Subs) ->
    Exp = expand_opt(RE, M),
    re_apply(Exp, More, S, P, Subs);
re_apply({subexpr,N,RE}, More, S, P, Subs) ->
    re_apply(RE, [{endsub,N,P}|More], S, P, Subs);
re_apply({endsub,N,St}, More, S, P, Subs0) ->
    Subs1 = setelement(N, Subs0, {St,P-St}),	%Record sub-expr
    re_apply_more(More, S, P, Subs1);
re_apply(bos, More, S, 1, Subs) -> re_apply_more(More, S, 1, Subs);
re_apply(bos, _More, _S, _, _) -> never_match;
re_apply(eos, More, [$\n], P, Subs) -> re_apply_more(More, [], P, Subs);
re_apply(eos, More, [], P, Subs) -> re_apply_more(More, [], P, Subs);
re_apply({char_class,Cc}, More, [C|S], P, Subs) ->
    case in_char_class(C, Cc) of
	true -> re_apply_more(More, S, P+1, Subs);
	false -> nomatch
    end;
re_apply({comp_class,Cc}, More, [C|S], P, Subs) ->
    case in_char_class(C, Cc) of
	true -> nomatch;
	false -> re_apply_more(More, S, P+1, Subs)
    end;
re_apply(C, More, [C|S], P, Subs) when is_integer(C) ->
    re_apply_more(More, S, P+1, Subs);
re_apply(_RE, _More, _S, _P, _Subs) ->
    %% ?dbg("~p : ~p\n", [_RE,_S]),
    nomatch.

%% re_apply_more([RegExp], String, Length, SubsExprs) ->
%%      {match,RestPos,Rest,SubExprs} | nomatch.

re_apply_more([RE|More], S, P, Subs) -> re_apply(RE, More, S, P, Subs);
re_apply_more([], S, P, Subs) -> {match,P,S,Subs}.

%% re_apply_lit(Literal, More, String, Position, SubExprs) ->
%%      {match,RestPos,Rest,SubExprs} | nomatch.
re_apply_lit([C|Lit], More, [C|Cs], P, Subs) ->
    re_apply_lit(Lit, More, Cs, P+1, Subs);
re_apply_lit([], More, Cs, P, Subs) ->
    re_apply_more(More, Cs, P, Subs);
re_apply_lit(_Lit, _More, _Cs, _P, _Subs) ->
    nomatch.

%% expand_iclosure(RE, N, M) -> RE.

expand_iclosure(RE, 0, M) -> expand_opt(RE, M);
expand_iclosure(RE, N, M) ->
    {concat,RE,expand_iclosure(RE, N-1, M)}.

%% expand_opt(RegExp, Count) -> RE.
%% Handle all the cases.

expand_opt(_RE, none) -> epsilon;
expand_opt(RE, any) -> {kclosure,RE};
expand_opt(_RE, 0) -> epsilon;
expand_opt(RE, 1) -> {optional,RE};
expand_opt(RE, N) ->
    {optional,{concat,RE,expand_opt(RE, N-1)}}.

%% find_prefix(PrefixStr, SourceStr)
%% if PrefixStr is a prefix of Str then return {ok,RemainingStr}
%% otherwise return false

%% find_prefix([C|Prest], [C|Rest]) ->
%%     find_prefix(Prest, Rest);
%% find_prefix([], Rest) -> {yes,Rest};
%% find_prefix(_, _) -> no.

%% in_char_class(Char, Class) -> bool().

in_char_class(C, [{C1,C2}|_Cc]) when C >= C1, C =< C2 -> true;
in_char_class(C, [C|_Cc]) -> true;
in_char_class(C, [_|Cc]) -> in_char_class(C, Cc);
in_char_class(_C, []) -> false.

%% re_apply_or(Match1, Match2, SubExprs) ->
%%      {match,RestPos,Rest,SubExprs} | nomatch.
%%  If we want the best match then choose the longest match, else just
%%  choose one by trying sequentially.

re_apply_or(M1={match,P1,_,_},{match,P2,_,_}) when P1 >= P2 -> M1;
re_apply_or({match,_,_,_},  M2={match,_,_,_}) -> M2;
re_apply_or(never_match, R2) -> R2;
re_apply_or(R1, never_match) -> R1;
re_apply_or(nomatch, R2) -> R2;
re_apply_or(R1, nomatch) -> R1.

%% Record definitions for the NFA, DFA and compiler.

-record(nfa_state, {no,edges=[],accept=no}).
-record(dfa_state, {no,nfa=[],trans=[],accept=no}).

-record(c_state, {no,trans=[],tmin=0,smin=none,tmax=0,smax=none,
		  accept=false,spec=[]}).

%% We use standard methods, Thompson's construction and subset
%% construction, to create first an NFA and then a DFA from the
%% regexps. A non-standard feature is that we work with sets of
%% character ranges (crs) instead sets of characters. This is most
%% noticeable when constructing DFAs. The major benefit is that we can
%% handle characters from any set, not just limited ASCII or 8859,
%% even 16/32 bit unicode.
%%
%% The whole range of characters is 0-maxchar, where maxchar is a BIG
%% number. We don't make any assumptions about the size of maxchar, it
%% is just bigger than any character.
%%
%% Using character ranges makes describing many regexps very simple,
%% for example the regexp "." just becomes the range
%% [{0-9},{11-maxchar}].

%% make_nfa(RegExpActions) -> {ok,{NFA,StartState}} | {error,E}.
%% Build a complete nfa from a list of {RegExp,Action}. The NFA field
%% accept has values {yes,Action}|no. The NFA is a list of states.

make_nfa(REAs0) ->
    case parse_reas(REAs0) of
	{ok,REAs1} ->
	    {NFA,Start} = build_combined_nfa(REAs1),
	    {ok,{NFA,Start}};
	{error,E} -> {error,E}
    end.

%% make_dfa(RegExpActions) -> {ok,{DFA,StartState}} | {error,E}.
%% make_dfa(RegExpActions, LowestState) -> {ok,{DFA,StartState}} | {error,E}.
%% Build a complete dfa from a list of {RegExp,Action}. The DFA field
%% accept has values {yes,Action}|no. If multiple Regexps can result
%% in same match string then RegExpActions list define priority.

make_dfa(REAs) -> make_dfa(REAs, 0).

make_dfa(REAs0, Low) ->
    case parse_reas(REAs0) of
	{ok,REAs1} ->
	    {NFA,Start0} = build_combined_nfa(REAs1),
	    {DFA0,Start1} = build_dfa(NFA, Start0),
	    {DFA,Start} = minimise_dfa(DFA0, Start1, Low),
	    {ok,{DFA,Start}};
	{error,E} -> {error,E}
    end.

parse_reas(REAs) -> parse_reas(REAs, []).

parse_reas([{{regexp,{R,_Sc}},A}|REAs], S) ->	%Already parsed
    parse_reas(REAs, [{R,A}|S]);
parse_reas([{RegExp,A}|REAs], S) ->
    case parse(RegExp) of
	{ok,{regexp,{R,_Sc}}} -> parse_reas(REAs, [{R,A}|S]);
	{error,E} -> {error,E}
    end;
parse_reas([], Stack) -> {ok,reverse(Stack)}.

%% build_combined_nfa(RegExpActionList) -> {NFA,StartState}.
%%  Build the combined NFA using Thompson's construction straight out
%%  of the book. Build the separate NFAs in the same order as the
%%  rules so that the accepting have ascending states have ascending
%%  state numbers.  Start numbering the states from 1 as we put the
%%  states in a tuple with the state number as the index.

build_combined_nfa(REAs) ->
    {NFA,Starts,Next} = build_nfa_list(REAs, [], [], 1),
    F = #nfa_state{no=Next,edges=epsilon_trans(Starts),accept=no},
    {[F|NFA],Next}.

build_nfa_list([{RE,Action}|REAs], NFA0, Starts, Next0) ->
    {NFA1,Next1,Start} = build_nfa(RE, Next0, Action),
    build_nfa_list(REAs, NFA1 ++ NFA0, [Start|Starts], Next1);
build_nfa_list([], NFA, Starts, Next) ->
    {NFA,reverse(Starts),Next}.

epsilon_trans(Sts) -> [ {epsilon,S} || S <- Sts ].

%% build_nfa(RegExp, NextState, Action) -> {NFA,NextFreeState,StartState}.
%%  When building the NFA states for a ??? we don't build the end
%%  state, just allocate a State for it and return this state
%%  number. This allows us to avoid building unnecessary states for
%%  concatenation which would then have to be removed by overwriting
%%  an existing state.

build_nfa(RE, Next, Action) ->
    {NFA,N,E} = build_nfa(RE, Next+1, Next, []),
    {[#nfa_state{no=E,accept={yes,Action}}|NFA],N,Next}.

%% build_nfa(RegExp, NextState, StartState, NFA) -> {NFA,NextState,EndState}.
%%  The NFA is a list of nfa_state is no predefined order. The state
%%  number of the returned EndState is already allocated!

build_nfa({'or',RE1,RE2}, N0, S, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE1, N0+1, N0, NFA0),
    {NFA2,N2,E2} = build_nfa(RE2, N1+1, N1, NFA1),
    E = N2,
    {[#nfa_state{no=S,edges=[{epsilon,N0},{epsilon,N1}]},
      #nfa_state{no=E1,edges=[{epsilon,E}]},
      #nfa_state{no=E2,edges=[{epsilon,E}]}|NFA2],
     N2+1,E};
build_nfa({literal,[]}, N, S, NFA) ->
    {NFA,N,S};
build_nfa({literal,[C|Cs]}, N0, S, NFA0) ->
    {NFA1,N1,E1} = build_nfa(C, N0, S, NFA0),
    build_nfa({literal,Cs}, N1, E1, NFA1);
build_nfa({concat,RE1,RE2}, N0, S, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE1, N0, S, NFA0),
    {NFA2,N2,E2} = build_nfa(RE2, N1, E1, NFA1),
    {NFA2,N2,E2};
build_nfa({kclosure,RE}, N0, S, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE, N0+1, N0, NFA0),
    E = N1,
    {[#nfa_state{no=S,edges=[{epsilon,N0},{epsilon,E}]},
      #nfa_state{no=E1,edges=[{epsilon,N0},{epsilon,E}]}|NFA1],
     N1+1,E};
build_nfa({pclosure,RE}, N0, S, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE, N0+1, N0, NFA0),
    E = N1,
    {[#nfa_state{no=S,edges=[{epsilon,N0}]},
      #nfa_state{no=E1,edges=[{epsilon,N0},{epsilon,E}]}|NFA1],
     N1+1,E};
build_nfa({optional,RE}, N0, S, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE, N0+1, N0, NFA0),
    E = N1,
    {[#nfa_state{no=S,edges=[{epsilon,N0},{epsilon,E}]},
      #nfa_state{no=E1,edges=[{epsilon,E}]}|NFA1],
     N1+1,E};
build_nfa({iclosure,RE,I1,I2}, N, S, NFA) ->
    Exp = expand_iclosure(RE, I1, I2),
    build_nfa(Exp, N, S, NFA);
build_nfa({char_class,Cc}, N, S, NFA) ->
    {[#nfa_state{no=S,edges=[{nfa_char_class(Cc),N}]}|NFA],N+1,N};
build_nfa({comp_class,Cc}, N, S, NFA) ->
    {[#nfa_state{no=S,edges=[{nfa_comp_class(Cc),N}]}|NFA],N+1,N};
build_nfa(epsilon, N, S, NFA) ->
    {NFA,N,S};
build_nfa({group,RE}, N, S, NFA) ->		%%% FIXME %%%%%%%
    build_nfa(RE, N, S, NFA);
build_nfa({subexpr,_N,RE}, N, S, NFA) ->	%%% FIXME %%%%%%%
    build_nfa(RE, N, S, NFA);
build_nfa(bos, N, S, NFA) ->
    {[#nfa_state{no=S,edges=[{[bos],N}]}|NFA],N+1,N};
build_nfa(eos, N, S, NFA) ->
    {[#nfa_state{no=S,edges=[{[eos],N}]}|NFA],N+1,N};
%%{[#nfa_state{no=S,edges=[{[eos],N}]}|NFA],N+1,N};
build_nfa(C, N, S, NFA) when is_integer(C) ->
    {[#nfa_state{no=S,edges=[{[{C,C}],N}]}|NFA],N+1,N}.

nfa_char_class(Cc) ->
    Crs = lists:foldl(fun({C1,C2}, Set) -> add_element({C1,C2}, Set);
			 (C, Set) -> add_element({C,C}, Set) end, [], Cc),
    %% ?dbg("cc: ~p\n", [Crs]),
    pack_crs(Crs).

pack_crs([{C1,C2}=Cr,{C3,C4}|Crs]) when C1 =< C3, C2 >= C4 ->
    %% C1      C2
    %%   C3  C4
    pack_crs([Cr|Crs]);
pack_crs([{C1,C2},{C3,C4}|Crs]) when C2 >= C3, C2 < C4 ->
    %% C1    C2
    %%    C3   C4
    pack_crs([{C1,C4}|Crs]);
pack_crs([{C1,C2},{C3,C4}|Crs]) when C2 + 1 == C3 ->
    %% C1   C2
    %%        C3  C4
    pack_crs([{C1,C4}|Crs]);
pack_crs([Cr|Crs]) -> [Cr|pack_crs(Crs)];
pack_crs([]) -> [].

nfa_comp_class(Cc) ->
    Crs = nfa_char_class(Cc),
    %% ?dbg("comp: ~p\n", [Crs]),
    comp_crs(Crs, 0).

comp_crs([{C1,C2}|Crs], Last) ->
    [{Last,C1-1}|comp_crs(Crs, C2+1)];
comp_crs([], Last) -> [{Last,maxchar}].

%% build_dfa(NFA, NfaStartState) -> {DFA,DfaStartState}.
%%  Build a DFA from an NFA using "subset construction". The major
%%  difference from the book is that we keep the marked and unmarked
%%  DFA states in seperate lists. New DFA states are added to the
%%  unmarked list and states are marked by moving them to the marked
%%  list. We assume that the NFA accepting state numbers are in
%%  ascending order for the rules and use ordsets to keep this order.

build_dfa(NFA0, Start) ->
    %% We want NFA as sorted tuple for fast access, assume lowest state 1.
    NFA1 = list_to_tuple(keysort(#nfa_state.no, NFA0)),
    D = #dfa_state{no=0,nfa=eclosure([Start], NFA1),accept=no},
    {build_dfa([D], 1, [], NFA1),0}.

%% build_dfa([UnMarked], NextState, [Marked], NFA) -> DFA.
%%  Traverse the unmarked states. Temporarily add the current unmarked
%%  state to the marked list before calculating translation, this is
%%  to avoid adding too many duplicate states. Add it properly to the
%%  marked list afterwards with correct translations.

build_dfa([U|Us0], N0, Ms, NFA) ->
    {Ts,Us1,N1} = build_dfa(U#dfa_state.nfa, Us0, N0, [], [U|Ms], NFA),
    M = U#dfa_state{trans=Ts,accept=accept(U#dfa_state.nfa, NFA)},
    build_dfa(Us1, N1, [M|Ms], NFA);
build_dfa([], _N, Ms, _NFA) -> Ms.

%% build_dfa([NfaState], [Unmarked], NextState, [Transition], [Marked], NFA) ->
%%	{Transitions,UnmarkedStates,NextState}.
%%  Foreach NFA state set calculate the legal translations. N.B. must
%%  search *BOTH* the unmarked and marked lists to check if DFA state
%%  already exists. As the range of characters is potentially VERY
%%  large we cannot explicitly test all characters. Instead we first
%%  calculate the set of all disjoint character ranges which are
%%  possible candidates to the set of NFA states.

build_dfa(Set, Us, N, Ts, Ms, NFA) ->
    %% List of all transition sets.
    Crs0 = [Cr || S <- Set,
		  {Crs,_St} <- (element(S, NFA))#nfa_state.edges,
		 is_list(Crs),
		  Cr <- Crs ],
    Crs1 = lists:usort(Crs0),			%Must remove duplicates!
    %% Build list of disjoint test ranges.
    Test = disjoint_crs(Crs1),
    %% ?dbg("bd: ~p\n    ~p\n    ~p\n    ~p\n", [Set,Crs0,Crs1,Test]),
    build_dfa(Test, Set, Us, N, Ts, Ms, NFA).

%% disjoint_crs([CharRange]) -> [CharRange].
%%  Take a sorted list of char ranges and make a sorted list of
%%  disjoint char ranges. No new char range extends past an existing
%%  char range.

disjoint_crs([{_C1,C2}=Cr1,{C3,_C4}=Cr2|Crs]) when C2 < C3 ->
    %% C1  C2
    %%        C3  C4
    [Cr1|disjoint_crs([Cr2|Crs])];
disjoint_crs([{C1,C2},{C3,C4}|Crs]) when C1 == C3 ->
    %% C1     C2
    %% C3       C4
    [{C1,C2}|disjoint_crs(add_element({C2+1,C4}, Crs))];
disjoint_crs([{C1,C2},{C3,C4}|Crs]) when C1 < C3, C2 >= C3, C2 < C4 ->
    %% C1     C2
    %%    C3     C4
    [{C1,C3-1}|disjoint_crs(union([{C3,C2},{C2+1,C4}], Crs))];
disjoint_crs([{C1,C2},{C3,C4}|Crs]) when C1 < C3, C2 == C4 ->
    %% C1      C2
    %%    C3   C4
    [{C1,C3-1}|disjoint_crs(add_element({C3,C4}, Crs))];
disjoint_crs([{C1,C2},{C3,C4}|Crs]) when C1 < C3, C2 > C4 ->
    %% C1        C2
    %%    C3   C4
    [{C1,C3-1}|disjoint_crs(union([{C3,C4},{C4+1,C2}], Crs))];
disjoint_crs([Cr|Crs]) -> [Cr|disjoint_crs(Crs)];
disjoint_crs([]) -> [].

build_dfa([Cr|Crs], Set, Us, N, Ts, Ms, NFA) ->
    case eclosure(move(Set, Cr, NFA), NFA) of
	S when S /= [] ->
	    case keysearch(S, #dfa_state.nfa, Us) of
		{value,#dfa_state{no=T}} ->
		    build_dfa(Crs, Set, Us, N, [{Cr,T}|Ts], Ms, NFA);
		false ->
		    case keysearch(S, #dfa_state.nfa, Ms) of
			{value,#dfa_state{no=T}} ->
			    build_dfa(Crs, Set, Us, N, [{Cr,T}|Ts], Ms, NFA);
			false ->
			    U = #dfa_state{no=N,nfa=S},
			    build_dfa(Crs, Set, [U|Us], N+1, [{Cr,N}|Ts], Ms, NFA)
		    end
	    end;
	[] ->
	    build_dfa(Crs, Set, Us, N, Ts, Ms, NFA)
    end;
build_dfa([], _Set, Us, N, Ts, _Ms, _NFA) ->
    {Ts,Us,N}.
   
%% eclosure([State], NFA) -> [State].
%% move([State], Char, NFA) -> [State].
%%  These are straight out of the book. As eclosure uses ordsets then
%%  the generated state sets are in ascending order.

eclosure(Sts, NFA) -> eclosure(Sts, NFA, []).

eclosure([St|Sts], NFA, Ec) ->
    #nfa_state{edges=Es} = element(St, NFA),
    eclosure([ N || {epsilon,N} <- Es,
		    not is_element(N, Ec) ] ++ Sts,
	     NFA, add_element(St, Ec));
eclosure([], _NFA, Ec) -> Ec.

move(Sts, Cr, NFA) ->
    [ St || N <- Sts,
	    {Crs,St} <- (element(N, NFA))#nfa_state.edges,
	   is_list(Crs),
%% 	    begin
%% 		?dbg("move1: ~p\n", [{Sts,Cr,Crs,in_crs(Cr,Crs)}]),
%% 		true
%% 	    end,
	    in_crs(Cr, Crs) ].

in_crs({C1,C2}, [{C3,C4}|_Crs]) when C1 >= C3, C2 =< C4 -> true;
in_crs(Cr, [Cr|_Crs]) -> true;			%Catch bos and eos.
in_crs(Cr, [_|Crs]) -> in_crs(Cr, Crs);
in_crs(_Cr, []) -> false.

%% accept([State], NFA) -> true | false.
%%  Scan down the state list until we find an accepting state.

accept([St|Sts], NFA) ->
    case element(St, NFA) of
	#nfa_state{accept={yes,A}} -> {yes,A};
	#nfa_state{accept=no} -> accept(Sts, NFA)
    end;
accept([], _NFA) -> no.

%% minimise_dfa(DFA, StartState, FirstState) -> {DFA,StartState}.
%%  Minimise the DFA by removing equivalent states. We consider a
%%  state if both the transitions and the their accept state is the
%%  same.  First repeatedly run throught the DFA state list removing
%%  equivalent states and updating remaining transitions with
%%  remaining equivalent state numbers. When no more reductions are
%%  possible then pack the remaining state numbers to get consecutive
%%  states.

minimise_dfa(DFA0, Start, N) ->
    case min_dfa(DFA0) of
	{DFA1,[]} ->				%No reduction!
	    {DFA2,Rs} = pack_dfa(DFA1, N),
	    {min_update(DFA2, Rs),min_new_state(Start, Rs)};
	{DFA1,Rs} ->
	    minimise_dfa(min_update(DFA1, Rs), min_new_state(Start, Rs), N)
    end.

min_dfa(DFA) -> min_dfa(DFA, [], []).

min_dfa([D|DFA0], Rs0, MDFA) ->
    {DFA1,Rs1} = min_delete(DFA0, D#dfa_state.trans, D#dfa_state.accept, 
			    D#dfa_state.no, Rs0, []),
    min_dfa(DFA1, Rs1, [D|MDFA]);
min_dfa([], Rs, MDFA) -> {MDFA,Rs}.

min_delete([#dfa_state{no=N,trans=T,accept=A}|DFA], T, A, NewN, Rs, MDFA) ->
    min_delete(DFA, T, A, NewN, [{N,NewN}|Rs], MDFA);
min_delete([D|DFA], T, A, NewN, Rs, MDFA) ->
    min_delete(DFA, T, A, NewN, Rs, [D|MDFA]);
min_delete([], _T, _A, _NewN, Rs, MDFA) -> {MDFA,Rs}.

min_update(DFA, Rs) ->
    [ D#dfa_state{trans=min_update_trans(D#dfa_state.trans, Rs)} || D <- DFA ].

min_update_trans(Tr, Rs) ->
    [ {C,min_new_state(S, Rs)} || {C,S} <- Tr ].

min_new_state(Old, [{Old,New}|_Reds]) -> New;
min_new_state(Old, [_R|Reds]) -> min_new_state(Old, Reds);
min_new_state(Old, []) -> Old.

pack_dfa(DFA, N) -> pack_dfa(DFA, N, [], []).

pack_dfa([D|DFA], NewN, Rs, PDFA) ->
    pack_dfa(DFA, NewN+1, [{D#dfa_state.no,NewN}|Rs],
	     [D#dfa_state{no=NewN}|PDFA]);
pack_dfa([], _NewN, Rs, PDFA) -> {PDFA,Rs}.

%% comp_apply(String, StartPos, DFAReg) -> {match,RestPos,Rest} | nomatch.
%% Apply the DFA of a regular expression to a string.  If
%%  there is a match return the position of the remaining string and
%%  the string if else return 'nomatch'.
%%
%%  StartPos should be the real start position as it is used to decide
%%  if we are at the beginning of the string.

comp_apply(Cs, P, {DFA,Start,_Fail}) ->
    comp_apply(element(Start, DFA), Cs, P, DFA, nomatch).

comp_apply(#c_state{spec=[]}=St, Cs, P, DFA, Accept) ->
    comp_apply_tr(St, Cs, P, DFA, Accept);
comp_apply(#c_state{spec=Sp}=St, Cs, P, DFA, Accept) ->
    comp_apply_sp(St, Cs, P, DFA, Accept, Sp).

comp_apply_tr(#c_state{trans=none,accept=A}, Cs, P, _DFA, Accept) ->
    %% End state.
    accept_value(A, Cs, P, Accept);
comp_apply_tr(#c_state{trans=Tr,tmin=Tmin,smin=Smin,tmax=Tmax,smax=Smax,accept=A},
	      [C|Cs]=Cs0, P, DFA, Accept) ->
    %% Get the next state number to go to.
    NextSt = if  C =< Tmin -> Smin;		%Below transition table
		 C >= Tmax -> Smax;		%Above transition table
		 true ->			%Otherwise use table
 		     element(C - Tmin, Tr)
	     end,
    comp_apply(element(NextSt, DFA), Cs, P+1, DFA,
	       accept_value(A, Cs0, P, Accept));
comp_apply_tr(#c_state{trans=_Tr,accept=A}, [], P, _DFA, Accept) ->
    accept_value(A, [], P, Accept).

comp_apply_sp(_St, Cs, 1, DFA, Accept, [{bos,S}|_]) ->
    comp_apply(element(S, DFA), Cs, 1, DFA, Accept);
comp_apply_sp(_St, [$\n], P, DFA, Accept, [{eos,S}|_]) ->
    comp_apply(element(S, DFA), [], P, DFA, Accept);
comp_apply_sp(_St, [], P, DFA, Accept, [{eos,S}|_]) ->
    comp_apply(element(S, DFA), [], P, DFA, Accept);
comp_apply_sp(St, Cs, P, DFA, Accept, [_|Sp]) ->
    comp_apply_sp(St, Cs, P, DFA, Accept, Sp);
comp_apply_sp(St, Cs, P, DFA, Accept, []) ->
    comp_apply_tr(St, Cs, P, DFA, Accept).
    
accept_value(true, Cs, P, _Accept) -> {match,P,Cs};
accept_value(false, _Cs, _P, Accept) -> Accept.

%% compile(RegExp) -> {ok,RE} | {error,E}.
%%  Parse the regexp described in the string RegExp.

compile(RegExp) ->
    case make_dfa([{RegExp,yes}], 2) of
	{ok,{DFA0,Start}} ->
	    Fail = 1,
	    DFA1 = [#dfa_state{no=Fail,accept=no,trans=[]}|DFA0],
	    DFA = tuplelise_dfa(DFA1, 1),
	    {ok,{comp_regexp,{DFA,Start,Fail}}};
	{error,E} -> {error,E}
    end.

%% tuplelise_dfa(DFAstates, NoAcceptState) -> {{CompState},FirstState}.

tuplelise_dfa(DFA0, NoAccept) ->
    DFA1 = map(fun (#dfa_state{no=N,trans=Ts,accept=A}) ->
		       {Tr,Tmin,Smin,Tmax,Smax,Sp} = build_trans(Ts, NoAccept),
		       #c_state{no=N,trans=Tr,tmin=Tmin,smin=Smin,
				tmax=Tmax,smax=Smax,
				accept=fix_accept(A),spec=Sp}
	       end, DFA0),
    list_to_tuple(keysort(#dfa_state.no, DFA1)).

build_trans(Ts0, NoAccept) ->
    %% Split transitions into character ranges and specials.
    {Ts1,Sp1} = foldl(fun ({{_,_},_}=T, {Ts,Sp}) -> {[T|Ts],Sp};
			  ({_,_}=T, {Ts,Sp}) -> {Ts,[T|Sp]}
		      end, {[],[]}, Ts0),
    if Ts1 == [] ->
	    {none,none,none,none,none,Sp1};
       true ->
	    %% Have transitions, convert to tuple.
	    Ts2 = keysort(1, Ts1),
	    {Tmin,Smin,Ts3} = min_trans(Ts2, NoAccept),
	    %% ?dbg("exptr: ~p\n", [{Ts3,Tmin}]),
	    {Trans,Tmax,Smax} = expand_trans(Ts3, Tmin, NoAccept),
	    {list_to_tuple(Trans),Tmin,Smin,Tmax,Smax,Sp1}
    end.
   
min_trans([{{0,C2},S}|Crs], _Def) -> {C2,S,Crs};
min_trans([{{C1,_C2},_S}|_]=Crs, Def) -> {C1-1,Def,Crs}.

expand_trans([{{C1,maxchar},S}], Last, Def) ->
    Trs = duplicate(C1-(Last+1), Def),
    {Trs,C1,S};
expand_trans([{{C1,C2},S}], Last, Def) ->
    Trs = duplicate(C1-(Last+1), Def) ++ duplicate(C2-C1+1, S),
    {Trs,C2+1,Def};
expand_trans([{{C1,C2},S}|Crs], Last, Def) ->
    {Trs0,Tmax,Smax} = expand_trans(Crs, C2, Def),
    Trs1 = duplicate(C1-(Last+1), Def) ++ duplicate(C2-C1+1, S) ++ Trs0,
    {Trs1,Tmax,Smax}.

fix_accept({yes,_}) -> true;
fix_accept(no) -> false.


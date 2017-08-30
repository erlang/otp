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
-module(asn1ct_tok).

%% Tokenize ASN.1 code (input to parser generated with yecc)   

-export([file/1,format_error/1]).

file(File0) ->
    case file:open(File0, [read])  of
	{error, Reason} ->
	    {error,{File0,file:format_error(Reason)}};
	{ok,Stream} ->
	    try
		process(Stream, 1, [])
	    catch
		throw:{error,Line,Reason} ->
		    File = filename:basename(File0),
		    Error = {structured_error,{File,Line},?MODULE,Reason},
		    {error,[Error]}
	    end
    end.

process(Stream, Lno, R) ->
    process(io:get_line(Stream, ''), Stream, Lno, R).

process(eof, Stream, Lno, Acc) ->
    ok = file:close(Stream),
    lists:reverse([{'$end',Lno}|Acc]);
process(L, Stream, Lno0, Acc) when is_list(L) ->
    try tokenise(Stream, L, Lno0, []) of
	{Lno,[]} ->
	    process(Stream, Lno, Acc);
	{Lno,Ts} ->
	    process(Stream, Lno, Ts++Acc)
    catch
	throw:{error,Reason} ->
	    throw({error,Lno0,Reason})
    end.

format_error(eof_in_comment) ->
    "premature end of file in multi-line comment";
format_error(eol_in_token) ->
    "end of line in token";
format_error({invalid_binary_number,Str}) ->
    io_lib:format("invalid binary number: '~s'", [Str]);
format_error({invalid_hex_number,Str}) ->
    io_lib:format("invalid hex number: '~s'", [Str]);
format_error(Other) ->
    io_lib:format("~p", [Other]).

tokenise(Stream, [$&,H|T], Lno, R) when $A =< H , H =< $Z ->
    {X,T1} = get_name(T, [H]),
    tokenise(Stream, T1, Lno, [{typefieldreference,Lno,X}|R]);
tokenise(Stream, [$&,H|T], Lno, R) when $a =< H , H =< $z ->
    {X,T1} = get_name(T, [H]),
    tokenise(Stream, T1, Lno, [{valuefieldreference,Lno,X}|R]);

tokenise(Stream, "--"++T, Lno, R) ->
    tokenise(Stream, skip_comment(T), Lno, R);

tokenise(Stream, [$-,H|T], Lno, R) when $0 =< H , H =< $9 ->
    {X, T1} = get_number(T, [H]),
    tokenise(Stream, T1, Lno, [{number,Lno,-list_to_integer(X)}|R]);

tokenise(Stream, "/*"++T, Lno0, R) ->
    {Lno,T1} = skip_multiline_comment(Stream, T, Lno0, 0),
    tokenise(Stream, T1, Lno, R);

tokenise(Stream, "::="++T, Lno, R) ->
    tokenise(Stream, T, Lno, [{'::=',Lno}|R]);
tokenise(Stream, ":"++T, Lno, R) ->
    tokenise(Stream, T, Lno, [{':',Lno}|R]);

tokenise(Stream, "'"++T0, Lno, R) ->
    {Thing, T1} = collect_quoted(T0, Lno, []),
    tokenise(Stream, T1, Lno, [Thing|R]);
tokenise(Stream,[$"|T],Lno,R) ->
    {Str,T1} = collect_string(T,Lno),
    tokenise(Stream,T1,Lno,[Str|R]);

tokenise(Stream, "{"++T, Lno, R) ->
    tokenise(Stream, T, Lno, [{'{',Lno}|R]);
tokenise(Stream, "}"++T, Lno, R) ->
    tokenise(Stream, T, Lno, [{'}',Lno}|R]);

%% Even though x.680 specify '[[' and ']]' as lexical items
%% it does not work to have them as such since the single '[' and ']' can
%% be used beside each other in 'WITH SYNTAX' in x.681.
%% The solution chosen here, i.e. to have them as separate lexical items
%% will not detect the cases where there is white space between them
%% which would be an error in the use in ExtensionAdditionGroups.

tokenise(Stream, "]"++T, Lno, R) ->
    tokenise(Stream, T, Lno, [{']',Lno}|R]);
tokenise(Stream, "["++T,Lno,R) ->
    tokenise(Stream, T, Lno, [{'[',Lno}|R]);

tokenise(Stream, ","++T,Lno,R) ->
    tokenise(Stream, T, Lno, [{',',Lno}|R]);

tokenise(Stream, "("++T, Lno, R) ->
    tokenise(Stream, T, Lno, [{'(',Lno}|R]);
tokenise(Stream, ")"++T, Lno, R) ->
    tokenise(Stream, T, Lno, [{')',Lno}|R]);

tokenise(Stream, "..."++T,Lno,R) ->
    tokenise(Stream, T, Lno, [{'...',Lno}|R]);
tokenise(Stream, ".."++T, Lno, R) ->
    tokenise(Stream, T, Lno, [{'..',Lno}|R]);
tokenise(Stream, "."++T, Lno, R) ->
    tokenise(Stream, T, Lno, [{'.',Lno}|R]);

tokenise(Stream, "|"++T, Lno, R) ->
    tokenise(Stream, T, Lno, [{'|',Lno}|R]);

tokenise(Stream, [H|T], Lno, R) when $A =< H , H =< $Z ->
    {X,T1} = get_name(T, [H]),
    case reserved_word(X) of
	true ->
	    tokenise(Stream, T1, Lno, [{X,Lno}|R]);
	false ->
	    tokenise(Stream, T1, Lno, [{typereference,Lno,X}|R]);
	rstrtype ->
	    tokenise(Stream, T1, Lno, [{restrictedcharacterstringtype,Lno,X}|R])
    end;

tokenise(Stream, [H|T], Lno, R) when $a =< H , H =< $z ->
    {X, T1} = get_name(T, [H]),
    tokenise(Stream, T1, Lno, [{identifier,Lno,X}|R]);

tokenise(Stream, [H|T], Lno, R) when $0 =< H , H =< $9 ->
    {X, T1} = get_number(T, [H]),
    tokenise(Stream, T1, Lno, [{number,Lno,list_to_integer(X)}|R]);

tokenise(Stream, [H|T], Lno, R) when H =< $\s ->
    tokenise(Stream, T, Lno, R);

tokenise(Stream, [H|T], Lno, R) ->
    tokenise(Stream, T, Lno, [{list_to_atom([H]),Lno}|R]);
tokenise(_Stream, [], Lno, R) ->
    {Lno+1,R}.


collect_string(L, Lno) ->
    collect_string(L, Lno, []).

collect_string([$"|T], _Lno, Str) ->
    {{cstring,1,lists:reverse(Str)},T};
collect_string([H|T], Lno, Str) ->
    collect_string(T, Lno, [H|Str]);
collect_string([], _, _) ->
    throw({error,missing_quote_at_eof}).
           
%% <name> is letters digits hyphens.
%% Hypen is not the last character. Hypen hyphen is NOT allowed.
%%
%% <identifier> ::= <lowercase> <name>

get_name([$-,Char|T]=T0, Acc) ->
    case isalnum(Char) of
	true ->
	    get_name(T, [Char,$-|Acc]);
	false ->
	    {list_to_atom(lists:reverse(Acc)),T0}
    end;
get_name([$-|_]=T, Acc) ->
    {list_to_atom(lists:reverse(Acc)),T};
get_name([Char|T]=T0, Acc) ->
    case isalnum(Char) of
	true ->
	    get_name(T, [Char|Acc]);
	false ->
	    {list_to_atom(lists:reverse(Acc)),T0}
    end;
get_name([], Acc) ->
    {list_to_atom(lists:reverse(Acc)),[]}.
	    
isalnum(H) when $A =< H , H =< $Z ->
    true;
isalnum(H) when $a =< H , H =< $z ->
    true;
isalnum(H) when $0 =< H , H =< $9 ->
    true;
isalnum(_) ->
    false.

isdigit(H) when $0 =< H , H =< $9 ->
    true;
isdigit(_) ->
    false.

get_number([H|T]=T0, L) ->
    case isdigit(H) of
	true ->
	    get_number(T, [H|L]);
	false ->
	    {lists:reverse(L), T0}
    end;
get_number([], L) ->
    {lists:reverse(L), []}.

skip_comment([]) -> [];
skip_comment("--"++T) -> T;
skip_comment([_|T]) -> skip_comment(T).

skip_multiline_comment(Stream, [], Lno, Level) ->
    case io:get_line(Stream, '') of
	eof ->
	    throw({error,eof_in_comment});
	Line ->
	    skip_multiline_comment(Stream, Line, Lno+1, Level)
    end;
skip_multiline_comment(_Stream, "*/"++T, Lno, 0) ->
    {Lno,T};
skip_multiline_comment(Stream, "*/"++T, Lno, Level) ->
    skip_multiline_comment(Stream, T, Lno, Level - 1);
skip_multiline_comment(Stream, "/*"++T, Lno, Level) ->
    skip_multiline_comment(Stream, T, Lno, Level + 1);
skip_multiline_comment(Stream, [_|T], Lno, Level) ->
    skip_multiline_comment(Stream, T, Lno, Level).

collect_quoted("'B"++T, Lno, L) ->
    case check_bin(L) of
        true ->
            {{bstring,Lno,lists:reverse(L)}, T};
        false ->
            throw({error,{invalid_binary_number,lists:reverse(L)}})
    end;
collect_quoted("'H"++T, Lno, L) ->
    case check_hex(L) of
        true ->
            {{hstring,Lno,lists:reverse(L)}, T};
        false ->
            throw({error,{invalid_hex_number,lists:reverse(L)}})
    end;
collect_quoted([H|T], Lno, L) ->
    collect_quoted(T, Lno,[H|L]);
collect_quoted([], _, _) ->        % This should be allowed FIX later
    throw({error,eol_in_token}).

check_bin([$0|T]) ->
    check_bin(T);
check_bin([$1|T]) ->
    check_bin(T);
check_bin([]) ->
    true;
check_bin(_) ->
    false.

check_hex([H|T]) when $0 =< H , H =< $9 ->
    check_hex(T);
check_hex([H|T])  when $A =< H , H =< $F ->
    check_hex(T);
check_hex([]) ->
    true;
check_hex(_) ->
    false.


%% reserved_word(A) -> true|false|rstrtype
%% A = atom()
%% returns true if A is a reserved ASN.1 word
%% returns false if A is not a reserved word
%% returns rstrtype if A is a reserved word in the group 
%% 	RestrictedCharacterStringType
reserved_word('ABSENT') -> true;
reserved_word('ALL') -> true;
reserved_word('ANY') -> true;
reserved_word('APPLICATION') -> true;
reserved_word('AUTOMATIC') -> true;
reserved_word('BEGIN') -> true;
reserved_word('BIT') -> true;
reserved_word('BMPString') -> rstrtype;
reserved_word('BOOLEAN') -> true;
reserved_word('BY') -> true;
reserved_word('CHARACTER') -> true;
reserved_word('CHOICE') -> true;
reserved_word('CLASS') -> true;
reserved_word('COMPONENT') -> true;
reserved_word('COMPONENTS') -> true;
reserved_word('CONSTRAINED') -> true;
reserved_word('CONTAINING') -> true;
reserved_word('DEFAULT') -> true;
reserved_word('DEFINED') -> true; % not present in X.680 07/2002
reserved_word('DEFINITIONS') -> true;
reserved_word('EMBEDDED') -> true;
reserved_word('ENCODED') -> true;
reserved_word('END') -> true;
reserved_word('ENUMERATED') -> true;
reserved_word('EXCEPT') -> true;
reserved_word('EXPLICIT') -> true;
reserved_word('EXPORTS') -> true;
reserved_word('EXTENSIBILITY') -> true;
reserved_word('EXTERNAL') -> true;
reserved_word('FALSE') -> true;
reserved_word('FROM') -> true;
reserved_word('GeneralizedTime') -> true;
reserved_word('GeneralString') -> rstrtype;
reserved_word('GraphicString') -> rstrtype;
reserved_word('IA5String') -> rstrtype;
reserved_word('IDENTIFIER') -> true;
reserved_word('IMPLICIT') -> true;
reserved_word('IMPLIED') -> true;
reserved_word('IMPORTS') -> true;
reserved_word('INCLUDES') -> true;
reserved_word('INSTANCE') -> true;
reserved_word('INTEGER') -> true;
reserved_word('INTERSECTION') -> true;
reserved_word('MAX') -> true;
reserved_word('MIN') -> true;
reserved_word('MINUS-INFINITY') -> true;
reserved_word('NULL') -> true;
reserved_word('NumericString') -> rstrtype;
reserved_word('OBJECT') -> true;
reserved_word('ObjectDescriptor') -> true;
reserved_word('OCTET') -> true;
reserved_word('OF') -> true;
reserved_word('OPTIONAL') -> true;
reserved_word('PATTERN') -> true;
reserved_word('PDV') -> true;
reserved_word('PLUS-INFINITY') -> true;
reserved_word('PRESENT') -> true;
reserved_word('PrintableString') -> rstrtype;
reserved_word('PRIVATE') -> true;
reserved_word('REAL') -> true;
reserved_word('RELATIVE-OID') -> true;
reserved_word('SEQUENCE') -> true;
reserved_word('SET') -> true;
reserved_word('SIZE') -> true;
reserved_word('STRING') -> true;
reserved_word('SYNTAX') -> true;
reserved_word('T61String') -> rstrtype;
reserved_word('TAGS') -> true;
reserved_word('TeletexString') -> rstrtype;
reserved_word('TRUE') -> true;
reserved_word('UNION') -> true;
reserved_word('UNIQUE') -> true;
reserved_word('UNIVERSAL') -> true;
reserved_word('UniversalString') -> rstrtype;
reserved_word('UTCTime') -> true;
reserved_word('UTF8String') -> rstrtype;
reserved_word('VideotexString') -> rstrtype;
reserved_word('VisibleString') -> rstrtype;
reserved_word('WITH') -> true;
reserved_word(_) -> false.

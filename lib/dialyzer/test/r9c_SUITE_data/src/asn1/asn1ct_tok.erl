%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: asn1ct_tok.erl,v 1.1 2008/12/17 09:53:30 mikpe Exp $
%%
-module(asn1ct_tok).

%% Tokenize ASN.1 code (input to parser generated with yecc)

-export([get_name/2,tokenise/2, file/1]).


file(File) ->
    case file:open(File, [read])  of
	{error, Reason} ->
	    {error,{File,file:format_error(Reason)}};
	{ok,Stream} ->
	    process0(Stream)
    end.

process0(Stream) ->
    process(Stream,0,[]).

process(Stream,Lno,R) ->
    process(io:get_line(Stream, ''), Stream,Lno+1,R).

process(eof, Stream,Lno,R) ->
    file:close(Stream),
    lists:flatten(lists:reverse([{'$end',Lno}|R]));


process(L, Stream,Lno,R) when list(L) ->
    %%io:format('read:~s',[L]),
    case catch tokenise(L,Lno) of
	{'ERR',Reason} ->
	    io:format("Tokeniser error on line: ~w ~w~n",[Lno,Reason]),
	    exit(0);
	T ->
	    %%io:format('toks:~w~n',[T]),
	    process(Stream,Lno,[T|R])
    end.


tokenise([H|T],Lno) when $a =< H , H =< $z ->
    {X, T1} = get_name(T, [H]),
    [{identifier,Lno, list_to_atom(X)}|tokenise(T1,Lno)];

tokenise([$&,H|T],Lno) when $A =< H , H =< $Z ->
    {Y, T1} = get_name(T, [H]),
    X = list_to_atom(Y),
    [{typefieldreference, Lno, X} | tokenise(T1, Lno)];

tokenise([$&,H|T],Lno) when $a =< H , H =< $z ->
    {Y, T1} = get_name(T, [H]),
    X = list_to_atom(Y),
    [{valuefieldreference, Lno, X} | tokenise(T1, Lno)];

tokenise([H|T],Lno) when $A =< H , H =< $Z ->
    {Y, T1} = get_name(T, [H]),
    X = list_to_atom(Y),
    case reserved_word(X) of
	true ->
	    [{X,Lno}|tokenise(T1,Lno)];
	false ->
	    [{typereference,Lno,X}|tokenise(T1,Lno)];
	rstrtype ->
	    [{restrictedcharacterstringtype,Lno,X}|tokenise(T1,Lno)]
    end;

tokenise([$-,H|T],Lno) when $0 =< H , H =< $9 ->
    {X, T1} = get_number(T, [H]),
    [{number,Lno,-1 * list_to_integer(X)}|tokenise(T1,Lno)];

tokenise([H|T],Lno) when $0 =< H , H =< $9 ->
    {X, T1} = get_number(T, [H]),
    [{number,Lno,list_to_integer(X)}|tokenise(T1,Lno)];

tokenise([$-,$-|T],Lno) ->
    tokenise(skip_comment(T),Lno);
tokenise([$:,$:,$=|T],Lno) ->
    [{'::=',Lno}|tokenise(T,Lno)];

tokenise([$'|T],Lno) ->
    case catch collect_quoted(T,Lno,[]) of
         {'ERR',_} ->
             throw({'ERR','bad_quote'});
         {Thing, T1} ->
             [Thing|tokenise(T1,Lno)]
    end;

tokenise([$"|T],Lno) ->
    collect_string(T,Lno);

tokenise([${|T],Lno) ->
    [{'{',Lno}|tokenise(T,Lno)];

tokenise([$}|T],Lno) ->
    [{'}',Lno}|tokenise(T,Lno)];

tokenise([$]|T],Lno) ->
    [{']',Lno}|tokenise(T,Lno)];

tokenise([$[|T],Lno) ->
    [{'[',Lno}|tokenise(T,Lno)];

tokenise([$,|T],Lno) ->
    [{',',Lno}|tokenise(T,Lno)];

tokenise([$(|T],Lno) ->
    [{'(',Lno}|tokenise(T,Lno)];
tokenise([$)|T],Lno) ->
    [{')',Lno}|tokenise(T,Lno)];

tokenise([$.,$.,$.|T],Lno) ->
    [{'...',Lno}|tokenise(T,Lno)];

tokenise([$.,$.|T],Lno) ->
    [{'..',Lno}|tokenise(T,Lno)];

tokenise([$.|T],Lno) ->
    [{'.',Lno}|tokenise(T,Lno)];
tokenise([$^|T],Lno) ->
    [{'^',Lno}|tokenise(T,Lno)];
tokenise([$!|T],Lno) ->
    [{'!',Lno}|tokenise(T,Lno)];
tokenise([$||T],Lno) ->
    [{'|',Lno}|tokenise(T,Lno)];


tokenise([H|T],Lno) ->
    case white_space(H) of
	true ->
	    tokenise(T,Lno);
	false ->
	    [{list_to_atom([H]),Lno}|tokenise(T,Lno)]
    end;
tokenise([],_) ->
    [].


collect_string(L,Lno) ->
    collect_string(L,Lno,[]).

collect_string([],_,_) ->
    throw({'ERR','bad_quote found eof'});

collect_string([H|T],Lno,Str) ->
    case H of
	$" ->
           [{cstring,1,lists:reverse(Str)}|tokenise(T,Lno)];
        Ch ->
           collect_string(T,Lno,[Ch|Str])
    end.



% <name> is letters digits hyphens
% hypen is not the last character. Hypen hyphen is NOT allowed
%
% <identifier> ::= <lowercase> <name>

get_name([$-,Char|T], L) ->
    case isalnum(Char) of
	true ->
	    get_name(T,[Char,$-|L]);
	false ->
	    {lists:reverse(L),[$-,Char|T]}
    end;
get_name([$-|T], L) ->
    {lists:reverse(L),[$-|T]};
get_name([Char|T], L) ->
    case isalnum(Char) of
	true ->
	    get_name(T,[Char|L]);
	false ->
	    {lists:reverse(L),[Char|T]}
    end;
get_name([], L) ->
    {lists:reverse(L), []}.


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

white_space(9) -> true;
white_space(10) -> true;
white_space(13) -> true;
white_space(32) -> true;
white_space(_) -> false.


get_number([H|T], L) ->
    case isdigit(H) of
	true ->
	    get_number(T, [H|L]);
	false ->
	    {lists:reverse(L), [H|T]}
    end;
get_number([], L) ->
    {lists:reverse(L), []}.

skip_comment([]) ->
    [];
skip_comment([$-,$-|T]) ->
    T;
skip_comment([_|T]) ->
    skip_comment(T).

collect_quoted([$',$B|T],Lno, L) ->
    case check_bin(L) of
        true ->
            {{bstring,Lno, lists:reverse(L)}, T};
        false ->
            throw({'ERR',{invalid_binary_number, lists:reverse(L)}})
    end;
collect_quoted([$',$H|T],Lno, L) ->
    case check_hex(L) of
        true ->
            {{hstring,Lno, lists:reverse(L)}, T};
        false ->
            throw({'ERR',{invalid_binary_number, lists:reverse(L)}})
    end;
collect_quoted([H|T], Lno, L) ->
    collect_quoted(T, Lno,[H|L]);
collect_quoted([], _, _) ->        % This should be allowed FIX later
    throw({'ERR',{eol_in_token}}).

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
%reserved_word('ABSTRACT-SYNTAX') -> true; % impl as predef item
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
reserved_word('DEFAULT') -> true;
reserved_word('DEFINED') -> true;
reserved_word('DEFINITIONS') -> true;
reserved_word('EMBEDDED') -> true;
reserved_word('END') -> true;
reserved_word('ENUMERATED') -> true;
reserved_word('EXCEPT') -> true;
reserved_word('EXPLICIT') -> true;
reserved_word('EXPORTS') -> true;
reserved_word('EXTERNAL') -> true;
reserved_word('FALSE') -> true;
reserved_word('FROM') -> true;
reserved_word('GeneralizedTime') -> true;
reserved_word('GeneralString') -> rstrtype;
reserved_word('GraphicString') -> rstrtype;
reserved_word('IA5String') -> rstrtype;
% reserved_word('TYPE-IDENTIFIER') -> true; % impl as predef item
reserved_word('IDENTIFIER') -> true;
reserved_word('IMPLICIT') -> true;
reserved_word('IMPORTS') -> true;
reserved_word('INCLUDES') -> true;
reserved_word('INSTANCE') -> true;
reserved_word('INTEGER') -> true;
reserved_word('INTERSECTION') -> true;
reserved_word('ISO646String') -> rstrtype;
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
reserved_word('PDV') -> true;
reserved_word('PLUS-INFINITY') -> true;
reserved_word('PRESENT') -> true;
reserved_word('PrintableString') -> rstrtype;
reserved_word('PRIVATE') -> true;
reserved_word('REAL') -> true;
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
reserved_word('VideotexString') -> rstrtype;
reserved_word('VisibleString') -> rstrtype;
reserved_word('WITH') -> true;
reserved_word(_) -> false.

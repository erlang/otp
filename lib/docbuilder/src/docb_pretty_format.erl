%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docb_pretty_format).

-export([term/1]).

%% pretty_format:term(Term) -> PNF list of characters
%%   
%% Note: this is usually used in expressions like:
%%	 io:format('~s\n', [pretty_format:term(Term)]).
%%
%% Uses the following simple heuristics:
%%
%% 1) Simple tuples are printed across the page.
%%    (Simple means *all* the elements are "flat")
%% 2) The complex tuple {Arg1, Arg2, Arg3,....} is printed thus:
%%       {Arg1,
%%        Arg2,
%%        Arg3,
%%        ...}
%% 3) Lists are treated as for tuples.
%% 4) Lists of printable characters are treated as strings.
%%
%% This method seems to work reasonable well for {Tag, ...} type
%% data structures.
term(Term) ->
    element(2, term(Term, 0)).

%% pretty_format:term(Term, Indent} -> {Indent', Chars}
%% Format <Term> -- use <Indent> to indent the *next* line.
%% Note: Indent' is a new indentaion level (sometimes printing <Term>
%% the next line to need an "extra" indent!).
term([], Indent) ->
    {Indent, [$[,$]]};
term(L, Indent) when is_list(L) ->
    case is_string(L) of
	true ->
	    {Indent, io_lib:write_string(L)};
	false ->
	    case complex_list(L) of
		true ->
		    write_complex_list(L, Indent);
		false ->
		    write_simple_list(L, Indent)
	    end
    end;
term(T, Indent) when is_tuple(T) ->
    case complex_tuple(T) of
	true ->
	    write_complex_tuple(T, Indent);
	false ->
	    write_simple_tuple(T, Indent)
    end;
term(A, Indent) ->
    {Indent, io_lib:write(A)}.

%% write_simple_list([H|T], Indent) -> {Indent', Chars}
write_simple_list([H|T], Indent) ->
    {_, S1} = term(H, Indent),
    {_, S2} = write_simple_list_tail(T, Indent),
    {Indent, [$[,S1|S2]}.

write_simple_list_tail([H|T], Indent) ->
    {_, S1} = term(H, Indent),
    {_, S2} = write_simple_list_tail(T, Indent),
    {Indent, [$,,S1| S2]};
write_simple_list_tail([], Indent) ->
    {Indent, "]"};
write_simple_list_tail(Other, Indent) ->
    {_, S} = term(Other, Indent),
    {Indent, [$|,S,$]]}.

%% write_complex_list([H|T], Indent) -> {Indent', Chars}
write_complex_list([H|T], Indent) ->
    {I1, S1} = term(H, Indent+1),
    {_, S2} = write_complex_list_tail(T, I1),
    {Indent, [$[,S1|S2]}.

write_complex_list_tail([H|T], Indent) ->
    {I1, S1} = term(H, Indent),
    {_, S2} = write_complex_list_tail(T, I1),
    {Indent, [$,,nl_indent(Indent),S1,S2]};
write_complex_list_tail([], Indent) ->
    {Indent, "]"};
write_complex_list_tail(Other, Indent) ->
    {_, S} = term(Other, Indent),
    {Indent, [$|,S,$]]}.

%% complex_list(List) -> true | false
%% Returns true if the list is complex otherwise false.
complex_list([]) -> 
    false;
complex_list([H|T]) when is_list(H) ->
    case is_string(H) of
	true ->
	    complex_list(T);
	false ->
	    true
    end;
complex_list([H|_]) when is_tuple(H) -> true;
complex_list(_) -> false.

%% complex_tuple(Tuple) -> true | false
%% Returns true if the tuple is complex otherwise false.
complex_tuple(T) -> 
    complex_list(tuple_to_list(T)).

%% write_simple_tuple(Tuple, Indent} -> {Indent', Chars}
write_simple_tuple({}, Indent) ->
    {Indent, "{}"};
write_simple_tuple(Tuple, Indent) ->
    {_, S} = write_simple_tuple_args(tuple_to_list(Tuple), Indent),
    {Indent, [${, S, $}]}.

write_simple_tuple_args([X], Indent) ->
    term(X, Indent);
write_simple_tuple_args([H|T], Indent) ->
    {_, SH} = term(H, Indent),
    {_, ST} = write_simple_tuple_args(T, Indent),
    {Indent, [SH, $,, ST]}.

%%  write_complex_tuple(Tuple, Indent} -> {Indent', Chars}
write_complex_tuple(Tuple, Indent) ->
    [H|T] = tuple_to_list(Tuple),
    {I1, SH} = term(H, Indent+2),
    {_, ST} = write_complex_tuple_args(T, I1),
    {Indent, [${, SH, ST, $}]}.

write_complex_tuple_args([X], Indent) ->
    {_, S} = term(X, Indent),
    {Indent, [$,, nl_indent(Indent), S]};
write_complex_tuple_args([H|T], Indent) ->
    {I1, SH} = term(H, Indent),
    {_, ST} = write_complex_tuple_args(T, I1),
    {Indent, [$,, nl_indent(Indent) , SH, ST]};
write_complex_tuple_args([], Indent) ->
    {Indent, []}.
	
%% utilities
				   
nl_indent(I) when I >= 0 ->
    ["\n"|indent(I)];
nl_indent(_I) ->
    [$ ].

indent(I) when I >= 8 ->
    [$\t|indent(I-8)];
indent(I) when I > 0 ->
    [$ |indent(I-1)];
indent(_) ->
    [].

is_string([9|T]) ->
    is_string(T);
is_string([10|T]) ->
    is_string(T);
is_string([H|T]) when H >31, H < 127 -> 
    is_string(T);
is_string([]) -> 
    true;
is_string(_) -> 
    false.

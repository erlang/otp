%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
-module(tv_io_lib_pretty).



-export([pretty_print/4]).

%% pretty_print(Term, Column, LineLength, Depth) -> [Chars]
%% Depth = -1 gives unlimited print depth. Use tv_io_lib:write for atomic terms.

pretty_print(_, _, _, 0) -> "...";
pretty_print([], _, _, _) -> "[]";
pretty_print({}, _, _, _) -> "{}";
pretty_print(List, Col, Ll, D) when is_list(List) ->
    case tv_io_lib:printable_list(List) of
	true ->
	    tv_io_lib:write_string(List, $");
	false ->
	    Len = write_length(List, D, 0, Ll - Col),
	    if
		D =:= 1 -> "[...]";
		Len + Col < Ll ->
		    write(List, D);
		true ->
		    [$[,
		     [pretty_print(hd(List), Col + 1, Ll, D - 1)|
		      pretty_print_tail(tl(List), Col + 1, Ll, D - 1)],
		     $]]
	    end
    end;
pretty_print(Fun, _Col, _Ll, _D) when is_function(Fun) ->
    tv_io_lib:write(Fun);
pretty_print(Tuple, Col, Ll, D) when is_tuple(Tuple) ->
    Len = write_length(Tuple, D, 0, Ll - Col),
    if
	D =:= 1 -> "{...}";
	Len + Col < Ll ->
	    write(Tuple, D);
	is_atom(element(1, Tuple)), size(Tuple) > 1 ->
	    print_tag_tuple(Tuple, Col, Ll, D);
	true ->
	    [${,
	     [pretty_print(element(1, Tuple), Col + 1, Ll, D - 1)|
	      pretty_print_tail(tl(tuple_to_list(Tuple)), Col + 1, Ll, D - 1)],
	     $}]
    end;
pretty_print(Term, _Col, _Ll, D) -> tv_io_lib:write(Term, D).

%% print_tag_tuple(Tuple, Column, LineLength, Depth) -> [Char]
%%  Print a tagged tuple by indenting the rest of the elements differently
%%  to the tag. Start beside the tag if start column not too far to
%%  the right. Tuple has size >= 2.

print_tag_tuple(Tuple, Col, Ll, D) ->
    Tag = tv_io_lib:write_atom(element(1, Tuple)),
    Tlen = length(Tag),
    Tcol = Col + Tlen + 2,
    if
	Tcol >= Ll div 2, Tlen > 2 ->
	    [${,Tag,
	     pretty_print_tail(tl(tuple_to_list(Tuple)), Col + 4, Ll, D - 2),
	     $}];
	true ->
	    [${,Tag,$,,
	     [pretty_print(element(2, Tuple), Col + Tlen + 2, Ll, D - 2)|
	      pretty_print_tail(tl(tl(tuple_to_list(Tuple))), Tcol, Ll, D - 3)],
	     $}]
    end.

%% pretty_print_tail([Element], Column, LineLength, D) -> [Char]
%%  Pretty print the elements of a list or tuple.

pretty_print_tail([], _Col, _Ll, _D) -> "";
pretty_print_tail(_Es, _Col, _Ll, 1) -> "|...";
pretty_print_tail([E|Es], Col, Ll, D) ->
    [$,,nl_indent(Col-1),
     pretty_print(E, Col, Ll, D-1)|
     pretty_print_tail(Es, Col, Ll, D-1)];
pretty_print_tail(E, Col, Ll, D) ->
    [$|,nl_indent(Col-1),pretty_print(E, Col, Ll, D-1)].

%% write(Term, Depth) -> [Char]
%%  Write a term down to Depth on one line. Use tv_io_lib:write/2 for
%%  atomic terms.

write(_, 0) -> "...";
write([], _) -> "[]";
write({}, _) -> "{}";
write(List, D) when is_list(List) ->
    case tv_io_lib:printable_list(List) of
	true ->
	    tv_io_lib:write_string(List, $");
	false ->
	    if
		D =:= 1 -> "[...]";
		true ->
		    [$[,
		     [write(hd(List), D-1)|write_tail(tl(List), D-1)],
		     $]]
	    end
    end;
write(Fun, _D) when is_function(Fun) -> tv_io_lib:write(Fun); %Must catch this first
write(T, D) when is_tuple(T) ->
    if
	D =:= 1 -> "{...}";
	true ->
	    [${,
	     [write(element(1, T), D-1)|write_tail(tl(tuple_to_list(T)), D-1)],
	     $}]
    end;
write(Term, D) -> tv_io_lib:write(Term, D).

write_tail([], _D) -> "";
write_tail(_Es, 1) -> "|...";
write_tail([E|Es], D) ->
    [$,,write(E, D - 1)|write_tail(Es, D - 1)];
write_tail(E, D) ->
    [$|,write(E, D - 1)].
     
%% write_length(Term, Depth, Accumulator, MaxLength) -> integer()
%%  Calculate the print length of a term, but exit when length becomes
%%  greater than MaxLength.

write_length(_T, _D, Acc, Max) when Acc > Max -> Acc;
write_length(_T, 0, Acc, _Max) -> Acc + 3;
write_length([], _, Acc, _) -> Acc + 2;
write_length({}, _, Acc, _) -> Acc + 2;
write_length(List, D, Acc, Max) when is_list(List) ->
    case tv_io_lib:printable_list(List) of
	true ->
	    Acc + length(tv_io_lib:write_string(List, $"));
	false ->
	    write_length_list(List, D, Acc, Max)
    end;
write_length(Fun, _D, Acc, _Max) when is_function(Fun) ->
    Acc + length(tv_io_lib:write(Fun));
write_length(Tuple, D, Acc, Max) when is_tuple(Tuple) ->
    write_length_list(tuple_to_list(Tuple), D, Acc, Max);
write_length(Term, _D, Acc, _Max) ->
    Acc + length(tv_io_lib:write(Term)).

write_length_list(_, _, Acc, Max) when Acc > Max -> Acc;
write_length_list([], _, Acc, _) -> Acc + 1;	%]
write_length_list(_Es, 1, Acc, _) -> Acc + 5;	%|...]
write_length_list([E|Es], D, Acc, Max) ->
    write_length_list(Es,
		      D - 1,
		      write_length(E, D - 1, Acc + 1, Max),
		      Max);
write_length_list(E, D, Acc, Max) ->
    write_length(E, D - 1, Acc + 2, Max).	%| ]



nl_indent(_) -> "".

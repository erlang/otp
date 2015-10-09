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
%%     $Id: core_pp.erl,v 1.1 2008/12/17 09:53:42 mikpe Exp $
%%
%% Purpose : Core Erlang (naive) prettyprinter

-module(core_pp).

-export([format/1]).

-include("core_parse.hrl").

%% ====================================================================== %%
%% format(Node) -> Text
%%	Node = coreErlang()
%%	Text = string() | [Text]
%%
%%	Prettyprint-formats (naively) an abstract Core Erlang syntax
%%	tree.

-record(ctxt, {class = term,
	       indent = 0,
	       item_indent = 2,
	       body_indent = 4,
	       tab_width = 8,
	       line = 0}).

format(Node) -> case catch format(Node, #ctxt{}) of
		    {'EXIT',_} -> io_lib:format("~p",[Node]);
		    Other -> Other
		end.

maybe_anno(Node, Fun, Ctxt) ->
    As = core_lib:get_anno(Node),
    case get_line(As) of
	none ->
	    maybe_anno(Node, Fun, Ctxt, As);
	Line ->
	    if  Line > Ctxt#ctxt.line ->
		    [io_lib:format("%% Line ~w",[Line]),
		     nl_indent(Ctxt),
		     maybe_anno(Node, Fun, Ctxt#ctxt{line = Line}, As)
		    ];
		true ->
		    maybe_anno(Node, Fun, Ctxt, As)
	    end
    end.

maybe_anno(Node, Fun, Ctxt, As) ->
    case strip_line(As) of
	[] ->
	    Fun(Node, Ctxt);
	List ->
	    Ctxt1 = add_indent(Ctxt, 2),
	    Ctxt2 = add_indent(Ctxt1, 3),
	    ["( ",
	     Fun(Node, Ctxt1),
	     nl_indent(Ctxt1),
	     "-| ",format_1(core_lib:make_literal(List), Ctxt2)," )"
	    ]
    end.

strip_line([A | As]) when integer(A) ->
    strip_line(As);
strip_line([A | As]) ->
    [A | strip_line(As)];
strip_line([]) ->
    [].

get_line([L | _As]) when integer(L) ->
    L;
get_line([_ | As]) ->
    get_line(As);
get_line([]) ->
    none.

format(Node, Ctxt) ->
    maybe_anno(Node, fun format_1/2, Ctxt).

format_1(#c_char{val=C}, _) -> io_lib:write_char(C);
format_1(#c_int{val=I}, _) -> integer_to_list(I);
format_1(#c_float{val=F}, _) -> float_to_list(F);
format_1(#c_atom{val=A}, _) -> core_atom(A);
format_1(#c_nil{}, _) -> "[]";
format_1(#c_string{val=S}, _) -> io_lib:write_string(S);
format_1(#c_var{name=V}, _) ->
    %% Internal variable names may be:
    %%     - atoms representing proper Erlang variable names, or
    %%     any atoms that may be printed without single-quoting
    %%     - nonnegative integers.
    %% It is important that when printing variables, no two names
    %% should ever map to the same string.
    if atom(V) ->
	    S = atom_to_list(V),
	    case S of
		[C | _] when C >= $A, C =< $Z ->
		    %% Ordinary uppercase-prefixed names are
		    %% printed just as they are.
		    S;
		[$_ | _] ->
		    %% Already "_"-prefixed names are prefixed
		    %% with "_X", e.g. '_foo' => '_X_foo', to
		    %% avoid generating things like "____foo" upon
		    %% repeated writing and reading of code.
		    %% ("_X_X_X_foo" is better.)
		    [$_, $X | S];
		_ ->
		    %% Plain atoms are prefixed with a single "_".
		    %% E.g. foo => "_foo".
		    [$_ | S]
	    end;
       integer(V) ->
	    %% Integers are also simply prefixed with "_".
	    [$_ | integer_to_list(V)]
    end;
format_1(#c_binary{segments=Segs}, Ctxt) ->
    ["#{",
     format_vseq(Segs, "", ",", add_indent(Ctxt, 2),
		 fun format_bitstr/2),
     "}#"
    ];
format_1(#c_tuple{es=Es}, Ctxt) ->
    [${,
     format_hseq(Es, ",", add_indent(Ctxt, 1), fun format/2),
     $}
    ];
format_1(#c_cons{hd=H,tl=T}, Ctxt) ->
    Txt = ["["|format(H, add_indent(Ctxt, 1))],
    [Txt|format_list_tail(T, add_indent(Ctxt, width(Txt, Ctxt)))];
format_1(#c_values{es=Es}, Ctxt) ->
    format_values(Es, Ctxt);
format_1(#c_alias{var=V,pat=P}, Ctxt) ->
    Txt = [format(V, Ctxt)|" = "],
    [Txt|format(P, add_indent(Ctxt, width(Txt, Ctxt)))];
format_1(#c_let{vars=Vs,arg=A,body=B}, Ctxt) ->
    Ctxt1 = add_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["let ",
     format_values(Vs, add_indent(Ctxt, 4)),
     " =",
     nl_indent(Ctxt1),
     format(A, Ctxt1),
     nl_indent(Ctxt),
     "in  "
     | format(B, add_indent(Ctxt, 4))
    ];
format_1(#c_letrec{defs=Fs,body=B}, Ctxt) ->
    Ctxt1 = add_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["letrec",
     nl_indent(Ctxt1),
     format_funcs(Fs, Ctxt1),
     nl_indent(Ctxt),
     "in  "
     | format(B, add_indent(Ctxt, 4))
    ];
format_1(#c_seq{arg=A,body=B}, Ctxt) ->
    Ctxt1 = add_indent(Ctxt, 4),
    ["do  ",
     format(A, Ctxt1),
     nl_indent(Ctxt1)
     | format(B, Ctxt1)
    ];
format_1(#c_case{arg=A,clauses=Cs}, Ctxt) ->
    Ctxt1 = add_indent(Ctxt, Ctxt#ctxt.item_indent),
    ["case ",
     format(A, add_indent(Ctxt, 5)),
     " of",
     nl_indent(Ctxt1),
     format_clauses(Cs, Ctxt1),
     nl_indent(Ctxt)
     | "end"
    ];
format_1(#c_receive{clauses=Cs,timeout=T,action=A}, Ctxt) ->
    Ctxt1 = add_indent(Ctxt, Ctxt#ctxt.item_indent),
    ["receive",
     nl_indent(Ctxt1),
     format_clauses(Cs, Ctxt1),
     nl_indent(Ctxt),
     "after ",
     format(T, add_indent(Ctxt, 6)),
     " ->",
     nl_indent(Ctxt1),
     format(A, Ctxt1)
    ];
format_1(#c_fname{id=I,arity=A}, _) ->
    [core_atom(I),$/,integer_to_list(A)];
format_1(#c_fun{vars=Vs,body=B}, Ctxt) ->
    Ctxt1 = add_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["fun (",
     format_hseq(Vs, ",", add_indent(Ctxt, 5), fun format/2),
     ") ->",
     nl_indent(Ctxt1)
     | format(B, Ctxt1)
    ];
format_1(#c_apply{op=O,args=As}, Ctxt0) ->
    Ctxt1 = add_indent(Ctxt0, 6),		%"apply "
    Op = format(O, Ctxt1),
    Ctxt2 = add_indent(Ctxt0, 4),
    ["apply ",Op,
     nl_indent(Ctxt2),
     $(,format_hseq(As, ", ", add_indent(Ctxt2, 1), fun format/2),$)
    ];
format_1(#c_call{module=M,name=N,args=As}, Ctxt0) ->
    Ctxt1 = add_indent(Ctxt0, 5),		%"call "
    Mod = format(M, Ctxt1),
    Ctxt2 = add_indent(Ctxt1, width(Mod, Ctxt1)+1),
    Name = format(N, Ctxt2),
    Ctxt3 = add_indent(Ctxt0, 4),
    ["call ",Mod,":",Name,
     nl_indent(Ctxt3),
     $(,format_hseq(As, ", ", add_indent(Ctxt3, 1), fun format/2),$)
    ];
format_1(#c_primop{name=N,args=As}, Ctxt0) ->
    Ctxt1 = add_indent(Ctxt0, 7),		%"primop "
    Name = format(N, Ctxt1),
    Ctxt2 = add_indent(Ctxt0, 4),
    ["primop ",Name,
     nl_indent(Ctxt2),
     $(,format_hseq(As, ", ", add_indent(Ctxt2, 1), fun format/2),$)
    ];
format_1(#c_catch{body=B}, Ctxt) ->
    Ctxt1 = add_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["catch",
     nl_indent(Ctxt1),
     format(B, Ctxt1)
    ];
format_1(#c_try{arg=E,vars=Vs,body=B,evars=Evs,handler=H}, Ctxt) ->
    Ctxt1 = add_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["try",
     nl_indent(Ctxt1),
     format(E, Ctxt1),
     nl_indent(Ctxt),
     "of ",
     format_values(Vs, add_indent(Ctxt, 3)),
     " ->",
     nl_indent(Ctxt1),
     format(B, Ctxt1),
     nl_indent(Ctxt),
     "catch ",
     format_values(Evs, add_indent(Ctxt, 6)),
     " ->",
     nl_indent(Ctxt1)
     | format(H, Ctxt1)
    ];
format_1(#c_def{name=N,val=V}, Ctxt) ->
    Ctxt1 = add_indent(set_class(Ctxt, expr), Ctxt#ctxt.body_indent),
    [format(N, Ctxt),
     " =",
     nl_indent(Ctxt1)
     | format(V, Ctxt1)
    ];
format_1(#c_module{name=N,exports=Es,attrs=As,defs=Ds}, Ctxt) ->
    Mod = ["module ", format(N, Ctxt)],
    [Mod," [",
     format_vseq(Es,
		 "", ",",
		 add_indent(set_class(Ctxt, term), width(Mod, Ctxt)+2),
		 fun format/2),
     "]",
     nl_indent(Ctxt),
     "    attributes [",
     format_vseq(As,
		 "", ",",
		 add_indent(set_class(Ctxt, def), 16),
		 fun format/2),
     "]",
     nl_indent(Ctxt),
     format_funcs(Ds, Ctxt),
     nl_indent(Ctxt)
     | "end"
    ];
format_1(Type, _) ->
    ["** Unsupported type: ",
     io_lib:write(Type)
     | " **"
    ].

format_funcs(Fs, Ctxt) ->
    format_vseq(Fs,
		"", "",
		set_class(Ctxt, def),
		fun format/2).

format_values(Vs, Ctxt) ->
    [$<,
     format_hseq(Vs, ",", add_indent(Ctxt, 1), fun format/2),
     $>].

format_bitstr(#c_bitstr{val=V,size=S,unit=U,type=T,flags=Fs}, Ctxt0) ->
    Vs = [S, U, T, Fs],
    Ctxt1 = add_indent(Ctxt0, 2),
    Val = format(V, Ctxt1),
    Ctxt2 = add_indent(Ctxt1, width(Val, Ctxt1) + 2),
    ["#<", Val, ">(", format_hseq(Vs,",", Ctxt2, fun format/2), $)].

format_clauses(Cs, Ctxt) ->
    format_vseq(Cs, "", "", set_class(Ctxt, clause),
		fun format_clause/2).

format_clause(Node, Ctxt) ->
    maybe_anno(Node, fun format_clause_1/2, Ctxt).

format_clause_1(#c_clause{pats=Ps,guard=G,body=B}, Ctxt) ->
    Ptxt = format_values(Ps, Ctxt),
    Ctxt2 = add_indent(Ctxt, Ctxt#ctxt.body_indent),
    [Ptxt,
     " when ",
     format_guard(G, add_indent(set_class(Ctxt, expr),
				 width(Ptxt, Ctxt) + 6)),
     " ->",
     nl_indent(Ctxt2)
     | format(B, set_class(Ctxt2, expr))
    ].

format_guard(Node, Ctxt) ->
    maybe_anno(Node, fun format_guard_1/2, Ctxt).

format_guard_1(#c_call{module=M,name=N,args=As}, Ctxt0) ->
    Ctxt1 = add_indent(Ctxt0, 5),		%"call "
    Mod = format(M, Ctxt1),
    Ctxt2 = add_indent(Ctxt1, width(Mod, Ctxt1)+1),
    Name = format(N, Ctxt2),
    Ctxt3 = add_indent(Ctxt0, 4),
    ["call ",Mod,":",Name,
     nl_indent(Ctxt3),
     $(,format_vseq(As, "",",", add_indent(Ctxt3, 1), fun format_guard/2),$)
    ];
format_guard_1(E, Ctxt) -> format_1(E, Ctxt).	%Anno already done

%% format_hseq([Thing], Separator, Context, Fun) -> Txt.
%%  Format a sequence horizontally on the same line with Separator between.

format_hseq([H], _, Ctxt, Fun) ->
    Fun(H, Ctxt);
format_hseq([H|T], Sep, Ctxt, Fun) ->
    Txt = [Fun(H, Ctxt)|Sep],
    Ctxt1 = add_indent(Ctxt, width(Txt, Ctxt)),
    [Txt|format_hseq(T, Sep, Ctxt1, Fun)];
format_hseq([], _, _, _) -> "".

%% format_vseq([Thing], LinePrefix, LineSuffix, Context, Fun) -> Txt.
%%  Format a sequence vertically in indented lines adding LinePrefix
%%  to the beginning of each line and LineSuffix to the end of each
%%  line.  No prefix on the first line or suffix on the last line.

format_vseq([H], _Pre, _Suf, Ctxt, Fun) ->
    Fun(H, Ctxt);
format_vseq([H|T], Pre, Suf, Ctxt, Fun) ->
    [Fun(H, Ctxt),Suf,nl_indent(Ctxt),Pre|
     format_vseq(T, Pre, Suf, Ctxt, Fun)];
format_vseq([], _, _, _, _) -> "".

format_list_tail(#c_nil{anno=[]}, _) -> "]";
format_list_tail(#c_cons{anno=[],hd=H,tl=T}, Ctxt) ->
    Txt = [$,|format(H, Ctxt)],
    Ctxt1 = add_indent(Ctxt, width(Txt, Ctxt)),
    [Txt|format_list_tail(T, Ctxt1)];
format_list_tail(Tail, Ctxt) ->
    ["|",format(Tail, add_indent(Ctxt, 1)),"]"].

indent(Ctxt) -> indent(Ctxt#ctxt.indent, Ctxt).

indent(N, _) when N =< 0 -> "";
indent(N, Ctxt) ->
    T = Ctxt#ctxt.tab_width,
    string:chars($\t, N div T, string:chars($\s, N rem T)).

nl_indent(Ctxt) -> [$\n|indent(Ctxt)].


unindent(T, Ctxt) ->
    unindent(T, Ctxt#ctxt.indent, Ctxt, []).

unindent(T, N, _, C) when N =< 0 ->
    [T|C];
unindent([$\s|T], N, Ctxt, C) ->
    unindent(T, N - 1, Ctxt, C);
unindent([$\t|T], N, Ctxt, C) ->
    Tab = Ctxt#ctxt.tab_width,
    if N >= Tab ->
	    unindent(T, N - Tab, Ctxt, C);
       true ->
	    unindent([string:chars($\s, Tab - N)|T], 0, Ctxt, C)
    end;
unindent([L|T], N, Ctxt, C) when list(L) ->
    unindent(L, N, Ctxt, [T|C]);
unindent([H|T], _, _, C) ->
    [H|[T|C]];
unindent([], N, Ctxt, [H|T]) ->
    unindent(H, N, Ctxt, T);
unindent([], _, _, []) -> [].


width(Txt, Ctxt) ->
    case catch width(Txt, 0, Ctxt, []) of
	{'EXIT',_} -> exit({bad_text,Txt});
	Other -> Other
    end.

width([$\t|T], A, Ctxt, C) ->
    width(T, A + Ctxt#ctxt.tab_width, Ctxt, C);
width([$\n|T], _, Ctxt, C) ->
    width(unindent([T|C], Ctxt), Ctxt);
width([H|T], A, Ctxt, C) when list(H) ->
    width(H, A, Ctxt, [T|C]);
width([_|T], A, Ctxt, C) ->
    width(T, A + 1, Ctxt, C);
width([], A, Ctxt, [H|T]) ->
    width(H, A, Ctxt, T);
width([], A, _, []) -> A.

add_indent(Ctxt, Dx) ->
    Ctxt#ctxt{indent = Ctxt#ctxt.indent + Dx}.

set_class(Ctxt, Class) ->
    Ctxt#ctxt{class = Class}.

core_atom(A) -> io_lib:write_string(atom_to_list(A), $').

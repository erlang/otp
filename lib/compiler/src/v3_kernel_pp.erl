%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
%% Purpose : Kernel Erlang (naive) prettyprinter

-module(v3_kernel_pp).

-export([format/1]).

%%-define(INCLUDE_ANNOTATIONS, 1).

-include("v3_kernel.hrl").

%% These are "internal" structures in sys_kernel which are here for
%% debugging purposes.
-record(iset, {anno=[],vars,arg,body}).
-record(ifun, {anno=[],vars,body}).

%% ====================================================================== %%
%% format(Node) -> Text
%%	Node = coreErlang()
%%	Text = string() | [Text]
%%
%%	Prettyprint-formats (naively) an abstract Core Erlang syntax
%%	tree.

-record(ctxt, {indent = 0      :: non_neg_integer(),
	       item_indent = 2 :: non_neg_integer(),
	       body_indent = 2 :: non_neg_integer(),
	       tab_width = 8   :: non_neg_integer()}).

canno(Cthing) -> element(2, Cthing).

-spec format(#k_mdef{}) -> iolist().

format(Node) -> format(Node, #ctxt{}).

format(Node, Ctxt) ->
    case canno(Node) of
	[] ->
	    format_1(Node, Ctxt);
	[L,{file,_}] when is_integer(L) ->
	    format_1(Node, Ctxt);
	#k{a=Anno}=K when Anno =/= [] ->
	    format(setelement(2, Node, K#k{a=[]}), Ctxt);
	List ->
	    format_anno(List, Ctxt, fun (Ctxt1) ->
					    format_1(Node, Ctxt1)
				    end)
    end.


-ifndef(INCLUDE_ANNOTATIONS).
%% Don't include annotations (for readability).
format_anno(_Anno, Ctxt, ObjFun) ->
    ObjFun(Ctxt).
-else.
%% Include annotations (for debugging of annotations).
format_anno(Anno, Ctxt0, ObjFun) ->
    Ctxt1 = ctxt_bump_indent(Ctxt0, 1),
    ["( ",
     ObjFun(Ctxt0),
     nl_indent(Ctxt1),
     "-| ",io_lib:write(Anno),
     " )"].
-endif.

%% format_1(Kexpr, Context) -> string().

format_1(#k_atom{val=A}, _Ctxt) -> core_atom(A);
%%format_1(#k_char{val=C}, _Ctxt) -> io_lib:write_char(C);
format_1(#k_float{val=F}, _Ctxt) -> float_to_list(F);
format_1(#k_int{val=I}, _Ctxt) -> integer_to_list(I);
format_1(#k_nil{}, _Ctxt) -> "[]";
format_1(#k_var{name=V}, _Ctxt) ->
    if is_atom(V) ->
	    case atom_to_list(V) of
		[$_|Cs] -> "_X" ++ Cs;
		[C|_Cs] = L when C >= $A, C =< $Z -> L;
		Cs -> [$_|Cs]
	    end;
       is_integer(V) -> [$_|integer_to_list(V)]
    end;
format_1(#k_cons{hd=H,tl=T}, Ctxt) ->
    Txt = ["["|format(H, ctxt_bump_indent(Ctxt, 1))],
    [Txt|format_list_tail(T, ctxt_bump_indent(Ctxt, width(Txt, Ctxt)))];
format_1(#k_tuple{es=Es}, Ctxt) ->
    [${,
     format_hseq(Es, ",", ctxt_bump_indent(Ctxt, 1), fun format/2),
     $}
    ];
format_1(#k_map{var=#k_literal{val=M},op=assoc,es=Es}, Ctxt) when is_map(M), map_size(M) =:= 0 ->
    ["~{",
     format_hseq(Es, ",", ctxt_bump_indent(Ctxt, 1), fun format/2),
     "}~"
    ];
format_1(#k_map{var=#k_literal{val=M},op=exact,es=Es}, Ctxt) when is_map(M), map_size(M) =:= 0 ->
    ["::{",
     format_hseq(Es, ",", ctxt_bump_indent(Ctxt, 1), fun format/2),
     "}::"
    ];
format_1(#k_map{var=Var,op=assoc,es=Es}, Ctxt) ->
    ["~{",
     format_hseq(Es, ",", ctxt_bump_indent(Ctxt, 1), fun format/2),
     " | ",format_1(Var, Ctxt),
     "}~"
    ];
format_1(#k_map{var=Var,op=exact,es=Es}, Ctxt) ->
    ["::{",
     format_hseq(Es, ",", ctxt_bump_indent(Ctxt, 1), fun format/2),
     " | ",format_1(Var, Ctxt),
     "}::"
    ];
format_1(#k_map_pair{key=K,val=V}, Ctxt) ->
    ["<",format(K, Ctxt),",",format(V, Ctxt),">"];
format_1(#k_binary{segs=S}, Ctxt) ->
    ["#<",format(S, ctxt_bump_indent(Ctxt, 2)),">#"];
format_1(#k_bin_seg{next=Next}=S, Ctxt) ->
    [format_bin_seg_1(S, Ctxt),
     format_bin_seg(Next, ctxt_bump_indent(Ctxt, 2))];
format_1(#k_bin_int{size=Sz,unit=U,flags=Fs,val=Val,next=Next}, Ctxt) ->
    S = #k_bin_seg{size=Sz,unit=U,type=integer,flags=Fs,seg=#k_int{val=Val},next=Next},
    [format_bin_seg_1(S, Ctxt),
     format_bin_seg(Next, ctxt_bump_indent(Ctxt, 2))];
format_1(#k_bin_end{}, _Ctxt) -> "#<>#";
format_1(#k_literal{val=Term}, _Ctxt) ->
    io_lib:format("~p", [Term]);
format_1(#k_local{name=N,arity=A}, Ctxt) ->
    "local " ++ format_fa_pair({N,A}, Ctxt);
format_1(#k_remote{mod=M,name=N,arity=A}, _Ctxt) ->
    %% This is for our internal translator.
    io_lib:format("remote ~ts:~ts/~w", [format(M),format(N),A]);
format_1(#k_internal{name=N,arity=A}, Ctxt) ->
    "internal " ++ format_fa_pair({N,A}, Ctxt);
format_1(#k_seq{arg=A,body=B}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, 2),
    ["do",
     nl_indent(Ctxt1),
     format(A, Ctxt1),
     nl_indent(Ctxt),
     "then",
     nl_indent(Ctxt)
     | format(B, Ctxt)
    ];
format_1(#k_match{vars=Vs,body=Bs,ret=Rs}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.item_indent),
    ["match ",
     format_hseq(Vs, ",", ctxt_bump_indent(Ctxt, 6), fun format/2),
     nl_indent(Ctxt1),
     format(Bs, Ctxt1),
     nl_indent(Ctxt),
     "end",
     format_ret(Rs, Ctxt1)
    ];
format_1(#k_guard_match{vars=Vs,body=Bs,ret=Rs}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.item_indent),
    ["guard_match ",
     format_hseq(Vs, ",", ctxt_bump_indent(Ctxt, 6), fun format/2),
     nl_indent(Ctxt1),
     format(Bs, Ctxt1),
     nl_indent(Ctxt),
     "end",
     format_ret(Rs, Ctxt1)
    ];
format_1(#k_alt{first=O,then=T}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.item_indent),
    ["alt",
     nl_indent(Ctxt1),
     format(O, Ctxt1),
     nl_indent(Ctxt1),
     format(T, Ctxt1)];
format_1(#k_select{var=V,types=Cs}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, 2),
    ["select ",
     format(V, Ctxt),
     nl_indent(Ctxt1),
     format_vseq(Cs, "", "", Ctxt1, fun format/2)
    ];
format_1(#k_type_clause{type=T,values=Cs}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["type ",
     io_lib:write(T),
     nl_indent(Ctxt1),
     format_vseq(Cs, "", "", Ctxt1, fun format/2)
    ];
format_1(#k_val_clause{val=Val,body=B}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    [format(Val, Ctxt),
     " ->",
     nl_indent(Ctxt1)
     | format(B, Ctxt1)
    ];
format_1(#k_guard{clauses=Gs}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, 5),
    ["when ",
     nl_indent(Ctxt1),
     format_vseq(Gs, "", "", Ctxt1, fun format/2)];
format_1(#k_guard_clause{guard=G,body=B}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    [format(G, Ctxt),
     nl_indent(Ctxt),
     "->",
     nl_indent(Ctxt1)
     | format(B, Ctxt1)
    ];
format_1(#k_call{op=Op,args=As,ret=Rs}, Ctxt) ->
    Txt = ["call (",format(Op, ctxt_bump_indent(Ctxt, 6)),$)],
    Ctxt1 = ctxt_bump_indent(Ctxt, 2),
    [Txt,format_args(As, Ctxt1),
     format_ret(Rs, Ctxt1)
    ];
format_1(#k_enter{op=Op,args=As}, Ctxt) ->
    Txt = ["enter (",format(Op, ctxt_bump_indent(Ctxt, 7)),$)],
    Ctxt1 = ctxt_bump_indent(Ctxt, 2),
    [Txt,format_args(As, Ctxt1)];
format_1(#k_bif{op=Op,args=As,ret=Rs}, Ctxt) ->
    Txt = ["bif (",format(Op, ctxt_bump_indent(Ctxt, 5)),$)],
    Ctxt1 = ctxt_bump_indent(Ctxt, 2),
    [Txt,format_args(As, Ctxt1),
     format_ret(Rs, Ctxt1)
    ];
format_1(#k_test{op=Op,args=As,inverted=Inverted}, Ctxt) ->
    Txt = case Inverted of
	      false ->
		  ["test (",format(Op, ctxt_bump_indent(Ctxt, 6)),$)];
	      true ->
		  ["inverted_test (",format(Op, ctxt_bump_indent(Ctxt, 6)),$)]
	  end,
    Ctxt1 = ctxt_bump_indent(Ctxt, 2),
    [Txt,format_args(As, Ctxt1)];
format_1(#k_put{arg=A,ret=Rs}, Ctxt) ->
    [format(A, Ctxt),
     format_ret(Rs, ctxt_bump_indent(Ctxt, 1))
    ];
format_1(#k_try{arg=A,vars=Vs,body=B,evars=Evs,handler=H,ret=Rs}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["try",
     nl_indent(Ctxt1),
     format(A, Ctxt1),
     nl_indent(Ctxt),
     "of ",
     format_hseq(Vs, ", ", ctxt_bump_indent(Ctxt, 3), fun format/2),
     nl_indent(Ctxt1),
     format(B, Ctxt1),
     nl_indent(Ctxt),
     "catch ",
     format_hseq(Evs, ", ", ctxt_bump_indent(Ctxt, 6), fun format/2),
     nl_indent(Ctxt1),
     format(H, Ctxt1),
     nl_indent(Ctxt),
     "end",
     format_ret(Rs, Ctxt)
    ];
format_1(#k_try_enter{arg=A,vars=Vs,body=B,evars=Evs,handler=H}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["try_enter",
     nl_indent(Ctxt1),
     format(A, Ctxt1),
     nl_indent(Ctxt),
     "of ",
     format_hseq(Vs, ", ", ctxt_bump_indent(Ctxt, 3), fun format/2),
     nl_indent(Ctxt1),
     format(B, Ctxt1),
     nl_indent(Ctxt),
     "catch ",
     format_hseq(Evs, ", ", ctxt_bump_indent(Ctxt, 6), fun format/2),
     nl_indent(Ctxt1),
     format(H, Ctxt1),
     nl_indent(Ctxt),
     "end"
    ];
format_1(#k_protected{arg=A,ret=Rs}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["protected",
     nl_indent(Ctxt1),
     format(A, Ctxt1),
     nl_indent(Ctxt),
     "end",
     format_ret(Rs, ctxt_bump_indent(Ctxt, 1))
    ];
format_1(#k_catch{body=B,ret=Rs}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["catch",
     nl_indent(Ctxt1),
     format(B, Ctxt1),
     nl_indent(Ctxt),
     "end",
     format_ret(Rs, Ctxt1)
    ];
format_1(#k_receive{var=V,body=B,timeout=T,action=A,ret=Rs}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.item_indent),
    ["receive ",
     format(V, Ctxt),
     nl_indent(Ctxt1),
     format(B, Ctxt1),
     nl_indent(Ctxt),
     "after ",
     format(T, ctxt_bump_indent(Ctxt, 6)),
     " ->",
     nl_indent(Ctxt1),
     format(A, Ctxt1),
     nl_indent(Ctxt),
     "end",
     format_ret(Rs, Ctxt1)
    ];
format_1(#k_receive_accept{}, _Ctxt) -> "receive_accept";
format_1(#k_receive_next{}, _Ctxt) -> "receive_next";
format_1(#k_break{args=As}, Ctxt) ->
    ["<",
     format_hseq(As, ",", ctxt_bump_indent(Ctxt, 1), fun format/2),
     ">"
    ];
format_1(#k_guard_break{args=As}, Ctxt) ->
    [":<",
     format_hseq(As, ",", ctxt_bump_indent(Ctxt, 1), fun format/2),
     ">:"
    ];
format_1(#k_return{args=As}, Ctxt) ->
    ["<<",
     format_hseq(As, ",", ctxt_bump_indent(Ctxt, 1), fun format/2),
     ">>"
    ];
format_1(#k_fdef{func=F,arity=A,vars=Vs,body=B}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["fdef ",
     format_fa_pair({F,A}, ctxt_bump_indent(Ctxt, 5)),
     format_args(Vs, ctxt_bump_indent(Ctxt, 14)),
     " =",
     nl_indent(Ctxt1),
     format(B, Ctxt1)
    ];
format_1(#k_mdef{name=N,exports=Es,attributes=As,body=B}, Ctxt) ->
    ["module ",
     format(#k_atom{val=N}, ctxt_bump_indent(Ctxt, 7)),
     nl_indent(Ctxt),
     "export [",
     format_vseq(Es,
		 "", ",",
		 ctxt_bump_indent(Ctxt, 8),
		 fun format_fa_pair/2),
     "]",
     nl_indent(Ctxt),
     "attributes [",
     format_vseq(As,
		 "", ",",
		 ctxt_bump_indent(Ctxt, 12),
		 fun format_attribute/2),
     "]",
     nl_indent(Ctxt),
     format_vseq(B,
		 "", "",
		 Ctxt,
		 fun format/2),
     nl_indent(Ctxt)
     | "end"
    ];
%% Internal sys_kernel structures.
format_1(#iset{vars=Vs,arg=A,body=B}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["set <",
     format_hseq(Vs, ", ", ctxt_bump_indent(Ctxt, 5), fun format/2),
     "> =",
     nl_indent(Ctxt1),
     format(A, Ctxt1),
     nl_indent(Ctxt),
     "in  "
     | format(B, ctxt_bump_indent(Ctxt, 2))
    ];
format_1(#ifun{vars=Vs,body=B}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["fun ",
     format_args(Vs, ctxt_bump_indent(Ctxt, 4)),
     " ->",
     nl_indent(Ctxt1)
     | format(B, Ctxt1)
    ];
format_1(Type, _Ctxt) ->
    ["** Unsupported type: ",
     io_lib:write(Type)
     | " **"
    ].

%% format_ret([RetVar], Context) -> Txt.
%%  Format the return vars of kexpr.

format_ret(Rs, Ctxt) ->
    [" >> ",
     "<",
     format_hseq(Rs, ",", ctxt_bump_indent(Ctxt, 5), fun format/2),
     ">"].

%% format_args([Arg], Context) -> Txt.
%%  Format arguments.

format_args(As, Ctxt) ->
  [$(,format_hseq(As, ", ", ctxt_bump_indent(Ctxt, 1), fun format/2),$)].

%% format_hseq([Thing], Separator, Context, Fun) -> Txt.
%%  Format a sequence horizontally.

format_hseq([H], _Sep, Ctxt, Fun) ->
    Fun(H, Ctxt);
format_hseq([H|T], Sep, Ctxt, Fun) ->
    Txt = [Fun(H, Ctxt)|Sep],
    Ctxt1 = ctxt_bump_indent(Ctxt, width(Txt, Ctxt)),
    [Txt|format_hseq(T, Sep, Ctxt1, Fun)];
format_hseq([], _, _, _) -> "".

%% format_vseq([Thing], LinePrefix, LineSuffix, Context, Fun) -> Txt.
%%  Format a sequence vertically.

format_vseq([H], _Pre, _Suf, Ctxt, Fun) ->
    Fun(H, Ctxt);
format_vseq([H|T], Pre, Suf, Ctxt, Fun) ->
    [Fun(H, Ctxt),Suf,nl_indent(Ctxt),Pre|
     format_vseq(T, Pre, Suf, Ctxt, Fun)];
format_vseq([], _, _, _, _) -> "".

format_fa_pair({F,A}, _Ctxt) -> [core_atom(F),$/,integer_to_list(A)].

%% format_attribute({Name,Val}, Context) -> Txt.

format_attribute({Name,Val}, Ctxt) when is_list(Val) ->
    Txt = format(#k_atom{val=Name}, Ctxt),
    Ctxt1 = ctxt_bump_indent(Ctxt, width(Txt,Ctxt)+4),
    [Txt," = ",
     $[,format_vseq(Val, "", ",", Ctxt1,
		    fun (A, _C) -> io_lib:write(A) end),$]
    ];
format_attribute({Name,Val}, Ctxt) ->
    Txt = format(#k_atom{val=Name}, Ctxt),
    [Txt," = ",io_lib:write(Val)].

format_list_tail(#k_nil{anno=[]}, _Ctxt) -> "]";
format_list_tail(#k_cons{anno=[],hd=H,tl=T}, Ctxt) ->
    Txt = [$,|format(H, Ctxt)],
    Ctxt1 = ctxt_bump_indent(Ctxt, width(Txt, Ctxt)),
    [Txt|format_list_tail(T, Ctxt1)];
format_list_tail(Tail, Ctxt) ->
    ["|",format(Tail, ctxt_bump_indent(Ctxt, 1)), "]"].

format_bin_seg([], _Ctx) -> "";
format_bin_seg(#k_bin_end{anno=[]}, _Ctxt) -> "";
format_bin_seg(#k_bin_seg{anno=[],next=N}=Seg, Ctxt) ->
    Txt = [$,|format_bin_seg_1(Seg, Ctxt)],
    [Txt|format_bin_seg(N, ctxt_bump_indent(Ctxt, width(Txt, Ctxt)))];
format_bin_seg(Seg, Ctxt) ->
    ["|",format(Seg, ctxt_bump_indent(Ctxt, 2))].

format_bin_seg_1(#k_bin_seg{size=S,unit=U,type=T,flags=Fs,seg=Seg}, Ctxt) ->
    [format(Seg, Ctxt),
     ":",format(S, Ctxt),"*",io_lib:write(U),
     ":",io_lib:write(T),
     [[$-,io_lib:write(F)] || F <- Fs]
    ].

% format_bin_elements(#k_binary_cons{hd=H,tl=T,size=S,info=I}, Ctxt) ->
%     A = canno(T),
%     Fe = fun (Eh, Es, Ei, Ct) ->
% 		 [format(Eh, Ct),":",format(Es, Ct),"/",io_lib:write(Ei)]
% 	 end,
%     case T of
% 	#k_zero_binary{} when A == [] ->
% 	    Fe(H, S, I, Ctxt);
% 	#k_binary_cons{} when A == [] ->
% 	    Txt = [Fe(H, S, I, Ctxt)|","],
% 	    Ctxt1 = ctxt_bump_indent(Ctxt, width(Txt, Ctxt)),
% 	    [Txt|format_bin_elements(T, Ctxt1)];
% 	_ ->
% 	    Txt = [Fe(H, S, I, Ctxt)|"|"],
% 	    [Txt|format(T, ctxt_bump_indent(Ctxt, width(Txt, Ctxt)))]
%     end.

indent(Ctxt) -> indent(Ctxt#ctxt.indent, Ctxt).

indent(N, _Ctxt) when N =< 0 -> "";
indent(N, Ctxt) ->
    T = Ctxt#ctxt.tab_width,
    lists:duplicate(N div T, $\t) ++ lists:duplicate(N rem T, $\s).

nl_indent(Ctxt) -> [$\n|indent(Ctxt)].


unindent(T, Ctxt) ->
    unindent(T, Ctxt#ctxt.indent, Ctxt, []).

unindent(T, N, _Ctxt, C) when N =< 0 ->
    [T|C];
unindent([$\s|T], N, Ctxt, C) ->
    unindent(T, N - 1, Ctxt, C);
unindent([$\t|T], N, Ctxt, C) ->
    Tab = Ctxt#ctxt.tab_width,
    if N >= Tab ->
	    unindent(T, N - Tab, Ctxt, C);
       true ->
            unindent([lists:duplicate(Tab - N, $\s)|T], 0, Ctxt, C)
    end;
unindent([L|T], N, Ctxt, C) when is_list(L) ->
    unindent(L, N, Ctxt, [T|C]);
unindent([H|T], _N, _Ctxt, C) ->
    [H|[T|C]];
unindent([], N, Ctxt, [H|T]) ->
    unindent(H, N, Ctxt, T);
unindent([], _, _, []) -> [].


width(Txt, Ctxt) ->
    width(Txt, 0, Ctxt, []).

width([$\t|T], A, Ctxt, C) ->
    width(T, A + Ctxt#ctxt.tab_width, Ctxt, C);
width([$\n|T], _A, Ctxt, C) ->
    width(unindent([T|C], Ctxt), Ctxt);
width([H|T], A, Ctxt, C) when is_list(H) ->
    width(H, A, Ctxt, [T|C]);
width([_|T], A, Ctxt, C) ->
    width(T, A + 1, Ctxt, C);
width([], A, Ctxt, [H|T]) ->
    width(H, A, Ctxt, T);
width([], A, _, []) -> A.

ctxt_bump_indent(Ctxt, Dx) ->
    Ctxt#ctxt{indent=Ctxt#ctxt.indent + Dx}.

core_atom(A) -> io_lib:write_string(atom_to_list(A), $').

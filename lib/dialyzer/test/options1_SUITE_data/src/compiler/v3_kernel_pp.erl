%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: v3_kernel_pp.erl,v 1.1 2008/12/17 09:53:43 mikpe Exp $
%%
%% Purpose : Kernel Erlang (naive) prettyprinter

-module(v3_kernel_pp).

-include("v3_kernel.hrl").

-export([format/1]).

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

-record(ctxt, {indent = 0,
	       item_indent = 2,
	       body_indent = 2,
	       tab_width = 8}).

canno(Cthing) -> element(2, Cthing).

format(Node) -> format(Node, #ctxt{}).

format(Node, Ctxt) ->
    case canno(Node) of
	[] ->
	    format_1(Node, Ctxt);
	List ->
	    format_anno(List, Ctxt, fun (Ctxt1) -> format_1(Node, Ctxt1) end)
    end.

format_anno(Anno, Ctxt, ObjFun) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, 2),
    ["( ",
     ObjFun(Ctxt1),
     nl_indent(Ctxt1),
     "-| ",io_lib:write(Anno),
     " )"].

%% format_1(Kexpr, Context) -> string().

format_1(#k_atom{val=A}, _Ctxt) -> core_atom(A);
%%format_1(#k_char{val=C}, _Ctxt) -> io_lib:write_char(C);
format_1(#k_float{val=F}, _Ctxt) -> float_to_list(F);
format_1(#k_int{val=I}, _Ctxt) -> integer_to_list(I);
format_1(#k_nil{}, _Ctxt) -> "[]";
format_1(#k_string{val=S}, _Ctxt) -> io_lib:write_string(S);
format_1(#k_var{name=V}, _Ctxt) ->
    if atom(V) ->
	    case atom_to_list(V) of
		[$_|Cs] -> "_X" ++ Cs;
		[C|Cs] when C >= $A, C =< $Z -> [C|Cs];
		Cs -> [$_|Cs]
	    end;
       integer(V) -> [$_|integer_to_list(V)]
    end;
format_1(#k_cons{hd=H,tl=T}, Ctxt) ->
    Txt = ["["|format(H, ctxt_bump_indent(Ctxt, 1))],
    [Txt|format_list_tail(T, ctxt_bump_indent(Ctxt, width(Txt, Ctxt)))];
format_1(#k_tuple{es=Es}, Ctxt) ->
    [${,
     format_hseq(Es, ",", ctxt_bump_indent(Ctxt, 1), fun format/2),
     $}
    ];
format_1(#k_binary{segs=S}, Ctxt) ->
    ["#<",format(S, ctxt_bump_indent(Ctxt, 2)),">#"];
format_1(#k_bin_seg{}=S, Ctxt) ->
    [format_bin_seg_1(S, Ctxt),
     format_bin_seg(S#k_bin_seg.next, ctxt_bump_indent(Ctxt, 2))];
format_1(#k_bin_end{}, _Ctxt) -> "#<>#";
format_1(#k_local{name=N,arity=A}, Ctxt) ->
    "local " ++ format_fa_pair({N,A}, Ctxt);
format_1(#k_remote{mod=M,name=N,arity=A}, _Ctxt) ->
    %% This is for our internal translator.
    io_lib:format("remote ~s:~s/~w", [format(M),format(N),A]);
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
format_1(#k_test{op=Op,args=As}, Ctxt) ->
    Txt = ["test (",format(Op, ctxt_bump_indent(Ctxt, 6)),$)],
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
     format_ret(Rs, Ctxt1)
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

format_attribute({Name,Val}, Ctxt) when list(Val) ->
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
     lists:map(fun (F) -> [$-,io_lib:write(F)] end, Fs)
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
    string:chars($\t, N div T, string:chars($\s, N rem T)).

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
	    unindent([string:chars($\s, Tab - N)|T], 0, Ctxt, C)
    end;
unindent([L|T], N, Ctxt, C) when list(L) ->
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
width([H|T], A, Ctxt, C) when list(H) ->
    width(H, A, Ctxt, [T|C]);
width([_|T], A, Ctxt, C) ->
    width(T, A + 1, Ctxt, C);
width([], A, Ctxt, [H|T]) ->
    width(H, A, Ctxt, T);
width([], A, _, []) -> A.

ctxt_bump_indent(Ctxt, Dx) ->
    Ctxt#ctxt{indent=Ctxt#ctxt.indent + Dx}.

core_atom(A) -> io_lib:write_string(atom_to_list(A), $').

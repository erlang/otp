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
%%     $Id: v3_life.erl,v 1.2 2010/03/04 13:54:20 maria Exp $
%%
%% Purpose : Convert annotated kernel expressions to annotated beam format.

%% This module creates beam format annotated with variable lifetime
%% information.  Each thing is given an index and for each variable we
%% store the first and last index for its occurrence.  The variable
%% database, VDB, attached to each thing is only relevant internally
%% for that thing.
%%
%% For nested things like matches the numbering continues locally and
%% the VDB for that thing refers to the variable usage within that
%% thing.  Variables which live through a such a thing are internally
%% given a very large last index.  Internally the indexes continue
%% after the index of that thing.  This creates no problems as the
%% internal variable info never escapes and externally we only see
%% variable which are alive both before or after.
%%
%% This means that variables never "escape" from a thing and the only
%% way to get values from a thing is to "return" them, with 'break' or
%% 'return'.  Externally these values become the return values of the
%% thing.  This is no real limitation as most nested things have
%% multiple threads so working out a common best variable usage is
%% difficult.

-module(v3_life).

-export([module/2]).

-export([vdb_find/2]).

-import(lists, [map/2,foldl/3]).
-import(ordsets, [add_element/2,intersection/2,union/2,union/1]).

-include("v3_kernel.hrl").
-include("v3_life.hrl").

%% These are not defined in v3_kernel.hrl.
get_kanno(Kthing) -> element(2, Kthing).
%%set_kanno(Kthing, Anno) -> setelement(2, Kthing, Anno).

module(#k_mdef{name=M,exports=Es,attributes=As,body=Fs0}, Opts) ->
    put(?MODULE, Opts),
    Fs1 = map(fun function/1, Fs0),
    erase(?MODULE),
    {ok,{M,Es,As,Fs1}}.

%% function(Kfunc) -> Func.

function(#k_fdef{func=F,arity=Ar,vars=Vs,body=Kb}) ->
    %%ok = io:fwrite("life ~w: ~p~n", [?LINE,{F,Ar}]),
    As = var_list(Vs),
    Vdb0 = foldl(fun ({var,N}, Vdb) -> new_var(N, 0, Vdb) end, [], As),
    %% Force a top-level match!
    B0 = case Kb of
	     #k_match{} -> Kb;
	     _ ->
		 Ka = get_kanno(Kb),
		 #k_match{anno=#k{us=Ka#k.us,ns=[],a=Ka#k.a},
			  vars=Vs,body=Kb,ret=[]}
	 end,
    {B1,_,Vdb1} = body(B0, 1, Vdb0),
    {function,F,Ar,As,B1,Vdb1}.

%% body(Kbody, I, Vdb) -> {[Expr],MaxI,Vdb}.
%%  Handle a body, need special cases for transforming match_fails.
%%  We KNOW that they only occur last in a body.

body(#k_seq{arg=#k_put{anno=Pa,arg=Arg,ret=[R]},
	    body=#k_enter{anno=Ea,op=#k_internal{name=match_fail,arity=1},
			  args=[R]}},
      I, Vdb0) ->
    Vdb1 = use_vars(Pa#k.us, I, Vdb0),		%All used here
    {[match_fail(Arg, I, Pa#k.a ++ Ea#k.a)],I,Vdb1};
body(#k_enter{anno=Ea,op=#k_internal{name=match_fail,arity=1},args=[Arg]},
      I, Vdb0) ->
    Vdb1 = use_vars(Ea#k.us, I, Vdb0),
    {[match_fail(Arg, I, Ea#k.a)],I,Vdb1};
body(#k_seq{arg=Ke,body=Kb}, I, Vdb0) ->
    %%ok = io:fwrite("life ~w:~p~n", [?LINE,{Ke,I,Vdb0}]),
    A = get_kanno(Ke),
    Vdb1 = use_vars(A#k.us, I, new_vars(A#k.ns, I, Vdb0)),
    {Es,MaxI,Vdb2} = body(Kb, I+1, Vdb1),
    E = expr(Ke, I, Vdb2),
    {[E|Es],MaxI,Vdb2};
body(Ke, I, Vdb0) ->
    %%ok = io:fwrite("life ~w:~p~n", [?LINE,{Ke,I,Vdb0}]),
    A = get_kanno(Ke),
    Vdb1 = use_vars(A#k.us, I, new_vars(A#k.ns, I, Vdb0)),
    E = expr(Ke, I, Vdb1),
    {[E],I,Vdb1}.

%% guard(Kguard, I, Vdb) -> Guard.

guard(#k_try{anno=A,arg=Ts,vars=[#k_var{name=X}],body=#k_var{name=X},
	     handler=#k_atom{val=false},ret=Rs}, I, Vdb) ->
    %% Lock variables that are alive before try and used afterwards.
    %% Don't lock variables that are only used inside the try expression.
    Pdb0 = vdb_sub(I, I+1, Vdb),
    {T,MaxI,Pdb1} = guard_body(Ts, I+1, Pdb0),
    Pdb2 = use_vars(A#k.ns, MaxI+1, Pdb1),	%Save "return" values
    #l{ke={protected,T,var_list(Rs)},i=I,a=A#k.a,vdb=Pdb2};
guard(#k_seq{}=G, I, Vdb0) ->
    {Es,_,Vdb1} = guard_body(G, I, Vdb0),
    #l{ke={block,Es},i=I,vdb=Vdb1,a=[]};
guard(G, I, Vdb) -> guard_expr(G, I, Vdb).

%% guard_body(Kbody, I, Vdb) -> {[Expr],MaxI,Vdb}.

guard_body(#k_seq{arg=Ke,body=Kb}, I, Vdb0) ->
    A = get_kanno(Ke),
    Vdb1 = use_vars(A#k.us, I, new_vars(A#k.ns, I, Vdb0)),
    {Es,MaxI,Vdb2} = guard_body(Kb, I+1, Vdb1),
    E = guard_expr(Ke, I, Vdb2),
    {[E|Es],MaxI,Vdb2};
guard_body(Ke, I, Vdb0) ->
    A = get_kanno(Ke),
    Vdb1 = use_vars(A#k.us, I, new_vars(A#k.ns, I, Vdb0)),
    E = guard_expr(Ke, I, Vdb1),
    {[E],I,Vdb1}.

%% guard_expr(Call, I, Vdb) -> Expr

guard_expr(#k_test{anno=A,op=Op,args=As}, I, _Vdb) ->
    #l{ke={test,test_op(Op),atomic_list(As)},i=I,a=A#k.a};
guard_expr(#k_bif{anno=A,op=Op,args=As,ret=Rs}, I, _Vdb) ->
    #l{ke={bif,bif_op(Op),atomic_list(As),var_list(Rs)},i=I,a=A#k.a};
guard_expr(#k_put{anno=A,arg=Arg,ret=Rs}, I, _Vdb) ->
    #l{ke={set,var_list(Rs),literal(Arg)},i=I,a=A#k.a};
guard_expr(#k_match{anno=A,body=Kb,ret=Rs}, I, Vdb) ->
    %% Experimental support for andalso/orelse in guards.
    %% Work out imported variables which need to be locked.
    Mdb = vdb_sub(I, I+1, Vdb),
    M = match(Kb, A#k.us, I+1, Mdb),
    #l{ke={match,M,var_list(Rs)},i=I,vdb=use_vars(A#k.us, I+1, Mdb),a=A#k.a};
guard_expr(G, I, Vdb) -> guard(G, I, Vdb).

%% expr(Kexpr, I, Vdb) -> Expr.

expr(#k_call{anno=A,op=Op,args=As,ret=Rs}, I, _Vdb) ->
    #l{ke={call,call_op(Op),atomic_list(As),var_list(Rs)},i=I,a=A#k.a};
expr(#k_enter{anno=A,op=Op,args=As}, I, _Vdb) ->
    #l{ke={enter,call_op(Op),atomic_list(As)},i=I,a=A#k.a};
expr(#k_bif{anno=A,op=Op,args=As,ret=Rs}, I, _Vdb) ->
    Bif = k_bif(A, Op, As, Rs),
    #l{ke=Bif,i=I,a=A#k.a};
expr(#k_match{anno=A,body=Kb,ret=Rs}, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Mdb = vdb_sub(I, I+1, Vdb),
    M = match(Kb, A#k.us, I+1, Mdb),
    #l{ke={match,M,var_list(Rs)},i=I,vdb=use_vars(A#k.us, I+1, Mdb),a=A#k.a};
expr(#k_try{anno=A,arg=Ka,vars=Vs,body=Kb,evars=Evs,handler=Kh,ret=Rs}, I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the try.
    Tdb0 = vdb_sub(I, I+1, Vdb),
    %% This is the tricky bit. Lock variables in Arg that are used in
    %% the body and handler. Add try tag 'variable'.
    Ab = get_kanno(Kb),
    Ah = get_kanno(Kh),
    Tdb1 = use_vars(Ab#k.us, I+3, use_vars(Ah#k.us, I+3, Tdb0)),
    Tdb2 = vdb_sub(I, I+2, Tdb1),
    Vnames = fun (Kvar) -> Kvar#k_var.name end,	%Get the variable names
    {Aes,_,Adb} = body(Ka, I+2, add_var({catch_tag,I+1}, I+1, 1000000, Tdb2)),
    {Bes,_,Bdb} = body(Kb, I+4, new_vars(map(Vnames, Vs), I+3, Tdb2)),
    {Hes,_,Hdb} = body(Kh, I+4, new_vars(map(Vnames, Evs), I+3, Tdb2)),
    #l{ke={'try',#l{ke={block,Aes},i=I+1,vdb=Adb,a=[]},
	   var_list(Vs),#l{ke={block,Bes},i=I+3,vdb=Bdb,a=[]},
	   var_list(Evs),#l{ke={block,Hes},i=I+3,vdb=Hdb,a=[]},
	   var_list(Rs)},
       i=I,vdb=Tdb1,a=A#k.a};
expr(#k_catch{anno=A,body=Kb,ret=[R]}, I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the catch.
    %% Add catch tag 'variable'.
    Cdb0 = vdb_sub(I, I+1, Vdb),
    {Es,_,Cdb1} = body(Kb, I+1, add_var({catch_tag,I}, I, 1000000, Cdb0)),
    #l{ke={'catch',Es,variable(R)},i=I,vdb=Cdb1,a=A#k.a};
expr(#k_receive{anno=A,var=V,body=Kb,timeout=T,action=Ka,ret=Rs}, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Rdb = vdb_sub(I, I+1, Vdb),
    M = match(Kb, add_element(V#k_var.name, A#k.us), I+1,
	      new_var(V#k_var.name, I, Rdb)),
    {Tes,_,Adb} = body(Ka, I+1, Rdb),
    #l{ke={receive_loop,atomic_lit(T),variable(V),M,
	   #l{ke=Tes,i=I+1,vdb=Adb,a=[]},var_list(Rs)},
       i=I,vdb=use_vars(A#k.us, I+1, Vdb),a=A#k.a};
expr(#k_receive_accept{anno=A}, I, _Vdb) ->
    #l{ke=receive_accept,i=I,a=A#k.a};
expr(#k_receive_next{anno=A}, I, _Vdb) ->
    #l{ke=receive_next,i=I,a=A#k.a};
expr(#k_put{anno=A,arg=Arg,ret=Rs}, I, _Vdb) ->
    #l{ke={set,var_list(Rs),literal(Arg)},i=I,a=A#k.a};
expr(#k_break{anno=A,args=As}, I, _Vdb) ->
    #l{ke={break,atomic_list(As)},i=I,a=A#k.a};
expr(#k_return{anno=A,args=As}, I, _Vdb) ->
    #l{ke={return,atomic_list(As)},i=I,a=A#k.a}.

%% call_op(Op) -> Op.
%% bif_op(Op) -> Op.
%% test_op(Op) -> Op.
%%  Do any necessary name translations here to munge into beam format.

call_op(#k_local{name=N}) -> N;
call_op(#k_remote{mod=M,name=N}) -> {remote,atomic_lit(M),atomic_lit(N)};
call_op(Other) -> variable(Other).

bif_op(#k_remote{mod=#k_atom{val=erlang},name=#k_atom{val=N}}) -> N;
bif_op(#k_internal{name=N}) -> N.

test_op(#k_remote{mod=#k_atom{val=erlang},name=#k_atom{val=N}}) -> N.

%% k_bif(Anno, Op, [Arg], [Ret]) -> Expr.
%%  Build bifs, do special handling of internal some calls.

k_bif(_A, #k_internal{name=dsetelement,arity=3}, As, []) ->
    {bif,dsetelement,atomic_list(As),[]};
k_bif(_A, #k_internal{name=make_fun},
      [#k_atom{val=Fun},#k_int{val=Arity},
       #k_int{val=Index},#k_int{val=Uniq}|Free],
      Rs) ->
    {bif,{make_fun,Fun,Arity,Index,Uniq},var_list(Free),var_list(Rs)};
k_bif(_A, Op, As, Rs) ->
    %% The general case.
    {bif,bif_op(Op),atomic_list(As),var_list(Rs)}.

%% match(Kexpr, [LockVar], I, Vdb) -> Expr.
%%  Convert match tree to old format.

match(#k_alt{anno=A,first=Kf,then=Kt}, Ls, I, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I, Vdb0),
    F = match(Kf, Ls, I+1, Vdb1),
    T = match(Kt, Ls, I+1, Vdb1),
    #l{ke={alt,F,T},i=I,vdb=Vdb1,a=A#k.a};
match(#k_select{anno=A,var=V,types=Kts}, Ls0, I, Vdb0) ->
    Ls1 = add_element(V#k_var.name, Ls0),
    Vdb1 = use_vars(union(A#k.us, Ls1), I, Vdb0),
    Ts = map(fun (Tc) -> type_clause(Tc, Ls1, I+1, Vdb1) end, Kts),
    #l{ke={select,literal(V),Ts},i=I,vdb=Vdb1,a=A#k.a};
match(#k_guard{anno=A,clauses=Kcs}, Ls, I, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I, Vdb0),
    Cs = map(fun (G) -> guard_clause(G, Ls, I+1, Vdb1) end, Kcs),
    #l{ke={guard,Cs},i=I,vdb=Vdb1,a=A#k.a};
match(Other, Ls, I, Vdb0) ->
    Vdb1 = use_vars(Ls, I, Vdb0),
    {B,_,Vdb2} = body(Other, I+1, Vdb1),
    #l{ke={block,B},i=I,vdb=Vdb2,a=[]}.

type_clause(#k_type_clause{anno=A,type=T,values=Kvs}, Ls, I, Vdb0) ->
    %%ok = io:format("life ~w: ~p~n", [?LINE,{T,Kvs}]),
    Vdb1 = use_vars(union(A#k.us, Ls), I+1, Vdb0),
    Vs = map(fun (Vc) -> val_clause(Vc, Ls, I+1, Vdb1) end, Kvs),
    #l{ke={type_clause,type(T),Vs},i=I,vdb=Vdb1,a=A#k.a}.

val_clause(#k_val_clause{anno=A,val=V,body=Kb}, Ls0, I, Vdb0) ->
    {_Used,New} = match_pat_vars(V),
    %% Not clear yet how Used should be used.
    Bus = (get_kanno(Kb))#k.us,
    %%ok = io:format("Ls0 = ~p, Used=~p\n  New=~p, Bus=~p\n", [Ls0,Used,New,Bus]),
    Ls1 = union(intersection(New, Bus), Ls0),	%Lock for safety
    Vdb1 = use_vars(union(A#k.us, Ls1), I+1, new_vars(New, I, Vdb0)),
    B = match(Kb, Ls1, I+1, Vdb1),
    #l{ke={val_clause,literal(V),B},i=I,vdb=use_vars(Bus, I+1, Vdb1),a=A#k.a}.

guard_clause(#k_guard_clause{anno=A,guard=Kg,body=Kb}, Ls, I, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I+2, Vdb0),
    Gdb = vdb_sub(I+1, I+2, Vdb1),
    G = guard(Kg, I+1, Gdb),
    B = match(Kb, Ls, I+2, Vdb1),
    #l{ke={guard_clause,G,B},
       i=I,vdb=use_vars((get_kanno(Kg))#k.us, I+2, Vdb1),
       a=A#k.a}.

%% match_fail(FailValue, I, Anno) -> Expr.
%%  Generate the correct match_fail instruction.  N.B. there is no
%%  generic case for when the fail value has been created elsewhere.

match_fail(#k_tuple{es=[#k_atom{val=function_clause}|As]}, I, A) ->
    #l{ke={match_fail,{function_clause,literal_list(As)}},i=I,a=A};
match_fail(#k_tuple{es=[#k_atom{val=badmatch},Val]}, I, A) ->
    #l{ke={match_fail,{badmatch,literal(Val)}},i=I,a=A};
match_fail(#k_tuple{es=[#k_atom{val=case_clause},Val]}, I, A) ->
    #l{ke={match_fail,{case_clause,literal(Val)}},i=I,a=A};
match_fail(#k_atom{val=if_clause}, I, A) ->
    #l{ke={match_fail,if_clause},i=I,a=A};
match_fail(#k_tuple{es=[#k_atom{val=try_clause},Val]}, I, A) ->
    #l{ke={match_fail,{try_clause,literal(Val)}},i=I,a=A}.

%% type(Ktype) -> Type.

type(k_int) -> integer;
type(k_char) -> integer;			%Hhhmmm???
type(k_float) -> float;
type(k_atom) -> atom;
type(k_nil) -> nil;
type(k_cons) -> cons;
type(k_tuple) -> tuple;
type(k_binary) -> binary;
type(k_bin_seg) -> bin_seg;
type(k_bin_end) -> bin_end.

%% variable(Klit) -> Lit.
%% var_list([Klit]) -> [Lit].

variable(#k_var{name=N}) -> {var,N}.

var_list(Ks) -> map(fun variable/1, Ks).

%% atomic_lit(Klit) -> Lit.
%% atomic_list([Klit]) -> [Lit].

atomic_lit(#k_var{name=N}) -> {var,N};
atomic_lit(#k_int{val=I}) -> {integer,I};
atomic_lit(#k_float{val=F}) -> {float,F};
atomic_lit(#k_atom{val=N}) -> {atom,N};
%%atomic_lit(#k_char{val=C}) -> {char,C};
%%atomic_lit(#k_string{val=S}) -> {string,S};
atomic_lit(#k_nil{}) -> nil.

atomic_list(Ks) -> map(fun atomic_lit/1, Ks).

%% literal(Klit) -> Lit.
%% literal_list([Klit]) -> [Lit].

literal(#k_var{name=N}) -> {var,N};
literal(#k_int{val=I}) -> {integer,I};
literal(#k_float{val=F}) -> {float,F};
literal(#k_atom{val=N}) -> {atom,N};
%%literal(#k_char{val=C}) -> {char,C};
literal(#k_string{val=S}) -> {string,S};
literal(#k_nil{}) -> nil;
literal(#k_cons{hd=H,tl=T}) ->
    {cons,[literal(H),literal(T)]};
literal(#k_binary{segs=V}) ->
    case proplists:get_bool(no_new_binaries, get(?MODULE)) of
	true ->
	    {old_binary,literal(V)};
	false ->
	    {binary,literal(V)}
    end;
literal(#k_bin_seg{size=S,unit=U,type=T,flags=Fs,seg=Seg,next=N}) ->
    {bin_seg,literal(S),U,T,Fs,[literal(Seg),literal(N)]};
literal(#k_bin_end{}) -> bin_end;
literal(#k_tuple{es=Es}) ->
    {tuple,literal_list(Es)}.

literal_list(Ks) -> map(fun literal/1, Ks).

%% match_pat_vars(Pattern) -> {[UsedVarName],[NewVarName]}.

match_pat_vars(#k_var{name=N}) -> {[],[N]};
match_pat_vars(#k_int{}) -> {[],[]};
match_pat_vars(#k_float{}) -> {[],[]};
match_pat_vars(#k_atom{}) -> {[],[]};
%%match_pat_vars(#k_char{}) -> {[],[]};
match_pat_vars(#k_string{}) -> {[],[]};
match_pat_vars(#k_nil{}) -> {[],[]};
match_pat_vars(#k_cons{hd=H,tl=T}) ->
    match_pat_list_vars([H,T]);
match_pat_vars(#k_binary{segs=V}) ->
    match_pat_vars(V);
match_pat_vars(#k_bin_seg{size=S,seg=Seg,next=N}) ->
    {U1,New1} = match_pat_vars(Seg),
    {U2,New2} = match_pat_vars(N),
    {[],U3} = match_pat_vars(S),
    {union([U1,U2,U3]),union(New1, New2)};
match_pat_vars(#k_bin_end{}) -> {[],[]};
match_pat_vars(#k_tuple{es=Es}) ->
    match_pat_list_vars(Es).

match_pat_list_vars(Ps) ->
    foldl(fun (P, {Used0,New0}) ->
		  {Used,New} = match_pat_vars(P),
		  {union(Used0, Used),union(New0, New)} end,
	  {[],[]}, Ps).

%% new_var(VarName, I, Vdb) -> Vdb.
%% new_vars([VarName], I, Vdb) -> Vdb.
%% use_var(VarName, I, Vdb) -> Vdb.
%% use_vars([VarName], I, Vdb) -> Vdb.
%% add_var(VarName, F, L, Vdb) -> Vdb.

new_var(V, I, Vdb) ->
    case vdb_find(V, Vdb) of
	{V,F,L} when I < F -> vdb_store(V, I, L, Vdb);
	{V,_,_} -> Vdb;
	error -> vdb_store(V, I, I, Vdb)
    end.

new_vars(Vs, I, Vdb0) ->
    foldl(fun (V, Vdb) -> new_var(V, I, Vdb) end, Vdb0, Vs).

use_var(V, I, Vdb) ->
    case vdb_find(V, Vdb) of
	{V,F,L} when I > L -> vdb_store(V, F, I, Vdb);
	{V,_,_} -> Vdb;
	error -> vdb_store(V, I, I, Vdb)
    end.

use_vars(Vs, I, Vdb0) ->
    foldl(fun (V, Vdb) -> use_var(V, I, Vdb) end, Vdb0, Vs).

add_var(V, F, L, Vdb) ->
    use_var(V, L, new_var(V, F, Vdb)).

vdb_find(V, Vdb) ->
    %% Peformance note: Profiling shows that this function accounts for
    %% a lot of the execution time when huge constants terms are built.
    %% Using the BIF lists:keysearch/3 is a lot faster than the
    %% original Erlang version.
    case lists:keysearch(V, 1, Vdb) of
	{value,Vd} -> Vd;
	false -> error
    end.

%vdb_find(V, [{V1,F,L}=Vd|Vdb]) when V < V1 -> error;
%vdb_find(V, [{V1,F,L}=Vd|Vdb]) when V == V1 -> Vd;
%vdb_find(V, [{V1,F,L}=Vd|Vdb]) when V > V1 -> vdb_find(V, Vdb);
%vdb_find(V, []) -> error.

vdb_store(V, F, L, [{V1,_,_}=Vd|Vdb]) when V > V1 ->
    [Vd|vdb_store(V, F, L, Vdb)];
vdb_store(V, F, L, [{V1,_,_}=Vd|Vdb]) when V < V1 -> [{V,F,L},Vd|Vdb];
vdb_store(V, F, L, [{_V1,_,_}|Vdb]) -> [{V,F,L}|Vdb]; %V == V1
vdb_store(V, F, L, []) -> [{V,F,L}].

%% vdb_sub(Min, Max, Vdb) -> Vdb.
%%  Extract variables which are used before and after Min.  Lock
%%  variables alive after Max.

vdb_sub(Min, Max, Vdb) ->
    [ if L >= Max -> {V,F,1000000};
	 true -> Vd
      end || {V,F,L}=Vd <- Vdb, F < Min, L >= Min ].

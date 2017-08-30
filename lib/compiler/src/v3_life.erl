%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-import(lists, [member/2,map/2,reverse/1,sort/1]).
-import(ordsets, [add_element/2,intersection/2,union/2]).

-include("v3_kernel.hrl").
-include("v3_life.hrl").

%% These are not defined in v3_kernel.hrl.
get_kanno(Kthing) -> element(2, Kthing).
%%set_kanno(Kthing, Anno) -> setelement(2, Kthing, Anno).

module(#k_mdef{name=M,exports=Es,attributes=As,body=Fs0}, _Opts) ->
    Fs1 = functions(Fs0, []),
    {ok,{M,Es,As,Fs1}}.

functions([F|Fs], Acc) ->
    functions(Fs, [function(F)|Acc]);
functions([], Acc) -> reverse(Acc).

%% function(Kfunc) -> Func.

function(#k_fdef{anno=#k{a=Anno},func=F,arity=Ar,vars=Vs,body=Kb}) ->
    try
	As = var_list(Vs),
	Vdb0 = init_vars(As),
	%% Force a top-level match!
	B0 = case Kb of
		 #k_match{} -> Kb;
		 _ ->
		     Ka = get_kanno(Kb),
		     #k_match{anno=#k{us=Ka#k.us,ns=[],a=Ka#k.a},
			      vars=Vs,body=Kb,ret=[]}
	     end,
	put(guard_refc, 0),
	{B1,_,Vdb1} = body(B0, 1, Vdb0),
	erase(guard_refc),
	{function,F,Ar,As,B1,Vdb1,Anno}
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [F,Ar]),
	    erlang:raise(Class, Error, Stack)
    end.

%% body(Kbody, I, Vdb) -> {[Expr],MaxI,Vdb}.
%%  Handle a body.

body(#k_seq{arg=Ke,body=Kb}, I, Vdb0) ->
    %%ok = io:fwrite("life ~w:~p~n", [?LINE,{Ke,I,Vdb0}]),
    A = get_kanno(Ke),
    Vdb1 = use_vars(union(A#k.us, A#k.ns), I, Vdb0),
    {Es,MaxI,Vdb2} = body(Kb, I+1, Vdb1),
    E = expr(Ke, I, Vdb2),
    {[E|Es],MaxI,Vdb2};
body(Ke, I, Vdb0) ->
    %%ok = io:fwrite("life ~w:~p~n", [?LINE,{Ke,I,Vdb0}]),
    A = get_kanno(Ke),
    Vdb1 = use_vars(union(A#k.us, A#k.ns), I, Vdb0),
    E = expr(Ke, I, Vdb1),
    {[E],I,Vdb1}.

%% guard(Kguard, I, Vdb) -> Guard.

guard(#k_try{anno=A,arg=Ts,vars=[#k_var{name=X}],body=#k_var{name=X},
	     handler=#k_atom{val=false},ret=Rs}, I, Vdb) ->
    %% Lock variables that are alive before try and used afterwards.
    %% Don't lock variables that are only used inside the try expression.
    Pdb0 = vdb_sub(I, I+1, Vdb),
    {T,MaxI,Pdb1} = body(Ts, I+1, Pdb0),
    Pdb2 = use_vars(A#k.ns, MaxI+1, Pdb1),	%Save "return" values
    #l{ke={protected,T,var_list(Rs)},i=I,a=A#k.a,vdb=Pdb2}.

%% expr(Kexpr, I, Vdb) -> Expr.

expr(#k_test{anno=A,op=Op,args=As}, I, _Vdb) ->
    #l{ke={test,test_op(Op),atomic_list(As)},i=I,a=A#k.a};
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
    M = match(Kb, A#k.us, I+1, [], Mdb),
    #l{ke={match,M,var_list(Rs)},i=I,vdb=use_vars(A#k.us, I+1, Mdb),a=A#k.a};
expr(#k_guard_match{anno=A,body=Kb,ret=Rs}, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Mdb = vdb_sub(I, I+1, Vdb),
    M = match(Kb, A#k.us, I+1, [], Mdb),
    #l{ke={guard_match,M,var_list(Rs)},i=I,vdb=use_vars(A#k.us, I+1, Mdb),a=A#k.a};
expr(#k_try{}=Try, I, Vdb) ->
    case is_in_guard() of
	false -> body_try(Try, I, Vdb);
	true -> guard(Try, I, Vdb)
    end;
expr(#k_try_enter{anno=A,arg=Ka,vars=Vs,body=Kb,evars=Evs,handler=Kh}, I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the try.
    Tdb0 = vdb_sub(I, I+1, Vdb),
    %% This is the tricky bit. Lock variables in Arg that are used in
    %% the body and handler. Add try tag 'variable'.
    Ab = get_kanno(Kb),
    Ah = get_kanno(Kh),
    Tdb1 = use_vars(union(Ab#k.us, Ah#k.us), I+3, Tdb0),
    Tdb2 = vdb_sub(I, I+2, Tdb1),
    Vnames = fun (Kvar) -> Kvar#k_var.name end,	%Get the variable names
    {Aes,_,Adb} = body(Ka, I+2, add_var({catch_tag,I+1}, I+1, 1000000, Tdb2)),
    {Bes,_,Bdb} = body(Kb, I+4, new_vars(sort(map(Vnames, Vs)), I+3, Tdb2)),
    {Hes,_,Hdb} = body(Kh, I+4, new_vars(sort(map(Vnames, Evs)), I+3, Tdb2)),
    #l{ke={try_enter,#l{ke={block,Aes},i=I+1,vdb=Adb,a=[]},
	   var_list(Vs),#l{ke={block,Bes},i=I+3,vdb=Bdb,a=[]},
	   var_list(Evs),#l{ke={block,Hes},i=I+3,vdb=Hdb,a=[]}},
       i=I,vdb=Tdb1,a=A#k.a};
expr(#k_catch{anno=A,body=Kb,ret=[R]}, I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the catch.
    %% Add catch tag 'variable'.
    Cdb0 = vdb_sub(I, I+1, Vdb),
    {Es,_,Cdb1} = body(Kb, I+1, add_var({catch_tag,I}, I, locked, Cdb0)),
    #l{ke={'catch',Es,variable(R)},i=I,vdb=Cdb1,a=A#k.a};
expr(#k_receive{anno=A,var=V,body=Kb,timeout=T,action=Ka,ret=Rs}, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Rdb = vdb_sub(I, I+1, Vdb),
    M = match(Kb, add_element(V#k_var.name, A#k.us), I+1, [],
	      new_vars([V#k_var.name], I, Rdb)),
    {Tes,_,Adb} = body(Ka, I+1, Rdb),
    #l{ke={receive_loop,atomic(T),variable(V),M,
	   #l{ke=Tes,i=I+1,vdb=Adb,a=[]},var_list(Rs)},
       i=I,vdb=use_vars(A#k.us, I+1, Vdb),a=A#k.a};
expr(#k_receive_accept{anno=A}, I, _Vdb) ->
    #l{ke=receive_accept,i=I,a=A#k.a};
expr(#k_receive_next{anno=A}, I, _Vdb) ->
    #l{ke=receive_next,i=I,a=A#k.a};
expr(#k_put{anno=A,arg=Arg,ret=Rs}, I, _Vdb) ->
    #l{ke={set,var_list(Rs),literal(Arg, [])},i=I,a=A#k.a};
expr(#k_break{anno=A,args=As}, I, _Vdb) ->
    #l{ke={break,atomic_list(As)},i=I,a=A#k.a};
expr(#k_guard_break{anno=A,args=As}, I, Vdb) ->
    Locked = [V || {V,_,_} <- Vdb],
    #l{ke={guard_break,atomic_list(As),Locked},i=I,a=A#k.a};
expr(#k_return{anno=A,args=As}, I, _Vdb) ->
    #l{ke={return,atomic_list(As)},i=I,a=A#k.a}.

body_try(#k_try{anno=A,arg=Ka,vars=Vs,body=Kb,evars=Evs,handler=Kh,ret=Rs},
	 I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the try.
    Tdb0 = vdb_sub(I, I+1, Vdb),
    %% This is the tricky bit. Lock variables in Arg that are used in
    %% the body and handler. Add try tag 'variable'.
    Ab = get_kanno(Kb),
    Ah = get_kanno(Kh),
    Tdb1 = use_vars(union(Ab#k.us, Ah#k.us), I+3, Tdb0),
    Tdb2 = vdb_sub(I, I+2, Tdb1),
    Vnames = fun (Kvar) -> Kvar#k_var.name end,	%Get the variable names
    {Aes,_,Adb} = body(Ka, I+2, add_var({catch_tag,I+1}, I+1, locked, Tdb2)),
    {Bes,_,Bdb} = body(Kb, I+4, new_vars(sort(map(Vnames, Vs)), I+3, Tdb2)),
    {Hes,_,Hdb} = body(Kh, I+4, new_vars(sort(map(Vnames, Evs)), I+3, Tdb2)),
    #l{ke={'try',#l{ke={block,Aes},i=I+1,vdb=Adb,a=[]},
	   var_list(Vs),#l{ke={block,Bes},i=I+3,vdb=Bdb,a=[]},
	   var_list(Evs),#l{ke={block,Hes},i=I+3,vdb=Hdb,a=[]},
	   var_list(Rs)},
       i=I,vdb=Tdb1,a=A#k.a}.

%% call_op(Op) -> Op.
%% bif_op(Op) -> Op.
%% test_op(Op) -> Op.
%%  Do any necessary name translations here to munge into beam format.

call_op(#k_local{name=N}) -> N; 
call_op(#k_remote{mod=M,name=N}) -> {remote,atomic(M),atomic(N)};
call_op(Other) -> variable(Other).

bif_op(#k_remote{mod=#k_atom{val=erlang},name=#k_atom{val=N}}) -> N;
bif_op(#k_internal{name=N}) -> N.

test_op(#k_remote{mod=#k_atom{val=erlang},name=#k_atom{val=N}}) -> N.

%% k_bif(Anno, Op, [Arg], [Ret], Vdb) -> Expr.
%%  Build bifs, do special handling of internal some calls.

k_bif(_A, #k_internal{name=dsetelement,arity=3}, As, []) ->
    {bif,dsetelement,atomic_list(As),[]};
k_bif(_A, #k_internal{name=bs_context_to_binary=Op,arity=1}, As, []) ->
    {bif,Op,atomic_list(As),[]};
k_bif(_A, #k_internal{name=bs_init_writable=Op,arity=1}, As, Rs) ->
    {bif,Op,atomic_list(As),var_list(Rs)};
k_bif(_A, #k_internal{name=make_fun},
      [#k_atom{val=Fun},#k_int{val=Arity},
       #k_int{val=Index},#k_int{val=Uniq}|Free],
      Rs) ->
    {bif,{make_fun,Fun,Arity,Index,Uniq},var_list(Free),var_list(Rs)};
k_bif(_A, Op, As, Rs) ->
    %% The general case.
    Name = bif_op(Op),
    Ar = length(As),
    case is_gc_bif(Name, Ar) of
	false ->
	    {bif,Name,atomic_list(As),var_list(Rs)};
	true ->
	    {gc_bif,Name,atomic_list(As),var_list(Rs)}
    end.

%% match(Kexpr, [LockVar], I, Vdb) -> Expr.
%%  Convert match tree to old format.

match(#k_alt{anno=A,first=Kf,then=Kt}, Ls, I, Ctxt, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I, Vdb0),
    F = match(Kf, Ls, I+1, Ctxt, Vdb1),
    T = match(Kt, Ls, I+1, Ctxt, Vdb1),
    #l{ke={alt,F,T},i=I,vdb=Vdb1,a=A#k.a};
match(#k_select{anno=A,var=V,types=Kts}, Ls0, I, Ctxt, Vdb0) ->
    Vanno = get_kanno(V),
    Ls1 = case member(no_usage, Vanno) of
	      false -> add_element(V#k_var.name, Ls0);
	      true -> Ls0
	  end,
    Anno = case member(reuse_for_context, Vanno) of
	       true -> [reuse_for_context|A#k.a];
	       false -> A#k.a
	   end,
    Vdb1 = use_vars(union(A#k.us, Ls1), I, Vdb0),
    Ts = [type_clause(Tc, Ls1, I+1, Ctxt, Vdb1) || Tc <- Kts],
    #l{ke={select,literal(V, Ctxt),Ts},i=I,vdb=Vdb1,a=Anno};
match(#k_guard{anno=A,clauses=Kcs}, Ls, I, Ctxt, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I, Vdb0),
    Cs = [guard_clause(G, Ls, I+1, Ctxt, Vdb1) || G <- Kcs],
    #l{ke={guard,Cs},i=I,vdb=Vdb1,a=A#k.a};
match(Other, Ls, I, _Ctxt, Vdb0) ->
    Vdb1 = use_vars(Ls, I, Vdb0),
    {B,_,Vdb2} = body(Other, I+1, Vdb1),
    #l{ke={block,B},i=I,vdb=Vdb2,a=[]}.

type_clause(#k_type_clause{anno=A,type=T,values=Kvs}, Ls, I, Ctxt, Vdb0) ->
    %%ok = io:format("life ~w: ~p~n", [?LINE,{T,Kvs}]),
    Vdb1 = use_vars(union(A#k.us, Ls), I+1, Vdb0),
    Vs = [val_clause(Vc, Ls, I+1, Ctxt, Vdb1) || Vc <- Kvs],
    #l{ke={type_clause,type(T),Vs},i=I,vdb=Vdb1,a=A#k.a}.

val_clause(#k_val_clause{anno=A,val=V,body=Kb}, Ls0, I, Ctxt0, Vdb0) ->
    New = (get_kanno(V))#k.ns,
    Bus = (get_kanno(Kb))#k.us,
    %%ok = io:format("Ls0 = ~p, Used=~p\n  New=~p, Bus=~p\n", [Ls0,Used,New,Bus]),
    Ls1 = union(intersection(New, Bus), Ls0),	%Lock for safety
    Vdb1 = use_vars(union(A#k.us, Ls1), I+1, new_vars(New, I, Vdb0)),
    Ctxt = case V of
	       #k_binary{segs=#k_var{name=C0}} -> C0;
	       _ -> Ctxt0
	   end,
    B = match(Kb, Ls1, I+1, Ctxt, Vdb1),
    #l{ke={val_clause,literal(V, Ctxt),B},i=I,vdb=use_vars(Bus, I+1, Vdb1),a=A#k.a}.

guard_clause(#k_guard_clause{anno=A,guard=Kg,body=Kb}, Ls, I, Ctxt, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I+2, Vdb0),
    Gdb = vdb_sub(I+1, I+2, Vdb1),
    OldRefc = put(guard_refc, get(guard_refc)+1),
    G = guard(Kg, I+1, Gdb),
    put(guard_refc, OldRefc),
    B = match(Kb, Ls, I+2, Ctxt, Vdb1),
    #l{ke={guard_clause,G,B},
       i=I,vdb=use_vars((get_kanno(Kg))#k.us, I+2, Vdb1),
       a=A#k.a}.

%% type(Ktype) -> Type.

type(k_literal) -> literal;
type(k_int) -> integer;
%%type(k_char) -> integer;			%Hhhmmm???
type(k_float) -> float;
type(k_atom) -> atom;
type(k_nil) -> nil;
type(k_cons) -> cons;
type(k_tuple) -> tuple;
type(k_binary) -> binary;
type(k_bin_seg) -> bin_seg;
type(k_bin_int) -> bin_int;
type(k_bin_end) -> bin_end;
type(k_map) -> map.

%% variable(Klit) -> Lit.
%% var_list([Klit]) -> [Lit].

variable(#k_var{name=N}) -> {var,N}.

var_list(Ks) -> [variable(K) || K <- Ks].

%% atomic(Klit) -> Lit.
%% atomic_list([Klit]) -> [Lit].

atomic(#k_literal{val=V}) -> {literal,V};
atomic(#k_var{name=N}) -> {var,N};
atomic(#k_int{val=I}) -> {integer,I};
atomic(#k_float{val=F}) -> {float,F};
atomic(#k_atom{val=N}) -> {atom,N};
%%atomic(#k_char{val=C}) -> {char,C};
atomic(#k_nil{}) -> nil.

atomic_list(Ks) -> [atomic(K) || K <- Ks].

%% literal(Klit) -> Lit.
%% literal_list([Klit]) -> [Lit].

literal(#k_var{name=N}, _) -> {var,N};
literal(#k_literal{val=I}, _) -> {literal,I};
literal(#k_int{val=I}, _) -> {integer,I};
literal(#k_float{val=F}, _) -> {float,F};
literal(#k_atom{val=N}, _) -> {atom,N};
%%literal(#k_char{val=C}, _) -> {char,C};
literal(#k_nil{}, _) -> nil;
literal(#k_cons{hd=H,tl=T}, Ctxt) ->
    {cons,[literal(H, Ctxt),literal(T, Ctxt)]};
literal(#k_binary{segs=V}, Ctxt) ->
    {binary,literal(V, Ctxt)};
literal(#k_bin_seg{size=S,unit=U,type=T,flags=Fs,seg=Seg,next=[]}, Ctxt) ->
    %% Only occurs in patterns.
    {bin_seg,Ctxt,literal(S, Ctxt),U,T,Fs,[literal(Seg, Ctxt)]};
literal(#k_bin_seg{size=S,unit=U,type=T,flags=Fs,seg=Seg,next=N}, Ctxt) ->
    {bin_seg,Ctxt,literal(S, Ctxt),U,T,Fs,
     [literal(Seg, Ctxt),literal(N, Ctxt)]};
literal(#k_bin_int{size=S,unit=U,flags=Fs,val=Int,next=N}, Ctxt) ->
    %% Only occurs in patterns.
    {bin_int,Ctxt,literal(S, Ctxt),U,Fs,Int,
     [literal(N, Ctxt)]};
literal(#k_bin_end{}, Ctxt) ->
    {bin_end,Ctxt};
literal(#k_tuple{es=Es}, Ctxt) ->
    {tuple,literal_list(Es, Ctxt)};
literal(#k_map{op=Op,var=Var,es=Es0}, Ctxt) ->
    {map,Op,literal(Var, Ctxt),literal_list(Es0, Ctxt)};
literal(#k_map_pair{key=K,val=V}, Ctxt) ->
    {map_pair,literal(K, Ctxt),literal(V, Ctxt)}.

literal_list(Ks, Ctxt) ->
    [literal(K, Ctxt) || K <- Ks].


%% is_gc_bif(Name, Arity) -> true|false
%%  Determines whether the BIF Name/Arity might do a GC.

is_gc_bif(hd, 1) -> false;
is_gc_bif(tl, 1) -> false;
is_gc_bif(self, 0) -> false;
is_gc_bif(node, 0) -> false;
is_gc_bif(node, 1) -> false;
is_gc_bif(element, 2) -> false;
is_gc_bif(get, 1) -> false;
is_gc_bif(raise, 2) -> false;
is_gc_bif(tuple_size, 1) -> false;
is_gc_bif(Bif, Arity) ->
    not (erl_internal:bool_op(Bif, Arity) orelse
	 erl_internal:new_type_test(Bif, Arity) orelse
	 erl_internal:comp_op(Bif, Arity)).

%% Keep track of life time for variables.
%%
%% init_vars([{var,VarName}]) -> Vdb.
%% new_vars([VarName], I, Vdb) -> Vdb.
%% use_vars([VarName], I, Vdb) -> Vdb.
%% add_var(VarName, F, L, Vdb) -> Vdb.
%%
%% The list of variable names for new_vars/3 and use_vars/3
%% must be sorted.

init_vars(Vs) ->
    vdb_new(Vs).

new_vars([], _, Vdb) -> Vdb;
new_vars([V], I, Vdb) -> vdb_store_new(V, {V,I,I}, Vdb);
new_vars(Vs, I, Vdb) -> vdb_update_vars(Vs, Vdb, I).

use_vars([], _, Vdb) ->
    Vdb;
use_vars([V], I, Vdb) ->
    case vdb_find(V, Vdb) of
	{V,F,L} when I > L -> vdb_update(V, {V,F,I}, Vdb);
	{V,_,_} -> Vdb;
	error -> vdb_store_new(V, {V,I,I}, Vdb)
    end;
use_vars(Vs, I, Vdb) -> vdb_update_vars(Vs, Vdb, I).

add_var(V, F, L, Vdb) ->
    vdb_store_new(V, {V,F,L}, Vdb).

%% is_in_guard() -> true|false.

is_in_guard() ->
    get(guard_refc) > 0.

%% vdb

vdb_new(Vs) ->
    sort([{V,0,0} || {var,V} <- Vs]).

vdb_find(V, Vdb) ->
    case lists:keyfind(V, 1, Vdb) of
	false -> error;
	Vd -> Vd
    end.

vdb_update(V, Update, [{V,_,_}|Vdb]) ->
    [Update|Vdb];
vdb_update(V, Update, [Vd|Vdb]) ->
    [Vd|vdb_update(V, Update, Vdb)].

vdb_store_new(V, New, [{V1,_,_}=Vd|Vdb]) when V > V1 ->
    [Vd|vdb_store_new(V, New, Vdb)];
vdb_store_new(V, New, [{V1,_,_}|_]=Vdb) when V < V1 ->
    [New|Vdb];
vdb_store_new(_, New, []) -> [New].

vdb_update_vars([V|_]=Vs, [{V1,_,_}=Vd|Vdb], I) when V > V1 ->
    [Vd|vdb_update_vars(Vs, Vdb, I)];
vdb_update_vars([V|Vs], [{V1,_,_}|_]=Vdb, I) when V < V1 ->
    %% New variable.
    [{V,I,I}|vdb_update_vars(Vs, Vdb, I)];
vdb_update_vars([V|Vs], [{_,F,L}=Vd|Vdb], I) ->
    %% Existing variable.
    if
	I > L -> [{V,F,I}|vdb_update_vars(Vs, Vdb, I)];
	true ->  [Vd|vdb_update_vars(Vs, Vdb, I)]
    end;
vdb_update_vars([V|Vs], [], I) ->
    %% New variable.
    [{V,I,I}|vdb_update_vars(Vs, [], I)];
vdb_update_vars([], Vdb, _) -> Vdb.

%% vdb_sub(Min, Max, Vdb) -> Vdb.
%%  Extract variables which are used before and after Min.  Lock
%%  variables alive after Max.

vdb_sub(Min, Max, Vdb) ->
    [ if L >= Max -> {V,F,locked};
	 true -> Vd
      end || {V,F,L}=Vd <- Vdb, F < Min, L >= Min ].

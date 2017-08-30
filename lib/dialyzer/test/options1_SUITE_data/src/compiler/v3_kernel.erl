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
%%     $Id: v3_kernel.erl,v 1.3 2010/03/04 13:54:20 maria Exp $
%%
%% Purpose : Transform Core Erlang to Kernel Erlang

%% Kernel erlang is like Core Erlang with a few significant
%% differences:
%%
%% 1. It is flat!  There are no nested calls or sub-blocks.
%%
%% 2. All variables are unique in a function.  There is no scoping, or
%% rather the scope is the whole function.
%%
%% 3. Pattern matching (in cases and receives) has been compiled.
%%
%% 4. The annotations contain variable usages.  Seeing we have to work
%% this out anyway for funs we might as well pass it on for free to
%% later passes.
%%
%% 5. All remote-calls are to statically named m:f/a. Meta-calls are
%% passed via erlang:apply/3.
%%
%% The translation is done in two passes:
%%
%% 1. Basic translation, translate variable/function names, flatten
%% completely, pattern matching compilation.
%%
%% 2. Fun-lifting (lambda-lifting), variable usage annotation and
%% last-call handling.
%%
%% All new Kexprs are created in the first pass, they are just
%% annotated in the second.
%%
%% Functions and BIFs
%%
%% Functions are "call"ed or "enter"ed if it is a last call, their
%% return values may be ignored.  BIFs are things which are known to
%% be internal by the compiler and can only be called, their return
%% values cannot be ignored.
%%
%% Letrec's are handled rather naively.  All the functions in one
%% letrec are handled as one block to find the free variables.  While
%% this is not optimal it reflects how letrec's often are used.  We
%% don't have to worry about variable shadowing and nested letrec's as
%% this is handled in the variable/function name translation.  There
%% is a little bit of trickery to ensure letrec transformations fit
%% into the scheme of things.
%%
%% To ensure unique variable names we use a variable substitution
%% table and keep the set of all defined variables.  The nested
%% scoping of Core means that we must also nest the substitution
%% tables, but the defined set must be passed through to match the
%% flat structure of Kernel and to make sure variables with the same
%% name from different scopes get different substitutions.
%%
%% We also use these substitutions to handle the variable renaming
%% necessary in pattern matching compilation.
%%
%% The pattern matching compilation assumes that the values of
%% different types don't overlap.  This means that as there is no
%% character type yet in the machine all characters must be converted
%% to integers!

-module(v3_kernel).

-export([module/2,format_error/1]).

-import(lists, [map/2,foldl/3,foldr/3,mapfoldl/3,splitwith/2,
		member/2,reverse/1,reverse/2]).
-import(ordsets, [add_element/2,del_element/2,union/2,union/1,subtract/2]).

-include("core_parse.hrl").
-include("v3_kernel.hrl").

%% These are not defined in v3_kernel.hrl.
get_kanno(Kthing) -> element(2, Kthing).
set_kanno(Kthing, Anno) -> setelement(2, Kthing, Anno).

%% Internal kernel expressions and help functions.
%% N.B. the annotation field is ALWAYS the first field!

-record(ivalues, {anno=[],args}).
-record(ifun, {anno=[],vars,body}).
-record(iset, {anno=[],vars,arg,body}).
-record(iletrec, {anno=[],defs}).
-record(ialias, {anno=[],vars,pat}).
-record(iclause, {anno=[],sub,pats,guard,body}).
-record(ireceive_accept, {anno=[],arg}).
-record(ireceive_next, {anno=[],arg}).

%% State record for kernel translator.
-record(kern, {func,				%Current function
	       vcount=0,			%Variable counter
	       fcount=0,			%Fun counter
	       ds=[],				%Defined variables
	       funs=[],				%Fun functions
	       free=[],				%Free variables
	       ws=[],				%Warnings.
	       extinstr=false}).		%Generate extended instructions

module(#c_module{anno=A,name=M,exports=Es,attrs=As,defs=Fs}, Options) ->
    ExtInstr = not member(no_new_apply, Options),
    {Kfs,St} = mapfoldl(fun function/2, #kern{extinstr=ExtInstr}, Fs),
    Kes = map(fun (#c_fname{id=N,arity=Ar}) -> {N,Ar} end, Es),
    Kas = map(fun (#c_def{name=#c_atom{val=N},val=V}) ->
		      {N,core_lib:literal_value(V)} end, As),
    {ok,#k_mdef{anno=A,name=M#c_atom.val,exports=Kes,attributes=Kas,
		body=Kfs ++ St#kern.funs},St#kern.ws}.

function(#c_def{anno=Af,name=#c_fname{id=F,arity=Arity},val=Body}, St0) ->
    %%ok = io:fwrite("kern: ~p~n", [{F,Arity}]),
    St1 = St0#kern{func={F,Arity},vcount=0,fcount=0,ds=sets:new()},
    {#ifun{anno=Ab,vars=Kvs,body=B0},[],St2} = expr(Body, new_sub(), St1),
    {B1,_,St3} = ubody(B0, return, St2),
    %%B1 = B0, St3 = St2,				%Null second pass
    {#k_fdef{anno=#k{us=[],ns=[],a=Af ++ Ab},
	     func=F,arity=Arity,vars=Kvs,body=B1},St3}.

%% body(Cexpr, Sub, State) -> {Kexpr,[PreKepxr],State}.
%%  Do the main sequence of a body.  A body ends in an atomic value or
%%  values.  Must check if vector first so do expr.

body(#c_values{anno=A,es=Ces}, Sub, St0) ->
    %% Do this here even if only in bodies.
    {Kes,Pe,St1} = atomic_list(Ces, Sub, St0),
    %%{Kes,Pe,St1} = expr_list(Ces, Sub, St0),
    {#ivalues{anno=A,args=Kes},Pe,St1};
body(#ireceive_next{anno=A}, _, St) ->
    {#k_receive_next{anno=A},[],St};
body(Ce, Sub, St0) ->
    expr(Ce, Sub, St0).

%% guard(Cexpr, Sub, State) -> {Kexpr,State}.
%%  We handle guards almost as bodies. The only special thing we
%%  must do is to make the final Kexpr a #k_test{}.
%%  Also, we wrap the entire guard in a try/catch which is
%%  not strictly needed, but makes sure that every 'bif' instruction
%%  will get a proper failure label.

guard(G0, Sub, St0) ->
    {G1,St1} = wrap_guard(G0, St0),
    {Ge0,Pre,St2} = expr(G1, Sub, St1),
    {Ge,St} = gexpr_test(Ge0, St2),
    {pre_seq(Pre, Ge),St}.

%% Wrap the entire guard in a try/catch if needed.

wrap_guard(#c_try{}=Try, St) -> {Try,St};
wrap_guard(Core, St0) ->
    {VarName,St} = new_var_name(St0),
    Var = #c_var{name=VarName},
    Try = #c_try{arg=Core,vars=[Var],body=Var,evars=[],handler=#c_atom{val=false}},
    {Try,St}.

%% gexpr_test(Kexpr, State) -> {Kexpr,State}.
%%  Builds the final boolean test from the last Kexpr in a guard test.
%%  Must enter try blocks and isets and find the last Kexpr in them.
%%  This must end in a recognised BEAM test!

gexpr_test(#k_bif{anno=A,op=#k_remote{mod=#k_atom{val=erlang},
				      name=#k_atom{val=is_boolean},arity=1}=Op,
		  args=Kargs}, St) ->
    %% XXX Remove this clause in R11. For bootstrap purposes, we must
    %% recognize erlang:is_boolean/1 here.
    {#k_test{anno=A,op=Op,args=Kargs},St};
gexpr_test(#k_bif{anno=A,op=#k_remote{mod=#k_atom{val=erlang},
				      name=#k_atom{val=internal_is_record},arity=3}=Op,
		  args=Kargs}, St) ->
    {#k_test{anno=A,op=Op,args=Kargs},St};
gexpr_test(#k_bif{anno=A,op=#k_remote{mod=#k_atom{val=erlang},
				      name=#k_atom{val=F},arity=Ar}=Op,
		  args=Kargs}=Ke, St) ->
    %% Either convert to test if ok, or add test.
    %% At this stage, erlang:float/1 is not a type test. (It should
    %% have been converted to erlang:is_float/1.)
    case erl_internal:new_type_test(F, Ar) orelse
	erl_internal:comp_op(F, Ar) of
	true -> {#k_test{anno=A,op=Op,args=Kargs},St};
	false -> gexpr_test_add(Ke, St)		%Add equality test
    end;
gexpr_test(#k_try{arg=B0,vars=[#k_var{name=X}],body=#k_var{name=X},
		  handler=#k_atom{val=false}}=Try, St0) ->
    {B,St} = gexpr_test(B0, St0),
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{B0,B}]),
    {Try#k_try{arg=B},St};
gexpr_test(#iset{body=B0}=Iset, St0) ->
    {B1,St1} = gexpr_test(B0, St0),
    {Iset#iset{body=B1},St1};
gexpr_test(Ke, St) -> gexpr_test_add(Ke, St).	%Add equality test

gexpr_test_add(Ke, St0) ->
    Test = #k_remote{mod=#k_atom{val='erlang'},
		     name=#k_atom{val='=:='},
		     arity=2},
    {Ae,Ap,St1} = force_atomic(Ke, St0),
    {pre_seq(Ap, #k_test{anno=get_kanno(Ke),
			 op=Test,args=[Ae,#k_atom{val='true'}]}),St1}.

%% expr(Cexpr, Sub, State) -> {Kexpr,[PreKexpr],State}.
%%  Convert a Core expression, flattening it at the same time.

expr(#c_var{anno=A,name=V}, Sub, St) ->
    {#k_var{anno=A,name=get_vsub(V, Sub)},[],St};
expr(#c_char{anno=A,val=C}, _Sub, St) ->
    {#k_int{anno=A,val=C},[],St};		%Convert to integers!
expr(#c_int{anno=A,val=I}, _Sub, St) ->
    {#k_int{anno=A,val=I},[],St};
expr(#c_float{anno=A,val=F}, _Sub, St) ->
    {#k_float{anno=A,val=F},[],St};
expr(#c_atom{anno=A,val=At}, _Sub, St) ->
    {#k_atom{anno=A,val=At},[],St};
expr(#c_string{anno=A,val=S}, _Sub, St) ->
    {#k_string{anno=A,val=S},[],St};
expr(#c_nil{anno=A}, _Sub, St) ->
    {#k_nil{anno=A},[],St};
expr(#c_cons{anno=A,hd=Ch,tl=Ct}, Sub, St0) ->
    %% Do cons in two steps, first the expressions left to right, then
    %% any remaining literals right to left.
    {Kh0,Hp0,St1} = expr(Ch, Sub, St0),
    {Kt0,Tp0,St2} = expr(Ct, Sub, St1),
    {Kt1,Tp1,St3} = force_atomic(Kt0, St2),
    {Kh1,Hp1,St4} = force_atomic(Kh0, St3),
    {#k_cons{anno=A,hd=Kh1,tl=Kt1},Hp0 ++ Tp0 ++ Tp1 ++ Hp1,St4};
expr(#c_tuple{anno=A,es=Ces}, Sub, St0) ->
    {Kes,Ep,St1} = atomic_list(Ces, Sub, St0),
    {#k_tuple{anno=A,es=Kes},Ep,St1};
expr(#c_binary{anno=A,segments=Cv}, Sub, St0) ->
    case catch atomic_bin(Cv, Sub, St0, 0) of
	{'EXIT',R} -> exit(R);
	bad_element_size ->
	    Erl = #c_atom{val=erlang},
	    Name = #c_atom{val=error},
	    Args = [#c_atom{val=badarg}],
	    Fault = #c_call{module=Erl,name=Name,args=Args},
	    expr(Fault, Sub, St0);
	{Kv,Ep,St1} ->
	    {#k_binary{anno=A,segs=Kv},Ep,St1}
    end;
expr(#c_fname{anno=A,arity=Ar}=Fname, Sub, St) ->
    %% A local in an expression.
    %% For now, these are wrapped into a fun by reverse
    %% etha-conversion, but really, there should be exactly one
    %% such "lambda function" for each escaping local name,
    %% instead of one for each occurrence as done now.
    Vs = [#c_var{name=list_to_atom("V" ++ integer_to_list(V))} ||
	     V <- integers(1, Ar)],
    Fun = #c_fun{anno=A,vars=Vs,body=#c_apply{op=Fname,args=Vs}},
    expr(Fun, Sub, St);
expr(#c_fun{anno=A,vars=Cvs,body=Cb}, Sub0, St0) ->
    {Kvs,Sub1,St1} = pattern_list(Cvs, Sub0, St0),
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{{Cvs,Sub0,St0},{Kvs,Sub1,St1}}]),
    {Kb,Pb,St2} = body(Cb, Sub1, St1),
    {#ifun{anno=A,vars=Kvs,body=pre_seq(Pb, Kb)},[],St2};
expr(#c_seq{arg=Ca,body=Cb}, Sub, St0) ->
    {Ka,Pa,St1} = body(Ca, Sub, St0),
    case is_exit_expr(Ka) of
	true -> {Ka,Pa,St1};
	false ->
	    {Kb,Pb,St2} = body(Cb, Sub, St1),
	    {Kb,Pa ++ [Ka] ++ Pb,St2}
    end;
expr(#c_let{anno=A,vars=Cvs,arg=Ca,body=Cb}, Sub0, St0) ->
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{Cvs,Sub0,St0}]),
    {Ka,Pa,St1} = body(Ca, Sub0, St0),
    case is_exit_expr(Ka) of
	true -> {Ka,Pa,St1};
	false ->
	    {Kps,Sub1,St2} = pattern_list(Cvs, Sub0, St1),
	    %%ok = io:fwrite("~w: ~p~n", [?LINE,{Kps,Sub1,St1,St2}]),
	    %% Break known multiple values into separate sets.
	    Sets = case Ka of
		       #ivalues{args=Kas} ->
			   foldr2(fun (V, Val, Sb) ->
					  [#iset{vars=[V],arg=Val}|Sb] end,
				  [], Kps, Kas);
		       _Other ->
			   [#iset{anno=A,vars=Kps,arg=Ka}]
		   end,
	    {Kb,Pb,St3} = body(Cb, Sub1, St2),
	    {Kb,Pa ++ Sets ++ Pb,St3}
    end;
expr(#c_letrec{anno=A,defs=Cfs,body=Cb}, Sub0, St0) ->
    %% Make new function names and store substitution.
    {Fs0,{Sub1,St1}} =
	mapfoldl(fun (#c_def{name=#c_fname{id=F,arity=Ar},val=B}, {Sub,St0}) ->
			 {N,St1} = new_fun_name(atom_to_list(F)
						++ "/" ++
						integer_to_list(Ar),
						St0),
			 {{N,B},{set_fsub(F, Ar, N, Sub),St1}}
		 end, {Sub0,St0}, Cfs),
    %% Run translation on functions and body.
    {Fs1,St2} = mapfoldl(fun ({N,Fd0}, St1) ->
				 {Fd1,[],St2} = expr(Fd0, Sub1, St1),
				 Fd = set_kanno(Fd1, A),
				 {{N,Fd},St2}
			 end, St1, Fs0),
    {Kb,Pb,St3} = body(Cb, Sub1, St2),
    {Kb,[#iletrec{anno=A,defs=Fs1}|Pb],St3};
expr(#c_case{arg=Ca,clauses=Ccs}, Sub, St0) ->
    {Ka,Pa,St1} = body(Ca, Sub, St0),		%This is a body!
    {Kvs,Pv,St2} = match_vars(Ka, St1),		%Must have variables here!
    {Km,St3} = kmatch(Kvs, Ccs, Sub, St2),
    Match = flatten_seq(build_match(Kvs, Km)),
    {last(Match),Pa ++ Pv ++ first(Match),St3};
expr(#c_receive{anno=A,clauses=Ccs0,timeout=Ce,action=Ca}, Sub, St0) ->
    {Ke,Pe,St1} = atomic_lit(Ce, Sub, St0),	%Force this to be atomic!
    {Rvar,St2} = new_var(St1),
    %% Need to massage accept clauses and add reject clause before matching.
    Ccs1 = map(fun (#c_clause{anno=Banno,body=B0}=C) ->
		       B1 = #c_seq{arg=#ireceive_accept{anno=A},body=B0},
		       C#c_clause{anno=Banno,body=B1}
	       end, Ccs0),
    {Mpat,St3} = new_var_name(St2),
    Rc = #c_clause{anno=[compiler_generated|A],
		   pats=[#c_var{name=Mpat}],guard=#c_atom{anno=A,val=true},
		   body=#ireceive_next{anno=A}},
    {Km,St4} = kmatch([Rvar], Ccs1 ++ [Rc], Sub, add_var_def(Rvar, St3)),
    {Ka,Pa,St5} = body(Ca, Sub, St4),
    {#k_receive{anno=A,var=Rvar,body=Km,timeout=Ke,action=pre_seq(Pa, Ka)},
     Pe,St5};
expr(#c_apply{anno=A,op=Cop,args=Cargs}, Sub, St) ->
    c_apply(A, Cop, Cargs, Sub, St);
expr(#c_call{anno=A,module=M0,name=F0,args=Cargs}, Sub, St0) ->
    {[M1,F1|Kargs],Ap,St1} = atomic_list([M0,F0|Cargs], Sub, St0),
    Ar = length(Cargs),
    case {M1,F1} of
	{#k_atom{val=Ma},#k_atom{val=Fa}} ->
	    Call = case is_remote_bif(Ma, Fa, Ar) of
		       true ->
			   #k_bif{anno=A,
				  op=#k_remote{mod=M1,name=F1,arity=Ar},
				  args=Kargs};
		       false ->
			   #k_call{anno=A,
				   op=#k_remote{mod=M1,name=F1,arity=Ar},
				   args=Kargs}
		   end,
	    {Call,Ap,St1};
	_Other when St0#kern.extinstr == false -> %Old explicit apply
	    Call = #c_call{anno=A,
			   module=#c_atom{val=erlang},
			   name=#c_atom{val=apply},
			   args=[M0,F0,make_list(Cargs)]},
	    expr(Call, Sub, St0);
	_Other ->				%New instruction in R10.
	    Call = #k_call{anno=A,
			   op=#k_remote{mod=M1,name=F1,arity=Ar},
			   args=Kargs},
	    {Call,Ap,St1}
    end;
expr(#c_primop{anno=A,name=#c_atom{val=match_fail},args=Cargs}, Sub, St0) ->
    %% This special case will disappear.
    {Kargs,Ap,St1} = atomic_list(Cargs, Sub, St0),
    Ar = length(Cargs),
    Call = #k_call{anno=A,op=#k_internal{name=match_fail,arity=Ar},args=Kargs},
    {Call,Ap,St1};
expr(#c_primop{anno=A,name=#c_atom{val=N},args=Cargs}, Sub, St0) ->
    {Kargs,Ap,St1} = atomic_list(Cargs, Sub, St0),
    Ar = length(Cargs),
    {#k_bif{anno=A,op=#k_internal{name=N,arity=Ar},args=Kargs},Ap,St1};
expr(#c_try{anno=A,arg=Ca,vars=Cvs,body=Cb,evars=Evs,handler=Ch}, Sub0, St0) ->
    %% The normal try expression. The body and exception handler
    %% variables behave as let variables.
    {Ka,Pa,St1} = body(Ca, Sub0, St0),
    {Kcvs,Sub1,St2} = pattern_list(Cvs, Sub0, St1),
    {Kb,Pb,St3} = body(Cb, Sub1, St2),
    {Kevs,Sub2,St4} = pattern_list(Evs, Sub0, St3),
    {Kh,Ph,St5} = body(Ch, Sub2, St4),
    {#k_try{anno=A,arg=pre_seq(Pa, Ka),
	    vars=Kcvs,body=pre_seq(Pb, Kb),
	    evars=Kevs,handler=pre_seq(Ph, Kh)},[],St5};
expr(#c_catch{anno=A,body=Cb}, Sub, St0) ->
    {Kb,Pb,St1} = body(Cb, Sub, St0),
    {#k_catch{anno=A,body=pre_seq(Pb, Kb)},[],St1};
%% Handle internal expressions.
expr(#ireceive_accept{anno=A}, _Sub, St) -> {#k_receive_accept{anno=A},[],St}.

%% expr_list([Cexpr], Sub, State) -> {[Kexpr],[PreKexpr],State}.

% expr_list(Ces, Sub, St) ->
%     foldr(fun (Ce, {Kes,Esp,St0}) ->
% 		  {Ke,Ep,St1} = expr(Ce, Sub, St0),
% 		  {[Ke|Kes],Ep ++ Esp,St1}
% 	  end, {[],[],St}, Ces).

%% match_vars(Kexpr, State) -> {[Kvar],[PreKexpr],State}.
%%  Force return from body into a list of variables.

match_vars(#ivalues{args=As}, St) ->
    foldr(fun (Ka, {Vs,Vsp,St0}) ->
		  {V,Vp,St1} = force_variable(Ka, St0),
		  {[V|Vs],Vp ++ Vsp,St1}
	  end, {[],[],St}, As);
match_vars(Ka, St0) ->
    {V,Vp,St1} = force_variable(Ka, St0),
    {[V],Vp,St1}.

%% c_apply(A, Op, [Carg], Sub, State) -> {Kexpr,[PreKexpr],State}.
%%  Transform application, detect which are guaranteed to be bifs.

c_apply(A, #c_fname{anno=Ra,id=F0,arity=Ar}, Cargs, Sub, St0) ->
    {Kargs,Ap,St1} = atomic_list(Cargs, Sub, St0),
    F1 = get_fsub(F0, Ar, Sub),			%Has it been rewritten
    {#k_call{anno=A,op=#k_local{anno=Ra,name=F1,arity=Ar},args=Kargs},
     Ap,St1};
c_apply(A, Cop, Cargs, Sub, St0) ->
    {Kop,Op,St1} = variable(Cop, Sub, St0),
    {Kargs,Ap,St2} = atomic_list(Cargs, Sub, St1),
    {#k_call{anno=A,op=Kop,args=Kargs},Op ++ Ap,St2}.

flatten_seq(#iset{anno=A,vars=Vs,arg=Arg,body=B}) ->
    [#iset{anno=A,vars=Vs,arg=Arg}|flatten_seq(B)];
flatten_seq(Ke) -> [Ke].

pre_seq([#iset{anno=A,vars=Vs,arg=Arg,body=B}|Ps], K) ->
    B = undefined,				%Assertion.
    #iset{anno=A,vars=Vs,arg=Arg,body=pre_seq(Ps, K)};
pre_seq([P|Ps], K) ->
    #iset{vars=[],arg=P,body=pre_seq(Ps, K)};
pre_seq([], K) -> K.

%% atomic_lit(Cexpr, Sub, State) -> {Katomic,[PreKexpr],State}.
%%  Convert a Core expression making sure the result is an atomic
%%  literal.

atomic_lit(Ce, Sub, St0) ->
    {Ke,Kp,St1} = expr(Ce, Sub, St0),
    {Ka,Ap,St2} = force_atomic(Ke, St1),
    {Ka,Kp ++ Ap,St2}.

force_atomic(Ke, St0) ->
    case is_atomic(Ke) of
	true -> {Ke,[],St0};
	false ->
	    {V,St1} = new_var(St0),
	    {V,[#iset{vars=[V],arg=Ke}],St1}
    end.

% force_atomic_list(Kes, St) ->
%     foldr(fun (Ka, {As,Asp,St0}) ->
% 		  {A,Ap,St1} = force_atomic(Ka, St0),
% 		  {[A|As],Ap ++ Asp,St1}
% 	  end, {[],[],St}, Kes).

atomic_bin([#c_bitstr{anno=A,val=E0,size=S0,unit=U,type=T,flags=Fs}|Es0],
	   Sub, St0, B0) ->
    {E,Ap1,St1} = atomic_lit(E0, Sub, St0),
    {S1,Ap2,St2} = atomic_lit(S0, Sub, St1),
    validate_bin_element_size(S1),
    U0 = core_lib:literal_value(U),
    Fs0 = core_lib:literal_value(Fs),
    {B1,Fs1} = aligned(B0, S1, U0, Fs0),
    {Es,Ap3,St3} = atomic_bin(Es0, Sub, St2, B1),
    {#k_bin_seg{anno=A,size=S1,
		unit=U0,
		type=core_lib:literal_value(T),
		flags=Fs1,
		seg=E,next=Es},
     Ap1++Ap2++Ap3,St3};
atomic_bin([], _Sub, St, _Bits) -> {#k_bin_end{},[],St}.

validate_bin_element_size(#k_var{}) -> ok;
validate_bin_element_size(#k_int{val=V}) when V >= 0 -> ok;
validate_bin_element_size(#k_atom{val=all}) -> ok;
validate_bin_element_size(_) -> throw(bad_element_size).

%% atomic_list([Cexpr], Sub, State) -> {[Kexpr],[PreKexpr],State}.

atomic_list(Ces, Sub, St) ->
    foldr(fun (Ce, {Kes,Esp,St0}) ->
		  {Ke,Ep,St1} = atomic_lit(Ce, Sub, St0),
		  {[Ke|Kes],Ep ++ Esp,St1}
	  end, {[],[],St}, Ces).

%% is_atomic(Kexpr) -> boolean().
%%  Is a Kexpr atomic?  Strings are NOT considered atomic!

is_atomic(#k_int{}) -> true;
is_atomic(#k_float{}) -> true;
is_atomic(#k_atom{}) -> true;
%%is_atomic(#k_char{}) -> true;			%No characters
%%is_atomic(#k_string{}) -> true;
is_atomic(#k_nil{}) -> true;
is_atomic(#k_var{}) -> true;
is_atomic(_) -> false.

%% variable(Cexpr, Sub, State) -> {Kvar,[PreKexpr],State}.
%%  Convert a Core expression making sure the result is a variable.

variable(Ce, Sub, St0) ->
    {Ke,Kp,St1} = expr(Ce, Sub, St0),
    {Kv,Vp,St2} = force_variable(Ke, St1),
    {Kv,Kp ++ Vp,St2}.

force_variable(#k_var{}=Ke, St) -> {Ke,[],St};
force_variable(Ke, St0) ->
    {V,St1} = new_var(St0),
    {V,[#iset{vars=[V],arg=Ke}],St1}.

%% pattern(Cpat, Sub, State) -> {Kpat,Sub,State}.
%%  Convert patterns.  Variables shadow so rename variables that are
%%  already defined.

pattern(#c_var{anno=A,name=V}, Sub, St0) ->
    case sets:is_element(V, St0#kern.ds) of
	true ->
	    {New,St1} = new_var_name(St0),
	    {#k_var{anno=A,name=New},
	     set_vsub(V, New, Sub),
	     St1#kern{ds=sets:add_element(New, St1#kern.ds)}};
	false ->
	    {#k_var{anno=A,name=V},Sub,
	     St0#kern{ds=sets:add_element(V, St0#kern.ds)}}
    end;
pattern(#c_char{anno=A,val=C}, Sub, St) ->
    {#k_int{anno=A,val=C},Sub,St};		%Convert to integers!
pattern(#c_int{anno=A,val=I}, Sub, St) ->
    {#k_int{anno=A,val=I},Sub,St};
pattern(#c_float{anno=A,val=F}, Sub, St) ->
    {#k_float{anno=A,val=F},Sub,St};
pattern(#c_atom{anno=A,val=At}, Sub, St) ->
    {#k_atom{anno=A,val=At},Sub,St};
pattern(#c_string{val=S}, Sub, St) ->
    L = foldr(fun (C, T) -> #k_cons{hd=#k_int{val=C},tl=T} end,
	      #k_nil{}, S),
    {L,Sub,St};
pattern(#c_nil{anno=A}, Sub, St) ->
    {#k_nil{anno=A},Sub,St};
pattern(#c_cons{anno=A,hd=Ch,tl=Ct}, Sub0, St0) ->
    {Kh,Sub1,St1} = pattern(Ch, Sub0, St0),
    {Kt,Sub2,St2} = pattern(Ct, Sub1, St1),
    {#k_cons{anno=A,hd=Kh,tl=Kt},Sub2,St2};
pattern(#c_tuple{anno=A,es=Ces}, Sub0, St0) ->
    {Kes,Sub1,St1} = pattern_list(Ces, Sub0, St0),
    {#k_tuple{anno=A,es=Kes},Sub1,St1};
pattern(#c_binary{anno=A,segments=Cv}, Sub0, St0) ->
    {Kv,Sub1,St1} = pattern_bin(Cv, Sub0, St0),
    {#k_binary{anno=A,segs=Kv},Sub1,St1};
pattern(#c_alias{anno=A,var=Cv,pat=Cp}, Sub0, St0) ->
    {Cvs,Cpat} = flatten_alias(Cp),
    {Kvs,Sub1,St1} = pattern_list([Cv|Cvs], Sub0, St0),
    {Kpat,Sub2,St2} = pattern(Cpat, Sub1, St1),
    {#ialias{anno=A,vars=Kvs,pat=Kpat},Sub2,St2}.

flatten_alias(#c_alias{var=V,pat=P}) ->
    {Vs,Pat} = flatten_alias(P),
    {[V|Vs],Pat};
flatten_alias(Pat) -> {[],Pat}.

pattern_bin(Es, Sub, St) -> pattern_bin(Es, Sub, St, 0).

pattern_bin([#c_bitstr{anno=A,val=E0,size=S0,unit=U,type=T,flags=Fs}|Es0],
	    Sub0, St0, B0) ->
    {S1,[],St1} = expr(S0, Sub0, St0),
    U0 = core_lib:literal_value(U),
    Fs0 = core_lib:literal_value(Fs),
    %%ok= io:fwrite("~w: ~p~n", [?LINE,{B0,S1,U0,Fs0}]),
    {B1,Fs1} = aligned(B0, S1, U0, Fs0),
    {E,Sub1,St2} = pattern(E0, Sub0, St1),
    {Es,Sub2,St3} = pattern_bin(Es0, Sub1, St2, B1),
    {#k_bin_seg{anno=A,size=S1,
		unit=U0,
		type=core_lib:literal_value(T),
		flags=Fs1,
		seg=E,next=Es},
     Sub2,St3};
pattern_bin([], Sub, St, _Bits) -> {#k_bin_end{},Sub,St}.

%% pattern_list([Cexpr], Sub, State) -> {[Kexpr],Sub,State}.

pattern_list(Ces, Sub, St) ->
    foldr(fun (Ce, {Kes,Sub0,St0}) ->
		  {Ke,Sub1,St1} = pattern(Ce, Sub0, St0),
		  {[Ke|Kes],Sub1,St1}
	  end, {[],Sub,St}, Ces).

%% new_sub() -> Subs.
%% set_vsub(Name, Sub, Subs) -> Subs.
%% subst_vsub(Name, Sub, Subs) -> Subs.
%% get_vsub(Name, Subs) -> SubName.
%%  Add/get substitute Sub for Name to VarSub.  Use orddict so we know
%%  the format is a list {Name,Sub} pairs.  When adding a new
%%  substitute we fold substitute chains so we never have to search
%%  more than once.

new_sub() -> orddict:new().

get_vsub(V, Vsub) ->
    case orddict:find(V, Vsub) of
	{ok,Val} -> Val;
	error -> V
    end.

set_vsub(V, S, Vsub) ->
    orddict:store(V, S, Vsub).

subst_vsub(V, S, Vsub0) ->
    %% Fold chained substitutions.
    Vsub1 = orddict:map(fun (_, V1) when V1 =:= V -> S;
			    (_, V1) -> V1
			end, Vsub0),
    orddict:store(V, S, Vsub1).

get_fsub(F, A, Fsub) ->
    case orddict:find({F,A}, Fsub) of
	{ok,Val} -> Val;
	error -> F
    end.

set_fsub(F, A, S, Fsub) ->
    orddict:store({F,A}, S, Fsub).

new_fun_name(St) ->
    new_fun_name("anonymous", St).

%% new_fun_name(Type, State) -> {FunName,State}.

new_fun_name(Type, #kern{func={F,Arity},fcount=C}=St) ->
    Name = "-" ++ atom_to_list(F) ++ "/" ++ integer_to_list(Arity) ++
	"-" ++ Type ++ "-" ++ integer_to_list(C) ++ "-",
    {list_to_atom(Name),St#kern{fcount=C+1}}.

%% new_var_name(State) -> {VarName,State}.

new_var_name(#kern{vcount=C}=St) ->
    {list_to_atom("ker" ++ integer_to_list(C)),St#kern{vcount=C+1}}.

%% new_var(State) -> {#k_var{},State}.

new_var(St0) ->
    {New,St1} = new_var_name(St0),
    {#k_var{name=New},St1}.

%% new_vars(Count, State) -> {[#k_var{}],State}.
%%  Make Count new variables.

new_vars(N, St) -> new_vars(N, St, []).

new_vars(N, St0, Vs) when N > 0 ->
    {V,St1} = new_var(St0),
    new_vars(N-1, St1, [V|Vs]);
new_vars(0, St, Vs) -> {Vs,St}.

make_vars(Vs) -> [ #k_var{name=V} || V <- Vs ].

add_var_def(V, St) ->
    St#kern{ds=sets:add_element(V#k_var.name, St#kern.ds)}.

%%add_vars_def(Vs, St) ->
%%    Ds = foldl(fun (#k_var{name=V}, Ds) -> add_element(V, Ds) end,
%%	       St#kern.ds, Vs),
%%    St#kern{ds=Ds}.

%% is_remote_bif(Mod, Name, Arity) -> true | false.
%%  Test if function is really a BIF.

is_remote_bif(erlang, is_boolean, 1) ->
    %% XXX Remove this clause in R11. For bootstrap purposes, we must
    %% recognize erlang:is_boolean/1 here.
    true;
is_remote_bif(erlang, internal_is_record, 3) -> true;
is_remote_bif(erlang, get, 1) -> true;
is_remote_bif(erlang, N, A) ->
    case erl_internal:guard_bif(N, A) of
	true -> true;
	false ->
	    case erl_internal:type_test(N, A) of
		true -> true;
		false ->
		    case catch erl_internal:op_type(N, A) of
			arith -> true;
			bool -> true;
			comp -> true;
			_Other -> false		%List, send or not an op
		    end
	    end
    end;
is_remote_bif(_, _, _) -> false.

%% bif_vals(Name, Arity) -> integer().
%% bif_vals(Mod, Name, Arity) -> integer().
%%  Determine how many return values a BIF has.  Provision for BIFs to
%%  return multiple values.  Only used in bodies where a BIF may be
%%  called for effect only.

bif_vals(dsetelement, 3) -> 0;
bif_vals(_, _) -> 1.

bif_vals(_, _, _) -> 1.

%% foldr2(Fun, Acc, List1, List2) -> Acc.
%%  Fold over two lists.

foldr2(Fun, Acc0, [E1|L1], [E2|L2]) ->
    Acc1 = Fun(E1, E2, Acc0),
    foldr2(Fun, Acc1, L1, L2);
foldr2(_, Acc, [], []) -> Acc.

%% first([A]) -> [A].
%% last([A]) -> A.

last([L]) -> L;
last([_|T]) -> last(T).

first([_]) -> [];
first([H|T]) -> [H|first(T)].

%% This code implements the algorithm for an optimizing compiler for
%% pattern matching given "The Implementation of Functional
%% Programming Languages" by Simon Peyton Jones. The code is much
%% longer as the meaning of constructors is different from the book.
%%
%% In Erlang many constructors can have different values, e.g. 'atom'
%% or 'integer', whereas in the original algorithm thse would be
%% different constructors. Our view makes it easier in later passes to
%% handle indexing over each type.
%%
%% Patterns are complicated by having alias variables.  The form of a
%% pattern is Pat | {alias,Pat,[AliasVar]}.  This is hidden by access
%% functions to pattern arguments but the code must be aware of it.
%%
%% The compilation proceeds in two steps:
%%
%% 1. The patterns in the clauses to converted to lists of kernel
%% patterns.  The Core clause is now hybrid, this is easier to work
%% with.  Remove clauses with trivially false guards, this simplifies
%% later passes.  Add local defined vars and variable subs to each
%% clause for later use.
%%
%% 2. The pattern matching is optimised.  Variable substitutions are
%% added to the VarSub structure and new variables are made visible.
%% The guard and body are then converted to Kernel form.

%% kmatch([Var], [Clause], Sub, State) -> {Kexpr,[PreExpr],State}.

kmatch(Us, Ccs, Sub, St0) ->
    {Cs,St1} = match_pre(Ccs, Sub, St0),	%Convert clauses
    %%Def = kernel_match_error,              %The strict case
    %% This should be a kernel expression from the first pass.
    Def = #k_call{anno=[compiler_generated],
		  op=#k_remote{mod=#k_atom{val=erlang},
			       name=#k_atom{val=exit},
			       arity=1},
		  args=[#k_atom{val=kernel_match_error}]},
    {Km,St2} = match(Us, Cs, Def, St1),               %Do the match.
    {Km,St2}.

%% match_pre([Cclause], Sub, State) -> {[Clause],State}.
%%  Must be careful not to generate new substitutions here now!
%%  Remove clauses with trivially false guards which will never
%%  succeed.

match_pre(Cs, Sub0, St) ->
    foldr(fun (#c_clause{anno=A,pats=Ps,guard=G,body=B}, {Cs0,St0}) ->
		  case is_false_guard(G) of
		      true -> {Cs0,St0};
		      false ->
			  {Kps,Sub1,St1} = pattern_list(Ps, Sub0, St0),
			  {[#iclause{anno=A,sub=Sub1,pats=Kps,guard=G,body=B}|
			    Cs0],St1}
		  end
	  end, {[],St}, Cs).

%% match([Var], [Clause], Default, State) -> {MatchExpr,State}.

match([U|Us], Cs, Def, St0) ->
    %%ok = io:format("match ~p~n", [Cs]),
    Pcss = partition(Cs),
    foldr(fun (Pcs, {D,St}) -> match_varcon([U|Us], Pcs, D, St) end,
	  {Def,St0}, Pcss);
match([], Cs, Def, St) ->
    match_guard(Cs, Def, St).

%% match_guard([Clause], Default, State) -> {IfExpr,State}.
%%  Build a guard to handle guards. A guard *ALWAYS* fails if no
%%  clause matches, there will be a surrounding 'alt' to catch the
%%  failure.  Drop redundant cases, i.e. those after a true guard.

match_guard(Cs0, Def0, St0) ->
    {Cs1,Def1,St1} = match_guard_1(Cs0, Def0, St0),
    {build_alt(build_guard(Cs1), Def1),St1}.

match_guard_1([#iclause{anno=A,sub=Sub,guard=G,body=B}|Cs0], Def0, St0) ->
    case is_true_guard(G) of
	true ->
	    %% The true clause body becomes the default.
	    {Kb,Pb,St1} = body(B, Sub, St0),
	    Line = get_line(A),
	    St2 = maybe_add_warning(Cs0, Line, St1),
	    St = maybe_add_warning(Def0, Line, St2),
	    {[],pre_seq(Pb, Kb),St};
	false ->
	    {Kg,St1} = guard(G, Sub, St0),
	    {Kb,Pb,St2} = body(B, Sub, St1),
	    {Cs1,Def1,St3} = match_guard_1(Cs0, Def0, St2),
	    {[#k_guard_clause{guard=Kg,body=pre_seq(Pb, Kb)}|Cs1],
	     Def1,St3}
    end;
match_guard_1([], Def, St) -> {[],Def,St}.

maybe_add_warning([C|_], Line, St) ->
    maybe_add_warning(C, Line, St);
maybe_add_warning([], _Line, St) -> St;
maybe_add_warning(fail, _Line, St) -> St;
maybe_add_warning(Ke, MatchLine, St) ->
    case get_kanno(Ke) of
	[compiler_generated|_] -> St;
	Anno ->
	    Line = get_line(Anno),
	    Warn = case MatchLine of
		       none -> nomatch_shadow;
		       _ -> {nomatch_shadow,MatchLine}
		   end,
	    add_warning(Line, Warn, St)
    end.

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.


%% is_true_guard(Guard) -> boolean().
%% is_false_guard(Guard) -> boolean().
%%  Test if a guard is either trivially true/false.  This has probably
%%  already been optimised away, but what the heck!

is_true_guard(G) -> guard_value(G) == true.
is_false_guard(G) -> guard_value(G) == false.

%% guard_value(Guard) -> true | false | unknown.

guard_value(#c_atom{val=true}) -> true;
guard_value(#c_atom{val=false}) -> false;
guard_value(#c_call{module=#c_atom{val=erlang},
		    name=#c_atom{val='not'},
		    args=[A]}) ->
    case guard_value(A) of
	true -> false;
	false -> true;
	unknown -> unknown
    end;
guard_value(#c_call{module=#c_atom{val=erlang},
		    name=#c_atom{val='and'},
		    args=[Ca,Cb]}) ->
    case guard_value(Ca) of
	true -> guard_value(Cb);
	false -> false;
	unknown ->
	    case guard_value(Cb) of
		false -> false;
		_Other -> unknown
	    end
    end;
guard_value(#c_call{module=#c_atom{val=erlang},
		    name=#c_atom{val='or'},
		    args=[Ca,Cb]}) ->
    case guard_value(Ca) of
	true -> true;
	false -> guard_value(Cb);
	unknown ->
	    case guard_value(Cb) of
		true -> true;
		_Other -> unknown
	    end
    end;
guard_value(#c_try{arg=E,vars=[#c_var{name=X}],body=#c_var{name=X},
		   handler=#c_atom{val=false}}) ->
    guard_value(E);
guard_value(_) -> unknown.

%% partition([Clause]) -> [[Clause]].
%%  Partition a list of clauses into groups which either contain
%%  clauses with a variable first argument, or with a "constructor".

partition([C1|Cs]) ->
    V1 = is_var_clause(C1),
    {More,Rest} = splitwith(fun (C) -> is_var_clause(C) == V1 end, Cs),
    [[C1|More]|partition(Rest)];
partition([]) -> [].

%% match_varcon([Var], [Clause], Def, [Var], Sub, State) ->
%%        {MatchExpr,State}.

match_varcon(Us, [C|_]=Cs, Def, St) ->
    case is_var_clause(C) of
	true -> match_var(Us, Cs, Def, St);
	false -> match_con(Us, Cs, Def, St)
    end.

%% match_var([Var], [Clause], Def, State) -> {MatchExpr,State}.
%%  Build a call to "select" from a list of clauses all containing a
%%  variable as the first argument.  We must rename the variable in
%%  each clause to be the match variable as these clause will share
%%  this variable and may have different names for it.  Rename aliases
%%  as well.

match_var([U|Us], Cs0, Def, St) ->
    Cs1 = map(fun (#iclause{sub=Sub0,pats=[Arg|As]}=C) ->
		      Vs = [arg_arg(Arg)|arg_alias(Arg)],
		      Sub1 = foldl(fun (#k_var{name=V}, Acc) ->
					   subst_vsub(V, U#k_var.name, Acc)
				   end, Sub0, Vs),
		      C#iclause{sub=Sub1,pats=As}
	      end, Cs0),
    match(Us, Cs1, Def, St).

%% match_con(Variables, [Clause], Default, State) -> {SelectExpr,State}.
%%  Build call to "select" from a list of clauses all containing a
%%  constructor/constant as first argument.  Group the constructors
%%  according to type, the order is really irrelevant but tries to be
%%  smart.

match_con([U|Us], Cs, Def, St0) ->
    %% Extract clauses for different constructors (types).
    %%ok = io:format("match_con ~p~n", [Cs]),
    Ttcs = [ {T,Tcs} || T <- [k_cons,k_tuple,k_atom,k_float,k_int,k_nil,
			      k_binary,k_bin_end],
		       begin Tcs = select(T, Cs),
			     Tcs /= []
		       end ] ++ select_bin_con(Cs),
    %%ok = io:format("ttcs = ~p~n", [Ttcs]),
    {Scs,St1} =
	mapfoldl(fun ({T,Tcs}, St) ->
			 {[S|_]=Sc,S1} = match_value([U|Us], T, Tcs, fail, St),
			 %%ok = io:format("match_con type2 ~p~n", [T]),
			 Anno = get_kanno(S),
			 {#k_type_clause{anno=Anno,type=T,values=Sc},S1} end,
		 St0, Ttcs),
    {build_alt_1st_no_fail(build_select(U, Scs), Def),St1}.

%% select_bin_con([Clause]) -> [{Type,[Clause]}].
%%  Extract clauses for the k_bin_seg constructor.  As k_bin_seg
%%  matching can overlap, the k_bin_seg constructors cannot be
%%  reordered, only grouped.

select_bin_con(Cs0) ->
    Cs1 = lists:filter(fun (C) ->
			       clause_con(C) == k_bin_seg
		       end, Cs0),
    select_bin_con_1(Cs1).

select_bin_con_1([C1|Cs]) ->
    Con = clause_con(C1),
    {More,Rest} = splitwith(fun (C) -> clause_con(C) == Con end, Cs),
    [{Con,[C1|More]}|select_bin_con_1(Rest)];
select_bin_con_1([]) -> [].

%% select(Con, [Clause]) -> [Clause].

select(T, Cs) -> [ C || C <- Cs, clause_con(C) == T ].

%% match_value([Var], Con, [Clause], Default, State) -> {SelectExpr,State}.
%%  At this point all the clauses have the same constructor, we must
%%  now separate them according to value.

match_value(_, _, [], _, St) -> {[],St};
match_value(Us, T, Cs0, Def, St0) ->
    Css = group_value(T, Cs0),
    %%ok = io:format("match_value ~p ~p~n", [T, Css]),
    {Css1,St1} = mapfoldl(fun (Cs, St) ->
				  match_clause(Us, Cs, Def, St) end,
			  St0, Css),
    {Css1,St1}.
    %%{#k_select_val{type=T,var=hd(Us),clauses=Css1},St1}.

%% group_value([Clause]) -> [[Clause]].
%%  Group clauses according to value.  Here we know that
%%  1. Some types are singled valued
%%  2. The clauses in bin_segs cannot be reordered only grouped
%%  3. Other types are disjoint and can be reordered

group_value(k_cons, Cs) -> [Cs];		%These are single valued
group_value(k_nil, Cs) -> [Cs];
group_value(k_binary, Cs) -> [Cs];
group_value(k_bin_end, Cs) -> [Cs];
group_value(k_bin_seg, Cs) ->
    group_bin_seg(Cs);
group_value(_, Cs) ->
    %% group_value(Cs).
    Cd = foldl(fun (C, Gcs0) -> dict:append(clause_val(C), C, Gcs0) end,
	       dict:new(), Cs),
    dict:fold(fun (_, Vcs, Css) -> [Vcs|Css] end, [], Cd).

group_bin_seg([C1|Cs]) ->
    V1 = clause_val(C1),
    {More,Rest} = splitwith(fun (C) -> clause_val(C) == V1 end, Cs),
    [[C1|More]|group_bin_seg(Rest)];
group_bin_seg([]) -> [].

%% Profiling shows that this quadratic implementation account for a big amount
%% of the execution time if there are many values.
% group_value([C|Cs]) ->
%     V = clause_val(C),
%     Same = [ Cv || Cv <- Cs, clause_val(Cv) == V ], %Same value
%     Rest = [ Cv || Cv <- Cs, clause_val(Cv) /= V ], % and all the rest
%     [[C|Same]|group_value(Rest)];
% group_value([]) -> [].

%% match_clause([Var], [Clause], Default, State) -> {Clause,State}.
%%  At this point all the clauses have the same "value".  Build one
%%  select clause for this value and continue matching.  Rename
%%  aliases as well.

match_clause([U|Us], [C|_]=Cs0, Def, St0) ->
    Anno = get_kanno(C),
    {Match0,Vs,St1} = get_match(get_con(Cs0), St0),
    Match = sub_size_var(Match0, Cs0),
    {Cs1,St2} = new_clauses(Cs0, U, St1),
    {B,St3} = match(Vs ++ Us, Cs1, Def, St2),
    {#k_val_clause{anno=Anno,val=Match,body=B},St3}.

sub_size_var(#k_bin_seg{size=#k_var{name=Name}=Kvar}=BinSeg, [#iclause{sub=Sub}|_]) ->
    BinSeg#k_bin_seg{size=Kvar#k_var{name=get_vsub(Name, Sub)}};
sub_size_var(K, _) -> K.

get_con([C|_]) -> arg_arg(clause_arg(C)).	%Get the constructor

get_match(#k_cons{}, St0) ->
    {[H,T],St1} = new_vars(2, St0),
    {#k_cons{hd=H,tl=T},[H,T],St1};
get_match(#k_binary{}, St0) ->
    {[V]=Mes,St1} = new_vars(1, St0),
    {#k_binary{segs=V},Mes,St1};
get_match(#k_bin_seg{}=Seg, St0) ->
    {[S,N]=Mes,St1} = new_vars(2, St0),
    {Seg#k_bin_seg{seg=S,next=N},Mes,St1};
get_match(#k_tuple{es=Es}, St0) ->
    {Mes,St1} = new_vars(length(Es), St0),
    {#k_tuple{es=Mes},Mes,St1};
get_match(M, St) ->
    {M,[],St}.

new_clauses(Cs0, U, St) ->
    Cs1 = map(fun (#iclause{sub=Sub0,pats=[Arg|As]}=C) ->
		      Head = case arg_arg(Arg) of
				 #k_cons{hd=H,tl=T} -> [H,T|As];
				 #k_tuple{es=Es} -> Es ++ As;
				 #k_binary{segs=E}  -> [E|As];
				 #k_bin_seg{seg=S,next=N} ->
				     [S,N|As];
				 _Other -> As
			     end,
		      Vs = arg_alias(Arg),
		      Sub1 = foldl(fun (#k_var{name=V}, Acc) ->
					   subst_vsub(V, U#k_var.name, Acc)
				   end, Sub0, Vs),
		      C#iclause{sub=Sub1,pats=Head}
	      end, Cs0),
    {Cs1,St}.

%% build_guard([GuardClause]) -> GuardExpr.

build_guard([]) -> fail;
build_guard(Cs) -> #k_guard{clauses=Cs}.

%% build_select(Var, [ConClause]) -> SelectExpr.

build_select(V, [Tc|_]=Tcs) ->
    Anno = get_kanno(Tc),
    #k_select{anno=Anno,var=V,types=Tcs}.

%% build_alt(First, Then) -> AltExpr.
%%  Build an alt, attempt some simple optimisation.

build_alt(fail, Then) -> Then;
build_alt(First,Then) -> build_alt_1st_no_fail(First, Then).

build_alt_1st_no_fail(First, fail) -> First;
build_alt_1st_no_fail(First, Then) -> #k_alt{first=First,then=Then}.

%% build_match([MatchVar], MatchExpr) -> Kexpr.
%%  Build a match expr if there is a match.

build_match(Us, #k_alt{}=Km) -> #k_match{vars=Us,body=Km};
build_match(Us, #k_select{}=Km) -> #k_match{vars=Us,body=Km};
build_match(Us, #k_guard{}=Km) -> #k_match{vars=Us,body=Km};
build_match(_, Km) -> Km.

%% clause_arg(Clause) -> FirstArg.
%% clause_con(Clause) -> Constructor.
%% clause_val(Clause) -> Value.
%% is_var_clause(Clause) -> boolean().

clause_arg(#iclause{pats=[Arg|_]}) -> Arg.

clause_con(C) -> arg_con(clause_arg(C)).

clause_val(C) -> arg_val(clause_arg(C)).

is_var_clause(C) -> clause_con(C) == k_var.

%% arg_arg(Arg) -> Arg.
%% arg_alias(Arg) -> Aliases.
%% arg_con(Arg) -> Constructor.
%% arg_val(Arg) -> Value.
%%  These are the basic functions for obtaining fields in an argument.

arg_arg(#ialias{pat=Con}) -> Con;
arg_arg(Con) -> Con.

arg_alias(#ialias{vars=As}) -> As;
arg_alias(_Con) -> [].

arg_con(Arg) ->
    case arg_arg(Arg) of
	#k_int{} -> k_int;
	#k_float{} -> k_float;
	#k_atom{} -> k_atom;
	#k_nil{} -> k_nil;
	#k_cons{} -> k_cons;
	#k_tuple{} -> k_tuple;
	#k_binary{} -> k_binary;
	#k_bin_end{} -> k_bin_end;
	#k_bin_seg{} -> k_bin_seg;
	#k_var{} -> k_var
    end.

arg_val(Arg) ->
    case arg_arg(Arg) of
	#k_int{val=I} -> I;
	#k_float{val=F} -> F;
	#k_atom{val=A} -> A;
	#k_nil{} -> 0;
	#k_cons{} -> 2;
	#k_tuple{es=Es} -> length(Es);
	#k_bin_seg{size=S,unit=U,type=T,flags=Fs} ->
	    {set_kanno(S, []),U,T,Fs};
	#k_bin_end{} -> 0;
	#k_binary{} -> 0
    end.

%% ubody(Expr, Break, State) -> {Expr,[UsedVar],State}.
%%  Tag the body sequence with its used variables.  These bodies
%%  either end with a #k_break{}, or with #k_return{} or an expression
%%  which itself can return, #k_enter{}, #k_match{} ... .

ubody(#iset{vars=[],arg=#iletrec{}=Let,body=B0}, Br, St0) ->
    %% An iletrec{} should never be last.
    St1 = iletrec_funs(Let, St0),
    ubody(B0, Br, St1);
ubody(#iset{anno=A,vars=Vs,arg=E0,body=B0}, Br, St0) ->
    {E1,Eu,St1} = uexpr(E0, {break,Vs}, St0),
    {B1,Bu,St2} = ubody(B0, Br, St1),
    Ns = lit_list_vars(Vs),
    Used = union(Eu, subtract(Bu, Ns)),		%Used external vars
    {#k_seq{anno=#k{us=Used,ns=Ns,a=A},arg=E1,body=B1},Used,St2};
ubody(#ivalues{anno=A,args=As}, return, St) ->
    Au = lit_list_vars(As),
    {#k_return{anno=#k{us=Au,ns=[],a=A},args=As},Au,St};
ubody(#ivalues{anno=A,args=As}, {break,_Vbs}, St) ->
    Au = lit_list_vars(As),
    {#k_break{anno=#k{us=Au,ns=[],a=A},args=As},Au,St};
ubody(E, return, St0) ->
    %% Enterable expressions need no trailing return.
    case is_enter_expr(E) of
	true -> uexpr(E, return, St0);
	false ->
	    {Ea,Pa,St1} = force_atomic(E, St0),
	    ubody(pre_seq(Pa, #ivalues{args=[Ea]}), return, St1)
    end;
ubody(E, {break,Rs}, St0) ->
    %%ok = io:fwrite("ubody ~w:~p~n", [?LINE,{E,Br}]),
    %% Exiting expressions need no trailing break.
    case is_exit_expr(E) of
	true -> uexpr(E, return, St0);
	false ->
	    {Ea,Pa,St1} = force_atomic(E, St0),
	    ubody(pre_seq(Pa, #ivalues{args=[Ea]}), {break,Rs}, St1)
    end.

iletrec_funs(#iletrec{defs=Fs}, St0) ->
    %% Use union of all free variables.
    %% First just work out free variables for all functions.
    Free = foldl(fun ({_,#ifun{vars=Vs,body=Fb0}}, Free0) ->
			 {_,Fbu,_} = ubody(Fb0, return, St0),
			 Ns = lit_list_vars(Vs),
			 Free1 = subtract(Fbu, Ns),
			 union(Free1, Free0)
		 end, [], Fs),
    FreeVs = make_vars(Free),
    %% Add this free info to State.
    St1 = foldl(fun ({N,#ifun{vars=Vs}}, Lst) ->
			store_free(N, length(Vs), FreeVs, Lst)
		end, St0, Fs),
    %% Now regenerate local functions to use free variable information.
    St2 = foldl(fun ({N,#ifun{anno=Fa,vars=Vs,body=Fb0}}, Lst0) ->
			{Fb1,_,Lst1} = ubody(Fb0, return, Lst0),
			Arity = length(Vs) + length(FreeVs),
			Fun = #k_fdef{anno=#k{us=[],ns=[],a=Fa},
				      func=N,arity=Arity,
				      vars=Vs ++ FreeVs,body=Fb1},
			Lst1#kern{funs=[Fun|Lst1#kern.funs]}
		end, St1, Fs),
    St2.

%% is_exit_expr(Kexpr) -> boolean().
%%  Test whether Kexpr always exits and never returns.

is_exit_expr(#k_call{op=#k_remote{mod=erlang,name=throw,arity=1}}) -> true;
is_exit_expr(#k_call{op=#k_remote{mod=erlang,name=exit,arity=1}}) -> true;
is_exit_expr(#k_call{op=#k_remote{mod=erlang,name=error,arity=1}}) -> true;
is_exit_expr(#k_call{op=#k_remote{mod=erlang,name=error,arity=2}}) -> true;
is_exit_expr(#k_call{op=#k_remote{mod=erlang,name=fault,arity=1}}) -> true;
is_exit_expr(#k_call{op=#k_remote{mod=erlang,name=fault,arity=2}}) -> true;
is_exit_expr(#k_call{op=#k_internal{name=match_fail,arity=1}}) -> true;
is_exit_expr(#k_bif{op=#k_internal{name=rethrow,arity=2}}) -> true;
is_exit_expr(#k_receive_next{}) -> true;
is_exit_expr(_) -> false.

%% is_enter_expr(Kexpr) -> boolean().
%%  Test whether Kexpr is "enterable", i.e. can handle return from
%%  within itself without extra #k_return{}.

is_enter_expr(#k_call{}) -> true;
is_enter_expr(#k_match{}) -> true;
is_enter_expr(#k_receive{}) -> true;
is_enter_expr(#k_receive_next{}) -> true;
%%is_enter_expr(#k_try{}) -> true;		%Soon
is_enter_expr(_) -> false.

%% uguard(Expr, State) -> {Expr,[UsedVar],State}.
%%  Tag the guard sequence with its used variables.

uguard(#k_try{anno=A,arg=B0,vars=[#k_var{name=X}],body=#k_var{name=X},
	      handler=#k_atom{val=false}}=Try, St0) ->
    {B1,Bu,St1} = uguard(B0, St0),
    {Try#k_try{anno=#k{us=Bu,ns=[],a=A},arg=B1},Bu,St1};
uguard(T, St) ->
    %%ok = io:fwrite("~w: ~p~n", [?LINE,T]),
    uguard_test(T, St).

%% uguard_test(Expr, State) -> {Test,[UsedVar],State}.
%%  At this stage tests are just expressions which don't return any
%%  values.

uguard_test(T, St) -> uguard_expr(T, [], St).

uguard_expr(#iset{anno=A,vars=Vs,arg=E0,body=B0}, Rs, St0) ->
    Ns = lit_list_vars(Vs),
    {E1,Eu,St1} = uguard_expr(E0, Vs, St0),
    {B1,Bu,St2} = uguard_expr(B0, Rs, St1),
    Used = union(Eu, subtract(Bu, Ns)),
    {#k_seq{anno=#k{us=Used,ns=Ns,a=A},arg=E1,body=B1},Used,St2};
uguard_expr(#k_try{anno=A,arg=B0,vars=[#k_var{name=X}],body=#k_var{name=X},
		   handler=#k_atom{val=false}}=Try, Rs, St0) ->
    {B1,Bu,St1} = uguard_expr(B0, Rs, St0),
    {Try#k_try{anno=#k{us=Bu,ns=lit_list_vars(Rs),a=A},arg=B1,ret=Rs},
     Bu,St1};
uguard_expr(#k_test{anno=A,op=Op,args=As}=Test, Rs, St) ->
    [] = Rs,					%Sanity check
    Used = union(op_vars(Op), lit_list_vars(As)),
    {Test#k_test{anno=#k{us=Used,ns=lit_list_vars(Rs),a=A}},
     Used,St};
uguard_expr(#k_bif{anno=A,op=Op,args=As}=Bif, Rs, St) ->
    Used = union(op_vars(Op), lit_list_vars(As)),
    {Bif#k_bif{anno=#k{us=Used,ns=lit_list_vars(Rs),a=A},ret=Rs},
     Used,St};
uguard_expr(#ivalues{anno=A,args=As}, Rs, St) ->
    Sets = foldr2(fun (V, Arg, Rhs) ->
			  #iset{anno=A,vars=[V],arg=Arg,body=Rhs}
		  end, #k_atom{val=true}, Rs, As),
    uguard_expr(Sets, [], St);
uguard_expr(#k_match{anno=A,vars=Vs,body=B0}, Rs, St0) ->
    %% Experimental support for andalso/orelse in guards.
    Br = case Rs of
	     [] -> return;
	     _ -> {break,Rs}
	 end,
    {B1,Bu,St1} = umatch(B0, Br, St0),
    {#k_match{anno=#k{us=Bu,ns=lit_list_vars(Rs),a=A},
	      vars=Vs,body=B1,ret=Rs},Bu,St1};
uguard_expr(Lit, Rs, St) ->
    %% Transform literals to puts here.
    Used = lit_vars(Lit),
    {#k_put{anno=#k{us=Used,ns=lit_list_vars(Rs),a=get_kanno(Lit)},
	    arg=Lit,ret=Rs},Used,St}.

%% uexpr(Expr, Break, State) -> {Expr,[UsedVar],State}.
%%  Tag an expression with its used variables.
%%  Break = return | {break,[RetVar]}.

uexpr(#k_call{anno=A,op=#k_local{name=F,arity=Ar}=Op,args=As0}=Call, Br, St) ->
    Free = get_free(F, Ar, St),
    As1 = As0 ++ Free,				%Add free variables LAST!
    Used = lit_list_vars(As1),
    {case Br of
	 {break,Rs} ->
	     Call#k_call{anno=#k{us=Used,ns=lit_list_vars(Rs),a=A},
			 op=Op#k_local{arity=Ar + length(Free)},
			 args=As1,ret=Rs};
	 return ->
	     #k_enter{anno=#k{us=Used,ns=[],a=A},
		      op=Op#k_local{arity=Ar + length(Free)},
		      args=As1}
     end,Used,St};
uexpr(#k_call{anno=A,op=Op,args=As}=Call, {break,Rs}, St) ->
    Used = union(op_vars(Op), lit_list_vars(As)),
    {Call#k_call{anno=#k{us=Used,ns=lit_list_vars(Rs),a=A},ret=Rs},
     Used,St};
uexpr(#k_call{anno=A,op=Op,args=As}, return, St) ->
    Used = union(op_vars(Op), lit_list_vars(As)),
    {#k_enter{anno=#k{us=Used,ns=[],a=A},op=Op,args=As},
     Used,St};
uexpr(#k_bif{anno=A,op=Op,args=As}=Bif, {break,Rs}, St0) ->
    Used = union(op_vars(Op), lit_list_vars(As)),
    {Brs,St1} = bif_returns(Op, Rs, St0),
    {Bif#k_bif{anno=#k{us=Used,ns=lit_list_vars(Brs),a=A},ret=Brs},
     Used,St1};
uexpr(#k_match{anno=A,vars=Vs,body=B0}, Br, St0) ->
    Rs = break_rets(Br),
    {B1,Bu,St1} = umatch(B0, Br, St0),
    {#k_match{anno=#k{us=Bu,ns=lit_list_vars(Rs),a=A},
	      vars=Vs,body=B1,ret=Rs},Bu,St1};
uexpr(#k_receive{anno=A,var=V,body=B0,timeout=T,action=A0}, Br, St0) ->
    Rs = break_rets(Br),
    Tu = lit_vars(T),				%Timeout is atomic
    {B1,Bu,St1} = umatch(B0, Br, St0),
    {A1,Au,St2} = ubody(A0, Br, St1),
    Used = del_element(V#k_var.name, union(Bu, union(Tu, Au))),
    {#k_receive{anno=#k{us=Used,ns=lit_list_vars(Rs),a=A},
		var=V,body=B1,timeout=T,action=A1,ret=Rs},
     Used,St2};
uexpr(#k_receive_accept{anno=A}, _, St) ->
    {#k_receive_accept{anno=#k{us=[],ns=[],a=A}},[],St};
uexpr(#k_receive_next{anno=A}, _, St) ->
    {#k_receive_next{anno=#k{us=[],ns=[],a=A}},[],St};
uexpr(#k_try{anno=A,arg=A0,vars=Vs,body=B0,evars=Evs,handler=H0},
      {break,Rs0}, St0) ->
    {Avs,St1} = new_vars(length(Vs), St0),	%Need dummy names here
    {A1,Au,St2} = ubody(A0, {break,Avs}, St1),	%Must break to clean up here!
    {B1,Bu,St3} = ubody(B0, {break,Rs0}, St2),
    {H1,Hu,St4} = ubody(H0, {break,Rs0}, St3),
    %% Guarantee ONE return variable.
    NumNew = if
		 Rs0 =:= [] -> 1;
		 true -> 0
	     end,
    {Ns,St5} = new_vars(NumNew, St4),
    Rs1 = Rs0 ++ Ns,
    Used = union([Au,subtract(Bu, lit_list_vars(Vs)),
		  subtract(Hu, lit_list_vars(Evs))]),
    {#k_try{anno=#k{us=Used,ns=lit_list_vars(Rs1),a=A},
	    arg=A1,vars=Vs,body=B1,evars=Evs,handler=H1,ret=Rs1},
     Used,St5};
uexpr(#k_catch{anno=A,body=B0}, {break,Rs0}, St0) ->
    {Rb,St1} = new_var(St0),
    {B1,Bu,St2} = ubody(B0, {break,[Rb]}, St1),
    %% Guarantee ONE return variable.
    {Ns,St3} = new_vars(1 - length(Rs0), St2),
    Rs1 = Rs0 ++ Ns,
    {#k_catch{anno=#k{us=Bu,ns=lit_list_vars(Rs1),a=A},body=B1,ret=Rs1},Bu,St3};
uexpr(#ifun{anno=A,vars=Vs,body=B0}=IFun, {break,Rs}, St0) ->
    {B1,Bu,St1} = ubody(B0, return, St0),	%Return out of new function
    Ns = lit_list_vars(Vs),
    Free = subtract(Bu, Ns),			%Free variables in fun
    Fvs = make_vars(Free),
    Arity = length(Vs) + length(Free),
    {{Index,Uniq,Fname}, St3} =
	case lists:keysearch(id, 1, A) of
	    {value,{id,Id}} ->
		{Id, St1};
	    false ->
		%% No id annotation. Must invent one.
		I = St1#kern.fcount,
		U = erlang:hash(IFun, (1 bsl 27)-1),
		{N, St2} = new_fun_name(St1),
		{{I,U,N}, St2}
	end,
    Fun = #k_fdef{anno=#k{us=[],ns=[],a=A},func=Fname,arity=Arity,
		  vars=Vs ++ Fvs,body=B1},
    {#k_bif{anno=#k{us=Free,ns=lit_list_vars(Rs),a=A},
	    op=#k_internal{name=make_fun,arity=length(Free)+3},
	    args=[#k_atom{val=Fname},#k_int{val=Arity},
		  #k_int{val=Index},#k_int{val=Uniq}|Fvs],
	    ret=Rs},
%      {#k_call{anno=#k{us=Free,ns=lit_list_vars(Rs),a=A},
% 	     op=#k_internal{name=make_fun,arity=length(Free)+3},
% 	     args=[#k_atom{val=Fname},#k_int{val=Arity},
% 		   #k_int{val=Index},#k_int{val=Uniq}|Fvs],
% 	     ret=Rs},
     Free,St3#kern{funs=[Fun|St3#kern.funs]}};
uexpr(Lit, {break,Rs}, St) ->
    %% Transform literals to puts here.
    %%ok = io:fwrite("uexpr ~w:~p~n", [?LINE,Lit]),
    Used = lit_vars(Lit),
    {#k_put{anno=#k{us=Used,ns=lit_list_vars(Rs),a=get_kanno(Lit)},
	    arg=Lit,ret=Rs},Used,St}.

%% get_free(Name, Arity, State) -> [Free].
%% store_free(Name, Arity, [Free], State) -> State.

get_free(F, A, St) ->
    case orddict:find({F,A}, St#kern.free) of
	{ok,Val} -> Val;
	error -> []
    end.

store_free(F, A, Free, St) ->
    St#kern{free=orddict:store({F,A}, Free, St#kern.free)}.

break_rets({break,Rs}) -> Rs;
break_rets(return) -> [].

%% bif_returns(Op, [Ret], State) -> {[Ret],State}.

bif_returns(#k_remote{mod=M,name=N,arity=Ar}, Rs, St0) ->
    %%ok = io:fwrite("uexpr ~w:~p~n", [?LINE,{M,N,Ar,Rs}]),
    {Ns,St1} = new_vars(bif_vals(M, N, Ar) - length(Rs), St0),
    {Rs ++ Ns,St1};
bif_returns(#k_internal{name=N,arity=Ar}, Rs, St0) ->
    %%ok = io:fwrite("uexpr ~w:~p~n", [?LINE,{N,Ar,Rs}]),
    {Ns,St1} = new_vars(bif_vals(N, Ar) - length(Rs), St0),
    {Rs ++ Ns,St1}.

%% umatch(Match, Break, State) -> {Match,[UsedVar],State}.
%%  Tag a match expression with its used variables.

umatch(#k_alt{anno=A,first=F0,then=T0}, Br, St0) ->
    {F1,Fu,St1} = umatch(F0, Br, St0),
    {T1,Tu,St2} = umatch(T0, Br, St1),
    Used = union(Fu, Tu),
    {#k_alt{anno=#k{us=Used,ns=[],a=A},first=F1,then=T1},
     Used,St2};
umatch(#k_select{anno=A,var=V,types=Ts0}, Br, St0) ->
    {Ts1,Tus,St1} = umatch_list(Ts0, Br, St0),
    Used = add_element(V#k_var.name, Tus),
    {#k_select{anno=#k{us=Used,ns=[],a=A},var=V,types=Ts1},Used,St1};
umatch(#k_type_clause{anno=A,type=T,values=Vs0}, Br, St0) ->
    {Vs1,Vus,St1} = umatch_list(Vs0, Br, St0),
    {#k_type_clause{anno=#k{us=Vus,ns=[],a=A},type=T,values=Vs1},Vus,St1};
umatch(#k_val_clause{anno=A,val=P,body=B0}, Br, St0) ->
    {U0,Ps} = pat_vars(P),
    {B1,Bu,St1} = umatch(B0, Br, St0),
    Used = union(U0, subtract(Bu, Ps)),
    {#k_val_clause{anno=#k{us=Used,ns=[],a=A},val=P,body=B1},
     Used,St1};
umatch(#k_guard{anno=A,clauses=Gs0}, Br, St0) ->
    {Gs1,Gus,St1} = umatch_list(Gs0, Br, St0),
    {#k_guard{anno=#k{us=Gus,ns=[],a=A},clauses=Gs1},Gus,St1};
umatch(#k_guard_clause{anno=A,guard=G0,body=B0}, Br, St0) ->
    %%ok = io:fwrite("~w: ~p~n", [?LINE,G0]),
    {G1,Gu,St1} = uguard(G0, St0),
    %%ok = io:fwrite("~w: ~p~n", [?LINE,G1]),
    {B1,Bu,St2} = umatch(B0, Br, St1),
    Used = union(Gu, Bu),
    {#k_guard_clause{anno=#k{us=Used,ns=[],a=A},guard=G1,body=B1},Used,St2};
umatch(B0, Br, St0) -> ubody(B0, Br, St0).

umatch_list(Ms0, Br, St) ->
    foldr(fun (M0, {Ms1,Us,Sta}) ->
		  {M1,Mu,Stb} = umatch(M0, Br, Sta),
		  {[M1|Ms1],union(Mu, Us),Stb}
	  end, {[],[],St}, Ms0).

%% op_vars(Op) -> [VarName].

op_vars(#k_local{}) -> [];
op_vars(#k_remote{mod=Mod,name=Name}) ->
    ordsets:from_list([V || #k_var{name=V} <- [Mod,Name]]);
op_vars(#k_internal{}) -> [];
op_vars(Atomic) -> lit_vars(Atomic).

%% lit_vars(Literal) -> [VarName].
%%  Return the variables in a literal.

lit_vars(#k_var{name=N}) -> [N];
lit_vars(#k_int{}) -> [];
lit_vars(#k_float{}) -> [];
lit_vars(#k_atom{}) -> [];
%%lit_vars(#k_char{}) -> [];
lit_vars(#k_string{}) -> [];
lit_vars(#k_nil{}) -> [];
lit_vars(#k_cons{hd=H,tl=T}) ->
    union(lit_vars(H), lit_vars(T));
lit_vars(#k_binary{segs=V}) -> lit_vars(V);
lit_vars(#k_bin_end{}) -> [];
lit_vars(#k_bin_seg{size=Size,seg=S,next=N}) ->
    union(lit_vars(Size), union(lit_vars(S), lit_vars(N)));
lit_vars(#k_tuple{es=Es}) ->
    lit_list_vars(Es).

lit_list_vars(Ps) ->
    foldl(fun (P, Vs) -> union(lit_vars(P), Vs) end, [], Ps).

%% pat_vars(Pattern) -> {[UsedVarName],[NewVarName]}.
%%  Return variables in a pattern.  All variables are new variables
%%  except those in the size field of binary segments.

pat_vars(#k_var{name=N}) -> {[],[N]};
%%pat_vars(#k_char{}) -> {[],[]};
pat_vars(#k_int{}) -> {[],[]};
pat_vars(#k_float{}) -> {[],[]};
pat_vars(#k_atom{}) -> {[],[]};
pat_vars(#k_string{}) -> {[],[]};
pat_vars(#k_nil{}) -> {[],[]};
pat_vars(#k_cons{hd=H,tl=T}) ->
    pat_list_vars([H,T]);
pat_vars(#k_binary{segs=V}) ->
    pat_vars(V);
pat_vars(#k_bin_seg{size=Size,seg=S,next=N}) ->
    {U1,New} = pat_list_vars([S,N]),
    {[],U2} = pat_vars(Size),
    {union(U1, U2),New};
pat_vars(#k_bin_end{}) -> {[],[]};
pat_vars(#k_tuple{es=Es}) ->
    pat_list_vars(Es).

pat_list_vars(Ps) ->
    foldl(fun (P, {Used0,New0}) ->
		  {Used,New} = pat_vars(P),
		  {union(Used0, Used),union(New0, New)} end,
	  {[],[]}, Ps).

%% aligned(Bits, Size, Unit, Flags) -> {Size,Flags}
%%  Add 'aligned' to the flags if the current field is aligned.
%%  Number of bits correct modulo 8.

aligned(B, S, U, Fs) when B rem 8 =:= 0 ->
    {incr_bits(B, S, U),[aligned|Fs]};
aligned(B, S, U, Fs) ->
    {incr_bits(B, S, U),Fs}.

incr_bits(B, #k_int{val=S}, U) when integer(B) -> B + S*U;
incr_bits(_, #k_atom{val=all}, _) -> 0;		%Always aligned
incr_bits(B, _, 8) -> B;
incr_bits(_, _, _) -> unknown.

make_list(Es) ->
    foldr(fun (E, Acc) -> #c_cons{hd=E,tl=Acc} end, #c_nil{}, Es).

%% List of integers in interval [N,M]. Empty list if N > M.

integers(N, M) when N =< M ->
    [N|integers(N + 1, M)];
integers(_, _) -> [].

%%%
%%% Handling of warnings.
%%%

format_error({nomatch_shadow,Line}) ->
    M = io_lib:format("this clause cannot match because a previous clause at line ~p "
		      "always matches", [Line]),
    lists:flatten(M);
format_error(nomatch_shadow) ->
    "this clause cannot match because a previous clause always matches".

add_warning(none, Term, #kern{ws=Ws}=St) ->
    St#kern{ws=[{?MODULE,Term}|Ws]};
add_warning(Line, Term, #kern{ws=Ws}=St) when Line >= 0 ->
    St#kern{ws=[{Line,?MODULE,Term}|Ws]};
add_warning(_, _, St) -> St.

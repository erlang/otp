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
%% 4. All remote-calls are to statically named m:f/a. Meta-calls are
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

-import(lists, [all/2,droplast/1,flatten/1,foldl/3,foldr/3,
                map/2,mapfoldl/3,member/2,
		keyfind/3,keyreplace/4,
                last/1,partition/2,reverse/1,
                sort/1,sort/2,splitwith/2]).
-import(ordsets, [add_element/2,intersection/2,
                  subtract/2,union/2,union/1]).

-include("core_parse.hrl").
-include("v3_kernel.hrl").
-define(EXPAND_MAX_SIZE_SEGMENT, 1024).

%% These are not defined in v3_kernel.hrl.
get_kanno(Kthing) -> element(2, Kthing).
set_kanno(Kthing, Anno) -> setelement(2, Kthing, Anno).
copy_anno(Kdst, Ksrc) ->
    Anno = get_kanno(Ksrc),
    set_kanno(Kdst, Anno).

%% Internal kernel expressions and help functions.
%% N.B. the annotation field is ALWAYS the first field!

-record(ivalues, {anno=[],args}).
-record(ifun, {anno=[],vars,body}).
-record(iset, {anno=[],vars,arg,body}).
-record(iletrec, {anno=[],defs}).
-record(ialias, {anno=[],vars,pat}).
-record(iclause, {anno=[],isub,osub,pats,guard,body}).

-type warning() :: term().	% XXX: REFINE

%% State record for kernel translator.
-record(kern, {func,				%Current host function
               fargs=[] :: [#k_var{}],          %Arguments for current function
	       vcount=0,			%Variable counter
	       fcount=0,			%Fun counter
               ds=cerl_sets:new() :: cerl_sets:set(), %Defined variables
	       funs=[],				%Fun functions
	       free=#{},			%Free variables
	       ws=[]   :: [warning()],		%Warnings.
               no_shared_fun_wrappers=false :: boolean(),
               labels=cerl_sets:new()
              }).

-spec module(cerl:c_module(), [compile:option()]) ->
	{'ok', #k_mdef{}, [warning()]}.

module(#c_module{anno=A,name=M,exports=Es,attrs=As,defs=Fs}, Options) ->
    Kas = attributes(As),
    Kes = map(fun (#c_var{name={_,_}=Fname}) -> Fname end, Es),
    NoSharedFunWrappers = proplists:get_bool(no_shared_fun_wrappers,
                                             Options),
    St0 = #kern{no_shared_fun_wrappers=NoSharedFunWrappers},
    {Kfs,St} = mapfoldl(fun function/2, St0, Fs),
    {ok,#k_mdef{anno=A,name=M#c_literal.val,exports=Kes,attributes=Kas,
                body=Kfs ++ St#kern.funs},sort(St#kern.ws)}.

attributes([{#c_literal{val=Name},#c_literal{val=Val}}|As]) ->
    case include_attribute(Name) of
	false ->
	    attributes(As);
	true ->
	    [{Name,Val}|attributes(As)]
    end;
attributes([]) -> [].

include_attribute(type) -> false;
include_attribute(spec) -> false;
include_attribute(callback) -> false;
include_attribute(opaque) -> false;
include_attribute(export_type) -> false;
include_attribute(record) -> false;
include_attribute(optional_callbacks) -> false;
include_attribute(file) -> false;
include_attribute(compile) -> false;
include_attribute(_) -> true.

function({#c_var{name={F,Arity}=FA},Body}, St0) ->
    %%io:format("~w/~w~n", [F,Arity]),
    try
        %% Find a suitable starting value for the variable counter. Note
        %% that this pass assumes that new_var_name/1 returns a variable
        %% name distinct from any variable used in the entire body of
        %% the function. We use integers as variable names to avoid
        %% filling up the atom table when compiling huge functions.
        Count = cerl_trees:next_free_variable_name(Body),
	St1 = St0#kern{func=FA,vcount=Count,fcount=0,ds=cerl_sets:new()},
	{#ifun{anno=Ab,vars=Kvs,body=B0},[],St2} = expr(Body, new_sub(), St1),
	{B1,_,St3} = ubody(B0, return, St2),
	%%B1 = B0, St3 = St2,				%Null second pass
        {make_fdef(Ab, F, Arity, Kvs, B1),St3}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [F,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

%% body(Cexpr, Sub, State) -> {Kexpr,[PreKepxr],State}.
%%  Do the main sequence of a body.  A body ends in an atomic value or
%%  values.  Must check if vector first so do expr.

body(#c_values{anno=A,es=Ces}, Sub, St0) ->
    %% Do this here even if only in bodies.
    {Kes,Pe,St1} = atomic_list(Ces, Sub, St0),
    {#ivalues{anno=A,args=Kes},Pe,St1};
body(Ce, Sub, St0) ->
    expr(Ce, Sub, St0).

%% guard(Cexpr, Sub, State) -> {Kexpr,State}.
%%  We handle guards almost as bodies. The only special thing we
%%  must do is to make the final Kexpr a #k_test{}.

guard(G0, Sub, St0) ->
    {Ge0,Pre,St1} = expr(G0, Sub, St0),
    {Ge,St} = gexpr_test(Ge0, St1),
    {pre_seq(Pre, Ge),St}.

%% gexpr_test(Kexpr, State) -> {Kexpr,State}.
%%  Builds the final boolean test from the last Kexpr in a guard test.
%%  Must enter try blocks and isets and find the last Kexpr in them.
%%  This must end in a recognised BEAM test!

gexpr_test(#k_bif{anno=A,
                  op=#k_remote{mod=#k_literal{val=erlang},
                               name=#k_literal{val=F},arity=Ar}=Op,
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
		  handler=#k_literal{val=false}}=Try, St0) ->
    {B,St} = gexpr_test(B0, St0),
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{B0,B}]),
    {Try#k_try{arg=B},St};
gexpr_test(#iset{body=B0}=Iset, St0) ->
    {B1,St1} = gexpr_test(B0, St0),
    {Iset#iset{body=B1},St1};
gexpr_test(Ke, St) -> gexpr_test_add(Ke, St).	%Add equality test

gexpr_test_add(Ke, St0) ->
    Test = #k_remote{mod=#k_literal{val='erlang'},
		     name=#k_literal{val='=:='},
		     arity=2},
    {Ae,Ap,St1} = force_atomic(Ke, St0),
    {pre_seq(Ap, #k_test{anno=get_kanno(Ke),
			 op=Test,args=[Ae,#k_literal{val='true'}]}),St1}.

%% expr(Cexpr, Sub, State) -> {Kexpr,[PreKexpr],State}.
%%  Convert a Core expression, flattening it at the same time.

expr(#c_var{anno=A0,name={Name,Arity}}=Fname, Sub, St) ->
    Vs = [#c_var{name=list_to_atom("V" ++ integer_to_list(V))} ||
             V <- integers(1, Arity)],
    case St#kern.no_shared_fun_wrappers of
        false ->
            %% Generate a (possibly shared) wrapper function for calling
            %% this function.
            Wrapper0 = ["-fun.",atom_to_list(Name),"/",integer_to_list(Arity),"-"],
            Wrapper = list_to_atom(flatten(Wrapper0)),
            Id = {id,{0,0,Wrapper}},
            A = keyreplace(id, 1, A0, Id),
            Fun = #c_fun{anno=A,vars=Vs,body=#c_apply{anno=A,op=Fname,args=Vs}},
            expr(Fun, Sub, St);
        true ->
            %% For backward compatibility with OTP 22 and earlier,
            %% use the pre-generated name for the fun wrapper.
            %% There will be one wrapper function for each occurrence
            %% of `fun F/A`.
            Fun = #c_fun{anno=A0,vars=Vs,body=#c_apply{anno=A0,op=Fname,args=Vs}},
            expr(Fun, Sub, St)
    end;
expr(#c_var{anno=A,name=V}, Sub, St) ->
    {#k_var{anno=A,name=get_vsub(V, Sub)},[],St};
expr(#c_literal{anno=A,val=V}, _Sub, St) ->
    {#k_literal{anno=A,val=V},[],St};
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
expr(#c_map{anno=A,arg=Var,es=Ces}, Sub, St0) ->
    expr_map(A, Var, Ces, Sub, St0);
expr(#c_binary{anno=A,segments=Cv}, Sub, St0) ->
    try atomic_bin(Cv, Sub, St0) of
	{Kv,Ep,St1} ->
	    {#k_binary{anno=A,segs=Kv},Ep,St1}
    catch
	throw:bad_element_size ->
	    St1 = add_warning(get_line(A), bad_segment_size, A, St0),
	    Erl = #c_literal{val=erlang},
	    Name = #c_literal{val=error},
	    Args = [#c_literal{val=badarg}],
	    Error = #c_call{anno=A,module=Erl,name=Name,args=Args},
	    expr(Error, Sub, St1)
    end;
expr(#c_fun{anno=A,vars=Cvs,body=Cb}, Sub0,
     #kern{fargs=OldFargs}=St0) ->
    {Kvs,Sub1,St1} = pattern_list(Cvs, Sub0, St0),
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{{Cvs,Sub0,St0},{Kvs,Sub1,St1}}]),
    {Kb,Pb,St2} = body(Cb, Sub1, St1#kern{fargs=Kvs}),
    {#ifun{anno=A,vars=Kvs,body=pre_seq(Pb, Kb)},[],St2#kern{fargs=OldFargs}};
expr(#c_seq{arg=Ca,body=Cb}, Sub, St0) ->
    {Ka,Pa,St1} = body(Ca, Sub, St0),
    {Kb,Pb,St2} = body(Cb, Sub, St1),
    {Kb,Pa ++ [Ka] ++ Pb,St2};
expr(#c_let{anno=A,vars=Cvs,arg=Ca,body=Cb}, Sub0, St0) ->
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{Cvs,Sub0,St0}]),
    {Ka,Pa,St1} = body(Ca, Sub0, St0),
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
    {Kb,Pa ++ Sets ++ Pb,St3};
expr(#c_letrec{anno=A,defs=Cfs,body=Cb}, Sub, St) ->
    case member(letrec_goto, A) of
        true ->
            letrec_goto(Cfs, Cb, Sub, St);
        false ->
            letrec_local_function(A, Cfs, Cb, Sub, St)
    end;
expr(#c_case{arg=Ca,clauses=Ccs}, Sub, St0) ->
    {Ka,Pa,St1} = body(Ca, Sub, St0),		%This is a body!
    {Kvs,Pv,St2} = match_vars(Ka, St1),		%Must have variables here!
    {Km,St3} = kmatch(Kvs, Ccs, Sub, St2),
    Match = flatten_seq(build_match(Km)),
    {last(Match),Pa ++ Pv ++ droplast(Match),St3};
expr(#c_apply{anno=A,op=Cop,args=Cargs}, Sub, St) ->
    c_apply(A, Cop, Cargs, Sub, St);
expr(#c_call{anno=A,module=M0,name=F0,args=Cargs}, Sub, St0) ->
    Ar = length(Cargs),
    {[M,F|Kargs],Ap,St1} = atomic_list([M0,F0|Cargs], Sub, St0),
    Remote = #k_remote{mod=M,name=F,arity=Ar},
    case call_type(M0, F0, Cargs) of
        bif ->
            {#k_bif{anno=A,op=Remote,args=Kargs},Ap,St1};
        call ->
            {#k_call{anno=A,op=Remote,args=Kargs},Ap,St1};
        error ->
            %% Invalid call (e.g. M:42/3). Issue a warning, and let
            %% the generated code use the old explict apply.
            St = add_warning(get_line(A), bad_call, A, St0),
	    Call = #c_call{anno=A,
			   module=#c_literal{val=erlang},
			   name=#c_literal{val=apply},
			   args=[M0,F0,cerl:make_list(Cargs)]},
	    expr(Call, Sub, St)
    end;
expr(#c_primop{anno=A,name=#c_literal{val=match_fail},args=[Arg]}, Sub, St) ->
    translate_match_fail(Arg, Sub, A, St);
expr(#c_primop{anno=A,name=#c_literal{val=N},args=Cargs}, Sub, St0) ->
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
    {#k_catch{anno=A,body=pre_seq(Pb, Kb)},[],St1}.

%% Implement letrec in the traditional way as a local
%% function for each definition in the letrec.

letrec_local_function(A, Cfs, Cb, Sub0, St0) ->
    %% Make new function names and store substitution.
    {Fs0,{Sub1,St1}} =
	mapfoldl(fun ({#c_var{name={F,Ar}},B0}, {Sub,S0}) ->
			 {N,St1} = new_fun_name(atom_to_list(F)
						++ "/" ++
						integer_to_list(Ar),
						S0),
			 B = set_kanno(B0, [{letrec_name,N}]),
			 {{N,B},{set_fsub(F, Ar, N, Sub),St1}}
		 end, {Sub0,St0}, Cfs),
    %% Run translation on functions and body.
    {Fs1,St2} = mapfoldl(fun ({N,Fd0}, S1) ->
				 {Fd1,[],St2} = expr(Fd0, Sub1, S1),
				 Fd = set_kanno(Fd1, A),
				 {{N,Fd},St2}
			 end, St1, Fs0),
    {Kb,Pb,St3} = body(Cb, Sub1, St2),
    {Kb,[#iletrec{anno=A,defs=Fs1}|Pb],St3}.

%% Implement letrec with the single definition as a label and each
%% apply of it as a goto.

letrec_goto([{#c_var{name={Label,0}},Cfail}], Cb, Sub0,
            #kern{labels=Labels0}=St0) ->
    Labels = cerl_sets:add_element(Label, Labels0),
    {Kb,Pb,St1} = body(Cb, Sub0, St0#kern{labels=Labels}),
    #c_fun{body=FailBody} = Cfail,
    {Kfail,Fb,St2} = body(FailBody, Sub0, St1),
    case {Kb,Kfail,Fb} of
        {#k_goto{label=Label},#k_goto{}=InnerGoto,[]} ->
            {InnerGoto,Pb,St2};
        {_,_,_} ->
            St3 = St2#kern{labels=Labels0},
            Alt = #k_letrec_goto{label=Label,first=Kb,then=pre_seq(Fb, Kfail)},
            {Alt,Pb,St3}
    end.

%% translate_match_fail(Arg, Sub, Anno, St) -> {Kexpr,[PreKexpr],State}.
%%  Translate a match_fail primop to a call erlang:error/1 or
%%  erlang:error/2.

translate_match_fail(Arg, Sub, Anno, St0) ->
    Cargs = case {cerl:data_type(Arg),cerl:data_es(Arg)} of
                {tuple,[#c_literal{val=function_clause}|As]} ->
                    translate_fc_args(As, Sub, St0);
                {_,_} ->
                    [Arg]
            end,
    {Kargs,Ap,St} = atomic_list(Cargs, Sub, St0),
    Ar = length(Cargs),
    Call = #k_call{anno=Anno,
                   op=#k_remote{mod=#k_literal{val=erlang},
                                name=#k_literal{val=error},
                                arity=Ar},args=Kargs},
    {Call,Ap,St}.

translate_fc_args(As, Sub, #kern{fargs=Fargs}) ->
    case same_args(As, Fargs, Sub) of
        true ->
            %% The arguments for the `function_clause` exception are
            %% the arguments for the current function in the correct
            %% order.
            [#c_literal{val=function_clause},cerl:make_list(As)];
        false ->
            %% The arguments in the `function_clause` exception don't
            %% match the arguments for the current function because
            %% of inlining. Keeping the `function_clause`
            %% exception reason would be confusing. Rewrite it to
            %% a `case_clause` exception with the arguments in a
            %% tuple.
	    [cerl:c_tuple([#c_literal{val=case_clause},
                           cerl:c_tuple(As)])]
    end.

same_args([#c_var{name=Cv}|Vs], [#k_var{name=Kv}|As], Sub) ->
    get_vsub(Cv, Sub) =:= Kv andalso same_args(Vs, As, Sub);
same_args([], [], _Sub) -> true;
same_args(_, _, _) -> false.

expr_map(A,Var0,Ces,Sub,St0) ->
    {Var,Mps,St1} = expr(Var0, Sub, St0),
    {Km,Eps,St2} = map_split_pairs(A, Var, Ces, Sub, St1),
    {Km,Eps++Mps,St2}.

map_split_pairs(A, Var, Ces, Sub, St0) ->
    %% 1. Force variables.
    %% 2. Group adjacent pairs with literal keys.
    %% 3. Within each such group, remove multiple assignments to the same key.
    %% 4. Partition each group according to operator ('=>' and ':=').
    Pairs0 = [{Op,K,V} ||
		 #c_map_pair{op=#c_literal{val=Op},key=K,val=V} <- Ces],
    {Pairs,Esp,St1} = foldr(fun
	    ({Op,K0,V0}, {Ops,Espi,Sti0}) when Op =:= assoc; Op =:= exact ->
		{K,Eps1,Sti1} = atomic(K0, Sub, Sti0),
		{V,Eps2,Sti2} = atomic(V0, Sub, Sti1),
		{[{Op,K,V}|Ops],Eps1 ++ Eps2 ++ Espi,Sti2}
	end, {[],[],St0}, Pairs0),
    map_split_pairs_1(A, Var, Pairs, Esp, St1).

map_split_pairs_1(A, Map0, [{Op,Key,Val}|Pairs1]=Pairs0, Esp0, St0) ->
    {Map1,Em,St1} = force_atomic(Map0, St0),
    case Key of
	#k_var{} ->
	    %% Don't combine variable keys with other keys.
	    Kes = [#k_map_pair{key=Key,val=Val}],
	    Map = #k_map{anno=A,op=Op,var=Map1,es=Kes},
	    map_split_pairs_1(A, Map, Pairs1, Esp0 ++ Em, St1);
	_ ->
	    %% Literal key. Split off all literal keys.
	    {L,Pairs} = splitwith(fun({_,#k_var{},_}) -> false;
				     ({_,_,_}) -> true
				  end, Pairs0),
	    {Map,Esp,St2} = map_group_pairs(A, Map1, L, Esp0 ++ Em, St1),
	    map_split_pairs_1(A, Map, Pairs, Esp, St2)
    end;
map_split_pairs_1(_, Map, [], Esp, St0) ->
    {Map,Esp,St0}.

map_group_pairs(A, Var, Pairs0, Esp, St0) ->
    Pairs = map_remove_dup_keys(Pairs0),
    Assoc = [#k_map_pair{key=K,val=V} || {_,{assoc,K,V}} <- Pairs],
    Exact = [#k_map_pair{key=K,val=V} || {_,{exact,K,V}} <- Pairs],
    case {Assoc,Exact} of
	{[_|_],[]} ->
	    {#k_map{anno=A,op=assoc,var=Var,es=Assoc},Esp,St0};
	{[],[_|_]} ->
	    {#k_map{anno=A,op=exact,var=Var,es=Exact},Esp,St0};
	{[_|_],[_|_]} ->
	    Map = #k_map{anno=A,op=assoc,var=Var,es=Assoc},
	    {Mvar,Em,St1} = force_atomic(Map, St0),
	    {#k_map{anno=A,op=exact,var=Mvar,es=Exact},Esp ++ Em,St1}
    end.

map_remove_dup_keys(Es) ->
    map_remove_dup_keys(Es, #{}).

map_remove_dup_keys([{assoc,K0,V}|Es0], Used0) ->
    K = map_key_clean(K0),
    Op = case Used0 of
             #{K:={exact,_,_}} -> exact;
             #{} -> assoc
         end,
    Used1 = Used0#{K=>{Op,K0,V}},
    map_remove_dup_keys(Es0, Used1);
map_remove_dup_keys([{exact,K0,V}|Es0], Used0) ->
    K = map_key_clean(K0),
    Op = case Used0 of
             #{K:={assoc,_,_}} -> assoc;
             #{} -> exact
         end,
    Used1 = Used0#{K=>{Op,K0,V}},
    map_remove_dup_keys(Es0, Used1);
map_remove_dup_keys([], Used) ->
    %% We must sort the map entries to ensure consistent
    %% order from compilation to compilation.
    sort(maps:to_list(Used)).

%% Clean a map key from annotations.
map_key_clean(#k_var{name=V})    -> {var,V};
map_key_clean(#k_literal{val=V}) -> {lit,V}.

%% call_type(Module, Function, Arity) -> call | bif | error.
%%  Classify the call.
call_type(#c_literal{val=M}, #c_literal{val=F}, As) when is_atom(M), is_atom(F) ->
    case is_remote_bif(M, F, As) of
	false -> call;
	true -> bif
    end;
call_type(#c_var{}, #c_literal{val=A}, _) when is_atom(A) -> call;
call_type(#c_literal{val=A}, #c_var{}, _) when is_atom(A) -> call;
call_type(#c_var{}, #c_var{}, _) -> call;
call_type(_, _, _) -> error.

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
%%  Transform application.

c_apply(A, #c_var{anno=Ra,name={F0,Ar}}, Cargs, Sub, #kern{labels=Labels}=St0) ->
    case Ar =:= 0 andalso cerl_sets:is_element(F0, Labels) of
        true ->
            %% This is a goto to a label in a letrec_goto construct.
            {#k_goto{label=F0},[],St0};
        false ->
            {Kargs,Ap,St1} = atomic_list(Cargs, Sub, St0),
            F1 = get_fsub(F0, Ar, Sub),         %Has it been rewritten
            {#k_call{anno=A,op=#k_local{anno=Ra,name=F1,arity=Ar},args=Kargs},
             Ap,St1}
    end;
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

%% atomic(Cexpr, Sub, State) -> {Katomic,[PreKexpr],State}.
%%  Convert a Core expression making sure the result is an atomic
%%  literal.

atomic(Ce, Sub, St0) ->
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

atomic_bin([#c_bitstr{anno=A,val=E0,size=S0,unit=U0,type=T,flags=Fs0}|Es0],
	   Sub, St0) ->
    {E,Ap1,St1} = atomic(E0, Sub, St0),
    {S1,Ap2,St2} = atomic(S0, Sub, St1),
    validate_bin_element_size(S1),
    U1 = cerl:concrete(U0),
    Fs1 = cerl:concrete(Fs0),
    {Es,Ap3,St3} = atomic_bin(Es0, Sub, St2),
    {#k_bin_seg{anno=A,size=S1,
		unit=U1,
		type=cerl:concrete(T),
		flags=Fs1,
		seg=E,next=Es},
     Ap1++Ap2++Ap3,St3};
atomic_bin([], _Sub, St) -> {#k_bin_end{},[],St}.

validate_bin_element_size(#k_var{}) -> ok;
validate_bin_element_size(#k_literal{val=Val}) ->
    case Val of
        all -> ok;
        undefined -> ok;
        _ when is_integer(Val), Val >= 0 -> ok;
        _ -> throw(bad_element_size)
    end.

%% atomic_list([Cexpr], Sub, State) -> {[Kexpr],[PreKexpr],State}.

atomic_list(Ces, Sub, St) ->
    foldr(fun (Ce, {Kes,Esp,St0}) ->
		  {Ke,Ep,St1} = atomic(Ce, Sub, St0),
		  {[Ke|Kes],Ep ++ Esp,St1}
	  end, {[],[],St}, Ces).

%% is_atomic(Kexpr) -> boolean().
%%  Is a Kexpr atomic?

is_atomic(#k_literal{}) -> true;
is_atomic(#k_var{}) -> true;
%%is_atomic(#k_char{}) -> true;			%No characters
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

%% pattern(Cpat, Isub, Osub, State) -> {Kpat,Sub,State}.
%%  Convert patterns.  Variables shadow so rename variables that are
%%  already defined.
%%
%%  Patterns are complicated by sizes in binaries.  These are pure
%%  input variables which create no bindings.  We, therefore, need to
%%  carry around the original substitutions to get the correct
%%  handling.

pattern(#c_var{anno=A,name=V}, _Isub, Osub, St0) ->
    case cerl_sets:is_element(V, St0#kern.ds) of
	true ->
	    {New,St1} = new_var_name(St0),
	    {#k_var{anno=A,name=New},
	     set_vsub(V, New, Osub),
	     St1#kern{ds=cerl_sets:add_element(New, St1#kern.ds)}};
	false ->
	    {#k_var{anno=A,name=V},Osub,
	     St0#kern{ds=cerl_sets:add_element(V, St0#kern.ds)}}
    end;
pattern(#c_literal{anno=A,val=Val}, _Isub, Osub, St) ->
    {#k_literal{anno=A,val=Val},Osub,St};
pattern(#c_cons{anno=A,hd=Ch,tl=Ct}, Isub, Osub0, St0) ->
    {Kh,Osub1,St1} = pattern(Ch, Isub, Osub0, St0),
    {Kt,Osub2,St2} = pattern(Ct, Isub, Osub1, St1),
    {#k_cons{anno=A,hd=Kh,tl=Kt},Osub2,St2};
pattern(#c_tuple{anno=A,es=Ces}, Isub, Osub0, St0) ->
    {Kes,Osub1,St1} = pattern_list(Ces, Isub, Osub0, St0),
    {#k_tuple{anno=A,es=Kes},Osub1,St1};
pattern(#c_map{anno=A,es=Ces}, Isub, Osub0, St0) ->
    {Kes,Osub1,St1} = pattern_map_pairs(Ces, Isub, Osub0, St0),
    {#k_map{anno=A,op=exact,es=Kes},Osub1,St1};
pattern(#c_binary{anno=A,segments=Cv}, Isub, Osub0, St0) ->
    {Kv,Osub1,St1} = pattern_bin(Cv, Isub, Osub0, St0),
    {#k_binary{anno=A,segs=Kv},Osub1,St1};
pattern(#c_alias{anno=A,var=Cv,pat=Cp}, Isub, Osub0, St0) ->
    {Cvs,Cpat} = flatten_alias(Cp),
    {Kvs,Osub1,St1} = pattern_list([Cv|Cvs], Isub, Osub0, St0),
    {Kpat,Osub2,St2} = pattern(Cpat, Isub, Osub1, St1),
    {#ialias{anno=A,vars=Kvs,pat=Kpat},Osub2,St2}.

flatten_alias(#c_alias{var=V,pat=P}) ->
    {Vs,Pat} = flatten_alias(P),
    {[V|Vs],Pat};
flatten_alias(Pat) -> {[],Pat}.

pattern_map_pairs(Ces0, Isub, Osub0, St0) ->
    %% pattern the pair keys and values as normal
    {Kes,{Osub1,St1}} = mapfoldl(fun
	    (#c_map_pair{anno=A,key=Ck,val=Cv},{Osubi0,Sti0}) ->
		{Kk,[],Sti1} = expr(Ck, Isub, Sti0),
		{Kv,Osubi2,Sti2} = pattern(Cv, Isub, Osubi0, Sti1),
		{#k_map_pair{anno=A,key=Kk,val=Kv},{Osubi2,Sti2}}
	end, {Osub0, St0}, Ces0),
    %% It is later assumed that these keys are term sorted
    %% so we need to sort them here
    Kes1 = sort(fun
	    (#k_map_pair{key=KkA},#k_map_pair{key=KkB}) ->
		A = map_key_clean(KkA),
		B = map_key_clean(KkB),
		erts_internal:cmp_term(A,B) < 0
	end, Kes),
    {Kes1,Osub1,St1}.

pattern_bin(Es, Isub, Osub0, St) ->
    pattern_bin_1(Es, Isub, Osub0, St).

pattern_bin_1([#c_bitstr{anno=A,val=E0,size=S0,unit=U0,type=T,flags=Fs0}|Es0],
              Isub, Osub0, St0) ->
    {S1,[],St1} = expr(S0, Isub, St0),
    S = case S1 of
	    #k_var{} -> S1;
            #k_literal{val=Val} when is_integer(Val); is_atom(Val) -> S1;
	    _ ->
		%% Bad size (coming from an optimization or Core Erlang
		%% source code) - replace it with a known atom because
		%% a literal or bit syntax construction can cause further
		%% problems.
		#k_literal{val=bad_size}
	end,
    U = cerl:concrete(U0),
    Fs = cerl:concrete(Fs0),
    {E,Osub1,St2} = pattern(E0, Isub, Osub0, St1),
    {Es,Osub,St3} = pattern_bin_1(Es0, Isub, Osub1, St2),
    {build_bin_seg(A, S, U, cerl:concrete(T), Fs, E, Es),Osub,St3};
pattern_bin_1([], _Isub, Osub, St) ->
    {#k_bin_end{},Osub,St}.

%% build_bin_seg(Anno, Size, Unit, Type, Flags, Seg, Next) -> #k_bin_seg{}.
%%  This function normalizes literal integers with size > 8 and literal
%%  utf8 segments into integers with size = 8 (and potentially an integer
%%  with size less than 8 at the end). This is so further optimizations
%%  have a normalized view of literal integers, allowing us to generate
%%  more literals and group more clauses. Those integers may be "squeezed"
%%  later into the largest integer possible.
%%
build_bin_seg(A, #k_literal{val=Bits} = Sz, U, integer=Type,
              [unsigned,big]=Flags, #k_literal{val=Int}=Seg, Next) ->
    Size = Bits * U,
    case integer_fits_and_is_expandable(Int, Size) of
	true -> build_bin_seg_integer_recur(A, Size, Int, Next);
	false -> #k_bin_seg{anno=A,size=Sz,unit=U,type=Type,flags=Flags,seg=Seg,next=Next}
    end;
build_bin_seg(A, Sz, U, utf8=Type, [unsigned,big]=Flags, #k_literal{val=Utf8} = Seg, Next) ->
    case utf8_fits(Utf8) of
      {Int, Bits} -> build_bin_seg_integer_recur(A, Bits, Int, Next);
      error -> #k_bin_seg{anno=A,size=Sz,unit=U,type=Type,flags=Flags,seg=Seg,next=Next}
    end;
build_bin_seg(A, Sz, U, Type, Flags, Seg, Next) ->
    #k_bin_seg{anno=A,size=Sz,unit=U,type=Type,flags=Flags,seg=Seg,next=Next}.

build_bin_seg_integer_recur(A, Bits, Val, Next) when Bits > 8 ->
    NextBits = Bits - 8,
    NextVal = Val band ((1 bsl NextBits) - 1),
    Last = build_bin_seg_integer_recur(A, NextBits, NextVal, Next),
    build_bin_seg_integer(A, 8, Val bsr NextBits, Last);

build_bin_seg_integer_recur(A, Bits, Val, Next) ->
    build_bin_seg_integer(A, Bits, Val, Next).

build_bin_seg_integer(A, Bits, Val, Next) ->
    Sz = #k_literal{anno=A,val=Bits},
    Seg = #k_literal{anno=A,val=Val},
    #k_bin_seg{anno=A,size=Sz,unit=1,type=integer,flags=[unsigned,big],seg=Seg,next=Next}.

integer_fits_and_is_expandable(Int, Size) when 0 < Size, Size =< ?EXPAND_MAX_SIZE_SEGMENT ->
    case <<Int:Size>> of
	<<Int:Size>> -> true;
	_ -> false
    end;
integer_fits_and_is_expandable(_Int, _Size) ->
    false.

utf8_fits(Utf8) ->
    try
	Bin = <<Utf8/utf8>>,
	Bits = bit_size(Bin),
	<<Int:Bits>> = Bin,
	{Int, Bits}
    catch
	_:_ -> error
    end.

%% pattern_list([Cexpr], Sub, State) -> {[Kexpr],Sub,State}.

pattern_list(Ces, Sub, St) ->
    pattern_list(Ces, Sub, Sub, St).

pattern_list(Ces, Isub, Osub, St) ->
    foldr(fun (Ce, {Kes,Osub0,St0}) ->
		  {Ke,Osub1,St1} = pattern(Ce, Isub, Osub0, St0),
		  {[Ke|Kes],Osub1,St1}
	  end, {[],Osub,St}, Ces).

%% new_sub() -> Subs.
%% set_vsub(Name, Sub, Subs) -> Subs.
%% subst_vsub(Name, Sub, Subs) -> Subs.
%% get_vsub(Name, Subs) -> SubName.
%%  Add/get substitute Sub for Name to VarSub.
%%
%%  We're using a many-to-one bimap so we can rename all references to a
%%  variable without having to scan through all of them, which can cause
%%  compile times to explode (see record_SUITE:slow_compilation/1).

new_sub() -> {#{}, #{}}.

get_vsub(Key, Subs) ->
    bimap_get(Key, Subs, Key).

get_fsub(Name, Arity, Subs) ->
    bimap_get({Name, Arity}, Subs, Name).

set_vsub(Same, Same, Subs) ->
    Subs;
set_vsub(Key, Val, Subs) ->
    bimap_set(Key, Val, Subs).

set_fsub(Name, Arity, Val, Subs) ->
    set_vsub({Name, Arity}, Val, Subs).

subst_vsub(Key, Val, Subs) ->
    bimap_rename(Key, Val, Subs).

bimap_get(Key, {Map, _InvMap}, Default) ->
    case Map of
        #{ Key := Val } -> Val;
        _ -> Default
    end.

%% Maps Key to Val without touching existing references to Key.
bimap_set(Key, Val, {Map0, InvMap0}) ->
    InvMap = bm_update_inv_lookup(Key, Val, Map0, InvMap0),
    Map = Map0#{ Key => Val },
    {Map, InvMap}.

bm_update_inv_lookup(Key, Val, Map, InvMap0) ->
    InvMap = bm_cleanup_inv_lookup(Key, Map, InvMap0),
    case InvMap of
        #{ Val := Keys } ->
            %% Other keys map to the same value, add ours to the set.
            InvMap#{ Val := ordsets:add_element(Key, Keys) };
        #{} ->
            InvMap#{ Val => [Key] }
    end.

bm_cleanup_inv_lookup(Key, Map, InvMap) when is_map_key(Key, Map) ->
    #{ Key := Old } = Map,
    case InvMap of
        #{ Old := [Key] } ->
            maps:remove(Old, InvMap);
        #{ Old := [_|_]=Keys } ->
            InvMap#{ Old := ordsets:del_element(Key, Keys) }
    end;
bm_cleanup_inv_lookup(_Key, _Map, InvMap) ->
    InvMap.

%% Maps Key to Val, and replaces all existing references to Key with Val.
bimap_rename(Key, Val, {Map0, InvMap0}) when is_map_key(Key, InvMap0) ->
    Keys = map_get(Key, InvMap0),

    Map1 = Map0#{ Key => Val },
    Map = bimap_update_lookup(Keys, Val, Map1),

    InvMap1 = maps:remove(Key, InvMap0),
    InvMap = InvMap1#{ Val => ordsets:add_element(Key, Keys) },

    {Map, InvMap};
bimap_rename(Key, Val, Subs) ->
    bimap_set(Key, Val, Subs).

bimap_update_lookup([Key | Keys], Val, Map) ->
    bimap_update_lookup(Keys, Val, Map#{ Key := Val });
bimap_update_lookup([], _Val, Map) ->
    Map.

new_fun_name(St) ->
    new_fun_name("anonymous", St).

%% new_fun_name(Type, State) -> {FunName,State}.

new_fun_name(Type, #kern{func={F,Arity},fcount=C}=St) ->
    Name = "-" ++ atom_to_list(F) ++ "/" ++ integer_to_list(Arity) ++
	"-" ++ Type ++ "-" ++ integer_to_list(C) ++ "-",
    {list_to_atom(Name),St#kern{fcount=C+1}}.

%% new_var_name(State) -> {VarName,State}.

new_var_name(#kern{vcount=C}=St) ->
    {C,St#kern{vcount=C+1}}.

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

%% is_remote_bif(Mod, Name, Arity) -> true | false.
%%  Test if function is really a BIF.

is_remote_bif(erlang, get, [_]) -> true;
is_remote_bif(erlang, is_record, [_,Tag,Sz]) ->
    case {Tag,Sz} of
	{#c_literal{val=Atom},#c_literal{val=Int}}
          when is_atom(Atom), is_integer(Int) ->
	    %% Tag and size are literals. This is a guard BIF.
            true;
        {_,_} ->
            false
    end;
is_remote_bif(erlang, N, As) ->
    Arity = length(As),
    case erl_internal:guard_bif(N, Arity) of
	true -> true;
	false ->
	    try erl_internal:op_type(N, Arity) of
		arith -> true;
		bool -> true;
		comp -> true;
		list -> false;
		send -> false
	    catch
		_:_ -> false		% not an op
	    end
    end;
is_remote_bif(_, _, _) -> false.

%% bif_vals(Name, Arity) -> integer().
%% bif_vals(Mod, Name, Arity) -> integer().
%%  Determine how many return values a BIF has.  Provision for BIFs to
%%  return multiple values.  Only used in bodies where a BIF may be
%%  called for effect only.

bif_vals(recv_peek_message, 0) -> 2;
bif_vals(_, _) -> 1.

bif_vals(_, _, _) -> 1.

%% foldr2(Fun, Acc, List1, List2) -> Acc.
%%  Fold over two lists.

foldr2(Fun, Acc0, [E1|L1], [E2|L2]) ->
    Acc1 = Fun(E1, E2, Acc0),
    foldr2(Fun, Acc1, L1, L2);
foldr2(_, Acc, [], []) -> Acc.

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
%% later passes.  Add locally defined vars and variable subs to each
%% clause for later use.
%%
%% 2. The pattern matching is optimised.  Variable substitutions are
%% added to the VarSub structure and new variables are made visible.
%% The guard and body are then converted to Kernel form.

%% kmatch([Var], [Clause], Sub, State) -> {Kexpr,State}.

kmatch(Us, Ccs, Sub, St0) ->
    {Cs,St1} = match_pre(Ccs, Sub, St0),	%Convert clauses
    Def = fail,
    match(Us, Cs, Def, St1).		%Do the match.

%% match_pre([Cclause], Sub, State) -> {[Clause],State}.
%%  Must be careful not to generate new substitutions here now!
%%  Remove clauses with trivially false guards which will never
%%  succeed.

match_pre(Cs, Sub0, St) ->
    foldr(fun (#c_clause{anno=A,pats=Ps,guard=G,body=B}, {Cs0,St0}) ->
		  {Kps,Osub1,St1} = pattern_list(Ps, Sub0, St0),
		  {[#iclause{anno=A,isub=Sub0,osub=Osub1,
			     pats=Kps,guard=G,body=B}|
		    Cs0],St1}
	  end, {[],St}, Cs).

%% match([Var], [Clause], Default, State) -> {MatchExpr,State}.

match([_U|_Us] = L, Cs, Def, St0) ->
    %%ok = io:format("match ~p~n", [Cs]),
    Pcss = partition(Cs),
    foldr(fun (Pcs, {D,St}) -> match_varcon(L, Pcs, D, St) end,
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

match_guard_1([#iclause{anno=A,osub=Osub,guard=G,body=B}|Cs0], Def0, St0) ->
    case is_true_guard(G) of
	true ->
	    %% The true clause body becomes the default.
	    {Kb,Pb,St1} = body(B, Osub, St0),
	    St2 = maybe_add_warning(Cs0, A, St1),
	    St = maybe_add_warning(Def0, A, St2),
	    {[],pre_seq(Pb, Kb),St};
	false ->
	    {Kg,St1} = guard(G, Osub, St0),
	    {Kb,Pb,St2} = body(B, Osub, St1),
	    {Cs1,Def1,St3} = match_guard_1(Cs0, Def0, St2),
	    {[#k_guard_clause{guard=Kg,body=pre_seq(Pb, Kb)}|Cs1],
	     Def1,St3}
    end;
match_guard_1([], Def, St) -> {[],Def,St}. 

maybe_add_warning([C|_], MatchAnno, St) ->
    maybe_add_warning(C, MatchAnno, St);
maybe_add_warning([], _MatchAnno, St) -> St;
maybe_add_warning(fail, _MatchAnno, St) -> St;
maybe_add_warning(Ke, MatchAnno, St) ->
    case is_compiler_generated(Ke) of
	true ->
	    St;
	false ->
	    Anno = get_kanno(Ke),
	    Line = get_line(Anno),
	    MatchLine = get_line(MatchAnno),
	    Warn = case MatchLine of
		       none -> nomatch_shadow;
		       _ -> {nomatch_shadow,MatchLine}
		   end,
	    add_warning(Line, Warn, Anno, St)
    end.
    
get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.

get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> "no_file". % should not happen

%% is_true_guard(Guard) -> boolean().
%%  Test if a guard is trivially true.

is_true_guard(#c_literal{val=true}) -> true;
is_true_guard(_) -> false.

%% partition([Clause]) -> [[Clause]].
%%  Partition a list of clauses into groups which either contain
%%  clauses with a variable first argument, or with a "constructor".

partition([C1|Cs]) ->
    V1 = is_var_clause(C1),
    {More,Rest} = splitwith(fun (C) -> is_var_clause(C) =:= V1 end, Cs),
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
    Cs1 = map(fun (#iclause{isub=Isub0,osub=Osub0,pats=[Arg|As]}=C) ->
		      Vs = [arg_arg(Arg)|arg_alias(Arg)],
 		      Osub1 = foldl(fun (#k_var{name=V}, Acc) ->
 					   subst_vsub(V, U#k_var.name, Acc)
 				   end, Osub0, Vs),
 		      Isub1 = foldl(fun (#k_var{name=V}, Acc) ->
					    subst_vsub(V, U#k_var.name, Acc)
				    end, Isub0, Vs),
		      C#iclause{isub=Isub1,osub=Osub1,pats=As}
	      end, Cs0),
    match(Us, Cs1, Def, St).

%% match_con(Variables, [Clause], Default, State) -> {SelectExpr,State}.
%%  Build call to "select" from a list of clauses all containing a
%%  constructor/constant as first argument.  Group the constructors
%%  according to type, the order is really irrelevant but tries to be
%%  smart.
match_con([U|_Us] = L, Cs, Def, St0) ->
    %% Extract clauses for different constructors (types).
    %%ok = io:format("match_con ~p~n", [Cs]),
    Ttcs0 = select_types(Cs, [], [], [], [], [], [], [], [], []),
    Ttcs1 = [{T, Types} || {T, [_ | _] = Types} <- Ttcs0],
    Ttcs = opt_single_valued(Ttcs1),
    %%ok = io:format("ttcs = ~p~n", [Ttcs]),
    {Scs,St1} =
	mapfoldl(fun ({T,Tcs}, St) ->
			 {[S|_]=Sc,S1} = match_value(L, T, Tcs, fail, St),
			 %%ok = io:format("match_con type2 ~p~n", [T]),
			 Anno = get_kanno(S),
			 {#k_type_clause{anno=Anno,type=T,values=Sc},S1} end,
		 St0, Ttcs),
    {build_alt_1st_no_fail(build_select(U, Scs), Def),St1}.

select_types([NoExpC | Cs], Bin, BinCon, Cons, Tuple, Map, Atom, Float, Int, Nil) ->
    C = expand_pat_lit_clause(NoExpC),
    case clause_con(C) of
	k_binary ->
	    select_types(Cs, [C |Bin], BinCon, Cons, Tuple, Map, Atom, Float, Int, Nil);
	k_bin_seg ->
	    select_types(Cs, Bin, [C | BinCon], Cons, Tuple, Map, Atom, Float, Int, Nil);
	k_bin_end ->
	    select_types(Cs, Bin, [C | BinCon], Cons, Tuple, Map, Atom, Float, Int, Nil);
	k_cons ->
	    select_types(Cs, Bin, BinCon, [C | Cons], Tuple, Map, Atom, Float, Int, Nil);
	k_tuple ->
	    select_types(Cs, Bin, BinCon, Cons, [C | Tuple], Map, Atom, Float, Int, Nil);
	k_map ->
	    select_types(Cs, Bin, BinCon, Cons, Tuple, [C | Map], Atom, Float, Int, Nil);
	k_atom ->
	    select_types(Cs, Bin, BinCon, Cons, Tuple, Map, [C | Atom], Float, Int, Nil);
	k_float ->
	    select_types(Cs, Bin, BinCon, Cons, Tuple, Map, Atom, [C | Float], Int, Nil);
	k_int ->
	    select_types(Cs, Bin, BinCon, Cons, Tuple, Map, Atom, Float, [C | Int], Nil);
	k_nil ->
	    select_types(Cs, Bin, BinCon, Cons, Tuple, Map, Atom, Float, Int, [C | Nil])
    end;
select_types([], Bin, BinCon, Cons, Tuple, Map, Atom, Float, Int, Nil) ->
    [{k_binary, reverse(Bin)}] ++ handle_bin_con(reverse(BinCon)) ++
	[
	    {k_cons, reverse(Cons)},
	    {k_tuple, reverse(Tuple)},
	    {k_map, reverse(Map)},
	    {k_atom, reverse(Atom)},
	    {k_float, reverse(Float)},
	    {k_int, reverse(Int)},
	    {k_nil, reverse(Nil)}
	].

expand_pat_lit_clause(#iclause{pats=[#ialias{pat=#k_literal{anno=A,val=Val}}=Alias|Ps]}=C) ->
    P = expand_pat_lit(Val, A),
    C#iclause{pats=[Alias#ialias{pat=P}|Ps]};
expand_pat_lit_clause(#iclause{pats=[#k_literal{anno=A,val=Val}|Ps]}=C) ->
    P = expand_pat_lit(Val, A),
    C#iclause{pats=[P|Ps]};
expand_pat_lit_clause(C) -> C.

expand_pat_lit([H|T], A) ->
    #k_cons{anno=A,hd=#k_literal{anno=A,val=H},tl=#k_literal{anno=A,val=T}};
expand_pat_lit(Tuple, A) when is_tuple(Tuple) ->
    #k_tuple{anno=A,es=[#k_literal{anno=A,val=E} || E <- tuple_to_list(Tuple)]};
expand_pat_lit(Lit, A) ->
    #k_literal{anno=A,val=Lit}.

%% opt_singled_valued([{Type,Clauses}]) -> [{Type,Clauses}].
%%  If a type only has one clause and if the pattern is a complex
%%  literal, the matching can be done more efficiently by directly
%%  comparing with the literal (that is especially true for binaries).
%%
%%  It is important not to do this transformation for atomic literals
%%  (such as `[]`), since that would cause the test for an emtpy list
%%  to be executed before the test for a nonempty list.

opt_single_valued(Ttcs) ->
    opt_single_valued(Ttcs, [], []).

opt_single_valued([{_,[#iclause{pats=[#k_literal{}|_]}]}=Ttc|Ttcs], TtcAcc, LitAcc) ->
    %% This is an atomic literal.
    opt_single_valued(Ttcs, [Ttc|TtcAcc], LitAcc);
opt_single_valued([{_,[#iclause{pats=[P0|Ps]}=Tc]}=Ttc|Ttcs], TtcAcc, LitAcc) ->
    try combine_lit_pat(P0) of
        P ->
            LitTtc = Tc#iclause{pats=[P|Ps]},
            opt_single_valued(Ttcs, TtcAcc, [LitTtc|LitAcc])
    catch
        not_possible ->
            opt_single_valued(Ttcs, [Ttc|TtcAcc], LitAcc)
    end;
opt_single_valued([Ttc|Ttcs], TtcAcc, LitAcc) ->
    opt_single_valued(Ttcs, [Ttc|TtcAcc], LitAcc);
opt_single_valued([], TtcAcc, []) ->
    reverse(TtcAcc);
opt_single_valued([], TtcAcc, LitAcc) ->
    Literals = {k_literal,reverse(LitAcc)},
    %% Test the literals as early as possible.
    case reverse(TtcAcc) of
        [{k_binary,_}=Bin|Ttcs] ->
            %% The delayed creation of sub binaries requires
            %% bs_start_match2 to be the first instruction in the
            %% function.
            [Bin,Literals|Ttcs];
        Ttcs ->
            [Literals|Ttcs]
    end.

combine_lit_pat(#ialias{pat=Pat0}=Alias) ->
    Pat = combine_lit_pat(Pat0),
    Alias#ialias{pat=Pat};
combine_lit_pat(#k_literal{}) ->
    %% This is an atomic literal. Rewriting would be a pessimization,
    %% especially for `[]`.
    throw(not_possible);
combine_lit_pat(Pat) ->
    do_combine_lit_pat(Pat).

do_combine_lit_pat(#k_binary{anno=A,segs=Segs}) ->
    Bin = combine_bin_segs(Segs),
    #k_literal{anno=A,val=Bin};
do_combine_lit_pat(#k_cons{anno=A,hd=Hd0,tl=Tl0}) ->
    #k_literal{val=Hd} = do_combine_lit_pat(Hd0),
    #k_literal{val=Tl} = do_combine_lit_pat(Tl0),
    #k_literal{anno=A,val=[Hd|Tl]};
do_combine_lit_pat(#k_literal{}=Lit) ->
    Lit;
do_combine_lit_pat(#k_tuple{anno=A,es=Es0}) ->
    Es = [begin
              #k_literal{val=Lit} = do_combine_lit_pat(El),
              Lit
          end || El <- Es0],
    #k_literal{anno=A,val=list_to_tuple(Es)};
do_combine_lit_pat(_) ->
    throw(not_possible).

combine_bin_segs(#k_bin_seg{size=#k_literal{val=8},unit=1,type=integer,
                            flags=[unsigned,big],seg=#k_literal{val=Int},next=Next})
	when is_integer(Int), 0 =< Int, Int =< 255 ->
    <<Int,(combine_bin_segs(Next))/bits>>;
combine_bin_segs(#k_bin_end{}) ->
    <<>>;
combine_bin_segs(_) ->
    throw(not_possible).

%% handle_bin_con([Clause]) -> [{Type,[Clause]}].
%%  Handle clauses for the k_bin_seg constructor.  As k_bin_seg
%%  matching can overlap, the k_bin_seg constructors cannot be
%%  reordered, only grouped.

handle_bin_con(Cs) ->
    try
	%% The usual way to match literals is to first extract the
	%% value to a register, and then compare the register to the
	%% literal value. Extracting the value is good if we need
	%% compare it more than once.
	%%
	%% But we would like to combine the extracting and the
	%% comparing into a single instruction if we know that
	%% a binary segment must contain specific integer value
	%% or the matching will fail, like in this example:
	%%
	%% <<42:8,...>> ->
	%% <<42:8,...>> ->
	%% .
	%% .
	%% .
	%% <<42:8,...>> ->
	%% <<>> ->
	%%
	%% The first segment must either contain the integer 42
	%% or the binary must end for the match to succeed.
	%%
	%% The way we do is to replace the generic #k_bin_seg{}
	%% record with a #k_bin_int{} record if all clauses will
	%% select the same literal integer (except for one or more
	%% clauses that will end the binary).

	{BinSegs0,BinEnd} =
	    partition(fun (C) ->
			      clause_con(C) =:= k_bin_seg
		      end, Cs),
	BinSegs = select_bin_int(BinSegs0),
	case BinEnd of
	    [] -> BinSegs;
	    [_|_] -> BinSegs ++ [{k_bin_end,BinEnd}]
	end
    catch
	throw:not_possible ->
	    handle_bin_con_not_possible(Cs)
    end.

handle_bin_con_not_possible([C1|Cs]) ->
    Con = clause_con(C1),
    {More,Rest} = splitwith(fun (C) -> clause_con(C) =:= Con end, Cs),
    [{Con,[C1|More]}|handle_bin_con_not_possible(Rest)];
handle_bin_con_not_possible([]) -> [].

%% select_bin_int([Clause]) -> {k_bin_int,[Clause]}
%%  If the first pattern in each clause selects the same integer,
%%  rewrite all clauses to use #k_bin_int{} (which will later be
%%  translated to a bs_match_string/4 instruction).
%%
%%  If it is not possible to do this rewrite, a 'not_possible'
%%  exception is thrown.

select_bin_int([#iclause{pats=[#k_bin_seg{anno=A,type=integer,
                                          size=#k_literal{val=Bits0}=Sz,unit=U,
                                          flags=Fl,seg=#k_literal{val=Val},
                                          next=N}|Ps]}=C|Cs0]) when is_integer(Bits0) ->
    Bits = U * Bits0,
    if
	Bits > ?EXPAND_MAX_SIZE_SEGMENT -> throw(not_possible); %Expands the code too much.
	true -> ok
    end,
    select_assert_match_possible(Bits, Val, Fl),
    P = #k_bin_int{anno=A,size=Sz,unit=U,flags=Fl,val=Val,next=N},
    case member(native, Fl) of
	true -> throw(not_possible);
	false -> ok
    end,
    Cs = select_bin_int_1(Cs0, Bits, Fl, Val),
    [{k_bin_int,[C#iclause{pats=[P|Ps]}|Cs]}];
select_bin_int(_) -> throw(not_possible).

select_bin_int_1([#iclause{pats=[#k_bin_seg{anno=A,type=integer,
					    size=#k_literal{val=Bits0}=Sz,
					    unit=U,
					    flags=Fl,seg=#k_literal{val=Val},
					    next=N}|Ps]}=C|Cs],
		 Bits, Fl, Val) when is_integer(Val) ->
    if
	Bits0*U =:= Bits -> ok;
	true -> throw(not_possible)
    end,
    P = #k_bin_int{anno=A,size=Sz,unit=U,flags=Fl,val=Val,next=N},
    [C#iclause{pats=[P|Ps]}|select_bin_int_1(Cs, Bits, Fl, Val)];
select_bin_int_1([], _, _, _) -> [];
select_bin_int_1(_, _, _, _) -> throw(not_possible).

select_assert_match_possible(Sz, Val, Fs) ->
    EmptyBindings = erl_eval:new_bindings(),
    MatchFun = match_fun(Val),
    EvalFun = fun({integer,_,S}, B) -> {value,S,B} end,
    Expr = [{bin_element,0,{integer,0,Val},{integer,0,Sz},[{unit,1}|Fs]}],
    {value,Bin,EmptyBindings} = eval_bits:expr_grp(Expr, EmptyBindings, EvalFun),
    try
	{match,_} = eval_bits:match_bits(Expr, Bin,
					 EmptyBindings,
					 EmptyBindings,
					 MatchFun, EvalFun),
	ok  % this is just an assertion (i.e., no return value)
    catch
	throw:nomatch ->
	    throw(not_possible)
    end.

match_fun(Val) ->
    fun(match, {{integer,_,_},NewV,Bs}) when NewV =:= Val ->
	    {match,Bs}
    end.

%% match_value([Var], Con, [Clause], Default, State) -> {SelectExpr,State}.
%%  At this point all the clauses have the same constructor, we must
%%  now separate them according to value.

match_value(Us0, T, Cs0, Def, St0) ->
    {Us1,Cs1,St1} = partition_intersection(T, Us0, Cs0, St0),
    UCss = group_value(T, Us1, Cs1),
    %%ok = io:format("match_value ~p ~p~n", [T, Css]),
    mapfoldl(fun ({Us,Cs}, St) -> match_clause(Us, Cs, Def, St) end, St1, UCss).

%% partition_intersection(Type, Us, [Clause], State) -> {Us,Cs,State}.
%%  Partitions a map into two maps with the most common keys to the
%%  first map.
%%
%%      case <M> of
%%          <#{a,b}>
%%          <#{a,c}>
%%          <#{a}>
%%      end
%%
%%  becomes
%%
%%      case <M,M> of
%%          <#{a}, #{b}>
%%          <#{a}, #{c}>
%%          <#{a}, #{ }>
%%      end
%%
%%  The intention is to group as many keys together as possible and
%%  thus reduce the number of lookups to that key.

partition_intersection(k_map, [U|_]=Us, [_,_|_]=Cs0, St0) ->
    Ps = [clause_val(C) || C <- Cs0],
    case find_key_intersection(Ps) of
        none ->
            {Us,Cs0,St0};
        Ks ->
            Cs1 = map(fun(#iclause{pats=[Arg|Args]}=C) ->
                              {Arg1,Arg2} = partition_keys(Arg, Ks),
                              C#iclause{pats=[Arg1,Arg2|Args]}
                      end, Cs0),
            {[U|Us],Cs1,St0}
    end;
partition_intersection(_, Us, Cs, St) ->
    {Us,Cs,St}.

partition_keys(#k_map{es=Pairs}=Map, Ks) ->
    F = fun(#k_map_pair{key=Key}) ->
                cerl_sets:is_element(map_key_clean(Key), Ks)
        end,
    {Ps1,Ps2} = partition(F, Pairs),
    {Map#k_map{es=Ps1},Map#k_map{es=Ps2}};
partition_keys(#ialias{pat=Map}=Alias, Ks) ->
    %% Only alias one of them.
    {Map1,Map2} = partition_keys(Map, Ks),
    {Map1,Alias#ialias{pat=Map2}}.

find_key_intersection(Ps) ->
    Sets = [cerl_sets:from_list(Ks) || Ks <- Ps],
    Intersection = cerl_sets:intersection(Sets),
    case cerl_sets:size(Intersection) of
        0 ->
            none;
        _ ->
            All = all(fun (Kset) -> Kset =:= Intersection end, Sets),
            case All of
                true ->
                    %% All clauses test the same keys. Partitioning
                    %% the keys could only make the code worse.
                    none;
                false ->
                    Intersection
            end
    end.

%% group_value([Clause]) -> [[Clause]].
%%  Group clauses according to value.  Here we know that
%%  1. Some types are singled valued
%%  2. The clauses in maps and bin_segs cannot be reordered,
%%     only grouped
%%  3. Other types are disjoint and can be reordered

group_value(k_cons, Us, Cs)    -> [{Us,Cs}];  %These are single valued
group_value(k_nil, Us, Cs)     -> [{Us,Cs}];
group_value(k_binary, Us, Cs)  -> [{Us,Cs}];
group_value(k_bin_end, Us, Cs) -> [{Us,Cs}];
group_value(k_bin_seg, Us, Cs) -> group_keeping_order(Us, Cs);
group_value(k_bin_int, Us, Cs) -> [{Us,Cs}];
group_value(k_map, Us, Cs)     -> group_keeping_order(Us, Cs);
group_value(_, Us, Cs) ->
    Map = group_values(Cs, #{}),
    %% We must sort the grouped values to ensure consistent
    %% order from compilation to compilation.
    sort(maps:fold(fun (_, Vcs, Css) ->
                           [{Us,reverse(Vcs)}|Css]
                   end, [], Map)).

group_values([C|Cs], Acc) ->
    Val = clause_val(C),
    case Acc of
        #{Val:=Gcs} ->
            group_values(Cs, Acc#{Val:=[C|Gcs]});
        #{} ->
            group_values(Cs, Acc#{Val=>[C]})
    end;
group_values([], Acc) -> Acc.

group_keeping_order(Us, [C1|Cs]) ->
    V1 = clause_val(C1),
    {More,Rest} = splitwith(fun (C) -> clause_val(C) =:= V1 end, Cs),
    [{Us,[C1|More]}|group_keeping_order(Us, Rest)];
group_keeping_order(_, []) -> [].

%% match_clause([Var], [Clause], Default, State) -> {Clause,State}.
%%  At this point all the clauses have the same "value".  Build one
%%  select clause for this value and continue matching.  Rename
%%  aliases as well.

match_clause([U|Us], [C|_]=Cs0, Def, St0) ->
    Anno = get_kanno(C),
    {Match0,Vs,St1} = get_match(get_con(Cs0), St0),
    Match = sub_size_var(Match0, Cs0),
    {Cs1,St2} = new_clauses(Cs0, U, St1),
    Cs2 = squeeze_clauses_by_bin_integer_count(Cs1, []),
    {B,St3} = match(Vs ++ Us, Cs2, Def, St2),
    {#k_val_clause{anno=Anno,val=Match,body=B},St3}.

sub_size_var(#k_bin_seg{size=#k_var{name=Name}=Kvar}=BinSeg, [#iclause{isub=Sub}|_]) ->
    BinSeg#k_bin_seg{size=Kvar#k_var{name=get_vsub(Name, Sub)}};
sub_size_var(K, _) -> K.

get_con([C|_]) -> arg_arg(clause_arg(C)).	%Get the constructor

get_match(#k_cons{}, St0) ->
    {[H,T]=L,St1} = new_vars(2, St0),
    {#k_cons{hd=H,tl=T},L,St1};
get_match(#k_binary{}, St0) ->
    {[V]=Mes,St1} = new_vars(1, St0),
    {#k_binary{segs=V},Mes,St1};
get_match(#k_bin_seg{size=#k_literal{val=all},next={k_bin_end,[]}}=Seg, St0) ->
    {[S,N],St1} = new_vars(2, St0),
    {Seg#k_bin_seg{seg=S,next=N},[S],St1};
get_match(#k_bin_seg{}=Seg, St0) ->
    {[S,N],St1} = new_vars(2, St0),
    {Seg#k_bin_seg{seg=S,next=N},[S,N],St1};
get_match(#k_bin_int{}=BinInt, St0) ->
    {N,St1} = new_var(St0),
    {BinInt#k_bin_int{next=N},[N],St1};
get_match(#k_tuple{es=Es}, St0) ->
    {Mes,St1} = new_vars(length(Es), St0),
    {#k_tuple{es=Mes},Mes,St1};
get_match(#k_map{op=exact,es=Es0}, St0) ->
    {Mes,St1} = new_vars(length(Es0), St0),
    {Es,_} = mapfoldl(fun
	    (#k_map_pair{}=Pair, [V|Vs]) ->
		{Pair#k_map_pair{val=V},Vs}
	end, Mes, Es0),
    {#k_map{op=exact,es=Es},Mes,St1};
get_match(M, St) ->
    {M,[],St}.

new_clauses(Cs0, U, St) ->
    Cs1 = map(fun (#iclause{isub=Isub0,osub=Osub0,pats=[Arg|As]}=C) ->
		      Head = case arg_arg(Arg) of
				 #k_cons{hd=H,tl=T} -> [H,T|As];
				 #k_tuple{es=Es} -> Es ++ As;
				 #k_binary{segs=E}  -> [E|As];
				 #k_bin_seg{size=#k_literal{val=all},
					    seg=S,next={k_bin_end,[]}} ->
				     [S|As];
				 #k_bin_seg{seg=S,next=N} ->
				     [S,N|As];
				 #k_bin_int{next=N} ->
				     [N|As];
				 #k_map{op=exact,es=Es} ->
				     Vals = [V || #k_map_pair{val=V} <- Es],
				     Vals ++ As;
				 _Other ->
				     As
			     end,
		      Vs = arg_alias(Arg),
		      Osub1 = foldl(fun (#k_var{name=V}, Acc) ->
					    subst_vsub(V, U#k_var.name, Acc)
				    end, Osub0, Vs),
		      Isub1 = foldl(fun (#k_var{name=V}, Acc) ->
					    subst_vsub(V, U#k_var.name, Acc)
				    end, Isub0, Vs),
		      C#iclause{isub=Isub1,osub=Osub1,pats=Head}
	      end, Cs0),
    {Cs1,St}.

%% group and squeeze
%%  The goal of those functions is to group subsequent integer k_bin_seg
%%  literals by count so we can leverage bs_get_integer_16 whenever possible.
%%
%%  The priority is to create large groups. So if we have three clauses matching
%%  on 16-bits/16-bits/8-bits, we will first have a single 8-bits match for all
%%  three clauses instead of clauses (one with 16 and another with 8). But note
%%  the algorithm is recursive, so the remaining 8-bits for the first two clauses
%%  will be grouped next.
%%
%%  We also try to not create too large groups. If we have too many clauses,
%%  it is preferrable to match on 8-bits, select a branch, then match on the
%%  next 8-bits, rather than match on 16-bits which would force us to have
%%  to select to many values at the same time, which would not be efficient.
%%
%%  Another restriction is that we create groups only if the end of the
%%  group is a variadic clause or the end of the binary. That's because
%%  if we have 16-bits/16-bits/catch-all, breaking it into a 16-bits lookup
%%  will make the catch-all more expensive.
%%
%%  Clauses are grouped in reverse when squeezing and then flattened and
%%  re-reversed at the end.
squeeze_clauses_by_bin_integer_count([Clause | Clauses], Acc) ->
    case clause_count_bin_integer_segments(Clause) of
	{literal, N} -> squeeze_clauses_by_bin_integer_count(Clauses, N, 1, [Clause], Acc);
	_ -> squeeze_clauses_by_bin_integer_count(Clauses, [[Clause] | Acc])
    end;
squeeze_clauses_by_bin_integer_count(_, Acc) ->
    flat_reverse(Acc, []).

squeeze_clauses_by_bin_integer_count([], N, Count, GroupAcc, Acc) ->
    Squeezed = squeeze_clauses(GroupAcc, fix_count_without_variadic_segment(N), Count),
    flat_reverse([Squeezed | Acc], []);
squeeze_clauses_by_bin_integer_count([#iclause{pats=[#k_bin_end{} | _]} = Clause], N, Count, GroupAcc, Acc) ->
    Squeezed = squeeze_clauses(GroupAcc, fix_count_without_variadic_segment(N), Count),
    flat_reverse([[Clause | Squeezed] | Acc], []);
squeeze_clauses_by_bin_integer_count([Clause | Clauses], N, Count, GroupAcc, Acc) ->
    case clause_count_bin_integer_segments(Clause) of
	{literal, NewN} ->
	    squeeze_clauses_by_bin_integer_count(Clauses, min(N, NewN), Count + 1, [Clause | GroupAcc], Acc);

	{variadic, NewN} when NewN =< N ->
	    Squeezed = squeeze_clauses(GroupAcc, NewN, Count),
	    squeeze_clauses_by_bin_integer_count(Clauses, [[Clause | Squeezed] | Acc]);

	_ ->
	    squeeze_clauses_by_bin_integer_count(Clauses, [[Clause | GroupAcc] | Acc])
    end.

clause_count_bin_integer_segments(#iclause{pats=[#k_bin_seg{seg=#k_literal{}} = BinSeg | _]}) ->
    count_bin_integer_segments(BinSeg, 0);
clause_count_bin_integer_segments(#iclause{pats=[#k_bin_seg{size=#k_literal{val=Size},unit=Unit,
                                                            type=integer,flags=[unsigned,big],
                                                            seg=#k_var{}} | _]})
  when ((Size * Unit) rem 8) =:= 0 ->
    {variadic, (Size * Unit) div 8};
clause_count_bin_integer_segments(_) ->
    error.

count_bin_integer_segments(#k_bin_seg{size=#k_literal{val=8},unit=1,type=integer,flags=[unsigned,big],
                                      seg=#k_literal{val=Int},next=Next}, Count)
  when is_integer(Int), 0 =< Int, Int =< 255 ->
    count_bin_integer_segments(Next, Count + 1);
count_bin_integer_segments(_, Count) when Count > 0 ->
    {literal, Count};
count_bin_integer_segments(_, _Count) ->
    error.

%% Since 4 bytes in on 32-bits systems are bignums, we convert
%% anything more than 3 into 2 bytes lookup. The goal is to convert
%% any multi-clause segment into 2-byte lookups with a potential
%% 3 byte lookup at the end.
fix_count_without_variadic_segment(N) when N > 3 -> 2;
fix_count_without_variadic_segment(N) -> N.

%% If we have more than 16 clauses, then it is better
%% to branch multiple times than getting a large integer.
%% We also abort if we have nothing to squeeze.
squeeze_clauses(Clauses, Size, Count) when Count >= 16; Size == 1 -> Clauses;
squeeze_clauses(Clauses, Size, _Count) -> squeeze_clauses(Clauses, Size).

squeeze_clauses([#iclause{pats=[#k_bin_seg{seg=#k_literal{}} = BinSeg | Pats]} = Clause | Clauses], Size) ->
    [Clause#iclause{pats=[squeeze_segments(BinSeg, 0, 0, Size) | Pats]} |
     squeeze_clauses(Clauses, Size)];
squeeze_clauses([], _Size) ->
    [].

squeeze_segments(#k_bin_seg{size=Sz, seg=#k_literal{val=Val}=Lit} = BinSeg, Acc, Size, 1) ->
    BinSeg#k_bin_seg{size=Sz#k_literal{val=Size + 8}, seg=Lit#k_literal{val=(Acc bsl 8) bor Val}};
squeeze_segments(#k_bin_seg{seg=#k_literal{val=Val},next=Next}, Acc, Size, Count) ->
    squeeze_segments(Next, (Acc bsl 8) bor Val, Size + 8, Count - 1).

flat_reverse([Head | Tail], Acc) -> flat_reverse(Tail, flat_reverse_1(Head, Acc));
flat_reverse([], Acc) -> Acc.

flat_reverse_1([Head | Tail], Acc) -> flat_reverse_1(Tail, [Head | Acc]);
flat_reverse_1([], Acc) -> Acc.

%% build_guard([GuardClause]) -> GuardExpr.

build_guard([]) -> fail;
build_guard(Cs) -> #k_guard{clauses=Cs}.

%% build_select(Var, [ConClause]) -> SelectExpr.

build_select(V, [Tc|_]=Tcs) ->
    copy_anno(#k_select{var=V,types=Tcs}, Tc).

%% build_alt(First, Then) -> AltExpr.
%%  Build an alt, attempt some simple optimisation.

build_alt(fail, Then) -> Then;
build_alt(First,Then) -> build_alt_1st_no_fail(First, Then).

build_alt_1st_no_fail(First, fail) -> First;
build_alt_1st_no_fail(First, Then) ->
    copy_anno(#k_alt{first=First,then=Then}, First).

%% build_match(MatchExpr) -> Kexpr.
%%  Build a match expr if there is a match.

build_match(#k_alt{}=Km) -> copy_anno(#k_match{body=Km}, Km);
build_match(#k_select{}=Km) -> copy_anno(#k_match{body=Km}, Km);
build_match(#k_guard{}=Km) -> copy_anno(#k_match{body=Km}, Km);
build_match(Km) -> Km.

%% clause_arg(Clause) -> FirstArg.
%% clause_con(Clause) -> Constructor.
%% clause_val(Clause) -> Value.
%% is_var_clause(Clause) -> boolean().

clause_arg(#iclause{pats=[Arg|_]}) -> Arg.

clause_con(C) -> arg_con(clause_arg(C)).

clause_val(C) -> arg_val(clause_arg(C), C).

is_var_clause(C) -> clause_con(C) =:= k_var.

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
	#k_cons{} -> k_cons;
	#k_tuple{} -> k_tuple;
	#k_map{} -> k_map;
	#k_binary{} -> k_binary;
	#k_bin_end{} -> k_bin_end;
	#k_bin_seg{} -> k_bin_seg;
	#k_var{} -> k_var;
	#k_literal{val=[]} -> k_nil;
	#k_literal{val=Val} ->
            if
                is_atom(Val) -> k_atom;
                is_integer(Val) -> k_int;
                is_float(Val) -> k_float;
                true -> k_literal
            end
    end.

arg_val(Arg, C) ->
    case arg_arg(Arg) of
	#k_literal{val=Lit} -> Lit;
	#k_tuple{es=Es} -> length(Es);
	#k_bin_seg{size=S,unit=U,type=T,flags=Fs} ->
	    case S of
		#k_var{name=V} ->
		    #iclause{isub=Isub} = C,
		    {#k_var{name=get_vsub(V, Isub)},U,T,Fs};
		_ ->
		    {set_kanno(S, []),U,T,Fs}
	    end;
	#k_map{op=exact,es=Es} ->
            sort(fun(A,B) ->
			%% on the form K :: {'lit' | 'var', term()}
			%% lit < var as intended
			erts_internal:cmp_term(A,B) < 0
		end, [map_key_clean(Key) || #k_map_pair{key=Key} <- Es])
    end.

%% ubody_used_vars(Expr, State) -> [UsedVar]
%%  Return all used variables for the body sequence. Much more
%%  efficient than using ubody/3 if the body contains nested letrecs.
ubody_used_vars(Expr, St) ->
    {_,Used,_} = ubody(Expr, return, St#kern{funs=ignore}),
    Used.

%% ubody(Expr, Break, State) -> {Expr,[UsedVar],State}.
%%  Tag the body sequence with its used variables.  These bodies
%%  either end with a #k_break{}, or with #k_return{} or an expression
%%  which itself can return, #k_enter{}, #k_match{} ... .

ubody(#iset{vars=[],arg=#iletrec{}=Let,body=B0}, Br, St0) ->
    %% An iletrec{} should never be last.
    St = iletrec_funs(Let, St0),
    ubody(B0, Br, St);
ubody(#iset{anno=A,vars=Vs,arg=E0,body=B0}, Br, St0) ->
    {E1,Eu,St1} = uexpr(E0, {break,Vs}, St0),
    {B1,Bu,St2} = ubody(B0, Br, St1),
    Ns = lit_list_vars(Vs),
    Used = union(Eu, subtract(Bu, Ns)),		%Used external vars
    {#k_seq{anno=A,arg=E1,body=B1},Used,St2};
ubody(#ivalues{anno=A,args=As}, return, St) ->
    Au = lit_list_vars(As),
    {#k_return{anno=A,args=As},Au,St};
ubody(#ivalues{anno=A,args=As}, {break,_Vbs}, St) ->
    Au = lit_list_vars(As),
    {#k_break{anno=A,args=As},Au,St};
ubody(#k_goto{}=Goto, _Br, St) ->
    {Goto,[],St};
ubody(E, return, St0) ->
    %% Enterable expressions need no trailing return.
    case is_enter_expr(E) of
	true -> uexpr(E, return, St0);
	false ->
	    {Ea,Pa,St1} = force_atomic(E, St0),
	    ubody(pre_seq(Pa, #ivalues{args=[Ea]}), return, St1)
    end;
ubody(E, {break,[_]} = Break, St0) ->
    {Ea,Pa,St1} = force_atomic(E, St0),
    ubody(pre_seq(Pa, #ivalues{args=[Ea]}), Break, St1);
ubody(E, {break,Rs}=Break, St0) ->
    {Vs,St1} = new_vars(length(Rs), St0),
    Iset = #iset{vars=Vs,arg=E},
    PreSeq = pre_seq([Iset], #ivalues{args=Vs}),
    ubody(PreSeq, Break, St1).

iletrec_funs(#iletrec{defs=Fs}, St0) ->
    %% Use union of all free variables.
    %% First just work out free variables for all functions.
    Free = foldl(fun ({_,#ifun{vars=Vs,body=Fb0}}, Free0) ->
			 Fbu = ubody_used_vars(Fb0, St0),
			 Ns = lit_list_vars(Vs),
			 Free1 = subtract(Fbu, Ns),
			 union(Free1, Free0)
		 end, [], Fs),
    FreeVs = make_vars(Free),
    %% Add this free info to State.
    St1 = foldl(fun ({N,#ifun{vars=Vs}}, Lst) ->
			store_free(N, length(Vs), FreeVs, Lst)
		end, St0, Fs),
    iletrec_funs_gen(Fs, FreeVs, St1).

%% Now regenerate local functions to use free variable information.
iletrec_funs_gen(_, _, #kern{funs=ignore}=St) ->
    %% Optimization: The ultimate caller is only interested in the used variables,
    %% not the updated state. Makes a difference if there are nested letrecs.
    St;
iletrec_funs_gen(Fs, FreeVs, St) ->
    foldl(fun ({N,#ifun{anno=Fa,vars=Vs,body=Fb0}}, Lst0) ->
		  Arity0 = length(Vs),
		  {Fb1,_,Lst1} = ubody(Fb0, return, Lst0),
		  Arity = Arity0 + length(FreeVs),
                  Fun = make_fdef(Fa, N, Arity, Vs++FreeVs, Fb1),
		  Lst1#kern{funs=[Fun|Lst1#kern.funs]}
	  end, St, Fs).


%% is_enter_expr(Kexpr) -> boolean().
%%  Test whether Kexpr is "enterable", i.e. can handle return from
%%  within itself without extra #k_return{}.

is_enter_expr(#k_try{}) -> true;
is_enter_expr(#k_call{}) -> true;
is_enter_expr(#k_match{}) -> true;
is_enter_expr(#k_letrec_goto{}) -> true;
is_enter_expr(_) -> false.

%% uexpr(Expr, Break, State) -> {Expr,[UsedVar],State}.
%%  Calculate the used variables for an expression.
%%  Break = return | {break,[RetVar]}.

uexpr(#k_test{anno=A,op=Op,args=As}=Test, {break,Rs}, St) ->
    [] = Rs,					%Sanity check
    Used = union(op_vars(Op), lit_list_vars(As)),
    {Test#k_test{anno=A},Used,St};
uexpr(#iset{anno=A,vars=Vs,arg=E0,body=B0}, {break,_}=Br, St0) ->
    Ns = lit_list_vars(Vs),
    {E1,Eu,St1} = uexpr(E0, {break,Vs}, St0),
    {B1,Bu,St2} = uexpr(B0, Br, St1),
    Used = union(Eu, subtract(Bu, Ns)),
    {#k_seq{anno=A,arg=E1,body=B1},Used,St2};
uexpr(#k_call{anno=A,op=#k_local{name=F,arity=Ar}=Op,args=As0}=Call, Br, St) ->
    Free = get_free(F, Ar, St),
    As1 = As0 ++ Free,				%Add free variables LAST!
    Used = lit_list_vars(As1),
    {case Br of
	 {break,Rs} ->
	     Call#k_call{anno=A,
			 op=Op#k_local{arity=Ar + length(Free)},
			 args=As1,ret=Rs};
	 return ->
	     #k_enter{anno=A,
		      op=Op#k_local{arity=Ar + length(Free)},
		      args=As1}
     end,Used,St};
uexpr(#k_call{anno=A,op=Op,args=As}=Call, {break,Rs}, St) ->
    Used = union(op_vars(Op), lit_list_vars(As)),
    {Call#k_call{anno=A,ret=Rs},Used,St};
uexpr(#k_call{anno=A,op=Op,args=As}, return, St) ->
    Used = union(op_vars(Op), lit_list_vars(As)),
    {#k_enter{anno=A,op=Op,args=As},Used,St};
uexpr(#k_bif{anno=A,op=Op,args=As}=Bif, {break,Rs}, St0) ->
    Used = union(op_vars(Op), lit_list_vars(As)),
    {Brs,St1} = bif_returns(Op, Rs, St0),
    {Bif#k_bif{anno=A,ret=Brs},Used,St1};
uexpr(#k_match{anno=A,body=B0}, Br, St0) ->
    Rs = break_rets(Br),
    {B1,Bu,St1} = umatch(B0, Br, St0),
    {#k_match{anno=A,body=B1,ret=Rs},Bu,St1};
uexpr(#k_try{anno=A,arg=A0,vars=Vs,body=B0,evars=Evs,handler=H0},
      {break,Rs0}=Br, St0) ->
    case {Vs,B0,H0,Rs0} of
	{[#k_var{name=X}],#k_var{name=X},#k_literal{},[]} ->
            %% This is a simple try/catch whose return value is
            %% ignored:
            %%
            %%   try E of V -> V when _:_:_ -> ignored_literal end, ...
            %%
            %% This is most probably a try/catch in a guard. To
            %% correctly handle the #k_test{} that ends the body of
            %% the guard, we MUST pass an empty list of break
            %% variables when processing the body.
	    {A1,Bu,St} = ubody(A0, {break,[]}, St0),
	    {#k_try{anno=A,arg=A1,vars=[],body=#k_break{},
                    evars=[],handler=#k_break{},ret=Rs0},
	     Bu,St};
	{_,_,_,_} ->
            %% The general try/catch (in a guard or in body).
	    {Avs,St1} = new_vars(length(Vs), St0),
	    {A1,Au,St2} = ubody(A0, {break,Avs}, St1),
	    {B1,Bu,St3} = ubody(B0, Br, St2),
	    {H1,Hu,St4} = ubody(H0, Br, St3),
	    Used = union([Au,subtract(Bu, lit_list_vars(Vs)),
			  subtract(Hu, lit_list_vars(Evs))]),
	    {#k_try{anno=A,arg=A1,vars=Vs,body=B1,evars=Evs,handler=H1,ret=Rs0},
	     Used,St4}
    end;
uexpr(#k_try{anno=A,arg=A0,vars=Vs,body=B0,evars=Evs,handler=H0},
      return, St0) ->
    {Avs,St1} = new_vars(length(Vs), St0),	%Need dummy names here
    {A1,Au,St2} = ubody(A0, {break,Avs}, St1),	%Must break to clean up here!
    {B1,Bu,St3} = ubody(B0, return, St2),
    {H1,Hu,St4} = ubody(H0, return, St3),
    Used = union([Au,subtract(Bu, lit_list_vars(Vs)),
		  subtract(Hu, lit_list_vars(Evs))]),
    {#k_try_enter{anno=A,arg=A1,vars=Vs,body=B1,evars=Evs,handler=H1},
     Used,St4};
uexpr(#k_catch{anno=A,body=B0}, {break,Rs0}, St0) ->
    {Rb,St1} = new_var(St0),
    {B1,Bu,St2} = ubody(B0, {break,[Rb]}, St1),
    %% Guarantee ONE return variable.
    {Ns,St3} = new_vars(1 - length(Rs0), St2),
    Rs1 = Rs0 ++ Ns,
    {#k_catch{anno=A,body=B1,ret=Rs1},Bu,St3};
uexpr(#ifun{anno=A,vars=Vs,body=B0}, {break,Rs}, St0) ->
    {B1,Bu,St1} = ubody(B0, return, St0),	%Return out of new function
    Ns = lit_list_vars(Vs),
    Free = subtract(Bu, Ns),			%Free variables in fun
    Fvs = make_vars(Free),
    Arity = length(Vs) + length(Free),
    {Fname,St} =
        case keyfind(id, 1, A) of 
	    {id,{_,_,Fname0}} ->
		{Fname0,St1};
	    false ->
		%% No id annotation. Must invent a fun name.
		new_fun_name(St1)
	end,
    Fun = make_fdef(A, Fname, Arity, Vs++Fvs, B1),
    Local = #k_local{name=Fname,arity=Arity},
    {#k_bif{anno=A,
	    op=#k_internal{name=make_fun,arity=length(Free)+2},
	    args=[Local|Fvs],
 	    ret=Rs},
     Free,add_local_function(Fun, St)};
uexpr(#k_letrec_goto{anno=A,first=F0,then=T0}=MatchAlt, Br, St0) ->
    Rs = break_rets(Br),
    {F1,Fu,St1} = ubody(F0, Br, St0),
    {T1,Tu,St2} = ubody(T0, Br, St1),
    Used = union(Fu, Tu),
    {MatchAlt#k_letrec_goto{anno=A,first=F1,then=T1,ret=Rs},Used,St2};
uexpr(Lit, {break,Rs0}, St0) ->
    %% Transform literals to puts here.
    %%ok = io:fwrite("uexpr ~w:~p~n", [?LINE,Lit]),
    Used = lit_vars(Lit),
    {Rs,St1} = ensure_return_vars(Rs0, St0),
    {#k_put{anno=get_kanno(Lit),arg=Lit,ret=Rs},Used,St1}.

add_local_function(_, #kern{funs=ignore}=St) ->
    St;
add_local_function(#k_fdef{func=Name,arity=Arity}=F, #kern{funs=Funs}=St) ->
    case is_defined(Name, Arity, Funs) of
        false ->
            St#kern{funs=[F|Funs]};
        true ->
            St
    end.

is_defined(Name, Arity, [#k_fdef{func=Name,arity=Arity}|_]) ->
    true;
is_defined(Name, Arity, [#k_fdef{}|T]) ->
    is_defined(Name, Arity, T);
is_defined(_, _, []) -> false.

%% Make a #k_fdef{}, making sure that the body is always a #k_match{}.
make_fdef(Anno, Name, Arity, Vs, #k_match{}=Body) ->
    #k_fdef{anno=Anno,func=Name,arity=Arity,vars=Vs,body=Body};
make_fdef(Anno, Name, Arity, Vs, Body) ->
    Ka = get_kanno(Body),
    Match = #k_match{anno=Ka,body=Body,ret=[]},
    #k_fdef{anno=Anno,func=Name,arity=Arity,vars=Vs,body=Match}.

%% get_free(Name, Arity, State) -> [Free].
%% store_free(Name, Arity, [Free], State) -> State.

get_free(F, A, #kern{free=FreeMap}) ->
    Key = {F,A},
    case FreeMap of
	#{Key:=Val} -> Val;
	_ -> []
    end.

store_free(F, A, Free, #kern{free=FreeMap0}=St) ->
    Key = {F,A},
    FreeMap = FreeMap0#{Key=>Free},
    St#kern{free=FreeMap}.

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

%% ensure_return_vars([Ret], State) -> {[Ret],State}.

ensure_return_vars([], St) -> new_vars(1, St);
ensure_return_vars([_]=Rs, St) -> {Rs,St}.

%% umatch(Match, Break, State) -> {Match,[UsedVar],State}.
%%  Calculate the used variables for a match expression.

umatch(#k_alt{anno=A,first=F0,then=T0}, Br, St0) ->
    {F1,Fu,St1} = umatch(F0, Br, St0),
    {T1,Tu,St2} = umatch(T0, Br, St1),
    Used = union(Fu, Tu),
    {#k_alt{anno=A,first=F1,then=T1},Used,St2};
umatch(#k_select{anno=A,var=V,types=Ts0}, Br, St0) ->
    {Ts1,Tus,St1} = umatch_list(Ts0, Br, St0),
    Used = add_element(V#k_var.name, Tus),
    {#k_select{anno=A,var=V,types=Ts1},Used,St1};
umatch(#k_type_clause{anno=A,type=T,values=Vs0}, Br, St0) ->
    {Vs1,Vus,St1} = umatch_list(Vs0, Br, St0),
    {#k_type_clause{anno=A,type=T,values=Vs1},Vus,St1};
umatch(#k_val_clause{anno=A,val=P0,body=B0}, Br, St0) ->
    {U0,Ps} = pat_vars(P0),
    {B1,Bu,St1} = umatch(B0, Br, St0),
    P = pat_anno_unused(P0, Bu, Ps),
    Used = union(U0, subtract(Bu, Ps)),
    {#k_val_clause{anno=A,val=P,body=B1},Used,St1};
umatch(#k_guard{anno=A,clauses=Gs0}, Br, St0) ->
    {Gs1,Gus,St1} = umatch_list(Gs0, Br, St0),
    {#k_guard{anno=A,clauses=Gs1},Gus,St1};
umatch(#k_guard_clause{anno=A,guard=G0,body=B0}, Br, St0) ->
    {G1,Gu,St1} = uexpr(G0, {break,[]}, St0),
    {B1,Bu,St2} = umatch(B0, Br, St1),
    Used = union(Gu, Bu),
    {#k_guard_clause{anno=A,guard=G1,body=B1},Used,St2};
umatch(B0, Br, St0) -> ubody(B0, Br, St0).

umatch_list(Ms0, Br, St) ->
    foldr(fun (M0, {Ms1,Us,Sta}) ->
		  {M1,Mu,Stb} = umatch(M0, Br, Sta),
		  {[M1|Ms1],union(Mu, Us),Stb}
	  end, {[],[],St}, Ms0).

pat_anno_unused(#k_tuple{es=Es0}=P, Used0, Ps) ->
    %% Not extracting unused tuple elements is an optimization for
    %% compile time and memory use during compilation. It is probably
    %% worthwhile because it is common to extract only a few elements
    %% from a huge record.
    Used = intersection(Used0, Ps),
    Es = [case member(V, Used) of
              true -> Var;
              false -> set_kanno(Var, [unused|get_kanno(Var)])
          end || #k_var{name=V}=Var <- Es0],
    P#k_tuple{es=Es};
pat_anno_unused(P, _Used, _Ps) -> P.

%% op_vars(Op) -> [VarName].

op_vars(#k_remote{mod=Mod,name=Name}) ->
    ordsets:from_list([V || #k_var{name=V} <- [Mod,Name]]);
op_vars(#k_internal{}) -> [];
op_vars(Atomic) -> lit_vars(Atomic).

%% lit_vars(Literal) -> [VarName].
%%  Return the variables in a literal.

lit_vars(#k_var{name=N}) -> [N];
%%lit_vars(#k_char{}) -> [];
lit_vars(#k_cons{hd=H,tl=T}) ->
    union(lit_vars(H), lit_vars(T));
lit_vars(#k_map{var=Var,es=Es}) ->
    lit_list_vars([Var|Es]);
lit_vars(#k_map_pair{key=K,val=V}) ->
    union(lit_vars(K), lit_vars(V));
lit_vars(#k_binary{segs=V}) -> lit_vars(V);
lit_vars(#k_bin_end{}) -> [];
lit_vars(#k_bin_seg{size=Size,seg=S,next=N}) ->
    union(lit_vars(Size), union(lit_vars(S), lit_vars(N)));
lit_vars(#k_tuple{es=Es}) ->
    lit_list_vars(Es);
lit_vars(#k_literal{}) -> [].

lit_list_vars(Ps) ->
    foldl(fun (P, Vs) -> union(lit_vars(P), Vs) end, [], Ps).

%% pat_vars(Pattern) -> {[UsedVarName],[NewVarName]}.
%%  Return variables in a pattern.  All variables are new variables
%%  except those in the size field of binary segments and the key
%%  field in map_pairs.

pat_vars(#k_var{name=N}) -> {[],[N]};
%%pat_vars(#k_char{}) -> {[],[]};
pat_vars(#k_literal{}) -> {[],[]};
pat_vars(#k_cons{hd=H,tl=T}) ->
    pat_list_vars([H,T]);
pat_vars(#k_binary{segs=V}) ->
    pat_vars(V);
pat_vars(#k_bin_seg{size=Size,seg=S,next=N}) ->
    {U1,New} = pat_list_vars([S,N]),
    {[],U2} = pat_vars(Size),
    {union(U1, U2),New};
pat_vars(#k_bin_int{size=Size,next=N}) ->
    {[],New} = pat_vars(N),
    {[],U} = pat_vars(Size),
    {U,New};
pat_vars(#k_bin_end{}) -> {[],[]};
pat_vars(#k_tuple{es=Es}) ->
    pat_list_vars(Es);
pat_vars(#k_map{es=Es}) ->
    pat_list_vars(Es);
pat_vars(#k_map_pair{key=K,val=V}) ->
    {U1,New} = pat_vars(V),
    {[], U2} = pat_vars(K),
    {union(U1,U2),New}.

pat_list_vars(Ps) ->
    foldl(fun (P, {Used0,New0}) ->
		  {Used,New} = pat_vars(P),
		  {union(Used0, Used),union(New0, New)} end,
	  {[],[]}, Ps).

%% List of integers in interval [N,M]. Empty list if N > M.

integers(N, M) when N =< M ->
    [N|integers(N + 1, M)];
integers(_, _) -> [].

%%%
%%% Handling of errors and warnings.
%%%

-type error() :: 'bad_call' | 'nomatch_shadow' | {'nomatch_shadow', integer()}.

-spec format_error(error()) -> string().

format_error({nomatch_shadow,Line}) ->
    M = io_lib:format("this clause cannot match because a previous clause at line ~p "
		      "always matches", [Line]),
    flatten(M);
format_error(nomatch_shadow) ->
    "this clause cannot match because a previous clause always matches";
format_error(bad_call) ->
    "invalid module and/or function name; this call will always fail";
format_error(bad_segment_size) ->
    "binary construction will fail because of a type mismatch".

add_warning(none, Term, Anno, #kern{ws=Ws}=St) ->
    File = get_file(Anno),
    St#kern{ws=[{File,[{none,?MODULE,Term}]}|Ws]};
add_warning(Line, Term, Anno, #kern{ws=Ws}=St) ->
    File = get_file(Anno),
    St#kern{ws=[{File,[{Line,?MODULE,Term}]}|Ws]}.

is_compiler_generated(Ke) ->
    Anno = get_kanno(Ke),
    member(compiler_generated, Anno).

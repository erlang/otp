%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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
%% Purpose: Transform Core Erlang to SSA code.

%%
%% The translation from Core Erlang to SSA code is done in three
%% passes:
%%
%% 1. Basic translation, translate variable/function names, flatten
%% completely, pattern matching compilation.  To ensure unique
%% variable names we use a variable substitution table and keep the
%% set of all defined variables.  The nested scoping of Core Erlang
%% means that we must also nest the substitution tables, but the
%% defined set must be passed through to match the flat structure of
%% SSA code and to make sure variables with the same name from
%% different scopes get different substitutions.
%%
%% We also use these substitutions to handle the variable renaming
%% necessary in pattern matching compilation.
%%
%% 2. Fun lifting (lambda lifting), variable usage annotation and
%% last-call handling.
%%
%% 3. Translation to SSA code.
%%
%% Historical note
%%
%% The translation from Core Erlang to SSA code used to be done
%% by two separate compiler passes:
%%
%% 1. Core Erlang was translated to Kernel Erlang by the `v3_kernel`
%% pass in two sub passes.
%%
%% 2. Kernel Erlang was translated to SSA code by the `beam_kernel_to_ssa`
%% pass.
%%
%% For the history of Kernel Erlang and Core Erlang, see the following
%% blog post:
%%
%%     https://www.erlang.org/blog/beam-compiler-history
%%

-module(beam_core_to_ssa).
-moduledoc false.

-export([module/2,format_error/1]).

-import(lists, [all/2,append/1,droplast/1,
                flatten/1,foldl/3,foldr/3,
                map/2,mapfoldl/3,member/2,
                keyfind/3,keysort/2,last/1,
                partition/2,reverse/1,reverse/2,
                sort/1,sort/2,splitwith/2]).
-import(ordsets, [add_element/2,del_element/2,intersection/2,
                  subtract/2,union/2,union/1]).

-include("core_parse.hrl").
-include("beam_ssa.hrl").

%% Matches collapse max segment in v3_core.
-define(EXPAND_MAX_SIZE_SEGMENT, 1024).

%% Internal records created by the first pass and eliminated in the
%% second pass.

-record(ivalues, {args}).
-record(iset, {vars,arg}).
-record(ilet, {vars,arg,body}).
-record(iletrec, {defs}).
-record(ialias, {vars,pat}).

-record(ifun, {anno=[],vars,body}).
-record(iclause, {anno=[],sub,pats,guard,body}).

%% The following records are used to represent complex terms used for
%% matching. (Construction of those term types is translated directly
%% to SSA instructions.)

-record(cg_tuple, {es,keep=ordsets:new()}).
-record(cg_map, {var=#b_literal{val=#{}},op,es}).
-record(cg_map_pair, {key,val}).
-record(cg_cons, {hd,tl}).
-record(cg_binary, {segs}).
-record(cg_bin_seg, {size,unit,type,flags,seg,next}).
-record(cg_bin_int, {size,unit,flags,val,next}).
-record(cg_bin_end, {}).

%% Other internal records.

-record(cg_seq, {arg,body}).
-record(cg_call, {anno=[],op,args,ret=[]}).
-record(cg_internal, {anno=[],op,args,ret=[]}).

-record(cg_try, {arg,vars,body,evars,handler,ret=[]}).
-record(cg_catch, {body,ret=[]}).

-record(cg_letrec_goto, {label,vars=[],first,then,ret=[]}).
-record(cg_goto, {label,args=[]}).

-record(cg_opaque, {val}).

-record(cg_match, {body,ret=[]}).
-record(cg_alt, {anno=[],first,then}).
-record(cg_select, {anno=[],var,types}).
-record(cg_type_clause, {anno=[],type,values}).
-record(cg_val_clause, {anno=[],val,body}).
-record(cg_guard, {anno=[],clauses}).
-record(cg_guard_clause, {guard,body}).
-record(cg_test, {op,args}).

-record(cg_break, {args=[] :: [beam_ssa:value()],
                   phi :: label() | 'undefined'}).
-record(cg_phi, {vars :: [beam_ssa:b_var()]}).
-record(cg_unreachable, {}).
-record(cg_succeeded, {set :: beam_ssa:b_set()}).

get_anno(#iclause{anno=Anno}) -> Anno;
get_anno(#cg_alt{anno=Anno}) -> Anno;
get_anno(#cg_guard{anno=Anno}) -> Anno;
get_anno(#cg_select{anno=Anno}) -> Anno.

-type warning() :: {'failed' | 'nomatch', term()}.

%% State record for the first two passes (formerly `v3_kernel`).
-record(kern, {module :: atom(),       %Current module
               func,                   %Current host function
               fargs=[] :: [#b_var{}], %Arguments for current function
               vcount=0,               %Variable counter
               fcount=0,               %Fun counter
               ds=sets:new() :: sets:set(), %Defined variables
               funs=[],                         %Fun functions
               free=#{},                        %Free variables
               ws=[]   :: [warning()],          %Warnings.
               no_min_max_bifs=false :: boolean(),
               beam_debug_info=false :: boolean()
              }).

-spec module(cerl:c_module(), [compile:option()]) ->
          {'ok', #b_module{}, [warning()]}.

module(#c_module{name=#c_literal{val=Mod},exports=Es,attrs=As,defs=Fs}, Options) ->
    Kas = attributes(As),
    Kes = map(fun (#c_var{name={_,_}=Fname}) -> Fname end, Es),
    NoMinMaxBifs = proplists:get_bool(no_min_max_bifs, Options),
    DebugInfo = proplists:get_bool(beam_debug_info, Options),
    St0 = #kern{module=Mod,
                no_min_max_bifs=NoMinMaxBifs,
                beam_debug_info=DebugInfo},
    {Kfs,St} = mapfoldl(fun function/2, St0, Fs),
    Body = Kfs ++ St#kern.funs,
    Code = #b_module{name=Mod,exports=Kes,attributes=Kas,body=Body},
    {ok,Code,sort(St#kern.ws)}.

-spec format_error(warning()) -> string() | binary().

format_error({nomatch,{shadow,Line}}) ->
    S = ~"this clause cannot match because a previous clause at line ~p matches the same pattern as this clause",
    M = io_lib:format(S, [Line]),
    flatten(M);
format_error({nomatch,shadow}) ->
    <<"this clause cannot match because a previous clause always matches">>;
format_error({failed,bad_call}) ->
    <<"invalid module and/or function name; this call will always fail">>;
format_error({failed,bad_segment_size}) ->
    <<"binary construction will fail because the size of a segment is invalid">>.

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

function({#c_var{anno=Anno,name={F,Arity}=FA},Body0}, St0) ->
    try
        %% Find a suitable starting value for the counter
        %% used for generating labels and variable names.
        Count0 = cerl_trees:next_free_variable_name(Body0),
        Count = max(?EXCEPTION_BLOCK + 1, Count0),

        %% If this module is being compiled with `beam_debug_info`,
        %% insert a special `debug_line` instruction as the
        %% first instruction in this function.
        Body = handle_debug_line(Anno, Body0),

        %% First pass: Basic translation.
        St1 = St0#kern{func=FA,vcount=Count,fcount=0},
        {#ifun{anno=Ab,vars=Kvs,body=B0},[],St2} = expr(Body, new_sub(), St1),
        St3 = St2#kern{ds=sets:new([{version,2}])},

        %% Second pass: Variable usage and lambda lifting.
        {B1,_,St4} = ubody(B0, return, St3),
        St5 = St4#kern{free=#{}},

        %% Third pass: Translation to SSA code.
        FDef = make_ssa_function(Ab, F, Kvs, B1, St5),

        {FDef,St5}
    catch
        Class:Error:Stack ->
            io:fwrite("Function: ~w/~w\n", [F,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

handle_debug_line([{debug_line,{Location,Index}}], #c_fun{body=Body}=Fun) ->
    DbgLine = #c_primop{anno=Location,
                        name=#c_literal{val=debug_line},
                        args=[#c_literal{val=Index}]},
    Seq = #c_seq{arg=DbgLine,body=Body},
    Fun#c_fun{body=Seq};
handle_debug_line(_, Fun) -> Fun.

%%%
%%% First pass: Basic translation.
%%%

%% body(Cexpr, Sub, State) -> {Kexpr,[PreKepxr],State}.
%%  Do the main sequence of a body.  A body ends in an atomic value or
%%  values.  Must check if vector first so do expr.

body(#c_values{es=Ces}, Sub, St0) ->
    %% Do this here even if only in bodies.
    {Kes,Pe,St1} = atomic_list(Ces, Sub, St0),
    {#ivalues{args=Kes},Pe,St1};
body(Ce, Sub, St0) ->
    expr(Ce, Sub, St0).

%% guard(Cexpr, Sub, State) -> {Kexpr,State}.
%%  We handle guards almost as bodies. The only special thing we
%%  must do is to make the final Kexpr a #cg_test{}.

guard(G0, Sub, St0) ->
    {Ge0,Pre,St1} = expr(G0, Sub, St0),
    {Ge,St} = gexpr_test(Ge0, St1),
    {pre_seq(Pre, Ge),St}.

%% gexpr_test(Kexpr, State) -> {Kexpr,State}.
%%  Builds the final boolean test from the last Kexpr in a guard test.
%%  Must enter try blocks and isets and find the last Kexpr in them.
%%  This must end in a recognised BEAM test!

gexpr_test(#b_set{op={bif,F},args=Args}, St) ->
    Ar = length(Args),
    true = erl_internal:new_type_test(F, Ar) orelse
        erl_internal:comp_op(F, Ar),            %Assertion
    {#cg_test{op=F,args=Args},St};
gexpr_test(#cg_try{arg=B0,vars=[#b_var{name=X}],body=#b_var{name=X},
                   handler=#b_literal{val=false}}=Try, St0) ->
    {B,St} = gexpr_test(B0, St0),
    {Try#cg_try{vars=[],arg=B,body=#cg_break{},
                evars=[],handler=#cg_break{}},St};
gexpr_test(#ilet{body=B0}=Iset, St0) ->
    {B1,St1} = gexpr_test(B0, St0),
    {Iset#ilet{body=B1},St1};
gexpr_test(Ke, St) -> gexpr_test_add(Ke, St).   %Add equality test

gexpr_test_add(Ke, St0) ->
    {Ae,Ap,St1} = force_atomic(Ke, St0),
    {pre_seq(Ap, #cg_test{op='=:=',args=[Ae,#b_literal{val='true'}]}),St1}.

%% expr(Cexpr, Sub, State) -> {Kexpr,[PreKexpr],State}.
%%  Convert a Core expression, flattening it at the same time.

expr(#c_var{name={Name0,Arity}}, Sub, St) ->
    Name = get_fsub(Name0, Arity, Sub),
    {#b_local{name=#b_literal{val=Name},arity=Arity},[],St};
expr(#c_var{name=V}, Sub, St) ->
    {#b_var{name=get_vsub(V, Sub)},[],St};
expr(#c_literal{val=V}, _Sub, St) ->
    {#b_literal{val=V},[],St};
expr(#c_cons{hd=Ch,tl=Ct}, Sub, St0) ->
    %% Do cons in two steps, first the expressions left to right, then
    %% any remaining literals right to left.
    {Kh0,Hp0,St1} = expr(Ch, Sub, St0),
    {Kt0,Tp0,St2} = expr(Ct, Sub, St1),
    {Kt1,Tp1,St3} = force_atomic(Kt0, St2),
    {Kh1,Hp1,St4} = force_atomic(Kh0, St3),
    {#b_set{op=put_list,args=[Kh1,Kt1]},Hp0 ++ Tp0 ++ Tp1 ++ Hp1,St4};
expr(#c_tuple{es=Ces}, Sub, St0) ->
    {Kes,Ep,St1} = atomic_list(Ces, Sub, St0),
    {#b_set{op=put_tuple,args=Kes},Ep,St1};
expr(#c_map{anno=A,arg=Var,es=Ces}, Sub, St0) ->
    expr_map(A, Var, Ces, Sub, St0);
expr(#c_binary{anno=A,segments=Cv}, Sub, St0) ->
    try
        expr_binary(A, Cv, Sub, St0)
    catch
        throw:{bad_segment_size,Anno} ->
            St1 = add_warning(Anno, {failed,bad_segment_size}, St0),
            Erl = #c_literal{val=erlang},
            Name = #c_literal{val=error},
            Args = [#c_literal{val=badarg}],
            Error = #c_call{anno=A,module=Erl,name=Name,args=Args},
            expr(Error, Sub, St1)
    end;
expr(#c_fun{anno=A,vars=Cvs}=Fun, Sub0, #kern{fargs=OldFargs}=St0) ->
    FilteredAnno = [Item || {debug_line,_}=Item <- A],
    #c_fun{body=Cb} = handle_debug_line(FilteredAnno, Fun),
    {Kvs,Sub1,St1} = pattern_list(Cvs, Sub0, St0),
    {Kb,Pb,St2} = body(Cb, Sub1, St1#kern{fargs=Kvs}),
    {#ifun{anno=A,vars=Kvs,body=pre_seq(Pb, Kb)},[],St2#kern{fargs=OldFargs}};
expr(#c_seq{arg=Ca,body=Cb}, Sub, St0) ->
    {Ka,Pa,St1} = body(Ca, Sub, St0),
    {Kb,Pb,St2} = body(Cb, Sub, St1),
    {Kb,Pa ++ [Ka] ++ Pb,St2};
expr(#c_let{vars=Cvs,arg=Ca,body=Cb}, Sub0, St0) ->
    {Ka,Pa,St1} = body(Ca, Sub0, St0),
    {Kps,Sub1,St2} = pattern_list(Cvs, Sub0, St1),
    %% Break known multiple values into separate sets.
    Sets = case Ka of
               #ivalues{args=Kas} ->
                   [#iset{vars=[V],arg=Val} || V <- Kps && Val <- Kas];
               _Other ->
                   [#iset{vars=Kps,arg=Ka}]
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
    {Ka,Pa,St1} = body(Ca, Sub, St0),           %This is a body!
    {Kvs,Pv,St2} = match_vars(Ka, St1),         %Must have variables here!
    {Km,St3} = kmatch(Kvs, Ccs, Sub, St2),
    Match = flatten_seq(build_match(Km)),
    {last(Match),Pa ++ Pv ++ droplast(Match),St3};
expr(#c_apply{anno=A,op=Cop,args=Cargs}, Sub, St) ->
    c_apply(A, Cop, Cargs, Sub, St);
expr(#c_call{anno=A,module=M0,name=F0,args=Cargs}, Sub, St0) ->
    case call_type(M0, F0, Cargs, St0) of
        bif ->
            #c_literal{val=Name} = F0,
            {Args,Ap,St} = atomic_list(Cargs, Sub, St0),
            Set = #b_set{anno=line_anno(A),op={bif,Name},args=Args},
            case erl_bifs:is_safe(erlang, Name, length(Args)) of
                true -> {Set,Ap,St};
                false -> {#cg_succeeded{set=Set},Ap,St}
            end;
        call ->
            {[M,F|Args],Ap,St} = atomic_list([M0,F0|Cargs], Sub, St0),
            Remote = #b_remote{mod=M,name=F,arity=length(Args)},
            {#cg_call{anno=A,op=Remote,args=Args},Ap,St};
        is_record ->
            {Args,Ap,St} = atomic_list(Cargs, Sub, St0),
            {#cg_internal{anno=internal_anno(A),op=is_record,args=Args},Ap,St};
        error ->
            %% Invalid call (e.g. M:42/3). Issue a warning, and let
            %% the generated code call apply/3.
            St = add_warning(A, {failed,bad_call}, St0),
            Call = #c_call{anno=A,
                           module=#c_literal{val=erlang},
                           name=#c_literal{val=apply},
                           args=[M0,F0,cerl:make_list(Cargs)]},
            expr(Call, Sub, St)
    end;
expr(#c_primop{anno=A0,name=#c_literal{val=debug_line},
               args=Cargs}, Sub, St0) ->
    {Args,Ap,St1} = atomic_list(Cargs, Sub, St0),
    #b_set{anno=A1} = I0 = primop(debug_line, A0, Args),
    {_,Alias} = Sub,
    A = A1#{alias => Alias},
    I = I0#b_set{anno=A},
    St2 = St1#kern{beam_debug_info=true},
    {I,Ap,St2};
expr(#c_primop{anno=A,name=#c_literal{val=match_fail},args=[Arg]}, Sub, St) ->
    translate_match_fail(Arg, Sub, A, St);
expr(#c_primop{anno=A,name=#c_literal{val=Op},args=Cargs}, Sub, St0) ->
    {Args,Ap,St1} = atomic_list(Cargs, Sub, St0),
    {primop(Op, A, Args),Ap,St1};
expr(#c_try{arg=Ca,vars=Cvs,body=Cb,evars=Evs,handler=Ch}, Sub0, St0) ->
    {Ka,Pa,St1} = body(Ca, Sub0, St0),
    {Kcvs,Sub1,St2} = pattern_list(Cvs, Sub0, St1),
    {Kb,Pb,St3} = body(Cb, Sub1, St2),
    {Kevs,Sub2,St4} = pattern_list(Evs, Sub0, St3),
    {Kh,Ph,St5} = body(Ch, Sub2, St4),
    {#cg_try{arg=pre_seq(Pa, Ka),
             vars=Kcvs,body=pre_seq(Pb, Kb),
             evars=Kevs,handler=pre_seq(Ph, Kh)},[],St5};
expr(#c_catch{body=Cb}, Sub, St0) ->
    {Kb,Pb,St1} = body(Cb, Sub, St0),
    {#cg_catch{body=pre_seq(Pb, Kb)},[],St1};
expr(#c_opaque{val=Check}, _Sub, St) ->
    {#cg_opaque{val=Check},[],St}.

primop(raise, Anno, Args) ->
    primop_succeeded(resume, Anno, Args);
primop(raw_raise, Anno, Args) ->
    primop_succeeded(raw_raise, Anno, Args);
primop(Op, Anno, Args) when Op =:= recv_peek_message;
                            Op =:= recv_wait_timeout ->
    #cg_internal{anno=internal_anno(Anno),op=Op,args=Args};
primop(Op, Anno, Args) ->
    #b_set{anno=internal_anno(Anno),op=Op,args=Args}.

primop_succeeded(Op, Anno0, Args) ->
    Anno = internal_anno(Anno0),
    Set = #b_set{anno=Anno,op=Op,args=Args},
    #cg_succeeded{set=Set}.

%% Implement letrec in the traditional way as a local
%% function for each definition in the letrec.

letrec_local_function(A, Cfs, Cb, Sub0, St0) ->
    %% Make new function names and store substitution.
    {Fs0,{Sub1,St1}} =
        mapfoldl(fun ({#c_var{name={F,Ar}},#c_fun{anno=Anno0}=B0}, {Sub,S0}) ->
                         {N,St1} = new_fun_name(atom_to_list(F)
                                                ++ "/" ++
                                                    integer_to_list(Ar),
                                                S0),
                         Anno = [{letrec_name,N} | [Dbg || {debug_line,_}=Dbg <- Anno0]],
                         B = B0#c_fun{anno=Anno},
                         {{N,B},{set_fsub(F, Ar, N, Sub),St1}}
                 end, {Sub0,St0}, Cfs),
    %% Run translation on functions and body.
    {Fs1,St2} = mapfoldl(fun ({N,Fd0}, S1) ->
                                 {Fd1,[],St2} = expr(Fd0, Sub1, S1),
                                 Fd = Fd1#ifun{anno=A},
                                 {{N,Fd},St2}
                         end, St1, Fs0),
    {Kb,Pb,St3} = body(Cb, Sub1, St2),
    {Kb,[#iletrec{defs=Fs1}|Pb],St3}.

%% Implement letrec with the single definition as a label and each
%% apply of it as a goto.

letrec_goto([{#c_var{name={Name,Arity}},Cfail}], Cb, Sub0, St0) ->
    {Label,St1} = new_var_name(St0),
    #c_fun{vars=FunVars,body=FunBody} = Cfail,
    Sub1 = set_fsub(Name, Arity, {letrec_goto,Label}, Sub0),
    {Kvars,{FunSub,St2}} =
        mapfoldl(fun(#c_var{name=V}, {SubInt,StInt0}) ->
                         {New,StInt1} = new_var_name(StInt0),
                         {#b_var{name=New},
                          {set_vsub(V, New, SubInt),
                           StInt1#kern{ds=sets:add_element(New, StInt1#kern.ds)}}}
                 end, {Sub1,St1}, FunVars),
    {Kb,Pb,St3} = body(Cb, Sub1, St2),
    {Kfail,Fb,St4} = body(FunBody, FunSub, St3),
    case {Kb,Kfail,Fb} of
        {#cg_goto{label=Label},#cg_goto{}=InnerGoto,[]} ->
            {InnerGoto,Pb,St4};
        {_,_,_} ->
            Alt = #cg_letrec_goto{label=Label,vars=Kvars,
                                  first=Kb,then=pre_seq(Fb, Kfail)},
            {Alt,Pb,St4}
    end.

%% translate_match_fail(Arg, Sub, Anno, St) -> {Kexpr,[PreKexpr],State}.
%%  Translate match_fail primops, paying extra attention to `function_clause`
%%  errors that may have been inlined from other functions.

translate_match_fail(Arg, Sub, Anno, St0) ->
    {Cargs,ExtraAnno,St1} =
        case {cerl:data_type(Arg),cerl:data_es(Arg)} of
            {tuple,[#c_literal{val=function_clause}|_]=As} ->
                translate_fc_args(As, Sub, Anno, St0);
            {tuple,[#c_literal{}|_]=As} ->
                {As,[],St0};
            {{atomic,Reason}, []} ->
                {[#c_literal{val=Reason}],[],St0}
        end,
    {Args,Ap,St} = atomic_list(Cargs, Sub, St1),
    SsaAnno = internal_anno(ExtraAnno ++ Anno),
    Set = #b_set{anno=SsaAnno,op=match_fail,args=Args},
    {#cg_succeeded{set=Set},Ap,St}.

translate_fc_args(As, Sub, Anno, #kern{fargs=Fargs}=St0) ->
    {ExtraAnno, St} =
        case same_args(As, Fargs, Sub) of
            true ->
                %% The arguments for the `function_clause` exception are
                %% the arguments for the current function in the correct
                %% order.
                {[], St0};
            false ->
                %% The arguments in the `function_clause` exception don't
                %% match the arguments for the current function because of
                %% inlining.
                case keyfind(function, 1, Anno) of
                    false ->
                        {Name, St1} = new_fun_name("inlined", St0),
                        {[{inlined,{Name,length(As) - 1}}], St1};
                    {_,{Name0,Arity}} ->
                        %% This is function that has been inlined.
                        Name1 = ["-inlined-",Name0,"/",Arity,"-"],
                        Name = list_to_atom(lists:concat(Name1)),
                        {[{inlined,{Name,Arity}}], St0}
                end
        end,
    {As, ExtraAnno, St}.

same_args([#c_var{name=Cv}|Vs], [#b_var{name=Kv}|As], Sub) ->
    get_vsub(Cv, Sub) =:= Kv andalso same_args(Vs, As, Sub);
same_args([], [], _Sub) -> true;
same_args(_, _, _) -> false.

expr_map(A, Var0, Ces, Sub, St0) ->
    {Var,Mps,St1} = expr(Var0, Sub, St0),
    {Km,Eps,St2} = map_split_pairs(A, Var, Ces, Sub, St1),
    {Km,Eps++Mps,St2}.

map_split_pairs(A, Var, Ces, Sub, St0) ->
    %% 1. Force variables.
    %% 2. Group adjacent pairs with literal keys.
    %% 3. Within each such group, remove multiple assignments to
    %%    the same key.
    %% 4. Partition each group according to operator ('=>' and ':=').
    {Pairs,Esp,St1} =
        foldr(fun(#c_map_pair{op=#c_literal{val=Op},key=K0,val=V0},
                  {Ops,Espi,Sti0}) when Op =:= assoc; Op =:= exact ->
                      {K,Eps1,Sti1} = atomic(K0, Sub, Sti0),
                      {V,Eps2,Sti2} = atomic(V0, Sub, Sti1),
                      {[{Op,K,V}|Ops],Eps1 ++ Eps2 ++ Espi,Sti2}
              end, {[],[],St0}, Ces),
    map_split_pairs_1(A, Var, Pairs, Esp, St1).

map_split_pairs_1(A, Map0, [{Op,Key,Val}|Pairs1]=Pairs0, Esp0, St0) ->
    {Map1,Em,St1} = force_atomic(Map0, St0),
    case Key of
        #b_var{} ->
            %% Don't combine variable keys with other keys.
            Kes = [[Key,Val]],
            Map = ssa_map(A, Op, Map1, Kes),
            map_split_pairs_1(A, Map, Pairs1, Esp0 ++ Em, St1);
        #b_literal{} ->
            %% Literal key. Split off all literal keys.
            {L,Pairs} = splitwith(fun({_,#b_var{},_}) -> false;
                                     ({_,_,_}) -> true
                                  end, Pairs0),
            {Map,Esp,St2} = map_group_pairs(A, Map1, L, Esp0 ++ Em, St1),
            map_split_pairs_1(A, Map, Pairs, Esp, St2)
    end;
map_split_pairs_1(_, Map, [], Esp, St0) ->
    {Map,Esp,St0}.

map_group_pairs(A, Var, Pairs0, Esp, St0) ->
    Pairs = map_remove_dup_keys(Pairs0),
    Assoc = [[K,V] || {_,{assoc,K,V}} <- Pairs],
    Exact = [[K,V] || {_,{exact,K,V}} <- Pairs],
    case {Assoc,Exact} of
        {[_|_],[]} ->
            {ssa_map(A, assoc, Var, Assoc),Esp,St0};
        {[],[_|_]} ->
            {ssa_map(A, exact, Var, Exact),Esp,St0};
        {[_|_],[_|_]} ->
            Map = ssa_map(A, assoc, Var, Assoc),
            {Mvar,Em,St1} = force_atomic(Map, St0),
            {ssa_map(A, exact, Mvar, Exact),Esp ++ Em,St1}
    end.

ssa_map(A, Op, SrcMap, Pairs) ->
    FlatList = append(Pairs),
    Args = [#b_literal{val=Op},SrcMap|FlatList],
    LineAnno = line_anno(A),
    Set = #b_set{anno=LineAnno,op=put_map,args=Args},
    case Op of
        assoc -> Set;
        exact -> #cg_succeeded{set=Set}
    end.

map_remove_dup_keys(Es) ->
    map_remove_dup_keys(Es, #{}).

map_remove_dup_keys([{assoc,K,V}|Es0], Used0) ->
    Op = case Used0 of
             #{K := {exact,_,_}} -> exact;
             #{} -> assoc
         end,
    Used1 = Used0#{K => {Op,K,V}},
    map_remove_dup_keys(Es0, Used1);
map_remove_dup_keys([{exact,K,V}|Es0], Used0) ->
    Op = case Used0 of
             #{K := {assoc,_,_}} -> assoc;
             #{} -> exact
         end,
    Used1 = Used0#{K => {Op,K,V}},
    map_remove_dup_keys(Es0, Used1);
map_remove_dup_keys([], Used) ->
    %% We must sort the map entries to ensure consistent
    %% order from compilation to compilation.
    sort(maps:to_list(Used)).

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

c_apply(A, #c_var{name={F0,Ar}}, Cargs, Sub, St0) ->
    {Args,Ap,St1} = atomic_list(Cargs, Sub, St0),
    case get_fsub(F0, Ar, Sub) of
        {letrec_goto,Label} ->
            {#cg_goto{label=Label,args=Args},Ap,St1};
        F1 ->
            {#cg_call{anno=A,op=#b_local{name=#b_literal{val=F1},arity=Ar},args=Args},
             Ap,St1}
    end;
c_apply(A, Cop, Cargs, Sub, St0) ->
    {Kop,Op,St1} = variable(Cop, Sub, St0),
    {Args,Ap,St2} = atomic_list(Cargs, Sub, St1),
    {#cg_call{anno=A,op=Kop,args=Args},Op ++ Ap,St2}.

flatten_seq(#ilet{vars=Vs,arg=Arg,body=B}) ->
    [#iset{vars=Vs,arg=Arg}|flatten_seq(B)];
flatten_seq(Ke) -> [Ke].

pre_seq([#iset{vars=Vs,arg=Arg}|Ps], K) ->
    #ilet{vars=Vs,arg=Arg,body=pre_seq(Ps, K)};
pre_seq([P|Ps], K) ->
    #ilet{vars=[],arg=P,body=pre_seq(Ps, K)};
pre_seq([], K) -> K.

%% atomic(Cexpr, Sub, State) -> {Katomic,[PreKexpr],State}.
%%  Convert a Core expression making sure the result is atomic
%%  (variable or literal).

atomic(Ce, Sub, St0) ->
    {Ke,Kp,St1} = expr(Ce, Sub, St0),
    {Ka,Ap,St2} = force_atomic(Ke, St1),
    {Ka,Kp ++ Ap,St2}.

force_atomic(#b_literal{}=Ke, St) ->
    {Ke,[],St};
force_atomic(Ke, St) ->
    force_variable(Ke, St).

%% atomic_list([Cexpr], Sub, State) -> {[Kexpr],[PreKexpr],State}.

atomic_list(Ces, Sub, St) ->
    foldr(fun (Ce, {Kes,Esp,St0}) ->
                  {Ke,Ep,St1} = atomic(Ce, Sub, St0),
                  {[Ke|Kes],Ep ++ Esp,St1}
          end, {[],[],St}, Ces).

%%%
%%% Construction of binaries.
%%%

expr_binary(Anno, Segs0, Sub, St0) ->
    {Segs1,Ep,St1} = atomic_bin(Segs0, Sub, St0),
    Segs = case Segs1 of
               [#b_literal{val=binary},UnitFlags,Val,#b_literal{val=all}|Segs2] ->
                   Op = case member(single_use, Anno) of
                            true -> private_append;
                            false -> append
                        end,
                   [#b_literal{val=Op},UnitFlags,Val,#b_literal{val=all}|Segs2];
               _ ->
                   Segs1
           end,
    LineAnno = line_anno(Anno),
    Build = #cg_succeeded{set=#b_set{anno=LineAnno,op=bs_create_bin,args=Segs}},
    {Build,Ep,St1}.

atomic_bin([#c_bitstr{anno=A,val=E0,size=S0,unit=U0,type=T,flags=Fs0}|Es0],
           Sub, St0) ->
    {E,Ap1,St1} = atomic(E0, Sub, St0),
    {S1,Ap2,St2} = atomic(S0, Sub, St1),
    validate_bin_element_size(S1, A),
    U1 = cerl:concrete(U0),
    Fs1 = cerl:concrete(Fs0),
    {Es,Ap3,St3} = atomic_bin(Es0, Sub, St2),
    {ssa_bin_segment(A, T, E, S1, U1, Fs1) ++ Es,
     Ap1++Ap2++Ap3,St3};
atomic_bin([], _Sub, St) -> {[],[],St}.

ssa_bin_segment(Anno, Type, Src, Size, U, Flags0) ->
    Seg = case lists:keyfind(segment, 1, Anno) of
              false -> [];
              {segment,_}=Seg0 -> [Seg0]
          end,
    TypeArg = #b_literal{val=cerl:concrete(Type)},
    Unit = case U of
               undefined -> 0;
               _ -> U
           end,
    Flags = strip_bs_construct_flags(Flags0),
    UnitFlags = #b_literal{val=[Unit|Flags++Seg]},
    [TypeArg,UnitFlags,Src,Size].

validate_bin_element_size(#b_var{}, _Anno) -> ok;
validate_bin_element_size(#b_literal{val=Val}, Anno) ->
    case Val of
        all -> ok;
        undefined -> ok;
        _ when is_integer(Val), Val >= 0 -> ok;
        _ -> throw({bad_segment_size,Anno})
    end.

%% Only keep the flags that have a meaning for binary construction and
%% are distinct from the default value.
strip_bs_construct_flags(Flags) ->
    [Flag || Flag <- Flags,
             case Flag of
                 little -> true;
                 native -> true;
                 big -> false;
                 signed -> false;
                 unsigned -> false
             end].

%% variable(Cexpr, Sub, State) -> {Kvar,[PreKexpr],State}.
%%  Convert a Core expression making sure the result is a variable.

variable(Ce, Sub, St0) ->
    {Ke,Kp,St1} = expr(Ce, Sub, St0),
    {Kv,Vp,St2} = force_variable(Ke, St1),
    {Kv,Kp ++ Vp,St2}.

force_variable(#b_var{}=Ke, St) ->
    {Ke,[],St};
force_variable(Ke, St0) ->
    {V,St1} = new_var(St0),
    {V,[#iset{vars=[V],arg=Ke}],St1}.

%% pattern(Cpat, Sub, State) -> {Kpat,Sub,State}.
%%  Convert patterns.  Variables shadow so rename variables that are
%%  already defined.

pattern(#c_var{name=V}, Sub, St0) ->
    case sets:is_element(V, St0#kern.ds) of
        true ->
            {New,St1} = new_var_name(St0),
            {#b_var{name=New},
             set_vsub(V, New, Sub),
             St1#kern{ds=sets:add_element(New, St1#kern.ds)}};
        false ->
            {#b_var{name=V},Sub,
             St0#kern{ds=sets:add_element(V, St0#kern.ds)}}
    end;
pattern(#c_literal{val=Val}, Sub, St) ->
    {#b_literal{val=Val},Sub,St};
pattern(#c_cons{hd=Ch,tl=Ct}, Sub0, St0) ->
    {Kh,Sub1,St1} = pattern(Ch, Sub0, St0),
    {Kt,Sub2,St2} = pattern(Ct, Sub1, St1),
    {#cg_cons{hd=Kh,tl=Kt},Sub2,St2};
pattern(#c_tuple{es=Ces}, Sub0, St0) ->
    {Kes,Sub1,St1} = pattern_list(Ces, Sub0, St0),
    {#cg_tuple{es=Kes},Sub1,St1};
pattern(#c_map{es=Ces}, Sub0, St0) ->
    {Kes,Sub1,St1} = pattern_map_pairs(Ces, Sub0, St0),
    {#cg_map{op=exact,es=Kes},Sub1,St1};
pattern(#c_binary{segments=Cv}, Sub0, St0) ->
    {Kv,Sub1,St1} = pattern_bin(Cv, Sub0, St0),
    {#cg_binary{segs=Kv},Sub1,St1};
pattern(#c_alias{var=Cv,pat=Cp}, Sub0, St0) ->
    {Cvs,Cpat} = flatten_alias(Cp),
    {Kvs,Sub1,St1} = pattern_list([Cv|Cvs], Sub0, St0),
    {Kpat,Sub2,St2} = pattern(Cpat, Sub1, St1),
    {#ialias{vars=Kvs,pat=Kpat},Sub2,St2}.

flatten_alias(#c_alias{var=V,pat=P}) ->
    {Vs,Pat} = flatten_alias(P),
    {[V|Vs],Pat};
flatten_alias(Pat) -> {[],Pat}.

pattern_map_pairs(Ces0, Sub0, St0) ->
    {Kes,{Sub1,St1}} =
        mapfoldl(fun(#c_map_pair{key=Ck,val=Cv},{Subi0,Sti0}) ->
                         {Kk,[],Sti1} = expr(Ck, Subi0, Sti0),
                         {Kv,Subi2,Sti2} = pattern(Cv, Subi0, Sti1),
                         {#cg_map_pair{key=Kk,val=Kv},{Subi2,Sti2}}
                 end, {Sub0, St0}, Ces0),
    %% It is later assumed that these keys are sorted according
    %% to the internal term order, so we'll need to sort them
    %% here.
    Kes1 = sort(fun(#cg_map_pair{key=A}, #cg_map_pair{key=B}) ->
                        erts_internal:cmp_term(A, B) < 0
                end, Kes),
    {Kes1,Sub1,St1}.

pattern_bin([#c_bitstr{val=E0,size=S0,unit=U0,type=T,flags=Fs0}|Es0],
            Sub0, St0) ->
    {S1,[],St1} = expr(S0, Sub0, St0),
    S = case S1 of
            #b_var{} -> S1;
            #b_literal{val=Val} when is_integer(Val); is_atom(Val) -> S1;
            _ ->
                %% Bad size (coming from an optimization or Core Erlang
                %% source code) - replace it with a known atom because
                %% a literal or bit syntax construction can cause further
                %% problems.
                #b_literal{val=bad_size}
        end,
    U = cerl:concrete(U0),
    Fs = cerl:concrete(Fs0),
    {E,Sub1,St2} = pattern(E0, Sub0, St1),
    {Es,Sub,St3} = pattern_bin(Es0, Sub1, St2),
    {build_bin_seg(S, U, cerl:concrete(T), Fs, E, Es),Sub,St3};
pattern_bin([], Sub, St) ->
    {#cg_bin_end{},Sub,St}.

%% build_bin_seg(Size, Unit, Type, Flags, Seg, Next) -> #cg_bin_seg{}.
%%  This function normalizes literal integers with size > 8 and literal
%%  utf8 segments into integers with size = 8 (and potentially an integer
%%  with size less than 8 at the end). This is so further optimizations
%%  have a normalized view of literal integers, allowing us to generate
%%  more literals and group more clauses. Those integers may be "squeezed"
%%  later into the largest integer possible.
%%
build_bin_seg(#b_literal{val=Bits}=Sz, U, integer=Type,
              [unsigned,big]=Flags, #b_literal{val=Int}=Seg, Next)
  when is_integer(Bits) ->
    Size = Bits * U,
    case integer_fits_and_is_expandable(Int, Size) of
        true ->
            build_bin_seg_integer_recur(Size, Int, Next);
        false ->
            #cg_bin_seg{size=Sz,unit=U,type=Type,
                        flags=Flags,seg=Seg,next=Next}
    end;
build_bin_seg(Sz, U, utf8=Type, [unsigned,big]=Flags,
              #b_literal{val=Utf8}=Seg, Next) ->
    case utf8_fits(Utf8) of
        {Int,Bits} ->
            build_bin_seg_integer_recur(Bits, Int, Next);
        error ->
            #cg_bin_seg{size=Sz,unit=U,type=Type,flags=Flags,seg=Seg,next=Next}
    end;
build_bin_seg(Sz, U, Type, Flags, Seg, Next) ->
    #cg_bin_seg{size=Sz,unit=U,type=Type,flags=Flags,seg=Seg,next=Next}.

build_bin_seg_integer_recur(Bits, Val, Next) ->
    Chunks = bitstring_to_list(<<Val:Bits>>),
    build_bin_seg_integer_recur_1(Chunks, Next).

build_bin_seg_integer_recur_1([Val0], Next) when is_bitstring(Val0) ->
    Bits = bit_size(Val0),
    <<Val:Bits>> = Val0,
    build_bin_seg_integer(Bits, Val, Next);
build_bin_seg_integer_recur_1([Val], Next) when is_integer(Val) ->
    build_bin_seg_integer(8, Val, Next);
build_bin_seg_integer_recur_1([Val|Values], Next0) when is_integer(Val) ->
    Next = build_bin_seg_integer_recur_1(Values, Next0),
    build_bin_seg_integer(8, Val, Next).

build_bin_seg_integer(Bits, Val, Next) ->
    Sz = #b_literal{val=Bits},
    Seg = #b_literal{val=Val},
    #cg_bin_seg{size=Sz,unit=1,type=integer,flags=[unsigned,big],
                seg=Seg,next=Next}.

integer_fits_and_is_expandable(Int, Size)
  when is_integer(Int), is_integer(Size),
       0 < Size, Size =< ?EXPAND_MAX_SIZE_SEGMENT ->
    case <<Int:Size>> of
        <<Int:Size>> -> true;
        _ -> false
    end;
integer_fits_and_is_expandable(_Int, _Size) -> false.

utf8_fits(Utf8) ->
    try <<Utf8/utf8>> of
        Bin ->
            Bits = bit_size(Bin),
            <<Int:Bits>> = Bin,
            {Int,Bits}
    catch
        _:_ -> error
    end.

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

set_vsub(Key, Val, Subs) ->
    true = Key =/= Val,                         %Assertion.
    bimap_set(Key, Val, Subs).

set_fsub(Name, Arity, Val, Subs) ->
    set_vsub({Name, Arity}, Val, Subs).

subst_vsub(Key, Val, Subs) ->
    bimap_rename(Key, Val, Subs).

bimap_get(Key, {Map, _InvMap}, Default) ->
    case Map of
        #{Key := Val} -> Val;
        #{} -> Default
    end.

%% Maps Key to Val without touching existing references to Key.
bimap_set(Key, Val, {Map0, InvMap0}) when is_map(Map0), is_map(InvMap0) ->
    InvMap = bm_update_inv_lookup(Key, Val, Map0, InvMap0),
    Map = Map0#{Key => Val},
    {Map,InvMap}.

bm_update_inv_lookup(Key, Val, Map, InvMap0) ->
    InvMap = bm_cleanup_inv_lookup(Key, Map, InvMap0),
    case InvMap of
        #{Val := Keys} ->
            %% Other keys map to the same value, add ours to the set.
            InvMap#{Val := add_element(Key, Keys)};
        #{} ->
            InvMap#{Val => [Key]}
    end.

bm_cleanup_inv_lookup(Key, Map, InvMap) when is_map_key(Key, Map) ->
    #{Key := Old} = Map,
    #{Old := Keys0} = InvMap,
    case del_element(Key, Keys0) of
        [] ->
            maps:remove(Old, InvMap);
        Keys ->
            InvMap#{Old := Keys}
    end;
bm_cleanup_inv_lookup(_Key, _Map, InvMap) ->
    InvMap.

%% Map Key to Val, and replace all existing references to Key with Val.
bimap_rename(Key, Val, {Map0, InvMap0}) when is_map_key(Key, InvMap0) ->
    {Keys,InvMap1} = maps:take(Key, InvMap0),
    InvMap = InvMap1#{Val => add_element(Key, Keys)},

    Map1 = Map0#{Key => Val},
    Map = bimap_update_lookup(Keys, Val, Map1),

    {Map,InvMap};
bimap_rename(Key, Val, Subs) ->
    bimap_set(Key, Val, Subs).

bimap_update_lookup([Key|Keys], Val, Map) ->
    bimap_update_lookup(Keys, Val, Map#{Key := Val});
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

%% new_var(State) -> {#b_var{},State}.

new_var(St0) ->
    {New,St1} = new_var_name(St0),
    {#b_var{name=New},St1}.

%% new_vars(Count, State) -> {[#b_var{}],State}.

new_vars(N, St) when is_integer(N) ->
    new_vars(N, St, []).

new_vars(N, St0, Vs) when N > 0 ->
    {V,St1} = new_var(St0),
    new_vars(N-1, St1, [V|Vs]);
new_vars(0, St, Vs) -> {Vs,St}.

make_vars(Vs) -> [#b_var{name=V} || V <- Vs].

%% call_type(Mod, Name, [Arg], State) -> bif | call | is_record | error.

call_type(#c_literal{val=M}, #c_literal{val=F}, As, St) when is_atom(M), is_atom(F) ->
    case is_guard_bif(M, F, As) of
        false ->
            call;
        true ->
            %% The guard BIFs min/2 and max/2 were introduced in
            %% Erlang/OTP 26. If we are compiling for an earlier
            %% version, we must translate them as call instructions.
            case {M,F,St#kern.no_min_max_bifs} of
                {erlang,min,true} -> call;
                {erlang,max,true} -> call;
                {erlang,is_record,_} when length(As) =:= 3 -> is_record;
                {erlang,_,_} -> bif
            end
    end;
call_type(#c_var{}, #c_literal{val=A}, _, _) when is_atom(A) -> call;
call_type(#c_literal{val=A}, #c_var{}, _, _) when is_atom(A) -> call;
call_type(#c_var{}, #c_var{}, _, _) -> call;
call_type(_, _, _, _) -> error.

%% is_guard_bif(Mod, Name, Args) -> true | false.
%%  Test whether this function is a guard BIF.

is_guard_bif(erlang, get, [_]) -> true;
is_guard_bif(erlang, is_record, [_,Tag,Sz]) ->
    case {Tag,Sz} of
        {#c_literal{val=Atom},#c_literal{val=Arity}}
          when is_atom(Atom), is_integer(Arity), Arity >= 1 ->
            true;
        {_,_} ->
            false
    end;
is_guard_bif(erlang, N, As) ->
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
                _:_ -> false            % not an op
            end
    end;
is_guard_bif(_, _, _) -> false.

%% This code implements the algorithm for an optimizing compiler for
%% pattern matching given "The Implementation of Functional
%% Programming Languages" by Simon Peyton Jones. The code is much
%% longer as the meaning of constructors is different from the book.
%%
%% In Erlang many constructors can have different values, e.g. 'atom'
%% or 'integer', whereas in the original algorithm these would be
%% different constructors. Our view makes it easier in later passes to
%% handle indexing over each type.
%%
%% Patterns are complicated by having alias variables.  The form of a
%% pattern is Pat | {alias,Pat,[AliasVar]}.  This is hidden by access
%% functions to pattern arguments but the code must be aware of it.
%%
%% The compilation proceeds in two steps:
%%
%% 1. The patterns in the clauses to converted to lists of Kernel
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
    {Cs,St1} = match_pre(Ccs, Sub, St0),        %Convert clauses
    Def = fail,
    match(Us, Cs, Def, St1).                    %Do the match.

%% match_pre([Cclause], Sub, State) -> {[Clause],State}.
%%  Must be careful not to generate new substitutions here now!

match_pre(Cs, Sub0, St) ->
    foldr(fun (#c_clause{anno=A,pats=Ps,guard=G,body=B}, {Cs0,St0}) ->
                  {Kps,Sub1,St1} = pattern_list(Ps, Sub0, St0),
                  {[#iclause{anno=A,sub=Sub1,
                             pats=Kps,guard=G,body=B}|Cs0],St1}
          end, {[],St}, Cs).

%% match([Var], [Clause], Default, State) -> {MatchExpr,State}.

match([_|_]=Vars, Cs, Def, St0) ->
    Pcss = partition(Cs),
    foldr(fun (Pcs, {D,St}) ->
                  match_varcon(Vars, Pcs, D, St)
          end, {Def,St0}, Pcss);
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
            St2 = maybe_add_warning(Cs0, A, St1),
            St = maybe_add_warning(Def0, A, St2),
            {[],pre_seq(Pb, Kb),St};
        false ->
            {Kg,St1} = guard(G, Sub, St0),
            {Kb,Pb,St2} = body(B, Sub, St1),
            {Cs1,Def1,St3} = match_guard_1(Cs0, Def0, St2),
            {[#cg_guard_clause{guard=Kg,body=pre_seq(Pb, Kb)}|Cs1],
             Def1,St3}
    end;
match_guard_1([], Def, St) -> {[],Def,St}.

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
    Cs1 = map(fun (#iclause{sub=Sub0,pats=[Arg|As]}=C) ->
                      Vs = [arg_arg(Arg)|arg_alias(Arg)],
                      Sub1 = foldl(fun (#b_var{name=V}, Acc) ->
                                           subst_vsub(V, U#b_var.name, Acc)
                                   end, Sub0, Vs),
                      C#iclause{sub=Sub1,pats=As}
              end, Cs0),
    match(Us, Cs1, Def, St).

%% match_con(Variables, [Clause], Default, State) -> {SelectExpr,State}.
%%  Build call to "select" from a list of clauses all containing a
%%  constructor/constant as first argument.  Group the constructors
%%  according to type, the order is really irrelevant but tries to be
%%  smart.
match_con([U|_Us]=L, Cs, Def, St0) ->
    %% Extract clauses for different constructors (types).
    Ttcs0 = select_types(Cs, [], [], [], [], [], [], [], [], []),
    Ttcs1 = [{T, Types} || {T, [_ | _] = Types} <- Ttcs0],
    Ttcs = opt_single_valued(Ttcs1),
    {Scs,St1} =
        mapfoldl(fun ({T,Tcs}, St) ->
                         {[S|_]=Sc,S1} = match_value(L, T, Tcs, fail, St),
                         #cg_val_clause{anno=Anno} = S,
                         {#cg_type_clause{anno=Anno,type=T,values=Sc},S1} end,
                 St0, Ttcs),
    {build_alt(build_select(U, Scs), Def),St1}.

select_types([NoExpC|Cs], Bin, BinCon, Cons, Tuple, Map, Atom, Float, Int, Nil) ->
    C = expand_pat_lit_clause(NoExpC),
    case clause_con(C) of
        cg_binary ->
            select_types(Cs, [C|Bin], BinCon, Cons, Tuple, Map, Atom, Float, Int, Nil);
        cg_bin_seg ->
            select_types(Cs, Bin, [C|BinCon], Cons, Tuple, Map, Atom, Float, Int, Nil);
        cg_bin_end ->
            select_types(Cs, Bin, [C|BinCon], Cons, Tuple, Map, Atom, Float, Int, Nil);
        cg_cons ->
            select_types(Cs, Bin, BinCon, [C|Cons], Tuple, Map, Atom, Float, Int, Nil);
        cg_tuple ->
            select_types(Cs, Bin, BinCon, Cons, [C|Tuple], Map, Atom, Float, Int, Nil);
        cg_map ->
            select_types(Cs, Bin, BinCon, Cons, Tuple, [C|Map], Atom, Float, Int, Nil);
        cg_nil ->
            select_types(Cs, Bin, BinCon, Cons, Tuple, Map, Atom, Float, Int, [C|Nil]);
        cg_atom ->
            select_types(Cs, Bin, BinCon, Cons, Tuple, Map, [C|Atom], Float, Int, Nil);
        cg_float ->
            select_types(Cs, Bin, BinCon, Cons, Tuple, Map, Atom, [C|Float], Int, Nil);
        cg_int ->
            select_types(Cs, Bin, BinCon, Cons, Tuple, Map, Atom, Float, [C|Int], Nil)
    end;
select_types([], Bin, BinCon, Cons, Tuple, Map, Atom, Float, Int, Nil) ->
    [{cg_binary, reverse(Bin)}] ++ handle_bin_con(reverse(BinCon)) ++
        [
         {cg_cons, reverse(Cons)},
         {cg_tuple, reverse(Tuple)},
         {cg_map, reverse(Map)},
         {{bif,is_atom}, reverse(Atom)},
         {{bif,is_float}, reverse(Float)},
         {{bif,is_integer}, reverse(Int)},
         {cg_nil, reverse(Nil)}
        ].

expand_pat_lit_clause(#iclause{pats=[#ialias{pat=#b_literal{val=Val}}=Alias|Ps]}=C) ->
    P = expand_pat_lit(Val),
    C#iclause{pats=[Alias#ialias{pat=P}|Ps]};
expand_pat_lit_clause(#iclause{pats=[#b_literal{val=Val}|Ps]}=C) ->
    P = expand_pat_lit(Val),
    C#iclause{pats=[P|Ps]};
expand_pat_lit_clause(C) -> C.

expand_pat_lit([H|T]) ->
    #cg_cons{hd=#b_literal{val=H},tl=#b_literal{val=T}};
expand_pat_lit(Tuple) when is_tuple(Tuple) ->
    #cg_tuple{es=[#b_literal{val=E} || E <- tuple_to_list(Tuple)]};
expand_pat_lit(Lit) ->
    #b_literal{val=Lit}.

%% opt_singled_valued([{Type,Clauses}]) -> [{Type,Clauses}].
%%  If a type only has one clause and if the pattern is a complex
%%  literal, the matching can be done more efficiently by directly
%%  comparing with the literal (that is especially true for binaries).
%%
%%  It is important not to do this transformation for atomic literals
%%  (such as `[]`), since that would cause the test for an empty list
%%  to be executed before the test for a nonempty list.

opt_single_valued(Ttcs) ->
    opt_single_valued(Ttcs, [], []).

opt_single_valued([{_,[#iclause{pats=[#b_literal{}|_]}]}=Ttc|Ttcs], TtcAcc, LitAcc) ->
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
    Literals = {b_literal,reverse(LitAcc)},
    %% Test the literals as early as possible.
    case reverse(TtcAcc) of
        [{cg_binary,_}=Bin|Ttcs] ->
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
combine_lit_pat(#b_literal{}) ->
    %% This is an atomic literal. Rewriting would be a pessimization,
    %% especially for `[]`.
    throw(not_possible);
combine_lit_pat(Pat) ->
    do_combine_lit_pat(Pat).

do_combine_lit_pat(#cg_binary{segs=Segs}) ->
    Bin = combine_bin_segs(Segs),
    #b_literal{val=Bin};
do_combine_lit_pat(#cg_cons{hd=Hd0,tl=Tl0}) ->
    #b_literal{val=Hd} = do_combine_lit_pat(Hd0),
    #b_literal{val=Tl} = do_combine_lit_pat(Tl0),
    #b_literal{val=[Hd|Tl]};
do_combine_lit_pat(#b_literal{}=Lit) ->
    Lit;
do_combine_lit_pat(#cg_tuple{es=Es0}) ->
    Es = [begin
              #b_literal{val=Lit} = do_combine_lit_pat(El),
              Lit
          end || El <- Es0],
    #b_literal{val=list_to_tuple(Es)};
do_combine_lit_pat(_) ->
    throw(not_possible).

combine_bin_segs(#cg_bin_seg{size=#b_literal{val=8},unit=1,type=integer,
                             flags=[unsigned,big],seg=#b_literal{val=Int},next=Next})
  when is_integer(Int), 0 =< Int, Int =< 255 ->
    <<Int,(combine_bin_segs(Next))/bits>>;
combine_bin_segs(#cg_bin_end{}) ->
    <<>>;
combine_bin_segs(_) ->
    throw(not_possible).

%% handle_bin_con([Clause]) -> [{Type,[Clause]}].
%%  Handle clauses for the cg_bin_seg constructor.  As cg_bin_seg
%%  matching can overlap, the cg_bin_seg constructors cannot be
%%  reordered, only grouped.

handle_bin_con(Cs) ->
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
    %% The way we do is to replace the generic #cg_bin_seg{}
    %% record with a #cg_bin_int{} record if all clauses will
    %% select the same literal integer (except for one or more
    %% clauses that will end the binary).
    try
        {BinSegs0,BinEnd} =
            partition(fun (C) ->
                              clause_con(C) =:= cg_bin_seg
                      end, Cs),
        BinSegs = select_bin_int(BinSegs0),
        case BinEnd of
            [] -> BinSegs;
            [_|_] -> BinSegs ++ [{cg_bin_end,BinEnd}]
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

%% select_bin_int([Clause]) -> {cg_bin_int,[Clause]}
%%  If the first pattern in each clause selects the same integer,
%%  rewrite all clauses to use #cg_bin_int{} (which will later be
%%  translated to a bs_match_string/4 instruction).
%%
%%  If it is not possible to do this rewrite, a 'not_possible'
%%  exception is thrown.

select_bin_int([#iclause{pats=[#cg_bin_seg{type=integer,
                                           size=#b_literal{val=Bits0}=Sz,unit=U,
                                           flags=Fl,seg=#b_literal{val=Val},
                                           next=N}|Ps]}=C|Cs0])
  when is_integer(Bits0), is_integer(U) ->
    Bits = U * Bits0,
    if
        Bits > ?EXPAND_MAX_SIZE_SEGMENT ->
            throw(not_possible);           %Expands the code too much.
        true ->
            ok
    end,
    select_assert_match_possible(Bits, Val, Fl),
    P = #cg_bin_int{size=Sz,unit=U,flags=Fl,val=Val,next=N},
    case member(native, Fl) of
        true -> throw(not_possible);
        false -> ok
    end,
    Cs1 = [C#iclause{pats=[P|Ps]}|select_bin_int_1(Cs0, Bits, Fl, Val)],
    Cs = reorder_bin_ints(Cs1),
    [{cg_bin_int,Cs}];
select_bin_int(_) -> throw(not_possible).

select_bin_int_1([#iclause{pats=[#cg_bin_seg{type=integer,
                                             size=#b_literal{val=Bits0}=Sz,
                                             unit=U,
                                             flags=Fl,seg=#b_literal{val=Val},
                                             next=N}|Ps]}=C|Cs],
                 Bits, Fl, Val) when is_integer(Val) ->
    if
        Bits0*U =:= Bits -> ok;
        true -> throw(not_possible)
    end,
    P = #cg_bin_int{size=Sz,unit=U,flags=Fl,val=Val,next=N},
    [C#iclause{pats=[P|Ps]}|select_bin_int_1(Cs, Bits, Fl, Val)];
select_bin_int_1([], _, _, _) -> [];
select_bin_int_1(_, _, _, _) -> throw(not_possible).

select_assert_match_possible(Sz, Val, Fs)
  when is_integer(Sz), Sz >= 0, is_integer(Val) ->
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
        ok         %this is just an assertion (i.e., no return value)
    catch
        throw:nomatch ->
            throw(not_possible)
    end;
select_assert_match_possible(_, _, _) ->
    throw(not_possible).

match_fun(Val) ->
    fun(match, {{integer,_,_},NewV,Bs}) when NewV =:= Val ->
            {match,Bs}
    end.

reorder_bin_ints([_]=Cs) ->
    Cs;
reorder_bin_ints(Cs0) ->
    %% It is safe to reorder clauses that match binaries if all
    %% of the followings conditions are true:
    %%
    %% * The first segments for all of them match the same number of
    %%   bits (guaranteed by caller).
    %%
    %% * All segments have fixed sizes.
    %%
    %% * The patterns that follow are also safe to re-order.
    try
        Cs = sort([{reorder_bin_int_sort_key(C),C} || C <- Cs0]),
        [C || {_,C} <:- Cs]
    catch
        throw:not_possible ->
            Cs0
    end.

reorder_bin_int_sort_key(#iclause{pats=[Pat|More],guard=#c_literal{val=true}}) ->
    case all(fun(#b_var{}) -> true;
                (_) -> false
             end, More) of
        true ->
            %% Only variables. Safe to re-order.
            ok;
        false ->
            %% Not safe to re-order. For example:
            %%    f([<<"prefix">>, <<"action">>]) -> ...
            %%    f([<<"prefix">>, Variable]) -> ...
            throw(not_possible)
    end,

    %% Ensure that the remaining segments have fixed sizes. For example, the following
    %% clauses are not safe to re-order:
    %%    f(<<"dd",_/binary>>) -> dd;
    %%    f(<<"d",_/binary>>) -> d.
    ensure_fixed_size(Pat#cg_bin_int.next),

    case Pat of
        #cg_bin_int{val=Val,next=#cg_bin_end{}} ->
            %% Sort before clauses with additional segments. This
            %% usually results in better code.
            [Val];
        #cg_bin_int{val=Val} ->
            [Val,more]
    end;
reorder_bin_int_sort_key(#iclause{}) ->
    throw(not_possible).

ensure_fixed_size(#cg_bin_seg{size=Size,next=Next}) ->
    case Size of
        #b_literal{val=Sz} when is_integer(Sz) ->
            ensure_fixed_size(Next);
        _ ->
            throw(not_possible)
    end;
ensure_fixed_size(#cg_bin_end{}) ->
    ok.

%% match_value([Var], Con, [Clause], Default, State) -> {SelectExpr,State}.
%%  At this point all the clauses have the same constructor; we must
%%  now separate them according to value.

match_value(Us0, cg_map=T, Cs0, Def, St0) ->
    {Cs1,St1} = remove_unreachable(Cs0, St0),
    {Us1,Cs2,St2} = partition_intersection(Us0, Cs1, St1),
    do_match_value(Us1, T, Cs2, Def, St2);
match_value(Us0, T, Cs0, Def, St0) ->
    do_match_value(Us0, T, Cs0, Def, St0).

do_match_value(Us0, T, Cs0, Def, St0) ->
    UCss = group_value(T, Us0, Cs0),
    mapfoldl(fun ({Us,Cs}, St) -> match_clause(Us, Cs, Def, St) end, St0, UCss).

%% remove_unreachable([Clause], State) -> {[Clause],State}
%%  Remove all clauses after a clause that will always match any
%%  map.
remove_unreachable([#iclause{anno=Anno,pats=Pats,guard=G}=C|Cs0], St0) ->
    maybe
        %% Will the first pattern match any map?
        [#cg_map{es=[]}|Ps] ?= Pats,

        %% Are all following pattern variables, which will always match?
        true ?= all(fun(#b_var{}) -> true;
                       (_) -> false
                    end, Ps),

        %% Will the guard always succeed?
        #c_literal{val=true} ?= G,

        %% This clause will always match. Warn and discard all clauses
        %% that follow.
        St1 = maybe_add_warning(Cs0, Anno, St0),
        {[C],St1}
    else
        _ ->
            {Cs,St} = remove_unreachable(Cs0, St0),
            {[C|Cs],St}
    end;
remove_unreachable([], St0) ->
    {[],St0}.

%% partition_intersection(Us, [Clause], State) -> {Us,Cs,State}.
%%  Partition a map into two maps with the most common keys to the
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

partition_intersection([U|_]=Us, [_,_|_]=Cs0, St0) ->
    Ps = [clause_val(C) || C <- Cs0],
    case find_key_intersection(Ps) of
        none ->
            {Us,Cs0,St0};
        {ok, Ks} ->
            Cs1 = map(fun(#iclause{pats=[Arg|Args]}=C) ->
                              {Arg1,Arg2} = partition_keys(Arg, Ks),
                              C#iclause{pats=[Arg1,Arg2|Args]}
                      end, Cs0),
            {[U|Us],Cs1,St0}
    end;
partition_intersection(Us, Cs, St) ->
    {Us,Cs,St}.

partition_keys(#cg_map{es=Pairs}=Map, Ks) ->
    F = fun(#cg_map_pair{key=Key}) ->
                sets:is_element(Key, Ks)
        end,
    {Ps1,Ps2} = partition(F, Pairs),
    {Map#cg_map{es=Ps1},Map#cg_map{es=Ps2}};
partition_keys(#ialias{pat=Map}=Alias, Ks) ->
    %% Only alias one of them.
    {Map1,Map2} = partition_keys(Map, Ks),
    {Map1,Alias#ialias{pat=Map2}}.

find_key_intersection(Ps) ->
    Sets = [sets:from_list(Ks) || Ks <- Ps],
    Intersection = sets:intersection(Sets),
    case sets:is_empty(Intersection) of
        true ->
            none;
        false ->
            All = all(fun (Kset) -> Kset =:= Intersection end, Sets),
            case All of
                true ->
                    %% All clauses test the same keys. Partitioning
                    %% the keys could only make the code worse.
                    none;
                false ->
                    {ok, Intersection}
            end
    end.

%% group_value([Clause]) -> [[Clause]].
%%  Group clauses according to value.  Here we know that:
%%  1. Some types are singled valued
%%  2. The clauses in maps and bin_segs cannot be reordered,
%%     only grouped
%%  3. Other types are disjoint and can be reordered

group_value(cg_cons, Us, Cs)    -> [{Us,Cs}];  %These are single valued
group_value(cg_nil, Us, Cs)     -> [{Us,Cs}];
group_value(cg_binary, Us, Cs)  -> [{Us,Cs}];
group_value(cg_bin_end, Us, Cs) -> [{Us,Cs}];
group_value(cg_bin_seg, Us, Cs) -> group_keeping_order(Us, Cs);
group_value(cg_bin_int, Us, Cs) -> [{Us,Cs}];
group_value(cg_map, Us, Cs)     -> group_keeping_order(Us, Cs);
group_value(_, Us, Cs) ->
    Map = group_values(Cs),

    %% We must sort the grouped values to ensure consistent
    %% order from compilation to compilation.
    sort([{Us,Vcs} || _ := Vcs <- Map]).

group_values(Cs) ->
    F = fun(C) -> clause_val(C) end,
    maps:groups_from_list(F, Cs).

group_keeping_order(Us, [C1|Cs]) ->
    V1 = clause_val(C1),
    {More,Rest} = splitwith(fun (C) -> clause_val(C) =:= V1 end, Cs),
    [{Us,[C1|More]}|group_keeping_order(Us, Rest)];
group_keeping_order(_, []) -> [].

%% match_clause([Var], [Clause], Default, State) -> {Clause,State}.
%%  At this point all the clauses have the same "value".  Build one
%%  select clause for this value and continue matching.  Rename
%%  aliases as well.

match_clause([U|Us], [#iclause{anno=Anno}|_]=Cs0, Def, St0) ->
    {Match,Vs,St1} = get_match(get_con(Cs0), St0),
    Cs1 = new_clauses(Cs0, U),
    Cs2 = squeeze_clauses(Cs1, []),
    {B,St2} = match(Vs ++ Us, Cs2, Def, St1),
    {#cg_val_clause{anno=Anno,val=Match,body=B},St2}.

get_con([C|_]) -> arg_arg(clause_arg(C)).       %Get the constructor

get_match(#cg_cons{}, St0) ->
    {[H,T]=L,St1} = new_vars(2, St0),
    {#cg_cons{hd=H,tl=T},L,St1};
get_match(#cg_binary{}, St0) ->
    {V,St1} = new_var(St0),
    {#cg_binary{segs=V},[V],St1};
get_match(#cg_bin_seg{size=#b_literal{val=all},next=#cg_bin_end{}}=Seg, St0) ->
    {[S,N],St1} = new_vars(2, St0),
    {Seg#cg_bin_seg{seg=S,next=N},[S],St1};
get_match(#cg_bin_seg{}=Seg, St0) ->
    {[S,N],St1} = new_vars(2, St0),
    {Seg#cg_bin_seg{seg=S,next=N},[S,N],St1};
get_match(#cg_bin_int{}=BinInt, St0) ->
    {N,St1} = new_var(St0),
    {BinInt#cg_bin_int{next=N},[N],St1};
get_match(#cg_tuple{es=Es}, #kern{beam_debug_info=DebugInfo}=St0) ->
    {Mes,St1} = new_vars(length(Es), St0),
    Keep =
        case DebugInfo of
            true ->
                %% Force extraction of all variables mentioned in the
                %% original source to give them a chance to appear in
                %% the debug information. This is a not guarantee that
                %% they will appear, since they can be killed before
                %% reaching a `debug_line` instruction.
                Keep0 = [New ||
                            #b_var{name=Old} <- Es && #b_var{name=New} <- Mes,
                            beam_ssa_codegen:is_original_variable(Old)],
                ordsets:from_list(Keep0);
            false ->
                []
        end,
    {#cg_tuple{es=Mes,keep=Keep},Mes,St1};
get_match(#cg_map{op=exact,es=Es0}, St0) ->
    {Mes,St1} = new_vars(length(Es0), St0),
    {Es,_} = mapfoldl(fun(#cg_map_pair{}=Pair, [V|Vs]) ->
                              {Pair#cg_map_pair{val=V},Vs}
                      end, Mes, Es0),
    {#cg_map{op=exact,es=Es},Mes,St1};
get_match(M, St) ->
    {M,[],St}.

new_clauses(Cs, #b_var{name=U}) ->
    map(fun(#iclause{sub=Sub0,pats=[Arg|As]}=C) ->
                Head = case arg_arg(Arg) of
                           #cg_cons{hd=H,tl=T} -> [H,T|As];
                           #cg_tuple{es=Es} -> Es ++ As;
                           #cg_binary{segs=E} -> [E|As];
                           #cg_bin_seg{size=#b_literal{val=all},
                                       seg=S,next=#cg_bin_end{}} ->
                               [S|As];
                           #cg_bin_seg{seg=S,next=N} ->
                               [S,N|As];
                           #cg_bin_int{next=N} ->
                               [N|As];
                           #cg_map{op=exact,es=Es} ->
                               Vals = [V || #cg_map_pair{val=V} <:- Es],
                               Vals ++ As;
                           _Other ->
                               As
                       end,
                Vs = arg_alias(Arg),
                Sub1 = foldl(fun (#b_var{name=V}, Acc) ->
                                     subst_vsub(V, U, Acc)
                             end, Sub0, Vs),
                C#iclause{sub=Sub1,pats=Head}
        end, Cs).

%%%
%%% Group and squeeze
%%%
%%% The goal of those functions is to group subsequent integer
%%% cg_bin_seg literals by count so we can leverage bs_get_integer_16
%%% whenever possible.
%%%
%%% The priority is to create large groups. So if we have three
%%% clauses matching on 16-bits/16-bits/8-bits, we will first have a
%%% single 8-bits match for all three clauses instead of clauses (one
%%% with 16 and another with 8). But note the algorithm is recursive,
%%% so the remaining 8-bits for the first two clauses will be grouped
%%% next.
%%%
%%% We also try to avoid creating too large groups. If we have too
%%% many clauses, it is preferable to match on 8 bits, select a
%%% branch, then match on the next 8 bits, rather than match on
%%% 16 bits which would force us to have to select too many values at
%%% the same time, which would not be efficient.
%%%
%%% Another restriction is that we create groups only if the end of
%%% the group is a variadic clause or the end of the binary. That's
%%% because if we have 16-bits/16-bits/catch-all, breaking it into a
%%% 16-bits lookup will make the catch-all more expensive.
%%%
%%% Clauses are grouped in reverse when squeezing and then flattened and
%%% re-reversed at the end.
%%%

squeeze_clauses([C|Cs], Acc) ->
    case clause_count_segments(C) of
        {literal,N} ->
            squeeze_clauses(Cs, N, 1, [C], Acc);
        _ ->
            squeeze_clauses(Cs, [[C]|Acc])
    end;
squeeze_clauses(_, Acc) ->
    flat_reverse(Acc).

squeeze_clauses([C|Cs], N0, Count, GroupAcc, Acc) ->
    case clause_count_segments(C) of
        {literal,N} ->
            squeeze_clauses(Cs, min(N0, N), Count + 1,
                            [C|GroupAcc], Acc);
        {variadic,N} when N =< N0 ->
            Squeezed = do_squeeze_clauses(GroupAcc, N, Count),
            squeeze_clauses(Cs, [[C|Squeezed] | Acc]);
        bin_end when Cs =:= [] ->
            Squeezed = do_squeeze_clauses(GroupAcc, fix_count(N0), Count),
            flat_reverse([[C|Squeezed] | Acc]);
        _ ->
            squeeze_clauses(Cs, [[C|GroupAcc] | Acc])
    end;
squeeze_clauses([], N, Count, GroupAcc, Acc) ->
    Squeezed = do_squeeze_clauses(GroupAcc, fix_count(N), Count),
    flat_reverse([Squeezed|Acc]).

clause_count_segments(#iclause{pats=[P|_]}) ->
    case P of
        #cg_bin_seg{seg=#b_literal{}} ->
            count_segments(P, 0);
        #cg_bin_seg{size=#b_literal{val=Size},
                    unit=Unit,
                    type=integer,
                    flags=[unsigned,big],
                    seg=#b_var{}} when ((Size * Unit) rem 8) =:= 0 ->
            {variadic, (Size * Unit) div 8};
        #cg_bin_end{} ->
            bin_end;
        _ ->
            error
    end;
clause_count_segments(_) -> error.

count_segments(#cg_bin_seg{size=#b_literal{val=8},
                           unit=1,type=integer,flags=[unsigned,big],
                           seg=#b_literal{val=Int},next=Next}, Count)
  when is_integer(Int), 0 =< Int, Int =< 255 ->
    count_segments(Next, Count + 1);
count_segments(_, Count) when Count > 0 ->
    {literal,Count};
count_segments(_, _Count) ->
    error.

%% Since 4 bytes in on 32-bits systems are bignums, we convert
%% anything more than 3 bytes into a 2-byte lookup. The goal is to
%% convert any multi-clause segment into 2-byte lookups with a
%% potential 3-byte lookup at the end.
fix_count(N) when N > 3 -> 2;
fix_count(N) -> N.

do_squeeze_clauses(Cs, Size, Count) when Count >= 16; Size =< 1 ->
    %% If we have more than 16 clauses it is better to branch multiple
    %% times than getting a large integer. We also give up if we have
    %% nothing to squeeze.
    Cs;
do_squeeze_clauses(Cs, Size, _Count) ->
    [C#iclause{pats=[squeeze_segments(P, Size)|Pats]} ||
        #iclause{pats=[P|Pats]}=C <:- Cs].

squeeze_segments(BinSeg, Size) ->
    squeeze_segments(BinSeg, 0, 0, Size).

squeeze_segments(#cg_bin_seg{seg=#b_literal{val=Val},next=Next}=BinSeg,
                 Acc0, Size0, Count) ->
    Acc = (Acc0 bsl 8) bor Val,
    Size = Size0 + 8,
    case Count of
        1 ->
            BinSeg#cg_bin_seg{size=#b_literal{val=Size},
                              seg=#b_literal{val=Acc}};
        _ ->
            squeeze_segments(Next, Acc, Size, Count - 1)
    end.

flat_reverse(L) ->
    flat_reverse(L, []).

flat_reverse([H|T], Acc) ->
    flat_reverse(T, reverse(H, Acc));
flat_reverse([], Acc) -> Acc.

%%%
%%% End of group and squeeze
%%%

%% build_guard([GuardClause]) -> GuardExpr.

build_guard([]) -> fail;
build_guard(Cs) -> #cg_guard{clauses=Cs}.

%% build_select(Var, [ConClause]) -> SelectExpr.

build_select(V, [#cg_type_clause{anno=Anno}|_]=Tcs) ->
    #cg_select{anno=Anno,var=V,types=Tcs}.

%% build_alt(First, Then) -> AltExpr.
%%  Build an alt.

build_alt(fail, Then) -> Then;
build_alt(First, fail) -> First;
build_alt(First, Then) ->
    Anno = get_anno(First),
    #cg_alt{anno=Anno,first=First,then=Then}.

%% build_match(MatchExpr) -> Kexpr.
%%  Build a match expr if there is a match.

build_match(#cg_alt{}=Km) -> #cg_match{body=Km};
build_match(#cg_select{}=Km) -> #cg_match{body=Km};
build_match(#cg_guard{}=Km) -> #cg_match{body=Km};
build_match(Km) -> Km.

%% clause_arg(Clause) -> FirstArg.
%% clause_con(Clause) -> Constructor.
%% clause_val(Clause) -> Value.
%% is_var_clause(Clause) -> boolean().

clause_arg(#iclause{pats=[Arg|_]}) -> Arg.

clause_con(C) -> arg_con(clause_arg(C)).

clause_val(C) -> arg_val(clause_arg(C), C).

is_var_clause(C) -> clause_con(C) =:= b_var.

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
        #cg_cons{} -> cg_cons;
        #cg_tuple{} -> cg_tuple;
        #cg_map{} -> cg_map;
        #cg_binary{} -> cg_binary;
        #cg_bin_end{} -> cg_bin_end;
        #cg_bin_seg{} -> cg_bin_seg;
        #b_var{} -> b_var;
        #b_literal{val=Val} ->
            if
                is_atom(Val) -> cg_atom;
                is_integer(Val) -> cg_int;
                is_float(Val) -> cg_float;
                Val =:= [] -> cg_nil;
                true -> b_literal
            end
    end.

arg_val(Arg, C) ->
    case arg_arg(Arg) of
        #b_literal{val=Lit} -> Lit;
        #cg_tuple{es=Es} -> length(Es);
        #cg_bin_seg{size=S,unit=U,type=T,flags=Fs} ->
            case S of
                #b_var{name=V} ->
                    #iclause{sub=Sub} = C,
                    {#b_var{name=get_vsub(V, Sub)},U,T,Fs};
                #b_literal{} ->
                    {S,U,T,Fs}
            end;
        #cg_map{op=exact,es=Es} ->
            sort(fun(A, B) ->
                         %% Keys are #b_var{} | #b_literal{}.
                         %% Literals will sort before variables
                         %% as intended.
                         erts_internal:cmp_term(A, B) < 0
                 end, [Key || #cg_map_pair{key=Key} <:- Es])
    end.

%%%
%%% Handling of errors and warnings (generated by the first pass).
%%%

maybe_add_warning([C|_], MatchAnno, St) ->
    maybe_add_warning(C, MatchAnno, St);
maybe_add_warning([], _MatchAnno, St) -> St;
maybe_add_warning(fail, _MatchAnno, St) -> St;
maybe_add_warning(Ke, MatchAnno, St) ->
    Anno = get_anno(Ke),
    case member(compiler_generated, Anno) of
        true ->
            St;
        false ->
            Warn = case get_location(MatchAnno) of
                       none ->
                           {nomatch,shadow};
                       {MatchLine,_} when is_integer(MatchLine) ->
                           {nomatch,{shadow,MatchLine}};
                       MatchLine when is_integer(MatchLine) ->
                           {nomatch,{shadow,MatchLine}}
                   end,
            add_warning(Anno, Warn, St)
    end.

add_warning(Anno, Term, #kern{ws=Ws}=St) ->
    Location = get_location(Anno),
    File = get_file(Anno),
    St#kern{ws=[{File,[{Location,?MODULE,Term}]}|Ws]}.

get_location([Line|_]) when is_integer(Line) ->
    Line;
get_location([{Line,Column}|_]) when is_integer(Line), is_integer(Column) ->
    {Line,Column};
get_location([_|T]) ->
    get_location(T);
get_location([]) ->
    none.

get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> "no_file".                      %Should not happen

%%%
%%% Second pass: Variable usage and lambda lifting.
%%%

%% ubody_used_vars(Expr, State) -> [UsedVar]
%%  Return all used variables for the body sequence. Much more
%%  efficient than using ubody/3 if the body contains nested letrecs.
ubody_used_vars(Expr, St) ->
    {_,Used,_} = ubody(Expr, return, St#kern{funs=ignore}),
    Used.

%% ubody(Expr, Break, State) -> {Expr,[UsedVar],State}.
%%  Tag the body sequence with its used variables.  These bodies
%%  either end with a #cg_break{}, #b_ret{} or, an expression
%%  which itself can return, such as #cg_match{}.

ubody(#ilet{vars=[],arg=#iletrec{}=Let,body=B0}, Br, St0) ->
    %% An iletrec{} should never be last.
    St = iletrec_funs(Let, St0),
    ubody(B0, Br, St);
ubody(#ilet{vars=[],arg=#b_literal{},body=B0}, Br, St0) ->
    ubody(B0, Br, St0);
ubody(#ilet{vars=[],arg=#b_var{},body=B0}, Br, St0) ->
    ubody(B0, Br, St0);
ubody(#ilet{vars=Vs,arg=E0,body=B0}, Br, St0) ->
    {E1,Eu,St1} = uexpr(E0, {break,Vs}, St0),
    {B1,Bu,St2} = ubody(B0, Br, St1),
    Ns = atomic_list_vars(Vs),
    Used = union(Eu, subtract(Bu, Ns)),         %Used external vars
    {#cg_seq{arg=E1,body=B1},Used,St2};
ubody(#ivalues{args=As}, {break,_Vbs}, St) ->
    Au = atomic_list_vars(As),
    {#cg_break{args=As},Au,St};
ubody(#cg_break{args=As}=Break, {break,_Vbs}, St) ->
    Au = atomic_list_vars(As),
    {Break,Au,St};
ubody(#b_ret{arg=Arg}=Ret, return, St) ->
    Used = atomic_vars(Arg),
    {Ret,Used,St};
ubody(#cg_goto{args=As}=Goto, _Br, St) ->
    Au = atomic_list_vars(As),
    {Goto,Au,St};
ubody(#cg_letrec_goto{}=E, return, St) ->
    uexpr(E, return, St);
ubody(#cg_match{}=E, return, St) ->
    uexpr(E, return, St);
ubody(#cg_try{}=E, return, St) ->
    uexpr(E, return, St);
ubody(E, return, St0) ->
    {Ea,Pa,St1} = force_atomic(E, St0),
    ubody(pre_seq(Pa, #b_ret{arg=Ea}), return, St1);
ubody(E, {break,[_]}=Break, St0) ->
    {Ea,Pa,St1} = force_atomic(E, St0),
    ubody(pre_seq(Pa, #cg_break{args=[Ea]}), Break, St1);
ubody(E, {break,Rs}=Break, St0) ->
    {Vs,St1} = new_vars(length(Rs), St0),
    PreSeq = #ilet{vars=Vs,arg=E,body=#cg_break{args=Vs}},
    ubody(PreSeq, Break, St1).

iletrec_funs(#iletrec{defs=Fs}, St0) ->
    %% Use union of all free variables.
    %% First just work out free variables for all functions.
    Free = foldl(fun ({_,#ifun{vars=Vs,body=Fb0}}, Free0) ->
                         Fbu = ubody_used_vars(Fb0, St0),
                         Ns = atomic_list_vars(Vs),
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
iletrec_funs_gen(Fs, FreeVs, St0) ->
    foldl(fun ({N,#ifun{anno=Fa,vars=Vs,body=Fb0}}, Lst0) ->
                  {Fb1,_,Lst1} = ubody(Fb0, return, Lst0),
                  Fun = make_ssa_function(Fa, N, Vs++FreeVs, Fb1, Lst1),
                  Lst1#kern{funs=[Fun|Lst1#kern.funs]}
          end, St0, Fs).

%% uexpr(Expr, Break, State) -> {Expr,[UsedVar],State}.
%%  Calculate the used variables for an expression.
%%  Break = return | {break,[RetVar]}.

uexpr(#cg_test{args=As}=Test, {break,Rs}, St) ->
    [] = Rs,                                    %Sanity check
    Used = atomic_list_vars(As),
    {Test,Used,St};
uexpr(#ilet{vars=Vs,arg=E0,body=B0}, {break,_}=Br, St0) ->
    Ns = atomic_list_vars(Vs),
    {E1,Eu,St1} = uexpr(E0, {break,Vs}, St0),
    {B1,Bu,St2} = uexpr(B0, Br, St1),
    Used = union(Eu, subtract(Bu, Ns)),
    {#cg_seq{arg=E1,body=B1},Used,St2};
uexpr(#cg_call{op=#b_local{name=#b_literal{val=F},arity=Ar}=Op,args=As0}=Call,
      {break,Rs0}, St0) ->
    {Rs,St} = ensure_return_vars(Rs0, St0),
    Free = get_free(F, Ar, St),
    As1 = As0 ++ Free,                          %Add free variables LAST!
    Used = atomic_list_vars(As1),
    {Call#cg_call{op=Op#b_local{arity=Ar + length(Free)},
                  args=As1,ret=Rs},Used,St};
uexpr(#cg_call{anno=A,op=Op,args=As}=Call0, {break,Rs0}, St0) ->
    {[R|Rs],St} = ensure_return_vars(Rs0, St0),
    Used = union(op_vars(Op), atomic_list_vars(As)),
    Call = Call0#cg_call{anno=A,ret=[R]},
    Seq = set_unused(Rs, Call),
    {Seq,Used,St};
uexpr(#cg_internal{args=As}=Internal, {break,Rs}, St0) ->
    Used = atomic_list_vars(As),
    {Brs,St1} = internal_returns(Internal, Rs, St0),
    {Internal#cg_internal{ret=Brs},Used,St1};
uexpr(#cg_match{body=B0}=Match, Br, St0) ->
    Rs = break_rets(Br),
    {B1,Bu,St1} = umatch(B0, Br, St0),
    {Match#cg_match{body=B1,ret=Rs},Bu,St1};
uexpr(#cg_try{arg=A0,vars=Vs,body=B0,evars=Evs,handler=H0}=Try, Br, St0) ->
    Rs = break_rets(Br),
    {Avs,St1} = new_vars(length(Vs), St0),
    {A1,Au,St2} = ubody(A0, {break,Avs}, St1),
    {B1,Bu,St3} = ubody(B0, Br, St2),
    {H1,Hu,St4} = ubody(H0, Br, St3),
    Used = union([Au,subtract(Bu, atomic_list_vars(Vs)),
                  subtract(Hu, atomic_list_vars(Evs))]),
    {Try#cg_try{arg=A1,vars=Vs,body=B1,evars=Evs,handler=H1,ret=Rs},
     Used,St4};
uexpr(#cg_catch{body=B0}=Catch, {break,Rs0}, St0) ->
    {Rb,St1} = new_var(St0),
    {B1,Bu,St2} = ubody(B0, {break,[Rb]}, St1),
    %% Guarantee ONE return variable.
    {Ns,St3} = new_vars(1 - length(Rs0), St2),
    Rs1 = Rs0 ++ Ns,
    {Catch#cg_catch{body=B1,ret=Rs1},Bu,St3};
uexpr(#ifun{anno=A,vars=Vs,body=B0}, {break,Rs}, St0) ->
    {B1,Bu,St1} = ubody(B0, return, St0),       %Return out of new function
    Ns = atomic_list_vars(Vs),
    Free = subtract(Bu, Ns),                    %Free variables in fun
    Fvs = make_vars(Free),
    Arity = length(Vs) + length(Free),
    {Fname,St2} =
        case keyfind(id, 1, A) of
            {id,{_,_,Fname0}} ->
                {Fname0,St1};
            false ->
                %% No id annotation. Must invent a fun name.
                new_fun_name(St1)
        end,
    Fun = make_ssa_function(A, Fname, Vs++Fvs, B1, St2),
    Local = #b_local{name=#b_literal{val=Fname},arity=Arity},
    {MakeFun,St3} = make_fun(Rs, Local, Fvs, St2),
    {MakeFun,Free,add_local_function(Fun, St3)};
uexpr(#b_local{name=#b_literal{val=Name},arity=Arity}=Local0, {break,Rs}, St0) ->
    Free = atomic_list_vars(get_free(Name, Arity, St0)),
    Fvs = make_vars(Free),
    FreeCount = length(Fvs),
    Local = Local0#b_local{arity=Arity+FreeCount},
    {MakeFun,St1} = make_fun(Rs, Local, Fvs, St0),
    {MakeFun,Free,St1};
uexpr(#cg_letrec_goto{vars=Vs,first=F0,then=T0}=LetrecGoto, Br, St0) ->
    Rs = break_rets(Br),
    Ns = atomic_list_vars(Vs),
    {F1,Fu,St1} = ubody(F0, Br, St0),
    {T1,Tu,St2} = ubody(T0, Br, St1),
    Used = subtract(union(Fu, Tu), Ns),
    {LetrecGoto#cg_letrec_goto{first=F1,then=T1,ret=Rs},Used,St2};
uexpr(#b_set{dst=none,args=Args}=Set, {break,[Dst]}, St) ->
    Used = atomic_list_vars(Args),
    {Set#b_set{dst=Dst},Used,St};
uexpr(#b_set{dst=none,args=Args}=Set0, {break,Rs0}, St0) ->
    Used = atomic_list_vars(Args),
    {[Dst|Ds],St1} = ensure_return_vars(Rs0, St0),
    Seq = set_unused(Ds, Set0#b_set{dst=Dst}),
    {Seq,Used,St1};
uexpr(#cg_succeeded{set=Set0}, {break,_}=Br, St0) ->
    {Set,Used,St1} = uexpr(Set0, Br, St0),
    {#cg_succeeded{set=Set},Used,St1};
uexpr(#cg_opaque{}=Opaque, _, St) ->
    {Opaque,[],St};
uexpr(Atomic, {break,[Dst]}, St0) ->
    Used = atomic_vars(Atomic),
    {#b_set{op=copy,dst=Dst,args=[Atomic]},Used,St0}.

make_fun(Rs, Local, FreeVars, St0) ->
    {[Dst],St1} = ensure_return_vars(Rs, St0),
    {#b_set{op=make_fun,dst=Dst,args=[Local|FreeVars]},St1}.

add_local_function(_, #kern{funs=ignore}=St) ->
    St;
add_local_function(#b_function{anno=Anno}=F,
                   #kern{funs=Funs}=St) ->
    FuncInfo = map_get(func_info, Anno),
    case is_defined(FuncInfo, Funs) of
        false ->
            St#kern{funs=[F|Funs]};
        true ->
            St
    end.

is_defined(FuncInfo, [#b_function{anno=Anno}|Fs]) ->
    case Anno of
        #{func_info := FuncInfo} -> true;
        #{} -> is_defined(FuncInfo, Fs)
    end;
is_defined(_, []) -> false.

set_unused([D|Ds], Seq) ->
    Copy = #b_set{op=copy,dst=D,args=[#b_literal{val=unused}]},
    set_unused(Ds, #cg_seq{arg=Copy,body=Seq});
set_unused([], Seq) -> Seq.

%% get_free(Name, Arity, State) -> [Free].
%% store_free(Name, Arity, [Free], State) -> State.

get_free(F, A, #kern{free=FreeMap}) ->
    case FreeMap of
        #{{F,A} := Val} -> Val;
        #{} -> []
    end.

store_free(F, A, Free, #kern{free=FreeMap0}=St) ->
    FreeMap = FreeMap0#{{F,A} => Free},
    St#kern{free=FreeMap}.

break_rets({break,Rs}) -> Rs;
break_rets(return) -> [].

%% internal_returns(Op, [Ret], State) -> {[Ret],State}.
%%  Fix return values for #cg_internal{}.

internal_returns(#cg_internal{op=Op,args=Args}, Rs, St0) ->
    Ar = length(Args),
    NumReturns = case {Op,Ar} of
                     {recv_peek_message,0} -> 2;
                     {_,_} -> 1
                 end,
    {Ns,St1} = new_vars(NumReturns - length(Rs), St0),
    {Rs ++ Ns,St1}.

%% ensure_return_vars([Ret], State) -> {[Ret],State}.

ensure_return_vars([], St) -> new_vars(1, St);
ensure_return_vars([_|_]=Rs, St) -> {Rs,St}.

%% umatch(Match, Break, State) -> {Match,[UsedVar],State}.
%%  Calculate the used variables for a match expression.

umatch(#cg_alt{first=F0,then=T0}=Alt, Br, St0) ->
    {F1,Fu,St1} = umatch(F0, Br, St0),
    {T1,Tu,St2} = umatch(T0, Br, St1),
    Used = union(Fu, Tu),
    {Alt#cg_alt{first=F1,then=T1},Used,St2};
umatch(#cg_select{var=#b_var{name=Var},types=Ts0}=Select, Br, St0) ->
    {Ts1,Tus,St1} = umatch_list(Ts0, Br, St0),
    Used = add_element(Var, Tus),
    {Select#cg_select{types=Ts1},Used,St1};
umatch(#cg_type_clause{values=Vs0}=TypeClause, Br, St0) ->
    {Vs1,Vus,St1} = umatch_list(Vs0, Br, St0),
    {TypeClause#cg_type_clause{values=Vs1},Vus,St1};
umatch(#cg_val_clause{val=P0,body=B0}=ValClause, Br, St0) ->
    {U0,Ps} = pat_vars(P0),
    {B1,Bu,St1} = umatch(B0, Br, St0),
    P = pat_mark_unused(P0, Bu, Ps),
    Used = union(U0, subtract(Bu, Ps)),
    {ValClause#cg_val_clause{val=P,body=B1},Used,St1};
umatch(#cg_guard{clauses=Gs0}=Guard, Br, St0) ->
    {Gs1,Gus,St1} = umatch_list(Gs0, Br, St0),
    {Guard#cg_guard{clauses=Gs1},Gus,St1};
umatch(#cg_guard_clause{guard=G0,body=B0}=GuardClause, Br, St0) ->
    {G1,Gu,St1} = uexpr(G0, {break,[]}, St0),
    {B1,Bu,St2} = umatch(B0, Br, St1),
    Used = union(Gu, Bu),
    {GuardClause#cg_guard_clause{guard=G1,body=B1},Used,St2};
umatch(B0, Br, St0) -> ubody(B0, Br, St0).

umatch_list(Ms0, Br, St) ->
    foldr(fun (M0, {Ms1,Us,Sta}) ->
                  {M1,Mu,Stb} = umatch(M0, Br, Sta),
                  {[M1|Ms1],union(Mu, Us),Stb}
          end, {[],[],St}, Ms0).

pat_mark_unused(#cg_tuple{es=Es0,keep=Keep}=P, Used0, Ps) ->
    %% Not extracting unused tuple elements is an optimization for
    %% compile time and memory use during compilation. It is probably
    %% worthwhile because it is common to extract only a few elements
    %% from a huge record.
    Used1 = ordsets:union(Used0, Keep),
    Used = intersection(Used1, Ps),
    Es = [case member(V, Used) of
              true -> Var;
              false -> #b_literal{val=unused}
          end || #b_var{name=V}=Var <- Es0],
    P#cg_tuple{es=Es};
pat_mark_unused(P, _Used, _Ps) -> P.

%% op_vars(Op) -> [VarName].

op_vars(#b_remote{mod=Mod,name=Name}) ->
    atomic_list_vars([Mod,Name]);
op_vars(Atomic) -> atomic_vars(Atomic).

%% atomic_vars(Literal) -> [VarName].
%%  Return the variables in an atomic (variable or literal).

atomic_vars(#b_var{name=N}) -> [N];
atomic_vars(#b_literal{}) -> [].

atomic_list_vars(Ps) ->
    foldl(fun (P, Vs) -> union(atomic_vars(P), Vs) end, [], Ps).

%% pat_vars(Pattern) -> {[UsedVarName],[NewVarName]}.
%%  Return variables in a pattern.  All variables are new variables
%%  except those in the size field of binary segments and the key
%%  field in map_pairs.

pat_vars(#b_var{name=N}) -> {[],[N]};
pat_vars(#b_literal{}) -> {[],[]};
pat_vars(#cg_cons{hd=H,tl=T}) ->
    pat_list_vars([H,T]);
pat_vars(#cg_binary{segs=V}) ->
    pat_vars(V);
pat_vars(#cg_bin_seg{size=Size,seg=S,next=N}) ->
    {U1,New} = pat_list_vars([S,N]),
    {[],U2} = pat_vars(Size),
    {union(U1, U2),New};
pat_vars(#cg_bin_int{size=Size,next=N}) ->
    {[],New} = pat_vars(N),
    {[],U} = pat_vars(Size),
    {U,New};
pat_vars(#cg_bin_end{}) -> {[],[]};
pat_vars(#cg_tuple{es=Es}) ->
    pat_list_vars(Es);
pat_vars(#cg_map{es=Es}) ->
    pat_list_vars(Es);
pat_vars(#cg_map_pair{key=K,val=V}) ->
    {U1,New} = pat_vars(V),
    {[],U2} = pat_vars(K),
    {union(U1, U2),New}.

pat_list_vars(Ps) ->
    foldl(fun (P, {Used0,New0}) ->
                  {Used,New} = pat_vars(P),
                  {union(Used0, Used),union(New0, New)} end,
          {[],[]}, Ps).

%%%
%%% Third pass: Translation to SSA code.
%%%

-type label() :: beam_ssa:label().

%% Main codegen structure for the SSA pass (formerly `beam_kernel_to_ssa`).
-record(cg, {lcount=1 :: label(),   %Label counter
             bfail=1 :: label(),
             catch_label=none :: 'none' | label(),
             vars=#{} :: map(),     %Defined variables.
             break=0 :: label(),    %Break label
             checks=[] :: [term()]
            }).

make_ssa_function(Anno0, Name, As, #cg_match{}=Body,
                  #kern{module=Mod,vcount=Count0}) ->
    Anno1 = line_anno(Anno0),
    Anno2 = Anno1#{func_info => {Mod,Name,length(As)}},
    St0 = #cg{lcount=Count0},
    {Asm,St} = cg_fun(Body, St0),
    #cg{checks=Checks,lcount=Count} = St,
    Anno = case Checks of
               [] ->
                   Anno2;
               [_|_] ->
                   Anno2#{ssa_checks => Checks}
           end,
    #b_function{anno=Anno,args=As,bs=Asm,cnt=Count};
make_ssa_function(Anno, Name, As, Body, St) ->
    Match = #cg_match{body=Body,ret=[]},
    make_ssa_function(Anno, Name, As, Match, St).

cg_fun(Ke, St0) ->
    {FailIs,St1} = make_exception_block(St0),
    {B,St} = cg(Ke, St1),
    Asm0 = [{label,0}|B++FailIs],
    Asm = fix_phis(Asm0),
    {build_map(Asm),St}.

make_exception_block(St0) ->
    {Dst,St} = new_ssa_var(St0),
    Is = [{label,?EXCEPTION_BLOCK},
          #b_set{op=call,dst=Dst,
                 args=[#b_remote{mod=#b_literal{val=erlang},
                                 name=#b_literal{val=error},
                                 arity=1},
                       #b_literal{val=badarg}]},
          #b_ret{arg=Dst}],
    {Is,St#cg{bfail=?EXCEPTION_BLOCK}}.

%% cg(Lkexpr, State) -> {[Ainstr],State}.
%%  Generate SSA code.

cg(#b_set{op=copy,dst=#b_var{name=Dst},args=[Arg0]}, St0) ->
    %% Create an alias for a variable or literal.
    Arg = ssa_arg(Arg0, St0),
    St = set_ssa_var(Dst, Arg, St0),
    {[],St};
cg(#b_set{anno=Anno0,op=debug_line,args=Args0}=Set0, St) ->
    Args = ssa_args(Args0, St),
    Literals = [{Val,From} || From := #b_literal{val=Val} <- St#cg.vars],
    Anno1 = Anno0#{literals => Literals},
    NewAlias = [{To,From} || From := #b_var{name=To} <- St#cg.vars],
    case NewAlias of
        [_|_] ->
            Alias0 = maps:get(alias, Anno0, #{}),
            Alias1 = foldl(fun({To,From}, A) ->
                                   case A of
                                       #{To := Vars0} ->
                                           Vars1 = ordsets:add_element(From, Vars0),
                                           A#{To := Vars1};
                                       #{} ->
                                           A#{To => [From]}
                                   end
                           end, Alias0, NewAlias),
            Anno = Anno1#{alias => Alias1},
            Set = Set0#b_set{anno=Anno,args=Args},
            {[Set],St};
        [] ->
            {[Set0#b_set{anno=Anno1,args=Args}],St}
    end;
cg(#b_set{args=Args0}=Set0, St) ->
    Args = ssa_args(Args0, St),
    Set = Set0#b_set{args=Args},
    {[Set],St};
cg(#b_ret{arg=Ret0}, St) ->
    Ret = ssa_arg(Ret0, St),
    {[#b_ret{arg=Ret}],St};
cg(#cg_succeeded{set=Set0}, St0) ->
    {[#b_set{dst=Dst}=Set],St1} = cg(Set0, St0),
    FailCtx = fail_context(St1),
    {Is,St} = make_succeeded(Dst, FailCtx, St1),
    {[Set|Is],St};
cg(#cg_match{body=M,ret=Rs}, #cg{bfail=Bfail,break=OldBreak}=St0) ->
    {B,St1} = new_label(St0),
    {Mis,St2} = match_cg(M, Bfail, St1#cg{break=B}),
    St = St2#cg{break=OldBreak},
    {Mis ++ [{label,B},#cg_phi{vars=Rs}],St};
cg(#cg_seq{arg=Arg,body=Body}, St0) ->
    {ArgIs,St1} = cg(Arg, St0),
    {BodyIs,St} = cg(Body, St1),
    {ArgIs ++ BodyIs,St};
cg(#cg_call{anno=Anno,op=Func,args=As,ret=Rs}, St) ->
    call_cg(Func, As, Rs, Anno, St);
cg(#cg_internal{anno=Anno,op=Op,args=As0,ret=Rs}, St) ->
    As = ssa_args(As0, St),
    internal_cg(Anno, Op, As, Rs, St);
cg(#cg_try{arg=Ta,vars=Vs,body=Tb,evars=Evs,handler=Th,ret=Rs}, St) ->
    try_cg(Ta, Vs, Tb, Evs, Th, Rs, St);
cg(#cg_catch{body=Cb,ret=[R]}, St) ->
    catch_cg(Cb, R, St);
cg(#cg_break{args=Bs}, #cg{break=Br}=St) ->
    Args = ssa_args(Bs, St),
    {[#cg_break{args=Args,phi=Br}],St};
cg(#cg_letrec_goto{label=Tf,vars=Vs,first=First,then=Then,ret=BreakVars},
   #cg{break=OldBreak}=St0) ->
    {B,St1} = new_label(St0),
    {Fis,St2} = cg(First, St1#cg{break=B}),
    {Sis,St} = cg(Then, St2),
    PostPhi = #cg_phi{vars=BreakVars},
    FailPhi = case Vs of
                  [] -> [];
                  [_|_] -> [#cg_phi{vars=Vs}]
              end,
    {Fis ++ [{label,Tf}] ++ FailPhi ++ Sis ++ [{label,B},PostPhi],
     St#cg{break=OldBreak}};
cg(#cg_goto{label=Label,args=[]}, St) ->
    {[make_uncond_branch(Label)],St};
cg(#cg_goto{label=Label,args=As0}, St) ->
    As = ssa_args(As0, St),
    Break = #cg_break{args=As,phi=Label},
    {[Break],St};
cg(#cg_opaque{val=Check}, St) ->
    {ssa_check_when,_,_,_,_} = Check,           %Assertion.
    {[],St#cg{checks=[Check|St#cg.checks]}}.

%% match_cg(Match, Fail, State) -> {[Ainstr],State}.
%%  Generate code for a match tree.

match_cg(#cg_alt{first=F,then=S}, Fail, St0) ->
    {Tf,St1} = new_label(St0),
    {Fis,St2} = match_cg(F, Tf, St1),
    St3 = restore_vars(St1, St2),
    {Sis,St4} = match_cg(S, Fail, St3),
    St5 = restore_vars(St3, St4),
    {Fis ++ [{label,Tf}] ++ Sis,St5};
match_cg(#cg_select{var=#b_var{}=Src0,types=Scs}, Fail, St) ->
    Src = ssa_arg(Src0, St),
    match_fmf(fun (#cg_type_clause{type=Type,values=Vs}, F, Sta) ->
                      select_cg(Type, Vs, Src, F, Fail, Sta)
              end, Fail, St, Scs);
match_cg(#cg_guard{clauses=Gcs}, Fail, St) ->
    match_fmf(fun (G, F, Sta) ->
                      guard_clause_cg(G, F, Sta)
              end, Fail, St, Gcs);
match_cg(Ke, _Fail, St0) ->
    cg(Ke, St0).

%% select_cg(Type, [ValueClause], Src, TypeFail, ValueFail, State) ->
%%       {Is,State}.
%%  Selecting type and value needs two failure labels, TypeFail is the
%%  label to jump to of the next type test when this type fails, and
%%  ValueFail is the label when this type is correct but the value is
%%  wrong.  These are different as in the second case there is no need
%%  to try the next type, as it will always fail.

select_cg(cg_binary, [S], Var, Tf, Vf, St) ->
    select_binary(S, Var, Tf, Vf, St);
select_cg(cg_bin_seg, Vs, Var, Tf, _Vf, St) ->
    select_bin_segs(Vs, Var, Tf, St);
select_cg(cg_bin_int, Vs, Var, Tf, _Vf, St) ->
    select_bin_segs(Vs, Var, Tf, St);
select_cg(cg_bin_end, [S], Var, Tf, _Vf, St) ->
    select_bin_end(S, Var, Tf, St);
select_cg(cg_map, Vs, Var, Tf, Vf, St) ->
    select_map(Vs, Var, Tf, Vf, St);
select_cg(cg_cons, [S], Var, Tf, Vf, St) ->
    select_cons(S, Var, Tf, Vf, St);
select_cg(cg_nil, [_]=Vs, Var, Tf, Vf, St) ->
    select_literal(Vs, Var, Tf, Vf, St);
select_cg(b_literal, Vs, Var, Tf, Vf, St) ->
    select_literal(Vs, Var, Tf, Vf, St);
select_cg(Type, Scs, Var, Tf, Vf, St0) ->
    {Vis,St1} =
        mapfoldl(fun (S, Sta) ->
                         {Val,Is,Stb} = select_val(S, Var, Vf, Sta),
                         Stc = restore_vars(Sta, Stb),
                         {{Is,[Val]},Stc}
                 end, St0, Scs),
    OptVls = combine(lists:sort(combine(Vis))),
    {Vls,Sis,St2} = select_labels(OptVls, St1, [], []),
    select_val_cg(Type, Var, Vls, Tf, Vf, Sis, St2).

select_val_cg({bif,is_atom}, {bool,Dst}, Vls, _Tf, _Vf, Sis, St) ->
    %% Generate a br instruction for a known boolean value from
    %% the `wait_timeout` instruction.
    #b_var{} = Dst,                             %Assertion.
    [{#b_literal{val=false},Fail},{#b_literal{val=true},Succ}] = sort(Vls),
    Br = #b_br{bool=Dst,succ=Succ,fail=Fail},
    {[Br|Sis],St};
select_val_cg({bif,is_atom}, {{succeeded,_}=SuccOp,Dst}, Vls, _Tf, _Vf, Sis, St0) ->
    [{#b_literal{val=false},Fail},{#b_literal{val=true},Succ}] = sort(Vls),
    #b_var{} = Dst,                             %Assertion.
    %% Generate a `{succeeded,guard}` instruction and two-way branch
    %% following the `peek_message` instruction.
    {Cond,St} = make_cond(SuccOp, [Dst], Fail, Succ, St0),
    {Cond ++ Sis,St};
select_val_cg(cg_tuple, Tuple, Vls, Tf, Vf, Sis, St0) ->
    {Is0,St1} = make_cond_branch({bif,is_tuple}, [Tuple], Tf, St0),
    {Arity,St2} = new_ssa_var(St1),
    GetArity = #b_set{op={bif,tuple_size},dst=Arity,args=[Tuple]},
    {Is,St} = select_val_cg({bif,is_integer}, Arity, Vls, Vf, Vf, Sis, St2),
    {Is0 ++ [GetArity|Is],St};
select_val_cg(Type, R, Vls, Tf, Vf, Sis, St0) ->
    {TypeIs,St1} =
        if
            Tf =:= Vf ->
                %% The type and value failure labels are the same; we
                %% don't need a type test.
                {[],St0};
            true ->
                %% Different labels for type failure and value
                %% failure; we need a type test.
                make_cond_branch(Type, [R], Tf, St0)
        end,
    case Vls of
        [{Val,Succ}] ->
            {Is,St} = make_cond({bif,'=:='}, [R,Val], Vf, Succ, St1),
            {TypeIs++Is++Sis,St};
        [_|_] ->
            {TypeIs++[#b_switch{arg=R,fail=Vf,list=Vls}|Sis],St1}
    end.

combine([{Is,Vs1},{Is,Vs2}|Vis]) -> combine([{Is,Vs1 ++ Vs2}|Vis]);
combine([V|Vis]) -> [V|combine(Vis)];
combine([]) -> [].

select_labels([{Is,Vs}|Vis], St0, Vls, Sis) ->
    {Lbl,St1} = new_label(St0),
    select_labels(Vis, St1, add_vls(Vs, Lbl, Vls), [{label,Lbl}|Is] ++ Sis);
select_labels([], St, Vls, Sis) ->
    {Vls,Sis,St}.

add_vls([V|Vs], Lbl, Acc) ->
    add_vls(Vs, Lbl, [{V,Lbl}|Acc]);
add_vls([], _, Acc) -> Acc.

select_literal(S, Src, Tf, Vf, St) ->
    F = fun(ValClause, Fail, St0) ->
                {Val,ValIs,St1} = select_val(ValClause, Src, Vf, St0),
                Args = [Src,Val],
                {Is,St2} = make_cond_branch({bif,'=:='}, Args, Fail, St1),
                {Is++ValIs,St2}
        end,
    match_fmf(F, Tf, St, S).

select_cons(#cg_val_clause{val=#cg_cons{hd=Hd,tl=Tl},body=B},
            Src, Tf, Vf, St0) ->
    {Bis,St1} = match_cg(B, Vf, St0),
    Args = [Src],
    {Is,St} = make_cond_branch(is_nonempty_list, Args, Tf, St1),
    GetHd = #b_set{op=get_hd,dst=Hd,args=Args},
    GetTl = #b_set{op=get_tl,dst=Tl,args=Args},
    {Is ++ [GetHd,GetTl|Bis],St}.

select_binary(#cg_val_clause{val=#cg_binary{segs=#b_var{}=Ctx},body=B},
              Src, Tf, Vf, St0) ->
    {Bis0,St1} = match_cg(B, Vf, St0),
    Bis1 = finish_bs_matching(Bis0),
    {TestIs,St} = make_succeeded(Ctx, {guard,Tf}, St1),
    Bis = [#b_set{op=bs_start_match,dst=Ctx,
                  args=[#b_literal{val=new},Src]}] ++ TestIs ++ Bis1,
    {Bis,St}.

finish_bs_matching([#b_set{op=bs_match,
                           args=[#b_literal{val=string},Ctx,
                                 #b_literal{val=BinList}]}=Set|Is])
  when is_list(BinList) ->
    I = Set#b_set{args=[#b_literal{val=string},Ctx,
                        #b_literal{val=list_to_bitstring(BinList)}]},
    finish_bs_matching([I|Is]);
finish_bs_matching([I|Is]) ->
    [I|finish_bs_matching(Is)];
finish_bs_matching([]) -> [].

%% Instructions for selection of binary segments.

select_bin_segs(Scs, #b_var{}=Ctx, Tf, St) ->
    match_fmf(fun(S, Fail, Sta) ->
                      select_bin_seg(S, Ctx, Fail, Sta)
              end, Tf, St, Scs).

select_bin_seg(#cg_val_clause{val=#cg_bin_seg{seg=Dst,next=Next}=Seg,
                              body=B,anno=Anno},
               Ctx, Fail, St0) ->
    LineAnno = line_anno(Anno),
    {Mis,St1} = select_extract_bin(Seg, Ctx, Fail, LineAnno, St0),
    {Bis,St} = match_cg(B, Fail, St1),
    BsGet = #b_set{op=bs_extract,dst=Dst,args=[ssa_arg(Next, St)]},
    Is = Mis ++ [BsGet] ++ Bis,
    {Is,St};
select_bin_seg(#cg_val_clause{val=#cg_bin_int{}=Seg,body=B},
               Ctx, Fail, St0) ->
    {Mis,St1} = select_extract_int(Seg, Fail, Ctx, St0),
    {Bis,St} = match_cg(B, Fail, St1),
    case Mis ++ Bis of
        [#b_set{op=bs_match,
                args=[#b_literal{val=string},OtherCtx1,Bin1]},
         #b_set{op={succeeded,guard},dst=Bool1},
         #b_br{bool=Bool1,succ=Succ,fail=Fail},
         {label,Succ},
         #b_set{op=bs_match,dst=Dst,
                args=[#b_literal{val=string},_OtherCtx2,Bin2]} |
         [#b_set{op={succeeded,guard},dst=Bool2},
          #b_br{bool=Bool2,fail=Fail}|_]=Is] ->
            %% We used to do this optimization later, but it turns out
            %% that in huge functions with many string matching
            %% instructions, it's a huge win to do the combination
            %% now. To avoid copying the binary data again and again,
            %% we'll combine bitstrings in a list and convert all of
            %% it to a bitstring later.
            {#b_literal{val=B1},#b_literal{val=B2}} = {Bin1,Bin2},
            Bin = #b_literal{val=[B1,B2]},
            Set = #b_set{op=bs_match,dst=Dst,
                         args=[#b_literal{val=string},OtherCtx1,Bin]},
            {[Set|Is],St};
        Is0 ->
            {Is0,St}
    end.

select_bin_end(#cg_val_clause{val=#cg_bin_end{},body=B}, #b_var{}=Ctx, Tf, St0) ->
    {Bis,St1} = match_cg(B, Tf, St0),
    {TestIs,St} = make_cond_branch(bs_test_tail, [Ctx,#b_literal{val=0}], Tf, St1),
    {TestIs ++ Bis,St}.

select_extract_bin(#cg_bin_seg{type=Type,size=Size0,unit=Unit0,
                               flags=Flags0,next=Dst},
                   Ctx, Fail, Anno, St0) ->
    Size = case {Size0,ssa_arg(Size0, St0)} of
               {#b_var{},#b_literal{val=all}} ->
                   %% The size `all` is used for the size of the final
                   %% binary segment in a pattern. Using `all`
                   %% explicitly is not allowed, so we convert it to
                   %% an obvious invalid size. Example:
                   %%
                   %%     All = all,
                   %%     <<Val:All/binary>> = Bin
                   %%
                   #b_literal{val=bad_size};
               {_,Size1} ->
                   Size1
           end,
    Unit = #b_literal{val=Unit0},
    Flags = #b_literal{val=Flags0},
    TypeArg = #b_literal{val=Type},
    Args = [TypeArg,Ctx,Flags|
            case bs_need_size(Type) of
                true -> [Size,Unit];
                false -> []
            end],
    BsMatch = #b_set{anno=Anno,op=bs_match,dst=Dst,args=Args},
    {Is,St} = make_succeeded(Dst, {guard,Fail}, St0),
    {[BsMatch|Is],St}.

bs_need_size(utf8) -> false;
bs_need_size(utf16) -> false;
bs_need_size(utf32) -> false;
bs_need_size(_) -> true.

select_extract_int(#cg_bin_int{val=0,size=#b_literal{val=0},
                               next=#b_var{name=Tl}},
                   _Fail, Ctx, St0) ->
    %% Example:
    %%     <<..., 0:0, ...>> = Bin,
    St = set_ssa_var(Tl, Ctx, St0),
    {[],St};
select_extract_int(#cg_bin_int{val=Val,size=#b_literal{val=Sz},
                               unit=U,flags=Fs,next=#b_var{}=Dst},
                   Fail, Ctx, St0) when is_integer(Sz), is_integer(U) ->
    Bits = U * Sz,
    Bin = case member(big, Fs) of
              true ->
                  <<Val:Bits>>;
              false ->
                  true = member(little, Fs),    %Assertion.
                  <<Val:Bits/little>>
          end,
    Bits = bit_size(Bin),                       %Assertion.
    {TestIs,St} = make_succeeded(Dst, {guard,Fail}, St0),
    Set = #b_set{op=bs_match,dst=Dst,
                 args=[#b_literal{val=string},Ctx,#b_literal{val=Bin}]},
    {[Set|TestIs],St}.

select_val(#cg_val_clause{val=#cg_tuple{es=Es},body=B}, V, Vf, St0) ->
    Eis = select_extract_tuple(Es, 0, V),
    {Bis,St1} = match_cg(B, Vf, St0),
    {#b_literal{val=length(Es)},Eis ++ Bis,St1};
select_val(#cg_val_clause{val=#b_literal{}=Val,body=B}, _V, Vf, St0) ->
    {Bis,St1} = match_cg(B, Vf, St0),
    {Val,Bis,St1}.

select_extract_tuple([E|Es], Index, Tuple) ->
    case E of
        #b_var{} ->
            Args = [Tuple,#b_literal{val=Index}],
            Get = #b_set{op=get_tuple_element,dst=E,args=Args},
            [Get|select_extract_tuple(Es, Index+1, Tuple)];
        #b_literal{val=unused} ->
            %% Not extracting tuple elements that are not used is an
            %% optimization for compile time and memory use during
            %% compilation, which is probably worthwhile because it is
            %% common to extract only a few elements from a huge
            %% record.
            select_extract_tuple(Es, Index + 1, Tuple)
    end;
select_extract_tuple([], _, _) -> [].

select_map(Scs, MapSrc, Tf, Vf, St0) ->
    {Is,St1} =
        match_fmf(fun(#cg_val_clause{val=#cg_map{op=exact,es=Es},
                                     body=B}, Fail, St1) ->
                          select_map_val(MapSrc, Es, B, Fail, St1)
                  end, Vf, St0, Scs),
    {TestIs,St} = make_cond_branch({bif,is_map}, [MapSrc], Tf, St1),
    {TestIs++Is,St}.

select_map_val(MapSrc, Es, B, Fail, St0) ->
    {Eis,St1} = select_extract_map(Es, MapSrc, Fail, St0),
    {Bis,St2} = match_cg(B, Fail, St1),
    {Eis++Bis,St2}.

select_extract_map([P|Ps], MapSrc, Fail, St0) ->
    #cg_map_pair{key=Key0,val=Dst} = P,
    Key = ssa_arg(Key0, St0),
    Set = #b_set{op=get_map_element,dst=Dst,args=[MapSrc,Key]},
    {TestIs,St1} = make_succeeded(Dst, {guard,Fail}, St0),
    {Is,St} = select_extract_map(Ps, MapSrc, Fail, St1),
    {[Set|TestIs]++Is,St};
select_extract_map([], _, _, St) ->
    {[],St}.

guard_clause_cg(#cg_guard_clause{guard=G,body=B}, Fail, St0) ->
    {Gis,St1} = guard_cg(G, Fail, St0),
    {Bis,St} = match_cg(B, Fail, St1),
    {Gis ++ Bis,St}.

%% guard_cg(Guard, Fail, State) -> {[Ainstr],State}.
%%  A guard is a boolean expression of tests.  Tests return true or
%%  false.  A fault in a test causes the test to return false.  Tests
%%  never return the boolean, instead we generate jump code to go to
%%  the correct exit point.  Primops and tests all go to the next
%%  instruction on success or jump to a failure label.

guard_cg(#cg_try{arg=Ts,vars=[],body=#cg_break{args=[]},
                 evars=[],handler=#cg_break{args=[]}},
         Fail,
         #cg{bfail=OldBfail,break=OldBreak}=St0) ->
    %% Do a try/catch without return value for effect. The return
    %% value is not checked; success passes on to the next instruction
    %% and failure jumps to Fail.
    {Next,St1} = new_label(St0),
    {Tis,St2} = guard_cg(Ts, Fail, St1#cg{bfail=Fail,break=Next}),
    Is = Tis ++ [{label,Next},#cg_phi{vars=[]}],
    {Is,St2#cg{bfail=OldBfail,break=OldBreak}};
guard_cg(#cg_test{op=Test,args=As}, Fail, St0) when is_atom(Test) ->
    test_cg(Test, false, As, Fail, St0);
guard_cg(#cg_seq{arg=Arg,body=Body}, Fail, St0) ->
    {ArgIs,St1} = guard_cg(Arg, Fail, St0),
    {BodyIs,St} = guard_cg(Body, Fail, St1),
    {ArgIs++BodyIs,St};
guard_cg(#cg_succeeded{set=Set0}, Fail, St0) ->
    {[#b_set{dst=Dst}=Set],St1} = cg(Set0, St0),
    {Is,St} = make_succeeded(Dst, {guard, Fail}, St1),
    {[Set|Is],St};
guard_cg(G, _Fail, St) ->
    cg(G, St).

test_cg('=/=', Inverted, As, Fail, St) ->
    test_cg('=:=', not Inverted, As, Fail, St);
test_cg('/=', Inverted, As, Fail, St) ->
    test_cg('==', not Inverted, As, Fail, St);
test_cg(Test, Inverted, As0, Fail, St0) ->
    As = ssa_args(As0, St0),
    {Succ,St} = new_label(St0),
    Bool = #b_var{name=Succ},
    Bif = #b_set{op={bif,Test},dst=Bool,args=As},
    Br = case Inverted of
             false ->
                 #b_br{bool=Bool,succ=Succ,fail=Fail};
             true ->
                 #b_br{bool=Bool,succ=Fail,fail=Succ}
         end,
    {[Bif,Br,{label,Succ}],St}.

%% match_fmf(Fun, LastFail, State, [Clause]) -> {Is,State}.
%%  This is a special flatmapfoldl for match code gen where we
%%  generate a "failure" label for each clause. The last clause uses
%%  an externally generated failure label, LastFail.  N.B. We do not
%%  know or care how the failure labels are used.

match_fmf(F, LastFail, St0, [H]) ->
    {R,St1} = F(H, LastFail, St0),
    {R,restore_vars(St0, St1)};
match_fmf(F, LastFail, St0, [H|T]) ->
    {Fail,St1} = new_label(St0),
    {R,St2} = F(H, Fail, St1),
    St3 = restore_vars(St1, St2),
    {Rs,St4} = match_fmf(F, LastFail, St3, T),
    {R ++ [{label,Fail}] ++ Rs,St4}.

%% restore_vars(PreviousState, CurrentSt) -> UpdatedCurrentState.
%%  Restore variables to their previous state. When exiting a scope,
%%  any substitutions that are no longer applicable will be
%%  discarded. More importantly, when generating BEAM debug
%%  information, variables bound to literal values will only appear in
%%  `debug_line` instructions if they are still in scope.

restore_vars(St0, St) ->
    St#cg{vars=St0#cg.vars}.

%% fail_context(State) -> {body | guard, FailureLabel}.
%%  Return an indication of which part of a function code is
%%  being generated for and the appropriate failure label to
%%  use.

fail_context(#cg{catch_label=Catch,bfail=Fail}) ->
    if
        Fail =/= ?EXCEPTION_BLOCK ->
            {guard,Fail};
        Catch =:= none ->
            {body,Fail};
        is_integer(Catch) ->
            {body,Catch}
    end.

%% call_cg(Func, [Arg], [Ret], Le, State) ->
%%      {[Ainstr],State}.
%%  Generate code for call.

call_cg(Func, As, [Dst], Le, St0) ->
    case fail_context(St0) of
        {guard,Fail} ->
            %% Inside a guard. The only allowed function call is to
            %% erlang:error/1,2. We will generate a branch to the
            %% failure branch.
            #b_remote{mod=#b_literal{val=erlang},
                      name=#b_literal{val=error}} = Func, %Assertion.
            {[make_uncond_branch(Fail),#cg_unreachable{}],St0};
        FailCtx ->
            %% Ordinary function call in a function body.
            Args = ssa_args([Func|As], St0),
            Call = #b_set{anno=line_anno(Le),op=call,dst=Dst,args=Args},
            {TestIs,St} = make_succeeded(Dst, FailCtx, St0),
            {[Call|TestIs],St}
    end.

internal_anno(Le) ->
    Anno = line_anno(Le),
    case keyfind(inlined, 1, Le) of
        false -> Anno;
        {inlined, NameArity} -> Anno#{inlined => NameArity}
    end.

%% internal_cg(Anno, Op, [Arg], [Ret], State) ->
%%      {[Ainstr],State}.
internal_cg(_Anno, is_record, [Tuple,TagVal,ArityVal], [Dst], St0) ->
    {Arity,St1} = new_ssa_var(St0),
    {Tag,St2} = new_ssa_var(St1),
    {Phi,St3} = new_label(St2),
    {False,St4} = new_label(St3),
    {Is0,St5} = make_cond_branch({bif,is_tuple}, [Tuple], False, St4),
    GetArity = #b_set{op={bif,tuple_size},dst=Arity,args=[Tuple]},
    {Is1,St6} = make_cond_branch({bif,'=:='}, [Arity,ArityVal], False, St5),
    GetTag = #b_set{op=get_tuple_element,dst=Tag,
                    args=[Tuple,#b_literal{val=0}]},
    {Is2,St} = make_cond_branch({bif,'=:='}, [Tag,TagVal], False, St6),
    Is3 = [#cg_break{args=[#b_literal{val=true}],phi=Phi},
           {label,False},
           #cg_break{args=[#b_literal{val=false}],phi=Phi},
           {label,Phi},
           #cg_phi{vars=[Dst]}],
    Is = Is0 ++ [GetArity] ++ Is1 ++ [GetTag] ++ Is2 ++ Is3,
    {Is,St};
internal_cg(Anno, recv_peek_message, [], [#b_var{name=Succeeded0},
                                          #b_var{}=Dst], St0) ->
    St = new_succeeded_value(Succeeded0, Dst, St0),
    Set = #b_set{anno=Anno,
                 op=peek_message,
                 dst=Dst,
                 args=[#b_literal{val=none}]},
    {[Set],St};
internal_cg(_Anno, recv_wait_timeout, As, [#b_var{name=Succeeded0}], St0) ->
    %% Note that the `wait_timeout` instruction can potentially branch in three
    %% different directions:
    %%
    %% * A new message is available in the message queue. `wait_timeout`
    %%   branches to the given label.
    %%
    %% * The timeout expired. `wait_timeout` transfers control to the next
    %%   instruction.
    %%
    %% * The value for timeout duration is invalid (either not an integer or
    %%   negative or too large). A `timeout_value` exception will be raised.
    %%
    %% `wait_timeout` will be represented like this in SSA code:
    %%
    %%       WaitBool = wait_timeout TimeoutValue
    %%       Succeeded = succeeded:body WaitBool
    %%       br Succeeded, ^good_timeout_value, ^bad_timeout_value
    %%
    %%   good_timeout_value:
    %%       br WaitBool, ^timeout_expired, ^new_message_received
    %%
    Args = ssa_args(As, St0),
    {Wait,St1} = new_ssa_var(St0),
    {Succ,St2} = make_succeeded(Wait, fail_context(St1), St1),
    St = new_bool_value(Succeeded0, Wait, St2),
    Set = #b_set{op=wait_timeout,dst=Wait,args=Args},
    {[Set|Succ],St}.

%% try_cg(TryBlock, [BodyVar], TryBody, [ExcpVar], TryHandler, [Ret], St) ->
%%         {[Ainstr],St}.

try_cg(Ta, SsaVs, Tb, SsaEvs, Th, BreakVars, St0) ->
    case try_cg_guard(Ta, SsaVs, Tb, Th, BreakVars, St0) of
        not_possible ->
            %% The general try/catch (not in a guard).
            {B,St1} = new_label(St0),           %Body label
            {H,St2} = new_label(St1),           %Handler label
            {E,St3} = new_label(St2),           %End label
            {Next,St4} = new_label(St3),
            {TryTag,St5} = new_ssa_var(St4),
            {Ais,St6} = cg(Ta, St5#cg{break=B,catch_label=H}),
            St7 = St6#cg{break=E,catch_label=St5#cg.catch_label},
            {Bis,St8} = cg(Tb, St7),
            {His,St9} = cg(Th, St8),
            {Handler,St10} = try_handler(H, TryTag, SsaEvs, St9),
            {KillTryTag,St} = kill_try_tag(TryTag, St10),
            {[#b_set{op=new_try_tag,dst=TryTag,args=[#b_literal{val='try'}]},
              #b_br{bool=TryTag,succ=Next,fail=H},
              {label,Next}] ++ Ais ++
                 [{label,B},#cg_phi{vars=SsaVs},KillTryTag] ++ Bis ++
                 Handler ++ His ++
                 [{label,E},#cg_phi{vars=BreakVars}],
             St#cg{break=St0#cg.break}};
        Result ->
            Result
    end.

try_cg_guard(Ta, SsaVs, Tb, Th, BreakVars, St0) ->
    {B,St1} = new_label(St0),                   %Body label
    {H,St2} = new_label(St1),                   %Handler label
    {Ais,_} = cg(Ta, St2#cg{break=B,catch_label=H}),

    %% We try to avoid constructing a try/catch if the expression to
    %% be evaluated don't have any side effects and if the error
    %% reason is not explicitly matched.
    %%
    %% Starting in OTP 23, segment sizes in binary matching and keys
    %% in map matching are allowed to be arbitrary guard
    %% expressions. Those expressions are evaluated in a try/catch
    %% so that matching can continue with the next clause if the evaluation
    %% of such expression fails.
    %%
    %% It is not allowed to use try/catch during matching in a receive
    %% (the try/catch would force the saving of fragile message references
    %% to the stack frame). Therefore, avoiding creating try/catch is
    %% not merely an optimization but necessary for correctness.

    case is_guard_cg_safe_list(Ais) of
        true ->
            %% All instructions are suitable in a guard. Check
            %% whether the exception is matched.
            {ProtIs,St3} = guard_cg(Ta, H, St2#cg{break=B,bfail=H}),
            case {SsaVs,Tb,Th} of
                {[#b_var{name=X}],#cg_break{args=[#b_var{name=X}]},
                 #cg_break{args=[#b_literal{}]}} ->
                    {His,St} = cg(Th, St3),
                    Is = ProtIs ++ [{label,H}] ++ His ++
                        [{label,B},#cg_phi{vars=BreakVars}],
                    {Is,St#cg{break=St0#cg.break,bfail=St0#cg.bfail}};
                {[#b_var{name=X}],#cg_break{args=[#b_literal{}=SuccLit,#b_var{name=X}]},
                 #cg_break{args=[#b_literal{val=false},#b_literal{}]}} ->
                    %% This code probably evaluates a key expression
                    %% in map matching.
                    {FinalLabel,St4} = new_label(St3),
                    {His,St5} = cg(Th, St4#cg{break=FinalLabel}),
                    {Result,St} = new_ssa_var(St5),
                    Is = ProtIs ++ [{label,H}] ++ His ++
                        [{label,B},
                         #cg_phi{vars=[Result]},
                         #cg_break{args=[SuccLit,Result],phi=FinalLabel},
                         {label,FinalLabel},
                         #cg_phi{vars=BreakVars}],
                    {Is,St#cg{break=St0#cg.break,bfail=St0#cg.bfail}};
                {_,#cg_break{args=[]},#cg_break{args=[]}} ->
                    %% This code probably does the size calculation
                    %% for a segment in binary matching.
                    {His,St} = cg(Th, St3),
                    Is = ProtIs ++ [{label,H}] ++ His ++
                        [{label,B},#cg_phi{vars=BreakVars}],
                    {Is,St#cg{break=St0#cg.break,bfail=St0#cg.bfail}};
                {_,_,_} ->
                    not_possible
            end;
        false ->
            not_possible
    end.

is_guard_cg_safe_list(Is) ->
    all(fun is_guard_cg_safe/1, Is).

is_guard_cg_safe(#b_set{op=call,args=Args}) ->
    case Args of
        [#b_remote{mod=#b_literal{val=erlang},
                   name=#b_literal{val=error},
                   arity=1}|_] ->
            true;
        _ ->
            false
    end;
is_guard_cg_safe(#b_set{}=I) -> not beam_ssa:clobbers_xregs(I);
is_guard_cg_safe(#b_br{}) -> true;
is_guard_cg_safe(#b_switch{}) -> true;
is_guard_cg_safe(#cg_break{}) -> true;
is_guard_cg_safe(#cg_phi{}) -> true;
is_guard_cg_safe({label,_}) -> true;
is_guard_cg_safe(#cg_unreachable{}) -> false.

try_handler(H, TryTag, SsaEvs, St0) ->
    {CatchedAgg,St1} = new_ssa_var(St0),
    {KillTryTag,St} = kill_try_tag(TryTag, St1),
    ExtractVs = extract_vars(SsaEvs, CatchedAgg, 0),
    Args = [#b_literal{val='try'},TryTag],
    Handler = [{label,H},
               #b_set{op=landingpad,dst=CatchedAgg,args=Args}] ++
        ExtractVs ++ [KillTryTag],
    {Handler,St}.

kill_try_tag(TryTag, St0) ->
    {Ignored,St} = new_ssa_var(St0),
    KillTryTag = #b_set{op=kill_try_tag,dst=Ignored,args=[TryTag]},
    {KillTryTag,St}.

extract_vars([V|Vs], Agg, N) ->
    I = #b_set{op=extract,dst=V,args=[Agg,#b_literal{val=N}]},
    [I|extract_vars(Vs, Agg, N+1)];
extract_vars([], _, _) -> [].

%% catch_cg(CatchBlock, Ret, St) -> {[Ainstr],St}.

'catch_cg'(Block, #b_var{}=Dst, St0) ->
    {B,St1} = new_label(St0),
    {Next,St2} = new_label(St1),
    {H,St3} = new_label(St2),
    {CatchReg,St4} = new_ssa_var(St3),
    {Succ,St5} = new_label(St4),
    {Cis,St6} = cg(Block, St5#cg{break=Succ,catch_label=H}),
    {CatchedVal,St7} = new_ssa_var(St6),
    {SuccVal,St8} = new_ssa_var(St7),
    {CatchedAgg,St9} = new_ssa_var(St8),
    {CatchEndVal,St} = new_ssa_var(St9),
    Args = [#b_literal{val='catch'},CatchReg],
    {[#b_set{op=new_try_tag,dst=CatchReg,args=[#b_literal{val='catch'}]},
      #b_br{bool=CatchReg,succ=Next,fail=H},
      {label,Next}] ++ Cis ++
         [{label,H},
          #b_set{op=landingpad,dst=CatchedAgg,args=Args},
          #b_set{op=extract,dst=CatchedVal,
                 args=[CatchedAgg,#b_literal{val=0}]},
          #cg_break{args=[CatchedVal],phi=B},
          {label,Succ},
          #cg_phi{vars=[SuccVal]},
          #cg_break{args=[SuccVal],phi=B},
          {label,B},#cg_phi{vars=[CatchEndVal]},
          #b_set{op=catch_end,dst=Dst,args=[CatchReg,CatchEndVal]}],
     St#cg{break=St1#cg.break,catch_label=St1#cg.catch_label}}.

%%%
%%% SSA utilities.
%%%

make_cond(Cond, Args, Fail, Succ, St0) ->
    {Bool,St} = new_ssa_var(St0),
    Bif = #b_set{op=Cond,dst=Bool,args=Args},
    Br = #b_br{bool=Bool,succ=Succ,fail=Fail},
    {[Bif,Br],St}.

make_cond_branch(Cond, Args, Fail, St0) ->
    {Succ,St} = new_label(St0),
    Bool = #b_var{name=Succ},
    Bif = #b_set{op=Cond,dst=Bool,args=Args},
    Br = #b_br{bool=Bool,succ=Succ,fail=Fail},
    {[Bif,Br,{label,Succ}],St}.

make_uncond_branch(Fail) ->
    #b_br{bool=#b_literal{val=true},succ=Fail,fail=Fail}.

%%
%% Success checks need to be treated differently in bodies and guards;
%% a check in a guard can be safely removed when we know it fails
%% because we know there's never any side effects, but in bodies the
%% checked instruction may throw an exception and we need to ensure it
%% isn't optimized away.
%%
%% Checks are expressed as {succeeded,guard} and {succeeded,body},
%% respectively, where the latter has a side effect (see
%% beam_ssa:no_side_effect/1) and the former does not. This ensures
%% that passes like ssa_opt_dead and ssa_opt_live won't optimize away
%% pure operations that may throw an exception, since their result is
%% used in {succeeded,body}.
%%
%% Other than the above details, the two variants are equivalent and
%% most passes that care about them can simply match {succeeded,_}.
%%

make_succeeded(Var, {Where,Fail}, St) when Where =:= body; Where =:= guard ->
    make_cond_branch({succeeded,Where}, [Var], Fail, St).

ssa_args(As, St) ->
    [ssa_arg(A, St) || A <- As].

ssa_arg(#b_var{name=V}=Var0, #cg{vars=Vars}) ->
    case Vars of
        #{V := Var} -> Var;
        #{} -> Var0
    end;
ssa_arg(#b_literal{}=Lit, _) ->
    Lit;
ssa_arg(#b_remote{mod=Mod0,name=Name0,arity=Arity}, St) ->
    Mod = ssa_arg(Mod0, St),
    Name = ssa_arg(Name0, St),
    #b_remote{mod=Mod,name=Name,arity=Arity};
ssa_arg(#b_local{name=#b_literal{}}=Local, _) ->
    Local.

new_succeeded_value(VarBase, Var, #cg{vars=Vars0}=St) ->
    Vars = Vars0#{VarBase => {{succeeded,guard},Var}},
    St#cg{vars=Vars}.

new_bool_value(VarBase, Var, #cg{vars=Vars0}=St) ->
    Vars = Vars0#{VarBase => {bool,Var}},
    St#cg{vars=Vars}.

new_ssa_var(#cg{lcount=Uniq}=St) ->
    {#b_var{name=Uniq},St#cg{lcount=Uniq+1}}.

set_ssa_var(VarBase, Val, #cg{vars=Vars}=St)
  when is_atom(VarBase); is_integer(VarBase) ->
    St#cg{vars=Vars#{VarBase => Val}}.

new_label(#cg{lcount=Next}=St) ->
    {Next,St#cg{lcount=Next+1}}.

%% line_anno(Le) -> #{} | #{location:={File,Line}}.
%%  Create a location annotation, containing information about the
%%  current filename and line number.  The annotation should be
%%  included in any operation that could cause an exception.

line_anno([Line,{file,Name}]) when is_integer(Line) ->
    line_anno_1(Name, Line);
line_anno([{Line,Column},{file,Name}]) when is_integer(Line),
                                            is_integer(Column) ->
    line_anno_1(Name, Line);
line_anno([_|_]=A) ->
    {Name,Line} = find_loc(A, no_file, 0),
    line_anno_1(Name, Line);
line_anno([]) ->
    #{}.

line_anno_1(no_file, _) ->
    #{};
line_anno_1(_, 0) ->
    %% Missing line number or line number 0.
    #{};
line_anno_1(Name, Line) ->
    #{location => {Name,Line}}.

find_loc([Line|T], File, _) when is_integer(Line) ->
    find_loc(T, File, Line);
find_loc([{Line, Column}|T], File, _) when is_integer(Line),
                                           is_integer(Column) ->
    find_loc(T, File, Line);
find_loc([{file,File}|T], _, Line) ->
    find_loc(T, File, Line);
find_loc([_|T], File, Line) ->
    find_loc(T, File, Line);
find_loc([], File, Line) -> {File,Line}.

%% fix_phis(Is0) -> Is.
%%  Rewrite #cg_break{} and #cg_phi{} records to #b_set{} records.
%%  A #cg_break{} is rewritten to an unconditional branch, and
%%  and a #cg_phi{} is rewritten to one or more phi nodes.

fix_phis(Is) ->
    fix_phis_1(Is, none, #{}).

fix_phis_1([{label,Lbl},#cg_phi{vars=Vars}|Is0], _Lbl, Map0) ->
    case Map0 of
        #{Lbl := Pairs} ->
            %% This phi node was referenced by at least one #cg_break{}.
            %% Create the phi nodes.
            Phis = gen_phis(Vars, Pairs),
            Map = maps:remove(Lbl, Map0),
            [{label,Lbl}] ++ Phis ++ fix_phis_1(Is0, Lbl, Map);
        #{} ->
            %% No #cg_break{} instructions reference this label.
            %% #cg_break{} instructions must reference the labels for
            %% #cg_phi{} instructions; therefore this label is
            %% unreachable and can be dropped.
            Is = drop_upto_label(Is0),
            fix_phis_1(Is, none, Map0)
    end;
fix_phis_1([{label,L}=I|Is], _Lbl, Map) ->
    [I|fix_phis_1(Is, L, Map)];
fix_phis_1([#cg_unreachable{}|Is0], _Lbl, Map) ->
    Is = drop_upto_label(Is0),
    fix_phis_1(Is, none, Map);
fix_phis_1([#cg_break{args=Args,phi=Target}|Is], Lbl, Map) when is_integer(Lbl) ->
    %% Pair each argument with the label for this block and save in the map.
    Pairs1 = case Map of
                 #{Target := Pairs0} -> Pairs0;
                 #{} -> []
             end,
    Pairs = [[{Arg,Lbl} || Arg <- Args]|Pairs1],
    I = make_uncond_branch(Target),
    [I|fix_phis_1(Is, none, Map#{Target => Pairs})];
fix_phis_1([#b_set{op=remove_message,dst=Dst}=Set,
            #b_ret{arg=Dst}=Ret0|Is], Lbl, Map) ->
    %% The remove_message instruction, which is an instruction without
    %% value, was used in effect context in an `after` block. Example:
    %%
    %%   try
    %%       . . .
    %%   after
    %%       .
    %%       .
    %%       .
    %%       receive _ -> ignored end
    %%   end,
    %%   ok.
    %%
    Ret = Ret0#b_ret{arg=#b_literal{val=ok}},
    [Set,Ret|fix_phis_1(Is, Lbl, Map)];
fix_phis_1([I|Is], Lbl, Map) ->
    [I|fix_phis_1(Is, Lbl, Map)];
fix_phis_1([], _, Map) ->
    0 = map_size(Map),                          %Assertion.
    [].

gen_phis([V|Vs], Preds0) ->
    {Pairs,Preds} = collect_predecessors(Preds0, [], []),
    [_|_] = Pairs,                              %Assertion.
    [#b_set{op=phi,dst=V,args=Pairs}|gen_phis(Vs, Preds)];
gen_phis([], _) -> [].

collect_predecessors([[First|Rest]|T], ColAcc, RestAcc) ->
    collect_predecessors(T, [First|ColAcc], [Rest|RestAcc]);
collect_predecessors([], ColAcc, RestAcc) ->
    {keysort(2, ColAcc),RestAcc}.

drop_upto_label([{label,_}|_]=Is) -> Is;
drop_upto_label([_|Is]) -> drop_upto_label(Is).

%% build_map(Is) -> #{}.
%%  Split up the sequential instruction stream into blocks and
%%  store them in a map.

build_map(Is) ->
    Linear0 = build_graph_1(Is, [], []),
    Linear = beam_ssa:trim_unreachable(Linear0),
    maps:from_list(Linear).

build_graph_1([{label,L}|Is], Lbls, []) ->
    build_graph_1(Is, [L|Lbls], []);
build_graph_1([{label,L}|Is], Lbls, [_|_]=BlockAcc) ->
    make_blocks(Lbls, BlockAcc) ++ build_graph_1(Is, [L], []);
build_graph_1([I|Is], Lbls, BlockAcc) ->
    build_graph_1(Is, Lbls, [I|BlockAcc]);
build_graph_1([], Lbls, BlockAcc) ->
    make_blocks(Lbls, BlockAcc).

make_blocks(Lbls, [Last0|Is0]) ->
    Is = reverse(Is0),
    Last = beam_ssa:normalize(Last0),
    Block = #b_blk{is=Is,last=Last},
    [{L,Block} || L <- Lbls].

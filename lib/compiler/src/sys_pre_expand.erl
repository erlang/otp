%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
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
%% Purpose : Expand some source Erlang constructions. This is part of the
%%           pre-processing phase.

%% N.B. Although structs (tagged tuples) are not yet allowed in the
%% language there is code included in pattern/2 and expr/3 (commented out)
%% that handles them by transforming them to tuples.

-module(sys_pre_expand).

%% Main entry point.
-export([module/2]).

-import(lists,   [member/2,foldl/3,foldr/3]).

-type fa() :: {atom(), arity()}.

-record(expand, {module=[],                     %Module name
                 exports=[],                    %Exports
                 attributes=[],                 %Attributes
                 callbacks=[],                  %Callbacks
                 optional_callbacks=[] :: [fa()],  %Optional callbacks
                 vcount=0,                      %Variable counter
                 func=[],                       %Current function
                 arity=[],                      %Arity for current function
                 fcount=0,			%Local fun count
		 ctype				%Call type map
                }).

%% module(Forms, CompileOptions)
%%      {ModuleName,Exports,TransformedForms,CompileOptions'}
%%  Expand the forms in one module.
%%
%%  CompileOptions is augmented with options from -compile attributes.

module(Fs0, Opts0) ->

    %% Expand records. Normalise guard tests.
    Fs = erl_expand_records:module(Fs0, Opts0),

    Opts = compiler_options(Fs) ++ Opts0,

    %% Set pre-defined exported functions.
    PreExp = [{module_info,0},{module_info,1}],

    %% Build the set of defined functions and the initial call
    %% type map.
    Defined = defined_functions(Fs, PreExp),
    Ctype = maps:from_list([{K,local} || K <- Defined]),

    %% Build initial expand record.
    St0 = #expand{exports=PreExp,
		  ctype=Ctype
                 },

    %% Expand the functions.
    {Tfs,St1} = forms(Fs, St0),

    %% Get the correct list of exported functions.
    Exports = case member(export_all, Opts) of
                  true -> Defined;
                  false -> St1#expand.exports
              end,
    St2 = St1#expand{exports=Exports,ctype=undefined},

    %% Generate all functions from stored info.
    {Ats,St3} = module_attrs(St2),
    {Mfs,St4} = module_predef_funcs(St3),
    {St4#expand.module, St4#expand.exports, Ats ++ Tfs ++ Mfs,
     Opts}.

compiler_options(Forms) ->
    lists:flatten([C || {attribute,_,compile,C} <- Forms]).
    
%% defined_function(Forms, Predef) -> Functions.
%%  Add function to defined if form is a function.

defined_functions(Forms, Predef) ->
    Fs = foldl(fun({function,_,N,A,_Cs}, Acc) -> [{N,A}|Acc];
                  (_, Acc) -> Acc
               end, Predef, Forms),
    ordsets:from_list(Fs).

module_attrs(#expand{attributes=Attributes}=St) ->
    Attrs = [{attribute,Line,Name,Val} || {Name,Line,Val} <- Attributes],
    Callbacks = [Callback || {_,_,callback,_}=Callback <- Attrs],
    OptionalCallbacks = get_optional_callbacks(Attrs),
    {Attrs,St#expand{callbacks=Callbacks,
                     optional_callbacks=OptionalCallbacks}}.

get_optional_callbacks(Attrs) ->
    L = [O ||
            {attribute, _, optional_callbacks, O} <- Attrs,
            is_fa_list(O)],
    lists:append(L).

is_fa_list([{FuncName, Arity}|L])
  when is_atom(FuncName), is_integer(Arity), Arity >= 0 ->
    is_fa_list(L);
is_fa_list([]) -> true;
is_fa_list(_) -> false.

module_predef_funcs(St0) ->
    {Mpf1,St1} = module_predef_func_beh_info(St0),
    Mpf2 = module_predef_funcs_mod_info(St1),
    Mpf = [erl_parse:new_anno(F) || F <- Mpf1++Mpf2],
    {Mpf,St1}.

module_predef_func_beh_info(#expand{callbacks=[]}=St) ->
    {[], St};
module_predef_func_beh_info(#expand{callbacks=Callbacks,
                                    optional_callbacks=OptionalCallbacks,
				    exports=Exports}=St) ->
    PreDef0 = [{behaviour_info,1}],
    PreDef = ordsets:from_list(PreDef0),
    {[gen_beh_info(Callbacks, OptionalCallbacks)],
     St#expand{exports=ordsets:union(PreDef, Exports)}}.

gen_beh_info(Callbacks, OptionalCallbacks) ->
    List = make_list(Callbacks),
    OptionalList = make_optional_list(OptionalCallbacks),
    {function,0,behaviour_info,1,
     [{clause,0,[{atom,0,callbacks}],[],
       [List]},
      {clause,0,[{atom,0,optional_callbacks}],[],
       [OptionalList]}]}.

make_list([]) -> {nil,0};
make_list([{_,_,_,[{{Name,Arity},_}]}|Rest]) ->
    {cons,0,
     {tuple,0,
      [{atom,0,Name},
       {integer,0,Arity}]},
     make_list(Rest)}.

make_optional_list([]) -> {nil,0};
make_optional_list([{Name,Arity}|Rest]) ->
    {cons,0,
     {tuple,0,
      [{atom,0,Name},
       {integer,0,Arity}]},
     make_optional_list(Rest)}.

module_predef_funcs_mod_info(#expand{module=Mod}) ->
    ModAtom = {atom,0,Mod},
    [{function,0,module_info,0,
      [{clause,0,[],[],
        [{call,0,{remote,0,{atom,0,erlang},{atom,0,get_module_info}},
          [ModAtom]}]}]},
     {function,0,module_info,1,
      [{clause,0,[{var,0,'X'}],[],
        [{call,0,{remote,0,{atom,0,erlang},{atom,0,get_module_info}},
          [ModAtom,{var,0,'X'}]}]}]}].

%% forms(Forms, State) ->
%%      {TransformedForms,State'}
%%  Process the forms. Attributes are lost and just affect the state.
%%  Ignore uninteresting forms like eof and type.

forms([{attribute,_,file,_File}=F|Fs0], St0) ->
    {Fs,St1} = forms(Fs0, St0),
    {[F|Fs],St1};
forms([{attribute,Line,Name,Val}|Fs0], St0) ->
    St1 = attribute(Name, Val, Line, St0),
    forms(Fs0, St1);
forms([{function,L,N,A,Cs}|Fs0], St0) ->
    {Ff,St1} = function(L, N, A, Cs, St0),
    {Fs,St2} = forms(Fs0, St1),
    {[Ff|Fs],St2};
forms([_|Fs], St) -> forms(Fs, St);
forms([], St) -> {[],St}.

%% attribute(Attribute, Value, Line, State) -> State'.
%%  Process an attribute, this just affects the state.

attribute(module, Module, _L, St) ->
    true = is_atom(Module),
    St#expand{module=Module};
attribute(export, Es, _L, St) ->
    St#expand{exports=ordsets:union(ordsets:from_list(Es),
				    St#expand.exports)};
attribute(import, Is, _L, St) ->
    import(Is, St);
attribute(compile, _C, _L, St) ->
    St;
attribute(Name, Val, Line, St) when is_list(Val) ->
    St#expand{attributes=St#expand.attributes ++ [{Name,Line,Val}]};
attribute(Name, Val, Line, St) ->
    St#expand{attributes=St#expand.attributes ++ [{Name,Line,[Val]}]}.

function(L, N, A, Cs0, St0) ->
    {Cs,St} = clauses(Cs0, St0#expand{func=N,arity=A,fcount=0}),
    {{function,L,N,A,Cs},St}.

%% clauses([Clause], State) ->
%%      {[TransformedClause],State}.
%%  Expand function clauses.

clauses([{clause,Line,H0,G0,B0}|Cs0], St0) ->
    {H,St1} = head(H0, St0),
    {G,St2} = guard(G0, St1),
    {B,St3} = exprs(B0, St2),
    {Cs,St4} = clauses(Cs0, St3),
    {[{clause,Line,H,G,B}|Cs],St4};
clauses([], St) -> {[],St}.

%% head(HeadPatterns, State) ->
%%      {TransformedPatterns,Variables,UsedVariables,State'}

head(As, St) -> pattern_list(As, St).

%% pattern(Pattern, State) ->
%%      {TransformedPattern,State'}
%%

pattern({var,_,_}=Var, St) ->
    {Var,St};
pattern({char,_,_}=Char, St) ->
    {Char,St};
pattern({integer,_,_}=Int, St) ->
    {Int,St};
pattern({float,_,_}=Float, St) ->
    {Float,St};
pattern({atom,_,_}=Atom, St) ->
    {Atom,St};
pattern({string,_,_}=String, St) ->
    {String,St};
pattern({nil,_}=Nil, St) ->
    {Nil,St};
pattern({cons,Line,H,T}, St0) ->
    {TH,St1} = pattern(H, St0),
    {TT,St2} = pattern(T, St1),
    {{cons,Line,TH,TT},St2};
pattern({tuple,Line,Ps}, St0) ->
    {TPs,St1} = pattern_list(Ps, St0),
    {{tuple,Line,TPs},St1};
pattern({map,Line,Ps}, St0) ->
    {TPs,St1} = pattern_list(Ps, St0),
    {{map,Line,TPs},St1};
pattern({map_field_exact,Line,K0,V0}, St0) ->
    %% Key should be treated as an expression
    %% but since expressions are not allowed yet,
    %% process it through pattern .. and handle assoc
    %% (normalise unary op integer -> integer)
    {K,St1} = pattern(K0, St0),
    {V,St2} = pattern(V0, St1),
    {{map_field_exact,Line,K,V},St2};
pattern({map_field_assoc,Line,K0,V0}, St0) ->
    %% when keys are Maps
    {K,St1} = pattern(K0, St0),
    {V,St2} = pattern(V0, St1),
    {{map_field_assoc,Line,K,V},St2};
%%pattern({struct,Line,Tag,Ps}, St0) ->
%%    {TPs,TPsvs,St1} = pattern_list(Ps, St0),
%%    {{tuple,Line,[{atom,Line,Tag}|TPs]},TPsvs,St1};
pattern({bin,Line,Es0}, St0) ->
    {Es1,St1} = pattern_bin(Es0, St0),
    {{bin,Line,Es1},St1};
pattern({op,_,'++',{nil,_},R}, St) ->
    pattern(R, St);
pattern({op,_,'++',{cons,Li,H,T},R}, St) ->
    pattern({cons,Li,H,{op,Li,'++',T,R}}, St);
pattern({op,_,'++',{string,Li,L},R}, St) ->
    pattern(string_to_conses(Li, L, R), St);
pattern({match,Line,Pat1, Pat2}, St0) ->
    {TH,St1} = pattern(Pat2, St0),
    {TT,St2} = pattern(Pat1, St1),
    {{match,Line,TT,TH},St2};
%% Compile-time pattern expressions, including unary operators.
pattern({op,_Line,_Op,_A}=Op, St) ->
    {erl_eval:partial_eval(Op),St};
pattern({op,_Line,_Op,_L,_R}=Op, St) ->
    {erl_eval:partial_eval(Op),St}.

pattern_list([P0|Ps0], St0) ->
    {P,St1} = pattern(P0, St0),
    {Ps,St2} = pattern_list(Ps0, St1),
    {[P|Ps],St2};
pattern_list([], St) -> {[],St}.

%% guard(Guard, State) ->
%%      {TransformedGuard,State'}
%%  Transform a list of guard tests. We KNOW that this has been checked
%%  and what the guards test are. Use expr for transforming the guard
%%  expressions.

guard([G0|Gs0], St0) ->
    {G,St1} = guard_tests(G0, St0),
    {Gs,St2} = guard(Gs0, St1),
    {[G|Gs],St2};
guard([], St) -> {[],St}.

guard_tests([Gt0|Gts0], St0) ->
    {Gt1,St1} = guard_test(Gt0, St0),
    {Gts1,St2} = guard_tests(Gts0, St1),
    {[Gt1|Gts1],St2};
guard_tests([], St) -> {[],St}.

guard_test(Test, St) ->
    expr(Test, St).

%% exprs(Expressions, State) ->
%%      {TransformedExprs,State'}

exprs([E0|Es0], St0) ->
    {E,St1} = expr(E0, St0),
    {Es,St2} = exprs(Es0, St1),
    {[E|Es],St2};
exprs([], St) -> {[],St}.

%% expr(Expression, State) ->
%%      {TransformedExpression,State'}

expr({var,_,_}=Var, St) ->
    {Var,St};
expr({char,_,_}=Char, St) ->
    {Char,St};
expr({integer,_,_}=Int, St) ->
    {Int,St};
expr({float,_,_}=Float, St) ->
    {Float,St};
expr({atom,_,_}=Atom, St) ->
    {Atom,St};
expr({string,_,_}=String, St) ->
    {String,St};
expr({nil,_}=Nil, St) ->
    {Nil,St};
expr({cons,Line,H0,T0}, St0) ->
    {H,St1} = expr(H0, St0),
    {T,St2} = expr(T0, St1),
    {{cons,Line,H,T},St2};
expr({lc,Line,E0,Qs0}, St0) ->
    {Qs1,St1} = lc_tq(Line, Qs0, St0),
    {E1,St2} = expr(E0, St1),
    {{lc,Line,E1,Qs1},St2};
expr({bc,Line,E0,Qs0}, St0) ->
    {Qs1,St1} = lc_tq(Line, Qs0, St0),
    {E1,St2} = expr(E0, St1),
    {{bc,Line,E1,Qs1},St2};
expr({tuple,Line,Es0}, St0) ->
    {Es1,St1} = expr_list(Es0, St0),
    {{tuple,Line,Es1},St1};
%%expr({struct,Line,Tag,Es0}, Vs, St0) ->
%%    {Es1,Esvs,Esus,St1} = expr_list(Es0, Vs, St0),
%%    {{tuple,Line,[{atom,Line,Tag}|Es1]},Esvs,Esus,St1};
expr({map,Line,Es0}, St0) ->
    {Es1,St1} = expr_list(Es0, St0),
    {{map,Line,Es1},St1};
expr({map,Line,E0,Es0}, St0) ->
    {E1,St1} = expr(E0, St0),
    {Es1,St2} = expr_list(Es0, St1),
    {{map,Line,E1,Es1},St2};
expr({map_field_assoc,Line,K0,V0}, St0) ->
    {K,St1} = expr(K0, St0),
    {V,St2} = expr(V0, St1),
    {{map_field_assoc,Line,K,V},St2};
expr({map_field_exact,Line,K0,V0}, St0) ->
    {K,St1} = expr(K0, St0),
    {V,St2} = expr(V0, St1),
    {{map_field_exact,Line,K,V},St2};
expr({bin,Line,Es0}, St0) ->
    {Es1,St1} = expr_bin(Es0, St0),
    {{bin,Line,Es1},St1};
expr({block,Line,Es0}, St0) ->
    {Es,St1} = exprs(Es0, St0),
    {{block,Line,Es},St1};
expr({'if',Line,Cs0}, St0) ->
    {Cs,St1} = clauses(Cs0, St0),
    {{'if',Line,Cs},St1};
expr({'case',Line,E0,Cs0}, St0) ->
    {E,St1} = expr(E0, St0),
    {Cs,St2} = clauses(Cs0, St1),
    {{'case',Line,E,Cs},St2};
expr({'receive',Line,Cs0}, St0) ->
    {Cs,St1} = clauses(Cs0, St0),
    {{'receive',Line,Cs},St1};
expr({'receive',Line,Cs0,To0,ToEs0}, St0) ->
    {To,St1} = expr(To0, St0),
    {ToEs,St2} = exprs(ToEs0, St1),
    {Cs,St3} = clauses(Cs0, St2),
    {{'receive',Line,Cs,To,ToEs},St3};
expr({'fun',Line,Body}, St) ->
    fun_tq(Line, Body, St);
expr({named_fun,Line,Name,Cs}, St) ->
    fun_tq(Line, Cs, St, Name);
expr({call,Line,{atom,La,N}=Atom,As0}, St0) ->
    {As,St1} = expr_list(As0, St0),
    Ar = length(As),
    Key = {N,Ar},
    case St1#expand.ctype of
	#{Key:=local} ->
	    {{call,Line,Atom,As},St1};
	#{Key:={imported,Mod}} ->
	    {{call,Line,{remote,La,{atom,La,Mod},Atom},As},St1};
	_ ->
	    true = erl_internal:bif(N, Ar),
	    {{call,Line,{remote,La,{atom,La,erlang},Atom},As},St1}
    end;
expr({call,Line,{remote,Lr,M0,F},As0}, St0) ->
    {[M1,F1|As1],St1} = expr_list([M0,F|As0], St0),
    {{call,Line,{remote,Lr,M1,F1},As1},St1};
expr({call,Line,F,As0}, St0) ->
    {[Fun1|As1],St1} = expr_list([F|As0], St0),
    {{call,Line,Fun1,As1},St1};
expr({'try',Line,Es0,Scs0,Ccs0,As0}, St0) ->
    {Es1,St1} = exprs(Es0, St0),
    {Scs1,St2} = clauses(Scs0, St1),
    {Ccs1,St3} = clauses(Ccs0, St2),
    {As1,St4} = exprs(As0, St3),
    {{'try',Line,Es1,Scs1,Ccs1,As1},St4};
expr({'catch',Line,E0}, St0) ->
    {E,St1} = expr(E0, St0),
    {{'catch',Line,E},St1};
expr({match,Line,P0,E0}, St0) ->
    {E,St1} = expr(E0, St0),
    {P,St2} = pattern(P0, St1),
    {{match,Line,P,E},St2};
expr({op,Line,Op,A0}, St0) ->
    {A,St1} = expr(A0, St0),
    {{op,Line,Op,A},St1};
expr({op,Line,Op,L0,R0}, St0) ->
    {L,St1} = expr(L0, St0),
    {R,St2} = expr(R0, St1),
    {{op,Line,Op,L,R},St2}.

expr_list([E0|Es0], St0) ->
    {E,St1} = expr(E0, St0),
    {Es,St2} = expr_list(Es0, St1),
    {[E|Es],St2};
expr_list([], St) -> {[],St}.

%% lc_tq(Line, Qualifiers, State) ->
%%      {[TransQual],State'}

lc_tq(Line, [{generate,Lg,P0,G0} | Qs0], St0) ->
    {G1,St1} = expr(G0, St0),
    {P1,St2} = pattern(P0, St1),
    {Qs1,St3} = lc_tq(Line, Qs0, St2),
    {[{generate,Lg,P1,G1} | Qs1],St3};

lc_tq(Line, [{b_generate,Lg,P0,G0}|Qs0], St0) ->
    {G1,St1} = expr(G0, St0),
    {P1,St2} = pattern(P0, St1),
    {Qs1,St3} = lc_tq(Line, Qs0, St2),
    {[{b_generate,Lg,P1,G1}|Qs1],St3};
lc_tq(Line, [F0 | Qs0], St0) ->
    {F1,St1} = expr(F0, St0),
    {Qs1,St2} = lc_tq(Line, Qs0, St1),
    {[F1|Qs1],St2};
lc_tq(_Line, [], St0) ->
    {[],St0}.


%% fun_tq(Line, Body, State) ->
%%      {Fun,State'}
%% Transform an "explicit" fun {'fun', Line, {clauses, Cs}} into an
%% extended form {'fun', Line, {clauses, Cs}, Info}, unless it is the
%% name of a BIF (erl_lint has checked that it is not an import).
%% "Implicit" funs {'fun', Line, {function, F, A}} are not changed.

fun_tq(Lf, {function,F,A}=Function, St0) ->
    case erl_internal:bif(F, A) of
        true ->
	    {As,St1} = new_vars(A, Lf, St0),
	    Cs = [{clause,Lf,As,[],[{call,Lf,{atom,Lf,F},As}]}],
            fun_tq(Lf, {clauses,Cs}, St1);
        false ->
            {Fname,St1} = new_fun_name(St0),
            Index = Uniq = 0,
            {{'fun',Lf,Function,{Index,Uniq,Fname}},St1}
    end;
fun_tq(L, {function,M,F,A}, St) when is_atom(M), is_atom(F), is_integer(A) ->
    %% This is the old format for external funs, generated by a pre-R15
    %% compiler. That means that a tool, such as the debugger or xref,
    %% directly invoked this module with the abstract code from a
    %% pre-R15 BEAM file. Be helpful, and translate it to the new format.
    fun_tq(L, {function,{atom,L,M},{atom,L,F},{integer,L,A}}, St);
fun_tq(Lf, {function,_,_,_}=ExtFun, St) ->
    {{'fun',Lf,ExtFun},St};
fun_tq(Lf, {clauses,Cs0}, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    {Fname,St2} = new_fun_name(St1),
    %% Set dummy values for Index and Uniq -- the real values will
    %% be assigned by beam_asm.
    Index = Uniq = 0,
    {{'fun',Lf,{clauses,Cs1},{Index,Uniq,Fname}},St2}.

fun_tq(Line, Cs0, St0, Name) ->
    {Cs1,St1} = clauses(Cs0, St0),
    {Fname,St2} = new_fun_name(St1, Name),
    {{named_fun,Line,Name,Cs1,{0,0,Fname}},St2}.

%% new_fun_name(State) -> {FunName,State}.

new_fun_name(St) ->
    new_fun_name(St, 'fun').

new_fun_name(#expand{func=F,arity=A,fcount=I}=St, FName) ->
    Name = "-" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A)
        ++ "-" ++ atom_to_list(FName) ++ "-" ++ integer_to_list(I) ++ "-",
    {list_to_atom(Name),St#expand{fcount=I+1}}.

%% pattern_bin([Element], State) -> {[Element],[Variable],[UsedVar],State}.

pattern_bin(Es0, St) ->
    Es1 = bin_expand_strings(Es0),
    foldr(fun (E, Acc) -> pattern_element(E, Acc) end, {[],St}, Es1).

pattern_element({bin_element,Line,Expr0,Size0,Type0}, {Es,St0}) ->
    {Expr1,St1} = pattern(Expr0, St0),
    {Size1,St2} = pat_bit_size(Size0, St1),
    {Size,Type} = make_bit_type(Line, Size1, Type0),
    Expr = coerce_to_float(Expr1, Type0),
    {[{bin_element,Line,Expr,Size,Type}|Es],St2}.

pat_bit_size(default, St) -> {default,St};
pat_bit_size({var,_Lv,_V}=Var, St) -> {Var,St};
pat_bit_size(Size, St) ->
    Line = element(2, Size),
    {value,Sz,_} = erl_eval:expr(Size, erl_eval:new_bindings()),
    {{integer,Line,Sz},St}.

make_bit_type(Line, default, Type0) ->
    case erl_bits:set_bit_type(default, Type0) of
        {ok,all,Bt} -> {{atom,Line,all},erl_bits:as_list(Bt)};
	{ok,undefined,Bt} -> {{atom,Line,undefined},erl_bits:as_list(Bt)};
        {ok,Size,Bt} -> {{integer,Line,Size},erl_bits:as_list(Bt)}
    end;
make_bit_type(_Line, Size, Type0) ->            %Integer or 'all'
    {ok,Size,Bt} = erl_bits:set_bit_type(Size, Type0),
    {Size,erl_bits:as_list(Bt)}.

coerce_to_float({integer,L,I}=E, [float|_]) ->
    try
        {float,L,float(I)}
    catch
        error:badarg -> E
    end;
coerce_to_float(E, _) -> E.
    
%% expr_bin([Element], State) -> {[Element],State}.

expr_bin(Es0, St) ->
    Es1 = bin_expand_strings(Es0),
    foldr(fun (E, Acc) -> bin_element(E, Acc) end, {[],St}, Es1).

bin_element({bin_element,Line,Expr,Size,Type}, {Es,St0}) ->
    {Expr1,St1} = expr(Expr, St0),
    {Size1,St2} = if Size == default -> {default,St1};
                             true -> expr(Size, St1)
                          end,
    {Size2,Type1} = make_bit_type(Line, Size1, Type),
    {[{bin_element,Line,Expr1,Size2,Type1}|Es],St2}.

bin_expand_strings(Es) ->
    foldr(fun ({bin_element,Line,{string,_,S},Sz,Ts}, Es1) ->
                  foldr(fun (C, Es2) ->
                                [{bin_element,Line,{char,Line,C},Sz,Ts}|Es2]
                        end, Es1, S);
              (E, Es1) -> [E|Es1]
          end, [], Es).

%% new_var_name(State) -> {VarName,State}.

new_var_name(St) ->
    C = St#expand.vcount,
    {list_to_atom("pre" ++ integer_to_list(C)),St#expand{vcount=C+1}}.

%% new_var(Line, State) -> {Var,State}.

new_var(L, St0) ->
    {New,St1} = new_var_name(St0),
    {{var,L,New},St1}.

%% new_vars(Count, Line, State) -> {[Var],State}.
%%  Make Count new variables.

new_vars(N, L, St) -> new_vars(N, L, St, []).

new_vars(N, L, St0, Vs) when N > 0 ->
    {V,St1} = new_var(L, St0),
    new_vars(N-1, L, St1, [V|Vs]);
new_vars(0, _L, St, Vs) -> {Vs,St}.

string_to_conses(Line, Cs, Tail) ->
    foldr(fun (C, T) -> {cons,Line,{char,Line,C},T} end, Tail, Cs).


%% import(Line, Imports, State) ->
%%      State'
%%  Handle import declarations.

import({Mod,Fs}, #expand{ctype=Ctype0}=St) ->
    true = is_atom(Mod),
    Ctype = foldl(fun(F, A) ->
			  A#{F=>{imported,Mod}}
		  end, Ctype0, Fs),
    St#expand{ctype=Ctype}.

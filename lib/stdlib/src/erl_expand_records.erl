%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2023. All Rights Reserved.
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
%% Purpose: Expand records into tuples. Also add explicit module
%% names to calls to imported functions and BIFs.

-module(erl_expand_records).

-export([module/2]).

-import(lists, [map/2,foldl/3,foldr/3,sort/1,reverse/1,duplicate/2]).

-record(exprec, {compile=[],	% Compile flags
		 vcount=0,	% Variable counter
		 calltype=#{},	% Call types
		 records=#{},	% Record definitions
                 raw_records=[],% Raw record forms
		 strict_ra=[],	% strict record accesses
		 checked_ra=[], % successfully accessed records
                 dialyzer=false % Cached value of compile flag 'dialyzer'
		}).

-spec(module(AbsForms, CompileOptions) -> AbsForms2 when
      AbsForms :: [erl_parse:abstract_form()],
      AbsForms2 :: [erl_parse:abstract_form()],
      CompileOptions :: [compile:option()]).

%% Is is assumed that Fs is a valid list of forms. It should pass
%% erl_lint without errors.
module(Fs0, Opts0) ->
    Opts = compiler_options(Fs0) ++ Opts0,
    Dialyzer = lists:member(dialyzer, Opts),
    Calltype = init_calltype(Fs0),
    St0 = #exprec{compile = Opts, dialyzer = Dialyzer, calltype = Calltype},
    {Fs,_St} = forms(Fs0, St0),
    Fs.

compiler_options(Forms) ->
    lists:flatten([C || {attribute,_,compile,C} <- Forms]).

init_calltype(Forms) ->
    Ctype = #{{Name,Arity} => local || {function,_,Name,Arity,_} <- Forms},
    init_calltype_imports(Forms, Ctype).

init_calltype_imports([{attribute,_,import,{Mod,Fs}}|T], Ctype0) ->
    true = is_atom(Mod),
    Ctype = foldl(fun(FA, Acc) ->
			  Acc#{FA=>{imported,Mod}}
		  end, Ctype0, Fs),
    init_calltype_imports(T, Ctype);
init_calltype_imports([_|T], Ctype) ->
    init_calltype_imports(T, Ctype);
init_calltype_imports([], Ctype) -> Ctype.

forms([{attribute,_,record,{Name,Defs}}=Attr | Fs], St0) ->
    NDefs = normalise_fields(Defs),
    St = St0#exprec{records=maps:put(Name, NDefs, St0#exprec.records),
                    raw_records=[Attr | St0#exprec.raw_records]},
    {Fs1, St1} = forms(Fs, St),
    {[Attr | Fs1], St1};
forms([{function,Anno,N,A,Cs0} | Fs0], St0) ->
    {Cs,St1} = clauses(Cs0, St0),
    {Fs,St2} = forms(Fs0, St1),
    {[{function,Anno,N,A,Cs} | Fs],St2};
forms([F | Fs0], St0) ->
    {Fs,St} = forms(Fs0, St0),
    {[F | Fs], St};
forms([], St) -> {[],St}.

clauses([{clause,Anno,H0,G0,B0} | Cs0], St0) ->
    {H1,St1} = head(H0, St0),
    {G1,St2} = guard(G0, St1),
    {H,G} = optimize_is_record(H1, G1, St2),
    {B,St3} = exprs(B0, St2),
    {Cs,St4} = clauses(Cs0, St3),
    {[{clause,Anno,H,G,B} | Cs],St4};
clauses([], St) -> {[],St}.

head(As, St) -> pattern_list(As, St).

pattern({var,_,'_'}=Var, St) ->
    {Var,St};
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
pattern({cons,Anno,H,T}, St0) ->
    {TH,St1} = pattern(H, St0),
    {TT,St2} = pattern(T, St1),
    {{cons,Anno,TH,TT},St2};
pattern({tuple,Anno,Ps}, St0) ->
    {TPs,St1} = pattern_list(Ps, St0),
    {{tuple,Anno,TPs},St1};
pattern({map,Anno,Ps}, St0) ->
    {TPs,St1} = pattern_list(Ps, St0),
    {{map,Anno,TPs},St1};
pattern({map_field_exact,Anno,K0,V0}, St0) ->
    {K,St1} = expr(K0, St0),
    {V,St2} = pattern(V0, St1),
    {{map_field_exact,Anno,K,V},St2};
pattern({record_index,Anno,Name,Field}, St) ->
    {index_expr(Anno, Field, Name, record_fields(Name, Anno, St)),St};
pattern({record,Anno0,Name,Pfs}, St0) ->
    Fs = record_fields(Name, Anno0, St0),
    {TMs,St1} = pattern_list(pattern_fields(Fs, Pfs), St0),
    Anno = mark_record(Anno0, St1),
    {{tuple,Anno,[{atom,Anno0,Name} | TMs]},St1};
pattern({bin,Anno,Es0}, St0) ->
    {Es1,St1} = pattern_bin(Es0, St0),
    {{bin,Anno,Es1},St1};
pattern({match,Anno,Pat1, Pat2}, St0) ->
    {TH,St1} = pattern(Pat2, St0),
    {TT,St2} = pattern(Pat1, St1),
    {{match,Anno,TT,TH},St2};
pattern({op,Anno,Op,A0}, St0) ->
    {A,St1} = pattern(A0, St0),
    {{op,Anno,Op,A},St1};
pattern({op,Anno,Op,L0,R0}, St0) ->
    {L,St1} = pattern(L0, St0),
    {R,St2} = pattern(R0, St1),
    {{op,Anno,Op,L,R},St2}.

pattern_list([P0 | Ps0], St0) ->
    {P,St1} = pattern(P0, St0),
    {Ps,St2} = pattern_list(Ps0, St1),
    {[P | Ps],St2};
pattern_list([], St) -> {[],St}.

guard([G0 | Gs0], St0) ->
    {G,St1} = guard_tests(G0, St0),
    {Gs,St2} = guard(Gs0, St1),
    {[G | Gs],St2};
guard([], St) -> {[],St}.

guard_tests(Gts0, St0) ->
    {Gts1,St1} = guard_tests1(Gts0, St0),
    {Gts1,St1#exprec{checked_ra = []}}.

guard_tests1([Gt0 | Gts0], St0) ->
    {Gt1,St1} = guard_test(Gt0, St0),
    {Gts1,St2} = guard_tests1(Gts0, St1),
    {[Gt1 | Gts1],St2};
guard_tests1([], St) -> {[],St}.

guard_test(G0, St0) ->
    in_guard(fun() ->
                     {G1,St1} = guard_test1(G0, St0),
                     strict_record_access(G1, St1)
             end).

%% Normalising guard tests ensures that none of the Boolean operands
%% created by strict_record_access/2 calls any of the old guard tests.
guard_test1({call,Anno,{atom,Tanno,Tname},As}, St) ->
    Test = {atom,Tanno,normalise_test(Tname, length(As))},
    expr({call,Anno,Test,As}, St);
guard_test1(Test, St) ->
    expr(Test, St).

normalise_test(atom, 1)      -> is_atom;
normalise_test(binary, 1)    -> is_binary;
normalise_test(float, 1)     -> is_float;
normalise_test(function, 1)  -> is_function;
normalise_test(integer, 1)   -> is_integer;
normalise_test(list, 1)      -> is_list;
normalise_test(number, 1)    -> is_number;
normalise_test(pid, 1)       -> is_pid; 
normalise_test(port, 1)      -> is_port; 
normalise_test(record, 2)    -> is_record;
normalise_test(reference, 1) -> is_reference;
normalise_test(tuple, 1)     -> is_tuple;
normalise_test(Name, _) -> Name.

is_in_guard() ->
    get(erl_expand_records_in_guard) =/= undefined.

in_guard(F) ->
    undefined = put(erl_expand_records_in_guard, true),
    Res = F(),
    true = erase(erl_expand_records_in_guard),
    Res.

%% record_test(Anno, Term, Name, Vs, St) -> TransformedExpr
%%  Generate code for is_record/1.

record_test(Anno, Term, Name, St) ->
    case is_in_guard() of
        false ->
            record_test_in_body(Anno, Term, Name, St);
        true ->
            record_test_in_guard(Anno, Term, Name, St)
    end.

record_test_in_guard(Anno, Term, Name, St) ->
    case not_a_tuple(Term) of
        true ->
            %% In case that later optimization passes have been turned off.
            expr({atom,Anno,false}, St);
        false ->
            Fs = record_fields(Name, Anno, St),
            NAnno = no_compiler_warning(Anno),
            expr({call,NAnno,{remote,NAnno,{atom,NAnno,erlang},{atom,NAnno,is_record}},
                  [Term,{atom,Anno,Name},{integer,Anno,length(Fs)+1}]},
                 St)
    end.

not_a_tuple({atom,_,_}) -> true;
not_a_tuple({integer,_,_}) -> true;
not_a_tuple({float,_,_}) -> true;
not_a_tuple({nil,_}) -> true;
not_a_tuple({cons,_,_,_}) -> true;
not_a_tuple({char,_,_}) -> true;
not_a_tuple({string,_,_}) -> true;
not_a_tuple({record_index,_,_,_}) -> true;
not_a_tuple({bin,_,_}) -> true;
not_a_tuple({op,_,_,_}) -> true;
not_a_tuple({op,_,_,_,_}) -> true;
not_a_tuple(_) -> false.

record_test_in_body(Anno, Expr, Name, St0) ->
    %% As Expr may have side effects, we must evaluate it
    %% first and bind the value to a new variable.
    %% We must use also handle the case that Expr does not
    %% evaluate to a tuple properly.
    Fs = record_fields(Name, Anno, St0),
    {Var,St} = new_var(Anno, St0),
    NAnno = no_compiler_warning(Anno),
    expr({block,Anno,
          [{match,Anno,Var,Expr},
           {call,NAnno,{remote,NAnno,{atom,NAnno,erlang},
                        {atom,NAnno,is_record}},
            [Var,{atom,Anno,Name},{integer,Anno,length(Fs)+1}]}]}, St).

exprs([E0 | Es0], St0) ->
    {E,St1} = expr(E0, St0),
    {Es,St2} = exprs(Es0, St1),
    {[E | Es],St2};
exprs([], St) -> {[],St}.

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
expr({cons,Anno,H0,T0}, St0) ->
    {H,St1} = expr(H0, St0),
    {T,St2} = expr(T0, St1),
    {{cons,Anno,H,T},St2};
expr({lc,Anno,E0,Qs0}, St0) ->
    {Qs1,St1} = lc_tq(Anno, Qs0, St0),
    {E1,St2} = expr(E0, St1),
    {{lc,Anno,E1,Qs1},St2};
expr({bc,Anno,E0,Qs0}, St0) ->
    {Qs1,St1} = lc_tq(Anno, Qs0, St0),
    {E1,St2} = expr(E0, St1),
    {{bc,Anno,E1,Qs1},St2};
expr({mc,Anno,E0,Qs0}, St0) ->
    {Qs1,St1} = lc_tq(Anno, Qs0, St0),
    {E1,St2} = expr(E0, St1),
    {{mc,Anno,E1,Qs1},St2};
expr({tuple,Anno,Es0}, St0) ->
    {Es1,St1} = expr_list(Es0, St0),
    {{tuple,Anno,Es1},St1};
expr({map,Anno,Es0}, St0) ->
    {Es1,St1} = expr_list(Es0, St0),
    {{map,Anno,Es1},St1};
expr({map,Anno,Arg0,Es0}, St0) ->
    {Arg1,St1} = expr(Arg0, St0),
    {Es1,St2} = expr_list(Es0, St1),
    {{map,Anno,Arg1,Es1},St2};
expr({map_field_assoc,Anno,K0,V0}, St0) ->
    {K,St1} = expr(K0, St0),
    {V,St2} = expr(V0, St1),
    {{map_field_assoc,Anno,K,V},St2};
expr({map_field_exact,Anno,K0,V0}, St0) ->
    {K,St1} = expr(K0, St0),
    {V,St2} = expr(V0, St1),
    {{map_field_exact,Anno,K,V},St2};
expr({record_index,Anno,Name,F}, St) ->
    I = index_expr(Anno, F, Name, record_fields(Name, Anno, St)),
    expr(I, St);
expr({record,Anno0,Name,Is}, St) ->
    Anno = mark_record(Anno0, St),
    expr({tuple,Anno,[{atom,Anno0,Name} |
                      record_inits(record_fields(Name, Anno0, St), Is)]},
         St);
expr({record_field,_A,R,Name,F}, St) ->
    Anno = erl_parse:first_anno(R),
    get_record_field(Anno, R, F, Name, St);
expr({record,Anno,R,Name,Us}, St0) ->
    {Ue,St1} = record_update(R, Name, record_fields(Name, Anno, St0), Us, St0),
    expr(Ue, St1);
expr({bin,Anno,Es0}, St0) ->
    {Es1,St1} = expr_bin(Es0, St0),
    {{bin,Anno,Es1},St1};
expr({block,Anno,Es0}, St0) ->
    {Es,St1} = exprs(Es0, St0),
    {{block,Anno,Es},St1};
expr({'if',Anno,Cs0}, St0) ->
    {Cs,St1} = clauses(Cs0, St0),
    {{'if',Anno,Cs},St1};
expr({'case',Anno,E0,Cs0}, St0) ->
    {E,St1} = expr(E0, St0),
    {Cs,St2} = clauses(Cs0, St1),
    {{'case',Anno,E,Cs},St2};
expr({'receive',Anno,Cs0}, St0) ->
    {Cs,St1} = clauses(Cs0, St0),
    {{'receive',Anno,Cs},St1};
expr({'receive',Anno,Cs0,To0,ToEs0}, St0) ->
    {To,St1} = expr(To0, St0),
    {ToEs,St2} = exprs(ToEs0, St1),
    {Cs,St3} = clauses(Cs0, St2),
    {{'receive',Anno,Cs,To,ToEs},St3};
expr({'fun',Anno,{function,F,A}}=Fun0, St0) ->
    case erl_internal:bif(F, A) of
        true ->
	    {As,St1} = new_vars(A, Anno, St0),
	    Cs = [{clause,Anno,As,[],[{call,Anno,{atom,Anno,F},As}]}],
	    Fun = {'fun',Anno,{clauses,Cs}},
	    expr(Fun,  St1);
	false ->
	    {Fun0,St0}
    end;
expr({'fun',_,{function,_M,_F,_A}}=Fun, St) ->
    {Fun,St};
expr({'fun',Anno,{clauses,Cs0}}, St0) ->
    {Cs,St1} = clauses(Cs0, St0),
    {{'fun',Anno,{clauses,Cs}},St1};
expr({named_fun,Anno,Name,Cs0}, St0) ->
    {Cs,St1} = clauses(Cs0, St0),
    {{named_fun,Anno,Name,Cs},St1};
expr({call,Anno,{atom,_,is_record},[A,{atom,_,Name}]}, St) ->
    record_test(Anno, A, Name, St);
expr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,is_record}},
      [A,{atom,_,Name}]}, St) ->
    record_test(Anno, A, Name, St);
expr({call,Anno,{tuple,_,[{atom,_,erlang},{atom,_,is_record}]},
      [A,{atom,_,Name}]}, St) ->
    record_test(Anno, A, Name, St);
expr({call,Anno,{atom,_,is_record},[_,_,{integer,_,Sz}]}, St)
  when is_integer(Sz), Sz =< 0 ->
    {{atom,Anno,false},St};
expr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,is_record}},
      [_,_,{integer,_,Sz}]}, St) when is_integer(Sz), Sz =< 0 ->
    {{atom,Anno,false},St};
expr({call,Anno,{atom,_AnnoA,record_info},[_,_]=As0}, St0) ->
    {As,St1} = expr_list(As0, St0),
    record_info_call(Anno, As, St1);
expr({call,Anno,{atom,_AnnoA,N}=Atom,As0}, St0) ->
    {As,St1} = expr_list(As0, St0),
    Ar = length(As),
    NA = {N,Ar},
    case St0#exprec.calltype of
	#{NA := local} ->
	    {{call,Anno,Atom,As},St1};
	#{NA := {imported,Module}} ->
	    ModAtom = {atom,Anno,Module},
	    {{call,Anno,{remote,Anno,ModAtom,Atom},As},St1};
	_ ->
	    case erl_internal:bif(N, Ar) of
		true ->
		    ModAtom = {atom,Anno,erlang},
		    {{call,Anno,{remote,Anno,ModAtom,Atom},As},St1};
		false ->
		    %% Call to a module_info/0,1 or one of the
		    %% pseudo-functions in the shell. Leave it as
		    %% a local call.
		    {{call,Anno,Atom,As},St1}
	    end
    end;
expr({call,Anno,{remote,AnnoR,M,F},As0}, St0) ->
    {[M1,F1 | As1],St1} = expr_list([M,F | As0], St0),
    {{call,Anno,{remote,AnnoR,M1,F1},As1},St1};
expr({call,Anno,F,As0}, St0) ->
    {[Fun1 | As1],St1} = expr_list([F | As0], St0),
    {{call,Anno,Fun1,As1},St1};
expr({'try',Anno,Es0,Scs0,Ccs0,As0}, St0) ->
    {Es1,St1} = exprs(Es0, St0),
    {Scs1,St2} = clauses(Scs0, St1),
    {Ccs1,St3} = clauses(Ccs0, St2),
    {As1,St4} = exprs(As0, St3),
    {{'try',Anno,Es1,Scs1,Ccs1,As1},St4};
expr({'catch',Anno,E0}, St0) ->
    {E,St1} = expr(E0, St0),
    {{'catch',Anno,E},St1};
expr({'maybe',MaybeAnno,Es0}, St0) ->
    {Es,St1} = exprs(Es0, St0),
    {{'maybe',MaybeAnno,Es},St1};
expr({'maybe',MaybeAnno,Es0,{'else',ElseAnno,Cs0}}, St0) ->
    {Es,St1} = exprs(Es0, St0),
    {Cs,St2} = clauses(Cs0, St1),
    {{'maybe',MaybeAnno,Es,{'else',ElseAnno,Cs}},St2};
expr({maybe_match,Anno,P0,E0}, St0) ->
    {E,St1} = expr(E0, St0),
    {P,St2} = pattern(P0, St1),
    {{maybe_match,Anno,P,E},St2};
expr({match,Anno,P0,E0}, St0) ->
    {E,St1} = expr(E0, St0),
    {P,St2} = pattern(P0, St1),
    {{match,Anno,P,E},St2};
expr({op,Anno,'not',A0}, St0) ->
    {A,St1} = bool_operand(A0, St0),
    {{op,Anno,'not',A},St1};
expr({op,Anno,Op,A0}, St0) ->
    {A,St1} = expr(A0, St0),
    {{op,Anno,Op,A},St1};
expr({op,Anno,Op,L0,R0}, St0) when Op =:= 'and';
                                   Op =:= 'or' ->
    {L,St1} = bool_operand(L0, St0),
    {R,St2} = bool_operand(R0, St1),
    {{op,Anno,Op,L,R},St2};
expr({op,Anno,Op,L0,R0}, St0) when Op =:= 'andalso';
                                   Op =:= 'orelse' ->
    {L,St1} = bool_operand(L0, St0),
    {R,St2} = bool_operand(R0, St1),
    {{op,Anno,Op,L,R},St2#exprec{checked_ra = St1#exprec.checked_ra}};
expr({op,Anno,Op,L0,R0}, St0) ->
    {L,St1} = expr(L0, St0),
    {R,St2} = expr(R0, St1),
    {{op,Anno,Op,L,R},St2};
expr(E={ssa_check_when,_,_,_,_,_}, St) ->
    {E, St}.

expr_list([E0 | Es0], St0) ->
    {E,St1} = expr(E0, St0),
    {Es,St2} = expr_list(Es0, St1),
    {[E | Es],St2};
expr_list([], St) -> {[],St}.

bool_operand(E0, St0) ->
    {E1,St1} = expr(E0, St0),
    strict_record_access(E1, St1).

strict_record_access(E, #exprec{strict_ra = []} = St) ->
    {E, St};
strict_record_access(E0, St0) ->
    #exprec{strict_ra = StrictRA, checked_ra = CheckedRA} = St0,
    {New,NC} = lists:foldl(fun ({Key,_Anno,_R,_Sz}=A, {L,C}) ->
                                   case lists:keymember(Key, 1, C) of
                                       true -> {L,C};
                                       false -> {[A|L],[A|C]}
                                   end
                           end, {[],CheckedRA}, StrictRA),
    E1 = if New =:= [] -> E0; true -> conj(New, E0) end,
    St1 = St0#exprec{strict_ra = [], checked_ra = NC},
    expr(E1, St1).

%% Make it look nice (?) when compiled with the 'E' flag 
%% ('and'/2 is left recursive).
conj([], _E) ->
    empty;
conj([{{Name,_Rp},Anno,R,Sz} | AL], E) ->
    NAnno = no_compiler_warning(Anno),
    T1 = {op,NAnno,'orelse',
          {call,NAnno,
	   {remote,NAnno,{atom,NAnno,erlang},{atom,NAnno,is_record}},
	   [R,{atom,NAnno,Name},{integer,NAnno,Sz}]},
	  {atom,NAnno,fail}},
    T2 = case conj(AL, none) of
        empty -> T1;
        C -> {op,NAnno,'and',C,T1}
    end,
    case E of
	none ->
	    case T2 of
		{op,_,'and',_,_} ->
		    T2;
		_ ->
		    %% Wrap the 'orelse' expression in an dummy 'and true' to make
		    %% sure that the entire guard fails if the 'orelse'
		    %% expression returns 'fail'. ('orelse' used to verify
		    %% that its right operand was a boolean, but that is no
		    %% longer the case.)
		    {op,NAnno,'and',T2,{atom,NAnno,true}}
	    end;
	_ ->
	    {op,NAnno,'and',T2,E}
    end.

%% lc_tq(Anno, Qualifiers, State) ->
%%      {[TransQual],State'}

lc_tq(Anno, [{generate,AnnoG,P0,G0} | Qs0], St0) ->
    {G1,St1} = expr(G0, St0),
    {P1,St2} = pattern(P0, St1),
    {Qs1,St3} = lc_tq(Anno, Qs0, St2),
    {[{generate,AnnoG,P1,G1} | Qs1],St3};
lc_tq(Anno, [{b_generate,AnnoG,P0,G0} | Qs0], St0) ->
    {G1,St1} = expr(G0, St0),
    {P1,St2} = pattern(P0, St1),
    {Qs1,St3} = lc_tq(Anno, Qs0, St2),
    {[{b_generate,AnnoG,P1,G1} | Qs1],St3};
lc_tq(Anno, [{m_generate,AnnoG,P0,G0} | Qs0], St0) ->
    {G1,St1} = expr(G0, St0),
    {map_field_exact,AnnoMFE,KeyP0,ValP0} = P0,
    {KeyP1,St2} = pattern(KeyP0, St1),
    {ValP1,St3} = pattern(ValP0, St2),
    {Qs1,St4} = lc_tq(Anno, Qs0, St3),
    P1 = {map_field_exact,AnnoMFE,KeyP1,ValP1},
    {[{m_generate,AnnoG,P1,G1} | Qs1],St4};
lc_tq(Anno, [F0 | Qs0], #exprec{calltype=Calltype,raw_records=Records}=St0) ->
    %% Allow record/2 and expand out as guard test.
    IsOverriden = fun(FA) ->
			  case Calltype of
			      #{FA := local} -> true;
			      #{FA := {imported,_}} -> true;
			      _ -> false
			  end
		  end,
    case erl_lint:is_guard_test(F0, Records, IsOverriden) of
        true ->
            {F1,St1} = guard_test(F0, St0),
            {Qs1,St2} = lc_tq(Anno, Qs0, St1),
            {[F1|Qs1],St2};
        false ->
            {F1,St1} = expr(F0, St0),
            {Qs1,St2} = lc_tq(Anno, Qs0, St1),
            {[F1 | Qs1],St2}
    end;
lc_tq(_Anno, [], St0) ->
    {[],St0#exprec{checked_ra = []}}.

%% normalise_fields([RecDef]) -> [Field].
%%  Normalise the field definitions to always have a default value. If
%%  none has been given then use 'undefined'.

normalise_fields(Fs) ->
    map(fun ({record_field,Anno,Field}) ->
                {record_field,Anno,Field,{atom,Anno,undefined}};
	    ({typed_record_field,{record_field,Anno,Field},_Type}) ->
		{record_field,Anno,Field,{atom,Anno,undefined}};
	    ({typed_record_field,Field,_Type}) ->
		Field;
            (F) -> F
	end, Fs).

%% record_fields(RecordName, Anno, State)
%% find_field(FieldName, Fields)

record_fields(R, Anno, St) ->
    Fields = maps:get(R, St#exprec.records),
    [{record_field,Anno,{atom,Anno,F},copy_expr(Di, Anno)} ||
        {record_field,_Anno,{atom,_AnnoA,F},Di} <- Fields].

find_field(F, [{record_field,_,{atom,_,F},Val} | _]) -> {ok,Val};
find_field(F, [_ | Fs]) -> find_field(F, Fs);
find_field(_, []) -> error.

%% copy_expr(Expr, Anno) -> Expr.
%%  Make a copy of Expr converting all annotations to Anno.

copy_expr(Expr, Anno) ->
    erl_parse:map_anno(fun(_A) -> Anno end, Expr).

%% field_names(RecFields) -> [Name].
%%  Return a list of the field names structures.

field_names(Fs) ->
    map(fun ({record_field,_,Field,_Val}) -> Field end, Fs).

%% index_expr(Anno, FieldExpr, Name, Fields) -> IndexExpr.
%%  Return an expression which evaluates to the index of a
%%  field. Currently only handle the case where the field is an
%%  atom. This expansion must be passed through expr again.

index_expr(Anno, {atom,_,F}, _Name, Fs) ->
    {integer,Anno,index_expr(F, Fs, 2)}.

index_expr(F, [{record_field,_,{atom,_,F},_} | _], I) -> I;
index_expr(F, [_ | Fs], I) -> index_expr(F, Fs, I+1).

%% get_record_field(Anno, RecExpr, FieldExpr, Name, St) -> {Expr,St'}.
%%  Return an expression which verifies that the type of record
%%  is correct and then returns the value of the field.
%%  This expansion must be passed through expr again.

get_record_field(Anno, R, Index, Name, St) ->
    case strict_record_tests(St#exprec.compile) of
        false ->
            sloppy_get_record_field(Anno, R, Index, Name, St);
        true ->
            strict_get_record_field(Anno, R, Index, Name, St)
    end.

strict_get_record_field(Anno, R, {atom,_,F}=Index, Name, St0) ->
    case is_in_guard() of
        false ->                                %Body context.
            {Var,St} = new_var(Anno, St0),
            Fs = record_fields(Name, Anno, St),
            I = index_expr(F, Fs, 2),
            P = record_pattern(2, I, Var, length(Fs)+1, Anno, [{atom,Anno,Name}]),
            NAnno = no_compiler_warning(Anno),
            RAnno = mark_record(NAnno, St),
	    E = {'case',Anno,R,
		     [{clause,NAnno,[{tuple,RAnno,P}],[],[Var]},
		      {clause,NAnno,[Var],[],
		       [{call,NAnno,{remote,NAnno,
				    {atom,NAnno,erlang},
				    {atom,NAnno,error}},
			 [{tuple,NAnno,[{atom,NAnno,badrecord},Var]}]}]}]},
            expr(E, St);
        true ->                                 %In a guard.
            Fs = record_fields(Name, Anno, St0),
            I = index_expr(Anno, Index, Name, Fs),
            {ExpR,St1}  = expr(R, St0),
            %% Just to make comparison simple:
            A0 = erl_anno:new(0),
            ExpRp = erl_parse:map_anno(fun(_A) -> A0 end, ExpR),
            RA = {{Name,ExpRp},Anno,ExpR,length(Fs)+1},
            St2 = St1#exprec{strict_ra = [RA | St1#exprec.strict_ra]},
            {{call,Anno,
	      {remote,Anno,{atom,Anno,erlang},{atom,Anno,element}},
	      [I,ExpR]},St2}
    end.

record_pattern(I, I, Var, Sz, Anno, Acc) ->
    record_pattern(I+1, I, Var, Sz, Anno, [Var | Acc]);
record_pattern(Cur, I, Var, Sz, Anno, Acc) when Cur =< Sz ->
    record_pattern(Cur+1, I, Var, Sz, Anno, [{var,Anno,'_'} | Acc]);
record_pattern(_, _, _, _, _, Acc) -> reverse(Acc).

sloppy_get_record_field(Anno, R, Index, Name, St) ->
    Fs = record_fields(Name, Anno, St),
    I = index_expr(Anno, Index, Name, Fs),
    expr({call,Anno,
	  {remote,Anno,{atom,Anno,erlang},{atom,Anno,element}},
	  [I,R]}, St).

strict_record_tests([strict_record_tests | _]) -> true;
strict_record_tests([no_strict_record_tests | _]) -> false;
strict_record_tests([_ | Os]) -> strict_record_tests(Os);
strict_record_tests([]) -> true.		%Default.

strict_record_updates([strict_record_updates | _]) -> true;
strict_record_updates([no_strict_record_updates | _]) -> false;
strict_record_updates([_ | Os]) -> strict_record_updates(Os);
strict_record_updates([]) -> false.		%Default.

%% pattern_fields([RecDefField], [Match]) -> [Pattern].
%%  Build a list of match patterns for the record tuple elements.
%%  This expansion must be passed through pattern again. N.B. We are
%%  scanning the record definition field list!

pattern_fields(Fs, Ms) ->
    Wildcard = record_wildcard_init(Ms),
    map(fun ({record_field,Anno,{atom,_,F},_}) ->
                case find_field(F, Ms) of
                    {ok,Match} -> Match;
                    error when Wildcard =:= none -> {var,Anno,'_'};
                    error -> Wildcard
		end
	end, Fs).

%% record_inits([RecDefField], [Init]) -> [InitExpr].
%%  Build a list of initialisation expressions for the record tuple
%%  elements. This expansion must be passed through expr
%%  again. N.B. We are scanning the record definition field list!

record_inits(Fs, Is) ->
    WildcardInit = record_wildcard_init(Is),
    map(fun ({record_field,_,{atom,_,F},D}) ->
                case find_field(F, Is) of
                    {ok,Init} -> Init;
                    error when WildcardInit =:= none -> D;
                    error -> WildcardInit
                end
	end, Fs).

record_wildcard_init([{record_field,_,{var,_,'_'},D} | _]) -> D;
record_wildcard_init([_ | Is]) -> record_wildcard_init(Is);
record_wildcard_init([]) -> none.

%% record_update(Record, RecordName, [RecDefField], [Update], State) ->
%%      {Expr,State'}
%%  Build an expression to update fields in a record returning a new
%%  record.  Try to be smart and optimise this. This expansion must be
%%  passed through expr again.

record_update(R, Name, Fs, Us0, St0) ->
    Anno = element(2, R),
    {Pre,Us,St1} = record_exprs(Us0, St0),

    %% We need a new variable for the record expression
    %% to guarantee that it is only evaluated once.
    {Var,St2} = new_var(Anno, St1),

    %% Honor the `strict_record_updates` option needed by `dialyzer`, otherwise
    %% expand everything to chains of `setelement/3` as that's far more
    %% efficient in the JIT.
    StrictUpdates = strict_record_updates(St2#exprec.compile),
    {Update,St} =
        if
            not StrictUpdates, Us =/= [] ->
                {record_setel(Var, Name, Fs, Us), St2};
            true ->
                record_match(Var, Name, Anno, Fs, Us, St2)
        end,
    {{block,Anno,Pre ++ [{match,Anno,Var,R},Update]},St}.

%% record_match(Record, RecordName, Anno, [RecDefField], [Update], State)
%%  Build a 'case' expression to modify record fields.

record_match(R, Name, AnnoR, Fs, Us, St0) ->
    {Ps,News,St1} = record_upd_fs(Fs, Us, St0),
    NAnnoR = no_compiler_warning(AnnoR),
    RAnno = mark_record(AnnoR, St1),
    {{'case',AnnoR,R,
      [{clause,AnnoR,[{tuple,RAnno,[{atom,AnnoR,Name} | Ps]}],[],
        [{tuple,RAnno,[{atom,AnnoR,Name} | News]}]},
       {clause,NAnnoR,[{var,NAnnoR,'_'}],[],
        [call_error(NAnnoR, {tuple,NAnnoR,[{atom,NAnnoR,badrecord},R]})]}
      ]},
     St1}.

record_upd_fs([{record_field,Anno,{atom,_AnnoA,F},_Val} | Fs], Us, St0) ->
    {P,St1} = new_var(Anno, St0),
    {Ps,News,St2} = record_upd_fs(Fs, Us, St1),
    case find_field(F, Us) of
        {ok,New} -> {[P | Ps],[New | News],St2};
        error -> {[P | Ps],[P | News],St2}
    end;
record_upd_fs([], _, St) -> {[],[],St}.

%% record_setel(Record, RecordName, [RecDefField], [Update])
%%  Build a nested chain of setelement calls to build the 
%%  updated record tuple.

record_setel(R, Name, Fs, Us0) ->
    Us1 = foldl(fun ({record_field,Anno,Field,Val}, Acc) ->
                        {integer,_,FieldIndex} = I = index_expr(Anno, Field, Name, Fs),
                        [{FieldIndex,{I,Anno,Val}} | Acc]
                end, [], Us0),
    Us2 = sort(Us1),
    Us = [T || {_,T} <- Us2],
    AnnoR = element(2, hd(Us)),
    Wildcards = duplicate(length(Fs), {var,AnnoR,'_'}),
    NAnnoR = no_compiler_warning(AnnoR),
    %% Note: calling mark_record() here is not necessary since it is
    %% targeted at Dialyzer which always calls the compiler with
    %% 'strict_record_updates' meaning that record_setel() will never
    %% be called.
    {'case',AnnoR,R,
     [{clause,AnnoR,[{tuple,AnnoR,[{atom,AnnoR,Name} | Wildcards]}],[],
       [foldr(fun ({I,Anno,Val}, Acc) ->
                      {call,Anno,{remote,Anno,{atom,Anno,erlang},
				{atom,Anno,setelement}},[I,Acc,Val]} end,
              R, Us)]},
      {clause,NAnnoR,[{var,NAnnoR,'_'}],[],
       [call_error(NAnnoR, {tuple,NAnnoR,[{atom,NAnnoR,badrecord},R]})]}]}.

%% Expand a call to record_info/2. We have checked that it is not
%% shadowed by an import.

record_info_call(Anno, [{atom,_AnnoI,Info},{atom,_AnnoN,Name}], St) ->
    case Info of
        size ->
            {{integer,Anno,1+length(record_fields(Name, Anno, St))},St};
        fields ->
            {make_list(field_names(record_fields(Name, Anno, St)), Anno),St}
    end.

%% Break out expressions from an record update list and bind to new
%% variables. The idea is that we will evaluate all update expressions
%% before starting to update the record.

record_exprs(Us, St) ->
    record_exprs(Us, St, [], []).

record_exprs([{record_field,Anno,{atom,_AnnoA,_F}=Name,Val}=Field0 | Us], St0, Pre, Fs) ->
    case is_simple_val(Val) of
        true ->
            record_exprs(Us, St0, Pre, [Field0 | Fs]);
        false ->
            {Var,St} = new_var(Anno, St0),
            Bind = {match,Anno,Var,Val},
            Field = {record_field,Anno,Name,Var},
            record_exprs(Us, St, [Bind | Pre], [Field | Fs])
    end;
record_exprs([], St, Pre, Fs) ->
    {reverse(Pre),Fs,St}.

is_simple_val({var,_,_}) -> true;
is_simple_val(Val) ->
    try
        erl_parse:normalise(Val),
        true
    catch error:_ ->
        false
    end.

%% pattern_bin([Element], State) -> {[Element],[Variable],[UsedVar],State}.

pattern_bin(Es0, St) ->
    foldr(fun (E, Acc) -> pattern_element(E, Acc) end, {[],St}, Es0).

pattern_element({bin_element,Anno,Expr0,Size0,Type}, {Es,St0}) ->
    {Expr,St1} = pattern(Expr0, St0),
    {Size,St2} = case Size0 of
                     default -> {Size0,St1};
                     _ -> expr(Size0, St1)
                 end,
    {[{bin_element,Anno,Expr,Size,Type} | Es],St2}.

%% expr_bin([Element], State) -> {[Element],State}.

expr_bin(Es0, St) ->
    foldr(fun (E, Acc) -> bin_element(E, Acc) end, {[],St}, Es0).

bin_element({bin_element,Anno,Expr,Size,Type}, {Es,St0}) ->
    {Expr1,St1} = expr(Expr, St0),
    {Size1,St2} = if Size =:= default -> {default,St1};
                     true -> expr(Size, St1)
                  end,
    {[{bin_element,Anno,Expr1,Size1,Type} | Es],St2}.

new_vars(N, Anno, St) -> new_vars(N, Anno, St, []).

new_vars(N, Anno, St0, Vs) when N > 0 ->
    {V,St1} = new_var(Anno, St0),
    new_vars(N-1, Anno, St1, [V|Vs]);
new_vars(0, _Anno, St, Vs) -> {Vs,St}.

new_var(Anno, St0) ->
    {New,St1} = new_var_name(St0),
    {{var,Anno,New},St1}.

new_var_name(St) ->
    C = St#exprec.vcount,
    {list_to_atom("rec" ++ integer_to_list(C)),St#exprec{vcount=C+1}}.

make_list(Ts, Anno) ->
    foldr(fun (H, T) -> {cons,Anno,H,T} end, {nil,Anno}, Ts).

call_error(Anno, R) ->
    {call,Anno,{remote,Anno,{atom,Anno,erlang},{atom,Anno,error}},[R]}.

%%%
%%% Replace is_record/3 in guards with matching if possible.
%%%

optimize_is_record(H0, G0, #exprec{dialyzer=Dialyzer}) ->
    case opt_rec_vars(G0) of
	[] ->
	    {H0,G0};
	Rs0 ->
	    case Dialyzer of % no_is_record_optimization
		true ->
		    {H0,G0};
		false ->
		    {H,Rs} = opt_pattern_list(H0, Rs0),
		    G = opt_remove(G0, Rs),
		    {H,G}
	    end
    end.


%% opt_rec_vars(Guards) -> Vars.
%%  Search through the guard expression, looking for
%%  variables referenced in those is_record/3 calls that
%%  will fail the entire guard if they evaluate to 'false'
%%
%%  In the following code
%%
%%      f(X, Y, Z) when is_record(X, r1) andalso
%%                           (is_record(Y, r2) orelse is_record(Z, r3))
%%
%%  the entire guard will be false if the record test for
%%  X fails, and the clause can be rewritten to:
%%
%%      f({r1,...}=X, Y, Z) when true andalso
%%                              (is_record(Y, r2) or is_record(Z, r3))
%%
opt_rec_vars([G|Gs]) ->
    Rs = opt_rec_vars_1(G, orddict:new()),
    opt_rec_vars(Gs, Rs);
opt_rec_vars([]) -> orddict:new().

opt_rec_vars([G|Gs], Rs0) ->
    Rs1 = opt_rec_vars_1(G, orddict:new()),
    Rs = ordsets:intersection(Rs0, Rs1),
    opt_rec_vars(Gs, Rs);
opt_rec_vars([], Rs) -> Rs.

opt_rec_vars_1([T|Ts], Rs0) ->
    Rs = opt_rec_vars_2(T, Rs0),
    opt_rec_vars_1(Ts, Rs);
opt_rec_vars_1([], Rs) -> Rs.

opt_rec_vars_2({op,_,'and',A1,A2}, Rs) ->
    opt_rec_vars_1([A1,A2], Rs);
opt_rec_vars_2({op,_,'andalso',A1,A2}, Rs) ->
    opt_rec_vars_1([A1,A2], Rs);
opt_rec_vars_2({op,_,'orelse',Arg,{atom,_,fail}}, Rs) ->
    %% Since the second argument guarantees failure,
    %% it is safe to inspect the first argument.
    opt_rec_vars_2(Arg, Rs);
opt_rec_vars_2({call,Anno,
                {remote,_,{atom,_,erlang},{atom,_,is_record}=IsRecord},
		Args}, Rs) ->
    opt_rec_vars_2({call,Anno,IsRecord,Args}, Rs);
opt_rec_vars_2({call,_,{atom,_,is_record},
		[{var,_,V},{atom,_,Tag},{integer,_,Sz}]}, Rs)
  when is_integer(Sz), 0 < Sz, Sz < 100 ->
    orddict:store(V, {Tag,Sz}, Rs);
opt_rec_vars_2(_, Rs) -> Rs.

opt_pattern_list(Ps, Rs) ->
    opt_pattern_list(Ps, Rs, []).

opt_pattern_list([P0|Ps], Rs0, Acc) ->
    {P,Rs} = opt_pattern(P0, Rs0),
    opt_pattern_list(Ps, Rs, [P|Acc]);
opt_pattern_list([], Rs, Acc) ->
    {reverse(Acc),Rs}.

opt_pattern({var,_,V}=Var, Rs0) ->
    case orddict:find(V, Rs0) of
	{ok,{Tag,Sz}} ->
	    Rs = orddict:store(V, {remove,Tag,Sz}, Rs0),
	    {opt_var(Var, Tag, Sz),Rs};
	_ ->
	    {Var,Rs0}
    end;
opt_pattern({cons,Anno,H0,T0}, Rs0) ->
    {H,Rs1} = opt_pattern(H0, Rs0),
    {T,Rs} = opt_pattern(T0, Rs1),
    {{cons,Anno,H,T},Rs};
opt_pattern({tuple,Anno,Es0}, Rs0) ->
    {Es,Rs} = opt_pattern_list(Es0, Rs0),
    {{tuple,Anno,Es},Rs};
opt_pattern({match,Anno,Pa0,Pb0}, Rs0) ->
    {Pa,Rs1} = opt_pattern(Pa0, Rs0),
    {Pb,Rs} = opt_pattern(Pb0, Rs1),
    {{match,Anno,Pa,Pb},Rs};
opt_pattern(P, Rs) -> {P,Rs}.

opt_var({var,Anno,_}=Var, Tag, Sz) ->
    Rp = record_pattern(2, -1, ignore, Sz, Anno, [{atom,Anno,Tag}]),
    {match,Anno,{tuple,Anno,Rp},Var}.

opt_remove(Gs, Rs) ->
    [opt_remove_1(G, Rs) || G <- Gs].

opt_remove_1(Ts, Rs) ->
    [opt_remove_2(T, Rs) || T <- Ts].

opt_remove_2({op,Anno,'and'=Op,A1,A2}, Rs) ->
    {op,Anno,Op,opt_remove_2(A1, Rs),opt_remove_2(A2, Rs)};
opt_remove_2({op,Anno,'andalso'=Op,A1,A2}, Rs) ->
    {op,Anno,Op,opt_remove_2(A1, Rs),opt_remove_2(A2, Rs)};
opt_remove_2({op,Anno,'orelse',A1,A2}, Rs) ->
    {op,Anno,'orelse',opt_remove_2(A1, Rs),A2};
opt_remove_2({call,Anno,{remote,_,{atom,_,erlang},{atom,_,is_record}},
	      [{var,_,V},{atom,_,Tag},{integer,_,Sz}]}=A, Rs) ->
    case orddict:find(V, Rs) of
	{ok,{remove,Tag,Sz}} ->
	    {atom,Anno,true};
	_ ->
	    A
    end;
opt_remove_2({call,Anno,{atom,_,is_record},
	      [{var,_,V},{atom,_,Tag},{integer,_,Sz}]}=A, Rs) ->
    case orddict:find(V, Rs) of
	{ok,{remove,Tag,Sz}} ->
	    {atom,Anno,true};
	_ ->
	    A
    end;
opt_remove_2(A, _) -> A.

no_compiler_warning(Anno) ->
    erl_anno:set_generated(true, Anno).

mark_record(Anno, St) ->
    case St#exprec.dialyzer of
        true -> erl_anno:set_record(true, Anno);
        false -> Anno
    end.

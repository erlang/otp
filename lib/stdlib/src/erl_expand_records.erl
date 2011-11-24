%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% Purpose : Expand records into tuples.

%% N.B. Although structs (tagged tuples) are not yet allowed in the
%% language there is code included in pattern/2 and expr/3 (commented out)
%% that handles them.

-module(erl_expand_records).

-export([module/2]).

-import(lists, [map/2,foldl/3,foldr/3,sort/1,reverse/1,duplicate/2]).

-record(exprec, {compile=[],          % Compile flags
                 vcount=0,            % Variable counter
                 imports=[],          % Imports
                 records=dict:new(),  % Record definitions
		 trecords=sets:new(), % Typed records
		 uses_types=false,    % Are there -spec or -type in the module
                 strict_ra=[],        % strict record accesses
                 checked_ra=[]        % successfully accessed records
                }).

-spec(module(AbsForms, CompileOptions) -> AbsForms when
      AbsForms :: [erl_parse:abstract_form()],
      CompileOptions :: [compile:option()]).

%% Is is assumed that Fs is a valid list of forms. It should pass
%% erl_lint without errors.
module(Fs0, Opts0) ->
    Opts = compiler_options(Fs0) ++ Opts0,
    TRecs = typed_records(Fs0),
    UsesTypes = uses_types(Fs0),
    St0 = #exprec{compile = Opts, trecords = TRecs, uses_types = UsesTypes},
    {Fs,_St} = forms(Fs0, St0),
    Fs.

compiler_options(Forms) ->
    lists:flatten([C || {attribute,_,compile,C} <- Forms]).

typed_records(Fs) ->
    typed_records(Fs, sets:new()).

typed_records([{attribute,_L,type,{{record, Name},_Defs,[]}} | Fs], Trecs) ->
    typed_records(Fs, sets:add_element(Name, Trecs));
typed_records([_|Fs], Trecs) ->
    typed_records(Fs, Trecs);
typed_records([], Trecs) ->
    Trecs.

uses_types([{attribute,_L,spec,_}|_]) -> true;
uses_types([{attribute,_L,type,_}|_]) -> true;
uses_types([{attribute,_L,opaque,_}|_]) -> true;
uses_types([_|Fs]) -> uses_types(Fs);
uses_types([]) -> false.
    
forms([{attribute,L,record,{Name,Defs}} | Fs], St0) ->
    NDefs = normalise_fields(Defs),
    St = St0#exprec{records=dict:store(Name, NDefs, St0#exprec.records)},
    {Fs1, St1} = forms(Fs, St),
    %% Check if we need to keep the record information for usage in types.
    case St#exprec.uses_types of
	true ->
	    case sets:is_element(Name, St#exprec.trecords) of
		true -> {Fs1, St1};
		false -> {[{attribute,L,type,{{record,Name},Defs,[]}}|Fs1], St1}
	    end;
	false ->
	    {Fs1, St1}
    end;
forms([{attribute,L,import,Is} | Fs0], St0) ->
    St1 = import(Is, St0),
    {Fs,St2} = forms(Fs0, St1),
    {[{attribute,L,import,Is} | Fs], St2};
forms([{function,L,N,A,Cs0} | Fs0], St0) ->
    {Cs,St1} = clauses(Cs0, St0),
    {Fs,St2} = forms(Fs0, St1),
    {[{function,L,N,A,Cs} | Fs],St2};
forms([F | Fs0], St0) ->
    {Fs,St} = forms(Fs0, St0),
    {[F | Fs], St};
forms([], St) -> {[],St}.

clauses([{clause,Line,H0,G0,B0} | Cs0], St0) ->
    {H1,St1} = head(H0, St0),
    {G1,St2} = guard(G0, St1),
    {H,G} = optimize_is_record(H1, G1, St2),
    {B,St3} = exprs(B0, St2),
    {Cs,St4} = clauses(Cs0, St3),
    {[{clause,Line,H,G,B} | Cs],St4};
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
pattern({cons,Line,H,T}, St0) ->
    {TH,St1} = pattern(H, St0),
    {TT,St2} = pattern(T, St1),
    {{cons,Line,TH,TT},St2};
pattern({tuple,Line,Ps}, St0) ->
    {TPs,St1} = pattern_list(Ps, St0),
    {{tuple,Line,TPs},St1};
%%pattern({struct,Line,Tag,Ps}, St0) ->
%%    {TPs,TPsvs,St1} = pattern_list(Ps, St0),
%%    {{struct,Line,Tag,TPs},TPsvs,St1};
pattern({record_field,_,_,_}=M, St) ->
    {M,St};  % must be a package name
pattern({record_index,Line,Name,Field}, St) ->
    {index_expr(Line, Field, Name, record_fields(Name, St)),St};
pattern({record,Line,Name,Pfs}, St0) ->
    Fs = record_fields(Name, St0),
    {TMs,St1} = pattern_list(pattern_fields(Fs, Pfs), St0),
    {{tuple,Line,[{atom,Line,Name} | TMs]},St1};
pattern({bin,Line,Es0}, St0) ->
    {Es1,St1} = pattern_bin(Es0, St0),
    {{bin,Line,Es1},St1};
pattern({match,Line,Pat1, Pat2}, St0) ->
    {TH,St1} = pattern(Pat2, St0),
    {TT,St2} = pattern(Pat1, St1),
    {{match,Line,TT,TH},St2};
pattern({op,Line,Op,A0}, St0) ->
    {A,St1} = pattern(A0, St0),
    {{op,Line,Op,A},St1};
pattern({op,Line,Op,L0,R0}, St0) ->
    {L,St1} = pattern(L0, St0),
    {R,St2} = pattern(R0, St1),
    {{op,Line,Op,L,R},St2}.

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
guard_test1({call,Line,{atom,Lt,Tname},As}, St) ->
    Test = {atom,Lt,normalise_test(Tname, length(As))},
    expr({call,Line,Test,As}, St);
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

%% record_test(Line, Term, Name, Vs, St) -> TransformedExpr
%%  Generate code for is_record/1.

record_test(Line, Term, Name, St) ->
    case is_in_guard() of
        false ->
            record_test_in_body(Line, Term, Name, St);
        true ->
            record_test_in_guard(Line, Term, Name, St)
    end.

record_test_in_guard(Line, Term, Name, St) ->
    case not_a_tuple(Term) of
        true ->
            %% In case that later optimization passes have been turned off.
            expr({atom,Line,false}, St);
        false ->
            Fs = record_fields(Name, St),
            NLine = neg_line(Line),
            expr({call,NLine,{remote,NLine,{atom,NLine,erlang},{atom,NLine,is_record}},
                  [Term,{atom,Line,Name},{integer,Line,length(Fs)+1}]},
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

record_test_in_body(Line, Expr, Name, St0) ->
    %% As Expr may have side effects, we must evaluate it
    %% first and bind the value to a new variable.
    %% We must use also handle the case that Expr does not
    %% evaluate to a tuple properly.
    Fs = record_fields(Name, St0),
    {Var,St} = new_var(Line, St0),
    NLine = neg_line(Line),
    expr({block,Line,
          [{match,Line,Var,Expr},
           {call,NLine,{remote,NLine,{atom,NLine,erlang},
                        {atom,NLine,is_record}},
            [Var,{atom,Line,Name},{integer,Line,length(Fs)+1}]}]}, St).

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
%%    {{struct,Line,Tag,Es1},Esvs,Esus,St1};
expr({record_field,_,_,_}=M, St) ->
    {M,St};  % must be a package name
expr({record_index,Line,Name,F}, St) ->
    I = index_expr(Line, F, Name, record_fields(Name, St)),
    expr(I, St);
expr({record,Line,Name,Is}, St) ->
    expr({tuple,Line,[{atom,Line,Name} | 
                      record_inits(record_fields(Name, St), Is)]},
         St);
expr({record_field,Line,R,Name,F}, St) ->
    get_record_field(Line, R, F, Name, St);
expr({record,_,R,Name,Us}, St0) ->
    {Ue,St1} = record_update(R, Name, record_fields(Name, St0), Us, St0),
    expr(Ue, St1);
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
expr({'fun',_,{function,_F,_A}}=Fun, St) ->
    {Fun,St};
expr({'fun',_,{function,_M,_F,_A}}=Fun, St) ->
    {Fun,St};
expr({'fun',Line,{clauses,Cs0}}, St0) ->
    {Cs,St1} = clauses(Cs0, St0),
    {{'fun',Line,{clauses,Cs}},St1};
expr({call,Line,{atom,_,is_record},[A,{atom,_,Name}]}, St) ->
    record_test(Line, A, Name, St);
expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,is_record}},
      [A,{atom,_,Name}]}, St) ->
    record_test(Line, A, Name, St);
expr({call,Line,{tuple,_,[{atom,_,erlang},{atom,_,is_record}]},
      [A,{atom,_,Name}]}, St) ->
    record_test(Line, A, Name, St);
expr({call,Line,{atom,_La,N}=Atom,As0}, St0) ->
    {As,St1} = expr_list(As0, St0),
    Ar = length(As),
    case erl_internal:bif(N, Ar) of
        true ->
            {{call,Line,Atom,As},St1};
        false ->
            case imported(N, Ar, St1) of
                {yes,_Mod} ->
                    {{call,Line,Atom,As},St1};
                no ->
                    case {N,Ar} of
                        {record_info,2} ->
                            record_info_call(Line, As, St1);
                        _ ->
                            {{call,Line,Atom,As},St1}
                    end
            end
    end;
expr({call,Line,{record_field,_,_,_}=M,As0}, St0) ->
    {As,St1} = expr_list(As0, St0),
    {{call,Line,M,As},St1};
expr({call,Line,{remote,Lr,M,F},As0}, St0) ->
    {[M1,F1 | As1],St1} = expr_list([M,F | As0], St0),
    {{call,Line,{remote,Lr,M1,F1},As1},St1};
expr({call,Line,{tuple,Lt,[{atom,_,_}=M,{atom,_,_}=F]},As0}, St0) ->
    {As,St1} = expr_list(As0, St0),
    {{call,Line,{tuple,Lt,[M,F]},As},St1};
expr({call,Line,F,As0}, St0) ->
    {[Fun1 | As1],St1} = expr_list([F | As0], St0),
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
expr({op,Line,'not',A0}, St0) ->
    {A,St1} = bool_operand(A0, St0),
    {{op,Line,'not',A},St1};
expr({op,Line,Op,A0}, St0) ->
    {A,St1} = expr(A0, St0),
    {{op,Line,Op,A},St1};
expr({op,Line,Op,L0,R0}, St0) when Op =:= 'and'; 
                                   Op =:= 'or' ->
    {L,St1} = bool_operand(L0, St0),
    {R,St2} = bool_operand(R0, St1),
    {{op,Line,Op,L,R},St2};
expr({op,Line,Op,L0,R0}, St0) when Op =:= 'andalso';
                                   Op =:= 'orelse' ->
    {L,St1} = bool_operand(L0, St0),
    {R,St2} = bool_operand(R0, St1),
    {{op,Line,Op,L,R},St2#exprec{checked_ra = St1#exprec.checked_ra}};
expr({op,Line,Op,L0,R0}, St0) ->
    {L,St1} = expr(L0, St0),
    {R,St2} = expr(R0, St1),
    {{op,Line,Op,L,R},St2}.

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
    {New,NC} = lists:foldl(fun ({Key,_L,_R,_Sz}=A, {L,C}) ->
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
conj([{{Name,_Rp},L,R,Sz} | AL], E) ->
    NL = neg_line(L),
    T1 = {op,NL,'orelse',
          {call,NL,{atom,NL,is_record},[R,{atom,NL,Name},{integer,NL,Sz}]},
          {atom,NL,fail}},
    T2 = case conj(AL, none) of
        empty -> T1;
        C -> {op,NL,'and',C,T1}
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
		    {op,NL,'and',T2,{atom,NL,true}}
	    end;
	_ ->
	    {op,NL,'and',T2,E}
    end.

%% lc_tq(Line, Qualifiers, State) ->
%%      {[TransQual],State'}

lc_tq(Line, [{generate,Lg,P0,G0} | Qs0], St0) ->
    {G1,St1} = expr(G0, St0),
    {P1,St2} = pattern(P0, St1),
    {Qs1,St3} = lc_tq(Line, Qs0, St2),
    {[{generate,Lg,P1,G1} | Qs1],St3};
lc_tq(Line, [{b_generate,Lg,P0,G0} | Qs0], St0) ->
    {G1,St1} = expr(G0, St0),
    {P1,St2} = pattern(P0, St1),
    {Qs1,St3} = lc_tq(Line, Qs0, St2),
    {[{b_generate,Lg,P1,G1} | Qs1],St3};
lc_tq(Line, [F0 | Qs0], St0) ->
    %% Allow record/2 and expand out as guard test.
    case erl_lint:is_guard_test(F0) of
        true ->
            {F1,St1} = guard_test(F0, St0),
            {Qs1,St2} = lc_tq(Line, Qs0, St1),
            {[F1|Qs1],St2};
        false ->
            {F1,St1} = expr(F0, St0),
            {Qs1,St2} = lc_tq(Line, Qs0, St1),
            {[F1 | Qs1],St2}
    end;
lc_tq(_Line, [], St0) ->
    {[],St0#exprec{checked_ra = []}}.


%% normalise_fields([RecDef]) -> [Field].
%%  Normalise the field definitions to always have a default value. If
%%  none has been given then use 'undefined'.

normalise_fields(Fs) ->
    map(fun ({record_field,Lf,Field}) ->
                {record_field,Lf,Field,{atom,Lf,undefined}};
	    ({typed_record_field,{record_field,Lf,Field},_Type}) ->
		{record_field,Lf,Field,{atom,Lf,undefined}};
	    ({typed_record_field,Field,_Type}) ->
		Field;
            (F) -> F
	end, Fs).

%% record_fields(RecordName, State)
%% find_field(FieldName, Fields)

record_fields(R, St) -> dict:fetch(R, St#exprec.records).

find_field(F, [{record_field,_,{atom,_,F},Val} | _]) -> {ok,Val};
find_field(F, [_ | Fs]) -> find_field(F, Fs);
find_field(_, []) -> error.

%% field_names(RecFields) -> [Name].
%%  Return a list of the field names structures.

field_names(Fs) ->
    map(fun ({record_field,_,Field,_Val}) -> Field end, Fs).

%% index_expr(Line, FieldExpr, Name, Fields) -> IndexExpr.
%%  Return an expression which evaluates to the index of a
%%  field. Currently only handle the case where the field is an
%%  atom. This expansion must be passed through expr again.

index_expr(Line, {atom,_,F}, _Name, Fs) ->
    {integer,Line,index_expr(F, Fs, 2)}.

index_expr(F, [{record_field,_,{atom,_,F},_} | _], I) -> I;
index_expr(F, [_ | Fs], I) -> index_expr(F, Fs, I+1).

%% get_record_field(Line, RecExpr, FieldExpr, Name, St) -> {Expr,St'}.
%%  Return an expression which verifies that the type of record
%%  is correct and then returns the value of the field.
%%  This expansion must be passed through expr again.

get_record_field(Line, R, Index, Name, St) ->
    case strict_record_tests(St#exprec.compile) of
        false ->
            sloppy_get_record_field(Line, R, Index, Name, St);
        true ->
            strict_get_record_field(Line, R, Index, Name, St)
    end.

strict_get_record_field(Line, R, {atom,_,F}=Index, Name, St0) ->
    case is_in_guard() of
        false ->                                %Body context.
            {Var,St} = new_var(Line, St0),
            Fs = record_fields(Name, St),
            I = index_expr(F, Fs, 2),
            P = record_pattern(2, I, Var, length(Fs)+1, Line, [{atom,Line,Name}]),
            NLine = neg_line(Line),
	    E = {'case',NLine,R,
		     [{clause,NLine,[{tuple,NLine,P}],[],[Var]},
		      {clause,NLine,[{var,NLine,'_'}],[],
		       [{call,NLine,{remote,NLine,
				    {atom,NLine,erlang},
				    {atom,NLine,error}},
			 [{tuple,NLine,[{atom,NLine,badrecord},{atom,NLine,Name}]}]}]}]},
            expr(E, St);
        true ->                                 %In a guard.
            Fs = record_fields(Name, St0),
            I = index_expr(Line, Index, Name, Fs),
            {ExpR,St1}  = expr(R, St0),
            %% Just to make comparison simple:
            ExpRp = erl_lint:modify_line(ExpR, fun(_L) -> 0 end),
            RA = {{Name,ExpRp},Line,ExpR,length(Fs)+1},
            St2 = St1#exprec{strict_ra = [RA | St1#exprec.strict_ra]},
            {{call,Line,{atom,Line,element},[I,ExpR]},St2}
    end.

record_pattern(I, I, Var, Sz, Line, Acc) ->
    record_pattern(I+1, I, Var, Sz, Line, [Var | Acc]);
record_pattern(Cur, I, Var, Sz, Line, Acc) when Cur =< Sz ->
    record_pattern(Cur+1, I, Var, Sz, Line, [{var,Line,'_'} | Acc]);
record_pattern(_, _, _, _, _, Acc) -> reverse(Acc).

sloppy_get_record_field(Line, R, Index, Name, St) ->
    Fs = record_fields(Name, St),
    I = index_expr(Line, Index, Name, Fs),
    expr({call,Line,{atom,Line,element},[I,R]}, St).

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
    map(fun ({record_field,L,{atom,_,F},_}) ->
                case find_field(F, Ms) of
                    {ok,Match} -> Match;
                    error when Wildcard =:= none -> {var,L,'_'};
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
    Line = element(2, R),
    {Pre,Us,St1} = record_exprs(Us0, St0),
    Nf = length(Fs),                            %# of record fields
    Nu = length(Us),                            %# of update fields
    Nc = Nf - Nu,                               %# of copy fields

    %% We need a new variable for the record expression
    %% to guarantee that it is only evaluated once.
    {Var,St2} = new_var(Line, St1),

    StrictUpdates = strict_record_updates(St2#exprec.compile),

    %% Try to be intelligent about which method of updating record to use.
    {Update,St} =
        if
            Nu =:= 0 -> 
                record_match(Var, Name, Line, Fs, Us, St2);
            Nu =< Nc, not StrictUpdates ->      %Few fields updated
                {record_setel(Var, Name, Fs, Us), St2};
            true ->                             %The wide area inbetween
                record_match(Var, Name, element(2, hd(Us)), Fs, Us, St2)
        end,
    {{block,Line,Pre ++ [{match,Line,Var,R},Update]},St}.

%% record_match(Record, RecordName, [RecDefField], [Update], State)
%%  Build a 'case' expression to modify record fields.

record_match(R, Name, Lr, Fs, Us, St0) ->
    {Ps,News,St1} = record_upd_fs(Fs, Us, St0),
    NLr = neg_line(Lr),
    {{'case',Lr,R,
      [{clause,Lr,[{tuple,Lr,[{atom,Lr,Name} | Ps]}],[],
        [{tuple,Lr,[{atom,Lr,Name} | News]}]},
       {clause,NLr,[{var,NLr,'_'}],[],
        [call_error(NLr, {tuple,NLr,[{atom,NLr,badrecord},{atom,NLr,Name}]})]}
      ]},
     St1}.

record_upd_fs([{record_field,Lf,{atom,_La,F},_Val} | Fs], Us, St0) ->
    {P,St1} = new_var(Lf, St0),
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
    Us1 = foldl(fun ({record_field,Lf,Field,Val}, Acc) ->
                        {integer,_,FieldIndex} = I = index_expr(Lf, Field, Name, Fs),
                        [{FieldIndex,{I,Lf,Val}} | Acc]
                end, [], Us0),
    Us2 = sort(Us1),
    Us = [T || {_,T} <- Us2],
    Lr = element(2, hd(Us)),
    Wildcards = duplicate(length(Fs), {var,Lr,'_'}),
    NLr = neg_line(Lr),
    {'case',Lr,R,
     [{clause,Lr,[{tuple,Lr,[{atom,Lr,Name} | Wildcards]}],[],
       [foldr(fun ({I,Lf,Val}, Acc) ->
                      {call,Lf,{atom,Lf,setelement},[I,Acc,Val]} end,
              R, Us)]},
      {clause,NLr,[{var,NLr,'_'}],[],
       [call_error(NLr, {tuple,NLr,[{atom,NLr,badrecord},{atom,NLr,Name}]})]}]}.

%% Expand a call to record_info/2. We have checked that it is not
%% shadowed by an import.

record_info_call(Line, [{atom,_Li,Info},{atom,_Ln,Name}], St) ->
    case Info of
        size ->
            {{integer,Line,1+length(record_fields(Name, St))},St};
        fields ->
            {make_list(field_names(record_fields(Name, St)), Line),St}
    end.

%% Break out expressions from an record update list and bind to new
%% variables. The idea is that we will evaluate all update expressions
%% before starting to update the record.

record_exprs(Us, St) ->
    record_exprs(Us, St, [], []).

record_exprs([{record_field,Lf,{atom,_La,_F}=Name,Val}=Field0 | Us], St0, Pre, Fs) ->
    case is_simple_val(Val) of
        true ->
            record_exprs(Us, St0, Pre, [Field0 | Fs]);
        false ->
            {Var,St} = new_var(Lf, St0),
            Bind = {match,Lf,Var,Val},
            Field = {record_field,Lf,Name,Var},
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

pattern_element({bin_element,Line,Expr0,Size,Type}, {Es,St0}) ->
    {Expr,St1} = pattern(Expr0, St0),
    {[{bin_element,Line,Expr,Size,Type} | Es],St1}.

%% expr_bin([Element], State) -> {[Element],State}.

expr_bin(Es0, St) ->
    foldr(fun (E, Acc) -> bin_element(E, Acc) end, {[],St}, Es0).

bin_element({bin_element,Line,Expr,Size,Type}, {Es,St0}) ->
    {Expr1,St1} = expr(Expr, St0),
    {Size1,St2} = if Size =:= default -> {default,St1};
                     true -> expr(Size, St1)
                  end,
    {[{bin_element,Line,Expr1,Size1,Type} | Es],St2}.

new_var(L, St0) ->
    {New,St1} = new_var_name(St0),
    {{var,L,New},St1}.

new_var_name(St) ->
    C = St#exprec.vcount,
    {list_to_atom("rec" ++ integer_to_list(C)),St#exprec{vcount=C+1}}.

make_list(Ts, Line) ->
    foldr(fun (H, T) -> {cons,Line,H,T} end, {nil,Line}, Ts).

call_error(L, R) ->
    {call,L,{remote,L,{atom,L,erlang},{atom,L,error}},[R]}.

import({Mod,Fs}, St) ->
    St#exprec{imports=add_imports(Mod, Fs, St#exprec.imports)};
import(_Mod0, St) ->
    St.

add_imports(Mod, [F | Fs], Is) ->
    add_imports(Mod, Fs, orddict:store(F, Mod, Is));
add_imports(_, [], Is) -> Is.

imported(F, A, St) ->
    case orddict:find({F,A}, St#exprec.imports) of
        {ok,Mod} -> {yes,Mod};
        error -> no
    end.

%%%
%%% Replace is_record/3 in guards with matching if possible.
%%%

optimize_is_record(H0, G0, #exprec{compile=Opts}) ->
    case opt_rec_vars(G0) of
	[] ->
	    {H0,G0};
	Rs0 ->
	    case lists:member(no_is_record_optimization, Opts) of
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
opt_rec_vars_2({call,_,{remote,_,{atom,_,erlang},{atom,_,is_record}},
		[{var,_,V},{atom,_,Tag},{integer,_,Sz}]}, Rs) ->
    orddict:store(V, {Tag,Sz}, Rs);
opt_rec_vars_2({call,_,{atom,_,is_record},
		[{var,_,V},{atom,_,Tag},{integer,_,Sz}]}, Rs) ->
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
opt_pattern({cons,Line,H0,T0}, Rs0) ->
    {H,Rs1} = opt_pattern(H0, Rs0),
    {T,Rs} = opt_pattern(T0, Rs1),
    {{cons,Line,H,T},Rs};
opt_pattern({tuple,Line,Es0}, Rs0) ->
    {Es,Rs} = opt_pattern_list(Es0, Rs0),
    {{tuple,Line,Es},Rs};
opt_pattern({match,Line,Pa0,Pb0}, Rs0) ->
    {Pa,Rs1} = opt_pattern(Pa0, Rs0),
    {Pb,Rs} = opt_pattern(Pb0, Rs1),
    {{match,Line,Pa,Pb},Rs};
opt_pattern(P, Rs) -> {P,Rs}.

opt_var({var,Line,_}=Var, Tag, Sz) ->
    Rp = record_pattern(2, -1, ignore, Sz, Line, [{atom,Line,Tag}]),
    {match,Line,{tuple,Line,Rp},Var}.

opt_remove(Gs, Rs) ->
    [opt_remove_1(G, Rs) || G <- Gs].

opt_remove_1(Ts, Rs) ->
    [opt_remove_2(T, Rs) || T <- Ts].

opt_remove_2({op,L,'and'=Op,A1,A2}, Rs) ->
    {op,L,Op,opt_remove_2(A1, Rs),opt_remove_2(A2, Rs)};
opt_remove_2({op,L,'andalso'=Op,A1,A2}, Rs) ->
    {op,L,Op,opt_remove_2(A1, Rs),opt_remove_2(A2, Rs)};
opt_remove_2({op,L,'orelse',A1,A2}, Rs) ->
    {op,L,'orelse',opt_remove_2(A1, Rs),A2};
opt_remove_2({call,Line,{remote,_,{atom,_,erlang},{atom,_,is_record}},
	      [{var,_,V},{atom,_,Tag},{integer,_,Sz}]}=A, Rs) ->
    case orddict:find(V, Rs) of
	{ok,{remove,Tag,Sz}} ->
	    {atom,Line,true};
	_ ->
	    A
    end;
opt_remove_2({call,Line,{atom,_,is_record},
	      [{var,_,V},{atom,_,Tag},{integer,_,Sz}]}=A, Rs) ->
    case orddict:find(V, Rs) of
	{ok,{remove,Tag,Sz}} ->
	    {atom,Line,true};
	_ ->
	    A
    end;
opt_remove_2(A, _) -> A.

neg_line(L) ->
    erl_parse:set_line(L, fun(Line) -> -abs(Line) end).

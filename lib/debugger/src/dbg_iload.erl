%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2017. All Rights Reserved.
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
-module(dbg_iload).

-export([load_mod/4]).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% load_mod(Mod, File, Binary, Db) -> {ok, Mod}
%%   Mod = module()
%%   File = string() Source file (including path)
%%   Binary = binary()
%%   Db = ETS identifier
%% Load a new module into the database.
%%
%% We want the loading of a module to be synchronous so that no other
%% process tries to interpret code in a module not being completely
%% loaded. This is achieved as this function is called from
%% dbg_iserver. We are suspended until the module has been loaded.
%%--------------------------------------------------------------------
-spec load_mod(Mod, file:filename(), binary(), ets:tid()) ->
        {'ok', Mod} when Mod :: atom().

load_mod(Mod, File, Binary, Db) ->
    Flag = process_flag(trap_exit, true),
    Pid = spawn_link(load_mod1(Mod, File, Binary, Db)),
    receive
	{'EXIT', Pid, What} ->
	    process_flag(trap_exit, Flag),
	    What
    end.

-spec load_mod1(atom(), file:filename(), binary(), ets:tid()) ->
                       fun(() -> no_return()).

load_mod1(Mod, File, Binary, Db) ->
    fun() ->
            store_module(Mod, File, Binary, Db),
            exit({ok, Mod})
    end.

%%====================================================================
%% Internal functions
%%====================================================================

store_module(Mod, File, Binary, Db) ->
    {interpreter_module, Exp, Abst, Src, MD5} = binary_to_term(Binary),
    Forms = case abstr(Abst) of
		{abstract_v1,_} ->
		    exit({Mod,too_old_beam_file});
		{abstract_v2,_} ->
		    exit({Mod,too_old_beam_file});
		{raw_abstract_v1,Code0} ->
                    Code = interpret_file_attribute(Code0),
		    standard_transforms(Code)
	    end,
    dbg_idb:insert(Db, mod_file, File),
    dbg_idb:insert(Db, defs, []),

    put(vcount, 0),
    put(fun_count, 0),
    put(funs, []),
    put(mod_md5, MD5),
    store_forms(Forms, Mod, Db, Exp),
    erase(mod_md5),
    erase(current_function),
    %% store_funs(Db, Mod),
    erase(vcount),
    erase(funs),
    erase(fun_count),
    
    NewBinary = store_mod_line_no(Mod, Db, binary_to_list(Src)),
    dbg_idb:insert(Db, mod_bin, NewBinary),
    dbg_idb:insert(Db, mod_raw, <<Src/binary,0:8>>). %% Add eos

standard_transforms(Forms0) ->
    Forms = erl_expand_records:module(Forms0, []),
    erl_internal:add_predefined_functions(Forms).


%% Adjust line numbers using the file/2 attribute. 
%% Also take the absolute value of line numbers.
%% This simple fix will make the marker point at the correct line
%% (assuming the file attributes are correct) in the source; it will
%% not point at code in included files.
interpret_file_attribute(Code) ->
    epp:interpret_file_attribute(Code).

abstr(Bin) when is_binary(Bin) -> binary_to_term(Bin);
abstr(Term) -> Term.

% store_funs(Db, Mod) ->
%     store_funs_1(get(funs), Db, Mod).

% store_funs_1([{Name,Index,Uniq,_,_,Arity,Cs}|Fs], Db, Mod) ->
%     dbg_idb:insert(Db, {Mod,Name,Arity,false}, Cs),
%     dbg_idb:insert(Db, {'fun',Mod,Index,Uniq}, {Name,Arity,Cs}),
%     store_funs_1(Fs, Db, Mod);
% store_funs_1([], _, _) -> ok.

store_forms([{function,_,Name,Arity,Cs0}|Fs], Mod, Db, Exp) ->
    FA = {Name,Arity},
    put(current_function, FA),
    Cs = clauses(Cs0),
    Exported = lists:member(FA, Exp),
    dbg_idb:insert(Db, {Mod,Name,Arity,Exported}, Cs),
    store_forms(Fs, Mod, Db, Exp);
store_forms([{attribute,_,_Name,_Val}|Fs], Mod, Db, Exp) ->
    store_forms(Fs, Mod, Db, Exp);
store_forms([_|Fs],  Mod, Db, Exp) ->
    %% Ignore other forms such as {eof,_} or {warning,_}.
    store_forms(Fs, Mod, Db, Exp);
store_forms([], _, _, _) ->
    ok.

store_mod_line_no(Mod, Db, Contents) ->
    store_mod_line_no(Mod, Db, Contents, 1, 0, []).

store_mod_line_no(_, _, [], _, _, NewCont) ->
    list_to_binary(lists:reverse(NewCont));
store_mod_line_no(Mod, Db, Contents, LineNo, Pos, NewCont) when is_integer(LineNo) ->
    {ContTail,Pos1,NewCont1} = store_line(Mod, Db, Contents, LineNo, Pos, NewCont),
    store_mod_line_no(Mod, Db, ContTail, LineNo+1, Pos1, NewCont1).

store_line(_, Db, Contents, LineNo, Pos, NewCont) ->
    {ContHead,ContTail,PosNL} = get_nl(Contents,Pos+8,[]),
    dbg_idb:insert(Db,LineNo,{Pos+8,PosNL}),
    {ContTail,PosNL+1,[make_lineno(LineNo, 8, ContHead)|NewCont]}.

make_lineno(N, P, Acc) ->
    S = integer_to_list(N),
    S ++ [$:|spaces(P-length(S)-1, Acc)].

spaces(P, Acc) when P > 0 ->
    spaces(P-1, [$\s|Acc]);
spaces(_, Acc) -> Acc.

get_nl([10|T],Pos,Head) -> {lists:reverse([10|Head]),T,Pos};
get_nl([H|T],Pos,Head) ->
    get_nl(T,Pos+1,[H|Head]);
get_nl([],Pos,Head) -> {lists:reverse(Head),[],Pos}.

%%% Rewrite the abstract syntax tree to that it will be easier (== faster)
%%% to interpret.

clauses([C0|Cs]) ->
    C1 = clause(C0, true),
    [C1|clauses(Cs)];
clauses([]) -> [].

clause({clause,Anno,H0,G0,B0}, Lc) ->
    H1 = head(H0),
    G1 = guard(G0),
    B1 = exprs(B0, Lc),
    {clause,ln(Anno),H1,G1,B1}.

head(Ps) -> patterns(Ps).

%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

patterns([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|patterns(Ps)];
patterns([]) -> [].

%%  N.B. Only valid patterns are included here.

pattern({var,Anno,V}) -> {var,ln(Anno),V};
pattern({char,Anno,I}) -> {value,ln(Anno),I};
pattern({integer,Anno,I}) -> {value,ln(Anno),I};
pattern({match,Anno,Pat1,Pat2}) ->
    {match,ln(Anno),pattern(Pat1),pattern(Pat2)};
pattern({float,Anno,F}) -> {value,ln(Anno),F};
pattern({atom,Anno,A}) -> {value,ln(Anno),A};
pattern({string,Anno,S}) -> {value,ln(Anno),S};
pattern({nil,Anno}) -> {value,ln(Anno),[]};
pattern({cons,Anno,H0,T0}) ->
    H1 = pattern(H0),
    T1 = pattern(T0),
    {cons,ln(Anno),H1,T1};
pattern({tuple,Anno,Ps0}) ->
    Ps1 = pattern_list(Ps0),
    {tuple,ln(Anno),Ps1};
pattern({map,Anno,Fs0}) ->
    Fs1 = lists:map(fun ({map_field_exact,L,K,V}) ->
                            {map_field_exact,L,gexpr(K),pattern(V)}
                    end, Fs0),
    {map,ln(Anno),Fs1};
pattern({op,_,'-',{integer,Anno,I}}) ->
    {value,ln(Anno),-I};
pattern({op,_,'+',{integer,Anno,I}}) ->
    {value,ln(Anno),I};
pattern({op,_,'-',{char,Anno,I}}) ->
    {value,ln(Anno),-I};
pattern({op,_,'+',{char,Anno,I}}) ->
    {value,ln(Anno),I};
pattern({op,_,'-',{float,Anno,I}}) ->
    {value,ln(Anno),-I};
pattern({op,_,'+',{float,Anno,I}}) ->
    {value,ln(Anno),I};
pattern({bin,Anno,Grp}) ->
    Grp1 = pattern_list(bin_expand_strings(Grp)),
    {bin,ln(Anno),Grp1};
pattern({bin_element,Anno,Expr0,Size0,Type0}) ->
    {Size1,Type} = make_bit_type(Anno, Size0, Type0),
    Expr1 = pattern(Expr0),
    Expr = coerce_to_float(Expr1, Type0),
    Size = expr(Size1, false),
    {bin_element,ln(Anno),Expr,Size,Type};
%% Evaluate compile-time expressions.
pattern({op,_,'++',{nil,_},R}) ->
    pattern(R);
pattern({op,_,'++',{cons,Li,H,T},R}) ->
    pattern({cons,Li,H,{op,Li,'++',T,R}});
pattern({op,_,'++',{string,Li,L},R}) ->
    pattern(string_to_conses(Li, L, R));
pattern({op,_Line,_Op,_A}=Op) ->
    pattern(erl_eval:partial_eval(Op));
pattern({op,_Line,_Op,_L,_R}=Op) ->
    pattern(erl_eval:partial_eval(Op)).

string_to_conses(Anno, Cs, Tail) ->
    lists:foldr(fun (C, T) -> {cons,Anno,{char,Anno,C},T} end, Tail, Cs).

coerce_to_float({value,Anno,Int}=E, [float|_]) when is_integer(Int) ->
    try
	{value,Anno,float(Int)}
    catch
        error:badarg -> E
    end;
coerce_to_float(E, _) -> E.

%%  These patterns are processed "in parallel" for purposes of variable
%%  definition etc.

pattern_list([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|pattern_list(Ps)];
pattern_list([]) -> [].

guard([G0|Gs]) ->
    G1 = and_guard(G0),
    [G1|guard(Gs)];
guard([]) -> [].

and_guard([G0|Gs]) ->
    G1 = guard_test(G0),
    [G1|and_guard(Gs)];
and_guard([]) -> [].

guard_test({call,Anno,{remote,_,{atom,_,erlang},{atom,_,F}},As0}) ->
    As = gexpr_list(As0),
    {safe_bif,ln(Anno),erlang,F,As};
guard_test({op,Anno,Op,L0}) ->
    true = erl_internal:arith_op(Op, 1) orelse %Assertion.
	erl_internal:bool_op(Op, 1),
    L1 = gexpr(L0),
    {safe_bif,ln(Anno),erlang,Op,[L1]};
guard_test({op,Anno,Op,L0,R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    L1 = gexpr(L0),
    R1 = gexpr(R0),				%They see the same variables
    {Op,ln(Anno),L1,R1};
guard_test({op,Anno,Op,L0,R0}) ->
    true = erl_internal:comp_op(Op, 2) orelse	%Assertion.
	erl_internal:bool_op(Op, 2) orelse
        erl_internal:arith_op(Op, 2),
    L1 = gexpr(L0),
    R1 = gexpr(R0),				%They see the same variables
    {safe_bif,ln(Anno),erlang,Op,[L1,R1]};
guard_test({var,_,_}=V) ->V;    % Boolean var
guard_test({atom,Anno,true}) -> {value,ln(Anno),true};
%% All other constants at this level means false.
guard_test({atom,Anno,_}) -> {value,ln(Anno),false};
guard_test({integer,Anno,_}) -> {value,ln(Anno),false};
guard_test({char,Anno,_}) -> {value,ln(Anno),false};
guard_test({float,Anno,_}) -> {value,ln(Anno),false};
guard_test({string,Anno,_}) -> {value,ln(Anno),false};
guard_test({nil,Anno}) -> {value,ln(Anno),false};
guard_test({cons,Anno,_,_}) -> {value,ln(Anno),false};
guard_test({tuple,Anno,_}) -> {value,ln(Anno),false};
guard_test({map,Anno,_}) -> {value,ln(Anno),false};
guard_test({map,Anno,_,_}) -> {value,ln(Anno),false};
guard_test({bin,Anno,_}) ->  {value,ln(Anno),false}.

gexpr({var,Anno,V}) -> {var,ln(Anno),V};
gexpr({integer,Anno,I}) -> {value,ln(Anno),I};
gexpr({char,Anno,I}) -> {value,ln(Anno),I};
gexpr({float,Anno,F}) -> {value,ln(Anno),F};
gexpr({atom,Anno,A}) -> {value,ln(Anno),A};
gexpr({string,Anno,S}) -> {value,ln(Anno),S};
gexpr({nil,Anno}) -> {value,ln(Anno),[]};
gexpr({cons,Anno,H0,T0}) ->
    case {gexpr(H0),gexpr(T0)} of
	{{value,Line,H1},{value,Line,T1}} -> {value,Line,[H1|T1]};
	{H1,T1} -> {cons,ln(Anno),H1,T1}
    end;
gexpr({tuple,Anno,Es0}) ->
    Es1 = gexpr_list(Es0),
    {tuple,ln(Anno),Es1};
gexpr({map,Anno,Fs0}) ->
    new_map(Fs0, Anno, fun gexpr/1);
gexpr({map,Anno,E0,Fs0}) ->
    E1 = gexpr(E0),
    Fs1 = map_fields(Fs0, fun gexpr/1),
    {map,ln(Anno),E1,Fs1};
gexpr({bin,Anno,Flds0}) ->
    Flds = gexpr_list(bin_expand_strings(Flds0)),
    {bin,ln(Anno),Flds};
gexpr({bin_element,Anno,Expr0,Size0,Type0}) ->
    {Size1,Type} = make_bit_type(Anno, Size0, Type0),
    Expr = gexpr(Expr0),
    Size = gexpr(Size1),
    {bin_element,ln(Anno),Expr,Size,Type};
%%% The erl_expand_records pass has added the module name 'erlang' to
%%% all BIF calls, even in guards.
gexpr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,self}},[]}) ->
    {dbg,ln(Anno),self,[]};
gexpr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,F}},As0}) ->
    As = gexpr_list(As0),
    {safe_bif,ln(Anno),erlang,F,As};
gexpr({op,Anno,Op,A0}) ->
    erl_internal:arith_op(Op, 1),
    A1 = gexpr(A0),
    {safe_bif,ln(Anno),erlang,Op,[A1]};
gexpr({op,Anno,Op,L0,R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    L1 = gexpr(L0),
    R1 = gexpr(R0),			%They see the same variables
    {Op,ln(Anno),L1,R1};
gexpr({op,Anno,Op,L0,R0}) ->
    true = erl_internal:arith_op(Op, 2) orelse erl_internal:comp_op(Op, 2)
	orelse erl_internal:bool_op(Op, 2),
    L1 = gexpr(L0),
    R1 = gexpr(R0),			%They see the same variables
    {safe_bif,ln(Anno),erlang,Op,[L1,R1]}.

%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list([E0|Es]) ->
    E1 = gexpr(E0),
    [E1|gexpr_list(Es)];
gexpr_list([]) -> [].

%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E], Lc) ->
    [expr(E, Lc)];
exprs([E0|Es], Lc) ->
    E1 = expr(E0, false),
    [E1|exprs(Es, Lc)];
exprs([], _Lc) -> [].

expr({var,Anno,V}, _Lc) -> {var,ln(Anno),V};
expr({integer,Anno,I}, _Lc) -> {value,ln(Anno),I};
expr({char,Anno,I}, _Lc) -> {value,ln(Anno),I};
expr({float,Anno,F}, _Lc) -> {value,ln(Anno),F};
expr({atom,Anno,A}, _Lc) -> {value,ln(Anno),A};
expr({string,Anno,S}, _Lc) -> {value,ln(Anno),S};
expr({nil,Anno}, _Lc) -> {value,ln(Anno),[]};
expr({cons,Anno,H0,T0}, _Lc) ->
    case {expr(H0, false),expr(T0, false)} of
	{{value,Line,H1},{value,Line,T1}} -> {value,Line,[H1|T1]};
	{H1,T1} -> {cons,ln(Anno),H1,T1}
    end;
expr({tuple,Anno,Es0}, _Lc) ->
    Es1 = expr_list(Es0),
    {tuple,ln(Anno),Es1};
expr({map,Anno,Fs}, _Lc) ->
    new_map(Fs, Anno, fun (E) -> expr(E, false) end);
expr({map,Anno,E0,Fs0}, _Lc) ->
    E1 = expr(E0, false),
    Fs1 = map_fields(Fs0),
    {map,ln(Anno),E1,Fs1};
expr({block,Anno,Es0}, Lc) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0, Lc),
    {block,ln(Anno),Es1};
expr({'if',Anno,Cs0}, Lc) ->
    Cs1 = icr_clauses(Cs0, Lc),
    {'if',ln(Anno),Cs1};
expr({'case',Anno,E0,Cs0}, Lc) ->
    E1 = expr(E0, false),
    Cs1 = icr_clauses(Cs0, Lc),
    {'case',ln(Anno),E1,Cs1};
expr({'receive',Anno,Cs0}, Lc) ->
    Cs1 = icr_clauses(Cs0, Lc),
    {'receive',ln(Anno),Cs1};
expr({'receive',Anno,Cs0,To0,ToEs0}, Lc) ->
    To1 = expr(To0, false),
    ToEs1 = exprs(ToEs0, Lc),
    Cs1 = icr_clauses(Cs0, Lc),
    {'receive',ln(Anno),Cs1,To1,ToEs1};
expr({'fun',Anno,{clauses,Cs0}}, _Lc) ->
    %% New R10B-2 format (abstract_v2).
    Cs = fun_clauses(Cs0),
    Name = new_fun_name(),
    {make_fun,ln(Anno),Name,Cs};
expr({'fun',Anno,{function,F,A}}, _Lc) ->
    %% New R8 format (abstract_v2).
    Line = ln(Anno),
    As = new_vars(A, Line),
    Name = new_fun_name(),
    Cs = [{clause,Line,As,[],[{local_call,Line,F,As,true}]}],
    {make_fun,Line,Name,Cs};
expr({named_fun,Anno,FName,Cs0}, _Lc) ->
    Cs = fun_clauses(Cs0),
    Name = new_fun_name(),
    {make_named_fun,ln(Anno),Name,FName,Cs};
expr({'fun',Anno,{function,{atom,_,M},{atom,_,F},{integer,_,A}}}, _Lc)
  when 0 =< A, A =< 255 ->
    %% New format in R15 for fun M:F/A (literal values).
    {value,ln(Anno),erlang:make_fun(M, F, A)};
expr({'fun',Anno,{function,M,F,A}}, _Lc) ->
    %% New format in R15 for fun M:F/A (one or more variables).
    MFA = expr_list([M,F,A]),
    {make_ext_fun,ln(Anno),MFA};
expr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,self}},[]}, _Lc) ->
    {dbg,ln(Anno),self,[]};
expr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,get_stacktrace}},[]}, _Lc) ->
    {dbg,ln(Anno),get_stacktrace,[]};
expr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,throw}},[_]=As}, _Lc) ->
    {dbg,ln(Anno),throw,expr_list(As)};
expr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,error}},[_]=As}, _Lc) ->
    {dbg,ln(Anno),error,expr_list(As)};
expr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,exit}},[_]=As}, _Lc) ->
    {dbg,ln(Anno),exit,expr_list(As)};
expr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,raise}},[_,_,_]=As}, _Lc) ->
    {dbg,ln(Anno),raise,expr_list(As)};
expr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,apply}},[_,_,_]=As0}, Lc) ->
    As = expr_list(As0),
    {apply,ln(Anno),As,Lc};
expr({call,Anno,{remote,_,{atom,_,Mod},{atom,_,Func}},As0}, Lc) ->
    As = expr_list(As0),
    case erlang:is_builtin(Mod, Func, length(As)) of
	false ->
	    {call_remote,ln(Anno),Mod,Func,As,Lc};
	true ->
	    case bif_type(Mod, Func, length(As0)) of
		safe -> {safe_bif,ln(Anno),Mod,Func,As};
		unsafe ->{bif,ln(Anno),Mod,Func,As}
	    end
    end;
expr({call,Anno,{remote,_,Mod0,Func0},As0}, Lc) ->
    %% New R8 format (abstract_v2).
    Mod = expr(Mod0, false),
    Func = expr(Func0, false),
    As = consify(expr_list(As0)),
    {apply,ln(Anno),[Mod,Func,As],Lc};
expr({call,Anno,{atom,_,Func},As0}, Lc) ->
    As = expr_list(As0),
    {local_call,ln(Anno),Func,As,Lc};
expr({call,Anno,Fun0,As0}, Lc) ->
    Fun = expr(Fun0, false),
    As = expr_list(As0),
    {apply_fun,ln(Anno),Fun,As,Lc};
expr({'catch',Anno,E0}, _Lc) ->
    %% No new variables added.
    E1 = expr(E0, false),
    {'catch',ln(Anno),E1};
expr({'try',Anno,Es0,CaseCs0,CatchCs0,As0}, Lc) ->
    %% No new variables added.
    Es = expr_list(Es0),
    CaseCs = icr_clauses(CaseCs0, Lc),
    CatchCs = icr_clauses(CatchCs0, Lc),
    As = expr_list(As0),
    {'try',ln(Anno),Es,CaseCs,CatchCs,As};
expr({lc,_,_,_}=Compr, _Lc) ->
    expr_lc_bc(Compr);
expr({bc,_,_,_}=Compr, _Lc) ->
    expr_lc_bc(Compr);
expr({match,Anno,P0,E0}, _Lc) ->
    E1 = expr(E0, false),
    P1 = pattern(P0),
    {match,ln(Anno),P1,E1};
expr({op,Anno,Op,A0}, _Lc) ->
    A1 = expr(A0, false),
    {op,ln(Anno),Op,[A1]};
expr({op,Anno,'++',L0,R0}, _Lc) ->
    L1 = expr(L0, false),
    R1 = expr(R0, false),		  %They see the same variables
    {op,ln(Anno),append,[L1,R1]};
expr({op,Anno,'--',L0,R0}, _Lc) ->
    L1 = expr(L0, false),
    R1 = expr(R0, false),		  %They see the same variables
    {op,ln(Anno),subtract,[L1,R1]};
expr({op,Anno,'!',L0,R0}, _Lc) ->
    L1 = expr(L0, false),
    R1 = expr(R0, false),		  %They see the same variables
    {send,ln(Anno),L1,R1};
expr({op,Anno,Op,L0,R0}, _Lc) when Op =:= 'andalso'; Op =:= 'orelse' ->
    L1 = expr(L0, false),
    R1 = expr(R0, false),		  %They see the same variables
    {Op,ln(Anno),L1,R1};
expr({op,Anno,Op,L0,R0}, _Lc) ->
    L1 = expr(L0, false),
    R1 = expr(R0, false),		  %They see the same variables
    {op,ln(Anno),Op,[L1,R1]};
expr({bin,Anno,Grp}, _Lc) ->
    Grp1 = expr_list(bin_expand_strings(Grp)),
    {bin,ln(Anno),Grp1};
expr({bin_element,Anno,Expr0,Size0,Type0}, _Lc) ->
    {Size1,Type} = make_bit_type(Anno, Size0, Type0),
    Expr = expr(Expr0, false),
    Size = expr(Size1, false),
    {bin_element,ln(Anno),Expr,Size,Type}.

consify([A|As]) -> 
    {cons,0,A,consify(As)};
consify([]) -> {value,0,[]}.

make_bit_type(Line, default, Type0) ->
    case erl_bits:set_bit_type(default, Type0) of
        {ok,all,Bt} -> {{atom,Line,all},erl_bits:as_list(Bt)};
	{ok,undefined,Bt} -> {{atom,Line,undefined},erl_bits:as_list(Bt)};
        {ok,Size,Bt} -> {{integer,Line,Size},erl_bits:as_list(Bt)}
    end;
make_bit_type(_Line, Size, Type0) ->            %Integer or 'all'
    {ok,Size,Bt} = erl_bits:set_bit_type(Size, Type0),
    {Size,erl_bits:as_list(Bt)}.

expr_lc_bc({Tag,Anno,E0,Gs0}) ->
    Gs = lists:map(fun ({generate,L,P0,Qs}) ->
			   {generate,L,pattern(P0),expr(Qs, false)};
		       ({b_generate,L,P0,Qs}) -> %R12.
			   {b_generate,L,pattern(P0),expr(Qs, false)};
		       (Expr) ->
			   case is_guard_test(Expr) of
			       true -> {guard,guard([[Expr]])};
			       false -> expr(Expr, false)
			   end
		   end, Gs0),
    {Tag,ln(Anno),expr(E0, false),Gs}.

is_guard_test(Expr) ->
    IsOverridden = fun({_,_}) -> true end,
    erl_lint:is_guard_test(Expr, [], IsOverridden).

%% The debugger converts both strings "abc" and lists [67, 68, 69]
%% into {value, Line, [67, 68, 69]}, making it impossible to later
%% distingish one or the other inside binaries when evaluating. To
%% avoid <<[67, 68, 69]>> from evaluating, we convert strings into
%% chars to avoid the ambiguity.
bin_expand_strings(Es) ->
    lists:foldr(fun ({bin_element,Line,{string,_,S},Sz,Ts}, Es1) ->
		  lists:foldr(fun (C, Es2) ->
				[{bin_element,Line,{char,Line,C},Sz,Ts}|Es2]
			end, Es1, S);
	      (E, Es1) -> [E|Es1]
	  end, [], Es).

%% -type expr_list([Expression]) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0|Es]) ->
    E1 = expr(E0, false),
    [E1|expr_list(Es)];
expr_list([]) -> [].

icr_clauses([C0|Cs], Lc) ->
    C1 = clause(C0, Lc),
    [C1|icr_clauses(Cs, Lc)];
icr_clauses([], _) -> [].

fun_clauses([{clause,A,H,G,B}|Cs]) ->
    [{clause,ln(A),head(H),guard(G),exprs(B, true)}|fun_clauses(Cs)];
fun_clauses([]) -> [].


new_map(Fs0, Anno, F) ->
    Line = ln(Anno),
    Fs1 = map_fields(Fs0, F),
    Fs2 = [{L,K,V} || {map_field_assoc,L,K,V} <- Fs1],
    try
	{value,Line,map_literal(Fs2, #{})}
    catch
	throw:not_literal ->
	    {map,Line,Fs2}
    end.

map_literal([{_,{value,_,K},{value,_,V}}|T], M) ->
    map_literal(T, maps:put(K, V, M));
map_literal([_|_], _) ->
    throw(not_literal);
map_literal([], M) -> M.

map_fields(Fs) ->
    map_fields(Fs, fun (E) -> expr(E, false) end).

map_fields([{map_field_assoc,A,N,V}|Fs], F) ->
    [{map_field_assoc,ln(A),F(N),F(V)}|map_fields(Fs)];
map_fields([{map_field_exact,A,N,V}|Fs], F) ->
    [{map_field_exact,ln(A),F(N),F(V)}|map_fields(Fs)];
map_fields([], _) -> [].

%% new_var_name() -> VarName.

new_var_name() ->
    C = get(vcount),
    put(vcount, C+1),
    list_to_atom("%" ++ integer_to_list(C)).

%% new_vars(Count, Line) -> [Var].
%%  Make Count new variables.

new_vars(N, L) -> new_vars(N, L, []).

new_vars(N, L, Vs) when N > 0 ->
    V = {var,L,new_var_name()},
    new_vars(N-1, L, [V|Vs]);
new_vars(0, _, Vs) -> Vs.

new_fun_name() ->
    {F,A} = get(current_function),
    I = get(fun_count),
    put(fun_count, I+1),
    Name = "-" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A) ++
        "-fun-" ++ integer_to_list(I) ++ "-",
    list_to_atom(Name).

ln(Anno) ->
    erl_anno:line(Anno).

bif_type(erlang, Name, Arity) ->
    case erl_internal:guard_bif(Name, Arity) of
	true ->
	    %% Guard BIFs are safe (except for self/0, but it is
	    %% handled with a special instruction anyway).
	    safe;
	false ->
	    bif_type(Name)
    end;
bif_type(_, _, _) -> unsafe.

bif_type(register)           -> safe;
bif_type(unregister)         -> safe;
bif_type(whereis)            -> safe;
bif_type(registered)         -> safe;
bif_type(setelement)         -> safe;
bif_type(atom_to_list)       -> safe;
bif_type(list_to_atom)       -> safe;
bif_type(integer_to_list)    -> safe;
bif_type(list_to_integer)    -> safe;
bif_type(float_to_list)      -> safe;
bif_type(list_to_float)      -> safe;
bif_type(tuple_to_list)      -> safe;
bif_type(list_to_tuple)      -> safe;
bif_type(make_ref)           -> safe;
bif_type(time)               -> safe;
bif_type(date)               -> safe;
bif_type(processes)          -> safe;
bif_type(process_info)       -> safe;
bif_type(load_module)        -> safe;
bif_type(delete_module)      -> safe;
bif_type(halt)               -> safe;
bif_type(check_process_code) -> safe;
bif_type(purge_module)       -> safe;
bif_type(pid_to_list)        -> safe;
bif_type(list_to_pid)        -> safe;
bif_type(module_loaded)      -> safe;
bif_type(binary_to_term)     -> safe;
bif_type(term_to_binary)     -> safe;
bif_type(nodes)              -> safe;
bif_type(is_alive)           -> safe;
bif_type(disconnect_node)    -> safe;
bif_type(binary_to_list)     -> safe;
bif_type(list_to_binary)     -> safe;
bif_type(split_binary)       -> safe;
bif_type(hash)               -> safe;
bif_type(pre_loaded)         -> safe;
bif_type(set_cookie)         -> safe;
bif_type(get_cookie)         -> safe;
bif_type(_)                  -> unsafe.

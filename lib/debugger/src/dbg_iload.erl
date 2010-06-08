%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2010. All Rights Reserved.
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
-module(dbg_iload).

-export([load_mod/4]).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% load_mod(Mod, File, Binary, Db) -> {ok, Mod}
%%   Mod = atom()
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
        {'ok', Mod} when is_subtype(Mod, atom()).

load_mod(Mod, File, Binary, Db) ->
    Flag = process_flag(trap_exit, true),
    Pid = spawn_link(fun () -> load_mod1(Mod, File, Binary, Db) end),
    receive
	{'EXIT', Pid, What} ->
	    process_flag(trap_exit, Flag),
	    What
    end.

-spec load_mod1(atom(), file:filename(), binary(), ets:tid()) -> no_return().

load_mod1(Mod, File, Binary, Db) ->
    store_module(Mod, File, Binary, Db),
    exit({ok, Mod}).

%%====================================================================
%% Internal functions
%%====================================================================

store_module(Mod, File, Binary, Db) ->
    {interpreter_module, Exp, Abst, Src, MD5} = binary_to_term(Binary),
    Forms = case abstr(Abst) of
		{abstract_v1,Forms0} -> Forms0;
		{abstract_v2,Forms0} -> Forms0;
		{raw_abstract_v1,Code0} ->
                    Code = interpret_file_attribute(Code0),
		    {_,_,Forms0,_} = sys_pre_expand:module(Code, []),
		    Forms0
	    end,
    dbg_idb:insert(Db, mod_file, File),
    dbg_idb:insert(Db, exports, Exp),
    dbg_idb:insert(Db, defs, []),

    put(vcount, 0),
    put(fun_count, 0),
    put(funs, []),
    put(mod_md5, MD5),
    Attr = store_forms(Forms, Mod, Db, Exp, []),
    erase(mod_md5),
    erase(current_function),
    %% store_funs(Db, Mod),
    erase(vcount),
    erase(funs),
    erase(fun_count),
    
    dbg_idb:insert(Db, attributes, Attr),
    NewBinary = store_mod_line_no(Mod, Db, binary_to_list(Src)),
    dbg_idb:insert(Db, mod_bin, NewBinary),
    dbg_idb:insert(Db, mod_raw, <<Src/binary,0:8>>), %% Add eos
    dbg_idb:insert(Db, module, Mod).
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

store_forms([{function,_,module_info,0,_}|Fs], Mod, Db, Exp, Attr) ->
    Cs = [{clause,0,[],[], [{module_info_0,0,Mod}]}],
    dbg_idb:insert(Db, {Mod,module_info,0,true}, Cs),
    store_forms(Fs, Mod, Db, Exp, Attr);
store_forms([{function,_,module_info,1,_}|Fs], Mod, Db, Exp, Attr) ->
    Cs = [{clause,0,[{var,0,'What'}],[], [{module_info_1,0,Mod,[{var,0,'What'}]}]}],
    dbg_idb:insert(Db, {Mod,module_info,1,true}, Cs),
    store_forms(Fs, Mod, Db, Exp, Attr);
store_forms([{function,_,Name,Arity,Cs0}|Fs], Mod, Db, Exp, Attr) ->
    FA = {Name,Arity},
    put(current_function, FA),
    Cs = clauses(Cs0),
    Exported = lists:member(FA, Exp),
    dbg_idb:insert(Db, {Mod,Name,Arity,Exported}, Cs),
    store_forms(Fs, Mod, Db, Exp, Attr);
store_forms([{attribute,_,Name,Val}|Fs], Mod, Db, Exp, Attr) ->
    store_forms(Fs, Mod, Db, Exp, [{Name,Val}|Attr]);
store_forms([F|_], _Mod, _Db, _Exp, _Attr) ->
    exit({unknown_form,F});
store_forms([], _, _, _, Attr) ->
    lists:reverse(Attr).

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
    C1 = clause(C0),
    [C1|clauses(Cs)];
clauses([]) -> [].

clause({clause,Line,H0,G0,B0}) ->
    H1 = head(H0),
    G1 = guard(G0),
    B1 = exprs(B0),
    {clause,Line,H1,G1,B1}.

head(Ps) -> patterns(Ps).

%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

patterns([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|patterns(Ps)];
patterns([]) -> [].

%%  N.B. Only valid patterns are included here.

pattern({var,Line,V}) -> {var,Line,V};
pattern({char,Line,I}) -> {value,Line,I};
pattern({integer,Line,I}) -> {value,Line,I};
pattern({match,Line,Pat1,Pat2}) ->
    {match,Line,pattern(Pat1),pattern(Pat2)};
pattern({float,Line,F}) -> {value,Line,F};
pattern({atom,Line,A}) -> {value,Line,A};
pattern({string,Line,S}) -> {value,Line,S};
pattern({nil,Line}) -> {value,Line,[]};
pattern({cons,Line,H0,T0}) ->
    H1 = pattern(H0),
    T1 = pattern(T0),
    {cons,Line,H1,T1};
pattern({tuple,Line,Ps0}) ->
    Ps1 = pattern_list(Ps0),
    {tuple,Line,Ps1};
pattern({op,_,'-',{integer,Line,I}}) ->
    {value,Line,-I};
pattern({op,_,'+',{integer,Line,I}}) ->
    {value,Line,I};
pattern({op,_,'-',{char,Line,I}}) ->
    {value,Line,-I};
pattern({op,_,'+',{char,Line,I}}) ->
    {value,Line,I};
pattern({op,_,'-',{float,Line,I}}) ->
    {value,Line,-I};
pattern({op,_,'+',{float,Line,I}}) ->
    {value,Line,I};
pattern({bin,Line,Grp}) ->
    Grp1 = pattern_list(Grp),
    {bin,Line,Grp1};
pattern({bin_element,Line,Expr,Size,Type}) ->
    Expr1 = pattern(Expr),
    Size1 = expr(Size),
    {bin_element,Line,Expr1,Size1,Type}.

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

and_guard([{atom,_,true}|Gs]) ->
    and_guard(Gs);
and_guard([G0|Gs]) ->
    G1 = guard_test(G0),
    [G1|and_guard(Gs)];
and_guard([]) -> [].

guard_test({call,Line,{remote,_,{atom,_,erlang},{atom,_,F}},As0}) ->
    As = gexpr_list(As0),
    case map_guard_bif(F, length(As0)) of
	{ok,Name} ->
	    {safe_bif,Line,erlang,Name,As};
	error ->
	    {safe_bif,Line,erlang,F,As}
    end;
guard_test({op,Line,Op,L0}) ->
    true = erl_internal:arith_op(Op, 1) orelse %Assertion.
	erl_internal:bool_op(Op, 1),
    L1 = gexpr(L0),
    {safe_bif,Line,erlang,Op,[L1]};
guard_test({op,Line,Op,L0,R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    L1 = gexpr(L0),
    R1 = gexpr(R0),				%They see the same variables
    {Op,Line,L1,R1};
guard_test({op,Line,Op,L0,R0}) ->
    true = erl_internal:comp_op(Op, 2) orelse	%Assertion.
	erl_internal:bool_op(Op, 2) orelse
        erl_internal:arith_op(Op, 2),
    L1 = gexpr(L0),
    R1 = gexpr(R0),				%They see the same variables
    {safe_bif,Line,erlang,Op,[L1,R1]};
guard_test({integer,_,_}=I) -> I;
guard_test({char,_,_}=C) -> C;
guard_test({float,_,_}=F) -> F;
guard_test({atom,_,_}=A) -> A;
guard_test({nil,_}=N) -> N;
guard_test({var,_,_}=V) ->V.    % Boolean var

map_guard_bif(integer, 1) -> {ok,is_integer};
map_guard_bif(float, 1) -> {ok,is_float};
map_guard_bif(number, 1) -> {ok,is_number};
map_guard_bif(atom, 1) -> {ok,is_atom};
map_guard_bif(list, 1) -> {ok,is_list};
map_guard_bif(tuple, 1) -> {ok,is_tuple};
map_guard_bif(pid, 1) -> {ok,is_pid};
map_guard_bif(reference, 1) -> {ok,is_reference};
map_guard_bif(port, 1) -> {ok,is_port};
map_guard_bif(binary, 1) -> {ok,is_binary};
map_guard_bif(function, 1) -> {ok,is_function};
map_guard_bif(_, _) -> error.

gexpr({var,Line,V}) -> {var,Line,V};
gexpr({integer,Line,I}) -> {value,Line,I};
gexpr({char,Line,I}) -> {value,Line,I};
gexpr({float,Line,F}) -> {value,Line,F};
gexpr({atom,Line,A}) -> {value,Line,A};
gexpr({string,Line,S}) -> {value,Line,S};
gexpr({nil,Line}) -> {value,Line,[]};
gexpr({cons,Line,H0,T0}) ->
    case {gexpr(H0),gexpr(T0)} of
	{{value,Line,H1},{value,Line,T1}} -> {value,Line,[H1|T1]};
	{H1,T1} -> {cons,Line,H1,T1}
    end;
gexpr({tuple,Line,Es0}) ->
    Es1 = gexpr_list(Es0),
    {tuple,Line,Es1};
gexpr({bin,Line,Flds0}) ->
    Flds = gexpr_list(Flds0),
    {bin,Line,Flds};
gexpr({bin_element,Line,Expr0,Size0,Type}) ->
    Expr = gexpr(Expr0),
    Size = gexpr(Size0),
    {bin_element,Line,Expr,Size,Type};
%%% The previous passes have added the module name 'erlang' to
%%% all BIF calls, even in guards.
gexpr({call,Line,{remote,_,{atom,_,erlang},{atom,_,self}},[]}) ->
    {dbg, Line, self, []};
gexpr({call,Line,{remote,_,{atom,_,erlang},{atom,_,F}},As0}) ->
    As = gexpr_list(As0),
    {safe_bif,Line,erlang,F,As};
gexpr({op,Line,Op,A0}) ->
    erl_internal:arith_op(Op, 1),
    A1 = gexpr(A0),
    {safe_bif,Line,erlang,Op,[A1]};
gexpr({op,Line,Op,L0,R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    L1 = gexpr(L0),
    R1 = gexpr(R0),			%They see the same variables
    {Op,Line,L1,R1};
gexpr({op,Line,Op,L0,R0}) ->
    true = erl_internal:arith_op(Op, 2) orelse erl_internal:comp_op(Op, 2)
	orelse erl_internal:bool_op(Op, 2),
    L1 = gexpr(L0),
    R1 = gexpr(R0),			%They see the same variables
    {safe_bif,Line,erlang,Op,[L1,R1]}.

%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list([E0|Es]) ->
    E1 = gexpr(E0),
    [E1|gexpr_list(Es)];
gexpr_list([]) -> [].

%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0|Es]) ->
    E1 = expr(E0),
    [E1|exprs(Es)];
exprs([]) -> [].

expr({var,Line,V}) -> {var,Line,V};
expr({integer,Line,I}) -> {value,Line,I};
expr({char,Line,I}) -> {value,Line,I};
expr({float,Line,F}) -> {value,Line,F};
expr({atom,Line,A}) -> {value,Line,A};
expr({string,Line,S}) -> {value,Line,S};
expr({nil,Line}) -> {value,Line,[]};
expr({cons,Line,H0,T0}) ->
    case {expr(H0),expr(T0)} of
	{{value,Line,H1},{value,Line,T1}} -> {value,Line,[H1|T1]};
	{H1,T1} -> {cons,Line,H1,T1}
    end;
expr({tuple,Line,Es0}) ->
    Es1 = expr_list(Es0),
    {tuple,Line,Es1};
expr({block,Line,Es0}) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0),
    {block,Line,Es1};
expr({'if',Line,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'if',Line,Cs1};
expr({'case',Line,E0,Cs0}) ->
    E1 = expr(E0),
    Cs1 = icr_clauses(Cs0),
    {'case',Line,E1,Cs1};
expr({'receive',Line,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'receive',Line,Cs1};
expr({'receive',Line,Cs0,To0,ToEs0}) ->
    To1 = expr(To0),
    ToEs1 = exprs(ToEs0),
    Cs1 = icr_clauses(Cs0),
    {'receive',Line,Cs1,To1,ToEs1};
expr({'fun',Line,{clauses,Cs0},{_,_,Name}}) when is_atom(Name) ->
    %% New R10B-2 format (abstract_v2).
    Cs = fun_clauses(Cs0),
    {make_fun,Line,Name,Cs};
expr({'fun',Line,{clauses,Cs0},{_,_,_,_,Name}}) when is_atom(Name) ->
    %% New R8 format (abstract_v2).
    Cs = fun_clauses(Cs0),
    {make_fun,Line,Name,Cs};
expr({'fun',Line,{function,F,A},{_Index,_OldUniq,Name}}) ->
    %% New R8 format (abstract_v2).
    As = new_vars(A, Line),
    Cs = [{clause,Line,As,[],[{local_call,Line,F,As}]}],
    {make_fun,Line,Name,Cs};
expr({'fun',_,{clauses,_},{_OldUniq,_Hvss,_Free}}) ->
    %% Old format (abstract_v1).
    exit({?MODULE,old_funs});
expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,self}},[]}) ->
    {dbg,Line,self,[]};
expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,get_stacktrace}},[]}) ->
    {dbg,Line,get_stacktrace,[]};
expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,throw}},[_]=As}) ->
    {dbg,Line,throw,expr_list(As)};
expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,error}},[_]=As}) ->
    {dbg,Line,error,expr_list(As)};
expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,fault}},[_]=As}) ->
    {dbg,Line,fault,expr_list(As)};
expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,exit}},[_]=As}) ->
    {dbg,Line,exit,expr_list(As)};
expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,apply}},As0}) 
  when length(As0) == 3 ->
    As = expr_list(As0),
    {apply,Line,As};
expr({call,Line,{remote,_,{atom,_,Mod},{atom,_,Func}},As0}) ->
    As = expr_list(As0),
    case erlang:is_builtin(Mod, Func, length(As)) of
	false ->
	    {call_remote,Line,Mod,Func,As};
	true ->
	    case bif_type(Mod, Func) of
		safe -> {safe_bif,Line,Mod,Func,As};
		spawn -> {spawn_bif,Line,Mod,Func,As};
		unsafe ->{bif,Line,Mod,Func,As}
	    end
    end;
expr({call,Line,{remote,_,Mod0,Func0},As0}) ->
    %% New R8 format (abstract_v2).
    Mod = expr(Mod0),
    Func = expr(Func0),
    As = consify(expr_list(As0)),
    {apply,Line,[Mod,Func,As]};
expr({call,Line,{atom,_,Func},As0}) ->
    As = expr_list(As0),
    {local_call,Line,Func,As};
expr({call,Line,Fun0,As0}) ->
    Fun = expr(Fun0),
    As = expr_list(As0),
    {apply_fun,Line,Fun,As};
expr({'catch',Line,E0}) ->
    %% No new variables added.
    E1 = expr(E0),
    {'catch',Line,E1};
expr({'try',Line,Es0,CaseCs0,CatchCs0,As0}) ->
    %% No new variables added.
    Es = expr_list(Es0),
    CaseCs = icr_clauses(CaseCs0),
    CatchCs = icr_clauses(CatchCs0),
    As = expr_list(As0),
    {'try',Line,Es,CaseCs,CatchCs,As};
expr({'query', Line, E0}) ->
    E = expr(E0),
    {'query', Line, E};
expr({lc,Line,E0,Gs0}) ->			%R8.
    Gs = lists:map(fun ({generate,L,P0,Qs}) ->
			   {generate,L,expr(P0),expr(Qs)};
		       ({b_generate,L,P0,Qs}) -> %R12.
			   {b_generate,L,expr(P0),expr(Qs)};
		       (Expr) ->
			   case is_guard_test(Expr) of
			       true -> {guard,[[guard_test(Expr)]]};
			       false -> expr(Expr)
			   end
		   end, Gs0),
    {lc,Line,expr(E0),Gs};
expr({bc,Line,E0,Gs0}) ->			%R12.
    Gs = lists:map(fun ({generate,L,P0,Qs}) ->
			   {generate,L,expr(P0),expr(Qs)};
		       ({b_generate,L,P0,Qs}) -> %R12.
			   {b_generate,L,expr(P0),expr(Qs)};
		       (Expr) ->
			   case is_guard_test(Expr) of
			       true -> {guard,[[guard_test(Expr)]]};
			       false -> expr(Expr)
			   end
		   end, Gs0),
    {bc,Line,expr(E0),Gs};
expr({match,Line,P0,E0}) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    {match,Line,P1,E1};
expr({op,Line,Op,A0}) ->
    A1 = expr(A0),
    {op,Line,Op,[A1]};
expr({op,Line,'++',L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {op,Line,append,[L1,R1]};
expr({op,Line,'--',L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {op,Line,subtract,[L1,R1]};
expr({op,Line,'!',L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {send,Line,L1,R1};
expr({op,Line,Op,L0,R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {Op,Line,L1,R1};
expr({op,Line,Op,L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {op,Line,Op,[L1,R1]};
expr({bin,Line,Grp}) ->
    Grp1 = expr_list(Grp),
    {bin,Line,Grp1};
expr({bin_element,Line,Expr,Size,Type}) ->
    Expr1 = expr(Expr),
    Size1 = expr(Size),
    {bin_element,Line,Expr1,Size1,Type};
expr(Other) ->
    exit({?MODULE,{unknown_expr,Other}}).

%% is_guard_test(Expression) -> true | false.
%%  Test if a general expression is a guard test.  Cannot use erl_lint
%%  here as sys_pre_expand has transformed source.

is_guard_test({op,_,Op,L,R}) ->
    case erl_internal:comp_op(Op, 2) of
	true -> is_gexpr_list([L,R]);
	false -> false
    end;
is_guard_test({call,_,{remote,_,{atom,_,erlang},{atom,_,Test}},As}) ->
    case erl_internal:type_test(Test, length(As)) of
	true -> is_gexpr_list(As);
	false -> false
    end;
is_guard_test({atom,_,true}) -> true;
is_guard_test(_) -> false.

is_gexpr({var,_,_}) -> true;
is_gexpr({atom,_,_}) -> true;
is_gexpr({integer,_,_}) -> true;
is_gexpr({char,_,_}) -> true;
is_gexpr({float,_,_}) -> true;
is_gexpr({string,_,_}) -> true;
is_gexpr({nil,_}) -> true;
is_gexpr({cons,_,H,T}) -> is_gexpr_list([H,T]);
is_gexpr({tuple,_,Es}) -> is_gexpr_list(Es);
is_gexpr({call,_,{remote,_,{atom,_,erlang},{atom,_,F}},As}) ->
    Ar = length(As),
    case erl_internal:guard_bif(F, Ar) of
	true -> is_gexpr_list(As);
	false ->
	    case erl_internal:arith_op(F, Ar) of
		true -> is_gexpr_list(As);
		false -> false
	    end
    end;
is_gexpr({op,_,Op,A}) ->
    case erl_internal:arith_op(Op, 1) of
	true -> is_gexpr(A);
	false -> false
    end;
is_gexpr({op,_,Op,A1,A2}) ->
    case erl_internal:arith_op(Op, 2) of
	true -> is_gexpr_list([A1,A2]);
	false -> false
    end;
is_gexpr(_) -> false.

is_gexpr_list(Es) -> lists:all(fun (E) -> is_gexpr(E) end, Es).

consify([A|As]) -> 
    {cons,0,A,consify(As)};
consify([]) -> {value,0,[]}.


%% -type expr_list([Expression]) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0|Es]) ->
    E1 = expr(E0),
    [E1|expr_list(Es)];
expr_list([]) -> [].

icr_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|icr_clauses(Cs)];
icr_clauses([]) -> [].

fun_clauses([{clause,L,H,G,B}|Cs]) ->
    [{clause,L,head(H),guard(G),exprs(B)}|fun_clauses(Cs)];
fun_clauses([]) -> [].

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

bif_type(erlang, Name) -> bif_type(Name);
bif_type(_, _) -> unsafe.

bif_type(register)           -> safe;
bif_type(unregister)         -> safe;
bif_type(whereis)            -> safe;
bif_type(registered)         -> safe;
bif_type(abs)                -> safe;
bif_type(float)              -> safe;
bif_type(trunc)              -> safe;
bif_type(round)              -> safe;
bif_type(math)               -> safe;
bif_type(node)               -> safe;
bif_type(length)             -> safe;
bif_type(hd)                 -> safe;
bif_type(tl)                 -> safe;
bif_type(size)               -> safe;
bif_type(element)            -> safe;
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
bif_type(alive)              -> safe;
bif_type(notalive)           -> safe;
bif_type(nodes)              -> safe;
bif_type(is_alive)           -> safe;
bif_type(disconnect_node)    -> safe;
bif_type(binary_to_list)     -> safe;
bif_type(list_to_binary)     -> safe;
bif_type(split_binary)       -> safe;
bif_type(concat_binary)      -> safe;
bif_type(term_to_atom)       -> safe;
bif_type(hash)               -> safe;
bif_type(pre_loaded)         -> safe;
bif_type(info)               -> safe;
bif_type(set_cookie)         -> safe;
bif_type(get_cookie)         -> safe;
bif_type(spawn)              -> spawn;
bif_type(spawn_link)         -> spawn;
bif_type(spawn_opt)          -> spawn;
bif_type(_)                  -> unsafe.

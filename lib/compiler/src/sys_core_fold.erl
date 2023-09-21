%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2023. All Rights Reserved.
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
%% Purpose : Constant folding optimisation for Core

%% Propagate atomic values and fold in values of safe calls to
%% constant arguments.  Also detect and remove literals which are
%% ignored in a 'seq'.  Could handle lets better by chasing down
%% complex 'arg' expressions and finding values.
%%
%% Try to optimise case expressions by removing unmatchable or
%% unreachable clauses.  Also change explicit tuple arg into multiple
%% values and extend clause patterns.  We must be careful here not to
%% generate cases which we know to be safe but later stages will not
%% recognise as such, e.g. the following is NOT acceptable:
%%
%%    case 'b' of
%%        <'b'> -> ...
%%    end
%%
%% Variable folding is complicated by variable shadowing, for example
%% in:
%%    'foo'/1 =
%%        fun (X) ->
%%            let <A> = X
%%            in  let <X> = Y
%%                in ... <use A>
%% If we were to simply substitute X for A then we would be using the
%% wrong X.  Our solution is to rename variables that are the values
%% of substitutions.  We could rename all shadowing variables but do
%% the minimum.  We would then get:
%%    'foo'/1 =
%%        fun (X) ->
%%            let <A> = X
%%            in  let <X1> = Y
%%                in ... <use A>
%% which is optimised to:
%%    'foo'/1 =
%%        fun (X) ->
%%            let <X1> = Y
%%            in ... <use X>
%%
%% This is done by carefully shadowing variables and substituting
%% values.  See details when defining functions.
%%
%% It would be possible to extend to replace repeated evaluation of
%% "simple" expressions by the value (variable) of the first call.
%% For example, after a "let Z = X+1" then X+1 would be replaced by Z
%% where X is valid.  The Sub uses the full Core expression as key.
%% It would complicate handling of patterns as we would have to remove
%% all values where the key contains pattern variables.

-module(sys_core_fold).

-export([module/2,format_error/1]).

-import(lists, [map/2,foldl/3,foldr/3,mapfoldl/3,all/2,any/2,
		reverse/1,reverse/2,member/2,flatten/1,
		unzip/1,keyfind/3]).

-import(cerl, [ann_c_cons/3,ann_c_map/3,ann_c_tuple/2]).

-include("core_parse.hrl").

%%-define(DEBUG, 1).

-ifdef(DEBUG).
-define(ASSERT(E),
	case E of
	    true ->
		ok;
	    false ->
		io:format("~p, line ~p: assertion failed\n", [?MODULE,?LINE]),
		error(assertion_failed)
	end).
-else.
-define(ASSERT(E), ignore).
-endif.

-define(MAX_FUNC_ARGS, 255).
-define(IS_FUNC_ARITY(A), is_integer(A) andalso 0 =< A andalso A =< ?MAX_FUNC_ARGS).

%% Variable value info.
-record(sub, {v=[],                                 %Variable substitutions
              s=sets:new([{version, 2}]) :: sets:set(), %Variables in scope
              t=#{} :: map(),                       %Types
              in_guard=false,                       %In guard or not.
              top=true}).                           %Not inside a term.

-spec module(cerl:c_module(), [compile:option()]) ->
	{'ok', cerl:c_module(), [_]}.

module(#c_module{defs=Ds0}=Mod, Opts) ->
    put(no_inline_list_funcs, not member(inline_list_funcs, Opts)),
    init_warnings(),
    Ds1 = [function_1(D) || D <- Ds0],
    erase(new_var_num),
    erase(no_inline_list_funcs),
    {ok,Mod#c_module{defs=Ds1},get_warnings()}.

function_1({#c_var{name={F,Arity}}=Name,B0}) ->
    try
        %% Find a suitable starting value for the variable
        %% counter. Note that this pass assumes that new_var_name/1
        %% returns a variable name distinct from any variable used in
        %% the entire body of the function. We use integers as
        %% variable names to avoid filling up the atom table when
        %% compiling huge functions.
        Count = cerl_trees:next_free_variable_name(B0),
        put(new_var_num, Count),
	B = find_fixpoint(fun(Core) ->
				  %% This must be a fun!
				  expr(Core, value, sub_new())
			  end, B0, 20),
	{Name,B}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [F,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

find_fixpoint(_OptFun, Core, 0) ->
    Core;
find_fixpoint(OptFun, Core0, Max) ->
    case OptFun(Core0) of
	Core0 -> Core0;
	Core -> find_fixpoint(OptFun, Core, Max-1)
    end.

%% body(Expr, Sub) -> Expr.
%% body(Expr, Context, Sub) -> Expr.
%%  No special handling of anything except values.

body(Body, Sub) ->
    body(Body, value, Sub).

body(#c_values{anno=A,es=Es0}, value, Sub) ->
    Es1 = expr_list(Es0, value, Sub),
    #c_values{anno=A,es=Es1};
body(E, Ctxt, Sub) ->
    ?ASSERT(verify_scope(E, Sub)),
    expr(E, Ctxt, Sub).

%% guard(Expr, Sub) -> Expr.
%%  Do guard expression.  We optimize it in the same way as
%%  expressions in function bodies.

guard(Expr, Sub) ->
    ?ASSERT(verify_scope(Expr, Sub)),
    expr(Expr, value, Sub#sub{in_guard=true}).

%% expr(Expr, Sub) -> Expr.
%% expr(Expr, Context, Sub) -> Expr.

expr(Expr, Sub) ->
    expr(Expr, value, Sub).

expr(#c_var{}=V, Ctxt, Sub) ->
    %% Return void() in effect context to potentially shorten the life time
    %% of the variable and potentially generate better code
    %% (for instance, if the variable no longer needs to survive a function
    %% call, there will be no need to save it in the stack frame).
    case Ctxt of
	effect -> void();
	value -> sub_get_var(V, Sub)
    end;
expr(#c_literal{val=Val}=L, Ctxt, Sub) ->
    case Ctxt of
	effect ->
	    case Val of
		[] ->
		    %% Keep as [] - might give slightly better code.
		    L;
		_ when is_atom(Val) ->
		    %% For cleanliness replace with void().
		    void();
		_ ->
		    %% Warn and replace with void().
                    warn_useless_building(L, Sub),
		    void()
	    end;
	value -> L
    end;
expr(#c_cons{anno=Anno,hd=H0,tl=T0}=Cons, Ctxt, Sub) ->
    DeeperSub = descend(Cons, Sub),
    H1 = expr(H0, Ctxt, DeeperSub),
    T1 = expr(T0, Ctxt, DeeperSub),
    case Ctxt of
	effect ->
            warn_useless_building(Cons, Sub),
	    make_effect_seq([H1,T1], Sub);
	value ->
	    ann_c_cons(Anno, H1, T1)
    end;
expr(#c_tuple{anno=Anno,es=Es0}=Tuple, Ctxt, Sub) ->
    Es = expr_list(Es0, Ctxt, descend(Tuple, Sub)),
    case Ctxt of
	effect ->
            warn_useless_building(Tuple, Sub),
	    make_effect_seq(Es, Sub);
	value ->
	    ann_c_tuple(Anno, Es)
    end;
expr(#c_map{anno=Anno,arg=V0,es=Es0}=Map, Ctxt, Sub) ->
    %% Warn for useless building, but always build the map
    %% anyway to preserve a possible exception.
    case Ctxt of
        effect -> warn_useless_building(Map, Sub);
        value -> ok
    end,
    Es = pair_list(Es0, descend(Map, Sub)),
    V = expr(V0, value, Sub),
    ann_c_map(Anno, V, Es);
expr(#c_binary{segments=Ss}=Bin0, Ctxt, Sub) ->
    %% Warn for useless building, but always build the binary
    %% anyway to preserve a possible exception.
    case Ctxt of
	effect -> warn_useless_building(Bin0, Sub);
	value -> ok
    end,
    Bin1 = Bin0#c_binary{segments=bitstr_list(Ss, Sub)},
    Bin = bin_un_utf(Bin1),
    eval_binary(Bin);
expr(#c_fun{}=Fun, effect, Sub) ->
    %% A fun is created, but not used. Warn, and replace with the void value.
    warn_useless_building(Fun, Sub),
    void();
expr(#c_fun{vars=Vs0,body=B0}=Fun, Ctxt0, Sub0) ->
    {Vs1,Sub1} = var_list(Vs0, Sub0),
    Ctxt = case Ctxt0 of
	       {letrec,Ctxt1} -> Ctxt1;
	       value -> value
	   end,
    B1 = body(B0, Ctxt, Sub1),
    Fun#c_fun{vars=Vs1,body=B1};
expr(#c_seq{arg=Arg0,body=B0}=Seq0, Ctxt, Sub) ->
    %% Optimise away pure literal arg as its value is ignored.
    B1 = body(B0, Ctxt, Sub),
    Arg = body(Arg0, effect, Sub),
    case will_fail(Arg) of
	true ->
	    Arg;
	false ->
	    %% Arg cannot be "values" here - only a single value
	    %% make sense here.
            case {Ctxt,is_safe_simple(Arg)} of
                {effect,true} -> B1;
                {effect,false} ->
                    case is_safe_simple(B1) of
                        true -> Arg;
                        false -> Seq0#c_seq{arg=Arg,body=B1}
                    end;
                {value,true} -> B1;
                {value,false} -> Seq0#c_seq{arg=Arg,body=B1}
	    end
    end;
expr(#c_let{}=Let0, Ctxt, Sub) ->
    Let = opt_case_in_let(Let0),
    ?ASSERT(verify_scope(Let, Sub)),
    opt_fun_call(opt_let(Let, Ctxt, Sub));
expr(#c_letrec{body=#c_var{}}=Letrec, effect, _Sub) ->
    %% This is named fun in an 'effect' context. Warn and ignore.
    add_warning(Letrec, {ignored,useless_building}),
    void();
expr(#c_letrec{defs=Fs0,body=B0}=Letrec, Ctxt, Sub) ->
    Fs1 = map(fun ({Name,Fb}) ->
                      case Ctxt =:= effect andalso is_fun_effect_safe(Name, B0) of
                          true ->
                              {Name,expr(Fb, {letrec, effect}, Sub)};
                          false ->
                              {Name,expr(Fb, {letrec, value}, Sub)}
                      end
	      end, Fs0),
    B1 = body(B0, Ctxt, Sub),
    Letrec#c_letrec{defs=Fs1,body=B1};
expr(#c_case{}=Case0, Ctxt, Sub) ->
    %% Ideally, the compiler should only emit warnings when there is
    %% a real mistake in the code being compiled. We use the follow
    %% heuristics in an attempt to approach that ideal:
    %%
    %% * If the guard for a clause always fails, we will emit a
    %%   warning.
    %%
    %% * If a case expression is a literal, we will emit no warnings
    %%   for clauses that will not match or for clauses that are
    %%   shadowed after a clause that will always match. That means
    %%   that code such as:
    %%
    %%      case ?DEBUG of
    %%         false -> ok;
    %%         true -> ...
    %%      end
    %%
    %%   (where ?DEBUG expands to either 'true' or 'false') will not
    %%   produce any warnings.
    %%
    %% * If the case expression is not literal, warnings will be
    %%   emitted for every clause that don't match and for all
    %%   clauses following a clause that will always match.
    %%
    %% * If no clause will ever match, there will be a warning
    %%   (in addition to any warnings that may have been emitted
    %%   according to the rules above).
    %%
    Case1 = opt_bool_case(Case0, Sub),
    #c_case{anno=Anno,arg=Arg0,clauses=Cs0} = Case1,
    Arg1 = body(Arg0, value, Sub),
    LitExpr = cerl:is_literal(Arg1),
    {Arg2,Cs1} = case_opt(Arg1, Cs0, Sub),
    Cs2 = clauses(Arg2, Cs1, Ctxt, Sub, LitExpr, Anno),
    Case = Case1#c_case{arg=Arg2,clauses=Cs2},
    warn_no_clause_match(Case1, Case),
    Expr = eval_case(Case, Sub),
    move_case_into_arg(Expr, Sub);
expr(#c_apply{anno=Anno,op=Op0,args=As0}=Apply0, _, Sub) ->
    Op1 = expr(Op0, value, Sub),
    As1 = expr_list(As0, value, Sub),
    case cerl:is_data(Op1) andalso not is_literal_fun(Op1) of
        false ->
            Apply = Apply0#c_apply{op=Op1,args=As1},
            fold_apply(Apply, Op1, As1);
	true ->
	    add_warning(Apply0, {failed,bad_call}),
	    Err = #c_call{anno=Anno,
			  module=#c_literal{val=erlang},
			  name=#c_literal{val=error},
			  args=[#c_tuple{es=[#c_literal{val='badfun'},
					     Op1]}]},
	    make_effect_seq(As1++[Err], Sub)
    end;
expr(#c_call{module=M0,name=N0}=Call0, Ctxt, Sub) ->
    M1 = expr(M0, value, Sub),
    N1 = expr(N0, value, Sub),
    Call = Call0#c_call{module=M1,name=N1},
    case useless_call(Ctxt, Call) of
	no -> call(Call, M1, N1, Sub);
	{yes,Seq} -> expr(Seq, Ctxt, Sub)
    end;
expr(#c_primop{name=#c_literal{val=build_stacktrace}}, effect, _Sub) ->
    void();
expr(#c_primop{args=As0}=Prim, _, Sub) ->
    As1 = expr_list(As0, value, Sub),
    Prim#c_primop{args=As1};
expr(#c_catch{anno=Anno,body=B}, effect, Sub) ->
    %% When the return value of the 'catch' is ignored, we can replace it
    %% with a try/catch to avoid building a stack trace when an exception
    %% occurs.
    Var = #c_var{name='catch_value'},
    Evs = [#c_var{name='Class'},#c_var{name='Reason'},#c_var{name='Stk'}],
    Try = #c_try{anno=Anno,arg=B,vars=[Var],body=Var,
                 evars=Evs,handler=void()},
    expr(Try, effect, Sub);
expr(#c_catch{body=B0}=Catch, _, Sub) ->
    %% We can remove catch if the value is simple
    B1 = body(B0, value, Sub),
    case is_safe_simple(B1) of
	true -> B1;
	false -> Catch#c_catch{body=B1}
    end;
expr(#c_try{arg=E0,vars=[#c_var{name=X}],body=#c_var{name=X},
	    handler=#c_literal{val=false}=False}=Try, _, Sub) ->
    %% Since guard may call expr/2, we must do some optimization of
    %% the kind of try's that occur in guards.
    E1 = body(E0, value, Sub),
    case will_fail(E1) of
	false ->
	    %% We can remove try/catch if the expression is an
	    %% expression that cannot fail.
	    case is_safe_bool_expr(E1) orelse is_safe_simple(E1) of
		true -> E1;
		false -> Try#c_try{arg=E1}
	    end;
	true ->
	    %% Expression will always fail.
	    False
    end;
expr(#c_try{anno=A,arg=E0,vars=Vs0,body=B0,evars=Evs0,handler=H0}=Try, _, Sub0) ->
    %% Here is the general try/catch construct outside of guards.
    %% We can remove try if the value is simple and replace it with a let.
    E1 = body(E0, value, Sub0),
    {Vs1,Sub1} = var_list(Vs0, Sub0),
    B1 = body(B0, value, Sub1),
    case is_safe_simple(E1) of
	true ->
	    expr(#c_let{anno=A,vars=Vs1,arg=E1,body=B1}, value, Sub0);
	false ->
	    {Evs1,Sub2} = var_list(Evs0, Sub0),
	    H1 = body(H0, value, Sub2),
	    Try#c_try{arg=E1,vars=Vs1,body=B1,evars=Evs1,handler=H1}
    end;
expr(#c_opaque{}=O, effect, _Sub) ->
    O.

%% If a fun or its application is used as an argument, then it's unsafe to
%% handle it in effect context as the side-effects may rely on its return
%% value. The following is a minimal example of where it can go wrong:
%%
%% do letrec 'f'/0 = fun () -> ... whatever ...
%%      in call 'side':'effect'(apply 'f'/0())
%%   'ok'
%%
%% This function returns 'true' if Body definitely does not rely on a
%% value produced by FVar, or 'false' if Body depends on or might depend on
%% a value produced by FVar.

is_fun_effect_safe(#c_var{}=FVar, Body) ->
    ifes_1(FVar, Body, true).

ifes_1(FVar, #c_alias{pat=Pat}, _Safe) ->
    ifes_1(FVar, Pat, false);
ifes_1(FVar, #c_apply{op=Op,args=Args}, Safe) ->
    %% FVar(...) is safe as long its return value is ignored, but it's never
    %% okay to pass FVar as an argument.
    ifes_list(FVar, Args, false) andalso ifes_1(FVar, Op, Safe);
ifes_1(FVar, #c_binary{segments=Segments}, _Safe) ->
    ifes_list(FVar, Segments, false);
ifes_1(FVar, #c_bitstr{val=Val,size=Size,unit=Unit}, _Safe) ->
    ifes_list(FVar, [Val, Size, Unit], false);
ifes_1(FVar, #c_call{args=Args}, _Safe) ->
    ifes_list(FVar, Args, false);
ifes_1(FVar, #c_case{arg=Arg,clauses=Clauses}, Safe) ->
    ifes_1(FVar, Arg, false) andalso ifes_list(FVar, Clauses, Safe);
ifes_1(FVar, #c_catch{body=Body}, _Safe) ->
    ifes_1(FVar, Body, false);
ifes_1(FVar, #c_clause{pats=Pats,guard=Guard,body=Body}, Safe) ->
    ifes_list(FVar, Pats, false) andalso
        ifes_1(FVar, Guard, false) andalso
        ifes_1(FVar, Body, Safe);
ifes_1(FVar, #c_cons{hd=Hd,tl=Tl}, _Safe) ->
    ifes_1(FVar, Hd, false) andalso ifes_1(FVar, Tl, false);
ifes_1(FVar, #c_fun{body=Body}, _Safe) ->
    ifes_1(FVar, Body, false);
ifes_1(FVar, #c_let{arg=Arg,body=Body}, Safe) ->
    ifes_1(FVar, Arg, false) andalso ifes_1(FVar, Body, Safe);
ifes_1(FVar, #c_letrec{defs=Defs,body=Body}, Safe) ->
    Funs = [Fun || {_,Fun} <- Defs],
    ifes_list(FVar, Funs, false) andalso ifes_1(FVar, Body, Safe);
ifes_1(_FVar, #c_literal{}, _Safe) ->
    true;
ifes_1(FVar, #c_map{arg=Arg,es=Elements}, _Safe) ->
    ifes_1(FVar, Arg, false) andalso ifes_list(FVar, Elements, false);
ifes_1(FVar, #c_map_pair{key=Key,val=Val}, _Safe) ->
    ifes_1(FVar, Key, false) andalso ifes_1(FVar, Val, false);
ifes_1(FVar, #c_primop{args=Args}, _Safe) ->
    ifes_list(FVar, Args, false);
ifes_1(FVar, #c_seq{arg=Arg,body=Body}, Safe) ->
    %% Arg of a #c_seq{} has no effect so it's okay to use FVar there even if
    %% Safe=false.
    ifes_1(FVar, Arg, true) andalso ifes_1(FVar, Body, Safe);
ifes_1(FVar, #c_try{arg=Arg,handler=Handler,body=Body}, Safe) ->
    ifes_1(FVar, Arg, false) andalso
        ifes_1(FVar, Handler, Safe) andalso
        ifes_1(FVar, Body, Safe);
ifes_1(FVar, #c_tuple{es=Elements}, _Safe) ->
    ifes_list(FVar, Elements, false);
ifes_1(FVar, #c_values{es=Elements}, _Safe) ->
    ifes_list(FVar, Elements, false);
ifes_1(#c_var{name=Name}, #c_var{name=Name}, Safe) ->
    %% It's safe to return FVar if it's unused.
    Safe;
ifes_1(_FVar, #c_var{}, _Safe) ->
    true.

ifes_list(FVar, [E|Es], Safe) ->
    ifes_1(FVar, E, Safe) andalso ifes_list(FVar, Es, Safe);
ifes_list(_FVar, [], _Safe) ->
    true.


expr_list(Es, Ctxt, Sub) ->
    [expr(E, Ctxt, Sub) || E <- Es].

pair_list(Es, Sub) ->
    [pair(E, Sub) || E <- Es].

pair(#c_map_pair{key=K0,val=V0}=Pair, Sub) ->
    K = expr(K0, value, Sub),
    V = expr(V0, value, Sub),
    Pair#c_map_pair{key=K,val=V}.

bitstr_list(Es, Sub) ->
    [bitstr(E, Sub) || E <- Es].

bitstr(#c_bitstr{val=Val,size=Size}=BinSeg, Sub) ->
    BinSeg#c_bitstr{val=expr(Val, Sub),size=expr(Size, value, Sub)}.

is_literal_fun(#c_literal{val=F}) -> is_function(F);
is_literal_fun(_) -> false.

%% is_safe_simple(Expr, Sub) -> true | false.
%%  A safe simple cannot fail with badarg and is safe to use
%%  in a guard.
%%
%%  Currently, we don't attempt to check binaries because they
%%  are difficult to check.

is_safe_simple(#c_var{}=Var) ->
    not cerl:is_c_fname(Var);
is_safe_simple(#c_cons{hd=H,tl=T}) ->
    is_safe_simple(H) andalso is_safe_simple(T);
is_safe_simple(#c_tuple{es=Es}) -> is_safe_simple_list(Es);
is_safe_simple(#c_literal{}) -> true;
is_safe_simple(#c_call{module=#c_literal{val=erlang},
		       name=#c_literal{val=Name},
		       args=Args}) when is_atom(Name) ->
    NumArgs = length(Args),
    case erl_internal:bool_op(Name, NumArgs) of
	true ->
	    %% Boolean operators are safe if the arguments are boolean.
	    all(fun is_bool_expr/1, Args);
	false ->
	    %% We need a rather complicated test to ensure that
	    %% we only allow safe calls that are allowed in a guard.
	    %% (Note that is_function/2 is a type test, but is not safe.)
	    erl_bifs:is_safe(erlang, Name, NumArgs) andalso
		      (erl_internal:comp_op(Name, NumArgs) orelse
		       erl_internal:new_type_test(Name, NumArgs))
    end;
is_safe_simple(_) -> false.

is_safe_simple_list(Es) -> all(fun(E) -> is_safe_simple(E) end, Es).

%% will_fail(Expr) -> true|false.
%%  Determine whether the expression will fail with an exception.
%%  Return true if the expression always will fail with an exception,
%%  i.e. never return normally.

will_fail(#c_let{arg=A,body=B}) ->
    will_fail(A) orelse will_fail(B);
will_fail(#c_call{module=#c_literal{val=Mod},name=#c_literal{val=Name},args=Args}) ->
    erl_bifs:is_exit_bif(Mod, Name, length(Args));
will_fail(#c_primop{name=#c_literal{val=match_fail},args=[_]}) -> true;
will_fail(_) -> false.

%% bin_un_utf(#c_binary{}) -> #c_binary{}
%%  Convert any literal UTF-8/16/32 literals to byte-sized
%%  integer fields.

bin_un_utf(#c_binary{anno=Anno,segments=Ss}=Bin) ->
    Bin#c_binary{segments=bin_un_utf_1(Ss, Anno)}.

bin_un_utf_1([#c_bitstr{val=#c_literal{},type=#c_literal{val=utf8}}=H|T],
	     Anno) ->
    bin_un_utf_eval(H, Anno) ++ bin_un_utf_1(T, Anno);
bin_un_utf_1([#c_bitstr{val=#c_literal{},type=#c_literal{val=utf16}}=H|T],
	     Anno) ->
    bin_un_utf_eval(H, Anno) ++ bin_un_utf_1(T, Anno);
bin_un_utf_1([#c_bitstr{val=#c_literal{},type=#c_literal{val=utf32}}=H|T],
	     Anno) ->
    bin_un_utf_eval(H, Anno) ++ bin_un_utf_1(T, Anno);
bin_un_utf_1([H|T], Anno) ->
    [H|bin_un_utf_1(T, Anno)];
bin_un_utf_1([], _) -> [].

bin_un_utf_eval(Bitstr, Anno) ->
    Segments = [Bitstr],
    case eval_binary(#c_binary{anno=Anno,segments=Segments}) of
	#c_literal{anno=Anno,val=Bytes} when is_binary(Bytes) ->
	    [#c_bitstr{anno=Anno,
		       val=#c_literal{anno=Anno,val=B},
		       size=#c_literal{anno=Anno,val=8},
		       unit=#c_literal{anno=Anno,val=1},
		       type=#c_literal{anno=Anno,val=integer},
		       flags=#c_literal{anno=Anno,val=[unsigned,big]}} ||
		B <- binary_to_list(Bytes)];
	_ ->
	    Segments
    end.

%% eval_binary(#c_binary{}) -> #c_binary{} | #c_literal{}
%%  Evaluate a binary at compile time if possible to create
%%  a binary literal.

eval_binary(#c_binary{anno=Anno,segments=Ss}=Bin) ->
    try
	#c_literal{anno=Anno,val=eval_binary_1(Ss, <<>>)}
    catch
	throw:impossible ->
	    Bin;
	  throw:{badarg,Warning} ->
	    add_warning(Bin, {failed,Warning}),
            Bin
    end.

eval_binary_1([#c_bitstr{val=#c_literal{val=Val},size=#c_literal{val=Sz},
			 unit=#c_literal{val=Unit},type=#c_literal{val=Type},
			 flags=#c_literal{val=Flags}}|Ss], Acc0) ->
    Endian = bs_endian(Flags),

    %% Make sure that the size is reasonable.
    case Type of
	binary when is_bitstring(Val) ->
	    if
		Sz =:= all ->
		    ok;
		Sz*Unit =< bit_size(Val) ->
		    ok;
		true ->
		    %% Field size is greater than the actual binary - will fail.
		    throw({badarg,embedded_binary_size})
	    end;
	integer when is_integer(Val) ->
	    %% Estimate the number of bits needed to to hold the integer
	    %% literal. Check whether the field size is reasonable in
	    %% proportion to the number of bits needed.
	    if
		Sz*Unit =< 256 ->
		    %% Don't be cheap - always accept fields up to this size.
		    ok;
		true ->
		    case count_bits(Val) of
			BitsNeeded when 2*BitsNeeded >= Sz*Unit ->
			    ok;
			_ ->
			    %% More than about half of the field size will be
			    %% filled out with zeroes - not acceptable.
			    throw(impossible)
		    end
	    end;
	float when is_float(Val) ->
	    %% Bad float size.
	    try Sz*Unit of
		16 -> ok;
		32 -> ok;
		64 -> ok;
		_ ->
                    throw({badarg,bad_float_size})
            catch
                error:_ ->
                    throw({badarg,bad_float_size})
	    end;
	utf8 -> ok;
	utf16 -> ok;
	utf32 -> ok;
	_ ->
	    throw(impossible)
    end,

    case Endian =:= native andalso Type =/= binary of
        true -> throw(impossible);
        false -> ok
    end,

    %% Evaluate the field.
    try eval_binary_2(Acc0, Val, Sz, Unit, Type, Endian) of
	Acc -> eval_binary_1(Ss, Acc)
    catch
	error:_ ->
	    throw(impossible)
    end;
eval_binary_1([], Acc) -> Acc;
eval_binary_1(_, _) -> throw(impossible).

eval_binary_2(Acc, Val, Size, Unit, integer, little) ->
    <<Acc/bitstring,Val:(Size*Unit)/little>>;
eval_binary_2(Acc, Val, Size, Unit, integer, big) ->
    <<Acc/bitstring,Val:(Size*Unit)/big>>;
eval_binary_2(Acc, Val, _Size, _Unit, utf8, _) ->
    try
	<<Acc/bitstring,Val/utf8>>
    catch
	error:_ ->
	    throw({badarg,bad_unicode})
    end;
eval_binary_2(Acc, Val, _Size, _Unit, utf16, big) ->
    try
	<<Acc/bitstring,Val/big-utf16>>
    catch
	error:_ ->
	    throw({badarg,bad_unicode})
    end;
eval_binary_2(Acc, Val, _Size, _Unit, utf16, little) ->
    try
	<<Acc/bitstring,Val/little-utf16>>
    catch
	error:_ ->
	    throw({badarg,bad_unicode})
    end;
eval_binary_2(Acc, Val, _Size, _Unit, utf32, big) ->
    try
	<<Acc/bitstring,Val/big-utf32>>
    catch
	error:_ ->
	    throw({badarg,bad_unicode})
    end;
eval_binary_2(Acc, Val, _Size, _Unit, utf32, little) ->
    try
	<<Acc/bitstring,Val/little-utf32>>
    catch
	error:_ ->
	    throw({badarg,bad_unicode})
    end;
eval_binary_2(Acc, Val, Size, Unit, float, little) ->
    <<Acc/bitstring,Val:(Size*Unit)/little-float>>;
eval_binary_2(Acc, Val, Size, Unit, float, big) ->
    <<Acc/bitstring,Val:(Size*Unit)/big-float>>;
eval_binary_2(Acc, Val, all, Unit, binary, _) ->
    case bit_size(Val) of
	Size when Size rem Unit =:= 0 ->
	    <<Acc/bitstring,Val:Size/bitstring>>;
	Size ->
	    throw({badarg,{embedded_unit,Unit,Size}})
    end;
eval_binary_2(Acc, Val, Size, Unit, binary, _) ->
    <<Acc/bitstring,Val:(Size*Unit)/bitstring>>.

bs_endian([big=E|_]) -> E;
bs_endian([little=E|_]) -> E;
bs_endian([native=E|_]) -> E;
bs_endian([_|Fs]) -> bs_endian(Fs).

%% Count the number of bits approximately needed to store Int.
%% (We don't need an exact result for this purpose.)

count_bits(Int) ->
    count_bits_1(abs(Int), 64).

count_bits_1(0, Bits) -> Bits;
count_bits_1(Int, Bits) -> count_bits_1(Int bsr 64, Bits+64).

%% useless_call(Context, #c_call{}) -> no | {yes,Expr}
%%  Check whether the function is called only for effect,
%%  and if the function either has no effect whatsoever or
%%  the only effect is an exception. Generate appropriate
%%  warnings. If the call is "useless" (has no effect),
%%  a rewritten expression consisting of a sequence of
%%  the arguments only is returned.

useless_call(effect, #c_call{module=#c_literal{val=Mod},
			     name=#c_literal{val=Name},
			     args=Args}=Call) ->
    A = length(Args),
    case erl_bifs:is_safe(Mod, Name, A) of
	false ->
	    case erl_bifs:is_pure(Mod, Name, A) of
		true ->
                    Classified = classify_call(Call),
                    add_warning(Call, {ignored,{result,Classified}});
		false ->
                    ok
	    end,
	    no;
	true ->
	    add_warning(Call, {ignored,{no_effect,{Mod,Name,A}}}),
	    {yes,make_effect_seq(Args, sub_new())}
    end;
useless_call(_, _) -> no.

%% make_effect_seq([Expr], Sub) -> #c_seq{}|void()
%%  Convert a list of expressions evaluated in effect context to a chain of
%%  #c_seq{}. The body in the innermost #c_seq{} will be void().
%%  Anything that will not have any effect will be thrown away.

make_effect_seq([H|T], Sub) ->
    case is_safe_simple(H) of
	true -> make_effect_seq(T, Sub);
	false -> #c_seq{arg=H,body=make_effect_seq(T, Sub)}
    end;
make_effect_seq([], _) -> void().

%% fold_apply(Apply, LiteraFun, Args) -> Apply.
%%  Replace an apply of a literal external fun with a call.

fold_apply(Apply, #c_literal{val=Fun}, Args) when is_function(Fun) ->
    {module,Mod} = erlang:fun_info(Fun, module),
    {name,Name} = erlang:fun_info(Fun, name),
    {arity,Arity} = erlang:fun_info(Fun, arity),
    if
        Arity =:= length(Args) ->
            #c_call{anno=Apply#c_apply.anno,
                    module=#c_literal{val=Mod},
                    name=#c_literal{val=Name},
                    args=Args};
        true ->
            Apply
    end;
fold_apply(Apply, _, _) -> Apply.


%% Handling remote calls. The module/name fields have been processed.

call(#c_call{args=As0}=Call0, #c_literal{val=M}=M0, #c_literal{val=N}=N0, Sub) ->
    As1 = expr_list(As0, value, Sub),
    case simplify_call(Call0, M, N, As1) of
        #c_call{args=As}=Call ->
            case get(no_inline_list_funcs) of
                true ->
                    fold_call(Call, M0, N0, As, Sub);
                false ->
                    case sys_core_fold_lists:call(Call, M, N, As) of
                        none -> fold_call(Call, M0, N0, As, Sub);
                        Core -> expr(Core, Sub)
                    end
            end;
        #c_let{}=Let ->
            Let;
        #c_literal{}=Lit ->
            Lit
    end;
call(#c_call{args=As0}=Call, M, N, Sub) ->
    As = expr_list(As0, value, Sub),
    fold_call(Call#c_call{args=As}, M, N, As, Sub).

%% Rewrite certain known functions to BIFs, improving performance
%% slightly at the cost of making tracing and stack traces incorrect.
simplify_call(Call, maps, get, [Key, Map]) ->
    rewrite_call(Call, erlang, map_get, [Key, Map]);
simplify_call(#c_call{anno=Anno0}, maps, get, [Key0, Map, Default]) ->
    Anno = [compiler_generated | Anno0],

    Key = make_var(Anno),
    Value = make_var(Anno),
    Fail = make_var(Anno),
    Raise = #c_primop{name=#c_literal{val=match_fail},
                      args=[#c_tuple{es=[#c_literal{val=badmap},
                                         Fail]}]},

    Cs = [#c_clause{anno=Anno,
                    pats=[#c_map{es=[#c_map_pair{op=#c_literal{val=exact},
                                                 key=Key,
                                                 val=Value}],
                                 is_pat=true}],
                    guard=#c_literal{val=true},
                    body=Value},
          #c_clause{anno=Anno,
                    pats=[#c_map{es=[],is_pat=true}],
                    guard=#c_literal{val=true},
                    body=Default},
          #c_clause{anno=Anno,
                    pats=[Fail],
                    guard=#c_literal{val=true},
                    body=Raise}],

    cerl:ann_c_let(Anno, [Key], Key0, #c_case{anno=Anno,arg=Map,clauses=Cs});
simplify_call(Call, maps, is_key, [Key, Map]) ->
    rewrite_call(Call, erlang, is_map_key, [Key, Map]);
simplify_call(_Call, maps, new, []) ->
    #c_literal{val=#{}};
simplify_call(Call, maps, size, [Map]) ->
    rewrite_call(Call, erlang, map_size, [Map]);
simplify_call(Call, _, _, Args) ->
    Call#c_call{args=Args}.

%% rewrite_call(Call0, Mod, Func, Args, Sub) -> Call
%%  Rewrites a call to the given MFA.
rewrite_call(Call, Mod, Func, Args) ->
    ModLit = #c_literal{val=Mod},
    FuncLit = #c_literal{val=Func},
    Call#c_call{module=ModLit,name=FuncLit,args=Args}.

%% fold_call(Call, Mod, Name, Args, Sub) -> Expr.
%%  Try to safely evaluate the call.  Just try to evaluate arguments,
%%  do the call and convert return values to literals.  If this
%%  succeeds then use the new value, otherwise just fail and use
%%  original call.  Do this at every level.
%%
%%  We attempt to evaluate calls to certain BIFs even if the
%%  arguments are not literals. For instance, we evaluate length/1
%%  if the shape of the list is known, and element/2 and setelement/3
%%  if the position is constant and the shape of the tuple is known.
%%
fold_call(Call, #c_literal{val=M}, #c_literal{val=F}, Args, Sub) ->
    fold_call_1(Call, M, F, Args, Sub);
fold_call(Call, _M, _N, _Args, _Sub) -> Call.

fold_call_1(Call, erlang, apply, [Fun,Args], _) ->
    simplify_fun_apply(Call, Fun, Args);
fold_call_1(Call, erlang, apply, [Mod,Func,Args], _) ->
    simplify_apply(Call, Mod, Func, Args);
fold_call_1(Call, Mod, Name, Args, Sub) ->
    NumArgs = length(Args),
    case erl_bifs:is_pure(Mod, Name, NumArgs) of
	false -> Call;				%Not pure - keep call.
	true -> fold_call_2(Call, Mod, Name, Args, Sub)
    end.

fold_call_2(Call, Module, Name, Args, Sub) ->
    case all(fun cerl:is_literal/1, Args) of
	true ->
	    %% All arguments are literals.
	    fold_lit_args(Call, Module, Name, Args);
	false ->
	    %% At least one non-literal argument.
	    fold_non_lit_args(Call, Module, Name, Args, Sub)
    end.

fold_lit_args(Call, Module, Name, Args0) ->
    Args = [cerl:concrete(A) || A <- Args0],
    try apply(Module, Name, Args) of
	Val ->
	    case cerl:is_literal_term(Val) of
		true ->
		    cerl:ann_abstract(cerl:get_ann(Call), Val);
		false ->
		    %% Successful evaluation, but it was not possible
		    %% to express the computed value as a literal.
		    Call
	    end
    catch
	error:Reason ->
            %% Evaluation of the function failed. Warn but keep
            %% the call to ensure that extended error information
            %% will be available at runtime.
	    eval_failure(Call, Reason)
    end.

%% fold_non_lit_args(Call, Module, Name, Args, Sub) -> Expr.
%%  Attempt to evaluate some pure BIF calls with one or more
%%  non-literals arguments.
%%
fold_non_lit_args(Call, erlang, length, [Arg], _) ->
    eval_length(Call, Arg);
fold_non_lit_args(Call, erlang, '++', [Arg1,Arg2], _) ->
    eval_append(Call, Arg1, Arg2);
fold_non_lit_args(Call, lists, append, [Arg1,Arg2], _) ->
    eval_append(Call, Arg1, Arg2);
fold_non_lit_args(Call, _, _, _, _) -> Call.

%% eval_length(Call, List) -> Val.
%%  Evaluates the length for the prefix of List which has a known
%%  shape.
%%
eval_length(Call, Core) -> eval_length(Call, Core, 0).

eval_length(Call, #c_literal{val=Val}, Len0) ->
    try
	Len = Len0 + length(Val),
	#c_literal{anno=Call#c_call.anno,val=Len}
    catch
	_:_ ->
	    eval_failure(Call, badarg)
    end;
eval_length(Call, #c_cons{tl=T}, Len) ->
    eval_length(Call, T, Len+1);
eval_length(Call, _List, 0) ->
    Call;		%Could do nothing
eval_length(Call, List, Len) ->
    A = Call#c_call.anno,
    #c_call{anno=A,
	    module=#c_literal{anno=A,val=erlang},
	    name=#c_literal{anno=A,val='+'},
	    args=[#c_literal{anno=A,val=Len},Call#c_call{args=[List]}]}.

%% eval_append(Call, FirstList, SecondList) -> Val.
%%  Evaluates the constant part of '++' expression.
%%
eval_append(Call, #c_literal{val=Cs1}=S1, #c_literal{val=Cs2}) ->
    try
	S1#c_literal{val=Cs1 ++ Cs2}
    catch error:badarg ->
	    eval_failure(Call, badarg)
    end;
eval_append(Call, #c_literal{val=Cs}, List) when length(Cs) =< 4 ->
    Anno = Call#c_call.anno,
    foldr(fun (C, L) ->
		  ann_c_cons(Anno, #c_literal{val=C}, L)
	  end, List, Cs);
eval_append(Call, #c_cons{anno=Anno,hd=H,tl=T}, List) ->
    ann_c_cons(Anno, H, eval_append(Call, T, List));
eval_append(Call, X, Y) ->
    Call#c_call{args=[X,Y]}.			%Rebuild call arguments.

%% eval_failure(Call, Reason) -> Core.
%%  Warn for a call that will fail but keep the call.
%%
eval_failure(Call, Reason) ->
    Classified = classify_call(Call),
    add_warning(Call, {failed,{eval_failure,Classified,Reason}}),
    Call.

%% simplify_apply(Call0, Mod, Func, Args) -> Call
%%  Simplify an apply/3 to a call if the number of arguments
%%  are known at compile time.

simplify_apply(Call, Mod, Func, Args0) ->
    case is_atom_or_var(Mod) andalso is_atom_or_var(Func) of
	true ->
            case get_fixed_args(Args0, []) of
                error ->
                    Call;
                {ok,Args} ->
                    Call#c_call{module=Mod,name=Func,args=Args}
            end;
	false ->
            Call
    end.
is_atom_or_var(#c_literal{val=Atom}) when is_atom(Atom) -> true;
is_atom_or_var(#c_var{}) -> true;
is_atom_or_var(_) -> false.

simplify_fun_apply(#c_call{anno=Anno}=Call, Fun, Args0) ->
    case get_fixed_args(Args0, []) of
        error ->
            Call;
        {ok,Args} ->
            #c_apply{anno=Anno,op=Fun,args=Args}
    end.

get_fixed_args(#c_literal{val=MoreArgs0}, Args)
  when length(MoreArgs0) >= 0 ->
    MoreArgs = [#c_literal{val=Arg} || Arg <- MoreArgs0],
    {ok,reverse(Args, MoreArgs)};
get_fixed_args(#c_cons{hd=Arg,tl=T}, Args) ->
    get_fixed_args(T, [Arg|Args]);
get_fixed_args(_, _) -> error.

%% clause(Clause, Cepxr, Context, Sub) -> Clause.

clause(#c_clause{pats=Ps0}=Cl, Cexpr, Ctxt, Sub0) ->
    try pattern_list(Ps0, Sub0) of
	{Ps1,Sub1} ->
	    clause_1(Cl, Ps1, Cexpr, Ctxt, Sub1)
    catch
	nomatch ->
	    Cl#c_clause{anno=[compiler_generated],
			guard=#c_literal{val=false}}
    end.

clause_1(#c_clause{guard=G0,body=B0}=Cl, Ps1, Cexpr, Ctxt, Sub1) ->
    GSub = case {Cexpr,Ps1,G0} of
	       {_,_,#c_literal{}} ->
		   %% No need for substitution tricks when the guard
		   %% does not contain any variables.
		   Sub1;
	       {#c_var{},[#c_var{}=Var],_} ->
		   %% The idea here is to optimize expressions such as
		   %%
		   %%   case A of A -> ...
		   %%
		   %% to get rid of the extra guard test that the compiler
		   %% added when converting to the Core Erlang representation:
		   %%
		   %%   case A of NewVar when A =:= NewVar -> ...
		   %%
		   %% By replacing NewVar with A everywhere in the guard
		   %% expression, we get
		   %%
		   %%   case A of NewVar when A =:= A -> ...
		   %%
		   %% which by constant-expression evaluation is reduced to
		   %%
		   %%   case A of NewVar when true -> ...
		   %%
		   case cerl:is_c_fname(Cexpr) of
		       false ->
			   sub_set_var(Var, Cexpr, Sub1);
		       true ->
			   %% We must not copy funs, and especially not into guards.
			   Sub1
		   end;
	       _ ->
		   Sub1
	   end,
    G1 = guard(G0, GSub),
    B1 = body(B0, Ctxt, Sub1),
    Cl#c_clause{pats=Ps1,guard=G1,body=B1}.

%% let_substs(LetVars, LetArg, Sub) -> {[Var],[Val],Sub}.
%%  Add suitable substitutions to Sub of variables in LetVars.  First
%%  remove variables in LetVars from Sub, then fix subs.  N.B. must
%%  work out new subs in parallel and then apply them to subs.  Return
%%  the unsubstituted variables and values.

let_substs(Vs0, As0, Sub0) ->
    {Vs1,Sub1} = var_list(Vs0, Sub0),
    {Vs2,As1,Ss} = let_substs_1(Vs1, As0, Sub1),
    Sub2 = sub_add_scope([V || #c_var{name=V} <- Vs2], Sub1),
    {Vs2,As1,
    foldl(fun ({V,S}, Sub) -> sub_set_name(V, S, Sub) end, Sub2, Ss)}.

let_substs_1(Vs, #c_values{es=As}, Sub) ->
    let_subst_list(Vs, As, Sub);
let_substs_1([V], A, Sub) -> let_subst_list([V], [A], Sub);
let_substs_1(Vs, A, _) -> {Vs,A,[]}.

let_subst_list([V|Vs0], [A|As0], Sub) ->
    {Vs1,As1,Ss} = let_subst_list(Vs0, As0, Sub),
    case is_subst(A) of
	true ->
	    {Vs1,As1,sub_subst_var(V, A, Sub) ++ Ss};
	false ->
	    {[V|Vs1],[A|As1],Ss}
    end;
let_subst_list([], [], _) -> {[],[],[]}.

%% pattern(Pattern, InSub) -> {Pattern,OutSub}.
%% pattern(Pattern, InSub, OutSub) -> {Pattern,OutSub}.
%%  Variables occurring in Pattern will shadow so they must be removed
%%  from Sub.  If they occur as a value in Sub then we create a new
%%  variable and then add a substitution for that.
%%
%%  Patterns are complicated by sizes in binaries.  These are pure
%%  input variables which create no bindings.  We, therefore, need to
%%  carry around the original substitutions to get the correct
%%  handling.

%%pattern(Pat, Sub) -> pattern(Pat, Sub, Sub).

pattern(#c_var{}=Pat, Isub, Osub) ->
    case sub_is_in_scope(Pat, Isub) of
	true ->
            %% This variable either has a substitution or is used in
            %% the variable list of an enclosing `let`. In either
            %% case, it must be renamed to an unused name to avoid
            %% name capture problems.
	    V1 = make_var_name(),
	    Pat1 = #c_var{name=V1},
	    {Pat1,sub_set_var(Pat, Pat1, sub_add_scope([V1], Osub))};
	false ->
            %% This variable has never been used. Add it to the scope.
	    {Pat,sub_add_scope([Pat#c_var.name], Osub)}
    end;
pattern(#c_literal{}=Pat, _, Osub) -> {Pat,Osub};
pattern(#c_cons{anno=Anno,hd=H0,tl=T0}, Isub, Osub0) ->
    {H1,Osub1} = pattern(H0, Isub, Osub0),
    {T1,Osub2} = pattern(T0, Isub, Osub1),
    {ann_c_cons(Anno, H1, T1),Osub2};
pattern(#c_tuple{anno=Anno,es=Es0}, Isub, Osub0) ->
    {Es1,Osub1} = pattern_list(Es0, Isub, Osub0),
    {ann_c_tuple(Anno, Es1),Osub1};
pattern(#c_map{anno=Anno,es=Es0}=Map, Isub, Osub0) ->
    {Es1,Osub1} = map_pair_pattern_list(Es0, Isub, Osub0),
    {Map#c_map{anno=Anno,es=Es1},Osub1};
pattern(#c_binary{segments=V0}=Pat, Isub, Osub0) ->
    {V1,Osub1} = bin_pattern_list(V0, Isub, Osub0),
    {Pat#c_binary{segments=V1},Osub1};
pattern(#c_alias{var=V0,pat=P0}=Pat, Isub, Osub0) ->
    {V1,Osub1} = pattern(V0, Isub, Osub0),
    {P1,Osub} = pattern(P0, Isub, Osub1),
    {Pat#c_alias{var=V1,pat=P1},Osub}.

map_pair_pattern_list(Ps0, Isub, Osub0) ->
    {Ps,{_,Osub}} = mapfoldl(fun map_pair_pattern/2, {Isub,Osub0}, Ps0),
    {Ps,Osub}.

map_pair_pattern(#c_map_pair{op=#c_literal{val=exact},key=K0,val=V0}=Pair,{Isub,Osub0}) ->
    K = expr(K0, Isub),
    {V,Osub} = pattern(V0,Isub,Osub0),
    {Pair#c_map_pair{key=K,val=V},{Isub,Osub}}.

bin_pattern_list(Ps, Isub, Osub0) ->
    mapfoldl(fun(P, Osub) ->
                     bin_pattern(P, Isub, Osub)
             end, Osub0, Ps).

bin_pattern(#c_bitstr{val=E0,size=Size0}=Pat0, Isub, Osub0) ->
    Size2 = case {Size0,expr(Size0, Isub)} of
                {#c_var{},#c_literal{val=all}} ->
                    %% The size `all` is used for the size of the final binary
                    %% segment in a pattern. Using `all` explicitly is not allowed,
                    %% so we convert it to an obvious invalid size. We also need
                    %% to add an annotation to get the correct wording of the warning
                    %% that will soon be issued.
                    #c_literal{anno=[size_was_all],val=bad_size};
                {_,Size1} ->
                    Size1
            end, 
    {E1,Osub} = pattern(E0, Isub, Osub0),
    Pat = Pat0#c_bitstr{val=E1,size=Size2},
    bin_pat_warn(Pat),
    {Pat,Osub}.

pattern_list(Ps, Sub) -> pattern_list(Ps, Sub, Sub).

pattern_list(Ps0, Isub, Osub0) ->
    mapfoldl(fun (P, Osub) -> pattern(P, Isub, Osub) end, Osub0, Ps0).

%% var_list([Var], InSub) -> {Pattern,OutSub}.
%%  Works like pattern_list/2 but only accept variables and is
%%  guaranteed not to throw an exception.

var_list(Vs, Sub0) ->
    mapfoldl(fun (#c_var{}=V, Sub) ->
		     pattern(V, Sub, Sub)
	     end, Sub0, Vs).


%%%
%%% Generate warnings for binary patterns that will not match.
%%%

bin_pat_warn(#c_bitstr{type=#c_literal{val=Type},
		       val=Val0,
		       size=#c_literal{anno=SizeAnno,val=Sz},
		       unit=#c_literal{val=Unit},
		       flags=Fl}=Pat) ->
    case {Type,Sz} of
	{_,_} when is_integer(Sz), Sz >= 0 -> ok;
	{binary,all} -> ok;
	{utf8,undefined} -> ok;
	{utf16,undefined} -> ok;
	{utf32,undefined} -> ok;
	{_,_} ->
            case member(size_was_all, SizeAnno) of
                true ->
                    add_warning(Pat, {nomatch,{bit_syntax_size,all}});
                false ->
                    add_warning(Pat, {nomatch,{bit_syntax_size,Sz}})
            end,
	    throw(nomatch)
    end,
    case {Type,Val0} of
	{integer,#c_literal{val=Val}} when is_integer(Val) ->
	    Signedness = signedness(Fl),
	    TotalSz = Sz * Unit,
	    bit_pat_warn_int(Val, TotalSz, Signedness, Pat);
	{float,#c_literal{val=Val}} when is_float(Val) ->
	    ok;
	{utf8,#c_literal{val=Val}} when is_integer(Val) ->
	    bit_pat_warn_unicode(Val, Pat);
	{utf16,#c_literal{val=Val}} when is_integer(Val) ->
	    bit_pat_warn_unicode(Val, Pat);
	{utf32,#c_literal{val=Val}} when is_integer(Val) ->
	    bit_pat_warn_unicode(Val, Pat);
	{_,#c_literal{val=Val}} ->
            add_warning(Pat, {nomatch,{bit_syntax_type,Val,Type}}),
	    throw(nomatch);
	{_,_} ->
	    ok
    end;
bin_pat_warn(#c_bitstr{type=#c_literal{val=Type},val=Val0,flags=Fl}=Pat) ->
    %% Size is variable. Not much that we can check.
    case {Type,Val0} of
	{integer,#c_literal{val=Val}} when is_integer(Val) ->
	    case signedness(Fl) of
		unsigned when Val < 0 ->
                    add_warning(Pat, {nomatch,{bit_syntax_unsigned,Val}}),
		    throw(nomatch);
		_ ->
		    ok
	    end;
	{float,#c_literal{val=Val}} when is_float(Val) ->
	    ok;
	{_,#c_literal{val=Val}} ->
            add_warning(Pat, {nomatch,{bit_syntax_type,Val,Type}}),
	    throw(nomatch);
	{_,_} ->
	    ok
    end.

bit_pat_warn_int(Val, 0, signed, Pat) ->
    if
	Val =:= 0 ->
	    ok;
	true ->
            add_warning(Pat, {nomatch,{bit_syntax_truncated,signed,Val,0}}),
	    throw(nomatch)
    end;
bit_pat_warn_int(Val, Sz, signed, Pat) ->
    if
	Val < 0, Val bsr (Sz - 1) =/= -1 ->
            add_warning(Pat, {nomatch,{bit_syntax_truncated,signed,Val,Sz}}),
	    throw(nomatch);
	Val > 0, Val bsr (Sz - 1) =/= 0 ->
            add_warning(Pat, {nomatch,{bit_syntax_truncated,signed,Val,Sz}}),
	    throw(nomatch);
	true ->
	    ok
    end;
bit_pat_warn_int(Val, _Sz, unsigned, Pat) when Val < 0 ->
    add_warning(Pat, {nomatch,{bit_syntax_unsigned,Val}}),
    throw(nomatch);
bit_pat_warn_int(Val, Sz, unsigned, Pat) ->
    if
	Val bsr Sz =:= 0 ->
	    ok;
	true ->
            add_warning(Pat, {nomatch,{bit_syntax_truncated,unsigned,Val,Sz}}),
	    throw(nomatch)
    end.

bit_pat_warn_unicode(U, _Pat) when 0 =< U, U =< 16#10FFFF ->
    ok;
bit_pat_warn_unicode(U, Pat) ->
    add_warning(Pat, {nomatch,{bit_syntax_unicode,U}}),
    throw(nomatch).

signedness(#c_literal{val=Flags}) ->
    [S] = [F || F <- Flags, F =:= signed orelse F =:= unsigned],
    S.


%% is_subst(Expr) -> true | false.
%%  Test whether an expression is a suitable substitution.

is_subst(#c_var{name={_,_}}) ->
    %% Funs must not be duplicated (which will happen if the variable
    %% is used more than once), because the funs will not be equal
    %% (their "index" fields will be different).
    false;
is_subst(#c_var{}) -> true;
is_subst(#c_literal{}) -> true;
is_subst(_) -> false.

%% sub_new() -> #sub{}.
%% sub_get_var(Var, #sub{}) -> Value.
%% sub_set_var(Var, Value, #sub{}) -> #sub{}.
%% sub_set_name(Name, Value, #sub{}) -> #sub{}.
%% sub_del_var(Var, #sub{}) -> #sub{}.
%% sub_subst_var(Var, Value, #sub{}) -> [{Name,Value}].
%% sub_is_in_scope(Var, #sub{}) -> boolean().
%% sub_add_scope([Var], #sub{}) -> #sub{}
%% sub_subst_scope(#sub{}) -> #sub{}
%%
%%  We use the variable name as key so as not have problems with
%%  annotations.  When adding a new substitute we fold substitute
%%  chains so we never have to search more than once.  Use orddict so
%%  we know the format.
%%
%%  In addition to the list of substitutions, we also keep track of
%%  all variable currently live (the scope).
%%
%%  sub_add_scope/2 adds variables to the scope.  sub_subst_scope/1
%%  adds dummy substitutions for all variables in the scope in order
%%  to force renaming if variables in the scope occurs as pattern
%%  variables.

sub_new() -> #sub{v=orddict:new(),s=sets:new([{version, 2}]),t=#{}}.

sub_new(#sub{}=Sub) ->
    Sub#sub{v=orddict:new(),t=#{}}.

sub_get_var(#c_var{name=V}=Var, #sub{v=S}) ->
    case orddict:find(V, S) of
	{ok,Val} ->
            propagate_compiler_generated(Var, Val);
	error ->
            Var
    end.

sub_set_var(#c_var{name=V}, Val, Sub) ->
    sub_set_name(V, Val, Sub).

sub_set_name(V, Val, #sub{v=S,s=Scope,t=Tdb0}=Sub) ->
    Tdb1 = kill_types(V, Tdb0),
    Tdb = copy_type(V, Val, Tdb1),
    Sub#sub{v=orddict:store(V, Val, S),s=sets:add_element(V, Scope),t=Tdb}.

sub_subst_var(#c_var{name=V}=Var, Val0, #sub{v=S0}) ->
    Val = propagate_compiler_generated(Var, Val0),

    %% Fold chained substitutions.
    [{V,Val}] ++ [{K,Val} || {K,#c_var{name=V1}} <- S0, V1 =:= V].

sub_add_scope(Vs, #sub{s=Scope0}=Sub) ->
    Scope = foldl(fun(V, S) when is_integer(V); is_atom(V) ->
			  sets:add_element(V, S)
		  end, Scope0, Vs),
    Sub#sub{s=Scope}.

sub_subst_scope(#sub{v=S0,s=Scope}=Sub) ->
    Initial = case S0 of
                  [{NegInt,_}|_] when is_integer(NegInt), NegInt < 0 ->
                      NegInt - 1;
                  _ ->
                      -1
              end,
    S = sub_subst_scope_1(sets:to_list(Scope), Initial, S0),
    Sub#sub{v=orddict:from_list(S)}.

%% The keys in an orddict must be unique. Make them so!
sub_subst_scope_1([H|T], Key, Acc) ->
    sub_subst_scope_1(T, Key-1, [{Key,#c_var{name=H}}|Acc]);
sub_subst_scope_1([], _, Acc) -> Acc.

sub_is_in_scope(#c_var{name=V}, #sub{s=Scope}) ->
    sets:is_element(V, Scope).

%% Propagate the 'compiler_generated' annotation (if any)
%% from From to To.
propagate_compiler_generated(From, To) ->
    case is_compiler_generated(From) andalso
        not is_compiler_generated(To) of
        true ->
            Ann = [compiler_generated|cerl:get_ann(To)],
            cerl:set_ann(To, Ann);
        false ->
            To
    end.

%% warn_no_clause_match(CaseOrig, CaseOpt) -> ok
%%  Generate a warning if none of the user-specified clauses
%%  will match.

warn_no_clause_match(CaseOrig, CaseOpt) ->
    OrigCs = cerl:case_clauses(CaseOrig),
    OptCs = cerl:case_clauses(CaseOpt),
    case any(fun(C) -> not is_compiler_generated(C) end, OrigCs) andalso
	all(fun is_compiler_generated/1, OptCs) of
	true ->
	    %% The original list of clauses did contain at least one
	    %% user-specified clause, but none of them will match.
	    %% That is probably a mistake.
	    add_warning(CaseOrig, {nomatch,no_clause});
	false ->
	    %% Either there were user-specified clauses left in
	    %% the transformed clauses, or else none of the original
	    %% clauses were user-specified to begin with (as in 'andalso').
	    ok
    end.

%% clauses(E, [Clause], TopLevel, Context, Sub, Anno) -> [Clause].
%%  Trim the clauses by removing all clauses AFTER the first one which
%%  is guaranteed to match.  Also remove all trivially false clauses.

clauses(E, [C0|Cs], Ctxt, Sub, LitExpr, Anno) ->
    #c_clause{pats=Ps,guard=G} = C1 = clause(C0, E, Ctxt, Sub),
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{E,Ps}]),
    case {will_match(E, Ps),will_succeed(G)} of
	{yes,yes} ->
	    case LitExpr of
		false ->
		    Line = get_line(cerl:get_ann(C1)),
		    shadow_warning(Cs, Line, Anno);
		true ->
		    %% If the case expression is a literal,
		    %% it is probably OK that some clauses don't match.
		    %% It is a probably some sort of debug macro.
		    ok
	    end,
	    [C1];				%Skip the rest
	{_Mat,no} ->				%Guard fails.
            add_warning(C1, {nomatch,guard}),
	    clauses(E, Cs, Ctxt, Sub, LitExpr, Anno);	%Skip this clause
	{_Mat,_Suc} ->
	    [C1|clauses(E, Cs, Ctxt, Sub, LitExpr, Anno)]
    end;
clauses(_, [], _, _, _, _) -> [].

shadow_warning([C|Cs], none, Anno) ->
    add_warning(C, {nomatch,shadow}),
    shadow_warning(Cs, none, Anno);
shadow_warning([C|Cs], Line, Anno) ->
    case keyfind(function, 1, Anno) of
	{function, {Name, Arity}} ->
            add_warning(C, {nomatch,{shadow,Line,{Name,Arity}}});
	_ ->
            add_warning(C, {nomatch,{shadow,Line}})
    end,
    shadow_warning(Cs, Line, Anno);
shadow_warning([], _, _) -> ok.

%% will_succeed(Guard) -> yes | 'maybe' | no.
%%  Test if we know whether a guard will succeed/fail or just don't
%%  know.  Be VERY conservative!

will_succeed(#c_literal{val=true}) -> yes;
will_succeed(#c_literal{val=false}) -> no;
will_succeed(_Guard) -> 'maybe'.

%% will_match(Expr, [Pattern]) -> yes | 'maybe'.
%%  We KNOW that this function is only used after optimizations
%%  in case_opt/4. Therefore clauses that can definitely not match
%%  have already been pruned.

will_match(#c_values{es=Es}, Ps) ->
    will_match_1(cerl_clauses:match_list(Ps, Es));
will_match(E, [P]) ->
    will_match_1(cerl_clauses:match(P, E)).

will_match_1({false,_}) -> 'maybe';
will_match_1({true,_}) -> yes.

%% opt_bool_case(CoreExpr, Sub) - CoreExpr'.
%%
%%  In bodies, do various optimizations to case statements that have
%%  boolean case expressions. We don't do the optimizations in guards,
%%  because they would thwart the optimization in beam_ssa_bool.
%%
%%  We start with some simple optimizations and normalizations
%%  to facilitate later optimizations.
%%
%%  If the case expression can only return a boolean we can remove any
%%  clause that cannot possibly match 'true' or 'false'. Also, any
%%  clause following both 'true' and 'false' clause can be removed. If
%%  successful, we will end up like this:
%%
%%  case BoolExpr of           	    case BoolExpr of
%%     true ->			       false ->
%%       ...; 			    	  ...;
%%     false ->            OR          true ->
%%       ...				  ...
%%     end.			    end.
%%
%%  We give up if there are clauses with guards, or if there
%%  is a variable clause that matches anything.

opt_bool_case(#c_case{}=Case, #sub{in_guard=true}) ->
    %% v3_kernel does a better job without "help".
    Case;
opt_bool_case(#c_case{arg=Arg}=Case0, #sub{in_guard=false}) ->
    case is_bool_expr(Arg) of
	false ->
	    Case0;
	true ->
	    try opt_bool_clauses(Case0) of
		Case ->
		    opt_bool_not(Case)
	    catch
		impossible ->
		    Case0
	    end
    end.

opt_bool_clauses(#c_case{clauses=Cs}=Case) ->
    Case#c_case{clauses=opt_bool_clauses(Cs, false, false)}.

opt_bool_clauses(Cs, true, true) ->
    %% We have now seen clauses that match both true and false.
    %% Any remaining clauses cannot possibly match.
    case Cs of
	[_|_] ->
	    shadow_warning(Cs, none, []),
	    [];
	[] ->
	    []
    end;
opt_bool_clauses([#c_clause{pats=[#c_literal{val=Lit}],
			    guard=#c_literal{val=true}}=C|Cs], SeenT, SeenF) ->
    case is_boolean(Lit) of
	false ->
	    %% Not a boolean - this clause can't match.
            add_warning(C, {nomatch,clause_type}),
	    opt_bool_clauses(Cs, SeenT, SeenF);
	true ->
	    %% This clause will match.
	    case {Lit,SeenT,SeenF} of
                {false,_,false} ->
                    [C|opt_bool_clauses(Cs, SeenT, true)];
                {true,false,_} ->
                    [C|opt_bool_clauses(Cs, true, SeenF)];
                _ ->
                    add_warning(C, {nomatch,shadow}),
                    opt_bool_clauses(Cs, SeenT, SeenF)
	    end
    end;
opt_bool_clauses([#c_clause{pats=Ps,guard=#c_literal{val=true}}=C|Cs], SeenT, SeenF) ->
    case Ps of
	[#c_var{}] ->
	    %% Will match a boolean.
	    throw(impossible);
	[#c_alias{}] ->
	    %% Might match a boolean.
	    throw(impossible);
	_ ->
	    %% The clause cannot possible match a boolean.
	    %% We can remove it.
	    add_warning(C, {nomatch,clause_type}),
	    opt_bool_clauses(Cs, SeenT, SeenF)
    end;
opt_bool_clauses([_|_], _, _) ->
    %% A clause with a guard. Give up.
    throw(impossible).
%% We intentionally do not have a clause that match an empty
%% list. An empty list would indicate that the clauses do not
%% match all possible values for the case expression, which
%% means that the Core Erlang program is illegal. We prefer to
%% crash on such illegal input, rather than producing code that will
%% fail mysteriously at run time.


%% opt_bool_not(Case) -> CoreExpr.
%%  Try to eliminate one or more calls to 'not' at the top level
%%  of the case expression.
%%
%%  We KNOW that the case expression is guaranteed to return
%%  a boolean and that there are exactly two clauses: one that
%%  matches 'true' and one that matches 'false'.
%%
%%  case not Expr of       	    case Expr of
%%     true ->			       false ->
%%       ...; 			    	  ...;
%%     false ->           ==>          true ->
%%       ...				  ...;
%%     end.			       NewVar ->
%%                                        erlang:error(badarg)
%%                                  end.

opt_bool_not(#c_case{arg=Arg,clauses=Cs0}=Case0) ->
    case Arg of
	#c_call{anno=Anno,module=#c_literal{val=erlang},
 		name=#c_literal{val='not'},
 		args=[Expr]} ->
	    Cs = [opt_bool_not_invert(C) || C <- Cs0] ++
		 [#c_clause{anno=[compiler_generated],
			    pats=[#c_var{name=cor_variable}],
			    guard=#c_literal{val=true},
			    body=#c_call{anno=Anno,
					 module=#c_literal{val=erlang},
					 name=#c_literal{val=error},
					 args=[#c_literal{val=badarg}]}}],
	    Case = Case0#c_case{arg=Expr,clauses=Cs},
	    opt_bool_not(Case);
	_ ->
            Case0
    end.

opt_bool_not_invert(#c_clause{pats=[#c_literal{val=Bool}]}=C) ->
    C#c_clause{pats=[#c_literal{val=not Bool}]}.

%% eval_case(Case) -> #c_case{} | #c_let{}.
%%  If possible, evaluate a case at compile time.  We know that the
%%  last clause is guaranteed to match so if there is only one clause
%%  with a pattern containing only variables then rewrite to a let.

eval_case(#c_case{arg=E,clauses=[#c_clause{pats=Ps0,
					   guard=#c_literal{val=true},
					   body=B}]}=Case, Sub) ->
    Es = case cerl:is_c_values(E) of
	     true -> cerl:values_es(E);
	     false -> [E]
	 end,
    %% Consider:
    %%
    %%   case SomeSideEffect() of
    %%      X=Y -> ...
    %%   end
    %%
    %% We must not rewrite it to:
    %%
    %%   let <X,Y> = <SomeSideEffect(),SomeSideEffect()> in ...
    %%
    %% because SomeSideEffect() would be evaluated twice.
    %%
    %% Instead we must evaluate the case expression in an outer let
    %% like this:
    %%
    %%   let NewVar = SomeSideEffect() in
    %%       let <X,Y> = <NewVar,NewVar> in ...
    %%
    Vs = make_vars([], length(Es)),
    case cerl_clauses:match_list(Ps0, Vs) of
	{false,_} ->
	    %% This can only happen if the Core Erlang code is
	    %% handwritten or generated by another code generator
	    %% than v3_core. Assuming that the Core Erlang program
	    %% is correct, the clause will always match at run-time.
	    Case;
	{true,Bs} ->
	    eval_case_warn(B),
	    {Ps,As} = unzip(Bs),
	    InnerLet = cerl:c_let(Ps, core_lib:make_values(As), B),
	    Let = cerl:c_let(Vs, E, InnerLet),
	    expr(Let, sub_new(Sub))
    end;
eval_case(Case, _) -> Case.

eval_case_warn(#c_primop{anno=Anno,
			 name=#c_literal{val=match_fail},
			 args=[_]}=Core) ->
    case keyfind(eval_failure, 1, Anno) of
	false ->
	    ok;
	{eval_failure,badmap} ->
	    %% Example: M = not_map, M#{k:=v}
	    add_warning(Core, {failed,bad_map_update})
    end;
eval_case_warn(_) -> ok.

%% case_opt(CaseArg, [Clause]) -> {CaseArg,[Clause]}.
%%  Try and optimise a case by avoid building tuples or lists
%%  in the case expression. Instead combine the variable parts
%%  of the case expression to multiple "values". If a clause
%%  refers to the constructed term in the case expression (which
%%  was not built), introduce a let into the guard and/or body to
%%  build the term.
%%
%%     case {ok,[Expr1,Expr2]} of	case <Expr1,Expr2> of
%%         {ok,[P1,P2]} -> ...		    <P1,P2> -> ...
%%          .  	       	       	  ==>        .
%%          .				     .
%%          .				     .
%%         Var ->                           <Var1,Var2> ->
%%             ... Var ...                     let <Var> = {ok,[Var1,Var2]}
%%                                                 in ... Var ...
%%          .                                 .
%%          .                                 .
%%          .				      .
%%     end.				end.
%%
case_opt(Arg, Cs0, Sub) ->
    Cs1 = [{cerl:clause_pats(C),C,[],[]} || C <- Cs0],
    Args0 = case cerl:is_c_values(Arg) of
		false -> [Arg];
		true -> cerl:values_es(Arg)
	    end,
    LitExpr = cerl:is_literal(Arg),
    {Args,Cs2} = case_opt_args(Args0, Cs1, Sub, LitExpr, []),
    Cs = [cerl:update_c_clause(C,
			       reverse(Ps),
			       letify(Bs, cerl:clause_guard(C)),
			       letify(Bs, cerl:clause_body(C))) ||
	     {[],C,Ps,Bs} <- Cs2],
    {core_lib:make_values(Args),Cs}.

case_opt_args([A0|As0], Cs0, Sub, LitExpr, Acc) ->
    case case_opt_arg(A0, Sub, Cs0, LitExpr) of
        {error,Cs1} ->
	    %% Nothing to be done. Move on to the next argument.
            Cs = [{Ps,C,[P|PsAcc],Bs} || {[P|Ps],C,PsAcc,Bs} <- Cs1],
	    case_opt_args(As0, Cs, Sub, LitExpr, [A0|Acc]);
	{ok,As1,Cs} ->
	    %% The argument was either expanded (from tuple/list) or
	    %% removed (literal).
	    case_opt_args(As1++As0, Cs, Sub, LitExpr, Acc)
    end;
case_opt_args([], Cs, _Sub, _LitExpr, Acc) ->
    {reverse(Acc),Cs}.

%% case_opt_arg(Expr, Sub, Clauses0, LitExpr) ->
%%         {ok,Args,Clauses} | error
%%  Try to expand one argument to several arguments (if tuple/list)
%%  or to remove a literal argument.
%%
case_opt_arg(E0, Sub, Cs, LitExpr) ->
    case cerl:is_c_var(E0) of
	false ->
	    case_opt_arg_1(E0, Cs, LitExpr);
	true ->
	    case case_will_var_match(Cs) of
		true ->
		    %% All clauses will match a variable in the
		    %% current position. Don't expand this variable
		    %% (that can only make the code worse).
		    {error,Cs};
		false ->
		    %% If possible, expand this variable to a previously
		    %% constructed tuple
		    E = case_expand_var(E0, Sub),
		    case_opt_arg_1(E, Cs, LitExpr)
	    end
    end.

case_opt_arg_1(E0, Cs0, LitExpr) ->
    case cerl:is_data(E0) of
	false ->
            {error,Cs0};
	true ->
	    E = case_opt_compiler_generated(E0),
	    Cs = case_opt_nomatch(E, Cs0, LitExpr),
            case cerl:is_literal(E) of
                true ->
		    case_opt_lit(E, Cs);
		false ->
		    case_opt_data(E, Cs)
	    end
    end.

%% case_will_var_match([Clause]) -> true | false.
%%  Return if all clauses will match a variable in the
%%  current position.
%%
case_will_var_match(Cs) ->
    all(fun({[P|_],_,_,_}) ->
		case cerl_clauses:match(P, any) of
		    {true,_} -> true;
		    _ -> false
		end
	end, Cs).


%% case_opt_compiler_generated(Core) -> Core'
%%  Mark Core expressions as compiler generated to ensure that
%%  no warnings are generated if they turn out to be unused.
%%  To pretty-printed Core Erlang easier to read, don't mark
%%  constructs that can't cause warnings to be emitted.
%%
case_opt_compiler_generated(Core) ->
    F = fun(C) ->
		case cerl:type(C) of
		    alias -> C;
		    var -> C;
		    _ -> cerl:set_ann(C, [compiler_generated])
		end
	end,
    cerl_trees:map(F, Core).


%% case_expand_var(Expr0, Sub) -> Expr
%%  If Expr0 is a variable that is known to be bound to a
%%  constructed tuple, return the tuple instead. Otherwise
%%  return Expr0 unchanged.

case_expand_var(E, #sub{t=Tdb}) ->
    Key = cerl:var_name(E),
    case Tdb of
        #{Key:=T} -> T;
        _ -> E
    end.

%% case_opt_nomatch(E, Clauses, LitExpr) -> Clauses'
%%  Remove all clauses that cannot possibly match.

case_opt_nomatch(E, [{[P|_],C,_,_}=Current|Cs], LitExpr) ->
    case cerl_clauses:match(P, E) of
        none ->
            %% The pattern will not match the case expression. Remove
            %% the clause.  Unless the entire case expression is a
            %% literal, also emit a warning.
            case LitExpr of
                false -> add_warning(C, {nomatch,clause_type});
                true -> ok
            end,
            case_opt_nomatch(E, Cs, LitExpr);
        _ ->
            [Current|case_opt_nomatch(E, Cs, LitExpr)]
    end;
case_opt_nomatch(_, [], _) -> [].

%% case_opt_lit(Literal, Clauses0) -> {ok,[],Clauses} | error
%%  The current part of the case expression is a literal. That
%%  means that we will know at compile-time whether a clause
%%  will match, and we can remove the corresponding pattern from
%%  each clause.
%%
%%  The only complication is if the literal is a binary or map.
%%  In general, it is difficult to know whether a binary or
%%  map pattern will match, so we give up in that case.

case_opt_lit(Lit, Cs0) ->
    try case_opt_lit_1(Lit, Cs0) of
	Cs ->
	    {ok,[],Cs}
    catch
	throw:impossible ->
            {error,Cs0}
    end.

case_opt_lit_1(E, [{[P|Ps],C,PsAcc,Bs0}|Cs]) ->
    %% Non-matching clauses have already been removed
    %% in case_opt_nomatch/3.
    case cerl_clauses:match(P, E) of
	{true,Bs} ->
	    %% The pattern matches the literal. Remove the pattern
	    %% and update the bindings.
            [{Ps,C,PsAcc,Bs++Bs0}|case_opt_lit_1(E, Cs)];
	{false,_} ->
	    %% Binary literal and pattern. We are not sure whether
	    %% the pattern will match.
	    throw(impossible)
    end;
case_opt_lit_1(_, []) -> [].

%% case_opt_data(Expr, Clauses0, LitExpr) -> {ok,Exprs,Clauses}
%%  The case expression is a non-atomic data constructor (cons
%%  or tuple). We can know at compile time whether each clause
%%  will match, and we can delay the building of the data to
%%  the clauses where it is actually needed.

case_opt_data(E, Cs0) ->
    TypeSig = {cerl:data_type(E),cerl:data_arity(E)},
    try case_opt_data_1(Cs0, TypeSig) of
	Cs ->
	    Es = cerl:data_es(E),
	    {ok,Es,Cs}
    catch
	throw:impossible ->
	    %% The pattern contained a binary or map.
	    {error,Cs0}
    end.

case_opt_data_1([{[P0|Ps0],C,PsAcc,Bs0}|Cs], TypeSig) ->
    P = case_opt_compiler_generated(P0),
    {Ps1,Bs} = case_opt_data_2(P, TypeSig, Bs0),
    [{Ps1++Ps0,C,PsAcc,Bs}|case_opt_data_1(Cs, TypeSig)];
case_opt_data_1([], _) -> [].

case_opt_data_2(P, TypeSig, Bs0) ->
    case case_analyze_pat(P) of
	{[],Pat} when Pat =/= none ->
	    DataEs = cerl:data_es(P),
	    {DataEs,Bs0};
	{[V|Vs],none} ->
	    {Type,Arity} = TypeSig,
	    Ann = [compiler_generated],
            true = ?IS_FUNC_ARITY(Arity),
	    Vars = make_vars(Ann, Arity),
	    Data = cerl:ann_make_data(Ann, Type, Vars),
	    Bs = [{V,Data} | [{Var,V} || Var <- Vs] ++ Bs0],
	    {Vars,Bs};
	{[V|Vs],Pat} when Pat =/= none ->
	    {Type,_} = TypeSig,
	    DataEs = cerl:data_es(Pat),
	    Vars = pat_to_expr_list(DataEs),
	    Ann = [compiler_generated],
	    Data = cerl:ann_make_data(Ann, Type, Vars),
	    Bs = [{V,Data} | [{Var,V} || Var <- Vs] ++ Bs0],
	    {DataEs,Bs}
    end.

case_analyze_pat(P) ->
    case_analyze_pat_1(P, [], none).

case_analyze_pat_1(P, Vs, Pat) ->
    case cerl:type(P) of
	alias ->
	    V = cerl:alias_var(P),
	    Apat = cerl:alias_pat(P),
	    case_analyze_pat_1(Apat, [V|Vs], Pat);
	var ->
	    {[P|Vs],Pat};
	_ ->
	    {Vs,P}
    end.

%% pat_to_expr(Pattern) -> Expression.
%%  Convert a pattern to an expression if possible. We KNOW that
%%  all variables in the pattern will be bound.
%%
%%  Throw an 'impossible' exception if a map or (non-literal)
%%  binary is encountered. Trying to use a map pattern as an
%%  expression is incorrect, while rebuilding a potentially
%%  huge binary in an expression would be wasteful.

pat_to_expr(P) ->
    case cerl:type(P) of
	alias ->
	    cerl:alias_var(P);
	var ->
	    P;
	_ ->
	    case cerl:is_data(P) of
		false ->
		    %% Map or binary.
		    throw(impossible);
		true ->
		    Es = pat_to_expr_list(cerl:data_es(P)),
		    cerl:update_data(P, cerl:data_type(P), Es)
	    end
    end.

pat_to_expr_list(Ps) -> [pat_to_expr(P) || P <- Ps].

make_vars(A, Max) when ?IS_FUNC_ARITY(Max) ->
    make_vars(A, 1, Max).

make_vars(A, I, Max) when I =< Max ->
    [make_var(A)|make_vars(A, I+1, Max)];
make_vars(_, _, _) -> [].

make_var(A) ->
    #c_var{anno=A,name=make_var_name()}.

make_var_name() ->
    N = get(new_var_num),
    put(new_var_num, N+1),
    N.

letify(Bs, Body) ->
    Ann = cerl:get_ann(Body),
    foldr(fun({V,Val}, B) ->
		  cerl:ann_c_let(Ann, [V], Val, B)
	  end, Body, Bs).

%% opt_not_in_let(Let) -> Cerl
%%  Try to optimize away a 'not' operator in a 'let'.

-spec opt_not_in_let(cerl:c_let()) -> cerl:cerl().

opt_not_in_let(#c_let{vars=[_]=Vs0,arg=Arg0,body=Body0}=Let) ->
    case opt_not_in_let_0(Vs0, Arg0, Body0) of
	{[],#c_values{es=[]},Body} ->
	    Body;
	{Vs,Arg,Body} ->
	    Let#c_let{vars=Vs,arg=Arg,body=Body}
    end;
opt_not_in_let(Let) -> Let.

opt_not_in_let_0([#c_var{name=V}]=Vs0, Arg0, Body0) ->
    case cerl:type(Body0) of
	call ->
	    %% let <V> = Expr in not V  ==>
	    %%    let <> = <> in notExpr
	    case opt_not_in_let_1(V, Body0, Arg0) of
		no ->
		    {Vs0,Arg0,Body0};
		{yes,Body} ->
		    {[],#c_values{es=[]},Body}
	    end;
	'let' ->
	    %% let <V> = Expr in let <Var> = not V in Body  ==>
	    %%    let <Var> = notExpr in Body
	    %% V must not be used in Body.
	    LetArg = cerl:let_arg(Body0),
	    case opt_not_in_let_1(V, LetArg, Arg0) of
		no ->
		    {Vs0,Arg0,Body0};
		{yes,Arg} ->
		    LetBody = cerl:let_body(Body0),
		    case core_lib:is_var_used(V, LetBody) of
			true ->
			    {Vs0,Arg0,Body0};
			false ->
			    LetVars = cerl:let_vars(Body0),
			    {LetVars,Arg,LetBody}
		    end
	    end;
	_ ->
	    {Vs0,Arg0,Body0}
    end.

opt_not_in_let_1(V, Call, Body) ->
    case Call of
	#c_call{module=#c_literal{val=erlang},
		name=#c_literal{val='not'},
		args=[#c_var{name=V}]} ->
	    opt_not_in_let_2(Body, Call);
	_ ->
	    no
    end.

opt_not_in_let_2(#c_case{clauses=Cs0}=Case, NotCall) ->
    Vars = make_vars([], 1),
    Body = NotCall#c_call{args=Vars},
    Cs = [begin
	      Let = #c_let{vars=Vars,arg=B,body=Body},
	      C#c_clause{body=opt_not_in_let(Let)}
	  end || #c_clause{body=B}=C <- Cs0],
    {yes,Case#c_case{clauses=Cs}};
opt_not_in_let_2(#c_call{}=Call0, _NotCall) ->
    invert_call(Call0);
opt_not_in_let_2(_, _) -> no.

invert_call(#c_call{module=#c_literal{val=erlang},
		    name=#c_literal{val=Name0},
		    args=[_,_]}=Call) ->
    case inverse_rel_op(Name0) of
	no -> no;
	Name -> {yes,Call#c_call{name=#c_literal{val=Name}}}
    end;
invert_call(#c_call{}) -> no.

%% inverse_rel_op(Op) -> no | RevOp

inverse_rel_op('=:=') -> '=/=';
inverse_rel_op('=/=') -> '=:=';
inverse_rel_op('==') -> '/=';
inverse_rel_op('/=') -> '==';
inverse_rel_op('>') -> '=<';
inverse_rel_op('<') -> '>=';
inverse_rel_op('>=') -> '<';
inverse_rel_op('=<') -> '>';
inverse_rel_op(_) -> no.


%% opt_bool_case_in_let(LetExpr) -> Core

opt_bool_case_in_let(#c_let{vars=Vs,arg=Arg,body=B}=Let, Sub) ->
    opt_bool_case_in_let_1(Vs, Arg, B, Let, Sub).

opt_bool_case_in_let_1([#c_var{name=V}], Arg,
		  #c_case{arg=#c_var{name=V}}=Case0, Let, Sub) ->
    case is_simple_case_arg(Arg) of
	true ->
	    Case = opt_bool_case(Case0#c_case{arg=Arg}, Sub),
	    case core_lib:is_var_used(V, Case) of
		false -> Case;
		true -> Let
	    end;
	false ->
	    Let
    end;
opt_bool_case_in_let_1(_, _, _, Let, _) -> Let.

%% is_simple_case_arg(Expr) -> true|false
%%  Determine whether the Expr is simple enough to be worth
%%  substituting into a case argument. (Common substitutions
%%  of variables and literals are assumed to have been already
%%  handled by the caller.)

is_simple_case_arg(#c_cons{}) -> true;
is_simple_case_arg(#c_tuple{}) -> true;
is_simple_case_arg(#c_call{}) -> true;
is_simple_case_arg(#c_apply{}) -> true;
is_simple_case_arg(_) -> false.

%% is_bool_expr(Core) -> true|false
%%  Check whether the Core expression is guaranteed to
%%  return a boolean.
%%

is_bool_expr(#c_call{module=#c_literal{val=erlang},
		     name=#c_literal{val=Name},args=Args}) ->
    NumArgs = length(Args),
    erl_internal:comp_op(Name, NumArgs) orelse
	erl_internal:new_type_test(Name, NumArgs) orelse
        erl_internal:bool_op(Name, NumArgs);
is_bool_expr(#c_try{arg=E,vars=[#c_var{name=X}],body=#c_var{name=X},
		   handler=#c_literal{val=false}}) ->
    is_bool_expr(E);
is_bool_expr(#c_case{clauses=Cs}) ->
    is_bool_expr_list(Cs);
is_bool_expr(#c_clause{body=B}) ->
    is_bool_expr(B);
is_bool_expr(#c_let{body=B}) ->
    is_bool_expr(B);
is_bool_expr(#c_literal{val=Val}) ->
    is_boolean(Val);
is_bool_expr(_) -> false.

is_bool_expr_list([C|Cs]) ->
    is_bool_expr(C) andalso is_bool_expr_list(Cs);
is_bool_expr_list([]) -> true.

%% is_safe_bool_expr(Core) -> true|false
%%  Check whether the Core expression ALWAYS returns a boolean
%%  (i.e. it cannot fail).
%%
is_safe_bool_expr(Core) ->
    is_safe_bool_expr_1(Core, sets:new([{version, 2}])).

is_safe_bool_expr_1(#c_call{module=#c_literal{val=erlang},
                            name=#c_literal{val=is_function},
                            args=[A,#c_literal{val=Arity}]},
                    _BoolVars) when is_integer(Arity), Arity >= 0 ->
    is_safe_simple(A);
is_safe_bool_expr_1(#c_call{module=#c_literal{val=erlang},
                            name=#c_literal{val=is_function}},
                    _BoolVars) ->
    false;
is_safe_bool_expr_1(#c_call{module=#c_literal{val=erlang},
			    name=#c_literal{val=Name},args=Args},
		    BoolVars) ->
    NumArgs = length(Args),
    case (erl_internal:comp_op(Name, NumArgs) orelse
	  erl_internal:new_type_test(Name, NumArgs)) andalso
	is_safe_simple_list(Args) of
	true ->
	    true;
	false ->
	    %% Boolean operators are safe if all arguments are boolean.
	    erl_internal:bool_op(Name, NumArgs) andalso
		is_safe_bool_expr_list(Args, BoolVars)
    end;
is_safe_bool_expr_1(#c_let{vars=Vars,arg=Arg,body=B}, BoolVars) ->
    case is_safe_simple(Arg) of
	true ->
	    case {is_safe_bool_expr_1(Arg, BoolVars),Vars} of
		{true,[#c_var{name=V}]} ->
		    is_safe_bool_expr_1(B, sets:add_element(V, BoolVars));
		{false,_} ->
		    is_safe_bool_expr_1(B, BoolVars)
	    end;
	false -> false
    end;
is_safe_bool_expr_1(#c_literal{val=Val}, _BoolVars) ->
    is_boolean(Val);
is_safe_bool_expr_1(#c_var{name=V}, BoolVars) ->
    sets:is_element(V, BoolVars);
is_safe_bool_expr_1(_, _) -> false.

is_safe_bool_expr_list([C|Cs], BoolVars) ->
    case is_safe_bool_expr_1(C, BoolVars) of
	true -> is_safe_bool_expr_list(Cs, BoolVars);
	false -> false
    end;
is_safe_bool_expr_list([], _) -> true.

opt_fun_call(#c_let{vars=[#c_var{name=V}],arg=#c_fun{}=FunDef,body=Body}=Let) ->
    try do_opt_fun_call(V, FunDef, Body) of
        impossible -> Let;
        Expr -> Expr
    catch
        throw:impossible ->
            Let
    end;
opt_fun_call(Expr) -> Expr.

do_opt_fun_call(V, FunDef, #c_apply{op=#c_var{name=V},args=CallArgs}) ->
    Values = core_lib:make_values(CallArgs),
    simplify_fun_call(V, Values, FunDef, CallArgs);
do_opt_fun_call(V, FunDef, #c_let{arg=#c_apply{op=#c_var{name=V},args=CallArgs},
                                  body=Rest}=Let) ->
    Values = core_lib:make_values([Rest|CallArgs]),
    Inlined = simplify_fun_call(V, Values, FunDef, CallArgs),
    Let#c_let{arg=Inlined};
do_opt_fun_call(V, FunDef, #c_seq{arg=#c_apply{op=#c_var{name=V},args=CallArgs},
                                  body=Rest}=Seq) ->
    Values = core_lib:make_values([Rest|CallArgs]),
    Inlined = simplify_fun_call(V, Values, FunDef, CallArgs),
    Seq#c_seq{arg=Inlined};
do_opt_fun_call(_, _, _) -> impossible.

simplify_fun_call(V, Values, #c_fun{vars=Vars,body=FunBody}, CallArgs) ->
    case not core_lib:is_var_used(V, Values) andalso length(Vars) =:= length(CallArgs) of
        true ->
            %% Safe to inline.
            #c_let{vars=Vars,
                   arg=core_lib:make_values(CallArgs),
                   body=FunBody};
        false ->
            %% The fun is used more than once or there is an arity mismatch.
            throw(impossible)
    end.

is_failing_clause(#c_clause{body=B}) ->
    will_fail(B).

%% opt_build_stacktrace(Let) -> Core.
%%  If the stacktrace is *only* used in a call to erlang:raise/3,
%%  there is no need to build a cooked stackframe using build_stacktrace/1.

opt_build_stacktrace(#c_let{vars=[#c_var{name=Cooked}],
                            arg=#c_primop{name=#c_literal{val=build_stacktrace},
                                          args=[RawStk]},
                            body=Body}=Let) ->
    case Body of
        #c_call{module=#c_literal{val=erlang},
                name=#c_literal{val=raise},
                args=[Class,Exp,#c_var{name=Cooked}]} ->
            case core_lib:is_var_used(Cooked, #c_cons{hd=Class,tl=Exp}) of
                true ->
                    %% Not safe. The stacktrace is used in the class or
                    %% reason.
                    Let;
                false ->
                    %% The stacktrace is only used in the last
                    %% argument for erlang:raise/3. There is no need
                    %% to build the stacktrace. Replace the call to
                    %% erlang:raise/3 with the the raw_raise/3
                    %% instruction, which will use a raw stacktrace.
                    #c_primop{name=#c_literal{val=raw_raise},
                              args=[Class,Exp,RawStk]}
            end;
        #c_let{vars=[#c_var{name=V}],arg=Arg,body=B0} when V =/= Cooked ->
            case core_lib:is_var_used(Cooked, Arg) of
                false ->
                    %% The built stacktrace is not used in the argument,
                    %% so we can sink the building of the stacktrace into
                    %% the body of the let.
                    B = opt_build_stacktrace(Let#c_let{body=B0}),
                    Body#c_let{body=B};
                true ->
                    Let
            end;
        #c_seq{arg=Arg,body=B0} ->
            case core_lib:is_var_used(Cooked, Arg) of
                false ->
                    %% The built stacktrace is not used in the argument,
                    %% so we can sink the building of the stacktrace into
                    %% the body of the sequence.
                    B = opt_build_stacktrace(Let#c_let{body=B0}),
                    Body#c_seq{body=B};
                true ->
                    Let
            end;
        #c_case{clauses=Cs0} ->
            NilBody = #c_literal{val=[]},
            Cs1 = [C#c_clause{body=NilBody} || C <- Cs0],
            Case = Body#c_case{clauses=Cs1},
            case core_lib:is_var_used(Cooked, Case) of
                false ->
                    %% The built stacktrace is not used in the case
                    %% argument or in the head of any clause. Thus
                    %% it is safe sink the building of the stacktrace
                    %% into each arm of the case.
                    Cs = [begin
                              B = opt_build_stacktrace(Let#c_let{body=B0}),
                              C#c_clause{body=B}
                          end || #c_clause{body=B0}=C <- Cs0],
                    Body#c_case{clauses=Cs};
                true ->
                    Let
            end;
        _ ->
            Let
    end;
opt_build_stacktrace(Expr) ->
    Expr.

%% opt_case_in_let(Let) -> Let'
%%  Try to avoid building tuples that are immediately matched.
%%  A common pattern is:
%%
%%      {V1,V2,...} = case E of P -> ... {Val1,Val2,...}; ... end
%%
%%  In Core Erlang the pattern would look like this:
%%
%%   let <V> = case E of
%%             	 ... -> ... {Val1,Val2}
%%   	         ...
%%   	       end,
%%   in case V of
%%   	{A,B} -> ... <use A and B> ...
%%   end
%%
%%  Rewrite this to:
%%
%%   let <V1,V2> = case E of
%%             	 ... -> ... <Val1,Val2>
%%   	         ...
%%   	       end,
%%   in
%%      let <V> = {V1,V2}
%%      in case V of
%%   	    {A,B} -> ... <use A and B> ...
%%        end
%%
%%  Note that the second 'case' is unchanged. The other optimizations
%%  in this module will eliminate the building of the tuple and
%%  rewrite the second case to:
%%
%%   case <V1,V2> of
%%    <A,B> -> ... <use A and B> ...
%%   end
%%

opt_case_in_let(#c_let{vars=Vs,arg=Arg0,body=B}=Let0) ->
    case matches_data(Vs, B) of
	{yes,TypeSig} ->
	    case delay_build(Arg0, TypeSig) of
		no ->
		    Let0;
		{yes,Vars,Arg,Data} ->
		    InnerLet = Let0#c_let{arg=Data},
		    Let0#c_let{vars=Vars,arg=Arg,body=InnerLet}
	    end;
	no ->
	    Let0
    end.

matches_data([#c_var{name=V}], #c_case{arg=#c_var{name=V},
				       clauses=[#c_clause{pats=[P]}|_]}) ->
    case cerl:is_data(P) of
	false ->
	    no;
	true ->
	    case cerl:data_type(P) of
		{atomic,_} ->
		    no;
		Type ->
		    {yes,{Type,cerl:data_arity(P)}}
	    end
    end;
matches_data(_, _) -> no.

delay_build(Core, TypeSig) ->
    case cerl:is_data(Core) of
	true -> no;
	false -> delay_build_1(Core, TypeSig)
    end.

delay_build_1(Core0, TypeSig) ->
    try delay_build_expr(Core0, TypeSig) of
	Core ->
	    {Type,Arity} = TypeSig,
	    Ann = [compiler_generated],
            true = ?IS_FUNC_ARITY(Arity),
	    Vars = make_vars(Ann, Arity),
	    Data = cerl:ann_make_data(Ann, Type, Vars),
	    {yes,Vars,Core,Data}
    catch
	throw:impossible ->
	    no
    end.

delay_build_cs([#c_clause{body=B0}=C0|Cs], TypeSig) ->
    B = delay_build_expr(B0, TypeSig),
    C = C0#c_clause{body=B},
    [C|delay_build_cs(Cs, TypeSig)];
delay_build_cs([], _) -> [].

delay_build_expr(Core, {Type,Arity}=TypeSig) ->
    case cerl:is_data(Core) of
	false ->
	    delay_build_expr_1(Core, TypeSig);
	true ->
	    case {cerl:data_type(Core),cerl:data_arity(Core)} of
		{Type,Arity} ->
		    core_lib:make_values(cerl:data_es(Core));
		{_,_} ->
		    throw(impossible)
	    end
    end.

delay_build_expr_1(#c_case{clauses=Cs0}=Case, TypeSig) ->
    Cs = delay_build_cs(Cs0, TypeSig),
    Case#c_case{clauses=Cs};
delay_build_expr_1(#c_let{body=B0}=Let, TypeSig) ->
    B = delay_build_expr(B0, TypeSig),
    Let#c_let{body=B};
delay_build_expr_1(#c_seq{body=B0}=Seq, TypeSig) ->
    B = delay_build_expr(B0, TypeSig),
    Seq#c_seq{body=B};
delay_build_expr_1(Core, _TypeSig) ->
    case will_fail(Core) of
	true -> Core;
	false -> throw(impossible)
    end.

%% opt_let(#c_let{}, Context, Sub) -> CoreTerm
%%  Optimize a let construct.

opt_let(Let0, Ctxt, Sub) ->
    case opt_not_in_let(Let0) of
	#c_let{}=Let ->
	    opt_let_0(Let, Ctxt, Sub);
	Expr ->
	    expr(Expr, Ctxt, Sub)
    end.

opt_let_0(#c_let{arg=Arg0}=Let, Ctxt, Sub) ->
    Arg = body(Arg0, value, Sub),		%This is a body
    case will_fail(Arg) of
	true -> Arg;
	false -> opt_let_1(Let, Arg, Ctxt, Sub)
    end.

opt_let_1(#c_let{vars=Vs0,body=B0}=Let, Arg0, Ctxt, Sub0) ->
    %% Optimise let and add new substitutions.
    {Vs,Args,Sub1} = let_substs(Vs0, Arg0, Sub0),
    BodySub = update_let_types(Vs, Args, Sub1),
    Sub = Sub1#sub{v=[],s=sets:new([{version, 2}])},
    B = body(B0, Ctxt, BodySub),
    Arg = core_lib:make_values(Args),
    opt_let_2(Let, Vs, Arg, B, B0, Sub).


%% opt_let_2(Let0, Vs0, Arg0, Body, PrevBody, Ctxt, Sub) -> Core.
%%  Do final simplifications of the let.
%%
%%  Note that the substitutions and scope in Sub have been cleared
%%  and should not be used.

opt_let_2(Let0, Vs0, Arg0, Body, PrevBody, Sub) ->
    case {Vs0,Arg0,Body} of
	{[#c_var{name=V}],Arg1,#c_var{name=V}} ->
            %% let <Var> = Arg in <Var>  ==>  Arg
            Arg1;
	{[],#c_values{es=[]},_} ->
	    %% No variables left.
	    Body;
	{[#c_var{name=V}=Var]=Vars0,Arg1,Body} ->
            case core_lib:is_var_used(V, Body) of
                false ->
                    %% If the variable is not used in the body, we can
                    %% rewrite the let to a sequence:
                    %%    let <Var> = Arg in BodyWithoutVar ==>
                    %%        seq Arg BodyWithoutVar
                    Arg = maybe_suppress_warnings(Arg1, Var, PrevBody),
                    #c_seq{arg=Arg,body=Body};
                true ->
                    Let1 = Let0#c_let{vars=Vars0,arg=Arg1,body=Body},
                    post_opt_let(Let1, Sub)
	    end;
        {_,_,_} ->
            %% The argument for a sequence must be a single value (not
            %% #c_values{}). Therefore, we must keep the let.
            Let1 = Let0#c_let{vars=Vs0,arg=Arg0,body=Body},
            post_opt_let(Let1, Sub)
    end.

%% post_opt_let(Let, Sub)
%%  Final optimizations of the let.
%%
%%  Note that the substitutions and scope in Sub have been cleared
%%  and should not be used.

post_opt_let(Let0, Sub) ->
    Let1 = opt_bool_case_in_let(Let0, Sub),
    opt_build_stacktrace(Let1).

%% maybe_suppress_warnings(Arg, #c_var{}, PreviousBody) -> Arg'
%%  Try to suppress false warnings when a variable is not used.
%%  For instance, we don't expect a warning for useless building in:
%%
%%    R = #r{},  %No warning expected.
%%    R#r.f      %Optimization would remove the reference to R.
%%
%%  To avoid false warnings, we will check whether the variables were
%%  referenced in the original unoptimized code. If they were, we will
%%  consider the warning false and suppress it.

maybe_suppress_warnings(Arg, #c_var{name=V}, PrevBody) ->
    case should_suppress_warning(Arg) of
	true ->
	    Arg;				%Already suppressed.
	false ->
	    case core_lib:is_var_used(V, PrevBody) of
		true ->
		    suppress_warning([Arg]);
		false ->
		    Arg
	    end
    end.

%% Suppress warnings for a Core Erlang expression whose value will
%% be ignored.
suppress_warning([H|T]) ->
    case cerl:is_literal(H) of
	true ->
	    suppress_warning(T);
	false ->
	    case cerl:is_data(H) of
		true ->
		    suppress_warning(cerl:data_es(H) ++ T);
		false ->
		    %% Some other thing, such as a function call.
		    %% This cannot be the compiler's fault, so the
		    %% warning should not be suppressed. We must
		    %% be careful not to destroy tail-recursion.
		    case T of
			[] ->
			    H;
			[_|_] ->
			    cerl:c_seq(H, suppress_warning(T))
		    end
	    end
    end;
suppress_warning([]) -> void().

move_case_into_arg(#c_case{arg=#c_let{vars=OuterVars0,arg=OuterArg,
                                      body=InnerArg0}=Outer,
                           clauses=InnerClauses}=Inner, Sub) ->
    %%
    %% case let <OuterVars> = <OuterArg> in <InnerArg> of
    %%     <InnerClauses>
    %% end
    %%
    %%       ==>
    %%
    %% let <OuterVars> = <OuterArg>
    %% in case <InnerArg> of <InnerClauses> end
    %%
    ScopeSub0 = sub_subst_scope(Sub#sub{t=#{}}),
    {OuterVars,ScopeSub} = var_list(OuterVars0, ScopeSub0),
    InnerArg = body(InnerArg0, ScopeSub),
    Outer#c_let{vars=OuterVars,arg=OuterArg,
                body=Inner#c_case{arg=InnerArg,clauses=InnerClauses}};
move_case_into_arg(#c_case{arg=#c_case{arg=OuterArg,
                                       clauses=[OuterCa0,OuterCb]}=Outer,
                           clauses=InnerClauses}=Inner0, Sub) ->
    case is_failing_clause(OuterCb) of
        true ->
            #c_clause{pats=OuterPats0,guard=OuterGuard0,
                      body=InnerArg0} = OuterCa0,
            %%
            %% case case <OuterArg> of
            %%          <OuterPats> when <OuterGuard> -> <InnerArg>
            %%          <OuterCb>
            %%          ...
            %%      end of
            %%     <InnerClauses>
            %% end
            %%
            %%       ==>
            %%
            %% case <OuterArg> of
            %%     <OuterPats> when <OuterGuard> ->
            %%         case <InnerArg> of <InnerClauses> end
            %%     <OuterCb>
            %% end
            %%
            ScopeSub0 = sub_subst_scope(Sub#sub{t=#{}}),

	    %% We KNOW that pattern_list/2 has already been called for OuterPats0;
	    %% therefore, it cannot throw an exception.
	    {OuterPats,ScopeSub} = pattern_list(OuterPats0, ScopeSub0),
	    OuterGuard = guard(OuterGuard0, ScopeSub),
	    InnerArg = body(InnerArg0, ScopeSub),
	    Inner = Inner0#c_case{arg=InnerArg,clauses=InnerClauses},
	    OuterCa = OuterCa0#c_clause{pats=OuterPats,
					guard=OuterGuard,
					body=Inner},
	    Outer#c_case{arg=OuterArg,
			 clauses=[OuterCa,OuterCb]};
        false ->
            Inner0
    end;
move_case_into_arg(#c_case{arg=#c_seq{arg=OuterArg,body=InnerArg}=Outer,
                           clauses=InnerClauses}=Inner, _Sub) ->
    %%
    %% case do <OuterArg> <InnerArg> of
    %%     <InnerClauses>
    %% end
    %%
    %%       ==>
    %%
    %% do <OuterArg>
    %%    case <InnerArg> of <InerClauses> end
    %%
    Outer#c_seq{arg=OuterArg,
                body=Inner#c_case{arg=InnerArg,clauses=InnerClauses}};
move_case_into_arg(Expr, _) ->
    Expr.

%%%
%%% Update type information.
%%%

update_let_types(Vs, Args, Sub) when is_list(Args) ->
    update_let_types_1(Vs, Args, Sub);
update_let_types(_Vs, _Arg, Sub) ->
    %% The argument is a complex expression (such as a 'case')
    %% that returns multiple values.
    Sub.

update_let_types_1([#c_var{name=V}|Vs], [A|As], Sub0) ->
    Sub = update_types(V, A, Sub0),
    update_let_types_1(Vs, As, Sub);
update_let_types_1([], [], Sub) -> Sub.

update_types(V, #c_tuple{}=P, #sub{t=Tdb}=Sub) ->
    Sub#sub{t=Tdb#{V=>P}};
update_types(_, _, Sub) -> Sub.

%% kill_types(V, Tdb) -> Tdb'
%%  Kill any entries that references the variable,
%%  either in the key or in the value.

kill_types(Var, Tdb) ->
    #{Key => Value ||
         Key := Value <- Tdb,
         Key =/= Var,
         not core_lib:is_var_used(Var, Value)}.

%% copy_type(DestVar, SrcVar, Tdb) -> Tdb'
%%  If the SrcVar has a type, assign it to DestVar.
%%
copy_type(V, #c_var{name=Src}, Tdb) ->
    case Tdb of
        #{Src:=Type} -> Tdb#{V=>Type};
        _ -> Tdb
    end;
copy_type(_, _, Tdb) -> Tdb.

%% The atom `ok', is widely used in Erlang for "void" values.

void() -> #c_literal{val=ok}.

%%%
%%% Handling of the `useless_building` warning (building a term that
%%% is never used).
%%%
%%% Consider this code fragment:
%%%
%%%     [ {ok,Term} ],
%%%     ok
%%%
%%% The list that is ignored contains a tuple that is also ignored.
%%% While optimizing this code fragment, two warnings for useless
%%% building will be generated: one for the list and one for the tuple
%%% inside. Before the introduction of column numbers, those two warnings
%%% would be coalesced to one because they had the same line number.
%%%
%%% With column numbers, we will need a more sophisticated solution to
%%% avoid emitting annoying duplicate warnings.
%%%
%%% Note that if two separate terms are being built on the same line, we
%%% do expect to get two warnings:
%%%
%%%     [ {ok,Term} ],   [ {error,BadTerm} ], ok
%%%     ^                ^
%%%
%%% (The carets mark the expected columns for the warnings.)
%%%
%%% To handle those requirements, we will use the #sub{} record to keep
%%% track of whether we are at the top level or have descended into
%%% a sub expression.
%%%

%% Note in the Sub record that we have are no longer at the top level.
descend(_Core, #sub{top=false}=Sub) ->
    Sub;
descend(Core, #sub{top=true}=Sub) ->
    case should_suppress_warning(Core) of
        true ->
            %% In a list comprehension being ignored such as:
            %%
            %%   [{error,Z} || Z <- List], ok
            %%
            %% the warning for ignoring the cons cell should be
            %% suppressed, but there should still be a warning for
            %% ignoring the {error,Z} tuple. Therefore, pretend that
            %% we are still at the top level.
            Sub;
        false ->
            %% No longer at top level. Warnings for useless building
            %% should now be suppressed.
            Sub#sub{top=false}
    end.

warn_useless_building(Core, #sub{top=Top}) ->
    case Top of
        true -> add_warning(Core, {ignored,useless_building});
        false -> ok
    end.

%%%
%%% Handling of warnings.
%%%

init_warnings() ->
    put({?MODULE,warnings}, []).
add_warning(Core, Term) ->
    case should_suppress_warning(Core) of
	true ->
	    ok;
	false ->
	    Anno = cerl:get_ann(Core),
	    Location = get_location(Anno),
	    File = get_file(Anno),
	    Key = {?MODULE,warnings},
	    case get(Key) of
		[{File,[{Location,?MODULE,Term}]}|_] ->
		    ok;				%We already have
						%an identical warning.
		Ws ->
		    put(Key, [{File,[{Location,?MODULE,Term}]}|Ws])
	    end
    end.

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([{Line, _Column} | _T]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.

get_location([Line|_]) when is_integer(Line) ->
    Line;
get_location([{Line, Column} | _T]) when is_integer(Line), is_integer(Column) ->
    {Line,Column};
get_location([_|T]) ->
    get_location(T);
get_location([]) ->
    none.

get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> "no_file". % should not happen

should_suppress_warning(Core) ->
    is_compiler_generated(Core) orelse
	is_result_unwanted(Core).

is_compiler_generated(Core) ->
    Ann = cerl:get_ann(Core),
    member(compiler_generated, Ann).

is_result_unwanted(Core) ->
    Ann = cerl:get_ann(Core),
    member(result_not_wanted, Ann).

get_warnings() ->
    ordsets:from_list((erase({?MODULE,warnings}))).

classify_call(Call) ->
    Mod = cerl:concrete(cerl:call_module(Call)),
    Name = cerl:concrete(cerl:call_name(Call)),
    Arity = cerl:call_arity(Call),
    {Mod, Name, Arity}.

-type error() :: {'failed' | 'nomatch' | 'ignored', term()}.

-spec format_error(error()) -> nonempty_string().

format_error({failed,{eval_failure,Call,Reason}}) ->
    flatten(io_lib:format("~ts will fail with a '~p' exception",
                          [format_call(Call, false),Reason]));
format_error({failed,embedded_binary_size}) ->
    "binary construction will fail with a 'badarg' exception "
	"(field size for binary/bitstring greater than actual size)";
format_error({failed,{embedded_unit,Unit,Size}}) ->
    M = io_lib:format("binary construction will fail with a 'badarg' exception "
		      "(size ~p cannot be evenly divided by unit ~p)", [Size,Unit]),
    flatten(M);
format_error({failed,bad_unicode}) ->
    "binary construction will fail with a 'badarg' exception "
	"(invalid Unicode code point in a utf8/utf16/utf32 segment)";
format_error({failed,bad_float_size}) ->
    "binary construction will fail with a 'badarg' exception "
	"(invalid size for a float segment)";
format_error({failed,bad_map_update}) ->
    "map update will fail with a 'badmap' exception";
format_error({failed,bad_call}) ->
    "invalid function call";
format_error({nomatch,{shadow,Line,{Name, Arity}}}) ->
    M = io_lib:format("this clause for ~ts/~B cannot match because a previous "
		      "clause at line ~p always matches", [Name, Arity, Line]),
    flatten(M);
format_error({nomatch,{shadow,Line}}) ->
    M = io_lib:format("this clause cannot match because a previous clause at line ~p "
		      "always matches", [Line]),
    flatten(M);
format_error({nomatch,shadow}) ->
    "this clause cannot match because a previous clause always matches";
format_error({nomatch,guard}) ->
    "this clause cannot match because its guard evaluates to 'false'";
format_error({nomatch,{bit_syntax_truncated,Signess,Val,Sz}}) ->
    S = case Signess of
	    signed -> "a 'signed'";
	    unsigned -> "an 'unsigned'"
	end,
    F = "this clause cannot match because the value ~P"
	" will not fit in ~s binary segment of size ~p",
    flatten(io_lib:format(F, [Val,10,S,Sz]));
format_error({nomatch,{bit_syntax_unsigned,Val}}) ->
    F = "this clause cannot match because the negative value ~P"
	" will never match the value of an 'unsigned' binary segment",
    flatten(io_lib:format(F, [Val,10]));
format_error({nomatch,{bit_syntax_size,Sz}}) ->
    F = "this clause cannot match because '~P' is not a valid size for a binary segment",
    flatten(io_lib:format(F, [Sz,10]));
format_error({nomatch,{bit_syntax_type,Val,Type}}) ->
    F = "this clause cannot match because '~P' is not of the"
	" expected type '~p'",
    flatten(io_lib:format(F, [Val,10,Type]));
format_error({nomatch,{bit_syntax_unicode,Val}}) ->
    F = "this clause cannot match because the value ~p"
	" is not a valid Unicode code point",
    flatten(io_lib:format(F, [Val]));
format_error({nomatch,no_clause}) ->
    "no clause will ever match";
format_error({nomatch,clause_type}) ->
    "this clause cannot match because of different types/sizes";
format_error({ignored,{no_effect,{erlang,F,A}}}) ->
    {Fmt,Args} = case erl_internal:comp_op(F, A) of
		     true ->
			 {"use of operator ~p has no effect",[F]};
		     false ->
			 case erl_internal:bif(F, A) of
			     false ->
				 {"the call to erlang:~p/~p has no effect",[F,A]};
			     true ->
				 {"the call to ~p/~p has no effect",[F,A]}
			 end
		 end,
    flatten(io_lib:format(Fmt, Args));
format_error({ignored,{result,Call}}) ->
    Fmt = "the result of ~ts is ignored "
	"(suppress the warning by assigning the expression to the _ variable)",
    flatten(io_lib:format(Fmt, [format_call(Call, true)]));
format_error({ignored,useless_building}) ->
    "a term is constructed, but never used".

format_call({erlang,make_fun,3}, _) ->
    "fun construction";
format_call({Mod, Name, Arity}, UseProgressiveForm) ->
    case is_operator(Mod, Name, Arity) of
        true ->
            Str = case UseProgressiveForm of
                      true -> "evaluating";
                      false -> "evaluation of"
                  end,
            [Str, io_lib:format(" operator ~p/~p", [Name,Arity])];
        false ->
            Str = case UseProgressiveForm of
                      true -> "calling";
                      false -> "the call to"
                  end,
            case is_auto_imported(Mod, Name, Arity) of
                true ->
                    [Str, io_lib:format(" ~p/~p", [Name,Arity])];
                false ->
                    [Str, io_lib:format(" ~p:~p/~p", [Mod,Name,Arity])]
            end
    end.

is_operator(erlang, Name, Arity) ->
    try
        _ = erl_internal:op_type(Name, Arity),
        true
    catch
        error:_ ->
            false
    end;
is_operator(_, _, _) -> false.

is_auto_imported(erlang, Name, Arity) ->
    erl_internal:bif(Name, Arity);
is_auto_imported(_, _, _) -> false.

-ifdef(DEBUG).
%% In order for simplify_let/2 to work correctly, the list of
%% in-scope variables must always be a superset of the free variables
%% in the current expression (otherwise we might fail to rename a variable
%% when needed and get a name capture bug).

verify_scope(E, #sub{s=Scope}) ->
    Free0 = cerl_trees:free_variables(E),
    Free = [V || V <- Free0, not is_tuple(V)],	%Ignore function names.
    case is_subset_of_scope(Free, Scope) of
	true ->
	    true;
	false ->
	    io:format("~p\n", [E]),
	    io:format("~p\n", [Free]),
	    io:format("~p\n", [ordsets:from_list(sets:to_list(Scope))]),
	    false
    end.

is_subset_of_scope([V|Vs], Scope) ->
    sets:is_element(V, Scope) andalso is_subset_of_scope(Vs, Scope);
is_subset_of_scope([], _) -> true.

-endif.

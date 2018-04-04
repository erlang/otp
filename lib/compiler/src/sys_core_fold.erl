%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2017. All Rights Reserved.
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

%% Variable value info.
-record(sub, {v=[],                                 %Variable substitutions
              s=cerl_sets:new() :: cerl_sets:set(), %Variables in scope
              t=#{} :: map(),                       %Types
              in_guard=false}).                     %In guard or not.

-type type_info() :: cerl:cerl() | 'bool' | 'integer'.
-type yes_no_maybe() :: 'yes' | 'no' | 'maybe'.
-type sub() :: #sub{}.

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
    %% Find a suitable starting value for the variable counter. Note
    %% that this pass assumes that new_var_name/1 returns a variable
    %% name distinct from any variable used in the entire body of
    %% the function. We use integers as variable names to avoid
    %% filling up the atom table when compiling huge functions.
    Count = cerl_trees:next_free_variable_name(B0),
    put(new_var_num, Count),
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

%% opt_guard_try(Expr) -> Expr.
%%
opt_guard_try(#c_seq{arg=Arg,body=Body0}=Seq) ->
    Body = opt_guard_try(Body0),
    WillFail = case Body of
		   #c_call{module=#c_literal{val=erlang},
			   name=#c_literal{val=error},
			   args=[_]} ->
		       true;
		   #c_literal{val=false} ->
		       true;
		   _ ->
		       false
	       end,
    case Arg of
	#c_call{module=#c_literal{val=Mod},
		name=#c_literal{val=Name},
		args=Args} when WillFail ->
	    %% We have sequence consisting of a call (evaluated
	    %% for a possible exception and/or side effect only),
	    %% followed by 'false' or a call to error/1.
	    %%   Since the sequence is inside a try block that will
	    %% default to 'false' if any exception occurs, not
	    %% evalutating the call will not change the behaviour
	    %% provided that the call has no side effects.
	    case erl_bifs:is_pure(Mod, Name, length(Args)) of
		false ->
		    %% Not a pure BIF (meaning that this is not
		    %% a guard and that we must keep the call).
		    Seq#c_seq{body=Body};
		true ->
		    %% The BIF has no side effects, so it can
		    %% be safely removed.
		    Body
	    end;
	_ ->
	    Seq#c_seq{body=Body}
    end;
opt_guard_try(#c_case{clauses=Cs}=Term) ->
    Term#c_case{clauses=opt_guard_try_list(Cs)};
opt_guard_try(#c_clause{body=B0}=Term) ->
    Term#c_clause{body=opt_guard_try(B0)};
opt_guard_try(#c_let{arg=Arg,body=B0}=Term) ->
    case opt_guard_try(B0) of
	#c_literal{}=B ->
	    opt_guard_try(#c_seq{arg=Arg,body=B});
	B ->
	    Term#c_let{body=B}
    end;
opt_guard_try(Term) -> Term.

opt_guard_try_list([C|Cs]) ->
    [opt_guard_try(C)|opt_guard_try_list(Cs)];
opt_guard_try_list([]) -> [].

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
expr(#c_literal{val=Val}=L, Ctxt, _Sub) ->
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
		    add_warning(L, useless_building),
		    void()
	    end;
	value -> L
    end;
expr(#c_cons{anno=Anno,hd=H0,tl=T0}=Cons, Ctxt, Sub) ->
    H1 = expr(H0, Ctxt, Sub),
    T1 = expr(T0, Ctxt, Sub),
    case Ctxt of
	effect ->
	    add_warning(Cons, useless_building),
	    make_effect_seq([H1,T1], Sub);
	value ->
	    ann_c_cons(Anno, H1, T1)
    end;
expr(#c_tuple{anno=Anno,es=Es0}=Tuple, Ctxt, Sub) ->
    Es = expr_list(Es0, Ctxt, Sub),
    case Ctxt of
	effect ->
	    add_warning(Tuple, useless_building),
	    make_effect_seq(Es, Sub);
	value ->
	    ann_c_tuple(Anno, Es)
    end;
expr(#c_map{anno=Anno,arg=V0,es=Es0}=Map, Ctxt, Sub) ->
    Es = pair_list(Es0, Ctxt, Sub),
    case Ctxt of
	effect ->
	    add_warning(Map, useless_building),
	    make_effect_seq(Es, Sub);
	value ->
	    V = expr(V0, Ctxt, Sub),
	    ann_c_map(Anno,V,Es)
    end;
expr(#c_binary{segments=Ss}=Bin0, Ctxt, Sub) ->
    %% Warn for useless building, but always build the binary
    %% anyway to preserve a possible exception.
    case Ctxt of
	effect -> add_warning(Bin0, useless_building);
	value -> ok
    end,
    Bin1 = Bin0#c_binary{segments=bitstr_list(Ss, Sub)},
    Bin = bin_un_utf(Bin1),
    eval_binary(Bin);
expr(#c_fun{}=Fun, effect, _) ->
    %% A fun is created, but not used. Warn, and replace with the void value.
    add_warning(Fun, useless_building),
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
            case {Ctxt,is_safe_simple(Arg, Sub)} of
                {effect,true} -> B1;
                {effect,false} ->
                    case is_safe_simple(B1, Sub) of
                        true -> Arg;
                        false -> Seq0#c_seq{arg=Arg,body=B1}
                    end;
                {value,true} -> B1;
                {value,false} -> Seq0#c_seq{arg=Arg,body=B1}
	    end
    end;
expr(#c_let{}=Let0, Ctxt, Sub) ->
    Let = opt_case_in_let(Let0),
    case simplify_let(Let, Sub) of
	impossible ->
	    %% The argument for the let is "simple", i.e. has no
	    %% complex structures such as let or seq that can be entered.
	    ?ASSERT(verify_scope(Let, Sub)),
	    opt_simple_let(Let, Ctxt, Sub);
	Expr ->
	    %% The let body was successfully moved into the let argument.
	    %% Now recursively re-process the new expression.
	    Expr
    end;
expr(#c_letrec{body=#c_var{}}=Letrec, effect, _Sub) ->
    %% This is named fun in an 'effect' context. Warn and ignore.
    add_warning(Letrec, useless_building),
    void();
expr(#c_letrec{defs=Fs0,body=B0}=Letrec, Ctxt, Sub) ->
    Fs1 = map(fun ({Name,Fb}) ->
		      {Name,expr(Fb, {letrec,Ctxt}, Sub)}
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
    case opt_bool_case(Case0, Sub) of
	#c_case{arg=Arg0,clauses=Cs0}=Case1 ->
	    Arg1 = body(Arg0, value, Sub),
	    LitExpr = cerl:is_literal(Arg1),
	    {Arg2,Cs1} = case_opt(Arg1, Cs0, Sub),
	    Cs2 = clauses(Arg2, Cs1, Ctxt, Sub, LitExpr),
	    Case = Case1#c_case{arg=Arg2,clauses=Cs2},
	    warn_no_clause_match(Case1, Case),
	    Expr = eval_case(Case, Sub),
            move_case_into_arg(Expr, Sub);
	Other ->
	    expr(Other, Ctxt, Sub)
    end;
expr(#c_receive{clauses=Cs0,timeout=T0,action=A0}=Recv, Ctxt, Sub) ->
    Cs1 = clauses(#c_var{name='_'}, Cs0, Ctxt, Sub, false),
    T1 = expr(T0, value, Sub),
    A1 = body(A0, Ctxt, Sub),
    Recv#c_receive{clauses=Cs1,timeout=T1,action=A1};
expr(#c_apply{anno=Anno,op=Op0,args=As0}=App, _, Sub) ->
    Op1 = expr(Op0, value, Sub),
    As1 = expr_list(As0, value, Sub),
    case cerl:is_data(Op1) andalso not is_literal_fun(Op1) of
        false ->
	    App#c_apply{op=Op1,args=As1};
	true ->
	    add_warning(App, invalid_call),
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
    case is_safe_simple(B1, Sub) of
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
	    %% Remove any calls that are evaluated for effect only.
	    E2 = opt_guard_try(E1),

	    %% We can remove try/catch if the expression is an
	    %% expression that cannot fail.
	    case is_safe_bool_expr(E2, Sub) orelse is_safe_simple(E2, Sub) of
		true -> E2;
		false -> Try#c_try{arg=E2}
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
    case is_safe_simple(E1, Sub0) of
	true ->
	    expr(#c_let{anno=A,vars=Vs1,arg=E1,body=B1}, value, Sub0);
	false ->
	    {Evs1,Sub2} = var_list(Evs0, Sub0),
	    H1 = body(H0, value, Sub2),
	    Try#c_try{arg=E1,vars=Vs1,body=B1,evars=Evs1,handler=H1}
    end.

expr_list(Es, Ctxt, Sub) ->
    [expr(E, Ctxt, Sub) || E <- Es].

pair_list(Es, Ctxt, Sub) ->
    [pair(E, Ctxt, Sub) || E <- Es].

pair(#c_map_pair{key=K,val=V}, effect, Sub) ->
    make_effect_seq([K,V], Sub);
pair(#c_map_pair{key=K0,val=V0}=Pair, value=Ctxt, Sub) ->
    K = expr(K0, Ctxt, Sub),
    V = expr(V0, Ctxt, Sub),
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

is_safe_simple(#c_var{}=Var, _) ->
    not cerl:is_c_fname(Var);
is_safe_simple(#c_cons{hd=H,tl=T}, Sub) ->
    is_safe_simple(H, Sub) andalso is_safe_simple(T, Sub);
is_safe_simple(#c_tuple{es=Es}, Sub) -> is_safe_simple_list(Es, Sub);
is_safe_simple(#c_literal{}, _) -> true;
is_safe_simple(#c_call{module=#c_literal{val=erlang},
		       name=#c_literal{val=Name},
		       args=Args}, Sub) when is_atom(Name) ->
    NumArgs = length(Args),
    case erl_internal:bool_op(Name, NumArgs) of
	true ->
	    %% Boolean operators are safe if the arguments are boolean.
	    all(fun(C) -> is_boolean_type(C, Sub) =:= yes end, Args);
	false ->
	    %% We need a rather complicated test to ensure that
	    %% we only allow safe calls that are allowed in a guard.
	    %% (Note that is_function/2 is a type test, but is not safe.)
	    erl_bifs:is_safe(erlang, Name, NumArgs) andalso
		      (erl_internal:comp_op(Name, NumArgs) orelse
		       erl_internal:new_type_test(Name, NumArgs))
    end;
is_safe_simple(_, _) -> false.

is_safe_simple_list(Es, Sub) -> all(fun(E) -> is_safe_simple(E, Sub) end, Es).

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
	    add_warning(Bin, Warning),
	    #c_call{anno=Anno,
		    module=#c_literal{val=erlang},
		    name=#c_literal{val=error},
		    args=[#c_literal{val=badarg}]}
    end.

eval_binary_1([#c_bitstr{val=#c_literal{val=Val},size=#c_literal{val=Sz},
			 unit=#c_literal{val=Unit},type=#c_literal{val=Type},
			 flags=#c_literal{val=Flags}}|Ss], Acc0) ->
    Endian = case member(big, Flags) of
		 true ->
		     big;
		 false ->
		     case member(little, Flags) of
			 true -> little;
			 false -> throw(impossible) %Native endian.
		     end
	     end,

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
	    case Sz*Unit of
		32 -> ok;
		64 -> ok;
		_ -> throw(impossible)
	    end;
	utf8 -> ok;
	utf16 -> ok;
	utf32 -> ok;
	_ ->
	    throw(impossible)
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
		true -> add_warning(Call, result_ignored);
		false -> ok
	    end,
	    no;
	true ->
	    add_warning(Call, {no_effect,{Mod,Name,A}}),
	    {yes,make_effect_seq(Args, sub_new())}
    end;
useless_call(_, _) -> no.

%% make_effect_seq([Expr], Sub) -> #c_seq{}|void()
%%  Convert a list of expressions evaluated in effect context to a chain of
%%  #c_seq{}. The body in the innermost #c_seq{} will be void().
%%  Anything that will not have any effect will be thrown away.

make_effect_seq([H|T], Sub) ->
    case is_safe_simple(H, Sub) of
	true -> make_effect_seq(T, Sub);
	false -> #c_seq{arg=H,body=make_effect_seq(T, Sub)}
    end;
make_effect_seq([], _) -> void().

%% Handling remote calls. The module/name fields have been processed.

call(#c_call{args=As}=Call, #c_literal{val=M}=M0, #c_literal{val=N}=N0, Sub) ->
    case get(no_inline_list_funcs) of
  	true ->
	    call_1(Call, M0, N0, As, Sub);
  	false ->
	    case sys_core_fold_lists:call(Call, M, N, As) of
		none ->
		    call_1(Call, M0, N0, As, Sub);
		Core ->
		    expr(Core, Sub)
	    end

      end;
call(#c_call{args=As}=Call, M, N, Sub) ->
    call_1(Call, M, N, As, Sub).

call_1(Call, M, N, As0, Sub) ->
    As1 = expr_list(As0, value, Sub),
    fold_call(Call#c_call{args=As1}, M, N, As1, Sub).

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
	    %% Evaluation of the function failed. Warn and replace
	    %% the call with a call to erlang:error/1.
	    eval_failure(Call, Reason)
    end.

%% fold_non_lit_args(Call, Module, Name, Args, Sub) -> Expr.
%%  Attempt to evaluate some pure BIF calls with one or more
%%  non-literals arguments.
%%
fold_non_lit_args(Call, erlang, is_boolean, [Arg], Sub) ->
    eval_is_boolean(Call, Arg, Sub);
fold_non_lit_args(Call, erlang, element, [Arg1,Arg2], Sub) ->
    eval_element(Call, Arg1, Arg2, Sub);
fold_non_lit_args(Call, erlang, length, [Arg], _) ->
    eval_length(Call, Arg);
fold_non_lit_args(Call, erlang, '++', [Arg1,Arg2], _) ->
    eval_append(Call, Arg1, Arg2);
fold_non_lit_args(Call, lists, append, [Arg1,Arg2], _) ->
    eval_append(Call, Arg1, Arg2);
fold_non_lit_args(Call, erlang, setelement, [Arg1,Arg2,Arg3], _) ->
    eval_setelement(Call, Arg1, Arg2, Arg3);
fold_non_lit_args(Call, erlang, is_record, [Arg1,Arg2,Arg3], Sub) ->
    eval_is_record(Call, Arg1, Arg2, Arg3, Sub);
fold_non_lit_args(Call, erlang, N, Args, Sub) ->
    NumArgs = length(Args),
    case erl_internal:comp_op(N, NumArgs) of
	true ->
	    eval_rel_op(Call, N, Args, Sub);
	false ->
	    case erl_internal:bool_op(N, NumArgs) of
		true ->
		    eval_bool_op(Call, N, Args, Sub);
		false ->
		    Call
	    end
    end;
fold_non_lit_args(Call, _, _, _, _) -> Call.

%% Evaluate a relational operation using type information.
eval_rel_op(Call, Op, [#c_var{name=V},#c_var{name=V}], _) ->
    Bool = erlang:Op(same, same),
    #c_literal{anno=cerl:get_ann(Call),val=Bool};
eval_rel_op(Call, '=:=', [Term,#c_literal{val=true}], Sub) ->
    %% BoolVar =:= true  ==>  BoolVar
    case is_boolean_type(Term, Sub) of
	yes -> Term;
	maybe -> Call;
	no -> #c_literal{val=false}
    end;
eval_rel_op(Call, '==', Ops, Sub) ->
    case is_exact_eq_ok(Ops, Sub) of
	true ->
	    Name = #c_literal{anno=cerl:get_ann(Call),val='=:='},
	    Call#c_call{name=Name};
	false ->
	    Call
    end;
eval_rel_op(Call, '/=', Ops, Sub) ->
    case is_exact_eq_ok(Ops, Sub) of
	true ->
	    Name = #c_literal{anno=cerl:get_ann(Call),val='=/='},
	    Call#c_call{name=Name};
	false ->
	    Call
    end;
eval_rel_op(Call, _, _, _) -> Call.

is_exact_eq_ok([A,B]=L, Sub) ->
    case is_int_type(A, Sub) =:= yes andalso is_int_type(B, Sub) =:= yes of
	true -> true;
	false -> is_exact_eq_ok_1(L)
    end.

is_exact_eq_ok_1([#c_literal{val=Lit}|_]) ->
    is_non_numeric(Lit);
is_exact_eq_ok_1([_|T]) ->
    is_exact_eq_ok_1(T);
is_exact_eq_ok_1([]) -> false.

is_non_numeric([H|T]) ->
    is_non_numeric(H) andalso is_non_numeric(T);
is_non_numeric(Tuple) when is_tuple(Tuple) ->
    is_non_numeric_tuple(Tuple, tuple_size(Tuple));
is_non_numeric(Map) when is_map(Map) ->
    %% Note that 17.x and 18.x compare keys in different ways.
    %% Be very conservative -- require that both keys and values
    %% are non-numeric.
    is_non_numeric(maps:to_list(Map));
is_non_numeric(Num) when is_number(Num) ->
    false;
is_non_numeric(_) -> true.

is_non_numeric_tuple(Tuple, El) when El >= 1 ->
    is_non_numeric(element(El, Tuple)) andalso
	is_non_numeric_tuple(Tuple, El-1);
is_non_numeric_tuple(_Tuple, 0) -> true.

%% Evaluate a bool op using type information. We KNOW that
%% there must be at least one non-literal argument (i.e.
%% there is no need to handle the case that all argments
%% are literal).

eval_bool_op(Call, 'and', [#c_literal{val=true},Term], Sub) ->
    eval_bool_op_1(Call, Term, Term, Sub);
eval_bool_op(Call, 'and', [Term,#c_literal{val=true}], Sub) ->
    eval_bool_op_1(Call, Term, Term, Sub);
eval_bool_op(Call, 'and', [#c_literal{val=false}=Res,Term], Sub) ->
    eval_bool_op_1(Call, Res, Term, Sub);
eval_bool_op(Call, 'and', [Term,#c_literal{val=false}=Res], Sub) ->
    eval_bool_op_1(Call, Res, Term, Sub);
eval_bool_op(Call, _, _, _) -> Call.

eval_bool_op_1(Call, Res, Term, Sub) ->
    case is_boolean_type(Term, Sub) of
	yes -> Res;
	no -> eval_failure(Call, badarg);
	maybe -> Call
    end.

%% Evaluate is_boolean/1 using type information.
eval_is_boolean(Call, Term, Sub) ->
    case is_boolean_type(Term, Sub) of
	no -> #c_literal{val=false};
	yes -> #c_literal{val=true};
	maybe -> Call
    end.

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

%% eval_element(Call, Pos, Tuple, Types) -> Val.
%%  Evaluates element/2 if the position Pos is a literal and
%%  the shape of the tuple Tuple is known.
%%
eval_element(Call, #c_literal{val=Pos}, Tuple, Types)
  when is_integer(Pos) ->
    case get_type(Tuple, Types) of
	none ->
	    Call;
	Type ->
	    Es = case cerl:is_c_tuple(Type) of
		     false -> [];
		     true -> cerl:tuple_es(Type)
		 end,
	    if
		1 =< Pos, Pos =< length(Es) ->
		    El = lists:nth(Pos, Es),
		    try
			cerl:set_ann(pat_to_expr(El), [compiler_generated])
		    catch
			throw:impossible ->
			    Call
		    end;
		true ->
		    %% Index outside tuple or not a tuple.
		    eval_failure(Call, badarg)
	    end
    end;
eval_element(Call, Pos, Tuple, Sub) ->
    case is_int_type(Pos, Sub) =:= no orelse
	is_tuple_type(Tuple, Sub) =:= no of
	true ->
	    eval_failure(Call, badarg);
	false ->
	    Call
    end.

%% eval_is_record(Call, Var, Tag, Size, Types) -> Val.
%%  Evaluates is_record/3 using type information.
%%
eval_is_record(Call, Term, #c_literal{val=NeededTag},
	       #c_literal{val=Size}, Types) ->
    case get_type(Term, Types) of
	none ->
	    Call;
	Type ->
	    Es = case cerl:is_c_tuple(Type) of
		     false -> [];
		     true -> cerl:tuple_es(Type)
		 end,
	    case Es of
		[#c_literal{val=Tag}|_] ->
		    Bool = Tag =:= NeededTag andalso
			length(Es) =:= Size,
		    #c_literal{val=Bool};
		_ ->
		    #c_literal{val=false}
	    end
    end;
eval_is_record(Call, _, _, _, _) -> Call.

%% eval_setelement(Call, Pos, Tuple, NewVal) -> Core.
%%  Evaluates setelement/3 if position Pos is an integer
%%  and the shape of the tuple Tuple is known.
%%
eval_setelement(Call, #c_literal{val=Pos}, Tuple, NewVal)
  when is_integer(Pos) ->
    case cerl:is_data(Tuple) of
	false ->
	    Call;
	true ->
	    Es0 = case cerl:is_c_tuple(Tuple) of
		      false -> [];
		      true -> cerl:tuple_es(Tuple)
		  end,
	    if
		1 =< Pos, Pos =< length(Es0) ->
		    Es = eval_setelement_1(Pos, Es0, NewVal),
		    cerl:update_c_tuple(Tuple, Es);
		true ->
		    eval_failure(Call, badarg)
	    end
    end;
eval_setelement(Call, _, _, _) -> Call.

eval_setelement_1(1, [_|T], NewVal) ->
    [NewVal|T];
eval_setelement_1(Pos, [H|T], NewVal) when Pos > 1 ->
    [H|eval_setelement_1(Pos-1, T, NewVal)].

%% eval_failure(Call, Reason) -> Core.
%%  Warn for a call that will fail and replace the call with
%%  a call to erlang:error(Reason).
%%
eval_failure(Call, Reason) ->
    add_warning(Call, {eval_failure,Reason}),
    Call#c_call{module=#c_literal{val=erlang},
		name=#c_literal{val=error},
		args=[#c_literal{val=Reason}]}.

%% simplify_apply(Call0, Mod, Func, Args) -> Call
%%  Simplify an apply/3 to a call if the number of arguments
%%  are known at compile time.

simplify_apply(Call, Mod, Func, Args) ->
    case is_atom_or_var(Mod) andalso is_atom_or_var(Func) of
	true -> simplify_apply_1(Args, Call, Mod, Func, []);
	false -> Call
    end.

simplify_apply_1(#c_literal{val=MoreArgs0}, Call, Mod, Func, Args)
  when length(MoreArgs0) >= 0 ->
    MoreArgs = [#c_literal{val=Arg} || Arg <- MoreArgs0],
    Call#c_call{module=Mod,name=Func,args=reverse(Args, MoreArgs)};
simplify_apply_1(#c_cons{hd=Arg,tl=T}, Call, Mod, Func, Args) ->
    simplify_apply_1(T, Call, Mod, Func, [Arg|Args]);
simplify_apply_1(_, Call, _, _, _) -> Call.

is_atom_or_var(#c_literal{val=Atom}) when is_atom(Atom) -> true;
is_atom_or_var(#c_var{}) -> true;
is_atom_or_var(_) -> false.

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
    Sub2 = update_types(Cexpr, Ps1, Sub1),
    GSub = case {Cexpr,Ps1,G0} of
	       {_,_,#c_literal{}} ->
		   %% No need for substitution tricks when the guard
		   %% does not contain any variables.
		   Sub2;
	       {#c_var{name='_'},_,_} ->
		   %% In a 'receive', Cexpr is the variable '_', which represents the
		   %% message being matched. We must NOT do any extra substiutions.
		   Sub2;
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
			   sub_set_var(Var, Cexpr, Sub2);
		       true ->
			   %% We must not copy funs, and especially not into guards.
			   Sub2
		   end;
	       _ ->
		   Sub2
	   end,
    G1 = guard(G0, GSub),
    B1 = body(B0, Ctxt, Sub2),
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

let_subst_list([V|Vs0], [A0|As0], Sub) ->
    {Vs1,As1,Ss} = let_subst_list(Vs0, As0, Sub),
    case is_subst(A0) of
	true ->
	    A = case is_compiler_generated(V) andalso
		    not is_compiler_generated(A0) of
		    true ->
			%% Propagate the 'compiler_generated' annotation
			%% along with the value.
			Ann = [compiler_generated|cerl:get_ann(A0)],
			cerl:set_ann(A0, Ann);
		    false ->
			A0
		end,
	    {Vs1,As1,sub_subst_var(V, A, Sub) ++ Ss};
	false ->
	    {[V|Vs1],[A0|As1],Ss}
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
    case sub_is_val(Pat, Isub) of
	true ->
	    V1 = make_var_name(),
	    Pat1 = #c_var{name=V1},
	    {Pat1,sub_set_var(Pat, Pat1, sub_add_scope([V1], Osub))};
	false ->
	    {Pat,sub_del_var(Pat, Osub)}
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
    {P1,Osub2} = pattern(P0, Isub, Osub1),
    Osub = update_types(V1, [P1], Osub2),
    {Pat#c_alias{var=V1,pat=P1},Osub}.

map_pair_pattern_list(Ps0, Isub, Osub0) ->
    {Ps,{_,Osub}} = mapfoldl(fun map_pair_pattern/2, {Isub,Osub0}, Ps0),
    {Ps,Osub}.

map_pair_pattern(#c_map_pair{op=#c_literal{val=exact},key=K0,val=V0}=Pair,{Isub,Osub0}) ->
    K = expr(K0, Isub),
    {V,Osub} = pattern(V0,Isub,Osub0),
    {Pair#c_map_pair{key=K,val=V},{Isub,Osub}}.

bin_pattern_list(Ps0, Isub, Osub0) ->
    {Ps,{_,Osub}} = mapfoldl(fun bin_pattern/2, {Isub,Osub0}, Ps0),
    {Ps,Osub}.

bin_pattern(#c_bitstr{val=E0,size=Size0}=Pat0, {Isub0,Osub0}) ->
    Size1 = expr(Size0, Isub0),
    {E1,Osub} = pattern(E0, Isub0, Osub0),
    Isub = case E0 of
	       #c_var{} -> sub_set_var(E0, E1, Isub0);
	       _ -> Isub0
	   end,
    Pat = Pat0#c_bitstr{val=E1,size=Size1},
    bin_pat_warn(Pat),
    {Pat,{Isub,Osub}}.

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
		       size=#c_literal{val=Sz},
		       unit=#c_literal{val=Unit},
		       flags=Fl}=Pat) ->
    case {Type,Sz} of
	{_,_} when is_integer(Sz), Sz >= 0 -> ok;
	{binary,all} -> ok;
	{utf8,undefined} -> ok;
	{utf16,undefined} -> ok;
	{utf32,undefined} -> ok;
	{_,_} ->
	    add_warning(Pat, {nomatch_bit_syntax_size,Sz}),
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
	    add_warning(Pat, {nomatch_bit_syntax_type,Val,Type}),
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
		    add_warning(Pat, {nomatch_bit_syntax_unsigned,Val}),
		    throw(nomatch);
		_ ->
		    ok
	    end;
	{float,#c_literal{val=Val}} when is_float(Val) ->
	    ok;
	{_,#c_literal{val=Val}} ->
	    add_warning(Pat, {nomatch_bit_syntax_type,Val,Type}),
	    throw(nomatch);
	{_,_} ->
	    ok
    end.

bit_pat_warn_int(Val, 0, signed, Pat) ->
    if
	Val =:= 0 ->
	    ok;
	true ->
	    add_warning(Pat, {nomatch_bit_syntax_truncated,signed,Val,0}),
	    throw(nomatch)
    end;
bit_pat_warn_int(Val, Sz, signed, Pat) ->
    if
	Val < 0, Val bsr (Sz - 1) =/= -1 ->
	    add_warning(Pat, {nomatch_bit_syntax_truncated,signed,Val,Sz}),
	    throw(nomatch);
	Val > 0, Val bsr (Sz - 1) =/= 0 ->
	    add_warning(Pat, {nomatch_bit_syntax_truncated,signed,Val,Sz}),
	    throw(nomatch);
	true ->
	    ok
    end;
bit_pat_warn_int(Val, _Sz, unsigned, Pat) when Val < 0 ->
    add_warning(Pat, {nomatch_bit_syntax_unsigned,Val}),
    throw(nomatch);
bit_pat_warn_int(Val, Sz, unsigned, Pat) ->
    if
	Val bsr Sz =:= 0 ->
	    ok;
	true ->
	    add_warning(Pat, {nomatch_bit_syntax_truncated,unsigned,Val,Sz}),
	    throw(nomatch)
    end.

bit_pat_warn_unicode(U, _Pat) when 0 =< U, U =< 16#10FFFF ->
    ok;
bit_pat_warn_unicode(U, Pat) ->
    add_warning(Pat, {nomatch_bit_syntax_unicode,U}),
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
%% sub_is_val(Var, #sub{}) -> boolean().
%% sub_add_scope(#sub{}) -> #sub{}
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

sub_new() -> #sub{v=orddict:new(),s=cerl_sets:new(),t=#{}}.

sub_new(#sub{}=Sub) ->
    Sub#sub{v=orddict:new(),t=#{}}.

sub_get_var(#c_var{name=V}=Var, #sub{v=S}) ->
    case orddict:find(V, S) of
	{ok,Val} -> Val;
	error -> Var
    end.

sub_set_var(#c_var{name=V}, Val, Sub) ->
    sub_set_name(V, Val, Sub).

sub_set_name(V, Val, #sub{v=S,s=Scope,t=Tdb0}=Sub) ->
    Tdb1 = kill_types(V, Tdb0),
    Tdb = copy_type(V, Val, Tdb1),
    Sub#sub{v=orddict:store(V, Val, S),s=cerl_sets:add_element(V, Scope),t=Tdb}.

sub_del_var(#c_var{name=V}, #sub{v=S,s=Scope,t=Tdb}=Sub) ->
    %% Profiling shows that for programs with many record operations,
    %% sub_del_var/2 is a bottleneck. Since the scope contains all
    %% variables that are live, we know that V cannot be present in S
    %% if it is not in the scope.
    case cerl_sets:is_element(V, Scope) of
	false ->
	    Sub#sub{s=cerl_sets:add_element(V, Scope)};
	true ->
	    Sub#sub{v=orddict:erase(V, S),t=kill_types(V, Tdb)}
    end.

sub_subst_var(#c_var{name=V}, Val, #sub{v=S0}) ->
    %% Fold chained substitutions.
    [{V,Val}] ++ [ {K,Val} || {K,#c_var{name=V1}} <- S0, V1 =:= V].

sub_add_scope(Vs, #sub{s=Scope0}=Sub) ->
    Scope = foldl(fun(V, S) when is_integer(V); is_atom(V) ->
			  cerl_sets:add_element(V, S)
		  end, Scope0, Vs),
    Sub#sub{s=Scope}.

sub_subst_scope(#sub{v=S0,s=Scope}=Sub) ->
    Initial = case S0 of
                  [{NegInt,_}|_] when is_integer(NegInt), NegInt < 0 ->
                      NegInt - 1;
                  _ ->
                      -1
              end,
    S = sub_subst_scope_1(cerl_sets:to_list(Scope), Initial, S0),
    Sub#sub{v=orddict:from_list(S)}.

%% The keys in an orddict must be unique. Make them so!
sub_subst_scope_1([H|T], Key, Acc) ->
    sub_subst_scope_1(T, Key-1, [{Key,#c_var{name=H}}|Acc]);
sub_subst_scope_1([], _, Acc) -> Acc.

sub_is_val(#c_var{name=V}, #sub{v=S,s=Scope}) ->
    %% When the bottleneck in sub_del_var/2 was eliminated, this
    %% became the new bottleneck. Since the scope contains all
    %% live variables, a variable V can only be the target for
    %% a substitution if it is in the scope.
    cerl_sets:is_element(V, Scope) andalso v_is_value(V, S).

v_is_value(Var, [{_,#c_var{name=Var}}|_]) -> true;
v_is_value(Var, [_|T]) -> v_is_value(Var, T);
v_is_value(_, []) -> false.

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
	    add_warning(CaseOrig, no_clause_match);
	false ->
	    %% Either there were user-specified clauses left in
	    %% the transformed clauses, or else none of the original
	    %% clauses were user-specified to begin with (as in 'andalso').
	    ok
    end.

%% clauses(E, [Clause], TopLevel, Context, Sub) -> [Clause].
%%  Trim the clauses by removing all clauses AFTER the first one which
%%  is guaranteed to match.  Also remove all trivially false clauses.

clauses(E, [C0|Cs], Ctxt, Sub, LitExpr) ->
    #c_clause{pats=Ps,guard=G} = C1 = clause(C0, E, Ctxt, Sub),
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{E,Ps}]),
    case {will_match(E, Ps),will_succeed(G)} of
	{yes,yes} ->
	    case LitExpr of
		false ->
		    Line = get_line(cerl:get_ann(C1)),
		    shadow_warning(Cs, Line);
		true ->
		    %% If the case expression is a literal,
		    %% it is probably OK that some clauses don't match.
		    %% It is a probably some sort of debug macro.
		    ok
	    end,
	    [C1];				%Skip the rest
	{_Mat,no} ->				%Guard fails.
	    add_warning(C1, nomatch_guard),
	    clauses(E, Cs, Ctxt, Sub, LitExpr);	%Skip this clause
	{_Mat,_Suc} ->
	    [C1|clauses(E, Cs, Ctxt, Sub, LitExpr)]
    end;
clauses(_, [], _, _, _) -> [].

shadow_warning([C|Cs], none) ->
    add_warning(C, nomatch_shadow),
    shadow_warning(Cs, none);
shadow_warning([C|Cs], Line) ->
    add_warning(C, {nomatch_shadow, Line}),
    shadow_warning(Cs, Line);
shadow_warning([], _) -> ok.

%% will_succeed(Guard) -> yes | maybe | no.
%%  Test if we know whether a guard will succeed/fail or just don't
%%  know.  Be VERY conservative!

will_succeed(#c_literal{val=true}) -> yes;
will_succeed(#c_literal{val=false}) -> no;
will_succeed(_Guard) -> maybe.

%% will_match(Expr, [Pattern]) -> yes | maybe.
%%  We KNOW that this function is only used after optimizations
%%  in case_opt/4. Therefore clauses that can definitely not match
%%  have already been pruned.

will_match(#c_values{es=Es}, Ps) ->
    will_match_1(cerl_clauses:match_list(Ps, Es));
will_match(E, [P]) ->
    will_match_1(cerl_clauses:match(P, E)).

will_match_1({false,_}) -> maybe;
will_match_1({true,_}) -> yes.

%% opt_bool_case(CoreExpr, Sub) - CoreExpr'.
%%
%%  In bodies, do various optimizations to case statements that have
%%  boolean case expressions. We don't do the optimizations in guards,
%%  because they would thwart the optimization in v3_kernel.
%%
%%  We start with some simple optimizations and normalization
%%  to facilitate later optimizations.
%%
%%  If the case expression can only return a boolean
%%  (or fail), we can remove any clause that cannot
%%  possibly match 'true' or 'false'. Also, any clause
%%  following both 'true' and 'false' clause can
%%  be removed. If successful, we will end up like this:
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
	    shadow_warning(Cs, none),
	    [];
	[] ->
	    []
    end;
opt_bool_clauses([#c_clause{pats=[#c_literal{val=Lit}],
			    guard=#c_literal{val=true}}=C|Cs], SeenT, SeenF) ->
    case is_boolean(Lit) of
	false ->
	    %% Not a boolean - this clause can't match.
	    add_warning(C, nomatch_clause_type),
	    opt_bool_clauses(Cs, SeenT, SeenF);
	true ->
	    %% This clause will match.
	    case {Lit,SeenT,SeenF} of
                {false,_,false} ->
                    [C|opt_bool_clauses(Cs, SeenT, true)];
                {true,false,_} ->
                    [C|opt_bool_clauses(Cs, true, SeenF)];
                _ ->
                    add_warning(C, nomatch_shadow),
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
	    add_warning(C, nomatch_clause_type),
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
	    opt_bool_case_redundant(Case0)
    end.

opt_bool_not_invert(#c_clause{pats=[#c_literal{val=Bool}]}=C) ->
    C#c_clause{pats=[#c_literal{val=not Bool}]}.

%% opt_bool_case_redundant(Core) -> Core'.
%%  If the sole purpose of the case is to verify that the case
%%  expression is indeed boolean, we do not need the case
%%  (since we have already verified that the case expression is
%%  boolean).
%%
%%    case BoolExpr of
%%      true -> true   	       	       ==>      BoolExpr
%%      false -> false
%%    end.
%%
opt_bool_case_redundant(#c_case{arg=Arg,clauses=Cs}=Case) ->
    case all(fun opt_bool_case_redundant_1/1, Cs) of
	true -> Arg;
	false -> opt_bool_case_guard(Case)
    end.

opt_bool_case_redundant_1(#c_clause{pats=[#c_literal{val=B}],
				    body=#c_literal{val=B}}) ->
    true;
opt_bool_case_redundant_1(_) -> false.

%% opt_bool_case_guard(Case) -> Case'.
%%  Move a boolean case expression into the guard if we are sure that
%%  it cannot fail.
%%
%%    case SafeBoolExpr of	 	case <> of
%%      true -> TrueClause;    	   ==>    <> when SafeBoolExpr -> TrueClause;
%%      false -> FalseClause		  <> when true -> FalseClause
%%    end.		 		end.
%%
%%  Generally, evaluting a boolean expression in a guard should
%%  be faster than evaulating it in the body.
%%
opt_bool_case_guard(#c_case{arg=#c_literal{}}=Case) ->
    %% It is not necessary to move a literal case expression into the
    %% guard, because it will be handled quite well in other
    %% optimizations, and moving the literal into the guard will
    %% cause some extra warnings, for instance for this code
    %%
    %%    case true of
    %%       true -> ...;
    %%       false -> ...
    %%    end.
    %%
    Case;
opt_bool_case_guard(#c_case{arg=Arg,clauses=Cs0}=Case) ->
    case is_safe_bool_expr(Arg, sub_new()) of
	false ->
	    Case;
	true ->
	    Cs = opt_bool_case_guard(Arg, Cs0),
	    Case#c_case{arg=#c_values{anno=cerl:get_ann(Arg),es=[]},
			clauses=Cs}
    end.

opt_bool_case_guard(Arg, [#c_clause{pats=[#c_literal{val=true}]}=Tc,Fc]) ->
    [Tc#c_clause{pats=[],guard=Arg},Fc#c_clause{pats=[]}];
opt_bool_case_guard(Arg, [#c_clause{pats=[#c_literal{val=false}]}=Fc,Tc]) ->
    [Tc#c_clause{pats=[],guard=Arg},Fc#c_clause{pats=[]}].

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
	{eval_failure,Reason} ->
	    %% Example: M = not_map, M#{k:=v}
	    add_warning(Core, {eval_failure,Reason})
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
		    %% matched term.
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
%%  If Expr0 is a variable that has been previously matched and
%%  is known to be a tuple, return the tuple instead. Otherwise
%%  return Expr0 unchanged.
%%
case_expand_var(E, #sub{t=Tdb}) ->
    Key = cerl:var_name(E),
    case Tdb of
        #{Key:=T0} ->
	    case cerl:is_c_tuple(T0) of
		false ->
		    E;
		true ->
		    %% The pattern was a tuple. Now we must make sure
		    %% that the elements of the tuple are suitable. In
		    %% particular, we don't want binary or map
		    %% construction here, since that means that the
		    %% binary or map will be constructed in the 'case'
		    %% argument. That is wasteful for binaries. Even
		    %% worse is that any map pattern that use the ':='
		    %% operator will fail when used in map
		    %% construction (only the '=>' operator is allowed
		    %% when constructing a map from scratch).
		    try
			cerl_trees:map(fun coerce_to_data/1, T0)
		    catch
			throw:impossible ->
			    %% Something unsuitable was found (map or
			    %% or binary). Keep the variable.
			    E
		    end
	    end;
        _ ->
	    E
    end.

%% coerce_to_data(Core) -> Core'
%%  Coerce an element originally from a pattern to an data item or or
%%  variable. Throw an 'impossible' exception if non-data Core Erlang
%%  terms such as binary construction or map construction are
%%  encountered.

coerce_to_data(C) ->
    case cerl:is_c_alias(C) of
	false ->
	    case cerl:is_data(C) orelse cerl:is_c_var(C) of
		true -> C;
		false -> throw(impossible)
	    end;
	true ->
	    coerce_to_data(cerl:alias_pat(C))
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
                false -> add_warning(C, nomatch_clause_type);
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

make_vars(A, Max) ->
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
%%  Check whether the Core expression is guaranteed to return
%%  a boolean IF IT RETURNS AT ALL.
%%
is_bool_expr(Core) ->
    is_bool_expr(Core, sub_new()).

%% is_bool_expr(Core, Sub) -> true|false
%%  Check whether the Core expression is guaranteed to return
%%  a boolean IF IT RETURNS AT ALL. Uses type information
%%  to be able to identify more expressions as booleans.
%%
is_bool_expr(#c_call{module=#c_literal{val=erlang},
		     name=#c_literal{val=Name},args=Args}=Call, _) ->
    NumArgs = length(Args),
    erl_internal:comp_op(Name, NumArgs) orelse
	erl_internal:new_type_test(Name, NumArgs) orelse
        erl_internal:bool_op(Name, NumArgs) orelse
	will_fail(Call);
is_bool_expr(#c_try{arg=E,vars=[#c_var{name=X}],body=#c_var{name=X},
		   handler=#c_literal{val=false}}, Sub) ->
    is_bool_expr(E, Sub);
is_bool_expr(#c_case{clauses=Cs}, Sub) ->
    is_bool_expr_list(Cs, Sub);
is_bool_expr(#c_clause{body=B}, Sub) ->
    is_bool_expr(B, Sub);
is_bool_expr(#c_let{vars=[V],arg=Arg,body=B}, Sub0) ->
    Sub = case is_bool_expr(Arg, Sub0) of
	      true -> update_types(V, [bool], Sub0);
	      false -> Sub0
	  end,
    is_bool_expr(B, Sub);
is_bool_expr(#c_let{body=B}, Sub) ->
    %% Binding of multiple variables.
    is_bool_expr(B, Sub);
is_bool_expr(C, Sub) ->
    is_boolean_type(C, Sub) =:= yes.

is_bool_expr_list([C|Cs], Sub) ->
    is_bool_expr(C, Sub) andalso is_bool_expr_list(Cs, Sub);
is_bool_expr_list([], _) -> true.

%% is_safe_bool_expr(Core) -> true|false
%%  Check whether the Core expression ALWAYS returns a boolean
%%  (i.e. it cannot fail). Also make sure that the expression
%%  is suitable for a guard (no calls to non-guard BIFs, local
%%  functions, or is_record/2).
%%
is_safe_bool_expr(Core, Sub) ->
    is_safe_bool_expr_1(Core, Sub, cerl_sets:new()).

is_safe_bool_expr_1(#c_call{module=#c_literal{val=erlang},
                            name=#c_literal{val=is_record},
                            args=[A,#c_literal{val=Tag},#c_literal{val=Size}]},
                    Sub, _BoolVars) when is_atom(Tag), is_integer(Size) ->
    is_safe_simple(A, Sub);
is_safe_bool_expr_1(#c_call{module=#c_literal{val=erlang},
                            name=#c_literal{val=is_record}},
                    _Sub, _BoolVars) ->
    %% The is_record/2 BIF is NOT allowed in guards.
    %% The is_record/3 BIF where its second argument is not an atom or its third
    %% is not an integer is NOT allowed in guards.
    %%
    %% NOTE: Calls like is_record(Expr, LiteralTag), where LiteralTag
    %% is a literal atom referring to a defined record, have already
    %% been rewritten to is_record(Expr, LiteralTag, TupleSize).
    false;
is_safe_bool_expr_1(#c_call{module=#c_literal{val=erlang},
                            name=#c_literal{val=is_function},
                            args=[A,#c_literal{val=Arity}]},
                    Sub, _BoolVars) when is_integer(Arity), Arity >= 0 ->
    is_safe_simple(A, Sub);
is_safe_bool_expr_1(#c_call{module=#c_literal{val=erlang},
                            name=#c_literal{val=is_function}},
                    _Sub, _BoolVars) ->
    false;
is_safe_bool_expr_1(#c_call{module=#c_literal{val=erlang},
			    name=#c_literal{val=Name},args=Args},
		    Sub, BoolVars) ->
    NumArgs = length(Args),
    case (erl_internal:comp_op(Name, NumArgs) orelse
	  erl_internal:new_type_test(Name, NumArgs)) andalso
	is_safe_simple_list(Args, Sub) of
	true ->
	    true;
	false ->
	    %% Boolean operators are safe if all arguments are boolean.
	    erl_internal:bool_op(Name, NumArgs) andalso
		is_safe_bool_expr_list(Args, Sub, BoolVars)
    end;
is_safe_bool_expr_1(#c_let{vars=Vars,arg=Arg,body=B}, Sub, BoolVars) ->
    case is_safe_simple(Arg, Sub) of
	true ->
	    case {is_safe_bool_expr_1(Arg, Sub, BoolVars),Vars} of
		{true,[#c_var{name=V}]} ->
		    is_safe_bool_expr_1(B, Sub, cerl_sets:add_element(V, BoolVars));
		{false,_} ->
		    is_safe_bool_expr_1(B, Sub, BoolVars)
	    end;
	false -> false
    end;
is_safe_bool_expr_1(#c_literal{val=Val}, _Sub, _) ->
    is_boolean(Val);
is_safe_bool_expr_1(#c_var{name=V}, _Sub, BoolVars) ->
    cerl_sets:is_element(V, BoolVars);
is_safe_bool_expr_1(_, _, _) -> false.

is_safe_bool_expr_list([C|Cs], Sub, BoolVars) ->
    case is_safe_bool_expr_1(C, Sub, BoolVars) of
	true -> is_safe_bool_expr_list(Cs, Sub, BoolVars);
	false -> false
    end;
is_safe_bool_expr_list([], _, _) -> true.

%% simplify_let(Let, Sub) -> Expr | impossible
%%  If the argument part of an let contains a complex expression, such
%%  as a let or a sequence, move the original let body into the complex
%%  expression.

simplify_let(#c_let{arg=Arg}=Let, Sub) ->
    move_let_into_expr(Let, Arg, Sub).

move_let_into_expr(#c_let{vars=InnerVs0,body=InnerBody0}=Inner,
		   #c_let{vars=OuterVs0,arg=Arg0,body=OuterBody0}=Outer, Sub0) ->
    %%
    %% let <InnerVars> = let <OuterVars> = <Arg>
    %%                   in <OuterBody>
    %% in <InnerBody>
    %%
    %%       ==>
    %%
    %% let <OuterVars> = <Arg>
    %% in let <InnerVars> = <OuterBody>
    %%    in <InnerBody>
    %%
    Arg = body(Arg0, Sub0),
    ScopeSub0 = sub_subst_scope(Sub0#sub{t=#{}}),
    {OuterVs,ScopeSub} = var_list(OuterVs0, ScopeSub0),

    OuterBody = body(OuterBody0, ScopeSub),

    {InnerVs,Sub} = var_list(InnerVs0, Sub0),
    InnerBody = body(InnerBody0, Sub),
    Outer#c_let{vars=OuterVs,arg=Arg,
		body=Inner#c_let{vars=InnerVs,arg=OuterBody,body=InnerBody}};
move_let_into_expr(#c_let{vars=Lvs0,body=Lbody0}=Let,
		   #c_case{arg=Cexpr0,clauses=[Ca0|Cs0]}=Case, Sub0) ->
    case not is_failing_clause(Ca0) andalso
        are_all_failing_clauses(Cs0) of
	true ->
	    %% let <Lvars> = case <Case-expr> of
	    %%                  <Cpats> -> <Clause-body>;
	    %%                  <OtherCpats> -> erlang:error(...)
	    %%               end
	    %% in <Let-body>
	    %%
	    %%     ==>
	    %%
	    %% case <Case-expr> of
	    %%   <Cpats> ->
	    %%       let <Lvars> = <Clause-body>
	    %%       in <Let-body>;
	    %%   <OtherCpats> -> erlang:error(...)
	    %% end

	    Cexpr = body(Cexpr0, Sub0),
	    CaPats0 = Ca0#c_clause.pats,
	    G0 = Ca0#c_clause.guard,
	    B0 = Ca0#c_clause.body,
	    ScopeSub0 = sub_subst_scope(Sub0#sub{t=#{}}),
	    try pattern_list(CaPats0, ScopeSub0) of
		{CaPats,ScopeSub} ->
		    G = guard(G0, ScopeSub),

		    B1 = body(B0, ScopeSub),

		    {Lvs,B2,Sub1} = let_substs(Lvs0, B1, Sub0),
		    Sub2 = Sub1#sub{s=cerl_sets:union(ScopeSub#sub.s,
						      Sub1#sub.s)},
		    Lbody = body(Lbody0, Sub2),
		    B = Let#c_let{vars=Lvs,
				  arg=core_lib:make_values(B2),
				  body=Lbody},

		    Ca = Ca0#c_clause{pats=CaPats,guard=G,body=B},
		    Cs = [clause(C, Cexpr, value, Sub0) || C <- Cs0],
		    Case#c_case{arg=Cexpr,clauses=[Ca|Cs]}
	    catch
		nomatch ->
		    %% This is not a defeat. The code will eventually
		    %% be optimized to erlang:error(...) by the other
		    %% optimizations done in this module.
		    impossible
	    end;
	false -> impossible
    end;
move_let_into_expr(#c_let{vars=Lvs0,body=Lbody0}=Let,
		   #c_seq{arg=Sarg0,body=Sbody0}=Seq, Sub0) ->
    %%
    %% let <Lvars> = do <Seq-arg>
    %%                  <Seq-body>
    %% in <Let-body>
    %%
    %%       ==>
    %%
    %% do <Seq-arg>
    %%    let <Lvars> = <Seq-body>
    %%    in <Let-body>
    %%
    Sarg = body(Sarg0, Sub0),
    Sbody1 = body(Sbody0, Sub0),
    {Lvs,Sbody,Sub} = let_substs(Lvs0, Sbody1, Sub0),
    Lbody = body(Lbody0, Sub),
    Seq#c_seq{arg=Sarg,body=Let#c_let{vars=Lvs,arg=core_lib:make_values(Sbody),
				      body=Lbody}};
move_let_into_expr(_Let, _Expr, _Sub) -> impossible.

are_all_failing_clauses(Cs) ->
    all(fun is_failing_clause/1, Cs).

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
            %% The stacktrace is only used in a call to erlang:raise/3.
            %% There is no need to build the stacktrace. Replace the
            %% call to erlang:raise/3 with the the raw_raise/3 instruction,
            %% which will use a raw stacktrace.
            #c_primop{name=#c_literal{val=raw_raise},
                      args=[Class,Exp,RawStk]};
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
        #c_case{arg=Arg,clauses=Cs0} ->
            case core_lib:is_var_used(Cooked, Arg) orelse
                is_used_in_any_guard(Cooked, Cs0) of
                false ->
                    %% The built stacktrace is not used in the argument,
                    %% so we can sink the building of the stacktrace into
                    %% each arm of the case.
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

is_used_in_any_guard(V, Cs) ->
    any(fun(#c_clause{guard=G}) ->
                core_lib:is_var_used(V, G)
        end, Cs).

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
delay_build_expr_1(#c_receive{clauses=Cs0,
			      timeout=Timeout,
			      action=A0}=Rec, TypeSig) ->
    Cs = delay_build_cs(Cs0, TypeSig),
    A = case {Timeout,A0} of
	    {#c_literal{val=infinity},#c_literal{}} ->
                {_Type,Arity} = TypeSig,
                Es = lists:duplicate(Arity, A0),
                core_lib:make_values(Es);
	    _ ->
                delay_build_expr(A0, TypeSig)
	end,
    Rec#c_receive{clauses=Cs,action=A};
delay_build_expr_1(#c_seq{body=B0}=Seq, TypeSig) ->
    B = delay_build_expr(B0, TypeSig),
    Seq#c_seq{body=B};
delay_build_expr_1(Core, _TypeSig) ->
    case will_fail(Core) of
	true -> Core;
	false -> throw(impossible)
    end.

%% opt_simple_let(#c_let{}, Context, Sub) -> CoreTerm
%%  Optimize a let construct that does not contain any lets in
%%  in its argument.

opt_simple_let(Let0, Ctxt, Sub) ->
    case opt_not_in_let(Let0) of
	#c_let{}=Let ->
	    opt_simple_let_0(Let, Ctxt, Sub);
	Expr ->
	    expr(Expr, Ctxt, Sub)
    end.

opt_simple_let_0(#c_let{arg=Arg0}=Let, Ctxt, Sub) ->
    Arg = body(Arg0, value, Sub),		%This is a body
    case will_fail(Arg) of
	true -> Arg;
	false -> opt_simple_let_1(Let, Arg, Ctxt, Sub)
    end.

opt_simple_let_1(#c_let{vars=Vs0,body=B0}=Let, Arg0, Ctxt, Sub0) ->
    %% Optimise let and add new substitutions.
    {Vs,Args,Sub1} = let_substs(Vs0, Arg0, Sub0),
    BodySub = update_let_types(Vs, Args, Sub1),
    Sub = Sub1#sub{v=[],s=cerl_sets:new()},
    B = body(B0, Ctxt, BodySub),
    Arg = core_lib:make_values(Args),
    opt_simple_let_2(Let, Vs, Arg, B, B0, Sub).


%% opt_simple_let_2(Let0, Vs0, Arg0, Body, PrevBody, Ctxt, Sub) -> Core.
%%  Do final simplifications of the let.
%%
%%  Note that the substitutions and scope in Sub have been cleared
%%  and should not be used.

opt_simple_let_2(Let0, Vs0, Arg0, Body, PrevBody, Sub) ->
    case {Vs0,Arg0,Body} of
	{[#c_var{name=V}],Arg1,#c_var{name=V}} ->
            %% let <Var> = Arg in <Var>  ==>  Arg
            Arg1;
	{[],#c_values{es=[]},_} ->
	    %% No variables left.
	    Body;
	{[#c_var{name=V}=Var|Vars]=Vars0,Arg1,Body} ->
            case core_lib:is_var_used(V, Body) of
                false when Vars =:= [] ->
                    %% If the variable is not used in the body, we can
                    %% rewrite the let to a sequence:
                    %%    let <Var> = Arg in BodyWithoutVar ==>
                    %%        seq Arg BodyWithoutVar
                    Arg = maybe_suppress_warnings(Arg1, Var, PrevBody),
                    #c_seq{arg=Arg,body=Body};
                false ->
                    %% There are multiple values returned by the argument
                    %% and the first value is not used (this is a 'case'
                    %% with exported variables, but the return value is
                    %% ignored). We can remove the first variable and the
                    %% the first value returned from the 'let' argument.
                    Arg2 = remove_first_value(Arg1, Sub),
                    Let1 = Let0#c_let{vars=Vars,arg=Arg2,body=Body},
                    post_opt_let(Let1, Sub);
                true ->
                    Let1 = Let0#c_let{vars=Vars0,arg=Arg1,body=Body},
                    post_opt_let(Let1, Sub)
	    end
    end.

%% post_opt_let(Let, Sub)
%%  Final optimizations of the let.
%%
%%  Note that the substitutions and scope in Sub have been cleared
%%  and should not be used.

post_opt_let(Let0, Sub) ->
    Let1 = opt_bool_case_in_let(Let0, Sub),
    opt_build_stacktrace(Let1).


%% remove_first_value(Core0, Sub) -> Core.
%%  Core0 is an expression that returns at least two values.
%%  Remove the first value returned from Core0.

remove_first_value(#c_values{es=[V|Vs]}, Sub) ->
    Values = core_lib:make_values(Vs),
    case is_safe_simple(V, Sub) of
        false ->
            #c_seq{arg=V,body=Values};
        true ->
            Values
    end;
remove_first_value(#c_case{clauses=Cs0}=Core, Sub) ->
    Cs = remove_first_value_cs(Cs0, Sub),
    Core#c_case{clauses=Cs};
remove_first_value(#c_receive{clauses=Cs0,action=Act0}=Core, Sub) ->
    Cs = remove_first_value_cs(Cs0, Sub),
    Act = remove_first_value(Act0, Sub),
    Core#c_receive{clauses=Cs,action=Act};
remove_first_value(#c_let{body=B}=Core, Sub) ->
    Core#c_let{body=remove_first_value(B, Sub)};
remove_first_value(#c_seq{body=B}=Core, Sub) ->
    Core#c_seq{body=remove_first_value(B, Sub)};
remove_first_value(#c_primop{}=Core, _Sub) ->
    Core;
remove_first_value(#c_call{}=Core, _Sub) ->
    Core.

remove_first_value_cs(Cs, Sub) ->
    [C#c_clause{body=remove_first_value(B, Sub)} ||
        #c_clause{body=B}=C <- Cs].

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
%%% Retrieving information about types.
%%%

-spec get_type(cerl:cerl(), #sub{}) -> type_info() | 'none'.

get_type(#c_var{name=V}, #sub{t=Tdb}) ->
    case Tdb of
        #{V:=Type} -> Type;
        _ -> none
    end;
get_type(C, _) ->
    case cerl:type(C) of
	binary -> C;
	map -> C;
	_ ->
	    case cerl:is_data(C) of
		true -> C;
		false -> none
	    end
    end.

-spec is_boolean_type(cerl:cerl(), sub()) -> yes_no_maybe().

is_boolean_type(Var, Sub) ->
    case get_type(Var, Sub) of
	none ->
	    maybe;
	bool ->
	    yes;
	C ->
	    B = cerl:is_c_atom(C) andalso
		is_boolean(cerl:atom_val(C)),
	    yes_no(B)
    end.

-spec is_int_type(cerl:cerl(), sub()) -> yes_no_maybe().

is_int_type(Var, Sub) ->
    case get_type(Var, Sub) of
	none -> maybe;
	integer -> yes;
	C -> yes_no(cerl:is_c_int(C))
    end.

-spec is_tuple_type(cerl:cerl(), sub()) -> yes_no_maybe().

is_tuple_type(Var, Sub) ->
    case get_type(Var, Sub) of
	none -> maybe;
	C -> yes_no(cerl:is_c_tuple(C))
    end.

yes_no(true) -> yes;
yes_no(false) -> no.

%%%
%%% Update type information.
%%%

update_let_types(Vs, Args, Sub) when is_list(Args) ->
    update_let_types_1(Vs, Args, Sub);
update_let_types(_Vs, _Arg, Sub) ->
    %% The argument is a complex expression (such as a 'case')
    %% that returns multiple values.
    Sub.

update_let_types_1([#c_var{}=V|Vs], [A|As], Sub0) ->
    Sub = update_types_from_expr(V, A, Sub0),
    update_let_types_1(Vs, As, Sub);
update_let_types_1([], [], Sub) -> Sub.

update_types_from_expr(V, Expr, Sub) ->
    Type = extract_type(Expr, Sub),
    update_types(V, [Type], Sub).

extract_type(#c_call{module=#c_literal{val=erlang},
		     name=#c_literal{val=Name},
		     args=Args}=Call, Sub) ->
    case returns_integer(Name, Args) of
	true -> integer;
	false -> extract_type_1(Call, Sub)
    end;
extract_type(Expr, Sub) ->
    extract_type_1(Expr, Sub).

extract_type_1(Expr, Sub) ->
    case is_bool_expr(Expr, Sub) of
	false -> Expr;
	true -> bool
    end.

returns_integer('band', [_,_]) -> true;
returns_integer('bnot', [_]) -> true;
returns_integer('bor', [_,_]) -> true;
returns_integer('bxor', [_,_]) -> true;
returns_integer(bit_size, [_]) -> true;
returns_integer('bsl', [_,_]) -> true;
returns_integer('bsr', [_,_]) -> true;
returns_integer(byte_size, [_]) -> true;
returns_integer(ceil, [_]) -> true;
returns_integer('div', [_,_]) -> true;
returns_integer(floor, [_]) -> true;
returns_integer(length, [_]) -> true;
returns_integer('rem', [_,_]) -> true;
returns_integer('round', [_]) -> true;
returns_integer(size, [_]) -> true;
returns_integer(tuple_size, [_]) -> true;
returns_integer(trunc, [_]) -> true;
returns_integer(_, _) -> false.

%% update_types(Expr, Pattern, Sub) -> Sub'
%%  Update the type database.

-spec update_types(cerl:cerl(), [type_info()], sub()) -> sub().

update_types(Expr, Pat, #sub{t=Tdb0}=Sub) ->
    Tdb = update_types_1(Expr, Pat, Tdb0),
    Sub#sub{t=Tdb}.

update_types_1(#c_var{name=V}, Pat, Types) ->
    update_types_2(V, Pat, Types);
update_types_1(_, _, Types) -> Types.

update_types_2(V, [#c_tuple{}=P], Types) ->
    Types#{V=>P};
update_types_2(V, [#c_literal{val=Bool}], Types) when is_boolean(Bool) ->
    Types#{V=>bool};
update_types_2(V, [Type], Types) when is_atom(Type) ->
    Types#{V=>Type};
update_types_2(_, _, Types) -> Types.

%% kill_types(V, Tdb) -> Tdb'
%%  Kill any entries that references the variable,
%%  either in the key or in the value.

kill_types(V, Tdb) ->
    maps:from_list(kill_types2(V,maps:to_list(Tdb))).

kill_types2(V, [{V,_}|Tdb]) ->
    kill_types2(V, Tdb);
kill_types2(V, [{_,#c_tuple{}=Tuple}=Entry|Tdb]) ->
    case core_lib:is_var_used(V, Tuple) of
	false -> [Entry|kill_types2(V, Tdb)];
	true -> kill_types2(V, Tdb)
    end;
kill_types2(V, [{_,Atom}=Entry|Tdb]) when is_atom(Atom) ->
    [Entry|kill_types2(V, Tdb)];
kill_types2(_, []) -> [].

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
	    Line = get_line(Anno),
	    File = get_file(Anno),
	    Key = {?MODULE,warnings},
	    case get(Key) of
		[{File,[{Line,?MODULE,Term}]}|_] ->
		    ok;				%We already have
						%an identical warning.
		Ws ->
		    put(Key, [{File,[{Line,?MODULE,Term}]}|Ws])
	    end
    end.

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.

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

-type error() :: 'bad_unicode' | 'bin_argument_order'
	       | 'bin_left_var_used_in_guard' | 'bin_opt_alias'
	       | 'bin_partition' | 'bin_var_used' | 'bin_var_used_in_guard'
	       | 'embedded_binary_size' | 'nomatch_clause_type'
	       | 'nomatch_guard' | 'nomatch_shadow' | 'no_clause_match'
	       | 'orig_bin_var_used_in_guard' | 'result_ignored'
	       | 'useless_building'
	       | {'eval_failure', term()}
	       | {'no_effect', {'erlang',atom(),arity()}}
	       | {'nomatch_shadow', integer()}
	       | {'embedded_unit', _, _}.

-spec format_error(error()) -> nonempty_string().

format_error({eval_failure,Reason}) ->
    flatten(io_lib:format("this expression will fail with a '~p' exception", [Reason]));
format_error(embedded_binary_size) ->
    "binary construction will fail with a 'badarg' exception "
	"(field size for binary/bitstring greater than actual size)";
format_error({embedded_unit,Unit,Size}) ->
    M = io_lib:format("binary construction will fail with a 'badarg' exception "
		      "(size ~p cannot be evenly divided by unit ~p)", [Size,Unit]),
    flatten(M);
format_error(bad_unicode) ->
    "binary construction will fail with a 'badarg' exception "
	"(invalid Unicode code point in a utf8/utf16/utf32 segment)";
format_error({nomatch_shadow,Line}) ->
    M = io_lib:format("this clause cannot match because a previous clause at line ~p "
		      "always matches", [Line]),
    flatten(M);
format_error(nomatch_shadow) ->
    "this clause cannot match because a previous clause always matches";
format_error(nomatch_guard) ->
    "the guard for this clause evaluates to 'false'";
format_error({nomatch_bit_syntax_truncated,Signess,Val,Sz}) ->
    S = case Signess of
	    signed -> "a 'signed'";
	    unsigned -> "an 'unsigned'"
	end,
    F = "this clause cannot match because the value ~P"
	" will not fit in ~s binary segment of size ~p",
    flatten(io_lib:format(F, [Val,10,S,Sz]));
format_error({nomatch_bit_syntax_unsigned,Val}) ->
    F = "this clause cannot match because the negative value ~P"
	" will never match the value of an 'unsigned' binary segment",
    flatten(io_lib:format(F, [Val,10]));
format_error({nomatch_bit_syntax_size,Sz}) ->
    F = "this clause cannot match because '~P' is not a valid size for a binary segment",
    flatten(io_lib:format(F, [Sz,10]));
format_error({nomatch_bit_syntax_type,Val,Type}) ->
    F = "this clause cannot match because '~P' is not of the"
	" expected type '~p'",
    flatten(io_lib:format(F, [Val,10,Type]));
format_error({nomatch_bit_syntax_unicode,Val}) ->
    F = "this clause cannot match because the value ~p"
	" is not a valid Unicode code point",
    flatten(io_lib:format(F, [Val]));
format_error(no_clause_match) ->
    "no clause will ever match";
format_error(nomatch_clause_type) ->
    "this clause cannot match because of different types/sizes";
format_error({no_effect,{erlang,F,A}}) ->
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
format_error(result_ignored) ->
    "the result of the expression is ignored "
	"(suppress the warning by assigning the expression to the _ variable)";
format_error(invalid_call) ->
    "invalid function call";
format_error(useless_building) ->
    "a term is constructed, but never used".

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
	    io:format("~p\n", [ordsets:from_list(cerl_sets:to_list(Scope))]),
	    false
    end.

is_subset_of_scope([V|Vs], Scope) ->
    cerl_sets:is_element(V, Scope) andalso is_subset_of_scope(Vs, Scope);
is_subset_of_scope([], _) -> true.

-endif.

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2010. All Rights Reserved.
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
		reverse/1,reverse/2,member/2,nth/2,flatten/1]).

-import(cerl, [ann_c_cons/3,ann_c_tuple/2]).

-include("core_parse.hrl").

%%-define(DEBUG, 1).

-ifdef(DEBUG).
-define(ASSERT(E),
	case E of
	    true -> ok;
	    false ->
		io:format("~p, line ~p: assertion failed\n", [?MODULE,?LINE]),
		exit(assertion_failed)
	end).
-else.
-define(ASSERT(E), ignore).
-endif.

%% Variable value info.
-record(sub, {v=[],				%Variable substitutions
	      s=[],				%Variables in scope
	      t=[],				%Types
	      in_guard=false}).			%In guard or not.

-spec module(cerl:c_module(), [compile:option()]) ->
	{'ok', cerl:c_module(), [_]}.

module(#c_module{defs=Ds0}=Mod, Opts) ->
    put(bin_opt_info, member(bin_opt_info, Opts)),
    put(no_inline_list_funcs, not member(inline_list_funcs, Opts)),
    case get(new_var_num) of
	undefined -> put(new_var_num, 0);
	_ -> ok
    end,
    init_warnings(),
    Ds1 = [function_1(D) || D <- Ds0],
    erase(no_inline_list_funcs),
    erase(bin_opt_info),
    {ok,Mod#c_module{defs=Ds1},get_warnings()}.

function_1({#c_var{name={F,Arity}}=Name,B0}) ->
    try
	B = expr(B0, value, sub_new()),			%This must be a fun!
	{Name,B}
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [F,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

%% body(Expr, Sub) -> Expr.
%% body(Expr, Context, Sub) -> Expr.
%%  No special handling of anything except values.

body(Body, Sub) ->
    body(Body, value, Sub).

body(#c_values{anno=A,es=Es0}, Ctxt, Sub) ->
    Es1 = expr_list(Es0, Ctxt, Sub),
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
    case {Arg,Body} of
	{#c_call{},#c_literal{val=false}} ->
	    %% We have sequence consisting of a call (evaluted
	    %% for a possible exception only), followed by 'false'.
	    %% Since the sequence is inside a try block that will
	    %% default to 'false' if any exception occurs, not
	    %% evalutating the call will not change the behaviour
	    %% of the guard.
	    Body;
	{_,_} ->
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
	    expr(make_effect_seq([H1,T1], Sub), Ctxt, Sub);
	value ->
	    ann_c_cons(Anno, H1, T1)
    end;
expr(#c_tuple{anno=Anno,es=Es0}=Tuple, Ctxt, Sub) ->
    Es = expr_list(Es0, Ctxt, Sub),
    case Ctxt of
	effect ->
	    add_warning(Tuple, useless_building),
	    expr(make_effect_seq(Es, Sub), Ctxt, Sub);
	value ->
	    ann_c_tuple(Anno, Es)
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
    {Vs1,Sub1} = pattern_list(Vs0, Sub0),
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
	    case is_safe_simple(Arg, Sub) of
		true -> B1;
		false -> Seq0#c_seq{arg=Arg,body=B1}
	    end
    end;
expr(#c_let{}=Let, Ctxt, Sub) ->
    case simplify_let(Let, Sub) of
	impossible ->
	    %% The argument for the let is "simple", i.e. has no
	    %% complex structures such as let or seq that can be entered.
	    ?ASSERT(verify_scope(Let, Sub)),
	    opt_simple_let(Let, Ctxt, Sub);
	Expr ->
	    %% The let body was successfully moved into the let argument.
	    %% Now recursively re-process the new expression.
	    expr(Expr, Ctxt, sub_new_preserve_types(Sub))
    end;
expr(#c_letrec{defs=Fs0,body=B0}=Letrec, Ctxt, Sub) ->
    Fs1 = map(fun ({Name,Fb}) ->
		      {Name,expr(Fb, {letrec,Ctxt}, Sub)}
	      end, Fs0),
    B1 = body(B0, value, Sub),
    Letrec#c_letrec{defs=Fs1,body=B1};
expr(#c_case{}=Case0, Ctxt, Sub) ->
    case opt_bool_case(Case0) of
	#c_case{arg=Arg0,clauses=Cs0}=Case1 ->
	    Arg1 = body(Arg0, value, Sub),
	    {Arg2,Cs1} = case_opt(Arg1, Cs0),
	    Cs2 = clauses(Arg2, Cs1, Case1, Ctxt, Sub),
	    Case = eval_case(Case1#c_case{arg=Arg2,clauses=Cs2}, Sub),
	    bsm_an(Case);
	Other ->
	    expr(Other, Ctxt, Sub)
    end;
expr(#c_receive{clauses=Cs0,timeout=T0,action=A0}=Recv, Ctxt, Sub) ->
    Cs1 = clauses(#c_var{name='_'}, Cs0, Recv, Ctxt, Sub), %This is all we know
    T1 = expr(T0, value, Sub),
    A1 = body(A0, Ctxt, Sub),
    Recv#c_receive{clauses=Cs1,timeout=T1,action=A1};
expr(#c_apply{op=Op0,args=As0}=App, _, Sub) ->
    Op1 = expr(Op0, value, Sub),
    As1 = expr_list(As0, value, Sub),
    App#c_apply{op=Op1,args=As1};
expr(#c_call{module=M0,name=N0}=Call0, Ctxt, Sub) ->
    M1 = expr(M0, value, Sub),
    N1 = expr(N0, value, Sub),
    Call = Call0#c_call{module=M1,name=N1},
    case useless_call(Ctxt, Call) of
	no -> call(Call, M1, N1, Sub);
	{yes,Seq} -> expr(Seq, Ctxt, Sub)
    end;
expr(#c_primop{args=As0}=Prim, _, Sub) ->
    As1 = expr_list(As0, value, Sub),
    Prim#c_primop{args=As1};
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
    {Vs1,Sub1} = pattern_list(Vs0, Sub0),
    B1 = body(B0, value, Sub1),
    case is_safe_simple(E1, Sub0) of
	true ->
	    expr(#c_let{anno=A,vars=Vs1,arg=E1,body=B1}, value, Sub0);
	false ->
	    {Evs1,Sub2} = pattern_list(Evs0, Sub0),
	    H1 = body(H0, value, Sub2),
	    Try#c_try{arg=E1,vars=Vs1,body=B1,evars=Evs1,handler=H1}
    end.

expr_list(Es, Ctxt, Sub) ->
    [expr(E, Ctxt, Sub) || E <- Es].

bitstr_list(Es, Sub) ->
    [bitstr(E, Sub) || E <- Es].

bitstr(#c_bitstr{val=Val,size=Size}=BinSeg, Sub) ->
    BinSeg#c_bitstr{val=expr(Val, Sub),size=expr(Size, value, Sub)}.

%% is_safe_simple(Expr, Sub) -> true | false.
%%  A safe simple cannot fail with badarg and is safe to use
%%  in a guard.
%%
%%  Currently, we don't attempt to check binaries because they
%%  are difficult to check.

is_safe_simple(#c_var{}, _) -> true;
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
	    all(fun(#c_var{name=V}) -> is_boolean_type(V, Sub);
		   (#c_literal{val=Lit}) -> is_boolean(Lit);
		   (_) -> false
		end, Args);
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
	    #c_call{module=#c_literal{val=erlang},
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

useless_call(effect, #c_call{anno=Anno,
			     module=#c_literal{val=Mod},
			     name=#c_literal{val=Name},
			     args=Args}=Call) ->
    A = length(Args),
    case erl_bifs:is_safe(Mod, Name, A) of
	false ->
	    case erl_bifs:is_pure(Mod, Name, A) of
		true ->
		    case member(result_not_wanted, Anno) of
			false ->
			    add_warning(Call, result_ignored);
			true ->
			    ok
		    end;
		false ->
		    ok
	    end,
	    no;
	true ->
	    add_warning(Call, {no_effect,{Mod,Name,A}}),
	    {yes,make_effect_seq(Args, sub_new())}
    end;
useless_call(_, _) -> no.

%% make_effect_seq([Expr], Sub) -> #c_seq{}|void()
%%  Convert a list of epressions evaluated in effect context to a chain of
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
 	    call_0(Call, M0, N0, As, Sub);
  	false ->
  	    call_1(Call, M, N, As, Sub)
      end;
call(#c_call{args=As}=Call, M, N, Sub) ->
    call_0(Call, M, N, As, Sub).

call_0(Call, M, N, As0, Sub) ->
    As1 = expr_list(As0, value, Sub),
    fold_call(Call#c_call{args=As1}, M, N, As1, Sub).

%% We inline some very common higher order list operations.
%% We use the same evaluation order as the library function.

call_1(_Call, lists, all, [Arg1,Arg2], Sub) ->
    Loop = #c_var{name={'lists^all',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Err1 = #c_tuple{es=[#c_literal{val='case_clause'}, X]},
    CC1 = #c_clause{pats=[#c_literal{val=true}], guard=#c_literal{val=true},
		    body=#c_apply{op=Loop, args=[Xs]}},
    CC2 = #c_clause{pats=[#c_literal{val=false}], guard=#c_literal{val=true},
		    body=#c_literal{val=false}},
    CC3 = #c_clause{pats=[X], guard=#c_literal{val=true},
		    body=#c_primop{name=#c_literal{val='match_fail'},
				   args=[Err1]}},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_case{arg=#c_apply{op=F, args=[X]},
				clauses = [CC1, CC2, CC3]}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=true}},
    Err2 = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err2]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, any, [Arg1,Arg2], Sub) ->
    Loop = #c_var{name={'lists^any',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Err1 = #c_tuple{es=[#c_literal{val='case_clause'}, X]},
    CC1 = #c_clause{pats=[#c_literal{val=true}], guard=#c_literal{val=true},
		    body=#c_literal{val=true}},
    CC2 = #c_clause{pats=[#c_literal{val=false}], guard=#c_literal{val=true},
		    body=#c_apply{op=Loop, args=[Xs]}},
    CC3 = #c_clause{pats=[X], guard=#c_literal{val=true},
		    body=#c_primop{name=#c_literal{val='match_fail'},
				   args=[Err1]}},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_case{arg=#c_apply{op=F, args=[X]},
				clauses = [CC1, CC2, CC3]}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=false}},
    Err2 = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err2]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, foreach, [Arg1,Arg2], Sub) ->
    Loop = #c_var{name={'lists^foreach',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_seq{arg=#c_apply{op=F, args=[X]},
			       body=#c_apply{op=Loop, args=[Xs]}}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=ok}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, map, [Arg1,Arg2], Sub) ->
    Loop = #c_var{name={'lists^map',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    H = #c_var{name='H'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_let{vars=[H], arg=#c_apply{op=F, args=[X]},
			       body=#c_cons{hd=H,
					    tl=#c_apply{op=Loop,
							args=[Xs]}}}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=[]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, flatmap, [Arg1,Arg2], Sub) ->
    Loop = #c_var{name={'lists^flatmap',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    H = #c_var{name='H'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_let{vars=[H],
			       arg=#c_apply{op=F, args=[X]},
			       body=#c_call{module=#c_literal{val=erlang},
					    name=#c_literal{val='++'},
					    args=[H,
						  #c_apply{op=Loop,
							   args=[Xs]}]}}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=[]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, filter, [Arg1,Arg2], Sub) ->
    Loop = #c_var{name={'lists^filter',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    B = #c_var{name='B'},
    Err1 = #c_tuple{es=[#c_literal{val='case_clause'}, X]},
    CC1 = #c_clause{pats=[#c_literal{val=true}], guard=#c_literal{val=true},
		    body=#c_cons{hd=X, tl=Xs}},
    CC2 = #c_clause{pats=[#c_literal{val=false}], guard=#c_literal{val=true},
		    body=Xs},
    CC3 = #c_clause{pats=[X], guard=#c_literal{val=true},
		    body=#c_primop{name=#c_literal{val='match_fail'},
				   args=[Err1]}},
    Case = #c_case{arg=B, clauses = [CC1, CC2, CC3]},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_let{vars=[B],
			       arg=#c_apply{op=F, args=[X]},
			       body=#c_let{vars=[Xs],
					   arg=#c_apply{op=Loop,
							args=[Xs]},
					   body=Case}}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=[]}},
    Err2 = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err2]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
    Sub);
call_1(_Call, lists, foldl, [Arg1,Arg2,Arg3], Sub) ->
    Loop = #c_var{name={'lists^foldl',2}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    A = #c_var{name='A'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_apply{op=Loop,
				 args=[Xs, #c_apply{op=F, args=[X, A]}]}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true}, body=A},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, A],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, A, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L, A]}}},
	 Sub);
call_1(_Call, lists, foldr, [Arg1,Arg2,Arg3], Sub) ->
    Loop = #c_var{name={'lists^foldr',2}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    A = #c_var{name='A'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_apply{op=F, args=[X, #c_apply{op=Loop,
							 args=[Xs, A]}]}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true}, body=A},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, A],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, A, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L, A]}}},
	 Sub);
call_1(_Call, lists, mapfoldl, [Arg1,Arg2,Arg3], Sub) ->
    Loop = #c_var{name={'lists^mapfoldl',2}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Avar = #c_var{name='A'},
    Match =
	fun (A, P, E) ->
		C1 = #c_clause{pats=[P], guard=#c_literal{val=true}, body=E},
		Err = #c_tuple{es=[#c_literal{val='badmatch'}, X]},
		C2 = #c_clause{pats=[X], guard=#c_literal{val=true},
			       body=#c_primop{name=#c_literal{val='match_fail'},
					      args=[Err]}},
		#c_case{arg=A, clauses=[C1, C2]}
	end,
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=Match(#c_apply{op=F, args=[X, Avar]},
			      #c_tuple{es=[X, Avar]},
%%% Tuple passing version
			      Match(#c_apply{op=Loop, args=[Xs, Avar]},
				    #c_tuple{es=[Xs, Avar]},
				    #c_tuple{es=[#c_cons{hd=X, tl=Xs}, Avar]})
%%% Multiple-value version
%%% 			      #c_let{vars=[Xs,A],
%%% 				     %% The tuple here will be optimised
%%% 				     %% away later; no worries.
%%% 				     arg=#c_apply{op=Loop, args=[Xs, A]},
%%% 				     body=#c_values{es=[#c_cons{hd=X, tl=Xs},
%%% 							A]}}
			     )},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
%%% Tuple passing version
		   body=#c_tuple{es=[#c_literal{val=[]}, Avar]}},
%%% Multiple-value version
%%% 		   body=#c_values{es=[#c_literal{val=[]}, A]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, Avar],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, Avar, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[{Loop,Fun}],
%%% Tuple passing version
			       body=#c_apply{op=Loop, args=[L, Avar]}}},
%%% Multiple-value version
%%% 			       body=#c_let{vars=[Xs, A],
%%% 					   arg=#c_apply{op=Loop,
%%% 							args=[L, A]},
%%% 					   body=#c_tuple{es=[Xs, A]}}}},
	 Sub);
call_1(_Call, lists, mapfoldr, [Arg1,Arg2,Arg3], Sub) ->
    Loop = #c_var{name={'lists^mapfoldr',2}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Avar = #c_var{name='A'},
    Match =
	fun (A, P, E) ->
		C1 = #c_clause{pats=[P], guard=#c_literal{val=true}, body=E},
		Err = #c_tuple{es=[#c_literal{val='badmatch'}, X]},
		C2 = #c_clause{pats=[X], guard=#c_literal{val=true},
			       body=#c_primop{name=#c_literal{val='match_fail'},
					      args=[Err]}},
		#c_case{arg=A, clauses=[C1, C2]}
	end,
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
%%% Tuple passing version
		   body=Match(#c_apply{op=Loop, args=[Xs, Avar]},
			      #c_tuple{es=[Xs, Avar]},
			      Match(#c_apply{op=F, args=[X, Avar]},
				    #c_tuple{es=[X, Avar]},
				    #c_tuple{es=[#c_cons{hd=X, tl=Xs}, Avar]}))
%%% Multiple-value version
%%% 		   body=#c_let{vars=[Xs,A],
%%% 			       %% The tuple will be optimised away
%%% 			       arg=#c_apply{op=Loop, args=[Xs, A]},
%%% 			       body=Match(#c_apply{op=F, args=[X, A]},
%%% 					  #c_tuple{es=[X, A]},
%%% 					  #c_values{es=[#c_cons{hd=X, tl=Xs},
%%% 						        A]})}
		  },
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
%%% Tuple passing version
		   body=#c_tuple{es=[#c_literal{val=[]}, Avar]}},
%%% Multiple-value version
%%% 		   body=#c_values{es=[#c_literal{val=[]}, A]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, Avar],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, Avar, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[{Loop,Fun}],
%%% Tuple passing version
 			       body=#c_apply{op=Loop, args=[L, Avar]}}},
%%% Multiple-value version
%%% 			       body=#c_let{vars=[Xs, A],
%%% 					   arg=#c_apply{op=Loop,
%%% 							args=[L, A]},
%%% 					   body=#c_tuple{es=[Xs, A]}}}},
	 Sub);
call_1(#c_call{module=M, name=N}=Call, _, _, As, Sub) ->
    call_0(Call, M, N, As, Sub).

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

fold_call_2(Call, Module, Name, Args0, Sub) ->
    try
	Args = [core_lib:literal_value(A) || A <- Args0],
	try apply(Module, Name, Args) of
	    Val ->
		case cerl:is_literal_term(Val) of
		    true ->
			#c_literal{val=Val};
		    false ->
			%% Successful evaluation, but it was not
			%% possible to express the computed value as a literal.
			Call
		end
	catch
	    error:Reason ->
		%% Evaluation of the function failed. Warn and replace
		%% the call with a call to erlang:error/1.
		eval_failure(Call, Reason)
	end
    catch
	error:_ ->
	    %% There was at least one non-literal argument.
	    fold_non_lit_args(Call, Module, Name, Args0, Sub)
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
    #c_literal{anno=core_lib:get_anno(Call),val=Bool};
eval_rel_op(Call, '=:=', [#c_var{name=V}=Var,#c_literal{val=true}], Sub) ->
    %% BoolVar =:= true  ==>  BoolVar
    case is_boolean_type(V, Sub) of
	true -> Var;
	false -> Call
    end;
eval_rel_op(Call, '==', Ops, _Sub) ->
    case is_exact_eq_ok(Ops) of
	true ->
	    Name = #c_literal{anno=core_lib:get_anno(Call),val='=:='},
	    Call#c_call{name=Name};
	false ->
	    Call
    end;
eval_rel_op(Call, '/=', Ops, _Sub) ->
    case is_exact_eq_ok(Ops) of
	true ->
	    Name = #c_literal{anno=core_lib:get_anno(Call),val='=/='},
	    Call#c_call{name=Name};
	false ->
	    Call
    end;
eval_rel_op(Call, _, _, _) -> Call.

is_exact_eq_ok([#c_literal{val=Lit}|_]) ->
    is_non_numeric(Lit);
is_exact_eq_ok([_|T]) ->
    is_exact_eq_ok(T);
is_exact_eq_ok([]) -> false.

is_non_numeric([H|T]) ->
    is_non_numeric(H) andalso is_non_numeric(T);
is_non_numeric(Tuple) when is_tuple(Tuple) ->
    is_non_numeric_tuple(Tuple, tuple_size(Tuple));
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
eval_bool_op(Call, 'and', [#c_literal{val=true},#c_var{name=V}=Res], Sub) ->
    case is_boolean_type(V, Sub) of
	true -> Res;
	false-> Call
    end;
eval_bool_op(Call, 'and', [#c_var{name=V}=Res,#c_literal{val=true}], Sub) ->
    case is_boolean_type(V, Sub) of
	true -> Res;
	false-> Call
    end;
eval_bool_op(Call, 'and', [#c_literal{val=false}=Res,#c_var{name=V}], Sub) ->
    case is_boolean_type(V, Sub) of
	true -> Res;
	false-> Call
    end;
eval_bool_op(Call, 'and', [#c_var{name=V},#c_literal{val=false}=Res], Sub) ->
    case is_boolean_type(V, Sub) of
	true -> Res;
	false-> Call
    end;
eval_bool_op(Call, _, _, _) -> Call.

%% Evaluate is_boolean/1 using type information.
eval_is_boolean(Call, #c_var{name=V}, Sub) ->
    case is_boolean_type(V, Sub) of
	true -> #c_literal{val=true};
	false -> Call
    end;
eval_is_boolean(_, #c_cons{}, _) ->
    #c_literal{val=false};
eval_is_boolean(_, #c_tuple{}, _) ->
    #c_literal{val=false};
eval_is_boolean(Call, _, _) ->
    Call.

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
eval_element(Call, #c_literal{val=Pos}, #c_tuple{es=Es}, _Types) when is_integer(Pos) ->
    if
	1 =< Pos, Pos =< length(Es) ->
	    lists:nth(Pos, Es);
	true ->
	    eval_failure(Call, badarg)
    end;
eval_element(Call, #c_literal{val=Pos}, #c_var{name=V}, Types)
  when is_integer(Pos) ->
    case orddict:find(V, Types#sub.t) of
	{ok,#c_tuple{es=Elements}} ->
	    if
		1 =< Pos, Pos =< length(Elements) ->
		    case lists:nth(Pos, Elements) of
			#c_alias{var=Alias} -> Alias;
			Res -> Res
		    end;
		true ->
		    eval_failure(Call, badarg)
	    end;
	error ->
	    Call
    end;
eval_element(Call, Pos, Tuple, _Types) ->
    case is_not_integer(Pos) orelse is_not_tuple(Tuple) of
	true ->
	    eval_failure(Call, badarg);
	false ->
	    Call
    end.

%% eval_is_record(Call, Var, Tag, Size, Types) -> Val.
%%  Evaluates is_record/3 using type information.
%%
eval_is_record(Call, #c_var{name=V}, #c_literal{val=NeededTag}=Lit,
	       #c_literal{val=Size}, Types) ->
    case orddict:find(V, Types#sub.t) of
	{ok,#c_tuple{es=[#c_literal{val=Tag}|_]=Es}} ->
	    Lit#c_literal{val=Tag =:= NeededTag andalso
			  length(Es) =:= Size};
	_ ->
	    Call
    end;
eval_is_record(Call, _, _, _, _) -> Call.

%% is_not_integer(Core) -> true | false.
%%  Returns true if Core is definitely not an integer.

is_not_integer(#c_literal{val=Val}) when not is_integer(Val) -> true;
is_not_integer(#c_tuple{}) -> true;
is_not_integer(#c_cons{}) -> true;
is_not_integer(_) -> false.

%% is_not_tuple(Core) -> true | false.
%%  Returns true if Core is definitely not a tuple.

is_not_tuple(#c_literal{val=Val}) when not is_tuple(Val) -> true;
is_not_tuple(#c_cons{}) -> true;
is_not_tuple(_) -> false.

%% eval_setelement(Call, Pos, Tuple, NewVal) -> Core.
%%  Evaluates setelement/3 if position Pos is an integer
%%  the shape of the tuple Tuple is known.
%%
eval_setelement(Call, Pos, Tuple, NewVal) ->
    try
	eval_setelement_1(Pos, Tuple, NewVal)
    catch
	error:_ ->
	    Call
    end.

eval_setelement_1(#c_literal{val=Pos}, #c_tuple{anno=A,es=Es}, NewVal)
  when is_integer(Pos) ->
    ann_c_tuple(A, eval_setelement_2(Pos, Es, NewVal));
eval_setelement_1(#c_literal{val=Pos}, #c_literal{anno=A,val=Es0}, NewVal)
  when is_integer(Pos) ->
    Es = [#c_literal{anno=A,val=E} || E <- tuple_to_list(Es0)],
    ann_c_tuple(A, eval_setelement_2(Pos, Es, NewVal)).

eval_setelement_2(1, [_|T], NewVal) ->
    [NewVal|T];
eval_setelement_2(Pos, [H|T], NewVal) when Pos > 1 ->
    [H|eval_setelement_2(Pos-1, T, NewVal)].

%% eval_failure(Call, Reason) -> Core.
%%  Warn for a call that will fail and replace the call with
%%  a call to erlang:error(Reason).
%%
eval_failure(Call, Reason) ->
    add_warning(Call, {eval_failure,Reason}),
    #c_call{module=#c_literal{val=erlang},
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

clause(#c_clause{pats=Ps0,guard=G0,body=B0}=Cl, Cexpr, Ctxt, Sub0) ->
    {Ps1,Sub1} = pattern_list(Ps0, Sub0),
    Sub2 = update_types(Cexpr, Ps1, Sub1),
    GSub = case {Cexpr,Ps1} of
	       {#c_var{name='_'},_} ->
		   %% In a 'receive', Cexpr is the variable '_', which represents the
		   %% message being matched. We must NOT do any extra substiutions.
		   Sub2;
	       {#c_var{},[#c_var{}=Var]} ->
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
		   sub_set_var(Var, Cexpr, Sub2);
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
    {Vs1,Sub1} = pattern_list(Vs0, Sub0),
    {Vs2,As1,Ss} = let_substs_1(Vs1, As0, Sub1),
    Sub2 = scope_add([V || #c_var{name=V} <- Vs2], Sub1),
    {Vs2,As1,
     foldl(fun ({V,S}, Sub) -> sub_set_name(V, S, Sub) end, Sub2, Ss)}.

let_substs_1(Vs, #c_values{es=As}, Sub) ->
    let_subst_list(Vs, As, Sub);
let_substs_1([V], A, Sub) -> let_subst_list([V], [A], Sub);
let_substs_1(Vs, A, _) -> {Vs,A,[]}.

let_subst_list([V|Vs0], [A|As0], Sub) ->
    {Vs1,As1,Ss} = let_subst_list(Vs0, As0, Sub),
    case is_subst(A) of
	true -> {Vs1,As1,sub_subst_var(V, A, Sub) ++ Ss};
	false -> {[V|Vs1],[A|As1],Ss}
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

pattern(#c_var{name=V0}=Pat, Isub, Osub) ->
    case sub_is_val(Pat, Isub) of
	true ->
	    V1 = make_var_name(),
	    Pat1 = #c_var{name=V1},
	    {Pat1,sub_set_var(Pat, Pat1, scope_add([V1], Osub))};
	false ->
	    {Pat,sub_del_var(Pat, scope_add([V0], Osub))}
    end;
pattern(#c_literal{}=Pat, _, Osub) -> {Pat,Osub};
pattern(#c_cons{anno=Anno,hd=H0,tl=T0}, Isub, Osub0) ->
    {H1,Osub1} = pattern(H0, Isub, Osub0),
    {T1,Osub2} = pattern(T0, Isub, Osub1),
    {ann_c_cons(Anno, H1, T1),Osub2};
pattern(#c_tuple{anno=Anno,es=Es0}, Isub, Osub0) ->
    {Es1,Osub1} = pattern_list(Es0, Isub, Osub0),
    {ann_c_tuple(Anno, Es1),Osub1};
pattern(#c_binary{segments=V0}=Pat, Isub, Osub0) ->
    {V1,Osub1} = bin_pattern_list(V0, Isub, Osub0),
    {Pat#c_binary{segments=V1},Osub1};
pattern(#c_alias{var=V0,pat=P0}=Pat, Isub, Osub0) ->
    {V1,Osub1} = pattern(V0, Isub, Osub0),
    {P1,Osub2} = pattern(P0, Isub, Osub1),
    Osub = update_types(V1, [P1], Osub2),
    {Pat#c_alias{var=V1,pat=P1},Osub}.

bin_pattern_list(Ps0, Isub, Osub0) ->
    {Ps,{_,Osub}} = mapfoldl(fun bin_pattern/2, {Isub,Osub0}, Ps0),
    {Ps,Osub}.

bin_pattern(#c_bitstr{val=E0,size=Size0}=Pat, {Isub0,Osub0}) ->
    Size1 = expr(Size0, Isub0),
    {E1,Osub} = pattern(E0, Isub0, Osub0),
    Isub = case E0 of
	       #c_var{} -> sub_set_var(E0, E1, Isub0);
	       _ -> Isub0
	   end,
    {Pat#c_bitstr{val=E1,size=Size1},{Isub,Osub}}.

pattern_list(Ps, Sub) -> pattern_list(Ps, Sub, Sub).

pattern_list(Ps0, Isub, Osub0) ->
    mapfoldl(fun (P, Osub) -> pattern(P, Isub, Osub) end, Osub0, Ps0).

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
%% sub_subst_scope(#sub{}) -> #sub{}
%%
%%  We use the variable name as key so as not have problems with
%%  annotations.  When adding a new substitute we fold substitute
%%  chains so we never have to search more than once.  Use orddict so
%%  we know the format.
%%
%%  sub_subst_scope/1 adds dummy substitutions for all variables
%%  in the scope in order to force renaming if variables in the
%%  scope occurs as pattern variables.

sub_new() -> #sub{v=orddict:new(),s=gb_trees:empty(),t=[]}.

sub_new(#sub{}=Sub) ->
    Sub#sub{v=orddict:new(),t=[]}.

sub_new_preserve_types(#sub{}=Sub) ->
    Sub#sub{v=orddict:new()}.

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
    Sub#sub{v=orddict:store(V, Val, S),s=gb_sets:add(V, Scope),t=Tdb}.

sub_del_var(#c_var{name=V}, #sub{v=S,t=Tdb}=Sub) ->
    Sub#sub{v=orddict:erase(V, S),t=kill_types(V, Tdb)}.

sub_subst_var(#c_var{name=V}, Val, #sub{v=S0}) ->
    %% Fold chained substitutions.
    [{V,Val}] ++ [ {K,Val} || {K,#c_var{name=V1}} <- S0, V1 =:= V].

sub_subst_scope(#sub{v=S0,s=Scope}=Sub) ->
    S = [{-1,#c_var{name=Sv}} || Sv <- gb_sets:to_list(Scope)]++S0,
    Sub#sub{v=S}.

sub_is_val(#c_var{name=V}, #sub{v=S}) ->
    v_is_value(V, S).

v_is_value(Var, Sub) ->
    any(fun ({_,#c_var{name=Val}}) when Val =:= Var -> true;
	    (_) -> false
	end, Sub).

%% clauses(E, [Clause], TopLevel, Context, Sub) -> [Clause].
%%  Trim the clauses by removing all clauses AFTER the first one which
%%  is guaranteed to match.  Also remove all trivially false clauses.

clauses(E, Cs0, TopLevel, Ctxt, Sub) ->
    Cs = clauses_1(E, Cs0, Ctxt, Sub),

    %% Here we want to warn if no clauses whatsoever will ever
    %% match, because that is probably a mistake.
    case all(fun is_compiler_generated/1, Cs) andalso
	any(fun(C) -> not is_compiler_generated(C) end, Cs0) of
	true ->
	    %% The original list of clauses did contain at least one
	    %% user-specified clause, but none of them will match.
	    %% That is probably a mistake.
	    add_warning(TopLevel, no_clause_match);
	false ->
	    %% Either there were user-specified clauses left in
	    %% the transformed clauses, or else none of the original
	    %% clauses were user-specified to begin with (as in 'andalso').
	    ok
    end,

    Cs.

clauses_1(E, [C0|Cs], Ctxt, Sub) ->
    #c_clause{pats=Ps,guard=G} = C1 = clause(C0, E, Ctxt, Sub),
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{E,Ps}]),
    case {will_match(E, Ps),will_succeed(G)} of
	{yes,yes} ->
	    Line = get_line(core_lib:get_anno(C1)),
	    case core_lib:is_literal(E) of
		false ->
		    shadow_warning(Cs, Line);
		true ->
		    %% If the case expression is a literal,
		    %% it is probably OK that some clauses don't match.
		    %% It is a probably some sort of debug macro.
		    ok
	    end,
	    [C1];				%Skip the rest
	{no,_Suc} ->
	    clauses_1(E, Cs, Ctxt, Sub);	%Skip this clause
	{_Mat,no} ->
	    add_warning(C1, nomatch_guard),
	    clauses_1(E, Cs, Ctxt, Sub);	%Skip this clause
	{_Mat,_Suc} ->
	    [C1|clauses_1(E, Cs, Ctxt, Sub)]
    end;
clauses_1(_, [], _, _) -> [].

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

%% will_match(Expr, [Pattern]) -> yes | maybe | no.
%%  Test if we know whether a match will succeed/fail or just don't
%%  know.  Be conservative.

will_match(#c_values{es=Es}, Ps) ->
    will_match_list(Es, Ps, yes);
will_match(E, [P]) ->
    will_match_1(E, P).

will_match_1(_E, #c_var{}) -> yes;		%Will always match
will_match_1(E, #c_alias{pat=P}) ->		%Pattern decides
    will_match_1(E, P);
will_match_1(#c_var{}, _P) -> maybe;
will_match_1(#c_tuple{es=Es}, #c_tuple{es=Ps}) ->
    will_match_list(Es, Ps, yes);
will_match_1(#c_literal{val=Lit}, P) ->
    will_match_lit(Lit, P);
will_match_1(_, _) -> maybe.

will_match_list([E|Es], [P|Ps], M) ->
    case will_match_1(E, P) of
	yes -> will_match_list(Es, Ps, M);
	maybe -> will_match_list(Es, Ps, maybe);
	no -> no
    end;
will_match_list([], [], M) -> M.

will_match_lit(Cons, #c_cons{hd=Hp,tl=Tp}) ->
    case Cons of
	[H|T] ->
	    case will_match_lit(H, Hp) of
		yes -> will_match_lit(T, Tp);
		Other -> Other
	    end;
	_ ->
	    no
    end;
will_match_lit(Tuple, #c_tuple{es=Es}) ->
    case is_tuple(Tuple) andalso tuple_size(Tuple) =:= length(Es) of
	true -> will_match_lit_list(tuple_to_list(Tuple), Es);
	false -> no
    end;
will_match_lit(Bin, #c_binary{}) ->
    case is_bitstring(Bin) of
	true -> maybe;
	false -> no
    end;
will_match_lit(_, #c_var{}) ->
    yes;
will_match_lit(Lit, #c_alias{pat=P}) ->
    will_match_lit(Lit, P);
will_match_lit(Lit1, #c_literal{val=Lit2}) ->
    case Lit1 =:= Lit2 of
	true -> yes;
	false -> no
    end.

will_match_lit_list([H|T], [P|Ps]) ->
    case will_match_lit(H, P) of
	yes -> will_match_lit_list(T, Ps);
	Other -> Other
    end;
will_match_lit_list([], []) -> yes.

%% opt_bool_case(CoreExpr) - CoreExpr'.
%%  Do various optimizations to case statement that has a
%%  boolean case expression.
%%
%%  We start with some simple optimizations and normalization
%%  to facilitate later optimizations.
%%
%%  If the case expression can only return a boolean
%%  (or fail), we can remove any clause that cannot
%%  possibly match 'true' or 'false'. Also, any clause
%%  following both 'true' and 'false' clause can
%%  be removed. If successful, we will end up this:
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
%%
opt_bool_case(#c_case{arg=Arg}=Case0) ->
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
    end;
opt_bool_case(Core) -> Core.

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
			    guard=#c_literal{val=true},
			    body=B}=C0|Cs], SeenT, SeenF) ->
    case is_boolean(Lit) of
	false ->
	    %% Not a boolean - this clause can't match.
	    add_warning(C0, nomatch_clause_type),
	    opt_bool_clauses(Cs, SeenT, SeenF);
	true ->
	    %% This clause will match.
	    C = C0#c_clause{body=opt_bool_case(B)},
	    case Lit of
		false -> [C|opt_bool_clauses(Cs, SeenT, true)];
		true -> [C|opt_bool_clauses(Cs, true, SeenF)]
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
%%
%%  We add the extra match-all clause at the end only if Expr is
%%  not guaranteed to evaluate to a boolean.

opt_bool_not(#c_case{arg=Arg,clauses=Cs0}=Case0) ->
    case Arg of
	#c_call{module=#c_literal{val=erlang},
 		name=#c_literal{val='not'},
 		args=[Expr]} ->
	    Cs = opt_bool_not(Expr, Cs0),
	    Case = Case0#c_case{arg=Expr,clauses=Cs},
	    opt_bool_not(Case);
	_ ->
	    opt_bool_case_redundant(Case0)
    end.

opt_bool_not(Expr, Cs) ->
    Tail = case is_bool_expr(Expr) of
	       false ->
		   [#c_clause{anno=[compiler_generated],
			      pats=[#c_var{name=cor_variable}],
			      guard=#c_literal{val=true},
			      body=#c_call{module=#c_literal{val=erlang},
					   name=#c_literal{val=error},
					   args=[#c_literal{val=badarg}]}}];
	       true -> []
	   end,
    [opt_bool_not_invert(C) || C <- Cs] ++ Tail.

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
	    Case#c_case{arg=#c_values{anno=core_lib:get_anno(Arg),es=[]},
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

eval_case(#c_case{arg=#c_var{name=V},
		  clauses=[#c_clause{pats=[P],guard=G,body=B}|_]}=Case,
	  #sub{t=Tdb}=Sub) ->
    case orddict:find(V, Tdb) of
	{ok,Type} ->
	    case {will_match_type(P, Type),will_succeed(G)} of
		{yes,yes} ->
		    {Ps,Es} = remove_non_vars(P, Type),
		    expr(#c_let{vars=Ps,arg=#c_values{es=Es},body=B},
			 sub_new(Sub));
		{_,_} ->
		    eval_case_1(Case, Sub)
	    end;
	error -> eval_case_1(Case, Sub)
    end;
eval_case(Case, Sub) -> eval_case_1(Case, Sub).

eval_case_1(#c_case{arg=E,clauses=[#c_clause{pats=Ps,body=B}]}=Case, Sub) ->
    case is_var_pat(Ps) of
	true -> expr(#c_let{vars=Ps,arg=E,body=B}, sub_new(Sub));
	false -> eval_case_2(E, Ps, B, Case)
    end;
eval_case_1(Case, _) -> Case.

eval_case_2(E, [P], B, Case) ->
    %% Recall that there is only one clause and that it is guaranteed to match.
    %%   If E and P are literals, they must be the same literal and the body
    %% can be used directly as there are no variables that need to be bound.
    %%   Otherwise, P could be an alias meaning that two or more variables
    %% would be bound to E. We don't bother to optimize that case as it
    %% is rather uncommon.
    case core_lib:is_literal(E) andalso core_lib:is_literal(P) of
	false -> Case;
	true -> B
    end;
eval_case_2(_, _, _, Case) -> Case.

is_var_pat(Ps) ->
    all(fun (#c_var{}) -> true;
	    (_Pat) -> false
	end, Ps).

will_match_type(#c_tuple{es=Es}, #c_tuple{es=Ps}) ->
    will_match_list_type(Es, Ps);
will_match_type(#c_literal{val=Atom}, #c_literal{val=Atom}) -> yes;
will_match_type(#c_var{}, #c_var{}) -> yes;
will_match_type(#c_var{}, #c_alias{}) -> yes;
will_match_type(_, _) -> no.

will_match_list_type([E|Es], [P|Ps]) ->
    case will_match_type(E, P) of
	yes -> will_match_list_type(Es, Ps);
	no -> no
    end;
will_match_list_type([], []) -> yes;
will_match_list_type(_, _) -> no.		%Different length

remove_non_vars(Ps0, Es0) ->
    {Ps,Es} = remove_non_vars(Ps0, Es0, [], []),
    {reverse(Ps),reverse(Es)}.

remove_non_vars(#c_tuple{es=Ps}, #c_tuple{es=Es}, Pacc, Eacc) ->
    remove_non_vars_list(Ps, Es, Pacc, Eacc);
remove_non_vars(#c_var{}=Var, #c_alias{var=Evar}, Pacc, Eacc) ->
    {[Var|Pacc],[Evar|Eacc]};
remove_non_vars(#c_var{}=Var, #c_var{}=Evar, Pacc, Eacc) ->
    {[Var|Pacc],[Evar|Eacc]};
remove_non_vars(P, E, Pacc, Eacc) ->
    true = core_lib:is_literal(P) andalso core_lib:is_literal(E), %Assertion.
    {Pacc,Eacc}.

remove_non_vars_list([P|Ps], [E|Es], Pacc0, Eacc0) ->
    {Pacc,Eacc} = remove_non_vars(P, E, Pacc0, Eacc0),
    remove_non_vars_list(Ps, Es, Pacc, Eacc);
remove_non_vars_list([], [], Pacc, Eacc) ->
    {Pacc,Eacc}.

%% case_opt(CaseArg, [Clause]) -> {CaseArg,[Clause]}.
%%  Try and optimise case by avoid building a tuple in
%%  the case expression. Instead of building a tuple
%%  in the case expression, combine the elements into
%%  multiple "values". If a clause refers to the tuple
%%  in the case expression (that was not built), introduce
%%  a let into the guard and/or body to build the tuple.
%%
%%     case {Expr1,Expr2} of		 case <Expr1,Expr2> of
%%         {P1,P2} -> ...		     <P1,P2> -> ...
%%          .  	       	       	  ==>        .
%%          .				     .
%%          .				     .
%%         Var ->                            <Var1,Var2> ->
%%             ... Var ...                      let <Var> = {Var1,Var2}
%%                                                  in ... Var ...
%%          .                                 .
%%          .                                 .
%%          .				      .
%%     end.				 end.
%%
case_opt(#c_tuple{anno=A,es=Es}, Cs0) ->
    Cs1 = case_opt_cs(Cs0, length(Es)),
    {core_lib:set_anno(core_lib:make_values(Es), A),Cs1};
case_opt(Arg, Cs) -> {Arg,Cs}.

case_opt_cs([#c_clause{pats=Ps0,guard=G,body=B}=C|Cs], Arity) ->
    case case_tuple_pat(Ps0, Arity) of
	{ok,Ps1,Avs} ->
	    Flet = fun ({V,Pat}, Body) -> letify(V, Pat, Body) end,
	    [C#c_clause{pats=Ps1,
			guard=foldl(Flet, G, Avs),
			body=foldl(Flet, B, Avs)}|case_opt_cs(Cs, Arity)];
	error ->				%Can't match
	    add_warning(C, nomatch_clause_type),
	    case_opt_cs(Cs, Arity)
    end;
case_opt_cs([], _) -> [].

%% case_tuple_pat([Pattern], Arity) -> {ok,[Pattern],[{AliasVar,Pat}]} | error.

case_tuple_pat([#c_tuple{es=Ps}], Arity) when length(Ps) =:= Arity ->
    {ok,Ps,[]};
case_tuple_pat([#c_literal{val=T}], Arity) when tuple_size(T) =:= Arity ->
    Ps = [#c_literal{val=E} || E <- tuple_to_list(T)],
    {ok,Ps,[]};
case_tuple_pat([#c_var{anno=A}=V], Arity) ->
    Vars = make_vars(A, 1, Arity),
    {ok,Vars,[{V,#c_tuple{es=Vars}}]};
case_tuple_pat([#c_alias{var=V,pat=P}], Arity) ->
    case case_tuple_pat([P], Arity) of
	{ok,Ps,Avs} -> {ok,Ps,[{V,#c_tuple{es=unalias_pat_list(Ps)}}|Avs]};
	error -> error
    end;
case_tuple_pat(_, _) -> error.

%% unalias_pat(Pattern) -> Pattern.
%%  Remove all the aliases in a pattern but using the alias variables
%%  instead of the values.  We KNOW they will be bound.

unalias_pat(#c_alias{var=V}) -> V;
unalias_pat(#c_cons{anno=Anno,hd=H0,tl=T0}) ->
    H1 = unalias_pat(H0),
    T1 = unalias_pat(T0),
    ann_c_cons(Anno, H1, T1);
unalias_pat(#c_tuple{anno=Anno,es=Ps}) ->
    ann_c_tuple(Anno, unalias_pat_list(Ps));
unalias_pat(Atomic) -> Atomic.

unalias_pat_list(Ps) -> [unalias_pat(P) || P <- Ps].

make_vars(A, I, Max) when I =< Max ->
    [make_var(A)|make_vars(A, I+1, Max)];
make_vars(_, _, _) -> [].

make_var(A) ->
    #c_var{anno=A,name=make_var_name()}.

make_var_name() ->
    N = get(new_var_num),
    put(new_var_num, N+1),
    list_to_atom("fol"++integer_to_list(N)).

letify(#c_var{name=Vname}=Var, Val, Body) ->
    case core_lib:is_var_used(Vname, Body) of
	true ->
	    A = element(2, Body),
	    #c_let{anno=A,vars=[Var],arg=Val,body=Body};
	false -> Body
    end.

%% opt_case_in_let(LetExpr) -> LetExpr'

opt_case_in_let(#c_let{vars=Vs,arg=Arg,body=B}=Let) ->
    opt_case_in_let_0(Vs, Arg, B, Let).

opt_case_in_let_0([#c_var{name=V}], Arg,
		  #c_case{arg=#c_var{name=V},clauses=Cs}=Case, Let) ->
    case opt_case_in_let_1(V, Arg, Cs) of
	impossible ->
	    case is_simple_case_arg(Arg) andalso
		not core_lib:is_var_used(V, Case#c_case{arg=#c_literal{val=nil}}) of
		true ->
		    opt_bool_case(Case#c_case{arg=Arg});
		false ->
		    Let
	    end;
	Expr -> Expr
    end;
opt_case_in_let_0(_, _, _, Let) -> Let.

opt_case_in_let_1(V, Arg, Cs) ->
    try
	opt_case_in_let_2(V, Arg, Cs)
    catch
	_:_ -> impossible
    end.

opt_case_in_let_2(V, Arg0,
		  [#c_clause{pats=[#c_tuple{es=Es}],
			     guard=#c_literal{val=true},body=B}|_]) ->

    %%  In {V1,V2,...} = case E of P -> ... {Val1,Val2,...}; ... end.
    %%  avoid building tuples, by converting tuples to multiple values.
    %%  (The optimisation is not done if the built tuple is used or returned.)

    true = all(fun (#c_var{}) -> true;
		   (_) -> false end, Es),	%Only variables in tuple
    false = core_lib:is_var_used(V, B),		%Built tuple must not be used.
    Arg1 = tuple_to_values(Arg0, length(Es)),	%Might fail.
    #c_let{vars=Es,arg=Arg1,body=B};
opt_case_in_let_2(_, Arg, Cs) ->
    %% simplify_bool_case(Case0) -> Case
    %%  Remove unecessary cases like
    %%
    %%     case BoolExpr of
    %%       true -> true;
    %%       false -> false;
    %%       ....
    %%     end
    %%
    %%  where BoolExpr is an expression that can only return true
    %%  or false (or throw an exception).

    true = is_bool_case(Cs) andalso is_bool_expr(Arg),
    Arg.

is_bool_case([A,B|_]) ->
    (is_bool_clause(true, A) andalso is_bool_clause(false, B))
	orelse (is_bool_clause(false, A) andalso is_bool_clause(true, B)).

is_bool_clause(Bool, #c_clause{pats=[#c_literal{val=Bool}],
			       guard=#c_literal{val=true},
			       body=#c_literal{val=Bool}}) ->
    true;
is_bool_clause(_, _) -> false.

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
	      true -> update_types(V, [#c_literal{val=true}], Sub0);
	      false -> Sub0
	  end,
    is_bool_expr(B, Sub);
is_bool_expr(#c_let{body=B}, Sub) ->
    %% Binding of multiple variables.
    is_bool_expr(B, Sub);
is_bool_expr(#c_literal{val=Bool}, _) when is_boolean(Bool) ->
    true;
is_bool_expr(#c_var{name=V}, Sub) ->
    is_boolean_type(V, Sub);
is_bool_expr(_, _) -> false.

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
    is_safe_bool_expr_1(Core, Sub, gb_sets:empty()).

is_safe_bool_expr_1(#c_call{module=#c_literal{val=erlang},
			    name=#c_literal{val=is_record},
			    args=[_,_]},
		    _Sub, _BoolVars) ->
    %% The is_record/2 BIF is NOT allowed in guards.
    %%
    %% NOTE: Calls like is_record(Expr, LiteralTag), where LiteralTag
    %% is a literal atom referring to a defined record, have already
    %% been rewritten to is_record(Expr, LiteralTag, TupleSize).
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
		    is_safe_bool_expr_1(B, Sub, gb_sets:add(V, BoolVars));
		{false,_} ->
		    is_safe_bool_expr_1(B, Sub, BoolVars)
	    end;
	false -> false
    end;
is_safe_bool_expr_1(#c_literal{val=Val}, _Sub, _) ->
    is_boolean(Val);
is_safe_bool_expr_1(#c_var{name=V}, _Sub, BoolVars) ->
    gb_sets:is_element(V, BoolVars);
is_safe_bool_expr_1(_, _, _) -> false.

is_safe_bool_expr_list([C|Cs], Sub, BoolVars) ->
    case is_safe_bool_expr_1(C, Sub, BoolVars) of
	true -> is_safe_bool_expr_list(Cs, Sub, BoolVars);
	false -> false
    end;
is_safe_bool_expr_list([], _, _) -> true.

%% tuple_to_values(Expr, TupleArity) -> Expr'
%%  Convert tuples in return position of arity TupleArity to values.
%%  Throws an exception for constructs that are not handled.

tuple_to_values(#c_tuple{es=Es}, Arity) when length(Es) =:= Arity ->
    core_lib:make_values(Es);
tuple_to_values(#c_literal{val=Tuple}=Lit, Arity) when tuple_size(Tuple) =:= Arity ->
    Es = [Lit#c_literal{val=E} || E <- tuple_to_list(Tuple)],
    core_lib:make_values(Es);
tuple_to_values(#c_case{clauses=Cs0}=Case, Arity) ->
    Cs1 = [tuple_to_values(E, Arity) || E <- Cs0],
    Case#c_case{clauses=Cs1};
tuple_to_values(#c_seq{body=B0}=Seq, Arity) ->
    Seq#c_seq{body=tuple_to_values(B0, Arity)};
tuple_to_values(#c_let{body=B0}=Let, Arity) ->
    Let#c_let{body=tuple_to_values(B0, Arity)};
tuple_to_values(#c_receive{clauses=Cs0,timeout=Timeout,action=A0}=Rec, Arity) ->
    Cs = [tuple_to_values(E, Arity) || E <- Cs0],
    A = case Timeout of
	    #c_literal{val=infinity} -> A0;
	    _ -> tuple_to_values(A0, Arity)
	end,
    Rec#c_receive{clauses=Cs,action=A};
tuple_to_values(#c_clause{body=B0}=Clause, Arity) ->
    B = tuple_to_values(B0, Arity),
    Clause#c_clause{body=B};
tuple_to_values(Expr, _) ->
    case will_fail(Expr) of
	true -> Expr;
	false -> erlang:error({not_handled,Expr})
    end.

%% simplify_let(Let, Sub) -> Expr | impossible
%%  If the argument part of an let contains a complex expression, such
%%  as a let or a sequence, move the original let body into the complex
%%  expression.

simplify_let(#c_let{arg=Arg0}=Let0, Sub) ->
    Arg = opt_bool_case(Arg0),
    Let = Let0#c_let{arg=Arg},
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
    ScopeSub0 = sub_subst_scope(Sub0#sub{t=[]}),
    {OuterVs,ScopeSub} = pattern_list(OuterVs0, ScopeSub0),
    
    OuterBody = body(OuterBody0, ScopeSub),

    {InnerVs,Sub} = pattern_list(InnerVs0, Sub0),
    InnerBody = body(InnerBody0, Sub),
    Outer#c_let{vars=OuterVs,arg=Arg,
		body=Inner#c_let{vars=InnerVs,arg=OuterBody,body=InnerBody}};
move_let_into_expr(#c_let{vars=Lvs0,body=Lbody0}=Let,
		   #c_case{arg=Cexpr0,clauses=[Ca0,Cb0|Cs]}=Case, Sub0) ->
    %% Test if there are no more clauses than Ca0 and Cb0, or if
    %% Cb0 is guaranteed to match.
    TwoClauses = Cs =:= [] orelse
	case Cb0 of
	    #c_clause{pats=[#c_var{}],guard=#c_literal{val=true}} -> true;
	    _ -> false
	end,
    case {TwoClauses,is_failing_clause(Ca0),is_failing_clause(Cb0)} of
	{true,false,true} ->
	    %% let <Lvars> = case <Case-expr> of
	    %%                  <Cvars> -> <Clause-body>;
	    %%                  <OtherCvars> -> erlang:error(...)
	    %%               end
	    %% in <Let-body>
	    %%
	    %%     ==>
	    %%
	    %% case <Case-expr> of
	    %%   <Cvars> ->
	    %%       let <Lvars> = <Clause-body>
	    %%       in <Let-body>;
	    %%   <OtherCvars> -> erlang:error(...)
	    %% end

	    Cexpr = body(Cexpr0, Sub0),
	    CaVars0 = Ca0#c_clause.pats,
	    G0 = Ca0#c_clause.guard,
	    B0 = Ca0#c_clause.body,
	    ScopeSub0 = sub_subst_scope(Sub0#sub{t=[]}),
	    {CaVars,ScopeSub} = pattern_list(CaVars0, ScopeSub0),
	    G = guard(G0, ScopeSub),

	    B1 = body(B0, ScopeSub),

	    {Lvs,B2,Sub1} = let_substs(Lvs0, B1, Sub0),
	    Sub2 = Sub1#sub{s=gb_sets:union(ScopeSub#sub.s,
					    Sub1#sub.s)},
	    Lbody = body(Lbody0, Sub2),
	    B = Let#c_let{vars=Lvs,arg=core_lib:make_values(B2),body=Lbody},

	    Ca = Ca0#c_clause{pats=CaVars,guard=G,body=B},
	    Cb = clause(Cb0, Cexpr, value, Sub0),
	    Case#c_case{arg=Cexpr,clauses=[Ca,Cb]};
	{_,_,_} -> impossible
    end;
move_let_into_expr(_Let, _Expr, _Sub) -> impossible.

is_failing_clause(#c_clause{body=B}) ->
    will_fail(B).

scope_add(Vs, #sub{s=Scope0}=Sub) ->
    Scope = foldl(fun(V, S) when is_integer(V); is_atom(V) ->
			  gb_sets:add(V, S)
		  end, Scope0, Vs),
    Sub#sub{s=Scope}.

%% opt_simple_let(#c_let{}, Context, Sub) -> CoreTerm
%%  Optimize a let construct that does not contain any lets in
%%  in its argument.

opt_simple_let(#c_let{arg=Arg0}=Let, Ctxt, Sub0) ->
    Arg = body(Arg0, value, Sub0),		%This is a body
    case will_fail(Arg) of
	true -> Arg;
	false -> opt_simple_let_1(Let, Arg, Ctxt, Sub0)
    end.

opt_simple_let_1(#c_let{vars=Vs0,body=B0}=Let, Arg0, Ctxt, Sub0) ->
    %% Optimise let and add new substitutions.
    {Vs,Args,Sub1} = let_substs(Vs0, Arg0, Sub0),
    BodySub = case {Vs,Args} of
		  {[V],[A]} ->
		      case is_bool_expr(A, Sub0) of
			  true ->
			      update_types(V, [#c_literal{val=true}], Sub1);
			  false ->
			      Sub1
		      end;
		  {_,_} -> Sub1
	      end,
    B = body(B0, Ctxt, BodySub),
    Arg = core_lib:make_values(Args),
    opt_simple_let_2(Let, Vs, Arg, B, Ctxt, Sub1).

opt_simple_let_2(Let0, Vs0, Arg0, Body0, effect, Sub) ->
    case {Vs0,Arg0,Body0} of
	{[],#c_values{es=[]},Body} ->
	    %% No variables left (because of substitutions).
	    Body;
	{[_|_],Arg,#c_literal{}} ->
	    %% The body is a literal. That means that we can ignore
	    %% it and that the return value is Arg revisited in
	    %% effect context.
	    body(Arg, effect, sub_new_preserve_types(Sub));
	{Vs,Arg,Body} ->
	    %% Since we are in effect context, there is a chance
	    %% that the body no longer references the variables.
	    %% In that case we can construct a sequence and visit
	    %% that in effect context:
	    %%   let <Var> = Arg in BodyWithoutVar  ==> seq Arg BodyWithoutVar
	    case is_any_var_used(Vs, Body) of
		false ->
		    expr(#c_seq{arg=Arg,body=Body}, effect, sub_new_preserve_types(Sub));
		true ->
		    Let = Let0#c_let{vars=Vs,arg=Arg,body=Body},
		    opt_case_in_let_arg(opt_case_in_let(Let), effect, Sub)
	    end
    end;
opt_simple_let_2(Let, Vs0, Arg0, Body, value, Sub) ->
    case {Vs0,Arg0,Body} of
	{[#c_var{name=N1}],Arg,#c_var{name=N2}} ->
	    case N1 =:= N2 of
		true ->
		    %% let <Var> = Arg in <Var>  ==>  Arg
		    Arg;
		false ->
		    %% let <Var> = Arg in <OtherVar>  ==>  seq Arg OtherVar
		    expr(#c_seq{arg=Arg,body=Body}, value, sub_new_preserve_types(Sub))
	    end;
	{[],#c_values{es=[]},_} ->
	    %% No variables left.
	    Body;
	{_,Arg,#c_literal{}} ->
	    %% The variable is not used in the body. The argument
	    %% can be evaluated in effect context to simplify it.
	    expr(#c_seq{arg=Arg,body=Body}, value, sub_new_preserve_types(Sub));
	{Vs,Arg,Body} ->
	    opt_case_in_let_arg(
	      opt_case_in_let(Let#c_let{vars=Vs,arg=Arg,body=Body}),
	      value, Sub)
    end.

%% In guards only, rewrite a case in a let argument like
%%
%%    let <Var> = case <> of
%%                    <> when AnyGuard -> Literal1;
%%                    <> when AnyGuard -> Literal2
%%                end
%%    in LetBody
%%
%% to
%%
%%    case <> of
%%         <> when AnyGuard ->
%%              let <Var> = Literal1 in LetBody
%%         <> when 'true' ->
%%              let <Var> = Literal2 in LetBody
%%    end
%%    
%% In the worst case, the size of the code could increase.
%% In practice, though, substituting the literals into
%% LetBody and doing constant folding will decrease the code
%% size. (Doing this transformation outside of guards could
%% lead to a substantational increase in code size.)
%%
opt_case_in_let_arg(#c_let{arg=#c_case{}=Case}=Let, Ctxt,
		    #sub{in_guard=true}=Sub) ->
    opt_case_in_let_arg_1(Let, Case, Ctxt, Sub);
opt_case_in_let_arg(Let, _, _) -> Let.

opt_case_in_let_arg_1(Let0, #c_case{arg=#c_values{es=[]},
				   clauses=Cs}=Case0, Ctxt, Sub) ->
    Let = mark_compiler_generated(Let0),
    case Cs of
	[#c_clause{body=#c_literal{}=BodyA}=Ca0,
	 #c_clause{body=#c_literal{}=BodyB}=Cb0] ->
	    Ca = Ca0#c_clause{body=Let#c_let{arg=BodyA}},
	    Cb = Cb0#c_clause{body=Let#c_let{arg=BodyB}},
	    Case = Case0#c_case{clauses=[Ca,Cb]},
	    expr(Case, Ctxt, sub_new_preserve_types(Sub));
	_ -> Let
    end;
opt_case_in_let_arg_1(Let, _, _, _) -> Let.

is_any_var_used([#c_var{name=V}|Vs], Expr) ->
    case core_lib:is_var_used(V, Expr) of
	false -> is_any_var_used(Vs, Expr);
	true -> true
    end;
is_any_var_used([], _) -> false.

is_boolean_type(V, #sub{t=Tdb}) ->
    case orddict:find(V, Tdb) of
	{ok,bool} -> true;
	_ -> false
    end.

%% update_types(Expr, Pattern, Sub) -> Sub'
%%  Update the type database.
update_types(Expr, Pat, #sub{t=Tdb0}=Sub) ->
    Tdb = update_types_1(Expr, Pat, Tdb0),
    Sub#sub{t=Tdb}.

update_types_1(#c_var{name=V,anno=Anno}, Pat, Types) ->
    case member(reuse_for_context, Anno) of
	true ->
	    %% If a variable has been marked for reuse of binary context,
	    %% optimizations based on type information are unsafe.
	    kill_types(V, Types);
	false ->
	    update_types_2(V, Pat, Types)
    end;
update_types_1(_, _, Types) -> Types.

update_types_2(V, [#c_tuple{}=P], Types) ->
    orddict:store(V, P, Types);
update_types_2(V, [#c_literal{val=Bool}], Types) when is_boolean(Bool) ->
    orddict:store(V, bool, Types);
update_types_2(_, _, Types) -> Types.

%% kill_types(V, Tdb) -> Tdb'
%%  Kill any entries that references the variable,
%%  either in the key or in the value.

kill_types(V, [{V,_}|Tdb]) ->
    kill_types(V, Tdb);
kill_types(V, [{_,#c_tuple{}=Tuple}=Entry|Tdb]) ->
    case core_lib:is_var_used(V, Tuple) of
	false -> [Entry|kill_types(V, Tdb)];
	true -> kill_types(V, Tdb)
    end;
kill_types(V, [{_,Atom}=Entry|Tdb]) when is_atom(Atom) ->
    [Entry|kill_types(V, Tdb)];
kill_types(_, []) -> [].

%% copy_type(DestVar, SrcVar, Tdb) -> Tdb'
%%  If the SrcVar has a type, assign it to DestVar.
%%
copy_type(V, #c_var{name=Src}, Tdb) ->
    case orddict:find(Src, Tdb) of
	{ok,Type} -> orddict:store(V, Type, Tdb);
	error -> Tdb
    end;
copy_type(_, _, Tdb) -> Tdb.

%% The atom `ok', is widely used in Erlang for "void" values.

void() -> #c_literal{val=ok}.

%%%
%%% Annotate bit syntax matching to faciliate optimization in further passes.
%%%

bsm_an(#c_case{arg=#c_var{}=V}=Case) ->
    bsm_an_1([V], Case);
bsm_an(#c_case{arg=#c_values{es=Es}}=Case) ->
    bsm_an_1(Es, Case);
bsm_an(Other) -> Other.

bsm_an_1(Vs, #c_case{clauses=Cs}=Case) ->
    case bsm_leftmost(Cs) of
	none -> Case;
	Pos -> bsm_an_2(Vs, Cs, Case, Pos)
    end.

bsm_an_2(Vs, Cs, Case, Pos) ->
    case bsm_nonempty(Cs, Pos) of
	true -> bsm_an_3(Vs, Cs, Case, Pos);
	false -> Case
    end.

bsm_an_3(Vs, Cs, Case, Pos) ->
    try
	bsm_ensure_no_partition(Cs, Pos),
	bsm_do_an(Vs, Pos, Cs, Case)
    catch
	throw:{problem,Where,What} ->
	    add_bin_opt_info(Where, What),
	    Case
    end.

bsm_do_an(Vs0, Pos, Cs0, Case) ->
    case nth(Pos, Vs0) of
	#c_var{name=Vname}=V0 ->
	    Cs = bsm_do_an_var(Vname, Pos, Cs0, []),
	    V = bsm_annotate_for_reuse(V0),
	    Bef = lists:sublist(Vs0, Pos-1),
	    Aft = lists:nthtail(Pos, Vs0),
	    case Bef ++ [V|Aft] of
		[_] ->
		    Case#c_case{arg=V,clauses=Cs};
		Vs ->
		    Case#c_case{arg=#c_values{es=Vs},clauses=Cs}
	    end;
	_ ->
	    Case
    end.

bsm_do_an_var(V, S, [#c_clause{pats=Ps,guard=G,body=B0}=C0|Cs], Acc) ->
    case nth(S, Ps) of
	#c_var{name=VarName} ->
	    case core_lib:is_var_used(V, G) of
		true -> bsm_problem(C0, orig_bin_var_used_in_guard);
		false -> ok
	    end,
	    case core_lib:is_var_used(VarName, G) of
		true -> bsm_problem(C0, bin_var_used_in_guard);
		false -> ok
	    end,
	    B1 = bsm_maybe_ctx_to_binary(VarName, B0),
	    B = bsm_maybe_ctx_to_binary(V, B1),
	    C = C0#c_clause{body=B},
	    bsm_do_an_var(V, S, Cs, [C|Acc]);
	#c_alias{}=P ->
	    case bsm_could_match_binary(P) of
		false ->
		    bsm_do_an_var(V, S, Cs, [C0|Acc]);
		true ->
		    bsm_problem(C0, bin_opt_alias)
	    end;
	P ->
	    case bsm_could_match_binary(P) andalso bsm_is_var_used(V, G, B0) of
		false ->
		    bsm_do_an_var(V, S, Cs, [C0|Acc]);
		true ->
		    bsm_problem(C0, bin_var_used)
	    end
    end;
bsm_do_an_var(_, _, [], Acc) -> reverse(Acc).

bsm_annotate_for_reuse(#c_var{anno=Anno}=Var) ->
    case member(reuse_for_context, Anno) of
	false -> Var#c_var{anno=[reuse_for_context|Anno]};
	true -> Var
    end.

bsm_is_var_used(V, G, B) ->
    core_lib:is_var_used(V, G) orelse core_lib:is_var_used(V, B).

bsm_maybe_ctx_to_binary(V, B) ->
    case core_lib:is_var_used(V, B) andalso not previous_ctx_to_binary(V, B) of
	false ->
	    B;
	true ->
	    #c_seq{arg=#c_primop{name=#c_literal{val=bs_context_to_binary},
				 args=[#c_var{name=V}]},
		   body=B}
    end.

previous_ctx_to_binary(V, #c_seq{arg=#c_primop{name=Name,args=As}}) ->
    case {Name,As} of
	{#c_literal{val=bs_context_to_binary},[#c_var{name=V}]} ->
	    true;
	{_,_} ->
	    false
    end;
previous_ctx_to_binary(_, _) -> false.

%% bsm_leftmost(Cs) -> none | ArgumentNumber
%%  Find the leftmost argument that does binary matching. Return
%%  the number of the argument (1-N).

bsm_leftmost(Cs) ->
    bsm_leftmost_1(Cs, none).

bsm_leftmost_1([#c_clause{pats=Ps}|Cs], Pos) ->
    bsm_leftmost_2(Ps, Cs, 1, Pos);
bsm_leftmost_1([], Pos) -> Pos.

bsm_leftmost_2(_, Cs, Pos, Pos) ->
    bsm_leftmost_1(Cs, Pos);
bsm_leftmost_2([#c_binary{}|_], Cs, N, _) ->
    bsm_leftmost_1(Cs, N);
bsm_leftmost_2([_|Ps], Cs, N, Pos) ->
    bsm_leftmost_2(Ps, Cs, N+1, Pos);
bsm_leftmost_2([], Cs, _, Pos) ->
    bsm_leftmost_1(Cs, Pos).

%% bsm_notempty(Cs, Pos) -> true|false
%%  Check if at least one of the clauses matches a non-empty
%%  binary in the given argumet position.
%%
bsm_nonempty([#c_clause{pats=Ps}|Cs], Pos) ->
    case nth(Pos, Ps) of
	#c_binary{segments=[_|_]} ->
	    true;
	_ ->
	    bsm_nonempty(Cs, Pos)
    end;
bsm_nonempty([], _ ) -> false.

%% bsm_ensure_no_partition(Cs, Pos) -> ok     (exception if problem)
%%  We must make sure that binary matching is not partitioned between
%%  variables like this:
%%             foo(<<...>>) -> ...
%%             foo(Var) when ... -> ...
%%             foo(<<...>>) ->
%%  If there is such partition, we are not allowed to reuse the binary variable
%%  for the match context. Also, arguments to the left of the argument that
%%  is matched against a binary, are only allowed to be simple variables, not
%%  used in guards. The reason is that we must know that the binary is only
%%  matched in one place.

bsm_ensure_no_partition(Cs, Pos) ->
    bsm_ensure_no_partition_1(Cs, Pos, before).

%% Loop through each clause.
bsm_ensure_no_partition_1([#c_clause{pats=Ps,guard=G}|Cs], Pos, State0) ->
    State = bsm_ensure_no_partition_2(Ps, Pos, G, simple_vars, State0),
    bsm_ensure_no_partition_1(Cs, Pos, State);
bsm_ensure_no_partition_1([], _, _) -> ok.

%% Loop through each pattern for this clause.
bsm_ensure_no_partition_2([#c_binary{}=Where|_], 1, _, Vstate, State) ->
    case State of
	before when Vstate =:= simple_vars -> within;
	before -> bsm_problem(Where, Vstate);
	within when Vstate =:= simple_vars -> within;
	within -> bsm_problem(Where, Vstate);
	'after' -> bsm_problem(Where, bin_partition)
    end;
bsm_ensure_no_partition_2([#c_alias{}=Alias|_], 1, N, Vstate, State) ->
    %% Retrieve the real pattern that the alias refers to and check that.
    P = bsm_real_pattern(Alias),
    bsm_ensure_no_partition_2([P], 1, N, Vstate, State);
bsm_ensure_no_partition_2([_|_], 1, _, _Vstate, before=State) ->
    %% No binary matching yet - therefore no partition.
    State;
bsm_ensure_no_partition_2([P|_], 1, _, Vstate, State) ->
    case bsm_could_match_binary(P) of
	false ->
	    %% If clauses can be freely arranged (Vstate =:= simple_vars),
	    %% a clause that cannot match a binary will not partition the clause.
	    %% Example:
	    %%
	    %% a(Var, <<>>) -> ...
	    %% a(Var, []) -> ...
	    %% a(Var, <<B>>) -> ...
	    %%
	    %% But if the clauses can't be freely rearranged, as in
	    %%
	    %% b(Var, <<>>) -> ...
	    %% b(1, 2) -> ...
	    %%
	    %% we do have a problem.
	    %%
	    case Vstate of
		simple_vars -> State;
		_ -> bsm_problem(P, Vstate)
	    end;
	true ->
	    %% The pattern P *may* match a binary, so we must update the state.
	    %% (P must be a variable.)
	    case State of
		within -> 'after';
		'after' -> 'after'
	    end
    end;
bsm_ensure_no_partition_2([#c_var{name=V}|Ps], N, G, Vstate, S) ->
    case core_lib:is_var_used(V, G) of
	false ->
	    bsm_ensure_no_partition_2(Ps, N-1, G, Vstate, S);
	true ->
	    bsm_ensure_no_partition_2(Ps, N-1, G, bin_left_var_used_in_guard, S)
    end;
bsm_ensure_no_partition_2([_|Ps], N, G, _, S) ->
    bsm_ensure_no_partition_2(Ps, N-1, G, bin_argument_order, S).

bsm_could_match_binary(#c_alias{pat=P}) -> bsm_could_match_binary(P);
bsm_could_match_binary(#c_cons{}) -> false;
bsm_could_match_binary(#c_tuple{}) -> false;
bsm_could_match_binary(#c_literal{val=Lit}) -> is_bitstring(Lit);
bsm_could_match_binary(_) -> true.

bsm_real_pattern(#c_alias{pat=P}) -> bsm_real_pattern(P);
bsm_real_pattern(P) -> P.

bsm_problem(Where, What) ->
    throw({problem,Where,What}).

%%%
%%% Handling of warnings.
%%%

mark_compiler_generated(Term) ->
    cerl_trees:map(fun mark_compiler_generated_1/1, Term).

mark_compiler_generated_1(#c_call{anno=Anno}=Term) ->
    Term#c_call{anno=[compiler_generated|Anno--[compiler_generated]]};
mark_compiler_generated_1(Term) -> Term.

init_warnings() ->
    put({?MODULE,warnings}, []).

add_bin_opt_info(Core, Term) ->
    case get(bin_opt_info) of
	true -> add_warning(Core, Term);
	false -> ok
    end.

add_warning(Core, Term) ->
    Anno = core_lib:get_anno(Core),
    case lists:member(compiler_generated, Anno) of
	true -> ok;
	false ->
	    case get_line(Anno) of
		Line when Line >= 0 ->		%Must be positive.
                    File = get_file(Anno),
		    Key = {?MODULE,warnings},
		    case get(Key) of
			[{File,[{Line,?MODULE,Term}]}|_] ->
			    ok;			%We already have 
						%an identical warning.
			Ws ->
			    put(Key, [{File,[{Line,?MODULE,Term}]}|Ws])
		    end;
		_ -> ok				%Compiler-generated code.
	    end
    end.

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.

get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> "no_file". % should not happen

is_compiler_generated(Core) ->
    Anno = core_lib:get_anno(Core),
    case lists:member(compiler_generated, Anno) of
	true -> true;
	false ->
	    case get_line(Anno) of
		Line when Line >= 0 -> false;
		_ -> true
	    end
    end.

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
format_error(useless_building) ->
    "a term is constructed, but never used";
format_error(bin_opt_alias) ->
    "INFO: the '=' operator will prevent delayed sub binary optimization";
format_error(bin_partition) ->
    "INFO: non-consecutive clauses that match binaries "
	"will prevent delayed sub binary optimization";
format_error(bin_left_var_used_in_guard) ->
    "INFO: a variable to the left of the binary pattern is used in a guard; "
	"will prevent delayed sub binary optimization";
format_error(bin_argument_order) ->
    "INFO: matching anything else but a plain variable to the left of "
	"binary pattern will prevent delayed sub binary optimization; "
	"SUGGEST changing argument order";
format_error(bin_var_used) ->
    "INFO: using a matched out sub binary will prevent "
	"delayed sub binary optimization";
format_error(orig_bin_var_used_in_guard) ->
    "INFO: using the original binary variable in a guard will prevent "
	"delayed sub binary optimization";
format_error(bin_var_used_in_guard) ->
    "INFO: using a matched out sub binary in a guard will prevent "
	"delayed sub binary optimization".

-ifdef(DEBUG).
%% In order for simplify_let/2 to work correctly, the list of
%% in-scope variables must always be a superset of the free variables
%% in the current expression (otherwise we might fail to rename a variable
%% when needed and get a name capture bug).

verify_scope(E, #sub{s=Scope}) ->
    Free0 = cerl_trees:free_variables(E),
    Free = [V || V <- Free0, not is_tuple(V)],	%Ignore function names.
    case ordsets:is_subset(Free, gb_sets:to_list(Scope)) of
	true -> true;
	false ->
	    io:format("~p\n", [E]),
	    io:format("~p\n", [Free]),
	    io:format("~p\n", [gb_sets:to_list(Scope)]),
	    false
    end.
-endif.

%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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

-module(beam_ssa_check).
-moduledoc false.

-export([module/2, format_error/1]).

-import(lists, [reverse/1, flatten/1]).

-include("beam_ssa.hrl").

%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(DP(FMT, ARGS), io:format(FMT, ARGS)).
-define(DP(FMT), io:format(FMT)).
-else.
-define(DP(FMT, ARGS), skip).
-define(DP(FMT), skip).
-endif.

-spec module(beam_ssa:b_module(), atom()) -> 'ok' | {'error',list()}.

module(#b_module{body=Body}, Tag) ->
    Errors = functions(Tag, Body),
    case Errors of
        [] ->
            ok;
        _ ->
            {error, reverse(Errors)}
    end.

functions(Tag, [F|Rest]) ->
    function(Tag, F) ++ functions(Tag, Rest);
functions(_Tag, []) ->
    [].

function(Tag, F) ->
    run_checks(beam_ssa:get_anno(ssa_checks, F, []), F, Tag).

run_checks([{ssa_check_when,WantedResult,{atom,_,Tag},Args,Exprs}|Checks],
           F, Tag) ->
    check_function(Args, Exprs, WantedResult, F) ++ run_checks(Checks, F, Tag);
run_checks([_|Checks], F, Tag) ->
    run_checks(Checks, F, Tag);
run_checks([], _, _) ->
    [].

check_function(CheckArgs, Exprs, {atom,Loc,pass}, #b_function{args=_Args}=F) ->
    run_check(CheckArgs, Exprs, Loc, F);
check_function(CheckArgs, Exprs, {atom,Loc,Key}, #b_function{args=_Args}=F)
  when Key =:= fail ; Key =:= xfail ->
    case run_check(CheckArgs, Exprs, Loc, F) of
        [] ->
            %% This succeeded but should have failed
            {File,_} = beam_ssa:get_anno(location, F),
            [{File,[{Loc,?MODULE,xfail_passed}]}];
        _ ->
            []
    end;
check_function(_, _, {atom,Loc,Result}, F) ->
    {File,_} = beam_ssa:get_anno(location, F),
    [{File,[{Loc,?MODULE,{unknown_result_kind,Result}}]}].

run_check(CheckArgs, Exprs, Loc, #b_function{args=FunArgs}=F) ->
    init_and_run_check(CheckArgs, FunArgs, #{}, Loc, Exprs, F).


%% Create a mapping from each argument in the check pattern to the
%% actual arguments of the SSA function.
init_and_run_check([{var,Loc,'_'}|CheckArgs],
                   [#b_var{}|FunArgs],
                   Env, _, Exprs, F) ->
    init_and_run_check(CheckArgs, FunArgs, Env, Loc, Exprs, F);
init_and_run_check([{var,Loc,CheckV}|CheckArgs],
                   [V=#b_var{}|FunArgs],
                   Env, _, Exprs, F) ->
    init_and_run_check(CheckArgs, FunArgs, Env#{CheckV=>V}, Loc, Exprs, F);
init_and_run_check([{'...',_}], [_|_], Env, _Loc, Exprs, F) ->
    check_exprs(Exprs, Env, F);
init_and_run_check([], [], Env, _Loc,  Exprs, F) ->
    check_exprs(Exprs, Env, F);
init_and_run_check([], _, _Env, Loc, _Exprs, F) ->
    {File,_} = beam_ssa:get_anno(location, F),
    [{File,[{Loc,?MODULE,too_few_pattern_args}]}];
init_and_run_check(_, [], _Env, Loc, _Exprs, F) ->
    {File,_} = beam_ssa:get_anno(location, F),
    [{File,[{Loc,?MODULE,too_many_pattern_args}]}].

check_exprs(Exprs, Env, #b_function{bs=Blocks}=F) ->
    %% A function check executes sequentially on the blocks as if they
    %% were in a beam ssa listing, so arrange the blocks in RPO and
    %% insert a label for each new block.
    Code =
        lists:foldr(fun(Lbl, Acc) ->
                            #b_blk{is=Is,last=Last} = map_get(Lbl, Blocks),
                            [{label,Lbl}|Is] ++ [Last] ++ Acc
                    end, [], beam_ssa:rpo(Blocks)),
    ?DP("  Env ~p~n", [Env]),
    ?DP("  Code~n   ~p~n", [Code]),
    ?DP("  Checks~n   ~p~n", [Exprs]),
    {File,_} = beam_ssa:get_anno(location, F),
    check_expr_seq(Exprs, Code, Env, never, File).

check_expr_seq([{check_expr,Loc,Args,Anno}|Rest]=Checks,
               [First|Code], Env0, LastMatchedLoc, File) ->
    Env = try
              ?DP("trying:~n  pat: ~p~n  i: ~p~n", [Args, First]),
              op_check(Args, Anno, First, Env0)
          catch
              throw:no_match ->
                  ?DP("op_check did not match~n"),
                  false;
              error:_E ->
                  ?DP("op_check failed ~p~n", [_E]),
                  false
          end,
    case Env of
        false ->
            %% Continue with the next op in the code
            check_expr_seq(Checks, Code, Env0, LastMatchedLoc, File);
        Env ->
            %% The code matched
            check_expr_seq(Rest, Code, Env, Loc, File)
     end;
check_expr_seq([], _Blocks, _Env, _LastMatchedLoc, _File) ->
    %% Done and all expressions matched.
    [];
check_expr_seq([{check_expr,Loc,Args,_}|_], [], Env, LastMatchedLoc, File) ->
    %% We didn't find a match and we have no more code to look at
    [{File,[{Loc,?MODULE,{no_match,Args,LastMatchedLoc,Env}}]}].


op_check([set,Result,{atom,_,Op}|PArgs], PAnno,
         #b_set{dst=Dst,args=AArgs,op=Op,anno=AAnno}=_I, Env0) ->
    ?DP("trying set ~p:~n  res: ~p <-> ~p~n  args: ~p <-> ~p~n  i: ~p~n",
        [Op, Result, Dst, PArgs, AArgs, _I]),
    Env = op_check_call(Op, Result, Dst, PArgs, AArgs, Env0),
    check_annos(PAnno, AAnno, Env);
op_check([set,Result,{{atom,_,bif},{atom,_,Op}}|PArgs], PAnno,
         #b_set{dst=Dst,args=AArgs,op={bif,Op},anno=AAnno}=_I, Env0) ->
    ?DP("trying bif ~p:~n  res: ~p <-> ~p~n  args: ~p <-> ~p~n  i: ~p~n",
        [Op, Result, Dst, PArgs, AArgs, _I]),
    Env = op_check_call(Op, Result, Dst, PArgs, AArgs, Env0),
    check_annos(PAnno, AAnno, Env);
op_check([set,Result,{{atom,_,succeeded},{atom,_,Kind}}|PArgs], PAnno,
         #b_set{dst=Dst,args=AArgs,op={succeeded,Kind},anno=AAnno}=_I, Env0) ->
    ?DP("trying succeed ~p:~n  res: ~p <-> ~p~n  args: ~p <-> ~p~n  i: ~p~n",
        [Kind, Result, Dst, PArgs, AArgs, _I]),
    Env = op_check_call(dont_care, Result, Dst, PArgs, AArgs, Env0),
    check_annos(PAnno, AAnno, Env);
op_check([none,{atom,_,ret}|PArgs], PAnno,
         #b_ret{arg=AArg,anno=AAnno}=_I, Env) ->
    ?DP("trying return:, arg: ~p <-> ~p~n  i: ~p~n",
        [PArgs, [AArg], _I]),
    check_annos(PAnno, AAnno, post_args(PArgs, [AArg], Env));
op_check([none,{atom,_,br}|PArgs], PAnno,
         #b_br{bool=ABool,succ=ASucc,fail=AFail,anno=AAnno}=_I, Env0) ->
    ?DP("trying br: arg: ~p <-> ~p~n  i: ~p~n",
        [PArgs, [ABool,ASucc,AFail], _I]),
    Env = post_args(PArgs,
                    [ABool,#b_literal{val=ASucc},#b_literal{val=AFail}], Env0),
    check_annos(PAnno, AAnno, Env);
op_check([none,{atom,_,switch},PArg,PFail,{list,_,PArgs}], PAnno,
         #b_switch{arg=AArg,fail=AFail,list=AList,anno=AAnno}=_I, Env0) ->
    ?DP("trying switch: arg: ~p <-> ~p~n  i: ~p~n",
        [PArgs, [AArg,AFail,AList], _I]),
    Env = env_post(PArg, AArg, env_post(PFail, #b_literal{val=AFail}, Env0)),
    check_annos(PAnno, AAnno, post_switch_args(PArgs, AList, Env));
op_check([label,PLbl], _Anno, {label,ALbl}, Env) when is_integer(ALbl) ->
    env_post(PLbl, #b_literal{val=ALbl}, Env).

op_check_call(Op, PResult, AResult, PArgs, AArgs, Env0) ->
    Env = env_post(PResult, AResult, Env0),
    case Op of
        phi ->
            post_phi_args(PArgs, AArgs, Env);
        _ ->
            post_args(PArgs, AArgs, Env)
    end.

post_args([{'...',_}], _, Env) ->
    Env;
post_args([PA|PArgs], [AA|AArgs], Env) ->
    post_args(PArgs, AArgs, env_post(PA, AA, Env));
post_args([], [], Env) ->
    Env;
post_args(Pattern, Args, _Env) ->
    io:format("Failed to match ~kp <-> ~kp~n", [Pattern, Args]),
    error({internal_pattern_match_error,post_args}).

post_phi_args([{'...',_}], _, Env) ->
    Env;
post_phi_args([{tuple,_,[PVar,PLbl]}|PArgs], [{AVar,ALbl}|AArgs], Env0) ->
    Env = env_post(PVar, AVar, env_post(PLbl, ALbl, Env0)),
    post_phi_args(PArgs, AArgs, Env);
post_phi_args([], [], Env) ->
    Env.

post_switch_args([{'...',_}], _, Env) ->
    Env;
post_switch_args([{tuple,_,[PVal,PLbl]}|PArgs], [{AVal,ALbl}|AArgs], Env0) ->
    Env = env_post(PVal, AVal, env_post(PLbl, #b_literal{val=ALbl}, Env0)),
    post_switch_args(PArgs, AArgs, Env);
post_switch_args([], [], Env) ->
    Env.

env_post({var,_,PV}, Actual, Env) ->
    env_post1(PV, Actual, Env);
env_post({atom,_,Atom}, #b_literal{val=Atom}, Env) ->
    Env;
env_post({atom,_,Atom}, Atom, Env) when is_atom(Atom) ->
    Env;
env_post({local_fun,{atom,_,N},{integer,_,A}},
         #b_local{name=#b_literal{val=N},arity=A}, Env) ->
    Env;
env_post({external_fun,{atom,_,M},{atom,_,N},{integer,_,A}},
         #b_remote{mod=#b_literal{val=M},name=#b_literal{val=N},arity=A},
         Env) ->
    Env;
env_post({external_fun,{atom,_,M},{atom,_,N},{integer,_,A}},
         #b_literal{val=F}, Env) ->
    {M,N,A} = erlang:fun_info_mfa(F),
    Env;
env_post({integer,_,V}, #b_literal{val=V}, Env) ->
    Env;
env_post({integer,_,V}, V, Env) when is_integer(V) ->
    Env;
env_post({float,_,V}, #b_literal{val=V}, Env) ->
    Env;
env_post({float,_,V}, V, Env) when is_float(V) ->
    Env;
env_post({float_epsilon,{float,_,V},{float,_,Epsilon}},
         #b_literal{val=Actual}, Env) ->
    true = abs(V - Actual) < Epsilon,
    Env;
env_post({float_epsilon,{float,_,V},{float,_,Epsilon}}, Actual, Env)
  when is_float(Actual) ->
    true = abs(V - Actual) < Epsilon,
    Env;
env_post({binary,_,Bits}, #b_literal{val=V}, Env) ->
    post_bitstring(Bits, V, Env);
env_post({binary,_,Bits}, Bin, Env) when is_bitstring(Bin)->
    post_bitstring(Bits, Bin, Env);
env_post({list,_,Elems}, #b_literal{val=Ls}, Env) ->
    post_list(Elems, Ls, Env);
env_post({list,_,Elems}, Ls, Env) when is_list(Ls) ->
    post_list(Elems, Ls, Env);
env_post({tuple,_,Es}, #b_literal{val=Ls}, Env) ->
    post_tuple(Es, tuple_to_list(Ls), Env);
env_post({tuple,_,Es}, Tuple, Env) when is_tuple(Tuple) ->
    post_tuple(Es, tuple_to_list(Tuple), Env);
env_post({map,_,Elems}, #b_literal{val=Map}, Env) when is_map(Map) ->
    post_map(Elems, Map, Env);
env_post({map,_,Elems}, Map, Env) when is_map(Map) ->
    post_map(Elems, Map, Env);
env_post(_Pattern, _Args, _Env) ->
    ?DP("Failed to match ~p <-> ~p~n", [_Pattern, _Args]),
    error({internal_pattern_match_error,env_post}).

env_post1('_', _Actual, Env) ->
    ?DP("Ignored posting _ => ~p~n", [_Actual]),
    Env;
env_post1(PV, Actual, Env) when is_map_key(PV, Env) ->
    ?DP("Attempting post ~p => ~p in ~p~n", [PV, Actual, Env]),
    Actual = map_get(PV, Env),
    Env;
env_post1(PV, #b_var{}=Actual, Env) ->
    ?DP("Posting ~p => ~p~n", [PV, Actual]),
    Env#{PV => Actual};
env_post1(PV, #b_literal{}=Actual, Env) ->
    ?DP("Posting ~p => ~p~n", [PV, Actual]),
    Env#{PV => Actual};
env_post1(_Pattern, _Actual, _Env) ->
    ?DP("Failed to match ~p <-> ~p~n", [_Pattern, _Actual]),
    error({internal_pattern_match_error,env_post1}).

post_bitstring(Bytes, Actual, Env) ->
    Actual = build_bitstring(Bytes, <<>>),
    Env.

%% Convert the parsed literal binary to an actual bitstring.
build_bitstring([{integer,_,V}|Bytes], Acc) ->
    build_bitstring(Bytes, <<Acc/bits, V:8>>);
build_bitstring([{{integer,_,V},{integer,_,N}}|Bytes], Acc) ->
    build_bitstring(Bytes, <<Acc/bits, V:N>>);
build_bitstring([], Acc) ->
    Acc.

post_list([{'...',_}], _, Env) ->
    Env;
post_list([Elem|Elements], [A|Actual], Env0) ->
    Env = env_post(Elem, A, Env0),
    post_list(Elements, Actual, Env);
post_list([], [], Env) ->
    Env;
post_list(Elem, Actual, Env) ->
    env_post(Elem, Actual, Env).

post_tuple([{'...',_}], _, Env) ->
    Env;
post_tuple([Elem|Elements], [A|Actual], Env0) ->
    Env = env_post(Elem, A, Env0),
    post_tuple(Elements, Actual, Env);
post_tuple([], [], Env) ->
    Env.

post_map([{Key,Val}|Items], Map, Env) ->
    K = build_map_key(Key, Env),
    V = build_map_key(Val, Env),
    #{K := V} = Map,

    post_map(Items, maps:remove(K, Map), Env);
post_map([], Map, Env) ->
    0 = maps:size(Map),
    Env.

build_map_key({atom,_,A}, _Env) ->
    A;
build_map_key({local_fun,{atom,_,N},{integer,_,A}}, _Env) ->
    #b_local{name=#b_literal{val=N},arity=A};
build_map_key({integer,_,V}, _Env) ->
    V;
build_map_key({float,_,V}, _Env) ->
    V;
build_map_key({binary,_,Bits}, _Env) ->
    build_bitstring(Bits, <<>>);
build_map_key({list,_,Elems}, Env) ->
    build_map_key_list(Elems, Env);
build_map_key({tuple,_,Elems}, Env) ->
    list_to_tuple([build_map_key(E, Env) || E <- Elems]);
build_map_key({map,_,Elems}, Env) ->
    #{build_map_key(K, Env) => build_map_key(V, Env) || {K,V} <:- Elems};
build_map_key({var,_,V}, Env) ->
    map_get(V, Env);
build_map_key(_Key, _Env) ->
    ?DP("Failed to match ~p~n", [_Key]),
    error({internal_pattern_match_error,build_map_key}).

build_map_key_list([E|Elems], Env) ->
    [build_map_key(E, Env)|build_map_key_list(Elems, Env)];
build_map_key_list([], _Env) ->
    [];
build_map_key_list(E, Env) ->
    build_map_key(E, Env).

check_annos([{term,{atom,_,Key},PTerm}|Patterns], Actual, Env0) ->
    ?DP("Checking term anno~n  wanted anno-key ~p~n", [Key]),
    ?DP("  actual anno keys ~p~n", [maps:keys(Actual)]),
    ?DP("  pattern on selected anno ~p~n", [PTerm]),
    #{ Key := ATerm } = Actual,
    ?DP("  actual selected anno ~p~n", [ATerm]),
    Env = env_post(PTerm, #b_literal{val=ATerm}, Env0),
    ?DP("ok~n"),
    check_annos(Patterns, Actual, Env);
check_annos([], _, Env) ->
    Env.

-spec format_error(term()) -> nonempty_string().

format_error(xfail_passed) ->
    "test which was expected to fail passed";
format_error({unknown_result_kind,Result}) ->
    "unknown expected result: " ++ atom_to_list(Result);
format_error(too_many_pattern_args) ->
    "pattern has more arguments than the function";
format_error(too_few_pattern_args) ->
    "pattern has fewer arguments than the function";
format_error({no_match,_Args,_LastMatchedLoc,Env}) ->
    flatten(io_lib:format("no match found for pattern, env: ~p~n", [Env])).


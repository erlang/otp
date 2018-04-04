%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-module(beam_literals_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([putting/1, matching_smalls/1, matching_smalls_jt/1,
	 matching_bigs/1, matching_more_bigs/1,
	 matching_bigs_and_smalls/1, badmatch/1, case_clause/1,
	 receiving/1, literal_type_tests/1,
	 put_list/1, fconv/1, literal_case_expression/1,
	 increment/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [putting, matching_smalls, matching_smalls_jt,
     matching_bigs, matching_more_bigs,
     matching_bigs_and_smalls, badmatch, case_clause,
     receiving, literal_type_tests, put_list, fconv,
     literal_case_expression, increment].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Test creating lists and tuples containing big number literals.
putting(Config) when is_list(Config) ->
    -773973888575883407313908 = chksum(putting1(8987697898797)).

putting1(X) ->
    {8797987987987987872256443, [1324483773773], {3.1415, 2.71, [2.5, 35.125|9.31]},
     [X|349873987387373],
     [329878349873|-387394729872], -773973937933873929749873}.

%% Test matching of a few big number literals (in Beam select_val/3 will NOT be used).
matching_bigs(Config) when is_list(Config) ->
    a = matching1(3972907842873739),
    b = matching1(-389789298378939783333333333333333333784),
    other = matching1(3141699999999999999999999999999999999),
    other = matching1(42).

%% Test matching small numbers (both positive and negative).
matching_smalls(Config) when is_list(Config) ->
    a = m_small(-42),
    b = m_small(0),
    c = m_small(105),
    d = m_small(-13),
    e = m_small(337848),
    other = m_small(324),
    other = m_small(-7),
    ok.

m_small(-42) -> a;
m_small(0) -> b;
m_small(105) -> c;
m_small(-13) -> d;
m_small(337848) -> e;
m_small(_) -> other.

%% Test matching small numbers (both positive and negative).
%% Make sure that a jump table is used.
matching_smalls_jt(Config) when is_list(Config) ->
    a = m_small_jt(-2),
    b = m_small_jt(-1),
    c = m_small_jt(0),
    d = m_small_jt(2),
    e = m_small_jt(3),
    other = m_small(324),
    other = m_small(-7),
    ok.

m_small_jt(-2) -> a;
m_small_jt(-1) -> b;
m_small_jt(0) -> c;
m_small_jt(2) -> d;
m_small_jt(3) -> e;
m_small_jt(_) -> other.

%% Big numbers, no select_val.

matching1(3972907842873739) -> a;
matching1(-389789298378939783333333333333333333784) -> b;
matching1(_) -> other.


%% Test matching of a big number literals (in Beam, a select_val/3 instruction will be used)
matching_more_bigs(Config) when is_list(Config) ->
    a = matching2(-999766349740978337),
    b = matching2(9734097866575478),
    c = matching2(-966394677364879734),
    d = matching2(13987294872948990),
    e = matching2(777723896192459245),
    other = matching2(7),
    other = matching2(39789827988888888888888888888347474444444444444444444).

%% Big numbers with select_val.

matching2(-999766349740978337) -> a;
matching2(9734097866575478) -> b;
matching2(-966394677364879734) -> c;
matching2(13987294872948990) -> d;
matching2(777723896192459245) -> e;
matching2(_) -> other.

%% Test matching of a mix of big numbers and literals.
matching_bigs_and_smalls(Config) when is_list(Config) ->
    a = matching3(38472928723987239873873),
    b = matching3(0),
    c = matching3(-3873973932710954671207461057614287561348756348743634876436784367873),
    d = matching3(3978429867297393873),
    e = matching3(42),
    f = matching3(-4533),
    other = matching3(77),
    other = matching3(39274120984379249874219748).

%% Mixed small and big.

matching3(38472928723987239873873) -> a;
matching3(0) -> b;
matching3(-3873973932710954671207461057614287561348756348743634876436784367873) -> c;
matching3(3978429867297393873) -> d;
matching3(42) -> e;
matching3(-4533) -> f;
matching3(_) -> other.

%% Test literal badmatches with big number and floats.
badmatch(Config) when is_list(Config) ->
    %% We are satisfied if we can load this module and run it.
    Big = id(32984798729847892498297824872982972978239874),
    Float = id(3.1415927),
    catch a = Big,
    catch b = Float,
    {'EXIT',{{badmatch,3879373498378993387},_}} =
	   (catch c = 3879373498378993387),
    {'EXIT',{{badmatch,7.0},_}} = (catch d = 7.0),
    case Big of
        Big -> ok
    end,
    case Float of
        Float -> ok
    end,
    ok.

case_clause(Config) when is_list(Config) ->
    {'EXIT',{{case_clause,337.0},_}} = (catch case_clause_float()),
    {'EXIT',{{try_clause,42.0},_}} = (catch try_case_clause_float()),
    {'EXIT',{{case_clause,37932749837839747383847398743789348734987},_}} =
	(catch case_clause_big()),
    {'EXIT',{{try_clause,977387349872349870423364354398566348},_}} =
	(catch try_case_clause_big()),
    ok.

case_clause_float() ->
    case 337.0 of
	blurf -> ok
    end.

try_case_clause_float() ->
    try 42.0 of
	blurf -> ok
    catch _:_ ->
	    error
    end.

case_clause_big() ->
    case 37932749837839747383847398743789348734987 of
	blurf -> ok
    end.

try_case_clause_big() ->
    try 977387349872349870423364354398566348 of
	blurf -> ok
    catch _:_ ->
	    error
    end.

%% Test receive with a big number literal (more than 27 bits, less than 32 bits).
receiving(Config) when is_list(Config) ->
    Self = self(),
    spawn(fun() -> Self ! here_is_a_message end),
    ok = receive
	     here_is_a_message ->
		 ok
	 after 16#f1234567 ->
		 timeout
	 end.

%% Test type tests on literal values.
literal_type_tests(Config) when is_list(Config) ->
    %% Generate an Erlang module with all different type of type tests.
    Tests = make_test([{T, L} || T <- type_tests(), L <- literals()]),
    Mod = literal_test,
    Anno = erl_anno:new(0),
    Func = {function, Anno, test, 0, [{clause,Anno,[],[],Tests}]},
    Form = [{attribute,Anno,module,Mod},
            {attribute,Anno,compile,export_all},
            Func, {eof,Anno}],

    %% Print generated code for inspection.
    lists:foreach(fun (F) -> io:put_chars([erl_pp:form(F),"\n"]) end, Form),

    %% Test compile:form/1.  This implies full optimization (default).
    {ok,Mod,Code1} = compile:forms(Form),
    {module,Mod} = code:load_binary(Mod, Mod, Code1),
    Mod:test(),
    true = code:delete(Mod),
    code:purge(Mod),
			       
    %% Test compile:form/2.  Turn off all optimizations.
    {ok,Mod,Code2} = compile:forms(Form, [binary,report,time,
						no_copt,no_postopt]),
    {module,Mod} = code:load_binary(Mod, Mod, Code2),
    Mod:test(),
    true = code:delete(Mod),
    code:purge(Mod),
    ok.

make_test([{is_function=T,L}|Ts]) ->
    [guard_test(T, L),guard_test(T, 0, L),body_test(T, L),body_test(T, 0, L)|make_test(Ts)];
make_test([{T,L}|Ts]) ->
    [guard_test(T, L),body_test(T, L)|make_test(Ts)];
make_test([]) -> [].

guard_test(_, L) when is_function(L) ->
    %% Skip guard tests with exports - they are not literals
    {atom,erl_anno:new(0),true};
guard_test(T, L) ->
    S = io_lib:format("begin io:format(\"~~p~n\", [{~p,~p}]), if ~w(~w) -> true; true -> false end end. ", [T, L, T, L]),
    {Val,Expr} = eval_string(S),
    Anno = erl_anno:new(0),
    {match,Anno,{atom,Anno,Val},Expr}.

guard_test(_, _, L) when is_function(L) ->
    %% Skip guard tests with exports - they are not literals
    {atom,erl_anno:new(0),true};
guard_test(T, A, L) ->
    S = io_lib:format("begin io:format(\"~~p~n\", [{~p,~p,~p}]), if ~w(~w, ~w) -> true; true -> false end end. ", [T,L,A,T,L,A]),
    {Val,Expr} = eval_string(S),
    Anno = erl_anno:new(0),
    {match,Anno,{atom,Anno,Val},Expr}.

body_test(T, L) ->
    S = io_lib:format("begin io:format(\"~~p~n\", [{~p,~p}]), ~w(~w) end. ", [T,L,T,L]),
    {Val,Expr} = eval_string(S),
    Anno = erl_anno:new(0),
    {match,Anno,{atom,Anno,Val},Expr}.

body_test(T, A, L) ->
    S = io_lib:format("begin io:format(\"~~p~n\", [{~p,~p,~p}]), ~w(~w,~w) end. ", [T,L,A,T,L,A]),
    {Val,Expr} = eval_string(S),
    Anno = erl_anno:new(0),
    {match,Anno,{atom,Anno,Val},Expr}.

eval_string(S) ->
    {ok,Toks,_Line} = erl_scan:string(lists:flatten(S)),
    {ok,E} = erl_parse:parse_exprs(Toks),
    {value,Val,_Bs} = erl_eval:exprs(E, []),
    {Val,hd(E)}.

literals() ->
    [42,
     3.14,
     -3,
     32982724987789283473473838474,
     [],
     "abc",
     <<"abc">>,
     {},
     xxxx,
     fun erlang:erase/0].

type_tests() ->
    [is_boolean,
     is_integer,
     is_float,
     is_number,
     is_atom,
     is_list,
     is_tuple,
     is_pid,
     is_reference,
     is_port,
     is_binary,
     is_function].

put_list(Config) when is_list(Config) ->
    %% put_list x0 Literal Reg
    [Config|8739757395764] = put_list_rqr(Config),
    {[Config|7779757395764],Config} = put_list_rqx(Config),
    [Config|98765432100000] = put_list_rqy(Config),

    %% put_list x Literal Reg
    [Config|16#FFFFF77777137483769] = put_list_xqr(ignore, Config),
    {[Config|16#AAAAAFFFFF77777],{a,b},Config} =  put_list_xqx({a,b}, Config),
    [Config|12777765432979879] = put_list_xqy(ignore, Config),

    %% put_list y Literal Reg
    [Config|17424134793676869867] = put_list_yqr(Config),
    {[Config|77424134793676869867],Config} = put_list_yqx(Config),
    {Config,[Config|16#BCDEFF4241676869867]} = put_list_yqy(Config),

    %% put_list Literal x0 Reg
    [42.0|Config] = put_list_qrr(Config),
    [Config,42.0|Config] = put_list_qrx(Config),
    [100.0|Config] = put_list_qry(Config),

    %% put_list Literal x1 Reg
    [127.0|Config] = put_list_qxr({ignore,me}, Config),
    [Config,130.0|Config] = put_list_qxx(ignore, Config),
    [99.0|Config] = put_list_qxy(Config),

    %% put_list Literal y0 Reg
    [200.0|Config] = put_list_qyr(Config),
    [Config,210.0|Config] = put_list_qyx(Config),
    [[300.0|Config]|Config] = put_list_qyy(Config),

    ok.
    
%% put_list x0 Literal x0
put_list_rqr(Config) -> [Config|8739757395764].

%% put_list x0 Literal x1
put_list_rqx(Config) -> {[Config|7779757395764],Config}.

%% put_list x0 Literal y0
put_list_rqy(Config) ->
    Res = [Config|98765432100000],
    id(42),
    Res.

%% put_list x1 Literal x0
put_list_xqr(_, Config) -> [Config|16#FFFFF77777137483769].

%% put_list x1 Literal x2
put_list_xqx(A, Config) -> {[Config|16#AAAAAFFFFF77777],A,Config}.

%% put_list x1 Literal y0
put_list_xqy(_, Config) ->
    Res = [Config|12777765432979879],
    id(42),
    Res.

%% put_list y0 Literal x0
put_list_yqr(Config) ->    
    id(Config),
    [Config|17424134793676869867].

%% put_list y0 Literal x1
put_list_yqx(Config) ->    
    id(Config),
    {[Config|77424134793676869867],Config}.

%% put_list y1 Literal y0
put_list_yqy(Config) ->
    id(Config),
    Res = [Config|16#BCDEFF4241676869867],
    id(Config),
    {Config,Res}.

%% put_list Literal x0 x0
put_list_qrr(Config) ->
    [42.0|Config].

%% put_list Literal x0 x1
put_list_qrx(Config) ->
    [Config,42.0|Config].

%% put_list Literal x0 y0
put_list_qry(Config) ->
    Res = [100.0|Config],
    id(0),
    Res.

%% put_list Literal x1 x0
put_list_qxr(_, Config) ->
    [127.0|Config].

%% put_list Literal x1 x2
put_list_qxx(_, Config) ->
    [Config,130.0|Config].

%% put_list Literal x1 y0
put_list_qxy(Config) ->
    Res = [99.0|Config],
    id(0),
    Res.

%% put_list Literal y0 x0
put_list_qyr(Config) ->    
    id(Config),
    [200.0|Config].

%% put_list Literal y0 x1
put_list_qyx(Config) ->    
    id(Config),
    [Config,210.0|Config].

%% put_list Literal y1 y0
put_list_qyy(Config) ->
    id(Config),
    Res = [300.0|Config],
    id(Config),
    [Res|Config].

fconv(Config) when is_list(Config) ->
    5.0 = fconv_1(-34444444450.0),
    13.0 = fconv_2(7.0),
    ok.

fconv_1(F) when is_float(F) ->
    34444444455 + F.

fconv_2(F) when is_float(F) ->
    6.0 + F.

literal_case_expression(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Src = filename:join(DataDir, "literal_case_expression"),
    {ok,literal_case_expression=Mod,Code} = compile:file(Src, [from_asm,binary]),
    {module,Mod} = code:load_binary(Mod, Src, Code),
    ok = Mod:x(),
    ok = Mod:y(),
    ok = Mod:zi1(),
    ok = Mod:zi2(),
    ok = Mod:za1(),
    ok = Mod:za2(),
    true = code:delete(Mod),
    code:purge(Mod),
    ok.

%% Test the i_increment instruction.
increment(Config) when is_list(Config) ->
    %% In the 32-bit emulator, Neg32 can be represented as a small,
    %% but -Neg32 cannot. Therefore the i_increment instruction must
    %% not be used in the subtraction that follows (since i_increment
    %% cannot handle a bignum literal).
    Neg32 = -(1 bsl 27),
    Big32 = id(1 bsl 32),
    Result32 = (1 bsl 32) + (1 bsl 27),
    Result32 = Big32 + (1 bsl 27),
    Result32 = Big32 - Neg32,

    %% Same thing, but for the 64-bit emulator.
    Neg64 = -(1 bsl 59),
    Big64 = id(1 bsl 64),
    Result64 = (1 bsl 64) + (1 bsl 59),
    Result64 = Big64 + (1 bsl 59),
    Result64 = Big64 - Neg64,

    %% Test error handling for the i_increment instruction.
    Bad = id(bad),
    {'EXIT',{badarith,_}} = (catch Bad + 42),

    %% Small operands, but a big result.
    Res32 = 1 bsl 27,
    Small32 = id(Res32-1),
    Res32 = Small32 + 1,
    Res64 = 1 bsl 59,
    Small64 = id(Res64-1),
    Res64 = Small64 + 1,
    ok.

%% Help functions.

chksum(Term) ->
    chksum(Term, 0).

chksum([List|T], Sum) when is_list(List) ->
    chksum(T, chksum(List, Sum));
chksum([H|T], Sum) ->
    chksum(T, chksum(H, Sum));
chksum([], Sum) -> Sum;
chksum(Tuple, Sum) when is_tuple(Tuple) ->
    chksum(tuple_to_list(Tuple), Sum);
chksum(Int, Sum) when is_integer(Int) ->
    Sum * 5 + Int;
chksum(Other, Sum) ->
    erlang:phash2([Other|Sum], 39729747).

id(I) -> I.

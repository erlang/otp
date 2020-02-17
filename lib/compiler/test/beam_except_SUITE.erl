%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2018. All Rights Reserved.
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
-module(beam_except_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 multiple_allocs/1,bs_get_tail/1,coverage/1,
         binary_construction_allocation/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,[parallel],
      [multiple_allocs,
       bs_get_tail,
       coverage,
       binary_construction_allocation]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

multiple_allocs(_Config) ->
    {'EXIT',{{badmatch,#{true:=[p]}},_}} =
	 (catch could(pda, 0.0, {false,true}, {p})),
    {'EXIT',{function_clause,_}} = (catch place(lee)),
    {'EXIT',{{badmatch,wanted},_}} = (catch conditions()),

    ok.

could(Coupons = pda, Favorite = _pleasure = 0.0, {_, true}, {Presents}) ->
  (0 = true) = #{true => [Presents]}.

place(lee) ->
    (pregnancy = presentations) = [hours | [purchase || _ <- 0]] + wine.

conditions() ->
    (talking = going) = storage + [large = wanted].

bs_get_tail(Config) ->
    {<<"abc">>,0,0,Config} = bs_get_tail_1(id(<<0:32, "abc">>), 0, 0, Config),
    {'EXIT',
     {function_clause,
      [{?MODULE,bs_get_tail_1,[<<>>,0,0,Config],_}|_]}} =
        (catch bs_get_tail_1(id(<<>>), 0, 0, Config)),

    ok = bs_get_tail_2(<<"W">>, <<"X">>, <<"Z">>),
    ok = bs_get_tail_2(<<"M">>, <<"X">>, <<"Z">>),
    {'EXIT',
     {function_clause,
      [{?MODULE,do_get_bs_tail_2,[<<"A">>,<<"B">>,[],<<"C">>],_}|_]}} =
        (catch bs_get_tail_2(<<"A">>, <<"B">>, <<"C">>)),

    ok.

bs_get_tail_1(<<_:32, Rest/binary>>, Z1, Z2, F1) ->
    {Rest,Z1,Z2,F1}.

bs_get_tail_2(A, B, C) ->
    do_get_bs_tail_2(A, B, [], C).

do_get_bs_tail_2(<<"W">>, <<"X">>, _, <<"Z">>) -> ok;
do_get_bs_tail_2(<<"M">>, <<"X">>, _, <<"Z">>) -> ok.

coverage(_) ->
    File = {file,"fake.erl"},
    ok = fc(a),
    {'EXIT',{function_clause,
	     [{?MODULE,fc,[[x]],[File,{line,2}]}|_]}} =
	(catch fc([x])),
    {'EXIT',{function_clause,
	     [{?MODULE,fc,[y],[File,{line,2}]}|_]}} =
	(catch fc(y)),
    case ?MODULE of
        beam_except_no_opt_SUITE ->
            %% There will be a different stack fram in
            %% unoptimized code.
            ok;
        _ ->
            {'EXIT',{function_clause,
                     [{?MODULE,fc,[[a,b,c]],[File,{line,6}]}|_]}} =
                (catch fc([a,b,c]))
    end,

    {'EXIT',{undef,[{erlang,error,[a,b,c],_}|_]}} =
	(catch erlang:error(a, b, c)),

    {'EXIT',{badarith,[{?MODULE,bar,1,[File,{line,9}]}|_]}} =
	(catch bar(x)),
    {'EXIT',{{case_clause,{1}},[{?MODULE,bar,1,[File,{line,9}]}|_]}} =
	(catch bar(0)),

    Self = self(),
    {'EXIT',{{strange,Self},[{?MODULE,foo,[any],[File,{line,14}]}|_]}} =
        (catch foo(any)),

    {ok,succeed,1,2} = foobar(succeed, 1, 2),
    {'EXIT',{function_clause,[{?MODULE,foobar,[[fail],1,2],
                               [{file,"fake.erl"},{line,16}]}|_]}} =
        (catch foobar([fail], 1, 2)),
    {'EXIT',{function_clause,[{?MODULE,fake_function_clause,[{a,b},42.0],_}|_]}} =
        (catch fake_function_clause({a,b})),

    {'EXIT',{{badmatch,0.0},_}} = (catch coverage_1(id(42))),
    {'EXIT',{badarith,_}} = (catch coverage_1(id(a))),
    ok.

coverage_1(X) ->
    %% ERL-1167: Would crash beam_except.
    true = 0 / X.

fake_function_clause(A) -> error(function_clause, [A,42.0]).


binary_construction_allocation(_Config) ->
    ok = do_binary_construction_allocation("PUT"),
    ok.

do_binary_construction_allocation(Req) ->
    %% Allocation for building the error term was done by the
    %% bs_init2 instruction. beam_except crashed because it expected
    %% an explicit allocation instruction.
    ok = case Req of
             "POST" -> {error, <<"BAD METHOD ", Req/binary>>, Req};
             _ -> ok
         end.

id(I) -> I.

-file("fake.erl", 1).
fc(a) ->	                                %Line 2
    ok;						%Line 3
fc(L) when length(L) > 2 ->			%Line 4
    %% Not the same as a "real" function_clause error.
    error(function_clause, [L]).		%Line 6
%% Would crash the compiler.
bar(X) ->					%Line 8
    case {X+1} of				%Line 9
	1 -> ok					%Line 10
    end.					%Line 11
%% Cover collection code for function_clause exceptions.
foo(A) ->                                       %Line 13
    error({strange,self()}, [A]).               %Line 14
%% Cover beam_except:tag_literal/1.
foobar(A, B, C) when is_atom(A) ->              %Line 16
    {ok,A,B,C}.                                 %Line 17

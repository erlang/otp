%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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
%%%----------------------------------------------------------------
%%% Purpose:Test Suite for the 'qlc' module.
%%%-----------------------------------------------------------------
-module(qlc_SUITE).

-define(QLC, qlc).
-define(QLCs, "qlc").

%%-define(debug, true).

%% There are often many tests per testcase. Most tests are copied to a
%% module, a file. The file is compiled and the test run. Should the
%% test fail, the module file is not removed from ?privdir, but is
%% kept for inspection. The name of the file is
%% ?privdir/qlc_test_CASE.erl.
-define(TESTMODULE, qlc_test).
-define(TESTCASE, testcase_name).

-ifdef(debug).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(datadir, ?QLCs ++  "_SUITE_data").
-define(privdir, ?QLCs ++ "_SUITE_priv").
-define(testcase, current_testcase). % don't know
-define(t, test_server).
-else.
-include_lib("common_test/include/ct.hrl").
-define(datadir, proplists:get_value(data_dir, Config)).
-define(privdir, proplists:get_value(priv_dir, Config)).
-define(testcase, proplists:get_value(?TESTCASE, Config)).
-endif.

-include_lib("stdlib/include/ms_transform.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([ 
	  badarg/1, nested_qlc/1, unused_var/1, lc/1, fun_clauses/1,
	  filter_var/1, single/1, exported_var/1, generator_vars/1,
	  nomatch/1, errors/1, pattern/1, overridden_bif/1,

	  eval/1, cursor/1, fold/1, eval_unique/1, eval_cache/1, append/1, 
	  evaluator/1, string_to_handle/1, table/1, process_dies/1, 
	  sort/1, keysort/1, filesort/1, cache/1, cache_list/1, filter/1, 
	  info/1, nested_info/1, lookup1/1, lookup2/1, lookup_rec/1, 
	  indices/1, pre_fun/1, skip_filters/1,

	  ets/1, dets/1,

	  join_option/1, join_filter/1, join_lookup/1, join_merge/1,
	  join_sort/1, join_complex/1,

	  otp_5644/1, otp_5195/1, otp_6038_bug/1, otp_6359/1, otp_6562/1,
	  otp_6590/1, otp_6673/1, otp_6964/1, otp_7114/1, otp_7238/1,
	  otp_7232/1, otp_7552/1, otp_6674/1, otp_7714/1, otp_11758/1,
          otp_12946/1,

	  manpage/1,

	  backward/1, forward/1,

	  eep37/1]).

%% Internal exports.
-export([bad_table_throw/1, bad_table_exit/1, default_table/1, bad_table/1,
         bad_table_format/1, bad_table_format_arity/1, bad_table_traverse/1,
         bad_table_post/1, bad_table_lookup/1, bad_table_max_lookup/1,
         bad_table_info_arity/1, bad_table_info_fun_n_objects/1,
         bad_table_info_fun_indices/1, bad_table_info_fun_keypos/1,
         bad_table_key_equality/1]).
-export([evaluator_2/2]).
-export([prep_scratchdir/1, truncate_tmpfile/2, crash/2, crash_tmpfile/2]).
-export([etsc/2, etsc/3, create_ets/2, lookup_keys/1]).
-export([strip_qlc_call/1, join_info/1, join_info_count/1]).
-export([i/1, i/2, format_info/2]).

-export([table_kill_parent/2, table_parent_throws/2, 
         table_parent_exits/2, table_bad_parent_fun/2]).
-export([table/2, table/3, stop_list/2, table_error/2, table_error/3, 
         table_lookup_error/1]).

%% error_logger
-export([install_error_logger/0, uninstall_error_logger/0, 
         read_error_logger/0]).
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

init_per_testcase(Case, Config) ->
    [{?TESTCASE, Case} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() -> 
    [{group, parse_transform}, {group, evaluation},
     {group, table_impls}, {group, join}, {group, tickets},
     manpage, {group, compat}].

groups() -> 
    [{parse_transform, [],
      [badarg, nested_qlc, unused_var, lc, fun_clauses,
       filter_var, single, exported_var, generator_vars,
       nomatch, errors, pattern, overridden_bif]},
     {evaluation, [],
      [eval, cursor, fold, eval_unique, eval_cache, append,
       evaluator, string_to_handle, table, process_dies, sort,
       keysort, filesort, cache, cache_list, filter, info,
       nested_info, lookup1, lookup2, lookup_rec, indices,
       pre_fun, skip_filters, eep37]},
     {table_impls, [], [ets, dets]},
     {join, [],
      [join_option, join_filter, join_lookup, join_merge,
       join_sort, join_complex]},
     {tickets, [],
      [otp_5644, otp_5195, otp_6038_bug, otp_6359, otp_6562,
       otp_6590, otp_6673, otp_6964, otp_7114, otp_7232,
       otp_7238, otp_7552, otp_6674, otp_7714, otp_11758, otp_12946]},
     {compat, [], [backward, forward]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

badarg(Config) when is_list(Config) ->
    Ts =
	[{badarg,
	  <<"-import(qlc, [q/1, q/2]).
          q(_, _, _) -> ok.

badarg() ->
    qlc:q(foo),
    qlc:q(foo, cache_all),
    qlc:q(foo, cache_all, extra),
    q(bar),
    q(bar, cache_all),
    q(bar, cache_all, extra).
">>,
       [],
{errors,[{5,?QLC,not_a_query_list_comprehension},
	 {6,?QLC,not_a_query_list_comprehension},
	 {8,?QLC,not_a_query_list_comprehension},
	 {9,?QLC,not_a_query_list_comprehension}],
 []}}],
    [] = compile(Config, Ts),
    ok.

%% Nested qlc expressions.
nested_qlc(Config) when is_list(Config) ->
    %% Nested QLC expressions. X is bound before the first one; Z and X
    %% before the second one.
    Ts = 
     [{nested_qlc1,
       <<"nested_qlc() ->
              X = 3, % X unused
              Q = qlc:q([Y || 
                            X <- % X shadowed
                                begin Z = 3, 
                                      qlc:q([Y || 
                                                Y <- [Z],
                                                X <- [1,2,3], % X shadowed
                                                X < Y])
                                end,
                            Y <- 
                                [y],
                            Y > X]),
              [y, y] = qlc:e(Q),
              ok.
       ">>,
       [warn_unused_vars],
       {warnings,[{{2,15},erl_lint,{unused_var,'X'}},
                  {{4,29},erl_lint,{shadowed_var,'X',generate}},
                  {{8,49},erl_lint,{shadowed_var,'X',generate}}]}},

      {nested_qlc2,
      <<"nested_qlc() ->
             H0 = qlc:append([a,b], [c,d]),
             qlc:q([{X,Y} || 
                       X <- H0,
                       Y <- qlc:q([{X,Y} || 
                                      X <- H0, % X shadowed
                                      Y <- H0])]),
             ok.
       ">>,
       [warn_unused_vars],
       {warnings,[{{6,39},erl_lint,{shadowed_var,'X',generate}}]}}
    ],
    [] = compile(Config, Ts),
    ok.

%% Unused variable with a name that should not be introduced.
unused_var(Config) when is_list(Config) ->
    Ts = 
     [{unused_var,
       <<"unused_var() ->
              qlc:q([X || begin Y1 = 3, true end, % Y1 unused
                          Y <- [1,2,3],
                          X <- [a,b,c],
                          X < Y]).
       ">>,
       [warn_unused_vars],
       {warnings,[{{2,33},erl_lint,{unused_var,'Y1'}}]}}],
    [] = compile(Config, Ts),
    ok.

%% Ordinary LC expression.
lc(Config) when is_list(Config) ->
    Ts = 
     [{lc,
       <<"lc() ->
              [X || X <- [], X <- X]. % X shadowed
        ">>,
       [],
       {warnings,[{{2,30},erl_lint,{shadowed_var,'X',generate}}]}}],
    [] = compile(Config, Ts),
    ok.
           
%% Fun with several clauses.
fun_clauses(Config) when is_list(Config) ->
    Ts = 
     [{fun_clauses,
       <<"fun_clauses() ->
            {X,X1,X2} = {1,2,3},
            F = fun({X}) -> qlc:q([X || X <- X]); % X shadowed (fun, generate)
                   ([X]) -> qlc:q([X || X <- X])  % X shadowed (fun, generate)
                end,
            {F,X,X1,X2}.
        ">>,
       [],
       {warnings,[{{3,22},erl_lint,{shadowed_var,'X','fun'}},
                  {{3,41},erl_lint,{shadowed_var,'X',generate}},
                  {{4,22},erl_lint,{shadowed_var,'X','fun'}},
                  {{4,41},erl_lint,{shadowed_var,'X',generate}}]}}],
    [] = compile(Config, Ts),
    ok.

%% Variable introduced in filter.
filter_var(Config) when is_list(Config) ->
    Ts = 
     [{filter_var,
       <<"filter_var() ->
              qlc:q([X || 
                  Y <- [X || 
                           X <- [1,2,3]],
                  begin X = Y, true end]).
        ">>,
       [],
       []},

      {unsafe_filter_var,
       <<"unsafe_filter_var() ->
              qlc:q([{X,V} || X <- [1,2],
                  case {a} of
                      {_} ->
                          true;
                      V ->
                          V
                  end]).
        ">>,
       [],
       {errors,[{{2,25},erl_lint,{unsafe_var,'V',{'case',{3,19}}}}],[]}}],
    [] = compile(Config, Ts),
    ok.


%% Unused pattern variable.
single(Config) when is_list(Config) ->
    Ts = 
     [{single,
       <<"single() ->
              qlc:q([X || {X,Y} <- [{1,2}]]), % Y unused
              qlc:q([[] || [] <- [[]]]).
        ">>,
       [warn_unused_vars],
       {warnings,[{{2,30},erl_lint,{unused_var,'Y'}}]}}],
    [] = compile(Config, Ts),
    ok.

%% Exported variable in list expression (rhs of generator).
exported_var(Config) when is_list(Config) ->
    Ts = 
     [{exported_var,
       <<"exported() ->
              qlc:q([X || X <- begin
                                   case foo:bar() of
                                       1 -> Z = a;
                                       2 -> Z = b
                                   end,
                                   [Z = 3, Z = 3] % Z exported (twice...)
                               end
                         ]).
       ">>,
       [warn_export_vars],
       {warnings,[{{7,37},erl_lint,{exported_var,'Z',{'case',{3,36}}}},
                  {{7,44},erl_lint,{exported_var,'Z',{'case',{3,36}}}}]}}],
    [] = compile(Config, Ts),
    ok.

%% Errors for generator variable used in list expression.
generator_vars(Config) when is_list(Config) ->
    Ts = 
      [{generator_vars,
        <<"generator_vars() ->
               qlc:q([X ||
                      Z <- [1,2],
                      X <- begin
                               case 1 of
                                   1 -> Z = a; % used_generator_variable
                                   2 -> Z = b % used_generator_variable
                               end,
                               [Z = 3, Z = 3] % used_generator_variable (2)
                           end
                     ]).
        ">>,
        [],
        {errors,[{{6,41},?QLC,{used_generator_variable,'Z'}},
                 {{7,41},?QLC,{used_generator_variable,'Z'}},
                 {{9,33},?QLC,{used_generator_variable,'Z'}},
                 {{9,40},?QLC,{used_generator_variable,'Z'}}],
         []}}],
    [] = compile(Config, Ts),
    ok.

%% Unreachable clauses also found when compiling.
nomatch(Config) when is_list(Config) ->
    Ts =
      [{unreachable1,
        <<"unreachable1() ->
               qlc:q([X || X <- [1,2],
                   case X of
                       true -> false;
                       true -> true   % clause cannot match
                    end]).
        ">>,
        [],
        {warnings,[{5,v3_kernel,{nomatch_shadow,4}}]}},

       {nomatch1,
        <<"generator1() ->
              qlc:q([3 || {3=4} <- []]).
        ">>,
        [],
        %% {warnings,[{{2,27},qlc,nomatch_pattern}]}},
        {warnings,[{2,v3_core,nomatch}]}},

       {nomatch2,
        <<"nomatch() ->
                etsc(fun(E) ->
                Q = qlc:q([3 || {3=4} <- ets:table(E)]),
                [] = qlc:eval(Q),
                false = lookup_keys(Q) 
          end, [{1},{2}]).
        ">>,
        [],
        %% {warnings,[{{3,33},qlc,nomatch_pattern}]}},
        {warnings,[{3,v3_core,nomatch}]}},
 
       {nomatch3,
        <<"nomatch() ->
              etsc(fun(E) ->
                          Q = qlc:q([{A,B,C,D} || A=B={C=D}={_,_} <- 
                                                    ets:table(E)]),
                          [] = qlc:eval(Q),
                          false = lookup_keys(Q)
                    end, [{1,2},{2,3}]).
        ">>,
        [],
        %% {warnings,[{{3,52},qlc,nomatch_pattern}]}},
        {warnings,[{3,v3_core,nomatch}]}},

       {nomatch4,
        <<"nomatch() ->
              etsc(fun(E) ->
                          Q = qlc:q([{X,Y} || {<<X>>} = {<<Y>>} <- 
                                                    ets:table(E)]),
                          [] = qlc:eval(Q),
                          false = lookup_keys(Q)
                    end, [{<<34>>},{<<40>>}]).
        ">>,
        [],
        {errors,[{{3,48},erl_lint,illegal_bin_pattern}],[]}},

       {nomatch5,
        <<"nomatch() ->
               etsc(fun(E) ->
                           Q = qlc:q([t || {\"a\"++\"b\"} = {\"ac\"} <- 
                                                    ets:table(E)]),
                           [t] = qlc:eval(Q),
                           [\"ab\"] = lookup_keys(Q)
                     end, [{\"ab\"}]).
        ">>,
        [],
        {warnings,[{3,v3_core,nomatch}]}}

      ],
    [] = compile(Config, Ts),
    ok.
    

%% Errors within qlc expressions also found when compiling.
errors(Config) when is_list(Config) ->
    Ts =
      [{errors1,
        <<"errors1() ->
               qlc:q([X || X <- A]). % A unbound
        ">>,
        [],
        {errors,[{{2,33},erl_lint,{unbound_var,'A'}}],[]}}],
    [] = compile(Config, Ts),
    ok.

%% Patterns.
pattern(Config) when is_list(Config) ->
    Ts = [
      <<"%% Records in patterns. No lookup.
         L = [#a{k=#k{v=91}}],
         H = qlc:q([Q || Q = #a{k=#k{v=91}} <- qlc_SUITE:table(L, 2, [])]),
         {qlc,_,[{generate,_,{table,{call,_,_,_}}}], []} = i(H),
         L = qlc:e(H),
         {call, _, _q, [{lc,_,{var,_,'Q'}, 
                         [{generate,_,
                           {match,_,_,_},
                           {call,_,_,_}}]}]}
              = i(H, {format,abstract_code})">>,

      <<"%% No matchspec since there is a binary in the pattern.
         etsc(fun(E) ->
             Q = qlc:q([A || {<<A:3/unit:8>>} <- ets:table(E)]),
             [_] = qlc:eval(Q),
             {qlc,_,[{generate,_,{table,_}}], []} = i(Q)
         end, [{<<\"hej\">>}])">>

       ],
    run(Config, <<"-record(a, {k,v}).
                         -record(k, {t,v}).\n">>, Ts),
    ok.

%% Override a guard BIF with an imported or local function.
overridden_bif(Config) ->
    Ts = [
	  <<"[2] = qlc:e(qlc:q([P || P <- [1,2,3], port(P)])),
             [10] = qlc:e(qlc:q([P || P <- [0,9,10,11,12],
                                      (is_reference(P) andalso P > 5)])),
             Empty = gb_sets:empty(), Single = gb_sets:singleton(42),
             GbSets = [Empty,Single],
             [Single] = qlc:e(qlc:q([S || S <- GbSets, size(S) =/= 0]))
            ">>
	 ],
    run(Config, "-import(gb_sets, [size/1]).
                 -compile({no_auto_import, [size/1, is_reference/1]}).
                 port(N) -> N rem 2 =:= 0.
                 is_reference(N) -> N rem 10 =:= 0.\n", Ts),
    ok.


%% eval/2
eval(Config) when is_list(Config) ->
    ScratchDir = filename:join([?privdir, "scratch","."]),

    Ts = [<<"{'EXIT',{badarg,_}} = (catch qlc:eval(not_a_qlc)),
             H = qlc:q([X || X <- [1,2]]),
             {'EXIT',{{unsupported_qlc_handle,{qlc_handle,foo}},_}}=
                       (catch qlc:e({qlc_handle,foo})),
             {'EXIT',{badarg,_}} = (catch qlc:eval(H, [{unique_all,badarg}])),
             {'EXIT',{badarg,_}} = 
                      (catch qlc:eval(H, [{spawn_options,badarg}])),
             {'EXIT',{badarg,_}} = 
                      (catch qlc:eval(H, [{unique_all,true},{bad,arg}])),
             {throw,t} = 
                (catch {any_term,qlc:e(qlc:q([X || X <- throw({throw,t})]))}),
             M = qlc,
             {'EXIT',{badarg,_}} = (catch M:q(bad))">>,

          [<<"Dir = \"">>,ScratchDir,<<"\",
              qlc_SUITE:prep_scratchdir(Dir),

              E = ets:new(foo, []),
              [true || I <- lists:seq(1, 50000), not ets:insert(E, {I, I})],
              H = qlc:q([{X,Y} || Y <- [1,2], 
                                  X <- qlc:sort(ets:table(E),{tmpdir,Dir}),
                                  qlc_SUITE:truncate_tmpfile(Dir, 0)]),
              R = qlc:eval(H),
              ets:delete(E),
              {error,_,{bad_object,_}} = R,
              \"the tempo\" ++ _ = lists:flatten(qlc:format_error(R))">>],

          [<<"Dir = \"">>,ScratchDir,<<"\",
              qlc_SUITE:prep_scratchdir(Dir),

              E = ets:new(foo, []),
              Bin = term_to_binary(lists:seq(1,20000)),
              [true || I <- lists:seq(1, 10), not ets:insert(E, {I, I, Bin})],
              H = qlc:q([{X,Y} || Y <- [1,2], 
                                  X <- qlc:sort(ets:table(E),{tmpdir,Dir}),
                                  qlc_SUITE:crash_tmpfile(Dir, 5)]),
              R = qlc:eval(H),
              ets:delete(E),
              {error,_,{bad_object,_}} = R">>],

          <<"E = ets:new(test, []),
             H = qlc:q([{X,Y} || X <- qlc_SUITE:bad_table_throw(E), 
                                 Y <- ets:table(E)]),
             R1 = (catch {any_term,qlc:eval(H, {unique_all,false})}),
             R2 = (catch {any_term,qlc:eval(H, {unique_all,true})}),
             ets:delete(E),
             true = {throw,bad_pre_fun} == R1,
             true = {throw,bad_pre_fun} == R2">>,

          <<"E = ets:new(test, []),
             H = qlc:q([{X,Y} || X <- qlc_SUITE:bad_table_exit(E), 
                                 Y <- ets:table(E)]),
             R1 = (catch qlc:eval(H, {unique_all,false})),
             R2 = (catch qlc:eval(H, {unique_all,true})),
             ets:delete(E),
             {'EXIT',{bad_pre_fun,_}} = R1,
             {'EXIT',{bad_pre_fun,_}} = R2">>,

          <<"Q = qlc:q([X || X <- [4,3,2,1,0,-1], begin 3/X > 0 end]),
             {'EXIT',{badarith,_}} = (catch qlc:eval(Q, {unique_all,false})),
             {'EXIT',{badarith,_}} = (catch qlc:eval(Q, {unique_all,true}))
            ">>,

          <<"[1,2] = qlc:eval(qlc:q([X || X <- [1,2]])),
             [1,2,3,4] = qlc:eval(qlc:append([1,2],[3,4])),
             [1,2] = qlc:eval(qlc:sort([2,1])),
             E = ets:new(foo, []),
             ets:insert(E, [{1},{2}]),
             [{1},{2}] = lists:sort(qlc:eval(ets:table(E))),
             true = ets:delete(E)">>,

          <<"H = qlc:q([X || X <- [1,2], 
                             begin F = fun() -> 
                                qlc:e(qlc:q([Y || Y <- [1,2]])) end, 
                                F() == (fun f/0)() end]),
             [1,2] = qlc:e(H),
             ok.

             f() -> [1,2].
             foo() -> bar">>,

          <<"C1_0_1 = [1,2],
             %% The PT cannot rename C to C1_0_1; another name is chosen.
             [1,2] = qlc:eval(qlc:q([C || C <- C1_0_1]))">>,

          <<"H = qlc:q([X || {X,X} <- [{1,a},{2,2},{b,b},{3,4}]]),
             [2,b] = qlc:e(H),
             H1 = qlc:q([3 || {X,X} <- [{1,a},{2,2},{b,b},{3,4}]]),
             [3,3] = qlc:e(H1)">>,

          %% Just to cover a certain line in qlc.erl (avoids returning [])
          <<"E = ets:new(foo, []),
             Bin = term_to_binary(lists:seq(1,20000)),
             [true || I <- lists:seq(1, 10), not ets:insert(E, {I, I, Bin})],
             H = qlc:q([{X,Y} || Y <- [1,2], X <- qlc:sort(ets:table(E))]),
             R = qlc:eval(H),
             ets:delete(E),
             20 = length(R)">>,

          <<"H = qlc:q([{A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W} ||
                 {A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W} <- 
                  [{a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w}]]),
             [_] = qlc:e(H)">>,

          <<"H = qlc:q([Y || Y <- [1,2]], 
                       {unique, begin [T] = qlc:e(qlc:q([X || X <- [true]],
                                                        cache)), 
                                      T end}),
             [1,2] = qlc:e(H)">>

          ],
    
    run(Config, Ts),
    ok.

%% cursor/2
cursor(Config) when is_list(Config) ->
    ScratchDir = filename:join([?privdir, "scratch","."]),
    Ts = [<<"{'EXIT',{badarg,_}} = 
                   (catch qlc:cursor(fun() -> not_a_cursor end)),
             H0 = qlc:q([X || X <- throw({throw,t})]),
             {throw,t} = (catch {any_term,qlc:cursor(H0)}),
             H = qlc:q([X || X <- [1,2]]),
             {'EXIT',{badarg,_}} = 
                       (catch qlc:cursor(H,{spawn_options, [a|b]})),
             {'EXIT',{badarg,_}} = 
                       (catch qlc:cursor(H,{bad_option,true}))">>,

          <<"{'EXIT',{badarg,_}} = (catch qlc:delete_cursor(not_a_cursor))">>,

          [<<"Dir = \"">>,ScratchDir,<<"\",
              qlc_SUITE:prep_scratchdir(Dir), % kludge
              E = ets:new(foo, []),
              [true || I <- lists:seq(1, 50000), not ets:insert(E, {I, I})],
              H = qlc:q([{X,Y} || begin put('$qlc_tmpdir', true), true end,
                                  Y <- [1,2], 
                                  X <- qlc:sort(ets:table(E),{tmpdir,Dir}),
                                  qlc_SUITE:truncate_tmpfile(Dir, 0)]),
              C = qlc:cursor(H),
              R = qlc:next_answers(C, all_remaining),
              qlc:delete_cursor(C),
              erase('$qlc_tmpdir'),
              ets:delete(E),
              {error,_,{bad_object,_}} = R">>],

          <<"H1 = qlc:q([X || X <- [1,2]]),
             C1 = qlc:cursor(H1),
             [1,2] = qlc:next_answers(C1, all_remaining),
             [] = qlc:next_answers(C1),
             [] = qlc:next_answers(C1),
             ok = qlc:delete_cursor(C1),

             H2 = qlc:append([1,2],[3,4]),
             C2 = qlc:cursor(H2),
             [1,2,3,4] = qlc:next_answers(C2, all_remaining),
             ok = qlc:delete_cursor(C2),

             H3 = qlc:sort([2,1]),
             C3 = qlc:cursor(H3),
             [1,2] = qlc:next_answers(C3, all_remaining),
             ok = qlc:delete_cursor(C3),
             
             E = ets:new(foo, []),
             ets:insert(E, [{1},{2}]),
             H4 = ets:table(E),
             C4 = qlc:cursor(H4),
             [{1},{2}] = lists:sort(qlc:next_answers(C4, all_remaining)),
             ok = qlc:delete_cursor(C4),
             true = ets:delete(E)">>,

          <<"H = qlc:q([{X,Y} || X <- [1,2], Y <- [a,b]]),
             C = qlc:cursor(H, []),
             [{1,a},{1,b}] = qlc:next_answers(C, 2),
             [{2,a}] = qlc:next_answers(C, 1),
             [{2,b}] = qlc:next_answers(C, all_remaining),
             {'EXIT',{badarg,_}} = (catch qlc:next_answers(C, -1)),
             P = self(),
             Pid1 = spawn_link(fun() -> 
                          {'EXIT',{not_cursor_owner,_}} = 
                                (catch qlc:delete_cursor(C)),
                          P ! {self(), done} end),
             Pid2 = spawn_link(fun() -> 
                          {'EXIT',{not_cursor_owner,_}} = 
                                (catch qlc:next_answers(C)),
                          P ! {self(), done} end),
             receive {Pid1, done} -> ok end,
             receive {Pid2, done} -> ok end,
             ok = qlc:delete_cursor(C),
             {'EXIT',{badarg,_}} = (catch qlc:next_answers(not_a_cursor)),
             ok = qlc:delete_cursor(C)">>,

          <<"Q = qlc:q([X || X <- [1,2,1,2,1]]),
             C1 = qlc:cursor(Q, [{unique_all,true}]),
             [1,2] = qlc:next_answers(C1, all_remaining),
             ok = qlc:delete_cursor(C1),
             C2 = qlc:cursor(Q, [{unique_all,true}]),
             [1,2] = qlc:next_answers(C2, all_remaining),
             ok = qlc:delete_cursor(C2)">>,

          <<"Q = qlc:q([X || X <- [1,2,1,2,1]]),
             C1 = qlc:cursor(Q, [{unique_all,true},{spawn_options, []}]),
             [1,2] = qlc:next_answers(C1, all_remaining),
             ok = qlc:delete_cursor(C1),
             C2 = qlc:cursor(Q, [{unique_all,true},{spawn_options, default}]),
             [1,2] = qlc:next_answers(C2, all_remaining),
             ok = qlc:delete_cursor(C2)">>,

          <<"Q = qlc:q([X || X <- [1,2,1,2,1]]),
             C1 = qlc:cursor(Q, [{unique_all,false},{spawn_options, []}]),
             [1,2,1,2,1] = qlc:next_answers(C1, all_remaining),
             ok = qlc:delete_cursor(C1),
             C2 = qlc:cursor(Q, [{unique_all,false},{spawn_options, []}]),
             [1,2,1,2,1] = qlc:next_answers(C2, all_remaining),
             ok = qlc:delete_cursor(C2)">>,

          <<"Q = qlc:q([X || X <- [1,2,1,2,1]]),
             C1 = qlc:cursor(Q, [{unique_all,false}]),
             [1,2,1,2,1] = qlc:next_answers(C1, all_remaining),
             ok = qlc:delete_cursor(C1),
             C2 = qlc:cursor(Q, [{unique_all,false}]),
             [1,2,1,2,1] = qlc:next_answers(C2, all_remaining),
             ok = qlc:delete_cursor(C2)">>

         ],
    run(Config, Ts),
    ok.

%% fold/4
fold(Config) when is_list(Config) ->
    ScratchDir = filename:join([?privdir, "scratch","."]),
    Ts = [<<"Q = qlc:q([X || X <- [1,2,1,2,1]]),
             F = fun(Obj, A) -> A++[Obj] end,
             {'EXIT',{badarg,_}} = (catch qlc:fold(F, [], Q, {bad,arg})),
             {'EXIT',{badarg,_}} = (catch qlc:fold(F, [], badarg)),
             {'EXIT',{badarg,_}} = 
               (catch qlc:fold(F, [], {spawn_options, [a|b]})),
             H = qlc:q([X || X <- throw({throw,t})]),
             {throw,t} = (catch {any_term,qlc:fold(F, [], H)}),
             [1,2] = qlc:fold(F, [], Q, {unique_all,true}),
             {'EXIT',{badarg,_}} = 
               (catch qlc:fold(F, [], Q, [{unique_all,bad}])),
             [1,2,1,2,1] = 
                  qlc:fold(F, [], Q, [{unique_all,false}])">>,

          [<<"Dir = \"">>,ScratchDir,<<"\",
              qlc_SUITE:prep_scratchdir(Dir),

              E = ets:new(foo, []),
              [true || I <- lists:seq(1, 50000), not ets:insert(E, {I, I})],
              H = qlc:q([{X,Y} || Y <- [1,2], 
                                  X <- qlc:sort(ets:table(E),{tmpdir,Dir}),
                                  qlc_SUITE:truncate_tmpfile(Dir, 0)]),
              F = fun(Obj, A) -> A++[Obj] end,
              R = qlc:fold(F, [], H),
              ets:delete(E),
              {error,_,{bad_object,_}} = R">>],

          <<"E = ets:new(test, []),
             H = qlc:q([{X,Y} || X <- qlc_SUITE:bad_table_throw(E), 
                                 Y <- ets:table(E)]),
             F = fun(Obj, A) -> A++[Obj] end,
             R1 = (catch {any_term,qlc:fold(F, [], H, {unique_all,false})}),
             R2 = (catch {any_term,qlc:fold(F, [], H, {unique_all,true})}),
             ets:delete(E),
             true = {throw,bad_pre_fun} == R1,
             true = {throw,bad_pre_fun} == R2">>,

          <<"E = ets:new(test, []),
             H = qlc:q([{X,Y} || X <- qlc_SUITE:bad_table_exit(E), 
                                 Y <- ets:table(E)]),
             F = fun(Obj, A) -> A++[Obj] end,
             R1 = (catch qlc:fold(F, [], H, {unique_all,false})),
             R2 = (catch qlc:fold(F, [], H, {unique_all,true})),
             ets:delete(E),
             {'EXIT',{bad_pre_fun,_}} = R1,
             {'EXIT',{bad_pre_fun,_}} = R2">>,

          <<"F = fun(Obj, A) -> A++[Obj] end,
             Q = qlc:q([X || X <- [1,2,1,2,1], throw({throw,wrong})]),
             {throw,wrong} = 
                 (catch {any_term,qlc:fold(F, [], Q, {unique_all,true})}),
             {throw,wrong} = 
                 (catch {any_term,qlc:fold(F, [], Q)})">>,

          <<"F = fun(Obj, A) -> A++[Obj] end,
             Q = qlc:q([X || X <- [4,3,2,1,0,-1], begin 3/X > 0 end]),
             {'EXIT',{badarith,_}} = 
                       (catch qlc:fold(F, [], Q, {unique_all,true})),
             {'EXIT',{badarith,_}} = 
                 (catch qlc:fold(F, [], Q, [{unique_all,false}]))
            ">>,

          <<"F = fun(Obj, A) -> A++[Obj] end,
             [1,2] = qlc:fold(F, [], qlc:q([X || X <- [1,2]])),
             [1,2,3,4] = qlc:fold(F, [], qlc:append([1,2],[3,4])),
             [1,2] = qlc:fold(F, [], qlc:sort([2,1])),
             E = ets:new(foo, []),
             ets:insert(E, [{1},{2}]),
             [{1},{2}] = lists:sort(qlc:fold(F, [], ets:table(E))),
             true = ets:delete(E)">>,

          <<"F = fun(_Obj, _A) -> throw({throw,fatal}) end,
             Q = qlc:q([X || X <- [4,3,2]]),
             {throw,fatal} = 
              (catch {any_term,qlc:fold(F, [], Q, {unique_all,true})}),
             {throw,fatal} = 
               (catch {any_term,qlc:fold(F, [], Q, [{unique_all,false}])})">>,

          <<"G = fun(_Obj, _A, D) -> 17/D  end,
             F = fun(Obj, A) -> G(Obj, A, 0) end,
             Q = qlc:q([X || X <- [4,3,2]]),
             {'EXIT',{badarith,_}} = 
                    (catch qlc:fold(F, [], Q, {unique_all,true})),
             {'EXIT',{badarith,_}} = 
               (catch qlc:fold(F, [], Q, [{unique_all,false}]))
            ">>
         ],
    run(Config, Ts),
    ok.

%% Test the unique_all option of eval.
eval_unique(Config) when is_list(Config) ->
    Ts = [<<"QLC1 = qlc:q([X || X <- qlc:append([[1,1,2], [1,2,3,2,3]])]),
             [1,2,3] = qlc:eval(QLC1, {unique_all,true}),
             QLC2 = qlc:q([X || X <- [1,2,1,2,1,2,1]]),
             [1,2] = qlc:e(QLC2, {unique_all,true})">>,

          <<"E = ets:new(test, []),
             true = ets:insert(E, [{1,a},{2,b},{3,c}]),
             H = qlc:q([X || X <- qlc:append([ets:table(E), ets:table(E)])]),
             R1 = qlc:e(H, {unique_all,false}),
             R2 = qlc:e(H, {unique_all,true}),
             ets:delete(E),
             true = lists:sort(R1) == [{1,a},{1,a},{2,b},{2,b},{3,c},{3,c}],
             true = lists:sort(R2) == [{1,a},{2,b},{3,c}]
            ">>,

          <<"Q1 = qlc:q([{X,make_ref()} || X <- [1,2,1,2]]),
             [_,_] = qlc:e(Q1, {unique_all,true}),
             [_,_,_,_] = qlc:e(Q1, {unique_all,false}),
             [_,_] = qlc:e(Q1, [{unique_all,true}]),
             Q2 = qlc:q([{X,make_ref()} || X <- qlc:append([[1,2,1,2]])]),
             [_,_] = qlc:e(Q2, {unique_all,true}),
             [_,_,_,_] = qlc:e(Q2, {unique_all,false}),
             [_,_] = qlc:e(Q2, [{unique_all,true}])
            ">>,

          <<"Q = qlc:q([{X,make_ref()} || X <- qlc:sort([1,2,1,2])]),
             [_, _] = qlc:e(Q, {unique_all,true}),
             Q1 = qlc:q([X || X <- [1,2,1,2]]),
             Q2 = qlc:q([{X,make_ref()} || X <- qlc:sort(Q1)]),
             [_, _] = qlc:e(Q2, {unique_all,true})
            ">>,

          <<"E = ets:new(test, []),
             true = ets:insert(E, [{1,a},{2,b},{3,c}]),
             H = qlc:q([X || X <- qlc:append([[1,2,1,2]])]),
             [1,2,1,2] = qlc:e(H, {unique_all,false}),
             [1,2] = qlc:e(H, {unique_all,true}),
             ets:delete(E)">>,

          <<"E = ets:new(foo, [duplicate_bag]),
             true = ets:insert(E, [{1,a},{1,a},{2,b},{3,c},{4,c},{4,d}]),
             Q1 = qlc:q([{X,make_ref()} || {_, X} <- ets:table(E)]),
             true = length(qlc:eval(Q1, {unique_all, true})) =:= 5,
             Q2 = qlc:q([X || {_, X} <- ets:table(E)]),
             true = length(qlc:eval(Q2, {unique_all, true})) =:= 4,
             Q3 = qlc:q([element(2, X) || X <- ets:table(E)]),
             true = length(qlc:eval(Q3, {unique_all, true})) =:= 4,
             Q4 = qlc:q([1 || _X <- ets:table(E)]),
             true = length(qlc:eval(Q4, {unique_all, true})) =:= 1,
             true = ets:delete(E)
            ">>,

          <<"Q1 = qlc:q([X || X <- qlc:append([[1], [2,1]])]),
             Q2 = qlc:q([X || X <- qlc:append([[2,1], [2]])]),
             Q3 = qlc:q([{X,Y} || X <- Q1, Y <- Q2]),
             [{1,2},{1,1},{2,2},{2,1}] = qlc:e(Q3, {unique_all,true}),
             Q4 = qlc:q([{X,Y,make_ref()} || X <- Q1, Y <- Q2]),
             [{1,2,_},{1,1,_},{2,2,_},{2,1,_}] = qlc:e(Q4, {unique_all,true})
            ">>,

          <<"Q1 = qlc:q([X || X <- [1,2,1]]),
             Q2 = qlc:q([X || X <- [2,1,2]]),
             Q3 = qlc:q([{X,Y} || X <- Q1, Y <- Q2]),
             [{1,2},{1,1},{2,2},{2,1}] = qlc:e(Q3,{unique_all,true}),
             Q4 = qlc:q([{X,Y,make_ref()} || X <- Q1, Y <- Q2]),
             [{1,2,_},{1,1,_},{2,2,_},{2,1,_}] = qlc:e(Q4, {unique_all,true})
            ">>,

          <<"Q1 = qlc:q([X || {X,_} <- [{1,a},{1,b}]]),
             [1] = qlc:e(Q1, {unique_all, true}),
             Q2 = qlc:q([a || _ <- [{1,a},{1,b}]]),
             [a] = qlc:e(Q2, {unique_all, true})
            ">>,

          <<"Q = qlc:q([SQV || SQV <- qlc:q([X || X <- [1,2,1,#{a => 1}]],
                                             unique)],
                       unique),
             {call,_,_,[{lc,_,{var,_,'X'},[{generate,_,{var,_,'X'},_}]},_]} =
                 qlc:info(Q, [{format,abstract_code},unique_all]),
             [1,2,#{a := 1}] = qlc:e(Q)">>,

          <<"Q = qlc:q([X || X <- [1,2,1]]),
             {call,_,_,[{lc,_,{var,_,'X'},[{generate,_,{var,_,'X'},_}]},_]} =
                 qlc:info(Q, [{format,abstract_code},unique_all]),
             [1,2] = qlc:e(Q, unique_all)">>,

          <<"Q1 = qlc:sort([{1},{2},{3},{1}], [{unique,true}]),
             Q = qlc:sort(Q1,[{unique,true}]),
             {sort,{sort,{list,_},[{unique,true}]},[]} = i(Q)">>

         ],
    run(Config, Ts),
    ok.

%% Test the cache_all and unique_all options of eval.
eval_cache(Config) when is_list(Config) ->
    Ts = [
       <<"E = ets:new(apa, [ordered_set]),
          ets:insert(E, [{1},{2}]),
          H = qlc:q([X || Y <- [3,4], 
                          ets:insert(E, {Y}),
                          X <- ets:table(E)]), % already unique, no cache...
          {qlc, _,
           [{generate, _, {qlc, _,
                           [{generate, _, {list, [3,4]}}],
                           [{unique,true}]}}, 
            _,
            {generate, _, {table,_}}],
           [{unique,true}]} = i(H, [cache_all, unique_all]),
          [{1},{2},{3},{4}] = qlc:e(H,  [cache_all, unique_all]),
          ets:delete(E)">>,

       <<"E = ets:new(apa, [ordered_set]),
          ets:insert(E, [{1},{2}]),
          H = qlc:q([X || Y <- [3,4], 
                          ets:insert(E, {Y}),
                          X <- ets:table(E)]), % no cache...
          {qlc, _,
           [{generate, _,{list, [3,4]}}, 
            _,
            {generate, _, {table,_}}],
           []} = i(H, cache_all),
          [{1},{2},{3},{1},{2},{3},{4}] = qlc:e(H,  [cache_all]),
          ets:delete(E)">>,

       <<"E = ets:new(apa, [ordered_set]),
          ets:insert(E, [{1},{2}]),
          H = qlc:q([X || Y <- [3,4], 
                          ets:insert(E, {Y}),
                          X <- qlc:q([X || X <- ets:table(E)], cache)]),
          {qlc, _,
           [{generate, _, {list, [3,4]}}, 
            _,
            {generate, _, {qlc, _,
                           [{generate, _, {table,_}}],
                           [{cache,ets}]}}],
           []} = i(H, cache_all),
          [{1},{2},{3},{1},{2},{3}] = qlc:e(H,  [cache_all]),
          ets:delete(E)">>,

       <<"%% {cache_all,no} does not override {cache,true}.
          E = ets:new(apa, [ordered_set]),
          ets:insert(E, [{1},{2}]),
          H = qlc:q([X || Y <- [3,4], 
                          ets:insert(E, {Y}),
                          X <- qlc:q([X || X <- ets:table(E)], cache)]),
          {qlc, _,
           [{generate, _, {list, [3,4]}}, 
            _,
            {generate, _, {qlc, _,
                           [{generate, _, {table,_}}],
                           [{cache,ets}]}}],
           []} = i(H, {cache_all,no}),
          [{1},{2},{3},{1},{2},{3}] = qlc:e(H,  [cache_all]),
          ets:delete(E)">>,

       <<"E = ets:new(apa, [ordered_set]),
          ets:insert(E, [{1},{2}]),
          H = qlc:q([X || Y <- [3,4], 
                          ets:insert(E, {Y}),
                          X <- ets:table(E)]),
          {qlc, _,
           [{generate, _, {qlc, _, [{generate, _,{list, [3,4]}}],
                           [{unique,true}]}}, 
            _,
            {generate, _,{table,_}}],
           [{unique,true}]} = i(H, unique_all),
          [{1},{2},{3},{4}] = qlc:e(H, [unique_all]),
          ets:delete(E)">>,

          %% cache_all is ignored 
       <<"E = ets:new(apa, [ordered_set]),
          ets:insert(E, [{1},{2},{0}]),
          H = qlc:q([X || X <- qlc:sort(ets:table(E))]),
          {sort,_Table,[]} = i(H, cache_all),
          [{0},{1},{2}] = qlc:e(H, cache_all),
          ets:delete(E)">>,

       <<"F = fun(Obj, A) -> A++[Obj] end,
          E = ets:new(apa, [duplicate_bag]),
          true = ets:insert(E, [{1,a},{2,b},{1,a}]),
          Q = qlc:q([X || X <- ets:table(E)], cache),
          {table, _} = i(Q, []),
          R = qlc:fold(F, [], Q, []),
          ets:delete(E),
          true = [{1,a},{1,a},{2,b}] == lists:sort(R)">>,

       <<"E = ets:new(apa, [ordered_set]),
          ets:insert(E, [{1},{2},{0}]),
          H = qlc:q([X || X <- ets:table(E)], cache),
          {table, _} = i(H, cache_all),
          [{0},{1},{2}]= qlc:e(H, cache_all),
          ets:delete(E)">>,

       <<"E = ets:new(foo, []),
          true = ets:insert(E, [{1}, {2}]),
          Q1 = qlc:q([{X} || X <- ets:table(E)]),
          Q2 = qlc:q([{X,Y} || {X} <- Q1, {Y} <- Q1]),
          {qlc, _, [{generate, _, {table, _}},
                    {generate, _, {qlc, _, [{generate, _, {table, _}}],
                                   [{cache,ets}]}}],
           []} = i(Q2, cache_all),
          [{{1},{1}},{{1},{2}},{{2},{1}},{{2},{2}}] = 
              lists:sort(qlc:e(Q2, cache_all)),
          ets:delete(E)">>,

       <<"L1 = [1,2,3], 
          L2 = [4,5,6],
          Q1 = qlc:append(L1, L2),
          Q2 = qlc:q([{X} || X <- Q1]),
          {qlc, _,[{generate, _,{append, [{list, L1}, {list, L2}]}}], []} = 
              i(Q2, [cache_all]),
          [{1},{2},{3},{4},{5},{6}] = qlc:e(Q2, [cache_all])">>,

       <<"H = qlc:sort(qlc:q([1 || _ <- [a,b]])),
          {sort, {qlc, _, [{generate, _, {qlc, _, [{generate, _, 
                                                    {list, [a,b]}}],
                                          [{unique,true}]}}],
                  [{unique,true}]},
                 []} = i(H, unique_all),
          [1] = qlc:e(H, unique_all)">>

         ],
    run(Config, Ts),
    ok.

%% Test the append function.
append(Config) when is_list(Config) ->
    Ts = [<<"C = qlc:cursor(qlc:q([X || X <- [0,1,2,3], begin 10/X > 0.0 end])),
             R = (catch qlc:next_answers(C)),
             {'EXIT',{badarith,_}} = R">>,

          <<"C = qlc:cursor(qlc:q([X || X <- [0 | fun() -> exit(bad) end]])),
             R = (catch qlc:next_answers(C)),
             {'EXIT',bad} = R">>,

          <<"{'EXIT',{badarg,_}} = (catch qlc:append([a], a)),
             {'EXIT',{badarg,_}} = (catch qlc:append([[a],a]))">>,

          <<"C = qlc:cursor(qlc:q([X || X <- [0,1,2,3], 
                                     begin throw({throw,wrong}), true end])),
             {throw,wrong} = (catch {any_term,qlc:next_answers(C)})">>,

          <<"QLC = qlc:q([X || X <- [0,1,2,3], 
                               begin throw({throw,wrong}), true end]),
             {throw,wrong} = (catch {any_term,qlc:eval(QLC)}),
             {throw,wrong} = 
                 (catch {any_term,qlc:e(QLC, {unique_all,true})})">>,

          <<"H1 = qlc:q([X || X <- [1,2,3]]),
             H2 = qlc:q([X || X <- [4,5,6]]),
             R = qlc:e(qlc:q([X || X <- qlc:append([H1, H2])])),
             true = R == [1,2,3,4,5,6]">>,

          <<"H1 = [1,2,3],
             H2 = qlc:q([X || X <- [4,5,6]]),
             R = qlc:e(qlc:q([X || X <- qlc:append(H1, H2)])),
             true = R == [1,2,3,4,5,6]">>,

          <<"H1 = qlc:q([X || X <- [1,2,3]]),
             H2 = qlc:q([X || X <- [4,5,6]]),
             R = qlc:e(qlc:q([X || X <- qlc:append(qlc:e(H1), H2)])),
             true = R == [1,2,3,4,5,6]">>,

          <<"H1 = qlc:q([X || X <- [1,2,3]]),
             H2 = [4,5,6],
             R = qlc:e(qlc:q([X || X <- qlc:append(H1, H2)])),
             true = R == [1,2,3,4,5,6]">>,

          <<"H1 = qlc:q([X || X <- [1,2,3]]),
             H2 = qlc:q([X || X <- [4,5,6]]),
             R = qlc:e(qlc:q([X || X <- qlc:append([H1, H2, H1]), X < 5])),
             true = R == [1,2,3,4,1,2,3]">>,

          <<"R = qlc:e(qlc:q([X || X <- qlc:append([lista(), anrop()])])),
             true = R == [a,b,1,2],
             ok.

             lista() ->
                 [a,b].

             anrop() ->
                  qlc:q([X || X <- [1,2]]).
             foo() -> bar">>,

          %% Used to work up to R11B.
          %% <<"apa = qlc:e(qlc:q([X || X <- qlc:append([[1,2,3], ugly()])])),
          %%   ok.
          %%
          %%   ugly() ->
          %%       [a | apa].
          %%   foo() -> bar">>,

          
          %% Maybe this one should fail.
          <<"[a|b] = qlc:e(qlc:q([X || X <- qlc:append([[a|b]])])),
             ok">>,

          <<"17 = qlc:e(qlc:q([X || X <- qlc:append([[1,2,3],ugly2()])])),
             ok.

             ugly2() ->
                 [a | fun() -> 17 end].
             foo() -> bar">>,

          <<"E = ets:new(test, []),
             true = ets:insert(E, [{1,a},{2,b},{3,c}]),
             H = qlc:q([X || X <- qlc:append([ets:table(E), apa])]),
             {'EXIT',{badarg,_}} = (catch qlc:e(H)),
             false = ets:info(E, safe_fixed),
             {'EXIT',{badarg,_}} = (catch qlc:e(H)),
             false = ets:info(E, safe_fixed),
             {'EXIT',{badarg,_}} = (catch qlc:cursor(H)),
             false = ets:info(E, safe_fixed),
             F = fun(Obj, A) -> A++[Obj] end,
             {'EXIT',{badarg,_}} = (catch qlc:fold(F, [], H)),
             false = ets:info(E, safe_fixed),
             ets:delete(E)">>,

          <<"H1 = qlc:q([X || X <- [1,2,3]]),
             H2 = qlc:q([X || X <- [a,b,c]]),
             R = qlc:e(qlc:q([X || X <- qlc:append(H1,qlc:append(H1,H2))])),
             true = R == [1,2,3,1,2,3,a,b,c]">>,

          <<"H = qlc:q([X || X <- qlc:append([],qlc:append([[], []]))]),
             [] = qlc:e(H)">>,

          <<"Q1 = qlc:q([X || X <- [3,4,4]]),
             Q2 = qlc:q([X || X <- qlc:sort(qlc:append([[1,2], Q1]))]),
             [1,2,3,4,4] = qlc:e(Q2),
             [1,2,3,4] = qlc:e(Q2, {unique_all,true})">>,

          <<"[] = qlc:e(qlc:q([X || X <- qlc:append([])]))">>,

          <<"Q1 = qlc:q([X || X <- [a,b]]),
             Q2 = qlc:q([X || X <- [1,2]]),
             Q3 = qlc:append([Q1, Q2, qlc:sort([2,1])]),
             Q = qlc:q([X || X <- Q3]),
             {append, [{list, [a,b]}, 
                       {list, [1,2]}, 
                       {sort,{list, [2,1]},[]}]} = i(Q),
             [a,b,1,2,1,2] = qlc:e(Q)">>

          ],
    run(Config, Ts),
    ok.

%% Simple call from evaluator.
evaluator(Config) when is_list(Config) ->
    true = is_alive(),
    evaluator_2(Config, []),
    {ok, Node} = start_node(qlc_SUITE_evaluator),
    ok = rpc:call(Node, ?MODULE, evaluator_2, [Config, [compiler]]),
    test_server:stop_node(Node),
    ok.

evaluator_2(Config, Apps) ->
    lists:foreach(fun(App) -> true = code:del_path(App) end, Apps),
    FileName = filename:join(?privdir, "eval"),
    ok = file:write_file(FileName,
                         <<"H = qlc:q([X || X <- L]),
                            [1,2,3] = qlc:e(H).">>),
    Bs = erl_eval:add_binding('L', [1,2,3], erl_eval:new_bindings()),
    ok = file:eval(FileName, Bs),

    %% The error message is "handled" a bit too much... 
    %% (no trace of erl_lint left)
    ok = file:write_file(FileName,
             <<"H = qlc:q([X || X <- L]), qlc:e(H).">>),
    {error,_} = file:eval(FileName),

    %% Ugly error message; badarg is caught by file.erl.
    ok = file:write_file(FileName,
             <<"H = qlc:q([Z || {X,Y} <- [{a,2}], Z <- [Y]]), qlc:e(H).">>),
    {error,_} = file:eval(FileName),

    _ = file:delete(FileName),
    ok.

start_node(Name) ->
    PA = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args, "-pa " ++ PA}]).

%% string_to_handle/1,2.
string_to_handle(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch qlc:string_to_handle(14)),
    {'EXIT',{badarg,_}} =
        (catch qlc:string_to_handle("[X || X <- [a].", unique_all)),
    R1 = {error, _, {_,erl_scan,_}} = qlc:string_to_handle("'"),
    "1: unterminated " ++ _ = lists:flatten(qlc:format_error(R1)),
    {error, _, {_,erl_parse,_}} = qlc:string_to_handle("foo"),
    {'EXIT',{badarg,_}} = (catch qlc:string_to_handle("foo, bar.")),
    R3 = {error, _, {_,?QLC,not_a_query_list_comprehension}} =
        qlc:string_to_handle("bad."),
    "1: argument is not" ++ _ = lists:flatten(qlc:format_error(R3)),
    R4 = {error, _, {_,?QLC,{used_generator_variable,'Y'}}} =
        qlc:string_to_handle("[X || begin Y = [1,2], true end, X <- Y]."),
    "1: generated variable 'Y'" ++ _ =
        lists:flatten(qlc:format_error(R4)),
    {error, _, {_,erl_lint,_}} = qlc:string_to_handle("[X || X <- A]."),
    H1 = qlc:string_to_handle("[X || X <- [1,2]]."),
    [1,2] = qlc:e(H1),
    H2 = qlc:string_to_handle("[X || X <- qlc:append([a,b],"
                                    "qlc:e(qlc:q([X || X <- [c,d,e]])))]."),
    [a,b,c,d,e] = qlc:e(H2),
    %% The generated fun has many arguments (erl_eval has a maximum of 20).
    H3 = qlc:string_to_handle(
           "[{A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W} ||"
           " {A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W} <- []]."),
    [] = qlc:e(H3),
    Bs1 = erl_eval:add_binding('L', [1,2,3], erl_eval:new_bindings()),
    H4 = qlc:string_to_handle("[X || X <- L].", [], Bs1),
    [1,2,3] = qlc:e(H4),
    H5 = qlc:string_to_handle("[X || X <- [1,2,1,2]].", [unique, cache]),
    [1,2] = qlc:e(H5),

    Ets = ets:new(test, []),
    true = ets:insert(Ets, [{1}]),
    Bs2 = erl_eval:add_binding('E', Ets, erl_eval:new_bindings()),
    Q = "[X || {X} <- ets:table(E)].",
    [1] = qlc:e(qlc:string_to_handle(Q, [], Bs2)),
    [1] = qlc:e(qlc:string_to_handle(Q, {max_lookup,1000}, Bs2)),
    [1] = qlc:e(qlc:string_to_handle(Q, {max_lookup,infinity}, Bs2)),
    {'EXIT',{badarg,_}} =
        (catch qlc:string_to_handle(Q, {max_lookup,-1}, Bs2)),
    {'EXIT', {no_lookup_to_carry_out, _}} =
        (catch qlc:e(qlc:string_to_handle(Q, {lookup,true}, Bs2))),
    ets:delete(Ets),

    %% References can be scanned and parsed.
    E2 = ets:new(test, [bag]),
    Ref = make_ref(),
    true = ets:insert(E2, [{Ref,Ref}]),
    S2 = "[{Val1} || {Ref1, Val1} <- ets:table("++io_lib:write(E2)++"),"
         "Ref1 =:= Ref].",
    Bs = erl_eval:add_binding('Ref', Ref, erl_eval:new_bindings()),
    [{Ref}] = qlc:e(qlc:string_to_handle(S2, [], Bs)),
    ets:delete(E2),

    ok.

%% table
table(Config) when is_list(Config) ->
    dets:start(),
    Ts = [
       <<"E = ets:new(test, []),
          {'EXIT',{badarg,_}} = 
               (catch qlc:e(qlc:q([X || X <- ets:table(E, [badarg])]))),
          [] = qlc:e(qlc:q([X || X <- ets:table(E)])),
          ets:delete(E)">>,

       <<"{'EXIT',{badarg,_}} = (catch qlc:table(not_a_fun, []))">>,

       <<"E = ets:new(test, []),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          H = qlc:q([{X,Y} || X <- ets:table(E), Y <- ets:table(E)]),
          R = qlc:e(H),
          ets:delete(E),
          [{{1,a},{1,a}},{{1,a},{2,b}},{{1,a},{3,c}},
           {{2,b},{1,a}},{{2,b},{2,b}},{{2,b},{3,c}},

           {{3,c},{1,a}},{{3,c},{2,b}},{{3,c},{3,c}}] = lists:sort(R)">>,

       <<"E = ets:new(test, []),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          H = qlc:q([X || X <- qlc:append([ets:table(E), [a,b,c], 
                                           ets:table(E)])]),
          R = qlc:e(H),
          ets:delete(E),
          [a,b,c,{1,a},{1,a},{2,b},{2,b},{3,c},{3,c}] = lists:sort(R)">>,

       <<"E = ets:new(test, []),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          false = ets:info(E, safe_fixed),
          H = qlc:q([{X,Y} || X <- ets:table(E, {n_objects, default}), 
                              Y <- ets:table(E, {n_objects, 2}), 
                              false =/= ets:info(E, safe_fixed), 
                              throw({throw,apa})]),
          {throw,apa} = (catch {any_term,qlc:e(H)}),
          false = ets:info(E, safe_fixed),
          ets:delete(E)">>,

       <<"E = ets:new(test, []),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          false = ets:info(E, safe_fixed),
          H = qlc:q([{X,Y} || X <- ets:table(E), Y <- ets:table(E), 
                              false =/= ets:info(E, safe_fixed), exit(apa)]),
          {'EXIT',apa} = (catch {any_term,qlc:e(H)}),
          false = ets:info(E, safe_fixed),
          ets:delete(E)">>,

       <<"E = ets:new(test, []),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          H = qlc:q([{X,Y} || X <- qlc_SUITE:bad_table_throw(E), 
                              Y <- ets:table(E)]),
          R = (catch {any_term,qlc:cursor(H)}),
          false = ets:info(E, safe_fixed),
          ets:delete(E),
          {throw,bad_pre_fun} = R">>,

       <<"E = ets:new(test, []),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          H = qlc:q([{X,Y} || X <- qlc_SUITE:bad_table_exit(E), 
                              Y <- ets:table(E)]),
          R = (catch {any_term,qlc:cursor(H)}),
          false = ets:info(E, safe_fixed),
          ets:delete(E),
          {'EXIT',{bad_pre_fun,_}} = R">>,

       <<"E = ets:new(test, [ordered_set]),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          H = qlc:q([X || X <- qlc_SUITE:default_table(E)]),
          R = qlc:e(H),
          ets:delete(E),
          [{1,a},{2,b},{3,c}] = R">>,

       <<"E = ets:new(test, [ordered_set]),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          H = qlc:q([X || X <- qlc_SUITE:bad_table(E)]),
          {'EXIT', {badarg, _}} = (catch qlc:e(H)),
          ets:delete(E)">>,

       %% The info tag num_of_objects is currently not used.
%%        <<"E = ets:new(test, [ordered_set]),
%%           true = ets:insert(E, [{1,a},{2,b},{3,c}]),
%%           H = qlc:q([X || X <- qlc_SUITE:bad_table_info_fun_n_objects(E)]),
%%           {'EXIT', finito} = (catch {any_term,qlc:e(H)}),
%%           ets:delete(E)">>,

       <<"E = ets:new(test, [ordered_set]),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          H = qlc:q([Y || {X,Y} <- qlc_SUITE:bad_table_info_fun_indices(E),
                          X =:= a]),
          %% This is due to lookup. If the table were traversed there
          %% would be no failure.
          {throw, apa} = (catch {any_term,qlc:e(H)}),
          ets:delete(E)">>,

       <<"E = ets:new(test, [ordered_set]),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          H = qlc:q([Y || {X,Y} <- qlc_SUITE:bad_table_info_fun_keypos(E),
                          X =:= a]),
          {'EXIT',{keypos,_}} = (catch {any_term,qlc:info(H)}),
          {'EXIT',{keypos,_}} = (catch {any_term,qlc:e(H)}),
          ets:delete(E)">>,

       begin
       MS = ets:fun2ms(fun(X) when element(1, X) > 1 -> X end),
       [<<"E = ets:new(test, []),
           true = ets:insert(E, [{1,a},{2,b},{3,c}]),
           MS = ">>, io_lib:format("~w", [MS]), <<",
           H = qlc:q([{X,Y} || X <- ets:table(E,{traverse,{select, MS}}), 
                               Y <- ets:table(E)]),
           R = qlc:e(H),
           ets:delete(E),
           [{{2,b},{1,a}},{{2,b},{2,b}},{{2,b},{3,c}},
            {{3,c},{1,a}},{{3,c},{2,b}},{{3,c},{3,c}}] = lists:sort(R)">>]
       end,

       begin % a short table
       MS = ets:fun2ms(fun(X) when element(1, X) > 1 -> X end),
       [<<"E = ets:new(test, []),
           true = ets:insert(E, [{0,b}]),
           MS =  ">>, io_lib:format("~w", [MS]), <<",
           H1 = qlc:q([X || X <- ets:table(E)]),
           R1 = qlc:e(H1),
           H2 = qlc:q([X || X <- ets:table(E, {traverse, {select, MS}})]),
           R2 = qlc:e(H2),
           ets:delete(E),
           [_] = R1,
           [] = R2">>]
       end,

       begin
       File = filename:join(?privdir, "detsfile"),
       _ = file:delete(File),
       [<<"{ok, Tab} = dets:open_file(apa, [{file,\"">>, File, <<"\"}, 
                                           {type,bag}]),
           ok = dets:insert(Tab, [{1,a},{1,b}]),
           R = qlc:e(qlc:q([X || X <- dets:table(Tab)])),
           dets:close(Tab),
           file:delete(\"">>, File, <<"\"),
           R">>]
       end,

       %% [T || P <- Table, F] turned into a match spec.
       <<"E = ets:new(apa, [duplicate_bag]),
          true = ets:insert(E, [{1,a},{2,b},{3,c},{4,d}]),
          QH = qlc:q([X || {X,_} <- ets:table(E), X > 2], unique),
          {qlc, _, [{generate, _, {table, _}}], [{unique,true}]} = i(QH),
          [3,4] = lists:sort(qlc:e(QH)),
          ets:delete(E)">>,

       <<"E = ets:new(apa, []),
          true = ets:insert(E, [{1,a},{2,b}]),
          {'EXIT', {badarg, _}} = (catch qlc_SUITE:bad_table_format(E)),
          ets:delete(E)">>,

       <<"E = ets:new(apa, []),
          true = ets:insert(E, [{1,a},{2,b}]),
          {'EXIT', {badarg, _}} = (catch qlc_SUITE:bad_table_format_arity(E)),
          ets:delete(E)">>,

       <<"E = ets:new(apa, []),
          true = ets:insert(E, [{1,a},{2,b}]),
          {'EXIT', {badarg, _}} = (catch qlc_SUITE:bad_table_info_arity(E)),
          ets:delete(E)">>,

       <<"E = ets:new(apa, []),
          true = ets:insert(E, [{1,a},{2,b}]),
          {'EXIT', {badarg, _}} = (catch qlc_SUITE:bad_table_traverse(E)),
          ets:delete(E)">>,

       <<"E = ets:new(apa, []),
          true = ets:insert(E, [{1,a},{2,b}]),
          {'EXIT', {badarg, _}} = (catch qlc_SUITE:bad_table_post(E)),
          ets:delete(E)">>,

       <<"E = ets:new(apa, []),
          true = ets:insert(E, [{1,a},{2,b}]),
          {'EXIT', {badarg, _}} = (catch qlc_SUITE:bad_table_max_lookup(E)),
          ets:delete(E)">>,

       <<"E = ets:new(apa, []),
          true = ets:insert(E, [{1,a},{2,b}]),
          {'EXIT', {badarg, _}} = (catch qlc_SUITE:bad_table_lookup(E)),
          ets:delete(E)">>,

       <<"L = [{1,a},{2,b},{3,c}],
          QH = qlc:q([element(2, X) || X <- qlc_SUITE:table(L, [2]),
                                       (element(1, X) =:= 1)
                                        or (2 =:= element(1, X))]),
          [a,b] = lists:sort(qlc:e(QH))">>,

       <<"etsc(fun(E) ->
              Q = qlc:q([{A,B} || {A,B} <- 
                                qlc:q([{B,A} || {A,B} <- ets:table(E), 
                                                (A =:= 1) or (A =:= 2),
                                                math:sqrt(B) < A])]),
              [{2,2}] = qlc:eval(Q),
              [1,2] = lookup_keys(Q)
           end, [{1,1},{2,2}])">>
       ],
    run(Config, Ts),

    Ts2 = [
       %% [T || P <- Table, F] turned into a match spec. Records needed.
       <<"E = ets:new(foo, [bag]),
          ets:insert(E, [{a,1,2},#a{b=3,c=4},{a,3}]),
          QH = qlc:q([X || X <- ets:table(E), is_record(X, a)]),
          {list,{table,_}, _} = i(QH),
          [{a,1,2},{a,3,4}] = lists:sort(qlc:eval(QH)),
          ets:delete(E)">>
       ],
    run(Config, <<"-record(a, {b,c}).\n">>, Ts2),

    ok.

%% Caller or cursor process dies.
process_dies(Config) when is_list(Config) ->
    Ts = [
       <<"E = ets:new(test, []),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          false = ets:info(E, safe_fixed),
          Parent = self(),
          F = fun() -> 
                      H = qlc:q([X || X <- ets:table(E)]),
                      qlc:cursor(H),
                      Parent ! {self(),ok}                
              end,
          Pid = spawn_link(F),
          receive {Pid,ok} -> ok end,
          timer:sleep(10),
          false = ets:info(E, safe_fixed),    
          ets:delete(E)">>,

       <<"%% This is not nice. The cursor's monitor kicks in.
          E = ets:new(test, []),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          false = ets:info(E, safe_fixed),
          Parent = self(),
          F = fun() -> 
                      H = qlc:q([X || begin
                                          process_flag(trap_exit, false),
                                          {links, [Pid]} = 
                                              process_info(self(), links),
                                          unlink(Pid),
                                          timer:sleep(1),
                                          {links, []} =
                                              process_info(self(), links),
                                          true 
                                      end, 
                                      X <- ets:table(E)]),
                      C = qlc:cursor(H),
                      qlc:next_answers(C),
                      Parent ! {self(),ok}                
              end,
          Pid = spawn_link(F),
          receive {Pid,ok} -> ok end,
          timer:sleep(10),
          false = ets:info(E, safe_fixed),    
          ets:delete(E)">>,

       <<"H = qlc:q([X || X <- [1,2]]),
          {qlc_cursor, Term} = C = qlc:cursor(H),
          PF = process_flag(trap_exit, true),
          F = fun(T) -> not is_pid(T) end,
          [Pid|_] = lists:dropwhile(F, tuple_to_list(Term)),
          exit(Pid, kill),
          timer:sleep(1),
          {'EXIT', {{qlc_cursor_pid_no_longer_exists, Pid}, _}} =
                (catch qlc:next_answers(C)),
          process_flag(trap_exit, PF)">>,
       <<"H = qlc:q([X || begin process_flag(trap_exit, true), true end, 
                          X <- [1,2]]),
          {qlc_cursor, Term} = C = qlc:cursor(H),
          PF = process_flag(trap_exit, true),
          F = fun(T) -> not is_pid(T) end,
          [Pid|_] = lists:dropwhile(F, tuple_to_list(Term)),
          [1] = qlc:next_answers(C, 1),
          exit(Pid, stop),
          timer:sleep(1),
          {'EXIT', {{qlc_cursor_pid_no_longer_exists, Pid}, _}} =
              (catch qlc:next_answers(C)),
          process_flag(trap_exit, PF)">>,
       <<"%% This is not nice. No cleanup is done...
          H = qlc:q([X || begin process_flag(trap_exit, false), true end, 
                     X <- [1,2]]),
          {qlc_cursor, Term} = C = qlc:cursor(H),
          PF = process_flag(trap_exit, true),
          F = fun(T) -> not is_pid(T) end,
          [Pid|_] = lists:dropwhile(F, tuple_to_list(Term)),
          [1] = qlc:next_answers(C, 1),
          exit(Pid, stop),
          timer:sleep(1),
          {'EXIT', {{qlc_cursor_pid_no_longer_exists, Pid}, _}} =
              (catch qlc:next_answers(C)),
          process_flag(trap_exit, PF)">>,

       <<"PF = process_flag(trap_exit, true),
          E = ets:new(test, []),
          %% Hard kill. No cleanup will be done.
          H = qlc:q([X || begin exit(self(), kill), true end, 
                          X <- ets:table(E)]),
          C = qlc:cursor(H),
          {'EXIT', {{qlc_cursor_pid_no_longer_exists, _}, _}} =
               (catch qlc:next_answers(C)),
          false = ets:info(E, safe_fixed), % - but Ets cleans up anyway.
          true = ets:delete(E),
          process_flag(trap_exit, PF)">>,

       <<"E = ets:new(test, []),
          true = ets:insert(E, [{1,a}]),
          %% The signal is caught by trap_exit. No process dies...
          H = qlc:q([X || begin exit(self(), normal), true end, 
                          X <- ets:table(E)]),
          C = qlc:cursor(H, {spawn_options, []}),
          [{1,a}] = qlc:next_answers(C),
          qlc:delete_cursor(C),
          false = ets:info(E, safe_fixed),
          true = ets:delete(E)">>,
       <<"E = ets:new(test, []),
          true = ets:insert(E, [{1,a}]),
          %% The same as last example.
          H = qlc:q([X || begin 
                              process_flag(trap_exit, true), 
                              exit(self(), normal), true 
                          end, 
                          X <- ets:table(E)]),
          C = qlc:cursor(H, {spawn_options, []}),
          [{1,a}] = qlc:next_answers(C),
          qlc:delete_cursor(C),
          false = ets:info(E, safe_fixed),
          true = ets:delete(E), ok">>,
       <<"E = ets:new(test, []),
          true = ets:insert(E, [{1,a}]),
          H = qlc:q([X || X <- ets:table(E)]),
          SpawnOpts = [link, monitor], % monitor is ignored
          {qlc_cursor, Term} = C = qlc:cursor(H, {spawn_options, SpawnOpts}),
          F = fun(T) -> not is_pid(T) end,
          [Pid|_] = lists:dropwhile(F, tuple_to_list(Term)),
          Me = self(),
          qlc_SUITE:install_error_logger(),
          Tuple = {this, tuple, is, writton, onto, the, error_logger},
          SP = spawn(fun() ->
                  Pid ! Tuple,
                  Me ! {self(), done}
                end),
          receive {SP, done} -> ok end,
          [{1,a}] = qlc:next_answers(C),
          qlc:delete_cursor(C),
          {error, _Pid, Tuple} = qlc_SUITE:read_error_logger(),
          qlc_SUITE:uninstall_error_logger(),
          false = ets:info(E, safe_fixed),
          true = ets:delete(E), ok">>

        ],
    run(Config, Ts),
    ok.

%% The sort option.
sort(Config) when is_list(Config) ->
    Ts = [
       <<"H = qlc:q([X || X <- qlc:sort([1,2,3,2], {unique,true})]),
          [1,2,3] = qlc:e(H),
          C1 = qlc:cursor(H),
          [1,2,3] = qlc:next_answers(C1, all_remaining),
          qlc:delete_cursor(C1)">>,

       <<"H = qlc:q([{X,Y} || X <- qlc:sort(qlc:q([X || X <- [1,2,3,2]]), 
                                            {unique,true}),
                              Y <- [a,b]]),
          [{1,a},{1,b},{2,a},{2,b},{3,a},{3,b}] = qlc:e(H),
          C = qlc:cursor(H),
          [{1,a},{1,b},{2,a},{2,b},{3,a},{3,b}] = 
              qlc:next_answers(C, all_remaining),
          qlc:delete_cursor(C)">>,

       <<"H = qlc:q([X || X <- qlc:sort(qlc:q([X || X <- apa]))]),
          {'EXIT',{badarg,_}} = (catch qlc:e(H))">>,

          %% An example with a side effect. The result may vary...
       <<"E = ets:new(test, [duplicate_bag]),
          true = ets:insert(E, [{1,17},{1,a}]),
          H_1 = qlc:q([X || X <- ets:table(E)]),
          H = qlc:q([X || X <- [1,2,3], ets:insert(E, {1,-X}), 
                          {_,Y} <- H_1, 
                          X > Y]),
          true = lists:sort(qlc:e(H)) == [1,2,2,3,3,3],
          true = ets:delete(E)">>,

       <<"E = ets:new(test, [duplicate_bag]),
          true = ets:insert(E, [{1,17}]),
          H_1 = qlc:q([X || X <- qlc:sort(ets:tab2list(E))]),
          %% Note: side effect in filter!
          H = qlc:q([X || X <- [1,2,3], ets:insert(E, {1,-X}),
                          {_,Y} <- H_1, X > Y]),
          [] = qlc:e(H),
          true = ets:delete(E)">>,

       <<"H = qlc:q([X || X <- qlc:sort([1,2,3], {fopp,la})]),
          {'EXIT',{badarg,_}} = (catch qlc:e(H)),
          {'EXIT',{badarg,_}} = (catch qlc:cursor(H)),
          F = fun(Obj, A) -> A++[Obj] end,
          {'EXIT',{badarg,_}} = (catch qlc:fold(F, [], H))">>,

       <<"Q1 = qlc:q([X || X <- [1,2]]),
          AL = [Q1, [1,2,3]],
          Q2 = qlc:q([X || X <- qlc:sort(qlc:append(AL))]),
          [1,1,2,2,3] = qlc:e(Q2)">>,

       <<"H = qlc:q([{X,Y} || X <- qlc:sort(qlc:q([X || X <- [1,2,3,2]]), 
                                            {unique,true}),
                              Y <- [a,b]]),
          {qlc, _,
           [{generate, _, {sort, {qlc, _, [{generate, _, {list, [1,2,3,2]}}],
                                  [{unique,true}]},
                           []}},
            {generate, _, {qlc, _, [{generate, _, {list, [a,b]}}],
                           [{unique,true}]}}],
           [{unique,true}]} = i(H, unique_all),
          [{1,a},{1,b},{2,a},{2,b},{3,a},{3,b}] = qlc:e(H, unique_all)">>,

       <<"L = [1,2,1,3,4,3,1],
          true = lists:sort(L) == qlc:e(qlc:q([X || X <- qlc:sort(L)])),
          true = lists:usort(L) == 
                 qlc:e(qlc:q([X || X <- qlc:sort(L, {unique,true})])),
          true = lists:reverse(lists:sort(L)) ==
                 qlc:e(qlc:q([X || X <- qlc:sort(L, {order, descending})])),
          true = lists:reverse(lists:usort(L)) ==
                 qlc:e(qlc:q([X || X <- qlc:sort(L, [{order, descending}, 
                                                     {unique, true}])])),
          CF = fun(X, Y) -> X =< Y end,
          true = lists:sort(L) == 
                 qlc:e(qlc:q([X || X <- qlc:sort(L, {order, CF})])),
          true = lists:usort(L) ==
                 qlc:e(qlc:q([X || X <- qlc:sort(L, [{order, CF}, 
                                                    {unique, true}])]))">>,

       <<"E = ets:new(foo, []),
          [true || I <- lists:seq(1, 50000), not ets:insert(E, {I, I})],
          H = qlc:q([{X,Y} || X <- [a,b], Y <- qlc:sort(ets:table(E))]),
          100000 = length(qlc:e(H)),
          ets:delete(E)">>,

       begin
       TmpDir = ?privdir,
       [<<"TE = process_flag(trap_exit, true),
           E = ets:new(foo, []),
           [true || I <- lists:seq(1, 50000), not ets:insert(E, {I, I})],
           Ports = erlang:ports(),
           H = qlc:q([{X,Y} || X <- [a,b], 
                               begin
                                   [P] = erlang:ports() -- Ports,
                                   exit(P, port_exit),
                                   true
                               end,
                               Y <- qlc:sort(ets:table(E),
                                             [{tmpdir,\"">>, 
                                               TmpDir, <<"\"}])]),
           {error, qlc, {file_error, _, _}} = (catch qlc:e(H)),
           receive {'EXIT', _, port_exit} -> ok end,
           ets:delete(E),
           process_flag(trap_exit, TE)">>]
       end

        ],
    run(Config, Ts),
    ok.

%% The sort option.
keysort(Config) when is_list(Config) ->

    Ts = [
       <<"OF = fun(X, Y) -> X =< Y end,
          F = fun(Obj, A) -> A++[Obj] end,
          H = qlc:q([X || X <- qlc:keysort(1, [{2,a},{1,b}], {order,OF})]),
          {'EXIT',{{badarg,order},_}} = (catch qlc:e(H)),
          {'EXIT',{{badarg,order},_}} = (catch qlc:fold(F, [], H)),
          {'EXIT',{{badarg,order},_}} = (catch qlc:cursor(H))">>,

       <<"E = create_ets(1, 2),
          H = qlc:q([X || X <- qlc:keysort([1], ets:table(E), 
                                           [{size,1},{tmpdir,\"/a/b/c\"}])]),
          H1 = qlc:q([X || {X,_} <- qlc:e(H), X < 4]),
          {error,_,{file_error,_,_}} = qlc:info(H1),
          {error,_,{file_error,_,_}} = qlc:e(H1),
          ets:delete(E)">>,

       <<"L = [{1,a},{2,b},{3,c},{2,b}],
          H = qlc:q([{X,Y} || {X,_} <- qlc:keysort(1, qlc:q([X || X <- L]), 
                                                   {unique,true}),
                              Y <- [a,b]]),
          [{1,a},{1,b},{2,a},{2,b},{3,a},{3,b}] = qlc:e(H),
          C = qlc:cursor(H),
          [{1,a},{1,b},{2,a},{2,b},{3,a},{3,b}] = 
              qlc:next_answers(C, all_remaining),
          qlc:delete_cursor(C)">>,

       <<"H1 = qlc:q([X || X <- qlc:keysort(0, [])]),
          {'EXIT',{badarg,_}} = (catch qlc:e(H1)),
          H2 = qlc:q([X || X <- qlc:keysort(1, [], {bad,arg})]),
          {'EXIT',{badarg,_}} = (catch qlc:e(H2)),
          H3 = qlc:q([X || X <- qlc:keysort([], [])]),
          {'EXIT',{badarg,_}} = (catch qlc:e(H3))">>,

       <<"H = qlc:q([X || X <- qlc:keysort(1, [{1,a},{2,b}], 
                                           [{order,descending},
                                           {compressed,true}])]),
          [{2,b},{1,a}] = qlc:e(H),
          H2 = qlc:q([X || X <- qlc:keysort(1, [{1},{2}], compressed)]),
          {'EXIT', {badarg, _}} = (catch qlc:e(H2))">>,

       <<"H = qlc:q([X || X <- qlc:keysort(1, [{1,a},{2,b}], {compressed,false})]),
          [{1,a},{2,b}] = qlc:e(H)">>,

       <<"E = create_ets(1, 2),
          H = qlc:q([X || X <- qlc:keysort([1], ets:table(E), 
                                           [{size,1},{tmpdir,\"/a/b/c\"}])]),
          F = fun(Obj, A) -> A++[Obj] end,
          {error,_,{file_error,_,_}} = qlc:e(H),
          \" \\\"no such\" ++ _ = lists:dropwhile(fun(A) -> A =/= $\s end, 
                                 lists:flatten(qlc:format_error(qlc:e(H)))),
          {error,_,{file_error,_,_}} = qlc:e(H, {unique_all,true}),
          {error,_,{file_error,_,_}} = qlc:cursor(H),
          {error,_,{file_error,_,_}} = qlc:cursor(H, {unique_all,true}),
          {error,_,{file_error,_,_}} = qlc:cursor(H, {spawn_options, []}),
          {error,_,{file_error,_,_}} = qlc:cursor(H, {spawn_options,default}),
          {error,_,{file_error,_,_}} = 
               qlc:cursor(H, [{unique_all,true},{spawn_options, []}]),
          {error,_,{file_error,_,_}} = qlc:fold(F, [], H),
          {error,_,{file_error,_,_}} = qlc:fold(F, [],H, {unique_all,true}),
          ets:delete(E)">>,

       <<"L = [{1,b,a},{1,b,b},{1,a,a}],
          H = qlc:q([X || X <- qlc:keysort([4,1], L)]),
          {error,_,bad_object} = qlc:e(H),
          \"the keys\" ++ _ = qlc:format_error(qlc:e(H))">>,

       begin
       File = filename:join(?privdir, "afile"),
       ok = file:write_file(File, <<>>),
       [<<"H = qlc:q([X || X <- qlc:keysort([1], [{1},{2},{1}],
                                            [{tmpdir,\"">>, File, <<"\"},
                                             {size,1}])]),
           {error,_,{file_error,_,_}} = qlc:e(H),
           file:delete(\"">>, File, <<"\")">>]
       end,

       <<"H0 = qlc:q([X || X <- [1,2,3]]),
          H = qlc:q([X || X <- qlc:sort(H0,{tmpdir,\".\"})]),
          [1,2,3] = qlc:e(H)">>,

       %% The global option 'tmpdir' takes precedence.
       begin
       PrivDir = ?privdir,
       [<<"L = [{1,a},{2,b}],
           H = qlc:q([X || X <- qlc:keysort([1], L, {tmpdir,\"/a/b/c\"})]),
           H1 = qlc:q([X || X <- H, X > 3]),
           Dir = \"">>, PrivDir, <<"\",
           Options = [{tmpdir, Dir}],
           {qlc,_,[{generate,_,{keysort,{list,L},[1],[{tmpdir,Dir}]}},_],[]} =
                i(H1, Options),
           [{1,a},{2,b}] = qlc:e(H1, Options)">>] % no check of "/a/b/c"
       end,

       <<"L = [{2,c},{1,b},{1,a},{3,a},{1,c},{2,b}],
          true = lists:sort(L) == 
                 qlc:e(qlc:q([X || X <- qlc:keysort([1,2], L)]))">>,

       <<"L = [{1,b},{2,c},{1,a},{3,e},{4,f},{3,d}],
          true = lists:keysort(1, L) ==
                 qlc:e(qlc:q([X || X <- qlc:keysort(1,L)])),
          true = lists:ukeysort(1, L) ==
                 qlc:e(qlc:q([X || X <- qlc:keysort(1, L, {unique,true})])),
          true = lists:reverse(lists:keysort(1, L)) ==
                 qlc:e(qlc:q([X || X <- qlc:keysort(1,L, 
                                                    {order, descending})])),
          true = lists:reverse(lists:ukeysort(1, L)) ==
                 qlc:e(qlc:q([X || X <- qlc:keysort(1, L, [{unique,true},
                                                {order, descending}])]))">>,

       <<"L = [{X} || X <- lists:seq(1,100000)],
          H1 = qlc:append([L,[{1,2},{2,3},{3,4}]]),
          H = qlc:keysort([1], qlc:keysort([1], H1, [{compressed,true}])),
          R = qlc:e(H),
          100003 = length(R)">>

        ],
    run(Config, Ts),

    ok.

%% keysort/1,2, using a file.
filesort(Config) when is_list(Config) ->
    Ts = [
       <<"Q = qlc:q([X || X <- [{3},{1},{2}]]),
          Opts = [{size,10},{no_files,3}],
          Q2 = qlc:q([{X,Y} || Y <- [1,2], X <- qlc:keysort([1],Q,Opts)]),
          [{{1},1},{{2},1},{{3},1},{{1},2},{{2},2},{{3},2}] = qlc:e(Q2)">>
        ],
    run(Config, Ts),
    ok.


%% The cache option.
cache(Config) when is_list(Config) ->
    Ts = [
       <<"{'EXIT', {badarg, _}} = (catch qlc:q([X || X <- [1,2]], badarg))">>,

       <<"Q1 = qlc:q([X || X <- [1,2,1,2,1]], {unique,true}),
          [1,2] = qlc:e(Q1),
          [1,2] = qlc:e(Q1, {unique_all,true}),
          Q2 = qlc:q([X || X <- qlc:q([X || X <- [1,2,1,2,1]],
                                      {unique,true})]),
          [1,2] = qlc:e(Q2),
          [1,2] = qlc:e(Q2, {unique_all,true}),
          Q3 = qlc:q([X || X <- qlc:append([[1,2,3], [4,5,6]])]),
          [1,2,3,4,5,6] = qlc:e(Q3)">>,

       <<"Q1 = qlc:q([X || {X,_} <- [{1,a},{2,a},{1,b},{2,b}]]),
          Q2 = qlc:q([{X,make_ref()} || X <- Q1]),
          [{1,_},{2,_},{1,_},{2,_}] = qlc:e(Q2, {unique_all,false}),
          [{1,_},{2,_}] = qlc:e(Q2, {unique_all,true})">>,

       <<"E = ets:new(test, [duplicate_bag]),
          true = ets:insert(E, [{1,a},{2,a},{1,b},{2,b}]),
          Q1 = qlc:q([X || {X,_} <- ets:table(E)]),
          Q2 = qlc:q([{X,make_ref()} || X <- Q1]),
          [{1,_},{1,_},{2,_},{2,_}] = lists:sort(qlc:e(Q2, {unique_all,false})),
          [{1,_},{2,_}] = lists:sort(qlc:e(Q2, {unique_all,true})),
          ets:delete(E)">>,

       <<"Q1 = qlc:q([X || {X,_} <- [{1,a},{2,a},{1,b},{2,b}]]),
          Q2 = qlc:q([{X,make_ref()} || X <- qlc:append([Q1, Q1])]),
          [{1,_},{2,_},{1,_},{2,_},{1,_},{2,_},{1,_},{2,_}] = 
                qlc:e(Q2, {unique_all,false}),
          [{1,_},{2,_}] = qlc:e(Q2, {unique_all,true})">>,

       <<"[] = qlc:e(qlc:q([X || X <- []], {unique, true})),
          [] = qlc:e(qlc:q([X || X <- qlc:q([X || X <- qlc:append([])], 
                                             {unique,true})]))">>,

       <<"Q1 = qlc:q([{X,make_ref()} || {X,_} <- [{1,a},{1,b}]]),
          [{1,_},{1,_}] = qlc:e(Q1, {unique_all, true}),
          Q2 = qlc:q([Y || {X,_} <- [{1,a},{1,b}],
                           begin Y = {X,make_ref()}, true end]),
          [{1,_},{1,_}] = qlc:e(Q2, {unique_all,true}),
          Q3 = qlc:q([Y || X <- [{1,a},{2,a}], 
                           begin {_,Z} = X, Y = {Z,make_ref()}, true end]),
          [{a,_},{a,_}] = qlc:e(Q3, {unique_all, true})">>,

       <<"E = ets:new(apa, [duplicate_bag]),
          ets:insert(E, [{1,a},{2,a},{1,a}]),
          H1 = qlc:q([X || X <- qlc:append(ets:table(E),[7,3,5])], 
                          {cache, true}),
          [{_,a},{_,a},{_,a},7,3,5] = qlc:e(H1),
          ets:delete(E)">>,
          
       <<"F = fun(Obj, A) -> A++[Obj] end,
          H = qlc:q([X || X <- [1,3,2,4]], cache),
          Q = qlc:q([X || X <- H]),
          [1,3,2,4] = qlc:fold(F, [], Q, [])">>,

       <<"F = fun(Obj, A) -> A++[Obj] end,
          E = ets:new(apa, [duplicate_bag]),
          true = ets:insert(E, [{1,a},{2,b},{1,a}]),
          Q1 = qlc:q([X || X <- ets:table(E)], [cache, unique]),
          Q = qlc:q([X || X <- Q1], [cache, unique]),
          {qlc, _, [{generate, _,{table,_}}], [{unique,true}]} = i(Q),
          R = qlc:fold(F, [], Q, []),
          ets:delete(E),
          true = [{1,a},{2,b}] == lists:sort(R)">>,

       <<"E = ets:new(apa, [duplicate_bag]),
          ets:insert(E, [{1,a},{2,b},{1,a}]),
          H1 = qlc:q([X || X <- qlc:append(ets:table(E),[7,3])], cache),
          H2 = qlc:q([{Y,X} || Y <- [2,1,3], X <- H1]),
          [{2,_},{2,_},{2,_},{2,7},{2,3},
           {1,_},{1,_},{1,_},{1,7},{1,3},
           {3,_},{3,_},{3,_},{3,7},{3,3}] = qlc:e(H2),
          ets:delete(E)">>,

          %% This case is not 100 percent determined. An Ets table
          %% is updated in a filter and later used in a generator.
       <<"E = ets:new(apa, [bag]),
          true = ets:insert(E, [{1,a},{2,b}]),
          H1 = qlc:q([Y || Y <- ets:table(E)], 
                     [{cache, no}, {unique, true}]),
          H = qlc:q([{X,Y} || X <- [{1,a},{2,d},{3,e}],
                              ets:insert(E, X),
                              Y <- H1]),
          [{{1,a},_}, {{1,a},_}, {{2,d},_}, {{2,d},_}, {{2,d},_}, 
           {{3,e},_}, {{3,e},_}, {{3,e},_}, {{3,e},_}] = qlc:e(H),
          ets:delete(E)">>,

          %% This case is not 100 percent determined. An Ets table
          %% is updated in a filter and later used in a generator.
       <<"E = ets:new(apa, [bag]),
          true = ets:insert(E, [{1,a},{2,b}]),
          H1 = qlc:q([Y || Y <- ets:table(E)], 
                     [{cache, true}, {unique, true}]),
          H = qlc:q([{X,Y} || X <- [{1,a},{2,d},{3,e}],
                              ets:insert(E, X),
                              Y <- H1]),
          [{{1,a},_}, {{1,a},_}, {{2,d},_}, {{2,d},_}, {{3,e},_}, {{3,e},_}] = 
              qlc:e(H),
          ets:delete(E)">>,

       <<"%% {5979} and {5549} are both hashed to 28244 by phash2/1
          E = ets:new(apa, [duplicate_bag]),
          true = ets:insert(E, [{5979},{5549},{5979},{5549},{0}]),
          H1 = qlc:q([X || X <- ets:table(E)], 
                     [{cache, true}, {unique, true}]),
          H = qlc:q([Y || _ <- [1,2], Y <- H1]),
          {qlc, _, [{generate, _, {list, [1,2]}},
                    {generate, _, {qlc, _, [{generate, _, {table,_}}],
                                   [{cache,ets},{unique,true}]}}],
           []} = i(H),
          [{0},{0},{5549},{5549},{5979},{5979}] = lists:sort(qlc:e(H)),
          ets:delete(E)">>,

       <<"E = ets:new(apa, [ordered_set]),
          ets:insert(E, [{1},{2}]),
          H1 = qlc:q([X || X <- ets:table(E)], [cache, unique]),
          H2 = qlc:q([X || Y <- [3,4], ets:insert(E, {Y}), X <- H1]),
          {qlc, _, [{generate, _, {list, [3,4]}}, _, 
                    {generate, _, {qlc, _, [{generate, _, 
                                             {table, _}}], 
                                   [{cache, ets}]}}], 
           []} = i(H2),
          [{1},{2},{3},{1},{2},{3}] = qlc:e(H2),
          ets:delete(E)">>,

       <<"E = ets:new(apa, [ordered_set]),
          ets:insert(E, [{1},{2}]),
          H1 = qlc:q([X || X <- ets:table(E)], [unique]),
          H2 = qlc:q([X || Y <- [3,4], ets:insert(E, {Y}), X <- H1]),
          [{1},{2},{3},{1},{2},{3},{4}] = qlc:e(H2),
          ets:delete(E)">>,

       <<"H0 = qlc:append([a,b], [c,d]),
          H = qlc:q([{X,Y} || 
                        X <- H0,
                        Y <- qlc:q([{X1,Y} || 
                                       X1 <- H0,
                                       Y <- qlc:q([{X2,Y} || 
                                                      X2 <- H0,
                                                      Y <- H0])])]),
          {qlc, _,
           [{generate, _,{append, [{list, [a,b]}, {list, [c,d]}]}},
            {generate, _, 
             {qlc, _,
              [{generate, _, {append,[{list, [a,b]},{list, [c,d]}]}},
               {generate, _, 
                {qlc, _,
                 [{generate, _,{append,[{list, [a,b]}, {list, [c,d]}]}},
                  {generate, _,{append,[{list, [a,b]}, {list, [c,d]}]}}],
                 [{cache,ets}]}}],
              [{cache,ets}]}}],
           []} = i(H, cache_all)">>
       
       ],
    run(Config, Ts),
    ok.

%% OTP-6038. The {cache,list} option.
cache_list(Config) when is_list(Config) ->
    Ts = [
       begin
       PrivDir = ?privdir,
       [<<"%% unique, cache list. A simple qlc.
          Options = [{cache,list}, unique],
          L0 = [1,2,3,4,1,2,3,4],
          L = qlc_SUITE:table(L0, []),
          Q1 = qlc:q([X || X <- L], Options),
          Q = qlc:q([{X,Y} || X <- [a,b], Y <- Q1]),
          GOptions = [{tmpdir,\"">>, PrivDir, <<"\"}],
          {qlc,_,[{generate,_,{list,[a,b]}},
                  {generate,_,{qlc,_,
                               [{generate,_,{table,_}}],
                               [{cache,list},{unique,true}]}}],
           []} = i(Q, GOptions),
          true = [{X,Y} || X <- [a,b], Y <- [1,2,3,4]] =:= 
                 qlc:e(Q, GOptions)">>]
       end,

       begin
       MS = ets:fun2ms(fun({X,_}) when X > 1 -> X end),
       [<<"%% No cache, even if explicit match specification.
          etsc(fun(E) ->
                   MS =  ">>, io_lib:format("~w", [MS]), <<",
                   Options = [{cache,list}, unique],
                   Q = qlc:q([{X,Y} || 
                                 X <- ets:table(E, {traverse, {select, MS}}),
                                 Y <- [1,2,3]], 
                             Options),
                   {qlc,_,[{generate,_,{table,{ets,table,_}}},
                           {generate,_,{list,[1,2,3]}}],
                    [{unique,true}]} = i(Q),
                   true = [{X,Y} || X <- lists:seq(2,10), Y <- [1,2,3]] =:=
                       lists:sort(qlc:e(Q))
               end, [{keypos,1}], [{I,a} || I <- lists:seq(1, 10)])">>]
       end,

       <<"%% Temporary files.
          %% Remove list expression caches when possible. (no visible effect)
          T = lists:seq(1, 100000), % Huge terms on file
          F = fun(C) ->
                      Q0 = qlc:q([{X} || X <- [T,T,T], begin X > 0 end], 
                                 {cache,C}),
                      Q1 = qlc:q([{X,Y,Z} ||
                                     X <- Q0,
                                     Y <- Q0,
                                     Z <- Q0],
                                 {cache,C}),
                      qlc:q([{X, Y} || Y <- [1], X <- Q1])
              end,
          Ql = F(list),
          Rl = qlc:e(Ql, {max_list_size, 64*1024}),
          Qe = F(ets),
          Re = qlc:e(Qe),
          Qf = F(no),
          Rf = qlc:e(Qf),
          Ri = qlc:e(Ql, {max_list_size, 1 bsl 35}), % heavy
          {27,27,27,27,true,true,true} = 
              {length(Rl), length(Re), length(Rf), length(Ri),
               Rl =:= Re, Re =:= Rf, Rf =:= Ri}">>,

       <<"%% File sorter.
          T = lists:seq(1, 10000),
          F = fun(C) ->
                      Q0 = qlc:q([{X} || X <- [T,T,T], begin X > 0 end], 
                                 [{cache,C},unique]),
                      Q1 = qlc:q([{X,Y,Z} ||
                                     X <- Q0,
                                     Y <- Q0,
                                     Z <- Q0],
                                 [{cache,C},unique]),
                      qlc:q([{X, Y} || Y <- [1], X <- Q1])
              end,
          GOpt = [{max_list_size, 10000}],
          Ql = F(list),
          Rl = qlc:e(Ql, GOpt),
          Qe = F(ets),
          Re = qlc:e(Qe, GOpt),
          Qf = F(no),
          Rf = qlc:e(Qf, GOpt),
          {1,1,1,true,true} = 
              {length(Rl), length(Re), length(Rf), Rl =:= Re, Re =:= Rf}">>,

       <<"%% Remove list expression caches when possible. (no visible effect)
          Q0 = qlc:q([{X} || X <- [1,2,3], begin X > 0 end], {cache,list}),
          Q1 = qlc:q([{X,Y,Z} ||
                         X <- Q0,
                         Y <- Q0,
                         Z <- Q0],
                     {cache,list}),
          Q = qlc:q([{X, Y} || Y <- [1], X <- Q1]),
          R = qlc:e(Q),
          L0 = [{X} || X <- [1,2,3], begin X > 0 end],
          L1 = [{X,Y,Z} ||
                   X <- L0,
                   Y <- L0,
                   Z <- L0],
          L = [{X, Y} || Y <- [1], X <- L1],
          true = R =:= L">>,

       <<"%% No temporary file.
          L = [{I,a} || I <- lists:seq(1, 10)],
          Q0 = qlc:q([X || X <- qlc_SUITE:table_error(L, 1, err),
                           begin element(1, X) > 5 end],
                     {cache,list}),
          Q = qlc:q([{X, element(1,Y)} || 
                        X <- lists:seq(1, 5),
                        Y <- Q0]),
          err = qlc:e(Q)">>,

       <<"%% Sort internally.
          L = [{I,a} || I <- lists:seq(1, 10)],
          Q0 = qlc:q([X || X <- qlc_SUITE:table_error(L, 1, err),
                           begin element(1, X) > 5 end],
                     [unique,{cache,list}]),
          Q = qlc:q([{X, element(1,Y)} || 
                        X <- lists:seq(1, 5),
                        Y <- Q0]),
          err = qlc:e(Q, {max_list_size,0})">>,

       <<"%% No temporary file.
          etsc(fun(E) ->
                       Q0 = qlc:q([X || X <- ets:table(E),
                                        begin element(1, X) > 5 end],
                                  {cache,list}),
                       Q = qlc:q([{X, element(1,Y)} || X <- lists:seq(1, 5),
                                                       Y <- Q0]),
                       R = [{X,Y} || X <- lists:seq(1, 5), 
                                     Y <- lists:seq(6, 10)],
                       R = lists:sort(qlc:e(Q))
               end, [{keypos,1}], [{I,a} || I <- lists:seq(1, 10)])">>,

       <<"%% Sort internally
          etsc(fun(E) ->
                       Q0 = qlc:q([X || X <- ets:table(E),
                                        begin element(1, X) > 5 end],
                                  [{cache,list},unique]),
                       Q = qlc:q([{X, element(1,Y)} || X <- lists:seq(1, 5),
                                                       Y <- Q0]),
                       R = [{X,Y} || X <- lists:seq(1, 5), 
                                     Y <- lists:seq(6, 10)],
                       R = lists:sort(qlc:e(Q))
               end, [{keypos,1}], [{I,a} || I <- lists:seq(1, 10)])">>,

       <<"%% A few more tests of unique and {cache,list}.
          F = fun(CU) ->
                      H1 = qlc:q([{X,Y} || 
                                     Y <- [a,b], 
                                     X <- [1,2]],
                                 CU),
                      qlc:q([{X,Y,Z} || X <- [3,4], {Y,Z} <- H1])
              end,
          Q1 = F([]),
          Q2 = F([{cache,list}, unique]),
          R1 = qlc:e(Q1),
          R2 = qlc:e(Q2),
          R3 = qlc:e(Q2, {max_list_size, 0}), % still in RAM
          true = R1 =:= R2,
          true = R2 =:= R3">>,

       <<"E = ets:new(t, [duplicate_bag]),
          true = ets:insert(E, [{2},{1},{2}]),
          H1 = qlc:q([{X,Y} || 
                         Y <- [a,b], 
                         {X} <- ets:table(E)], 
                     [{cache,list}, unique]),
          H2 = qlc:q([{X,Y,Z} || X <- [3,4], {Y,Z} <- H1]),
          {qlc,_,[{generate,_,{list,[3,4]}},
                  {generate,_,{qlc,_,
                               [{generate,_,{list,[a,b]}},
                                {generate,_,
                                 {qlc,_,[{generate,_,{table,{ets,table,_}}}],
                                  [{cache,list},{unique,true}]}}],
                               [{cache,list},{unique,true}]}}], []} = i(H2),
          L1s = [[{X,Y} || Y <- [a,b], X <- Xs] || Xs <- [[1,2], [2,1]]],
          L2s = [[{X,Y,Z} || X <- [3,4], {Y,Z} <- L1] || L1 <- L1s],
          R1 = qlc:e(H2),
          R2 = qlc:e(H2, {max_list_size, 0}), % on temporary file
          ets:delete(E),
          true = lists:member(R1, L2s),
          true = R1 =:= R2">>,

       <<"E = ets:new(t, [duplicate_bag]),
          true = ets:insert(E, [{2},{1},{2}]),
          H1 = qlc:q([{X,Y} || 
                         Y <- [a,b], 
                         {X} <- ets:table(E)], 
                     [{cache,list}]),
          H2 = qlc:q([{X,Y,Z} || X <- [3,4], {Y,Z} <- H1]),
          L1s = [[{X,Y} || Y <- [a,b], X <- Xs] || Xs <- [[1,2,2], [2,2,1]]],
          L2s = [[{X,Y,Z} || X <- [3,4], {Y,Z} <- L1] || L1 <- L1s],
          R1 = qlc:e(H2),
          R2 = qlc:e(H2, {max_list_size, 0}), % on temporary file
          ets:delete(E),
          true = lists:member(R1, L2s),
          true = R1 =:= R2">>,

       <<"Q1 = qlc:q([{X,Y} || 
                         Y <- [a,b], 
                         {X,_} <- qlc_SUITE:table_error([{a,1}], 2, err)],
                     [{cache,list}, unique]),
          Q = qlc:q([{X,Y,Z} || X <- [3,4], {Y,Z} <- Q1]),
          {qlc,_,[{generate,_,{list,[3,4]}},
                  {generate,_,{qlc,_,
                               [{generate,_,{list,[a,b]}},
                                {generate,_,{table,_}}],
                               [{cache,list},{unique,true}]}}],
           []} = i(Q),
          err = qlc:e(Q,{max_list_size,0})">>,

       begin
       Privdir = ?privdir,
       [<<"
          E = ets:new(t, [duplicate_bag]),
          N = 17000,
          true = ets:insert(E, [{X,X} || X <- lists:seq(1, N)]),
          N = ets:info(E, size),
          RF = fun(GOpts) ->
                       F = fun(CU) ->
                                   H1 = qlc:q([{X,Y} || 
                                                  Y <- [a,b], 
                                                  {X,_} <- ets:table(E)], 
                                              CU),
                                   qlc:q([{X,Y,Z} || X <- [3,4], {Y,Z} <- H1])
                           end,
                       Q1 = F([{cache,list}, unique]),
                       _ = qlc:info(Q1, GOpts),
                       R1 = qlc:e(Q1, GOpts),
                       Q2 = F([unique]),
                       R2 = qlc:e(Q2, GOpts),
                       true = lists:sort(R1) =:= lists:sort(R2)
               end,
          GOpts = [{tmpdir,\"">>,Privdir,<<"\"}],
          RF([{max_list_size, 1 bsl 35} | GOpts]),
          RF(GOpts),
          RF([{max_list_size, 40000} | GOpts]),
          true = ets:insert(E, [{X,X} || X <- lists:seq(1, N)]),
          true = N+N =:= ets:info(E, size),
          RF([{max_list_size, 1 bsl 30} | GOpts]),
          RF(GOpts),
          RF([{max_list_size, 40000} | GOpts]),
          ets:delete(E)">>]
       end,

       <<"%% Temporary file employed.
          etsc(fun(E) ->
                       Q0 = qlc:q([X || X <- ets:table(E),
                                        begin element(1, X) > 5 end],
                                  {cache,list}),
                       Q = qlc:q([{X, element(1,Y)} || X <- lists:seq(1, 5),
                                                       Y <- Q0]),
                       R = [{X,Y} || X <- lists:seq(1, 5), 
                                     Y <- lists:seq(6, 10)],
                       R = lists:sort(qlc:e(Q, {max_list_size, 100*1024}))
               end, [{keypos,1}], [{I,a,lists:duplicate(100000,1)} || 
                                        I <- lists:seq(1, 10)])">>,

       <<"%% Temporary file employed. The file is removed after error.
          L = [{I,a,lists:duplicate(100000,1)} || I <- lists:seq(1, 10)],
          Q0 = qlc:q([X || X <- qlc_SUITE:table_error(L, 1, err),
                           begin element(1, X) > 5 end],
                     {cache,list}),
          Q = qlc:q([{X, element(1,Y)} || 
                        X <- lists:seq(1, 5),
                        Y <- Q0]),
          err = qlc:e(Q)">>,

       <<"%% Temporary file employed. The file is removed after error.
          L = [{I,a,lists:duplicate(100000,1)} || I <- lists:seq(1, 10)],
          Q0 = qlc:q([X || X <- qlc_SUITE:table(L, 1, []),
                           begin element(1, X) > 5 end],
                     {cache,list}),
          Q = qlc:q([{X, element(1,Y)} || 
                        X <- lists:seq(1, 5),
                        Y <- Q0]),
          {error, _, {file_error,_,_}} = qlc:e(Q, {tmpdir, \"/a/b/c\"})">>,

       <<"Q = qlc:q([X || X <- [1,2]]),
          {'EXIT', {badarg, _}} = (catch qlc:e(Q, {max_list_size, -1}))">>,

       <<"Q = qlc:q([X || X <- [1,2]]),
          {'EXIT', {badarg, _}} = (catch qlc:e(Q, {max_list_size, foo}))">>

       ],
    run(Config, Ts),
    ok.

%% Filters and match specs.
filter(Config) when is_list(Config) ->
    Ts = [
       <<"L = [1,2,3,4,5],
          QH1 = qlc:q([X || X <- L, X > 1, X < 4]),
          [2,3] = qlc:e(QH1),
          {list,{list,L},_MS} = i(QH1)
         ">>,

       <<"L = [1,2,3,4,5],
          QH2 = qlc:q([X || X <- L, X > 1, X < 4, X > 2]),
          [3] = qlc:e(QH2),
          {list,{list,L},_MS} = i(QH2)
         ">>,

          %% "X > 1" is skipped since the matchspec does the job
       <<"QH3 = qlc:q([X || X <- [1,2,3,4,5], X > 1, begin X < 4 end]),
          [2,3] = qlc:e(QH3),
          {qlc,_,[{generate,_,{list,{list,[1,2,3,4,5]},_MS}},_],[]} = i(QH3)
         ">>,

       <<"QH4 = qlc:q([{X,Y} || X <- [1,2], Y <- [1,2]]),
          [{1,1},{1,2},{2,1},{2,2}] = qlc:e(QH4),
          {qlc,_,[{generate,_,{list,[1,2]}},{generate,_,{list,[1,2]}}],[]} =
              i(QH4)">>,

          %% "X > 1" is skipped since the matchspec does the job
       <<"QH5 = qlc:q([{X,Y} || X <- [1,2], X > 1, Y <- [1,2]]),
          [{2,1},{2,2}] = qlc:e(QH5),
          {qlc,_,[{generate,_,{list,{list,[1,2]},_MS}},
                  {generate,_,{list,[1,2]}}],[]} = 
              i(QH5)">>,

       <<"%% Binaries are not handled at all when trying to find lookup values
          etsc(fun(E) ->
                      A = 2, 
                      Q = qlc:q([X || {X} <- ets:table(E), <<A>> =:= <<X>>]),
                      [2] = lists:sort(qlc:e(Q)),
                      false = lookup_keys(Q)
              end, [{1},{2},{3}])">>,

       <<"etsc(fun(E) ->
                      Q = qlc:q([X || {X,_} <- ets:table(E), 
                                   qlc:e(qlc:q([Y || {Y,_} <- ets:table(E), 
                                                              Y > X])) == []]),
                      [3] = qlc:e(Q)
              end, [{1,a},{2,b},{3,c}])">>,

       <<"Q = qlc:q([X || {X} <- [], (false or (X/0 > 3))]),
          \"[]\" = qlc:info(Q),
          [] = qlc:e(Q)">>,

       <<"%% match spec
          [] = qlc:e(qlc:q([X || {X} <- [{1},{2}], 
                                 (false orelse (X/0 > 3))])),
          %% generated code
          {'EXIT', {badarith, _}} = 
            (catch qlc:e(qlc:q([X || {X} <- [{1}], 
                                     begin (false orelse (X/0 > 3)) end])))">>,

       <<"%% Partial evaluation in filter.
          etsc(fun(E) ->
                     QH = qlc:q([{X+1,Y} || {X,Y} <- ets:table(E), 
                                            X =:= 1-1+1+(+1)]),
                     [{3,2}] = qlc:e(QH),
                     [2] = lookup_keys(QH)
              end, [{1,1},{2,2},{3,3}])">>,

       <<"%% =/2 in filters must not be recognized when 'badmatch' is
          %% possible.
          etsc(fun(E) ->
                     QH = qlc:q([{X,Y} || {X,Y} <- ets:table(E),
                                          ((Y = X) =:= 3)]),
                     {'EXIT', {{badmatch,4},_}} = (catch qlc:e(QH)),
                     false = lookup_keys(QH)
               end, [{3,3},{4,true}])">>,

       <<"%% One more of the same kind.
          etsc(fun(E) ->
                      QH = qlc:q([{X,Y} || {X,_} <- ets:table(E), 
                                           (Y=X) =:= (Y=1+1)]),
                      {'EXIT', {{badmatch,2},_}} = (catch qlc:e(QH)),
                      false = lookup_keys(QH)
              end, [{1,1},{2,2},{3,3}])">>,

       <<"%% OTP-5195. Used to return a value, but badarith is correct.
          etsc(fun(E) ->
                      QH = qlc:q([X || {X,_} <- ets:table(E), 
                                       (X =:= 1) and
                                    if X =:= 1 -> true;
                                       true -> X/0
                                    end]),
                      {'EXIT',{badarith,_}} = (catch qlc:e(QH)),
                      false = lookup_keys(QH)
              end, [{1,1},{2,2},{3,3}])">>,

       <<"fun(Z) ->
            Q = qlc:q([X || Z < 2, X <- [1,2,3]]),
            [] = qlc:e(Q)
          end(3)">>,

       <<"H = qlc:q([{P1,A,P2,B,P3,C} ||
                  P1={A,_} <- [{1,a},{2,b}],
                  {_,B}=P2 <- [{1,a},{2,b}],
                  C=P3 <- [1],
                  {[X,_],{_,X}} <- [{[1,2],{3,1}}, {[a,b],{3,4}}],
                  A > 0,
                  B =/= c,
                  C > 0]),
          L = [{{1,a},1,{1,a},a,1,1}, {{1,a},1,{2,b},b,1,1},
               {{2,b},2,{1,a},a,1,1}, {{2,b},2,{2,b},b,1,1}],
          L = qlc:e(H)">>,

       <<"H = qlc:q([{X,Y} ||
                  X = _ <- [1,2,3],
                  _ = Y <- [a,b,c],
                  _ = _ <- [foo],
                  X > 1,
                  Y =/= a]),
          [{2,b},{2,c},{3,b},{3,c}] = qlc:e(H)">>

       ],
    run(Config, Ts),
    ok.

%% info/2.
info(Config) when is_list(Config) ->
    Ts = [
       <<"{list, [1,2]} = i(qlc:q([X || X <- [1,2]])),
          {append,[{list, [1,2]}, {list, [3,4]}]} = 
               i(qlc:append([1,2],[3,4])),
          {sort,{list, [1,2]},[]} = i(qlc:sort([1,2])),
          E = ets:new(foo, []),
          ets:insert(E, [{1},{2}]),
          {table, _} = i(ets:table(E)),
          true = ets:delete(E),
          {list, [1,2]} = i([1,2]),
          {append, [{list, [1,2]}, {list, [3,4]}]} =
             i(qlc:q([X || X <- qlc:append([1,2],[3,4])])),
          
          H0 = qlc:q([X || X <- throw({throw,t})]),
          {throw,t} = (catch {any_term,qlc:info(H0)}),
          {'EXIT', {badarg, _}} = 
               (catch qlc:info(foobar)),
          {'EXIT', {badarg, _}} = 
               (catch qlc:info(qlc:q([X || X <- [1,2]]), badarg))">>,

       <<"{'EXIT', {badarg, _}} = 
               (catch qlc:info([X || {X} <- []], {n_elements, 0})),
          L = lists:seq(1, 1000),
          \"[1,2,3,4,5,6,7,8,9,10|'...']\" = qlc:info(L, {n_elements, 10}),
          {cons,A1,{integer,A2,1},{atom,A3,'...'}} =
            qlc:info(L, [{n_elements, 1},{format,abstract_code}]),
          1 = erl_anno:line(A1),
          1 = erl_anno:line(A2),
          1 = erl_anno:line(A3),
          Q = qlc:q([{X} || X <- [a,b,c,d,e,f]]),
          {call,_,_,[{cons,_,{atom,_,a},{cons,_,{atom,_,b},{cons,_,{atom,_,c},
                                                            {atom,_,'...'}}}},
                     {call,_,_,_}]} = 
          qlc:info(Q, [{n_elements, 3},{format,abstract_code}]),
          \"ets:match_spec_run([a,b,c,d,e,f],\n\"
          \"                   ets:match_spec_compile([{'$1',[true],\"
          \"[{{'$1'}}]}]))\" = 
             qlc:info(Q, [{n_elements, infinity}])">>,

       <<"Q1 = qlc:q([{X} || X <- qlc:q([X || X <- [1,2]])]),
          {qlc, _, [{generate, _, {list, [1,2]}}],[]} = i(Q1),
          Q2 = qlc:q([X || X <- qlc:q([{X} || X <- [1,2]])]),
          {list,{list,[1,2]},_} = i(Q2),
          [{1},{2}] = qlc:eval(Q2),
          Q3 = qlc:q([{X,Y} || X <- qlc:q([X || X <- [a,b]]),       
                               Y <- qlc:q([Z || Z <- [a,b]])]),
          {qlc, _, [{generate, _, {list, [a,b]}}, 
                    {generate, _, {list, [a,b]}}], []} = i(Q3),
          Q4 = qlc:q([X || X <- [a]]),
          {list, [a]} = i(Q4),
          Q5 = qlc:q([X || X <- qlc:q([Y || Y <- [a,b]], unique)]),
          {qlc, _, [{generate, _, {list, [a,b]}}], [{unique,true}]} = 
             i(Q5)">>,

       <<"H = qlc:q([X || X <- qlc:append([qlc:q([X || X <- [1,2]]),[1,2]])]),
          {append, [{list, [1,2]},{list, [1,2]}]} = i(H),
          [1,2,1,2] = qlc:e(H)">>,

       <<"H = qlc:q([{X} || X <- [], X > 1]),
          {list, []} = i(H),
          [] = qlc:e(H)">>,

       <<"H1 = qlc:q([{X} || X <- [], X > 1]),
          H = qlc:q([{X} || X <- H1, X < 10]),
          {list, []} = i(H),
          [] = qlc:e(H)">>,

       <<"L = [1,2,3],
          QH1 = qlc:q([{X} || X <- L, X > 1]),
          QH2 = qlc:q([{X} || X <- QH1]),
          [{{2}},{{3}}] = qlc:e(QH2),
          {list,{list,{list,L},_},_} = i(QH2)">>,

       <<"H = qlc:q([X || X <- qlc:q([Y || Y <- qlc:q([Z || Z <-[1,2,1]])])]),
          {list, [1,2,1]} = i(H),
          [1,2,1] = qlc:eval(H)">>,

       <<"%% Used to replace empty ETS tables with [], but that won't work.
          E = ets:new(apa,[]),
          QH1 = qlc:q([{X} || X <- ets:table(E), X > 1]),
          QH2 = qlc:q([{X} || X <- QH1], cache),
          [] = qlc:e(QH2),
          {qlc,_,[{generate,_,{table,{ets,table,_}}}],[]} = i(QH2),
          ets:delete(E)">>,

       <<"Q1 = qlc:q([W || W <- [a,b]]),
          Q2 = qlc:q([Z || Z <- qlc:sort([55296,56296,57296])], unique),
          Q3 = qlc:q([{X,Y} || X <- qlc:keysort([2], [{1,a}]),
                               Y <- qlc:append([Q1, Q2]),
                               X > Y]),
          {qlc, T1,
           [{generate, P1, {list, [{1,a}]}},
            {generate, P2, {append, [{list, [a,b]},
                                    {qlc, T2, [{generate, P3,
                                                {sort, {list,[55296,56296,57296]},[]}}],
                                     [{cache,ets},{unique,true}]}]}},F],
           []} = i(Q3, cache_all),
          {tuple, _, [{var,_,'X'}, {var,_,'Y'}]} = binary_to_term(T1),
          {var, _, 'X'} = binary_to_term(P1),
          {var, _, 'Y'} = binary_to_term(P2),
          {var, _, 'Z'} = binary_to_term(P3),
          {var, _, 'Z'} = binary_to_term(T2),
          {op, _, '>', {var, _, 'X'}, {var, _, 'Y'}} = binary_to_term(F),
          true = binary_to_list(<<
           \"beginV1=qlc:q([Z||Z<-qlc:sort([55296,56296,57296],[])],[{unique,true}]),\"
           \"qlc:q([{X,Y}||X<-[{1,a}],Y<-qlc:append([[a,b],V1]),X>Y])end\"
              >>) == format_info(Q3, true)">>,

       <<"Q1 = qlc:q([{X} || X <- qlc:q([X || X <- [a,b]])]),
          {qlc, _, [{generate, _, {list, [a,b]}}], []} = i(Q1),
          Q2 = qlc:q([X || X <- qlc:q([{X} || X <- [a,b]])]),
          {list,{list,[a,b]},_} = i(Q2),
          [{a},{b}] = qlc:eval(Q2)">>,

       <<"Q = qlc:keysort(2, [{1,a,b},{2,b,c},{3,4,c}]),
          {keysort,{list,[{1,a,b},{2,b,c},{3,4,c}]},2,[]} = i(Q),
          true = binary_to_list(<<
             \"qlc:keysort(2,[{1,a,b},{2,b,c},{3,4,c}],[])\">>) 
              == format_info(Q, true),
          [{3,4,c},{1,a,b},{2,b,c}] = qlc:e(Q)">>,

       <<"E = ets:new(foo, []),
          ets:insert(E, [{1},{2}]),
          Q = qlc_SUITE:default_table(E),
          {table,{'$MOD','$FUN',[]}} = i(Q),
          true = binary_to_list(<<\"'$MOD':'$FUN'()\">>) 
                == format_info(Q, true),
          true = ets:delete(E)">>,

       <<"\"[]\" = qlc:info([], flat),
          \"[]\" = qlc:info([]),
          \"[]\" = qlc:info([], {flat, true})">>,

       <<"H = qlc:q([{X} || X <- [a,b]]),
         \"ets:match_spec_run([a,b],ets:match_spec_compile(\" ++ _ =
                format_info(H, true),
         \"ets:match_spec_run([a,b],ets:match_spec_compile(\" ++ _ =
                format_info(H, false)">>,

       <<"H = qlc:q([{X} || X <- [a,b], begin true end]),
          true = binary_to_list(<<\"qlc:q([{X}||X<-[a,b],begintrueend])\">>) 
             == format_info(H, true),
          true = binary_to_list(<<\"qlc:q([{X}||X<-[a,b],begintrueend])\">>)
             == format_info(H, false)">>,

       <<"H = qlc:q([A || {A} <- [{1},{2}], (A =:= 2) andalso true]),
          {call,_,{remote,_,{atom,_,ets},{atom,_,match_spec_run}},_} = 
             qlc:info(H, {format,abstract_code})">>,

       <<"H = qlc:q([{X} || X <- qlc:q([{X} || X <- [a,b], begin true end],
                                       unique), 
                            begin true end]),
          true = binary_to_list(<< 
         \"beginV1=qlc:q([{X}||X<-[a,b],begintrueend],[{unique,true}]),\"
         \"qlc:q([{X}||X<-V1,begintrueend])end\">>) == 
              format_info(H, true),
          true = binary_to_list(<<
         \"qlc:q([{X}||X<-qlc:q([{X}||X<-[a,b],begintrueend],\"
         \"[{unique,true}]),begintrueend])\">>) == format_info(H, false)">>,

       <<"H0 = qlc:q([{V3} || V3 <- qlc:q([{V1} || V1 <- [a,b], 
                                                   begin true end], unique), 
                              begin true end]),
          H = qlc:sort(H0),
          true = binary_to_list(<<
          \"qlc:sort(qlc:q([{V3}||V3<-qlc:q([{V1}||\"
          \"V1<-[a,b],begintrueend],[{unique,true}]),begintrueend]),[])\">>) 
              == format_info(H, false),
          true = binary_to_list(<<
          \"beginV2=qlc:q([{V1}||V1<-[a,b],begintrueend],[{unique,true}]),\"
          \"V4=qlc:q([{V3}||V3<-V2,begintrueend]),qlc:sort(V4,[])end\">>) 
              == format_info(H, true)">>,

       <<"H0 = qlc:q([X || X <- [true], begin true end]),
          H1 = qlc:q([{X} || X <- [a,b], begin true end], 
                     [{unique,begin [T] = qlc:e(H0), T end}]),
          {call,_,{remote,_,{atom,_,qlc},{atom,_,q}},
                  [{lc,_,{tuple,_,[{var,_,'X'}]},
                         [{generate,_,{var,_,'X'},
                                      {cons,_,{atom,_,a},_}},
                          {block, _, [{atom, _, true}]}]},
                   {cons,_,_,_}]} = i(H1, {format, abstract_code})">>,

       <<"E = ets:new(apa, [duplicate_bag]),
          true = ets:insert(E, [{1,a},{2,b},{3,c},{4,d}]),
          QH = qlc:q([X || {X,_} <- ets:tab2list(E), X > 2], unique),
          {qlc, _, [{generate, _, {list, _, _MS}}], [{unique, true}]} = 
                i(QH),
          [3,4] = lists:sort(qlc:e(QH)),
          ets:delete(E)">>,

       %% "Imported" variable.
       <<"F = fun(U) -> qlc:q([{X} || X <- [1,2,3,4,5,6], X > U]) end,
          QH = F(4),
          {call, _ ,
                {remote, _, {atom, _, ets},{atom, _, match_spec_run}},
                [{string, _, [1,2,3,4,5,6]},
                 {call, _,
                       _compile,
                       [{cons, _,
                              {tuple, _,
                                     [{atom, _,'$1'},
                                      {cons, _,
                                            {tuple,
                                                 _,
                                                [{atom, _,'>'},
                                                 {atom, _,'$1'},
                                                 {tuple,
                                                      _,
                                                     [{atom, _,const},
                                                      {integer, _,4}]}]},
                                            _},
                                      {cons, _, _, _}]},
                              {nil,_}}]}]} = i(QH, {format, abstract_code}),
          [{5},{6}] = qlc:e(QH),
          [{4},{5},{6}] = qlc:e(F(3))">>,

          <<"Fun = fun ?MODULE:i/2,
             L = [{#{k => #{v => Fun}}, Fun}],
             H = qlc:q([Q || Q <- L, Q =:= {#{k => #{v => Fun}}, Fun}]),
             L = qlc:e(H),
             {call,_,_,[{lc,_,{var,_,'Q'},
                         [{generate,_,_,_},
                          {op,_,_,_,_}]}]} =
                qlc:info(H, [{format,abstract_code}])">>

       ],
    run(Config, Ts),
    ok.

%% Nested QLC expressions. QLC expressions in filter and template.
nested_info(Config) when is_list(Config) ->
    Ts = [
       <<"L = [{1,a},{2,b},{3,c}],
          Q = qlc:q(
                [{X,R} || 
                    {X,_} <- qlc_SUITE:table(L, []),
                    begin % X imported
                        R = qlc:e(qlc:q([{X,Y} || {Y,_}
                                                    <- qlc_SUITE:table(L, []),
                                                  Y > X])),
                        true
                    end]),
          true = binary_to_list(<<
            \"qlc:q([{X,R}||{X,_}<-qlc_SUITE:the_list([{1,a},{2,b},{3,c}]),\"
            \"beginR=qlc:e(qlc:q([{X,Y}||{Y,_}<-qlc_SUITE:table(L,[]),Y>X]))\"
            \",trueend])\">>) == format_info(Q, true),
          [{1,[{1,2},{1,3}]},{2,[{2,3}]},{3,[]}] = qlc:e(Q)">>,

       <<"L = [{1,a},{2,b},{3,c}],
          Q = qlc:q( % X imported
                [{X,qlc:e(qlc:q([{X,Y} || {Y,_} <- qlc_SUITE:table(L, []),
                                          Y > X]))} || 
                    {X,_} <- qlc_SUITE:table(L, [])]),
          true = binary_to_list(<<
            \"qlc:q([{X,qlc:e(qlc:q([{X,Y}||{Y,_}<-qlc_SUITE:table(L,[]),\"
            \"Y>X]))}||{X,_}<-qlc_SUITE:the_list([{1,a},{2,b},{3,c}])])\">>)
              == format_info(Q, true),
          [{1,[{1,2},{1,3}]},{2,[{2,3}]},{3,[]}] = qlc:e(Q)">>,

       <<"L = [{1,a},{2,b},{3,c}],
          Q = qlc:q(
                [{X,R} || 
                    {X,_} <- qlc_SUITE:table(L, []),
                    begin % X imported
                        R = qlc:e(qlc:q([{X,Y} || {Y,_} 
                                                    <- qlc_SUITE:table(L, []),
                                                  Y =:= X])),
                        true
                    end]),
          true = binary_to_list(<<
            \"qlc:q([{X,R}||{X,_}<-qlc_SUITE:the_list([{1,a},{2,b},{3,c}]),\"
            \"beginR=qlc:e(qlc:q([{X,Y}||{Y,_}<-qlc_SUITE:table(L,[]),\"
            \"Y=:=X])),trueend])\">>) == format_info(Q, true),
          [{1,[{1,1}]},{2,[{2,2}]},{3,[{3,3}]}] = qlc:e(Q)">>,

       <<"L = [{1,a},{2,b},{3,c}],
          Q = qlc:q(
                [{X, % X imported
                  qlc:e(qlc:q([{X,Y} || {Y,_} <- qlc_SUITE:table(L, []),
                                        Y =:= X]))} || 
                    {X,_} <- qlc_SUITE:table(L, [])]),
          true = binary_to_list(<<
            \"qlc:q([{X,qlc:e(qlc:q([{X,Y}||{Y,_}<-qlc_SUITE:table(L,[]),\"
            \"Y=:=X]))}||{X,_}<-qlc_SUITE:the_list([{1,a},{2,b},{3,c}])])\">>)
             == format_info(Q, true),
          [{1,[{1,1}]},{2,[{2,2}]},{3,[{3,3}]}] = qlc:e(Q)">>,

       <<"L = [{1,a},{2,b},{3,c}],
          Q = qlc:q(
                [{X,R} || 
                    {X,_} <- qlc_SUITE:table(L, []),
                    begin
                        R = qlc:e(qlc:q([Y || Y <- [X]])),
                        true
                    end]),
          true = binary_to_list(<<
            \"qlc:q([{X,R}||{X,_}<-qlc_SUITE:the_list([{1,a},{2,b},{3,c}]),\"
            \"beginR=qlc:e(qlc:q([Y||Y<-[X]])),trueend])\">>)
             == format_info(Q, true),
          [{1,[1]},{2,[2]},{3,[3]}] = qlc:e(Q)">>,

       <<"L = [{1,a},{2,b},{3,c}],
          Q = qlc:q(
                [{X,qlc:e(qlc:q([Y || Y <- [X]]))} || 
                    {X,_} <- qlc_SUITE:table(L, [])]),
          true = binary_to_list(<<
            \"qlc:q([{X,qlc:e(qlc:q([Y||Y<-[X]]))}||{X,_}<-qlc_SUITE:\"
            \"the_list([{1,a},{2,b},{3,c}])])\">>) == format_info(Q, true),
          [{1,[1]},{2,[2]},{3,[3]}] = qlc:e(Q)">>,
          
       <<"L = [{1,a},{2,b}],
          Q = qlc:q(
                [{X,Y} ||
                    {X,_} <- qlc_SUITE:table(L, []),
                    {Y,_} <- qlc:q(
                               [{Z,V} || 
                                   {Z,_} <- qlc_SUITE:table(L, []),
                                   {V} <- qlc:q(
                                              [{W} || W 
                                                  <- qlc_SUITE:table(L, [])])
                                      ])
                       ]),
          true = binary_to_list(<<
           \"beginV1=qlc:q([{W}||W<-qlc_SUITE:the_list([{1,a},{2,b}])]),\"
           \"V2=qlc:q([{Z,V}||{Z,_}<-qlc_SUITE:the_list([{1,a},{2,b}]),\"
           \"{V}<-V1]),qlc:q([{X,Y}||{X,_}<-qlc_SUITE:the_list([{1,a},\"
           \"{2,b}]),{Y,_}<-V2])end\">>) == format_info(Q, true),
          [{1,1},{1,1},{1,2},{1,2},{2,1},{2,1},{2,2},{2,2}] = qlc:e(Q)">>

       ],
    run(Config, Ts),
    ok.


%% Lookup keys. Mostly test of patterns.
lookup1(Config) when is_list(Config) ->
    Ts = [
       <<"etsc(fun(E) ->
                Q = qlc:q([A || {A=3} <- ets:table(E)]),
                [3] = qlc:eval(Q),
                [3] = lookup_keys(Q) 
          end, [{1},{2},{3},{4}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([A || {A=3} <- ets:table(E)],{max_lookup,0}),
                [3] = qlc:eval(Q),
                false = lookup_keys(Q) 
          end, [{1},{2},{3},{4}])">>,

       <<"%% The lookup and max_lookup options interact.
          etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E),
                                (X =:= 1) or (X =:= 2)],
                          [{lookup,true},{max_lookup,1}]),
                {'EXIT', {no_lookup_to_carry_out, _}} = (catch qlc:e(Q))
         end, [{1},{2}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([{A,B,C,D} || {A,B}={C,D} <- ets:table(E)]),
                [{1,2,1,2},{3,4,3,4}] = lists:sort(qlc:eval(Q)),
                false = lookup_keys(Q)
          end, [{1,2},{3,4}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([{A,B,D} || {A,B}={D,A} <- ets:table(E)]),
                [{1,1,1},{2,2,2}] = lists:sort(qlc:eval(Q)),
                false = lookup_keys(Q)
          end, [{1,2},{2,2},{1,1}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([{A,B,D} || {A,B}={D,A} <- ets:table(E),
                                      (D =:= 2) or (B =:= 1)],
                          {max_lookup,infinity}),
                [{1,1,1},{2,2,2}] = qlc:eval(Q),
                [1,2] = lookup_keys(Q)
         end, [{1,2},{2,2},{1,1}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([{A,B,D} || {A,B}={D,A} <- ets:table(E),
                                      (D =:= 2) xor (B =:= 1)]),
                [{1,1,1},{2,2,2}] = qlc:eval(Q),
                [1,2] = lookup_keys(Q)
         end, [{1,2},{2,2},{1,1}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([3 || {{[3,4],apa}} <- ets:table(E)]),
                [3] = qlc:e(Q),
                [{[3,4],apa}] = lookup_keys(Q)
        end, [{{[4,3],foo}},{{[3,4],apa}}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([3 || {3} <- ets:table(E)]),
                [3] = qlc:e(Q),
                [3] = lookup_keys(Q)
        end, [{2},{3},{4}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([{X,Y,Z} || {{X,_},Y,Y={_,Z},X,Y} <- ets:table(E)]),
                [] = qlc:e(Q),
                false = lookup_keys(Q)
        end, [{{1,1},1,{1,1},1,1}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {X,X=2} <- ets:table(E)]),
                [2] = qlc:e(Q),
                [2] = lookup_keys(Q)
        end, [{2,2},{3,3}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {{_,3}={4,_}=X} <- ets:table(E)]),
                [{4,3}] = qlc:e(Q),
                [{4,3}] = lookup_keys(Q)
        end, [{{2,3}},{{4,3}}])">>,

       <<"U = 17.0,
          etsc(fun(E) ->
                Q = qlc:q([X || {_=X=_} <- ets:table(E)]),
                [U] = qlc:e(Q),
                false = lookup_keys(Q)
        end, [{U},{U+U,U}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([{X,Y,Z,W} || {X=Y}=Z={V=W} <- ets:table(E), 
                                        V == {h,g}]),
                [{{h,g},{h,g},{{h,g}},{h,g}}] = qlc:e(Q),
                [{h,g}] = lookup_keys(Q)
        end, [{h,g},{{h,g}}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([{C,Y,Z,X} || {{X=Y}=Z}={{A=B}=C} <- ets:table(E), 
                                        A == a, B =/= c]),
                [{{a},a,{a},a}] = qlc:e(Q),
                [{a}] = lookup_keys(Q)
        end, [{{1}},{{a}}])">>,

       <<"etsc(fun(E) ->
               Q = qlc:q([{A,D,Y,X} || 
                             {{A={B=C}},{D={C}}} = {X,Y} <- ets:table(E),
                             [] == B]),
                [{{[]},{[]},{{[]}},{{[]}}}] = qlc:e(Q),
                [{{[]}}] = lookup_keys(Q)
        end, [{{{[]}},{{[]}}}])">>,

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X}=X <- ets:table(E)]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
         end, [{1},{a}])">>,
        %% {warnings,[{{2,37},qlc,nomatch_pattern}]}},
        []},

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {X=X,Y=Y}={Y=Y,X=X} <- ets:table(E), 
                                {} == X]),
                [{}] = qlc:e(Q),
                [{}] = lookup_keys(Q)
        end, [{{},{}},{[],[]}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {3+4=7,X} <- ets:table(E), 
                                X =:= 3+997]),
                [1000] = qlc:e(Q),
                [7] = lookup_keys(Q)
        end, [{7,1000},{8,1000}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([{X, Y} || [X]=[Y] <- ets:table(E)]),
                [] = qlc:eval(Q),
                false = lookup_keys(Q)
        end, [{a}])">>,

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || X={1,2,3,X,5} <- ets:table(E)]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
         end, [{a},{b}])">>,
        %% {warnings,[{{2,35},qlc,nomatch_pattern}]}},
        []},

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || X=[1,2,3,X,5] <- ets:table(E)]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
         end, [{a},{b}])">>,
        %% {warnings,[{{2,35},qlc,nomatch_pattern}]}},
        []},

       <<"etsc(fun(E) ->
                Q = qlc:q([X || X = <<X>> <- ets:table(E)]),
                [] = qlc:e(Q),
                false = lookup_keys(Q)
        end, [{a},{b}])">>,

       <<"Tre = 3.0,
          etsc(fun(E) ->
                Q = qlc:q([{A,B} || {A,B}={{a,C},{a,C}} <- ets:table(E), 
                                    C =:= Tre]),
                [] = qlc:e(Q),
                [{a,Tre}] = lookup_keys(Q)
        end, [{a,b}])">>,

       <<"A = 3,
          etsc(fun(E) ->
                Q = qlc:q([X || X <- ets:table(E), A =:= element(1, X)]),
                [{3,3}] = qlc:e(Q),
                [3] = lookup_keys(Q)
        end, [{1,a},{3,3}])">>,

       <<"A = 3,
          etsc(fun(E) ->
                Q = qlc:q([X || X <- ets:table(E), A =:= erlang:element(1, X)]),
                [{3,3}] = qlc:e(Q),
                [3] = lookup_keys(Q)
        end, [{1,a},{3,3}])">>,

       <<"etsc(fun(E) ->
                A = 3,
                Q = qlc:q([X || X <- ets:table(E), 
                                A == element(1,X), 
                                element(1,X) =:= a]),
                [] = qlc:e(Q),
                [a] = lookup_keys(Q)
        end, [{a},{b},{c}])">>,

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X = {[a,Z]}, 
                                  Z = [foo, {[Y]}], 
                                  Y = {{foo,[X]}}} <- ets:table(E)]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
         end, [{a,b,c},{d,e,f}])">>,
        %% {warnings,[{{2,34},qlc,nomatch_pattern}]}}
        []}

       ],
    run(Config, Ts),
    ok.

%% Lookup keys. Mostly test of filters.
lookup2(Config) when is_list(Config) ->
    Ts = [
       <<"%% Only guards are inspected. No lookup.
          etsc(fun(E) ->
                 Q = qlc:q([{X,Y} || {X,Y} <- ets:table(E),
                                     ((Y = X) =:= 3)]),
                 {'EXIT', {{badmatch,4},_}} = (catch qlc:e(Q))
         end, [{3,3},{4,true}])">>,

       <<"%% Only guards are inspected. No lookup.
          etsc(fun(E) ->
                 Q = qlc:q([{X,Y} || {X,Y} <- ets:table(E),
                                     Y = (X =:= 3)]),
                 {'EXIT', {{badmatch,false},_}} = (catch qlc:e(Q))
         end, [{false,3},{true,3}])">>,

       <<"%% Only guards are inspected. No lookup.
          etsc(fun(E) ->
                 Q = qlc:q([{X,Y} || {X,Y} <- ets:table(E),
                                     Y = (X =:= 3)]),
                 {'EXIT', {{badmatch,false},_}} = (catch qlc:e(Q))
         end, [{3,true},{4,true}])">>,

       <<"%% Only guards are inspected. No lookup.
          E1 = ets:new(e, [ordered_set]),
          true = ets:insert(E1, [{1,1}, {2,2}, {3,3}, {4,4}, {5,5}]),
          E2 = ets:new(join, [ordered_set]),
          true = ets:insert(E2, [{true,1},{false,2}]),
          Q = qlc:q([{X,Z} || {_,X} <- ets:table(E1),
                              {Y,Z} <- ets:table(E2),
                              Y = (X =:= 3)]),
          {'EXIT', {{badmatch,false},_}} = (catch qlc:e(Q)),
          ets:delete(E1),
          ets:delete(E2)">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([{A,B,D} || {A,B}={D,A} <- ets:table(E), 
                                      (A =:= 3) or (4 =:= D)]),
                [{3,3,3},{4,4,4}] = lists:sort(qlc:e(Q)),
                [3,4] = lookup_keys(Q)
        end, [{2,2},{3,3},{4,4}])">>,

       <<"etsc(fun(E) ->
               Q = qlc:q([X || {X,U} <- ets:table(E), X =:= U]),
               [1] = qlc:e(Q),
               false = lookup_keys(Q)
       end, [{1,1}])">>,

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([{X,Y} || {X=Y} <- ets:table(E), 
                                     {[X],4} =:= {[3],X}]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
         end, [{1}, {2}])">>,
        %% {warnings,[{{3,46},qlc,nomatch_filter}]}},
        []},

       {cres,
        <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), 
                                X == 1, X =:= 2]),
                [] = qlc:e(Q),
                false = lookup_keys(Q)
        end, [{1}, {2}])">>,
        %% {warnings,[{{3,43},qlc,nomatch_filter}]}},
        []},

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([{X,Y} || {X=Y} <- ets:table(E), 
                                     {[X,Y],4} =:= {[3,X],X}]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
         end, [{1}, {2}])">>,
        %% {warnings,[{{3,48},qlc,nomatch_filter}]}},
        []},

       <<"etsc(fun(E) ->
                Q = qlc:q([{X,Y} || {X,Y} <- ets:table(E), 
                                    ({X,3} =:= {Y,Y}) or (X =:= 4)]),
                [{3,3},{4,4}] = lists:sort(qlc:e(Q)),
                [3,4] = lookup_keys(Q)
        end, [{2,2},{3,3},{4,4},{5,5}])">>,

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X} <- ets:table(E), {[X]} =:= {[3,4]}]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
         end, [{[3]},{[3,4]}])">>,
        %% {warnings,[{{2,61},qlc,nomatch_filter}]}},
        []},

       <<"etsc(fun(E) ->
                U = 18, 
                Q = qlc:q([{X,Y} || {X=Y} <- ets:table(E), [X|a] =:= [3|U]]),
                [] = qlc:e(Q),
                [3] = lookup_keys(Q)
        end, [{2}, {3}])">>,

       <<"etsc(fun(E) ->
                U = 18, V = 19,
                Q = qlc:q([{X,Y} || {X=Y} <- ets:table(E), 
                                    [X|V] =:= [3|U+1]]),
                [{3,3}] = qlc:e(Q),
                [3] = lookup_keys(Q)
        end, [{2},{3}])">>,

       <<"%% Blocks are not handled.
          etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), begin X == a end]),
                [a] = qlc:e(Q),
                false = lookup_keys(Q)
        end, [{a},{b}])">>,

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X} <- ets:table(E), 
                                 (3 =:= X) or (X =:= 12), 
                                 (8 =:= X) or (X =:= 10)]),
                 [] = lists:sort(qlc:e(Q)),
                 false = lookup_keys(Q)
         end, [{2},{3},{4},{8}])">>,
        %% {warnings,[{{4,44},qlc,nomatch_filter}]}},
        []},

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X} <- ets:table(E),
                                 ((3 =:= X) or (X =:= 12)) 
                                  and ((8 =:= X) or (X =:= 10))]),
                 [] = lists:sort(qlc:e(Q)),
                 false = lookup_keys(Q)
         end, [{2},{3},{4},{8}])">>,
        %% {warnings,[{{4,35},qlc,nomatch_filter}]}},
        []},

       <<"F = fun(U) ->
                Q = qlc:q([X || {X} <- [a,b,c], 
                                 X =:= if U -> true; true -> false end]),
                [] = qlc:eval(Q),
                false = lookup_keys(Q)
              end,
          F(apa)">>,

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X=1,X} <- ets:table(E), X =:= 2]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
           end, [{1,1},{2,1}])">>,
        %% {warnings,[{{2,61},qlc,nomatch_filter}]}},
        []},

       <<"Two = 2.0,
          etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), X =:= Two]),
                [Two] = qlc:e(Q),
                [Two] = lookup_keys(Q)
        end, [{2.0},{2}])">>,

       <<"etsc(fun(E) ->
                %% This float is equal (==) to an integer. Not a constant!
                Q = qlc:q([X || {X} <- ets:table(E), X == {a,b,c,[2.0]}]),
                [_,_] = qlc:e(Q),
                false = lookup_keys(Q)
        end, [{{a,b,c,[2]}},{{a,b,c,[2.0]}}])">>,

       <<"%% Must _not_ regard floats as constants. Check imported variables
          %% in runtime.
          etsc(fun(E) -> 
                U = 3.0,
                QH = qlc:q([X || {X,_} <- ets:table(E), X =:= U]),
                [] = qlc:e(QH),
                [U] = lookup_keys(QH)
        end, [{1,a},{2,b},{3,c},{4,d}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), 
                                length(X) =:= 1]),
                [[1]] = qlc:e(Q),
                false = lookup_keys(Q)
        end, [{[1]},{[2,3]}])">>,

       <<"etsc(fun(E) ->
                A=3, 
                Q = qlc:q([X || {X,Y} <- ets:table(E), X =:= A, Y =:= 3]),
                [3] = qlc:e(Q),
                [3] = lookup_keys(Q)
        end, [{3,3},{4,3}])">>,

       <<"etsc(fun(E) ->
                A = 1,
                Q = qlc:q([X || {X} <- ets:table(E), 
                                X =:= 1, <<X>> =:= <<A>>]),
                [1] = qlc:e(Q),
                [1] = lookup_keys(Q)
        end, [{1},{2}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), X == a]),
                [a] = qlc:e(Q),
                [a] = lookup_keys(Q)
        end, [{a},{b},{c}])">>,

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X}=Y <- ets:table(E), 
                                 element(2, Y) == b, 
                                 X =:= 1]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
         end, [{1,b},{2,3}])">>,
        %% {warnings,[{2,sys_core_fold,nomatch_guard},
	%% 	   {3,qlc,nomatch_filter},
	%% 	   {3,sys_core_fold,{eval_failure,badarg}}]}},
        {warnings,[{2,sys_core_fold,nomatch_guard},
		   {3,sys_core_fold,{eval_failure,badarg}}]}},

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), element(1,{X}) =:= 1]),
                [1] = qlc:e(Q),
                [1] = lookup_keys(Q)
        end, [{1}, {2}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), 1 =:= element(1,{X})]),
                [1] = qlc:e(Q),
                [1] = lookup_keys(Q)
        end, [{1}, {2}])">>,

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X} <- ets:table(E), 
                                 X =:= {1}, 
                                 element(1,X) =:= 2]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
         end, [{{1}},{{2}}])">>,
        %% {warnings,[{{4,47},qlc,nomatch_filter}]}},
        []},

       {cres,
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X} <- ets:table(E), 
                                 X =:= {1}, 
                                 element(1,X) =:= element(1, {2})]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
         end, [{{1}},{{2}}])">>,
        %% {warnings,[{{4,47},qlc,nomatch_filter}]}},
        []},

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E),
                                element(1,X) =:= 1, X =:= {1}]),
                [{1}] = qlc:e(Q),
                [{1}] = lookup_keys(Q)
        end, [{{1}},{{2}}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E),
                                {{element(1,element(1,{{1}}))}} =:= {X}]),
                [{1}] = qlc:e(Q),
                [{1}] = lookup_keys(Q)
        end, [{{1}},{{2}}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || X <- ets:table(E),
                                {element(1,element(1, {{1}}))} =:= 
                                      {element(1,X)}]),
                [{1}] = qlc:e(Q),
                [1] = lookup_keys(Q)
        end, [{1},{2}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {{X,Y}} <- ets:table(E), 
                                (X =:= 1) and (Y =:= 2) 
                                    or (X =:= 3) and (Y =:= 4)]),
                [1,3] = lists:sort(qlc:e(Q)),
                [{1,2}, {3,4}] = lookup_keys(Q)
        end, [{{1,2}}, {{3,4}}, {{2,3}}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {{X,a}} <- ets:table(E), X =:= 3]),
                [3] = qlc:e(Q),
                [{3,a}] = lookup_keys(Q)
        end, [{{3,a}},{{3,b}}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {{X,Y},_Z} <- ets:table(E), 
                                X =:= 3, Y =:= a]),
                [3] = qlc:e(Q),
                [{3,a}] = lookup_keys(Q)
        end, [{{3,a},3}, {{4,a},3}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {{X,Y},_Z} <- ets:table(E), 
                                (X =:= 3) and (Y =:= a)
                                   or (X =:= 4) and (Y =:= a)]),
                [3,4] = qlc:e(Q),
                [{3,a}, {4,a}] = lookup_keys(Q)
        end, [{{3,a},3}, {{4,a},3}])">>,

       {cres, 
        <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X} <- ets:table(E), 
                                 (X =:= 3) and (X =:= a)]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
         end, [{3}, {4}])">>,
        %% {warnings,[{{3,44},qlc,nomatch_filter}]}},
        []},

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {{X,Y}} <- ets:table(E), 
                                X =:= 3, ((Y =:= a) or (Y =:= b))]),
                [3,3] = qlc:e(Q),
                [{3,a},{3,b}] = lists:sort(lookup_keys(Q))
        end, [{{3,a}},{{2,b}},{{3,b}}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {X,Y} <- ets:table(E), 
                                ((X =:= 3) or (Y =:= 4))  and (X == a)]),
                [a] = qlc:e(Q),
                [a] = lookup_keys(Q)
        end, [{a,4},{3,3}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {X,Y} <- ets:table(E), 
                                (X =:= 3) or ((Y =:= 4)  and (X == a))]),
                [3,a] = lists:sort(qlc:e(Q)),
                [3,a] = lookup_keys(Q)
        end, [{a,4},{3,3}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {{X,Y}} <- ets:table(E), 
                                (X =:= 3) or ((Y =:= 4)  and (X == a))]),
                [3,a] = lists:sort(qlc:e(Q)),
                false = lookup_keys(Q)
        end, [{{3,a}},{{2,b}},{{a,4}}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || {{X,Y}} <- ets:table(E), 
                                ((X =:= 3) or (Y =:= 4))  and (X == a)]),
                [a] = lists:sort(qlc:e(Q)),
                [{a,4}] = lookup_keys(Q)
        end, [{{3,a}},{{2,b}},{{a,4}}])">>,

        <<"etsc(fun(E) ->
                NoAnswers = 3*3*3+2*2*2,
                Q = qlc:q([{X,Y,Z} || 
                              {{X,Y,Z}} <- ets:table(E), 
                              (((X =:= 4) or (X =:= 5)) and
                               ((Y =:= 4) or (Y =:= 5)) and
                               ((Z =:= 4) or (Z =:= 5))) or
                              (((X =:= 1) or (X =:= 2) or (X =:= 3)) and
                               ((Y =:= 1) or (Y =:= 2) or (Y =:= 3)) and
                               ((Z =:= 1) or (Z =:= 2) or (Z =:= 3)))],
                          {max_lookup, NoAnswers}),
                 {list, {table, _}, _} = i(Q),
                 [{1,1,1},{2,2,2},{3,3,3}] = lists:sort(qlc:e(Q)),
                 true = NoAnswers =:= length(lookup_keys(Q))
         end, [{{1,1,1}},{{2,2,2}},{{3,3,3}},{{3,3,4}},{{4,1,1}}])">>,

        <<"etsc(fun(E) ->
                Q = qlc:q([{X,Y,Z} || 
                              {{X,Y,Z}} <- ets:table(E), 
                              (((X =:= 4) or (X =:= 5)) and
                               ((Y =:= 4) or (Y =:= 5)) and
                               ((Z =:= 4) or (Z =:= 5))) or
                              (((X =:= 1) or (X =:= 2) or (X =:= 3)) and
                               ((Y =:= 1) or (Y =:= 2) or (Y =:= 3)) and
                               ((Z =:= 1) or (Z =:= 2) or (Z =:= 3)))],
                          {max_lookup, 10}),
                 [{1,1,1},{2,2,2},{3,3,3}] = lists:sort(qlc:e(Q)),
                 {table,{ets,table,[_,[{traverse,{select,_}}]]}} = i(Q)
         end, [{{1,1,1}},{{2,2,2}},{{3,3,3}},{{3,3,4}},{{4,1,1}}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || X={_,_,_} <- ets:table(E), 
                                element(1, X) =:= 3, element(2, X) == a]),
                [{3,a,s}] = qlc:e(Q),
                [3] = lookup_keys(Q)
        end, [{1,c,q},{2,b,r},{3,a,s}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([X || X <- ets:table(E), 
                                element(0, X) =:= 3]),
                [] = qlc:e(Q),
                false = lookup_keys(Q)
        end, [{1},{2}])">>,

       <<"etsc(fun(E) ->
                F = fun(_) -> 3 end, 
                %% No occurs check; X =:= F(X) is ignored.
                Q = qlc:q([X || {X} <- ets:table(E), 
                                X =:= 3, X =:= F(X)]),
                {qlc,_,[{generate,_,{list,{table,_},_}},_],[]} = i(Q),
                [3] = lists:sort(qlc:e(Q)),
                [3] = lookup_keys(Q)
        end, [{2},{3},{4}])">>,

       <<"etsc(fun(E) ->
                A = a, B = a,
                Q = qlc:q([X || {{X,Y}} <- ets:table(E), 
                                ((X =:= A) and (Y =:= B))
                                 or ((X =:= B) and (Y =:= A))]),
                [a] = qlc:e(Q),
                %% keys are usorted, duplicate removed:
                [{a,a}] = lookup_keys(Q) 
        end, [{{a,a}},{{b,b}}])">>,

       <<"etsc(fun(E) ->
                A = a, B = b,
                Q = qlc:q([X || {{X,Y}} <- ets:table(E), 
                                ((X =:= A) and (Y =:= B))
                                 or ((X =:= B) and (Y =:= A))]),
                [a,b] = lists:sort(qlc:e(Q)),
                [{a,b},{b,a}] = lookup_keys(Q)
        end, [{{a,b}},{{b,a}},{{c,a}},{{d,b}}])">>,

       %% The atom 'fail' is recognized - lookup.
       <<"etsc(fun(E) ->
                Q = qlc:q([A || {A} <- ets:table(E), 
                                (A =:= 2) 
                                    orelse fail
                                   ]),
                [2] = lists:sort(qlc:e(Q)),
                [2] = lookup_keys(Q)
           end, [{1},{2}])">>

       ],

    ok = run(Config, Ts),

    TsR = [
       %% is_record/2,3:
       <<"etsc(fun(E) ->
                Q = qlc:q([element(1, X) || X <- ets:table(E), 
                                            erlang:is_record(X, r, 2)]),
                 [r] = qlc:e(Q),
                 [r] = lookup_keys(Q)
         end, [{keypos,1}], [#r{}])">>,
       <<"etsc(fun(E) ->
                Q = qlc:q([element(1, X) || X <- ets:table(E), 
                                            is_record(X, r, 2)]),
                 [r] = qlc:e(Q),
                 [r] = lookup_keys(Q)
         end, [{keypos,1}], [#r{}])">>,
       {cres,
        <<"etsc(fun(E) ->
                Q = qlc:q([element(1, X) || X <- ets:table(E), 
                                            record(X, r)]),
                 [r] = qlc:e(Q),
                 [r] = lookup_keys(Q)
         end, [{keypos,1}], [#r{}])">>,
        {warnings,[{{4,45},erl_lint,{obsolete_guard,{record,2}}}]}},
       <<"etsc(fun(E) ->
                Q = qlc:q([element(1, X) || X <- ets:table(E), 
                                            erlang:is_record(X, r)]),
                 [r] = qlc:e(Q),
                 [r] = lookup_keys(Q)
         end, [{keypos,1}], [#r{}])">>,
       <<"etsc(fun(E) ->
                Q = qlc:q([element(1, X) || X <- ets:table(E), 
                                            is_record(X, r)]),
                 [r] = qlc:e(Q),
                 [r] = lookup_keys(Q)
         end, [{keypos,1}], [#r{}])">>

       ],

    ok = run(Config, <<"-record(r, {a}).\n">>, TsR),

    Ts2 = [
       <<"etsc(fun(E) ->
                 Q0 = qlc:q([X || 
                                X <- ets:table(E),
                                (element(1, X) =:= 1) or 
                                  (element(1, X) =:= 2)],
                           {cache,ets}),
                 Q = qlc:q([{X,Y} ||
                               X <- [1,2],
                               Y <- Q0]),
                 {qlc,_,[{generate,_,{list,[1,2]}},
                         {generate,_,{table,_}}], []} = i(Q),
                 [{1,{1}},{1,{2}},{2,{1}},{2,{2}}] = lists:sort(qlc:e(Q)),
                 [1,2] = lookup_keys(Q) 
          end, [{keypos,1}], [{1},{2}])">>,

       <<"etsc(fun(E) ->
                 Q0 = qlc:q([X || 
                                X <- ets:table(E),
                                (element(1, X) =:= 1) or 
                                  (element(1, X) =:= 2)]),
                 Q = qlc:q([{X,Y} ||
                               X <- [1,2],
                               Y <- Q0],
                           {cache,true}),
                 {qlc,_,[{generate,_,{list,[1,2]}},
                         {generate,_,{table,_}}],[]} = i(Q),
                 [1,2] = lookup_keys(Q) 
          end, [{keypos,1}], [{1},{2}])">>,

       %% One introduced QLC expression (join, ms), and the cache option.
       <<"%% Match spec and lookup. The lookup is done twice, which might
          %% be confusing...
          etsc(fun(E) ->
                       Q = qlc:q([{X,Y} ||
                                     X <- [1,2],
                                     {Y} <- ets:table(E),
                                     (Y =:= 1) or (Y =:= 2)],
                                 []),
                       [{1,1},{1,2},{2,1},{2,2}] = qlc:e(Q),
                       {qlc,_,[{generate,_,{list,[1,2]}},
                               {generate,_,{list,{table,_},_}}],[]} = i(Q),
                       [1,2] = lookup_keys(Q)
               end, [{keypos,1}], [{1},{2},{3}])">>,
       <<"%% The same as last example, but with cache. 
          %% No cache needed (always one lookup only).
          etsc(fun(E) ->
                       Q = qlc:q([{X,Y} ||
                                     X <- [1,2],
                                     {Y} <- ets:table(E),
                                     (Y =:= 1) or (Y =:= 2)],
                                 [cache]),
                       [{1,1},{1,2},{2,1},{2,2}] = qlc:e(Q),
                       {qlc,_,[{generate,_,{list,[1,2]}},
                               {generate,_,{list,{table,_},_}}],[]} = i(Q),
                       [1,2] = lookup_keys(Q)
               end, [{keypos,1}], [{1},{2},{3}])">>,

       <<"%% And again, this time only lookup, no mach spec.
          etsc(fun(E) ->
                       Q = qlc:q([{X,Y} ||
                                     X <- [1,2],
                                     Y <- ets:table(E),
                                     (element(1, Y) =:= 1) 
                                      or (element(1, Y) =:= 2)],
                                 []),
                       [{1,{1}},{1,{2}},{2,{1}},{2,{2}}] = qlc:e(Q),
                       {qlc,_,[{generate,_,{list,[1,2]}},
                               {generate,_,{table,_}}],[]} = i(Q),
                       [1,2] = lookup_keys(Q)
               end, [{keypos,1}], [{1},{2},{3}])">>,
       <<"%% As last one, but with cache.
          %% No cache needed (always one lookup only).
          etsc(fun(E) ->
                       Q = qlc:q([{X,Y} ||
                                     X <- [1,2],
                                     Y <- ets:table(E),
                                     (element(1, Y) =:= 1) 
                                      or (element(1, Y) =:= 2)],
                                 [cache]),
                       {qlc,_,[{generate,_,{list,[1,2]}},
                               {generate,_,{table,_}}],[]} = i(Q),
                       [{1,{1}},{1,{2}},{2,{1}},{2,{2}}] = qlc:e(Q),
                       [1,2] = lookup_keys(Q)
               end, [{keypos,1}], [{1},{2},{3}])">>,

       <<"%% Lookup only. No cache.
          etsc(fun(E) ->
                       Q = qlc:q([{X,Y} ||
                                     X <- [1,2],
                                     {Y=2} <- ets:table(E)],
                                 []),
                       {qlc,_,[{generate,_,{list,[1,2]}},
                               {generate,_,{table,_}}],[]} = i(Q),
                       [{1,2},{2,2}] = qlc:e(Q),
                       [2] = lookup_keys(Q)
               end, [{keypos,1}], [{1},{2},{3}])">>,
       <<"%% Lookup only. No cache.
          etsc(fun(E) ->
                       Q = qlc:q([{X,Y} ||
                                     X <- [1,2],
                                     {Y=2} <- ets:table(E)],
                                 [cache]),
                       {qlc,_,[{generate,_,{list,[1,2]}},
                               {generate,_,{table,_}}],[]} = i(Q),
                       [{1,2},{2,2}] = qlc:e(Q),
                       [2] = lookup_keys(Q)
               end, [{keypos,1}], [{1},{2},{3}])">>,
       <<"%% Matchspec only. No cache.
          etsc(fun(E) ->
                       Q = qlc:q([{X,Y} ||
                                     X <- [1,2],
                                     {Y} <- ets:table(E),
                                     Y > 1],
                                 []),
                       {qlc,_,[{generate,_,{list,[1,2]}},
                               {generate,_,
                                {table,{ets,_,[_,[{traverse,_}]]}}}],[]} = 
                                       i(Q),
                       [{1,2},{1,3},{2,2},{2,3}] = lists:sort(qlc:e(Q)),
                       false = lookup_keys(Q)
               end, [{keypos,1}], [{1},{2},{3}])">>,
       <<"%% Matchspec only. Cache
          etsc(fun(E) ->
                       Q = qlc:q([{X,Y} ||
                                     X <- [1,2],
                                     {Y} <- ets:table(E),
                                     Y > 1],
                                 [cache]),
                       {qlc,_,[{generate,_,{list,[1,2]}},
                            {generate,_,{qlc,_,
                           [{generate,_,{table,{ets,_,[_,[{traverse,_}]]}}}],
                          [{cache,ets}]}}],[]} = i(Q),
                       [{1,2},{1,3},{2,2},{2,3}] = lists:sort(qlc:e(Q)),
                       false = lookup_keys(Q)
               end, [{keypos,1}], [{1},{2},{3}])">>,
       <<"%% An empty list. Always unique and cached.
          Q = qlc:q([X || {X} <- [], X =:= 1, begin X > 0 end],
                   [{cache,true},{unique,true}]),
          {qlc,_,[{generate,_,{list,[]}},_],[{unique,true}]} = i(Q),
          _ = qlc:info(Q),
          [] = qlc:e(Q)">>,
       <<"%% A list is always cached.
          Q = qlc:q([{X,Y} || Y <- [1,2], X <- [2,1,2]],
                    [cache]),
          {qlc,_,[{generate,_,{list,[1,2]}},
                  {generate,_,{list,[2,1,2]}}],[]} = i(Q),
          [{2,1},{1,1},{2,1},{2,2},{1,2},{2,2}] = qlc:e(Q)">>,
       <<"%% But a processed list is never cached.
          Q = qlc:q([{X,Y} || Y <- [1,2], X <- [2,1,2], X > 1],
                    [cache]),
          {qlc,_,[{generate,_, {list,[1,2]}},
                  {generate,_,{qlc,_,
                               [{generate,_,{list,{list,[2,1,2]},_}}],
                               [{cache,ets}]}}],[]} = i(Q),
          [{2,1},{2,1},{2,2},{2,2}] = qlc:e(Q)">>,
       <<"%% A bug fixed in R11B-2: coalescing simple_qlc:s works now.
          Q0 = qlc:q([X || {X} <- [{1},{2},{3}]],  {cache, ets}),
          Q1 = qlc:q([X || X <- Q0], {cache, list}),
          Q = qlc:q([{Y,X} || Y <- [1,2], X <- Q1, X < 2], {cache, list}),
          {qlc,_,[{generate,_,{list,_}},
                  {generate,_,{qlc,_,[{generate,_,{list,{list,_},_}}],
                               [{cache,ets}]}},_],[]} = i(Q),
          [{1,1},{2,1}] = qlc:e(Q)">>,
       <<"Q = qlc:q([{X,Y} || Y <- [1,2], X <- [1,2], X > 1],
                    [cache,unique]),
          {qlc,_,[{generate,_,{list,[1,2]}},
                  {generate,_,{qlc,_,
                               [{generate,_,{list,{list,[1,2]},_}}],
                               [{cache,ets},{unique,true}]}}],
           [{unique,true}]} = i(Q),
          [{2,1},{2,2}] = qlc:e(Q)">>,
       <<"L = [1,2,3],
          QH1 = qlc:q([{X} || X <- L, X > 1]),
          QH2 = qlc:q([{X} || X <- QH1, X > 0], [cache]),
          [{{2}},{{3}}] = qlc:e(QH2),
          {list,{list,{list,L},_},_} = i(QH2)">>,
       <<"L = [1,2,3,1,2,3],
          QH1 = qlc:q([{X} || X <- L, X > 1]),
          QH2 = qlc:q([{X} || X <- QH1, X > 0], [cache,unique]),
          [{{2}},{{3}}] = qlc:e(QH2),
          {qlc,_,[{generate,_,{list,{list,{list,L},_},_}}],
           [{unique,true}]} = i(QH2)">>

      ],

    ok = run(Config, Ts2),

    LTs = [
       <<"etsc(fun(E) ->
                       Q  = qlc:q([X || X <- ets:table(E), 
                                        element(1, X) =:= 1],
                                  {lookup,true}),
                       {table,L} = i(Q),
                       true = is_list(L),
                       [{1,a}] = qlc:e(Q),
                       [1] = lookup_keys(Q)
               end, [{1,a},{2,b}])">>,
       <<"%% No lookup, use the match spec for traversal instead.
          etsc(fun(E) ->
                       Q  = qlc:q([X || X <- ets:table(E), 
                                        element(1, X) =:= 1],
                                  {lookup,false}),
                       {table,{ets,table,_}} = i(Q),
                       [{1,a}] = qlc:e(Q),
                       false = lookup_keys(Q)
               end, [{1,a},{2,b}])">>,
       <<"%% As last one. {max_lookup,0} has the same effect.
          etsc(fun(E) ->
                       Q  = qlc:q([X || X <- ets:table(E), 
                                        element(1, X) =:= 1],
                                  {max_lookup,0}),
                       {table,{ets,table,_}} = i(Q),
                       [{1,a}] = qlc:e(Q),
                       false = lookup_keys(Q)
               end, [{1,a},{2,b}])">>

       ],

    ok = run(Config, LTs),

    ok.

%% Lookup keys. With records.
lookup_rec(Config) when is_list(Config) ->
    Ts = [
       <<"etsc(fun(E) ->
                Q = qlc:q([A || #r{a = A} <- ets:table(E), 
                                (A =:= 3) or (4 =:= A)]),
                [3] = qlc:e(Q),
                [3,4] = lookup_keys(Q)
            end, [{keypos,2}], [#r{a = a}, #r{a = 3}, #r{a = 5}])">>,

       {cres, 
        <<"etsc(fun(E) ->
                 Q = qlc:q([A || #r{a = 17 = A} <- ets:table(E),
                                 (A =:= 3) or (4 =:= A)]),
                 [] = qlc:e(Q),
                 false = lookup_keys(Q)
             end, [{keypos,2}], [#r{a = 17}, #r{a = 3}, #r{a = 5}])">>,
        %% {warnings,[{{4,44},qlc,nomatch_filter}]}},
        []},

      <<"%% Compares an integer and a float.
         etsc(fun(E) ->
                Q = qlc:q([A || #r{a = 17 = A} <- ets:table(E),
                                (A == 17) or (17.0 == A)]),
                [_] = qlc:e(Q),
                [_] = lookup_keys(Q)
            end, [{keypos,2}], [#r{a = 17}, #r{a = 3}, #r{a = 5}])">>,

      <<"%% Compares an integer and a float.
         %% There is a test in qlc_pt.erl (Op =:= '=:=', C1 =:= C2), but
         %% that case is handled in an earlier clause (unify ... E, E).
         etsc(fun(E) ->
                Q = qlc:q([A || #r{a = 17.0 = A} <- ets:table(E),
                                (A =:= 17) or (17.0 =:= A)]),
                [_] = qlc:e(Q),
                [_] = lookup_keys(Q)
            end, [{keypos,2}], [#r{a = 17.0}, #r{a = 3}, #r{a = 5}])">>,

      <<"%% Matches an integer and a float.
         etsc(fun(E) ->
                Q = qlc:q([A || #r{a = 17 = A} <- ets:table(E),
                                (A =:= 17) or (17.0 =:= A)]),
                [_] = qlc:e(Q),
                [_] = lookup_keys(Q)
            end, [{keypos,2}], [#r{a = 17}, #r{a = 3}, #r{a = 5}])">>,

      <<"etsc(fun(E) ->
                F = fun(_) -> 17 end,
                Q = qlc:q([A || #r{a = A} <- ets:table(E),
                                (F(A) =:= 3) and (A =:= 4)]),
                [] = qlc:e(Q),
                false = lookup_keys(Q) % F(A) could fail
            end, [{keypos,2}], [#r{a = 4}, #r{a = 3}, #r{a = 5}])">>,

      <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), 
                                #r{} == X]),
                [#r{}] = lists:sort(qlc:e(Q)),
                {call,_,_,[_,_]} = i(Q, {format, abstract_code}),
                [#r{}] = lookup_keys(Q)
        end, [{#r{}},{#r{a=foo}}])">>,

      <<"etsc(fun(E) ->
                Q = qlc:q([R#r.a || R <- ets:table(E), R#r.a =:= foo]),
                [foo] = qlc:e(Q),
                [_] = lookup_keys(Q)
        end, [{keypos,2}], [#r{a=foo}])">>
       ],
    run(Config, <<"-record(r, {a}).\n">>, Ts),
    ok.

%% Using indices for lookup.
indices(Config) when is_list(Config) ->
    Ts = [
       <<"L = [{1,a},{2,b},{3,c}],
          QH = qlc:q([element(1, X) || X <- qlc_SUITE:table(L, [2]),
                                       (element(2, X) =:= a)
                                           or (b =:= element(2, X))]),
          {list, {table,{qlc_SUITE,list_keys,[[a,b],2,L]}}, _MS} = i(QH),
          [1,2] = qlc:eval(QH)">>,

       <<"L = [{1,a},{2,b},{3,c}],
          QH = qlc:q([element(1, X) || X <- qlc_SUITE:table(L, [2]),
                                       begin (element(2, X) =:= a)
                                           or (b =:= element(2, X)) end]),
          {qlc,_,[{generate,_,{table,{call,_,
                               {remote,_,_,{atom,_,the_list}},_}}},_],[]}
                  = i(QH),
          [1,2] = qlc:eval(QH)">>,

       <<"L = [{1,a,q},{2,b,r},{3,c,s}],
          QH = qlc:q([element(1, X) || X <- qlc_SUITE:table(L, [2,3]),
                                       (element(3, X) =:= q)
                                           or (r =:= element(3, X))]),
          {list, {table,{qlc_SUITE,list_keys, [[q,r],3,L]}}, _MS} = i(QH),
          [1,2] = qlc:eval(QH)">>,

       <<"L = [{1,a,q},{2,b,r},{3,c,s}],
          QH = qlc:q([element(1, X) || X <- qlc_SUITE:table(L, 1, [2]),
                                       (element(3, X) =:= q)
                                           or (r =:= element(3, X))]),
          {qlc,_,[{generate,_,{table,{call,_,_,_}}},
                  _],[]} = i(QH),
          [1,2] = qlc:eval(QH)">>,

       <<"L = [{a,1},{b,2},{c,3}],
          QH = qlc:q([E || {K,I}=E <- qlc_SUITE:table(L, 1, [2]),
                           ((K =:= a) or (K =:= b) or (K =:= c))
                               and ((I =:= 1) or (I =:= 2))],
                     {max_lookup, 3}),
          {list, {table,{qlc_SUITE,list_keys,[[a,b,c],1,L]}}, _MS} = i(QH),
          [{a,1},{b,2}] = qlc:eval(QH)">>,

       <<"L = [{a,1},{b,2},{c,3}],
          QH = qlc:q([E || {K,I}=E <- qlc_SUITE:table(L, 1, [2]),
                           ((K =:= a) or (K =:= b) or (K =:= c))
                               and ((I =:= 1) or (I =:= 2))],
                     {max_lookup, 2}),
          {list, {table,{qlc_SUITE,list_keys, [[1,2],2,L]}}, _MS} = i(QH),
          [{a,1},{b,2}] = qlc:eval(QH)">>,

       <<"L = [{a,1,x,u},{b,2,y,v},{c,3,z,w}],
          QH = qlc:q([E || {K,I1,I2,I3}=E <- qlc_SUITE:table(L, 1, [2,3,4]),
                           ((K =/= a) or (K =/= b) or (K =/= c))
                             and ((I1 =:= 1) or (I1 =:= 2) or 
                                  (I1 =:= 3) or (I1 =:= 4))
                             and ((I2 =:= x) or (I2 =:= z)) 
                             and ((I3 =:= v) or (I3 =:= w))],
                     {max_lookup, 5}),
          {list, {table,{qlc_SUITE,list_keys, [[x,z],3,L]}}, _MS} = i(QH),
          [{c,3,z,w}] = qlc:eval(QH)">>

       ],
    run(Config, <<"-record(r, {a}).\n">>, Ts),
    ok.

%% Test the table/2 callback functions parent_fun and stop_fun.
pre_fun(Config) when is_list(Config) ->
    Ts = [
       <<"PF = process_flag(trap_exit, true),
          %% cursor: table killing parent
          L = [{1,a},{2,b},{3,c}],
          F1 = fun() ->
                   QH = qlc:q([element(1, X) || 
                                X <- qlc_SUITE:table_kill_parent(L, [2]),
                                (element(2, X) =:= a)
                                    or (b =:= element(2, X))]),
                   _ = qlc:info(QH),
                   _ = qlc:cursor(QH)
               end,
          Pid1 = spawn_link(F1),
          receive {'EXIT', Pid1, killed} ->
              ok
          end,
          timer:sleep(1),
          process_flag(trap_exit, PF)">>,

       <<"PF = process_flag(trap_exit, true),
          %% eval without cursor: table killing parent
          L = [{1,a},{2,b},{3,c}],
          F2 = fun() ->
                 QH = qlc:q([element(1, X) || 
                                X <- qlc_SUITE:table_kill_parent(L, [2]),
                                (element(2, X) =:= a)
                                    or (b =:= element(2, X))]),
                 _ = qlc:eval(QH)
               end,
          Pid2 = spawn_link(F2),
          receive {'EXIT', Pid2, killed} ->
              ok
          end,
          process_flag(trap_exit, PF)">>,

       <<"L = [{1,a},{2,b},{3,c}],
          QH = qlc:q([element(1, X) || 
                        X <- qlc_SUITE:table_parent_throws(L, [2]),
                        (element(2, X) =:= a)
                            or (b =:= element(2, X))]),
          _ = qlc:info(QH),
          {throw,thrown} = (catch {any_term,qlc:cursor(QH)}),
          {throw,thrown} = (catch {any_term,qlc:eval(QH)})">>,

       <<"L = [{1,a},{2,b},{3,c}],
          QH = qlc:q([element(1, X) || 
                        X <- qlc_SUITE:table_parent_exits(L, [2]),
                        (element(2, X) =:= a)
                            or (b =:= element(2, X))]),
          _ = qlc:info(QH),
          {'EXIT', {badarith,_}} = (catch qlc:cursor(QH)),
          {'EXIT', {badarith,_}} = (catch qlc:eval(QH))">>,

       <<"L = [{1,a},{2,b},{3,c}],
          QH = qlc:q([element(1, X) || 
                        X <- qlc_SUITE:table_bad_parent_fun(L, [2]),
                        (element(2, X) =:= a)
                            or (b =:= element(2, X))]),
          {'EXIT', {badarg,_}} = (catch qlc:cursor(QH)),
          {'EXIT', {badarg,_}} = (catch qlc:eval(QH))">>,

       <<"%% Very simple test of stop_fun.
          Ets = ets:new(apa, [public]),
          L = [{1,a},{2,b},{3,c}],
          H = qlc:q([X || {X,_} <- qlc_SUITE:stop_list(L, Ets)]),
          C = qlc:cursor(H),
          [{stop_fun,StopFun}] = ets:lookup(Ets, stop_fun),
          StopFun(),
          {'EXIT', {{qlc_cursor_pid_no_longer_exists, _}, _}} =
                  (catch qlc:next_answers(C, all_remaining)),
          ets:delete(Ets)">>

       ],
    
    run(Config, Ts),
    ok.

%% Lookup keys. With records.
skip_filters(Config) when is_list(Config) ->
    %% Skipped filters
    TsS = [
       %% Cannot skip the filter.
       <<"etsc(fun(E) ->
                H = qlc:q([X || X <- ets:table(E), 
                          (element(1, X) =:= 1) xor (element(1, X) =:= 1)]),
                [] = qlc:eval(H),
                [1] = lookup_keys(H)
               end, [{keypos,1}], [{1},{2}])">>,

       %% The filter can be skipped. Just a lookup remains.
       <<"etsc(fun(E) ->
                H = qlc:q([X || X <- ets:table(E), 
                          (element(1, X) =:= 1) or (element(1, X) =:= 1)]),
                [{1}] = qlc:eval(H),
                {table, _} = i(H),
                [1] = lookup_keys(H)
               end, [{keypos,1}], [{1},{2}])">>,

       %% safe_unify fails on 3 and <<X:32>>
       <<"etsc(fun(E) ->
                H = qlc:q([X || X <- ets:table(E), 
                     (element(1, X) =:= 1) and (3 =:= <<X:32>>)]),
                [] = qlc:eval(H),
                [1] = lookup_keys(H)
               end, [{keypos,1}], [{1},{2}])">>,

       %% Two filters are skipped.
       <<"etsc(fun(E) ->
                Q = qlc:q([{B,C,D} || {A={C},B} <- ets:table(E), 
                                      (A =:= {1}) or (A =:= {2}),
                                      (C =:= 1) or (C =:= 2),
                                      D <- [1,2]]),
                {qlc,_,[{generate,_,{table,_}},{generate,_,{list,[1,2]}}],[]}
                  = i(Q),
                [{1,1,1},{1,1,2},{2,2,1},{2,2,2}] = lists:sort(qlc:eval(Q)),
                [{1},{2}] = lookup_keys(Q)
         end, [{{1},1},{{2},2},{{3},3}])">>,

       <<"etsc(fun(E) ->
                Q = qlc:q([{B,C} || {A={C},B} <- ets:table(E), 
                                    (A =:= {1}) or (A =:= {2}),
                                    (C =:= 1) or (C =:= 2)]),
                {qlc,_,[{generate,_,{table,_}}],[]} = i(Q),
                [{1,1},{2,2}] = lists:sort(qlc:eval(Q)),
                [{1},{2}] = lookup_keys(Q)
         end, [{{1},1},{{2},2},{{3},3}])">>,

       %% Lookup. No match spec, no filter.
       <<"etsc(fun(E) ->
                Q = qlc:q([X || X <- ets:table(E), 
                               element(1, X) =:= 1]),
                {table, _} = i(Q),
                [{1}] = qlc:e(Q),
                [1] = lookup_keys(Q)
         end, [{1},{2}])">>,

       <<"etsc(fun(E) ->
                 Q = qlc:q([{X,Y} || X <- ets:table(E), 
                                     element(1, X) =:= 1,
                                     Y <- [1,2]]),
                 {qlc,_,[{generate,_,{table,_}},{generate,_,{list,_}}],[]}
                      = i(Q),
                 [{{1},1},{{1},2}] = lists:sort(qlc:e(Q)),
                 [1] = lookup_keys(Q)
         end, [{1},{2}])">>,

       <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X,Y} <- ets:table(E), 
                                 X =:= a, 
                                 X =:= Y]),
                 {list,{table,_},_} = i(Q),
                 [a] = qlc:e(Q),
                 [a] = lookup_keys(Q)
          end, [{a,a},{b,c},{c,a}])">>,

       %% The imported variable (A) is never looked up in the current
       %% implementation. This means that the first filter cannot be skipped;
       %% the constant 'a' is looked up, and then the first filter evaluates
       %% to false.
       <<"etsc(fun(E) ->
                 A = 3,
                 Q = qlc:q([X || X <- ets:table(E), 
                                 A == element(1,X),
                                 element(1,X) =:= a]),
                 [] = qlc:e(Q),
                 [a] = lookup_keys(Q)
          end, [{a},{b},{c}])">>,

       %% No lookup.
       {cres, 
        <<"etsc(fun(E) ->
                  Q = qlc:q([X || {X} <- ets:table(E), 
                                  X =:= 1, 
                                  X =:= 2]),
                  {table, _} = i(Q),
                  [] = qlc:e(Q),
                  false = lookup_keys(Q)
          end, [{1,1},{2,0}])">>,
        %% {warnings,[{{4,37},qlc,nomatch_filter}]}},
        []},

       <<"etsc(fun(E) ->
                 Q = qlc:q([{A,B,C} ||
                               {A} <- ets:table(E),
                               A =:= 1,
                               {B} <- ets:table(E),
                               B =:= 2,
                               {C} <- ets:table(E),
                               C =:= 3]),
                 {qlc,_,[{generate,_,{list,{table,_},_}},
                         {generate,_,{list,{table,_},_}},
                         {generate,_,{list,{table,_},_}}],[]} = i(Q),
                 [{1,2,3}] = qlc:e(Q),
                 [1,2,3] = lookup_keys(Q)
          end, [{0},{1},{2},{3},{4}])">>

           ],
    run(Config, TsS),

    Ts = [
       <<"etsc(fun(E) ->
                 H = qlc:q([X || {X,_} <- ets:table(E),
                                 X =:= 2]),
                 {list,{table,_},_} = i(H),
                 [2] = qlc:e(H)
         end, [{1,a},{2,b}])">>,

       <<"etsc(fun(E) ->
                 H = qlc:q([X || {X,_} <- ets:table(E),
                                 ((X =:= 2) or (X =:= 1)) and (X > 1)]),
                 {list,{table,_},_} = i(H),
                 [2] = qlc:e(H)
         end, [{1,a},{2,b}])">>,

       <<"etsc(fun(E) ->
                 H = qlc:q([X || {X,Y} <- ets:table(E),
                                 (X =:= 2) and (Y =:= b)]),
                 {list,{table,_},_} = i(H),
                 [2] = qlc:e(H)
         end, [{1,a},{2,b}])">>,

       <<"etsc(fun(E) ->
                 H = qlc:q([X || X <- ets:table(E),
                                 (element(1,X) =:= 2) and (X =:= {2,b})]),
                 {list,{table,_},_} = i(H),
                 [{2,b}] = qlc:e(H)
         end, [{1,a},{2,b}])">>,

       <<"etsc(fun(E) ->
                 H = qlc:q([{X,Y,Z,W} || 
                               {X,Y} <- ets:table(E),
                               {Z,W} <- ets:table(E),
                               (Y =:= 3) or (Y =:= 4)]),
                 {qlc,_,[{generate,_,{table,{ets,table,_}}},
                         {generate,_,{table,{ets,table,_}}}],[]} = i(H),
                 [{a,3,a,3},{a,3,b,5}] = lists:sort(qlc:e(H))
         end, [{a,3},{b,5}])">>,

       <<"etsc(fun(E) ->
                 H = qlc:q([{X,Y} || 
                               {X,Y=3} <- ets:table(E), % no matchspec
                               %% Two columns restricted, but lookup anyway
                               (X =:= a)]),
                 {qlc,_,[{generate,_,{table,_}}],[]} = i(H),
                 [{a,3}] = qlc:e(H)
         end, [{a,3},{b,4}])">>,

       <<"etsc(fun(E) ->
                 V = 3,
                 H = qlc:q([{X,Y} || 
                               {X,Y} <- ets:table(E),
                               (Y =:= V)]), % imported variable, no lookup
                 {table,{ets,table,_}} = i(H),
                 [{a,3}] = qlc:e(H)
         end, [{a,3},{b,4}])">>,

       <<"etsc(fun(E) ->
                 V = b,
                 H = qlc:q([{X,Y} || 
                               {X,Y} <- ets:table(E),
                               (X =:= V)]), % imported variable, lookup
                 {list,{table,_},_} = i(H),
                 [{b,4}] = qlc:e(H)
         end, [{a,3},{b,4}])">>,

       <<"H = qlc:q([{A,B} || {{A,B}} <- [{{1,a}},{{2,b}}],
                              A =:= 1,
                              B =:= a]),
              {list,{list,[_,_]},_} = i(H),
              [{1,a}] = qlc:e(H)">>,

       <<"etsc(fun(E) ->
                 H = qlc:q([{A,B} || {{A,B}} <- ets:table(E),
                                     A =:= 1,
                                     B =:= a]),
                 {list,{table,_},_} = i(H),
                 [{1,a}] = qlc:e(H)
         end, [{{1,a}},{{2,b}}])">>,

       %% The filters are skipped, and the guards of the match specifications
       %% are skipped as well. Only the transformations of the matchspecs
       %% are kept.
       <<"etsc(fun(E1) ->
                 etsc(fun(E2) ->
                              H = qlc:q([{X,Y,Z,W} ||
                                             {X,_}=Z <- ets:table(E1),
                                             W={Y} <- ets:table(E2),
                                             (X =:= 1) or (X =:= 2),
                                             (Y =:= a) or (Y =:= b)]
                                         ,{lookup,true}
                                        ),
                              {qlc,_,[{generate,_,{list,{table,_},
                                                   [{{'$1','_'},[],['$_']}]}},
                                      {generate,_,{list,{table,_},
                                                   [{{'$1'},[],['$_']}]}}],[]}
                              = i(H),
                              [{1,a,{1,a},{a}},
                               {1,b,{1,a},{b}},
                               {2,a,{2,b},{a}},
                               {2,b,{2,b},{b}}] = qlc:e(H)
                      end, [{a},{b}])
         end, [{1,a},{2,b}])">>,

       %% The same example again, but this time no match specs are run.
       <<"fun(Z) -> 
              etsc(fun(E1) ->
                     etsc(fun(E2) ->
                                  H = qlc:q([{X,Y} ||
                                                 Z > 2,
                                                 X <- ets:table(E1),
                                                 Y <- ets:table(E2),
                                                 (element(1, X) =:= 1) or
                                                 (element(1, X) =:= 2),
                                                 (element(1, Y) =:= a) or
                                                 (element(1, Y) =:= b)]
                                             ,{lookup,true}
                                            ),
                                  {qlc,_,[_,{generate,_,{table,_}},
                                          {generate,_,{table,_}}],[]} = i(H),
                                  [{{1,a},{a}},
                                   {{1,a},{b}},
                                   {{2,b},{a}},
                                   {{2,b},{b}}] = qlc:e(H)
                          end, [{a},{b}])
             end, [{1,a},{2,b}])
         end(4)">>,

       %% Once again, this time with a join.
       <<"etsc(fun(E1) ->
                 etsc(fun(E2) ->
                              H = qlc:q([{X,Y,Z,W} ||
                                             {X,V}=Z <- ets:table(E1),
                                             W={Y} <- ets:table(E2),
                                             (X =:= 1) or (X =:= 2),
                                             (Y =:= a) or (Y =:= b),
                                             Y =:= V]
                                         ,[{lookup,true},{join,merge}]
                                        ),
                              {qlc,_,[{generate,_,{qlc,_,
                                [{generate,_,{qlc,_,[{generate,_,
                                    {keysort,{list,{table,_},_},2,[]}},
                                 _C1,_C2],[]}},
                                  {generate,_,
                                      {qlc,_,[{generate, _,
                                         {keysort,{list,{table,_},_},1,[]}},
                                              _C3],
                                       []}},
                                 _],
                                [{join,merge}]}},_],[]} = i(H),
                              [{1,a,{1,a},{a}},{2,b,{2,b},{b}}] = 
                                  lists:sort(qlc:e(H))
                      end, [{a},{b}])
         end, [{1,a},{2,b}])">>,

       %% Filters 2 and 3 are not skipped. 
       %% (Only one filter at a time is tried by the parse transform.)
       <<"etsc(fun(E) ->
                 H = qlc:q([X || {{A,B}=X,Y} <- ets:table(E), % no matchspec
                                     Y =:= 3,
                                     A =:= 1,
                                     B =:= a]),

                 {qlc,_,[{generate,_,{table,_}},_,_,_],[]}= i(H),
                 [{1,a}] = qlc:e(H)
         end, [{{1,a},3},{{2,b},4}])">>,

       <<"etsc(fun(E) ->
                 H = qlc:q([X || {X=_,_} <- ets:table(E), % no matchspec
                                  (X =:= 3) and (X > 3)]),
                 {qlc,_,[{generate,_,{table,_}},_],[]} = i(H),
                 [] = qlc:e(H)
         end, [{3,a},{4,b}])">>,

       <<"etsc(fun(E) ->
                 H = qlc:q([X || {X=_,_} <- ets:table(E), % no matchspec
                                 (X =:= 3) or true]),
                 {qlc,_,[{generate,_,{table,{ets,table,_}}},_],[]} = i(H),
                 [3,4] = lists:sort(qlc:e(H))
         end, [{3,a},{4,b}])">>,

       <<"etsc(fun(E) ->
                 H = qlc:q([X || {X=_,_} <- ets:table(E), % no matchspec
                                 (X =:= 3) or false]),
                 {qlc,_,[{generate,_,{table,_}}],[]} = i(H),
                 [3] = lists:sort(qlc:e(H))
         end, [{3,a},{4,b}])">>,

       <<"etsc(fun(E) ->
                 H = qlc:q([X || {X=_,_} <- ets:table(E), % no matchspec
                                 (X =:= X) and (X =:= 3)]),
                 {qlc,_,[{generate,_,{table,_}}],[]} = i(H),
                 [3] = lists:sort(qlc:e(H))
         end, [{3,a},{4,b}])">>,

       %% The order of filters matters. A guard filter cannot be used
       %% unless there are no non-guard filter placed before the guard
       %% filter that uses the guard filter's generator. There is
       %% more examples in join_filter().
       <<"etsc(fun(E) ->
                 %% Lookup. 
                 Q = qlc:q([{A,B,A} ||
                               {A=_,B} <- ets:table(E), % no match spec
                               A =:= 1,
                               begin 1/B > 0 end]),
                 [{1,1,1}] = lists:sort(qlc:e(Q))
         end, [{1,1},{2,0}])">>,
       <<"etsc(fun(E) ->
                 %% No lookup. 
                 Q = qlc:q([{A,B,A} ||
                               {A=_,B} <- ets:table(E), % no match spec
                               begin 1/B > 0 end,
                               A =:= 1]),
                 {'EXIT', _} = (catch qlc:e(Q))
         end, [{1,1},{2,0}])">>,
       %% The same thing, with a match specification.
       <<"etsc(fun(E) ->
                 Q = qlc:q([{A,B,A} ||
                               {A,B} <- ets:table(E), % match spec
                               A < 2,
                               begin 1/B > 0 end]),
                 [{1,1,1}] = lists:sort(qlc:e(Q))
         end, [{1,1},{2,0}])">>,
       <<"etsc(fun(E) ->
                 Q = qlc:q([{A,B,A} ||
                               {A,B} <- ets:table(E), % match spec
                               begin 1/B > 0 end,
                               A < 2]),
                 {'EXIT', _} = (catch qlc:e(Q))
         end, [{1,1},{2,0}])">>,
       %% More examples, this time two tables.
       <<"etsc(fun(E) ->
                 Q = qlc:q([{A,B,C,D} ||
                               {A,B} <- ets:table(E), % match spec
                               A < 2,
                               {C,D} <- ets:table(E),
                               begin 1/B > 0 end, %\"invalidates\" next filter
                               C =:= 1,
                               begin 1/D > 0 end]),
                 {qlc,_,[{generate,_,{table,{ets,table,_}}},
                         {generate,_,{table,{ets,table,_}}},
                         _,_,_],[]} = i(Q),
                 [{1,1,1,1}] = lists:sort(qlc:e(Q))
         end, [{1,1},{2,0}])">>,
       <<"etsc(fun(E) ->
              Q = qlc:q([{A,B,C,D} ||
                            {A,B} <- ets:table(E),
                            {C,D} <- ets:table(E),
                            begin 1/B > 0 end, % \"invalidates\" two filters
                            A < 2,
                            C =:= 1,
                            begin 1/D > 0 end]),
              {qlc,_,[{generate,_,{table,{ets,table,_}}},
                      {generate,_,{table,{ets,table,_}}},_,_,_,_],[]} = i(Q),
              {'EXIT', _} = (catch qlc:e(Q))
         end, [{1,1},{2,0}])">>,
      <<"%% There are objects in the ETS table, but none passes the filter.
         %% F() would not be run if it did not \"invalidate\" the following
         %% guards. 
         etsc(fun(E) ->
                      F = fun() -> [foo || A <- [0], 1/A] end,
                      Q1 = qlc:q([X || {X} <- ets:table(E),
                                       F(), % \"invalidates\" next guard
                                       X =:= 17]),
                      {'EXIT', _} = (catch qlc:e(Q1))
              end, [{1},{2},{3}])">>,
       <<"%% The last example works just like this one:
          etsc(fun(E) ->
                      F = fun() -> [foo || A <- [0], 1/A] end,
                      Q1 = qlc:q([X || {X} <- ets:table(E),
                                       F(),
                                       begin X =:= 17 end]),
                      {'EXIT', _} = (catch qlc:e(Q1))
              end, [{1},{2},{3}])">>

          ],
    run(Config, Ts),

    ok.


%% ets:table/1,2.
ets(Config) when is_list(Config) ->
    Ts = [
       <<"E = ets:new(t, [ordered_set]),
          true = ets:insert(E, [{1},{2}]),
          {'EXIT', _} = 
              (catch qlc:e(qlc:q([X || {X} <- ets:table(E, bad_option)]))),
          {'EXIT', _} = 
              (catch qlc:e(qlc:q([X || {X} <- ets:table(E,{traverse,bad})]))),
          All = [{'$1',[],['$1']}],
          TravAll = {traverse,{select,All}},
          [_, _] = qlc:e(qlc:q([X || {X} <- ets:table(E, TravAll)])),
          [_, _] = qlc:e(qlc:q([X || {X} <- ets:table(E,{traverse,select})])),
          [1,2] = 
             qlc:e(qlc:q([X || {X} <- ets:table(E, {traverse, first_next})])),
          [2,1] = 
             qlc:e(qlc:q([X || {X} <- ets:table(E, {traverse, last_prev})])),
          {table,{ets,table,[_,[{traverse,{select,_}},{n_objects,1}]]}} = 
              i(qlc:q([X || {X} <- ets:table(E, {n_objects,1})])),
          {qlc,_,[{generate,_,{table,{ets,table,[_,{n_objects,1}]}}},_],[]} =
              i(qlc:q([X || {X} <- ets:table(E,{n_objects,1}), 
                                   begin (X >= 1) or (X < 1) end])),
          {qlc,_,[{generate,_,{table,{ets,table,[_]}}},_],[]} = 
              i(qlc:q([X || {X} <- ets:table(E), 
                                   begin (X >= 1) or (X < 1) end])),
          ets:delete(E)">>,

       begin
       MS = ets:fun2ms(fun({X,Y}) when X > 1 -> {X,Y} end),
       [<<"E = ets:new(apa,[]),
           true = ets:insert(E, [{1,a},{2,b},{3,c}]),
           MS =  ">>, io_lib:format("~w", [MS]), <<",
           Q = qlc:q([X || {X,_} <- ets:table(E, {traverse, {select, MS}}), 
                           X =:= 1]),
           R = qlc:e(Q),
           ets:delete(E),
           [] = R">>]
       end,

       <<"E2 = ets:new(test, [bag]),
          Ref = make_ref(),
          true = ets:insert(E2, [{Ref,Ref}]),
          Q2 = qlc:q([{Val1} ||
                         {Ref1, Val1} <- ets:table(E2),
                         Ref1 =:= Ref]),
          S = qlc:info(Q2),
          true = is_list(S),
          [{Ref}] = qlc:e(Q2),
          ets:delete(E2)">>

       ],
    
    run(Config, Ts),
    ok.

%% dets:table/1,2.
dets(Config) when is_list(Config) ->
    dets:start(),
    T = t,
    Fname = filename(T, Config),
    Ts = [
       [<<"T = t, Fname = \"">>, Fname, <<"\",
           file:delete(Fname),
           {ok, _} = dets:open_file(T, [{file,Fname}]),
           ok = dets:insert(T, [{1},{2}]),
           {'EXIT', _} = 
              (catch qlc:e(qlc:q([X || {X} <- dets:table(T, bad_option)]))),
           {'EXIT', _} = 
              (catch qlc:e(qlc:q([X || {X} <- dets:table(T,{traverse,bad})]))),
           {'EXIT', _} = 
              (catch 
               qlc:e(qlc:q([X || {X} <- dets:table(T,{traverse,last_prev})]))),
           All = [{'$1',[],['$1']}],
           TravAll = {traverse,{select,All}},
           [_,_] = qlc:e(qlc:q([X || {X} <- dets:table(T, TravAll)])),
           [_,_] = qlc:e(qlc:q([X || {X} <- dets:table(T,{traverse,select})])),
           [_,_] = 
             qlc:e(qlc:q([X || {X} <- dets:table(T, {traverse, first_next})])),
           {table,{dets,table,[T,[{traverse,{select,_}},{n_objects,1}]]}} =
               i(qlc:q([X || {X} <- dets:table(T, {n_objects,1})])),
           {qlc,_,[{generate,_,{table,{dets,table,[t,{n_objects,1}]}}},_],[]}=
               i(qlc:q([X || {X} <- dets:table(T,{n_objects,1}), 
                             begin (X >= 1) or (X < 1) end])),
           {qlc,_,[{generate,_,{table,{dets,table,[_]}}},_],[]} = 
               i(qlc:q([X || {X} <- dets:table(T), 
                             begin (X >= 1) or (X < 1) end])),
           H = qlc:q([X || {X} <- dets:table(T, {n_objects, default}),
                           begin (X =:= 1) or (X =:= 2) or (X =:= 3) end]),
           [1,2] = lists:sort(qlc:e(H)),
           {qlc,_,[{generate,_,{table,_}},_],[]} = i(H),

           H2 = qlc:q([X || {X} <- dets:table(T), (X =:= 1) or (X =:= 2)]),
           [1,2] = lists:sort(qlc:e(H2)),
           {list,{table,_},_} = i(H2),
           true = binary_to_list(<<
            \"ets:match_spec_run(lists:flatmap(fun(V)->dets:lookup(t,V)end,\"
            \"[1,2]),ets:match_spec_compile([{{'$1'},[],['$1']}]))\">>)
                   == format_info(H2, true),

           H3 = qlc:q([X || {X} <- dets:table(T), (X =:= 1)]),
           [1] = qlc:e(H3),
           {list,{table,_},_} = i(H3),

           ok = dets:close(T),
           file:delete(\"">>, Fname, <<"\"),
           ok">>],

       begin
       MS = ets:fun2ms(fun({X,Y}) when X > 1 -> {X,Y} end),
       [<<"T = t, Fname = \"">>, Fname, <<"\",
           {ok, _} = dets:open_file(T, [{file,Fname}]),
           MS =  ">>, io_lib:format("~w", [MS]), <<",
           ok = dets:insert(T, [{1,a},{2,b},{3,c}]),
           Q = qlc:q([X || {X,_} <- dets:table(T, {traverse, {select, MS}}), 
                           X =:= 1]),
           R = qlc:e(Q),
           ok = dets:close(T),
           file:delete(\"">>, Fname, <<"\"),
           [] = R">>]
       end,

       [<<"T = t, Fname = \"">>, Fname, <<"\",
           {ok, _} = dets:open_file(T, [{file,Fname}]),
           Objs = [{X} || X <- lists:seq(1,10)],
           ok = dets:insert(T, Objs),
           {ok, Where} = dets:where(T, {2}),
           ok = dets:close(T),
           qlc_SUITE:crash(Fname, Where),

           {ok, _} = dets:open_file(T, [{file,Fname}]),
           HT = qlc:q([X || {X} <- dets:table(T, {traverse, first_next})]),
           {'EXIT',{error,{{bad_object,_},_}}} = (catch qlc:e(HT)),
           _ = dets:close(T),

           {ok, _} = dets:open_file(T, [{file,Fname}]),
           HMS = qlc:q([X || {X} <- dets:table(T, {traverse, select})]),
           {error,{{bad_object,_},_}} = qlc:e(HMS),
           _ = dets:close(T),

           {ok, _} = dets:open_file(T, [{file,Fname}]),
           HLU = qlc:q([X || {X} <- dets:table(T), X =:= 2]),
           {error,{{bad_object,_},_}} = qlc:e(HLU),
           _ = dets:close(T),

           file:delete(Fname)">>]

       ],
    
    run(Config, Ts),
    _ = file:delete(Fname),
    ok.


%% The 'join' option (any, lookup, merge, nested_loop). Also cache/unique.
join_option(Config) when is_list(Config) ->
    Ts = [
       <<"Q1 = qlc:q([X || X <- [1,2,3]],{join,merge}),
          {'EXIT', {no_join_to_carry_out,_}} = (catch {foo, qlc:info(Q1)}),
          {'EXIT', {no_join_to_carry_out,_}} = (catch {foo, qlc:e(Q1)}),

          Q2 = qlc:q([X || X <- [1,2,3], X > 1],{join,merge}),
          {'EXIT', {no_join_to_carry_out,_}} = (catch {foo, qlc:info(Q2)}),
          {'EXIT', {no_join_to_carry_out,_}} = (catch {foo, qlc:e(Q2)}),

          Q3 = qlc:q([{X,Y} ||
                         {X} <- [{1},{2},{3}],
                         {Y} <- [{a},{b},{c}],
                         X =:= Y],
                     {join, merge}),

          {1,0,0,2} = join_info(Q3),
          [] = qlc:e(Q3),

          Q4 = qlc:q([{X,Y} ||
                         {X} <- [{1},{2},{3}],
                         {Y} <- [{a},{b},{c}],
                         X > Y],
                     {join, lookup}),
          {'EXIT', {no_join_to_carry_out, _}} = (catch {foo, qlc:info(Q4)}),
          {'EXIT', {no_join_to_carry_out, _}} = (catch {foo, qlc:e(Q4)}),

          Q5 = qlc:q([{X,Y} ||
                         {X} <- [{1},{2},{3}],
                         {Y} <- [{3},{4},{5}],
                         X == Y],
                     {join, merge}),
          [{3,3}] = qlc:e(Q5),

          Q6 = qlc:q([{X,Y} ||
                         {X} <- [{1},{2},{3}],
                         {Y} <- [{3},{4},{5}],
                         X == Y],
                     {join, lookup}),
          {'EXIT', {cannot_carry_out_join, _}} = (catch {foo, qlc:info(Q6)}),
          {'EXIT', {cannot_carry_out_join, _}} = (catch {foo, qlc:e(Q6)}),

          Q7 = qlc:q([{X,Y} ||
                         {X} <- [{1},{2},{3}],
                         {Y} <- [{3},{4},{5}],
                         X == Y],
                     {join, nested_loop}),
          {0,0,1,0} = join_info(Q7),
          [{3,3}] = qlc:e(Q7),

          Q8 = qlc:q([{X,Y} ||
                         {X} <- [{1},{2},{3}],
                         {Y} <- [{3},{4},{5}],
                         X =:= Y],
                     {join, nested_loop}),
          {0,0,1,0} = join_info(Q8),
          [{3,3}] = qlc:e(Q8),

          %% Only guards are inspected...
          Q9 = qlc:q([{X,Y} ||
                         {X} <- [{1},{2},{3}],
                         {Y} <- [{3},{4},{5}],
                         begin X =:= Y end],
                     {join, nested_loop}),
          {'EXIT', {no_join_to_carry_out, _}} = (catch {foo, qlc:info(Q9)}),
          {'EXIT', {no_join_to_carry_out, _}} = (catch {foo, qlc:e(Q9)}),

          Q10 = qlc:q([{X,Y} ||
                         {X} <- [{1},{2},{3}],
                         {Y} <- [{3},{4},{5}],
                         X < Y],
                     {join, nested_loop}),
          {'EXIT', {no_join_to_carry_out, _}} = (catch {foo, qlc:info(Q10)}),
          {'EXIT', {no_join_to_carry_out, _}} = (catch {foo, qlc:e(Q10)}),

          F = fun(J) -> qlc:q([X || X <- [1,2]], {join,J}) end,
          {'EXIT', {no_join_to_carry_out, _}} = 
                (catch {foo, qlc:e(F(merge))}),
          {'EXIT', {no_join_to_carry_out, _}} = 
                (catch {foo, qlc:e(F(lookup))}),
          {'EXIT', {no_join_to_carry_out, _}} = 
                (catch {foo, qlc:e(F(nested_loop))}),
          [1,2] = qlc:e(F(any)),

          %% No join of columns in the same table.
          Q11 = qlc:q([{X,Y} || {a = X, X = Y} <- [{a,1},{a,a},{a,3},{a,a}]],
                      {join,merge}),
          {'EXIT', {no_join_to_carry_out, _}} = (catch qlc:e(Q11)),
          Q12 = qlc:q([{X,Y} || {X = a, X = Y} <- [{a,1},{a,a},{a,3},{a,a}]],
                      {join,merge}),
          {'EXIT', {no_join_to_carry_out, _}} = (catch qlc:e(Q12)),
          %% X and Y are \"equal\" (same constants), but must not be joined.
          Q13 = qlc:q([{X,Y} || {X,_Z} <- [{a,1},{a,2},{b,1},{b,2}],
                                {Y} <- [{a}],
                                (X =:= a) and (Y =:= b) or 
                                (X =:= b) and (Y =:= a)],
                     {join,merge}),
          {'EXIT', {no_join_to_carry_out, _}} = (catch qlc:e(Q13))

">>,

       <<"Q1 = qlc:q([X || X <- [1,2,3]], {lookup,true}),
          {'EXIT', {no_lookup_to_carry_out, _}} = (catch {foo, qlc:info(Q1)}),
          {'EXIT', {no_lookup_to_carry_out, _}} = (catch {foo, qlc:e(Q1)}),
          Q2 = qlc:q([{X,Y} || X <- [1,2,3], Y <- [x,y,z]], lookup),
          {'EXIT', {no_lookup_to_carry_out, _}} = (catch {foo, qlc:info(Q2)}),
          {'EXIT', {no_lookup_to_carry_out, _}} = (catch {foo, qlc:e(Q2)}),
          Q3 = qlc:q([X || {X} <- [{1},{2},{3}]], {lookup,true}),
          {'EXIT', {no_lookup_to_carry_out, _}} = (catch {foo, qlc:e(Q3)}),
          {'EXIT', {no_lookup_to_carry_out, _}} = (catch {foo, qlc:info(Q3)}),

          E1 = create_ets(1, 10),
          Q4 = qlc:q([{X,Y} || {X,Y} <- ets:table(E1), X =:= 3], lookup),
          {match_spec, _} = strip_qlc_call(Q4),
          [{3,3}] = qlc:e(Q4),
          Q5 = qlc:q([{X,Y} || {X,Y} <- ets:table(E1), X =:= 3], {lookup,false}),
          {table, ets, _} = strip_qlc_call(Q5),
          [{3,3}] = qlc:e(Q5),
          Q6 = qlc:q([{X,Y} || {X,Y} <- ets:table(E1), X =:= 3], {lookup,any}),
          {match_spec, _} = strip_qlc_call(Q6),
          [{3,3}] = qlc:e(Q6),
          ets:delete(E1)">>

       ],
    run(Config, Ts),

    %% The 'cache' and 'unique' options of qlc/2 affects join.
    CUTs = [
       <<"L1 = [1,2],
          L2 = [{1,a},{2,b}],
          L3 = [{a,1},{b,2}],
          Q = qlc:q([{X,Y,Z} ||
                        Z <- L1,
                        {X,_} <- L2,
                        {_,Y} <- L3,
                        X =:= Y],
                    [cache, unique]),
          {qlc,_,
              [{generate,_,{list,L1}},
               {generate,_,{qlc,_,[{generate,_,
                      {qlc,_,[{generate,_,{keysort,{list,L2},1,[]}}],[]}},
                                {generate,_,{qlc,_,
                              [{generate,_,{keysort,{list,L3},2,[]}}],[]}},_],
                            [{join,merge},{cache,ets},{unique,true}]}},_],
              [{unique,true}]} = i(Q),
          [{1,1,1},{2,2,1},{1,1,2},{2,2,2}] = qlc:e(Q)">>,
       <<"L1 = [1,2],
          L2 = [{1,a},{2,b}],
          L3 = [{a,1},{b,2}],
          Q = qlc:q([{X,Y,Z} ||
                        Z <- L1,
                        {X,_} <- L2,
                        {_,Y} <- L3,
                        X =:= Y],
                    []),
          Options = [{cache_all,ets}, unique_all],
          {qlc,_,[{generate,_,{qlc,_,[{generate,_,{list,L1}}],
                               [{unique,true}]}},
                  {generate,_,{qlc,_,
                              [{generate,_,{qlc,_,[{generate,_,
                                 {keysort,{qlc,_,[{generate,_,{list,L2}}],
                                           [{cache,ets},{unique,true}]},
                                          1,[]}}],[]}},
                               {generate,_,{qlc,_,
                                    [{generate,_,{keysort,
                                      {qlc,_,[{generate,_,{list,L3}}],
                                       [{cache,ets},{unique,true}]},
                                                  2,[]}}],[]}},_],
                              [{join,merge},{cache,ets},{unique,true}]}},
                  _],[{unique,true}]} = i(Q, Options),
          [{1,1,1},{2,2,1},{1,1,2},{2,2,2}] = qlc:e(Q, Options)">>
       ],
    run(Config, CUTs),

    ok.

%% Various aspects of filters and join.
join_filter(Config) when is_list(Config) ->
    Ts = [
      <<"E1 = create_ets(1, 10),
          Q = qlc:q([X || {X,_} <- ets:table(E1),
                          begin A = X * X end, % ej true (?)
                          X >= A]),
          {'EXIT', _} = (catch qlc:e(Q)),
          ets:delete(E1)">>,

      %% The order of filters matters. See also skip_filters().
      <<"Q = qlc:q([{X,Y} || {X,Y} <- [{a,1},{b,2}], 
         {Z,W} <- [{a,1},{c,0}], 
         X =:= Z,
         begin Y/W > 0 end]),
         [{a,1}] = qlc:e(Q)">>,
      <<"Q = qlc:q([{X,Y} || {X,Y} <- [{a,1},{b,2}], 
         {Z,W} <- [{a,1},{c,0}], 
         begin Y/W > 0 end,
         X =:= Z]),
         {'EXIT', _} = (catch qlc:e(Q))">>,

      <<"etsc(fun(E1) ->
                   etsc(fun(E2) ->
                             F = fun() -> [foo || A <- [0], 1/A] end,
                             Q1 = qlc:q([X || {X} <- ets:table(E1),
                                              {Y} <- ets:table(E2),
                                              F(), % invalidates next filter
                                              X =:= Y]),
                              {qlc,_,[{generate,_,{table,{ets,table,_}}},
                                      {generate,_,{table,{ets,table,_}}},_,_],
                              []} = i(Q1),
                             {'EXIT', _} = (catch qlc:e(Q1))
                        end, [{1},{2},{3}])
              end, [{a},{b},{c}])">>

    ],
    run(Config, Ts),
    ok.

%% Lookup join.
join_lookup(Config) when is_list(Config) ->
    Ts = [
       <<"E1 = create_ets(1, 10),
          E2 = create_ets(5, 15),
          Q = qlc:q([{X,Y} || {_,Y} <- ets:table(E2),
                              {X,_} <- ets:table(E1),
                              X =:= Y], [{join,lookup}]),
          {0,1,0,0} = join_info_count(Q),
          R = qlc:e(Q),
          ets:delete(E1),
          ets:delete(E2),
          [{5,5},{6,6},{7,7},{8,8},{9,9},{10,10}] = lists:sort(R)">>,

       <<"E1 = create_ets(1, 10),
          E2 = create_ets(5, 15),
          F = fun(J) -> qlc:q([{X,Y} || {X,_} <- ets:table(E1),
                                        {_,Y} <- ets:table(E2),
                                        X =:= Y], {join, J})
              end,
          Q = F(lookup),
          {0,1,0,0} = join_info_count(Q),
          R = qlc:e(Q),
          ets:delete(E1),
          ets:delete(E2),
          [{5,5},{6,6},{7,7},{8,8},{9,9},{10,10}] = lists:sort(R)">>,

       <<"etsc(fun(E1) ->
                  E2 = qlc_SUITE:table([{1,a},{a},{1,b},{b}], 2, []),
                  Q = qlc:q([{X,Y} || {X,Y} <- ets:table(E1), % (1)
                                      {_,Z} <- E2,   % (2)
                                      (Z =:= Y) and (X =:= a) 
                                      or
                                      (Z =:= Y) and (X =:= b)]),
                  %% Cannot look up in (1) (X is keypos). Can look up (2).
                  %% Lookup-join: traverse (1), look up in (2).
                  {0,1,0,0} = join_info_count(Q),
                  [{a,a},{b,a}] = qlc:e(Q)
               end, [{a,a},{b,a},{c,3},{d,4}])">>,

       <<"%% The pattern {X,_} is used to filter out looked up objects.
          etsc(fun(E) ->
                       Q = qlc:q([X || {X,_} <- ets:table(E), 
                                       Y <- [{a,b},{c,d},{1,2},{3,4}], 
                                       X =:= element(1, Y)]),
                       {0,1,0,0} = join_info_count(Q),
                       [1] = qlc:e(Q)
               end, [{1,2},{3}])">>,

       <<"E = ets:new(e, [bag,{keypos,2}]),
          L = lists:sort([{a,1},{b,1},{c,1},{d,1},
                          {aa,2},{bb,2},{cc,2},{dd,2}]),
          true = ets:insert(E, L ++ [{aaa,1,1},{bbb,2,2},{ccc,3,3}]),
          Q = qlc:q([Z || {_,Y}=Z <- ets:table(E),
                          {X} <- [{X} || X <- lists:seq(0, 10)],
                          X =:= Y]),
          {0,1,0,0} = join_info_count(Q),
          R = qlc:e(Q),
          ets:delete(E),
          L = lists:sort(R)">>,

       <<"E = ets:new(e, [bag,{keypos,2}]),
          L = lists:sort([{a,1},{b,1},{c,1},{d,1},
                          {aa,2},{bb,2},{cc,2},{dd,2}]),
          true = ets:insert(E, L ++ [{aaa,1,1},{bbb,2,2},{ccc,3,3}]),
          Q = qlc:q([Z || {X} <- [{X} || X <- lists:seq(0, 10)],
                          {_,Y}=Z <- ets:table(E),
                          X =:= Y]),
          {0,1,0,0} = join_info_count(Q),
          R = qlc:e(Q),
          ets:delete(E),
          L = lists:sort(R)">>,

       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- [{b,1},{c,3}],
                            {Y,YY} <- qlc_SUITE:table_lookup_error([{1,a}]),
                            X =:= Y],
                        {join,lookup}),
          {error, lookup, failed} = qlc:e(Q)">>,

       <<"E = create_ets(1, 10),
          Q = qlc:q([{X,Y} ||
                        {X,_} <- ets:table(E),
                        {_,Y} <- qlc_SUITE:table_error([{a,1}], 1, err),
                        X =:= Y]),
          {0,1,0,0} = join_info_count(Q),
          err = qlc:e(Q),
          ets:delete(E)">>

          ],
    run(Config, Ts),
    ok.

%% Merge join.
join_merge(Config) when is_list(Config) ->
    Ts = [
       <<"Q = qlc:q([{X,Y} || {X} <- [], {Y} <- [{1}], X =:= Y], 
                    {join,merge}),
          [] = qlc:e(Q)
       ">>,

       <<"Q = qlc:q([{X,Y} || {X} <- [{1}], {Y} <- [], X =:= Y], 
                    {join,merge}),
          [] = qlc:e(Q)
       ">>,

       <<"Q = qlc:q([{X,Y} || {X} <- [{1},{1},{1}], 
                              {Y} <- [{1},{1},{1}], X =:= Y], 
                    {join,merge}),
          9 = length(qlc:e(Q))
       ">>,

       <<"%% Two merge joins possible.
          Q = qlc:q([{X,Y,Z,W} || {X,Y} <- [{1,a},{1,b},{1,c}], 
                                  {Z,W} <- [{1,a},{1,b},{1,c}], 
                                  X =:= Z, 
                                  Y =:= W]),
          {qlc,_,[{generate,_,
                   {qlc,_,
                    [{generate,_,
                      {qlc,_,[{generate,_,{keysort,{list,_},C,[]}}],[]}},
                     {generate,_,
                      {qlc,_,[{generate,_,{keysort,{list,_},C,[]}}],[]}},
                     _],
                    [{join,merge}]}},
                  _,_],[]} = qlc:info(Q, {format,debug}),
          [{1,a,1,a},{1,b,1,b},{1,c,1,c}] = qlc:e(Q)">>,

       <<"%% As the last one, but comparison.
          Q = qlc:q([{X,Y,Z,W} || {X,Y} <- [{1,a},{1,b},{1,c}], 
                                  {Z,W} <- [{1,a},{1,b},{1,c}], 
                                  X == Z, % skipped
                                  Y =:= W]),
          {qlc,_,[{generate,_,
                   {qlc,_,
                    [{generate,_,
                      {qlc,_,[{generate,_,{keysort,{list,_},1,[]}}],[]}},
                     {generate,_,
                      {qlc,_,[{generate,_,{keysort,{list,_},1,[]}}],[]}},
                     _],
                    [{join,merge}]}},
                  _],[]} = qlc:info(Q, {format,debug}),
          [{1,a,1,a},{1,b,1,b},{1,c,1,c}] = qlc:e(Q)">>,

       <<"%% This is no join.
          Q = qlc:q([{X,Y,Z,W} || {X,Y} <- [], {Z,W} <- [], 
                                  X =:= Y, Z =:= W]),
          {0,0,0,0} = join_info_count(Q)">>,

       <<"%% Used to replace empty ETS tables with [], but that won't work.
          E1 = ets:new(e1, []),
          E2 = ets:new(e2, []),
          Q = qlc:q([{X,Z,W} ||
                        {X, Z} <- ets:table(E1),
                        {W, Y} <- ets:table(E2),
                        X =:= Y],
                    {join, lookup}),
          [] = qlc:e(Q),
          ets:delete(E1),
          ets:delete(E2)">>,

       <<"Q = qlc:q([{X,Y} || {X} <- [{3},{1},{0}], 
                              {Y} <- [{1},{2},{3}], 
                              X =:= Y]),
          {1,0,0,2} = join_info_count(Q),
          [{1,1},{3,3}] = qlc:e(Q)">>,

       <<"QH = qlc:q([{X,Y,Z,W} || {X,Y} <- [{3,c},{2,b},{1,a}],
                                   {Z,W} <- [{2,b},{4,d},{5,e},{3,c}], 
                                   X =:= Z, 
                                   Y =:= W]),
          {1,0,0,2} = join_info_count(QH),
          [{2,b,2,b},{3,c,3,c}] = qlc:e(QH)">>,

       <<"%% QLC finds no join column at run time...
          QH = qlc:q([1 || X <- [{1,2,3},{4,5,6}], 
                           Y <- [{1,2},{3,4}], 
                           X =:= Y]),
          {0,0,0,0} = join_info_count(QH),
          [] = qlc:e(QH)">>,

       <<"QH = qlc:q([X || X <- [{1,2,3},{4,5,6}], 
                           Y <- [{1,2},{3,4}], 
                           element(1, X) =:= element(2, Y)]),
          {1,0,0,2} = join_info_count(QH),
          [{4,5,6}] = qlc:e(QH)">>,

       <<"Q = qlc:q([{A,X,Z,W} ||
                        A <- [a,b,c],
                        {X,Z} <- [{a,1},{b,4},{c,6}],
                        {W,Y} <- [{2,a},{3,b},{4,c}],
                        X =:= Y],
                   {cache, list}),
          _ = qlc:info(Q),
         [{a,a,1,2},{a,b,4,3},{a,c,6,4},{b,a,1,2},{b,b,4,3},
          {b,c,6,4},{c,a,1,2},{c,b,4,3},{c,c,6,4}] = qlc:e(Q)">>,

       <<"Q = qlc:q([{X,Y} || 
                        {X,Z} <- [{a,1},{b,4},{c,6}],
                        {W,Y} <- [{2,a},{3,b},{4,c}],
                        Z > W,
                        X =:= Y],
                    {join,merge}),
          {qlc,_,[{generate,_,{qlc,_,
                              [{generate,_,
                                {qlc,_,[{generate,_,{keysort,_,1,[]}}],[]}},
                               {generate,_,
                                {qlc,_,[{generate,_,{keysort,_,2,[]}}],
                                 []}},_],[{join,merge}]}},
                  _,_],[]} = i(Q),
          [{b,b},{c,c}] = qlc:e(Q)">>,

       <<"E1 = create_ets(1, 10),
          E2 = create_ets(5, 15),
          %% A match spec.; Q does not see Q1 and Q2 as lookup-tables.
          Q1 = qlc:q([X || X <- ets:table(E1)]),
          Q2 = qlc:q([X || X <- ets:table(E2)]),
          F = fun(J) -> qlc:q([{X,Y} || X <- Q1,
                                        Y <- Q2,
                                        element(1,X) =:= element(1,Y)], 
                              [{join,J}])
              end,
          {'EXIT',{cannot_carry_out_join,_}} = (catch qlc:e(F(lookup))),
          Q = F(merge),
          {1,0,0,2} = join_info(Q),
          R = lists:sort(qlc:e(Q)),
          ets:delete(E1),
          ets:delete(E2),
          true = [{Y,Y} || X <- lists:seq(5, 10), {} =/= (Y = {X,X})] =:= R
       ">>,

       <<"E1 = create_ets(1, 10),
          E2 = create_ets(5, 15),
          Q = qlc:q([{X,Y} || X <- ets:table(E1),
                              Y <- ets:table(E2),
                              element(1,X) =:= element(1,Y)], 
                    [{join,merge}]),
          {1,0,0,2} = join_info(Q),
          R = lists:sort(qlc:e(Q)),
          ets:delete(E1),
          ets:delete(E2),
          true = [{Y,Y} || X <- lists:seq(5, 10), {} =/= (Y = {X,X})] =:= R
       ">>,

       <<"E1 = create_ets(1, 10),
          E2 = create_ets(5, 15),
          Q1 = qlc:q([Y || X <- ets:table(E1), begin Y = {X}, true end]),
          %% A match spec.; Q does not see Q2 as a lookup-table.
          %%
          %% OTP-6673: lookup join is considered but since there is no
          %% filter that can do the job of Q2, lookup join is not an option..
          Q2 = qlc:q([{X} || X <- ets:table(E2)]),
          F = fun(J) ->
                      qlc:q([{X,Y} || X <- Q1,
                                      Y <- Q2,
                                      element(1,X) =:= element(1,Y)],
                            [{join,J}])
              end,
          {'EXIT',{cannot_carry_out_join,_}} = (catch qlc:e(F(lookup))),
          Q = F(any),
          {1,0,0,2} = join_info(Q),
          R = lists:sort(qlc:e(Q)),
          ets:delete(E1),
          ets:delete(E2),
          true = [{Y,Y} || X <- lists:seq(5, 10), {} =/= (Y = {{X,X}})] =:= R
       ">>,

       <<"L1 = [{1,a},{2,a},{1,b},{2,b},{1,c},{2,c}],
          L2 = [{b,Y} || Y <- lists:seq(1, 10000)],
          F = fun(J) ->
                      Q = qlc:q([{XX,YY} ||
                                    {X,XX} <- L1,
                                    {YY,Y} <- L2,
                                    X == Y],
                                {join,J}),
                      qlc:q([{XX1,YY1,XX2,YY2} ||
                                {XX1,YY1} <- Q,
                                {XX2,YY2} <- Q])
              end,
          Qm = F(merge),
          Qn = F(nested_loop),
          true = lists:sort(qlc:e(Qm)) =:= lists:sort(qlc:e(Qn))">>,

       <<"L1 = [{{1,a},2},{{3,c},4}],
          L2 = [{a,{1,a}},{c,{4,d}}],
          Q = qlc:q([{X,Y} || {X,_} <- L1,
                              {_,{Y,Z}} <- L2,
                              X == {Y,Z}
                           ]),
          {qlc,_,[{generate,_,{qlc,_,
                   [{generate,_,
                     {qlc,_,[{generate,_,{keysort,{list,L1},1,[]}}],[]}},
                    {generate,_,
                     {qlc,_,[{generate,_,{keysort,{list,L2},2,[]}}],[]}},
                    _],
                   [{join,merge}]}}],[]}  = i(Q),
          [{{1,a},1}] = qlc:e(Q)">>,

       <<"etsc(fun(E1) ->
                   etsc(fun(E2) ->
                      Q = qlc:q([{X,Y} || {X,Y} <- ets:table(E1), % (1)
                                          {Z} <- ets:table(E2),   % (2)
                                            (Z =:= X) and 
                                            (Y =:= a) and 
                                            (X =:= Y) or 
                                          (Y =:= b) and 
                                          (Z =:= Y)]),
                      %% Cannot look up in (1) (X is keypos). Can look up (2).
                      %% Lookup join not possible (cannot look up in (1)).
                      %% Merge join is possible (after lookup in (2)).
                      {1,0,0,2} = join_info_count(Q),
                      {qlc,_,
                       [{generate,_,
                         {qlc,_,[{generate,_,
                                  {qlc,_,[{generate,_,
                                           {keysort,
                                            {table,{ets,table,_}},
                                                  2,[]}},_C1],[]}},
                                 {generate,_,
                                  {qlc,_,[{generate,_,
                                           {keysort,{table,_},1,[]}},_C2],
                                   []}},
                                 _],[{join,merge}]}},_],[]} = i(Q),
                      [{a,a}] = qlc:e(Q)
                   end, [{a}])
                end, [{a,1},{a,a},{b,1},{b,2}])">>,

       <<"Q = qlc:q([{G1,G2} || 
                        G1<- [{1}],
                        G2 <- [{1}],
                        element(1, G1) =:= element(1, G2)]),
          {1,0,0,2} = join_info(Q),
          [{{1},{1}}] = qlc:e(Q)">>,

       <<"Q = qlc:q([{X,Y} || 
                         X <- [{1}], 
                         Y <- [{1}], 
                         element(1, X) =:= element(1, Y)], 
                     {join,merge}),
          {1,0,0,2} = join_info(Q),
          [{{1},{1}}] = qlc:e(Q)">>,

       <<"%% Generator after the join-filter.
          Q = qlc:q([Z || 
                        {X} <- [{1},{2},{3}], 
                        {Y} <- [{2},{3},{4}], 
                        X =:= Y, 
                        Z <- [1,2]]),
          {qlc,_,
           [{generate,_,{qlc,_,
                [{generate,_,{qlc,_,
                    [{generate,_,{keysort,{list,[{1},{2},{3}]},1,[]}}],[]}},
                {generate,_,{qlc,_,
                    [{generate,_,{keysort,{list,_},1,[]}}],[]}},_],
                [{join,merge}]}}, _,{generate,_,{list,_}}],[]} = i(Q),
          [1,2,1,2] = qlc:e(Q)">>,

       <<"%% X and W occur twice in the pattern of the extra join handle.
          Q = qlc:q([{Z,W} ||
                        {X,Z,X} <- [{1,2,1},{1,2,2}],
                        {W,Y,W} <- [{a,1,a}],
                        X =:= Y]),
          [{2,a}] = qlc:e(Q)">>

          ],
    run(Config, Ts),

    %% Small examples. Returning an error term.
    ETs = [
       <<"F = fun(M) ->
                  qlc:q([{XX,YY} ||
                         {XX,X} <- [{a,1},{b,2},{bb,2},{c,3},{cc,3}],
                         {Y,YY} <- [{0,a},{1,a},{1,aa},{2,b},{2,bb},{2,bbb},
                                    {3,c},{3,cc}],
                          X =:= Y],
                        {join,M})
              end,
          R = qlc:e(F(nested_loop)),
          R = qlc:e(F(merge))">>,

       <<"F = fun(M) ->
                qlc:q([{XX,YY} ||
                       {XX,X} <- [{a,1},{b,2},{bb,2},{c,3},{cc,3}],
                       {Y,YY} <- [{0,a},{1,a},{1,aa},{2,b},{2,bb},{2,bbb},
                                  {4,d}],
                        X =:= Y],
                      {join,M})
              end,
          R = qlc:e(F(nested_loop)),
          R = qlc:e(F(merge))">>,

       <<"Q = qlc:q([{XX,YY} ||
                        {XX,X} <- [{b,1},{c,3}],
                        {Y,YY} <- [{1,a}],
                        X =:= Y],
                    {join,merge}),
          [{b,a}] = qlc:e(Q)">>,

       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- [{b,1},{c,3}],
                            {Y,YY} <- qlc_SUITE:table_error([{1,a}], 1, err),
                            X =:= Y],
                        {join,merge}),
              err = qlc:e(Q)">>,
           
       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- [{a,1},{aa,1}],
                            {Y,YY} <- [{1,a}],
                            X =:= Y],
                        {join,merge}),
              [{a,a},{aa,a}] = qlc:e(Q)">>,

       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- qlc_SUITE:table_error([{a,1},{aa,1}], 
                                                             2, err),
                            {Y,YY} <- [{1,a}],
                            X =:= Y],
                        {join,merge}),
              err = qlc:e(Q)">>,
           
       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- [{a,1}],
                            {Y,YY} <- [{1,a},{1,aa}],
                            X =:= Y],
                        {join,merge}),
              [{a,a},{a,aa}]= qlc:e(Q)">>,
           
       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- qlc_SUITE:table_error([{a,1}], 2, err),
                            {Y,YY} <- [{1,a},{1,aa}],
                            X =:= Y],
                        {join,merge}),
          C = qlc:cursor(Q),
          [{a,a}] = qlc:next_answers(C, 1),
          qlc:delete_cursor(C),
          err = qlc:e(Q)">>,

       <<"F = fun(M) ->
                    qlc:q([{XX,YY} ||
                               {XX,X} <- [{a,1},{b,2},{bb,2},{c,3},{cc,3}],
                               {Y,YY} <- [{0,a},{1,a},{1,aa},{2,b},
                                          {2,bb},{2,bbb}],
                            X =:= Y],
                          {join,M})
              end,
              %% [{a,a},{a,aa},{b,b},{b,bb},{b,bbb},{bb,b},{bb,bb},{bb,bbb}]
              R = qlc:e(F(nested_loop)),
              R = qlc:e(F(merge))">>,


       <<"F = fun(M) ->
                   qlc:q([{XX,YY} ||
                          {XX,X} <- [{a,1},{b,2},{bb,2},{c,3},{cc,3}],
                          {Y,YY} <- qlc_SUITE:table_error([{0,a},{1,a},{1,aa},
                                                           {2,b},{2,bb},
                                                           {2,bbb}], 
                                                          1, err),
                           X =:= Y],
                         {join,M})
              end,
              %% [{a,a},{a,aa},{b,b},{b,bb},{b,bbb},{bb,b},{bb,bb},{bb,bbb}]
              err = qlc:e(F(nested_loop)),
              err = qlc:e(F(merge))">>,

       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- qlc_SUITE:table_error([], 2, err),
                            {Y,YY} <- [{2,b},{3,c}],
                            X =:= Y],
                        {join,merge}),
              err = qlc:e(Q)">>,

       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- [{a,1},{c,3}],
                            {Y,YY} <- [{2,b},{3,c}],
                            X =:= Y],
                        {join,merge}),
              [{c,c}] = qlc:e(Q)">>,

       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- [{a,1},{aa,1}],
                            {Y,YY} <- [{1,a},{1,aa}],
                            X =:= Y],
                        {join,merge}),
              [{a,a},{a,aa},{aa,a},{aa,aa}] = qlc:e(Q)">>,

       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- [{a,1},{b,2}],
                            {Y,YY} <- [{1,a},{1,aa}],
                            X =:= Y],
                        {join,merge}),
              [{a,a},{a,aa}] = qlc:e(Q)">>,

       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- [{a,1},{b,2}],
                            {Y,YY} <- qlc_SUITE:table_error([{1,a},{1,aa}], 
                                                            1, err),
                            X =:= Y],
                        {join,merge}),
              err = qlc:e(Q)">>,

       <<"Q = qlc:q([{XX,YY} ||
                            {XX,X} <- [{a,1},{b,2}],
                            {Y,YY} <- [{1,a},{1,aa},{1,aaa},{1,aaaa}],
                            X =:= Y],
                        {join,merge}),
              [{a,a},{a,aa},{a,aaa},{a,aaaa}]= qlc:e(Q)">>,

       <<"Q = qlc:q([{element(1, X), element(2, Y)} ||
                            X <- [{a,1},{aa,1}],
                            Y <- [{1,a},{1,aa}],
                            element(2, X) =:= element(1, Y)],
                        {join,merge}),
              [{a,a},{a,aa},{aa,a},{aa,aa}] = qlc:e(Q)">>,

       <<"Q = qlc:q([{element(1, X), element(2, Y)} ||
                            X <- [{a,1},{aa,1}],
                            Y <- qlc_SUITE:table_error([], 1, err),
                            element(2, X) =:= element(1, Y)],
                        {join,merge}),
              err = qlc:e(Q)">>,

       <<"Q = qlc:q([{element(1, X), element(2, Y)} ||
                            X <- qlc_SUITE:table_error([{a,1}], 2, err),
                            Y <- [{2,b}],
                            element(2, X) =:= element(1, Y)],
                        {join,merge}),
              err = qlc:e(Q)">>,

       <<"Q = qlc:q([{XX,YY} ||
                  {XX,X} <- [{1,a},{'1b',b},{2,b}],
                  {Y,YY} <- [{a,1},{b,'1b'},{c,1}],
                  X == Y],
              {join,merge}),
          [{1,1},{'1b','1b'},{2,'1b'}] = qlc:e(Q)">>,

       <<"Q = qlc:q([{XX,YY} ||
                  {XX,X} <- qlc_SUITE:table_error([{1,a},{'1b',b},{2,b}], 
                                                  2, err),
                  {Y,YY} <- [{a,1},{b,'1b'},{c,1}],
                  X == Y],
              {join,merge}),
          err = qlc:e(Q)">>

          ],
    run(Config, ETs),

    %% Mostly examples where temporary files are needed while merging.
    FTs = [
       <<"L1 = [{Y,a} || Y <- lists:seq(1, 2)],
          L2 = [{a,Y} || Y <- lists:seq(1, 10000)],
          F = fun(J) ->
                      qlc:q([{XX,YY} ||
                                {XX,X} <- L1,
                                {Y,YY} <- L2,
                                X == Y],
                            {join,J})
              end,
          Qm = F(merge),
          Qn = F(nested_loop),
          true = qlc:e(Qm,{max_list_size, 0}) =:= qlc:e(Qn)">>,

       <<"L1 = [{Y,a} || Y <- lists:seq(1, 2)],
          L2 = [{a,Y} || Y <- lists:seq(1, 10000)],
          Q = qlc:q([{XX,YY} ||
                        {XX,X} <- L1,
                        {Y,YY} <- L2,
                        X == Y],
                    {join,merge}),
          {error,_,{file_error,_,_}} = 
               qlc:e(Q, [{max_list_size,64*1024},{tmpdir,\"/a/b/c\"}])">>,

       <<"L1 = qlc_SUITE:table_error([{1,a},{2,a}], 2, err),
          L2 = [{a,Y} || Y <- lists:seq(1, 10000)],
          F = fun(J) ->
                      qlc:q([{XX,YY} ||
                                {XX,X} <- L1,
                                {Y,YY} <- L2,
                                X == Y],
                            {join,J})
              end,
          Qm = F(merge),
          Qn = F(nested_loop),
          err = qlc:e(Qm, {max_list_size,64*1024}),
          err = qlc:e(Qn)">>,

       <<"L1 = [{Y,a} || Y <- lists:seq(1, 2)],
          L2 = qlc_SUITE:table_error([{a,Y} || Y <- lists:seq(1, 10000)], 
                                     1, err),
          F = fun(J) ->
                      qlc:q([{XX,YY} ||
                                {XX,X} <- L1,
                                {Y,YY} <- L2,
                                X == Y],
                            {join,J})
              end,
          Qm = F(merge),
          Qn = F(nested_loop),
          err = qlc:e(Qm, {max_list_size,64*1024}),
          err = qlc:e(Qn)">>,

       <<"L1 = [{Y,a} || Y <- lists:seq(1, 2)] ++ 
               [{'1b',b},{2,b}] ++ [{Y,d} || Y <- lists:seq(1, 2)],
          L2 = [{a,Y} || Y <- lists:seq(1, 10000)] ++ 
               [{b,'1b'}] ++ [{c,1}] ++ [{d,Y} || Y <- lists:seq(1, 10000)],
          F = fun(J) ->
                      qlc:q([{XX,YY} ||
                                {XX,X} <- L1,
                                {Y,YY} <- L2,
                                X == Y],
                            {join,J})
              end,
          Qm = F(merge),
          Qn = F(nested_loop),
          true = lists:sort(qlc:e(Qm, {max_list_size,64*1024})) =:= 
                 lists:sort(qlc:e(Qn))">>,

       <<"F = fun(J) ->
                      qlc:q([{XX,YY} ||
                                {XX,X} <- [{Y,a} || Y <- lists:seq(1, 2)],
                                {Y,YY} <- [{a,Y} || Y <- lists:seq(1,100000)],
                                X == Y],
                            {join,J})
              end,
          Qm = F(merge),
          Qn = F(nested_loop),
          true = qlc:e(Qm, {max_list_size,64*1024}) =:= qlc:e(Qn)">>,

       %% More than one join in one QLC expression.
       <<"L1 = [{Y,a} || Y <- lists:seq(1, 2)],
          L2 = [{a,Y} || Y <- lists:seq(1, 10000)],
          F = fun(J) ->
                      Q = qlc:q([{XX,YY} ||
                                    {XX,X} <- L1,
                                    {Y,YY} <- L2,
                                    X == Y,
                                    begin XX > 1 end,
                                    begin YY > 9999 end],
                                {join,J}),
                      qlc:q([{XX1,YY1,XX2,YY2} ||
                                {XX1,YY1} <- Q,
                                {XX2,YY2} <- Q])
              end,
          Qm = F(merge),
          Qn = F(nested_loop),
          R1 = lists:sort(qlc:e(Qm, {max_list_size,64*1024})),
          R2 = lists:sort(qlc:e(Qm, {max_list_size,1 bsl 31})),
          true = R1 =:= lists:sort(qlc:e(Qn)),
          true = R1 =:= R2">>,

       <<"L1 = [{Y,a} || Y <- lists:seq(1, 2)],
          L2 = [{a,Y} || Y <- lists:seq(1, 10000)],
          F = fun(J) ->
                      Q = qlc:q([{XX,YY} ||
                                    {XX,X} <- L1,
                                    {Y,YY} <- L2,
                                    X == Y,
                                    begin XX > 1 end,
                                    begin YY > 9999 end],
                                {join,J}),
                      qlc:q([{XX1,YY1,XX2,YY2} ||
                                {XX1,YY1} <- Q,
                                {XX2,YY2} <- Q,
                                throw(thrown)])
              end,
          Qm = F(merge),
          thrown = (catch {any_term, qlc:e(Qm, {max_list_size,64*1024})})">>,

       <<"%% Bigger than 64*1024.
          T1 = {1, lists:seq(1, 20000)},
          L1 = [{a,T1},{b,T1}],
          L2 = [{T1,a},{T1,b}],
          F = fun(J) ->
                      qlc:q([{XX,YY} ||
                                {XX,X} <- L1,
                                {Y,YY} <- L2,
                                X == Y],
                            {join,J})
              end,
          Qm = F(merge),
          Qn = F(nested_loop),
          R = [{a,a},{a,b},{b,a},{b,b}],
          R = qlc:e(Qm, {max_list_size,64*1024}),
          R = qlc:e(Qn)">>,

       <<"%% Bigger than 64*1024. No temporary files.
          T1 = {1, lists:seq(1, 20000)},
          L1 = [{a,T1},{b,T1}],
          L2 = [{T1,a},{T1,b}],
          F = fun(J) ->
                      qlc:q([{XX,YY} ||
                                {XX,X} <- L1,
                                {Y,YY} <- L2,
                                X == Y],
                            {join,J})
              end,
          Qm = F(merge),
          Qn = F(nested_loop),
          R = [{a,a},{a,b},{b,a},{b,b}],
          R = qlc:e(Qm, {max_list_size,1 bsl 31}),
          R = qlc:e(Qn)">>


          ],
    run(Config, FTs),
    
    ok.

%% Merge join optimizations (avoid unnecessary sorting).
join_sort(Config) when is_list(Config) ->
    Ts = [
       <<"H1_1 = qlc:keysort(1, [{1,2,3},{4,5,6}]),
          H1 = qlc:q([X || X <- H1_1], unique),
          H2 = qlc:keysort(2, [{1,2},{3,4}]),
          H3 = qlc:q([{X,Y} || {X,_,_} <- H1, 
                               {_,Y} <- H2, 
                               X =:= Y]),
          {1,0,0,2} = join_info(H3),
          [{4,4}] = qlc:e(H3)">>,

       <<"H1_1 = qlc:keysort(1, [{1,2,3},{4,5,6}]),
          H1 = qlc:q([X || X <- H1_1], unique), % keeps the order
          H2 = qlc:keysort(2, [{1,2},{3,4}]),
          H3 = qlc:q([{X,Y} || {X,_,_} <- H1, % no extra keysort
                               {Y,_} <- H2,   % an extra keysort
                               X =:= Y]),
          {1,0,0,3} = join_info(H3),
          [{1,1}] = qlc:e(H3)">>,

       <<"H1_1 = qlc:keysort(1, [{1,2,3},{4,5,6}], {tmpdir,\"\"}),
          H1 = qlc:q([X || X <- H1_1], unique),
          H2 = qlc:keysort(2, [{1,2},{3,4}]),
          H3 = qlc:q([{X,Y} || {_,X,_} <- H1, 
                               {_,Y} <- H2, 
                               X =:= Y]),
          {1,0,0,3} = join_info(H3),
          [{2,2}] = qlc:e(H3)">>,

       <<"H1_1 = qlc:keysort(1, [{1,2,3},{4,5,6}], {tmpdir,\"\"}),
          H1 = qlc:q([X || X <- H1_1], unique),
          H2 = qlc:keysort(2, [{1,2},{3,4}]),
          H3 = qlc:q([{X,Y} || {_,X,_} <- H1, 
                               {_,Y} <- H2, 
                               X =:= Y]),
          {1,0,0,3} = join_info(H3),
          [{2,2}] = qlc:e(H3)">>,

       <<"H1 = qlc:sort([{1,a},{2,b},{3,c}]),
          %% Since H1 is sorted it is also keysorted on the first column.
          Q = qlc:q([{X, Y} || {X,_} <- H1,
                               {Y} <- [{0},{1},{2}],
                               X == Y]),
          {1,0,0,1} = join_info(Q),
          [{1,1},{2,2}] = qlc:e(Q)">>,

       <<"H1 = qlc:sort([{r,a,1},{r,b,2},{r,c,3}]),
          Q = qlc:q([{X, Y} || {r,_,X} <- H1, % needs keysort(3)
                               {Y} <- [{0},{1},{2}],
                               X == Y]),
          {1,0,0,2} = join_info(Q),
          [{1,1},{2,2}] = qlc:e(Q)">>,

       <<"QH = qlc:q([X || X <- [{1,2,3},{4,5,6}], 
                           Y <- qlc:sort([{1,2},{3,4}]), 
                           element(1, X) =:= element(2, Y)]),
          {1,0,0,2} = join_info_count(QH),
          [{4,5,6}] = qlc:e(QH)">>,

       <<"H1_1 = qlc:keysort(1, [{1,2,3},{4,5,6},{1,2,3}]),
          H1 = qlc:q([X || X <- H1_1], unique),
          H2 = qlc:keysort(2, [{2,1},{3,4}]),
          H3 = qlc:q([{X,Y} || {X,_,_} <- H1, 
                               {_,Y} <- H2, 
                               X =:= Y]),
          H4 = qlc:keysort(1, [{1,2},{3,4},{4,a}]),
          H5 = qlc:q([{X,Y} || {X,_} <- H4,
                               {_,Y} <- H3,
                               X =:= Y]),
          {2,0,0,3} = join_info_count(H5),
          [{1,1},{4,4}]= qlc:e(H5)">>,

       <<"
          H1 = qlc:keysort(2, [{1,a,u},{2,b,k},{3,c,l}]),
          H2 = qlc:q([{a,X,Y,a} || {1,X,u} <- H1,
                                   {2,Y,k} <- H1]),
          %% Neither H1 nor H2 need to be key-sorted 
          %% (the columns are constant).
          H3 = qlc:q([{A,B,C,D,E,F,G,H} ||
                         {A,B,C,D} <- H2,
                         {E,F,G,H} <- H2,
                         A =:= H],
                     {join,merge}),
          {1,0,0,4} = join_info_count(H3),
          [{a,a,b,a,a,a,b,a}] = qlc:e(H3)">>,

       <<"%% Q1 is sorted on X or Y.
          Q1 = qlc:q([{X,Y} ||
                         {X,_} <- qlc:keysort(1, [{1,a},{2,b}]),
                         {_,Y} <- qlc:keysort(2, [{aa,11},{bb,22}]),
                         X < Y]),
          [{1,11},{1,22},{2,11},{2,22}] = qlc:e(Q1),
          Q = qlc:q([{X,Y} ||
                        {X,_} <- Q1, % no need to sort Q1
                        {Y} <- [{0},{1},{2},{3}],
                        X =:= Y]),
          {1,0,0,3} = join_info_count(Q),
          [{1,1},{1,1},{2,2},{2,2}] = qlc:e(Q)">>,

       <<"H1 = qlc:keysort([2], [{r,1},{r,2},{r,3}]),
          %% H1 is actually sorted, but this info is not captured.
          Q = qlc:q([{X, Y} || {r,X} <- H1,
                               {Y} <- [{0},{1},{2}],
                               X == Y]),
          {1,0,0,2} = join_info_count(Q),
          [{1,1},{2,2}] = qlc:e(Q)">>,

       <<"%% Two leading constants columns and sorted objects
          %% implies keysorted on column 3.
          H1 = qlc:sort(qlc:q([{a,X,Y} || {X,Y} <- [{1,2},{2,3},{3,3}]])),
          H2 = qlc:q([{X,Y} || 
                         {a,3,X} <- H1,
                         {a,2,Y} <- H1,
                         X =:= Y]),
          {1,0,0,0} = join_info_count(H2),
          [{3,3}] = qlc:e(H2)">>,

       <<"QH = qlc:q([{X,Y} || {X,Y} <- [{1,4},{1,3}],
                               {Z} <- [{1}],
                               X =:= Z, (Y =:= 3) or (Y =:= 4)]),
          {1,0,0,1} = join_info_count(QH),
          [{1,4},{1,3}] = qlc:e(QH)">>,

       <<"E = ets:new(join, [ordered_set]),
          true = ets:insert(E, [{1,a},{2,b},{3,c}]),
          Q = qlc:q([{X, Y} || {X,_} <- ets:table(E), % no need to sort
                               {Y} <- [{0},{1},{2}],
                               X == Y], {join,merge}),
          {1,0,0,1} = join_info(Q),
          [{1,1},{2,2}] = qlc:e(Q),
          ets:delete(E)">>,

       <<"H1 = qlc:sort([{r,1,a},{r,2,b},{r,3,c}]),
          Q = qlc:q([{X, Y} || {r,X,_} <- H1, % does not need keysort(3)
                               {Y} <- [{0},{1},{2}],
                               X == Y]),
          {1,0,0,1} = join_info(Q),
          [{1,1},{2,2}] = qlc:e(Q)">>,

       <<"H1 = qlc:keysort(2,[{r,1},{r,2},{r,3}]),
          H2 = [{a},{b}],
          %% Several columns in different qualifiers have initial 
          %% constant columns.
          H3 = qlc:keysort(1,[{c1,c2,1},{foo,bar,2},{c1,c2,3},{c1,c2,2}]),
          Q = qlc:q([{r,X,Y,Z} || {r,X} <- H1,
                                  {Y} <- H2,
                                  {c1,c2,Z} <- H3,
                                  X =:= Z], {join,merge}),
          {1,0,0,3} = join_info(Q),
          [{r,1,a,1},{r,1,b,1},{r,2,a,2},{r,2,b,2},{r,3,a,3},{r,3,b,3}] =
              qlc:e(Q)">>,

       <<"H1 = qlc:keysort(2,[{r,1},{r,2},{r,3}]),
          H2 = [{a},{b}],
          %% As the last one, but one keysort less.
          H3 = qlc:keysort(3,[{c1,c2,1},{foo,bar,2},{c1,c2,3},{c1,c2,2}]),
          Q = qlc:q([{r,X,Y,Z} || {r,X} <- H1,
                                  {Y} <- H2,
                                  {c1,c2,Z} <- H3,
                                  X =:= Z], {join,merge}),
          {1,0,0,2} = join_info(Q),
          [{r,1,a,1},{r,1,b,1},{r,2,a,2},{r,2,b,2},{r,3,a,3},{r,3,b,3}] =
              qlc:e(Q)">>,

       <<"H1 = qlc:keysort(2,[{r,1},{r,2},{r,3}]),
          H2 = [{a},{b}],
          H3 = qlc:keysort(1,[{c1,c2,1},{foo,bar,2},{c1,c2,3},{c1,c2,2}]),
          %% One generator before the joined generators.
          Q = qlc:q([{r,X,Y,Z} || {Y} <- H2,
                                  {r,X} <- H1,
                                  {c1,c2,Z} <- H3,
                                  X =:= Z], {join,merge}),
          {1,0,0,3} = join_info(Q),
          [{r,1,a,1},{r,2,a,2},{r,3,a,3},{r,1,b,1},{r,2,b,2},{r,3,b,3}] = 
              qlc:e(Q)">>,

       <<"H1 = [{a,1},{b,2},{c,3},{d,4}],
          H2 = [{a},{b}],
          H3 = [{c1,c2,a},{foo,bar,b},{c1,c2,c},{c1,c2,d}],
          %% A couple of \"extra\" filters and generators.
          Q = qlc:q([{X,Y,Z} || {X,_} <- H1,
                                {Y} <- H2,
                                X > Y,
                                {c1,c2,Z} <- H3,
                                {W} <- [{a},{b}],
                                W > a,
                                X =:= Z]),
          {1,0,0,2} = join_info(Q),
          [{c,a,c},{c,b,c},{d,a,d},{d,b,d}] = qlc:e(Q)">>,

       <<"H1 = qlc:keysort(2,[{r,1},{r,2},{r,3}]),
          H2 = qlc:sort([{c1,c2,1},{foo,bar,2},{c1,c2,3},{c1,c2,2}]),
          %% H2 is sorted, no keysort necessary.
          %% This example shows that the 'filter-part' of the pattern
          %% ({c1,c2,Z}) should be evaluated _before_ the join.
          %% Otherwise the objects cannot be assumed to be keysort:ed on the
          %% third column (if merge join), and lookup-join would lookup
          %% more keys than necessary.
          Q = qlc:q([{r,X,Z} || {r,X} <- H1,
                                {c1,c2,Z} <- H2,
                                X =:= Z] ,{join,merge}),
          {1,0,0,1} = join_info(Q),
          [{r,1,1},{r,2,2},{r,3,3}] = qlc:e(Q)">>,

       <<"H1 = [{1,a},{2,b},{3,c}],
          H2 = [{0,0},{1,1},{2,2}],
          H3 = qlc:q([{A,C,D} ||
                         {A,_B} <- H1,
                         {C,D} <- H2,
                         A == D, C == D]),
          H4 = [{1,1},{2,2},{3,3}],
          H5 = qlc:q([{X,Y} ||
                         {X,_,_} <- H3, % no need to sort this one (merge join)
                         {_,Y} <- H4,
                         X == Y]),
          Q = qlc:q([{X,Y} ||
                        {X,_} <- H5, % no need to sort this one
                        {Y,_} <- H4,
                        X == Y]),
          {{3,0,0,4},{3,0,0,6}} = join_info(Q),
          [{1,1},{2,2}] = qlc:e(Q)">>,

       <<"%% There is an extra test (_C1, element(1, X) =:= 1) that is not
          %% necessary since the match spec does the same check. This can be
          %% improved upon.
          Q = qlc:q([{X,Y} ||
                        X <- [{2},{1}],
                        element(1, X) =:= 1,
                        Y=_ <- [{2},{1}],
                        element(1, X) =:= element(1, Y)]),
          {qlc,_,
              [{generate,_,{qlc,_,
                              [{generate,_,{qlc,_,
                                             [{generate,_,{list,{list,_},_}},
                                              _C1],[]}},
                               {generate,_,{qlc,_,
                                             [{generate,_,{list,[{2},{1}]}},
                                              _C2],[]}},_],
                              [{join,merge}]}},_],[]} = i(Q),
          {1,0,0,0} = join_info_count(Q),
          [{{1},{1}}] = qlc:e(Q)">>,

       <<"etsc(fun(E) ->
                       L = [{a,b,a},{c,d,b},{1,2,a},{3,4,b}],
                       Q = qlc:q([P1 || {X,2,Z}=P1 <- ets:table(E), 
                                        Y <- L,
                                        X =:= 1,
                                        Z =:= a,
                                        P1 =:= Y, 
                                        X =:= element(1, Y)]),
                       {1,0,0,0} = join_info_count(Q),
                       [{1,2,a}] = qlc:e(Q)
               end, [{1,2,a},{3,4,b}])">>,

       %% Merge join on Z and element(3, Y). No need to sort!
       <<"etsc(fun(E) ->
                       L = [{a,b,a},{c,d,b},{1,2,a},{3,4,b}],
                       Q = qlc:q([P1 || {X,2,Z}=P1 <- ets:table(E), 
                                        Y <- L,
                                        (X =:= 1) or (X =:= 2),
                                        Z =:= a,
                                        P1 =:= Y, 
                                        X =:= element(1, Y)]),
                       {1,0,0,0} = join_info_count(Q),
                       [{1,2,a}] = qlc:e(Q)
               end, [{1,2,a},{3,4,b}])">>,

       <<"%% Y is constant as well as X. No keysort, which means that
          %% Y must be filtered before merge join.
          etsc(fun(E) ->
                       Q = qlc:q([X || {1,2}=X <- ets:table(E), 
                                       Y <- [{a,b},{c,d},{1,2},{3,4}], 
                                       X =:= Y, 
                                       element(1, X) =:= element(1, Y)]),
                       {1,0,0,0} = join_info_count(Q),
                       [{1,2}] = qlc:e(Q)
               end, [{1,2},{3,4}])">>

         ],
    run(Config, Ts),
    ok.

%% Join of more than two columns.
join_complex(Config) when is_list(Config) ->
    Ts = [{three,
           <<"three() ->
                  L = [],
                  Q = qlc:q([{X,Y,Z} || {X,_} <- L,
                                        {_,Y} <- L,
                                        {Z,_} <- L,
                                        X =:= Y, Y == Z
                                     ]),
                  qlc:e(Q).">>,
           [],
           {warnings,[{3,qlc,too_complex_join}]}},

          {two,
           <<"two() ->
                  Q = qlc:q([{X,Y,Z,W} || 
                      {X} <- [], 
                      {Y} <- [], 
                      {Z} <- [], 
                      {W} <- [], 
                      X =:= Y, 
                      Z =:= W],{join,merge}),
                  qlc:e(Q).">>,
           [],
           {warnings,[{2,qlc,too_many_joins}]}}
       ],

    compile(Config, Ts),

    Ts2 = [{three,
            <<"three() ->
                  L = [],
                  Q = qlc:q([{X,Y,Z} || {X,_} <- L,
                                        {_,Y} <- L,
                                        {Z,_} <- L,
                                        X =:= Y, Y == Z
                                     ]),
                  qlc:e(Q).">>,
            [],
            {[],
             ["cannot handle join of three or more generators efficiently"]}},

          {two,
           <<"two() ->
                  Q = qlc:q([{X,Y,Z,W} || 
                      {X} <- [], 
                      {Y} <- [], 
                      {Z} <- [], 
                      {W} <- [], 
                      X =:= Y, 
                      Z =:= W],{join,merge}),
                  qlc:e(Q).">>,
           [],
           {[],["cannot handle more than one join efficiently"]}}
       ],

    compile_format(Config, Ts2),

    ok.


%% OTP-5644. Handle the new language element M:F/A.
otp_5644(Config) when is_list(Config) ->
    Ts = [
       <<"Q = qlc:q([fun modul:mfa/0 || _ <- [1,2], 
                                        is_function(fun modul:mfa/0, 0)]),
          [_,_] = qlc:eval(Q)">>
       ],
    
    run(Config, Ts),
    ok.

%% OTP-5195. Allow traverse functions returning terms.
otp_5195(Config) when is_list(Config) ->
    %% Several minor improvements have been implemented in OTP-5195.
    %% The test cases are spread all over... except these.
    %%
    %% Traverse functions returning terms.

    Ts = [<<"L = [1,2,3],
             Err = {error,modul,err},
             H = qlc:q([X || X <- qlc_SUITE:table_error(L, Err)]),
             Err = qlc:e(H)">>,

          <<"Err = {error,modul,err},
             TravFun = fun() -> Err end,
             H1 = qlc:sort(qlc:q([X || X <- qlc:table(TravFun, [])])),
             H = qlc:q([{X} || X <- H1]),
             Err = qlc:e(H)">>,

          <<"L = [1,2,3],
             Err = {error,modul,err},
             H = qlc:q([X || X <- qlc_SUITE:table_error(L, Err)]),
             C = qlc:cursor(H),
             R = qlc:next_answers(C, all_remaining),
             qlc:delete_cursor(C),
             Err = R">>,

          <<"L = [1,2,3],
             Err = {error,modul,err},
             H = qlc:q([X || X <- qlc_SUITE:table_error(L, Err)]),
             F = fun(Obj, A) -> A++[Obj] end,
             Err = qlc:fold(F, [], H)">>,

          <<"Err = {error,modul,err},
             TravFun = fun() -> Err end,
             H1 = qlc:sort(qlc:q([X || X <- qlc:table(TravFun, [])])),
             H = qlc:q([{X} || X <- H1]),
             F = fun(Obj, A) -> A++[Obj] end,
             Err = qlc:fold(F, [], H)">>,

          <<"Q1 = qlc:append([qlc:append([ugly()]),[3]]),
             Q = qlc:q([X || X <- Q1]),
             42 = qlc:e(Q),
             ok.

             ugly() ->
                 [apa | fun() -> 42 end].
             foo() -> bar">>,

          <<"L = [1,2,3],
             Err = {error,modul,err},
             H = qlc:q([X || X <- qlc_SUITE:table_error(L, Err)]),
             H1 = qlc:q([X || X <- H], unique),
             Err = qlc:e(H1)">>,

          <<"Err = {error, module, err},
             L = [1,2,3],
             H1 = qlc:q([{X} || X <- qlc_SUITE:table_error(L, Err)]),
             H = qlc:q([{X,Y,Z} || X <- H1, Y <- H1, Z <- L], cache),
             qlc:e(H, cache_all)">>,

          <<"Err = {error, module, err},
             L = [1,2,3],
             H1 = qlc:q([X || X <- qlc_SUITE:table_error(L, Err)]),
             H = qlc:q([{X,Y,Z} || X <- H1, Y <- H1, Z <- L], cache),
             qlc:e(H, [cache_all,unique_all])">>,

          <<"L = [{1},{2},{3}],
             H = qlc:q([X || {X} <- qlc_SUITE:table_lookup_error(L), 
                             X =:= 2]),
             {error, lookup, failed} = qlc:e(H)">>,

          %% The traverse function can return any value, but it must not
          %% return an improper list. Improper lists must not be given anyway.
          <<"{'EXIT', {{badfun,a},_}} =
             (catch qlc:e(qlc:q([{X} || X <- [1 | a], begin true end])))">>

       ],
    
    run(Config, Ts),

    Ts2 = [<<"Q = qlc:q([{X,Y} || {X} <- [{1},{2},{3}],
                                  begin
                                      %% Used to generate a badly formed file
                                      Y = 3, true
                                  end,
                                  X =:= Y]),
              [{3,3}] = qlc:e(Q)">>],
    run(Config, Ts2),

    ok.

%% OTP-6038. Bug fixes: unique and keysort; cache.
otp_6038_bug(Config) when is_list(Config) ->
    %% The 'unique' option can no longer be merged with the keysort options.
    %% This used to return [{1,a},{1,c},{2,b},{2,d}], but since 
    %% file_sorter:keysort now removes duplicates based on keys, the
    %% correct return value is [{1,a},{2,b}].
    Ts = [<<"H1 = qlc:q([X || X <- [{1,a},{2,b},{1,c},{2,d}]], unique),
             H2 = qlc:keysort(1, H1, [{unique,true}]),
             [{1,a},{2,b}] = qlc:e(H2)">>],

    run(Config, Ts),
    
    %% Sometimes the cache options did not empty the correct tables.
    CTs = [
       <<"Options = [cache,unique],
          V1 = qlc:q([{X,Y} || X <- [1,2], Y <- [3]], Options),
          V2 = qlc:q([{X,Y} || X <- [a,b], Y <- V1]),
          V3 = qlc:q([{X,Y} || X <- [5,6], Y <- [7]], Options),
          Q = qlc:q([{X,Y} || X <- V2, Y <- V3]),
          R = qlc:e(Q),
          L1 = [{X,Y} || X <- [1,2], Y <- [3]],
          L2 = [{X,Y} || X <- [a,b], Y <- L1],
          L3 = [{X,Y} || X <- [5,6], Y <- [7]],
          L = [{X,Y} || X <- L2, Y <- L3],
          true = R =:= L">>,
       <<"Options = [cache,unique],
          V1 = qlc:q([{X,Y} || X <- [1,2], Y <- [3]], Options),
          V2 = qlc:q([{X,Y} || X <- [a,b], Y <- V1]),
          V3 = qlc:q([{X,Y} || X <- [5,6], Y <- [7]], Options),
          V4 = qlc:q([{X,Y} || X <- V2, Y <- V3], Options),
          Q = qlc:q([{X,Y} || X <- [1,2], Y <- V4]),
          R = qlc:e(Q),
          L1 = [{X,Y} || X <- [1,2], Y <- [3]],
          L2 = [{X,Y} || X <- [a,b], Y <- L1],
          L3 = [{X,Y} || X <- [5,6], Y <- [7]],
          L4 = [{X,Y} || X <- L2, Y <- L3],
          L = [{X,Y} || X <- [1,2], Y <- L4],
          true = R =:= L">>
       ],
    run(Config, CTs),

    ok.

%% OTP-6359. dets:select() never returns the empty list.
otp_6359(Config) when is_list(Config) ->
    dets:start(),
    T = luna,
    Fname = filename(T, Config),

    Ts = [
       [<<"T = luna, Fname = \"">>, Fname, <<"\",
           {ok, _} = dets:open_file(T, [{file,Fname}]),
           Q = qlc:q([F || 
                         F <- dets:table(T), 
                         (F band ((1 bsl 0)) =/= 0), 
                         true]),
           [] = qlc:eval(Q),
           ok = dets:close(T),
           file:delete(\"">>, Fname, <<"\"),
           ok">>]
    ],

    run(Config, Ts),
    ok.

%% OTP-6562. compressed = false (should be []) when sorting before join.
otp_6562(Config) when is_list(Config) ->
    Bug = [
      %% This example uses a file to sort E2 on the second column. It is
      %% not easy to verify that this happens; the file_sorter module's
      %% size option cannot be set in this case. But it is not likely
      %% that the default size (512 KiB) will ever change, so it should
      %% be future safe.
      <<"E1 = create_ets(1, 10),
         E2 = create_ets(5, 150000),
         Q = qlc:q([{XX,YY} ||
                       {X,XX} <- ets:table(E1),
                       {YY,Y} <- ets:table(E2),
                       X == Y],
                   {join,merge}),
         [{5,5},{6,6},{7,7},{8,8},{9,9},{10,10}] = qlc:e(Q),
         ets:delete(E1),
         ets:delete(E2)">>
    ],
    run(Config, Bug),

    Bits = [
       {otp_6562_1,
        <<"otp_6562_1() ->
               Q = qlc:q([X || <<X:8>> <= <<\"hej\">>]),
               qlc:info(Q).
        ">>,
        [],
        {errors,[{2,qlc,binary_generator}],
         []}}
       ],
    [] = compile(Config, Bits),

    R1 = {error,qlc,{1,qlc,binary_generator}}
             = qlc:string_to_handle("[X || <<X:8>> <= <<\"hej\">>]."),
    "1: cannot handle binary generators\n" =
             lists:flatten(qlc:format_error(R1)),

    ok.

%% OTP-6590. Bug fix (join info).
otp_6590(Config) when is_list(Config) ->
    Ts = [<<"fun(Tab1Value) -> 
                    Q = qlc:q([T1#tab1.id || T1 <- [#tab1{id = id1,
                                                          value = v, 
                                                          tab2_id = id}],
                                             T2 <- [#tab2{id = id}],
                                             T1#tab1.value =:= Tab1Value,
                                             T1#tab1.tab2_id =:= T2#tab2.id]),
                    [id1] = qlc:e(Q)
            end(v)">>],

    run(Config, <<"-record(tab1, {id, tab2_id, value}).
                         -record(tab2, {id, value}).\n">>, Ts),
    ok.

%% OTP-6673. Optimizations and fixes.
otp_6673(Config) when is_list(Config) ->
    Ts_PT = 
        [<<"etsc(fun(E1) ->
                etsc(fun(E2) ->
                       Q = qlc:q([{A,B,C,D} || 
                                     {A,B} <- ets:table(E1),
                                     {C,D} <- ets:table(E2),
                                     A =:= 2, % lookup
                                     B =:= D, % join
                                     C =:= g]), % lookup invalidated by join
                       {qlc,_,[{generate,_,
                                {qlc,_,
                                 [{generate,_,
                                   {qlc,_,[{generate,_,
                                            {keysort,
                                             {list,{table,_},
                                              [{{'$1','$2'},[],['$_']}]},
                                             2,[]}},_],[]}},
                                  {generate,_,{qlc,_,
                                    [{generate,_,
                                      {keysort,{table,_},2,[]}}],
                                    []}},_],
                                 [{join,merge}]}},_,_],[]} = i(Q),
                       [{2,y,g,y}] = qlc:e(Q)
                     end, [{f,x},{g,y},{h,z}])
                 end, 
                 [{1,x},{2,y},{3,z}])">>,
         <<"etsc(fun(E1) ->
                etsc(fun(E2) ->
                       Q = qlc:q([{A,B,C,D} || 
                                     {A,B} <- ets:table(E1),
                                     {C,D} <- ets:table(E2),
                                     A =:= 2, % lookup
                                     C =:= g, % lookup
                                     B =:= D]), % join
                       {qlc,_,[{generate,_,
                                {qlc,_,
                                 [{generate,_,
                                   {qlc,_,[{generate,_,
                                            {keysort,
                                             {list,{table,_},
                                              [{{'$1','$2'},[],['$_']}]},
                                             2,[]}},_],[]}},
                                  {generate,_,{qlc,_,
                                               [{generate,_,
                                                 {keysort,
                                                  {list,{table,_},
                                                   [{{'$1','$2'},[],['$_']}]},
                                                  2,[]}},_],[]}},_],
                                 [{join,merge}]}},_],[]} = i(Q),
                       [{2,y,g,y}] = qlc:e(Q)
                     end, [{f,x},{g,y},{h,z}])
                 end, 
                 [{1,x},{2,y},{3,z}])">>],

    run(Config, Ts_PT),

    MS = ets:fun2ms(fun({X,_Y}=T) when X > 1 -> T end),
    Ts_RT = [
        [<<"%% Explicit match-spec. ets:table() ensures there is no lookup
            %% function, which means that lookup join will not be considered.
            MS = ">>, io_lib:format("~w", [MS]), <<",
            etsc(fun(E) ->
                         F = fun(J) ->
                                   qlc:q([{X,W} ||
                                             {X,_Y} <- 
                                                 ets:table(E,{traverse,
                                                              {select,MS}}),
                                             {Z,W} <- [{1,1},{2,2},{3,3}],
                                             X =:= Z], {join,J})
                             end,
                         Qm = F(any),
                         [{2,2},{3,3}] = qlc:e(Qm),
                         {'EXIT',{cannot_carry_out_join,_}} = 
                             (catch qlc:e(F(lookup)))
                 end, [{1,a},{2,b},{3,c}])">>],

         <<"%% The filter 'A =< y' can be evaluated by traversing E1 using a
            %% match specification, but then lookup join cannot use E1 for
            %% looking up keys. This example shows that the filter is kept if
            %% lookup join is employed (otherwise it is optimized away since
            %% the match spec is used).
            etsc(fun(E1) ->
                         Q = qlc:q([{A,B,C,D} || 
                                       {A,B} <- ets:table(E1),
                                       {C,D} <- [{x,f},{y,g},{z,h}],
                                       A =< y, % kept
                                       A =:= C], {join,lookup}),
                         [{x,1,x,f},{y,2,y,g}] = lists:sort(qlc:e(Q))
                 end, [{x,1},{y,2},{z,3}])">>

    ],
    run(Config, Ts_RT),

    ok.

%% OTP-6964. New option 'tmpdir_usage'.
otp_6964(Config) when is_list(Config) ->
    T1 = [
       <<"Q1 = qlc:q([{X} || X <- [1,2]]),
          {'EXIT', {badarg,_}} = (catch qlc:e(Q1, {tmpdir_usage,bad})),
          %% merge join
          F = fun(Use) ->
                      L1 = [{Y,a} || Y <- lists:seq(1, 2)],
                      L2 = [{a,Y} || Y <- lists:seq(1, 10000)],
                      Q = qlc:q([{XX,YY} ||
                                    {XX,X} <- L1,
                                    {Y,YY} <- L2,
                                    X == Y],
                                {join,merge}),
                      qlc:e(Q, [{max_list_size,64*1024},{tmpdir_usage,Use}])
              end,
          D = erlang:system_flag(backtrace_depth, 0),
      try
          20000 = length(F(allowed)),
          ErrReply = F(not_allowed),
          {error, qlc, {tmpdir_usage,joining}} = ErrReply,
          \"temporary file was needed for joining\n\" = 
              lists:flatten(qlc:format_error(ErrReply)),
          qlc_SUITE:install_error_logger(),
          20000 = length(F(warning_msg)),
          {warning, joining} = qlc_SUITE:read_error_logger(),
          20000 = length(F(info_msg)),
          {info, joining} = qlc_SUITE:read_error_logger(),
          20000 = length(F(error_msg)),
          {error, joining} = qlc_SUITE:read_error_logger()
      after
          _ = erlang:system_flag(backtrace_depth, D)
      end,
          qlc_SUITE:uninstall_error_logger()">>],
    run(Config, T1),

    T2 = [
       <<"%% File sorter.
          T = lists:seq(1, 10000),
          Q0 = qlc:q([{X} || X <- [T,T,T], begin X > 0 end], 
                     [{cache,list},unique]),
          Q1 = qlc:q([{X,Y,Z} ||
                         X <- Q0,
                         Y <- Q0,
                         Z <- Q0],
                     [{cache,list},unique]),
          Q = qlc:q([{X, Y} || Y <- [1], X <- Q1]),
          F = fun(Use) ->
                      qlc:e(Q, [{max_list_size,10000},{tmpdir_usage,Use}])
              end,
          1 = length(F(allowed)),
          ErrReply = F(not_allowed),
          {error, qlc, {tmpdir_usage,caching}} = ErrReply,
          \"temporary file was needed for caching\n\" = 
              lists:flatten(qlc:format_error(ErrReply)),
          qlc_SUITE:install_error_logger(),
          1 = length(F(error_msg)),
          {error, caching} = qlc_SUITE:read_error_logger(),
          {error, caching} = qlc_SUITE:read_error_logger(),
          1 = length(F(warning_msg)),
          {warning, caching} = qlc_SUITE:read_error_logger(),
          {warning, caching} = qlc_SUITE:read_error_logger(),
          1 = length(F(info_msg)),
          {info, caching} = qlc_SUITE:read_error_logger(),
          {info, caching} = qlc_SUITE:read_error_logger(),
          qlc_SUITE:uninstall_error_logger()">>],

    run(Config, T2),

    T3 = [
       <<"%% sort/keysort
          E1 = create_ets(1, 10),
          E2 = create_ets(5, 50000),
          Q = qlc:q([{XX,YY} ||
                        {X,XX} <- ets:table(E1),
                        {YY,Y} <- ets:table(E2),
                        X == Y],
                    {join,merge}),
          F = fun(Use) ->
                      qlc:e(Q, {tmpdir_usage,Use})
              end,
          ErrReply = F(not_allowed),
          {error,qlc,{tmpdir_usage,sorting}} = ErrReply,
          \"temporary file was needed for sorting\n\" = 
              lists:flatten(qlc:format_error(ErrReply)),
          qlc_SUITE:install_error_logger(),
          L = [{5,5},{6,6},{7,7},{8,8},{9,9},{10,10}],
          L = F(allowed),
          L = F(error_msg),
          {error, sorting} = qlc_SUITE:read_error_logger(),
          L = F(info_msg),
          {info, sorting} = qlc_SUITE:read_error_logger(),
          L = F(warning_msg),
          {warning, sorting} = qlc_SUITE:read_error_logger(),
          qlc_SUITE:uninstall_error_logger(),
          ets:delete(E1),
          ets:delete(E2)">>],
    run(Config, T3),

    T4 = [
       <<"%% cache list
          etsc(fun(E) ->
                       Q0 = qlc:q([X || X <- ets:table(E),
                                        begin element(1, X) > 5 end],
                                  {cache,list}),
                       Q = qlc:q([{X, element(1,Y)} || X <- lists:seq(1, 5),
                                                       Y <- Q0]),
                       R = [{X,Y} || X <- lists:seq(1, 5), 
                                     Y <- lists:seq(6, 10)],
                       F = fun(Use) ->
                                   qlc:e(Q, [{max_list_size, 100*1024},
                                             {tmpdir_usage, Use}])
                           end,
                       R = lists:sort(F(allowed)),
                       qlc_SUITE:install_error_logger(),
                       R = lists:sort(F(info_msg)),
                       {info, caching} = qlc_SUITE:read_error_logger(),
                       R = lists:sort(F(error_msg)),
                       {error, caching} = qlc_SUITE:read_error_logger(),
                       R = lists:sort(F(warning_msg)),
                       {warning, caching} = qlc_SUITE:read_error_logger(),
                       qlc_SUITE:uninstall_error_logger(),
                       ErrReply = F(not_allowed),
                       {error,qlc,{tmpdir_usage,caching}} = ErrReply,
                       \"temporary file was needed for caching\n\" = 
                           lists:flatten(qlc:format_error(ErrReply))
               end, [{keypos,1}], [{I,a,lists:duplicate(100000,1)} || 
                                       I <- lists:seq(1, 10)])">>],
    run(Config, T4),
    ok.

%% OTP-7238. info-option 'depth', &c.
otp_7238(Config) when is_list(Config) ->
    dets:start(),
    T = otp_7238, 
    Fname = filename(T, Config),

    ok = compile_gb_table(Config),

    %% A few more warnings.
    T1 = [
       %% The same error message string, but with different tags
       %% (the strings are not compared :-(
       {nomatch_1, 
        <<"nomatch_1() ->
               {qlc:q([X || X={X} <- []]), [t || \"a\"=\"b\" <- []]}.">>,
        [],
        %% {warnings,[{{2,30},qlc,nomatch_pattern},
        %%            {{2,44},v3_core,nomatch}]}},
        {warnings,[{2,v3_core,nomatch}]}},

       %% Not found by qlc...
       {nomatch_2,
        <<"nomatch_2() ->
               qlc:q([t || {\"a\"++\"b\"} = {\"ac\"} <- []]).">>,
        [],
        {warnings,[{{2,22},v3_core,nomatch}]}},

       {nomatch_3,
        <<"nomatch_3() ->
               qlc:q([t || [$a, $b] = \"ba\" <- []]).">>,
        [],
        %% {warnings,[{{2,37},qlc,nomatch_pattern}]}},
        {warnings,[{2,v3_core,nomatch}]}},

       %% Not found by qlc...
       {nomatch_4,
        <<"nomatch_4() ->
               qlc:q([t || \"a\"++_=\"b\" <- []]).">>,
        [],
        {warnings,[{{2,22},v3_core,nomatch}]}},

       %% Found neither by the compiler nor by qlc...
       {nomatch_5,
        <<"nomatch_5() ->
               qlc:q([X || X = <<X>> <- [3]]).">>,
        [],
        []},

       {nomatch_6,
        <<"nomatch_6() ->
               qlc:q([X || X <- [],
                           X =:= {X}]).">>,
        [],
        %% {warnings,[{{3,30},qlc,nomatch_filter}]}},
        []},

       {nomatch_7,
        <<"nomatch_7() ->
               qlc:q([X || {X=Y,{Y}=X} <- []]).">>,
        [],
        %% {warnings,[{{2,28},qlc,nomatch_pattern}]}},
        []},

       {nomatch_8,
        <<"nomatch_8() ->
               qlc:q([X || {X={},X=[]} <- []]).">>,
        [],
        %% {warnings,[{{2,28},qlc,nomatch_pattern}]}},
        []},

       {nomatch_9,
        <<"nomatch_9() ->
               qlc:q([X || X <- [], X =:= {}, X =:= []]).">>,
        [],
        %% {warnings,[{{2,49},qlc,nomatch_filter}]}},
        []},

       {nomatch_10,
        <<"nomatch_10() ->
               qlc:q([X || X <- [],
                           ((X =:= 1) or (X =:= 2)) and (X =:= 3)]).">>,
        [],
        %% {warnings,[{{3,53},qlc,nomatch_filter}]}},
        []},

       {nomatch_11,
        <<"nomatch_11() ->
               qlc:q([X || X <- [], x =:= []]).">>,
        [],
        %% {warnings,[{{2,39},qlc,nomatch_filter}]}},
        {warnings,[{2,sys_core_fold,nomatch_guard}]}},

       {nomatch_12,
        <<"nomatch_12() ->
               qlc:q([X || X={} <- [], X =:= []]).">>,
        [],
        %% {warnings,[{{2,42},qlc,nomatch_filter}]}},
        []},

       {nomatch_13,
        <<"nomatch_13() ->
               qlc:q([Z || Z <- [], 
                           X={X} <- [], 
                           Y={Y} <- []]).">>,
        [],
        %% {warnings,[{{3,29},qlc,nomatch_pattern},
        %%            {{4,29},qlc,nomatch_pattern}]}},
        []},

       {nomatch_14,
        <<"nomatch_14() ->
               qlc:q([X || X={X} <- [],
                           1 > 0,
                           1 > X]).">>,
        [],
        %% {warnings,[{{2,29},qlc,nomatch_pattern}]}},
        []},

       {nomatch_15,
        <<"nomatch_15() ->
              qlc:q([{X,Y} || X={X} <- [1],
                              Y <- [1],
                              1 > 0,
                              1 > X]).">>,
        [],
        %% {warnings,[{{2,32},qlc,nomatch_pattern}]}},
        []},

       %% Template warning.
       {nomatch_template1,
        <<"nomatch_template1() ->
               qlc:q([{X} = {} || X <- []]).">>,
        [],
        {warnings,[{2,sys_core_fold,no_clause_match}]}}
         ],
    [] = compile(Config, T1),

    %% 'depth' is a new option used by info()
    T2 = [
       %% Firstly: lists
       <<"L = [[a,b,c],{a,b,c},[],<<\"foobar\">>],
          Q = qlc:q([{X} || X <- L]),
          {call, _,
           {remote,_,{atom,_,ets},{atom,_,match_spec_run}},
           [{cons,_,{atom,_,'...'},
             {cons,_,{atom,_,'...'},
              {cons,_,{nil,_},{cons,_,{atom,_,'...'},{nil,_}}}}},
            _]} = qlc:info(Q, [{format,abstract_code},{depth,0}]),

          {call,_,_,
           [{cons,_,{cons,_,{atom,_,'...'},{nil,_}},
             {cons,_,
              {tuple,_,[{atom,_,'...'}]},
              {cons,_,{nil,_},
               {cons,_,
                {bin,_,
                 [{_,_,{_,_,$.},_,_},
                  {_,_,{_,_,$.},_,_},
                  {_,_,{_,_,$.},_,_}]},
                {nil,_}}}}},
            _]} = qlc:info(Q, [{format,abstract_code},{depth,1}]),

          {call,_,
           _,
          [{cons,_,{cons,_,{atom,_,a},{atom,_,'...'}},
            {cons,_,
             {tuple,_,[{atom,_,a},{atom,_,'...'}]},
             {cons,_,{nil,_},
              {cons,_,
               {bin,_,
                [{_,_,{_,_,$f},_,_},
                 {_,_,{_,_,$.},_,_},
                 {_,_,{_,_,$.},_,_},
                 {_,_,{_,_,$.},_,_}]},
               {nil,_}}}}},
          _]} = qlc:info(Q, [{format,abstract_code},{depth,2}]),

          {call,_,_,
           [{cons,_,
             {cons,_,{atom,_,a},{cons,_,{atom,_,b},{atom,_,'...'}}},
             {cons,_,
              {tuple,_,[{atom,_,a},{atom,_,b},{atom,_,'...'}]},
              {cons,_,{nil,_},
               {cons,_,
                {bin,_,
                 [{_,_,{_,_,$f},_,_},
                  {_,_,{_,_,$o},_,_},_,_,_]},
                {nil,_}}}}},
            _]} = qlc:info(Q, [{format,abstract_code},{depth,3}]),

          {call,_,_,
           [{cons,_,
             {cons,_,
              {atom,_,a},{cons,_,{atom,_,b},{cons,_,{atom,_,c},{nil,_}}}},
             {cons,_,
              {tuple,_,[{atom,_,a},{atom,_,b},{atom,_,c}]},
              {cons,_,{nil,_},
               {cons,_,
                {bin,_,
                 [{_,_,{_,_,$f},_,_},
                  {_,_,{_,_,$o},_,_},
                  {_,_,{_,_,$o},_,_},
                  {_,_,{_,_,$b},_,_},
                  {_,_,{_,_,$a},_,_},
                  {_,_,{_,_,$r},_,_}]},
                {nil,_}}}}},
            _]} = qlc:info(Q, [{format,abstract_code},{depth,10}]),

          {call,_,_,
           [{cons,_,
             {cons,_,
              {atom,_,a},{cons,_,{atom,_,b},{cons,_,{atom,_,c},{nil,_}}}},
             {cons,_,
              {tuple,_,[{atom,_,a},{atom,_,b},{atom,_,c}]},
              {cons,_,{nil,_},
               {cons,_,
                {bin,_,
                 [{_,_,{_,_,$f},_,_},
                  {_,_,{_,_,$o},_,_},
                  {_,_,{_,_,$o},_,_},
                  {_,_,{_,_,$b},_,_},
                  {_,_,{_,_,$a},_,_},
                  {_,_,{_,_,$r},_,_}]},
                {nil,_}}}}},
            _]} = qlc:info(Q, [{format,abstract_code},{depth,infinity}])">>,
       
       %% Secondly: looked up keys
       <<"F = fun(D) ->
                etsc(fun(E) ->
                       Q = qlc:q([C || {N,C} <- ets:table(E), 
                                       (N =:= {2,2}) or (N =:= {3,3})]),
                       F = qlc:info(Q, [{format,abstract_code},{depth,D}]),
                       {call,_,_,[{call,_,_,[_Fun,Values]},_]} = F,
                       [b,c] = lists:sort(qlc:eval(Q)),
                       Values
                     end, [{{1,1},a},{{2,2},b},{{3,3},c},{{4,4},d}])
              end,

          [{cons,_,{atom,_,'...'},{cons,_,{atom,_,'...'},{nil,_}}},
           {cons,_,
            {tuple,_,[{atom,_,'...'}]},
            {cons,_,{tuple,_,[{atom,_,'...'}]},{nil,_}}},
           {cons,_,
            {tuple,_,[{integer,_,2},{atom,_,'...'}]},
            {cons,_,{tuple,_,[{integer,_,3},{atom,_,'...'}]},{nil,_}}},
           {cons,_,
            {tuple,_,[{integer,_,2},{integer,_,2}]},
            {cons,_,{tuple,_,[{integer,_,3},{integer,_,3}]},{nil,_}}},
           {cons,_,
            {tuple,_,[{integer,_,2},{integer,_,2}]},
            {cons,_,{tuple,_,[{integer,_,3},{integer,_,3}]},{nil,_}}}] =
              lists:map(F, [0,1,2,3,infinity])">>,
       [<<"T = otp_7238, Fname = \"">>, Fname, <<"\",
           {ok, _} = dets:open_file(T, [{file,Fname}]),
           ok = dets:insert(T, [{{1,1},a},{{2,2},b},{{3,3},c},{{4,4},d}]),
           Q = qlc:q([C || {N,C} <- dets:table(T), 
                           (N =:= {2,2}) or (N =:= {3,3})]),
           F = qlc:info(Q, [{format,abstract_code},{depth,1}]),
           [b,c] = lists:sort(qlc:eval(Q)),
           {call,_,_,
            [{call,_,_,
              [_,
               {cons,_,
                {tuple,_,[{atom,_,'...'}]},
                {cons,_,{tuple,_,[{atom,_,'...'}]},{nil,_}}}]},
             _]} = F,
           ok = dets:close(T),
           file:delete(\"">>, Fname, <<"\")">>],

       %% Thirdly: format_fun has been extended (in particular: gb_table)
       <<"T = gb_trees:from_orddict([{{1,a},w},{{2,b},v},{{3,c},u}]),
          QH = qlc:q([X || {{X,Y},_} <- gb_table:table(T),
                           ((X =:= 1) or (X =:= 2)),
                           ((Y =:= a) or (Y =:= b) or (Y =:= c))]),
          {call,_,_,
           [{call,_,_,
             [{'fun',_,
               {clauses,
                [{clause,_,_,[],
                  [{'case',_,
                    {call,_,_,
                     [_,
                      {call,_,_,
                       [{cons,_,
                         {tuple,_,[{atom,_,'...'}]},
                         {cons,_,
                          {tuple,_,[{atom,_,'...'}]},
                          {cons,_,{tuple,_,[{atom,_,'...'}]},{nil,_}}}}]}]},
                    [_,_]}]}]}},
              {cons,_,
               {tuple,_,[{atom,_,'...'}]},
               {cons,_,
                {tuple,_,[{atom,_,'...'}]},
                {cons,_,
                 {tuple,_,[{atom,_,'...'}]},
                 {cons,_,
                  {tuple,_,[{atom,_,'...'}]},
                  {cons,_,
                   {tuple,_,[{atom,_,'...'}]},
                   {cons,_,{tuple,_,[{atom,_,'...'}]},{nil,_}}}}}}}]},
            {call,_,_,
             [{cons,_,{tuple,_,[{atom,_,'...'}]},{nil,_}}]}]} = 
            qlc:info(QH, [{format,abstract_code},{depth,1}])">>,
       <<"T1 = [{1,1,a},{2,2,b},{3,3,c},{4,4,d}],
          T2 = [{x,1},{y,1},{z,2}],
          QH1 = T1,
          T = gb_trees:from_orddict(T2),
          QH2 = qlc:q([X || {_,X} <- gb_table:table(T)], cache),
          Q = qlc:q([{X1,X2,X3} || {X1,X2,X3} <- QH1, 
                                   Y2 <- QH2, 
                                   X2 =:= Y2]),
          {block,_,
           [{match,_,_,
             {call,_,_,
              [{lc,_,_,
                [{generate,_,_,
                  {call,_,_,
                   [{call,_,_,
                     [{cons,_,
                       {tuple,_,[{atom,_,'...'}]},
                       {atom,_,'...'}}]}]}}]},
               _]}},
            {call,_,_,
             [{lc,_,_,
               [{generate,_,_,
                 {cons,_,{tuple,_,[{atom,_,'...'}]},{atom,_,'...'}}},
                _,_]}]}]} = 
              qlc:info(Q, [{format,abstract_code},{depth, 1},
                           {n_elements,1}])">>,
       <<"L = [{{key,1},a},{{key,2},b},{{key,3},c}],
          T = gb_trees:from_orddict(orddict:from_list(L)),
          Q = qlc:q([K || {K,_} <- gb_table:table(T), 
                                   (K =:= {key,1}) or (K =:= {key,2})]),
{call,_,_,
 [{call,_,_,
   [{'fun',_,
     {clauses,
      [{clause,_,_,[],
        [{'case',_,
          {call,_,_,
           [_,
            {call,_,_,
             [{cons,_,
               {tuple,_,[{tuple,_,[{atom,_,'...'}]},{atom,_,'...'}]},
               {cons,_,
                {tuple,_,[{tuple,_,[{atom,_,'...'}]},{atom,_,'...'}]},
                {cons,_,
                 {tuple,_,[{tuple,_,[{atom,_,'...'}]},{atom,_,'...'}]},
                 {nil,_}}}}]}]},
          _}]}]}},
    {cons,_,
     {tuple,_,[{atom,_,key},{atom,_,'...'}]},
     {cons,_,{tuple,_,[{atom,_,key},{atom,_,'...'}]},{nil,_}}}]},
  {call,_,
   {remote,_,{atom,_,ets},{atom,_,match_spec_compile}},
   [{cons,_,
     {tuple,_,[{tuple,_,[{atom,_,'...'}]},{atom,_,'...'}]},
     {nil,_}}]}]} = 
          qlc:info(Q, [{format,abstract_code},{depth, 2}])">>

         ],
    run(Config, T2),

    T3 = [
%%        {nomatch_6,
%%         <<"nomatch_6() ->
%%                qlc:q([X || X <- [],
%%                            X =:= {X}]).">>,
%%         [],
%%         {[],["filter evaluates to 'false'"]}},

%%        {nomatch_7,
%%         <<"nomatch_7() ->
%%                qlc:q([X || {X=Y,{Y}=X} <- []]).">>,
%%         [],
%%         {[],["pattern cannot possibly match"]}}
       ],
    compile_format(Config, T3),

    %% *Very* simple test - just check that it doesn't crash.
    Type = [{cres,
             <<"Q = qlc:q([X || {X} <- []]),
                {'EXIT',{{badfun,_},_}} = (catch qlc:e(Q))">>,
             [type_checker],
             []}],
    run(Config, Type),

    ok.
    
%% OTP-7114. Match spec, table and duplicated objects...
otp_7114(Config) when is_list(Config) ->
    Ts = [<<"T = ets:new(t, [bag]),
             [ets:insert(T, {t, I, I div 2}) || I <- lists:seq(1,10)],
             Q1 = qlc:q([element(3, E) || E <- ets:table(T)]),
             [0,1,1,2,2,3,3,4,4,5] = lists:sort(qlc:e(Q1)),
             [0,1,2,3,4,5] = qlc:e(Q1, unique_all),
             [0,1,2,3,4,5] = qlc:e(qlc:sort(Q1), unique_all),
             [0,1,2,3,4,5] = qlc:e(qlc:sort(qlc:e(Q1)), unique_all),
             ets:delete(T),
             ok">>],
    run(Config, Ts).

%% OTP-7232. qlc:info() bug (pids, ports, refs, funs).
otp_7232(Config) when is_list(Config) ->
    Ts = [<<"L = [fun math:sqrt/1, list_to_pid(\"<0.4.1>\"),
                  erlang:make_ref()],
             \"[fun math:sqrt/1,<0.4.1>,#Ref<\" ++ _  = qlc:info(L),
             {call,_,
               {remote,_,{atom,_,qlc},{atom,_,sort}},
               [{cons,_,
                      {'fun',_,{function,{atom,_,math},{atom,_,sqrt},_}},
                      {cons,_,
                            {string,_,\"<0.4.1>\"}, % could use list_to_pid..
                            {cons,_,{string,_,\"#Ref<\"++_},{nil,_}}}},
                {nil,_}]} = 
              qlc:info(qlc:sort(L),{format,abstract_code})">>,

          <<"Q1 = qlc:q([X || X <- [55296,56296]]),
             Q = qlc:sort(Q1, {order, fun(A,B)-> A>B end}),
             \"qlc:sort([55296,56296],[{order,fun'-function/0-fun-2-'/2}])\" =
                format_info(Q, true),
             AC = qlc:info(Q, {format, abstract_code}),
             \"qlc:sort([55296,56296], [{order,fun '-function/0-fun-2-'/2}])\" =
                binary_to_list(iolist_to_binary(erl_pp:expr(AC)))">>,

         %% OTP-7234. erl_parse:abstract() handles bit strings
          <<"Q = qlc:sort([<<17:9>>]),
             \"[<<8,1:1>>]\" = qlc:info(Q)">>

         ],
    run(Config, Ts).

%% OTP-7552. Merge join bug.
otp_7552(Config) when is_list(Config) ->
    %% The poor performance cannot be observed unless the 
    %% (redundant) join filter is skipped. 
    Ts = [<<"Few = lists:seq(1, 2),
             Many = lists:seq(1, 10),
             S = [d,e],
             L1 = [{Y,a} || Y <- Few] ++ [{'1b',b},{2,b}] ++ 
                    [{Y,X} || X <- S, Y <- Few],
             L2 = [{a,Y} || Y <- Many] ++ 
                    [{b,'1b'}] ++ [{c,1}] ++ 
                    [{X,Y} || X <- S, Y <- Many],
                   F = fun(J) ->
                               qlc:q([{XX,YY} ||
                                         {XX,X} <- L1,
                                         {Y,YY} <- L2,
                                         X == Y],
                                     {join,J})
                       end,
                   Qm = F(merge),
                   Qn = F(nested_loop),
                   true = lists:sort(qlc:e(Qm, {max_list_size,20})) =:= 
                          lists:sort(qlc:e(Qn))">>],
    run(Config, Ts).

%% OTP-7714. Merge join bug.
otp_7714(Config) when is_list(Config) ->
    %% The original example uses Mnesia. This one does not.
    Ts = [<<"E1 = ets:new(set,[]),
             true = ets:insert(E1, {a,1}),
             E2 = ets:new(set,[]),
             _ = [true = ets:insert(E2, {I, 1}) ||
                     I <- lists:seq(1, 3)],
             Q = qlc:q([{A,B} || 
                           {A,I1} <- ets:table(E1),
                           {B,I2} <- ets:table(E2),
                           I1 =:= I2],{join,merge}),
             [{a,1},{a,2},{a,3}] = lists:sort(qlc:e(Q)),
             ets:delete(E1),
             ets:delete(E2)">>],
    run(Config, Ts).

%% OTP-11758. Bug.
otp_11758(Config) when is_list(Config) ->
    Ts = [<<"T = ets:new(r, [{keypos, 2}]),
             L = [{rrr, xxx, aaa}, {rrr, yyy, bbb}],
             true = ets:insert(T, L),
             QH = qlc:q([{rrr, B, C} || {rrr, B, C} <- ets:table(T),
                              (B =:= xxx) or (B =:= yyy) and (C =:= aaa)]),
             [{rrr,xxx,aaa}] = qlc:e(QH),
             ets:delete(T)">>],
    run(Config, Ts).

%% OTP-6674. match/comparison.
otp_6674(Config) when is_list(Config) ->

    ok = compile_gb_table(Config),

    Ts = [%% lookup join
          <<"E = ets:new(join, [ordered_set]),
             true = ets:insert(E, [{1,a},{2,b},{3,c}]),
             Q = qlc:q([{X, Y} || {X,_} <- ets:table(E),
                                  {Y} <- [{0},{1},{2}],
                                  X == Y]),
             {0,1,0,0} = join_info(Q),
             [{1,1},{2,2}] = qlc:e(Q),
             ets:delete(E)">>,

          <<"E = ets:new(join, [ordered_set]),
             true = ets:insert(E, [{1,a},{2,b},{3,c}]),
             Q = qlc:q([{X, Y} || {X,_} <- ets:table(E),
                                  {Y} <- [{0},{1},{2}],
                                  X =:= Y]),
             {0,1,0,0} = join_info(Q),
             {block,_,
              [_,
               {match,_,_,
                 {call,_,_,
                  [{lc,_,_,
                    [_,_,{op,_,'==',_,_}]},
                   {cons,_,
                    {tuple,_,[{atom,_,join},{atom,_,lookup}]},_}]}},
               _]} = qlc:info(Q, {format, abstract_code}),
             [{1,1},{2,2}] = qlc:e(Q),
             ets:delete(E)">>,

       <<"E = ets:new(join, [set]),
          Q = qlc:q([{X, Y} || {X,_} <- ets:table(E),
                               {Y} <- [{0},{1},{2}],
                               X == Y], {join, lookup}),
          {'EXIT',{cannot_carry_out_join,_}} = (catch qlc:e(Q)),
          ets:delete(E)">>,

       %% Lookup join possible in both directions.
       <<"E1 = ets:new(join, [ordered_set]),
          E2 = ets:new(join, [set]),
          true = ets:insert(E1, [{1.0,a},{2,b},{3,c}]),
          true = ets:insert(E2, [{0},{1},{2}]),
          Q = qlc:q([{X, Y} || {X,_} <- ets:table(E1),
                               {Y} <- ets:table(E2),
                               X == Y],{join,lookup}), % skipped
          {qlc,_,
            [{generate,_,
                 {qlc,_,
                     [{generate,_,
                          {qlc,_,[{generate,_,{table,{ets,table,[_]}}}],[]}},
                      {generate,_,{table,{ets,table,[_]}}},
                      _],
                     [{join,lookup}]}}],
            []} = qlc:info(Q, {format,debug}),
          {0,1,0,0} = join_info(Q),
          [{1.0,1},{2,2}] = lists:sort(qlc:e(Q)),
          ets:delete(E1), 
          ets:delete(E2)">>,
       <<"E1 = ets:new(join, [ordered_set]),
          E2 = ets:new(join, [set]),
          true = ets:insert(E1, [{1.0,a},{2,b},{3,c}]),
          true = ets:insert(E2, [{0},{1},{2}]),
          Q = qlc:q([{X, Y} || {X,_} <- ets:table(E1),
                               {Y} <- ets:table(E2),
                               X =:= Y],{join,merge}), % not skipped
          {1,0,0,1} = join_info(Q),
          [{2,2}] = qlc:e(Q),
          ets:delete(E1), 
          ets:delete(E2)">>,
       <<"E1 = ets:new(join, [ordered_set]),
          E2 = ets:new(join, [set]),
          true = ets:insert(E1, [{1.0,a},{2,b},{3,c}]),
          true = ets:insert(E2, [{0},{1},{2}]),
          Q = qlc:q([{X, Y} || {X,_} <- ets:table(E1),
                               {Y} <- ets:table(E2),
                               X =:= Y],{join,lookup}), % skipped
          {qlc,_,
           [{generate,_,
             {qlc,_,
              [{generate,_,
                {qlc,_,
                 [{generate,_,{table,{ets,table,[_]}}}],
                 []}},
               {generate,_,{table,{ets,table,[_]}}},
               _],
              [{join,lookup}]}}],
            []} = qlc:info(Q, {format,debug}),
          {0,1,0,0} = join_info(Q),
          [{2,2}] = qlc:e(Q),
          ets:delete(E1), 
          ets:delete(E2)">>,
       <<"E1 = ets:new(join, [ordered_set]),
          E2 = ets:new(join, [set]),
          true = ets:insert(E1, [{1.0,a},{2,b},{3,c}]),
          true = ets:insert(E2, [{0},{1},{2}]),
          Q = qlc:q([{X, Y} || {X,_} <- ets:table(E1),
                               {Y} <- ets:table(E2),
                               %% Independent of term comparison:
                               X =:= Y, X == Y]),
          {0,1,0,0} = join_info(Q),
          [{2,2}] = qlc:e(Q),
          ets:delete(E1), 
          ets:delete(E2)">>,

       <<"E = ets:new(join, [ordered_set]),
          true = ets:insert(E, [{1,1},{2,2},{3,c}]),
          Q = qlc:q([{X, Y} || {X,Z} <- ets:table(E),
                               {Y} <- [{0},{1},{2}],
                               X == Z, Y == Z]), % cannot skip (yet)
          {qlc,_,
            [{generate,_,
                 {qlc,_,[_,_,_],[{join,lookup}]}},
             _,_],[]} = qlc:info(Q,{format,debug}),
          {0,1,0,0} = join_info(Q),
          [{1,1},{2,2}] = qlc:e(Q),
          ets:delete(E)">>,

       %% The following moved here from skip_filters. It was buggy!
       <<"etsc(fun(E) ->
                 A = 3,
                 Q = qlc:q([X || X <- ets:table(E),
                                 A == element(1,X)]),
                 {table, _} = i(Q),
                 case qlc:e(Q) of
                       [{3},{3.0}] -> ok;
                       [{3.0},{3}] -> ok
                 end,
                 false = lookup_keys(Q)
         end, [{3},{3.0},{c}])">>,
    <<"H1 = qlc:sort([{{192,192.0},1,a},{{192.0,192.0},2,b},{{192,192.0},3,c}]),
       Q = qlc:q([{X, Y} || {{A,B},X,_} <- H1, % does not need keysort(3)
                            A == 192, B =:= 192.0,
                            {Y} <- [{0},{1},{2}],
                            X == Y]),
       A0 = erl_anno:new(0),
       {block,A0,
         [{match,_,_,
           {call,_,_,
            [{lc,_,_,
              [_,
               %% Has to compare extra constant:
               {op,_,'==',
                {tuple,_,[{integer,_,192},{float,_,192.0}]},
                {call,_,{atom,_,element},[{integer,_,1},{var,_,'P0'}]}}]}]}},
          _,_,
          {call,_,_,
           [{lc,_,_,
             [_,
              %% The join filter has been skipped.
              {op,_,'==',{var,_,'A'},{integer,_,192}},
              {op,_,'=:=',{var,_,'B'},{float,_,192.0}}]}]}]}
      = qlc:info(Q, {format,abstract_code}),
       {1,0,0,1} = join_info(Q),
       [{1,1},{2,2}] = qlc:e(Q)">>,

    %% Does not handle more than one lookup value (conjunctive).
    <<"T = gb_trees:from_orddict([{1,a},{2,b}]),
       H = qlc:q([X || {X,_} <- gb_table:table(T),
                                X =:= 1 andalso X == 1.0]),
       false = lookup_keys(H),
       [1] = qlc:e(H)">>,

    %% EqualConstants...
    <<"etsc(fun(E) ->
                Q = qlc:q([{X,Y} || {X} <- ets:table(E), 
                                {Y} <- [{{1}},{{2}},{{1.0}},{{2.0}}],
                                X =:= {1}, X == {1.0},
                                X == Y], {join, merge}),
                [{{1},{1}},{{1},{1.0}}] = lists:sort(qlc:e(Q)),
                false = lookup_keys(Q)
         end, [{{1}}, {{2}}])">>,

    <<"T = gb_trees:from_orddict([{foo,{1}}, {bar,{2}}]),
       Q = qlc:q([{X,Y} || {_,X} <- gb_table:table(T), 
                       {Y} <- [{{1}},{{2}},{{1.0}},{{2.0}}],
                         (X =:= {1}) or (X == {2}), 
                         (X == {1.0}) or (X =:= {2.0}),
                       X == Y], {join, merge}),
       [{{1},{1}},{{1},{1.0}}] = qlc:e(Q)">>,

    %% Compare key
    <<"T = gb_trees:from_orddict([{1,a},{2,b}]),
       H = qlc:q([X || {X,_} <- gb_table:table(T),
                       X == 1]),
       [1] = lookup_keys(H),
       [1] = qlc:e(H)">>,
    <<"T = gb_trees:from_orddict([{1,a},{2,b}]),
       H = qlc:q([X || {X,_} <- gb_table:table(T),
                       X == 1.0]),
       [1.0] = lookup_keys(H), % this is how gb_table works...
       [1.0] = qlc:e(H)">>,
    <<"etsc(fun(E) ->
                   H = qlc:q([X || {X,_} <- ets:table(E), 
                                   X == 1.0]),
                   [1] = qlc:e(H), % and this is how ETS works.
                   [1.0] = lookup_keys(H)
           end, [ordered_set], [{1,a},{2,b}])">>,

    <<"T = gb_trees:from_orddict([{1,a},{2,b}]),
       H = qlc:q([X || {X,_} <- gb_table:table(T),
                       X =:= 2]),
       [2] = lookup_keys(H),
       %% Cannot (generally) remove the matching filter (the table
       %% compares the key). But note that gb_table returns the given
       %% term as key, so in this case the filter _could_ have been removed.
       %% However, there is no callback to inform qlc about that.
       {call,_,_,
             [_,{call,_,_,
               [{cons,_,{tuple,_,
                  [_,{cons,_,
                    {tuple,_,[{atom,_,'=:='},{atom,_,'$1'},{integer,_,2}]},
                    _},_]},_}]}]} =  qlc:info(H, {format,abstract_code}),
       [2] = qlc:e(H)">>,
    <<"T = gb_trees:from_orddict([{1,a},{2,b}]),
       H = qlc:q([X || {X,_} <- gb_table:table(T),
                       X =:= 2.0]),
       %% Just shows that the term (not the key) is returned.
       [2.0] = lookup_keys(H),
       [2.0] = qlc:e(H)">>,

    <<"I = 1,
       T = gb_trees:from_orddict([{1,a},{2,b}]),
       H = qlc:q([X || {X,_} <- gb_table:table(T),
                       X == I]), % imported variable
       [1] = lookup_keys(H),
       {call,_,_,
             [_,{call,_,_,
               [{cons,_,
                 {tuple,_,
                  [{tuple,_,[{atom,_,'$1'},{atom,_,'_'}]},
                   {nil,_}, % the filter has been skipped
                   {cons,_,{atom,_,'$1'},_}]},
                 _}]}]} = qlc:info(H, {format, abstract_code}),
       [1] = qlc:e(H)">>,
    <<"I = 2,
       T = gb_trees:from_orddict([{1,a},{2,b}]),
       H = qlc:q([X || {X,_} <- gb_table:table(T),
                       X =:= I]),
       [2] = lookup_keys(H),
       {call,_,_,
            [_,{call,_,_,
              [{cons,_,{tuple,_,
                 [_,{cons,_,
                   {tuple,_,
                    [{atom,_,'=:='},
                     {atom,_,'$1'},
                     {tuple,_,[{atom,_,const},{integer,_,2}]}]},
                   _},_]},
                _}]}]} = qlc:info(H, {format, abstract_code}),
          [2] = qlc:e(H)">>,

    <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X,_} <- ets:table(E),
                                 X =:= a]), % skipped
                 [a] = qlc:e(Q),
                 {list,{table,_},_} = i(Q),
                 [a] = lookup_keys(Q)
            end, [ordered_set], [{a,1},{b,2},{3,c}])">>,

    %% Does not find that if for instance X =:= {1} then the filter
    %% X == {1} can be removed.
    <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), 
                                X =:= {1}, X == {1.0}]),
                [{1}] = qlc:e(Q),
                [{1}] = lookup_keys(Q)
         end, [{{1}}, {{2}}])">>,
    <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), 
                                X =:= {1}, X == {1.0}]),
                [{1}] = qlc:e(Q),
                false = lookup_keys(Q)
         end, [ordered_set], [{{1}}, {{2}}])">>,
    <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), 
                                X == {1.0}, X =:= {1}]),
                [{1}] = qlc:e(Q),
                [{1}] = lookup_keys(Q)
         end, [{{1}}, {{2}}])">>,
    <<"etsc(fun(E) ->
                Q = qlc:q([X || {X} <- ets:table(E), 
                                X == {1.0}, X =:= {1}]),
                [{1}] = qlc:e(Q),
                false = lookup_keys(Q)
         end, [ordered_set], [{{1}}, {{2}}])">>,

    <<"E = ets:new(apa, []),
       true = ets:insert(E, [{1,a},{2,b}]),
       {'EXIT', {badarg, _}} = 
              (catch qlc_SUITE:bad_table_key_equality(E)),
       ets:delete(E)">>,

    <<"etsc(fun(E) ->
           Q = qlc:q([X || {X} <- ets:table(E), 
                               X =:= 1, X =:= is_integer(X)]),
           [] = qlc:e(Q),
           [1] = lookup_keys(Q)
       end, [{1}, {2}])">>,

    <<"etsc(fun(E) ->
                 Q = qlc:q([X || {X=1} <- ets:table(E), 
                                 X =:= is_integer(X)]),
                 {call,_,_,
                  [{lc,_,_,
                    [_,
                     {op,_,'=:=',
                      {var,_,'X'},
                      {call,_,
                       {atom,_,is_integer},
                       [{var,_,'X'}]}}]}]} = 
             qlc:info(Q, {format, abstract_code}),
             [] = qlc:e(Q),
             [1] = lookup_keys(Q)
         end, [{1}, {2}])">>,

    <<"T = gb_trees:from_orddict([{1,a},{2,b}]),
       H = qlc:q([X || {X,Y} <- gb_table:table(T),
                                Y =:= a, true, X =:= 1]),
       [1] = lookup_keys(H),
       [1] = qlc:e(H)">>,

    <<"T = gb_trees:from_orddict([{{1.0,1},a},{{2.0,2},b}]),
       H = qlc:q([X || {{1.0,B}=X,_} <- gb_table:table(T),
                       B == 1]), % skipped
       [{1.0, 1}] = qlc:e(H),
       {qlc,_,[{generate,_,{table,_}}], []} = qlc:info(H, {format,debug})">>,
    <<"T = gb_trees:from_orddict([{{1.0,1},a},{{2.0,2},b}]),
       H = qlc:q([X || {{1.0,B}=X,_} <- gb_table:table(T),
                    B == 1.0]), % skipped
       [{1.0, 1.0}] = qlc:e(H), % this is how gb_table works...
       {qlc,_,[{generate,_,{table,_}}], []} = qlc:info(H, {format,debug})">>,
    <<"T = gb_trees:from_orddict([{{1.0,1},a},{{2.0,2},b}]),
       H = qlc:q([X || {{1.0,B}=X,_} <- gb_table:table(T),
                       B =:= 1.0]), % not skipped
       [{1.0, 1.0}] = qlc:e(H),
       {qlc,_,[{generate,_,{table,_}},_], []} = qlc:info(H,{format,debug})">>,
    <<"T = gb_trees:from_orddict([{{1.0,1},a},{{2.0,2},b}]),
       H = qlc:q([X || {{1.0,B}=X,_} <- gb_table:table(T),
                       B =:= 1]), % not skipped
       [{1.0, 1}] = qlc:e(H),
       {qlc,_,[{generate,_,{table,_}},_], []} = qlc:info(H,{format,debug})">>,

    <<"%% The imported variables do not interfere with join.
       E = ets:new(join, [ordered_set]),
       {A, B} = {1,1},
       true = ets:insert(E, [{1,a},{2,b},{3,c}]),
       Q = qlc:q([{X, Y} || {X,_Z} <- ets:table(E),
                            {Y} <- [{0},{1},{2}],
                            X =:= A, Y =:= B, 
                            Y == X], % skipped
                {join, merge}),
       [{1,1}] = qlc:e(Q),
       {qlc,_,
           [{generate,_,
             {qlc,_,
              [{generate,_,
                {qlc,_,[{generate,_,{list,{table,_},_}},_],[]}},
               {generate,_,
                {qlc,_,[{generate,_,{list,_,_}},_],[]}},
               _],
              [{join,merge}]}}],
           []} = qlc:info(Q, {format, debug}),
       ets:delete(E)">>,

    <<"% An old bug: usort() should not be used when matching values
       etsc(fun(E) ->
                   I = 1,
                   H = qlc:q([X || {X,_} <- ets:table(E), 
                                   X =:= 1.0 orelse X =:= I]),
                   [1] = qlc:e(H),
                   [1.0] = lookup_keys(H) % do not look up twice
            end, [set], [{1,a},{2,b}])">>,
    <<"etsc(fun(E) ->
                   H = qlc:q([X || {X,_} <- ets:table(E), 
                                    X =:= 1.0 orelse X == 1]),
                   [1] = qlc:e(H),
                   false = lookup_keys(H) % doesn't handle this case
         end, [ordered_set], [{1,a},{2,b}])">>,

    <<"etsc(fun(E) ->
                 I1 = 1, I2 = 1,
                 H = qlc:q([X || {X,_} <- ets:table(E), 
                                 X =:= I1 orelse X == I2]),
                 [1] = qlc:e(H), % do not look up twice
                 [1] = lookup_keys(H)
         end, [ordered_set], [{1,a},{2,b}])">>,

    <<"etsc(fun(E) ->
                 I1 = 1, I2 = 1, I3 = 1,
                 H = qlc:q([X || {X,_} <- ets:table(E), 
                                 I1 == I2, I1 =:= I3, I3 == I2, I2 =:= I3,
                                 X =:= I1 orelse X == I2
                                 ]),
                 [1] = qlc:e(H),
                 [1] = lookup_keys(H)
         end, [ordered_set], [{1,a},{2,b}])">>,

    <<"E = ets:new(join, [ordered_set]),
       true = ets:insert(E, [{1,a},{2,b,x},{3,c}]),
       Q = qlc:q([P || P <- ets:table(E),
                       P =:= {1,a} orelse P =:= {2,b,x}]),
       [{1,a},{2,b,x}] = qlc:e(Q),
       ets:delete(E)">>,

    <<"etsc(fun(E) ->
                   Q = qlc:q([X || {X,Y} <- ets:table(E), 
                                   ((X =:= 3) or (Y =:= 4))  and (X == a)]),
                   {list,{table,_},_} = i(Q),
                   [] = qlc:e(Q), % a is not an answer
                   [a] = lookup_keys(Q)
          end, [{keypos,1},ordered_set], [{a,3},{b,4}])">>,

    <<"Q = qlc:q([{X,Y} ||
                  {X} <- [{<<3:4>>}],
                  {Y} <- [{<<3:4>>}],
                  X =:= <<1:3,1:1>>,        % <<3:4>>
                  Y =:= <<0:2,1:1,1:1>>,    % <<3:4>>
                  X =:= Y]),
                  [{<<3:4>>,<<3:4>>}] = qlc:e(Q)">>


    ],

    run(Config, Ts).

%% Syntax error.
otp_12946(Config) when is_list(Config) ->
    Text =
        <<"-export([init/0]).
           init() ->
               ok.
           y">>,
    {errors,[{4,erl_parse,_}],[]} = compile_file(Config, Text, []),
    ok.

%% Examples from qlc(3).
manpage(Config) when is_list(Config) ->
    dets:start(),
    ok = compile_gb_table(Config),

    Ts = [
       <<"QH = qlc:q([{X,Y} || X <- [a,b], Y <- [1,2]]),
          QC = qlc:cursor(QH),
          [{a,1}] = qlc:next_answers(QC, 1),
          [{a,2}] = qlc:next_answers(QC, 1),
          [{b,1},{b,2}] = qlc:next_answers(QC, all_remaining),
          ok = qlc:delete_cursor(QC)">>,

       <<"QH = qlc:q([{X,Y} || X <- [a,b], Y <- [1,2]]),
          [{a,1},{a,2},{b,1},{b,2}] = qlc:eval(QH)">>,

       <<"QH = [1,2,3,4,5,6],
          21 = qlc:fold(fun(X, Sum) -> X + Sum end, 0, QH)">>,

       <<"QH = qlc:q([{X,Y} || X <- [x,y], Y <- [a,b]]),
          B = \"begin\n\"
              \"    V1 =\n\"
              \"        qlc:q([ \n\"
              \"               SQV ||\n\"
              \"                   SQV <- [x,y]\n\"
              \"              ],\n\"
              \"              [{unique,true}]),\n\"
              \"    V2 =\n\"
              \"        qlc:q([ \n\"
              \"               SQV ||\n\"
              \"                   SQV <- [a,b]\n\"
              \"              ],\n\"
              \"              [{unique,true}]),\n\"
              \"    qlc:q([ \n\"
              \"           {X,Y} ||\n\"
              \"               X <- V1,\n\"
              \"               Y <- V2\n\"
              \"          ],\n\"
              \"          [{unique,true}])\n\"
              \"end\",
          true = B =:= qlc:info(QH, unique_all)">>,

       <<"E1 = ets:new(e1, []),
          E2 = ets:new(e2, []),
          true = ets:insert(E1, [{1,a},{2,b}]),
          true = ets:insert(E2, [{a,1},{b,2}]),
          Q = qlc:q([{X,Z,W} ||
                        {X, Z} <- ets:table(E1),
                        {W, Y} <- ets:table(E2),
                        X =:= Y]),
          L = \"begin\n\"
              \"    V1 =\n\"
              \"        qlc:q([ \n\"
              \"               P0 ||\n\"
              \"                   P0 = {W,Y} <- ets:table(_)\n\"
              \"              ]),\n\"
              \"    V2 =\n\"
              \"        qlc:q([ \n\"
              \"               [G1|G2] ||\n\"
              \"                   G2 <- V1,\n\"
              \"                   G1 <- ets:table(_),\n\"
              \"                   element(2, G1) =:= element(1, G2)\n\"
              \"              ],\n\"
              \"              [{join,lookup}]),\n\"
              \"    qlc:q([ \n\"
              \"           {X,Z,W} ||\n\"
              \"               [{X,Z}|{W,Y}] <- V2\n\"
              \"          ])\n\"
              \"end\",
          Info1 =
             re:replace(qlc:info(Q), 
                        \"table\\\\(#Ref<[\\.0-9]*>\",
                        \"table(_\", [{return,list},global]),
          F = fun(C) -> C =/= $\n andalso C =/= $\s end,
          Info = lists:filter(F, Info1),
          L1 = lists:filter(F, L),
          L1 = Info,
          ets:delete(E1),
          ets:delete(E2)">>,

       <<"Q = qlc:q([{A,X,Z,W} ||
                        A <- [a,b,c],
                        {X,Z} <- [{a,1},{b,4},{c,6}],
                        {W,Y} <- [{2,a},{3,b},{4,c}],
                        X =:= Y],
                    {cache, list}),
          L =
       \"begin\n\"
       \"    V1 =\n\"
       \"        qlc:q([ \n\"
       \"               P0 ||\n\"
       \"                   P0 = {X,Z} <- qlc:keysort(1, [{a,1},{b,4},{c,6}], [])\n\"
       \"              ]),\n\"
       \"    V2 =\n\"
       \"        qlc:q([ \n\"
       \"               P0 ||\n\"
       \"                   P0 = {W,Y} <- qlc:keysort(2, [{2,a},{3,b},{4,c}], [])\n\"
       \"              ]),\n\"
       \"    V3 =\n\"
       \"        qlc:q([ \n\"
       \"               [G1|G2] ||\n\"
       \"                   G1 <- V1,\n\"
       \"                   G2 <- V2,\n\"
       \"                   element(1, G1) == element(2, G2)\n\"
       \"              ],\n\"
       \"              [{join,merge},{cache,list}]),\n\"
       \"    qlc:q([ \n\"
       \"           {A,X,Z,W} ||\n\"
       \"               A <- [a,b,c],\n\"
       \"               [{X,Z}|{W,Y}] <- V3,\n\"
       \"               X =:= Y\n\"
       \"          ])\n\"
       \"end\",
          L = qlc:info(Q)">>,

       <<"E1 = ets:new(t, [set]), % uses =:=/2
          Q1 = qlc:q([K ||
          {K} <- ets:table(E1),
          K == 2.71 orelse K == a]),
          {list,{table,_}, [{{'$1'},[],['$1']}]} = i(Q1),
          true = ets:delete(E1)">>,

       <<"F = fun(E, I) ->
                    qlc:q([V || {K,V} <- ets:table(E), K == I])
              end,
          E2 = ets:new(t, [set]),
          true = ets:insert(E2, [{{2,2},a},{{2,2.0},b},{{2.0,2},c}]),
          Q2 = F(E2, {2,2}),
          {table,{ets,table,[_,
                [{traverse,{select,[{{'$1','$2'},
                                     [{'==','$1',{const,{2,2}}}],
                                     ['$2']}]}}]]}} = i(Q2),
          [a,b,c] = lists:sort(qlc:e(Q2)),
          true = ets:delete(E2),

          E3 = ets:new(t, [ordered_set]), % uses ==/2
          true = ets:insert(E3, [{{2,2.0},b}]),
          Q3 = F(E3,{2,2}),
          {list,{table,_},[{{'$1','$2'},[],['$2']}]} = i(Q3),
          [b] = qlc:e(Q3),
          true = ets:delete(E3)">>,

       <<"T = gb_trees:empty(),
          QH = qlc:q([X || {{X,Y},_} <- gb_table:table(T),
                           ((X == 1) or (X == 2)) andalso
                           ((Y == a) or (Y == b) or (Y == c))]),
          L = \"ets:match_spec_run(lists:flatmap(fun(K) ->
                                        case
                                            gb_trees:lookup(K,
                                                            gb_trees:from_orddict([]))
                                        of
                                            {value,V} ->
                                                [{K,V}];
                                            none ->
                                                []
                                        end
                                 end,
                                 [{1,a},{1,b},{1,c},{2,a},{2,b},{2,c}]),
                   ets:match_spec_compile([{{{'$1','$2'},'_'},[],['$1']}]))\",
          L = qlc:info(QH)">>
      ],
    run(Config, Ts),

    L = [1,2,3],
    Bs = erl_eval:add_binding('L', L, erl_eval:new_bindings()),
    QH = qlc:string_to_handle("[X+1 || X <- L].", [], Bs),
    [2,3,4] = qlc:eval(QH),

    %% ets(3)
    MS = ets:fun2ms(fun({X,Y}) when (X > 1) or (X < 5) -> {Y} end),
    ETs = [
        [<<"true = ets:insert(Tab = ets:new(t, []),[{1,a},{2,b},{3,c},{4,d}]),
            MS = ">>, io_lib:format("~w", [MS]), <<",
            QH1 = ets:table(Tab, [{traverse, {select, MS}}]),

            QH2 = qlc:q([{Y} || {X,Y} <- ets:table(Tab), (X > 1) or (X < 5)]),

            true = qlc:info(QH1) =:= qlc:info(QH2),
            true = ets:delete(Tab)">>]],
    run(Config, ETs),

    %% dets(3)
    DTs = [
        [<<"{ok, T} = dets:open_file(t, []),
            ok = dets:insert(T, [{1,a},{2,b},{3,c},{4,d}]),
            MS = ">>, io_lib:format("~w", [MS]), <<",
            QH1 = dets:table(T, [{traverse, {select, MS}}]),

            QH2 = qlc:q([{Y} || {X,Y} <- dets:table(t), (X > 1) or (X < 5)]),

            true = qlc:info(QH1) =:= qlc:info(QH2),
            ok = dets:close(T)">>]],
    run(Config, DTs),

    ok.

compile_gb_table(Config) ->
    GB_table_file = filename("gb_table.erl", Config),
    ok = file:write_file(GB_table_file, gb_table()),
    {ok, gb_table} = compile:file(GB_table_file, [{outdir,?privdir}]),
    code:purge(gb_table),
    {module, gb_table} =
        code:load_abs(filename:rootname(GB_table_file)),
    ok.

gb_table() ->
    <<"
-module(gb_table).

-export([table/1]).

table(T) ->
    TF = fun() -> qlc_next(gb_trees:next(gb_trees:iterator(T))) end,
    InfoFun = fun(num_of_objects) -> gb_trees:size(T);
                 (keypos) -> 1;
                 (is_sorted_key) -> true;
                 (is_unique_objects) -> true;
                 (_) -> undefined
              end,
    LookupFun =
        fun(1, Ks) ->
                lists:flatmap(fun(K) ->
                                      case gb_trees:lookup(K, T) of
                                          {value, V} -> [{K,V}];
                                          none -> []
                                      end
                              end, Ks)
        end,
    FormatFun =
        fun({all, NElements, ElementFun}) ->
                ValsS = io_lib:format(\"gb_trees:from_orddict(~w)\",
                                      [gb_nodes(T, NElements, ElementFun)]),
                io_lib:format(\"gb_table:table(~s)\", [ValsS]);
           ({lookup, 1, KeyValues, _NElements, ElementFun}) ->
                ValsS = io_lib:format(\"gb_trees:from_orddict(~w)\",
                                      [gb_nodes(T, infinity, ElementFun)]),
                io_lib:format(\"lists:flatmap(fun(K) -> \"
                              \"case gb_trees:lookup(K, ~s) of \"
                              \"{value, V} -> [{K,V}];none -> [] end \"
                              \"end, ~w)\",
                              [ValsS, [ElementFun(KV) || KV <- KeyValues]])
        end,
    qlc:table(TF, [{info_fun, InfoFun}, {format_fun, FormatFun},
                   {lookup_fun, LookupFun},{key_equality,'=='}]).

qlc_next({X, V, S}) ->
    [{X,V} | fun() -> qlc_next(gb_trees:next(S)) end];
qlc_next(none) ->
    [].

gb_nodes(T, infinity, ElementFun) ->
    gb_nodes(T, -1, ElementFun);
gb_nodes(T, NElements, ElementFun) ->
    gb_iter(gb_trees:iterator(T), NElements, ElementFun).

gb_iter(_I, 0, _EFun) ->
    '...';
gb_iter(I0, N, EFun) ->
    case gb_trees:next(I0) of
        {X, V, I} ->
            [EFun({X,V}) | gb_iter(I, N-1, EFun)];
        none ->
            []
    end.
    ">>.


%% OTP-6674. Join info and extra constants.
backward(Config) when is_list(Config) ->
    try_old_join_info(Config),
    ok.

try_old_join_info(Config) ->
    %% Check join info for handlers of extra constants.
    File = filename:join(?datadir, "join_info_compat.erl"),
    M = join_info_compat,
    {ok, M} = compile:file(File, [{outdir, ?datadir}]),
    {module, M} = code:load_abs(filename:rootname(File)),
    H = M:create_handle(),
    A0 = erl_anno:new(0),
    {block,A0,
     [{match,_,_,
       {call,_,_,
        [{lc,_,_,
          [_,
           {op,_,'=:=',
            {float,_,192.0},
            {call,_,{atom,_,element},[{integer,_,1},_]}}]}]}},
      _,_,
      {call,_,_,
       [{lc,_,_,
         [_,
          {op,_,'=:=',{var,_,'B'},{float,_,192.0}},
          {op,_,'==',{var,_,'X'},{var,_,'Y'}}]}]}]}
        = qlc:info(H,{format,abstract_code}),
    [{1,1},{2,2}] = qlc:e(H),

    H2 = M:lookup_handle(),
    {qlc,_,[{generate,_,{qlc,_,_,[{join,lookup}]}},_],[]} =
        qlc:info(H2, {format,debug}),
    [{1,1},{2,2}] = qlc:e(H2).

forward(Config) when is_list(Config) ->
    Ts = [
      %% LC_fun() returns something unknown.
      <<"FakeH = {qlc_handle,{qlc_lc,fun() -> {foo,bar} end,
                             {qlc_opt,false,false,-1,any,[],any,524288}}},
         {'EXIT', {{unsupported_qlc_handle,_},_}} = (catch qlc:e(FakeH))">>,

%% 'f1' should be used for new stuff that does not interfer with old behavior
%%       %% The unused element 'f1' of #qlc_table seems to be used.
%%       <<"DF = fun() -> foo end,
%%          FakeH = {qlc_handle,{qlc_table,DF,
%%                        true,DF,DF,DF,DF,DF,
%%                        undefined,not_undefined,undefined,no_match_spec}},
%%          {'EXIT', {{unsupported_qlc_handle,_},_}} = (catch qlc:e(FakeH))">>,

      %% #qlc_opt has changed.
      <<"H = qlc:q([X || X <- []]),
         {qlc_handle, {qlc_lc, Fun, _Opt}} = H,
         FakeH = {qlc_handle, {qlc_lc, Fun, {new_qlc_opt, a,b,c}}},
         {'EXIT', {{unsupported_qlc_handle,_},_}} = (catch qlc:e(FakeH))">>

     ],
    run(Config, Ts),
    ok.

eep37(Config) when is_list(Config) ->
    Ts = [
        <<"H = (fun _Handle() -> qlc:q([X || X <- []]) end)(),
           [] = qlc:eval(H)">>
    ],
    run(Config, Ts),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bad_table_throw(Tab) ->
    Limit = 1,
    Select = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    PreFun = fun(_) -> throw({throw,bad_pre_fun}) end,
    PostFun = fun() -> throw({throw,bad_post_fun}) end,
    InfoFun = fun(Tag) -> info(Tab, Tag) end,
    qlc:table(Select, [{pre_fun,PreFun}, {post_fun, PostFun}, 
                       {info_fun, InfoFun}]).
    
bad_table_exit(Tab) ->
    Limit = 1,
    Select = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    PreFun = fun(_) -> erlang:error(bad_pre_fun) end,
    PostFun = fun() -> erlang:error(bad_post_fun) end,
    InfoFun = fun(Tag) -> info(Tab, Tag) end,
    qlc:table(Select, [{pre_fun,PreFun}, {post_fun, PostFun}, 
                       {info_fun, InfoFun}]).
    
info(_Tab, is_unique_objects) -> 
    false;
info(Tab, Tag) -> 
    try ets:info(Tab, Tag) catch _:_ -> undefined end.

create_ets(S, E) ->
    create_ets(lists:seq(S, E)).

create_ets(L) ->
    E1 = ets:new(e, []),
    true = ets:insert(E1, [{X,X} || X <- L]),
    E1.

etsc(F, Objs) ->
    etsc(F, [{keypos,1}], Objs).

etsc(F, Opts, Objs) ->
    E = ets:new(test, Opts),
    true = ets:insert(E, Objs),
    V = F(E),
    ets:delete(E),
    V.

join_info(H) ->
    {{qlc, S, Options}, Bs} = strip_qlc_call2(H),
    %% "Hide" the call to qlc_pt from the test in run_test().
    LoadedPT = code:is_loaded(qlc_pt),
    QH = qlc:string_to_handle(S, Options, Bs),
    _ = [unload_pt() || false <- [LoadedPT]], % doesn't take long...
    case {join_info_count(H), join_info_count(QH)} of
        {N, N} -> 
            N;
        Ns -> 
            Ns
    end.

strip_qlc_call(H) ->
    {Expr, _Bs} = strip_qlc_call2(H),
    Expr.

strip_qlc_call2(H) ->
    S = qlc:info(H, {flat, false}),
    {ok, Tokens, _EndLine} = erl_scan:string(S++".", 1, [text]),
    {ok, [Expr], Bs} = lib:extended_parse_exprs(Tokens),
    {case Expr of
         {call,_,{remote,_,{atom,_,qlc},{atom,_,q}},[LC]} ->
             {qlc, lists:flatten([erl_pp:expr(LC), "."]), []};
         {call,_,{remote,_,{atom,_,qlc},{atom,_,q}},[LC, Opts]} ->
             {qlc, lists:flatten([erl_pp:expr(LC), "."]),
              erl_parse:normalise(Opts)};
         {call,_,{remote,_,{atom,_,ets},{atom,_,match_spec_run}},_} ->
             {match_spec, Expr};
         {call,_,{remote,_,{atom,_,M},{atom,_,table}},_} ->
             {table, M, Expr};
         _ ->
             []
     end, Bs}.

-record(ji, {nmerge = 0, nlookup = 0, nnested_loop = 0, nkeysort = 0}).

%% Counts join options and (all) calls to qlc:keysort().
join_info_count(H) ->
    S = qlc:info(H, {flat, false}),    
    {ok, Tokens, _EndLine} = erl_scan:string(S++".", 1, [text]),
    {ok, [Expr], _Bs} = lib:extended_parse_exprs(Tokens),
    #ji{nmerge = Nmerge, nlookup = Nlookup, 
        nkeysort = NKeysort, nnested_loop = Nnested_loop} = 
        ji(Expr, #ji{}),
    {Nmerge, Nlookup, Nnested_loop, NKeysort}.

ji({call,_,{remote,_,{atom,_,qlc},{atom,_,q}},[LC,Options]}, JI) ->
    NJI = case lists:keysearch(join, 1, erl_parse:normalise(Options)) of
              {value, {join, merge}} ->
                  JI#ji{nmerge = JI#ji.nmerge + 1};
              {value, {join, lookup}} ->
                  JI#ji{nlookup = JI#ji.nlookup + 1};
              {value, {join, nested_loop}} ->
                  JI#ji{nnested_loop = JI#ji.nnested_loop + 1};
              _  ->
                  JI
          end,
    ji(LC, NJI);
ji({call,_,{remote,_,{atom,_,qlc},{atom,_,keysort}},[_KP,H,_Options]}, JI) ->
    ji(H, JI#ji{nkeysort = JI#ji.nkeysort + 1});
ji(T, JI) when is_tuple(T) ->
    ji(tuple_to_list(T), JI);
ji([E | Es], JI) ->
    ji(Es, ji(E, JI));
ji(_, JI) ->
    JI.

%% Designed for ETS' and gb_table's format funs.
lookup_keys(Q) ->
    case lists:flatten(lookup_keys(i(Q), [])) of
        [] -> false;
        L -> lists:usort(L)
    end.

lookup_keys([Q | Qs], L) ->
    lookup_keys(Qs, lookup_keys(Q, L));
lookup_keys({qlc,_,Quals,_}, L) ->
    lookup_keys(Quals, L);
lookup_keys({list,Q,_}, L) ->
    lookup_keys(Q, L);
lookup_keys({generate,_,Q}, L) ->
    lookup_keys(Q, L);
lookup_keys({table,Chars}, L) when is_list(Chars) ->
    {ok, Tokens, _} = erl_scan:string(lists:flatten(Chars++"."), 1, [text]),
    {ok, [Expr], _Bs} = lib:extended_parse_exprs(Tokens),
    case Expr of
        {call,_,_,[_fun,AKs]} ->
            case erl_parse:normalise(AKs) of
                Ks when is_list(Ks) ->
                    [lists:sort(Ks) | L];
                K -> % assume keys are never lists (ets only)
                    [K | L]
            end;
        _ -> % gb_table
            L
    end;
lookup_keys(_Q, L) ->
    L.

bad_table_format(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    FormatFun = {is, no, good},
    qlc:table(SelectFun, [{format_fun, FormatFun}]).

bad_table_format_arity(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    FormatFun = fun() -> {?MODULE, bad_table_format_arity, [Tab]} end,
    qlc:table(SelectFun, [{format_fun, FormatFun}]).

bad_table_traverse(Tab) ->
    Limit = 1,
    Select = fun(MS, _) -> cb(ets:select(Tab, MS, Limit)) end,
    qlc:table(Select, []).
    
bad_table_post(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    qlc:table(SelectFun, [{pre_fun,undefined}, 
                          {post_fun, fun(X) -> X end}, 
                          {info_fun, undefined}]).
    
bad_table_lookup(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    qlc:table(SelectFun, {lookup_fun, fun(X) -> X end}).

bad_table_max_lookup(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    qlc:table(SelectFun, {max_lookup, -2}).

bad_table_info_arity(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    InfoFun = fun() -> {?MODULE, bad_table_info_arity, [Tab]} end,
    qlc:table(SelectFun, [{info_fun, InfoFun}]).

default_table(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    qlc:table(SelectFun, [{format_fun, undefined},
                          {info_fun, undefined},
                          {lookup_fun, undefined},
                          {parent_fun, undefined},
                          {pre_fun,undefined}, 
                          {post_fun, undefined}]).
    
bad_table(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    qlc:table(SelectFun, [{info, fun() -> ok end}]).

bad_table_info_fun_n_objects(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    LookupFun = fun(_Pos, Ks) -> 
                        lists:flatmap(fun(K) -> ets:lookup(Tab, K) end, Ks) 
                end,
    InfoFun = fun(num_of_objects) -> exit(finito);
                 (_) -> undefined
              end,
    qlc:table(SelectFun, [{info_fun, InfoFun}, {lookup_fun, LookupFun}]).

bad_table_info_fun_indices(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    LookupFun = fun(_Pos, Ks) -> 
                        lists:flatmap(fun(K) -> ets:lookup(Tab, K) end, Ks) 
                end,
    InfoFun = fun(indices) -> throw({throw,apa});
                 (_) -> undefined
              end,
    qlc:table(SelectFun, [{info_fun, InfoFun}, {lookup_fun, LookupFun}]).

bad_table_info_fun_keypos(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    LookupFun = fun(_Pos, Ks) -> 
                        lists:flatmap(fun(K) -> ets:lookup(Tab, K) end, Ks) 
                end,
    InfoFun = fun(indices) -> erlang:error(keypos);
                  (_) -> undefined
              end,
    qlc:table(SelectFun, [{info_fun, InfoFun}, {lookup_fun, LookupFun}]).

bad_table_key_equality(Tab) ->
    Limit = 1,
    SelectFun = fun(MS) -> cb(ets:select(Tab, MS, Limit)) end,
    LookupFun = fun(_Pos, Ks) -> 
                        lists:flatmap(fun(K) -> ets:lookup(Tab, K) end, Ks) 
                end,
    qlc:table(SelectFun, [{lookup_fun, LookupFun},{key_equality,'=/='}]).

cb('$end_of_table') -> 
    [];
cb({Objects,Cont}) -> 
    Objects ++ fun() -> cb(ets:select(Cont)) end.

i(H) ->
    i(H, []).

i(H, Options) when is_list(Options) ->
    case has_format(Options) of
        true -> qlc:info(H, Options);
        false -> qlc:info(H, [{format, debug} | Options])
    end;
i(H, Option) ->
    i(H, [Option]).

has_format({format,_}) ->
    true;
has_format([E | Es]) ->
    has_format(E) or has_format(Es);
has_format(_) ->
    false.

format_info(H, Flat) ->
    L = qlc:info(H, [{flat, Flat}, {format,string}]),
    re:replace(L, "\s|\n|\t|\f|\r|\v", "", [{return,list},global]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A list turned into a table...

table_kill_parent(List, Indices) ->
    ParentFun = fun() -> exit(self(), kill) end,
    table_i(List, Indices, ParentFun).

table_parent_throws(List, Indices) ->
    ParentFun = fun() -> throw({throw,thrown}) end,
    table_i(List, Indices, ParentFun).

table_parent_exits(List, Indices) ->
    ParentFun = fun() -> 1 + Indices end,
    table_i(List, Indices, ParentFun).

table_bad_parent_fun(List, Indices) ->
    ParentFun = fun(X) -> X end, % parent_fun should be nullary
    table_i(List, Indices, ParentFun).

table(List, Indices) ->
    ParentFun = fun() -> self() end,
    table_i(List, Indices, ParentFun).    

table(List, KeyPos, Indices) ->
    ParentFun = fun() -> self() end,
    table(List, Indices, KeyPos, ParentFun).    

table_i(List, Indices, ParentFun) ->
    table(List, Indices, undefined, ParentFun).

table(List, Indices, KeyPos, ParentFun) ->
    TraverseFun = fun() -> list_traverse(List) end,
    PreFun = fun(PreArgs) ->
                     {value, {parent_value, Pid}} = 
                         lists:keysearch(parent_value, 1, PreArgs),
                     true = is_pid(Pid)
             end,
    PostFun = fun() -> ok end,
    InfoFun = fun(indices) ->
                      Indices;
                 (is_unique_objects) ->
                      undefined;
                 (keypos) ->
                      KeyPos;
                 (num_of_objects) ->
                      undefined;
                 (_) ->
                      undefined
              end,
    LookupFun =
        fun(Column, Values) ->
                lists:flatmap(fun(V) ->
                                      case lists:keysearch(V, Column, List) of
                                          false -> [];
                                          {value,Val} -> [Val]
                                      end
                              end, Values)

                end,
    FormatFun = fun(all) ->
                        L = erl_anno:new(17),
                        {call,L,{remote,L,{atom,L,?MODULE},{atom,L,the_list}},
                                 [erl_parse:abstract(List, 17)]};
                   ({lookup, Column, Values}) ->
                        {?MODULE, list_keys, [Values, Column, List]}
                end,
    qlc:table(TraverseFun, [{info_fun,InfoFun}, {pre_fun, PreFun}, 
                            {post_fun, PostFun}, {lookup_fun, LookupFun}, 
                            {format_fun, FormatFun}, 
                            {parent_fun, ParentFun}]).

stop_list(List, Ets) ->
    Traverse = fun() -> list_traverse(List) end,
    PV = a_sample_parent_value,
    ParentFun = fun() -> PV end,
    Pre = fun(PreArgs) ->
                  {value, {parent_value, PV}} = 
                      lists:keysearch(parent_value, 1, PreArgs),
                  {value, {stop_fun, Fun}} = 
                      lists:keysearch(stop_fun, 1, PreArgs),
                  true = ets:insert(Ets, {stop_fun, Fun})
          end,
    qlc:table(Traverse, [{pre_fun, Pre}, {parent_fun, ParentFun}]).

list_traverse([]) ->
    [];
list_traverse([E | Es]) ->
    [E | fun() -> list_traverse(Es) end].

table_error(List, Error) ->
    table_error(List, undefined, Error).

table_error(List, KeyPos, Error) ->
    TraverseFun = fun() -> list_traverse2(lists:sort(List), Error) end,
    InfoFun = fun(is_sorted_key) -> true;
                 (keypos) -> KeyPos;
                 (_) -> undefined
              end,
    qlc:table(TraverseFun, [{info_fun,InfoFun}]).

list_traverse2([], Err) ->
    Err;
list_traverse2([E | Es], Err) ->
    [E | fun() -> list_traverse2(Es, Err) end].

table_lookup_error(List) ->
    TraverseFun = fun() -> list_traverse(List) end,
    LookupFun = fun(_Column, _Values) -> {error,lookup,failed} end,
    InfoFun = fun(keypos) -> 1;
                 (_) -> undefined
              end,
    qlc:table(TraverseFun, [{lookup_fun,LookupFun},{info_fun,InfoFun}]).

prep_scratchdir(Dir) ->
    put('$qlc_tmpdir', true),
    _ = filelib:ensure_dir(Dir),
    lists:foreach(fun(F) -> file:delete(F)
                  end, filelib:wildcard(filename:join(Dir, "*"))),
    true.

%% Truncate just once.
truncate_tmpfile(Dir, Where) ->
    case get('$qlc_tmpdir') of
        true -> 
            {ok, [TmpFile0 | _]} = file:list_dir(Dir),
            TmpFile = filename:join(Dir, TmpFile0),
            truncate(TmpFile, Where),
            erase('$qlc_tmpdir');
        _ ->
            true
    end.

truncate(File, Where) ->
    {ok, Fd} = file:open(File, [read, write]),
    {ok, _} = file:position(Fd, Where),
    ok = file:truncate(Fd),
    ok = file:close(Fd).

%% Crash just once.
crash_tmpfile(Dir, Where) ->
    case get('$qlc_tmpdir') of
        true -> 
            {ok, [TmpFile0 | _]} = file:list_dir(Dir),
            TmpFile = filename:join(Dir, TmpFile0),
            crash(TmpFile, Where),
            erase('$qlc_tmpdir');
        _ ->
            true
    end.

crash(File, Where) ->
    {ok, Fd} = file:open(File, [read, write]),
    {ok, _} = file:position(Fd, Where),
    ok = file:write(Fd, [10]),
    ok = file:close(Fd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(Config, Tests) ->
    run(Config, [], Tests).

run(Config, Extra, Tests) ->
    lists:foreach(fun(Body) -> run_test(Config, Extra, Body) end, Tests).

run_test(Config, Extra, {cres, Body, ExpectedCompileReturn}) ->
    run_test(Config, Extra, {cres, Body, _Opts = [], ExpectedCompileReturn});
run_test(Config, Extra, {cres, Body, Opts, ExpectedCompileReturn}) ->
    {SourceFile, Mod} = compile_file_mod(Config),
    P = [Extra,<<"function() -> ">>, Body, <<", ok. ">>],
    CompileReturn = compile_file(Config, P, Opts),
    case comp_compare(ExpectedCompileReturn, CompileReturn) of
        true -> ok;
        false -> expected(ExpectedCompileReturn, CompileReturn, SourceFile)
    end,
    AbsFile = filename:rootname(SourceFile, ".erl"),
    _ = code:purge(Mod),
    {module, _} = code:load_abs(AbsFile, Mod),

    Ms0 = erlang:process_info(self(),messages),
    Before = {{get(), lists:sort(ets:all()), Ms0}, pps()},

    %% Prepare the check that the qlc module does not call qlc_pt.
    _ = [unload_pt() || {file, Name} <- [code:is_loaded(qlc_pt)], 
                        Name =/= cover_compiled],

    R = case catch Mod:function() of
            {'EXIT', _Reason} = Error ->
                io:format("failed, got ~p~n", [Error]),
                fail(SourceFile);
            Reply ->
                Reply
        end,

    %% Check that the qlc module does not call qlc_pt:
    case code:is_loaded(qlc_pt) of
        {file, cover_compiled} ->
            ok;
        {file, _} ->
            io:format("qlc_pt was loaded in runtime~n", []),
            fail(SourceFile);
        false ->
            ok
    end,

    wait_for_expected(R, Before, SourceFile, true),
    code:purge(Mod);
run_test(Config, Extra, Body) ->
    run_test(Config, Extra, {cres,Body,[]}).

wait_for_expected(R, {Strict0,PPS0}=Before, SourceFile, Wait) ->
    Ms = erlang:process_info(self(),messages),
    After = {_,PPS1} = {{get(), lists:sort(ets:all()), Ms}, pps()},
    case {R, After} of
        {ok, Before} ->
            ok;
        {ok, {Strict0,_}} ->
            {Ports0,Procs0} = PPS0,
            {Ports1,Procs1} = PPS1,
            case {Ports1 -- Ports0, Procs1 -- Procs0} of
                {[], []} -> ok;
                _ when Wait ->
                    timer:sleep(1000),
                    wait_for_expected(R, Before, SourceFile, false);
                {PortsDiff,ProcsDiff} ->
                    io:format("failure, got ~p~n, expected ~p\n",
                              [PPS1, PPS0]),
                    show("Old port", Ports0 -- Ports1),
                    show("New port", PortsDiff),
                    show("Old proc", Procs0 -- Procs1),
                    show("New proc", ProcsDiff),
                    fail(SourceFile)
            end;
        _ when Wait ->
            timer:sleep(1000),
            wait_for_expected(R, Before, SourceFile, false);
        _ ->
            expected({ok,Before}, {R,After}, SourceFile)
    end.

unload_pt() ->
    erlang:garbage_collect(), % get rid of references to qlc_pt...
    _ = code:purge(qlc_pt),
    _ = code:delete(qlc_pt).

compile_format(Config, Tests) ->
    Fun = fun(Test, Opts) ->
                  Return = compile_file(Config, Test, Opts),
                  format_messages(Return)
          end, 
    compile(Config, Tests, Fun).    

format_messages({warnings,Ws}) ->
    format_messages({errors,[],Ws});
format_messages({errors,Es,Ws}) ->
    {[format_msg(E, Mod) || {_Line,Mod,E} <- Es],
     [format_msg(W, Mod) || {_Line,Mod,W} <- Ws]}.

format_msg(Msg, Mod) ->
    IOlist = Mod:format_error(Msg),
    binary_to_list(iolist_to_binary(IOlist)).

compile(Config, Tests) ->
    Fun = fun(Test, Opts) -> catch compile_file(Config, Test, Opts) end,
    compile(Config, Tests, Fun).

compile(Config, Tests, Fun) ->
    F = fun({TestName,Test,Opts,Expected}, BadL) ->
                Return = Fun(Test, Opts),
                case comp_compare(Expected, Return) of
                    true ->
                        BadL;
                    false -> 
                        {File, _Mod} = compile_file_mod(Config),
                        expected(TestName, Expected, Return, File)
                end
        end,
    lists:foldl(F, [], Tests).

%% Compiles a test module and returns the list of errors and warnings.

compile_file(Config, Test0, Opts0) ->
    {File, Mod} = compile_file_mod(Config),
    Test = list_to_binary(["-module(", atom_to_list(Mod), "). "
                           "-import(qlc_SUITE, [i/1,i/2,format_info/2]). "
                           "-import(qlc_SUITE, [etsc/2, etsc/3]). "
                           "-import(qlc_SUITE, [create_ets/2]). "
                           "-import(qlc_SUITE, [strip_qlc_call/1]). "
                           "-import(qlc_SUITE, [join_info/1]). "
                           "-import(qlc_SUITE, [join_info_count/1]). "
                           "-import(qlc_SUITE, [lookup_keys/1]). "
                           "-include_lib(\"stdlib/include/qlc.hrl\"). ",
                           Test0]),
    Opts = [export_all,nowarn_export_all,return,nowarn_unused_record,{outdir,?privdir}|Opts0],
    ok = file:write_file(File, Test),
    case compile:file(File, Opts) of
        {ok, _M, Ws} -> warnings(File, Ws);
        {error, [{File,Es}], []} -> {errors, Es, []};
        {error, [{File,Es}], [{File,Ws}]} -> {errors, Es, Ws}
    end.

comp_compare(T, T) ->
    true;
comp_compare(T1, T2_0) ->
    T2 = wskip(T2_0),
    T1 =:= T2
       %% This clause should eventually be removed. 
       orelse ln(T1) =:= T2 orelse T1 =:= ln(T2).

wskip([]) ->
    [];
wskip([{_,sys_core_fold,{eval_failure,badarg}}|L]) ->
    wskip(L);
wskip([{{L,_C},sys_core_fold,M}|L]) ->
    [{L,sys_core_fold,M}|wskip(L)];
wskip({T,L}) ->
    {T,wskip(L)};
wskip([M|L]) ->
    [M|wskip(L)];
wskip(T) ->
    T.

%% Replaces locations like {Line,Column} with Line. 
ln({warnings,L}) ->
    {warnings,ln0(L)};
ln({errors,EL,WL}) ->
    {errors,ln0(EL),ln0(WL)};
ln(L) ->
    ln0(L).

ln0(L) ->
    lists:sort(ln1(L)).

ln1([]) ->
    [];
ln1([{File,Ms}|MsL]) when is_list(File) ->
    [{File,ln0(Ms)}|ln1(MsL)];
ln1([{{L,_C},Mod,Mess0}|Ms]) ->
    Mess = case Mess0 of
               {exported_var,V,{Where,{L1,_C1}}} ->
                   {exported_var,V,{Where,L1}};
               {unsafe_var,V,{Where,{L1,_C1}}} ->
                   {unsafe_var,V,{Where,L1}};
               %% There are more...
               M ->
                   M
           end,
    [{L,Mod,Mess}|ln1(Ms)];
ln1([M|Ms]) ->
    [M|ln1(Ms)].

%% -> {FileName, Module}; {string(), atom()}
compile_file_mod(Config) ->
    NameL = lists:concat([?TESTMODULE, "_", ?testcase]),
    Name = list_to_atom(NameL),
    File = filename(NameL ++ ".erl", Config),
    {File, Name}.

filename(Name, Config) when is_atom(Name) ->
    filename(atom_to_list(Name), Config);
filename(Name, Config) ->
    filename:join(?privdir, Name).

show(_S, []) ->
    ok;
show(S, [{Pid, Name, InitCall}|Pids]) when is_pid(Pid) ->
    io:format("~s: ~w (~w), ~w: ~p~n",
              [S, Pid, proc_reg_name(Name), InitCall,
               erlang:process_info(Pid)]),
    show(S, Pids);
show(S, [{Port, _}|Ports]) when is_port(Port)->
    io:format("~s: ~w: ~p~n", [S, Port, erlang:port_info(Port)]),
    show(S, Ports).

pps() ->
    {port_list(), process_list()}.

port_list() ->
    [{P,safe_second_element(erlang:port_info(P, name))} || 
        P <- erlang:ports()].

process_list() ->
    [{P,process_info(P, registered_name),
      safe_second_element(process_info(P, initial_call))} || 
        P <- processes(), is_process_alive(P)].

proc_reg_name({registered_name, Name}) -> Name;
proc_reg_name([]) -> no_reg_name.

safe_second_element({_,Info}) -> Info;
safe_second_element(Other) -> Other.

warnings(File, Ws) ->
    case lists:append([W || {F, W} <- Ws, F =:= File]) of
        [] -> [];
        L -> {warnings, L}
    end.

expected(Test, Expected, Got, File) ->
    io:format("~nTest ~p failed. ", [Test]),
    expected(Expected, Got, File).

expected(Expected, Got, File) ->
    io:format("Expected~n  ~p~n, but got~n  ~p~n", [Expected, Got]),
    fail(File).

fail(Source) ->
    ct:fail({failed,testcase,on,Source}).

%% Copied from global_SUITE.erl.

install_error_logger() ->
    error_logger:add_report_handler(?MODULE, self()).

uninstall_error_logger() ->
    error_logger:delete_report_handler(?MODULE).

read_error_logger() ->
    receive
	{error, Why} ->
            {error, Why};
        {info, Why} ->
            {info, Why};
        {warning, Why} ->
            {warning, Why};
        {error, Pid, Tuple} ->
            {error, Pid, Tuple}
    after 1000 ->
	    io:format("No reply after 1 s\n", []),
	    ct:fail(failed)
    end.

%%-----------------------------------------------------------------
%% The error_logger handler used.
%% (Copied from stdlib/test/proc_lib_SUITE.erl.)
%%-----------------------------------------------------------------
init(Tester) ->
    {ok, Tester}.
    
handle_event({error, _GL, {_Pid, _Msg, [Why, _]}}, Tester) when is_atom(Why) ->
    Tester ! {error, Why},
    {ok, Tester};
handle_event({error, _GL, {_Pid, _Msg, [P, T]}}, Tester) when is_pid(P) ->
    Tester ! {error, P, T},
    {ok, Tester};
handle_event({info_msg, _GL, {_Pid, _Msg, [Why, _]}}, Tester) ->
    Tester ! {info, Why},
    {ok, Tester};
handle_event({warning_msg, _GL, {_Pid, _Msg, [Why, _]}}, Tester) when is_atom(Why) ->
    Tester ! {warning, Why},
    {ok, Tester};
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, State) -> {ok, {error, bad_query}, State}.

terminate(_Reason, State) ->
    State.

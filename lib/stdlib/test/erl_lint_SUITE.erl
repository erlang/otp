%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2011. All Rights Reserved.
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
-module(erl_lint_SUITE).

%-define(debug, true).

-ifdef(debug).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(datadir, "erl_lint_SUITE_data").
-define(privdir, "erl_lint_SUITE_priv").
-define(t, test_server).
-else.
-include_lib("test_server/include/test_server.hrl").
-define(datadir, ?config(data_dir, Conf)).
-define(privdir, ?config(priv_dir, Conf)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([ 
	  unused_vars_warn_basic/1, 
	  unused_vars_warn_lc/1, 
	  unused_vars_warn_rec/1,
	  unused_vars_warn_fun/1, 
	  unused_vars_OTP_4858/1,
	  export_vars_warn/1,
	  shadow_vars/1,
	  unused_import/1,
	  unused_function/1,
	  unsafe_vars/1,unsafe_vars2/1,
	  unsafe_vars_try/1,
	  guard/1, otp_4886/1, otp_4988/1, otp_5091/1, otp_5276/1, otp_5338/1,
	  otp_5362/1, otp_5371/1, otp_7227/1, otp_5494/1, otp_5644/1, otp_5878/1,
	  otp_5917/1, otp_6585/1, otp_6885/1, export_all/1,
	  bif_clash/1,
	  behaviour_basic/1, behaviour_multiple/1,
	  otp_7550/1,
	  otp_8051/1,
	  format_warn/1,
	  on_load_successful/1, on_load_failing/1, 
	  too_many_arguments/1
        ]).

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].

end_per_testcase(_Case, _Config) ->
    Dog = ?config(watchdog, _Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, unused_vars_warn}, export_vars_warn,
     shadow_vars, unused_import, unused_function,
     unsafe_vars, unsafe_vars2, unsafe_vars_try, guard,
     otp_4886, otp_4988, otp_5091, otp_5276, otp_5338,
     otp_5362, otp_5371, otp_7227, otp_5494, otp_5644,
     otp_5878, otp_5917, otp_6585, otp_6885, export_all,
     bif_clash, behaviour_basic, behaviour_multiple,
     otp_7550, otp_8051, format_warn, {group, on_load},
     too_many_arguments].

groups() -> 
    [{unused_vars_warn, [],
      [unused_vars_warn_basic, unused_vars_warn_lc,
       unused_vars_warn_rec, unused_vars_warn_fun,
       unused_vars_OTP_4858]},
     {on_load, [], [on_load_successful, on_load_failing]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



unused_vars_warn_basic(doc) ->
    "Warnings for unused variables in some simple cases.";
unused_vars_warn_basic(suite) -> [];
unused_vars_warn_basic(Config) when is_list(Config) ->
    Ts = [{basic1,
           <<"f(F) -> % F unused.
                ok.

            f(F, F) ->
                ok.

            g(_X) ->
                y.

            h(P) ->
                P.

            x(N) ->
                case a:b() of
                    [N|Y] -> % Y unused.
                        ok
                end.

            y(N, L) ->
                lists:map(fun(T) -> T*N end, L).

            z(N, L) -> % N unused
                lists:map(fun(N, T) -> T*N end, L).  % N shadowed.


            c(A) ->
                case A of
                    1 -> B = []; % B unused.
                    2 -> B = []; % B unused.
                    3 -> B = f, B
                end.
           ">>,
	   [warn_unused_vars],
	   {warnings,[{1,erl_lint,{unused_var,'F'}},
             {15,erl_lint,{unused_var,'Y'}},
             {22,erl_lint,{unused_var,'N'}},
             {23,erl_lint,{shadowed_var,'N','fun'}},
             {28,erl_lint,{unused_var,'B'}},
             {29,erl_lint,{unused_var,'B'}}]}}],
    ?line [] = run(Config, Ts),
    ok.

unused_vars_warn_lc(doc) ->
    "Warnings for unused variables in list comprehensions.";
unused_vars_warn_lc(suite) -> [];
unused_vars_warn_lc(Config) when is_list(Config) ->
    Ts = [{lc1, 
           <<"bin([X]) ->
                  [A || <<A:X>> <- []]; % X used, not shadowed.
              bin({X}) ->
                  [X || <<X:X>> <- []]; % X used, and shadowed.
              bin({X,Y,Z}) ->
                  [{A,B} || <<A:X>> <- Z, <<B:Y>> <- Z];
              bin([X,Y,Z]) -> % Y unused.
                  [C || <<V:X>> <- Z, <<B:V>> <- Z, <<C:B>> <- Z].
           ">>, 
           [warn_unused_vars],
           {warnings, [{4,erl_lint,{shadowed_var,'X',generate}},
                       {7,erl_lint,{unused_var,'Y'}}]}},

          {lc2,
           <<"bin([X]) ->
                  [A || <<A:X>> <- []]; % X used, not shadowed.
              bin({X}) ->
                  [X || <<X:X>> <- []]; % X used, and shadowed.
              bin({X,Y,Z}) ->
                  [{A,B} || <<A:X>> <- Z, <<B:Y>> <- Z];
              bin([X,Y,Z]) -> % Y unused.
                  [C || <<V:X>> <- Z, <<B:V>> <- Z, <<C:B>> <- Z].
           ">>,
           [warn_unused_vars],
           {warnings,[{4,erl_lint,{shadowed_var,'X',generate}},
                      {7,erl_lint,{unused_var,'Y'}}]}},

          {lc3,
           <<"a([A]) ->
                  B = foo,
                  [{C,B} || {C,_} <- A];
              a({A}) ->
                  B = foo,
                  [C || {C,_} <- [B,A]];
              a({A,A}) ->
                  B = foo,
                  [C || {C,_} <- B, B < A].
           ">>,
           [warn_unused_vars],
           []},

          {lc4,
           <<"b(A) ->
                  B = foo, % B unused.
                  [C || {C,_} <- A].
           ">>,
           [warn_unused_vars],
           {warnings,[{2,erl_lint,{unused_var,'B'}}]}},

          {lc5,
           <<"c(A) ->
                  B = foo,
                  [C || {C,_} <- A],
                  B.
           ">>,
           [warn_unused_vars],
           []},

          {lc6,
           <<"d(A) ->
                  B = foo,
                  [{A,B} || {Id,_} <- A]. % Id unused.
           ">>,
           [warn_unused_vars],
           {warnings,[{3,erl_lint,{unused_var,'Id'}}]}},

          {lc7,
           <<"e(A) ->
                  B = foo, % B unused.
                  [B || B <- A]. % B shadowed.
           ">>,
           [warn_unused_vars],
           {warnings,[{2,erl_lint,{unused_var,'B'}},
                      {3,erl_lint,{shadowed_var,'B',generate}}]}},

          {lc8,
           <<"f(A) ->
                  B = foo,
                  [B || B <- A], % B shadowed.
                  B.
           ">>,
           [warn_unused_vars],
           {warnings,[{3,erl_lint,{shadowed_var,'B',generate}}]}},

          {lc9,
           <<"g(A) ->
                  B = foo, % B unused.
                  [A || B <- A]. % B shadowed, B unused.
           ">>,
           [warn_unused_vars],
           {warnings,[{2,erl_lint,{unused_var,'B'}},
                      {3,erl_lint,{unused_var,'B'}},
                      {3,erl_lint,{shadowed_var,'B',generate}}]}},

          {lc10,
           <<"h(A) ->
                  B = foo,
                  [A || B <- A], % B shadowed, B unused.
                  B.
           ">>,
           [warn_unused_vars],
           {warnings,[{3,erl_lint,{unused_var,'B'}},
                      {3,erl_lint,{shadowed_var,'B',generate}}]}},

          {lc11,
           <<"i(X) ->
                  [Z || Z <- X, % Z unused.
                        Z = X <- [foo]]. % X and Z shadowed. X unused!
           ">>,
           [warn_unused_vars],
           {warnings,[{2,erl_lint,{unused_var,'Z'}},
                      {3,erl_lint,{unused_var,'X'}},
                      {3,erl_lint,{shadowed_var,'X',generate}},
                      {3,erl_lint,{shadowed_var,'Z',generate}}]}},

          {lc12,
           <<"j({X}) ->
                  [Z || Z <- X, % Z unused.
                        Z <- X = [[1,2,3]], % Z shadowed. Z unused.
                        Z <- X, % Z shadowed. Z unused.
                        Z <- X]; % Z shadowed.
              j(X) ->
                  [foo || X <- X, % X shadowed.
                        X <-    % X shadowed. X unused.
                             X = 
                                 Y = [[1,2,3]], % Y unused.
                        X <- [], % X shadowed.
                        X <- X]. % X shadowed. X unused.
           ">>,
           [warn_unused_vars],
           {warnings,[{2,erl_lint,{unused_var,'Z'}},
                      {3,erl_lint,{unused_var,'Z'}},
                      {3,erl_lint,{shadowed_var,'Z',generate}},
                      {4,erl_lint,{unused_var,'Z'}},
                      {4,erl_lint,{shadowed_var,'Z',generate}},
                      {5,erl_lint,{shadowed_var,'Z',generate}},
                      {7,erl_lint,{shadowed_var,'X',generate}},
                      {8,erl_lint,{unused_var,'X'}},
                      {8,erl_lint,{shadowed_var,'X',generate}},
                      {10,erl_lint,{unused_var,'Y'}},
                      {11,erl_lint,{shadowed_var,'X',generate}},
                      {12,erl_lint,{unused_var,'X'}},
                      {12,erl_lint,{shadowed_var,'X',generate}}]}},

          {lc13,
           <<"k(X) ->
                  [Z || Z <- Y = X]; % Y unused.
              k(X) ->
                  [Z || Z <- X = Y = X]; % Y unused!
              k(X) ->
                  [Z || Z <- begin X = Y = X, Y end];
              k(X) ->
                  [{Y,W} || W <- Y = X]; % Y unbound
              k(X) ->
                  [Z || Z <- (Y = X), % Y unused.
                       Y > X]; % Y unbound.
              k(X) ->
                  [Y || Y = X > 3, Z = X]; % Z unused.
              k(X) ->
                  [Z || Y = X > 3, Z = X]. % Y unused.
           ">>,
           [warn_unused_vars],
           {error,[{8,erl_lint,{unbound_var,'Y'}},
                   {11,erl_lint,{unbound_var,'Y'}}],
                  [{2,erl_lint,{unused_var,'Y'}},
                   {4,erl_lint,{unused_var,'Y'}},
                   {8,erl_lint,{unused_var,'Y'}},
                   {10,erl_lint,{unused_var,'Y'}},
                   {13,erl_lint,{unused_var,'Z'}},
                   {15,erl_lint,{unused_var,'Y'}}]}},

          {lc14,
           <<"lc2() ->
                  Z = [[1],[2],[3]], 
                  [X || Z <- Z, % Z shadowed.
                        X <- Z].
           ">>,
           [warn_unused_vars],
           {warnings,[{3,erl_lint,{shadowed_var,'Z',generate}}]}},

          {lc15,
           <<"lc3() ->
                  Z = [1,2,3], 
                  [X || X <- Z, 
                        Z <- Z]. % Z shadowed. Z unused.
           ">>,
           [warn_unused_vars],
           {warnings,[{4,erl_lint,{unused_var,'Z'}},
                      {4,erl_lint,{shadowed_var,'Z',generate}}]}},

          {lc16,
           <<"bin(Z) ->
                  case bar of
                      true ->
                          U = 2;
                      false ->
                          true
                  end,
                  case bar of
                      true ->
                          X = 2;
                      false ->
                          X = 3
                  end,
                  case foo of
                      true ->
                          Y = 3; % Y unused.
                      false ->
                          4
                  end,
                  case foo of
                      1 ->
                          U; % U unsafe.
                      2 ->
                          [Z || <<U:X>> <- Z]; % (X exported.) U unused.
                      3 ->
                          [Z || <<U:X>> <- Z], % (X exported.) U unused.
                          U % U unsafe.
                  end.
           ">>,
           [warn_unused_vars],
           {error,[{22,erl_lint,{unsafe_var,'U',{'case',2}}},
                   {27,erl_lint,{unsafe_var,'U',{'case',2}}}],
            [{16,erl_lint,{unused_var,'Y'}},
     %  {24,erl_lint,{exported_var,'X',{'case',8}}},
             {24,erl_lint,{unused_var,'U'}},
     %  {26,erl_lint,{exported_var,'X',{'case',8}}},
             {26,erl_lint,{unused_var,'U'}}]}},

          {lc17,
           <<"bin(Z) ->
                  %% This used to pass erl_lint...
                  case bar of
                      true ->
                          U = 2;
                      false ->
                          true
                  end,
                  case bar of
                      true ->
                          X = 2;
                      false ->
                          X = 3
                  end,
                  case foo of
                      true ->
                          Y = 3; % Y unused.
                      false ->
                          4
                  end,
                  [Z || <<U:X>> <- Z], % (X exported.) U unused.
                  U. % U unsafe.
           ">>,
           [warn_unused_vars],
           {error,[{22,erl_lint,{unsafe_var,'U',{'case',3}}}],
            [{17,erl_lint,{unused_var,'Y'}},
     %  {21,erl_lint,{exported_var,'X',{'case',9}}},
             {21,erl_lint,{unused_var,'U'}}]}},

          {lc18,
           <<"bin(Z) ->
                  case bar of
                      true ->
                          U = 2;
                      false ->
                          true
                  end,
                  case bar of
                      true ->
                          X = 2;
                      false ->
                          X = 3
                  end,
                  case foo of
                      true ->
                          Y = 3;
                      false ->
                          4
                  end,
                  [B || <<U: % U unused
                          U>> <- X, <<B:Y>> <- Z]. % U unsafe. Y unsafe. 
                                                   % U shadowed. (X exported.)
           ">>,
           [warn_unused_vars],
           {error,[{21,erl_lint,{unsafe_var,'U',{'case',2}}},
                   {21,erl_lint,{unsafe_var,'Y',{'case',14}}}],
            [{20,erl_lint,{unused_var,'U'}}
     %       ,{21,erl_lint,{exported_var,'X',{'case',8}}}
     %       ,{21,erl_lint,{shadowed_var,'U',generate}}
            ]}},

          {lc19,
           <<"p({B,C}) ->
                  <<A:B,A:C>> = <<17:32>>;
              p(B) ->
                  <<A:B>> = <<17:32>>. % A unused.
           ">>,
           [warn_unused_vars],
           {warnings,[{4,erl_lint,{unused_var,'A'}}]}},

          {lc20,
           <<"c({I1,I2}) ->
                  if
                      <<I1:I2>> == <<>> ->
                          foo
                  end;
              c([C1,C2]) -> % C1 unused.
                  case foo of
                      <<C2:C2,
                        C3:C2>> -> % C3 unused.
                          bar
                  end.

           ">>,
           [warn_unused_vars],
           {warnings,[{6,erl_lint,{unused_var,'C1'}},
		      {7,sys_core_fold,no_clause_match},
                      {9,erl_lint,{unused_var,'C3'}}]}},

          {lc21,
           <<"t() ->
                  S = 8,
                  case <<3:8>> of
                      <<X:S>> ->
                          X;
                      <<S:X>> -> % X unbound
                          foo
                  end;
              t() ->
                  S = 8,
                  case <<3:8>> of
                      <<S:S>> ->
                          S;
                      <<Q:32>> -> % Q unused.
                          foo
                  end.
           ">>,
           [warn_unused_vars],
           {error,[{6,erl_lint,{unbound_var,'X'}}],
                  [{14,erl_lint,{unused_var,'Q'}}]}}

          ],
    ?line [] = run(Config, Ts),
    ok.


unused_vars_warn_rec(doc) ->
    "Warnings for unused variables in records.";
unused_vars_warn_rec(suite) -> [];
unused_vars_warn_rec(Config) when is_list(Config) ->
    Ts = [{rec1, % An example provided by Bjorn.
           <<"-record(edge,
                      {ltpr,
                       ltsu,
                       rtpr,
                       rtsu
                      }).

              f1(#edge{ltpr = A, ltsu = A}) ->
                  true;
              f1({Q, Q}) ->
                  true.

              f2(Edge, Etab) ->
                  case gb_trees:lookup(Edge, Etab) of
                      {value,#edge{ltpr=Same,ltsu=Same}} -> ok;
                      {value,_} -> error
                  end.

              bar(Edge, Etab) ->
                  case gb_trees:lookup(Edge, Etab) of
                      {Same,Same} -> ok;
                      {value,#edge{ltpr=Same}} -> ok; % Same unused.
                      {value,_} -> error
                  end.
           ">>,
           [warn_unused_vars],
           {warnings,[{22,erl_lint,{unused_var,'Same'}}]}}],
    ?line [] = run(Config, Ts),
    ok.

unused_vars_warn_fun(doc) ->
    "Warnings for unused variables in records.";
unused_vars_warn_fun(suite) -> [];
unused_vars_warn_fun(Config) when is_list(Config) ->
    Ts = [{fun1,
           <<"a({A,B}) -> % A unused.
                  fun(A) -> B end; % A shadowed. A unused.
              a([A,B]) ->
                  fun(<<A:B>>, % A shadowed. A unused.
                       <<Q:A>>) -> foo % Q unused.
                  end;
              a({A,B,C,D,E}) ->
                  fun(E) when C == <<A:A>>, <<17:B>> == D -> % E shadowed. E unused.
                          foo
                  end,
                  E;
              a([A,B,C,D,E]) -> % E unused.
                  fun() ->
                          (C == <<A:A>>) and (<<17:B>> == D)
                  end.
           ">>,
           [warn_unused_vars],
           {warnings,[{1,erl_lint,{unused_var,'A'}},
                  {2,erl_lint,{unused_var,'A'}},
                  {2,erl_lint,{shadowed_var,'A','fun'}},
                  {4,erl_lint,{unused_var,'A'}},
                  {4,erl_lint,{shadowed_var,'A','fun'}},
                  {5,erl_lint,{unused_var,'Q'}},
                  {8,erl_lint,{unused_var,'E'}},
                  {8,erl_lint,{shadowed_var,'E','fun'}},
		  {8,sys_core_fold,useless_building},
                  {12,erl_lint,{unused_var,'E'}}]}},

          {fun2,
           <<"u() ->
                  case foo of
                      true ->
                          U = 2;
                      false ->
                          true
                  end,
                  fun(U) -> foo end, % U unused.
                  U; % U unsafe.
              u() ->
                  case foo of
                      true ->
                          U = 2;
                      false ->
                          U = 3
                  end,
                  fun(U) -> foo end, % U shadowed. U unused.
                  U;
              u() ->
                  case foo of
                      true ->
                          U = 2; % U unused.
                      false ->
                          U = 3 % U unused.
                  end,
                  fun(U) -> foo end. % U shadowed. U unused.
           ">>,
           [warn_unused_vars],
           {error,[{9,erl_lint,{unsafe_var,'U',{'case',2}}}],
              [{8,erl_lint,{unused_var,'U'}},
               {17,erl_lint,{unused_var,'U'}},
               {17,erl_lint,{shadowed_var,'U','fun'}},
               {22,erl_lint,{unused_var,'U'}},
               {24,erl_lint,{unused_var,'U'}},
               {26,erl_lint,{unused_var,'U'}},
               {26,erl_lint,{shadowed_var,'U','fun'}}]}}
          ],
    ?line [] = run(Config, Ts),
    ok.

unused_vars_OTP_4858(doc) ->
    "Bit syntax, binsize variable used in the same matching.";
unused_vars_OTP_4858(suite) -> [];
unused_vars_OTP_4858(Config) when is_list(Config) ->
    Ts = [{otp_4858,
           <<"objs(<<Size:4/unit:8, B:Size/binary>>) ->
                  B.

              fel(<<Size:4/unit:8, B:BadSize/binary>>) -> % BadSize unbound.
                  BadSize.                                % B, Size unused.

              r9c_highlight() -> % B, Rest unused.
                 <<Size, B:Size/binary,Rest/binary>> = <<2,\"AB\",3,\"CDE\">>.
           ">>,
           [warn_unused_vars],
           {error,[{4,erl_lint,{unbound_var,'BadSize'}}],
              [{4,erl_lint,{unused_var,'B'}},
               {4,erl_lint,{unused_var,'Size'}},
               {8,erl_lint,{unused_var,'B'}},
               {8,erl_lint,{unused_var,'Rest'}}]}}
         ],
    ?line [] = run(Config, Ts),
    ok.

export_vars_warn(doc) ->
    "Warnings for exported variables";
export_vars_warn(suite) -> [];
export_vars_warn(Config) when is_list(Config) ->
    Ts = [{exp1,
           <<"u() ->
                  case foo of
                      1 ->
                          A = 1,
                          B = 2,
                          W = 3, % W unused.
                          Z = 3; % Z unused.
                      2 ->
                          B = 2,
                          Z = 4 % Z unused.
                  end,
                  case bar of
                      true ->
                          A = 17, % A unsafe.
                          X = 3, % X unused.
                          U = 2,
                          U;
                      false ->
                          B = 19, % B exported.
                          U = 3; % U unused.
                      foo ->
                          X = 3,
                          X;
                      bar ->
                          X = 9, % X unused.
                          U = 14 % U unused.
                  end.
           ">>,
           [warn_unused_vars],
           {error,[{14,erl_lint,{unsafe_var,'A',{'case',2}}}],
                  [{6,erl_lint,{unused_var,'W'}},
                   {7,erl_lint,{unused_var,'Z'}},
                   {10,erl_lint,{unused_var,'Z'}},
                   {15,erl_lint,{unused_var,'X'}},
                   {19,erl_lint,{exported_var,'B',{'case',2}}},
                   {20,erl_lint,{unused_var,'U'}},
                   {25,erl_lint,{unused_var,'X'}},
                   {26,erl_lint,{unused_var,'U'}}]}},

          {exp2,
           <<"bin(A) ->
                  receive
                      M ->
                           X = M,
                           Y = M,
                           Z = M
                  end,
                  [B || <<B:X>> <- A], % X exported.
                  Y = B, % Y exported. B unbound.
                  [B || B <- Z]. % Z exported. B shadowed.
           ">>,
           [warn_export_vars],
           {error,[{9,erl_lint,{unbound_var,'B'}}],
                  [{8,erl_lint,{exported_var,'X',{'receive',2}}},
                   {9,erl_lint,{exported_var,'Y',{'receive',2}}},
                   {10,erl_lint,{exported_var,'Z',{'receive',2}}},
                   {10,erl_lint,{shadowed_var,'B',generate}}]}},

          {exp3,
           <<"bin(A) ->
                  receive
                      M ->
                           X = M,
                           Y = M,
                           Z = M
                  end,
                  [B || <<B:X>> <- A], % (X exported.)
                  Y = B, % Y exported. B unbound.
                  [B || B <- Z]. % (Z exported.) B shadowed.
           ">>,
           [],
           {error,[{9,erl_lint,{unbound_var,'B'}}],
                  [{9,erl_lint,{exported_var,'Y',{'receive',2}}},
                   {10,erl_lint,{shadowed_var,'B',generate}}]}}
         ],
    ?line [] = run(Config, Ts),
    ok.

shadow_vars(doc) ->
    "Shadowed variables are tested in other places, but here we test "
	"that the warning can be turned off.";
shadow_vars(suite) -> [];
shadow_vars(Config) when is_list(Config) ->
    Ts = [{shadow1,
	   <<"bin(A) ->
                  receive
                      M ->
                           X = M,
                           Y = M,
                           Z = M
                  end,
                  [B || <<B:X>> <- A],
                  Y = B,
                  [B || B <- Z]. % B shadowed.
           ">>,
	   [nowarn_shadow_vars],
	   {error,[{9,erl_lint,{unbound_var,'B'}}],
	    [{9,erl_lint,{exported_var,'Y',{'receive',2}}}]}}],
    
    ?line [] = run(Config, Ts),
    ok.

unused_import(doc) ->
    "Test that the 'warn_unused_import' option works.";
unused_import(suite) -> [];
unused_import(Config) when is_list(Config) ->
    Ts = [{imp1,
	   <<"-import(lists, [map/2,foldl/3]).
              t(L) ->
                 map(fun(X) -> 2*X end, L).
           ">>,
	   [warn_unused_import],
	   {warnings,[{1,erl_lint,{unused_import,{{foldl,3},lists}}}]}}],
    ?line [] = run(Config, Ts),
    ok.

unused_function(doc) ->
    "Test warnings for unused functions.";
unused_function(suite) -> [];
unused_function(Config) when is_list(Config) ->
    Ts = [{func1,
	   <<"-export([t/1]).
              t(L) ->
                 lists:map(fun(X) -> 2*X end, L).

              fact(N) ->
                fact_1(N, 1).

              fact_1(1, P) -> P;
              fact_1(N, P) -> fact_1(N-1, P*N).
           ">>,
	   {[]},				%Tuple indicates no 'export_all'.
	   {warnings,[{5,erl_lint,{unused_function,{fact,1}}},
		      {8,erl_lint,{unused_function,{fact_1,2}}}]}},

	  %% Turn off warnings for unused functions.
	  {func2,
	   <<"-export([t/1]).
              t(L) ->
                 lists:map(fun(X) -> 2*X end, L).

              b(X) ->
                32*X.
           ">>,
	   {[nowarn_unused_function]},         %Tuple indicates no 'export_all'.
	   []},

	  %% Turn off warnings for unused functions using a -compile() directive.
	  {func3,
	   <<"-export([t/1]).
              -compile(nowarn_unused_function).

              t(L) ->
                 lists:map(fun(X) -> 2*X end, L).

              b(X) ->
                32*X.
           ">>,
	   {[]},		     %Tuple indicates no 'export_all'.
	   []}],

    ?line [] = run(Config, Ts),
    ok.
    
unsafe_vars(doc) ->
    "OTP-4671. Errors for unsafe variables";
unsafe_vars(suite) -> [];
unsafe_vars(Config) when is_list(Config) ->
    Ts = [{unsafe1,
           <<"t() ->
                 (X = true) orelse (Y = false),
                  Y.
           ">>,
           [warn_unused_vars],
           {error,[{3,erl_lint,{unsafe_var,'Y',{'orelse',2}}}],
            [{2,erl_lint,{unused_var,'X'}}]}},
          {unsafe2,
           <<"t2() ->
                  (X = true) orelse (Y = false),
                  X.
           ">>,
           [warn_unused_vars],
           {warnings,[{2,erl_lint,{unused_var,'Y'}}]}},
          {unsafe3,
           <<"t3() ->
                  (X = true) andalso (Y = false),
                  Y.
           ">>,
           [warn_unused_vars],
           {error,[{3,erl_lint,{unsafe_var,'Y',{'andalso',2}}}],
            [{2,erl_lint,{unused_var,'X'}}]}},
          {unsafe4,
           <<"t4() ->
                  (X = true) andalso (true = X),
                  X.
           ">>,
           [warn_unused_vars],
           []},
          {unsafe5,
           <<"t5() ->
                  Y = 3,
                  (X = true) andalso (X = true),
                  {X,Y}.
           ">>,
           [warn_unused_vars],
           []},
          {unsafe6,
           <<"t6() ->
                  X = true,
                  (X = true) andalso (true = X),
                  X.
           ">>,
           [warn_unused_vars],
           []},
          {unsafe7,
           <<"t7() ->
                  (if true -> X = 3; false -> true end) 
                      andalso (X > 2),
                  X.
           ">>,
           [warn_unused_vars],
           {errors,[{3,erl_lint,{unsafe_var,'X',{'if',2}}},
                    {4,erl_lint,{unsafe_var,'X',{'if',2}}}],
            []}}
         ],
    ?line [] = run(Config, Ts),
    ok.

unsafe_vars2(doc) ->
    "OTP-4831, seq8202. No warn_unused_vars and unsafe variables";
unsafe_vars2(suite) -> [];
unsafe_vars2(Config) when is_list(Config) ->
    Ts = [{unsafe2_1,
           <<"foo(State) ->
                  case State of
                      true ->
                          if
                              false -> ok;
                              true ->  State1=State
                          end
                  end,
                  State1. % unsafe
           ">>,
           [warn_unused_vars],
           {errors,[{9,erl_lint,{unsafe_var,'State1',{'if',4}}}],[]}},
          {unsafe2_2,
           <<"foo(State) ->
                  case State of
                      true ->
                          if
                              false -> ok;
                              true ->  State1=State
                          end
                  end,
                  State1. % unsafe
           ">>,
           [],
           {errors,[{9,erl_lint,{unsafe_var,'State1',{'if',4}}}],[]}}
         ],
    ?line [] = run(Config, Ts),
    ok.

unsafe_vars_try(doc) ->
    "Errors for unsafe variables in try/catch constructs.";
unsafe_vars_try(suite) -> [];
unsafe_vars_try(Config) when is_list(Config) ->
    Ts = [{unsafe_try1,
	   <<"foo2() ->
                try self()
                catch
                  Class:Data -> Result={Class,Data}
                end,
                Result.
              foo3a() ->
                try self() of
                  R -> R
                catch
                  Class:Data -> Result={Class,Data}
                end,
                Result.
              foo3b() ->
                try self() of
                  Result -> ok
                catch
                  Class:Data -> {Class,Data}
                end,
                Result.
           ">>,
	   [],
	   {errors,[{6,erl_lint,{unsafe_var,'Result',{'try',2}}},
		    {13,erl_lint,{unsafe_var,'Result',{'try',8}}},
		    {20,erl_lint,{unsafe_var,'Result',{'try',15}}}],
	    []}},
	  {unsafe_try2,
	   <<"foo1a() ->
                Try = 
                  try self()
                  catch
                    Class:Data -> Rc={Class,Data}
                  after
                    Ra=ok
                  end,
                {Try,Rc,Ra}.
              foo1b() ->
                Try = 
                  try self() of
                    R -> R
                  catch
                    Class:Data -> Rc={Class,Data}
                  after
                    Ra=R
                  end,
                {Try,Rc,Ra}.
              foo2() ->
                Try = 
                  try self() of
                    R -> Ro=R
                  catch
                    Class:Data -> {Class,Data}
                  after
                    Ra=R
                  end,
                {Try,Ro,Ra}.
              foo3() ->
                Try = 
                  try self() of
                    R -> Ro=R
                  catch
                    Class:Data -> Rc={Class,Data}
                  after
                    Ra=R
                  end,
                {Try,R,Ro,Rc,Ra}.
           ">>,
	   [],
	   {errors,[{9,erl_lint,{unsafe_var,'Ra',{'try',3}}},
                    {9,erl_lint,{unsafe_var,'Rc',{'try',3}}},
		    {17,erl_lint,{unsafe_var,'R',{'try',12}}},
		    {19,erl_lint,{unsafe_var,'Ra',{'try',12}}},
		    {19,erl_lint,{unsafe_var,'Rc',{'try',12}}},
		    {27,erl_lint,{unsafe_var,'R',{'try',22}}},
		    {29,erl_lint,{unsafe_var,'Ra',{'try',22}}},
		    {29,erl_lint,{unsafe_var,'Ro',{'try',22}}},
		    {37,erl_lint,{unsafe_var,'R',{'try',32}}},
		    {39,erl_lint,{unsafe_var,'R',{'try',32}}},
		    {39,erl_lint,{unsafe_var,'Ra',{'try',32}}},
                    {39,erl_lint,{unsafe_var,'Rc',{'try',32}}},
                    {39,erl_lint,{unsafe_var,'Ro',{'try',32}}}],
	    []}},
	  {unsafe_try3,
	   <<"foo1(X) ->
                Try = 
                  try R=self()
                  catch
                    Class:Data -> Rc={X,R,Class,Data}
                  end,
                {X,Try,Rc}.
	      foo2(X) ->
                Try = 
                  try R=self() of
                    RR -> Ro={X,R,RR}
                  catch
                    Class:Data -> {X,R,RR,Ro,Class,Data}
                  end,
                {X,Try,R,RR,Ro,Class,Data}.
	      foo3(X) ->
                Try = 
                  try R=self() of
                    RR -> {X,R,RR}
                  catch
                    Class:Data -> {X,R,RR,Class,Data}
                  after
                    Ra={X,R,RR,Class,Data}
                  end,
                {X,Try,R,RR,Ra,Class,Data}.
           ">>,
	   [],
	   {errors,[{5,erl_lint,{unsafe_var,'R',{'try',3}}},
		    {7,erl_lint,{unsafe_var,'Rc',{'try',3}}},
		    {11,erl_lint,{unsafe_var,'R',{'try',10}}},
		    {13,erl_lint,{unbound_var,'RR'}},
		    {13,erl_lint,{unbound_var,'Ro'}},
		    {13,erl_lint,{unsafe_var,'R',{'try',10}}},
		    {15,erl_lint,{unsafe_var,'Class',{'try',10}}},
		    {15,erl_lint,{unsafe_var,'Data',{'try',10}}},
		    {15,erl_lint,{unsafe_var,'R',{'try',10}}},
		    {15,erl_lint,{unsafe_var,'RR',{'try',10}}},
		    {15,erl_lint,{unsafe_var,'Ro',{'try',10}}},
		    {19,erl_lint,{unsafe_var,'R',{'try',18}}},
		    {21,erl_lint,{unbound_var,'RR'}},
		    {21,erl_lint,{unsafe_var,'R',{'try',18}}},
		    {23,erl_lint,{unsafe_var,'Class',{'try',18}}},
		    {23,erl_lint,{unsafe_var,'Data',{'try',18}}},
		    {23,erl_lint,{unsafe_var,'R',{'try',18}}},
		    {23,erl_lint,{unsafe_var,'RR',{'try',18}}},
		    {25,erl_lint,{unsafe_var,'Class',{'try',18}}},
                    {25,erl_lint,{unsafe_var,'Data',{'try',18}}},
		    {25,erl_lint,{unsafe_var,'R',{'try',18}}},
		    {25,erl_lint,{unsafe_var,'RR',{'try',18}}},
		    {25,erl_lint,{unsafe_var,'Ra',{'try',18}}}],
	    []}},
	  {unsafe_try4,
	   <<"foo1(X) ->
                Try = 
                  try R=self() of
                    RR -> Ro={X,R,RR}
                  catch
                    Class:Data -> Rc={X,R,RR,Ro,Class,Data}
                  after
                    Ra={X,R,RR,Ro,Rc,Class,Data}
                  end,
                {X,Try,R,RR,Ro,Rc,Ra,Class,Data}.
           ">>,
	   [],
	   {errors,[{4,erl_lint,{unsafe_var,'R',{'try',3}}},
		    {6,erl_lint,{unbound_var,'RR'}},
		    {6,erl_lint,{unbound_var,'Ro'}},
		    {6,erl_lint,{unsafe_var,'R',{'try',3}}},
		    {8,erl_lint,{unsafe_var,'Class',{'try',3}}},
		    {8,erl_lint,{unsafe_var,'Data',{'try',3}}},
		    {8,erl_lint,{unsafe_var,'R',{'try',3}}},
		    {8,erl_lint,{unsafe_var,'RR',{'try',3}}},
		    {8,erl_lint,{unsafe_var,'Rc',{'try',3}}},
		    {8,erl_lint,{unsafe_var,'Ro',{'try',3}}},
		    {10,erl_lint,{unsafe_var,'Class',{'try',3}}},
                    {10,erl_lint,{unsafe_var,'Data',{'try',3}}},
		    {10,erl_lint,{unsafe_var,'R',{'try',3}}},
		    {10,erl_lint,{unsafe_var,'RR',{'try',3}}},
		    {10,erl_lint,{unsafe_var,'Ra',{'try',3}}},
		    {10,erl_lint,{unsafe_var,'Rc',{'try',3}}},
		    {10,erl_lint,{unsafe_var,'Ro',{'try',3}}}],
	    []}}],
        ?line [] = run(Config, Ts),
    ok.

guard(doc) ->
    "OTP-4670. Guards, is_record in particular.";
guard(suite) -> [];
guard(Config) when is_list(Config) ->
    %% Well, these could be plain code...
    Ts = [{guard1,
           <<"-record(apa, {}).
              t(A) when atom(A) ->
                  atom;
              t(A) when binary(A) ->
                  binary;
              t(A) when constant(A) ->
                  constant;
              t(A) when float(A) ->
                  float;
              t(A) when function(A) ->
                  function;
              t(A) when integer(A) ->
                  integer;
              t(A) when is_atom(A) ->
                  is_atom;
              t(A) when is_binary(A) ->
                  is_binary;
              t(A) when is_constant(A) ->
                  is_constant;
              t(A) when is_float(A) ->
                  is_float;
              t(A) when is_function(A) ->
                  is_function;
              t(A) when is_integer(A) ->
                  is_integer;
              t(A) when is_list(A) ->
                  is_list;
              t(A) when is_number(A) ->
                  is_number;
              t(A) when is_pid(A) ->
                  is_pid;
              t(A) when is_port(A) ->
                  is_port;
              t(A) when is_record(A, apa) ->
                  is_record;
              t(A) when is_record(A, apa, 1) ->
                  is_record;
              t(A) when is_reference(A) ->
                  is_reference;
              t(A) when is_tuple(A) ->
                  is_tuple;
              t(A) when list(A) ->
                  list;
              t(A) when number(A) ->
                  number;
              t(A) when pid(A) ->
                  pid;
              t(A) when port(A) ->
                  port;
              t(A) when record(A, apa) ->
                  record;
              t(A) when reference(A) ->
                  reference;
              t(A) when tuple(A) ->
                  tuple.
           ">>,
           [nowarn_obsolete_guard],
           {error,
	    [{6,erl_lint,illegal_guard_expr},{18,erl_lint,illegal_guard_expr}],
	    [{18,erl_lint,{removed,{erlang,is_constant,1},
			   "Removed in R13B"}}]}},
          {guard2,
           <<"-record(apa,{}).
              t1(A) when atom(A), atom(A) ->
                  atom;
              t1(A) when binary(A), binary(A) ->
                  binary;
              t1(A) when constant(A), constant(A) ->
                  constant;
              t1(A) when float(A), float(A) ->
                  float;
              t1(A) when function(A), function(A) ->
                  function;
              t1(A) when integer(A), integer(A) ->
                  integer;
              t1(A) when is_atom(A), is_atom(A) ->
                  is_atom;
              t1(A) when is_binary(A), is_binary(A) ->
                  is_binary;
              t1(A) when is_constant(A), is_constant(A) ->
                  is_constant;
              t1(A) when is_float(A), is_float(A) ->
                  is_float;
              t1(A) when is_function(A), is_function(A) ->
                  is_function;
              t1(A) when is_integer(A), is_integer(A) ->
                  is_integer;
              t1(A) when is_list(A), is_list(A) ->
                  is_list;
              t1(A) when is_number(A), is_number(A) ->
                  is_number;
              t1(A) when is_pid(A), is_pid(A) ->
                  is_pid;
              t1(A) when is_port(A), is_port(A) ->
                  is_port;
              t1(A) when is_record(A, apa), is_record(A, apa) ->
                  is_record;
              t1(A) when is_record(A, apa, 1), is_record(A, apa, 1) ->
                  is_record;
              t1(A) when is_reference(A), is_reference(A) ->
                  is_reference;
              t1(A) when is_tuple(A), is_tuple(A) ->
                  is_tuple;
              t1(A) when list(A), list(A) ->
                  list;
              t1(A) when number(A), number(A) ->
                  number;
              t1(A) when pid(A), pid(A) ->
                  pid;
              t1(A) when port(A), port(A) ->
                  port;
              t1(A) when record(A, apa), record(A, apa) ->
                  record;
              t1(A) when reference(A), reference(A) ->
                  reference;
              t1(A) when tuple(A), tuple(A) ->
                  tuple.
           ">>,
           [nowarn_obsolete_guard],
	   {error,[{6,erl_lint,illegal_guard_expr},
		   {6,erl_lint,illegal_guard_expr},
		   {18,erl_lint,illegal_guard_expr},
		   {18,erl_lint,illegal_guard_expr}],
	    [{18,erl_lint,{removed,{erlang,is_constant,1},
			   "Removed in R13B"}},
             {18,erl_lint,{removed,{erlang,is_constant,1},
			   "Removed in R13B"}}]}},
          {guard3,
           <<"-record(apa,{}).
              t2(A) when atom(A); atom(A) ->
                  atom;
              t2(A) when binary(A); binary(A) ->
                  binary;
              t2(A) when float(A); float(A) ->
                  float;
              t2(A) when function(A); function(A) ->
                  function;
              t2(A) when integer(A); integer(A) ->
                  integer;
              t2(A) when is_atom(A); is_atom(A) ->
                  is_atom;
              t2(A) when is_binary(A); is_binary(A) ->
                  is_binary;
              t2(A) when is_float(A); is_float(A) ->
                  is_float;
              t2(A) when is_function(A); is_function(A) ->
                  is_function;
              t2(A) when is_integer(A); is_integer(A) ->
                  is_integer;
              t2(A) when is_list(A); is_list(A) ->
                  is_list;
              t2(A) when is_number(A); is_number(A) ->
                  is_number;
              t2(A) when is_pid(A); is_pid(A) ->
                  is_pid;
              t2(A) when is_port(A); is_port(A) ->
                  is_port;
              t2(A) when is_record(A, apa); is_record(A, apa) ->
                  is_record;
              t2(A) when is_record(A, gurka, 1); is_record(A, gurka, 1) ->
                  is_record;
              t2(A) when is_reference(A); is_reference(A) ->
                  is_reference;
              t2(A) when is_tuple(A); is_tuple(A) ->
                  is_tuple;
              t2(A) when list(A); list(A) ->
                  list;
              t2(A) when number(A); number(A) ->
                  number;
              t2(A) when pid(A); pid(A) ->
                  pid;
              t2(A) when port(A); port(A) ->
                  port;
              t2(A) when record(A, apa); record(A, apa) ->
                  record;
              t2(A) when reference(A); reference(A) ->
                  reference;
              t2(A) when tuple(A); tuple(A) ->
                  tuple.
           ">>,
           [nowarn_obsolete_guard],
	   []},
          {guard4,
           <<"-record(apa, {}).
              t3(A) when float(A) or float(A) -> % coercing... (badarg)
                  float;
              t3(A) when is_atom(A) or is_atom(A) ->
                  is_atom;
              t3(A) when is_binary(A) or is_binary(A) ->
                  is_binary;
              t3(A) when is_float(A) or is_float(A) ->
                  is_float;
              t3(A) when is_function(A) or is_function(A) ->
                  is_function;
              t3(A) when is_integer(A) or is_integer(A) ->
                  is_integer;
              t3(A) when is_list(A) or is_list(A) ->
                  is_list;
              t3(A) when is_number(A) or is_number(A) ->
                  is_number;
              t3(A) when is_pid(A) or is_pid(A) ->
                  is_pid;
              t3(A) when is_port(A) or is_port(A) ->
                  is_port;
              t3(A) when is_record(A, apa) or is_record(A, apa) ->
                  is_record;
              t3(A) when is_record(A, apa, 1) or is_record(A, apa, 1) ->
                  is_record;
              t3(A) when is_reference(A) or is_reference(A) ->
                  is_reference;
              t3(A) when is_tuple(A) or is_tuple(A) ->
                  is_tuple.
           ">>,
           [nowarn_obsolete_guard],
           []}],
    ?line [] = run(Config, Ts),
    Ts1 = [{guard5,
            <<"-record(apa, {}).
               t3(A) when record(A, {apa}) ->
                   foo;
               t3(A) when is_record(A, {apa}) ->
                   foo;
               t3(A) when erlang:is_record(A, {apa}) ->
                   foo;
               t3(A) when {erlang,is_record}(A, {apa}) ->
                   foo;
               t3(A) when is_record(A, {apa}, 1) ->
                   foo;
               t3(A) when erlang:is_record(A, {apa}, 1) ->
                   foo;
               t3(A) when {erlang,is_record}(A, {apa}, 1) ->
                   foo;
               t3(A) when is_record(A, apa, []) ->
                   foo;
               t3(A) when erlang:is_record(A, apa, []) ->
                   foo;
               t3(A) when {erlang,is_record}(A, apa, []) ->
                   foo;
               t3(A) when record(A, apa) ->
                   foo;
               t3(A) when is_record(A, apa) ->
                   foo;
               t3(A) when erlang:is_record(A, apa) ->
                   foo;
               t3(A) when {erlang,is_record}(A, apa) ->
                   foo.
            ">>,
            [warn_unused_vars, nowarn_obsolete_guard],
            {errors,[{2,erl_lint,illegal_guard_expr},
                     {4,erl_lint,illegal_guard_expr},
                     {6,erl_lint,illegal_guard_expr},
                     {8,erl_lint,illegal_guard_expr},
		     {10,erl_lint,illegal_guard_expr},
		     {12,erl_lint,illegal_guard_expr},
		     {14,erl_lint,illegal_guard_expr},
		     {16,erl_lint,illegal_guard_expr},
		     {18,erl_lint,illegal_guard_expr},
		     {20,erl_lint,illegal_guard_expr}],
             []}},
           {guard6,
            <<"-record(apa,{a=a,b=foo:bar()}).
              apa() ->
                 [X || X <- [], #apa{a = a} == {r,X,foo}];
              apa() ->
                 [X || X <- [], #apa{b = b} == {r,X,foo}];
              apa() ->
                 [X || X <- [], #ful{a = a} == {r,X,foo}].
            ">>,
            [],
            {errors,[{7,erl_lint,{undefined_record,ful}}],
             []}},
           {guard7,
            <<"-record(apa,{}).
               t() ->
               [X || X <- [1,#apa{},3], (3+is_record(X, apa)) or 
                                        (is_record(X, apa)*2)].
            ">>,
            [],
            []}],
    ?line [] = run(Config, Ts1),
    ok.

otp_4886(doc) ->
    "OTP-4886. Calling is_record with given record name.";
otp_4886(suite) -> [];
otp_4886(Config) when is_list(Config) ->
    Ts = [{otp_4886,
           <<"t() ->
                  X = {foo},
                  is_record(X, foo),
                  erlang:is_record(X, foo),
                  {erlang,is_record}(X, foo),
                  %% Note: is_record/3 does not verify that the record is defined,
                  %% so the following lines should give no errors.
                  is_record(X, foo, 1),
                  erlang:is_record(X, foo, 1),
                  {erlang,is_record}(X, foo, 1).
             ">>,
           [],
           {errors,[{3,erl_lint,{undefined_record,foo}},
                    {4,erl_lint,{undefined_record,foo}},
                    {5,erl_lint,{undefined_record,foo}}],
            []}}],
    ?line [] = run(Config, Ts),
    ok.

otp_4988(doc) ->
    "OTP-4988. Error when in-lining non-existent functions.";
otp_4988(suite) -> [];
otp_4988(Config) when is_list(Config) ->
    Ts = [{otp_4988,
           <<"-compile({inline, [{f,3},{f,4},{f,2},{f,a},{1,foo}]}).
              -compile({inline, {g,1}}).
              -compile({inline, {g,12}}).
              -compile(inline).
              -compile({inline_size,100}).

              f(A, B) ->
                  {g(A), B}.

              g(A) ->
                  {A}.
             ">>,
           [],
           {errors,[{1,erl_lint,{bad_inline,{1,foo}}},
                    {1,erl_lint,{bad_inline,{f,3}}},
                    {1,erl_lint,{bad_inline,{f,4}}},
                    {1,erl_lint,{bad_inline,{f,a}}},
                    {3,erl_lint,{bad_inline,{g,12}}}],
            []}}],
    ?line [] = run(Config, Ts),
    ok.

otp_5091(doc) ->
    "OTP-5091. Patterns and the bit syntax: invalid warnings.";
otp_5091(suite) -> [];
otp_5091(Config) when is_list(Config) ->
    Ts = [{otp_5091_1,
           <<"t() ->
                 [{Type, Value} || <<Type:16, _Len:16, 
                                    Value:_Len/binary>> <- []].
             ">>,
           [],
           []},
          {otp_5091_2,
           <<"t() ->
                 %% This one has always been handled OK:
                 <<Type:16, _Len:16, 
                      Value:_Len/binary>> = <<18:16, 9:16, \"123456789\">>,
                 {Type, Value}.
             ">>,
           [],
           []},
          {otp_5091_3,
           <<"t() ->
                 fun(<<Type:16, _Len:16, Value:_Len/binary>>) ->
                     {Type, Value}
                 end.
             ">>,
           [],
           []},
          {otp_5091_4,
           <<"t() ->
                 L = 8,
                 F = fun(<<A:L,B:A>>) -> B end,
                 F(<<16:8, 7:16>>).
             ">>,
           [],
           []},
          {otp_5091_5,
           <<"t() ->
                 L = 8,
                 F = fun(<<L: % L shadowed.
                            L,
                           B:
                            L>>) -> B end,
                 F(<<16:8, 7:16>>).
             ">>,
           [],
           {warnings,[{3,erl_lint,{shadowed_var,'L','fun'}}]}},
          {otp_5091_6,
           <<"t(A) ->
                 (fun(<<L:16,M:L,N:M>>) -> ok end)(A).
             ">>,
           [],
           {warnings,[{2,erl_lint,{unused_var,'N'}}]}},
          {otp_5091_7,
           <<"t() ->
                  U = 8, 
                  (fun(<<U: % U shadowed.
                          U>>) -> U end)(<<32:8>>).
             ">>,
           [],
           {warnings,[{3,erl_lint,{shadowed_var,'U','fun'}}]}},
          {otp_5091_8,
           <<"t() ->
                  [X || <<A:8,
                          B:A>> <- [],
                        <<X:8>> <- [B]].
             ">>,
           [],
           []},
          {otp_5091_9,
           <<"t() ->
                  L = 8,
                  F = fun(<<L: % Shadow.
                           L,
                           L:
                           L,
                           L:
                           L
                           >>) ->
                              L
                      end,
                  F(<<16:8, 8:16, 32:8>>).
             ">>,
           [],
           {warnings,[{3,erl_lint,{shadowed_var,'L','fun'}}]}},
          {otp_5091_10,
           <<"t() ->
                L = 8, <<A:L,B:A>> = <<16:8, 7:16>>, B.
             ">>,
           [],
           []},
          {otp_5091_11,
           <<"t() ->
                fun(<<L:16,L:L,L:L>>) -> ok end.
             ">>,
           [],
           []},
          {otp_5091_12,
           <<"t([A,B]) ->
                 fun(<<A:B>>, % A shadowed and unused
                     <<Q:A>>) -> foo % Q unused. 'outer' A is used.
                 end.
             ">>,
           [],
           {warnings,[{2,erl_lint,{unused_var,'A'}},
                      {2,erl_lint,{shadowed_var,'A','fun'}},
                      {3,erl_lint,{unused_var,'Q'}}]}},
          {otp_5091_13,
           <<"t([A,B]) -> % A unused, B unused
                 fun({A,B}, % A shadowed, B unused, B shadowed
                     {Q,A}) -> foo % Q unused. 'inner' A is used
                 end.
             ">>,
           [],
           {warnings,[{1,erl_lint,{unused_var,'A'}},
                      {1,erl_lint,{unused_var,'B'}},
                      {2,erl_lint,{unused_var,'B'}},
                      {2,erl_lint,{shadowed_var,'A','fun'}},
                      {2,erl_lint,{shadowed_var,'B','fun'}},
                      {3,erl_lint,{unused_var,'Q'}}]}},
          {otp_5091_14,
           <<"t() ->
                 A = 4,
                 fun(<<A: % shadowed, unused
                       A>>) -> 2 end.
             ">>,
           [],
           {warnings,[{3,erl_lint,{unused_var,'A'}},
                      {3,erl_lint,{shadowed_var,'A','fun'}}]}},
          {otp_5091_15,
           <<"t() ->
                 A = 4, % unused
                 fun(<<A:8, % shadowed
                       16:A>>) -> 2 end.
             ">>,
           [],
           {warnings,[{2,erl_lint,{unused_var,'A'}},
                      {3,erl_lint,{shadowed_var,'A','fun'}}]}},
          {otp_5091_16,
           <<"t() ->
                 A = 4,
                 fun(<<8:A, % 
                       A:8>>) -> 7 end. % shadowed, unused
             ">>,
           [],
           {warnings,[{4,erl_lint,{unused_var,'A'}},
                      {4,erl_lint,{shadowed_var,'A','fun'}}]}},
          {otp_5091_17,
           <<"t() ->
                 L = 16,
                 fun(<<L: % shadow
                       L>>, % 'outer' L
                     <<L: % shadow and match
                       L>>) -> % 'outer' L
                         a
                 end.
             ">>,
           [],
           {warnings,[{3,erl_lint,{shadowed_var,'L','fun'}}]}},
          {otp_5091_18,
           <<"t() ->
                 L = 4,      % L unused
                 fun({L,     % L shadowed
                      L},
                     {L,
                      L}) ->
                         a
                 end.
             ">>,
           [],
           {warnings,[{2,erl_lint,{unused_var,'L'}},
                      {3,erl_lint,{shadowed_var,'L','fun'}}]}},
          {otp_5091_19,
           <<"t() ->
                 L = 4,
                 [L || <<L: % shadowed
                         L, 
                         L:
                         L>> <- []].
             ">>,
           [],
           {warnings,[{3,erl_lint,{shadowed_var,'L',generate}}]}},
          {otp_5091_20,
           <<"t() ->
                 L = 4, % L unused.
                 [1 || L <- []]. % L unused, L shadowed.
             ">>,
           [],
           {warnings,[{2,erl_lint,{unused_var,'L'}},
                      {3,erl_lint,{unused_var,'L'}},
                      {3,erl_lint,{shadowed_var,'L',generate}}]}},
          {otp_5091_21,
           <<"t() ->
                 L = 4,
                 [1 || L <- [L]]. % L shadowed. L unused.
             ">>,
           [],
           {warnings,[{3,erl_lint,{unused_var,'L'}},
                      {3,erl_lint,{shadowed_var,'L',generate}}]}},
          {otp_5091_22,
           <<"t() ->
                 L = 4, % unused
                 fun(L) -> L end. % shadowed
             ">>,
           [],
           {warnings,[{2,erl_lint,{unused_var,'L'}},
                      {3,erl_lint,{shadowed_var,'L','fun'}}]}},
          {otp_5091_23,
           <<"t([A,A]) -> a.">>, [], []},
          {otp_5091_24,
           <<"t({A,A}) -> a.">>, [], []},
          {otp_5091_25,
           <<"-record(r, {f1,f2}).
              t(#r{f1 = A, f2 = A}) -> a.">>, [], []}],

    ?line [] = run(Config, Ts),
    ok.

otp_5276(doc) ->
    "OTP-5276. Check the 'deprecated' attributed.";
otp_5276(suite) -> [];
otp_5276(Config) when is_list(Config) ->
    Ts = [{otp_5276_1,
          <<"-deprecated([{frutt,0,next_version}]).
             -deprecated([{does_not_exist,1}]).
             -deprecated('foo bar').
             -deprecated(module).
             -deprecated([{f,'_'}]).
             -deprecated([{t,0}]).
             -deprecated([{t,'_',eventually}]).
             -deprecated([{'_','_',never}]).
             -deprecated([{{badly,formed},1}]).
             -deprecated([{'_','_',next_major_release}]).
             -export([t/0]).
             frutt() -> ok.
             t() -> ok.
            ">>,
           {[]},
           {error,[{1,erl_lint,{bad_deprecated,{frutt,0}}},
                   {2,erl_lint,{bad_deprecated,{does_not_exist,1}}},
                   {3,erl_lint,{invalid_deprecated,'foo bar'}},
                   {5,erl_lint,{bad_deprecated,{f,'_'}}},
                   {8,erl_lint,{invalid_deprecated,{'_','_',never}}},
                   {9,erl_lint,{invalid_deprecated,{{badly,formed},1}}}],
            [{12,erl_lint,{unused_function,{frutt,0}}}]}}],
    ?line [] = run(Config, Ts),
    ok.

otp_5917(doc) ->
    "OTP-5917. Check the 'deprecated' attributed.";
otp_5917(suite) -> [];
otp_5917(Config) when is_list(Config) ->
    Ts = [{otp_5917_1,
          <<"-compile(export_all).

             -deprecated({t,0}).

             t() ->
                 foo.
            ">>,
           {[]},
           []}],
    ?line [] = run(Config, Ts),
    ok.

otp_6585(doc) ->
    "OTP-6585. Check the deprecated guards list/1, pid/1, ....";
otp_6585(suite) -> [];
otp_6585(Config) when is_list(Config) ->
    Ts = [{otp_6585_1,
          <<"-compile(export_all).

             -record(r, {}).

             f(A) when list(A) -> list;
             f(R) when record(R, r) -> rec;
             f(P) when pid(P) -> pid.

             t() ->
                 f([]).
            ">>,
           [warn_obsolete_guard],
           {warnings,[{5,erl_lint,{obsolete_guard,{list,1}}},
                      {6,erl_lint,{obsolete_guard,{record,2}}},
                      {7,erl_lint,{obsolete_guard,{pid,1}}}]}}],
    ?line [] = run(Config, Ts),
    ok.

otp_5338(doc) ->
    "OTP-5338. Bad warning in record initialization.";
otp_5338(suite) -> [];
otp_5338(Config) when is_list(Config) ->
    %% OTP-5878: variables like X are no longer allowed in initialisations
    Ts = [{otp_5338,
          <<"-record(c, {a = <<X:7/binary-unit:8>>}).
              t() ->
                  X = <<\"hejsans\">>,
                  #c{}.
            ">>,
           [],
           {error,[{1,erl_lint,{unbound_var,'X'}}],
                  [{3,erl_lint,{unused_var,'X'}}]}}],
    ?line [] = run(Config, Ts),
    ok.

otp_5362(doc) ->
    "OTP-5362. deprecated_function, "
    "{nowarn_unused_funtion,FAs}, 'better' line numbers.";
otp_5362(suite) -> [];
otp_5362(Config) when is_list(Config) ->
    Ts = [{otp_5362_1,
          <<"-include_lib(\"stdlib/include/qlc.hrl\").

             -file(?FILE, 1000).

             t() ->
                 qlc:q([X || X <- [],
                             begin A = 3, true end]).
            ">>,
           {[warn_unused_vars]},
           {warnings,[{1002,erl_lint,{unused_function,{t,0}}},
                      {1004,erl_lint,{unused_var,'A'}}]}},

          {otp_5362_2,
          <<"-export([inline/0]).

             -import(lists.foo, [a/1,b/1]). % b/1 is not used

             -compile([{inline,{inl,7}}]).    % undefined
             -compile([{inline,[{inl,17}]}]). % undefined
             -compile([{inline,{inl,1}}]).    % OK

             foop() ->   % unused function
                 a([]),  % used import, OK
                 fipp(). % undefined

             inline() ->
                 inl(foo).

             inl(_) ->
                 true.

             not_used() ->      % unused function
                 true.

             -compile({nowarn_unused_function,[{and_not_used,2}]}). % unknown 
             and_not_used(_) -> % unused function
                 foo.

             -compile({nowarn_unused_function,{unused_function,2}}).
             unused_function(_, _) ->
                 ok.
           ">>,
          {[warn_unused_vars, warn_unused_import]},
           {error,[{5,erl_lint,{bad_inline,{inl,7}}},
                   {6,erl_lint,{bad_inline,{inl,17}}},
                   {11,erl_lint,{undefined_function,{fipp,0}}},
                   {22,erl_lint,{bad_nowarn_unused_function,{and_not_used,2}}}],
            [{3,erl_lint,{unused_import,{{b,1},'lists.foo'}}},
             {9,erl_lint,{unused_function,{foop,0}}},
             {19,erl_lint,{unused_function,{not_used,0}}},
             {23,erl_lint,{unused_function,{and_not_used,1}}}]}},

          {otp_5362_3,
           <<"-record(a, {x,
                          x}).
              -record(a, {x,
                          X}). % erl_parse
              -record(a, [x,
                          x]). % erl_parse
              -record(ok, {a,b}).

              -record(r, {a = #ok{}, 
                          b = (#ok{})#ok.a}).

              t() ->
                  {#a{},
                   #nix{},
                   #ok{nix = []},
                   #ok{Var = 4}, 
                   #r{}
                  }.
           ">>,
           {[nowarn_unused_function]},
           {errors2, [{4,erl_parse,"bad record field"},
                      {5,erl_parse,"bad record declaration"}],
                     [{2,erl_lint,{redefine_field,a,x}},
                      {14,erl_lint,{undefined_record,nix}},
                      {15,erl_lint,{undefined_field,ok,nix}},
                      {16,erl_lint,{field_name_is_variable,ok,'Var'}}]}},

	  %% Nowarn_bif_clash has changed behaviour as local functions
	  %% nowdays supersede auto-imported BIFs, why nowarn_bif_clash in itself generates an error
	  %% (OTP-8579) /PaN
          {otp_5362_4,
           <<"-compile(nowarn_deprecated_function).
              -compile(nowarn_bif_clash).
              spawn(A) ->
                  erlang:hash(A, 3000),
                  spawn(A).
           ">>,
           {[nowarn_unused_function, 
             warn_deprecated_function,
             warn_bif_clash]},
           {error,
            [{5,erl_lint,{call_to_redefined_old_bif,{spawn,1}}}],
	    [{4,erl_lint,{deprecated,{erlang,hash,2},{erlang,phash2,2},
			  "in a future release"}}]}},

          {otp_5362_5,
           <<"-compile(nowarn_deprecated_function).
              -compile(nowarn_bif_clash).
              spawn(A) ->
                  erlang:hash(A, 3000),
                  spawn(A).
           ">>,
           {[nowarn_unused_function]},
	   {errors,
            [{2,erl_lint,disallowed_nowarn_bif_clash}],[]}},

          %% The special nowarn_X are not affected by general warn_X.
          {otp_5362_6,
           <<"-compile({nowarn_deprecated_function,{erlang,hash,2}}).
              -compile({nowarn_bif_clash,{spawn,1}}).
              spawn(A) ->
                  erlang:hash(A, 3000),
                  spawn(A).
           ">>,
           {[nowarn_unused_function, 
             warn_deprecated_function, 
             warn_bif_clash]},
           {errors,
            [{2,erl_lint,disallowed_nowarn_bif_clash}],[]}},

          {otp_5362_7,
           <<"-export([spawn/1]).
              -compile({nowarn_deprecated_function,{erlang,hash,2}}).
              -compile({nowarn_bif_clash,{spawn,1}}).
              -compile({nowarn_bif_clash,{spawn,2}}). % bad
              -compile([{nowarn_deprecated_function, 
                                [{erlang,hash,-1},{3,hash,-1}]}, % 2 bad
                     {nowarn_deprecated_function, {{a,b,c},hash,-1}}]). % bad
              spawn(A) ->
                  erlang:hash(A, 3000),
                  spawn(A).
           ">>,
           {[nowarn_unused_function]},
           {error,[{3,erl_lint,disallowed_nowarn_bif_clash},
		   {4,erl_lint,disallowed_nowarn_bif_clash},
		   {4,erl_lint,{bad_nowarn_bif_clash,{spawn,2}}}],
            [{5,erl_lint,{bad_nowarn_deprecated_function,{3,hash,-1}}},
             {5,erl_lint,{bad_nowarn_deprecated_function,{erlang,hash,-1}}},
             {5,erl_lint,{bad_nowarn_deprecated_function,{{a,b,c},hash,-1}}}]}
           },

          {otp_5362_8,
           <<"-export([spawn/1]).
              -compile(warn_deprecated_function).
              -compile(warn_bif_clash).
              spawn(A) ->
                  erlang:hash(A, 3000),
                  spawn(A).
           ">>,
           {[nowarn_unused_function,
             {nowarn_bif_clash,{spawn,1}}]}, % has no effect
           {warnings,
            [{5,erl_lint,{deprecated,{erlang,hash,2},{erlang,phash2,2},
			  "in a future release"}}]}},

          {otp_5362_9,
           <<"-include_lib(\"stdlib/include/qlc.hrl\").
              -record(a, {x = qlc:q([{X,Y} || {X} <- [],{Y} <- [],X =:= Y])}).
              -export([t/0]).              
              t() -> #a{}.
          ">>,
           {[]},
           []},

          {otp_5362_10,
           <<"-compile({nowarn_deprecated_function,{erlang,hash,2}}).
              -compile({nowarn_bif_clash,{spawn,1}}).
              -import(x,[spawn/1]).
              spin(A) ->
                  erlang:hash(A, 3000),
                  spawn(A).
           ">>,
           {[nowarn_unused_function,
             warn_deprecated_function,
             warn_bif_clash]},
           {errors,
            [{2,erl_lint,disallowed_nowarn_bif_clash}],[]}}

          ],

    ?line [] = run(Config, Ts),
    ok.

otp_5371(doc) ->
    "OTP-5371. Aliases for bit syntax expressions are no longer allowed.";
otp_5371(suite) -> [];
otp_5371(Config) when is_list(Config) ->
    Ts = [{otp_5371_1,
           <<"t(<<A:8>> = <<B:8>>) ->
                  {A,B}.
             ">>,
	   [],
	   {errors,[{1,erl_lint,illegal_bin_pattern}],[]}},
	  {otp_5371_2,
           <<"x([<<A:8>>] = [<<B:8>>]) ->
                  {A,B}.
              y({a,<<A:8>>} = {b,<<B:8>>}) ->
                  {A,B}.
             ">>,
	   [],
	   {errors,[{1,erl_lint,illegal_bin_pattern},
		    {3,erl_lint,illegal_bin_pattern}],[]}},
	  {otp_5371_3,
           <<"-record(foo, {a,b,c}).
              -record(bar, {x,y,z}).
              -record(buzz, {x,y}).
              a(#foo{a = <<X:8>>} = #bar{x = <<Y:8>>}) ->
                  {X,Y}.
              b(#foo{b = <<X:8>>} = #foo{b = <<Y:4,Z:4>>}) ->
                  {X,Y,Z}.
              c(#foo{a = <<X:8>>} = #buzz{x = <<Y:8>>}) ->
                  {X,Y}.
              d(#foo{a=x,b = <<X:8>>} = #buzz{y = <<Y:8>>}) ->
                  {X,Y}.
              e(#foo{a=x,b = <<X:8>>} = #buzz{x=glurf,y = <<Y:8>>}) ->
                  {X,Y}.
             ">>,
	   [],
	   {errors,[{4,erl_lint,illegal_bin_pattern},
		    {6,erl_lint,illegal_bin_pattern},
		    {8,erl_lint,illegal_bin_pattern},
		    {10,erl_lint,illegal_bin_pattern},
		    {12,erl_lint,illegal_bin_pattern}],[]}},
	  {otp_5371_4,
           <<"-record(foo, {a,b,c}).
              -record(bar, {x,y,z}).
              -record(buzz, {x,y}).
              a(#foo{a = <<X:8>>,b=x} = #foo{b = <<Y:8>>}) ->
                  {X,Y}.
              b(#foo{a = <<X:8>>} = #bar{y = <<Y:4,Z:4>>}) ->
                  {X,Y,Z}.
              c(#foo{a = <<X:8>>} = #buzz{y = <<Y:8>>}) ->
                  {X,Y}.
             ">>,
	   [],
	   {warnings,[{4,v3_core,nomatch},
		      {6,v3_core,nomatch},
		      {8,v3_core,nomatch}]}}
	 ],
    ?line [] = run(Config, Ts),
    ok.

otp_7227(doc) -> "OTP_7227. Some aliases for bit syntax expressions were still allowed.";
otp_7227(Config) when is_list(Config) ->
    Ts = [{otp_7227_1,
           <<"t([<<A:8>> = {C,D} = <<B:8>>]) ->
                  {A,B,C,D}.
             ">>,
	   [],
	   {errors,[{1,erl_lint,illegal_bin_pattern}],[]}},
	  {otp_7227_2,
           <<"t([(<<A:8>> = {C,D}) = <<B:8>>]) ->
                  {A,B,C,D}.
             ">>,
	   [],
	   {errors,[{1,erl_lint,illegal_bin_pattern}],[]}},
	  {otp_7227_3,
           <<"t([(<<A:8>> = {C,D}) = (<<B:8>> = <<C:8>>)]) ->
                  {A,B,C,D}.
             ">>,
	   [],
	   {errors,[{1,erl_lint,illegal_bin_pattern},
		    {1,erl_lint,illegal_bin_pattern},
		    {1,erl_lint,illegal_bin_pattern}],[]}},
	  {otp_7227_4,
           <<"t(Val) ->
                  <<A:8>> = <<B:8>> = Val,
                  {A,B}.
             ">>,
	   [],
	   {errors,[{2,erl_lint,illegal_bin_pattern}],[]}},
	  {otp_7227_5,
           <<"t(Val) ->
                  <<A:8>> = X = <<B:8>> = Val,
                  {A,B,X}.
             ">>,
	   [],
	   {errors,[{2,erl_lint,illegal_bin_pattern}],[]}},
	  {otp_7227_6,
           <<"t(X, Y) ->
                  <<A:8>> = <<X:4,Y:4>>,
                  A.
             ">>,
	   [],
	   []},
	  {otp_7227_7,
           <<"t(Val) ->
                  (<<A:8>> = X) = (<<B:8>> = <<A:4,B:4>>) = Val,
                  {A,B,X}.
             ">>,
	   [],
	   {errors,[{2,erl_lint,illegal_bin_pattern},
		    {2,erl_lint,illegal_bin_pattern},
		    {2,erl_lint,illegal_bin_pattern}],[]}},
	  {otp_7227_8,
           <<"t(Val) ->
                  (<<A:8>> = X) = (Y = <<B:8>>) = Val,
                  {A,B,X,Y}.
             ">>,
	   [],
	   {errors,[{2,erl_lint,illegal_bin_pattern}],[]}},
	  {otp_7227_9,
           <<"t(Val) ->
                  (Z = <<A:8>> = X) = (Y = <<B:8>> = W) = Val,
                  {A,B,X,Y,Z,W}.
             ">>,
	   [],
	   {errors,[{2,erl_lint,illegal_bin_pattern}],[]}}
	 ],
    ?line [] = run(Config, Ts),
    ok.

otp_5494(doc) ->
    "OTP-5494. Warnings for functions exported more than once.";
otp_5494(suite) -> [];
otp_5494(Config) when is_list(Config) ->
    Ts = [{otp_5494_1,
           <<"-export([t/0]).
              -export([t/0]).
              t() -> a.
             ">>,
           [],
           {warnings,[{2,erl_lint,{duplicated_export,{t,0}}}]}}],
    ?line [] = run(Config, Ts),
    ok.

otp_5644(doc) ->
    "OTP-5644. M:F/A in record initialization.";
otp_5644(suite) -> [];
otp_5644(Config) when is_list(Config) ->
    %% This test is a no-op. Although {function,mfa,i,1} was
    %% transformed into {function,Line,i,1} by copy_expr, the module
    %% was never checked (Line is the line number).
    %% (OTP-5878: somewhat modified.)
    Ts = [{otp_5644,
          <<"-record(c, {a = fun ?MODULE:i/1(17)}).
              t() ->
                  #c{}.

              i(X) ->
                  X.
            ">>,
           [],
           []}],
    ?line [] = run(Config, Ts),
    ok.

otp_5878(doc) ->
    "OTP-5878. Record declaration: forward references, introduced variables.";
otp_5878(suite) -> [];
otp_5878(Config) when is_list(Config) ->
    Ts = [{otp_5878_10,
          <<"-record(rec1, {a = #rec2{}}).
             -record(rec2, {a = #rec1{}}).
             t() ->#rec1{}.
            ">>,
           [warn_unused_record],
           {error,[{1,erl_lint,{undefined_record,rec2}}],
                  [{2,erl_lint,{unused_record,rec2}}]}},

          {otp_5878_20,
           <<"-record(r1, {a = begin A = 4, {A,B} end}). % B unbound
              -record(r2, {e = begin A = 3, #r1{} end}).
              t() -> #r2{}.
             ">>,
           [warn_unused_record],
           {error,[{1,erl_lint,{unbound_var,'B'}},
                   {1,erl_lint,{variable_in_record_def,'A'}},
                   {2,erl_lint,{variable_in_record_def,'A'}}],
            [{1,erl_lint,{unused_record,r1}}]}},

          {otp_5878_30,
           <<"-record(r1, {t = case foo of _ -> 3 end}).
              -record(r2, {a = case foo of A -> A; _ -> 3 end}).
              -record(r3, {a = case foo of A -> A end}).
              t() -> {#r1{},#r2{},#r3{}}.
             ">>,
           [warn_unused_record],
           {errors,[{2,erl_lint,{variable_in_record_def,'A'}},
                    {3,erl_lint,{variable_in_record_def,'A'}}],
            []}},

          {otp_5878_40,
           <<"-record(r1, {foo = A}). % A unbound
              -record(r2, {a = fun(X) -> X end(3)}).
              -record(r3, {a = [X || X <- [1,2,3]]}).
              t() -> {#r1{},#r2{},#r3{}}.
             ">>,
           [warn_unused_record],
           {errors,[{1,erl_lint,{unbound_var,'A'}}],[]}},

          {otp_5878_50,
           <<"-record(r1, {a = {A, % A unbound
                                A}}). % A unbound
              -record(r2, {a = begin case foo of 
                                         A -> A
                                     end,
                                     A
                                end}).
              -record(r3, {a = fun(X) ->
                                       case foo of
                                           A -> A
                                       end
                               end
                          }).
              -record(r4, {a = case foo of
                                   foo ->
                                       case foo of
                                           A -> A
                                       end;
                                   _ -> 
                                       bar
                               end}).
              t() -> {#r1{},#r2{},#r3{},#r4{}}.
             ">>,
           [warn_unused_record],
           {error,[{1,erl_lint,{unbound_var,'A'}},
                   {2,erl_lint,{unbound_var,'A'}},
                   {4,erl_lint,{variable_in_record_def,'A'}},
                   {17,erl_lint,{variable_in_record_def,'A'}}],
            [{8,erl_lint,{unused_var,'X'}}]}},

          {otp_5878_60,
           <<"-record(r1, {a = fun(NotShadowing) -> NotShadowing end}).
              t() ->
                  NotShadowing = 17,
                  {#r1{}, NotShadowing}.
             ">>,
           [warn_unused_record],
           []},

          {otp_5878_70,
           <<"-record(r1, {a = fun(<<X:8>>) -> X end,
                           b = case <<17:8>> of
                                   <<_:Y>> -> Y;
                                   <<Y:8>> -> 
                                       Y
                               end}).
              t() -> #r1{}.
             ">>,
           [warn_unused_record],
           {errors,[{3,erl_lint,{unbound_var,'Y'}},
                    {4,erl_lint,{variable_in_record_def,'Y'}}],
            []}},

          {otp_5878_80,
           <<"-record(r, {a = [X || {A,Y} <- [{1,2},V={3,4}],
                                    begin Z = [1,2,3], true end,
                                    X <- Z ++ [A,Y]]}).
              t() ->#r{}.
             ">>,
           [warn_unused_record],
           {warnings,[{1,erl_lint,{unused_var,'V'}}]}},

          {otp_5878_90,
           <<"-record(r, {a = foo()}). % unused

              t() -> ok.
             ">>,
           [warn_unused_record],
           {error,[{1,erl_lint,{undefined_function,{foo,0}}}],
            [{1,erl_lint,{unused_record,r}}]}}

         ],
    ?line [] = run(Config, Ts),

    Abstr = <<"-module(lint_test, [A, B]).

               -export([args/1]).

               -record(r, {a = A, b = THIS}). % A and THIS are unbound

               %% param:args(compile,param:new(1,2)).

               args(C) ->
                   X = local(C),
                   Z = new(A, B),
                   F = fun(THIS) -> {x, A} end, % THIS unused and shadowed
                   {X, Z, THIS, F, #r{}}.

               local(C) ->
                   module_info(C).
            ">>,
    ?line {error,[{5,erl_lint,{unbound_var,'A'}},
                  {5,erl_lint,{unbound_var,'THIS'}}],
           [{12,erl_lint,{unused_var,'THIS'}},
            {12,erl_lint,{shadowed_var,'THIS','fun'}}]}
        = run_test2(Config, Abstr, [warn_unused_record]),

    QLC1 = <<"-module(lint_test).
              -include_lib(\"stdlib/include/qlc.hrl\").
              -export([t/0]).
              -record(r1, {a = qlc:e(qlc:q([X || X <- [1,2,3]]))}).
              -record(r2, {a = qlc:q([X || X <- [1,2,3]])}).
              -record(r3, {a = qlc:q([X || {A,Y} <- [{1,2},V={3,4}],
                                           begin Z = [1,2,3], true end,
                                           X <- Z ++ [A,Y]])}).
              t() -> {#r1{},#r2{},#r3{}}.
             ">>,
    ?line {error,[{8,qlc,{used_generator_variable,'A'}},
                  {8,qlc,{used_generator_variable,'Y'}},
                  {8,qlc,{used_generator_variable,'Z'}}],
           [{6,erl_lint,{unused_var,'V'}}]} = 
        run_test2(Config, QLC1, [warn_unused_record]),

    Ill1 = <<"-module(lint_test).
              -export([t/0]).
              -record(r, {a = true}).
              -record(r1, {a,b}).
              -record(r2, {a = #r1{a = true}}).
              -record(r3, {a = A}). % A is unbound
              -record(r4, {a = dict:new()}).

              t() ->
                  case x() of
                      _ when (#r{})#r.a ->
                          a; 
                      _ when (#r4{})#r.a -> % illegal
                          b;
                      _ when (#r3{q = 5})#r.a -> % no warning for unbound A
                          q;
                      _ when (#r{q = 5})#r.a ->
                          a; 
                      _ when (((#r{a = #r2{}})#r.a)#r2.a)#r1.a ->
                          b;
                      _ when #r{a = dict:new()} -> % illegal
                          c; 
                      _ when l() > 3 -> % illegal, does not use l/0...
                          d;
                      _ ->
                          w
                  end.

              l() ->
                  foo.

              x() ->
                  bar.
              ">>,
   
    ?line {errors,[{6,erl_lint,{unbound_var,'A'}},
                   {13,erl_lint,illegal_guard_expr},
                   {15,erl_lint,{undefined_field,r3,q}},
                   {17,erl_lint,{undefined_field,r,q}},
                   {21,erl_lint,illegal_guard_expr},
                   {23,erl_lint,{illegal_guard_local_call,{l,0}}}],
           []} = 
        run_test2(Config, Ill1, [warn_unused_record]),

    Ill2 = <<"-module(lint_test).
              -export([t/0]).
              t() ->
                  case x() of
                      _ when l() 
                             or
                             l() ->
                          foo
                  end.
             ">>,
    ?line {errors,[{4,erl_lint,{undefined_function,{x,0}}},
                   {5,erl_lint,illegal_guard_expr},
                   {7,erl_lint,illegal_guard_expr}],
           []} = 
        run_test2(Config, Ill2, [warn_unused_record]),
    
    Ill3 = <<"t() -> ok.">>,
    ?line {errors,[{1,erl_lint,undefined_module}],[]} = 
        run_test2(Config, Ill3, [warn_unused_record]),

    Usage1 = <<"-module(lint_test).
                -export([t/0]).
                -record(u1, {a}).
                -record(u2, {a = #u1{}}).
                -record(u3, {a}). % unused
                -record(u4, {a = #u3{}}). % unused

                t() ->
                    {#u2{}}.
               ">>,
    ?line {warnings,[{5,erl_lint,{unused_record,u3}},
                     {6,erl_lint,{unused_record,u4}}]} = 
        run_test2(Config, Usage1, [warn_unused_record]),

    Usage2 = <<"-module(lint_test).
                -export([t/0]).
                -record(u1, {a}).
                -record(u2, {a = #u1{}}).
                -file(\"some_file.hrl\", 1).
                -record(u3, {a}). % unused, but on other file
                -record(u4, {a = #u3{}}). % -\"-

                t() ->
                    {#u2{}}.
               ">>,
    ?line [] = run_test2(Config, Usage2, [warn_unused_record]),

    %% This a completely different story...
    %% The linter checks if qlc.hrl hasn't been included
    QLC2 = <<"-module(lint_test).
              -import(qlc, [q/2]).
              -export([t/0]).

              t() ->
                  H1 = qlc:q([X || X <- [1,2]]),
                  H2 = qlc:q([X || X <- [1,2]], []),
                  H3 = q([X || X <- [1,2]], []),
                  {H1,H2,H3}.
             ">>,
    ?line {warnings,[{6,erl_lint,{missing_qlc_hrl,1}},
                     {7,erl_lint,{missing_qlc_hrl,2}},
                     {8,erl_lint,{missing_qlc_hrl,2}}]} = 
        run_test2(Config, QLC2, [warn_unused_record]),

    %% Records that are used by types are not unused.
    %% (Thanks to Fredrik Thulin and Kostis Sagonas.)
    UsedByType = <<"-module(t).
                    -export([foo/1]).
                    -record(sipurl,  {host :: string()}).
                    -record(keylist, {list = [] :: [_]}).
                    -type sip_headers() :: #keylist{}.
                    -record(request, {uri :: #sipurl{}, header :: sip_headers()}).

                    foo(#request{}) -> ok.
                  ">>,
    ?line [] = run_test2(Config, UsedByType, [warn_unused_record]),

    ok.

otp_6885(doc) ->
    "OTP-6885. Binary fields in bit syntax matching is now only allowed at the end.";
otp_6885(suite) -> [];
otp_6885(Config) when is_list(Config) ->
    Ts = <<"-module(otp_6885).
            -export([t/1]).
            t(<<_/binary,I>>) -> I;
            t(<<X/binary,I:X>>) -> I;
	    t(<<B/binary,T/binary>>) -> {B,T}.

            build(A, B) ->
               <<A/binary,B/binary>>.

            foo(<<\"abc\"/binary>>) ->
               ok;
            foo(<<\"abc\":13/integer>>) ->
               ok;
            foo(<<\"abc\"/float>>) ->
               ok;
            foo(<<\"abc\":19>>) ->
               ok;
            foo(<<\"abc\"/utf8>>) ->
               ok;
            foo(<<\"abc\"/utf16>>) ->
               ok;
            foo(<<\"abc\"/utf32>>) ->
               ok.

           ">>,
    ?line {errors,[{3,erl_lint,unsized_binary_not_at_end},
		   {4,erl_lint,unsized_binary_not_at_end},
		   {5,erl_lint,unsized_binary_not_at_end},
		   {10,erl_lint,typed_literal_string},
		   {12,erl_lint,typed_literal_string},
		   {14,erl_lint,typed_literal_string},
		   {16,erl_lint,typed_literal_string}],
	   []} = run_test2(Config, Ts, []),
    ok.

export_all(doc) ->
    "OTP-7392. Warning for export_all.";
export_all(Config) when is_list(Config) ->
    Ts = <<"-module(export_all_module).
            -compile([export_all]).

            id(I) -> I.
           ">>,
    ?line [] = run_test2(Config, Ts, []),
    ?line {warnings,[{2,erl_lint,export_all}]} =
	run_test2(Config, Ts, [warn_export_all]),
    ok.

bif_clash(doc) ->
    "Test warnings for functions that clash with BIFs.";
bif_clash(suite) -> [];
bif_clash(Config) when is_list(Config) ->
    Ts = [{clash1,
           <<"t(X) ->
                  size(X).

              %% No warning for the following calls, since they
              %% are unambigous.
              b(X) ->
                  erlang:size(X).

              c(X) ->
                  ?MODULE:size(X).

              size({N,_}) ->
                N.
             ">>,
           [],
	   {errors,[{2,erl_lint,{call_to_redefined_old_bif,{size,1}}}],[]}},

	  %% Verify that warnings can not be turned off in the old way.
	  {clash2,
           <<"-export([t/1,size/1]).
              t(X) ->
                  size(X).

              size({N,_}) ->
                N.

              %% My own abs/1 function works on lists too. From R14 this really works.
              abs([H|T]) when $a =< H, H =< $z -> [H-($a-$A)|abs(T)];
              abs([H|T]) -> [H|abs(T)];
              abs([]) -> [];
              abs(X) -> erlang:abs(X).
             ">>,
	   {[nowarn_unused_function,nowarn_bif_clash]},
	   {errors,[{erl_lint,disallowed_nowarn_bif_clash}],[]}},
	  %% As long as noone calls an overridden BIF, it's totally OK
	  {clash3,
           <<"-export([size/1]).
              size({N,_}) ->
                N;
              size(X) ->
                erlang:size(X).
             ">>,
	   [],
	   []},
	  %% But this is totally wrong - meaning of the program changed in R14, so this is an error
	  {clash4,
           <<"-export([size/1]).
              size({N,_}) ->
                N;
              size(X) ->
                size(X).
             ">>,
	   [],
	   {errors,[{5,erl_lint,{call_to_redefined_old_bif,{size,1}}}],[]}},
	  %% For a post R14 bif, its only a warning
	  {clash5,
           <<"-export([binary_part/2]).
              binary_part({B,_},{X,Y}) ->
                binary_part(B,{X,Y});
              binary_part(B,{X,Y}) ->
                binary:part(B,X,Y).
             ">>,
	   [],
	   {warnings,[{3,erl_lint,{call_to_redefined_bif,{binary_part,2}}}]}},
	  %% If you really mean to call yourself here, you can "unimport" size/1
	  {clash6,
           <<"-export([size/1]).
              -compile({no_auto_import,[size/1]}).
              size([]) ->
                0;
              size({N,_}) ->
                N;
              size([_|T]) ->
                1+size(T).
             ">>,
	   [],
	   []},
	  %% Same for the post R14 autoimport warning
	  {clash7,
           <<"-export([binary_part/2]).
              -compile({no_auto_import,[binary_part/2]}).
              binary_part({B,_},{X,Y}) ->
                binary_part(B,{X,Y});
              binary_part(B,{X,Y}) ->
                binary:part(B,X,Y).
             ">>,
	   [],
	   []},
          %% but this doesn't mean the local function is allowed in a guard...
	  {clash8,
           <<"-export([x/1]).
              -compile({no_auto_import,[binary_part/2]}).
              x(X) when binary_part(X,{1,2}) =:= <<1,2>> ->
                 hej.
              binary_part({B,_},{X,Y}) ->
                binary_part(B,{X,Y});
              binary_part(B,{X,Y}) ->
                binary:part(B,X,Y).
             ">>,
	   [],
	   {errors,[{3,erl_lint,{illegal_guard_local_call,{binary_part,2}}}],[]}},
          %% no_auto_import is not like nowarn_bif_clash, it actually removes the autoimport
	  {clash9,
           <<"-export([x/1]).
              -compile({no_auto_import,[binary_part/2]}).
              x(X) ->
                 binary_part(X,{1,2}) =:= <<1,2>>.
             ">>,
	   [],
	   {errors,[{4,erl_lint,{undefined_function,{binary_part,2}}}],[]}},
          %% but we could import it again...
	  {clash10,
           <<"-export([x/1]).
              -compile({no_auto_import,[binary_part/2]}).
              -import(erlang,[binary_part/2]).
              x(X) ->
                 binary_part(X,{1,2}) =:= <<1,2>>.
             ">>,
	   [],
	   []},
          %% and actually use it in a guard...
	  {clash11,
           <<"-export([x/1]).
              -compile({no_auto_import,[binary_part/2]}).
              -import(erlang,[binary_part/2]).
              x(X) when binary_part(X,{0,1}) =:= <<0>> ->
                 binary_part(X,{1,2}) =:= <<1,2>>.
             ">>,
	   [],
	   []},
          %% but for non-obvious historical reasons, imported functions cannot be used in
	  %% fun construction without the module name...
	  {clash12,
           <<"-export([x/1]).
              -compile({no_auto_import,[binary_part/2]}).
              -import(erlang,[binary_part/2]).
              x(X) when binary_part(X,{0,1}) =:= <<0>> ->
                 binary_part(X,{1,2}) =:= fun binary_part/2.
             ">>,
	   [],
	   {errors,[{5,erl_lint,{undefined_function,{binary_part,2}}}],[]}},
          %% Not from erlang and not from anywhere else
	  {clash13,
           <<"-export([x/1]).
              -compile({no_auto_import,[binary_part/2]}).
              -import(x,[binary_part/2]).
              x(X) ->
                 binary_part(X,{1,2}) =:= fun binary_part/2.
             ">>,
	   [],
	   {errors,[{5,erl_lint,{undefined_function,{binary_part,2}}}],[]}},
	  %% ...while real auto-import is OK.
	  {clash14,
           <<"-export([x/1]).
              x(X) when binary_part(X,{0,1}) =:= <<0>> ->
                 binary_part(X,{1,2}) =:= fun binary_part/2.
             ">>,
	   [],
	   []},
          %% Import directive clashing with old bif is an error, regardless of if it's called or not
	  {clash15,
           <<"-export([x/1]).
              -import(x,[abs/1]).
              x(X) ->
                 binary_part(X,{1,2}).
             ">>,
	   [],
	   {errors,[{2,erl_lint,{redefine_old_bif_import,{abs,1}}}],[]}},
	  %% For a new BIF, it's only a warning
	  {clash16,
           <<"-export([x/1]).
              -import(x,[binary_part/3]).
              x(X) ->
                 abs(X).
             ">>,
	   [],
	   {warnings,[{2,erl_lint,{redefine_bif_import,{binary_part,3}}}]}},
	  %% And, you cannot redefine already imported things that aren't auto-imported
	  {clash17,
           <<"-export([x/1]).
              -import(x,[binary_port/3]).
              -import(y,[binary_port/3]).
              x(X) ->
                 abs(X).
             ">>,
	   [],
	   {errors,[{3,erl_lint,{redefine_import,{{binary_port,3},x}}}],[]}},
	  %% Not with local functions either
	  {clash18,
           <<"-export([x/1]).
              -import(x,[binary_port/3]).
              binary_port(A,B,C) ->
                 binary_part(A,B,C).
              x(X) ->
                 abs(X).
             ">>,
	   [],
	   {errors,[{3,erl_lint,{define_import,{binary_port,3}}}],[]}},
	  %% Like clash8: Dont accept a guard if it's explicitly module-name called either
	  {clash19,
           <<"-export([binary_port/3]).
              -compile({no_auto_import,[binary_part/3]}).
              -import(x,[binary_part/3]).
              binary_port(A,B,C) when x:binary_part(A,B,C) ->
                 binary_part(A,B,C+1).
             ">>,
	   [],
	   {errors,[{4,erl_lint,illegal_guard_expr}],[]}},
	  %% Not with local functions either
	  {clash20,
           <<"-export([binary_port/3]).
              -import(x,[binary_part/3]).
              binary_port(A,B,C) ->
                 binary_part(A,B,C).
             ">>,
	   [warn_unused_import],
	   {warnings,[{2,erl_lint,{redefine_bif_import,{binary_part,3}}}]}}
	 ],

    ?line [] = run(Config, Ts),
    ok.

behaviour_basic(doc) ->
    "Basic tests with one behaviour.";
behaviour_basic(suite) -> [];
behaviour_basic(Config) when is_list(Config) ->
    Ts = [{behaviour1,
           <<"-behaviour(application).
             ">>,
           [],
	   {warnings,[{1,erl_lint,{undefined_behaviour_func,{start,2},application}},
		      {1,erl_lint,{undefined_behaviour_func,{stop,1},application}}]}},

	  {behaviour2,
           <<"-behaviour(application).
              -export([stop/1]).
              stop(_) -> ok.
             ">>,
           [],
	   {warnings,[{1,erl_lint,{undefined_behaviour_func,{start,2},application}}]}},
	  
	  {behaviour3,
           <<"-behavior(application).  %% Test American spelling.
              -export([start/2,stop/1]).
              start(_, _) -> ok.
              stop(_) -> ok.
             ">>,
           [],
	   []}
	 ],
    ?line [] = run(Config, Ts),
    ok.

behaviour_multiple(doc) ->
    "Basic tests with multiple behaviours.";
behaviour_multiple(suite) -> [];
behaviour_multiple(Config) when is_list(Config) ->
    Ts = [{behaviour1,
           <<"-behaviour(application).
              -behaviour(supervisor).
             ">>,
           [],
	   {warnings,[{1,erl_lint,{undefined_behaviour_func,{start,2},application}},
		      {1,erl_lint,{undefined_behaviour_func,{stop,1},application}},
		      {2,erl_lint,{undefined_behaviour_func,{init,1},supervisor}}]}},

	  {behaviour2,
           <<"-behaviour(application).
              -behaviour(supervisor).
              -export([start/2,stop/1,init/1]).
              start(_, _) -> ok.
              stop(_) -> ok.
              init(_) -> ok.
             ">>,
           [],
	   []},

	  {american_behavior2,
           <<"-behavior(application).
              -behavior(supervisor).
              -export([start/2,stop/1,init/1]).
              start(_, _) -> ok.
              stop(_) -> ok.
              init(_) -> ok.
             ">>,
           [],
	   []},

	  {behaviour3,
           <<"-behaviour(gen_server).
              -behaviour(supervisor).
              -export([handle_call/3,handle_cast/2,handle_info/2]).
              handle_call(_, _, _) -> ok.
              handle_cast(_, _) -> ok.
              handle_info(_, _) -> ok.
             ">>,
           [],
	   {warnings,[{1,erl_lint,
		       {undefined_behaviour_func,{code_change,3},gen_server}},
		      {1,erl_lint,{undefined_behaviour_func,{init,1},gen_server}},
		      {1,erl_lint,{undefined_behaviour_func,{terminate,2},gen_server}},
		      {2,erl_lint,{undefined_behaviour_func,{init,1},supervisor}},
		      {2,
		       erl_lint,
		       {conflicting_behaviours,{init,1},supervisor,1,gen_server}}]}},
	  {american_behavior3,
           <<"-behavior(gen_server).
              -behavior(supervisor).
              -export([handle_call/3,handle_cast/2,handle_info/2]).
              handle_call(_, _, _) -> ok.
              handle_cast(_, _) -> ok.
              handle_info(_, _) -> ok.
             ">>,
           [],
	   {warnings,[{1,erl_lint,
		       {undefined_behaviour_func,{code_change,3},gen_server}},
		      {1,erl_lint,{undefined_behaviour_func,{init,1},gen_server}},
		      {1,erl_lint,{undefined_behaviour_func,{terminate,2},gen_server}},
		      {2,erl_lint,{undefined_behaviour_func,{init,1},supervisor}},
		      {2,
		       erl_lint,
		       {conflicting_behaviours,{init,1},supervisor,1,gen_server}}]}},

	  {behaviour4,
           <<"-behaviour(gen_server).
              -behaviour(gen_fsm).
              -behaviour(supervisor).
              -export([init/1,handle_call/3,handle_cast/2,
                       handle_info/2,handle_info/3,
                       handle_event/3,handle_sync_event/4,
                       code_change/3,code_change/4,
                       terminate/2,terminate/3,terminate/4]).
              init(_) -> ok.
              handle_call(_, _, _) -> ok.
              handle_event(_, _, _) -> ok.
              handle_sync_event(_, _, _, _) -> ok.
              handle_cast(_, _) -> ok.
              handle_info(_, _) -> ok.
              handle_info(_, _, _) -> ok.
              code_change(_, _, _) -> ok.
              code_change(_, _, _, _) -> ok.
              terminate(_, _) -> ok.
              terminate(_, _, _) -> ok.
              terminate(_, _, _, _) -> ok.
             ">>,
           [],
	   {warnings,[{2,
		       erl_lint,
		       {conflicting_behaviours,{init,1},gen_fsm,1,gen_server}},
		      {3,
		       erl_lint,
		       {conflicting_behaviours,{init,1},supervisor,1,gen_server}}]}}
	 ],
    ?line [] = run(Config, Ts),
    ok.

otp_7550(doc) ->
    "Test that the new utf8/utf16/utf32 types do not allow size or unit specifiers.";
otp_7550(Config) when is_list(Config) ->
    Ts = [{otp_7550,
           <<"f8(A) ->
                  <<A:8/utf8>>.
              g8(A) ->
                  <<A:8/utf8-unit:1>>.
              h8(A) ->
                  <<A/utf8-unit:1>>.

              f16(A) ->
                  <<A:8/utf16>>.
              g16(A) ->
                  <<A:8/utf16-unit:1>>.
              h16(A) ->
                  <<A/utf16-unit:1>>.

              f32(A) ->
                  <<A:8/utf32>>.
              g32(A) ->
                  <<A:8/utf32-unit:1>>.
              h32(A) ->
                  <<A/utf32-unit:1>>.
             ">>,
           [],
           {errors,[{2,erl_lint,utf_bittype_size_or_unit},
		    {4,erl_lint,utf_bittype_size_or_unit},
		    {6,erl_lint,utf_bittype_size_or_unit},
		    {9,erl_lint,utf_bittype_size_or_unit},
		    {11,erl_lint,utf_bittype_size_or_unit},
		    {13,erl_lint,utf_bittype_size_or_unit},
		    {16,erl_lint,utf_bittype_size_or_unit},
		    {18,erl_lint,utf_bittype_size_or_unit},
		    {20,erl_lint,utf_bittype_size_or_unit}
		   ],
            []}}],
    ?line [] = run(Config, Ts),
    ok.
    

otp_8051(doc) ->
    "Bugfix: -opaque with invalid type.";
otp_8051(Config) when is_list(Config) ->
    Ts = [{otp_8051,
           <<"-opaque foo() :: bar().
             ">>,
           [],
           {error,[{1,erl_lint,{undefined_type,{bar,0}}}],
            [{1,erl_lint,{unused_type,{foo,0}}}]}}],
    ?line [] = run(Config, Ts),
    ok.

format_warn(doc) ->
    "Check that format warnings are generated.";
format_warn(suite) -> [];
format_warn(Config) when is_list(Config) ->
    L1 = 14,
    L2 = 4,
    format_level(1, L1, Config),
    format_level(2, L1+L2, Config),
    format_level(3, L1+L2, Config),             %there is no level 3
    ok.

format_level(Level, Count, Config) ->
    ?line W = get_compilation_warnings(Config, "format",
                                       [{warn_format, Level}]),
    %% Pick out the 'format' warnings.
    ?line FW = lists:filter(fun({_Line, erl_lint, {format_error, _}}) -> true;
                               (_) -> false
                            end,
                            W),
    ?line case length(FW) of
              Count ->
                  ok;
              Other ->
                  ?t:format("Expected ~w warning(s); got ~w", [Count,Other]),
                  fail()
          end,
    ok.

%% Test the -on_load(Name/0) directive.


on_load_successful(Config) when is_list(Config) ->
    Ts = [{on_load_1,
	   %% Exported on_load function.
	   <<"-export([do_on_load/0]).
             -on_load(do_on_load/0).
             do_on_load() -> ok.
             ">>,
	   {[]},				%Tuple indicates no 'export_all'.
	   []},

	  {on_load_2,
	   %% Local on_load function.
	   <<"-on_load(do_on_load/0).
             do_on_load() -> ok.
             ">>,
	   {[]},				%Tuple indicates no 'export_all'.
	   []},

	  {on_load_3,
	   %% Local on_load function, calling other local functions.
	   <<"-on_load(do_on_load/0).
             do_on_load() -> foo().
             foo() -> bar(5) + 42.
             bar(N) -> 2*N.
             ">>,
	   {[]},				%Tuple indicates no 'export_all'.
	   []}
	 ],
    ?line [] = run(Config, Ts),
    ok.

on_load_failing(Config) when is_list(Config) ->
    Ts = [{on_load_1,
	   %% Badly formed.
	   <<"-on_load(atom).
             ">>,
	   {[]},				%Tuple indicates no 'export_all'.
	   {errors,
	    [{1,erl_lint,{bad_on_load,atom}}],[]}},

	  {on_load_2,
	   %% Badly formed.
	   <<"-on_load({42,0}).
             ">>,
	   {[]},				%Tuple indicates no 'export_all'.
	   {errors,
	    [{1,erl_lint,{bad_on_load,{42,0}}}],[]}},

	  {on_load_3,
	   %% Multiple on_load attributes.
	   <<"-on_load(foo/0).
              -on_load(bar/0).
              foo() -> ok.
              bar() -> ok.
             ">>,
	   {[]},				%Tuple indicates no 'export_all'.
	   {errors,
	    [{2,erl_lint,multiple_on_loads}],[]}},

	  {on_load_4,
	   %% Wrong arity.
	   <<"-on_load(foo/1).
              foo(_) -> ok.
             ">>,
	   {[]},				%Tuple indicates no 'export_all'.
	   {errors,
	    [{1,erl_lint,{bad_on_load_arity,{foo,1}}}],[]}},

	  {on_load_5,
	   %% Non-existing function.
	   <<"-on_load(non_existing/0).
             ">>,
	   {[]},				%Tuple indicates no 'export_all'.
	   {errors,
	    [{1,erl_lint,{undefined_on_load,{non_existing,0}}}],[]}}
	 ],
    ?line [] = run(Config, Ts),
    ok.

too_many_arguments(doc) ->
    "Test that too many arguments is not accepted.";
too_many_arguments(suite) -> [];
too_many_arguments(Config) when is_list(Config) ->
    Ts = [{too_many_1,
	   <<"f(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> ok.">>,
	   [],
	   {errors,
	    [{1,erl_lint,{too_many_arguments,256}}],[]}}
	 ],
	  
    ?line [] = run(Config, Ts),
    ok.


run(Config, Tests) ->
    F = fun({N,P,Ws,E}, BadL) ->
                case catch run_test(Config, P, Ws) of
                    E -> 
                        BadL;
                    Bad -> 
                        ?t:format("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, E, Bad]),
			fail()
                end
        end,
    lists:foldl(F, [], Tests).

%% Compiles a test file and returns the list of warnings.

get_compilation_warnings(Conf, Filename, Warnings) ->
    ?line DataDir = ?datadir,
    ?line File = filename:join(DataDir, Filename),
    {ok,Bin} = file:read_file(File++".erl"),
    FileS = binary_to_list(Bin),
    {match,[{Start,Length}|_]} = re:run(FileS, "-module.*\\n"),
    Test = lists:nthtail(Start+Length, FileS),
    {warnings, Ws} = run_test(Conf, Test, Warnings),
    Ws.

%% Compiles a test module and returns the list of errors and warnings.

run_test(Conf, Test0, Warnings0) ->
    Test = list_to_binary(["-module(lint_test). ", Test0]),
    run_test2(Conf, Test, Warnings0).

run_test2(Conf, Test, Warnings0) ->
    Filename = "lint_test.erl",
    DataDir = ?privdir,
    File = filename:join(DataDir, Filename),
    Opts = case Warnings0 of
               {Warnings} ->		%Hairy trick to not add export_all.
                   [return|Warnings];
               Warnings ->
                   [export_all,return|Warnings]
           end,
    ok = file:write_file(File, Test),

    %% We will use the 'binary' option so that the compiler will not
    %% compare the module name to the output file name. Also, there
    %% is no reason to produce an output file since we are only
    %% interested in the errors and warnings.

    %% Print warnings, call erl_lint:format_error/1.
    compile:file(File, [binary,report|Opts]),

    case compile:file(File, [binary|Opts]) of
        {ok, _M, Code, Ws} when is_binary(Code) -> warnings(File, Ws);
        {error, [{File,Es}], []} -> {errors, Es, []};
        {error, [{File,Es}], [{File,Ws}]} -> {error, Es, Ws};
        {error, [{File,Es1},{File,Es2}], []} -> {errors2, Es1, Es2}
    end.

warnings(File, Ws) ->
    case lists:append([W || {F, W} <- Ws, F =:= File]) of
        [] -> [];
        L -> {warnings, L}
    end.

fail() ->
    io:format("failed~n"),
    ?t:fail().

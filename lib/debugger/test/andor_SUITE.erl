%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

%%
-module(andor_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_suite/1,end_per_suite/1,
	 t_andalso/1,t_orelse/1,inside/1,overlap/1,
	 combined/1,in_case/1]).

-include_lib("common_test/include/ct.hrl").

-warning("Ignore me -- testing that the debugger can handle warnings").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

init_per_suite(Config) when is_list(Config) ->
    test_lib:interpret(?MODULE),
    true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

cases() -> 
    [t_andalso, t_orelse, inside, overlap, combined,
     in_case].

t_andalso(Config) when is_list(Config) ->
    Bs = [true,false],
    Ps = [{X,Y} || X <- Bs, Y <- Bs],
    lists:foreach(fun (P) -> t_andalso_1(P) end, Ps),

    true = true andalso true,
    false = true andalso false,
    false = false andalso true,
    false = false andalso false,

    false = false andalso glurf,
    false = false andalso exit(exit_now),

    true = not id(false) andalso not id(false),
    false = not id(false) andalso not id(true),
    false = not id(true) andalso not id(false),
    false = not id(true) andalso not id(true),

    {'EXIT',{badarg,_}} = (catch not id(glurf) andalso id(true)),
    {'EXIT',{badarg,_}} = (catch not id(false) andalso not id(glurf)),
    false = id(false) andalso not id(glurf),
    false = false andalso not id(glurf),

    ok.

t_orelse(Config) when is_list(Config) ->
    Bs = [true,false],
    Ps = [{X,Y} || X <- Bs, Y <- Bs],
    lists:foreach(fun (P) -> t_orelse_1(P) end, Ps),

    true = true orelse true,
    true = true orelse false,
    true = false orelse true,
    false = false orelse false,

    true = true orelse glurf,
    true = true orelse exit(exit_now),

    true = not id(false) orelse not id(false),
    true = not id(false) orelse not id(true),
    true = not id(true) orelse not id(false),
    false = not id(true) orelse not id(true),

    {'EXIT',{badarg,_}} = (catch not id(glurf) orelse id(true)),
    {'EXIT',{badarg,_}} = (catch not id(true) orelse not id(glurf)),
    true = id(true) orelse not id(glurf),
    true = true orelse not id(glurf),

    ok.

t_andalso_1({X,Y}) ->
    io:fwrite("~w andalso ~w: ",[X,Y]),
    V1 = echo(X) andalso echo(Y),
    V1 = if
	     X andalso Y -> true;
	     true -> false
	 end,
    check(V1, X and Y).

t_orelse_1({X,Y}) ->
    io:fwrite("~w orelse ~w: ",[X,Y]),
    V1 = echo(X) orelse echo(Y),
    V1 = if
	     X orelse Y -> true;
	     true -> false
	 end,
    check(V1, X or Y).

inside(Config) when is_list(Config) ->
    true = inside(-8, 1),
    false = inside(-53.5, -879798),
    false = inside(1.0, -879),
    false = inside(59, -879),
    false = inside(-11, 1.0),
    false = inside(100, 0.2),
    false = inside(100, 1.2),
    false = inside(-53.5, 4),
    false = inside(1.0, 5.3),
    false = inside(59, 879),
    ok.

inside(Xm, Ym) ->
    X = -10.0,
    Y = -2.0,
    W = 20.0,
    H = 4.0,
    Res = inside(Xm, Ym, X, Y, W, H),
    Res = if
	      X =< Xm andalso Xm < X+W andalso Y =< Ym andalso Ym < Y+H -> true;
	      true -> false
	  end,
    case not id(Res) of
	Outside ->
	    Outside = if
			  not(X =< Xm andalso Xm < X+W andalso Y =< Ym andalso Ym < Y+H) -> true;
			  true -> false
		      end
    end,
    {Res,Xm,Ym,X,Y,W,H} = inside_guard(Xm, Ym, X, Y, W, H),
    io:format("~p =< ~p andalso ~p < ~p andalso ~p =< ~p andalso ~p < ~p ==> ~p",
	      [X,Xm,Xm,X+W,Y,Ym,Ym,Y+H,Res]),
    Res.

inside(Xm, Ym, X, Y, W, H) ->
    X =< Xm andalso Xm < X+W andalso Y =< Ym andalso Ym < Y+H.

inside_guard(Xm, Ym, X, Y, W, H) when X =< Xm andalso Xm < X+W
				      andalso Y =< Ym andalso Ym < Y+H ->
    {true,Xm,Ym,X,Y,W,H};
inside_guard(Xm, Ym, X, Y, W, H) ->
    {false,Xm,Ym,X,Y,W,H}.

overlap(Config) when is_list(Config) ->
    true = overlap(7.0, 2.0, 8.0, 0.5),
    true = overlap(7.0, 2.0, 8.0, 2.5),
    true = overlap(7.0, 2.0, 5.3, 2),
    true = overlap(7.0, 2.0, 0.0, 100.0),

    false = overlap(-1, 2, -35, 0.5),
    false = overlap(-1, 2, 777, 0.5),
    false = overlap(-1, 2, 2, 10),
    false = overlap(2, 10, 12, 55.3),
    ok.

overlap(Pos1, Len1, Pos2, Len2) ->
    Res = case Pos1 of
	      Pos1 when (Pos2 =< Pos1 andalso Pos1 < Pos2+Len2)
			orelse (Pos1 =< Pos2 andalso Pos2 < Pos1+Len1) ->
		  true;
	      Pos1 -> false
	  end,
    Res = (Pos2 =< Pos1 andalso Pos1 < Pos2+Len2)
	orelse (Pos1 =< Pos2 andalso Pos2 < Pos1+Len1),
    Res = case Pos1 of
	      Pos1 when (Pos2 =< Pos1 andalso Pos1 < Pos2+Len2)
			orelse (Pos1 =< Pos2 andalso Pos2 < Pos1+Len1) ->
		  true;
	      Pos1 -> false
	  end,
    id(Res).


-define(COMB(A,B,C), (A andalso B orelse C)).

combined(Config) when is_list(Config) ->
    false = comb(false, false, false),
    true = comb(false, false, true),
    false = comb(false, true, false),
    true = comb(false, true, true),

    false = comb(true, false, false),
    true = comb(true, true, false),
    true = comb(true, false, true),
    true = comb(true, true, true),

    false = comb(false, blurf, false),
    true = comb(false, blurf, true),
    true = comb(true, true, blurf),

    false = ?COMB(false, false, false),
    true = ?COMB(false, false, true),
    false = ?COMB(false, true, false),
    true = ?COMB(false, true, true),

    false = ?COMB(true, false, false),
    true = ?COMB(true, true, false),
    true = ?COMB(true, false, true),
    true = ?COMB(true, true, true),

    false = ?COMB(false, blurf, false),
    true = ?COMB(false, blurf, true),
    true = ?COMB(true, true, blurf),

    ok.
-undef(COMB).

comb(A, B, C) ->
    Res = A andalso B orelse C,
    Res = if
	      A andalso B orelse C -> true;
	      true -> false
	  end,
    NotRes = if
		 not(A andalso B orelse C) -> true;
		 true -> false
	     end,
    NotRes = id(not Res),
    Res = A andalso B orelse C,
    Res = if
	      A andalso B orelse C -> true;
	      true -> false
	  end,
    NotRes = id(not Res),
    Res = if
	      A andalso B orelse C -> true;
	      true -> false
	  end,
    id(Res).

%% Test that a boolean expression in a case expression is properly
%% optimized (in particular, that the error behaviour is correct).
in_case(Config) when is_list(Config) ->
    edge_rings = in_case_1(1, 1, 1, 1, 1),
    not_loop = in_case_1(0.5, 1, 1, 1, 1),
    loop = in_case_1(0.5, 0.9, 1.1, 1, 4),
    {'EXIT',{badarith,_}} = (catch in_case_1(1, 1, 1, 1, 0)),
    {'EXIT',{badarith,_}} = (catch in_case_1(1, 1, 1, 1, nan)),
    {'EXIT',{badarg,_}} = (catch in_case_1(1, 1, 1, blurf, 1)),
    {'EXIT',{badarith,_}} = (catch in_case_1([nan], 1, 1, 1, 1)),
    ok.

in_case_1(LenUp, LenDw, LenN, Rotation, Count) ->
    Res = in_case_1_body(LenUp, LenDw, LenN, Rotation, Count),
    Res = in_case_1_guard(LenUp, LenDw, LenN, Rotation, Count),
    Res.

in_case_1_body(LenUp, LenDw, LenN, Rotation, Count) ->
    case (LenUp/Count > 0.707) and (LenN/Count > 0.707) and
	(abs(Rotation) > 0.707) of
	true ->
	    edge_rings;
	false ->
	    case (LenUp >= 1) or (LenDw >= 1) or
		(LenN =< 1) or (Count < 4) of
		true ->
		    not_loop;
		false ->
		    loop
	    end
    end.

in_case_1_guard(LenUp, LenDw, LenN, Rotation, Count) ->
    case (LenUp/Count > 0.707) andalso (LenN/Count > 0.707) andalso
	(abs(Rotation) > 0.707) of
	true -> edge_rings;
	false when LenUp >= 1 orelse LenDw >= 1 orelse
		   LenN =< 1 orelse Count < 4 -> not_loop;
	false -> loop
    end.

check(V1, V0) ->
    if V1 /= V0 ->
	    io:fwrite("error: ~w.\n", [V1]),
	    ct:fail(failed);
       true ->
	    io:fwrite("ok: ~w.\n", [V1])
    end.

echo(X) ->
    io:fwrite("eval(~w); ",[X]),
    X.

id(I) -> I.

%%% -*- erlang-indent-level: 2 -*-
%%%----------------------------------------------------------------------
%%% Contains
%%%----------------------------------------------------------------------
-module(basic_edge_cases).

-export([test/0]).

test() ->
  ok = test_float_spills(),
  ok = test_infinite_loops(),
  ok.

%% Contains more float temps live at a single point than there are float
%% registers in any backend

test_float_spills() ->
    {{{2942.0,4670.0,3198.0,4926.0,2206.0,4734.0},
      {3118.0,2062.0,5174.0,3038.0,3618.0,3014.0},
      {2542.0,2062.0,4934.0,2590.0,3098.0,3062.0},
      {2950.0,3666.0,2574.0,5038.0,1866.0,2946.0},
      {3126.0,3050.0,3054.0,5070.0,2258.0,2714.0},
      {4734.0,2206.0,4926.0,3198.0,4670.0,2942.0}},
     58937.0} =
	mat66_flip_sum(35.0,86.0,32.0,88.0,33.0,57.0,
		       22.0,77.0,91.0,80.0,14.0,33.0,
		       51.0,28.0,87.0,20.0,91.0,11.0,
		       68.0,83.0,64.0,82.0,10.0,86.0,
		       74.0,18.0,08.0,52.0,10.0,14.0,
		       89.0,34.0,64.0,66.0,58.0,55.0,
		       0.0, 5),
    ok.

mat66_flip_sum(M11, M12, M13, M14, M15, M16,
	       M21, M22, M23, M24, M25, M26,
	       M31, M32, M33, M34, M35, M36,
	       M41, M42, M43, M44, M45, M46,
	       M51, M52, M53, M54, M55, M56,
	       M61, M62, M63, M64, M65, M66,
	       Acc, Ctr)
  when is_float(M11), is_float(M12), is_float(M13),
       is_float(M14), is_float(M15), is_float(M16),
       is_float(M21), is_float(M22), is_float(M23),
       is_float(M24), is_float(M25), is_float(M26),
       is_float(M31), is_float(M32), is_float(M33),
       is_float(M34), is_float(M35), is_float(M36),
       is_float(M41), is_float(M42), is_float(M43),
       is_float(M44), is_float(M45), is_float(M46),
       is_float(M51), is_float(M52), is_float(M53),
       is_float(M54), is_float(M55), is_float(M56),
       is_float(M61), is_float(M62), is_float(M63),
       is_float(M64), is_float(M65), is_float(M66),
       is_float(Acc) ->
    R11 = M66+M11, R12 = M65+M12, R13 = M64+M13,
    R14 = M63+M14, R15 = M62+M15, R16 = M61+M16,
    R21 = M56+M21, R22 = M55+M22, R23 = M54+M23,
    R24 = M53+M24, R25 = M52+M25, R26 = M51+M26,
    R31 = M46+M31, R32 = M45+M32, R33 = M44+M33,
    R34 = M43+M34, R35 = M42+M35, R36 = M41+M36,
    R41 = M26+M41, R42 = M25+M42, R43 = M24+M43,
    R44 = M23+M44, R45 = M22+M45, R46 = M21+M46,
    R51 = M36+M51, R52 = M35+M52, R53 = M34+M53,
    R54 = M33+M54, R55 = M32+M55, R56 = M31+M56,
    R61 = M16+M61, R62 = M15+M62, R63 = M14+M63,
    R64 = M13+M64, R65 = M12+M65, R66 = M11+M66,
    case Ctr of
	0 ->
	    {{{R11, R12, R13, R14, R15, R16},
	      {R21, R22, R23, R24, R25, R26},
	      {R31, R32, R33, R34, R35, R36},
	      {R41, R42, R43, R44, R45, R46},
	      {R51, R52, R53, R54, R55, R56},
	      {R61, R62, R63, R64, R65, R66}},
	     Acc};
	_ ->
	    NewAcc = 0.0 + M11 + M12 + M13 + M14 + M15 + M16 +
		+ M21 + M22 + M23 + M24 + M25 + M26
		+ M31 + M32 + M33 + M34 + M35 + M36
		+ M41 + M42 + M43 + M44 + M45 + M46
		+ M51 + M52 + M53 + M54 + M55 + M56
		+ M61 + M62 + M63 + M64 + M65 + M66
		+ Acc,
	    mat66_flip_sum(R11+1.0, R12+1.0, R13+1.0, R14+1.0, R15+1.0, R16+1.0,
			   R21+1.0, R22+1.0, R23+1.0, R24+1.0, R25+1.0, R26+1.0,
			   R31+1.0, R32+1.0, R33+1.0, R34+1.0, R35+1.0, R36+1.0,
			   R41+1.0, R42+1.0, R43+1.0, R44+1.0, R45+1.0, R46+1.0,
			   R51+1.0, R52+1.0, R53+1.0, R54+1.0, R55+1.0, R56+1.0,
			   R61+1.0, R62+1.0, R63+1.0, R64+1.0, R65+1.0, R66+1.0,
			   NewAcc, Ctr-1)
    end.

%% Infinite loops must receive reduction tests, and might trip up basic block
%% weighting, leading to infinite weights and/or divisions by zero.

test_infinite_loops() ->
  OldTrapExit = process_flag(trap_exit, true),
  ok = test_infinite_loop(fun infinite_recursion/0),
  ok = test_infinite_loop(fun infinite_corecursion/0),
  RecursiveFun = fun RecursiveFun() -> RecursiveFun() end,
  ok = test_infinite_loop(RecursiveFun),
  CorecursiveFunA = fun CorecursiveFunA() ->
		       CorecursiveFunA1 = fun () -> CorecursiveFunA() end,
		       CorecursiveFunA1()
		   end,
  ok = test_infinite_loop(CorecursiveFunA),
  CorecursiveFunB1 = fun(CorecursiveFunB) -> CorecursiveFunB() end,
  CorecursiveFunB = fun CorecursiveFunB() ->
		       CorecursiveFunB1(CorecursiveFunB)
		   end,
  ok = test_infinite_loop(CorecursiveFunB),
  CorecursiveFunC1 = fun CorecursiveFunC1(Other) ->
			 Other(CorecursiveFunC1)
		     end,
  CorecursiveFunC = fun CorecursiveFunC(Other) ->
		       Other(CorecursiveFunC)
		   end,
  ok = test_infinite_loop(fun() -> CorecursiveFunC(CorecursiveFunC1) end),
  ok = test_infinite_loop(fun() -> CorecursiveFunC(CorecursiveFunC) end),
  true = process_flag(trap_exit, OldTrapExit),
  ok.

-define(INFINITE_LOOP_TIMEOUT, 100).
test_infinite_loop(Fun) ->
  Tester = spawn_link(Fun),
  kill_soon(Tester),
  receive {'EXIT', Tester, awake} ->
      undefined = process_info(Tester),
      ok
  after ?INFINITE_LOOP_TIMEOUT -> error(timeout)
  end.

infinite_recursion() -> infinite_recursion().

infinite_corecursion() -> infinite_corecursion_1().
infinite_corecursion_1() -> infinite_corecursion().

kill_soon(Pid) ->
  _ = spawn_link(fun() ->
		     timer:sleep(1),
		     erlang:exit(Pid, awake)
		 end),
  ok.

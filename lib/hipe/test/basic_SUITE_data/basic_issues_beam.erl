%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains code examples, mostly taken from the mailing list, that
%%% crashed the BEAM compiler or gave an internal error at some point.
%%%-------------------------------------------------------------------
-module(basic_issues_beam).

-export([test/0]).

test() ->
  ok = test_crash_R10_hinde(),
  ok = test_error_R10_mander(),
  ok = test_error_R11_bjorklund(),
  ok = test_error_R11_rath(),
  ok = test_error_R12_empty_bin_rec(),
  ok = test_bug_R12_cornish(),
  ok = test_crash_R12_morris(),
  ok = test_error_R13_almeida(),
  ok = test_error_R13B01_fisher(),
  ok = test_error_R13B01_sawatari(),
  ok = test_error_R13B01_whongo(),
  ok = test_error_R16B03_norell(),
  ok = test_error_try_wings(),
  ok.

%%--------------------------------------------------------------------
%% Fisher R10 compiler crash
%%--------------------------------------------------------------------

-record(r, {a, b, c}).

test_crash_R10_hinde() ->
  rec_R10_hinde(#r{}).

rec_R10_hinde(As) ->
  case As of
    A when A#r.b == ""; A#r.b == undefined -> ok;
    _ -> error
  end.

%%--------------------------------------------------------------------
%% From: Peter-Henry Mander
%% Date: 27 Jan, 2005
%%
%% I managed to isolate a non-critical BEAM compilation error
%% (internal error in v3_codegen) when compiling the following code:
%%--------------------------------------------------------------------

test_error_R10_mander() ->
  try just_compile_me_R10() catch _:_ -> ok end.

just_compile_me_R10() ->
  URI_Before =
    {absoluteURI,
     {scheme, fun() -> nil end},
     {hier_part,
      {net_path,
       {srvr,
	{userinfo, nil},
	fun() -> nil end},
       nil},
      {port, nil}}},
  {absoluteURI,
   {scheme, _},
   {hier_part,
    {net_path,
     {srvr,
      {userinfo, nil},
      _HostportBefore},
     nil},
    {port, nil}}} = URI_Before,
  %% ... some funky code ommitted, not relevant ...
  {absoluteURI,
   {scheme, _},
   {hier_part,
    {net_path,
     {srvr,
      {userinfo, nil},
      HostportAfter},
     nil},
    {port, nil}}} = URI_Before,
  %% NOTE: I intended to write URI_After instead of URI_Before
  %% but the accident revealed that when you add the line below,
  %% it causes internal error in v3_codegen on compilation
  {hostport, {hostname, "HostName"}, {port, nil}} = HostportAfter,
  ok.

%%--------------------------------------------------------------------
%% From: Martin Bjorklund
%% Date: Aug 16, 2006
%%
%% I found this compiler bug in R10B-10 and R11B-0.
%%
%% Function -just_compile_me/0-fun-2-/1 refers to undefined label 18
%% ./bjorklund_R11compiler_bug.erl:none: internal error in beam_clean;
%% crash reason: {{case_clause,{'EXIT',{undefined_label,18}}},
%%                [{compile,'-select_passes/2-anonymous-2-',2},
%%                 {compile,'-internal_comp/4-anonymous-1-',2},
%%                 {compile,fold_comp,3},
%%                 {compile,internal_comp,4},
%%                 {compile,internal,3}]}
%%--------------------------------------------------------------------

test_error_R11_bjorklund() ->
  just_compile_me_R11_bjorklund().

just_compile_me_R11_bjorklund() ->
  G = fun() -> ok end,
  try
    G() %% fun() -> ok end
  after
    fun({A, B}) -> A + B end
  end.

%%--------------------------------------------------------------------
%% From: Tim Rath
%% Date: Sep 13, 2006
%% Subject: Compiler bug not quite fixed
%%
%%
%% I saw a compiler bug posted to the list by Martin Bjorklund that
%% appeared to be exactly the problem I'm seeing, and then noticed
%% that this was fixed in R11B-1. Unfortunately, though R11B-1 appears
%% to fix the code submitted by Martin, it does not fix my case.
%%
%% Function -just_compile_me/0-fun-2-/1 refers to undefined label 13
%% ./rath_R11compiler_bug.erl:none: internal error in beam_clean;
%% crash reason: {{case_clause,{'EXIT',{undefined_label,13}}},
%%                [{compile,'-select_passes/2-anonymous-2-',2},
%%                 {compile,'-internal_comp/4-anonymous-1-',2},
%%                 {compile,fold_comp,3},
%%                 {compile,internal_comp,4},
%%                 {compile,internal,3}]}
%%--------------------------------------------------------------------

test_error_R11_rath() ->
  just_compile_me_R11_rath().

just_compile_me_R11_rath() ->
  A = {6},
  try
    io:fwrite("")
  after
    fun () ->
      fun () -> {_} = A end
    end
  end.

%%----------------------------------------------------------------------
%% Program that crashed the R12B-0 compiler: internal error in v3_codegen
%%----------------------------------------------------------------------

-record(rec, {a = <<>> :: binary(), b = 42 :: integer()}).

test_error_R12_empty_bin_rec() ->
  42 = test_empty_bin_rec(#rec{}),
  ok.

test_empty_bin_rec(R) ->
  #rec{a = <<>>} = R,
  R#rec.b.

%%----------------------------------------------------------------------
%% From: Simon Cornish
%% Date: Jan 13, 2008
%%
%% The attached Erlang code demonstrates an R12B-0 bug with funs.
%% Compile and evaluate the two die/1 calls for two different failure modes.
%% It seems to me that the live register check for call_fun is off by one.
%%----------------------------------------------------------------------

-record(b, {c}).

test_bug_R12_cornish() ->
  {a2, a} = die(a),
  {a2, {b, c}} = die({b, c}),
  ok.

die(A) ->
  F = fun() -> {ok, A} end,
  if A#b.c =:= [] -> one;
     true ->
      case F() of
        {ok, A2} -> {a2, A2};
        _ -> three
      end
  end.

%%----------------------------------------------------------------------
%% From: Hunter Morris
%% Date: Nov 20, 2008
%%
%% The following code (tested with R12B-4 or R12B-5, vanilla compiler
%% options) produces a compiler crash.  It's nonsensical, and I realise
%% that andalso can be quite evil, but it's a crash nonetheless.
%%----------------------------------------------------------------------

test_crash_R12_morris() ->
  foo(42).

foo(Bar) when (is_integer(Bar) andalso Bar =:= 0) ; Bar =:= 42 ->
  ok.
    
%%--------------------------------------------------------------------
%% From: Paulo Sergio Almeida
%% Date: May 20, 2009
%%
%% The following code when compiled under R13B gives a compiler error.
%%   Function loop/1 refers to undefined label 6
%%   ./almeida_R13compiler_bug.erl:none: internal error in beam_peep;
%%   crash reason: {{case_clause,{'EXIT',{undefined_label,6}}},
%%                  [{compile,'-select_passes/2-anonymous-2-',2},
%%                   {compile,'-internal_comp/4-anonymous-1-',2},
%%--------------------------------------------------------------------

test_error_R13_almeida() ->
  self() ! {backup, 42, false},
  loop([]).

loop(Tids) ->
  receive
    {backup, Tid, Dumping} ->
      case Dumping of
        false -> ok;
        _ -> receive {logged, Tab, Tid} -> put({log, Tab}, Tid) end
      end,
      collect(Tid, Tids, [])
  end.

collect(_, _, _) -> ok.

%%--------------------------------------------------------------------
%% Fisher R13B01 compiler error
%%--------------------------------------------------------------------

test_error_R13B01_fisher() ->
  perform_select({foo, "42"}).

perform_select({Type, Keyval}) ->
  try
    if is_atom(Type) andalso length(Keyval) > 0 -> ok;
       true -> ok
    end
  catch
    _:_ -> fail
  end.

%%--------------------------------------------------------------------
%% From: Mikage Sawatari
%% Date: Jun 12, 2009
%%
%% I have the following compilation problem on Erlang R13B01.
%% Compiler reports "Internal consistency check failed".
%%--------------------------------------------------------------------

test_error_R13B01_sawatari() ->
  test_sawatari([1, null, 3], <<1, 2, 3>>).

test_sawatari([], _Bin) -> ok;
test_sawatari([H|T], Bin) ->
  _ = case H of
        null -> <<Bin/binary>>;
        _ -> ok
      end,
  test_sawatari(T, Bin).

%%--------------------------------------------------------------------

test_error_R13B01_whongo() ->
  S = "gazonk",
  S = orgno_alphanum(S),
  ok.

orgno_alphanum(Cs) ->
  [C || C <- Cs, ((C >= $0) andalso (C =< $9))
          orelse ((C >= $a) andalso (C =< $z))
          orelse ((C >= $A) andalso (C =< $Z))].

%%--------------------------------------------------------------------
%% I'm getting an Internal Consistency Check error when attempting to
%% build Wings3D on Mac OS X 10.4.2 (Erlang OTP R10B-6):
%%
%% erlc -pa /ebin +warn_unused_vars -I/include -I ../e3d -W +debug_info
%% '-Dwings_version="0.98.31"' -pa ../ebin -o../ebin wings_color.erl
%% wings_color: function internal_rgb_to_hsv/3+97:
%%    Internal consistency check failed - please report this bug.
%%    Instruction: {test,is_eq_exact,{f,80},[{x,0},{atom,error}]}
%%    Error:       {unsafe_instruction,{float_error_state,cleared}}:
%%
%% The problem is the interaction of the 'try' construct with the
%% handling of FP exceptions.
%%--------------------------------------------------------------------

test_error_try_wings() ->
  %% a call with a possible FP exception
  {199.99999999999997, 0.045454545454545456, 44} = rgb_to_hsv(42, 43, 44),
  ok.

rgb_to_hsv(R, G, B) ->
  Max = lists:max([R, G, B]),
  Min = lists:min([R, G, B]),
  V = Max,
  {Hue, Sat} = try
		 {if Min == B -> (G-Min)/(R+G-2.0*Min);
		     Min == R -> (1.0+(B-Min)/(B+G-2.0*Min));
		     Min == G -> (2.0+(R-Min)/(B+R-2.0*Min))
		  end * 120, (Max-Min)/Max}
	       catch
		 error:badarith -> {undefined, 0.0}
	       end,
  {Hue, Sat, V}.

%%--------------------------------------------------------------------
%% From: Ulf Norell
%% Date: Feb 28, 2014
%%
%% This caused an internal error in v3_codegen
%%--------------------------------------------------------------------

test_error_R16B03_norell() ->
  test_error_R16B03_norell(#r{}, gazonk).

test_error_R16B03_norell(Rec, Tag) ->
  is_record(Rec, Tag, 3) orelse ok.

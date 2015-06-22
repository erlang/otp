%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
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

-module(float_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 fpe/1,fp_drv/1,fp_drv_thread/1,denormalized/1,match/1,
	 bad_float_unpack/1, write/1, cmp_zero/1, cmp_integer/1, cmp_bignum/1]).
-export([otp_7178/1]).
-export([hidden_inf/1]).


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(3)),
    [{watchdog, Dog},{testcase,Func}|Config].

end_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [fpe, fp_drv, fp_drv_thread, otp_7178, denormalized,
     match, bad_float_unpack, write, {group, comparison}
     ,hidden_inf
    ].

groups() -> 
    [{comparison, [parallel], [cmp_zero, cmp_integer, cmp_bignum]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%
%% OTP-7178, list_to_float on very small numbers should give 0.0
%% instead of exception, i.e. ignore underflow.
%%
otp_7178(suite) ->
    [];
otp_7178(doc) ->
    ["test that list_to_float on very small numbers give 0.0"];
otp_7178(Config) when is_list(Config) ->
    ?line X = list_to_float("1.0e-325"),
    ?line true = (X < 0.00000001) and (X > -0.00000001),
    ?line Y = list_to_float("1.0e-325325325"),
    ?line true = (Y < 0.00000001) and (Y > -0.00000001),
    ?line {'EXIT', {badarg,_}} = (catch list_to_float("1.0e83291083210")),
    ok.

%% Forces floating point exceptions and tests that subsequent, legal,
%% operations are calculated correctly.  Original version by Sebastian
%% Strollo.

fpe(Config) when is_list(Config) ->
    ?line 0.0 = math:log(1.0),
    ?line {'EXIT', {badarith, _}} = (catch math:log(-1.0)),
    ?line 0.0 = math:log(1.0),
    ?line {'EXIT', {badarith, _}} = (catch math:log(0.0)),
    ?line 0.0 = math:log(1.0),
    ?line {'EXIT',{badarith,_}} = (catch 3.23e133 * id(3.57e257)),
    ?line 0.0 = math:log(1.0),
    ?line {'EXIT',{badarith,_}} = (catch 5.0/id(0.0)),
    ?line 0.0 = math:log(1.0),
    ok.


-define(ERTS_FP_CONTROL_TEST, 0).
-define(ERTS_FP_THREAD_TEST, 1).

fp_drv(Config) when is_list(Config) ->
    fp_drv_test(?ERTS_FP_CONTROL_TEST, ?config(data_dir, Config)).

fp_drv_thread(Config) when is_list(Config) ->
    %% Run in a separate node since it used to crash the emulator...
    ?line Parent = self(),
    ?line DrvDir = ?config(data_dir, Config),
    ?line {ok,Node} = start_node(Config),
    ?line Tester = spawn_link(Node,
			      fun () ->
				      Parent !
					  {self(),
					   fp_drv_test(?ERTS_FP_THREAD_TEST,
						       DrvDir)}
			      end),
    ?line Result = receive {Tester, Res} -> Res end,
    ?line stop_node(Node),
    ?line Result.

fp_drv_test(Test, DrvDir) ->
    ?line Drv = fp_drv,
    ?line try
	      begin
		  ?line case erl_ddll:load_driver(DrvDir, Drv) of
			    ok ->
				ok;
			    {error, permanent} ->
				ok;
			    {error, LoadError} ->
				exit({load_error,
				      erl_ddll:format_error(LoadError)});
			     LoadError ->
				exit({load_error, LoadError})
			end,
		  case open_port({spawn, Drv}, []) of
		      Port when is_port(Port) ->
			      try port_control(Port, Test, "") of
				  "ok" ->
				      0.0 = math:log(1.0),
				      ok;
				  [$s,$k,$i,$p,$:,$ | Reason] ->
				      {skipped, Reason};
				  Error ->
				      exit(Error)
			      after
				  Port ! {self(), close},
				receive {Port, closed} -> ok end,
				false = lists:member(Port, erlang:ports()),
				ok
			      end;
		      Error ->
			  exit({open_port_failed, Error})
		  end
	      end
	  catch
	      throw:Term -> ?line Term
	  after
	      erl_ddll:unload_driver(Drv)
	  end.

denormalized(Config) when is_list(Config) ->
    ?line Denormalized = 1.0e-307 / 1000,
    ?line roundtrip(Denormalized),
    ?line NegDenormalized = -1.0e-307 / 1000,
    ?line roundtrip(NegDenormalized),
    ok.

roundtrip(N) ->
    N = binary_to_term(term_to_binary(N)),
    N = binary_to_term(term_to_binary(N, [{minor_version,1}])).

match(Config) when is_list(Config) ->
    ?line one = match_1(1.0),
    ?line two = match_1(2.0),
    ?line a_lot = match_1(1000.0),
    ?line {'EXIT',_} = (catch match_1(0.5)),
    ok.
    
match_1(1.0) -> one;
match_1(2.0) -> two;
match_1(1000.0) -> a_lot.

%% Thanks to Per Gustafsson.

bad_float_unpack(Config) when is_list(Config) ->
    ?line Bin = <<-1:64>>,
    ?line -1 = bad_float_unpack_match(Bin),
    ok.

bad_float_unpack_match(<<F:64/float>>) -> F;
bad_float_unpack_match(<<I:64/integer-signed>>) -> I.

%% Exposes endianness issues.

write(Config) when is_list(Config) ->
    "1.0" = io_lib:write(1.0).

cmp_zero(_Config) ->
    cmp(0.5e-323,0).

cmp_integer(_Config) ->
    Axis = (1 bsl 53)-2.0, %% The point where floating points become unprecise
    span_cmp(Axis,2,200),
    cmp(Axis*Axis,round(Axis)).

cmp_bignum(_Config) ->
    span_cmp((1 bsl 58) - 1.0),%% Smallest bignum float

    %% Test when the big num goes from I to I+1 in size
    [span_cmp((1 bsl (32*I)) - 1.0) || I <- lists:seq(2,30)],

    %% Test bignum greater then largest float
    cmp((1 bsl (64*16)) - 1, (1 bsl (64*15)) * 1.0),
    %% Test when num is much larger then float
    [cmp((1 bsl (32*I)) - 1, (1 bsl (32*(I-2))) * 1.0) || I <- lists:seq(3,30)],
    %% Test when float is much larger than num
    [cmp((1 bsl (64*15)) * 1.0, (1 bsl (32*(I)))) || I <- lists:seq(1,29)],

    %% Test that all int == float works as they should
    [true = 1 bsl N == (1 bsl N)*1.0 || N <- lists:seq(0, 1023)],
    [true = (1 bsl N)*-1 == (1 bsl N)*-1.0 || N <- lists:seq(0, 1023)].

span_cmp(Axis) ->
    span_cmp(Axis, 25).
span_cmp(Axis, Length) ->
    span_cmp(Axis, round(Axis) bsr 52, Length).
span_cmp(Axis, Incr, Length) ->
    [span_cmp(Axis, Incr, Length, 1 bsl (1 bsl I)) || I <- lists:seq(0,6)].
%% This function creates tests around number axis. Both <, > and == is tested
%% for both negative and positive numbers.
%%
%% Axis: The number around which to do the tests eg. (1 bsl 58) - 1.0
%% Incr: How much to increment the test numbers inbetween each test.
%% Length: Length/2 is the number of Incr away from Axis to test on the
%%         negative and positive plane.
%% Diff: How much the float and int should differ when comparing
span_cmp(Axis, Incr, Length, Diff) ->
    [begin
	 cmp(round(Axis*-1.0)+Diff+I*Incr,Axis*-1.0+I*Incr),
	 cmp(Axis*-1.0+I*Incr,round(Axis*-1.0)-Diff+I*Incr)
     end || I <- lists:seq((Length div 2)*-1,(Length div 2))],
    [begin
	 cmp(round(Axis)+Diff+I*Incr,Axis+I*Incr),
	 cmp(Axis+I*Incr,round(Axis)-Diff+I*Incr)
     end || I <- lists:seq((Length div 2)*-1,(Length div 2))].

cmp(Big,Small) when is_float(Big) ->
    BigGtSmall = lists:flatten(
		 io_lib:format("~f > ~p",[Big,Small])),
    BigLtSmall = lists:flatten(
		 io_lib:format("~f < ~p",[Big,Small])),
    BigEqSmall = lists:flatten(
		 io_lib:format("~f == ~p",[Big,Small])),
    SmallGtBig = lists:flatten(
		   io_lib:format("~p > ~f",[Small,Big])),
    SmallLtBig = lists:flatten(
		   io_lib:format("~p < ~f",[Small,Big])),
    SmallEqBig = lists:flatten(
		   io_lib:format("~p == ~f",[Small,Big])),
    cmp(Big,Small,BigGtSmall,BigLtSmall,SmallGtBig,SmallLtBig,
	SmallEqBig,BigEqSmall);
cmp(Big,Small) when is_float(Small) ->
    BigGtSmall = lists:flatten(
		   io_lib:format("~p > ~f",[Big,Small])),
    BigLtSmall = lists:flatten(
		   io_lib:format("~p < ~f",[Big,Small])),
    BigEqSmall = lists:flatten(
		   io_lib:format("~p == ~f",[Big,Small])),
    SmallGtBig = lists:flatten(
		   io_lib:format("~f > ~p",[Small,Big])),
    SmallLtBig = lists:flatten(
		   io_lib:format("~f < ~p",[Small,Big])),
    SmallEqBig = lists:flatten(
		   io_lib:format("~f == ~p",[Small,Big])),
    cmp(Big,Small,BigGtSmall,BigLtSmall,SmallGtBig,SmallLtBig,
	SmallEqBig,BigEqSmall).

cmp(Big,Small,BigGtSmall,BigLtSmall,SmallGtBig,SmallLtBig,
    SmallEqBig,BigEqSmall) ->
    {_,_,_,true} = {Big,Small,BigGtSmall,
		    Big > Small},
    {_,_,_,false} = {Big,Small,BigLtSmall,
		     Big < Small},
    {_,_,_,false} = {Big,Small,SmallGtBig,
		     Small > Big},
    {_,_,_,true} = {Big,Small,SmallLtBig,
		    Small < Big},
    {_,_,_,false} = {Big,Small,SmallEqBig,
		     Small == Big},
    {_,_,_,false} = {Big,Small,BigEqSmall,
		     Big == Small}.

id(I) -> I.
    
start_node(Config) when is_list(Config) ->
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line Name = list_to_atom(atom_to_list(?MODULE)
			      ++ "-"
			      ++ atom_to_list(?config(testcase, Config))
			      ++ "-"
			      ++ integer_to_list(erlang:system_time(seconds))
			      ++ "-"
			      ++ integer_to_list(erlang:unique_integer([positive]))),
    ?line ?t:start_node(Name, slave, [{args, "-pa "++Pa}]).

stop_node(Node) ->
    ?t:stop_node(Node).


%% Test that operations that might hide infinite intermediate results
%% do not supress the badarith.
hidden_inf(Config) when is_list(Config) ->
    ZeroP = 0.0,
    ZeroN = id(ZeroP) * (-1),
    [hidden_inf_1(A, B, Z, 9.23e307)
     || A <- [1.0, -1.0, 3.1415, -0.00001000131, 3.57e257, ZeroP, ZeroN],
	B <- [1.0, -1.0, 3.1415, -0.00001000131, 3.57e257, ZeroP, ZeroN],
	Z <- [ZeroP, ZeroN]],
    ok.

hidden_inf_1(A, B, Zero, Huge) ->
    {'EXIT',{badarith,_}} = (catch (B / (A / Zero))),
    {'EXIT',{badarith,_}} = (catch (B * (A / Zero))),
    {'EXIT',{badarith,_}} = (catch (B / (Huge * Huge))),
    {'EXIT',{badarith,_}} = (catch (B * (Huge * Huge))),
    {'EXIT',{badarith,_}} = (catch (B / (Huge + Huge))),
    {'EXIT',{badarith,_}} = (catch (B * (Huge + Huge))),
    {'EXIT',{badarith,_}} = (catch (B / (-Huge - Huge))),
    {'EXIT',{badarith,_}} = (catch (B * (-Huge - Huge))).

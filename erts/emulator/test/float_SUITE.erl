%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

-module(float_SUITE).

-include("test_server.hrl").

-export([all/1,init_per_testcase/2,fin_per_testcase/2,
	 fpe/1,fp_drv/1,fp_drv_thread/1,denormalized/1,match/1,bad_float_unpack/1]).

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(3)),
    [{watchdog, Dog},{testcase,Func}|Config].

fin_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

all(suite) ->
    [fpe,fp_drv,fp_drv_thread,denormalized,match,bad_float_unpack].

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

id(I) -> I.
    
start_node(Config) when is_list(Config) ->
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line {A, B, C} = now(),
    ?line Name = list_to_atom(atom_to_list(?MODULE)
			      ++ "-"
			      ++ atom_to_list(?config(testcase, Config))
			      ++ "-"
			      ++ integer_to_list(A)
			      ++ "-"
			      ++ integer_to_list(B)
			      ++ "-"
			      ++ integer_to_list(C)),
    ?line ?t:start_node(Name, slave, [{args, "-pa "++Pa}]).

stop_node(Node) ->
    ?t:stop_node(Node).

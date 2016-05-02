%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2014. All Rights Reserved.
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

-module(dirty_nif_SUITE).

%%-define(line_trace,true).
-define(CHECK(Exp,Got), check(Exp,Got,?LINE)).
%%-define(CHECK(Exp,Got), Exp = Got).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2,
	 dirty_nif/1, dirty_nif_send/1,
	 dirty_nif_exception/1, call_dirty_nif_exception/1,
	 dirty_scheduler_exit/1]).

-define(nif_stub,nif_stub_error(?LINE)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [dirty_nif,
     dirty_nif_send,
     dirty_nif_exception,
     dirty_scheduler_exit].

init_per_suite(Config) ->
    try erlang:system_info(dirty_cpu_schedulers) of
	N when is_integer(N), N > 0 ->
	    case lib_loaded() of
		false ->
		    ok = erlang:load_nif(
			   filename:join(?config(data_dir, Config),
					 "dirty_nif_SUITE"), []);
		true ->
		    ok
	    end,
	    Config
    catch _:_ ->
	    {skipped, "No dirty scheduler support"}
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Case, Config) ->
    [{testcase, Case} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

dirty_nif(Config) when is_list(Config) ->
    Val1 = 42,
    Val2 = "Erlang",
    Val3 = list_to_binary([Val2, 0]),
    {Val1, Val2, Val3} = call_dirty_nif(Val1, Val2, Val3),
    LargeArray = lists:duplicate(1000, ok),
    LargeArray = call_dirty_nif_zero_args(),
    ok.

dirty_nif_send(Config) when is_list(Config) ->
    Parent = self(),
    Pid = spawn_link(fun() ->
			     Self = self(),
			     {ok, Self} = receive_any(),
			     Parent ! {ok, Self}
		     end),
    {ok, Pid} = send_from_dirty_nif(Pid),
    {ok, Pid} = receive_any(),
    ok.

dirty_nif_exception(Config) when is_list(Config) ->
    try
	%% this checks that the expected exception occurs when the
	%% dirty NIF returns the result of enif_make_badarg
	%% directly
	call_dirty_nif_exception(1),
	ct:fail(expected_badarg)
    catch
	error:badarg ->
	    [{?MODULE,call_dirty_nif_exception,[1],_}|_] =
		erlang:get_stacktrace(),
	    ok
    end,
    try
	%% this checks that the expected exception occurs when the
	%% dirty NIF calls enif_make_badarg at some point but then
	%% returns a value that isn't an exception
	call_dirty_nif_exception(0),
	ct:fail(expected_badarg)
    catch
	error:badarg ->
	    [{?MODULE,call_dirty_nif_exception,[0],_}|_] =
		erlang:get_stacktrace(),
	    ok
    end,
    %% this checks that a dirty NIF can raise various terms as
    %% exceptions
    ok = nif_raise_exceptions(call_dirty_nif_exception).

nif_raise_exceptions(NifFunc) ->
    ExcTerms = [{error, test}, "a string", <<"a binary">>,
                42, [1,2,3,4,5], [{p,1},{p,2},{p,3}]],
    lists:foldl(fun(Term, ok) ->
                        try
                            erlang:apply(?MODULE,NifFunc,[Term]),
                            ct:fail({expected,Term})
                        catch
                            error:Term ->
                                [{?MODULE,NifFunc,[Term],_}|_] = erlang:get_stacktrace(),
                                ok
                        end
                end, ok, ExcTerms).

dirty_scheduler_exit(Config) when is_list(Config) ->
    try
        erlang:system_info(dirty_cpu_schedulers),
        dirty_scheduler_exit_test(Config)
    catch
        error:badarg ->
            {skipped, "No dirty scheduler support"}
    end.

dirty_scheduler_exit_test(Config) ->
    {ok, Node} = start_node(Config, "+SDio 1"),
    Path = proplists:get_value(data_dir, Config),
    NifLib = filename:join(Path, atom_to_list(?MODULE)),
    [ok] = mcall(Node,
                 [fun() ->
                          ok = erlang:load_nif(NifLib, []),
			  Start = erlang:monotonic_time(milli_seconds),
                          ok = test_dirty_scheduler_exit(),
			  End = erlang:monotonic_time(milli_seconds),
			  io:format("Time=~p ms~n", [End-Start]),
			  ok
                  end]),
    stop_node(Node),
    ok.

test_dirty_scheduler_exit() ->
    process_flag(trap_exit,true),
    test_dse(10,[]).
test_dse(0,Pids) ->
    timer:sleep(100),
    kill_dse(Pids,[]);
test_dse(N,Pids) ->
    Pid = spawn_link(fun () ->
			     F = fun dirty_sleeper/0,
			     F()
		     end),
    test_dse(N-1,[Pid|Pids]).

kill_dse([],Killed) ->
    wait_dse(Killed);
kill_dse([Pid|Pids],AlreadyKilled) ->
    exit(Pid,kill),
    kill_dse(Pids,[Pid|AlreadyKilled]).

wait_dse([]) ->
    ok;
wait_dse([Pid|Pids]) ->
    receive
        {'EXIT',Pid,Reason} ->
	    killed = Reason
    end,
    wait_dse(Pids).

%%
%% Internal...
%%

receive_any() ->
    receive M -> M end.	     

start_node(Config) ->
    start_node(Config, "").

start_node(Config, Args) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-"
			++ atom_to_list(proplists:get_value(testcase, Config))
			++ "-"
			++ integer_to_list(erlang:system_time(seconds))
			++ "-"
			++ integer_to_list(erlang:unique_integer([positive]))),
    test_server:start_node(Name, slave, [{args, "-pa "++Pa++" "++Args}]).

stop_node(Node) ->
    test_server:stop_node(Node).

mcall(Node, Funs) ->
    Parent = self(),
    Refs = lists:map(fun (Fun) ->
                             Ref = make_ref(),
                             spawn_link(Node,
                                        fun () ->
                                                Res = Fun(),
                                                unlink(Parent),
                                                Parent ! {Ref, Res}
                                        end),
                             Ref
                     end, Funs),
    lists:map(fun (Ref) ->
                      receive
                          {Ref, Res} ->
                              Res
                      end
              end, Refs).

%% The NIFs:
lib_loaded() -> false.
call_nif_schedule(_,_) -> ?nif_stub.
call_dirty_nif(_,_,_) -> ?nif_stub.
send_from_dirty_nif(_) -> ?nif_stub.
call_dirty_nif_exception(_) -> ?nif_stub.
call_dirty_nif_zero_args() -> ?nif_stub.
dirty_sleeper() -> ?nif_stub.

nif_stub_error(Line) ->
    exit({nif_not_loaded,module,?MODULE,line,Line}).

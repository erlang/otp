%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-module(beam_ssa_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
         calls/1,tuple_matching/1,recv/1,maps/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,test_lib:parallel(),
      [tuple_matching,
       calls,
       recv,
       maps
      ]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

calls(Config) ->
    Ret = {return,value,Config},
    Ret = fun_call(fun(42) -> ok end, Ret),
    Ret = apply_fun(fun(a, b) -> ok end, [a,b], Ret),
    Ret = apply_mfa(test_lib, id, [anything], Ret),
    {'EXIT',{badarg,_}} = (catch call_error()),
    {'EXIT',{badarg,_}} = (catch call_error(42)),
    5 = start_it([erlang,length,1,2,3,4,5]),
    ok.

fun_call(Fun, X0) ->
    X = id(X0),
    Fun(42),
    X.

apply_fun(Fun, Args, X0) ->
    X = id(X0),
    apply(Fun, Args),
    X.

apply_mfa(Mod, Name, Args, X0) ->
    X = id(X0),
    apply(Mod, Name, Args),
    X.

call_error() ->
    error(badarg),
    ok.

call_error(I) ->
    <<I:(-8)>>,
    ok.

start_it([_|_]=MFA) ->
    case MFA of
	[M,F|Args] -> M:F(Args)
    end.

tuple_matching(_Config) ->
    do_tuple_matching({tag,42}).

do_tuple_matching(Arg) ->
    Res = do_tuple_matching_1(Arg),
    Res = do_tuple_matching_2(Arg),
    Res = do_tuple_matching_3(Arg),
    Res.

do_tuple_matching_1({tag,V}) ->
    {ok,V}.

do_tuple_matching_2(Tuple) when is_tuple(Tuple) ->
    Size = tuple_size(Tuple),
    if
        Size =:= 2 ->
            {ok,element(2, Tuple)}
    end.

do_tuple_matching_3(Tuple) when is_tuple(Tuple) ->
    Size = tuple_size(Tuple),
    if
        Size =:= 2 ->
            2 = id(Size),
            {ok,element(2, Tuple)}
    end.

-record(reporter_state, {res,run_config}).
-record(run_config, {report_interval=0}).

recv(_Config) ->
    Parent = self(),

    %% Test sync_wait_mon/2.
    Succ = fun() -> Parent ! {ack,self(),{result,42}} end,
    {result,42} = sync_wait_mon(spawn_monitor(Succ), infinity),

    Down = fun() -> exit(down) end,
    {error,down} = sync_wait_mon(spawn_monitor(Down), infinity),

    Exit = fun() ->
                   Self = self(),
                   spawn(fun() -> exit(Self, kill_me) end),
                   receive _ -> ok end
           end,
    {error,kill_me} = sync_wait_mon(spawn_monitor(Exit), infinity),

    Timeout = fun() -> receive _ -> ok end end,
    {error,timeout} = sync_wait_mon(spawn_monitor(Timeout), 0),

    %% Test reporter_loop/1.
    {a,Parent} = reporter_loop(#reporter_state{res={a,Parent},
                                               run_config=#run_config{}}),

    %% Test bad_sink/0.
    bad_sink(),

    %% Test tricky_recv_1/0.
    self() ! 1,
    a = tricky_recv_1(),
    self() ! 2,
    b = tricky_recv_1(),

    %% Test tricky_recv_2/0.
    self() ! 1,
    {1,yes} = tricky_recv_2(),
    self() ! 2,
    {2,maybe} = tricky_recv_2(),

    %% Test 'receive after infinity' in try/catch.
    Pid = spawn(fun recv_after_inf_in_try/0),
    exit(Pid, done),

    %% Test tricky_recv_3().
    self() ! {{self(),r0},{1,42,"name"}},
    {Parent,r0,[<<1:32,1:8,42:8>>,"name",0]} = tricky_recv_3(),
    self() ! {{self(),r1},{2,99,<<"data">>}},
    {Parent,r1,<<1:32,2:8,99:8,"data">>} = tricky_recv_3(),

    %% Test tricky_recv_4().
    self() ! {[self(),r0],{1,42,"name"}},
    {Parent,r0,[<<1:32,1:8,42:8>>,"name",0]} = tricky_recv_4(),
    self() ! {[self(),r1],{2,99,<<"data">>}},
    {Parent,r1,<<1:32,2:8,99:8,"data">>} = tricky_recv_4(),

    ok.

sync_wait_mon({Pid, Ref}, Timeout) ->
    receive
	{ack,Pid,Return} ->
	    erlang:demonitor(Ref, [flush]),
	    Return;
	{'DOWN',Ref,_Type,Pid,Reason} ->
	    {error,Reason};
	{'EXIT',Pid,Reason} ->
	    erlang:demonitor(Ref, [flush]),
	    {error,Reason}
    after Timeout ->
            erlang:demonitor(Ref, [flush]),
            exit(Pid, kill),
            {error,timeout}
    end.

reporter_loop(State) ->
    RC = State#reporter_state.run_config,
    receive after RC#run_config.report_interval ->
                    State#reporter_state.res
    end.

bad_sink() ->
    {ok,Pid} = my_spawn(self()),
    %% The get_tuple_element instruction for the matching
    %% above was sinked into the receive loop. That will
    %% not work (and would be bad for performance if it
    %% would work).
    receive
        {ok,Pid} ->
            ok;
        error ->
            exit(failed)
    end,
    exit(Pid, kill).

my_spawn(Parent) ->
    Pid = spawn(fun() ->
                        Parent ! {ok,self()},
                        receive _ -> ok end
                end),
    {ok,Pid}.

tricky_recv_1() ->
    receive
        X=1 ->
            id(42),
            a;
        X=2 ->
            b
    end,
    case X of
        1 -> a;
        2 -> b
    end.

tricky_recv_2() ->
    receive
        X=1 ->
            Y = case id(X) of
                    1 -> yes;
                    _ -> no
                end,
            a;
        X=2 ->
            Y = maybe,
            b
    end,
    {X,Y}.

recv_after_inf_in_try() ->
    try
        %% Used to crash beam_kernel_to_ssa.
        receive after infinity -> ok end
    catch
	_A:_B ->
	    receive after infinity -> ok end
    end.

tricky_recv_3() ->
    {Pid, R, Request} =
	receive
	    {{Pid0,R0}, {1, Proto0, Name0}} ->
		{Pid0, R0,
		 [<<1:32, 1:8, Proto0:8>>,Name0,0]};
	    {{Pid1,R1}, {2, Proto1, Data1}}  ->
		{Pid1, R1,
		 <<1:32, 2:8, Proto1:8, Data1/binary>>}
	end,
    id({Pid,R,Request}).

tricky_recv_4() ->
    {Pid, R, Request} =
	receive
	    {[Pid0,R0], {1, Proto0, Name0}} ->
		{Pid0, R0,
		 [<<1:32, 1:8, Proto0:8>>,Name0,0]};
	    {[Pid1,R1], {2, Proto1, Data1}}  ->
		{Pid1, R1,
		 <<1:32, 2:8, Proto1:8, Data1/binary>>}
	end,
    id({Pid,R,Request}).

maps(_Config) ->
    {'EXIT',{{badmatch,#{}},_}} = (catch maps_1(any)),
    ok.

maps_1(K) ->
    _ = id(42),
    #{K:=V} = #{},
    V.

%% The identity function.
id(I) -> I.

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
         calls/1,tuple_matching/1,recv/1,maps/1,
         cover_ssa_dead/1,combine_sw/1,share_opt/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,test_lib:parallel(),
      [tuple_matching,
       calls,
       recv,
       maps,
       cover_ssa_dead,
       combine_sw,
       share_opt
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
    do_tuple_matching({tag,42}),

    true = is_two_tuple({a,b}),
    false = is_two_tuple({a,b,c}),
    false = is_two_tuple(atom),

    ok.

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

is_two_tuple(Arg) ->
    case is_tuple(Arg) of
        false -> false;
        true -> tuple_size(Arg) == 2
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

-record(wx_ref, {type=any_type,ref=any_ref}).

cover_ssa_dead(_Config) ->
    str = format_str(str, escapable, [], true),
    [iolist,str] = format_str(str, escapable, iolist, true),
    bad = format_str(str, not_escapable, [], true),
    bad = format_str(str, not_escapable, iolist, true),
    bad = format_str(str, escapable, [], false),
    bad = format_str(str, escapable, [], bad),

    DefWxRef = #wx_ref{},
    {DefWxRef,77,9999,[]} = contains(#wx_ref{}, 77, 9999),
    {DefWxRef,77.0,9999,[]} = contains(#wx_ref{}, 77.0, 9999),
    {DefWxRef,77,9999.0,[]} = contains(#wx_ref{}, 77, 9999.0),
    {DefWxRef,77.0,9999.0,[]} = contains(#wx_ref{}, 77.0, 9999.0),
    {any_type,any_ref,42,43,[option]} = contains(#wx_ref{}, {42,43}, [option]),
    {any_type,any_ref,42,43,[]} = contains(#wx_ref{}, {42,43}, []),
    {any_type,any_ref,42.0,43,[]} = contains(#wx_ref{}, {42.0,43}, []),
    {any_type,any_ref,42,43.0,[]} = contains(#wx_ref{}, {42,43.0}, []),
    {any_type,any_ref,42.0,43.0,[]} = contains(#wx_ref{}, {42.0,43.0}, []),

    nope = conv_alub(false, '=:='),
    ok = conv_alub(true, '=:='),
    ok = conv_alub(true, none),
    error = conv_alub(false, none),

    {false,false} = eval_alu(false, false, false),
    {true,false}  = eval_alu(false, false, true),
    {false,true}  = eval_alu(false, true, false),
    {false,false} = eval_alu(false, true, true),
    {false,true}  = eval_alu(true, false, false),
    {false,false} = eval_alu(true, false, true),
    {true,true}   = eval_alu(true, true, false),
    {false,true}  = eval_alu(true, true, true),

    100.0 = percentage(1.0, 0.0),
    100.0 = percentage(1, 0),
    0.0 = percentage(0, 0),
    0.0 = percentage(0.0, 0.0),
    40.0 = percentage(4.0, 10.0),
    60.0 = percentage(6, 10),

    %% Cover '=:=', followed by '=/='.
    false = 'cover__=:=__=/='(41),
    true = 'cover__=:=__=/='(42),
    false = 'cover__=:=__=/='(43),

    %% Cover '<', followed by '=/='.
    true = 'cover__<__=/='(41),
    false = 'cover__<__=/='(42),
    false = 'cover__<__=/='(43),

    %% Cover '=<', followed by '=/='.
    true = 'cover__=<__=/='(41),
    true = 'cover__=<__=/='(42),
    false = 'cover__=<__=/='(43),

    %% Cover '>=', followed by '=/='.
    false = 'cover__>=__=/='(41),
    true = 'cover__>=__=/='(42),
    true = 'cover__>=__=/='(43),

    %% Cover '>', followed by '=/='.
    false = 'cover__>__=/='(41),
    false = 'cover__>__=/='(42),
    true = 'cover__>__=/='(43),

    ok.

'cover__=:=__=/='(X) when X =:= 42 -> X =/= 43;
'cover__=:=__=/='(_) -> false.

'cover__<__=/='(X) when X < 42 -> X =/= 42;
'cover__<__=/='(_) -> false.

'cover__=<__=/='(X) when X =< 42 -> X =/= 43;
'cover__=<__=/='(_) -> false.

'cover__>=__=/='(X) when X >= 42 -> X =/= 41;
'cover__>=__=/='(_) -> false.

'cover__>__=/='(X) when X > 42 -> X =/= 42;
'cover__>__=/='(_) -> false.

format_str(Str, FormatData, IoList, EscChars) ->
    Escapable = FormatData =:= escapable,
    case id(Str) of
        IoStr when Escapable, EscChars, IoList == [] ->
            id(IoStr);
        IoStr when Escapable, EscChars ->
            [IoList,id(IoStr)];
        _ ->
            bad
    end.

contains(This, X, Y) when is_record(This, wx_ref), is_number(X), is_number(Y) ->
    {This,X,Y,[]};
contains(#wx_ref{type=ThisT,ref=ThisRef}, {CX,CY}, Options)
  when is_number(CX), is_number(CY), is_list(Options) ->
    {ThisT,ThisRef,CX,CY,Options}.

conv_alub(HasDst, CmpOp) ->
    case (not HasDst) andalso CmpOp =/= none of
        true -> nope;
        false ->
            case HasDst of
                false -> error;
                true -> ok
            end
    end.

eval_alu(Sign1, Sign2, N) ->
    V = (Sign1 andalso Sign2 andalso (not N))
        or ((not Sign1) andalso (not Sign2) andalso N),
    C = (Sign1 andalso Sign2)
          or ((not N) andalso (Sign1 orelse Sign2)),
    {V,C}.

percentage(Divident, Divisor) ->
    if Divisor == 0 andalso Divident /= 0 ->
            100.0;
       Divisor == 0 ->
            0.0;
       true ->
            Divident / Divisor * 100
    end.

combine_sw(_Config) ->
    [a] = do_comb_sw_1(a),
    [b,b] = do_comb_sw_1(b),
    [c] = do_comb_sw_1(c),
    [c] = do_comb_sw_1(c),
    [] = do_comb_sw_1(z),

    [a] = do_comb_sw_2(a),
    [b2,b1] = do_comb_sw_2(b),
    [c] = do_comb_sw_2(c),
    [c] = do_comb_sw_2(c),
    [] = do_comb_sw_2(z),

    ok.

do_comb_sw_1(X) ->
    put(?MODULE, []),
    if
        X == a; X == b ->
            put(?MODULE, [X|get(?MODULE)]);
        true ->
            ok
    end,
    if
        X == b; X == c ->
            put(?MODULE, [X|get(?MODULE)]);
        true ->
            ok
    end,
    erase(?MODULE).

do_comb_sw_2(X) ->
    put(?MODULE, []),
    case X of
        a ->
            put(?MODULE, [a|get(?MODULE)]);
        b ->
            put(?MODULE, [b1|get(?MODULE)]);
        _ ->
            ok
    end,
    case X of
        b ->
            put(?MODULE, [b2|get(?MODULE)]);
        c ->
            put(?MODULE, [c|get(?MODULE)]);
        _ ->
            ok
    end,
    erase(?MODULE).

share_opt(_Config) ->
    ok = do_share_opt(0).

do_share_opt(A) ->
    %% The compiler would be stuck in an infinite loop in beam_ssa_share.
    case A of
        0 -> a;
        1 -> b;
        2 -> c
    end,
    receive after 1 -> ok end.


%% The identity function.
id(I) -> I.

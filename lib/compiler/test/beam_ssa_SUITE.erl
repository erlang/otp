%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2024. All Rights Reserved.
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
         cover_ssa_dead/1,combine_sw/1,share_opt/1,
         beam_ssa_dead_crash/1,stack_init/1,
         mapfoldl/0,mapfoldl/1,
         grab_bag/1,redundant_br/1,
         coverage/1,normalize/1,
         trycatch/1,gh_6599/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [mapfoldl,
     {group,p}].

groups() ->
    [{p,test_lib:parallel(),
      [tuple_matching,
       calls,
       recv,
       maps,
       cover_ssa_dead,
       combine_sw,
       share_opt,
       beam_ssa_dead_crash,
       stack_init,
       grab_bag,
       redundant_br,
       coverage,
       normalize,
       trycatch,
       gh_6599
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

    {_,ok} = cover_call(id(true)),
    {_,ok} = cover_call(id(false)),
    {'EXIT',{{case_clause,ok},_}} = catch cover_call(id(ok)),

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

cover_call(A) ->
    case A =/= ok of
        B ->
            {(term_to_binary(ok)),
             case ok of
                 _  when B -> ok
             end}
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
    {2,'maybe'} = tricky_recv_2(),

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

    %% Test tricky_recv_5/0.
    self() ! 1,
    a = tricky_recv_5(),
    self() ! 2,
    b = tricky_recv_5(),

    %% Test tricky_recv_5a/0.
    self() ! 1,
    a = tricky_recv_5a(),
    self() ! 2,
    b = tricky_recv_5a(),
    self() ! any,
    b = tricky_recv_5a(),

    %% tricky_recv_6/0 is a compile-time error.
    tricky_recv_6(),

    recv_coverage(),

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
            Y = 'maybe',
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

%% beam_ssa_pre_codegen would accidentally create phi nodes on critical edges
%% when fixing up receives; the call to id/2 can either succeed or land in the
%% catch block, and we added a phi node to its immediate successor.
tricky_recv_5() ->
    try
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
        end
    catch
        _:_ -> c
    end.

%% beam_ssa_pre_codegen would find the wrong exit block when fixing up
%% receives.
tricky_recv_5a() ->
    try
        receive
            X=1 ->
                id(42),
                a;
            X=_ ->
                b
        end,
        %% The following is the code in the common exit block.
        if X =:= 1 -> a;
           true -> b
        end
    catch
        %% But this code with the landingpad instruction was found,
        %% because it happened to occur before the true exit block
        %% in the reverse post order.
        _:_ -> c
    end.


%% When fixing tricky_recv_5, we introduced a compiler crash when the common
%% exit block was ?EXCEPTION_BLOCK and floats were in the picture.
tricky_recv_6() ->
    RefA = make_ref(),
    RefB = make_ref(),
    receive
        {RefA, Number} -> Number + 1.0;
        {RefB, Number} -> Number + 2.0
    after 0 ->
        ok
    end.

recv_coverage() ->
    self() ! 1,
    a = recv_coverage_1(),
    self() ! 2,
    b = recv_coverage_1(),

    self() ! 1,
    a = recv_coverage_2(),
    self() ! 2,
    b = recv_coverage_2(),

    ok.

%% Similar to tricky_recv_5/0, but provides test coverage for the #b_switch{}
%% terminator.
recv_coverage_1() ->
    receive
        X=1 ->
            %% Jump to common exit block through #b_switch{list=L}
            case id(0) of
                0 -> a;
                1 -> b;
                2 -> c;
                3 -> d
            end;
        X=2 ->
            %% Jump to common exit block through #b_switch{fail=F}
            case id(42) of
                0 -> exit(quit);
                1 -> exit(quit);
                2 -> exit(quit);
                3 -> exit(quit);
                _ -> b
            end
    end,
    case X of
        1 -> a;
        2 -> b
    end.

%% Similar to recv_coverage_1/0, providing test coverage for #b_br{}.
recv_coverage_2() ->
    receive
        X=1 ->
            A = id(1),
            %% Jump to common exit block through #b_br{succ=S}.
            if
                A =:= 1 -> a;
                true -> exit(quit)
            end;
        X=2 ->
            A = id(2),
            %% Jump to common exit block through #b_br{fail=F}.
            if
                A =:= 1 -> exit(quit);
                true -> a
            end
    end,
    case X of
        1 -> a;
        2 -> b
    end.

maps(_Config) ->
    {'EXIT',{{badmatch,#{}},_}} = (catch maps_1(any)),

    {jkl,nil,nil} = maps_2(#{abc => 0, jkl => 0}),
    {def,ghi,abc} = maps_2(#{abc => 0, def => 0}),
    {def,ghi,jkl} = maps_2(#{def => 0, jkl => 0}),
    {mno,nil,abc} = maps_2(#{abc => 0, mno => 0, jkl => 0}),
    {jkl,nil,nil} = maps_2(#{jkl => 0}),
    error = maps_2(#{}),

    [] = maps_3(),

    {'EXIT',{{badmap,true},_}} = catch maps_4(id(true), id(true)),
    error = maps_4(id(#{}), id(true)),
    error = maps_4(id(#{}), id(#{})),

    ok.

maps_1(K) ->
    _ = id(42),
    #{K:=V} = #{},
    V.

maps_2(Map) ->
    Res = maps_2a(Map),
    Res = maps_2b(Map),
    Res.

maps_2a(#{} = Map) ->
    case case Abc = is_map_key(abc, Map) of
             false -> false;
             _ -> is_map_key(def, Map)
         end of
        true ->
            {def, ghi, abc};
        false ->
            case case Jkl = is_map_key(jkl, Map) of
                     false -> false;
                     _ -> is_map_key(def, Map)
                 end of
                true ->
                    {def, ghi, jkl};
                false ->
                    case case Abc of
                             false -> false;
                             _ -> is_map_key(mno, Map)
                         end of
                        true ->
                            {mno, nil, abc};
                        false ->
                            case Jkl of
                                true -> {jkl, nil, nil};
                                false -> error
                            end
                    end
            end
    end.

maps_2b(#{}=Map) ->
    case case is_map_key(abc, Map) of
             false -> false;
             _ -> is_map_key(def, Map)
         end of
        true ->
            {def, ghi, abc};
        false ->
            case case is_map_key(jkl, Map) of
                     false -> false;
                     _ -> is_map_key(def, Map)
                 end of
                true ->
                    {def, ghi, jkl};
                false ->
                    case case is_map_key(abc, Map) of
                             false -> false;
                             _ -> is_map_key(mno, Map)
                         end of
                        true ->
                            {mno, nil, abc};
                        false ->
                            case is_map_key(jkl, Map) of
                                true -> {jkl, nil, nil};
                                false -> error
                            end
                    end
            end
    end.

%% Cover code in beam_ssa_codegen.
maps_3() ->
    [] = case #{} of
             #{ok := {}} ->
                 ok;
             _ ->
                 []
         end -- [].

maps_4(A, B = A) when B; A ->
    A#{ok := ok},
    try A of
        B -> B
    after
        ok
    end#{ok := ok};
maps_4(_, _) ->
    error.

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

    {'EXIT',{{badmatch,42},_}} = (catch #{key => abs(("a" = id(42)) /= teacher)}),

    <<>> = id(<< V || V <- [], V andalso false >>),

    false = id(([] = id([])) =/= []),

    ok.

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
    ok = do_share_opt_1(0),
    ok = do_share_opt_2(),
    ok = do_share_opt_3(),
    ok.

do_share_opt_1(A) ->
    %% The compiler would be stuck in an infinite loop in beam_ssa_share.
    case A of
        0 -> a;
        1 -> b;
        2 -> c
    end,
    receive after 1 -> ok end.

do_share_opt_2() ->
    ok = sopt_2({[pointtopoint], [{dstaddr,any}]}, ok),
    ok = sopt_2({[broadcast], [{broadaddr,any}]}, ok),
    ok = sopt_2({[], []}, ok),
    ok.

sopt_2({Flags, Opts}, ok) ->
    Broadcast = lists:member(broadcast, Flags),
    P2P = lists:member(pointtopoint, Flags),
    case Opts of
        %% The following two clauses would be combined to one, silently
        %% discarding the guard test of the P2P variable.
        [{broadaddr,_}|Os] when Broadcast ->
            sopt_2({Flags, Os}, ok);
        [{dstaddr,_}|Os] when P2P ->
            sopt_2({Flags, Os}, ok);
        [] ->
            ok
    end.

do_share_opt_3() ->
    true = sopt_3(id(ok)),
    false = sopt_3(id(nok)),
    ok.

sopt_3(X) ->
    %% Must be one line to trigger bug.
    case X of ok -> id(?LINE), true; _ -> id(?LINE), false end.

beam_ssa_dead_crash(_Config) ->
    not_A_B = do_beam_ssa_dead_crash(id(false), id(true)),
    not_A_not_B = do_beam_ssa_dead_crash(false, false),
    neither = do_beam_ssa_dead_crash(true, false),
    neither = do_beam_ssa_dead_crash(true, true),
    ok.

do_beam_ssa_dead_crash(A, B) ->
    %% beam_ssa_dead attempts to shortcut branches that branch other
    %% branches. When a two-way branch is encountered, beam_ssa_dead
    %% will simulate execution along both paths, in the hope that both
    %% paths happens to end up in the same place.
    %%
    %% During the simulated execution of this function, the boolean
    %% variable for a `br` instruction would be replaced with the
    %% literal atom `nil`, which is not allowed, and would crash the
    %% compiler. In practice, during the actual execution, control
    %% would never be transferred to that `br` instruction when the
    %% variable in question had the value `nil`.
    %%
    %% beam_ssa_dead has been updated to immediately abort the search
    %% along the current path if there is an attempt to substitute a
    %% non-boolean value into a `br` instruction.

    case
        case not A of
            false ->
                false;
            true ->
                B
        end
    of
        V
            when
                V /= nil
                andalso
                V /= false ->
            not_A_B;
        _ ->
            case
                case not A of
                    false ->
                        false;
                    true ->
                        not B
                end
            of
                true ->
                    not_A_not_B;
                false ->
                    neither
            end
    end.

stack_init(_Config) ->
    6 = stack_init(a, #{a => [1,2,3]}),
    0 = stack_init(missing, #{}),
    ok.

stack_init(Key, Map) ->
    %% beam_ssa_codegen would wrongly assume that y(0) would always be
    %% initialized by the `get_map_elements` instruction that follows, and
    %% would set up the stack frame using an `allocate` instruction and
    %% would not generate an `init` instruction to initialize y(0).
    Res = case Map of
              #{Key := Elements} ->
                  %% Elements will be assigned to y(0) if the key Key exists.
                  lists:foldl(fun(El, Acc) ->
                                      Acc + El
                              end, 0, Elements);
              #{} ->
                  %% y(0) will be left uninitialized when the key is not
                  %% present in the map.
                  0
          end,
    %% y(0) would be uninitialized here if the key was not present in the map
    %% (if the second clause was executed).
    id(Res).

%% Test that compiler "optimizations" don't rewrite mapfold/3 to the
%% equivalent of slow_mapfoldl/3.
mapfoldl() ->
    {N,Size} = mapfoldl_limits(),
    {Time,_} = timer:tc(fun() ->
                                mapfoldl(fun(Sz, _) ->
                                                 erlang:garbage_collect(),
                                                 {Sz,erlang:make_tuple(Sz, a)}
                                         end, [], [Size])
                        end),
    Seconds = 15 + ceil(10 * Time * N / 1_000_000),
    io:format("~p seconds timetrap\n", [Seconds]),
    [{timetrap,{seconds,Seconds}}].

mapfoldl(_Config) ->
    test_mapfoldl_implementations(),
    F = fun(Sz, _) ->
                erlang:garbage_collect(),
                {Sz,erlang:make_tuple(Sz, a)}
        end,
    {N,Size} = mapfoldl_limits(),
    List = lists:duplicate(N, Size),
    {List,Tuple} = mapfoldl(F, [], List),
    {List,Tuple} = fast_mapfoldl(F, [], List),
    Size = tuple_size(Tuple),
    ok.

mapfoldl_limits() ->
    {1_000,100_000}.

test_mapfoldl_implementations() ->
    Seq = lists:seq(1, 10),
    F = fun(N, Sum) -> {N,Sum+N} end,
    {Seq,55} = mapfoldl(F, 0, Seq),
    {Seq,55} = fast_mapfoldl(F, 0, Seq),
    {Seq,55} = slow_mapfoldl(F, 0, Seq),
    ok.

mapfoldl(F, Acc0, [Hd|Tail]) ->
    {R,Acc1} = F(Hd, Acc0),
    {Rs,Acc2} = mapfoldl(F, Acc1, Tail),
    {[R|Rs],Acc2};
mapfoldl(F, Acc, []) when is_function(F, 2) -> {[],Acc}.

%% Here is an illustration of how the compiler used to sink
%% get_tuple_element instructions in a way that would cause all
%% versions of the accumulator to be kept until the end. The compiler
%% now uses a heuristic to only sink get_tuple_element instructions if
%% that would cause fewer values to be saved in the stack frame.
slow_mapfoldl(F, Acc0, [Hd|Tail]) ->
    Res1 = F(Hd, Acc0),
    %% By saving the Res1 tuple, all intermediate accumulators will be
    %% kept to the end.
    Res2 = slow_mapfoldl(F, element(2, Res1), Tail),
    {[element(1, Res1)|element(1, Res2)],element(2, Res2)};
slow_mapfoldl(F, Acc, []) when is_function(F, 2) -> {[],Acc}.

%% Here is an illustration how the compiler should compile mapfoldl/3
%% to avoid keeping all intermediate accumulators. Note that
%% slow_mapfoldl/3 and fast_mapfoldl/3 use the same amount of stack
%% space.
fast_mapfoldl(F, Acc0, [Hd|Tail]) ->
    Res1 = F(Hd, Acc0),
    R = element(1, Res1),
    Res2 = fast_mapfoldl(F, element(2, Res1), Tail),
    {[R|element(1, Res2)],element(2, Res2)};
fast_mapfoldl(F, Acc, []) when is_function(F, 2) -> {[],Acc}.

grab_bag(_Config) ->
    {'EXIT',_} = (catch grab_bag_1()),
    {'EXIT',_} = (catch grab_bag_2()),
    {'EXIT',_} = (catch grab_bag_3()),
    {'EXIT',_} = (catch grab_bag_4()),
    {'EXIT',{function_clause,[{?MODULE,grab_bag_5,[a,17],_}|_]}} =
        (catch grab_bag_5(a, 17)),
    way = grab_bag_6(face),
    no_match = grab_bag_6("ABC"),
    no_match = grab_bag_6(any),
    ok = grab_bag_7(),
    [] = grab_bag_8(),
    ok = grab_bag_9(),
    whatever = grab_bag_10(ignore, whatever),
    other = grab_bag_11(),
    {'EXIT',_} = (catch grab_bag_12()),
    {'EXIT',{{badmatch,[]},_}} = (catch grab_bag_13()),
    timeout = grab_bag_14(),
    ?MODULE = grab_bag_15(?MODULE),

    error = grab_bag_16a(timeout_value),
    {'EXIT',{timeout_value,_}} = (catch grab_bag_16a(whatever)),
    {'EXIT',{timeout_value,_}} = (catch grab_bag_16b(whatever)),
    timeout_value = grab_bag_16b(error),

    fact = grab_bag_17(),

    {'EXIT',{{try_clause,[]},[_|_]}} = catch grab_bag_18(),

    {'EXIT',{{badmatch,[whatever]},[_|_]}} = catch grab_bag_19(),

    {'EXIT',{if_clause,[_|_]}} = catch grab_bag_20(),

    6 = grab_bag_21(id(64)),
    {'EXIT',{badarith,_}} = catch grab_bag_21(id(a)),

    false = grab_bag_22(),

    ok.

grab_bag_1() ->
    %% beam_kernel_to_ssa would crash when attempting to translate a make_fun
    %% instruction without a destination variable.
    (catch fun () -> 15 end)(true#{}).

grab_bag_2() ->
    %% is_guard_cg_safe/1 will be called with #cg_unreachable{}, which was
    %% not handled.
    27
        or
    try
        try
            x#{}
        catch
            _:_ ->
                []
        end
    after
        false
    end.

grab_bag_3() ->
    case
        fun (V0)
              when
                  %% The only thing left after optimizations would be
                  %% a bs_add instruction not followed by succeeded,
                  %% which would crash beam_ssa_codegen because there
                  %% was no failure label available.
                  binary_part(<<>>,
                              <<V0:V0/unit:196>>) ->
                []
        end
    of
        <<>> ->
            []
    end.

grab_bag_4() ->
    %% beam_kernel_to_ssa would crash because there was a #cg_phi{}
    %% instruction that was not referenced from any #cg_break{}.
    case $f of
        V0 ->
            try
                try fy of
                    V0 ->
                        fu
                catch
                    throw:$s ->
                        fy
                end
            catch
                error:#{#{[] + [] => []} := false} when [] ->
                    fy
            after
                ok
            end
    end.

grab_bag_5(_A, _B) when <<business:(node(power))>> ->
    true.

grab_bag_6(face) ->
    way;
grab_bag_6("ABC") when (node([]))#{size(door) => $k} ->
    false;
grab_bag_6(_) ->
    no_match.

grab_bag_7() ->
    catch
        case
            case 1.6 of
                %% The hd([] call will be translated to erlang:error(badarg).
                %% This case exports two variables in Core Erlang (the
                %% return value of the case and V). beam_kernel_to_ssa was not
                %% prepared to handle a call to error/1 which is supposed to
                %% export two variables.
                <<0.5:(hd([])),V:false>> ->
                    ok
            end
        of
            _ ->
                V
        end,
        ok.

%% ssa_opt_sink would crash if sys_core_fold had not been run.
grab_bag_8() ->
    try
        []
    catch
        _:_ ->
            try
                []
            catch
                _:any:_ ->
                    a
            end;
        _:right ->
            b
    end.

%% The ssa_opt_try optimization would leave a succeeded:body
%% instruction followed by a #b_ret{} terminator, which would crash
%% beam_ssa_pre_codegen.
grab_bag_9() ->
    catch
        <<1 || 99, [] <- hour>> bsr false,
        ok.

grab_bag_10(_, V) ->
    %% This function needs a stack frame in order to preserve V.
    fun() -> ok end,
    V.

grab_bag_11() ->
    try 0 of
        false -> error;
        true -> ok;
        _ -> other
    catch
        _:_ ->
            caught
    end.

grab_bag_12() ->
    %% beam_ssa_pre_codegen would try to place the created map in x1.
    %% That would not be safe because x0 is not initialized.
    check_process_code(1, (#{})#{key := teacher}),
    ok.

grab_bag_13() ->
    %% If sys_core_fold was skipped, beam_ssa_beam would leave
    %% unreachable code with invalid phi nodes.
    case <<810:true>> = [] of
        <<709:false>> ->
            ok;
        whatever ->
            case 42 of
                175 ->
                    {ok,case "b" of
                            $X -> time
                        end}
            end
    end.

grab_bag_14() ->
    %% If optimizations were turned off, beam_ssa_pre_codegen would
    %% sanitize the binary construction instruction, replacing it with
    %% a call to erlang:error/1, which is not allowed in a receive.
    receive
        #{<<42:(-1)>> := _} ->
            ok
    after 0 ->
            timeout
    end.

grab_bag_15(V) ->
    %% Instead of:
    %%
    %%    move x0, y0
    %%    move y0, x0
    %%
    %% a swap instruction would be emitted by beam_ssa_codegen:
    %%
    %%    swap x0, y0
    %%
    case [] of
        [] -> V
    end:all(),
    V.

grab_bag_16a(V) ->
    try
        catch 22,
    receive
    after bad ->
            not_reached
    end
    catch
        _:V ->
            error
    end.

grab_bag_16b(V) ->
    try
        receive
        after get() ->
                ok
        end
    catch
        V:Reason ->
            Reason
    end.

grab_bag_17() ->
    try "xwCl" of
        V when V ->
            <<[] || V>>;
        [_|_] ->
            %% Constant propagation in beam_ssa_codegen:prefer_xregs/2
            %% would produce get_hd and get_tl instructions with literal
            %% operands.
            fact
    catch
        _:_ ->
            []
    end.

grab_bag_18() ->
    try 0 of
        _V0 ->
            bnot false
    after
        try [] of
            wrong ->
                ok
        catch
            _:_ when false ->
                error
        end
    end.

grab_bag_19() ->
    ([<<bad/utf8>>] =
         %% beam_ssa_pre_codegen would produce single-valued phi
         %% nodes, which in turn would cause the constant propagation
         %% in beam_ssa_codegen:prefer_xregs/2 to produce get_hd and
         %% get_tl instructions with literal operands.
         try
             [whatever]
         catch
             _:_ when false ->
                 ok
         end) ! (some_atom ++ <<>>).

grab_bag_20() ->
    %% Similarly to grab_bag_19, beam_ssa_pre_codegen would produce
    %% single-valued phi nodes. The fix for grab_bag_19 would not
    %% suffice because several phi nodes were involved.
    {[_ | _] =
         receive
             list ->
                 "list";
             1 when day ->
                 []
         after
             0 ->
                 if
                     false ->
                         error
                 end
         end,
     try
         ok
     catch
         error:_ ->
             error
     end}.

%% With the `no_copt` and `no_ssa_opt` options, an internal
%% consistency error would be reported:
%%
%% Internal consistency check failed - please report this bug.
%% Instruction: {test_heap,2,2}
%% Error:       {{x,0},not_live}:
grab_bag_21(A) ->
    _ = id(0),
    grab_bag_21(ok, A div 10, node(), [-1]).

grab_bag_21(_, D, _, _) ->
    D.

%% GH-7128: With optimizations disabled, the code would fail to
%% load with the following message:
%%
%%    beam/beam_load.c(367): Error loading function
%%      beam_ssa_no_opt_SUITE:grab_bag_22/0: op get_list: Sdd:
%%         bad tag 2 for destination
grab_bag_22() ->
    maybe
        [_ | _] ?= ((true xor true) andalso foo),
        bar ?= id(42)
    end.

redundant_br(_Config) ->
    {false,{x,y,z}} = redundant_br_1(id({x,y,z})),
    {true,[[a,b,c]]} = redundant_br_1(id([[[a,b,c]]])),
    ok.

redundant_br_1(Specs0) ->
    {Join,Specs} =
        if
            is_list(hd(hd(Specs0))) -> {true,hd(Specs0)};
            true -> {false,Specs0}
        end,
    id({Join,Specs}).

-record(coverage, {name}).

coverage(_Config) ->

    %% Cover beam_ssa_codegen:force_reg/2
    no_match = case true of
                   <<_:42>> -> true;
                   _ -> no_match
              end,

    no_match = case [] of
                   <<$f:1.7>> -> ok;
                   _ -> no_match
               end,
    {'EXIT',{{badmatch,$T},_}} = (catch coverage_1()),

    error = coverage_2(),
    ok = coverage_3(),
    #coverage{name=whatever} = coverage_4(),
    {'EXIT',{{badrecord,ok},_}} = catch coverage_5(),

    ok.

coverage_1() ->
    <<area/signed-bitstring>> = $T.

coverage_2() when << []:<<0/native>> >> -> ok;
coverage_2() -> error.

coverage_3() ->
    %% Cover a line in beam_ssa_pre_codegen:need_frame_1/2.
    get(),
    ok.

coverage_4() ->
    try
        << <<42>> || false >>,
        #coverage{}
    catch
        _:_ ->
            error
    end#coverage{name = whatever}.

coverage_5() ->
    try
        << <<42>> || false >>,
        ok
    catch
        _:_ ->
            error
    end#coverage{name = whatever}.

%% Test beam_ssa:normalize/1, especially that argument types are
%% correctly updated when arguments are swapped.
normalize(_Config) ->
    normalize_commutative({bif,'band'}),
    normalize_commutative({bif,'+'}),

    normalize_noncommutative({bif,'div'}),

    ok.

-record(b_var, {name}).
-record(b_literal, {val}).

normalize_commutative(Op) ->
    A = #b_var{name=a},
    B = #b_var{name=b},
    Lit = #b_literal{val=42},

    normalize_same(Op, [A,B]),
    normalize_same(Op, [A,Lit]),

    normalize_swapped(Op, [Lit,A]),

    ok.

normalize_noncommutative(Op) ->
    A = #b_var{name=a},
    B = #b_var{name=b},
    Lit = #b_literal{val=42},

    normalize_same(Op, [A,B]),
    normalize_same(Op, [A,Lit]),

    ArgTypes0 = [{1,beam_types:make_integer(0, 1023)}],
    I1 = make_bset(ArgTypes0, Op, [Lit,A]),
    I1 = beam_ssa:normalize(I1),

    ok.

normalize_same(Op, Args) ->
    I0 = make_bset(#{}, Op, Args),
    I0 = beam_ssa:normalize(I0),

    ArgTypes0 = [{0,beam_types:make_integer(0, 1023)}],
    I1 = make_bset(ArgTypes0, Op, Args),
    I1 = beam_ssa:normalize(I1),

    case Args of
        [#b_var{},#b_var{}] ->
            ArgTypes1 = [{0,beam_types:make_integer(0, 1023)},
                         {1,beam_types:make_integer(42)}],
            I2 = make_bset(ArgTypes1, Op, Args),
            I2 = beam_ssa:normalize(I2);
        [_,_] ->
            ok
    end,

    ok.

normalize_swapped(Op, [#b_literal{}=Lit,#b_var{}=Var]=Args) ->
    EmptyAnno = #{},
    I0 = make_bset(EmptyAnno, Op, Args),
    {b_set,EmptyAnno,#b_var{name=1000},Op,[Var,Lit]} = beam_ssa:normalize(I0),

    EmptyTypes = #{arg_types => #{}},
    I1 = make_bset(EmptyTypes, Op, Args),
    {b_set,EmptyTypes,#b_var{name=1000},Op,[Var,Lit]} = beam_ssa:normalize(I1),

    IntRange = beam_types:make_integer(0, 1023),
    ArgTypes0 = [{1,IntRange}],
    I2 = make_bset(ArgTypes0, Op, Args),
    {[{0,IntRange}],Op,[Var,Lit]} = unpack_bset(beam_ssa:normalize(I2)),

    LitType = beam_types:make_type_from_value(Lit),

    ArgTypes1 = [{0,LitType}],
    I3 = make_bset(ArgTypes1, Op, Args),
    {[],Op,[Var,Lit]} = unpack_bset(beam_ssa:normalize(I3)),

    ArgTypes2 = [{0,LitType},{1,IntRange}],
    I4 = make_bset(ArgTypes1, Op, Args),
    {[],Op,[Var,Lit]} = unpack_bset(beam_ssa:normalize(I4)),

    ok.

make_bset(ArgTypes, Op, Args) when is_list(ArgTypes) ->
    Anno = #{arg_types => maps:from_list(ArgTypes)},
    {b_set,Anno,#b_var{name=1000},Op,Args};
make_bset(Anno, Op, Args) when is_map(Anno) ->
    {b_set,Anno,#b_var{name=1000},Op,Args}.

unpack_bset({b_set,Anno,{b_var,1000},Op,Args}) ->
    ArgTypes = maps:get(arg_types, Anno, #{}),
    {lists:sort(maps:to_list(ArgTypes)),Op,Args}.

trycatch(_Config) ->
    8 = trycatch_1(),

    ok = trycatch_2(id(ok)),
    ok = trycatch_2(id(z)),

    false = trycatch_3(id(42)),

    ok.

trycatch_1() ->
    try B = (A = bit_size(iolist_to_binary("a"))) rem 1 of
        _ ->
            A;
        _ ->
            B
    after
        ok
    end.

trycatch_2(A) ->
    try not (B = (ok >= A)) of
        B ->
            iolist_size(maybe
                            [] ?= B,
                            <<>> ?= list_to_binary(ok)
                        end);
        _ ->
            ok
    after
        ok
    end.

trycatch_3(A) ->
    try erlang:bump_reductions(A) of
        B ->
            try not (C = (B andalso is_number(ok))) of
                C ->
                    ok andalso ok;
                _ ->
                    C
            catch
                _ ->
                    ok
            end
    after
        ok
    end.

%% GH-6599. beam_validator would not realize that the code was safe.
gh_6599(_Config) ->
    ok = gh_6599_1(id(42), id(42)),
    #{ok := ok} = gh_6599_1(id(#{ok => 0}), id(#{ok => 0})),

    {'EXIT',{{try_clause,#{ok:=ok}},_}} =
        catch gh_6599_2(id(whatever), id(#{0 => whatever})),

    ok = gh_6599_3(id(true), id(true)),
    {'EXIT',{function_clause,_}} = catch gh_6599_3(id(false), id(false)),
    0.0 = gh_6599_3(id(0.0), id(0.0)),

    {'EXIT',{{badmatch,true},_}} = catch gh_6599_4(id(false)),

    {'EXIT',{{badmatch,ok},_}} = catch gh_6599_5(id([a]), id(#{0 => [a]}), id([a])),

    #{ok := ok} = gh_6599_6(id(#{}), id(#{})),
    {'EXIT',{{badmap,a},_}} = catch gh_6599_6(id(a), id(a)),

    {'EXIT',{{badarg,ok},_}} = catch gh_6599_7(id([a]), id([a])),

    ok.

gh_6599_1(X, X) when is_integer(X) ->
    ok;
gh_6599_1(Y, Y = #{}) ->
    Y#{ok := ok}.

gh_6599_2(X, #{0 := X, 0 := Y}) ->
    try #{ok => ok} of
        Y ->
            bnot (Y = X)
    after
        ok
    end.

gh_6599_3(X, X) when X ->
    ok;
gh_6599_3(X, X = 0.0) ->
    X + X.

gh_6599_4(X) ->
    Y =
        try
            false = X
        catch
            _ ->
                ok
        end /= ok,
    X = Y,
    false = Y,
    0 = ok.

%% Crashes in beam_ssa_type because a type assertion fails.
gh_6599_5(X, #{0 := X, 0 := Y}, Y=[_ | _]) ->
    try
        Y = ok
    catch
        _ ->
            [_ | []] = Y = X
    end.

gh_6599_6(A, B = A) ->
    A#{},
    case A of B -> B end#{ok => ok}.

gh_6599_7(X, Y) ->
    try Y of
        X ->
            (id(
                try ([_ | _] = Y) of
                    X ->
                        ok
                after
                    ok
                end
            ) orelse X) bsl 0
    after
        ok
    end.


%% The identity function.
id(I) -> I.

%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Define to run outside of test server
%%%
%%% -define(STANDALONE,1).
%%%
%%%
%%% Define for debug output
%%%
%%% -define(debug,1).

-module(trace_meta_SUITE).

%% Exported end user tests
-export([basic_test/0, return_test/0, on_and_off_test/0, stack_grow_test/0,
	 info_test/0, tracer_test/0, combo_test/0, nosilent_test/0]).

%% Internal exports
-export([exported/1, exported_wrap/1, loop/4, id/1, receiver/1]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test server related stuff
%%

-ifdef(STANDALONE).
-define(config(A,B),config(A,B)).
-export([config/2]).
-else.
-include_lib("common_test/include/ct.hrl").
-endif.
 
-ifdef(debug).
-ifdef(STANDALONE).
-define(line, erlang:display({?MODULE,?LINE}), ).
-endif.
-define(dbgformat(A,B),io:format(A,B)).
-else.
-ifdef(STANDALONE).
-define(line, noop, ).
-endif.
-define(dbgformat(A,B),noop).
-endif.
 
-ifdef(STANDALONE).
config(priv_dir,_) ->
    ".".
-else.
%% When run in test server.
-export([all/0, suite/0,
	 init_per_testcase/2, end_per_testcase/2, not_run/1]).
-export([basic/1, return/1, on_and_off/1, stack_grow/1, 
	 info/1, tracer/1, combo/1, nosilent/1]).
	 
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    shutdown(),
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 5}}].

all() -> 
case test_server:is_native(trace_meta_SUITE) of
  true -> [not_run];
  false ->
      [basic, return, on_and_off, stack_grow, info, tracer,
       combo, nosilent]
end.


not_run(Config) when is_list(Config) -> 
    {skipped,"Native code"}.

%% Tests basic meta trace
basic(Config) when is_list(Config) ->
    basic_test().

%% Tests return trace
return(Config) when is_list(Config) ->
    return_test().
 
%% Tests turning trace parameters on and off
on_and_off(Config) when is_list(Config) ->
    on_and_off_test().
 
%% Tests the stack growth during return traces
stack_grow(Config) when is_list(Config) ->
    stack_grow_test().
 
%% Tests the trace_info BIF
info(Config) when is_list(Config) ->
    info_test().
 
%% Tests stopping and changing tracer process
tracer(Config) when is_list(Config) ->
    tracer_test().

%% Tests combining local call trace with meta trace
combo(Config) when is_list(Config) ->
    combo_test().

%% Tests that meta trace is not silenced by the silent process flag
nosilent(Config) when is_list(Config) ->
    nosilent_test().

-endif.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Result examination macros

-define(CT(P,MFA),{trace,P,call,MFA}).
-define(CTT(P, MFA),{trace_ts,P,call,MFA,{_,_,_}}).
-define(RF(P,MFA,V),{trace,P,return_from,MFA,V}).
-define(RFT(P,MFA,V),{trace_ts,P,return_from,MFA,V,{_,_,_}}).
-define(RT(P,MFA),{trace,P,return_to,MFA}).
-define(RTT(P,MFA),{trace_ts,P,return_to,MFA,{_,_,_}}).
-define(NM, receive_no_next(100)).

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The Tests
%%%

basic_test() ->
    Pid = setup(),
    erlang:trace_pattern({?MODULE,'_','_'},[],[meta]),
    [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CTT(Pid,{?MODULE,exported_wrap,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,exported,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,local,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,local2,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,local_tail,[1]}) = receive_next(),
    erlang:trace_pattern({?MODULE,'_','_'},false,[meta]),
    [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    ?NM,
    [1,1,1,0] = lambda_slave(fun() ->
                                     exported_wrap(1)
                             end),
    ?NM,
    erlang:trace_pattern({?MODULE,'_','_'},[],[meta]),
    [1,1,1,0] = lambda_slave(fun() ->
                                     exported_wrap(1)
                             end),
    ?CTT(Pid,{?MODULE,_,_}) = receive_next(), %% The fun
    ?CTT(Pid,{?MODULE,exported_wrap,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,exported,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,local,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,local2,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,local_tail,[1]}) = receive_next(),
    erlang:trace_pattern({?MODULE,'_','_'},false,[meta]),
    shutdown(),
    ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

return_test() ->
    Pid = setup(),
    erlang:trace_pattern({?MODULE,'_','_'},[{'_',[],[{return_trace}]}],
			 [meta]),
    erlang:trace_pattern({erlang,phash2,'_'},[{'_',[],[{return_trace}]}],
			 [meta]),
    [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CTT(Pid,{?MODULE,exported_wrap,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,exported,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,local,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,local2,[1]}) = receive_next(),
    ?CTT(Pid,{?MODULE,local_tail,[1]}) = receive_next(),
    ?CTT(Pid,{erlang,phash2,[1,1]}) = receive_next(),
    ?RFT(Pid,{erlang,phash2,2},0) = receive_next(),
    ?RFT(Pid,{?MODULE,local_tail,1},[1,0]) = receive_next(),
    ?RFT(Pid,{?MODULE,local2,1},[1,0]) = receive_next(),
    ?RFT(Pid,{?MODULE,local,1},[1,1,0]) = receive_next(),
    ?RFT(Pid,{?MODULE,exported,1},[1,1,1,0]) = receive_next(),
    ?RFT(Pid,{?MODULE,exported_wrap,1},[1,1,1,0]) = receive_next(),
    shutdown(),
    ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

on_and_off_test() ->
    Pid = setup(),
    1 = erlang:trace_pattern({?MODULE,local_tail,1},[],[meta]),
    LocalTail = fun() ->
                        local_tail(1)
                end,
    [1,0] = lambda_slave(LocalTail),
    ?CTT(Pid,{?MODULE,local_tail,[1]}) = receive_next(),
    0 = erlang:trace_pattern({?MODULE,local_tail,1},[],[global]),
    [1,0] = lambda_slave(LocalTail),
    ?NM,
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1},[],[meta]),
    [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CTT(Pid,{?MODULE,exported_wrap,[1]}) = receive_next(),
    1 = erlang:trace_pattern({erlang,phash2,2},[],[meta]),
    [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CTT(Pid,{?MODULE,exported_wrap,[1]}) = receive_next(),
    ?CTT(Pid,{erlang,phash2,[1,1]}) = receive_next(),
    shutdown(),
    erlang:trace_pattern({'_','_','_'},false,[meta]),
    N = erlang:trace_pattern({erlang,'_','_'},true,[meta]),
    case erlang:trace_pattern({erlang,'_','_'},false,[meta]) of
        N -> ok;
        Else ->
            exit({number_mismatch, {expected, N}, {got, Else}})
    end,
    case erlang:trace_pattern({erlang,'_','_'},false,[meta]) of
        N -> ok;
        Else2 ->
            exit({number_mismatch, {expected, N}, {got, Else2}})
    end,
    %% ?NM,
    %% Can't check for erlang:*/* stuff since common_test or test_server
    %% will likely call list_to_binary in the logger. just drain any potential
    %% message
    ok = flush(),
    ok.

flush() ->
    receive _ -> flush() after 0 -> ok end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stack_grow_test() ->    
    Pid = setup(),
    1 = erlang:trace_pattern({?MODULE,loop,4},
				   [{'_',[],[{return_trace}]}],[meta]),
    Num = 1 bsl 15,
    Surface =
	fun (This, ?RFT(P,{?MODULE,loop,4},N), N) when P == Pid->
		if N == Num ->
			?NM,
			ok;
		   true ->
			This(This, receive_next(), N+1)
		end
	end,
    Dive =
	fun (This, ?CTT(P,{?MODULE,loop,[{hej,hopp},[a,b,c],4.5,N]}), N)
	    when P == Pid->
		if N == 0 ->
			Surface(Surface, receive_next(), 0);
		   true ->
			This(This, receive_next(), N-1)
		end
	end,
    apply_slave(?MODULE,loop,[{hej,hopp},[a,b,c],4.5,Num]),
%     apply_slave_async(?MODULE,loop,[{hej,hopp},[a,b,c],4.5,Num]),
%     List = collect(test_server:seconds(5)),
    ok = Dive(Dive, receive_next(), Num),
    ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info_test() ->
    setup(),
    Prog = [{['$1'],[{is_integer,'$1'}],[{message, false}]}, {'_',[],[]}],
    Self = self(),
    GoOn = make_ref(),
    Pid =
	spawn_link(
	  fun () ->
		  erlang:trace_pattern({?MODULE,exported_wrap,1}, 
                                       Prog, [{meta, Self}]),
		  Self ! {self(), GoOn}
	  end),
    receive {Pid, GoOn} -> ok end,
    {traced,false} = erlang:trace_info({?MODULE,exported_wrap,1}, traced),
    {match_spec, false} =
	erlang:trace_info({?MODULE,exported_wrap,1}, match_spec),
    {meta, Self} = erlang:trace_info({?MODULE,exported_wrap,1}, meta),
    {meta_match_spec, MMS} =
	erlang:trace_info({?MODULE,exported_wrap,1}, meta_match_spec),
    case MMS of
        Prog ->
            ok;
        Wrong ->
            exit({bad_result, {erlang,trace_info,
                               [{?MODULE,exported_wrap,1},
                                meta_match_spec]},
                  {expected, Prog}, {got, Wrong}})
    end,
    erlang:garbage_collect(self()),
    receive
    after 1 ->
              ok
    end,
    io:format("~p~n",[MMS]),
    {meta_match_spec,MMS2} =
	erlang:trace_info({?MODULE,exported_wrap,1}, meta_match_spec),
    io:format("~p~n",[MMS2]),
    case MMS2 of
        Prog ->
            ok;
        Wrong2 ->
            exit({bad_result, {erlang,trace_info,
                               [{?MODULE,exported_wrap,1},
                                meta_match_spec]},
                  {expected, Prog}, {got, Wrong2}})
    end,
    {all, [_|_]=L} = erlang:trace_info({?MODULE,exported_wrap,1}, all),
    {value, {meta, Self}} = lists:keysearch(meta, 1, L),
    {value, {meta_match_spec, MMS}} = lists:keysearch(meta_match_spec, 1, L),

    erlang:trace_pattern({?MODULE,exported_wrap,1}, true, [meta]),
    {meta_match_spec, []} =
	erlang:trace_info({?MODULE,exported_wrap,1}, meta_match_spec),

    erlang:trace_pattern({?MODULE,exported_wrap,1}, false, [meta]),
    {meta, false} = erlang:trace_info({?MODULE,exported_wrap,1}, meta),
    {meta_match_spec, false} =
	erlang:trace_info({?MODULE,exported_wrap,1}, meta_match_spec),
    {all, false} = erlang:trace_info({?MODULE,exported_wrap,1}, all),
    shutdown(),
    ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tracer_test() ->
    Slave = setup(),
    Self = self(),

    MatchSpec = [{'_',[],[{return_trace}]}],
    Tracer1 = spawn_link(fun () -> relay_n(3, Self) end),
    Setter = spawn_link(
               fun () ->
                       erlang:trace_pattern({?MODULE,receiver,1},
                                            MatchSpec,
                                            [{meta,Tracer1}]),
                       erlang:trace_pattern({erlang,phash2,2},
                                            MatchSpec,
                                            [{meta,Tracer1}]),
                       Self ! {self(), done}
               end),
    receive {Setter, done} -> ok end,
    Ref = make_ref(),
    apply_slave_async(?MODULE, receiver, [Ref]),
    {Tracer1,?CTT(Slave,{?MODULE,receiver,[Ref]})} = receive_next(100),
    {Tracer1,?CTT(Slave,{erlang,phash2,[1,1]})} = receive_next(100),
    {Tracer1,?RFT(Slave,{erlang,phash2,2},0)} = receive_next(100),
    %% Initiate a return_trace that will fail since the tracer just stopped
    Slave ! Ref,
    receive_no_next(100),
    %% The breakpoint has not been hit since the tracer stopped
    {meta,Tracer1} =
	erlang:trace_info({?MODULE,receiver,1}, meta),
    {meta_match_spec, MatchSpec} =
	erlang:trace_info({?MODULE,receiver,1}, meta_match_spec),
    {meta,Tracer1} =
	erlang:trace_info({erlang,phash2,2}, meta),
    {meta_match_spec, MatchSpec} =
	erlang:trace_info({erlang,phash2,2}, meta_match_spec),
    %% Initiate trace messages that will fail
    Ref2 = make_ref(),
    apply_slave_async(?MODULE, receiver, [Ref2]),
    Slave ! Ref2,
    receive_no_next(100),
    {meta,[]} =
	erlang:trace_info({?MODULE,receiver,1}, meta),
    {meta_match_spec, MatchSpec} = 
	erlang:trace_info({?MODULE,receiver,1}, meta_match_spec),
    {meta,[]} =
	erlang:trace_info({erlang,phash2,2}, meta),
    {meta_match_spec, MatchSpec} =
	erlang:trace_info({erlang,phash2,2}, meta_match_spec),
    %% Change tracer
    Tracer2 = spawn_link(fun () -> relay_n(4, Self) end),
    erlang:trace_pattern({?MODULE,receiver,1},
                         MatchSpec,
                         [{meta,Tracer2}]),
    erlang:trace_pattern({erlang,phash2,2},
                         MatchSpec,
                         [{meta,Tracer2}]),
    Ref3 = make_ref(),
    apply_slave_async(?MODULE, receiver, [Ref3]),
    {Tracer2,?CTT(Slave,{?MODULE,receiver,[Ref3]})} = receive_next(),
    {Tracer2,?CTT(Slave,{erlang,phash2,[1,1]})} = receive_next(),
    {Tracer2,?RFT(Slave,{erlang,phash2,2},0)} = receive_next(),
    %% Change tracer between call trace and return trace
    Tracer3 = spawn_link(fun () -> relay_n(4, Self) end),
    erlang:trace_pattern({?MODULE,receiver,1},
                         MatchSpec,
                         [{meta,Tracer3}]),
    erlang:trace_pattern({erlang,phash2,2},
                         MatchSpec,
                         [{meta,Tracer3}]),
    Slave ! Ref3,
    %% The return trace should still come from Tracer2
    {Tracer2,?RFT(Slave,{?MODULE,receiver,1},Ref3)} = receive_next(),
    Ref4 = make_ref(),
    %% Now should Tracer3 be used
    apply_slave_async(?MODULE, receiver, [Ref4]),
    Slave ! Ref4,
    {Tracer3,?CTT(Slave,{?MODULE,receiver,[Ref4]})} = receive_next(),
    {Tracer3,?CTT(Slave,{erlang,phash2,[1,1]})} = receive_next(),
    {Tracer3,?RFT(Slave,{erlang,phash2,2},0)} = receive_next(),
    {Tracer3,?RFT(Slave,{?MODULE,receiver,1},Ref4)} = receive_next(),
    %% The breakpoint has not been hit since the tracer stopped
    {meta,Tracer3} = erlang:trace_info({?MODULE,receiver,1}, meta),
    {meta_match_spec, MatchSpec} =
	erlang:trace_info({?MODULE,receiver,1}, meta_match_spec),
    {meta,Tracer3} =
	erlang:trace_info({erlang,phash2,2}, meta),
    {meta_match_spec, MatchSpec} =
	erlang:trace_info({erlang,phash2,2}, meta_match_spec),
    shutdown(),
    ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

combo_test() ->
    Slave = setup(),
    Self = self(),

    MatchSpec = [{'_',[],[{return_trace}]}],
    Flags = lists:sort([call, return_to]),
    LocalTracer = spawn_link(fun () -> relay_n(6, Self) end),
    MetaTracer = spawn_link(fun () -> relay_n(4, Self) end),
    1 = erlang:trace_pattern({?MODULE,receiver,1},
                             MatchSpec,
                             [local,{meta,MetaTracer}]),
    1 = erlang:trace_pattern({erlang,phash2,2},
                             MatchSpec,
                             [local,{meta,MetaTracer}]),
    1 = erlang:trace(Slave, true,
                     [{tracer,LocalTracer} | Flags]),
    %%
    {all, TraceInfo1} =
	erlang:trace_info({?MODULE,receiver,1}, all),
    {meta,MetaTracer} =
	erlang:trace_info({?MODULE,receiver,1}, meta),
    {value,{meta,MetaTracer}} =
	lists:keysearch(meta, 1, TraceInfo1),
    {meta_match_spec,MatchSpec} =
	erlang:trace_info({?MODULE,receiver,1}, meta_match_spec),
    {value,{meta_match_spec,MatchSpec}} =
	lists:keysearch(meta_match_spec, 1, TraceInfo1),
    {traced,local} =
	erlang:trace_info({?MODULE,receiver,1}, traced),
    {value,{traced,local}} =
	lists:keysearch(traced, 1, TraceInfo1),
    {match_spec,MatchSpec} =
	erlang:trace_info({?MODULE,receiver,1}, match_spec),
    {value,{match_spec,MatchSpec}} =
	lists:keysearch(match_spec, 1, TraceInfo1),
    %%
    {all, TraceInfo2} =
	erlang:trace_info({erlang,phash2,2}, all),
    {meta,MetaTracer} =
	erlang:trace_info({erlang,phash2,2}, meta),
    {value,{meta,MetaTracer}} =
	lists:keysearch(meta, 1, TraceInfo2),
    {meta_match_spec,MatchSpec} =
	erlang:trace_info({erlang,phash2,2}, meta_match_spec),
    {value,{meta_match_spec,MatchSpec}} =
	lists:keysearch(meta_match_spec, 1, TraceInfo2),
    {traced,local} =
	erlang:trace_info({erlang,phash2,2}, traced),
    {value,{traced,local}} =
	lists:keysearch(traced, 1, TraceInfo2),
    {match_spec,MatchSpec} =
	erlang:trace_info({erlang,phash2,2}, match_spec),
    {value,{match_spec,MatchSpec}} =
	lists:keysearch(match_spec, 1, TraceInfo2),
    %%
    {flags,Flags1} = erlang:trace_info(Slave, flags),
    Flags = lists:sort(Flags1),
    {tracer,LocalTracer} = erlang:trace_info(Slave, tracer),
    %%
    Ref = make_ref(),
    apply_slave_async(?MODULE, receiver, [Ref]),
    Slave ! Ref,
    ?CTT(Slave,{?MODULE,receiver,[Ref]}) = receive_next_bytag(MetaTracer),
    ?CTT(Slave,{erlang,phash2,[1,1]}) = receive_next_bytag(MetaTracer),
    ?RFT(Slave,{erlang,phash2,2},0) = receive_next_bytag(MetaTracer),
    ?RFT(Slave,{?MODULE,receiver,1},Ref) = receive_next_bytag(MetaTracer),
    ?CT(Slave,{?MODULE,receiver,[Ref]}) = receive_next_bytag(LocalTracer),
    ?CT(Slave,{erlang,phash2,[1,1]}) = receive_next_bytag(LocalTracer),
    case {receive_next_bytag(LocalTracer),
		receive_next_bytag(LocalTracer)} of
	      {?RF(Slave,{erlang,phash2,2},0),
	       ?RT(Slave,{?MODULE,receiver,1})} ->
		  ok;
	      {?RT(Slave,{?MODULE,receiver,1}),
	       ?RF(Slave,{erlang,phash2,2},0)} ->
		ok;
	      Error1 -> ct:fail({unexpected_message, Error1})
	end,
    case {receive_next_bytag(LocalTracer),
		receive_next_bytag(LocalTracer)} of
	      {?RF(Slave,{?MODULE,receiver,1},Ref),
	       ?RT(Slave,{?MODULE,slave,1})} ->
		  ok;
	      {?RT(Slave,{?MODULE,slave,1}),
	       ?RF(Slave,{?MODULE,receiver,1},Ref)} ->
		  ok;
	      Error2 -> ct:fail({unexpected_message, Error2})
	  end,
    shutdown(),
    ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Use case for Inviso meta tracing:
%% Setup silent local call tracing, and start it using meta trace.

nosilent_test() ->
    Pid = setup(),
    Trigger = {?MODULE,id,1},
    TriggerMS = [{[start],[],[{silent,false}]},
                 {[stop],[],[{silent,true},{return_trace}]}],
    1 = erlang:trace(Pid, true, [call,silent,return_to]),
    erlang:trace_pattern({?MODULE,'_','_'},[],[local]),
    1 = erlang:trace_pattern({?MODULE,local2,1},
                             [{'_',[],[{return_trace}]}],
                             [local]),
    1 = erlang:trace_pattern({?MODULE,slave,1},false,[local]),
    1 = erlang:trace_pattern(Trigger,false,[local]),
    1 = erlang:trace_pattern(Trigger,TriggerMS,[meta]),
    [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    receive_no_next(17),
    start = apply_slave(?MODULE, id, [start]),
    ?CTT(Pid,{?MODULE,id,[start]})       = receive_next(),
    [2,2,2,0] = apply_slave(?MODULE,exported_wrap,[2]),
    ?CT(Pid,{?MODULE,exported_wrap,[2]}) = receive_next(),
    ?CT(Pid,{?MODULE,exported,[2]})      = receive_next(),
    ?CT(Pid,{?MODULE,local,[2]})         = receive_next(),
    ?CT(Pid,{?MODULE,local2,[2]})        = receive_next(),
    ?CT(Pid,{?MODULE,local_tail,[2]})    = receive_next(),
    ?RF(Pid,{?MODULE,local2,1}, [2,0])   = receive_next(),
    ?RT(Pid,{?MODULE,local,1})           = receive_next(),
    ?RT(Pid,{?MODULE,exported,1})        = receive_next(),
    ?RT(Pid,{?MODULE,slave,1})           = receive_next(),
    stop = apply_slave(?MODULE, id, [stop]),
    ?CTT(Pid,{?MODULE,id,[stop]})        = receive_next(),
    ?RFT(Pid,{?MODULE,id,1}, stop)       = receive_next(),
    [3,3,3,0] = apply_slave(?MODULE,exported_wrap,[3]),
    receive_no_next(17),
    shutdown(),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Trace target functions

loop(D1,D2,D3,0) ->
    io:format("~p~n",[[D1,D2,D3]]),
    0;
loop(D1,D2,D3,N) ->
    max(N,loop(D1,D2,D3,N-1)).

id(X) ->
    X.


exported_wrap(Val) ->
    exported(Val).

exported(Val) ->
    [Val | local(Val)]. %% Non tail recursive local call

local(Val) ->
    [Val | local2(Val)]. %% Non tail recursive local call

local2(Val) ->
    local_tail(Val). %% Tail recursive call

local_tail(Val) ->
    [Val , erlang:phash2(1,1)]. 



receiver(Msg) ->
    erlang:phash2(1,1),
    receive Msg -> Msg end.
	    
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Trace target process and utilities

slave(Sync) ->
    Sync ! sync,
    receive
	{From,apply, M, F, A} ->
	    ?dbgformat("Apply: ~p:~p/~p (~p)~n",[M,F,length(A),A]),
	    Res = apply(M,F,A),
	    ?dbgformat("done Apply: ~p:~p/~p (~p)~n",[M,F,length(A),A]),
	    From ! {apply, Res},
	    erlang:trace_pattern({?MODULE,slave,1},false,[meta]),
	    slave(From);
	{From, lambda, Fun} ->
	    Res = Fun(),
	    From ! {lambda, Res},
	    erlang:trace_pattern({?MODULE,slave,1},false,[meta]),
	    slave(From);
	die ->
	    exit(normal)
    end.

setup() ->
    trace_off(),
    Self = self(),
    Pid = spawn(fun () -> slave(Self) end),
    receive sync -> ok end,
    put(slave,Pid),
    Pid.

shutdown() ->
    trace_off(),
    Pid = get(slave),
    case (catch is_process_alive(Pid)) of
	true ->
	    Ref = erlang:monitor(process,Pid),
	    Pid ! die,
	    receive
		{'DOWN',Ref,process,Pid,_} ->
		    ok
	    end;
	_ ->
	    ok
    end.

trace_off() ->
    erlang:trace(all, false, [all]),
    erlang:trace_pattern({'_','_','_'},false,[]),
    erlang:trace_pattern({'_','_','_'},false,[local]),
    erlang:trace_pattern({'_','_','_'},false,[meta]),
    erlang:trace_pattern(on_load,false,[]),
    erlang:trace_pattern(on_load,false,[local]),
    erlang:trace_pattern(on_load,false,[meta]),
    ok.

apply_slave_async(M,F,A) ->
    Slave = get(slave),
    Pid = 
	spawn(
	  fun () ->
		  Slave ! {self(),apply, M, F, A},
		  receive
		      {apply, _} ->
			  receive
			      sync ->
				  ok
			  end
		  end
	  end),
    Pid.

apply_slave(M,F,A) ->
    Pid = get(slave),
    Pid ! {self(),apply, M, F, A},
    receive
	{apply, Res} ->
	    receive
		sync ->
		    Res
	    end
    end.

lambda_slave(Fun) ->
    Pid = get(slave),
    Pid ! {self(), lambda, Fun},
    receive
	{lambda, Res} ->
	    receive
		sync ->
		    Res
	    end
    end.

relay_n(0, _) ->
    ok;
relay_n(N, Dest) ->
    receive Msg ->
	    Dest ! {self(), Msg},
	    relay_n(N-1, Dest)
    end.


receive_next() ->
    receive_next(infinity).

receive_next(TO) ->
    receive
	M ->
	    M
    after TO ->
	    ct:fail(timeout)
    end.

receive_no_next(TO) ->
    receive
	M ->
	    ct:fail({unexpected_message, M})
    after
	TO ->
	    ok
    end.

receive_next_bytag(Tag) ->
    receive_next_bytag(Tag, infinity).
receive_next_bytag(Tag, TO) ->
    receive
	{Tag, Msg} ->
	    Msg
    after
	TO ->
	    timeout
    end.

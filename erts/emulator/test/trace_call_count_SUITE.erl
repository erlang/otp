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

-module(trace_call_count_SUITE).

%% Exported end user tests
-export([basic_test/0, on_and_off_test/0, info_test/0, 
	 pause_and_restart_test/0, combo_test/0]).

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
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2, not_run/1]).
-export([basic/1, on_and_off/1, info/1, 
	 pause_and_restart/1, combo/1]).
	 
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_count]),
    erlang:trace_pattern(on_load, false, [local,meta,call_count]),
    erlang:trace(all, false, [all]),
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 4}}].

all() -> 
    case test_server:is_native(trace_call_count_SUITE) of
	true -> [not_run];
	false ->
	    [basic, on_and_off, info, pause_and_restart, combo]
    end.

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


not_run(Config) when is_list(Config) -> 
    {skipped,"Native code"}.

%% Tests basic call count trace
basic(Config) when is_list(Config) ->
    basic_test().

%% Tests turning trace parameters on and off
on_and_off(Config) when is_list(Config) ->
    on_and_off_test().
 
%% Tests the trace_info BIF
info(Config) when is_list(Config) ->
    info_test().
 
%% Tests pausing and restarting call counters
pause_and_restart(Config) when is_list(Config) ->
    pause_and_restart_test().
 
%% Tests combining local call trace and meta trace with call count trace
combo(Config) when is_list(Config) ->
    combo_test().

-endif. %-ifdef(STANDALONE). ... -else.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Result examination macros

-define(CT(P,MFA),{trace,P,call,MFA}).
-define(CTT(P, MFA),{trace_ts,P,call,MFA,{_,_,_}}).
-define(RF(P,MFA,V),{trace,P,return_from,MFA,V}).
-define(RFT(P,MFA,V),{trace_ts,P,return_from,MFA,V,{_,_,_}}).
-define(RT(P,MFA),{trace,P,return_to,MFA}).
-define(RTT(P,MFA),{trace_ts,P,return_to,MFA,{_,_,_}}).

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The Tests
%%%

basic_test() ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    M = 1000,
    %%
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, true, [call_count]),
    2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, true, [call_count]),
    L = seq(1, M, fun(X) -> X+1 end),
    {call_count,M} = erlang:trace_info({?MODULE,seq,3}, call_count),
    {call_count,0} = erlang:trace_info({?MODULE,seq_r,3}, call_count),
    Lr = seq_r(1, M, fun(X) -> X+1 end),
    {call_count,M} = erlang:trace_info({?MODULE,seq,3}, call_count),
    {call_count,1} = erlang:trace_info({?MODULE,seq_r,3}, call_count),
    {call_count,M} = erlang:trace_info({?MODULE,seq_r,4}, call_count),
    L = lists:reverse(Lr),
    %%
    P = erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

on_and_off_test() ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    M = 100,
    %%
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, true, [call_count]),
    L = seq(1, M, fun(X) -> X+1 end),
    {call_count,M} = erlang:trace_info({?MODULE,seq,3}, call_count),
    N = erlang:trace_pattern({?MODULE,'_','_'}, true, [call_count]),
    L = seq(1, M, fun(X) -> X+1 end),
    {call_count,M} = erlang:trace_info({?MODULE,seq,3}, call_count),
    P = erlang:trace_pattern({'_','_','_'}, true, [call_count]),
    L = seq(1, M, fun(X) -> X+1 end),
    {call_count,M} = erlang:trace_info({?MODULE,seq,3}, call_count),
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, false, [call_count]),
    {call_count,false} = erlang:trace_info({?MODULE,seq,3}, call_count),
    L = seq(1, M, fun(X) -> X+1 end),
    {call_count,false} = erlang:trace_info({?MODULE,seq,3}, call_count),
    {call_count,0} = erlang:trace_info({?MODULE,seq_r,4}, call_count),
    Lr = seq_r(1, M, fun(X) -> X+1 end),
    {call_count,M} = erlang:trace_info({?MODULE,seq_r,4}, call_count),
    N = erlang:trace_pattern({?MODULE,'_','_'}, false, [call_count]),
    {call_count,false} = erlang:trace_info({?MODULE,seq_r,4}, call_count),
    Lr = seq_r(1, M, fun(X) -> X+1 end),
    {call_count,false} = erlang:trace_info({?MODULE,seq_r,4}, call_count),
    L = lists:reverse(Lr),
    %%
    P = erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info_test() ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    %%
    1 = erlang:trace_pattern({?MODULE,seq,3}, true, [call_count]),
    {call_count,0} = erlang:trace_info({?MODULE,seq,3}, call_count),
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, pause, [call_count]),
    {call_count,0} = erlang:trace_info({?MODULE,seq,3}, call_count),
    {all,[_|_]=L} = erlang:trace_info({?MODULE,seq,3}, all),
    {value,{call_count,0}} = lists:keysearch(call_count, 1, L),
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, restart, [call_count]),
    {call_count,0} = erlang:trace_info({?MODULE,seq,3}, call_count),
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, false, [call_count]),
    {call_count,false} = erlang:trace_info({?MODULE,seq,3}, call_count),
    {all,false} = erlang:trace_info({?MODULE,seq,3}, all),
    %%
    P = erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pause_and_restart_test() ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    M = 100,
    %%
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, true, [call_count]),
    {call_count,0} = erlang:trace_info({?MODULE,seq,3}, call_count),
    L = seq(1, M, fun(X) -> X+1 end),
    {call_count,M} = erlang:trace_info({?MODULE,seq,3}, call_count),
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, pause, [call_count]),
    {call_count,M} = erlang:trace_info({?MODULE,seq,3}, call_count),
    L = seq(1, M, fun(X) -> X+1 end),
    {call_count,M} = erlang:trace_info({?MODULE,seq,3}, call_count),
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, restart, [call_count]),
    {call_count,0} = erlang:trace_info({?MODULE,seq,3}, call_count),
    L = seq(1, M, fun(X) -> X+1 end),
    {call_count,M} = erlang:trace_info({?MODULE,seq,3}, call_count),
    %%
    P = erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

combo_test() ->
    Self = self(),

    MetaMatchSpec = [{'_',[],[{return_trace}]}],
    Flags = lists:sort([call, return_to]),
    LocalTracer = spawn_link(fun () -> relay_n(5, Self) end),
    MetaTracer = spawn_link(fun () -> relay_n(9, Self) end),
    2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, [], [local]),
    2 = erlang:trace_pattern({?MODULE,seq_r,'_'},
                             MetaMatchSpec,
                             [{meta,MetaTracer}, call_count]),
    1 = erlang:trace(Self, true, [{tracer,LocalTracer} | Flags]),
    %%
    {traced,local} = 
    erlang:trace_info({?MODULE,seq_r,3}, traced),
    {match_spec,[]} = 
    erlang:trace_info({?MODULE,seq_r,3}, match_spec),
    {meta,MetaTracer} = 
    erlang:trace_info({?MODULE,seq_r,3}, meta),
    {meta_match_spec,MetaMatchSpec} = 
    erlang:trace_info({?MODULE,seq_r,3}, meta_match_spec),
    {call_count,0} = 
    erlang:trace_info({?MODULE,seq_r,3}, call_count),
    %%
    {all,[_|_]=TraceInfo} = 
    erlang:trace_info({?MODULE,seq_r,3}, all),
    {value,{traced,local}} = 
    lists:keysearch(traced, 1, TraceInfo),
    {value,{match_spec,[]}} = 
    lists:keysearch(match_spec, 1, TraceInfo),
    {value,{meta,MetaTracer}} = 
    lists:keysearch(meta, 1, TraceInfo),
    {value,{meta_match_spec,MetaMatchSpec}} = 
    lists:keysearch(meta_match_spec, 1, TraceInfo),
    {value,{call_count,0}} = 
    lists:keysearch(call_count, 1, TraceInfo),
    %%
    [3,2,1] = seq_r(1, 3, fun(X) -> X+1 end),
    %%
    List = collect(100),
    {MetaR, LocalR} = 
    lists:foldl(
      fun ({P,X}, {M,L}) when P == MetaTracer ->
              {[X|M],L};
          ({P,X}, {M,L}) when P == LocalTracer ->
              {M,[X|L]}
      end,
      {[],[]},
      List),
    Meta = lists:reverse(MetaR),
    Local = lists:reverse(LocalR),
    [?CTT(Self,{?MODULE,seq_r,[1,3,_]}),
     ?CTT(Self,{?MODULE,seq_r,[1,3,_,[]]}),
     ?CTT(Self,{?MODULE,seq_r,[2,3,_,[1]]}),
     ?CTT(Self,{?MODULE,seq_r,[3,3,_,[2,1]]}),
     ?RFT(Self,{?MODULE,seq_r,4},[3,2,1]),
     ?RFT(Self,{?MODULE,seq_r,4},[3,2,1]),
     ?RFT(Self,{?MODULE,seq_r,4},[3,2,1]),
     ?RFT(Self,{?MODULE,seq_r,3},[3,2,1])] = Meta,
    [?CT(Self,{?MODULE,seq_r,[1,3,_]}),
     ?CT(Self,{?MODULE,seq_r,[1,3,_,[]]}),
     ?CT(Self,{?MODULE,seq_r,[2,3,_,[1]]}),
     ?CT(Self,{?MODULE,seq_r,[3,3,_,[2,1]]}),
     ?RT(Self,{?MODULE,combo_test,0})] = Local,
    {call_count,1} = erlang:trace_info({?MODULE,seq_r,3}, call_count),
    {call_count,3} = erlang:trace_info({?MODULE,seq_r,4}, call_count),
    %%
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_count]),
    erlang:trace_pattern(on_load, false, [local,meta,call_count]),
    erlang:trace(all, false, [all]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local helpers

%% Stack recursive seq
seq(Stop, Stop, Succ) when is_function(Succ) ->
    [Stop];
seq(Start, Stop, Succ) when is_function(Succ) ->
    [Start | seq(Succ(Start), Stop, Succ)].



%% Tail recursive seq, result list is reversed
seq_r(Start, Stop, Succ) when is_function(Succ) ->
    seq_r(Start, Stop, Succ, []).

seq_r(Stop, Stop, _, R) ->
    [Stop | R];
seq_r(Start, Stop, Succ, R) ->
    seq_r(Succ(Start), Stop, Succ, [Start | R]).



%% Message relay process
relay_n(0, _) ->
    ok;
relay_n(N, Dest) ->
    receive Msg ->
                Dest ! {self(), Msg},
                relay_n(N-1, Dest)
    end.



%% Collect received messages
collect(Time) ->
    Ref = erlang:start_timer(Time, self(), done),
    L = lists:reverse(collect([], Ref)),
    ?dbgformat("Got: ~p~n",[L]),
    L.

collect(A, 0) ->
    receive
        Mess ->
            collect([Mess | A], 0)
    after 0 ->
              A
    end;
collect(A, Ref) ->
    receive
        {timeout, Ref, done} ->
            collect(A, 0);
        Mess ->
            collect([Mess | A], Ref)
    end.

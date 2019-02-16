%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2018. All Rights Reserved.
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
-module(multitrace).
-author('siri@erix.ericsson.se').

-export([debug/1,gc/1,schedule/1,stop/0,format/1,format/2]).
-export([handle_debug/4,handle_gc/4,handle_schedule/4]).

%%
%% Tool API
%%
debug(Func) ->
    case running() of
	false ->
	    {ok,_} = ttb:tracer(all,
				[{file,"debug_trace"},
				 {handler, {{?MODULE,handle_debug},initial}}]),
	    init(),
	    {ok,_} = ttb:p(all,[timestamp,c]),
	    tp(Func),
	    ok;
	true ->
	    {error, tracer_already_running}
    end.

tp([Func|Funcs]) ->
    tp(Func),
    tp(Funcs);
tp([]) -> ok;
tp({M,F,A}) -> do_tp(M,F,A);
tp({M,F}) -> do_tp(M,F,'_');
tp(M) -> do_tp(M,'_','_').

do_tp(M,F,A) ->
    {ok,_}=ttb:tp(M,F,A,[{'_',[],[{message,{caller}},{return_trace}]}]).

gc(Proc) ->
    case running() of
	false ->
	    {ok,_} = ttb:tracer(all,[{file,"gc_trace"},
				     {handler,{{?MODULE,handle_gc},initial}},
				     {process_info,false}]),
	    init(),
	    {ok,_} = ttb:p(Proc,[timestamp,garbage_collection]),
	    ok;
	true ->
	    {error, tracer_already_running}
    end.

schedule(Proc) ->
    case running() of
	false ->
	    {ok,_} = ttb:tracer(all,
				[{file,"schedule_trace"},
				 {handler,{{?MODULE,handle_schedule},initial}},
				 {process_info,false}]),
	    init(),
	    {ok,_} = ttb:p(Proc,[timestamp,running]),
	    ok;
	true ->
	    {error, tracer_already_running}
    end.
    
stop() ->
    ttb:stop().

format(File) ->
    ttb:format(File).
format(File,Out) ->
    ttb:format(File,[{out,Out}]).


%%
%% Print call trace
%%
handle_debug(Out,Trace,TI,initial) ->
    print_header(Out,TI),
    handle_debug(Out,Trace,TI,0);
handle_debug(_Out,end_of_trace,_TI,N) ->
    N;
handle_debug(Out,Trace,_TI,N) ->
    print_func(Out,Trace,N),
    N+1.

print_func(Out,{trace_ts,P,call,{M,F,A},C,Ts},N) ->
    io:format(Out,
	      "~w: ~s~n"
	      "Process   : ~w~n"
	      "Call      : ~w:~tw/~w~n"
	      "Arguments : ~tp~n"
	      "Caller    : ~tw~n~n",
	      [N,ts(Ts),P,M,F,length(A),A,C]);
print_func(Out,{trace_ts,P,return_from,{M,F,A},R,Ts},N) ->
    io:format(Out,
	      "~w: ~s~n"
	      "Process      : ~w~n"
	      "Return from  : ~w:~tw/~w~n"
	      "Return value : ~tp~n~n",
	      [N,ts(Ts),P,M,F,A,R]).


%%
%% Print GC trace
%%
handle_gc(_Out,end_of_trace,_TI,S) ->
    S;
handle_gc(Out,Trace,TI,initial) ->
    print_header(Out,TI),
    print_gc_header(Out),
    handle_gc(Out,Trace,TI,dict:new());
handle_gc(_Out,{trace_ts,P,gc_start,Info,Ts},_TI,S) ->
    dict:store(P,{Info,Ts},S);
handle_gc(Out,{trace_ts,P,gc_end,Info,Ts},_TI,S) ->
    case dict:find(P,S) of
	{ok,{StartInfo,StartTime}} ->
	    {EM,ER,ES,EO,EH,EOB,EB} = sort(Info),
	    {SM,SR,SS,SO,SH,SOB,SB} = sort(StartInfo),
	    io:format(Out,
		      "start\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~n"
		      "end\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~n~n", 
		      [SM,SR,SS,SO,SH,SOB,SB,P,EM,ER,ES,EO,EH,EOB,EB,diff(StartTime,Ts)]),
	    dict:erase(P,S);
	error ->
	    S
    end.

print_gc_header(Out) ->
    io:format(Out,
	      "\tMBuf\tRecent\tStack\tOldHeap\tHeap\tOldBL\tBlock\t"
              "Process/Time(micro sec)~n"
              "============================================="
              "============================================~n",[]).

sort(GC) ->
    sort(GC,{0,0,0,0,0,'_','_'}).
sort([{mbuf_size,M}|Rest],{_,R,S,O,H,OB,B}) ->
    sort(Rest,{M,R,S,O,H,OB,B});
sort([{recent_size,R}|Rest],{M,_,S,O,H,OB,B}) ->
    sort(Rest,{M,R,S,O,H,OB,B});
sort([{stack_size,S}|Rest],{M,R,_,O,H,OB,B}) ->
    sort(Rest,{M,R,S,O,H,OB,B});
sort([{old_heap_size,O}|Rest],{M,R,S,_,H,OB,B}) ->
    sort(Rest,{M,R,S,O,H,OB,B});
sort([{heap_size,H}|Rest],{M,R,S,O,_,OB,B}) ->
    sort(Rest,{M,R,S,O,H,OB,B});
sort([{old_heap_block_size,OB}|Rest],{M,R,S,O,H,_,B}) ->
    sort(Rest,{M,R,S,O,H,OB,B});
sort([{heap_block_size,B}|Rest],{M,R,S,O,H,OB,_}) ->
    sort(Rest,{M,R,S,O,H,OB,B});
sort([],GC) ->
    GC.


%%
%% Print scheduling trace
%%
handle_schedule(Out,Trace,TI,initial) ->
    print_header(Out,TI),
    handle_schedule(Out,Trace,TI,[]);
handle_schedule(Out,end_of_trace,_TI,S) ->
    summary(Out,S);
handle_schedule(Out,{trace_ts,P,out,Info,Ts},_TI,S) ->
    io:format(Out,
	      "out:~n"
	      "Process  : ~w~n"
	      "Time     : ~s~n"
	      "Function : ~tw~n~n",[P,ts(Ts),Info]),
    case lists:keysearch(P,1,S) of
	{value,{P,List}} ->
	    lists:keyreplace(P,1,S,{P,[{out,Ts}|List]});
	false ->
	    [{P,[{out,Ts}]} | S]
    end;
handle_schedule(Out,{trace_ts,P,in,Info,Ts},_TI,S) ->
    io:format(Out,
	      "in:~n"
	      "Process  : ~w~n"
	      "Time     : ~s~n"
	      "Function : ~tw~n~n",[P,ts(Ts),Info]),
    case lists:keysearch(P,1,S) of
	{value,{P,List}} ->
	    lists:keyreplace(P,1,S,{P,[{in,Ts}|List]});
	false ->
	    [{P,[{in,Ts}]} | S]
    end.


summary(Out,[{P,List}|Rest]) ->
    Sum = proc_summary(List,0),
    io:format(Out,"Total time 'in' for process ~w: ~w micro seconds~n",[P,Sum]),
    summary(Out,Rest);
summary(_Out,[]) ->
    ok.

proc_summary([{in,_Start}|Rest],Acc) ->
    proc_summary(Rest,Acc);
proc_summary([{out,End},{in,Start}|Rest],Acc) ->
    Diff = diff(Start,End),
    proc_summary(Rest,Acc+Diff);
proc_summary([],Acc) ->
    Acc.
    

%%
%% Library functions
%%
ts({_, _, Micro} = Now) ->
    {{Y,M,D},{H,Min,S}} = calendar:now_to_local_time(Now),
    io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w,~6.6.0w",
		  [Y,M,D,H,Min,S,Micro]).


diff({SMeg,SS,SMic},{EMeg,ES,EMic}) ->
    (EMeg-SMeg)*1000000000000 + (ES-SS)*1000000 + (EMic-SMic).


init() ->
    ttb:write_trace_info(start_time,fun() -> now() end).

print_header(Out,TI) ->
    {value,{node,[Node]}} = lists:keysearch(node,1,TI),
    {value,{flags,Flags}} = lists:keysearch(flags,1,TI),
    case lists:keysearch(start_time,1,TI) of
	{value,{start_time,[ST]}} ->
	    io:format(Out,
		      "~nTracing started on node ~w at ~s~n"
		      "Flags: ~p~n~n~n",
		      [Node,ts(ST),Flags]);
	false -> % in case this file was not loaded on the traced node 
	    io:format(Out,
		      "~nTracing from node ~w~n"
		      "Flags: ~p~n~n~n",
		      [Node,Flags])
    end.

running() ->
    case whereis(ttb) of
	undefined -> false;
	_Pid -> true
    end.

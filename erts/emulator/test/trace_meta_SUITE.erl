%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2011. All Rights Reserved.
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
-include_lib("test_server/include/test_server.hrl").
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
-export([basic/1, return/1, on_and_off/1, stack_grow/1, 
	 info/1, tracer/1, combo/1, nosilent/1]).
	 
init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(test_server:minutes(5)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    shutdown(),
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
case test_server:is_native(trace_meta_SUITE) of
  true -> [not_run];
  false ->
      [basic, return, on_and_off, stack_grow, info, tracer,
       combo, nosilent]
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

basic(suite) ->
    [];
basic(doc) ->
    ["Tests basic meta trace"];
basic(Config) when is_list(Config) ->
    basic_test().

return(suite) ->
    [];
return(doc) ->
    ["Tests return trace"];
return(Config) when is_list(Config) ->
    return_test().
 
on_and_off(suite) ->
    [];
on_and_off(doc) ->
    ["Tests turning trace parameters on and off"];
on_and_off(Config) when is_list(Config) ->
    on_and_off_test().
 
stack_grow(doc) ->
    ["Tests the stack growth during return traces"];
stack_grow(Config) when is_list(Config) ->
    stack_grow_test().
 
info(doc) ->
    ["Tests the trace_info BIF"];
info(Config) when is_list(Config) ->
    info_test().
 
tracer(suite) ->
    [];
tracer(doc) ->
    ["Tests stopping and changing tracer process"];
tracer(Config) when is_list(Config) ->
    tracer_test().

combo(suite) ->
    [];
combo(doc) ->
    ["Tests combining local call trace with meta trace"];
combo(Config) when is_list(Config) ->
    combo_test().

nosilent(suite) ->
    [];
nosilent(doc) ->
    ["Tests that meta trace is not silenced by the silent process flag"];
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
    ?line Pid = setup(),
    ?line erlang:trace_pattern({?MODULE,'_','_'},[],[meta]),
    ?line [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    ?line ?CTT(Pid,{?MODULE,exported_wrap,[1]}) = receive_next(), 
    ?line ?CTT(Pid,{?MODULE,exported,[1]}) = receive_next(),
    ?line ?CTT(Pid,{?MODULE,local,[1]}) = receive_next(),
    ?line ?CTT(Pid,{?MODULE,local2,[1]}) = receive_next(),
    ?line ?CTT(Pid,{?MODULE,local_tail,[1]}) = receive_next(),
    ?line erlang:trace_pattern({?MODULE,'_','_'},false,[meta]),
    ?line [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    ?line ?NM,
    ?line [1,1,1,0] = lambda_slave(fun() ->
					   exported_wrap(1)
				   end),
    ?line ?NM, 
    ?line erlang:trace_pattern({?MODULE,'_','_'},[],[meta]),
    ?line [1,1,1,0] = lambda_slave(fun() ->
					   exported_wrap(1)
				   end),
    ?line ?CTT(Pid,{?MODULE,_,_}) = receive_next(), %% The fun
    ?line ?CTT(Pid,{?MODULE,exported_wrap,[1]}) = receive_next(), 
    ?line ?CTT(Pid,{?MODULE,exported,[1]}) = receive_next(),
    ?line ?CTT(Pid,{?MODULE,local,[1]}) = receive_next(),
    ?line ?CTT(Pid,{?MODULE,local2,[1]}) = receive_next(),
    ?line ?CTT(Pid,{?MODULE,local_tail,[1]}) = receive_next(),
    ?line erlang:trace_pattern({?MODULE,'_','_'},false,[meta]),
    ?line shutdown(),
    ?line ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

return_test() ->
    ?line Pid = setup(),
    ?line erlang:trace_pattern({?MODULE,'_','_'},[{'_',[],[{return_trace}]}],
			 [meta]),
    ?line erlang:trace_pattern({erlang,phash2,'_'},[{'_',[],[{return_trace}]}],
			 [meta]),
    ?line [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    ?line ?CTT(Pid,{?MODULE,exported_wrap,[1]}) = receive_next(), 
    ?line ?CTT(Pid,{?MODULE,exported,[1]}) = receive_next(),
    ?line ?CTT(Pid,{?MODULE,local,[1]}) = receive_next(),
    ?line ?CTT(Pid,{?MODULE,local2,[1]}) = receive_next(),
    ?line ?CTT(Pid,{?MODULE,local_tail,[1]}) = receive_next(),
    ?line ?CTT(Pid,{erlang,phash2,[1,1]}) = receive_next(),
    ?line ?RFT(Pid,{erlang,phash2,2},0) = receive_next(),
    ?line ?RFT(Pid,{?MODULE,local_tail,1},[1,0]) = receive_next(),
    ?line ?RFT(Pid,{?MODULE,local2,1},[1,0]) = receive_next(),
    ?line ?RFT(Pid,{?MODULE,local,1},[1,1,0]) = receive_next(),
    ?line ?RFT(Pid,{?MODULE,exported,1},[1,1,1,0]) = receive_next(),
    ?line ?RFT(Pid,{?MODULE,exported_wrap,1},[1,1,1,0]) = receive_next(),
    ?line shutdown(),
    ?line ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

on_and_off_test() ->
    ?line Pid = setup(),
    ?line 1 = erlang:trace_pattern({?MODULE,local_tail,1},[],[meta]),
    ?line LocalTail = fun() ->
			      local_tail(1)
		      end,
    ?line [1,0] = lambda_slave(LocalTail),
    ?line ?CTT(Pid,{?MODULE,local_tail,[1]}) = receive_next(),
    ?line 0 = erlang:trace_pattern({?MODULE,local_tail,1},[],[global]),
    ?line [1,0] = lambda_slave(LocalTail),
    ?line ?NM,
    ?line 1 = erlang:trace_pattern({?MODULE,exported_wrap,1},[],[meta]),
    ?line [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    ?line ?CTT(Pid,{?MODULE,exported_wrap,[1]}) = receive_next(),
    ?line 1 = erlang:trace_pattern({erlang,phash2,2},[],[meta]),
    ?line [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    ?line ?CTT(Pid,{?MODULE,exported_wrap,[1]}) = receive_next(),
    ?line ?CTT(Pid,{erlang,phash2,[1,1]}) = receive_next(),
    ?line shutdown(),
    ?line erlang:trace_pattern({'_','_','_'},false,[meta]),
    ?line N = erlang:trace_pattern({erlang,'_','_'},true,[meta]),
    ?line case erlang:trace_pattern({erlang,'_','_'},false,[meta]) of
	      N -> 
		  ok;
	      Else ->
		  exit({number_mismatch, {expected, N}, {got, Else}})
	  end,
    ?line case erlang:trace_pattern({erlang,'_','_'},false,[meta]) of
	      N -> 
		  ok;
	      Else2 ->
		  exit({number_mismatch, {expected, N}, {got, Else2}})
	  end,
    ?line ?NM,
    ok.
    
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stack_grow_test() ->    
    ?line Pid = setup(),
    ?line 1 = erlang:trace_pattern({?MODULE,loop,4},
				   [{'_',[],[{return_trace}]}],[meta]),
    ?line Num = 1 bsl 15,
    ?line Surface =
	fun (This, ?RFT(P,{?MODULE,loop,4},N), N) when P == Pid->
		if N == Num ->
			?NM,
			ok;
		   true ->
			This(This, receive_next(), N+1)
		end
	end,
    ?line Dive = 
	fun (This, ?CTT(P,{?MODULE,loop,[{hej,hopp},[a,b,c],4.5,N]}), N)
	    when P == Pid->
		if N == 0 ->
			Surface(Surface, receive_next(), 0);
		   true ->
			This(This, receive_next(), N-1)
		end
	end,
    ?line apply_slave(?MODULE,loop,[{hej,hopp},[a,b,c],4.5,Num]),
%     ?line apply_slave_async(?MODULE,loop,[{hej,hopp},[a,b,c],4.5,Num]),
%     ?line List = collect(test_server:seconds(5)),
    ?line ok = Dive(Dive, receive_next(), Num),
    ?line ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info_test() ->
    ?line setup(),
    ?line Prog = [{['$1'],[{is_integer,'$1'}],[{message, false}]},
		  {'_',[],[]}],
    ?line Self = self(),
    ?line GoOn = make_ref(),
    
    ?line Pid = 
	spawn_link(
	  fun () ->
		  erlang:trace_pattern({?MODULE,exported_wrap,1}, 
				       Prog, [{meta, Self}]),
		  Self ! {self(), GoOn}
	  end),
    ?line receive {Pid, GoOn} -> ok end,
    ?line {traced,false} = erlang:trace_info({?MODULE,exported_wrap,1}, traced),
    ?line {match_spec, false} = 
	erlang:trace_info({?MODULE,exported_wrap,1}, match_spec),
    ?line {meta, Self} = erlang:trace_info({?MODULE,exported_wrap,1}, meta),
    ?line {meta_match_spec, MMS} = 
	erlang:trace_info({?MODULE,exported_wrap,1}, meta_match_spec),
    ?line case MMS of
	      Prog ->
		  ok;
	      Wrong ->
		  exit({bad_result, {erlang,trace_info,
				     [{?MODULE,exported_wrap,1},
				      meta_match_spec]},
			{expected, Prog}, {got, Wrong}})
	  end,
    ?line erlang:garbage_collect(self()),
    ?line receive
	  after 1 ->
		  ok
	  end,
    ?line io:format("~p~n",[MMS]),
    ?line {meta_match_spec,MMS2} = 
	erlang:trace_info({?MODULE,exported_wrap,1}, meta_match_spec),
    ?line io:format("~p~n",[MMS2]),
    ?line case MMS2 of
	      Prog ->
		  ok;
	      Wrong2 ->
		  exit({bad_result, {erlang,trace_info,
				     [{?MODULE,exported_wrap,1},
				      meta_match_spec]},
			{expected, Prog}, {got, Wrong2}})
	  end,
    ?line {all, [_|_]=L} = erlang:trace_info({?MODULE,exported_wrap,1}, all),
    ?line {value, {meta, Self}} = 
	lists:keysearch(meta, 1, L),
    ?line {value, {meta_match_spec, MMS}} = 
	lists:keysearch(meta_match_spec, 1, L),
    
    ?line erlang:trace_pattern({?MODULE,exported_wrap,1}, true, [meta]),
    ?line {meta_match_spec, []} = 
	erlang:trace_info({?MODULE,exported_wrap,1}, meta_match_spec),
    
    ?line erlang:trace_pattern({?MODULE,exported_wrap,1}, false, [meta]),
    ?line {meta, false} = erlang:trace_info({?MODULE,exported_wrap,1}, meta),
    ?line {meta_match_spec, false} = 
	erlang:trace_info({?MODULE,exported_wrap,1}, meta_match_spec),
    ?line {all, false} = erlang:trace_info({?MODULE,exported_wrap,1}, all),
    
    ?line shutdown(),
    ?line ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tracer_test() ->
    ?line Slave = setup(),
    ?line Self = self(),
    
    ?line MatchSpec = [{'_',[],[{return_trace}]}],
    ?line Tracer1 = spawn_link(fun () -> relay_n(3, Self) end),
    ?line Setter = 
	spawn_link(
	  fun () ->
		  erlang:trace_pattern({?MODULE,receiver,1},
				       MatchSpec,
				       [{meta,Tracer1}]),
		  erlang:trace_pattern({erlang,phash2,2},
				       MatchSpec,
				       [{meta,Tracer1}]),
		  Self ! {self(), done}
	  end),
    ?line receive {Setter, done} -> ok end,
    ?line Ref = make_ref(),
    ?line apply_slave_async(?MODULE, receiver, [Ref]),
    ?line {Tracer1,?CTT(Slave,{?MODULE,receiver,[Ref]})} = receive_next(100),
    ?line {Tracer1,?CTT(Slave,{erlang,phash2,[1,1]})} = receive_next(100),
    ?line {Tracer1,?RFT(Slave,{erlang,phash2,2},0)} = receive_next(100),
    %% Initiate a return_trace that will fail since the tracer just stopped
    ?line Slave ! Ref,
    ?line receive_no_next(100),
    %% The breakpoint has not been hit since the tracer stopped
    ?line {meta,Tracer1} = 
	erlang:trace_info({?MODULE,receiver,1}, meta),
    ?line {meta_match_spec, MatchSpec} = 
	erlang:trace_info({?MODULE,receiver,1}, meta_match_spec),
    ?line {meta,Tracer1} = 
	erlang:trace_info({erlang,phash2,2}, meta),
    ?line {meta_match_spec, MatchSpec} = 
	erlang:trace_info({erlang,phash2,2}, meta_match_spec),
    %% Initiate trace messages that will fail
    ?line Ref2 = make_ref(),
    ?line apply_slave_async(?MODULE, receiver, [Ref2]),
    ?line Slave ! Ref2,
    ?line receive_no_next(100),
    ?line {meta,[]} = 
	erlang:trace_info({?MODULE,receiver,1}, meta),
    ?line {meta_match_spec, MatchSpec} = 
	erlang:trace_info({?MODULE,receiver,1}, meta_match_spec),
    ?line {meta,[]} =
	erlang:trace_info({erlang,phash2,2}, meta),
    ?line {meta_match_spec, MatchSpec} = 
	erlang:trace_info({erlang,phash2,2}, meta_match_spec),
    %% Change tracer
    ?line Tracer2 = spawn_link(fun () -> relay_n(4, Self) end),
    ?line erlang:trace_pattern({?MODULE,receiver,1}, 
			       MatchSpec, 
			       [{meta,Tracer2}]),
    ?line erlang:trace_pattern({erlang,phash2,2},
			       MatchSpec,
			       [{meta,Tracer2}]),
    ?line Ref3 = make_ref(),
    ?line apply_slave_async(?MODULE, receiver, [Ref3]),
    ?line {Tracer2,?CTT(Slave,{?MODULE,receiver,[Ref3]})} = receive_next(),
    ?line {Tracer2,?CTT(Slave,{erlang,phash2,[1,1]})} = receive_next(),
    ?line {Tracer2,?RFT(Slave,{erlang,phash2,2},0)} = receive_next(),
    %% Change tracer between call trace and return trace
    ?line Tracer3 = spawn_link(fun () -> relay_n(4, Self) end),
    ?line erlang:trace_pattern({?MODULE,receiver,1}, 
			       MatchSpec, 
			       [{meta,Tracer3}]),
    ?line erlang:trace_pattern({erlang,phash2,2},
			       MatchSpec,
			       [{meta,Tracer3}]),
    ?line Slave ! Ref3, 
    %% The return trace should still come from Tracer2
    ?line {Tracer2,?RFT(Slave,{?MODULE,receiver,1},Ref3)} = receive_next(),
    ?line Ref4 = make_ref(),
    %% Now should Tracer3 be used
    ?line apply_slave_async(?MODULE, receiver, [Ref4]),
    ?line Slave ! Ref4,
    ?line {Tracer3,?CTT(Slave,{?MODULE,receiver,[Ref4]})} = receive_next(),
    ?line {Tracer3,?CTT(Slave,{erlang,phash2,[1,1]})} = receive_next(),
    ?line {Tracer3,?RFT(Slave,{erlang,phash2,2},0)} = receive_next(),
    ?line {Tracer3,?RFT(Slave,{?MODULE,receiver,1},Ref4)} = receive_next(),
    %% The breakpoint has not been hit since the tracer stopped
    ?line {meta,Tracer3} = 
	erlang:trace_info({?MODULE,receiver,1}, meta),
    ?line {meta_match_spec, MatchSpec} = 
	erlang:trace_info({?MODULE,receiver,1}, meta_match_spec),
    ?line {meta,Tracer3} = 
	erlang:trace_info({erlang,phash2,2}, meta),
    ?line {meta_match_spec, MatchSpec} = 
	erlang:trace_info({erlang,phash2,2}, meta_match_spec),
    
    ?line shutdown(),
    ?line ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

combo_test() ->
    ?line Slave = setup(),
    ?line Self = self(),
    
    ?line MatchSpec = [{'_',[],[{return_trace}]}],
    ?line Flags = lists:sort([call, return_to]),
    ?line LocalTracer = spawn_link(fun () -> relay_n(6, Self) end),
    ?line MetaTracer = spawn_link(fun () -> relay_n(4, Self) end),
    ?line 1 = erlang:trace_pattern({?MODULE,receiver,1}, 
				   MatchSpec,
				   [local,{meta,MetaTracer}]),
    ?line 1 = erlang:trace_pattern({erlang,phash2,2}, 
				   MatchSpec,
				   [local,{meta,MetaTracer}]),
    ?line 1 = erlang:trace(Slave, true, 
			   [{tracer,LocalTracer} | Flags]),
    %%
    ?line {all, TraceInfo1} = 
	erlang:trace_info({?MODULE,receiver,1}, all),
    ?line {meta,MetaTracer} = 
	erlang:trace_info({?MODULE,receiver,1}, meta),
    ?line {value,{meta,MetaTracer}} = 
	lists:keysearch(meta, 1, TraceInfo1),
    ?line {meta_match_spec,MatchSpec} = 
	erlang:trace_info({?MODULE,receiver,1}, meta_match_spec),
    ?line {value,{meta_match_spec,MatchSpec}} = 
	lists:keysearch(meta_match_spec, 1, TraceInfo1),
    ?line {traced,local} = 
	erlang:trace_info({?MODULE,receiver,1}, traced),
    ?line {value,{traced,local}} = 
	lists:keysearch(traced, 1, TraceInfo1),
    ?line {match_spec,MatchSpec} = 
	erlang:trace_info({?MODULE,receiver,1}, match_spec),
    ?line {value,{match_spec,MatchSpec}} = 
	lists:keysearch(match_spec, 1, TraceInfo1),
    %%
    ?line {all, TraceInfo2} = 
	erlang:trace_info({erlang,phash2,2}, all),
    ?line {meta,MetaTracer} = 
	erlang:trace_info({erlang,phash2,2}, meta),
    ?line {value,{meta,MetaTracer}} = 
	lists:keysearch(meta, 1, TraceInfo2),
    ?line {meta_match_spec,MatchSpec} = 
	erlang:trace_info({erlang,phash2,2}, meta_match_spec),
    ?line {value,{meta_match_spec,MatchSpec}} = 
	lists:keysearch(meta_match_spec, 1, TraceInfo2),
    ?line {traced,local} = 
	erlang:trace_info({erlang,phash2,2}, traced),
    ?line {value,{traced,local}} = 
	lists:keysearch(traced, 1, TraceInfo2),
    ?line {match_spec,MatchSpec} = 
	erlang:trace_info({erlang,phash2,2}, match_spec),
    ?line {value,{match_spec,MatchSpec}} = 
	lists:keysearch(match_spec, 1, TraceInfo2),
    %%
    ?line {flags,Flags1} = erlang:trace_info(Slave, flags),
    ?line Flags = lists:sort(Flags1),
    ?line {tracer,LocalTracer} = erlang:trace_info(Slave, tracer),
    %%
    ?line Ref = make_ref(),
    ?line apply_slave_async(?MODULE, receiver, [Ref]),
    ?line Slave ! Ref,
    ?line ?CTT(Slave,{?MODULE,receiver,[Ref]}) = receive_next_bytag(MetaTracer),
    ?line ?CTT(Slave,{erlang,phash2,[1,1]}) = receive_next_bytag(MetaTracer),
    ?line ?RFT(Slave,{erlang,phash2,2},0) = receive_next_bytag(MetaTracer),
    ?line ?RFT(Slave,{?MODULE,receiver,1},Ref) = receive_next_bytag(MetaTracer),
    ?line ?CT(Slave,{?MODULE,receiver,[Ref]}) = receive_next_bytag(LocalTracer),
    ?line ?CT(Slave,{erlang,phash2,[1,1]}) = receive_next_bytag(LocalTracer),
    ?line case {receive_next_bytag(LocalTracer),
		receive_next_bytag(LocalTracer)} of
	      {?RF(Slave,{erlang,phash2,2},0),
	       ?RT(Slave,{?MODULE,receiver,1})} ->
		  ?line ok;
	      {?RT(Slave,{?MODULE,receiver,1}),
	       ?RF(Slave,{erlang,phash2,2},0)} ->
		?line ok;
	      Error1 -> ?t:fail({unexpected_message, Error1})
	end,
    ?line case {receive_next_bytag(LocalTracer),
		receive_next_bytag(LocalTracer)} of
	      {?RF(Slave,{?MODULE,receiver,1},Ref),
	       ?RT(Slave,{?MODULE,slave,1})} ->
		  ?line ok;
	      {?RT(Slave,{?MODULE,slave,1}),
	       ?RF(Slave,{?MODULE,receiver,1},Ref)} ->
		  ?line ok;
	      Error2 -> ?t:fail({unexpected_message, Error2})
	  end,
    
    ?line shutdown(),
    ?line ?NM,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Use case for Inviso meta tracing:
%% Setup silent local call tracing, and start it using meta trace.

nosilent_test() ->
    ?line Pid = setup(),
    ?line Trigger = {?MODULE,id,1},
    ?line TriggerMS = [{[start],[],[{silent,false}]},
		       {[stop],[],[{silent,true},{return_trace}]}],
    ?line 1 = erlang:trace(Pid, true, [call,silent,return_to]),
    ?line erlang:trace_pattern({?MODULE,'_','_'},[],[local]),
    ?line 1 = erlang:trace_pattern({?MODULE,local2,1},
				   [{'_',[],[{return_trace}]}],
				   [local]),
    ?line 1 = erlang:trace_pattern({?MODULE,slave,1},false,[local]),
    ?line 1 = erlang:trace_pattern(Trigger,false,[local]),
    ?line 1 = erlang:trace_pattern(Trigger,TriggerMS,[meta]),
    ?line [1,1,1,0] = apply_slave(?MODULE,exported_wrap,[1]),
    ?line receive_no_next(17),
    ?line start = apply_slave(?MODULE, id, [start]),
    ?line ?CTT(Pid,{?MODULE,id,[start]})		= receive_next(),
    ?line [2,2,2,0] = apply_slave(?MODULE,exported_wrap,[2]),
    ?line ?CT(Pid,{?MODULE,exported_wrap,[2]})		= receive_next(), 
    ?line ?CT(Pid,{?MODULE,exported,[2]})		= receive_next(),
    ?line ?CT(Pid,{?MODULE,local,[2]})			= receive_next(),
    ?line ?CT(Pid,{?MODULE,local2,[2]})			= receive_next(),
    ?line ?CT(Pid,{?MODULE,local_tail,[2]})		= receive_next(),
    ?line ?RF(Pid,{?MODULE,local2,1}, [2,0])		= receive_next(),
    ?line ?RT(Pid,{?MODULE,local,1})			= receive_next(),
    ?line ?RT(Pid,{?MODULE,exported,1})			= receive_next(),
    ?line ?RT(Pid,{?MODULE,slave,1})			= receive_next(),
    ?line stop = apply_slave(?MODULE, id, [stop]),
    ?line ?CTT(Pid,{?MODULE,id,[stop]})			= receive_next(),
    ?line ?RFT(Pid,{?MODULE,id,1}, stop)		= receive_next(),
    ?line [3,3,3,0] = apply_slave(?MODULE,exported_wrap,[3]),
    ?line receive_no_next(17),
    ?line shutdown(),
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
	    ?line ?dbgformat("Apply: ~p:~p/~p (~p)~n",[M,F,length(A),A]),
	    ?line Res = apply(M,F,A),
	    ?line ?dbgformat("done Apply: ~p:~p/~p (~p)~n",[M,F,length(A),A]),
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
	    ?t:fail(timeout)
    end.

receive_no_next(TO) ->
    receive
	M ->
	    ?t:fail({unexpected_message, M})
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

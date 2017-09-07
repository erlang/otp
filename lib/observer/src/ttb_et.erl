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
-module(ttb_et).
-author('siri@erix.ericsson.se').

-include("et.hrl").
-export([handler/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------- TTB format handler -----------

handler(Out,Trace,Traci,initial) ->
    S = self(),
    spawn(fun() -> init_et(S) end),
    receive {et_started,Collector} -> ok end,
    handler(Out,Trace,Traci,Collector);
handler(_,end_of_trace,_Traci,Col) ->
    get_returns(Col),
    ok;
handler(_,Trace,_Traci,Col) ->
    {ok,NewCol} = et_collector:report(Col,Trace),
    NewCol.


%%% ----------- Collector Filter -----------

collector(Event) when is_record(Event,event) -> 
    %% if collector is selected from viewer menu
    true; 
collector(Trace) ->
    et_selector:parse_event(undefined,Trace).

%% After applying collector filter to all events, iterate over
%% all events backwards and collect call/return information:
%%
%% MFA collected from return_to events is added to call and
%% return_from events as {caller,MFA} and {return_to,MFA} respecively.
%% MFA collected from call events is added to return_to events as
%% {return_from,MFA}
%%
%% This information can then be used by any filter for generating to-
%% and from fields.
get_returns(Col) ->
    Fun = fun(Event,Acc) -> collect_return_info(Event,Acc,Col) end,
    et_collector:iterate(Col, last, '-infinity', Fun, dict:new()).

collect_return_info(#event{label=L,from=Pid}=E,Acc,_Col)
  when L==return_to;L==return_from->
    %% Stacking all return_to and return_from events
    dict:update(Pid,fun(Old) -> [E|Old] end, [E], Acc);
collect_return_info(#event{label=call,from=Pid,contents=Contents}=E,Acc,Col) ->
    %% Popping return_from and return_to events
    %% If both exist, return_from will _always_ be first!!!
    MFA = get_mfarity(Contents),
    {Caller,NewAcc} =
	case dict:find(Pid,Acc) of
	    {ok,[#event{label=return_from}=RetFrom,
		 #event{label=return_to}=RetTo | Rets]} ->
		RetToCont = RetTo#event.contents,
		C = get_mfarity(RetToCont),
		NewRetTo = RetTo#event{contents=RetToCont++[{return_from,MFA}]},
		RetFromCont = RetFrom#event.contents,
		NewRetFrom = 
		    RetFrom#event{contents=RetFromCont++[{return_to,C}]},
		et_collector:report(Col,NewRetTo),
		et_collector:report(Col,NewRetFrom),
		{C, dict:store(Pid,Rets,Acc)};
	    {ok,[#event{label=return_to}=RetTo | Rets]} ->
		%% No return_from
		RetToCont = RetTo#event.contents,
		C = get_mfarity(RetToCont),
		NewRetTo = RetTo#event{contents=RetToCont++[{return_from,MFA}]},
		et_collector:report(Col,NewRetTo),
		{C, dict:store(Pid,Rets,Acc)};
	    {ok,[#event{label=return_from}=RetFrom | Rets]} ->
		%% No return_to, check if caller(pam_result) is in call event
		C = get_caller(Contents),
		RetFromCont = RetFrom#event.contents,
		NewRetFrom = 
		    RetFrom#event{contents=RetFromCont++[{return_to,C}]},
		et_collector:report(Col,NewRetFrom),
		{C, dict:store(Pid,Rets,Acc)};
	    _noreturn ->
		{nocaller,Acc}
	end,
    NewE = E#event{contents=Contents++[{caller,Caller}]},
    et_collector:report(Col,NewE),
    NewAcc;
collect_return_info(_E,Acc,_Col) ->
    Acc.

init_et(Parent) ->
    process_flag(trap_exit,true),
%    ets:new(ttb_et_table,[set,named_table,public]),
%    ets:insert(ttb_et_table,{traci,Traci}),
    EtOpt = [{active_filter,processes},
	     {dict_insert, {filter, collector}, fun collector/1},
	     {dict_insert, {filter, processes}, fun processes/1},
	     {dict_insert, {filter, modules}, fun modules/1},
	     {dict_insert, {filter, mods_and_procs}, fun mods_and_procs/1},
	     {dict_insert, {filter, functions}, fun functions/1},
	     {dict_insert, {filter, funcs_and_procs}, fun funcs_and_procs/1},
	     {hide_actions, false},
	     {max_events, infinity},
	     {max_actors, infinity}],
    {ok, Viewer} = et_viewer:start_link(EtOpt),
    Collector = et_viewer:get_collector_pid(Viewer),
    Parent ! {et_started,Collector},
    receive
	{'EXIT',Viewer,shutdown} ->
	    ok
    end.
	    


%%% ----------- Viewer Filters -----------

processes(E0) ->
    E = label(E0),
    {{FromProc,FromNode},{ToProc,ToNode}} = 
	get_actors(E#event.from,E#event.to),
    {true,E#event{from = io_lib:format("~tw~n~w",[FromProc,FromNode]),
		  to = io_lib:format("~tw~n~w",[ToProc,ToNode])}}.


mods_and_procs(E) ->
    ActorFun = fun({M,_F,_A},{Proc,Node}) -> 
		       io_lib:format("~w~n~tw~n~w",[M,Proc,Node])
	       end,
    calltrace_filter(E,ActorFun).

modules(E) ->
    ActorFun = fun({M,_F,_A},{_Proc,Node}) -> 
		       io_lib:format("~w~n~w",[M,Node])
	       end,
    calltrace_filter(E,ActorFun).

funcs_and_procs(E) ->
    ActorFun = fun({M,F,A},{Proc,Node}) -> 
		       io_lib:format("~ts~n~tw~n~w",[mfa(M,F,A),Proc,Node])
	       end,
    calltrace_filter(E,ActorFun).
    
functions(E) ->
    ActorFun = fun({M,F,A},{_Proc,Node}) -> 
		       io_lib:format("~ts~n~w",[mfa(M,F,A),Node])
	       end,
    calltrace_filter(E,ActorFun).
    


%% Common filter used by mods_and_procs/1 and modules/1
calltrace_filter(E,ActorFun) ->
    {From,To} = get_actors(E#event.from,E#event.to),
    calltrace_filter(E,From,To,ActorFun).
    
calltrace_filter(#event{label=call}=E,From,To,ActorFun) ->
    Cont = E#event.contents,
    MFA = get_mfarity(Cont),
    case lists:keysearch(caller,1,Cont) of
	{value,{_,{_CM,_CF,_CA}=Caller}} ->
	    {true, E#event{label = label(call,MFA), 
			   from = ActorFun(Caller,From),
			   to = ActorFun(MFA,To)}};
	{value,{_, _}} ->
	    {true, E#event{label = label(call,MFA), 
			   from = ActorFun(MFA,From),
			   to = ActorFun(MFA,To)}}
    end;
calltrace_filter(#event{label=return_from}=E,From,To,ActorFun) ->
    Cont = E#event.contents,
    MFA = get_mfarity(Cont),
    case lists:keysearch(return_to,1,Cont) of
	{value,{_,{_M2,_F2,_A2}=MFA2}} ->
	    {true, E#event{label = label(return_from,MFA),
			   from = ActorFun(MFA,From),
			   to = ActorFun(MFA2,To)}};
	{value,{_, _}} ->
	    {true, E#event{label = label(return_from,MFA), 
			   from = ActorFun(MFA,From),
			   to = ActorFun(MFA,To)}}
    end;
calltrace_filter(#event{label=return_to}=E,From,To,ActorFun) ->
    Cont = E#event.contents,
    {value,{_,{_M2,_F2,_A2}=MFA2}} = lists:keysearch(return_from,1,Cont),
    case get_mfarity(Cont) of
	{_M,_F,_A}=MFA ->
	    {true, E#event{label = label(return_to,MFA), 
			   from = ActorFun(MFA2,From),
			   to = ActorFun(MFA,To)}};
	undefined ->
	    {true, E#event{label = "return_to unknown", 
			   from = ActorFun(MFA2,From),
			   to = ActorFun(MFA2,To)}}
    end;
calltrace_filter(_E,_From,_To,_ActorFun) ->
    false.


label(Event=#event{label=L,contents=C}) ->
    case lists:keysearch(mfa,1,C) of
	{value,{mfa,MFA}} -> Event#event{label=label(L,MFA)};
	false -> Event
    end.
label(L,{M,F,A}) -> label(L,M,F,A);
label(L,Other) -> io_lib:format("~w ~tw",[L,Other]).
label(call,M,F,A) -> "call " ++ mfa(M,F,A);
label(return_from,M,F,A) -> "return_from " ++ mfa(M,F,A);
label(return_to,M,F,A) -> "return_to " ++ mfa(M,F,A);
label(spawn,M,F,A) -> "spawn " ++ mfa(M,F,A);
label(out,M,F,A) -> "out " ++ mfa(M,F,A);
label(in,M,F,A) -> "in " ++ mfa(M,F,A).

mfa(M,F,A) -> atom_to_list(M) ++ ":" ++ fa(F,A).
fa(F,A) -> atom_to_list(F) ++ "/" ++ integer_to_list(arity(A)).

arity(L) when is_list(L) -> length(L);
arity(I) when is_integer(I) -> I.
    
get_actors(From,To) ->
    case {get_proc(From),get_proc(To)} of
	{{_FP,_FN},{_TP,_TN}}=R -> R;
	{{FP,FN},T} -> {{FP,FN},{T,FN}};
	{F,{TP,TN}} -> {{F,TN},{TP,TN}};
	{F,T} -> {{F,unknown},{T,unknown}}
    end.
	    
get_proc({_Pid,Name,Node}) when is_atom(Name) -> {Name,Node};
get_proc({Pid,_initfunc,Node}) -> {Pid,Node};
get_proc(P) when is_pid(P); is_port(P) -> {P,node(P)};
get_proc(P) -> P.

get_mfarity(List) ->
    case get_mfa(List) of
	{M,F,A} -> {M,F,arity(A)};
	Other -> Other
    end.
get_mfa(List) ->
    {value,{mfa,MFA}} = lists:keysearch(mfa,1,List),
    MFA.

get_caller(List) ->
    case lists:keysearch(pam_result,1,List) of
	{value,{pam_result,{M,F,A}}} -> {M,F,arity(A)};
	{value,{pam_result,undefined}} -> undefined;
	{value,{pam_result,_Other}} -> nocaller;
	false -> nocaller
    end.



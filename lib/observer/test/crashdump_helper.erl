%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-module(crashdump_helper).
-export([n1_proc/2,remote_proc/2]).
-compile(r18).
-include_lib("common_test/include/ct.hrl").

n1_proc(N2,Creator) ->
    spawn(fun() -> n1_proc(Creator,N2,x,y,[]) end).
n1_proc(Creator,N2,Pid2,Port2,L) when Pid2==x;length(L)<2->
    receive 
	{N2,Pid,Port} ->
	    n1_proc(Creator,N2,Pid,Port,L);
	P ->
	    n1_proc(Creator,N2,Pid2,Port2,[P|L])
    end;
n1_proc(Creator,_N2,Pid2,Port2,_L) ->
    register(aaaaaaaa,self()),
    process_flag(save_calls,3),
    ets:new(cdv_test_ordset_table,[ordered_set]),
    erlang:send_after(1000000,self(),cdv_test_timer_message1),
    erlang:send_after(1000000,aaaaaaaa,cdv_test_timer_message2),
    erlang:send_after(1000000,noexistproc,cdv_test_timer_message3),
    Port = hd(erlang:ports()),
    Fun = fun() -> ok end,
    Ref = make_ref(),
    Pid = self(),
    Bin = list_to_binary(lists:seq(1, 255)),
    <<_:2,SubBin:17/binary,_/bits>> = Bin,

    register(named_port,Port),

    %% Dictionary
    put(list,"list"),
    put(atom,atom),
    put(integer,42),
    put(float,54.654),
    put(big_float,math:pow(2,1023)),
    put(tuple,{1,2,{}}),
    put(port,Port),
    put('fun',Fun),
    put(ref,Ref),
    put(pid,Pid),
    put(bin,Bin),
    put(sub_bin,SubBin),
    put(bignum,83974938738373873),
    put(neg_bignum,-38748762783736367),
    put(ext_pid,Pid2),
    put(ext_port,Port2),

    %% Message queue
    L = lists:seq(0,255),
    BigMsg = {message,list_to_binary(L),L},
    Port = hd(erlang:ports()),
    self() ! {short,message,1,2.5,"hello world",Port,{}},
    self() ! BigMsg,

    OtherPid = spawn(fun() -> register(aaaaaaab,self()),
			      receive after infinity -> ok end
		     end),
    link(OtherPid), % own node
    link(Pid2),     % external node
    erlang:monitor(process,OtherPid),
    erlang:monitor(process,Pid2),

    code:load_file(?MODULE),

    Creator ! {self(),done},
    receive after infinity -> ok end.

remote_proc(P1,Creator) ->
    spawn(fun() ->
		  P1 ! {node(),self(),hd(erlang:ports())},
		  Creator ! {self(),done},
		  receive after infinity -> ok end
	  end).

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2011. All Rights Reserved.
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

-module(crashdump_helper).
-export([n1_proc/2,remote_proc/2]).
-compile(r13).
-include("test_server.hrl").

n1_proc(N2,Creator) ->
    spawn(fun() -> n1_proc(Creator,N2,x,[]) end).
n1_proc(Creator,N2,P2,L) when P2==x;length(L)<2->
    receive 
	{N2,P} ->
	    n1_proc(Creator,N2,P,L);
	P ->
	    n1_proc(Creator,N2,P2,[P|L])
    end;
n1_proc(Creator,_N2,P2,_L) ->
    register(aaaaaaaa,self()),
    process_flag(save_calls,3),
    ets:new(cdv_test_ordset_table,[ordered_set]),
    erlang:send_after(1000000,self(),cdv_test_timer_message),
    Port = hd(erlang:ports()),
    Fun = fun() -> ok end,
    Ref = make_ref(),
    Pid = self(),
    Bin = list_to_binary(lists:seq(1, 255)),
    SubBin = element(1, split_binary(element(2, split_binary(Bin, 8)), 17)),
    DictionaryValue = {"list",atom,42,54.654,math:pow(2,1023),{},
		       Port,Fun,Ref,Pid,
		       Bin,SubBin,83974938738373873,-38748762783736367},
    put(dictionary_key,DictionaryValue),
    spawn(fun() -> register(aaaaaaab,self()),
		   receive after infinity -> ok end 
	  end),
    link(P2),
    Creator ! {self(),done},
    receive after infinity -> ok end.

remote_proc(P1,Creator) ->
    spawn(fun() ->
		  P1 ! {node(),self()},
		  Creator ! {self(),done},
		  receive after infinity -> ok end
	  end).

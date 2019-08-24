%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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
-export([n1_proc/2,remote_proc/2,
         dump_maps/0,create_maps/0,
         create_binaries/0,create_sub_binaries/1,
         dump_persistent_terms/0,
         create_persistent_terms/0]).
-compile(r20).
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
    <<_:2,SubBin:65/binary,_/bits>> = Bin,

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
    put(proc_bins,create_proc_bins()),
    put(bins,create_binaries()),
    put(sub_bin,SubBin),
    put(sub_bins,create_sub_binaries(get(bins))),
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
    erlang:monitor(process,init), % named process
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

create_binaries() ->
    Sizes = lists:seq(100, 120) ++ lists:seq(200, 240),
    [begin
         <<H:16/unit:8>> = erlang:md5(<<Size:32>>),
         Data = ((H bsl (8*150)) div (H+7919)),
         <<Data:Size/unit:8>>
     end || Size <- Sizes].

create_sub_binaries(Bins) ->
    [create_sub_binary(Bin, Start, LenSub) ||
        Bin <- Bins,
        Start <- [0,1,2,3,4,5,10,22],
        LenSub <- [0,1,2,3,4,6,9,65]].

create_sub_binary(Bin, Start, LenSub) ->
    Len = byte_size(Bin) - LenSub - Start,
    <<_:Start/bytes,Sub:Len/bytes,_/bytes>> = Bin,
    Sub.

create_proc_bins() ->
    Parent = self(),
    Pid =
        spawn(
          fun() ->
                  %% Just reverse the list here, so this binary is not
                  %% confused with the one created in n1_proc/5 above,
                  %% which is used for testing truncation (see
                  %% crashdump_viewer_SUITE:truncate_dump_binary/1)
                  Bin = list_to_binary(lists:reverse(lists:seq(1, 255))),
                  <<A:65/bytes,B:65/bytes,C/bytes>> = Bin,
                  Parent ! {self(),{A,B,C}}
          end),
    receive
        {Pid,ProcBins} -> ProcBins
    end.

%%%
%%% Test dumping of maps. Dumping of maps only from OTP 20.2.
%%%

dump_maps() ->
    Parent = self(),
    F = fun() ->
                register(aaaaaaaa_maps, self()),
                put(maps, create_maps()),
                Parent ! {self(),done},
                receive _ -> ok end
        end,
    Pid = spawn_link(F),
    receive
        {Pid,done} ->
            {ok,Pid}
    end.

create_maps() ->
    Map0 = maps:from_list([{I,[I,I+1]} || I <- lists:seq(1, 40)]),
    Map1 = maps:from_list([{I,{a,[I,I*I],{}}} || I <- lists:seq(1, 100)]),
    Map2 = maps:from_list([{{I},(I*I) bsl 24} || I <- lists:seq(1, 10000)]),
    Map3 = lists:foldl(fun(I, A) ->
                               A#{I=>I*I}
                       end, Map2, lists:seq(-10, 0)),
    #{a=>Map0,b=>Map1,c=>Map2,d=>Map3,e=>#{},literal=>literal_map()}.

literal_map() ->
    %% A literal map such as the one below will produce a heap dump
    %% like this:
    %%
    %%   Address1:t4:H<Address3>,H<Address4>,H<Address5>,H<Address6>
    %%   Address2:Mf4:H<Adress1>:I1,I2,I3,I4
    %%   Address3: ...  % "one"
    %%   Address4: ...  % "two"
    %%   Address5: ...  % "three"
    %%   Address6: ...  % "four"
    %%
    %% The map cannot be reconstructed in a single sequential pass.
    %%
    %% To reconstruct the map, first the string keys "one"
    %% through "four" must be reconstructed, then the tuple at
    %% Adress1, then the map at Address2.

    #{"one"=>1,"two"=>2,"three"=>3,"four"=>4}.

%%%
%%% Test dumping of persistent terms (from OTP 21.2).
%%%

dump_persistent_terms() ->
    Parent = self(),
    F = fun() ->
                register(aaaaaaaa_persistent_terms, self()),
                put(pts, create_persistent_terms()),
                Parent ! {self(),done},
                receive _ -> ok end
        end,
    Pid = spawn_link(F),
    receive
        {Pid,done} ->
            {ok,Pid}
    end.

create_persistent_terms() ->
    persistent_term:put({?MODULE,first}, {pid,42.0}),
    persistent_term:put({?MODULE,second}, [1,2,3]),
    {persistent_term:get({?MODULE,first}),persistent_term:get({?MODULE,second})}.

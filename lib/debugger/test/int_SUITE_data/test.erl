%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

%%

-module(test).

%%-compile(export_all).
-export([test1/0,
	 test2/0,
	 test3/0,
	 test4/0,
	 test5/0,
	 test6/1,
	 test7/0,
	 test8/1,
	 test9/0,
	 test10/0,
	 test11/0
	]).


-export([test9_server/1]).


test1() ->
    R1 = lists1:reverse("retep"),
    R2 = ordsets1:list_to_set([b,c,a,2,4,1]),
    R3 = lists1:reverse("nilo"),
    {R1,R2, R3}.

test2() ->
    R1 = spawn(lists1,reverse,["retep"]),
    R2 = spawn(ordsets1,list_to_set,[[b,c,a,2,4,1]]),
    R3 = spawn(lists1,reverse,["nilo"]),
    {R1,R2, R3}.


test3() ->
    A = a,
    Pid = spawn(?MODULE, test1,[]),
    B = b,
    {A, B, Pid}.

test4() ->
    Pid = spawn(?MODULE, test1,[]),
    A = a,
    B = b,
    {A, B, Pid}.
test5() ->
    L1 = [a,b,c],
    L = length(L1),
    A = a,
    B = b,
    {A, B, L, L1}.



test6(0) ->
    ok;
test6(N) when N>0 ->
    spawn(lists1,reverse,["adolfiparisrorsirapifloda"]),
    test6(N-1).


test7() ->
    CurDirReturn = file:get_cwd(),
    {ok, CurDir} = CurDirReturn,
    DirListReturn = file:list_dir(CurDir),
    {ok, DirList} = DirListReturn,
    io:format("~w~n",[DirList]).


test8(List) ->
    %% foo
    %%bar
    %% foo
    %%bar
    %% foo
    %%bar
    %% foo
    %%bar

    L2 =  [gamma|List],
    {L2, List}.

test9() ->
    S1 = spawn(?MODULE, test9_server,[self()]),
    S2 = spawn(?MODULE, test9_server,[bongo]),
    S3 = spawn(?MODULE, test9_server,[42]),

    test9_loop(S1,S2,S3).

test9_loop(S1,S2,S3) ->
    receive
	{S1, hej} ->
	    io:format("S1 ~n"),
	    test9_loop(S1,S2,S3);
	{S2, hej} ->
	    io:format("S2 ~n"),
	    test9_loop(S1,S2,S3);
	{S3, hej} ->
	    io:format("S3 ~n"),
	    test9_loop(S1,S2,S3)
    end.


test9_server(Pid) ->
    io:format("started server: ~p~n",[Pid]),
    test9_server1(Pid).

test9_server1(Pid) ->
    Pad = {pad, Pid},
    test9_server2(Pad).

test9_server2(Pad) ->
    {pad, Pid} = Pad,
    Pid ! {self(), hej}.





test10() ->
    receive
	X ->
	    done
    after 20000 ->
	    timeout
    end.

test11() ->
    receive
	X ->
	    done
    end.

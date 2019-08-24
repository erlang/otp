%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-module(otp_5235).
-export([?MODULE/0]).

-record(commit, {node,
		 decision, % presume_commit | Decision
		 ram_copies = [],
		 disc_copies = [],
		 disc_only_copies = [],
		 snmp = [],
		 schema_ops = [self(),make_ref()]
		}).

?MODULE() ->
    process_flag(trap_exit, true),
    N = 1024,
    clone(N),
    wait(N).

wait(0) -> ok;
wait(N) ->
    receive
	{'EXIT',_,normal} ->
	    wait(N-1);
	Other ->
	    exit(Other)
    end.

clone(0) -> ok;
clone(N) ->
    spawn_link(fun worker/0),
    clone(N-1).

worker() ->
    Seq = lists:seq(1, 10),
    PidList = [{N,self()} || N <- Seq],
    Commit = #commit{ram_copies=PidList,disc_copies=[],
		     disc_only_copies=[],snmp=[]},
    List = lists:duplicate(2, Commit),
    verify(run(2, List)).

verify([#commit{node=true,ram_copies=L}|T]) ->
    verify_1(L, 1),
    verify(T);
verify([]) -> ok.

verify_1([{N,Pid}|T], N) when Pid =:= self() ->
    verify_1(T, N+1);
verify_1([], _) -> ok.

run(0, L) -> L;
run(N, L) -> run(N-1, reverse(L)).

reverse([]) -> [];
reverse([H|R]) when record(H, commit) ->
    [H#commit{
       ram_copies       =  lists:reverse(H#commit.ram_copies),
       disc_copies      =  lists:reverse(H#commit.disc_copies),
       disc_only_copies =  lists:reverse(H#commit.disc_only_copies),
       snmp             = lists:reverse(H#commit.snmp),
       node = erlang:yield()
      }  
     | reverse(R)].





%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
-module(code_b_test).

-export([do_spawn/0, loop/0, check_exit/1, call/2, call/3]).

do_spawn() ->
    spawn_link(code_b_test, loop, []).

loop() ->
    receive
	dummy -> loop()
    end.

check_exit(Pid) ->
    receive
	{'EXIT',Pid,_} ->
	    true
    after 10 ->
	    %% We used to wait 1 ms. That is not always enough when
	    %% running the SMP emulator on a slow computer.
	    false
    end.

call({M,F}=Fun, Arg) when is_atom(M), is_atom(F) ->
    [Fun(Arg)];
call(Fun, Arg) when is_function(Fun) ->
    [Fun(Arg)].
    
call(M, F, Args) ->
    [erlang:apply(M, F, Args)].

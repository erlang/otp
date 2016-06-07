%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

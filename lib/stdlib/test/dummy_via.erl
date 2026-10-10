%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2026. All Rights Reserved.
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
-module(dummy_via).
-export([reset/0,
	 register_name/2,
	 whereis_name/1,
	 unregister_name/1,
	 send/2]).


reset() ->
    P = whereis(?MODULE),
    _ = try unlink(P) catch _:_ -> ok end,
    Ref = erlang:monitor(process, P),
    _ = try exit(P, kill) catch _:_ -> ok end,
    receive {'DOWN',Ref,_,_,_} -> ok end,
    Me = self(),
    Pid = spawn_link(fun() ->
			     register(?MODULE, self()),
			     Me ! {self(), started},
			     loop([])
		     end),
    receive
	{Pid, started} ->
	    Pid
    after 10000 ->
	    exit(timeout)
    end.

register_name(Name, Pid) when is_pid(Pid) ->
    call({register_name, Name, Pid}).

unregister_name(Name) ->
    call({unregister_name, Name}).

whereis_name(Name) ->
    call({whereis_name, Name}).

send(Name, Msg) ->
    case whereis_name(Name) of
	undefined ->
	    exit({badarg, {Name, Msg}});
	Pid when is_pid(Pid) ->
	    Pid ! Msg,
	    Pid
    end.

call(Req) ->
    MRef = erlang:monitor(process, ?MODULE),
    ?MODULE ! {self(), MRef, Req},
    receive
	{'DOWN', MRef, _, _, _} ->
	    erlang:error(badarg);
	{MRef, badarg} ->
	    erlang:demonitor(MRef),
	    erlang:error(badarg);
	{MRef, Reply} ->
	    erlang:demonitor(MRef),
	    Reply
    after 5000 ->
	    erlang:error(timeout)
    end.

loop(Reg) ->
    receive
	{'DOWN', _, _, P, _} when is_pid(P) ->
	    loop([X || {_,Pid,_} = X <- Reg, Pid =/= P]);
	{From, Ref, Request} when is_pid(From), is_reference(Ref) ->
	    {Reply, NewReg} = handle_request(Request, Reg),
	    From ! {Ref, Reply},
	    loop(NewReg)
    end.

handle_request({register_name, Name, Pid}, Reg) when is_pid(Pid) ->
    case lists:keyfind(Name, 1, Reg) of
	false ->
	    Ref = erlang:monitor(process, Pid),
	    {yes, [{Name, Pid, Ref}|Reg]};
	_ ->
	    {no, Reg}
    end;
handle_request({whereis_name, Name}, Reg) ->
    case lists:keyfind(Name, 1, Reg) of
	{_, Pid, _} ->
	    {Pid, Reg};
	false ->
	    {undefined, Reg}
    end;
handle_request({unregister_name, Name}, Reg) ->
    case lists:keyfind(Name, 1, Reg) of
	{_, _, Ref} ->
            _ = try erlang:demonitor(Ref) catch _:_ -> ok end;
	_ ->
	    ok
    end,
    {ok, lists:keydelete(Name, 1, Reg)};
handle_request(_, Reg) ->
    {badarg, Reg}.

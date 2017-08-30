%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
-module(code_archive_dict).
-behaviour(sys).

%% Public
-export([new/1, store/3, erase/2, find/2, foldl/3, erase/1]).

%% Internal
-export([init/3, loop/3]).

%% supervisor callback
-export([start_link/2]).

%% sys callback functions
-export([
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4
	]).

-define(SUPERVISOR, code_archive_dict_sup).

start_link(Name, Debug) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, Debug], infinity, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client

new(Name) ->
    supervisor:start_child(?SUPERVISOR, [Name]).

store(Pid, Key, Val) ->
    call(Pid, {store, Key, Val}).

erase(Pid, Key) ->
    call(Pid, {erase, Key}).

find(Pid, Key) ->
    call(Pid, {find, Key}).

foldl(Pid, Fun, Acc) ->
    call(Pid, {foldl, Fun, Acc}).

erase(Pid) ->
    call(Pid, stop).

call(Name, Msg) when is_atom(Name) ->
    call(whereis(Name), Msg);
call(Pid, Msg) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, Msg},
    receive
	{Ref, Reply} ->
	    erlang:demonitor(Ref, [flush]),
	    Reply;
	{'DOWN', Ref, _, _, Reason} ->
	    {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server

init(Parent, Name, Debug) ->
    register(Name, self()),
    Dict = dict:new(),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Dict, Parent, Debug).

loop(Dict, Parent, Debug) ->
    receive
	{system, From, Msg} ->
	    sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, Dict);
	{ReplyTo, Ref, {store, Key, Val}} ->
	    Dict2 = dict:store(Key, Val, Dict),
	    ReplyTo ! {Ref, ok},
	    ?MODULE:loop(Dict2, Parent, Debug);
	{ReplyTo, Ref, {erase, Key}} ->
	    Dict2 = dict:erase(Key, Dict),
	    ReplyTo ! {Ref, ok},
	    ?MODULE:loop(Dict2, Parent, Debug);
	{ReplyTo, Ref, {find, Key}} ->
	    Res = dict:find(Key, Dict),
	    ReplyTo ! {Ref, Res},
	    ?MODULE:loop(Dict, Parent, Debug);
	{ReplyTo, Ref, {foldl, Fun, Acc}} ->
	    Acc2 = dict:foldl(Fun, Acc, Dict),
	    ReplyTo ! {Ref, {ok, Acc2}},
	    ?MODULE:loop(Dict, Parent, Debug);
	{ReplyTo, Ref, stop} ->
	    ReplyTo ! {Ref, ok},
	    exit(normal);
	Msg ->
	    error_logger:format("~p got unexpected message: ~p\n",
				[self(), Msg]),
	    ?MODULE:loop(Dict, Parent, Debug)    
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sys callbacks

system_continue(Parent, Debug, Dict) ->
    ?MODULE:loop(Dict, Parent, Debug).

system_terminate(Reason, _Parent, _Debug, _Dict) ->
    exit(Reason).

system_code_change(Dict,_Module,_OldVsn,_Extra) ->
    {ok, Dict}.

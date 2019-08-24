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
-module(ch).
-behaviour(gen_server). 

%% External exports 
-export([start_link/1]). 
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2,
	 handle_cast/2, code_change/3]).

start_link(Name) -> gen_server:start_link(ch, Name, []). 

%%----------------------------------------------------------------- 
%% Callback functions from gen_server 
%%----------------------------------------------------------------- 
init(Name) ->
    process_flag(trap_exit, true),
    global:re_register_name(Name, self()),
    St = application:start_type(),
    St1 = case St of
	      normal ->
		  normal;
	      local ->
		  local;
	      {takeover, _N} ->
		  takeover;
	      {failover, _N} ->
		  failover;
	      Else ->
		  Else
	  end,

    %% Slow start to make sure that applications are started
    %% "at the same time". (otp_2973)
    case Name of
	{ch,77} -> timer:sleep(100);
	_ -> ok
    end,

    (catch global:send(Name, {st_type,{st, St1}})),
    {ok, []}.

handle_call({get_pid_key, Key}, _, State) -> 
    Res = application:get_key(Key),
    {reply, Res, State};

handle_call(get_pid_all_key, _, State) -> 
    Res = application:get_all_key(),
    {reply, Res, State}.

handle_info({st_type, Msg}, State) -> 
    timer:sleep(1000),
    (catch global:send(st_type, Msg)),
    {noreply, State};

handle_info(_, State) -> 
    {noreply, State}.

terminate(_Reason, _State) -> 
    ok.

handle_cast(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(os_mon_sysinfo).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_disk_info/0, get_disk_info/1, get_mem_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(DISK_INFO, $d).
-define(MEM_INFO,  $m).
-define(OK,        $o).

-record(state, {port}).

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local,os_mon_sysinfo}, os_mon_sysinfo, [],[]).

get_disk_info() ->
    gen_server:call(os_mon_sysinfo, get_disk_info).

get_disk_info(DriveRoot) ->
    gen_server:call(os_mon_sysinfo, {get_disk_info,DriveRoot}).

get_mem_info() ->
    gen_server:call(os_mon_sysinfo, get_mem_info).

%%----------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    process_flag(priority, low),
    Port = case os:type() of
	       {win32, _OSname} -> start_portprogram();
	       OS -> exit({unsupported_os, OS})
	   end,
    {ok, #state{port=Port}}.

handle_call(get_disk_info, _From, State) ->
    {reply, get_disk_info1(State#state.port), State};
handle_call({get_disk_info,RootList}, _From, State) ->
    {reply, get_disk_info1(State#state.port,RootList), State};
handle_call(get_mem_info, _From, State) ->
    {reply, get_mem_info1(State#state.port), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Port, Reason}, State) ->
    {stop, {port_died, Reason}, State#state{port=not_used}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.port of
	not_used ->
	    ok;
	Port ->
	    port_close(Port)
    end,
    ok.

%% os_mon-2.0
%% For live downgrade to/upgrade from os_mon-1.8[.1]
code_change(Vsn, PrevState, "1.8") ->
    case Vsn of

	%% Downgrade from this version
	{down, _Vsn} ->
	    process_flag(trap_exit, false);

	%% Upgrade to this version
	_Vsn ->
	    process_flag(trap_exit, true)
    end,
    {ok, PrevState};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------

start_portprogram() ->
    Port = os_mon:open_port("win32sysinfo.exe", [{packet,1}]),
    receive
	{Port, {data, [?OK]}} ->
	    Port;
	{Port, {data, Data}} ->
	    exit({port_error, Data});
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason})
    after 5000 ->
	    exit({port_error, timeout})
    end.

get_disk_info1(Port) ->
    Port ! {self(),{command,[?DISK_INFO]}},
    get_data(Port,[]).

get_disk_info1(Port,PathList) ->
    Port ! {self(),{command,[?DISK_INFO|[P++[0]||P <- PathList]]}},
    get_data(Port,[]).

get_mem_info1(Port) ->
    Port ! {self(),{command,[?MEM_INFO]}},
    get_data(Port,[]).

get_data(Port, Sofar) ->
    receive
	{Port, {data, [?OK]}} ->
	    lists:reverse(Sofar);
	{Port, {data, Bytes}} ->
	    get_data(Port, [Bytes|Sofar]);
	{'EXIT', Port, Reason} ->
	    exit({port_died, Reason})
    after 5000 ->
	    lists:reverse(Sofar)
    end.

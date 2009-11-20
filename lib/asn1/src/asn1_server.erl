%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

%% Purpose: Provide complete encode/and pre-decode of asn1.
-module(asn1_server).



-behaviour(gen_server).

-export([start_link/0,client_port/0]).

%% Internal exports, call-back functions.
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,
	 terminate/2]).


%% Macros
-define(port_names,
	{ asn1_drv01, asn1_drv02, asn1_drv03, asn1_drv04,
	  asn1_drv05, asn1_drv06, asn1_drv07, asn1_drv08,
	  asn1_drv09, asn1_drv10, asn1_drv11, asn1_drv12,
	  asn1_drv13, asn1_drv14, asn1_drv15, asn1_drv16 }).
%%% --------------------------------------------------------
%%% Interface Functions. 
%%% --------------------------------------------------------

start_link() ->
    gen_server:start_link({local, asn1_server}, asn1_server, [], []).

init([]) ->
    process_flag(trap_exit, true),
    erl_ddll:start(),
    PrivDir = code:priv_dir(asn1),
    LibDir1 = filename:join([PrivDir, "lib"]),
    case erl_ddll:load_driver(LibDir1, asn1_erl_drv) of
	ok -> ok;
	{error,_} ->
	    LibDir2 = 
		filename:join(LibDir1, 
			      erlang:system_info(system_architecture)),
	    erl_ddll:load_driver(LibDir2, asn1_erl_drv)
    end,
    open_ports("asn1_erl_drv",size(?port_names)).

open_ports(_,0) ->
    {ok, []};
open_ports(Cmd,N) ->   
    Port = open_port({spawn, Cmd}, []),
    %% check that driver is loaded, linked and working
    case catch port_control(Port, 0, []) of
	{'EXIT', _} ->
	    {stop, nodriver};
	_ ->
	    register(element(N,?port_names), Port),
	    open_ports(Cmd,N-1)
    end.

client_port() ->
    element(erlang:system_info(scheduler_id) rem size(?port_names) + 1,
	    ?port_names).


%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
    {noreply, State};

handle_info({'EXIT', Port, Reason}, State) when is_port(Port) ->
    {stop, {port_died, Reason}, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    close_ports(size(?port_names)).

close_ports(0) ->
    ok;
close_ports(N) ->   
    element(N,?port_names) ! {self(), close}, %% almost same as port_close(Name)
    close_ports(N-1).

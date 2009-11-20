%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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

%%

%%% Description: THIS MODULE IS DEPRECATD AND SHOULD BE REMOVED IN R14

-module(ssh_ssh).

-export([connect/1, connect/2, connect/3]).
-deprecated({connect, 1, next_major_release}).
-deprecated({connect, 2, next_major_release}).
-deprecated({connect, 3, next_major_release}).

-include("ssh.hrl").
-include("ssh_connect.hrl").

-define(default_timeout, 10000).

%%% Backwards compatibility
connect(A) -> 
    connect(A, []).

connect(Host, Opts) when is_list(Host) ->
    connect(Host, 22, Opts);
connect(CM, Opts) ->
    Timeout = proplists:get_value(connect_timeout, Opts, ?default_timeout),
    session(CM, Timeout).

connect(Host, Port, Opts) ->
    case ssh:connect(Host, Port, Opts) of
	{ok, CM} ->
	    session(CM, proplists:get_value(connect_timeout,
					    Opts, ?default_timeout));
	Error ->
	    Error
    end.

session(CM, Timeout) ->
    case ssh_connection:session_channel(CM, Timeout) of
	{ok, ChannelId}  ->
	    Args = [{channel_cb, ssh_shell}, 
		    {init_args,[CM, ChannelId]},
		    {cm, CM}, {channel_id, ChannelId}],
	    {ok, State} = ssh_channel:init([Args]),
	    ssh_channel:enter_loop(State);
	Error ->
	    Error
    end.

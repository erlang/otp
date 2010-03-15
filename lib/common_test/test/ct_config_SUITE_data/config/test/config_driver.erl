%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File: ct_config_SUITE
%%%
%%% Description:
%%% Config driver used in the CT's tests (config_2_SUITE)
%%%-------------------------------------------------------------------
-module(config_driver).
-export([read_config/1, check_parameter/1]).

read_config(ServerName)->
    ServerModule = list_to_atom(ServerName),
    ServerModule:start(),
    ServerModule:get_config().

check_parameter(ServerName)->
    ServerModule = list_to_atom(ServerName),
    case code:is_loaded(ServerModule) of
	{file, _}->
	    {ok, {config, ServerName}};
	false->
	    case code:load_file(ServerModule) of
		{module, ServerModule}->
		    {ok, {config, ServerName}};
		{error, nofile}->
		    {error, {wrong_config, "File not found: " ++ ServerName ++ ".beam"}}
	    end
    end.

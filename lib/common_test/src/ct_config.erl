%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : ct_config.erl
%% Description : CT module for reading and manipulating of configuration
%%		 data
%%
%% Created : 15 February 2010
%%----------------------------------------------------------------------
-module(ct_config).

-export([read_config_files/1,
	set_config/1, set_config/2, set_config/3,
	delete_config/1,
	get_config_file_list/1]).

-include("ct_event.hrl").
-include("ct_util.hrl").

-record(ct_conf,{key,value,ref,name='_UNDEF',default=false}).
%% default = {true,suite} | {true,testcase} | false

read_config_files(Opts) ->
    AddCallback = fun(CallBack, Files)->
	lists:map(fun(X)-> {CallBack, X} end, Files)
    end,
    ConfigFiles = case lists:keyfind(config, 1, Opts) of
	{config, ConfigLists}->
	    lists:foldr(fun({Callback,Files}, Acc)->
				AddCallback(Callback,Files) ++ Acc
			end,
			[],
			ConfigLists);
	false->
	    []
    end,
    read_config_files_int(ConfigFiles).

read_config_files_int([{Callback, File}|Files])->
    case Callback:read_config_file(File) of
	{ok, Config}->
	    set_config(Config),
	    read_config_files_int(Files);
	{error, ErrorName, ErrorDetail}->
	    {user_error, {ErrorName, File, ErrorDetail}}
    end;
read_config_files_int([])->
    ok.

set_config(Config) ->
    set_config('_UNDEF',Config,false).

set_config(Config,Default) ->
    set_config('_UNDEF',Config,Default).

set_config(Name,Config,Default) ->
    [ets:insert(?attr_table,
		#ct_conf{key=Key,value=Val,ref=ct_util:ct_make_ref(),
			 name=Name,default=Default}) ||
	{Key,Val} <- Config].

delete_config(Default) ->
    ets:match_delete(?attr_table,#ct_conf{default=Default,_='_'}),
    ok.

process_user_configs(Opts, Acc)->
    case lists:keytake(userconfig, 1, Opts) of
	false->
	    Acc;
	{value, {userconfig, {Callback, Files=[File|_]}}, NewOpts} when
	    is_list(File)->
		process_user_configs(NewOpts, [{Callback, Files} | Acc]);
	{value, {userconfig, {Callback, File=[C|_]}}, NewOpts} when
	    is_integer(C)->
		process_user_configs(NewOpts, [{Callback, [File]} | Acc]);
	{value, {userconfig, {_Callback, []}}, NewOpts}->
	    process_user_configs(NewOpts, Acc)
    end.

process_default_configs(Opts)->
    case lists:keysearch(config, 1, Opts) of
	{value,{_,Files=[File|_]}} when is_list(File) ->
	    Files;
	{value,{_,File=[C|_]}} when is_integer(C) ->
	    [File];
	{value,{_,[]}} ->
	    [];
	false ->
	    []
    end.

get_config_file_list(Opts)->
    DefaultConfigs = process_default_configs(Opts),
    CfgFiles =
	if
	    DefaultConfigs == []->
		[];
	    true->
		[{ct_config_plain, DefaultConfigs}]
	end ++
	process_user_configs(Opts, []),
    CfgFiles.

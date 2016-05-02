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
%%

-module(httpd).

-behaviour(inets_service).

-include("httpd.hrl").
-include("httpd_internal.hrl").

%% Behavior callbacks
-export([
	 start_standalone/1, 
	 start_service/1, 
	 stop_service/1, 
	 services/0, 
	 service_info/1
	]).

%% API
-export([parse_query/1, reload_config/2, info/1, info/2, info/3]).

%%%========================================================================
%%% API
%%%========================================================================

parse_query(String) ->
  SplitString = re:split(String,"[&;]", [{return, list}]),
  foreach(SplitString).

reload_config(Config = [Value| _], Mode) when is_tuple(Value) ->
    do_reload_config(Config, Mode);
reload_config(ConfigFile, Mode) ->
    case httpd_conf:load(ConfigFile) of
	{ok, ConfigList} ->
	    do_reload_config(ConfigList, Mode);
	Error ->
	    Error
    end.

info(Pid) when is_pid(Pid) ->
    info(Pid, []).

info(Pid, Properties) when is_pid(Pid) andalso is_list(Properties) ->
    {ok, ServiceInfo} = service_info(Pid), 
    Address = proplists:get_value(bind_address, ServiceInfo),
    Port = proplists:get_value(port, ServiceInfo),
    Profile = proplists:get_value(profile, ServiceInfo, default),
    case Properties of
	[] ->
	    info(Address, Port, Profile);
	_ ->
	    info(Address, Port, Profile, Properties)
    end; 

info(Address, Port) when is_integer(Port) ->    
    info(Address, Port, default).

info(Address, Port, Profile) when is_integer(Port), is_atom(Profile) ->    
    httpd_conf:get_config(Address, Port, Profile);

info(Address, Port, Properties) when is_integer(Port) andalso 
				     is_list(Properties) ->    
    httpd_conf:get_config(Address, Port, default, Properties).

info(Address, Port, Profile, Properties) when is_integer(Port) andalso 
					      is_atom(Profile) andalso is_list(Properties) ->    
    httpd_conf:get_config(Address, Port, Profile, Properties).


%%%========================================================================
%%% Behavior callbacks
%%%========================================================================

start_standalone(Config) ->
    httpd_sup:start_link([{httpd, Config}], stand_alone).

start_service(Conf) ->
    httpd_sup:start_child(Conf).

stop_service({Address, Port}) ->
    stop_service({Address, Port, ?DEFAULT_PROFILE});
stop_service({Address, Port, Profile}) ->
     httpd_sup:stop_child(Address, Port, Profile);
stop_service(Pid) when is_pid(Pid) ->
    case service_info(Pid)  of
	{ok, Info} ->	   
	    Address = proplists:get_value(bind_address, Info),
	    Port = proplists:get_value(port, Info),
	    Profile = proplists:get_value(profile, Info, ?DEFAULT_PROFILE),
	    stop_service({Address, Port, Profile});
	Error ->
	    Error
    end.
	    
services() ->
    [{httpd, ChildPid} || {_, ChildPid, _, _} <- 
			      supervisor:which_children(httpd_sup)].
service_info(Pid) ->
    try
	[{ChildName, ChildPid} || 
	    {ChildName, ChildPid, _, _} <- 
		supervisor:which_children(httpd_sup)] of
	Children ->
	    child_name2info(child_name(Pid, Children))
    catch
	exit:{noproc, _} ->
	    {error, service_not_available} 
    end.

%%%--------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

child_name(_, []) ->
    undefined;
child_name(Pid, [{Name, Pid} | _]) ->
    Name;
child_name(Pid, [_ | Children]) ->
    child_name(Pid, Children).

child_name2info(undefined) ->
    {error, no_such_service};
child_name2info({httpd_instance_sup, any, Port, Profile}) ->
    {ok, Host} = inet:gethostname(),
    Info = info(any, Port, Profile, [server_name]),
    {ok, [{bind_address,  any}, {host, Host}, {port, Port} | Info]};
child_name2info({httpd_instance_sup, Address, Port, Profile}) ->
    Info = info(Address, Port, Profile, [server_name]),
    case inet:gethostbyaddr(Address) of
	{ok, {_, Host, _, _,_, _}} ->
	    {ok, [{bind_address, Address}, 
		  {host, Host}, {port, Port} | Info]};
	_  ->
	    {ok, [{bind_address, Address}, {port, Port} | Info]}
    end.


reload(Config, Address, Port, Profile) ->
    Name = make_name(Address,Port, Profile),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:reload(Pid, Config);
	_ ->
	    {error,not_started}
    end.

    
%%% =========================================================
%%% Function:    block/3, block/4
%%%              block(Addr, Port, Mode)
%%%              block(ConfigFile, Mode, Timeout)
%%%              block(Addr, Port, Mode, Timeout)
%%% 
%%% Returns:     ok | {error,Reason}
%%%              
%%% Description: This function is used to block an HTTP server.
%%%              The blocking can be done in two ways, 
%%%              disturbing or non-disturbing. Default is disturbing.
%%%              When a HTTP server is blocked, all requests are rejected
%%%              (status code 503).
%%% 
%%%              disturbing:
%%%              By performing a disturbing block, the server
%%%              is blocked forcefully and all ongoing requests
%%%              are terminated. No new connections are accepted.
%%%              If a timeout time is given then, on-going requests
%%%              are given this much time to complete before the
%%%              server is forcefully blocked. In this case no new 
%%%              connections is accepted.
%%% 
%%%              non-disturbing:
%%%              A non-disturbing block is more gracefull. No
%%%              new connections are accepted, but the ongoing 
%%%              requests are allowed to complete.
%%%              If a timeout time is given, it waits this long before
%%%              giving up (the block operation is aborted and the 
%%%              server state is once more not-blocked).
%%%
%%% Types:       Port       -> integer()             
%%%              Addr       -> {A,B,C,D} | string() | undefined
%%%              ConfigFile -> string()
%%%              Mode       -> disturbing | non_disturbing
%%%              Timeout    -> integer()
%%%

block(Addr, Port, Profile, disturbing) when is_integer(Port) ->
    do_block(Addr, Port, Profile, disturbing);
block(Addr, Port, Profile, non_disturbing) when is_integer(Port) ->
    do_block(Addr, Port, Profile, non_disturbing).
do_block(Addr, Port, Profile, Mode) when is_integer(Port) andalso is_atom(Mode) -> 
    Name = make_name(Addr, Port, Profile),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:block(Pid, Mode);
	_ ->
	    {error,not_started}
    end.
    
%%% =========================================================
%%% Function:    unblock/2
%%%              unblock(Addr, Port)
%%%              
%%% Description: This function is used to reverse a previous block 
%%%              operation on the HTTP server.
%%%
%%% Types:       Port       -> integer()             
%%%              Addr       -> {A,B,C,D} | string() | undefined
%%%              ConfigFile -> string()
%%%

unblock(Addr, Port, Profile) when is_integer(Port) -> 
    Name = make_name(Addr,Port, Profile),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:unblock(Pid);
	_ ->
	    {error,not_started}
    end.

foreach([]) ->
  [];
foreach([KeyValue|Rest]) ->
    Plus2Space = re:replace(KeyValue,"[\+]"," ", [{return,list}, global]),
    case re:split(Plus2Space,"=", [{return, list}]) of
	[Key|Value] ->
	    [{http_uri:decode(Key),
	      http_uri:decode(lists:flatten(Value))}|foreach(Rest)];
	_ ->
	    foreach(Rest)
    end.


make_name(Addr, Port, Profile) ->
    httpd_util:make_name("httpd", Addr, Port, Profile).


do_reload_config(ConfigList, Mode) ->
    case (catch httpd_conf:validate_properties(ConfigList)) of
	{ok, Config} ->
	    Address = proplists:get_value(bind_address, Config, any), 
	    Port    = proplists:get_value(port, Config, 80),
	    Profile = proplists:get_value(profile, Config, default),
	    case block(Address, Port, Profile, Mode) of
		ok ->
		    reload(Config, Address, Port, Profile),
		    unblock(Address, Port, Profile);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

%%%--------------------------------------------------------------
%%% Deprecated 
%%%--------------------------------------------------------------

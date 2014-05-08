%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2014. All Rights Reserved.
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

-module(httpd).

-behaviour(inets_service).

-include("httpd.hrl").

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
  {ok, SplitString} = inets_regexp:split(String,"[&;]"),
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
    case Properties of
	[] ->
	    info(Address, Port);
	_ ->
	    info(Address, Port, Properties)
    end; 
info(Address, Port) when is_integer(Port) ->    
    httpd_conf:get_config(Address, Port).

info(Address, Port, Properties) when is_integer(Port) andalso 
				     is_list(Properties) ->    
    httpd_conf:get_config(Address, Port, Properties).


%%%========================================================================
%%% Behavior callbacks
%%%========================================================================

start_standalone(Config) ->
    httpd_sup:start_link([{httpd, Config}], stand_alone).

start_service(Conf) ->
    httpd_sup:start_child(Conf).

stop_service({Address, Port}) ->
    httpd_sup:stop_child(Address, Port);

stop_service(Pid) when is_pid(Pid) ->
    case service_info(Pid)  of
	{ok, Info} ->	   
	    Address = proplists:get_value(bind_address, Info),
	    Port = proplists:get_value(port, Info),
	    stop_service({Address, Port});
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
child_name2info({httpd_instance_sup, any, Port}) ->
    {ok, Host} = inet:gethostname(),
    Info = info(any, Port, [server_name]),
    {ok, [{bind_address,  any}, {host, Host}, {port, Port} | Info]};
child_name2info({httpd_instance_sup, Address, Port}) ->
    Info = info(Address, Port, [server_name]),
    case inet:gethostbyaddr(Address) of
	{ok, {_, Host, _, _,_, _}} ->
	    {ok, [{bind_address, Address}, 
		  {host, Host}, {port, Port} | Info]};
	_  ->
	    {ok, [{bind_address, Address}, {port, Port} | Info]}
    end.


reload(Config, Address, Port) ->
    Name = make_name(Address,Port),
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

block(Addr, Port, disturbing) when is_integer(Port) ->
    do_block(Addr, Port, disturbing);
block(Addr, Port, non_disturbing) when is_integer(Port) ->
    do_block(Addr, Port, non_disturbing);

block(ConfigFile, Mode, Timeout) 
  when is_list(ConfigFile) andalso 
       is_atom(Mode) andalso 
       is_integer(Timeout) ->
    case get_addr_and_port(ConfigFile) of
	{ok, Addr, Port} ->
	    block(Addr, Port, Mode, Timeout);
	Error ->
	    Error
    end.


block(Addr, Port, non_disturbing, Timeout) 
  when is_integer(Port) andalso is_integer(Timeout) ->
    do_block(Addr, Port, non_disturbing, Timeout);
block(Addr,Port,disturbing,Timeout) 
  when is_integer(Port) andalso is_integer(Timeout) ->
    do_block(Addr, Port, disturbing, Timeout).

do_block(Addr, Port, Mode) when is_integer(Port) andalso is_atom(Mode) -> 
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:block(Pid,Mode);
	_ ->
	    {error,not_started}
    end.
    

do_block(Addr, Port, Mode, Timeout) 
  when is_integer(Port) andalso is_atom(Mode) -> 
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:block(Pid,Mode,Timeout);
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

unblock(Addr, Port) when is_integer(Port) -> 
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:unblock(Pid);
	_ ->
	    {error,not_started}
    end.

foreach([]) ->
  [];
foreach([KeyValue|Rest]) ->
  {ok, Plus2Space, _} = inets_regexp:gsub(KeyValue,"[\+]"," "),
  case inets_regexp:split(Plus2Space,"=") of
    {ok,[Key|Value]} ->
      [{http_uri:decode(Key),
	http_uri:decode(lists:flatten(Value))}|foreach(Rest)];
    {ok,_} ->
      foreach(Rest)
  end.

get_addr_and_port(ConfigFile) ->
    case httpd_conf:load(ConfigFile) of
	{ok, ConfigList} ->
	    case (catch httpd_conf:validate_properties(ConfigList)) of
		{ok, Config} ->
		    Address = proplists:get_value(bind_address, Config, any), 
		    Port    = proplists:get_value(port, Config, 80),
		    {ok, Address, Port};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.


make_name(Addr, Port) ->
    httpd_util:make_name("httpd", Addr, Port).


do_reload_config(ConfigList, Mode) ->
    case (catch httpd_conf:validate_properties(ConfigList)) of
	{ok, Config} ->
	    Address = proplists:get_value(bind_address, Config, any), 
	    Port    = proplists:get_value(port, Config, 80),
	    case block(Address, Port, Mode) of
		ok ->
		    reload(Config, Address, Port),
		    unblock(Address, Port);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

%%%--------------------------------------------------------------
%%% Deprecated 
%%%--------------------------------------------------------------

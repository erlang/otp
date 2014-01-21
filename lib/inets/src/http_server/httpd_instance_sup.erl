%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2013. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: The top supervisor for an instance of the http server. (You may
%%          have several instances running on the same machine.) Hangs under
%%          httpd_sup.
%%----------------------------------------------------------------------

-module(httpd_instance_sup).

-behaviour(supervisor).

%% Internal application API
-export([start_link/3, start_link/4]).

%% Supervisor callbacks
-export([init/1]).

%%%=========================================================================
%%%  Internal Application API
%%%=========================================================================
start_link([{_, _}| _] = Config, AcceptTimeout, Debug)  ->
    case (catch httpd_conf:validate_properties(Config)) of
	{ok, Config2} ->
	    Address = proplists:get_value(bind_address, Config2),
	    Port    = proplists:get_value(port, Config2), 
	    Name = make_name(Address, Port),
	    SupName = {local, Name},
	    supervisor:start_link(SupName, ?MODULE, 
				  [undefined, Config2, AcceptTimeout,
				   Debug, Address, Port]);
	{error, Reason} ->
	    error_logger:error_report(Reason),
	    {stop, Reason}
    end;

start_link(ConfigFile, AcceptTimeout, Debug) ->
    case file_2_config(ConfigFile) of
	{ok, ConfigList, Address, Port} ->
	    Name    = make_name(Address, Port),
	    SupName = {local, Name},
	    supervisor:start_link(SupName, ?MODULE, 
				  [ConfigFile, ConfigList, AcceptTimeout,
				   Debug, Address, Port]);	
	{error, Reason} ->
	    error_logger:error_report(Reason),
	    {stop, Reason}
    end.


start_link([{_, _}| _] = Config, AcceptTimeout, ListenInfo, Debug) ->
    case (catch httpd_conf:validate_properties(Config)) of
	{ok, Config2} ->
	    Address = proplists:get_value(bind_address, Config2),
	    Port    = proplists:get_value(port, Config2), 
	    Name = make_name(Address, Port),
	    SupName = {local, Name},
	    supervisor:start_link(SupName, ?MODULE, 
				  [undefined, Config2, AcceptTimeout,
				   Debug, Address, Port, ListenInfo]);
	{error, Reason} ->
	    error_logger:error_report(Reason),
	    {stop, Reason}
    end;

start_link(ConfigFile, AcceptTimeout, ListenInfo, Debug) ->
    case file_2_config(ConfigFile) of
	{ok, ConfigList, Address, Port} ->
	    Name    = make_name(Address, Port),
	    SupName = {local, Name},
	    supervisor:start_link(SupName, ?MODULE, 
				  [ConfigFile, ConfigList, AcceptTimeout,
				   Debug, Address, Port, ListenInfo]);	
	{error, Reason} ->
	    error_logger:error_report(Reason),
	    {stop, Reason}
    end.


%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([ConfigFile, ConfigList, AcceptTimeout, Debug, Address, Port]) -> 
    httpd_util:enable_debug(Debug), 
    Flags = {one_for_one, 0, 1},
    Children  = [httpd_connection_sup_spec(Address, Port), 
		 httpd_acceptor_sup_spec(Address, Port, ConfigList, AcceptTimeout,
					 undefined), 
		 sup_spec(httpd_misc_sup, Address, Port), 
		 worker_spec(httpd_manager, Address, Port, 
			     ConfigFile, ConfigList,AcceptTimeout)],
    {ok, {Flags, Children}};
init([ConfigFile, ConfigList, AcceptTimeout, Debug, Address, Port, ListenInfo]) -> 
    httpd_util:enable_debug(Debug), 
    Flags = {one_for_one, 0, 1},
    Children  = [httpd_connection_sup_spec(Address, Port), 
		 httpd_acceptor_sup_spec(Address, Port, ConfigList, AcceptTimeout,
					ListenInfo), 
		 sup_spec(httpd_misc_sup, Address, Port), 
		 worker_spec(httpd_manager, Address, Port, ListenInfo, 
			     ConfigFile, ConfigList, AcceptTimeout)],
    {ok, {Flags, Children}}.


%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
httpd_connection_sup_spec(Address, Port) -> 
    Name = {httpd_connection_sup, Address, Port},
    StartFunc = {httpd_connection_sup, start_link, [[Address, Port]]},
    Restart = permanent,
    Shutdown = 5000,
     Modules = [httpd_connection_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

httpd_acceptor_sup_spec(Address, Port, ConfigList, AcceptTimeout, ListenInfo) ->
    Name = {httpd_acceptor_sup, Address, Port},
    StartFunc = {httpd_acceptor_sup, start_link, [[Address, Port, ConfigList, AcceptTimeout, ListenInfo]]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [httpd_acceptor_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.
    
sup_spec(SupModule, Address, Port) ->
    Name = {SupModule, Address, Port},
    StartFunc = {SupModule, start_link, [Address, Port]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [SupModule],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.
    
worker_spec(WorkerModule, Address, Port, ConfigFile, 
	    ConfigList, AcceptTimeout) ->
    Name = {WorkerModule, Address, Port},
    StartFunc = {WorkerModule, start_link, 
		 [ConfigFile, ConfigList, AcceptTimeout]}, 
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [WorkerModule],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

worker_spec(WorkerModule, Address, Port, ListenInfo, ConfigFile, 
	    ConfigList, AcceptTimeout) ->
    Name = {WorkerModule, Address, Port},
    StartFunc = {WorkerModule, start_link, 
		 [ConfigFile, ConfigList, AcceptTimeout, ListenInfo]}, 
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [WorkerModule],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

make_name(Address,Port) ->
    httpd_util:make_name("httpd_instance_sup", Address, Port).


file_2_config(ConfigFile) ->
    case httpd_conf:load(ConfigFile) of
	{ok, ConfigList} ->
	    case (catch httpd_conf:validate_properties(ConfigList)) of
		{ok, Config} ->
		    Address = proplists:get_value(bind_address, ConfigList),
		    Port    = proplists:get_value(port, ConfigList),
		    {ok, Config, Address, Port};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

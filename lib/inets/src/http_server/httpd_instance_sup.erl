%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: The top supervisor for an instance of the http server. (You may
%%          have several instances running on the same machine.) Hangs under
%%          httpd_sup.
%%----------------------------------------------------------------------

-module(httpd_instance_sup).

-behaviour(supervisor).

-include("httpd_internal.hrl").

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
	    Profile = proplists:get_value(profile, Config2, ?DEFAULT_PROFILE), 
	    Name = make_name(Address, Port, Profile),
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
	    Profile = proplists:get_value(profile, ConfigList, ?DEFAULT_PROFILE), 
	    Name    = make_name(Address, Port, Profile),
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
	    Profile = proplists:get_value(profile, Config2, ?DEFAULT_PROFILE), 
	    Name = make_name(Address, Port, Profile),
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
	    Profile = proplists:get_value(profile, ConfigList, ?DEFAULT_PROFILE), 
	    Name    = make_name(Address, Port, Profile),
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
    Profile = proplists:get_value(profile, ConfigList, ?DEFAULT_PROFILE),
    Flags = {one_for_one, 0, 1},
    Children  = [httpd_connection_sup_spec(Address, Port, Profile), 
		 httpd_acceptor_sup_spec(Address, Port, Profile, ConfigList, AcceptTimeout,
					 undefined), 
		 sup_spec(httpd_misc_sup, Address, Port, Profile), 
		 worker_spec(httpd_manager, Address, Port, Profile, 
			     ConfigFile, ConfigList,AcceptTimeout)],
    {ok, {Flags, Children}};
init([ConfigFile, ConfigList, AcceptTimeout, Debug, Address, Port, ListenInfo]) -> 
    httpd_util:enable_debug(Debug), 
    Profile = proplists:get_value(profile, ConfigList, ?DEFAULT_PROFILE),
    Flags = {one_for_one, 0, 1},
    Children  = [httpd_connection_sup_spec(Address, Port, Profile), 
		 httpd_acceptor_sup_spec(Address, Port, Profile, ConfigList, AcceptTimeout,
					 ListenInfo), 
		 sup_spec(httpd_misc_sup, Address, Port, Profile), 
		 worker_spec(httpd_manager, Address, Port, Profile, ListenInfo, 
			     ConfigFile, ConfigList, AcceptTimeout)],
    {ok, {Flags, Children}}.


%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
httpd_connection_sup_spec(Address, Port, Profile) -> 
    Name = {httpd_connection_sup, Address, Port, Profile},
    StartFunc = {httpd_connection_sup, start_link, [[Address, Port]]},
    Restart = permanent,
    Shutdown = 5000,
     Modules = [httpd_connection_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

httpd_acceptor_sup_spec(Address, Port, Profile, ConfigList, AcceptTimeout, ListenInfo) ->
    Name = {httpd_acceptor_sup, Address, Port, Profile},
    StartFunc = {httpd_acceptor_sup, start_link, [[Address, Port, ConfigList, AcceptTimeout, ListenInfo]]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [httpd_acceptor_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.
    
sup_spec(SupModule, Address, Port, Profile) ->
    Name = {SupModule, Address, Port, Profile},
    StartFunc = {SupModule, start_link, [Address, Port, Profile]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [SupModule],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.
    
worker_spec(WorkerModule, Address, Port, Profile, ConfigFile, 
	    ConfigList, AcceptTimeout) ->
    Name = {WorkerModule, Address, Port, Profile},
    StartFunc = {WorkerModule, start_link, 
		 [ConfigFile, ConfigList, AcceptTimeout]}, 
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [WorkerModule],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

worker_spec(WorkerModule, Address, Port, Profile, ListenInfo, ConfigFile, 
	    ConfigList, AcceptTimeout) ->
    Name = {WorkerModule, Address, Port, Profile},
    StartFunc = {WorkerModule, start_link, 
		 [ConfigFile, ConfigList, AcceptTimeout, ListenInfo]}, 
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [WorkerModule],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

make_name(Address, Port, Profile) ->
    httpd_util:make_name("httpd_instance_sup", Address, Port, Profile).


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

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2013. All Rights Reserved.
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
%% Purpose: The top supervisor for the http server (httpd) hangs under 
%%          inets_sup.
%%----------------------------------------------------------------------

-module(httpd_sup).

-behaviour(supervisor).

%% Internal application API
-export([start_link/1, start_link/2]).
-export([start_child/1, restart_child/2, stop_child/2]).

%% Supervisor callback
-export([init/1]).

-export([listen_init/4]).

-define(TIMEOUT, 15000).
-include("httpd_internal.hrl").
-include("inets_internal.hrl").

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(HttpdServices) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [HttpdServices]).

start_link(HttpdServices, stand_alone) ->
    supervisor:start_link(?MODULE, [HttpdServices]).

start_child(Config) ->
    try httpd_config(Config) of
	{ok, NewConfig} ->
	    Spec = httpd_child_spec(NewConfig, ?TIMEOUT, []),
	    case supervisor:start_child(?MODULE, Spec) of
		{error, {invalid_child_spec, Error}} ->
		    Error;
		Other ->
		    Other
	    end
    catch
	throw:Error ->
	    Error
    end.
    

restart_child(Address, Port) ->
    Name = id(Address, Port),
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            supervisor:restart_child(?MODULE, Name);
        Error ->
            Error
    end.
    
stop_child(Address, Port) ->
    Name = id(Address, Port),
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            supervisor:delete_child(?MODULE, Name);
        Error ->
            Error
    end.
    
id(Address, Port) ->
    {httpd_instance_sup, Address, Port}.


%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([HttpdServices]) ->
    ?hdrd("starting", [{httpd_service, HttpdServices}]),
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = child_specs(HttpdServices, []),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.


%%%=========================================================================
%%%  Internal functions
%%%=========================================================================

%% The format of the httpd service is:
%% httpd_service() -> {httpd,httpd()}
%% httpd()         -> [httpd_config()] | file()
%% httpd_config()  -> {file,file()} |
%%                    {debug,debug()} |
%%                    {accept_timeout,integer()}
%% debug()         -> disable | [debug_options()]
%% debug_options() -> {all_functions,modules()} | 
%%                    {exported_functions,modules()} |
%%                    {disable,modules()}
%% modules()       -> [atom()]


child_specs([], Acc) ->
    Acc;
child_specs([{httpd, HttpdService} | Rest], Acc) ->
    ?hdrd("child specs", [{httpd, HttpdService}]),
    NewHttpdService = (catch mk_tuple_list(HttpdService)),
    ?hdrd("child specs", [{new_httpd, NewHttpdService}]),
    case catch child_spec(NewHttpdService) of
	{error, Reason} ->
	    ?hdri("failed generating child spec", [{reason, Reason}]),
	    error_msg("Failed to start service: ~n~p ~n due to: ~p~n",
		      [HttpdService, Reason]),
	    child_specs(Rest, Acc);
	Spec ->
	    ?hdrt("child spec", [{child_spec, Spec}]),
	    child_specs(Rest, [Spec | Acc])
    end.

child_spec(HttpdService) ->
    {ok, Config}  = httpd_config(HttpdService),
    ?hdrt("child spec", [{config, Config}]),
    Debug         = proplists:get_value(debug, Config, []),
    AcceptTimeout = proplists:get_value(accept_timeout, Config, 15000),
    httpd_util:valid_options(Debug, AcceptTimeout, Config),
    httpd_child_spec(Config, AcceptTimeout, Debug).

httpd_config([Value| _] = Config) when is_tuple(Value) ->
    case proplists:get_value(file, Config) of
	undefined -> 
	    case proplists:get_value(proplist_file, Config) of
		undefined ->
		    httpd_conf:validate_properties(Config);
		File ->
		   try file:consult(File) of
		       {ok, [PropList]} ->
			   httpd_conf:validate_properties(PropList)
		   catch 
		       exit:_ ->
			   throw({error, 
				  {could_not_consult_proplist_file, File}})  
		   end
	    end;
	File -> 
	    {ok, File}
    end.

httpd_child_spec([Value| _] = Config, AcceptTimeout, Debug)  
  when is_tuple(Value)  ->
    ?hdrt("httpd_child_spec - entry", [{accept_timeout, AcceptTimeout}, 
				       {debug,          Debug}]),
    Address = proplists:get_value(bind_address, Config, any),
    Port    = proplists:get_value(port, Config, 80),
    httpd_child_spec(Config, AcceptTimeout, Debug, Address, Port);

%% In this case the AcceptTimeout and Debug will only have default values...
httpd_child_spec(ConfigFile, AcceptTimeoutDef, DebugDef) ->
    ?hdrt("httpd_child_spec - entry", [{config_file,        ConfigFile}, 
				       {accept_timeout_def, AcceptTimeoutDef}, 
				       {debug_def,          DebugDef}]),
    case httpd_conf:load(ConfigFile) of
	{ok, ConfigList} ->
	    ?hdrt("httpd_child_spec - loaded", [{config_list, ConfigList}]),
	    case (catch httpd_conf:validate_properties(ConfigList)) of
		{ok, Config} ->
		    ?hdrt("httpd_child_spec - validated", [{config, Config}]),
		    Address = proplists:get_value(bind_address, Config, any), 
		    Port    = proplists:get_value(port, Config, 80),
		    AcceptTimeout = 
			proplists:get_value(accept_timeout, Config, 
					    AcceptTimeoutDef),
		    Debug   = 
			proplists:get_value(debug, Config, DebugDef),
		    httpd_child_spec([{file, ConfigFile} | Config], 
				     AcceptTimeout, Debug, Address, Port);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

httpd_child_spec(Config, AcceptTimeout, Debug, Addr, Port) ->
    Fd  = proplists:get_value(fd, Config, undefined),
    case Port == 0 orelse Fd =/= undefined of
	true ->
	    httpd_child_spec_listen(Config, AcceptTimeout, Debug, Addr, Port);
	false ->
	    httpd_child_spec_nolisten(Config, AcceptTimeout, Debug, Addr, Port)
    end.

httpd_child_spec_listen(Config, AcceptTimeout, Debug, Addr, Port) ->
    case start_listen(Addr, Port, Config) of
	{Pid, {NewPort, NewConfig, ListenSocket}} ->
	    Name      = {httpd_instance_sup, Addr, NewPort},
	    StartFunc = {httpd_instance_sup, start_link,
			 [NewConfig, AcceptTimeout, 
			  {Pid, ListenSocket}, Debug]},
	    Restart   = permanent, 
	    Shutdown  = infinity,
	    Modules   = [httpd_instance_sup],
	    Type      = supervisor,
	    {Name, StartFunc, Restart, Shutdown, Type, Modules};
	{Pid, {error, Reason}}  ->
	    exit(Pid, normal),
	    {error, Reason}
    end.
		    
httpd_child_spec_nolisten(Config, AcceptTimeout, Debug, Addr, Port) ->
    Name = {httpd_instance_sup, Addr, Port},
    StartFunc = {httpd_instance_sup, start_link,
		 [Config, AcceptTimeout, Debug]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [httpd_instance_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.


mk_tuple_list([]) ->
    [];
mk_tuple_list([H={_,_}|T]) ->
    [H|mk_tuple_list(T)];
mk_tuple_list(F) ->
    [{file, F}].

error_msg(F, A) ->
    error_logger:error_msg(F ++ "~n", A).

listen(Address, Port, Config)  ->
    try socket_type(Config) of 
	SocketType ->
	    case http_transport:start(SocketType) of
		ok ->
		    Fd = proplists:get_value(fd, Config),
		    IpFamily =  proplists:get_value(ipfamily, Config, inet6fb4),
		    case http_transport:listen(SocketType, Address, Port, Fd, IpFamily) of
			{ok, ListenSocket} ->
			    NewConfig = proplists:delete(port, Config),
			    {NewPort, _} = http_transport:sockname(SocketType, ListenSocket),
			    {NewPort, [{port, NewPort} | NewConfig], ListenSocket};
			{error, Reason} ->
			    {error, {listen, Reason}}
		    end;
		{error, Reason} ->
		    {error, {socket_start_failed, Reason}}
	    end
    catch 
	_:Reason ->
	    {error, {socket_start_failed, Reason}}
    end.		

start_listen(Address, Port, Config) ->
    Pid = listen_owner(Address, Port, Config),
    receive
	{Pid, Result} ->
	    {Pid, Result}
    end.

listen_owner(Address, Port, Config) ->
    spawn(?MODULE, listen_init, [self(), Address, Port, Config]).

listen_init(From, Address, Port, Config) ->			 
    process_flag(trap_exit, true),
    Result = listen(Address, Port, Config), 
    From ! {self(), Result},
    listen_loop().

listen_loop() ->
    receive
	{'EXIT', _, _} ->
	    ok
    end.
	    
socket_type(Config) ->
    SocketType = proplists:get_value(socket_type, Config, ip_comm), 
    socket_type(SocketType, Config).

socket_type(ip_comm = SocketType, _) ->
    SocketType;
socket_type({essl, _} = SocketType, _) ->
    SocketType;
socket_type(_, Config) ->
    {essl, ssl_config(Config)}.

%%% Backwards compatibility    
ssl_config(Config) ->
    ssl_certificate_key_file(Config) ++
	ssl_verify_client(Config) ++
	ssl_ciphers(Config) ++
	ssl_password(Config) ++
	ssl_verify_depth(Config) ++
	ssl_ca_certificate_file(Config).

ssl_certificate_key_file(Config) ->
    case proplists:get_value(ssl_certificate_key_file, Config) of
	undefined ->
	    [];
	SSLCertificateKeyFile ->
	    [{keyfile,SSLCertificateKeyFile}]
    end.

ssl_verify_client(Config) ->
    case proplists:get_value(ssl_verify_client, Config) of
	undefined ->
	    [];
	SSLVerifyClient ->
	    [{verify,SSLVerifyClient}]
    end.

ssl_ciphers(Config) ->
    case proplists:get_value(ssl_ciphers, Config) of
	undefined ->
	    [];
	Ciphers ->
	    [{ciphers, Ciphers}]
    end.

ssl_password(Config) ->
    case  proplists:get_value(ssl_password_callback_module, Config) of
	undefined ->
	    [];
	Module ->
	    case proplists:get_value(ssl_password_callback_function, Config) of
		undefined ->
		    [];
		Function ->
		    Args = case  proplists:get_value(ssl_password_callback_arguments, Config) of
			       undefined ->
				   [];
			       Arguments  ->
				   [Arguments]
			   end,
		    Password = apply(Module, Function, Args),
		    [{password, Password}]
	    end
    end.

ssl_verify_depth(Config) ->
    case proplists:get_value(ssl_verify_client_depth, Config) of
	undefined ->
	    [];
	Depth ->
	    [{depth, Depth}]
    end.

ssl_ca_certificate_file(Config) ->
    case proplists:get_value(ssl_ca_certificate_file, Config) of
	undefined ->
	    [];
	File ->
	    [{cacertfile, File}]
    end.

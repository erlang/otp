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
-module(httpd_acceptor).

-include("httpd.hrl").
-include("httpd_internal.hrl").
%%-include("inets_internal.hrl").

%% Internal application API
-export([start_link/7, start_link/8]).

%% Other exports (for spawn's etc.)
-export([acceptor_init/8, acceptor_init/9, acceptor_loop/8]).

%%
%% External API
%%

%% start_link

start_link(Manager, SocketType, Addr, Port, IpFamily, ConfigDb, AcceptTimeout) ->
    %% ?hdrd("start link", 
	  %% [{manager, Manager}, 
	  %%  {socket_type, SocketType}, 
	  %%  {address, Addr}, 
	  %%  {port, Port}, 
	  %%  {timeout, AcceptTimeout}]),
    Args = [self(), Manager, SocketType, Addr, Port, IpFamily, ConfigDb, AcceptTimeout],
    proc_lib:start_link(?MODULE, acceptor_init, Args).

start_link(Manager, SocketType, Addr, Port, ListenSocket, IpFamily, ConfigDb, AcceptTimeout) ->
    %% ?hdrd("start link", 
	  %% [{manager, Manager}, 
	  %%  {socket_type, SocketType}, 
	  %%  {listen_socket, ListenSocket}, 
	  %%  {timeout, AcceptTimeout}]),
    Args = [self(), Manager, SocketType, Addr, Port, ListenSocket, IpFamily, 
	    ConfigDb, AcceptTimeout],
    proc_lib:start_link(?MODULE, acceptor_init, Args).

acceptor_init(Parent, Manager, SocketType, Addr, Port, {ListenOwner, ListenSocket}, IpFamily,
	      ConfigDb, AcceptTimeout) ->
    %% ?hdrd("acceptor init", 
    %% 	  [{parent, Parent}, 
    %% 	   {manager, Manager}, 
    %% 	   {socket_type, SocketType}, 
    %% 	   {listen_owner, ListenOwner}, 
    %% 	   {listen_socket, ListenSocket}, 
    %% 	   {timeout, AcceptTimeout}]),
    link(ListenOwner),
    proc_lib:init_ack(Parent, {ok, self()}),
    acceptor_loop(Manager, SocketType, Addr, Port,
		  ListenSocket, IpFamily, ConfigDb, AcceptTimeout).

acceptor_init(Parent, Manager, SocketType, Addr, Port, IpFamily, 
	      ConfigDb, AcceptTimeout) ->
    %% ?hdrd("acceptor init", 
    %% 	  [{parent, Parent}, 
    %% 	   {manager, Manager}, 
    %% 	   {socket_type, SocketType}, 
    %% 	   {address, Addr}, 
    %% 	   {port, Port}, 
    %% 	   {timeout, AcceptTimeout}]),
    case (catch do_init(SocketType, Addr, Port, IpFamily)) of
	{ok, ListenSocket} ->
	    proc_lib:init_ack(Parent, {ok, self()}),
	    acceptor_loop(Manager, SocketType, Addr, Port, 
			  ListenSocket, IpFamily,ConfigDb, AcceptTimeout);
	Error ->
	    proc_lib:init_ack(Parent, Error),
	    error
    end.
   
do_init(SocketType, Addr, Port, IpFamily) ->
    %% ?hdrt("do init", []),
    do_socket_start(SocketType),
    ListenSocket = do_socket_listen(SocketType, Addr, Port, IpFamily),
    {ok, ListenSocket}.


do_socket_start(SocketType) ->
    %% ?hdrt("do socket start", []),
    case http_transport:start(SocketType) of
	ok ->
	    ok;
	{error, Reason} ->
	    %% ?hdrv("failed starting transport", [{reason, Reason}]),
	    throw({error, {socket_start_failed, Reason}})
    end.


do_socket_listen(SocketType, Addr, Port, IpFamily) ->
    %% ?hdrt("do socket listen", []),
    case http_transport:listen(SocketType, Addr, Port, IpFamily) of
	{ok, ListenSocket} ->
	    ListenSocket;
	{error, Reason} ->
	    %% ?hdrv("listen failed", [{reason,      Reason}, 
	    %% 			    {socket_type, SocketType},
	    %% 			    {addr,        Addr},
	    %% 			    {port,        Port}]),
	    throw({error, {listen, Reason}})
    end.


%% acceptor 

acceptor_loop(Manager, SocketType, Addr, Port, ListenSocket, IpFamily, ConfigDb, AcceptTimeout) ->
    %% ?hdrd("awaiting accept", 
    %% 	  [{manager, Manager}, 
    %% 	   {socket_type, SocketType}, 
    %% 	   {listen_socket, ListenSocket}, 
    %% 	   {timeout, AcceptTimeout}]),
    case (catch http_transport:accept(SocketType, ListenSocket, 50000)) of
	{ok, Socket} ->
	    %% ?hdrv("accepted", [{socket, Socket}]),
	    handle_connection(Addr, Port, Manager, ConfigDb, AcceptTimeout, 
			      SocketType, Socket),
	    ?MODULE:acceptor_loop(Manager, SocketType,  Addr, Port,
				  ListenSocket, IpFamily, ConfigDb,AcceptTimeout);
	{error, Reason} ->
	    %% ?hdri("accept failed", [{reason, Reason}]),
	    handle_error(Reason, ConfigDb),
	    ?MODULE:acceptor_loop(Manager, SocketType, Addr, Port, ListenSocket, 
				  IpFamily, ConfigDb, AcceptTimeout);
	{'EXIT', Reason} ->
	    %% ?hdri("accept exited", [{reason, Reason}]),
	    ReasonString = 
		lists:flatten(io_lib:format("Accept exit: ~p", [Reason])),
	    accept_failed(ConfigDb, ReasonString)
    end.


handle_connection(Address, Port,  Manager, ConfigDb, AcceptTimeout, SocketType, Socket) ->
    Sup = httpd_connection_sup:connection_sup(Address, Port),
    {ok, Pid} = httpd_connection_sup:start_child(Sup, [Manager, ConfigDb, AcceptTimeout]),
    http_transport:controlling_process(SocketType, Socket, Pid),
    httpd_request_handler:socket_ownership_transfered(Pid, SocketType, Socket).

handle_error(timeout, _) ->
    ok;

handle_error({enfile, _}, _) ->
    %% Out of sockets...
    sleep(200);

handle_error(emfile, _) ->
    %% Too many open files -> Out of sockets...
    sleep(200);

handle_error(closed, _) ->
    error_logger:info_report("The httpd accept socket was closed by " 
			     "a third party. "
			     "This will not have an impact on inets "
			     "that will open a new accept socket and " 
			     "go on as nothing happened. It does however "
			     "indicate that some other software is behaving "
			     "badly."),
    exit(normal);

%% This will only happen when the client is terminated abnormaly
%% and is not a problem for the server, so we want
%% to terminate normal so that we can restart without any 
%% error messages.
handle_error(econnreset,_) ->
    exit(normal);

handle_error(econnaborted, _) ->
    ok;

handle_error(esslaccept, _) ->
    %% The user has selected to cancel the installation of 
    %% the certifikate, This is not a real error, so we do 
    %% not write an error message.
    ok;

handle_error(Reason, ConfigDb) ->
    String = lists:flatten(io_lib:format("Accept error: ~p", [Reason])),
    accept_failed(ConfigDb, String).


-spec accept_failed(ConfigDB     :: term(), 
		    ReasonString :: string()) -> no_return(). 

accept_failed(ConfigDb, String) ->
    error_logger:error_report(String),
    InitData = #init_data{peername =  {0, "unknown"}},
    Info = #mod{config_db = ConfigDb, init_data = InitData},
    mod_log:error_log(Info, String),
    mod_disk_log:error_log(Info, String),
    exit({accept_failed, String}).    

sleep(T) -> receive after T -> ok end.



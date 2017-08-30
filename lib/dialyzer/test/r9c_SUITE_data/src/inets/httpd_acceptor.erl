%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: httpd_acceptor.erl,v 1.1 2008/12/17 09:53:33 mikpe Exp $
-module(httpd_acceptor).

-include("httpd.hrl").
-include("httpd_verbosity.hrl").


%% External API
-export([start_link/6]).

%% Other exports (for spawn's etc.)
-export([acceptor/4, acceptor/7]).


%%
%% External API
%%

%% start_link

start_link(Manager, SocketType, Addr, Port, ConfigDb, Verbosity) ->
    Args = [self(), Manager, SocketType, Addr, Port, ConfigDb, Verbosity],
    proc_lib:start_link(?MODULE, acceptor, Args).


acceptor(Parent, Manager, SocketType, Addr, Port, ConfigDb, Verbosity) ->
    put(sname,acc),
    put(verbosity,Verbosity),
    ?vlog("starting",[]),
    case (catch do_init(SocketType, Addr, Port)) of
	{ok, ListenSocket} ->
	    proc_lib:init_ack(Parent, {ok, self()}),
	    acceptor(Manager, SocketType, ListenSocket, ConfigDb);
	Error ->
	    proc_lib:init_ack(Parent, Error),
	    error
    end.

do_init(SocketType, Addr, Port) ->
    do_socket_start(SocketType),
    ListenSocket = do_socket_listen(SocketType, Addr, Port),
    {ok, ListenSocket}.


do_socket_start(SocketType) ->
    case httpd_socket:start(SocketType) of
	ok ->
	    ok;
	{error, Reason} ->
	    ?vinfo("failed socket start: ~p",[Reason]),
	    throw({error, {socket_start_failed, Reason}})
    end.


do_socket_listen(SocketType, Addr, Port) ->
    case httpd_socket:listen(SocketType, Addr, Port) of
	{error, Reason} ->
	    ?vinfo("failed socket listen operation: ~p", [Reason]),
	    throw({error, {listen, Reason}});
	ListenSocket ->
	    ListenSocket
    end.


%% acceptor

acceptor(Manager, SocketType, ListenSocket, ConfigDb) ->
    ?vdebug("await connection",[]),
    case (catch httpd_socket:accept(SocketType, ListenSocket, 30000)) of
	{error, Reason} ->
	    handle_error(Reason, ConfigDb, SocketType),
	    ?MODULE:acceptor(Manager, SocketType, ListenSocket, ConfigDb);

	{'EXIT', Reason} ->
	    handle_error({'EXIT', Reason}, ConfigDb, SocketType),
	    ?MODULE:acceptor(Manager, SocketType, ListenSocket, ConfigDb);

	Socket ->
	    handle_connection(Manager, ConfigDb, SocketType, Socket),
	    ?MODULE:acceptor(Manager, SocketType, ListenSocket, ConfigDb)
    end.


handle_connection(Manager, ConfigDb, SocketType, Socket) ->
    case httpd_request_handler:start_link(Manager, ConfigDb) of
	{ok, Pid} ->
	    httpd_socket:controlling_process(SocketType, Socket, Pid),
	    httpd_request_handler:synchronize(Pid, SocketType, Socket);
	{error, Reason} ->
	    handle_connection_err(SocketType, Socket, ConfigDb, Reason)
    end.


handle_connection_err(SocketType, Socket, ConfigDb, Reason) ->
    String =
	lists:flatten(
	  io_lib:format("failed starting request handler:~n   ~p", [Reason])),
    report_error(ConfigDb, String),
    httpd_socket:close(SocketType, Socket).


handle_error(timeout, _, _) ->
    ?vtrace("Accept timeout",[]),
    ok;

handle_error({enfile, _}, _, _) ->
    ?vinfo("Accept error: enfile",[]),
    %% Out of sockets...
    sleep(200);

handle_error(emfile, _, _) ->
    ?vinfo("Accept error: emfile",[]),
    %% Too many open files -> Out of sockets...
    sleep(200);

handle_error(closed, _, _) ->
    ?vlog("Accept error: closed",[]),
    %% This propably only means that the application is stopping,
    %% but just in case
    exit(closed);

handle_error(econnaborted, _, _) ->
    ?vlog("Accept aborted",[]),
    ok;

handle_error(esslaccept, _, _) ->
    %% The user has selected to cancel the installation of
    %% the certifikate, This is not a real error, so we do
    %% not write an error message.
    ok;

handle_error({'EXIT', Reason}, ConfigDb, SocketType) ->
    ?vinfo("Accept exit:~n   ~p",[Reason]),
    String = lists:flatten(io_lib:format("Accept exit: ~p", [Reason])),
    accept_failed(SocketType, ConfigDb, String);

handle_error(Reason, ConfigDb, SocketType) ->
    ?vinfo("Accept error:~n   ~p",[Reason]),
    String = lists:flatten(io_lib:format("Accept error: ~p", [Reason])),
    accept_failed(SocketType, ConfigDb, String).


accept_failed(SocketType, ConfigDb, String) ->
    error_logger:error_report(String),
    mod_log:error_log(SocketType, undefined, ConfigDb,
		      {0, "unknown"}, String),
    mod_disk_log:error_log(SocketType, undefined, ConfigDb,
			   {0, "unknown"}, String),
    exit({accept_failed, String}).


report_error(Db, String) ->
    error_logger:error_report(String),
    mod_log:report_error(Db, String),
    mod_disk_log:report_error(Db, String).


sleep(T) -> receive after T -> ok end.

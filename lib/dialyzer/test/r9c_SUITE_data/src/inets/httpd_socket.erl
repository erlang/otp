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
%%     $Id: httpd_socket.erl,v 1.1 2008/12/17 09:53:34 mikpe Exp $
%%
-module(httpd_socket).
-export([start/1,
	 listen/2, listen/3, accept/2, accept/3,
	 deliver/3, send/3, recv/4,
	 close/2,
	 peername/2, resolve/1, config/1,
	 controlling_process/3,
	 active_once/2]).

-include("httpd.hrl").

-define(VMODULE,"SOCKET").
-include("httpd_verbosity.hrl").

-include_lib("kernel/include/inet.hrl").

%% start -> ok | {error,Reason}

start(ip_comm) ->
    case inet_db:start() of
	{ok,_Pid} ->
	    ok;
	{error,{already_started,_Pid}} ->
	    ok;
	Error ->
	    Error
    end;
start({ssl,_SSLConfig}) ->
    case ssl:start() of
	ok ->
	    ok;
	{ok, _} ->
	    ok;
	{error,{already_started,_}} ->
	    ok;
	Error ->
	    Error
    end.

%% listen

listen(SocketType,Port) ->
    listen(SocketType,undefined,Port).

listen(ip_comm,Addr,Port) ->
    ?DEBUG("listening(ip_comm) to port ~p", [Port]),
    Opt = sock_opt(Addr,[{backlog,128},{reuseaddr,true}]),
    case gen_tcp:listen(Port,Opt) of
	{ok,ListenSocket} ->
	    ListenSocket;
	Error ->
	    Error
    end;
listen({ssl,SSLConfig},Addr,Port) ->
    ?DEBUG("listening(ssl) to port ~p"
           "~n   SSLConfig: ~p", [Port,SSLConfig]),
    Opt = sock_opt(Addr,SSLConfig),
    case ssl:listen(Port, Opt) of
	{ok,ListenSocket} ->
	    ListenSocket;
	Error ->
	    Error
    end.


sock_opt(undefined,Opt) -> [{packet,0},{active,false}|Opt];
sock_opt(Addr,Opt)      -> [{ip, Addr},{packet,0},{active,false}|Opt].

%% -define(packet_type_http,true).
%% -define(packet_type_httph,true).

%% -ifdef(packet_type_http).
%% sock_opt(undefined,Opt) -> [{packet,http},{active,false}|Opt];
%% sock_opt(Addr,Opt)      -> [{ip, Addr},{packet,http},{active,false}|Opt].
%% -elif(packet_type_httph).
%% sock_opt(undefined,Opt) -> [{packet,httph},{active,false}|Opt];
%% sock_opt(Addr,Opt)      -> [{ip, Addr},{packet,httph},{active,false}|Opt].
%% -else.
%% sock_opt(undefined,Opt) -> [{packet,0},{active,false}|Opt];
%% sock_opt(Addr,Opt)      -> [{ip, Addr},{packet,0},{active,false}|Opt].
%% -endif.


%% active_once

active_once(Type, Sock) ->
    active(Type, Sock, once).

active(ip_comm, Sock, Active) ->
    inet:setopts(Sock, [{active, Active}]);
active({ssl, _SSLConfig}, Sock, Active) ->
    ssl:setopts(Sock, [{active, Active}]).

%% accept

accept(A, B) ->
    accept(A, B, infinity).


accept(ip_comm,ListenSocket, T) ->
    ?DEBUG("accept(ip_comm) on socket ~p", [ListenSocket]),
    case gen_tcp:accept(ListenSocket, T) of
	{ok,Socket} ->
	    Socket;
	Error ->
	    ?vtrace("accept(ip_comm) failed for reason:"
		    "~n   Error: ~p",[Error]),
	    Error
    end;
accept({ssl,_SSLConfig},ListenSocket, T) ->
    ?DEBUG("accept(ssl) on socket ~p", [ListenSocket]),
    case ssl:accept(ListenSocket, T) of
	{ok,Socket} ->
	    Socket;
	Error ->
	    ?vtrace("accept(ssl) failed for reason:"
		    "~n   Error: ~p",[Error]),
	    Error
    end.


%% controlling_process

controlling_process(ip_comm, Socket, Pid) ->
    gen_tcp:controlling_process(Socket, Pid);
controlling_process({ssl, _}, Socket, Pid) ->
    ssl:controlling_process(Socket, Pid).


%% deliver

deliver(SocketType, Socket, IOListOrBinary)  ->
    case send(SocketType, Socket, IOListOrBinary) of
% 	{error, einval} ->
% 	    ?vlog("deliver failed for reason: einval"
% 		  "~n   SocketType: ~p"
% 		  "~n   Socket:     ~p"
% 		  "~n   Data:       ~p",
% 		  [SocketType, Socket, type(IOListOrBinary)]),
% 	    (catch close(SocketType, Socket)),
% 	    socket_closed;
	{error, _Reason} ->
	    ?vlog("deliver(~p) failed for reason:"
		  "~n   Reason: ~p",[SocketType,_Reason]),
	    (catch close(SocketType, Socket)),
	    socket_closed;
	_ ->
	    ok
    end.

% type(L) when list(L) ->
%     {list, L};
% type(B) when binary(B) ->
%     Decoded =
% 	case (catch binary_to_term(B)) of
% 	    {'EXIT', _} ->
% 		%% Oups, not a term, try list
% 		case (catch binary_to_list(B)) of
% 		    %% Oups, not a list either, give up
% 		    {'EXIT', _} ->
% 			{size, size(B)};
% 		    L ->
% 		    {list, L}
% 		end;

% 	    T ->
% 		{term, T}
% 	end,
%     {binary, Decoded};
% type(T) when tuple(T) ->
%     {tuple, T};
% type(I) when integer(I) ->
%     {integer, I};
% type(F) when float(F) ->
%     {float, F};
% type(P) when pid(P) ->
%     {pid, P};
% type(P) when port(P) ->
%     {port, P};
% type(R) when reference(R) ->
%     {reference, R};
% type(T) ->
%     {term, T}.



send(ip_comm,Socket,Data) ->
    ?DEBUG("send(ip_comm) -> ~p bytes on socket ~p",[data_size(Data),Socket]),
    gen_tcp:send(Socket,Data);
send({ssl,SSLConfig},Socket,Data) ->
    ?DEBUG("send(ssl) -> ~p bytes on socket ~p",[data_size(Data),Socket]),
    ssl:send(Socket, Data).

recv(ip_comm,Socket,Length,Timeout) ->
    ?DEBUG("recv(ip_comm) -> read from socket ~p",[Socket]),
    gen_tcp:recv(Socket,Length,Timeout);
recv({ssl,SSLConfig},Socket,Length,Timeout) ->
    ?DEBUG("recv(ssl) -> read from socket ~p",[Socket]),
    ssl:recv(Socket,Length,Timeout).

-ifdef(inets_debug).
data_size(L) when list(L) ->
    httpd_util:flatlength(L);
data_size(B) when binary(B) ->
    size(B);
data_size(O) ->
    {unknown_size,O}.
-endif.


%% peername

peername(ip_comm, Socket) ->
    case inet:peername(Socket) of
	{ok,{{A,B,C,D},Port}} ->
	    PeerName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    ?DEBUG("peername(ip_comm) on socket ~p: ~p",
		   [Socket,{Port,PeerName}]),
	    {Port,PeerName};
	{error,Reason} ->
	    ?vlog("failed getting peername:"
		  "~n   Reason: ~p"
		  "~n   Socket: ~p",
		  [Reason,Socket]),
	    {-1,"unknown"}
    end;
peername({ssl,_SSLConfig},Socket) ->
    case ssl:peername(Socket) of
	{ok,{{A,B,C,D},Port}} ->
	    PeerName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    ?DEBUG("peername(ssl) on socket ~p: ~p",
		   [Socket, {Port,PeerName}]),
	    {Port,PeerName};
	{error,_Reason} ->
	    {-1,"unknown"}
    end.

%% resolve

resolve(_) ->
    {ok,Name} = inet:gethostname(),
    Name.

%% close

close(ip_comm,Socket) ->
    Res =
	case (catch gen_tcp:close(Socket)) of
	    ok ->                  ok;
	    {error,Reason} ->      {error,Reason};
	    {'EXIT',{noproc,_}} -> {error,closed};
	    {'EXIT',Reason} ->     {error,Reason};
	    Otherwise ->           {error,Otherwise}
	end,
    ?vtrace("close(ip_comm) result: ~p",[Res]),
    Res;
close({ssl,_SSLConfig},Socket) ->
    Res =
	case (catch ssl:close(Socket)) of
	    ok ->                  ok;
	    {error,Reason} ->      {error,Reason};
	    {'EXIT',{noproc,_}} -> {error,closed};
	    {'EXIT',Reason} ->     {error,Reason};
	    Otherwise ->           {error,Otherwise}
	end,
    ?vtrace("close(ssl) result: ~p",[Res]),
    Res.

%% config (debug: {certfile, "/var/tmp/server_root/conf/ssl_server.pem"})

config(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,com_type,ip_comm) of
	ssl ->
	    case ssl_certificate_file(ConfigDB) of
		undefined ->
		    {error,
		     ?NICE("Directive SSLCertificateFile "
			   "not found in the config file")};
		SSLCertificateFile ->
		    {ssl,
		     SSLCertificateFile++
		     ssl_certificate_key_file(ConfigDB)++
		     ssl_verify_client(ConfigDB)++
		     ssl_ciphers(ConfigDB)++
		     ssl_password(ConfigDB)++
		     ssl_verify_depth(ConfigDB)++
		     ssl_ca_certificate_file(ConfigDB)}
	    end;
	ip_comm ->
	    ip_comm
    end.

ssl_certificate_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_certificate_file) of
	undefined ->
	    undefined;
	SSLCertificateFile ->
	    [{certfile,SSLCertificateFile}]
    end.

ssl_certificate_key_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_certificate_key_file) of
	undefined ->
	    [];
	SSLCertificateKeyFile ->
	    [{keyfile,SSLCertificateKeyFile}]
    end.

ssl_verify_client(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_verify_client) of
	undefined ->
	    [];
	SSLVerifyClient ->
	    [{verify,SSLVerifyClient}]
    end.

ssl_ciphers(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_ciphers) of
	undefined ->
	    [];
	Ciphers ->
	    [{ciphers, Ciphers}]
    end.

ssl_password(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_password_callback_module) of
	undefined ->
	    [];
	Module ->
	    case httpd_util:lookup(ConfigDB, ssl_password_callback_function) of
		undefined ->
		    [];
		Function ->
		    case catch apply(Module, Function, []) of
			Password when list(Password) ->
			    [{password, Password}];
			Error ->
			    error_report(ssl_password,Module,Function,Error),
			    []
		    end
	    end
    end.

ssl_verify_depth(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, ssl_verify_client_depth) of
	undefined ->
	    [];
	Depth ->
	    [{depth, Depth}]
    end.

ssl_ca_certificate_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, ssl_ca_certificate_file) of
	undefined ->
	    [];
	File ->
	    [{cacertfile, File}]
    end.


error_report(Where,M,F,Error) ->
    error_logger:error_report([{?MODULE, Where}, {apply, {M, F, []}}, Error]).

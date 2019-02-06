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
%% The Initial Developer of the Original Code is Mobile Arts AB
%% Portions created by Mobile Arts are Copyright 2002, Mobile Arts AB
%% All Rights Reserved.''
%%

%%% This version of the HTTP/1.1 client implements:
%%%      - RFC 2616 HTTP 1.1 client part
%%%      - RFC 2817 Upgrading to TLS Within HTTP/1.1 (not yet!)
%%%      - RFC 2818 HTTP Over TLS
%%%      - RFC 3229 Delta encoding in HTTP (not yet!)
%%%      - RFC 3230 Instance Digests in HTTP (not yet!)
%%%      - RFC 3310 Authentication and Key Agreement (AKA) (not yet!)
%%%      - HTTP/1.1 Specification Errata found at
%%%        http://world.std.com/~lawrence/http_errata.html
%%%    Additionally follows the following recommendations:
%%%      - RFC 3143 Known HTTP Proxy/Caching Problems (not yet!)
%%%      - draft-nottingham-hdrreg-http-00.txt (not yet!)
%%%
%%% Depends on
%%%      - uri.erl for all URL parsing (except what is handled by the C driver)
%%%      - http_lib.erl for all parsing of body and headers
%%%
%%% Supported Settings are:
%%% http_timeout      % (int) Milliseconds before a request times out
%%% http_useproxy     % (bool) True if a proxy should be used
%%% http_proxy        % (string) Proxy
%%% http_noproxylist  % (list) List with hosts not requiring proxy
%%% http_autoredirect % (bool) True if automatic redirection on 30X responses
%%% http_ssl          % (list) SSL settings. A non-empty list enables SSL/TLS
%%%                      support in the HTTP client
%%% http_pipelinesize % (int) Length of pipeline. 1 means no pipeline.
%%%                      Only has effect when initiating a new session.
%%% http_sessions     % (int) Max number of open sessions for {Addr,Port}
%%%
%%% TODO: (Known bugs!)
%% - Cache handling
%% - Doesn't handle a bunch of entity headers properly
%% - Better handling of status codes different from 200,30X and 50X
%% - Many of the settings above are not implemented!
%% - close_session/2 and cancel_request/1 doesn't work
%% - Variable pipe size.
%% - Due to the fact that inet_drv only has a single timer, the timeouts given
%%   for pipelined requests are not ok (too long)
%%
%% Note:
%% - Some servers (e.g. Microsoft-IIS/5.0) may sometimes not return a proper
%%   'Location' header on a redirect.
%%   The client will fail with {error,no_scheme} in these cases.

-module(http).
-author("johan.blom@mobilearts.se").

-export([start/0,
	 request/3,request/4,cancel_request/1,
	 request_sync/2,request_sync/3]).

-include("http.hrl").
-include("jnets_httpd.hrl").

-define(START_OPTIONS,[]).

%%% HTTP Client manager. Used to store open connections.
%%% Will be started automatically unless started explicitly.
start() ->
    application:start(ssl),
    httpc_manager:start().

%%% Asynchronous HTTP request that spawns a handler.
%%% Method                          HTTPReq
%%% options,get,head,delete,trace = {Url,Headers}
%%% post,put                      = {Url,Headers,ContentType,Body}
%%%  where Url is a {Scheme,Host,Port,PathQuery} tuple, as returned by uri.erl
%%%
%%% Returns: {ok,ReqId} |
%%%          {error,Reason}
%%% If {ok,Pid} was returned, the handler will return with
%%%    gen_server:cast(From,{Ref,ReqId,{error,Reason}}) |
%%%    gen_server:cast(From,{Ref,ReqId,{Status,Headers,Body}})
%%%  where Reason is an atom and Headers a #res_headers{} record
%%% http:format_error(Reason) gives a more informative description.
%%%
%%% Note:
%%% - Always try to find an open connection to a given host and port, and use
%%%   the associated socket.
%%% - Unless a 'Connection: close' header is provided don't close the socket
%%%   after a response is given
%%% - A given Pid, found in the database, might be terminated before the
%%%   message is sent to the Pid. This will happen e.g., if the connection is
%%%   closed by the other party and there are no pending requests.
%%% - The HTTP connection process is spawned, if necessary, in
%%%   httpc_manager:add_connection/4
request(Ref,Method,HTTPReqCont) ->
    request(Ref,Method,HTTPReqCont,[],self()).

request(Ref,Method,HTTPReqCont,Settings) ->
    request(Ref,Method,HTTPReqCont,Settings,self()).

request(Ref,Method,{{Scheme,Host,Port,PathQuery},
		    Headers,ContentType,Body},Settings,From) ->
    case create_settings(Settings,#client_settings{}) of
	{error,Reason} ->
	    {error,Reason};
	CS ->
	    case create_headers(Headers,#req_headers{}) of
		{error,Reason} ->
		    {error,Reason};
		H ->
		    Req=#request{ref=Ref,from=From,
				 scheme=Scheme,address={Host,Port},
				 pathquery=PathQuery,method=Method,
				 headers=H,content={ContentType,Body},
				 settings=CS},
		    httpc_manager:request(Req)
	    end
    end;
request(Ref,Method,{Url,Headers},Settings,From) ->
    request(Ref,Method,{Url,Headers,[],[]},Settings,From).

%%% Cancels requests identified with ReqId.
%%% FIXME! Doesn't work...
cancel_request(ReqId) ->
    httpc_manager:cancel_request(ReqId).

%%% Close all sessions currently open to Host:Port
%%% FIXME! Doesn't work...
close_session(Host,Port) ->
    httpc_manager:close_session(Host,Port).


%%% Synchronous HTTP request that waits until a response is created
%%% (e.g. successfull reply or timeout)
%%% Method                          HTTPReq
%%% options,get,head,delete,trace = {Url,Headers}
%%% post,put                      = {Url,Headers,ContentType,Body}
%%%  where Url is a string() or a {Scheme,Host,Port,PathQuery} tuple
%%%
%%% Returns: {Status,Headers,Body} |
%%%          {error,Reason}
%%% where Reason is an atom.
%%% http:format_error(Reason) gives a more informative description.
request_sync(Method,HTTPReqCont) ->
    request_sync(Method,HTTPReqCont,[]).

request_sync(Method,{Url,Headers},Settings)
  when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
    case uri:parse(Url) of
	{error,Reason} ->
	    {error,Reason};
	ParsedUrl ->
	    request_sync(Method,{ParsedUrl,Headers,[],[]},Settings,0)
    end;
request_sync(Method,{Url,Headers,ContentType,Body},Settings)
  when Method==post;Method==put ->
    case uri:parse(Url) of
	{error,Reason} ->
	    {error,Reason};
	ParsedUrl ->
	    request_sync(Method,{ParsedUrl,Headers,ContentType,Body},Settings,0)
    end;
request_sync(Method,Request,Settings) ->
    {error,bad_request}.

request_sync(Method,HTTPCont,Settings,_Redirects) ->
    case request(request_sync,Method,HTTPCont,Settings,self()) of
	{ok,_ReqId} ->
	    receive
		{'$gen_cast',{request_sync,_ReqId2,{Status,Headers,Body}}} ->
		    {Status,pp_headers(Headers),binary_to_list(Body)};
		{'$gen_cast',{request_sync,_ReqId2,{error,Reason}}} ->
		    {error,Reason};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.


create_settings([],Out) ->
    Out;
create_settings([{http_timeout,Val}|Settings],Out) ->
    create_settings(Settings,Out#client_settings{timeout=Val});
create_settings([{http_useproxy,Val}|Settings],Out) ->
    create_settings(Settings,Out#client_settings{useproxy=Val});
create_settings([{http_proxy,Val}|Settings],Out) ->
    create_settings(Settings,Out#client_settings{proxy=Val});
create_settings([{http_noproxylist,Val}|Settings],Out) ->
    create_settings(Settings,Out#client_settings{noproxylist=Val});
create_settings([{http_autoredirect,Val}|Settings],Out) ->
    create_settings(Settings,Out#client_settings{autoredirect=Val});
create_settings([{http_ssl,Val}|Settings],Out) ->
    create_settings(Settings,Out#client_settings{ssl=Val});
create_settings([{http_pipelinesize,Val}|Settings],Out)
  when integer(Val),Val>0 ->
    create_settings(Settings,Out#client_settings{max_quelength=Val});
create_settings([{http_sessions,Val}|Settings],Out)
  when integer(Val),Val>0 ->
    create_settings(Settings,Out#client_settings{max_sessions=Val});
create_settings([{Key,_Val}|_Settings],_Out) ->
    io:format("ERROR bad settings, got ~p~n",[Key]),
    {error,bad_settings}.


create_headers([],Req) ->
    Req;
create_headers([{Key,Val}|Rest],Req) ->
    case httpd_util:to_lower(Key) of
	"expect" ->
	    create_headers(Rest,Req#req_headers{expect=Val});
	OtherKey ->
	    create_headers(Rest,
			   Req#req_headers{other=[{OtherKey,Val}|
						  Req#req_headers.other]})
    end.


pp_headers(#res_headers{connection=Connection,
			transfer_encoding=Transfer_encoding,
			retry_after=Retry_after,
			content_length=Content_length,
			content_type=Content_type,
			location=Location,
			other=Other}) ->
    H1=case Connection of
	   undefined -> [];
	   _ ->	 [{'Connection',Connection}]
       end,
    H2=case Transfer_encoding of
	   undefined -> [];
	   _ ->	 [{'Transfer-Encoding',Transfer_encoding}]
       end,
    H3=case Retry_after of
	   undefined -> [];
	   _ ->	 [{'Retry-After',Retry_after}]
       end,
    H4=case Location of
	   undefined -> [];
	   _ ->	 [{'Location',Location}]
       end,
    HCL=case Content_length of
	   "0" -> [];
	   _ ->	 [{'Content-Length',Content_length}]
       end,
    HCT=case Content_type of
	   undefined -> [];
	   _ ->	 [{'Content-Type',Content_type}]
       end,
    H1++H2++H3++H4++HCL++HCT++Other.

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

%%% TODO:
%%% - If an error is returned when sending a request, don't use this
%%%   session anymore.
%%% - Closing of sessions not properly implemented for some cases

%%% File    : httpc_handler.erl
%%% Author  : Johan Blom <johan.blom@mobilearts.se>
%%% Description : Handles HTTP client responses, for a single TCP session
%%% Created :  4 Mar 2002 by Johan Blom

-module(httpc_handler).

-include("http.hrl").
-include("jnets_httpd.hrl").

-export([init_connection/2,http_request/2]).

%%% ==========================================================================
%%% "Main" function in the spawned process for the session.
init_connection(Req,Session) when record(Req,request) ->
    case catch http_lib:connect(Req) of
	{ok,Socket} ->
	    case catch http_request(Req,Socket) of
		ok ->
		    case Session#session.clientclose of
			true ->
			    ok;
			false ->
			    httpc_manager:register_socket(Req#request.address,
							  Session#session.id,
							  Socket)
		    end,
		    next_response_with_request(Req,
					       Session#session{socket=Socket});
		{error,Reason} -> % Not possible to use new session
		    gen_server:cast(Req#request.from,
				    {Req#request.ref,Req#request.id,{error,Reason}}),
		    exit_session_ok(Req#request.address,
				    Session#session{socket=Socket})
	    end;
	{error,Reason} -> % Not possible to set up new session
	    gen_server:cast(Req#request.from,
			    {Req#request.ref,Req#request.id,{error,Reason}}),
	    exit_session_ok2(Req#request.address,
			     Session#session.clientclose,Session#session.id)
    end.

next_response_with_request(Req,Session) ->
    Timeout=(Req#request.settings)#client_settings.timeout,
    case catch read(Timeout,Session#session.scheme,Session#session.socket) of
	{Status,Headers,Body} ->
	    NewReq=handle_response({Status,Headers,Body},Timeout,Req,Session),
	    next_response_with_request(NewReq,Session);
	{error,Reason} ->
	    gen_server:cast(Req#request.from,
			    {Req#request.ref,Req#request.id,{error,Reason}}),
	    exit_session(Req#request.address,Session,aborted_request);
	{'EXIT',Reason} ->
	    gen_server:cast(Req#request.from,
			    {Req#request.ref,Req#request.id,{error,Reason}}),
	    exit_session(Req#request.address,Session,aborted_request)
    end.

handle_response(Response,Timeout,Req,Session) ->
    case http_response(Response,Req,Session) of
	ok ->
	    next_response(Timeout,Req#request.address,Session);
	stop ->
	    exit(normal);
	{error,Reason} ->
	    gen_server:cast(Req#request.from,
			    {Req#request.ref,Req#request.id,{error,Reason}}),
	    exit_session(Req#request.address,Session,aborted_request)
    end.



%%% Wait for the next respond until
%%% - session is closed by the other side
%%%      => set up a new a session, if there are pending requests in the que
%%% - "Connection:close" header is received
%%%      => close the connection (release socket) then
%%%         set up a new a session, if there are pending requests in the que
%%%
%%% Note:
%%% - When invoked there are no pending responses on received requests.
%%% - Never close the session explicitly, let it timeout instead!
next_response(Timeout,Address,Session) ->
    case httpc_manager:next_request(Address,Session#session.id) of
	no_more_requests ->
	    %% There are no more pending responses, now just wait for
	    %% timeout or a new response.
	    case catch read(Timeout,
			    Session#session.scheme,Session#session.socket) of
		{error,Reason} when Reason==session_remotely_closed;
				    Reason==session_local_timeout ->
		    exit_session_ok(Address,Session);
		{error,Reason} ->
		    exit_session(Address,Session,aborted_request);
		{'EXIT',Reason} ->
		    exit_session(Address,Session,aborted_request);
		{Status2,Headers2,Body2} ->
		    case httpc_manager:next_request(Address,
						    Session#session.id) of
			no_more_requests -> % Should not happen!
			    exit_session(Address,Session,aborted_request);
			{error,Reason} -> % Should not happen!
			    exit_session(Address,Session,aborted_request);
			NewReq ->
			    handle_response({Status2,Headers2,Body2},
					    Timeout,NewReq,Session)
		    end
	    end;
	{error,Reason} -> % The connection has been closed by httpc_manager
	    exit_session(Address,Session,aborted_request);
	NewReq ->
	    NewReq
    end.

%% ===========================================================================
%% Internals

%%% Read in and parse response data from the socket
read(Timeout,SockType,Socket) ->
    Info=#response{scheme=SockType,socket=Socket},
    http_lib:setopts(SockType,Socket,[{packet, http}]),
    Info1=read_response(SockType,Socket,Info,Timeout),
    http_lib:setopts(SockType,Socket,[binary,{packet, raw}]),
    case (Info1#response.headers)#res_headers.content_type of
	"multipart/byteranges"++Param ->
	    range_response_body(Info1,Timeout,Param);
	_ ->
	    #response{status=Status2,headers=Headers2,body=Body2}=
		http_lib:read_client_body(Info1,Timeout),
	    {Status2,Headers2,Body2}
    end.


%%% From RFC 2616:
%%%      Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
%%%      HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT
%%%      Status-Code    = 3DIGIT
%%%      Reason-Phrase  = *<TEXT, excluding CR, LF>
read_response(SockType,Socket,Info,Timeout) ->
    case http_lib:recv0(SockType,Socket,Timeout) of
	{ok,{http_response,{1,VerMin}, Status, _Phrase}} when VerMin==0;
							      VerMin==1 ->
	    Info1=Info#response{status=Status,http_version=VerMin},
	    http_lib:read_client_headers(Info1,Timeout);
	{ok,{http_response,_Version, _Status, _Phrase}} ->
	    throw({error,bad_status_line});
	{error, timeout} ->
	    throw({error,session_local_timeout});
	{error, Reason} when Reason==closed;Reason==enotconn ->
	    throw({error,session_remotely_closed});
	{error, Reason} ->
	    throw({error,Reason})
    end.

%%% From RFC 2616, Section 4.4, Page 34
%% 4.If the message uses the media type "multipart/byteranges", and the
%%   transfer-length is not otherwise specified, then this self-
%%   delimiting media type defines the transfer-length. This media type
%%   MUST NOT be used unless the sender knows that the recipient can parse
%%   it; the presence in a request of a Range header with multiple byte-
%%   range specifiers from a 1.1 client implies that the client can parse
%%   multipart/byteranges responses.
%%% FIXME !!
range_response_body(Info,Timeout,Param) ->
    Headers=Info#response.headers,
    case {Headers#res_headers.content_length,
	  Headers#res_headers.transfer_encoding} of
	{undefined,undefined} ->
	    #response{status=Status2,headers=Headers2,body=Body2}=
		http_lib:read_client_multipartrange_body(Info,Param,Timeout),
	    {Status2,Headers2,Body2};
	_ ->
	    #response{status=Status2,headers=Headers2,body=Body2}=
		http_lib:read_client_body(Info,Timeout),
	    {Status2,Headers2,Body2}
    end.


%%% ----------------------------------------------------------------------------
%%% Host: field is required when addressing multi-homed sites ...
%%% It must not be present when the request is being made to a proxy.
http_request(#request{method=Method,id=Id,
		      scheme=Scheme,address={Host,Port},pathquery=PathQuery,
		      headers=Headers, content={ContentType,Body},
		      settings=Settings},
	     Socket) ->
    PostData=
	if
	    Method==post;Method==put ->
		case Headers#req_headers.expect of
		    "100-continue" ->
			content_type_header(ContentType) ++
			    content_length_header(length(Body)) ++
			    "\r\n";
		    _ ->
			content_type_header(ContentType) ++
			    content_length_header(length(Body)) ++
			    "\r\n" ++ Body
		end;
	    true ->
		"\r\n"
	end,
    Message=
	case useProxy(Settings#client_settings.useproxy,
		      {Scheme,Host,Port,PathQuery}) of
	    false ->
		method(Method)++" "++PathQuery++" HTTP/1.1\r\n"++
		    host_header(Host)++te_header()++
		    headers(Headers) ++ PostData;
	    AbsURI ->
		method(Method)++" "++AbsURI++" HTTP/1.1\r\n"++
		    te_header()++
		    headers(Headers)++PostData
	end,
    http_lib:send(Scheme,Socket,Message).

useProxy(false,_) ->
    false;
useProxy(true,{Scheme,Host,Port,PathQuery}) ->
    [atom_to_list(Scheme),"://",Host,":",integer_to_list(Port),PathQuery].



headers(#req_headers{expect=Expect,
		     other=Other}) ->
    H1=case Expect of
	   undefined ->[];
	   _ -> "Expect: "++Expect++"\r\n"
       end,
    H1++headers_other(Other).


headers_other([]) ->
    [];
headers_other([{Key,Value}|Rest]) when atom(Key) ->
    Head = atom_to_list(Key)++": "++Value++"\r\n",
    Head ++ headers_other(Rest);
headers_other([{Key,Value}|Rest]) ->
    Head = Key++": "++Value++"\r\n",
    Head ++ headers_other(Rest).

host_header(Host) ->
    "Host: "++lists:concat([Host])++"\r\n".
content_type_header(ContentType) ->
    "Content-Type: " ++ ContentType ++ "\r\n".
content_length_header(ContentLength) ->
    "Content-Length: "++integer_to_list(ContentLength) ++ "\r\n".
te_header() ->
    "TE: \r\n".

method(Method) ->
    httpd_util:to_upper(atom_to_list(Method)).


%%% ----------------------------------------------------------------------------
http_response({Status,Headers,Body},Req,Session) ->
    case Status of
	100 ->
	    status_continue(Req,Session);
	200 ->
	    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
					      {Status,Headers,Body}}),
	    ServerClose=http_lib:connection_close(Headers),
	    handle_connection(Session#session.clientclose,ServerClose,
			      Req,Session);
	300 -> status_multiple_choices(Headers,Body,Req,Session);
	301 -> status_moved_permanently(Req#request.method,
					Headers,Body,Req,Session);
	302 -> status_found(Headers,Body,Req,Session);
	303 -> status_see_other(Headers,Body,Req,Session);
	304 -> status_not_modified(Headers,Body,Req,Session);
	305 -> status_use_proxy(Headers,Body,Req,Session);
	%% 306 This Status code is not used in HTTP 1.1
	307 -> status_temporary_redirect(Headers,Body,Req,Session);
	503 -> status_service_unavailable({Status,Headers,Body},Req,Session);
	Status50x when Status50x==500;Status50x==501;Status50x==502;
		       Status50x==504;Status50x==505 ->
	    status_server_error_50x({Status,Headers,Body},Req,Session);
	_ -> % FIXME May want to take some action on other Status codes as well
	    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
					      {Status,Headers,Body}}),
	    ServerClose=http_lib:connection_close(Headers),
	    handle_connection(Session#session.clientclose,ServerClose,
			      Req,Session)
    end.


%%% Status code dependent functions.

%%% Received a 100 Status code ("Continue")
%%% From RFC2616
%%% The client SHOULD continue with its request. This interim response is
%%% used to inform the client that the initial part of the request has
%%% been received and has not yet been rejected by the server. The client
%%% SHOULD continue by sending the remainder of the request or, if the
%%% request has already been completed, ignore this response. The server
%%% MUST send a final response after the request has been completed. See
%%% section 8.2.3 for detailed discussion of the use and handling of this
%%% status code.
status_continue(Req,Session) ->
    {_,Body}=Req#request.content,
    http_lib:send(Session#session.scheme,Session#session.socket,Body),
    next_response_with_request(Req,Session).


%%% Received a 300 Status code ("Multiple Choices")
%%% The resource is located in any one of a set of locations
%%% - If a 'Location' header is present (preserved server choice), use that
%%%   to automatically redirect to the given URL
%%% - else if the Content-Type/Body both are non-empty let the user agent make
%%%   the choice and thus return a response with status 300
%%% Note:
%%% - If response to a HEAD request, the Content-Type/Body both should be empty.
%%% - The behaviour on an empty Content-Type or Body is unspecified.
%%%   However, e.g. "Apache/1.3" servers returns both empty if the header
%%%   'if-modified-since: Date' was sent in the request and the content is
%%%   "not modified" (instead of 304). Thus implicitly giving the cache as the
%%%   only choice.
status_multiple_choices(Headers,Body,Req,Session)
  when ((Req#request.settings)#client_settings.autoredirect)==true ->
    ServerClose=http_lib:connection_close(Headers),
    case Headers#res_headers.location of
	undefined ->
	    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
					      {300,Headers,Body}}),
	    handle_connection(Session#session.clientclose,ServerClose,
			      Req,Session);
	RedirUrl ->
	    Scheme=Session#session.scheme,
	    case uri:parse(RedirUrl) of
		{error,Reason} ->
		    {error,Reason};
		{Scheme,Host,Port,PathQuery} -> % Automatic redirection
		    NewReq=Req#request{redircount=Req#request.redircount+1,
				       address={Host,Port},pathquery=PathQuery},
		    handle_redirect(Session#session.clientclose,ServerClose,
				    NewReq,Session)
	    end
    end;
status_multiple_choices(Headers,Body,Req,Session) ->
    ServerClose=http_lib:connection_close(Headers),
    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
				      {300,Headers,Body}}),
    handle_connection(Session#session.clientclose,ServerClose,Req,Session).


%%% Received a 301 Status code ("Moved Permanently")
%%% The resource has been assigned a new permanent URI
%%% - If a 'Location' header is present, use that to automatically redirect to
%%%   the given URL if GET or HEAD request
%%% - else return
%%% Note:
%%% - The Body should contain a short hypertext note with a hyperlink to the
%%%   new URI. Return this if Content-Type acceptable (some HTTP servers doesn't
%%%   deal properly with Accept headers)
status_moved_permanently(Method,Headers,Body,Req,Session)
  when (((Req#request.settings)#client_settings.autoredirect)==true) and
       (Method==get) or (Method==head) ->
    ServerClose=http_lib:connection_close(Headers),
    case Headers#res_headers.location of
	undefined ->
	    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
					      {301,Headers,Body}}),
	    handle_connection(Session#session.clientclose,ServerClose,
			      Req,Session);
	RedirUrl ->
	    Scheme=Session#session.scheme,
	    case uri:parse(RedirUrl) of
		{error,Reason} ->
		    {error,Reason};
		{Scheme,Host,Port,PathQuery} -> % Automatic redirection
		    NewReq=Req#request{redircount=Req#request.redircount+1,
				       address={Host,Port},pathquery=PathQuery},
		    handle_redirect(Session#session.clientclose,ServerClose,
				    NewReq,Session)
	    end
    end;
status_moved_permanently(_Method,Headers,Body,Req,Session) ->
    ServerClose=http_lib:connection_close(Headers),
    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
				      {301,Headers,Body}}),
    handle_connection(Session#session.clientclose,ServerClose,Req,Session).


%%% Received a 302 Status code ("Found")
%%% The requested resource resides temporarily under a different URI.
%%% Note:
%%% - Only cacheable if indicated by a Cache-Control or Expires header
status_found(Headers,Body,Req,Session)
  when ((Req#request.settings)#client_settings.autoredirect)==true ->
    ServerClose=http_lib:connection_close(Headers),
    case Headers#res_headers.location of
	undefined ->
	    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
					      {302,Headers,Body}}),
	    handle_connection(Session#session.clientclose,ServerClose,
			      Req,Session);
	RedirUrl ->
	    Scheme=Session#session.scheme,
	    case uri:parse(RedirUrl) of
		{error,Reason} ->
		    {error,Reason};
		{Scheme,Host,Port,PathQuery} -> % Automatic redirection
		    NewReq=Req#request{redircount=Req#request.redircount+1,
				       address={Host,Port},pathquery=PathQuery},
		    handle_redirect(Session#session.clientclose,ServerClose,
				    NewReq,Session)
	    end
    end;
status_found(Headers,Body,Req,Session) ->
    ServerClose=http_lib:connection_close(Headers),
    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
				      {302,Headers,Body}}),
    handle_connection(Session#session.clientclose,ServerClose,Req,Session).

%%% Received a 303 Status code ("See Other")
%%% The request found under a different URI and should be retrieved using GET
%%% Note:
%%% - Must not be cached
status_see_other(Headers,Body,Req,Session)
  when ((Req#request.settings)#client_settings.autoredirect)==true ->
    ServerClose=http_lib:connection_close(Headers),
    case Headers#res_headers.location of
	undefined ->
	    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
					      {303,Headers,Body}}),
	    handle_connection(Session#session.clientclose,ServerClose,
			      Req,Session);
	RedirUrl ->
	    Scheme=Session#session.scheme,
	    case uri:parse(RedirUrl) of
		{error,Reason} ->
		    {error,Reason};
		{Scheme,Host,Port,PathQuery} -> % Automatic redirection
		    NewReq=Req#request{redircount=Req#request.redircount+1,
				       method=get,
				       address={Host,Port},pathquery=PathQuery},
		    handle_redirect(Session#session.clientclose,ServerClose,
				    NewReq,Session)
	    end
    end;
status_see_other(Headers,Body,Req,Session) ->
    ServerClose=http_lib:connection_close(Headers),
    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
				      {303,Headers,Body}}),
    handle_connection(Session#session.clientclose,ServerClose,Req,Session).


%%% Received a 304 Status code ("Not Modified")
%%% Note:
%%% - The response MUST NOT contain a body.
%%% - The response MUST include the following header fields:
%%%   - Date, unless its omission is required
%%%   - ETag and/or Content-Location, if the header would have been sent
%%%        in a 200 response to the same request
%%%   - Expires, Cache-Control, and/or Vary, if the field-value might
%%%        differ from that sent in any previous response for the same
%%%        variant
status_not_modified(Headers,Body,Req,Session)
  when ((Req#request.settings)#client_settings.autoredirect)==true ->
    ServerClose=http_lib:connection_close(Headers),
    case Headers#res_headers.location of
	undefined ->
	    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
					      {304,Headers,Body}}),
	    handle_connection(Session#session.clientclose,ServerClose,
			      Req,Session);
	RedirUrl ->
	    Scheme=Session#session.scheme,
	    case uri:parse(RedirUrl) of
		{error,Reason} ->
		    {error,Reason};
		{Scheme,Host,Port,PathQuery} -> % Automatic redirection
		    NewReq=Req#request{redircount=Req#request.redircount+1,
				       address={Host,Port},pathquery=PathQuery},
		    handle_redirect(Session#session.clientclose,ServerClose,
				    NewReq,Session)
	    end
    end;
status_not_modified(Headers,Body,Req,Session) ->
    ServerClose=http_lib:connection_close(Headers),
    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
				      {304,Headers,Body}}),
    handle_connection(Session#session.clientclose,ServerClose,Req,Session).



%%% Received a 305 Status code ("Use Proxy")
%%% The requested resource MUST be accessed through the proxy given by the
%%% Location field
status_use_proxy(Headers,Body,Req,Session)
  when ((Req#request.settings)#client_settings.autoredirect)==true ->
    ServerClose=http_lib:connection_close(Headers),
    case Headers#res_headers.location of
	undefined ->
	    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
					      {305,Headers,Body}}),
	    handle_connection(Session#session.clientclose,ServerClose,
			      Req,Session);
	RedirUrl ->
	    Scheme=Session#session.scheme,
	    case uri:parse(RedirUrl) of
		{error,Reason} ->
		    {error,Reason};
		{Scheme,Host,Port,PathQuery} -> % Automatic redirection
		    NewReq=Req#request{redircount=Req#request.redircount+1,
				       address={Host,Port},pathquery=PathQuery},
		    handle_redirect(Session#session.clientclose,ServerClose,
				    NewReq,Session)
	    end
    end;
status_use_proxy(Headers,Body,Req,Session) ->
    ServerClose=http_lib:connection_close(Headers),
    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
				      {305,Headers,Body}}),
    handle_connection(Session#session.clientclose,ServerClose,Req,Session).


%%% Received a 307 Status code ("Temporary Redirect")
status_temporary_redirect(Headers,Body,Req,Session)
  when ((Req#request.settings)#client_settings.autoredirect)==true ->
    ServerClose=http_lib:connection_close(Headers),
    case Headers#res_headers.location of
	undefined ->
	    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
					      {307,Headers,Body}}),
	    handle_connection(Session#session.clientclose,ServerClose,
			      Req,Session);
	RedirUrl ->
	    Scheme=Session#session.scheme,
	    case uri:parse(RedirUrl) of
		{error,Reason} ->
		    {error,Reason};
		{Scheme,Host,Port,PathQuery} -> % Automatic redirection
		    NewReq=Req#request{redircount=Req#request.redircount+1,
				       address={Host,Port},pathquery=PathQuery},
		    handle_redirect(Session#session.clientclose,ServerClose,
				    NewReq,Session)
	    end
    end;
status_temporary_redirect(Headers,Body,Req,Session) ->
    ServerClose=http_lib:connection_close(Headers),
    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
				      {307,Headers,Body}}),
    handle_connection(Session#session.clientclose,ServerClose,Req,Session).



%%% Received a 503 Status code ("Service Unavailable")
%%%    The server is currently unable to handle the request due to a
%%%    temporary overloading or maintenance of the server. The implication
%%%    is that this is a temporary condition which will be alleviated after
%%%    some delay. If known, the length of the delay MAY be indicated in a
%%%    Retry-After header. If no Retry-After is given, the client SHOULD
%%%    handle the response as it would for a 500 response.
%% Note:
%% - This session is now considered busy, thus cancel any requests in the
%%   pipeline and close the session.
%% FIXME! Implement a user option to automatically retry if the 'Retry-After'
%%        header is given.
status_service_unavailable(Resp,Req,Session) ->
%    RetryAfter=Headers#res_headers.retry_after,
    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,Resp}),
    close_session(server_connection_close,Req,Session).


%%% Received a 50x Status code (~ "Service Error")
%%%   Response status codes beginning with the digit "5" indicate cases in
%%%   which the server is aware that it has erred or is incapable of
%%%   performing the request.
status_server_error_50x(Resp,Req,Session) ->
    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,Resp}),
    close_session(server_connection_close,Req,Session).


%%% Handles requests for redirects
%%% The redirected request might be:
%%% - FIXME! on another TCP session, another scheme
%%% - on the same TCP session, same scheme
%%% - on another TCP session , same scheme
%%% However, in all cases treat it as a new request, with redircount updated.
%%%
%%% The redirect may fail, but this not a reason to close this session.
%%% Instead return a error for this request, and continue as ok.
handle_redirect(ClientClose,ServerClose,Req,Session) ->
    case httpc_manager:request(Req) of
	{ok,_ReqId} -> % FIXME Should I perhaps reuse the Reqid?
	    handle_connection(ClientClose,ServerClose,Req,Session);
	{error,Reason} ->
	    gen_server:cast(Req#request.from,{Req#request.ref,Req#request.id,
					      {error,Reason}}),
	    handle_connection(ClientClose,ServerClose,Req,Session)
    end.

%%% Check if the persistent connection flag is false (ie client request
%%% non-persistive connection), or if the server requires a closed connection
%%% (by sending a "Connection: close" header). If the connection required
%%% non-persistent, we may close the connection immediately.
handle_connection(ClientClose,ServerClose,Req,Session) ->
    case {ClientClose,ServerClose} of
	{false,false} ->
	    ok;
	{false,true} -> % The server requests this session to be closed.
	    close_session(server_connection_close,Req,Session);
	{true,_} -> % The client requested a non-persistent connection
	    close_session(client_connection_close,Req,Session)
    end.


%%% Close the session.
%%% We now have three cases:
%%% - Client request a non-persistent connection when initiating the request.
%%%   Session info not stored in httpc_manager
%%% - Server requests a non-persistent connection when answering a request.
%%%   No need to resend request, but there might be a pipeline.
%%% - Some kind of error
%%%   Close the session, we may then try resending all requests in the pipeline
%%%   including the current depending on the error.
%%% FIXME! Should not always abort the session (see close_session in
%%%     httpc_manager for more details)
close_session(client_connection_close,_Req,Session) ->
    http_lib:close(Session#session.scheme,Session#session.socket),
    stop;
close_session(server_connection_close,Req,Session) ->
    http_lib:close(Session#session.scheme,Session#session.socket),
    httpc_manager:abort_session(Req#request.address,Session#session.id,
			       aborted_request),
    stop.

exit_session(Address,Session,Reason) ->
    http_lib:close(Session#session.scheme,Session#session.socket),
    httpc_manager:abort_session(Address,Session#session.id,Reason),
    exit(normal).

%%% This is the "normal" case to close a persistent connection. I.e., there are
%%% no more requests waiting and the session was closed by the client, or
%%% server because of a timeout or user request.
exit_session_ok(Address,Session) ->
    http_lib:close(Session#session.scheme,Session#session.socket),
    exit_session_ok2(Address,Session#session.clientclose,Session#session.id).

exit_session_ok2(Address,ClientClose,Sid) ->
    case ClientClose of
	false ->
	    httpc_manager:close_session(Address,Sid);
	true ->
	    ok
    end,
    exit(normal).

%%% ============================================================================
%%% This is deprecated code, to be removed

format_time() ->
    {_,_,MicroSecs}=TS=now(),
    {{Y,Mon,D},{H,M,S}}=calendar:now_to_universal_time(TS),
    lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w,~2.2.0w:~2.2.0w:~6.3.0f",
				[Y,Mon,D,H,M,S+(MicroSecs/1000000)])).

%%% Read more data from the open socket.
%%% Two different read functions is used because for the {active, once} socket
%%% option is (currently) not available for SSL...
%%% FIXME
% read_more_data(http,Socket,Timeout) ->
%     io:format("read_more_data(ip_comm) -> "
% 	"~n   set active = 'once' and "
% 	"await a chunk data", []),
%     http_lib:setopts(Socket, [{active,once}]),
%     read_more_data_ipcomm(Socket,Timeout);
% read_more_data(https,Socket,Timeout) ->
%     case ssl:recv(Socket,0,Timeout) of
% 	{ok,MoreData} ->
% 	    MoreData;
% 	{error,closed} ->
% 	    throw({error, session_remotely_closed});
% 	{error,etimedout} ->
% 	    throw({error, session_local_timeout});
% 	{error,Reason} ->
% 	    throw({error, Reason});
% 	Other ->
% 	    throw({error, Other})
%     end.

% %%% Send any incoming requests on the open session immediately
% read_more_data_ipcomm(Socket,Timeout) ->
%     receive
% 	{tcp,Socket,MoreData} ->
% %	    ?vtrace("read_more_data(ip_comm) -> got some data:~p",
% %		[MoreData]),
% 	    MoreData;
% 	{tcp_closed,Socket} ->
% %	    ?vtrace("read_more_data(ip_comm) -> socket closed",[]),
% 	    throw({error,session_remotely_closed});
% 	{tcp_error,Socket,Reason} ->
% %	    ?vtrace("read_more_data(ip_comm) -> ~p socket error: ~p",
% %		[self(),Reason]),
% 	    throw({error, Reason});
% 	stop ->
% 	    throw({error, user_req})
%     after Timeout ->
% 	    throw({error, session_local_timeout})
%     end.

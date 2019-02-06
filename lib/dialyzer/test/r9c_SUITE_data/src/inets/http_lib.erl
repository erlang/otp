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
%%% File    : http_lib.erl
%%% Author  : Johan Blom <johan.blom@mobilearts.se>
%%% Description : Generic, HTTP specific helper functions
%%% Created :  4 Mar 2002 by Johan Blom

%%% TODO
%%% - Check if I need to anything special when parsing
%%%   "Content-Type:multipart/form-data"

-module(http_lib).
-author("johan.blom@mobilearts.se").

-include("http.hrl").
-include("jnets_httpd.hrl").

-export([connection_close/1,
	 accept/3,deliver/3,recv/4,recv0/3,
	 connect/1,send/3,close/2,controlling_process/3,setopts/3,
	 getParameterValue/2,
%	 get_var/2,
	 create_request_line/3]).

-export([read_client_headers/2,read_server_headers/2,
	 get_auth_data/1,create_header_list/1,
	 read_client_body/2,read_client_multipartrange_body/3,
	 read_server_body/2]).


%%% Server response:
%%%    Check "Connection" header if server requests session to be closed.
%%%    No 'close' means returns false
%%% Client Request:
%%%    Check if 'close' in request headers
%%% Only care about HTTP 1.1 clients!
connection_close(Headers) when record(Headers,req_headers) ->
    case Headers#req_headers.connection of
	"close" ->
	    true;
	"keep-alive" ->
	    false;
	Value when list(Value) ->
	    true;
	_ ->
	    false
    end;
connection_close(Headers) when record(Headers,res_headers) ->
    case Headers#res_headers.connection of
	"close" ->
	    true;
	"keep-alive" ->
	    false;
	Value when list(Value) ->
	    true;
	_ ->
	    false
    end.


%% =============================================================================
%%% Debugging:

% format_time(TS) ->
%     {_,_,MicroSecs}=TS,
%     {{Y,Mon,D},{H,M,S}}=calendar:now_to_universal_time(TS),
%     lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w,~2.2.0w:~2.2.0w:~6.3.0f",
% 				[Y,Mon,D,H,M,S+(MicroSecs/1000000)])).

%% Time in milli seconds
% t() ->
%     {A,B,C} = erlang:now(),
%     A*1000000000+B*1000+(C div 1000).

% sz(L) when list(L) ->
%     length(L);
% sz(B) when binary(B) ->
%     size(B);
% sz(O) ->
%     {unknown_size,O}.


%% =============================================================================

getHeaderValue(_Attr,[]) ->
    [];
getHeaderValue(Attr,[{Attr,Value}|_Rest]) ->
    Value;
getHeaderValue(Attr,[_|Rest]) ->
    getHeaderValue(Attr,Rest).

getParameterValue(_Attr,undefined) ->
    undefined;
getParameterValue(Attr,List) ->
    case lists:keysearch(Attr,1,List) of
	{value,{Attr,Val}} ->
	    Val;
	_ ->
	    undefined
    end.

create_request_line(Method,Path,{Major,Minor}) ->
    [atom_to_list(Method)," ",Path,
     " HTTP/",integer_to_list(Major),".",integer_to_list(Minor)];
create_request_line(Method,Path,Minor) ->
    [atom_to_list(Method)," ",Path," HTTP/1.",integer_to_list(Minor)].


%%% ============================================================================
read_client_headers(Info,Timeout) ->
    Headers=read_response_h(Info#response.scheme,Info#response.socket,Timeout,
			    Info#response.headers),
    Info#response{headers=Headers}.

read_server_headers(Info,Timeout) ->
    Headers=read_request_h(Info#mod.socket_type,Info#mod.socket,Timeout,
			   Info#mod.headers),
    Info#mod{headers=Headers}.


%% Parses the header of a HTTP request and returns a key,value tuple
%% list containing Name and Value of each header directive as of:
%%
%% Content-Type: multipart/mixed -> {"Content-Type", "multipart/mixed"}
%%
%% But in http/1.1 the field-names are case insencitive so now it must be
%% Content-Type: multipart/mixed -> {"content-type", "multipart/mixed"}
%% The standard furthermore says that leading and traling white space
%% is not a part of the fieldvalue and shall therefore be removed.
read_request_h(SType,S,Timeout,H) ->
    case recv0(SType,S,Timeout) of
	{ok,{http_header,_,'Connection',_,Value}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{connection=Value});
	{ok,{http_header,_,'Content-Type',_,Val}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{content_type=Val});
	{ok,{http_header,_,'Host',_,Value}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{host=Value});
	{ok,{http_header,_,'Content-Length',_,Value}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{content_length=Value});
%	{ok,{http_header,_,'Expect',_,Value}} -> % FIXME! Update inet_drv.c!!
%	    read_request_h(SType,S,Timeout,H#req_headers{expect=Value});
	{ok,{http_header,_,'Transfer-Encoding',_,V}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{transfer_encoding=V});
	{ok,{http_header,_,'Authorization',_,Value}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{authorization=Value});
	{ok,{http_header,_,'User-Agent',_,Value}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{user_agent=Value});
	{ok,{http_header,_,'Range',_,Value}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{range=Value});
	{ok,{http_header,_,'If-Range',_,Value}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{if_range=Value});
	{ok,{http_header,_,'If-Match',_,Value}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{if_match=Value});
	{ok,{http_header,_,'If-None-Match',_,Value}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{if_none_match=Value});
	{ok,{http_header,_,'If-Modified-Since',_,V}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{if_modified_since=V});
	{ok,{http_header,_,'If-Unmodified-Since',_,V}} ->
	    read_request_h(SType,S,Timeout,H#req_headers{if_unmodified_since=V});
	{ok,{http_header,_,K,_,V}} ->
	    read_request_h(SType,S,Timeout,
			   H#req_headers{other=H#req_headers.other++[{K,V}]});
	{ok,http_eoh} ->
	    H;
	{error, timeout} when SType==http ->
	    throw({error, session_local_timeout});
	{error, etimedout} when SType==https ->
	    throw({error, session_local_timeout});
	{error, Reason} when Reason==closed;Reason==enotconn ->
	    throw({error, session_remotely_closed});
	{error, Reason} ->
	    throw({error,Reason})
    end.


read_response_h(SType,S,Timeout,H) ->
    case recv0(SType,S,Timeout) of
	{ok,{http_header,_,'Connection',_,Val}} ->
	    read_response_h(SType,S,Timeout,H#res_headers{connection=Val});
	{ok,{http_header,_,'Content-Length',_,Val}} ->
	    read_response_h(SType,S,Timeout,H#res_headers{content_length=Val});
	{ok,{http_header,_,'Content-Type',_,Val}} ->
	    read_response_h(SType,S,Timeout,H#res_headers{content_type=Val});
	{ok,{http_header,_,'Transfer-Encoding',_,V}} ->
	    read_response_h(SType,S,Timeout,H#res_headers{transfer_encoding=V});
	{ok,{http_header,_,'Location',_,V}} ->
	    read_response_h(SType,S,Timeout,H#res_headers{location=V});
	{ok,{http_header,_,'Retry-After',_,V}} ->
	    read_response_h(SType,S,Timeout,H#res_headers{retry_after=V});
	{ok,{http_header,_,K,_,V}} ->
	    read_response_h(SType,S,Timeout,
			    H#res_headers{other=H#res_headers.other++[{K,V}]});
	{ok,http_eoh} ->
	    H;
	{error, timeout} when SType==http ->
	    throw({error, session_local_timeout});
	{error, etimedout} when SType==https ->
	    throw({error, session_local_timeout});
	{error, Reason} when Reason==closed;Reason==enotconn ->
	    throw({error, session_remotely_closed});
	{error, Reason} ->
	    throw({error,Reason})
    end.


%%% Got the headers, and maybe a part of the body, now read in the rest
%%% Note:
%%% - No need to check for Expect header if client
%%% - Currently no support for setting MaxHeaderSize in client, set to
%%%   unlimited.
%%% - Move to raw packet mode as we are finished with HTTP parsing
read_client_body(Info,Timeout) ->
    Headers=Info#response.headers,
    case Headers#res_headers.transfer_encoding of
	"chunked" ->
	    ?DEBUG("read_entity_body2()->"
		"Transfer-encoding:Chunked Data:",[]),
	    read_client_chunked_body(Info,Timeout,?MAXBODYSIZE);
	Encoding when list(Encoding) ->
	    ?DEBUG("read_entity_body2()->"
		"Transfer-encoding:Unknown",[]),
	    throw({error,unknown_coding});
	_ ->
	    ContLen=list_to_integer(Headers#res_headers.content_length),
	    if
		ContLen>?MAXBODYSIZE ->
		    throw({error,body_too_big});
		true ->
		    ?DEBUG("read_entity_body2()->"
			"Transfer-encoding:none ",[]),
		    Info#response{body=read_plain_body(Info#response.scheme,
						       Info#response.socket,
						       ContLen,
						       Info#response.body,
						       Timeout)}
	    end
    end.


%%% ----------------------------------------------------------------------
read_server_body(Info,Timeout) ->
    MaxBodySz=httpd_util:lookup(Info#mod.config_db,max_body_size,?MAXBODYSIZE),
    ContLen=list_to_integer((Info#mod.headers)#req_headers.content_length),
    %% ?vtrace("ContentLength: ~p", [ContLen]),
    if
	integer(ContLen),integer(MaxBodySz),ContLen>MaxBodySz ->
	    throw({error,body_too_big});
	true ->
	    read_server_body2(Info,Timeout,ContLen,MaxBodySz)
    end.


%%----------------------------------------------------------------------
%% Control if the body is transfer encoded, if so decode it.
%% Note:
%% - MaxBodySz has an integer value or 'nolimit'
%% - ContLen has an integer value or 'undefined'
%% All applications MUST be able to receive and decode the "chunked"
%% transfer-coding, see RFC 2616 Section 3.6.1
read_server_body2(Info,Timeout,ContLen,MaxBodySz) ->
    ?DEBUG("read_entity_body2()->Max: ~p ~nLength:~p ~nSocket: ~p ~n",
	[MaxBodySz,ContLen,Info#mod.socket]),
    case (Info#mod.headers)#req_headers.transfer_encoding of
	"chunked" ->
	    ?DEBUG("read_entity_body2()->"
		"Transfer-encoding:Chunked Data:",[]),
	    read_server_chunked_body(Info,Timeout,MaxBodySz);
	Encoding when list(Encoding) ->
	    ?DEBUG("read_entity_body2()->"
		"Transfer-encoding:Unknown",[]),
	    httpd_response:send_status(Info,501,"Unknown Transfer-Encoding"),
	    http_lib:close(Info#mod.socket_type,Info#mod.socket),
	    throw({error,{status_sent,"Unknown Transfer-Encoding "++Encoding}});
	_ when integer(ContLen),integer(MaxBodySz),ContLen>MaxBodySz ->
	    throw({error,body_too_big});
	_ when integer(ContLen) ->
	    ?DEBUG("read_entity_body2()->"
		"Transfer-encoding:none ",[]),
	    Info#mod{entity_body=read_plain_body(Info#mod.socket_type,
						 Info#mod.socket,
						 ContLen,Info#mod.entity_body,
						 Timeout)}
    end.


%%% ----------------------------------------------------------------------------
%%% The body was plain, just read it from the socket.
read_plain_body(_SocketType,Socket,0,Cont,_Timeout) ->
    Cont;
read_plain_body(SocketType,Socket,ContLen,Cont,Timeout) ->
    Body=read_more_data(SocketType,Socket,ContLen,Timeout),
    <<Cont/binary,Body/binary>>.

%%% ----------------------------------------------------------------------------
%%% The body was chunked, decode it.
%%% From RFC2616, Section 3.6.1
%%        Chunked-Body   = *chunk
%%                         last-chunk
%%                         trailer
%%                         CRLF
%%
%%        chunk          = chunk-size [ chunk-extension ] CRLF
%%                         chunk-data CRLF
%%        chunk-size     = 1*HEX
%%        last-chunk     = 1*("0") [ chunk-extension ] CRLF
%%
%%        chunk-extension= *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
%%        chunk-ext-name = token
%%        chunk-ext-val  = token | quoted-string
%%        chunk-data     = chunk-size(OCTET)
%%        trailer        = *(entity-header CRLF)
%%
%%% "All applications MUST ignore chunk-extension extensions they do not
%%% understand.", see RFC 2616 Section 3.6.1
%%% We don't understand any extension...
read_client_chunked_body(Info,Timeout,MaxChunkSz) ->
    case read_chunk(Info#response.scheme,Info#response.socket,
		    Timeout,0,MaxChunkSz) of
	{last_chunk,_ExtensionList} -> % Ignore extension
	    TrailH=read_headers_old(Info#response.scheme,Info#response.socket,
				    Timeout),
	    H=Info#response.headers,
	    OtherHeaders=H#res_headers.other++TrailH,
	    Info#response{headers=H#res_headers{other=OtherHeaders}};
	{Chunk,ChunkSize,_ExtensionList} -> % Ignore extension
	    Info1=Info#response{body= <<(Info#response.body)/binary,
				        Chunk/binary>>},
	    read_client_chunked_body(Info1,Timeout,MaxChunkSz-ChunkSize);
	{error,Reason} ->
	    throw({error,Reason})
    end.


read_server_chunked_body(Info,Timeout,MaxChunkSz) ->
    case read_chunk(Info#mod.socket_type,Info#mod.socket,
		    Timeout,0,MaxChunkSz) of
	{last_chunk,_ExtensionList} -> % Ignore extension
	    TrailH=read_headers_old(Info#mod.socket_type,Info#mod.socket,
				    Timeout),
	    H=Info#mod.headers,
	    OtherHeaders=H#req_headers.other++TrailH,
	    Info#mod{headers=H#req_headers{other=OtherHeaders}};
	{Chunk,ChunkSize,_ExtensionList} -> % Ignore extension
	    Info1=Info#mod{entity_body= <<(Info#mod.entity_body)/binary,
					   Chunk/binary>>},
	    read_server_chunked_body(Info1,Timeout,MaxChunkSz-ChunkSize);
	{error,Reason} ->
	    throw({error,Reason})
    end.


read_chunk(Scheme,Socket,Timeout,Int,MaxChunkSz) when MaxChunkSz>Int ->
    case read_more_data(Scheme,Socket,1,Timeout) of
	<<C>> when $0=<C,C=<$9 ->
	    read_chunk(Scheme,Socket,Timeout,16*Int+(C-$0),MaxChunkSz);
	<<C>> when $a=<C,C=<$f ->
	    read_chunk(Scheme,Socket,Timeout,16*Int+10+(C-$a),MaxChunkSz);
	<<C>> when $A=<C,C=<$F ->
	    read_chunk(Scheme,Socket,Timeout,16*Int+10+(C-$A),MaxChunkSz);
	<<$;>> when Int>0 ->
	    ExtensionList=read_chunk_ext_name(Scheme,Socket,Timeout,[],[]),
	    read_chunk_data(Scheme,Socket,Int+1,ExtensionList,Timeout);
	<<$;>> when Int==0 ->
	    ExtensionList=read_chunk_ext_name(Scheme,Socket,Timeout,[],[]),
	    read_data_lf(Scheme,Socket,Timeout),
	    {last_chunk,ExtensionList};
	<<?CR>> when Int>0 ->
	    read_chunk_data(Scheme,Socket,Int+1,[],Timeout);
	<<?CR>> when Int==0 ->
	    read_data_lf(Scheme,Socket,Timeout),
	    {last_chunk,[]};
	<<C>> when C==$ -> % Some servers (e.g., Apache 1.3.6) throw in
			   % additional whitespace...
	    read_chunk(Scheme,Socket,Timeout,Int,MaxChunkSz);
	_Other ->
	    {error,unexpected_chunkdata}
    end;
read_chunk(_Scheme,_Socket,_Timeout,_Int,_MaxChunkSz) ->
    {error,body_too_big}.


%%% Note:
%%% - Got the initial ?CR already!
%%% - Bitsyntax does not allow matching of ?CR,?LF in the end of the first read
read_chunk_data(Scheme,Socket,Int,ExtensionList,Timeout) ->
    case read_more_data(Scheme,Socket,Int,Timeout) of
	<<?LF,Chunk/binary>> ->
	    case read_more_data(Scheme,Socket,2,Timeout) of
		<<?CR,?LF>> ->
		    {Chunk,size(Chunk),ExtensionList};
		_ ->
		    {error,bad_chunkdata}
	    end;
	_ ->
	    {error,bad_chunkdata}
    end.

read_chunk_ext_name(Scheme,Socket,Timeout,Name,Acc) ->
    Len=length(Name),
    case read_more_data(Scheme,Socket,1,Timeout) of
	$= when Len>0 ->
	    read_chunk_ext_val(Scheme,Socket,Timeout,Name,[],Acc);
	$; when Len>0 ->
	    read_chunk_ext_name(Scheme,Socket,Timeout,[],
				[{lists:reverse(Name),""}|Acc]);
	?CR when Len>0 ->
	    lists:reverse([{lists:reverse(Name,"")}|Acc]);
	Token -> % FIXME Check that it is "token"
	    read_chunk_ext_name(Scheme,Socket,Timeout,[Token|Name],Acc);
	_ ->
	    {error,bad_chunk_extension_name}
    end.

read_chunk_ext_val(Scheme,Socket,Timeout,Name,Val,Acc) ->
    Len=length(Val),
    case read_more_data(Scheme,Socket,1,Timeout) of
	$; when Len>0 ->
	    read_chunk_ext_name(Scheme,Socket,Timeout,[],
				[{Name,lists:reverse(Val)}|Acc]);
	?CR when Len>0 ->
	    lists:reverse([{Name,lists:reverse(Val)}|Acc]);
	Token -> % FIXME Check that it is "token" or "quoted-string"
	    read_chunk_ext_val(Scheme,Socket,Timeout,Name,[Token|Val],Acc);
	_ ->
	    {error,bad_chunk_extension_value}
    end.

read_data_lf(Scheme,Socket,Timeout) ->
    case read_more_data(Scheme,Socket,1,Timeout) of
	?LF ->
	    ok;
	_ ->
	    {error,bad_chunkdata}
    end.

%%% ----------------------------------------------------------------------------
%%% The body was "multipart/byteranges", decode it.
%%% Example from RFC 2616, Appendix 19.2
%%%    HTTP/1.1 206 Partial Content
%%%    Date: Wed, 15 Nov 1995 06:25:24 GMT
%%%    Last-Modified: Wed, 15 Nov 1995 04:58:08 GMT
%%%    Content-type: multipart/byteranges; boundary=THIS_STRING_SEPARATES
%%%
%%%    --THIS_STRING_SEPARATES
%%%    Content-type: application/pdf
%%%    Content-range: bytes 500-999/8000
%%%
%%%    ...the first range...
%%%    --THIS_STRING_SEPARATES
%%%    Content-type: application/pdf
%%%    Content-range: bytes 7000-7999/8000
%%%
%%%    ...the second range
%%%    --THIS_STRING_SEPARATES--
%%%
%%%       Notes:
%%%
%%%       1) Additional CRLFs may precede the first boundary string in the
%%%          entity.
%%% FIXME!!
read_client_multipartrange_body(Info,Parstr,Timeout) ->
    Boundary=get_boundary(Parstr),
    scan_boundary(Info,Boundary),
    Info#response{body=read_multipart_body(Info,Boundary,Timeout)}.

read_multipart_body(Info,Boundary,Timeout) ->
    Info.

%     Headers=read_headers_old(Info#response.scheme,Info#response.socket,Timeout),
%     H=Info#response.headers,
%     OtherHeaders=H#res_headers.other++TrailH,
%     Info#response{headers=H#res_headers{other=OtherHeaders}}.


scan_boundary(Info,Boundary) ->
    Info.


get_boundary(Parstr) ->
    case skip_lwsp(Parstr) of
	[] ->
	    throw({error,missing_range_boundary_parameter});
	Val ->
	    get_boundary2(string:tokens(Val, ";"))
    end.

get_boundary2([]) ->
    undefined;
get_boundary2([Param|Rest]) ->
    case string:tokens(skip_lwsp(Param), "=") of
	["boundary"++Attribute,Value] ->
	    Value;
	_ ->
	    get_boundary2(Rest)
    end.


%% skip space & tab
skip_lwsp([$ | Cs]) -> skip_lwsp(Cs);
skip_lwsp([$\t | Cs]) -> skip_lwsp(Cs);
skip_lwsp(Cs) -> Cs.

%%% ----------------------------------------------------------------------------

%%% Read the incoming data from the open socket.
read_more_data(http,Socket,Len,Timeout) ->
    case gen_tcp:recv(Socket,Len,Timeout) of
	{ok,Val} ->
	    Val;
	{error, timeout} ->
	    throw({error, session_local_timeout});
	{error, Reason} when Reason==closed;Reason==enotconn ->
	    throw({error, session_remotely_closed});
	{error, Reason} ->
%	    httpd_response:send_status(Info,400,none),
	    throw({error, Reason})
    end;
read_more_data(https,Socket,Len,Timeout) ->
    case ssl:recv(Socket,Len,Timeout) of
	{ok,Val} ->
	    Val;
	{error, etimedout} ->
	    throw({error, session_local_timeout});
	{error, Reason} when Reason==closed;Reason==enotconn ->
	    throw({error, session_remotely_closed});
	{error, Reason} ->
%	    httpd_response:send_status(Info,400,none),
	    throw({error, Reason})
    end.


%% =============================================================================
%%% Socket handling

accept(http,ListenSocket, Timeout) ->
    gen_tcp:accept(ListenSocket, Timeout);
accept(https,ListenSocket, Timeout) ->
    ssl:accept(ListenSocket, Timeout).


close(http,Socket) ->
    gen_tcp:close(Socket);
close(https,Socket) ->
    ssl:close(Socket).


connect(#request{scheme=http,settings=Settings,address=Addr}) ->
    case proxyusage(Addr,Settings) of
	{error,Reason} ->
	    {error,Reason};
	{Host,Port} ->
	    Opts=[binary,{active,false},{reuseaddr,true}],
	    gen_tcp:connect(Host,Port,Opts)
    end;
connect(#request{scheme=https,settings=Settings,address=Addr}) ->
    case proxyusage(Addr,Settings) of
	{error,Reason} ->
	    {error,Reason};
	{Host,Port} ->
	    Opts=case Settings#client_settings.ssl of
		     false ->
			 [binary,{active,false}];
		     SSLSettings ->
			 [binary,{active,false}]++SSLSettings
		 end,
	    ssl:connect(Host,Port,Opts)
    end.


%%% Check to see if the given {Host,Port} tuple is in the NoProxyList
%%% Returns an eventually updated {Host,Port} tuple, with the proxy address
proxyusage(HostPort,Settings) ->
    case Settings#client_settings.useproxy of
	true ->
	    case noProxy(HostPort,Settings#client_settings.noproxylist) of
		true ->
		    HostPort;
		_ ->
		    case Settings#client_settings.proxy of
			undefined ->
			    {error,no_proxy_defined};
			ProxyHostPort ->
			    ProxyHostPort
		    end
	    end;
	_ ->
	    HostPort
    end.

noProxy(_HostPort,[]) ->
    false;
noProxy({Host,Port},[{Host,Port}|Rest]) ->
    true;
noProxy(HostPort,[_|Rest]) ->
    noProxy(HostPort,Rest).


controlling_process(http,Socket,Pid) ->
    gen_tcp:controlling_process(Socket,Pid);
controlling_process(https,Socket,Pid) ->
    ssl:controlling_process(Socket,Pid).


deliver(SocketType, Socket, Message)  ->
    case send(SocketType, Socket, Message) of
	{error, einval} ->
	    close(SocketType, Socket),
	    socket_closed;
	{error, _Reason} ->
%	    ?vlog("deliver(~p) failed for reason:"
%		  "~n   Reason: ~p",[SocketType,_Reason]),
	    close(SocketType, Socket),
	    socket_closed;
	_Other ->
	    ok
    end.


recv0(http,Socket,Timeout) ->
    gen_tcp:recv(Socket,0,Timeout);
recv0(https,Socket,Timeout) ->
    ssl:recv(Socket,0,Timeout).

recv(http,Socket,Len,Timeout) ->
    gen_tcp:recv(Socket,Len,Timeout);
recv(https,Socket,Len,Timeout) ->
    ssl:recv(Socket,Len,Timeout).


setopts(http,Socket,Options) ->
    inet:setopts(Socket,Options);
setopts(https,Socket,Options) ->
    ssl:setopts(Socket,Options).


send(http,Socket,Message) ->
    gen_tcp:send(Socket,Message);
send(https,Socket,Message) ->
    ssl:send(Socket,Message).


%%% ============================================================================
%%% HTTP Server only

%%% Returns the Authenticating data in the HTTP request
get_auth_data("Basic "++EncodedString) ->
    UnCodedString=httpd_util:decode_base64(EncodedString),
    case catch string:tokens(UnCodedString,":") of
	[User,PassWord] ->
	    {User,PassWord};
	{error,Error}->
	    {error,Error}
    end;
get_auth_data(BadCredentials) when list(BadCredentials) ->
    {error,BadCredentials};
get_auth_data(_) ->
    {error,nouser}.


create_header_list(H) ->
    lookup(connection,H#req_headers.connection)++
	lookup(host,H#req_headers.host)++
	lookup(content_length,H#req_headers.content_length)++
	lookup(transfer_encoding,H#req_headers.transfer_encoding)++
	lookup(authorization,H#req_headers.authorization)++
	lookup(user_agent,H#req_headers.user_agent)++
	lookup(user_agent,H#req_headers.range)++
	lookup(user_agent,H#req_headers.if_range)++
	lookup(user_agent,H#req_headers.if_match)++
	lookup(user_agent,H#req_headers.if_none_match)++
	lookup(user_agent,H#req_headers.if_modified_since)++
	lookup(user_agent,H#req_headers.if_unmodified_since)++
	H#req_headers.other.

lookup(_Key,undefined) ->
    [];
lookup(Key,Val) ->
    [{Key,Val}].



%%% ============================================================================
%%% This code is for parsing trailer headers in chunked messages.
%%% Will be deprecated whenever I have found an alternative working solution!
%%% Note:
%%% - The header names are returned slightly different from what the what
%%%   inet_drv returns
read_headers_old(Scheme,Socket,Timeout) ->
    read_headers_old(<<>>,Scheme,Socket,Timeout,[],[]).

read_headers_old(<<>>,Scheme,Socket,Timeout,Acc,AccHdrs) ->
    read_headers_old(read_more_data(Scheme,Socket,1,Timeout),
		     Scheme,Socket,Timeout,Acc,AccHdrs);
read_headers_old(<<$\r>>,Scheme,Socket,Timeout,Acc,AccHdrs) ->
    read_headers_old(<<$\r,(read_more_data(Scheme,Socket,1,Timeout))/binary>>,
		     Scheme,Socket,Timeout,Acc,AccHdrs);
read_headers_old(<<$\r,$\n>>,Scheme,Socket,Timeout,Acc,AccHdrs) ->
    if
	Acc==[] -> % Done!
            tagup_header(lists:reverse(AccHdrs));
        true ->
            read_headers_old(read_more_data(Scheme,Socket,1,Timeout),
			     Scheme,Socket,
			     Timeout,[],[lists:reverse(Acc)|AccHdrs])
    end;
read_headers_old(<<C>>,Scheme,Socket,Timeout,Acc,AccHdrs) ->
    read_headers_old(read_more_data(Scheme,Socket,1,Timeout),
		     Scheme,Socket,Timeout,[C|Acc],AccHdrs);
read_headers_old(Bin,_Scheme,_Socket,_Timeout,_Acc,_AccHdrs) ->
    io:format("ERROR: Unexpected data from inet driver: ~p",[Bin]),
    throw({error,this_is_a_bug}).


%% Parses the header of a HTTP request and returns a key,value tuple
%% list containing Name and Value of each header directive as of:
%%
%% Content-Type: multipart/mixed -> {"Content-Type", "multipart/mixed"}
%%
%% But in http/1.1 the field-names are case insencitive so now it must be
%% Content-Type: multipart/mixed -> {"content-type", "multipart/mixed"}
%% The standard furthermore says that leading and traling white space
%% is not a part of the fieldvalue and shall therefore be removed.
tagup_header([]) ->          [];
tagup_header([Line|Rest]) -> [tag(Line, [])|tagup_header(Rest)].

tag([], Tag) ->
    {httpd_util:to_lower(lists:reverse(Tag)), ""};
tag([$:|Rest], Tag) ->
    {httpd_util:to_lower(lists:reverse(Tag)), httpd_util:strip(Rest)};
tag([Chr|Rest], Tag) ->
    tag(Rest, [Chr|Tag]).

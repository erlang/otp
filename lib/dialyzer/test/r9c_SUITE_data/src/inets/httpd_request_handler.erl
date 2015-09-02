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
%%     $Id: httpd_request_handler.erl,v 1.1 2008/12/17 09:53:34 mikpe Exp $
-module(httpd_request_handler).

%% app internal api
-export([start_link/2, synchronize/3]).

%% module internal api
-export([connection/2, do_next_connection/6, read_header/7]).
-export([parse_trailers/1, newline/1]).

-include("httpd.hrl").
-include("httpd_verbosity.hrl").


%% start_link

start_link(Manager, ConfigDB) ->
    Pid = proc_lib:spawn(?MODULE, connection, [Manager, ConfigDB]),
    {ok, Pid}.


%% synchronize

synchronize(Pid, SocketType, Socket) ->
    Pid ! {synchronize, SocketType, Socket}.

% connection

connection(Manager, ConfigDB) ->
    {SocketType, Socket, {Status, Verbosity}} = await_synchronize(Manager),
    put(sname,self()),
    put(verbosity,?vvalidate(Verbosity)),
    connection1(Status, Manager, ConfigDB, SocketType, Socket).


connection1({reject, busy}, Manager, ConfigDB, SocketType, Socket) ->
    handle_busy(Manager, ConfigDB, SocketType, Socket);

connection1({reject, blocked}, Manager, ConfigDB, SocketType, Socket) ->
    handle_blocked(Manager, ConfigDB, SocketType, Socket);

connection1(accept, Manager, ConfigDB, SocketType, Socket) ->
    handle_connection(Manager, ConfigDB, SocketType, Socket).


%% await_synchronize

await_synchronize(Manager) ->
    receive
	{synchronize, SocketType, Socket} ->
	    ?vlog("received syncronize: "
		  "~n   SocketType: ~p"
		  "~n   Socket:     ~p", [SocketType, Socket]),
	    {SocketType, Socket, httpd_manager:new_connection(Manager)}
    after 5000 ->
	    exit(synchronize_timeout)
    end.


% handle_busy

handle_busy(Manager, ConfigDB, SocketType, Socket) ->
    ?vlog("handle busy: ~p", [Socket]),
    MaxClients = httpd_util:lookup(ConfigDB, max_clients, 150),
    String = io_lib:format("heavy load (>~w processes)", [MaxClients]),
    reject_connection(Manager, ConfigDB, SocketType, Socket, String).


% handle_blocked

handle_blocked(Manager, ConfigDB, SocketType, Socket) ->
    ?vlog("handle blocked: ~p", [Socket]),
    String = "Server maintenance performed, try again later",
    reject_connection(Manager, ConfigDB, SocketType, Socket, String).


% reject_connection

reject_connection(Manager, ConfigDB, SocketType, Socket, Info) ->
    String = lists:flatten(Info),
    ?vtrace("send status (503) message", []),
    httpd_response:send_status(SocketType, Socket, 503, String, ConfigDB),
    %% This ugly thing is to make ssl deliver the message, before the close...
    close_sleep(SocketType, 1000),
    ?vtrace("close the socket", []),
    close(SocketType, Socket, ConfigDB).


% handle_connection

handle_connection(Manager, ConfigDB, SocketType, Socket) ->
    ?vlog("handle connection: ~p", [Socket]),
    Resolve     = httpd_socket:resolve(SocketType),
    Peername    = httpd_socket:peername(SocketType, Socket),
    InitData    = #init_data{peername=Peername, resolve=Resolve},
    TimeOut     = httpd_util:lookup(ConfigDB, keep_alive_timeout, 150000),
    NrOfRequest = httpd_util:lookup(ConfigDB, max_keep_alive_request, forever),
    ?MODULE:do_next_connection(ConfigDB, InitData,
			       SocketType, Socket,NrOfRequest,TimeOut),
    ?vlog("handle connection: done", []),
    httpd_manager:done_connection(Manager),
    ?vlog("handle connection: close socket", []),
    close(SocketType, Socket, ConfigDB).


% do_next_connection
do_next_connection(_ConfigDB, _InitData, _SocketType, _Socket, NrOfRequests,
		   _Timeout) when NrOfRequests < 1 ->
    ?vtrace("do_next_connection: done", []),
    ok;
do_next_connection(ConfigDB, InitData, SocketType, Socket, NrOfRequests,
		   Timeout) ->
    Peername = InitData#init_data.peername,
    case (catch read(ConfigDB, SocketType, Socket, InitData, Timeout)) of
        {'EXIT', Reason} ->
            ?vlog("exit reading from socket: ~p",[Reason]),
            error_logger:error_report({'EXIT',Reason}),
	    String =
		lists:flatten(
		  io_lib:format("exit reading from socket: ~p => ~n~p~n",
				[Socket, Reason])),
	    error_log(mod_log,
		      SocketType, Socket, ConfigDB, Peername, String),
	    error_log(mod_disk_log,
		      SocketType, Socket, ConfigDB, Peername, String);
        {error, Reason} ->
            handle_read_error(Reason,SocketType,Socket,ConfigDB,Peername);
        Info when record(Info, mod) ->
            case Info#mod.connection of
                true ->
                    ReqTimeout = httpd_util:lookup(ConfigDB,
						   keep_alive_timeout, 150000),
		    ?MODULE:do_next_connection(ConfigDB,          InitData,
					       SocketType,        Socket,
					       dec(NrOfRequests), ReqTimeout);
		_ ->
                    ok
            end;
        _ ->
            ok
    end.



%% read
read(ConfigDB, SocketType, Socket, InitData, Timeout) ->
    ?vdebug("read from socket ~p with Timeout ~p",[Socket, Timeout]),
    MaxHdrSz = httpd_util:lookup(ConfigDB, max_header_size, 10240),
    case ?MODULE:read_header(SocketType, Socket, Timeout, MaxHdrSz,
			     ConfigDB, InitData, []) of
	{socket_closed, Reason} ->
	    ?vlog("Socket closed while reading request header: "
		  "~n   ~p", [Reason]),
	    socket_close;
	{error, Error} ->
	    {error, Error};
	{ok, Info, EntityBodyPart} ->
	    read1(SocketType, Socket, ConfigDB, InitData, Timeout, Info,
		  EntityBodyPart)
    end.

%% Got the head and maybe a part of the body: read in the rest
read1(SocketType, Socket, ConfigDB, InitData, Timeout, Info, BodyPart)->
    MaxBodySz     = httpd_util:lookup(ConfigDB, max_body_size, nolimit),
    ContentLength = content_length(Info),
    ?vtrace("ContentLength: ~p", [ContentLength]),
    case read_entity_body(SocketType, Socket, Timeout, MaxBodySz,
			  ContentLength, BodyPart, Info, ConfigDB) of
	{socket_closed, Reason} ->
	    ?vlog("Socket closed while reading request body: "
		  "~n   ~p", [Reason]),
	    socket_close;
	{ok, EntityBody} ->
	    finish_request(EntityBody, [], Info);
	{ok, ExtraHeader, EntityBody} ->
	    finish_request(EntityBody, ExtraHeader, Info);
	Response ->
	    httpd_socket:close(SocketType, Socket),
	    socket_closed
	    %% Catch up all bad return values
    end.


%% The request is read in send it forward to the module that
%% generates the response

finish_request(EntityBody, ExtraHeader,
	       #mod{parsed_header = ParsedHeader} = Info)->
    ?DEBUG("finish_request -> ~n"
	"    EntityBody:   ~p~n"
	"    ExtraHeader:  ~p~n"
	"    ParsedHeader: ~p~n",
	[EntityBody, ExtraHeader, ParsedHeader]),
    httpd_response:send(Info#mod{parsed_header = ParsedHeader ++ ExtraHeader,
				 entity_body   = EntityBody}).


%% read_header

%% This algorithm rely on the buffer size of the inet driver together
%% with the {active, once} socket option. Atmost one message of this
%% size will be received at a given time. When a full header has been
%% read, the body is read with the recv function (the body size is known).
%%
read_header(SocketType, Socket, Timeout, MaxHdrSz, ConfigDB,
	    InitData, SoFar0) ->
    T = t(),
    %% remove any newlines at the begining, they might be crap from ?
    SoFar = remove_newline(SoFar0),

    case terminated_header(MaxHdrSz, SoFar) of
	{true, Header, EntityBodyPart} ->
	    ?vdebug("read_header -> done reading header: "
		    "~n   length(Header):         ~p"
		    "~n   length(EntityBodyPart): ~p",
		    [length(Header), length(EntityBodyPart)]),
	    transform_header(SocketType, Socket, Header, ConfigDB, InitData,
			     EntityBodyPart);
	false ->
	    ?vtrace("read_header -> "
		    "~n   set active = 'once' and "
		    "await a chunk of the header", []),

	    case httpd_socket:active_once(SocketType, Socket) of
		ok ->
		    receive
			%%
			%% TCP
			%%
			{tcp, Socket, Data} ->
			    ?vtrace("read_header(ip) -> got some data: ~p",
				[sz(Data)]),
			    ?MODULE:read_header(SocketType, Socket,
						Timeout - (t()-T),
						MaxHdrSz, ConfigDB,
						InitData, SoFar ++ Data);
			{tcp_closed, Socket} ->
			    ?vtrace("read_header(ip) -> socket closed",[]),
			    {socket_closed,normal};
			{tcp_error, Socket, Reason} ->
			    ?vtrace("read_header(ip) -> socket error: ~p",
				[Reason]),
			    {socket_closed, Reason};

			%%
			%% SSL
			%%
			{ssl, Socket, Data} ->
			    ?vtrace("read_header(ssl) -> got some data: ~p",
				[sz(Data)]),
			    ?MODULE:read_header(SocketType, Socket,
						Timeout - (t()-T),
						MaxHdrSz, ConfigDB,
						InitData, SoFar ++ Data);
			{ssl_closed, Socket} ->
			    ?vtrace("read_header(ssl) -> socket closed", []),
			    {socket_closed, normal};
			{ssl_error, Socket, Reason} ->
			    ?vtrace("read_header(ssl) -> socket error: ~p",
				[Reason]),
			    {socket_closed, Reason}

		    after Timeout ->
			    ?vlog("read_header -> timeout", []),
			    {socket_closed, timeout}
		    end;

		Error ->
		    httpd_response:send_status(SocketType, Socket,
					       500, none, ConfigDB),
		    Error
	    end
    end.


terminated_header(MaxHdrSz, Data) ->
    D1 = lists:flatten(Data),
    ?vtrace("terminated_header -> Data size: ~p",[sz(D1)]),
    case hsplit(MaxHdrSz,[],D1) of
	not_terminated ->
	    false;
	[Header, EntityBodyPart] ->
	    {true, Header++"\r\n\r\n",EntityBodyPart}
    end.


transform_header(SocketType, Socket, Request, ConfigDB, InitData, BodyPart) ->
    case httpd_parse:request_header(Request) of
	{not_implemented, RequestLine, Method, RequestURI, ParsedHeader,
	 HTTPVersion} ->
	    httpd_response:send_status(SocketType, Socket, 501,
				       {Method, RequestURI, HTTPVersion},
				       ConfigDB),
	    {error,"Not Implemented"};
	{bad_request, {forbidden, URI}} ->
	    httpd_response:send_status(SocketType, Socket, 403, URI, ConfigDB),
	    {error,"Forbidden Request"};
	{bad_request, Reason} ->
	    httpd_response:send_status(SocketType, Socket, 400, none,
				       ConfigDB),
	    {error,"Malformed request"};
	{ok,[Method, RequestURI, HTTPVersion, RequestLine, ParsedHeader]} ->
	    ?DEBUG("send -> ~n"
		   "    Method:      ~p~n"
		   "    RequestURI:  ~p~n"
		   "    HTTPVersion: ~p~n"
		   "    RequestLine: ~p~n",
		   [Method, RequestURI, HTTPVersion, RequestLine]),
	    {ok, Info} =
		httpd_parse:get_request_record(Socket, SocketType, ConfigDB,
					       Method, RequestURI, HTTPVersion,
					       RequestLine, ParsedHeader,
					       [], InitData),
	    %% Control that the Host header field is provided
	    case Info#mod.absolute_uri of
		nohost ->
		    case Info#mod.http_version of
			"HTTP/1.1" ->
			    httpd_response:send_status(Info, 400, none),
			    {error,"No host specified"};
			_ ->
			    {ok, Info, BodyPart}
		    end;
		_ ->
		    {ok, Info, BodyPart}
	    end
    end.


hsplit(_MaxHdrSz, Accu,[]) ->
    not_terminated;
hsplit(_MaxHdrSz, Accu, [ $\r, $\n, $\r, $\n | Tail]) ->
    [lists:reverse(Accu), Tail];
hsplit(nolimit, Accu, [H|T]) ->
    hsplit(nolimit,[H|Accu],T);
hsplit(MaxHdrSz, Accu, [H|T]) when length(Accu) < MaxHdrSz ->
    hsplit(MaxHdrSz,[H|Accu],T);
hsplit(MaxHdrSz, Accu, D) ->
    throw({error,{header_too_long,length(Accu),length(D)}}).



%%----------------------------------------------------------------------
%% The http/1.1 standard chapter 8.2.3 says that a request containing
%% An Except header-field must be responded to by 100 (Continue) by
%% the server before the client sends the body.
%%----------------------------------------------------------------------

read_entity_body(SocketType, Socket, Timeout, Max, Length, BodyPart, Info,
		 ConfigDB) when integer(Max) ->
    case expect(Info#mod.http_version, Info#mod.parsed_header, ConfigDB) of
	continue when Max > Length ->
	    ?DEBUG("read_entity_body()->100 Continue  ~n", []),
	    httpd_response:send_status(Info, 100, ""),
	    read_entity_body2(SocketType, Socket, Timeout, Max, Length,
			      BodyPart, Info, ConfigDB);
	continue when Max < Length ->
	    httpd_response:send_status(Info, 417, "Body to big"),
	    httpd_socket:close(SocketType, Socket),
	    {socket_closed,"Expect denied according to size"};
	break ->
	    httpd_response:send_status(Info, 417, "Method not allowed"),
	    httpd_socket:close(SocketType, Socket),
	    {socket_closed,"Expect conditions was not fullfilled"};
	no_expect_header ->
	    read_entity_body2(SocketType, Socket, Timeout, Max, Length,
			      BodyPart, Info, ConfigDB);
	http_1_0_expect_header ->
	    httpd_response:send_status(Info, 400,
				       "Only HTTP/1.1 Clients "
				       "may use the Expect Header"),
	    httpd_socket:close(SocketType, Socket),
	    {socket_closed,"Due to a HTTP/1.0 expect header"}
    end;

read_entity_body(SocketType, Socket, Timeout, Max, Length, BodyPart,
		 Info, ConfigDB) ->
    case expect(Info#mod.http_version, Info#mod.parsed_header, ConfigDB) of
	continue ->
	    ?DEBUG("read_entity_body() -> 100 Continue  ~n", []),
	    httpd_response:send_status(Info, 100, ""),
	    read_entity_body2(SocketType, Socket, Timeout, Max, Length,
			      BodyPart, Info, ConfigDB);
	break->
	    httpd_response:send_status(Info, 417, "Method not allowed"),
	    httpd_socket:close(SocketType, Socket),
	    {socket_closed,"Expect conditions was not fullfilled"};
	no_expect_header ->
	    read_entity_body2(SocketType, Socket, Timeout, Max, Length,
			      BodyPart, Info, ConfigDB);
	http_1_0_expect_header ->
	    httpd_response:send_status(Info, 400,
				       "HTTP/1.0 Clients are not allowed "
				       "to use the Expect Header"),
	    httpd_socket:close(SocketType, Socket),
	    {socket_closed,"Expect header field in an HTTP/1.0 request"}
	end.

%%----------------------------------------------------------------------
%% control if the body is transfer encoded
%%----------------------------------------------------------------------
read_entity_body2(SocketType, Socket, Timeout, Max, Length, BodyPart,
		  Info, ConfigDB) ->
    ?DEBUG("read_entity_body2() -> "
	"~n   Max:    ~p"
	"~n   Length: ~p"
	"~n   Socket: ~p", [Max, Length, Socket]),

    case transfer_coding(Info) of
	{chunked, ChunkedData} ->
	    ?DEBUG("read_entity_body2() -> "
		"Transfer-encoding: Chunked Data: BodyPart ~s", [BodyPart]),
	    read_chunked_entity(Info, Timeout, Max, Length, ChunkedData, [],
				BodyPart);
	unknown_coding ->
	    ?DEBUG("read_entity_body2() -> Transfer-encoding: Unknown",[]),
	    httpd_response:send_status(Info, 501, "Unknown Transfer-Encoding"),
	    httpd_socket:close(SocketType, Socket),
	    {socket_closed,"Expect conditions was not fullfilled"};
	none ->
	      ?DEBUG("read_entity_body2() -> Transfer-encoding: none",[]),
	    read_entity_body(SocketType, Socket, Timeout, Max, Length,
			     BodyPart)
    end.


%%----------------------------------------------------------------------
%% The body was plain read it from the socket
%% ----------------------------------------------------------------------
read_entity_body(_SocketType, _Socket, _Timeout, _Max, 0, _BodyPart) ->
    {ok, []};

read_entity_body(_SocketType, _Socket, _Timeout, Max, Len, _BodyPart)
  when Max < Len ->
    ?vlog("body to long: "
	  "~n   Max: ~p"
	  "~n   Len: ~p", [Max,Len]),
    throw({error,{body_too_long,Max,Len}});

%% OTP-4409: Fixing POST problem
read_entity_body(_,_,_,_, Len, BodyPart) when Len == length(BodyPart) ->
    ?vtrace("read_entity_body -> done when"
	"~n   Len = length(BodyPart): ~p", [Len]),
    {ok, BodyPart};

%% OTP-4550: Fix problem with trailing garbage produced by some clients.
read_entity_body(_, _, _, _, Len, BodyPart) when Len < length(BodyPart) ->
    ?vtrace("read_entity_body -> done when"
	"~n   Len:              ~p"
	"~n   length(BodyPart): ~p", [Len, length(BodyPart)]),
    {ok, lists:sublist(BodyPart,Len)};

read_entity_body(SocketType, Socket, Timeout, Max, Len, BodyPart) ->
    ?vtrace("read_entity_body -> entry when"
	"~n   Len:              ~p"
	"~n   length(BodyPart): ~p", [Len, length(BodyPart)]),
    %% OTP-4548:
    %% The length calculation was previously (inets-2.*) done in the
    %% read function. As of 3.0 it was removed from read but not
    %% included here.
    L = Len - length(BodyPart),
    case httpd_socket:recv(SocketType, Socket, L, Timeout) of
	{ok, Body} ->
	    ?vtrace("read_entity_body -> received some data:"
		"~n   length(Body): ~p", [length(Body)]),
	    {ok, BodyPart ++ Body};
	{error,closed} ->
	    {socket_closed,normal};
	{error,etimedout} ->
	    {socket_closed, timeout};
	{error,Reason} ->
	    {socket_closed, Reason};
	Other ->
	    {socket_closed, Other}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% If the body of the message is encoded used the chunked transfer encoding
%% it looks somethin like this:
%% METHOD URI HTTP/VSN
%% Transfer-Encoding: chunked
%% CRLF
%% ChunkSize
%% Chunk
%% ChunkSize
%% Chunk
%% 0
%% Trailer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_chunked_entity(Info, Timeout, Max, Length, ChunkedData, Body, []) ->
    ?DEBUG("read_chunked_entity()->:no_chunks ~n", []),
    read_chunked_entity(Info#mod.socket_type, Info#mod.socket,
			Timeout, Max, Length, ChunkedData, Body,
			Info#mod.config_db, Info);

read_chunked_entity(Info, Timeout, Max, Length, ChunkedData, Body, BodyPart) ->
    %% Get the size
    ?DEBUG("read_chunked_entity() -> PrefetchedBodyPart: ~p ~n",[BodyPart]),
    case parse_chunk_size(Info, Timeout, BodyPart) of
	{ok, Size, NewBodyPart} when Size > 0 ->
	    ?DEBUG("read_chunked_entity() -> Size: ~p ~n", [Size]),
	    case parse_chunked_entity_body(Info, Timeout, Max, length(Body),
					   Size, NewBodyPart) of
		{ok, Chunk, NewBodyPart1} ->
		    ?DEBUG("read_chunked_entity()->Size: ~p ~n", [Size]),
		    read_chunked_entity(Info, Timeout, Max, Length,
					ChunkedData, Body ++ Chunk,
					NewBodyPart1);
		OK ->
		    httpd_socket:close(Info#mod.socket_type, Info#mod.socket),
		    {socket_closed, error}
	    end;
	{ok, 0, Trailers} ->
	   ?DEBUG("read_chunked_entity()->Size: 0, Trailers: ~s Body: ~s ~n",
		[Trailers, Body]),
	    case parse_chunk_trailer(Info, Timeout, Info#mod.config_db,
				     Trailers) of
		{ok, TrailerFields} ->
		    {ok, TrailerFields, Body};
		_->
		    {ok, []}
	    end;
	Error ->
	    Error
    end.


parse_chunk_size(Info, Timeout, BodyPart) ->
    case httpd_util:split(remove_newline(BodyPart), "\r\n", 2) of
	{ok, [Size, Body]} ->
	    ?DEBUG("parse_chunk_size()->Size: ~p ~n", [Size]),
	    {ok, httpd_util:hexlist_to_integer(Size), Body};
	{ok, [Size]} ->
	    ?DEBUG("parse_chunk_size()->Size: ~p ~n", [Size]),
	    Sz = get_chunk_size(Info#mod.socket_type,
				Info#mod.socket, Timeout,
				lists:reverse(Size)),
	    {ok, Sz, []}
    end.

%%----------------------------------------------------------------------
%% We got the chunk size get the chunk
%%
%% Max:     Max numbers of bytes to read may also be undefined
%% Length:  Numbers of bytes already read
%% Size     Numbers of byte to read for the chunk
%%----------------------------------------------------------------------

%% body to big
parse_chunked_entity_body(Info, Timeout, Max, Length, Size, BodyPart)
  when Max =< (Length + Size) ->
    {error, body_to_big};

%% Prefetched body part is bigger than the current chunk
%% (i.e. BodyPart includes more than one chunk)
parse_chunked_entity_body(Info, Timeout, Max, Length, Size, BodyPart)
  when (Size+2) =< length(BodyPart) ->
    Chunk = string:substr(BodyPart, 1, Size),
    Rest  = string:substr(BodyPart, Size+3),
    ?DEBUG("parse_chunked_entity_body() -> ~nChunk: ~s ~nRest: ~s ~n",
	[Chunk, Rest]),
    {ok, Chunk, Rest};


%% We just got a part of the current chunk
parse_chunked_entity_body(Info, Timeout, Max, Length, Size, BodyPart) ->
    %% OTP-4551:
    %% Subtracting BodyPart from Size does not produce an integer
    %% when BodyPart is a list...
    Remaining = Size - length(BodyPart),
    LastPartOfChunk = read_chunked_entity_body(Info#mod.socket_type,
					       Info#mod.socket,
					       Timeout, Max,
					       Length, Remaining),
    %% Remove newline
    httpd_socket:recv(Info#mod.socket_type, Info#mod.socket, 2, Timeout),
    ?DEBUG("parse_chunked_entity_body() -> "
	"~nBodyPart: ~s"
	"~nLastPartOfChunk: ~s  ~n",
	[BodyPart, LastPartOfChunk]),
    {ok, BodyPart ++ LastPartOfChunk, []}.


%%----------------------------------------------------------------------
%% If the data we got along with the header contained the whole chunked body
%% It may aswell contain the trailer :-(
%%----------------------------------------------------------------------
%% Either trailer begins with  \r\n and then all data is there or
%% The trailer has data  then read upto \r\n\r\n
parse_chunk_trailer(Info,Timeout,ConfigDB,"\r\n")->
    {ok,[]};
parse_chunk_trailer(Info,Timeout,ConfigDB,Trailers) ->
    ?DEBUG("parse_chunk_trailer()->Trailers: ~s ~n", [Trailers]),
    case string:rstr(Trailers,"\r\n\r\n") of
	0 ->
	    MaxHdrSz=httpd_util:lookup(ConfigDB, max_header_size, 10240),
	    read_trailer_end(Info,Timeout,MaxHdrSz,Trailers);
	_->
	    %%We got the whole header parse it up
	    parse_trailers(Trailers)
    end.

parse_trailers(Trailer)->
    ?DEBUG("parse_trailer()->Trailer: ~s",[Trailer]),
    {ok,[Fields0|Crap]}=httpd_util:split(Trailer,"\r\n\r\n",2),
    Fields=string:tokens(Fields0,"\r\n"),
	[getTrailerField(X)||X<-Fields,lists:member($:,X)].


read_trailer_end(Info,Timeout,MaxHdrSz,[])->
    ?DEBUG("read_trailer_end()->[]",[]),
    case read_trailer(Info#mod.socket_type,Info#mod.socket,
		 Timeout,MaxHdrSz,[],[],
		      httpd_util:key1search(Info#mod.parsed_header,"trailer",[])) of
	{ok,Trailers}->
	    Trailers;
	_->
	    []
    end;
read_trailer_end(Info,Timeout,MaxHdrSz,Trailers)->
    ?DEBUG("read_trailer_end()->Trailers: ~s ~n ",[Trailers]),
    %% Get the last paart of the the last headerfield
    End=lists:reverse(lists:takewhile(fun(X)->case X of 10 ->false;13->false;_ ->true end end,lists:reverse(Trailers))),
    Fields0=regexp:split(Trailers,"\r\n"),
    %%Get rid of the last header field
    [_Last|Fields]=lists:reverse(Fields0),
    Headers=[getTrailerField(X)||X<-Fields,lists:member($:,X)],
    case read_trailer(Info#mod.socket_type,Info#mod.socket,
		 Timeout,MaxHdrSz,Headers,End,
		      httpd_util:key1search(Info#mod.parsed_header,"trailer",[])) of
	{ok,Trailers}->
	    Trailers;
	_->
	    []
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The code below is a a good way to read in chunked encoding but
%% that require that the encoding comes from a stream and not from a list
%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

%%----------------------------------------------------------------------
%% The body is encoded by chubnked encoding read it in
%% ChunkedData= Chunked extensions
%% Body= the inread chunked body
%% Max:     Max numbers of bytes to read
%% Length:  Numbers of bytes already readed
%% Size     Numbers of byte to read for the chunk
%%----------------------------------------------------------------------



read_chunked_entity(SocketType, Socket, Timeout, Max, Length, ChunkedData,
		    Body, ConfigDB, Info) ->
    T = t(),
    case get_chunk_size(SocketType,Socket,Timeout,[]) of
	Size when integer(Size), Size>0 ->
	    case read_chunked_entity_body(SocketType, Socket,
					  Timeout-(t()-T),
					  Max, length(Body), Size) of
		{ok,Chunk} ->
		    ?DEBUG("read_chunked_entity/9 Got a chunk: ~p " ,[Chunk]),
		    %% Two bytes are left of the chunk, that is the CRLF
		    %% at the end that is not a part of the message
		    %% So we read it and do nothing with it.
		    httpd_socket:recv(SocketType,Socket,2,Timeout-(t()-T)),
		    read_chunked_entity(SocketType, Socket, Timeout-(t()-T),
					Max, Length, ChunkedData, Body++Chunk,
					ConfigDB, Info);
		Error ->
		    ?DEBUG("read_chunked_entity/9 Error: ~p " ,[Error]),
		    httpd_socket:close(SocketType,Socket),
		    {socket_closed,error}
	    end;
	Size when integer(Size), Size == 0 ->
	    %% Must read in any trailer fields here
	    read_chunk_trailer(SocketType, Socket, Timeout,
			       Max, Info, ChunkedData, Body, ConfigDB);
	Error ->
	    Error
    end.


%% If a user wants to send header data after the chunked data we
%% must pick it out
read_chunk_trailer(SocketType, Socket, Timeout, Max, Info, ChunkedData,
		   Body, ConfigDB) ->
    ?DEBUG("read_chunk_trailer/8: ~p " ,[Body]),
    MaxHdrSz = httpd_util:lookup(ConfigDB,max_header_size,10240),
    case httpd_util:key1search(Info#mod.parsed_header,"trailer")of
	undefined ->
	    {ok,Body};
	Fields ->
	    case read_trailer(SocketType, Socket, Timeout,
			      MaxHdrSz, [], [],
			      string:tokens(
				httpd_util:to_lower(Fields),",")) of
		{ok,[]} ->
		    {ok,Body};
		{ok,HeaderFields} ->
		    % ParsedExtraHeaders =
		    % httpd_parse:tagup_header(httpd_parse:split_lines(HeaderFields)),
		    {ok,HeaderFields,Body};
		Error ->
		    Error
	    end
    end.

read_chunked_entity_body(SocketType, Socket, Timeout, Max, Length, Size)
  when integer(Max) ->
    read_entity_body(SocketType, Socket, Timeout, Max-Length, Size, []);

read_chunked_entity_body(SocketType, Socket, Timeout, Max, _Length, Size) ->
    read_entity_body(SocketType, Socket, Timeout, Max, Size, []).

%% If we read in the \r\n the httpd_util:hexlist_to_integer
%% Will remove it and we get rid of it emmediatly :-)
get_chunk_size(SocketType, Socket, Timeout, Size) ->
    T = t(),
    ?DEBUG("get_chunk_size: ~p " ,[Size]),
    case httpd_socket:recv(SocketType,Socket,1,Timeout) of
	{ok,[Digit]} when Digit==$\n ->
	    httpd_util:hexlist_to_integer(lists:reverse(Size));
       {ok,[Digit]} ->
	    get_chunk_size(SocketType,Socket,Timeout-(t()-T),[Digit|Size]);
	{error,closed} ->
	    {socket_closed,normal};
	{error,etimedout} ->
	    {socket_closed, timeout};
	{error,Reason} ->
	    {socket_closed, Reason};
	Other ->
	    {socket_closed,Other}
    end.




%%----------------------------------------------------------------------
%% Reads the HTTP-trailer
%% Would be easy to tweak the read_head to do this but in this way
%% the chunked encoding can be updated better.
%%----------------------------------------------------------------------


%% When end is reached
%% read_trailer(SocketType,Socket,Timeout,MaxHdrSz,Headers,Last,[]) ->
%%    {ok,Headers};

%% When header to big
read_trailer(_,_,_,MaxHdrSz,Headers,Bs,_Fields)
  when MaxHdrSz < length(Headers) ->
    ?vlog("header to long: "
	  "~n   MaxHdrSz:   ~p"
	  "~n   length(Bs): ~p", [MaxHdrSz,length(Bs)]),
    throw({error,{header_too_long,MaxHdrSz,length(Bs)}});

%% The last Crlf is there
read_trailer(_, _, _, _, Headers, [$\n, $\r], _) ->
    {ok,Headers};

read_trailer(SocketType, Socket, Timeout, MaxHdrSz, Headers,
	     [$\n, $\r|Rest], Fields) ->
    case getTrailerField(lists:reverse(Rest))of
	{error,Reason}->
	    {error,"Bad trailer"};
	{HeaderField,Value}->
	    case lists:member(HeaderField,Fields) of
		true ->
		    read_trailer(SocketType,Socket,Timeout,MaxHdrSz,
				 [{HeaderField,Value} |Headers],[],
				 lists:delete(HeaderField,Fields));
		false ->
		    read_trailer(SocketType,Socket,Timeout,MaxHdrSz,
				 Headers,[],Fields)
	    end
    end;

% read_trailer(SocketType,Socket,Timeout,MaxHdrSz,Headers,[$\n, $\r|Rest],Fields) ->
%     case Rest of
% 	[] ->
% 	   read_trailer(SocketType,Socket,Timeout,MaxHdrSz,Headers,Rest,Fields);
% 	Field ->
% 	    case getTrailerField(lists:reverse(Rest))of
% 		{error,Reason}->
% 		    {error,"Bad trailer"};
% 		{HeaderField,Value}->
% 		    case lists:member(HeaderField,Fields) of
% 			true ->
% 			    read_trailer(SocketType,Socket,Timeout,MaxHdrSz,
% 					 [{HeaderField,Value} |Headers],[],
% 					 lists:delete(HeaderField,Fields));
% 			false ->
% 			    read_trailer(SocketType,Socket,Timeout,MaxHdrSz,
% 					 Headers,[],Fields)
% 		    end
% 	    end
%     end;

read_trailer(SocketType,Socket,Timeout,MaxHdrSz,Headers,Bs,Fields) ->
    %% ?vlog("read_header -> entry with Timeout: ~p",[Timeout]),
    T = t(),
    case (catch httpd_socket:recv(SocketType,Socket,1,Timeout)) of
	{ok,[B]} ->
	    read_trailer(SocketType, Socket, Timeout-(t()-T),
			 MaxHdrSz, Headers, [B|Bs], Fields);
	{error,closed} ->
	    {socket_closed,normal};
	{error,etimedout} ->
	    {socket_closed, timeout};
	{error,Reason} ->
	    {socket_closed, Reason};
	Other ->
	    {socket_closed,Other}
    end.

getTrailerField(HeaderField)->
   case string:str(HeaderField,":") of
       0->
	   {error,"badheaderfield"};
       Number ->
	   {httpd_util:to_lower(string:substr(HeaderField,1,Number-1)),
	    httpd_util:to_lower(string:substr(HeaderField,Number+1))}
   end.




%% Time in milli seconds
t() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).

%%----------------------------------------------------------------------
%% If the user sends an expect header-field with the value 100-continue
%% We must send a 100 status message if he is a HTTP/1.1 client.

%% If it is an HTTP/1.0 client it's little more difficult.
%% If expect is not defined it is easy but in the other case shall we
%% Break or the transmission or let it continue the standard is not clear
%% if to break connection or wait for data.
%%----------------------------------------------------------------------
expect(HTTPVersion,ParsedHeader,ConfigDB)->
    case HTTPVersion of
	[$H,$T,$T,$P,$\/,$1,$.,N|_Whatever]when N>=1->
	    case httpd_util:key1search(ParsedHeader,"expect") of
		"100-continue" ->
		    continue;
		undefined ->
		    no_expect_header;
		NewValue ->
		    break
	    end;
	_OldVersion ->
	    case httpd_util:key1search(ParsedHeader,"expect") of
		undefined ->
		    no_expect_header;
		NewValue ->
		    case httpd_util:lookup(ConfigDB,expect,continue) of
			continue->
			    no_expect_header;
			_ ->
			    http_1_0_expect_header
		    end
	    end
    end.


%%----------------------------------------------------------------------
%% According to the http/1.1 standard all applications must understand
%% Chunked encoded data. (Last line chapter 3.6.1).
transfer_coding(#mod{parsed_header = Ph}) ->
    case httpd_util:key1search(Ph, "transfer-encoding", none) of
	none ->
	    none;
	[$c,$h,$u,$n,$k,$e,$d|Data]->
	    {chunked,Data};
	_ ->
	    unknown_coding
    end.



handle_read_error({header_too_long,Max,Rem},
                  SocketType,Socket,ConfigDB,Peername) ->
    String = io_lib:format("header too long: ~p : ~p",[Max,Rem]),
    handle_read_error(ConfigDB,String,SocketType,Socket,Peername,
                      max_header_action,close);
handle_read_error({body_too_long,Max,Actual},
                  SocketType,Socket,ConfigDB,Peername) ->
    String = io_lib:format("body too long: ~p : ~p",[Max,Actual]),
    handle_read_error(ConfigDB,String,SocketType,Socket,Peername,
                      max_body_action,close);
handle_read_error(Error,SocketType,Socket,ConfigDB,Peername) ->
    ok.


handle_read_error(ConfigDB, ReasonString, SocketType, Socket, Peername,
                  Item, Default) ->
    ?vlog("error reading request: ~s",[ReasonString]),
    E = lists:flatten(
          io_lib:format("Error reading request: ~s",[ReasonString])),
    error_log(mod_log, SocketType, Socket, ConfigDB, Peername, E),
    error_log(mod_disk_log, SocketType, Socket, ConfigDB, Peername, E),
    case httpd_util:lookup(ConfigDB,Item,Default) of
        reply414 ->
            send_read_status(SocketType, Socket, 414, ReasonString, ConfigDB);
        _ ->
            ok
    end.

send_read_status(SocketType, Socket, Code, ReasonString, ConfigDB) ->
    httpd_response:send_status(SocketType, Socket, Code, ReasonString,
			       ConfigDB).


error_log(Mod, SocketType, Socket, ConfigDB, Peername, String) ->
    Modules = httpd_util:lookup(ConfigDB, modules,
				[mod_get, mod_head, mod_log]),
    case lists:member(Mod, Modules) of
	true ->
	    Mod:error_log(SocketType, Socket, ConfigDB, Peername, String);
	_ ->
	    ok
    end.


sz(L) when list(L) ->
    length(L);
sz(B) when binary(B) ->
    size(B);
sz(O) ->
    {unknown_size,O}.


%% Socket utility functions:

close(SocketType, Socket, ConfigDB) ->
    case httpd_socket:close(SocketType, Socket) of
        ok ->
            ok;
        {error, Reason} ->
            ?vlog("error while closing socket: ~p",[Reason]),
	    ok
    end.

close_sleep({ssl, _}, Time) ->
    sleep(Time);
close_sleep(_, _) ->
    ok.


sleep(T) -> receive after T -> ok end.


dec(N) when integer(N) ->
    N-1;
dec(N) ->
    N.


content_length(#mod{parsed_header = Ph}) ->
    list_to_integer(httpd_util:key1search(Ph, "content-length","0")).


remove_newline(List)->
    lists:dropwhile(fun newline/1,List).

newline($\r) ->
    true;
newline($\n) ->
    true;
newline(_Sign) ->
    false.

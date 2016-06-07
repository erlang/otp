%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(mod_get).

-export([do/1]).

-include("httpd.hrl").
-include("httpd_internal.hrl").
-include("inets_internal.hrl").

-define(VMODULE,"GET").


%% do

do(Info) ->
    ?DEBUG("do -> entry",[]),
    case Info#mod.method of
	"GET" ->
	    case proplists:get_value(status, Info#mod.data) of
		%% A status code has been generated!
		{_StatusCode, _PhraseArgs, _Reason} ->
		    {proceed,Info#mod.data};
		%% No status code has been generated!
		undefined ->
		    case proplists:get_value(response, Info#mod.data) of
			%% No response has been generated!
			undefined ->
			    do_get(Info);
			%% A response has been generated or sent!
			_Response ->
			    {proceed,Info#mod.data}
		    end
	    end;
	%% Not a GET method!
	_ ->
	    {proceed,Info#mod.data}
    end.


do_get(Info) ->
    ?DEBUG("do_get -> Request URI: ~p",[Info#mod.request_uri]),
    Path = mod_alias:path(Info#mod.data, Info#mod.config_db, 
			  Info#mod.request_uri),
 
    send_response(Info#mod.socket,Info#mod.socket_type, Path, Info).


%% The common case when no range is specified
send_response(_Socket, _SocketType, Path, Info)->
    %% Send the file!
    %% Find the modification date of the file
    case file:open(Path,[raw,binary]) of
	{ok, FileDescriptor} ->
	    {FileInfo, LastModified} = get_modification_date(Path),
	    ?DEBUG("do_get -> FileDescriptor: ~p",[FileDescriptor]),
	    Suffix = httpd_util:suffix(Path),
	    MimeType = httpd_util:lookup_mime_default(Info#mod.config_db,
						      Suffix,"text/plain"),
	    %% FileInfo = file:read_file_info(Path),
	    Size = integer_to_list(FileInfo#file_info.size),
	    Headers = case Info#mod.http_version of
			 "HTTP/1.1" ->
			      [{content_type, MimeType},
			       {etag, httpd_util:create_etag(FileInfo)},
			       {content_length, Size}|LastModified];
			  %% OTP-4935
			 _ ->
			     %% i.e http/1.0 and http/0.9
			      [{content_type, MimeType},
			       {content_length, Size}|LastModified]
			  end,
	    send(Info, 200, Headers, FileDescriptor),
	    file:close(FileDescriptor),
	    {proceed,[{response,{already_sent,200,
				 FileInfo#file_info.size}},
		      {mime_type,MimeType} | Info#mod.data]};
	{error, Reason} ->
	    ?hdrt("send_response -> failed open file", 
		  [{path, Path}, {reason, Reason}]), 
	    Status = httpd_file:handle_error(Reason, "open", Info, Path),
	    {proceed, [{status, Status} | Info#mod.data]}
    end.

%% send
	       
send(#mod{socket = Socket, socket_type = SocketType} = Info,
     StatusCode, Headers, FileDescriptor) ->
    ?DEBUG("send -> send header",[]),
    httpd_response:send_header(Info, StatusCode, Headers),
    send_body(SocketType,Socket,FileDescriptor).


send_body(SocketType,Socket,FileDescriptor) ->
    case file:read(FileDescriptor,?FILE_CHUNK_SIZE) of
	{ok,Binary} ->
	    ?DEBUG("send_body -> send another chunk: ~p",[size(Binary)]),
	    case httpd_socket:deliver(SocketType,Socket,Binary) of
		socket_closed ->
		    ?LOG("send_body -> socket closed while sending",[]),
		    socket_close;
		_ ->
		    send_body(SocketType,Socket,FileDescriptor)
	    end;
	eof ->
	    ?DEBUG("send_body -> done with this file",[]),
	    eof
    end.

get_modification_date(Path)->
    {ok, FileInfo0} = file:read_file_info(Path), 
    LastModified = 
	case catch httpd_util:rfc1123_date(FileInfo0#file_info.mtime) of
	    Date when is_list(Date) -> [{last_modified, Date}];
	    _ -> []
	end,
    {FileInfo0, LastModified}.

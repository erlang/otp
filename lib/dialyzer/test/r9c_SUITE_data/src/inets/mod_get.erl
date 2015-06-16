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
%%     $Id: mod_get.erl,v 1.1 2008/12/17 09:53:35 mikpe Exp $
-module(mod_get).
-export([do/1]).
-include("httpd.hrl").

%% do

do(Info) ->
    ?DEBUG("do -> entry",[]),
    case Info#mod.method of
	"GET" ->
	    case httpd_util:key1search(Info#mod.data,status) of
		%% A status code has been generated!
		{StatusCode,PhraseArgs,Reason} ->
		    {proceed,Info#mod.data};
		%% No status code has been generated!
		undefined ->
		    case httpd_util:key1search(Info#mod.data,response) of
			%% No response has been generated!
			undefined ->
			    do_get(Info);
			%% A response has been generated or sent!
			Response ->
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
    {FileInfo, LastModified} =get_modification_date(Path),

    send_response(Info#mod.socket,Info#mod.socket_type,Path,Info,FileInfo,LastModified).


%%The common case when no range is specified
send_response(Socket,SocketType,Path,Info,FileInfo,LastModified)->
    %% Send the file!
    %% Find the modification date of the file
    case file:open(Path,[raw,binary]) of
	{ok, FileDescriptor} ->
	    ?DEBUG("do_get -> FileDescriptor: ~p",[FileDescriptor]),
	    Suffix = httpd_util:suffix(Path),
	    MimeType = httpd_util:lookup_mime_default(Info#mod.config_db,
						      Suffix,"text/plain"),
	    %FileInfo=file:read_file_info(Path),
	    Date = httpd_util:rfc1123_date(),
	    Size = integer_to_list(FileInfo#file_info.size),
	    Header=case Info#mod.http_version of
		       "HTTP/1.1" ->
			   [httpd_util:header(200, MimeType, Info#mod.connection),
			    "Last-Modified: ", LastModified, "\r\n",
			    "Etag: ",httpd_util:create_etag(FileInfo),"\r\n",
			    "Content-Length: ",Size,"\r\n\r\n"];
		       "HTTP/1.0" ->
			   [httpd_util:header(200, MimeType, Info#mod.connection),
			    "Last-Modified: ", LastModified, "\r\n",
			    "Content-Length: ",Size,"\r\n\r\n"]
		   end,

	    send(Info#mod.socket_type, Info#mod.socket,
		 Header, FileDescriptor),
	    file:close(FileDescriptor),
	    {proceed,[{response,{already_sent,200,
				 FileInfo#file_info.size}},
		      {mime_type,MimeType}|Info#mod.data]};
	{error, Reason} ->

	    {proceed,
	     [{status,open_error(Reason,Info,Path)}|Info#mod.data]}
    end.

%% send

send(SocketType,Socket,Header,FileDescriptor) ->
    ?DEBUG("send -> send header",[]),
    case httpd_socket:deliver(SocketType,Socket,Header) of
	socket_closed ->
	    ?LOG("send -> socket closed while sending header",[]),
	    socket_close;
	_ ->
	    send_body(SocketType,Socket,FileDescriptor)
    end.

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


%% open_error - Handle file open failure
%%
open_error(eacces,Info,Path) ->
    open_error(403,Info,Path,"");
open_error(enoent,Info,Path) ->
    open_error(404,Info,Path,"");
open_error(enotdir,Info,Path) ->
    open_error(404,Info,Path,
	       ": A component of the file name is not a directory");
open_error(emfile,_Info,Path) ->
    open_error(500,none,Path,": To many open files");
open_error({enfile,_},_Info,Path) ->
    open_error(500,none,Path,": File table overflow");
open_error(_Reason,_Info,Path) ->
    open_error(500,none,Path,"").

open_error(StatusCode,none,Path,Reason) ->
    {StatusCode,none,?NICE("Can't open "++Path++Reason)};
open_error(StatusCode,Info,Path,Reason) ->
    {StatusCode,Info#mod.request_uri,?NICE("Can't open "++Path++Reason)}.

get_modification_date(Path)->
    case file:read_file_info(Path) of
	{ok, FileInfo0} ->
	    {FileInfo0, httpd_util:rfc1123_date(FileInfo0#file_info.mtime)};
	_ ->
	    {#file_info{},""}
    end.

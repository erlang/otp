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
%%     $Id: mod_head.erl,v 1.1 2008/12/17 09:53:35 mikpe Exp $
-module(mod_head).
-export([do/1]).

-include("httpd.hrl").

%% do

do(Info) ->
    ?DEBUG("do -> entry",[]),
    case Info#mod.method of
	"HEAD" ->
	    case httpd_util:key1search(Info#mod.data,status) of
		%% A status code has been generated!
		{StatusCode,PhraseArgs,Reason} ->
		    {proceed,Info#mod.data};
		%% No status code has been generated!
		_undefined ->
		    case httpd_util:key1search(Info#mod.data,response) of
			%% No response has been generated!
			undefined ->
			    do_head(Info);
			%% A response has been sent! Nothing to do about it!
			{already_sent,StatusCode,Size} ->
			    {proceed,Info#mod.data};
			%% A response has been generated!
			{StatusCode,Response} ->
			    {proceed,Info#mod.data}
		    end
	    end;
	%% Not a HEAD method!
	_ ->
	    {proceed,Info#mod.data}
    end.

do_head(Info) ->
    ?DEBUG("do_head -> Request URI: ~p",[Info#mod.request_uri]),
    Path = mod_alias:path(Info#mod.data,Info#mod.config_db,
			  Info#mod.request_uri),
    Suffix = httpd_util:suffix(Path),
    %% Does the file exists?
    case file:read_file_info(Path) of
	{ok,FileInfo} ->
	    MimeType=httpd_util:lookup_mime_default(Info#mod.config_db,Suffix,"text/plain"),
	    Length=io_lib:write(FileInfo#file_info.size),
	    Head=[{content_type,MimeType},{content_length,Length},{code,200}],
	    {proceed,[{response,{response,Head,nobody}}|Info#mod.data]};
	{error,Reason} ->
	    {proceed,
	     [{status,read_file_info_error(Reason,Info,Path)}|Info#mod.data]}
    end.

%% read_file_info_error - Handle file info read failure
%%
read_file_info_error(eacces,Info,Path) ->
    read_file_info_error(403,Info,Path,"");
read_file_info_error(enoent,Info,Path) ->
    read_file_info_error(404,Info,Path,"");
read_file_info_error(enotdir,Info,Path) ->
    read_file_info_error(404,Info,Path,
			 ": A component of the file name is not a directory");
read_file_info_error(emfile,_Info,Path) ->
    read_file_info_error(500,none,Path,": To many open files");
read_file_info_error({enfile,_},_Info,Path) ->
    read_file_info_error(500,none,Path,": File table overflow");
read_file_info_error(_Reason,_Info,Path) ->
    read_file_info_error(500,none,Path,"").

read_file_info_error(StatusCode,none,Path,Reason) ->
    {StatusCode,none,?NICE("Can't access "++Path++Reason)};
read_file_info_error(StatusCode,Info,Path,Reason) ->
    {StatusCode,Info#mod.request_uri,
     ?NICE("Can't access "++Path++Reason)}.

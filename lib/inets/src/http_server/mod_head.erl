%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
-module(mod_head).
-export([do/1]).

-include("httpd.hrl").

-define(VMODULE,"HEAD").

%% do

do(Info) ->
    case Info#mod.method of
	"HEAD" ->
	    case proplists:get_value(status, Info#mod.data) of
		%% A status code has been generated!
		{_StatusCode, _PhraseArgs, _Reason} ->
		    {proceed,Info#mod.data};
		%% No status code has been generated!
		_undefined ->
		    case proplists:get_value(response, Info#mod.data) of
			%% No response has been generated!
			undefined ->
			    do_head(Info);
			%% A response has been sent! Nothing to do about it!
			{already_sent, _StatusCode, _Size} ->
			    {proceed,Info#mod.data};
			%% A response has been generated!
			{_StatusCode, _Response} ->
			    {proceed,Info#mod.data}
		    end
	    end;
	%% Not a HEAD method!
	_ ->
	    {proceed,Info#mod.data}
    end.

do_head(Info) -> 
    Path = mod_alias:path(Info#mod.data,
			  Info#mod.config_db,
			  Info#mod.request_uri),
    Suffix = httpd_util:suffix(Path),
    %% Does the file exists?
    case file:read_file_info(Path) of
	{ok, FileInfo} ->
	    MimeType = 
		httpd_util:lookup_mime_default(Info#mod.config_db,
					       Suffix,"text/plain"),
	    Length = io_lib:write(FileInfo#file_info.size),
	    Head = 
		[{content_type, MimeType},
		 {content_length, Length}, {code,200}],
	    {proceed,[{response, {response, Head,  nobody}} | Info#mod.data]};
	{error, Reason} ->
	    Status = httpd_file:handle_error(Reason, "access", Info, Path),
	    {proceed,
	     [{status, Status} | Info#mod.data]}
    end.

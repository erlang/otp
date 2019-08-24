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
-module(mod_dir).

-include("httpd.hrl").
-include("httpd_internal.hrl").

-export([do/1]).

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
			    do_dir(Info);
			%% A response has been generated or sent!
			_Response ->
			    {proceed,Info#mod.data}
		    end
	    end;
	%% Not a GET method!
	_ ->
	    {proceed,Info#mod.data}
    end.

do_dir(Info) ->
    ?DEBUG("do_dir -> Request URI: ~p",[Info#mod.request_uri]),
    Path = mod_alias:path(Info#mod.data,Info#mod.config_db,
			  Info#mod.request_uri),
    DefaultPath = mod_alias:default_index(Info#mod.config_db,Path),
    %% Is it a directory?
    case file:read_file_info(DefaultPath) of
	{ok,FileInfo} when FileInfo#file_info.type == directory ->
	    DecodedRequestURI =
		http_uri:decode(Info#mod.request_uri),
	    ?DEBUG("do_dir -> ~n"
		   "      Path:              ~p~n"
		   "      DefaultPath:       ~p~n"
		   "      DecodedRequestURI: ~p",
		   [Path,DefaultPath,DecodedRequestURI]),
	    case dir(DefaultPath,string:strip(DecodedRequestURI,right,$/),
		     Info#mod.config_db) of
		{ok, Dir} ->
		    LastModified =
			case (catch httpd_util:rfc1123_date(
				      FileInfo#file_info.mtime)) of
			    Date when is_list(Date) ->
				[{"date", Date}];
			    _ -> %% This will rarly happen, but could happen
				%% if a computer is wrongly configured. 
				[]
			end,
		    Head=[{content_type,"text/html"},
			  {content_length,
			   integer_to_list(httpd_util:flatlength(Dir))},
			  {code,200} | LastModified],
		    {proceed,[{response,{response, Head, Dir}},
			      {mime_type,"text/html"} | Info#mod.data]};
		{error, Reason} ->
		    ?ERROR("do_dir -> dir operation failed: ~p",[Reason]),
		    {proceed,
		     [{status,{404,Info#mod.request_uri,Reason}}|
		      Info#mod.data]}
	    end;
	{ok, _FileInfo} ->
	    ?DEBUG("do_dir -> ~n"
		   "      Path:        ~p~n"
		   "      DefaultPath: ~p~n"
		   "      FileInfo:    ~p",
		   [Path,DefaultPath,FileInfo]),
	    {proceed,Info#mod.data};
	{error,Reason} ->
	    ?LOG("do_dir -> failed reading file info (~p) for: ~p",
		 [Reason,DefaultPath]),
	    Status = httpd_file:handle_error(Reason, "access", Info,
					     DefaultPath),
	    {proceed, [{status, Status}| Info#mod.data]}
    end.

dir(Path,RequestURI,ConfigDB) ->
    case file:list_dir(Path) of
	{ok,FileList} ->
	    SortedFileList=lists:sort(FileList),
	    {ok,[header(Path,RequestURI),
		 body(Path,RequestURI,ConfigDB,SortedFileList),
		 footer(Path,SortedFileList)]};
	{error,Reason} ->
	    {error,?NICE("Can't open directory "++Path++": "++
			 file:format_error(Reason))}
    end.

%% header

header(Path,RequestURI) ->
    Header = "<HTML>\n<HEAD>\n<TITLE>Index of "++ RequestURI ++
	"</TITLE>\n</HEAD>\n<BODY>\n<H1>Index of "++
	RequestURI ++ "</H1>\n<PRE><IMG SRC=\"" ++ icon(blank) ++
	"\" ALT="     "> Name                   Last modified         "
	"Size  Description <HR>\n",
    case re:replace(RequestURI,"[^/]*\$","", [{return,list}]) of
	"/" ->
	    Header;
	ParentRequestURI ->
	    ParentPath =
		re:replace(string:strip(Path,right,$/),"[^/]*\$","",
			   [{return,list}]),
	    Header++format(ParentPath,ParentRequestURI)
    end.

format(Path,RequestURI) ->
    {ok,FileInfo}=file:read_file_info(Path),
    {{Year, Month, Day},{Hour, Minute, _Second}} = FileInfo#file_info.mtime,
    io_lib:format("<IMG SRC=\"~s\" ALT=\"[~s]\">"
		  " <A HREF=\"~s\">Parent directory</A>      "
		  " ~2.2.0w-~s-~w ~2.2.0w:~2.2.0w        -\n",
		  [icon(back),"DIR",RequestURI,Day,
		   httpd_util:month(Month),Year,Hour,Minute]).

%% body

body(_Path, _RequestURI, _ConfigDB, []) ->
    [];
body(Path, RequestURI, ConfigDB, [Entry | Rest]) ->
    [format(Path, RequestURI, ConfigDB, Entry)|
     body(Path, RequestURI, ConfigDB, Rest)].

format(Path,RequestURI,ConfigDB,Entry) ->
    case file:read_file_info(Path++"/"++Entry) of
	{ok,FileInfo} when FileInfo#file_info.type == directory ->
	    {{Year, Month, Day},{Hour, Minute, _Second}} = 
		FileInfo#file_info.mtime,
	    EntryLength=length(Entry),
	    if
		EntryLength > 21 ->
		    io_lib:format("<IMG SRC=\"~s\" ALT=\"[~s]\"> "
				  "<A HREF=\"~s\">~-21.s..</A>"
				  "~2.2.0w-~s-~w ~2.2.0w:~2.2.0w"
				  "        -\n", [icon(folder),"DIR",
						  RequestURI++"/"++Entry++"/",
						  Entry,
						  Day, httpd_util:month(Month),
						  Year,Hour,Minute]);
		true ->
		    io_lib:format("<IMG SRC=\"~s\" ALT=\"[~s]\">"
				  " <A HREF=\"~s\">~s</A>~*.*c~2.2.0"
				  "w-~s-~w ~2.2.0w:~2.2.0w        -\n",
				  [icon(folder),"DIR",RequestURI ++ "/" ++
				   Entry ++ "/",Entry,
				   23-EntryLength,23-EntryLength,$ ,Day,
				   httpd_util:month(Month),Year,Hour,Minute])
	    end;
	{ok,FileInfo} ->
	    {{Year, Month, Day},{Hour, Minute,_Second}} =
		FileInfo#file_info.mtime,
	    Suffix=httpd_util:suffix(Entry),
	    MimeType=httpd_util:lookup_mime(ConfigDB,Suffix,""),
	    EntryLength=length(Entry),
	    if
		EntryLength > 21 ->
		    io_lib:format("<IMG SRC=\"~s\" ALT=\"[~s]\">"
				  " <A HREF=\"~s\">~-21.s..</A>~2.2.0"
				  "w-~s-~w ~2.2.0w:~2.2.0w~8wk  ~s\n",
				  [icon(Suffix, MimeType), Suffix, RequestURI 
				   ++"/"++Entry, Entry,Day,
				   httpd_util:month(Month),Year,Hour,Minute,
				   trunc(FileInfo#file_info.size/1024+1),
				   MimeType]);
		true ->
		    io_lib:format("<IMG SRC=\"~s\" ALT=\"[~s]\"> "
				  "<A HREF=\"~s\">~s</A>~*.*c~2.2.0w-~s-~w"
				  " ~2.2.0w:~2.2.0w~8wk  ~s\n",
				  [icon(Suffix, MimeType), Suffix, RequestURI
				   ++ "/" ++ Entry, Entry, 23-EntryLength,
				   23-EntryLength, $ ,Day,
				   httpd_util:month(Month),Year,Hour,Minute,
				   trunc(FileInfo#file_info.size/1024+1),
				   MimeType])
	    end;
	{error, _Reason} ->
	    ""
    end.

%% footer

footer(Path,FileList) ->
    case lists:member("README",FileList) of
	true ->
	    {ok,Body}=file:read_file(Path++"/README"),
	    "</PRE>\n<HR>\n<PRE>\n"++binary_to_list(Body)++
		"\n</PRE>\n</BODY>\n</HTML>\n";
	false ->
	    "</PRE>\n</BODY>\n</HTML>\n"
    end.

%%
%% Icon mappings are hard-wired ala default Apache (Ugly!)
%%

icon(Suffix,MimeType) ->
    case icon(Suffix) of
	undefined ->
	    case MimeType of
		[$t,$e,$x,$t,$/|_] ->
		    "/icons/text.gif";
		[$i,$m,$a,$g,$e,$/|_] ->
		    "/icons/image2.gif";
		[$a,$u,$d,$i,$o,$/|_] ->
		    "/icons/sound2.gif";
		[$v,$i,$d,$e,$o,$/|_] ->
		    "/icons/movie.gif";
		_ ->
		    "/icons/unknown.gif"
	    end;
	Icon ->
	    Icon
    end.

icon(blank) -> "/icons/blank.gif";
icon(back) -> "/icons/back.gif";
icon(folder) -> "/icons/folder.gif";
icon("bin") -> "/icons/binary.gif";
icon("exe") -> "/icons/binary.gif";
icon("hqx") -> "/icons/binhex.gif";
icon("tar") -> "/icons/tar.gif";
icon("wrl") -> "/icons/world2.gif";
icon("wrl.gz") -> "/icons/world2.gif";
icon("vrml") -> "/icons/world2.gif";
icon("vrm") -> "/icons/world2.gif";
icon("iv") -> "/icons/world2.gif";
icon("Z") -> "/icons/compressed.gif";
icon("z") -> "/icons/compressed.gif";
icon("tgz") -> "/icons/compressed.gif";
icon("gz") -> "/icons/compressed.gif";
icon("zip") -> "/icons/compressed.gif";
icon("ps") -> "/icons/a.gif";
icon("ai") -> "/icons/a.gif";
icon("eps") -> "/icons/a.gif";
icon("html") -> "/icons/layout.gif";
icon("shtml") -> "/icons/layout.gif";
icon("htm") -> "/icons/layout.gif";
icon("pdf") -> "/icons/layout.gif";
icon("txt") -> "/icons/text.gif";
icon("erl") -> "/icons/burst.gif";
icon("c") -> "/icons/c.gif";
icon("pl") -> "/icons/p.gif";
icon("py") -> "/icons/p.gif";
icon("for") -> "/icons/f.gif";
icon("dvi") -> "/icons/dvi.gif";
icon("uu") -> "/icons/uuencoded.gif";
icon("conf") -> "/icons/script.gif";
icon("sh") -> "/icons/script.gif";
icon("shar") -> "/icons/script.gif";
icon("csh") -> "/icons/script.gif";
icon("ksh") -> "/icons/script.gif";
icon("tcl") -> "/icons/script.gif";
icon("tex") -> "/icons/tex.gif";
icon("core") -> "/icons/tex.gif";
icon(_) -> undefined.



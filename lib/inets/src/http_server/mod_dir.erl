%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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
-moduledoc false.

-include("httpd.hrl").
-include("httpd_internal.hrl").

-export([do/1]).

%% do

do(Info) ->
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
    Path = mod_alias:path(Info#mod.data,Info#mod.config_db,
			  Info#mod.request_uri),
    DefaultPath = mod_alias:default_index(Info#mod.config_db,Path),
    %% Is it a directory?
    case file:read_file_info(DefaultPath) of
	{ok,FileInfo} when FileInfo#file_info.type == directory ->
	    case dir(DefaultPath,string:strip( Info#mod.request_uri,right,$/),
		     Info#mod.config_db) of
		{ok, DirUnicode} ->
		    Dir = unicode:characters_to_binary(DirUnicode),
		    LastModified =
			case (catch httpd_util:rfc1123_date(
				      FileInfo#file_info.mtime)) of
			    Date when is_list(Date) ->
				[{"date", Date}];
			    _ -> %% This will rarly happen, but could happen
				%% if a computer is wrongly configured. 
				[]
			end,
		    Head=[{content_type,"text/html; charset=UTF-8"},
			  {content_length,
			   integer_to_list(erlang:iolist_size(Dir))},
			  {code,200} | LastModified],
		    {proceed,[{response,{response, Head, Dir}},
			      {mime_type,"text/html"} | Info#mod.data]};
		{error, Reason} ->
		    {proceed,
		     [{status,{404,Info#mod.request_uri,Reason}}|
		      Info#mod.data]}
	    end;
	{ok, _FileInfo} ->
	    {proceed,Info#mod.data};
	{error,Reason} ->
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

encode_html_entity(FileName) ->
	Enc = fun($&) -> "&amp;";
	         ($<) -> "&lt;";
			 ($>) -> "&gt;";
			 ($") -> "&quot;";
			 ($') -> "&#x27;";
			 ($/) -> "&#x2F;";
			 (C)  -> C
		  end,
	unicode:characters_to_list([Enc(C) || C <- FileName]).

%% header

header(Path,RequestURI) ->
    DisplayURI = case RequestURI of
                     "" -> "/";
                     _  -> RequestURI
                 end,
    Header = "<!DOCTYPE html>\n"
	"<HTML>\n<HEAD>\n"
	"<meta charset=\"UTF-8\">"
	"<TITLE>Index of " ++ DisplayURI ++ "</TITLE>\n"
	"</HEAD>\n<BODY>\n<H1>Index of "++
	DisplayURI ++ "</H1>\n<PRE><span>" ++ icon(blank) ++
	"</span> Name                   Last modified         "
	"Size  Description <HR>\n",
    case RequestURI of
	"" ->
	    Header;
	_ ->
            ParentRequestURI = re:replace(RequestURI,"[^/]*\$", "",
                                          [{return,list}]),
	    ParentPath =
		re:replace(string:strip(Path,right,$/),"[^/]*\$","",
			   [{return,list}]),
	    Header++format(ParentPath,ParentRequestURI)
    end.

format(Path,RequestURI) ->
    {ok,FileInfo}=file:read_file_info(Path),
    {{Year, Month, Day},{Hour, Minute, _Second}} = FileInfo#file_info.mtime,
    io_lib:format("<span title=\"~s\">~s</span>"
		  " <A HREF=\"~ts\">Parent directory</A>      "
		  " ~2.2.0w-~s-~w ~2.2.0w:~2.2.0w        -\n",
		  ["DIR",icon(back),RequestURI,Day,
		   httpd_util:month(Month),Year,Hour,Minute]).

%% body

body(_Path, _RequestURI, _ConfigDB, []) ->
    [];
body(Path, RequestURI, ConfigDB, [Entry | Rest]) ->
    [format(Path, RequestURI, ConfigDB, Entry)|
     body(Path, RequestURI, ConfigDB, Rest)].

format(Path,RequestURI,ConfigDB,InitEntry) ->
	Entry = encode_html_entity(InitEntry),
    case file:read_file_info(Path++"/"++InitEntry) of
	{ok,FileInfo} when FileInfo#file_info.type == directory ->
	    {{Year, Month, Day},{Hour, Minute, _Second}} = 
		FileInfo#file_info.mtime,
	    EntryLength = string:length(InitEntry),
	    if
		EntryLength > 21 ->
		    TruncatedEntry = encode_html_entity(
                                       string:slice(InitEntry, 0, 19)),
		    io_lib:format("<span title=\"[~s]\">~s</span> "
				  "<A HREF=\"~ts\">~ts..</A>"
				  "~*.*c~2.2.0w-~s-~w ~2.2.0w:~2.2.0w"
				  "        -\n",
                                  ["DIR", icon(folder),
                                   RequestURI ++ "/" ++ percent_encode(InitEntry) ++ "/",
                                   TruncatedEntry, 23-21, 23-21, $ ,
                                   Day, httpd_util:month(Month),
                                   Year,Hour,Minute]);
		true ->
		    io_lib:format("<span title=\"[~s]\">~s</span>"
				  " <A HREF=\"~ts\">~ts</A>~*.*c~2.2.0"
				  "w-~s-~w ~2.2.0w:~2.2.0w        -\n",
				  ["DIR", icon(folder),
                                   RequestURI ++ "/" ++ percent_encode(InitEntry) ++ "/",Entry,
				   23-EntryLength,23-EntryLength,$ ,Day,
				   httpd_util:month(Month),Year,Hour,Minute])
	    end;
	{ok,FileInfo} ->
	    {{Year, Month, Day},{Hour, Minute,_Second}} =
		FileInfo#file_info.mtime,
	    Suffix=httpd_util:strip_extension_dot(Entry),
	    MimeType=httpd_util:lookup_mime(ConfigDB,Suffix,""),
	    EntryLength = string:length(InitEntry),
	    if
		EntryLength > 21 ->
		    TruncatedEntry = encode_html_entity(
                                       string:slice(InitEntry, 0, 19)),
		    io_lib:format("<span title=\"[~s]\">~s</span>"
				  " <A HREF=\"~ts\">~ts..</A>~*.*c~2.2.0"
				  "w-~s-~w ~2.2.0w:~2.2.0w~8wk  ~s\n",
				  [Suffix, icon(Suffix, MimeType),
                                   RequestURI ++ "/" ++ percent_encode(InitEntry),
                                   TruncatedEntry, 23-21, 23-21, $ , Day,
				   httpd_util:month(Month),Year,Hour,Minute,
				   trunc(FileInfo#file_info.size/1024+1),
				   MimeType]);
		true ->
		    io_lib:format("<span title=\"[~s]\">~s</span> "
				  "<A HREF=\"~ts\">~ts</A>~*.*c~2.2.0w-~s-~w"
				  " ~2.2.0w:~2.2.0w~8wk  ~s\n",
				  [Suffix, icon(Suffix, MimeType),
                                   RequestURI ++ "/" ++ percent_encode(InitEntry), Entry, 23-EntryLength,
				   23-EntryLength, $ ,Day,
				   httpd_util:month(Month),Year,Hour,Minute,
				   trunc(FileInfo#file_info.size/1024+1),
				   MimeType])
	    end;
	{error, _Reason} ->
	    ""
    end.

percent_encode(URI) when is_list(URI) ->
    Reserved = reserved(),
    lists:append([uri_encode(Char, Reserved) || Char <- URI]).

reserved() ->
    sets:from_list([$;, $:, $@, $&, $=, $+, $,, $?, $/,
            $#, $[, $], $<, $>, $\", ${, $}, $|, %"
			       $\\, $', $^, $%, $ ]).

uri_encode(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
	true ->
	    [ $% | http_util:integer_to_hexlist(Char)];
	false ->
	    [Char]
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
%% Icon mappings (Emoji)
%%

-define(package, "&#x1F4E6;").                  % package
-define(image, "&#x1F4F7;").                    % camera
-define(audio, "&#x1F4E2;").                    % loudspeaker
-define(video, "&#x1F3A5;").                    % movie camera

-define(page, "&#x1F4C4;").                     % page
-define(page2, "&#x1F4C3;").                    % page, curled
-define(world, "&#x1F30D;").                    % globe
-define(unknown, ?page).
-define(text, "&#x1F4DD;").                     % page with pencil
-define(sourcecode, "&#x1F4DC;").               % scroll

icon(Suffix,MimeType) ->
    case icon(Suffix) of
	undefined ->
	    case MimeType of
		[$t,$e,$x,$t,$/|_] ->
		    ?text;
		[$i,$m,$a,$g,$e,$/|_] ->
		    ?image;
		[$a,$u,$d,$i,$o,$/|_] ->
		    ?audio;
		[$v,$i,$d,$e,$o,$/|_] ->
		    ?video;
		_ ->
		    ?unknown
	    end;
	Icon ->
	    Icon
    end.

icon(blank) -> "&#x1F4C2;"; % open folder
icon(back) -> "&#x1F519;"; % back arrow
icon(folder) -> "&#x1F4C1;"; % closed folder
icon("bin") -> ?page2;
icon("exe") -> ?page2;
icon("hqx") -> ?page2;
icon("tar") -> ?package;
icon("wrl") -> ?world;
icon("wrl.gz") -> ?world;
icon("vrml") -> ?world;
icon("vrm") -> ?world;
icon("iv") -> ?world;
icon("Z") -> ?package;
icon("z") -> ?package;
icon("tgz") -> ?package;
icon("gz") -> ?package;
icon("zip") -> ?package;
icon("bz2") -> ?package;
icon("ps") -> ?page;
icon("ai") -> ?image;
icon("eps") -> ?image;
icon("html") -> ?text;
icon("shtml") -> ?text;
icon("htm") -> ?text;
icon("pdf") -> ?text;
icon("txt") -> ?text;
icon("erl") -> ?sourcecode;
icon("c") -> ?sourcecode;
icon("pl") -> ?sourcecode;
icon("py") -> ?sourcecode;
icon("for") -> ?sourcecode;
icon("dvi") -> ?text;
icon("conf") -> ?sourcecode;
icon("sh") -> ?sourcecode;
icon("shar") -> ?sourcecode;
icon("csh") -> ?sourcecode;
icon("ksh") -> ?sourcecode;
icon("tcl") -> ?sourcecode;
icon("tex") -> ?sourcecode;
icon("core") -> ?sourcecode;
icon("xml") -> ?sourcecode;
icon("jpg") -> ?image;
icon("JPG") -> ?image;
icon("jpeg") -> ?image;
icon("png") -> ?image;
icon("gif") -> ?image;
icon("avi") -> ?video;
icon("mp4") -> ?video;
icon("m4a") -> ?audio;
icon("mp3") -> ?audio;
icon("aac") -> ?audio;
icon("flac") -> ?audio;
icon(_) -> undefined.

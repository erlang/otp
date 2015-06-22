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
%%     $Id: mod_include.erl,v 1.1 2008/12/17 09:53:35 mikpe Exp $
-module(mod_include).
-export([do/1,parse/2,config/6,include/6,echo/6,fsize/6,flastmod/6,exec/6]).

-include("httpd.hrl").

-define(VMODULE,"INCLUDE").
-include("httpd_verbosity.hrl").

%% do

do(Info) ->
    ?vtrace("do",[]),
    case Info#mod.method of
	"GET" ->
	    case httpd_util:key1search(Info#mod.data,status) of
		%% A status code has been generated!
		{StatusCode,PhraseArgs,Reason} ->
		    {proceed,Info#mod.data};
		%% No status code has been generated!
		undefined ->
		    case httpd_util:key1search(Info#mod.data, response) of
			%% No response has been generated!
			undefined ->
			    do_include(Info);
			%% A response has been generated or sent!
			Response ->
			    {proceed,Info#mod.data}
		    end
	    end;
	%% Not a GET method!
	_ ->
	    {proceed,Info#mod.data}
    end.

do_include(Info) ->
    ?vtrace("do_include -> entry with"
	    "~n   URI: ~p",[Info#mod.request_uri]),
    Path = mod_alias:path(Info#mod.data,Info#mod.config_db,
			  Info#mod.request_uri),
    Suffix = httpd_util:suffix(Path),
    case httpd_util:lookup_mime_default(Info#mod.config_db,Suffix) of
	"text/x-server-parsed-html" ->
	    HeaderStart =
		httpd_util:header(200, "text/html", Info#mod.connection),
	    ?vtrace("do_include -> send ~p", [Path]),
	    case send_in(Info,Path,HeaderStart,file:read_file_info(Path)) of
		{ok, ErrorLog, Size} ->
		    ?vtrace("do_include -> sent ~w bytes", [Size]),
		    {proceed,[{response,{already_sent,200,Size}},
			      {mime_type,"text/html"}|
			      lists:append(ErrorLog,Info#mod.data)]};
		{error, Reason} ->
		    ?vlog("send in failed:"
			  "~n   Reason: ~p"
			  "~n   Path:   ~p"
			  "~n   Info:   ~p",
			  [Reason,Info,Path]),
		    {proceed,
		     [{status,send_error(Reason,Info,Path)}|Info#mod.data]}
	    end;
	_ -> %% Unknown mime type, ignore
	    {proceed,Info#mod.data}
    end.


%%
%% config directive
%%

config(Info, Context, ErrorLog, TagList, ValueList, R) ->
    case verify_tags("config",[errmsg,timefmt,sizefmt],
		     TagList,ValueList) of
	ok ->
	    {ok,update_context(TagList,ValueList,Context),ErrorLog,"",R};
	{error,Reason} ->
	    {ok,Context,[{internal_info,Reason}|ErrorLog],
	     httpd_util:key1search(Context,errmsg,""),R}
    end.

update_context([],[],Context) ->
    Context;
update_context([Tag|R1],[Value|R2],Context) ->
    update_context(R1,R2,[{Tag,Value}|Context]).

verify_tags(Command,ValidTags,TagList,ValueList) when length(TagList)==length(ValueList) ->
    verify_tags(Command,ValidTags,TagList);
verify_tags(Command,ValidTags,TagList,ValueList) ->
    {error,?NICE(Command++" directive has spurious tags")}.

verify_tags(Command, ValidTags, []) ->
    ok;
verify_tags(Command, ValidTags, [Tag|Rest]) ->
    case lists:member(Tag, ValidTags) of
	true ->
	    verify_tags(Command, ValidTags, Rest);
	false ->
	    {error,?NICE(Command++" directive has a spurious tag ("++
			 atom_to_list(Tag)++")")}
    end.

%%
%% include directive
%%

include(Info,Context,ErrorLog,[virtual],[VirtualPath],R) ->
    Aliases = httpd_util:multi_lookup(Info#mod.config_db,alias),
    {_, Path, _AfterPath} =
	mod_alias:real_name(Info#mod.config_db, VirtualPath, Aliases),
    include(Info,Context,ErrorLog,R,Path);
include(Info, Context, ErrorLog, [file], [FileName], R) ->
    Path = file(Info#mod.config_db, Info#mod.request_uri, FileName),
    include(Info, Context, ErrorLog, R, Path);
include(Info, Context, ErrorLog, TagList, ValueList, R) ->
    {ok, Context,
     [{internal_info,?NICE("include directive has a spurious tag")}|
      ErrorLog], httpd_util:key1search(Context, errmsg, ""), R}.

include(Info, Context, ErrorLog, R, Path) ->
    ?DEBUG("include -> read file: ~p",[Path]),
    case file:read_file(Path) of
	{ok, Body} ->
	    ?DEBUG("include -> size(Body): ~p",[size(Body)]),
	    {ok, NewContext, NewErrorLog, Result} =
		parse(Info, binary_to_list(Body), Context, ErrorLog, []),
	    {ok, Context, NewErrorLog, Result, R};
	{error, Reason} ->
	    {ok, Context,
	     [{internal_info, ?NICE("Can't open "++Path)}|ErrorLog],
	     httpd_util:key1search(Context, errmsg, ""), R}
    end.

file(ConfigDB, RequestURI, FileName) ->
    Aliases = httpd_util:multi_lookup(ConfigDB, alias),
    {_, Path, _AfterPath}
	= mod_alias:real_name(ConfigDB, RequestURI, Aliases),
    Pwd = filename:dirname(Path),
    filename:join(Pwd, FileName).

%%
%% echo directive
%%

echo(Info,Context,ErrorLog,[var],["DOCUMENT_NAME"],R) ->
    {ok,Context,ErrorLog,document_name(Info#mod.data,Info#mod.config_db,
				       Info#mod.request_uri),R};
echo(Info,Context,ErrorLog,[var],["DOCUMENT_URI"],R) ->
    {ok,Context,ErrorLog,document_uri(Info#mod.config_db,
				      Info#mod.request_uri),R};
echo(Info,Context,ErrorLog,[var],["QUERY_STRING_UNESCAPED"],R) ->
    {ok,Context,ErrorLog,query_string_unescaped(Info#mod.request_uri),R};
echo(Info,Context,ErrorLog,[var],["DATE_LOCAL"],R) ->
    {ok,Context,ErrorLog,date_local(),R};
echo(Info,Context,ErrorLog,[var],["DATE_GMT"],R) ->
    {ok,Context,ErrorLog,date_gmt(),R};
echo(Info,Context,ErrorLog,[var],["LAST_MODIFIED"],R) ->
    {ok,Context,ErrorLog,last_modified(Info#mod.data,Info#mod.config_db,
				       Info#mod.request_uri),R};
echo(Info,Context,ErrorLog,TagList,ValueList,R) ->
    {ok,Context,
     [{internal_info,?NICE("echo directive has a spurious tag")}|
      ErrorLog],"(none)",R}.

document_name(Data,ConfigDB,RequestURI) ->
    Path = mod_alias:path(Data,ConfigDB,RequestURI),
    case regexp:match(Path,"[^/]*\$") of
	{match,Start,Length} ->
	    string:substr(Path,Start,Length);
	nomatch ->
	    "(none)"
    end.

document_uri(ConfigDB, RequestURI) ->
    Aliases = httpd_util:multi_lookup(ConfigDB, alias),
    {Path, AfterPath} =
	case mod_alias:real_name(ConfigDB, RequestURI, Aliases) of
	    {_, Name, {[], []}} ->
		{Name, ""};
	    {_, Name, {PathInfo, []}} ->
		{Name, "/"++PathInfo};
	    {_, Name, {PathInfo, QueryString}} ->
		{Name, "/"++PathInfo++"?"++QueryString};
	    {_, Name, _} ->
		{Name, ""};
	    Gurka ->
		io:format("Gurka: ~p~n", [Gurka])
	end,
    VirtualPath = string:substr(RequestURI, 1,
				length(RequestURI)-length(AfterPath)),
    {match, Start, Length} = regexp:match(Path,"[^/]*\$"),
    FileName = string:substr(Path,Start,Length),
    case regexp:match(VirtualPath, FileName++"\$") of
	{match, _, _} ->
	    httpd_util:decode_hex(VirtualPath)++AfterPath;
	nomatch ->
	    string:strip(httpd_util:decode_hex(VirtualPath),right,$/)++
		"/"++FileName++AfterPath
    end.

query_string_unescaped(RequestURI) ->
  case regexp:match(RequestURI,"[\?].*\$") of
    {match,Start,Length} ->
      %% Escape all shell-special variables with \
      escape(string:substr(RequestURI,Start+1,Length-1));
    nomatch ->
      "(none)"
  end.

escape([]) -> [];
escape([$;|R]) -> [$\\,$;|escape(R)];
escape([$&|R]) -> [$\\,$&|escape(R)];
escape([$(|R]) -> [$\\,$(|escape(R)];
escape([$)|R]) -> [$\\,$)|escape(R)];
escape([$||R]) -> [$\\,$||escape(R)];
escape([$^|R]) -> [$\\,$^|escape(R)];
escape([$<|R]) -> [$\\,$<|escape(R)];
escape([$>|R]) -> [$\\,$>|escape(R)];
escape([$\n|R]) -> [$\\,$\n|escape(R)];
escape([$ |R]) -> [$\\,$ |escape(R)];
escape([$\t|R]) -> [$\\,$\t|escape(R)];
escape([C|R]) -> [C|escape(R)].

date_local() ->
  {{Year,Month,Day},{Hour,Minute,Second}}=calendar:local_time(),
  %% Time format hard-wired to: "%a %b %e %T %Y" according to strftime(3)
  io_lib:format("~s ~s ~2w ~2.2.0w:~2.2.0w:~2.2.0w ~w",
		[httpd_util:day(calendar:day_of_the_week(Year,Month,Day)),
		 httpd_util:month(Month),Day,Hour,Minute,Second,Year]).

date_gmt() ->
  {{Year,Month,Day},{Hour,Minute,Second}}=calendar:universal_time(),
  %% Time format hard-wired to: "%a %b %e %T %Z %Y" according to strftime(3)
  io_lib:format("~s ~s ~2w ~2.2.0w:~2.2.0w:~2.2.0w GMT ~w",
		[httpd_util:day(calendar:day_of_the_week(Year,Month,Day)),
		 httpd_util:month(Month),Day,Hour,Minute,Second,Year]).

last_modified(Data,ConfigDB,RequestURI) ->
  {ok,FileInfo}=file:read_file_info(mod_alias:path(Data,ConfigDB,RequestURI)),
  {{Year,Month,Day},{Hour,Minute,Second}}=FileInfo#file_info.mtime,
  io_lib:format("~s ~s ~2w ~2.2.0w:~2.2.0w:~2.2.0w ~w",
		[httpd_util:day(calendar:day_of_the_week(Year,Month,Day)),
		 httpd_util:month(Month),Day,Hour,Minute,Second,Year]).

%%
%% fsize directive
%%

fsize(Info,Context,ErrorLog,[virtual],[VirtualPath],R) ->
  Aliases=httpd_util:multi_lookup(Info#mod.config_db,alias),
  {_,Path,AfterPath}=
    mod_alias:real_name(Info#mod.config_db,VirtualPath,Aliases),
  fsize(Info, Context, ErrorLog, R, Path);
fsize(Info,Context,ErrorLog,[file],[FileName],R) ->
  Path=file(Info#mod.config_db,Info#mod.request_uri,FileName),
  fsize(Info,Context,ErrorLog,R,Path);
fsize(Info,Context,ErrorLog,TagList,ValueList,R) ->
  {ok,Context,[{internal_info,?NICE("fsize directive has a spurious tag")}|
	       ErrorLog],httpd_util:key1search(Context,errmsg,""),R}.

fsize(Info,Context,ErrorLog,R,Path) ->
    case file:read_file_info(Path) of
	{ok,FileInfo} ->
	    case httpd_util:key1search(Context,sizefmt) of
		"bytes" ->
		    {ok,Context,ErrorLog,
		     integer_to_list(FileInfo#file_info.size),R};
		"abbrev" ->
		    Size = integer_to_list(trunc(FileInfo#file_info.size/1024+1))++"k",
		    {ok,Context,ErrorLog,Size,R};
		Value->
		    {ok,Context,
		     [{internal_info,
		       ?NICE("fsize directive has a spurious tag value ("++
			     Value++")")}|
		      ErrorLog],
		     httpd_util:key1search(Context, errmsg, ""), R}
	    end;
	{error,Reason} ->
	    {ok,Context,[{internal_info,?NICE("Can't open "++Path)}|ErrorLog],
	     httpd_util:key1search(Context,errmsg,""),R}
    end.

%%
%% flastmod directive
%%

flastmod(Info, Context, ErrorLog, [virtual], [VirtualPath],R) ->
    Aliases=httpd_util:multi_lookup(Info#mod.config_db,alias),
    {_,Path,AfterPath}=
	mod_alias:real_name(Info#mod.config_db,VirtualPath,Aliases),
    flastmod(Info,Context,ErrorLog,R,Path);
flastmod(Info, Context, ErrorLog, [file], [FileName], R) ->
    Path = file(Info#mod.config_db, Info#mod.request_uri, FileName),
    flastmod(Info, Context, ErrorLog, R, Path);
flastmod(Info,Context,ErrorLog,TagList,ValueList,R) ->
    {ok,Context,[{internal_info,?NICE("flastmod directive has a spurious tag")}|
		 ErrorLog],httpd_util:key1search(Context,errmsg,""),R}.

flastmod(Info,Context,ErrorLog,R,File) ->
    case file:read_file_info(File) of
	{ok,FileInfo} ->
	    {{Yr,Mon,Day},{Hour,Minute,Second}}=FileInfo#file_info.mtime,
	    Result=
		io_lib:format("~s ~s ~2w ~w:~w:~w ~w",
			      [httpd_util:day(
				 calendar:day_of_the_week(Yr,Mon, Day)),
			       httpd_util:month(Mon),Day,Hour,Minute,Second, Yr]),
	    {ok,Context,ErrorLog,Result,R};
	{error,Reason} ->
	    {ok,Context,[{internal_info,?NICE("Can't open "++File)}|ErrorLog],
	     httpd_util:key1search(Context,errmsg,""),R}
    end.

%%
%% exec directive
%%

exec(Info,Context,ErrorLog,[cmd],[Command],R) ->
    ?vtrace("exec cmd:~n   Command: ~p",[Command]),
    cmd(Info,Context,ErrorLog,R,Command);
exec(Info,Context,ErrorLog,[cgi],[RequestURI],R) ->
    ?vtrace("exec cgi:~n   RequestURI: ~p",[RequestURI]),
    cgi(Info,Context,ErrorLog,R,RequestURI);
exec(Info,Context,ErrorLog,TagList,ValueList,R) ->
    ?vtrace("exec with spurious tag:"
	    "~n   TagList:   ~p"
	    "~n   ValueList: ~p",
	    [TagList,ValueList]),
    {ok, Context,
     [{internal_info,?NICE("exec directive has a spurious tag")}|
      ErrorLog], httpd_util:key1search(Context,errmsg,""),R}.

%% cmd

cmd(Info, Context, ErrorLog, R, Command) ->
    process_flag(trap_exit,true),
    Env  = env(Info),
    Dir  = filename:dirname(Command),
    Port = (catch open_port({spawn,Command},[stream,{cd,Dir},{env,Env}])),
    case Port of
	P when port(P) ->
	    {NewErrorLog, Result} = proxy(Port, ErrorLog),
	    {ok, Context, NewErrorLog, Result, R};
	{'EXIT', Reason} ->
	    ?vlog("open port failed: exit"
		  "~n   URI:    ~p"
		  "~n   Reason: ~p",
		  [Info#mod.request_uri,Reason]),
	    exit({open_port_failed,Reason,
		  [{uri,Info#mod.request_uri},{script,Command},
		   {env,Env},{dir,Dir}]});
	O ->
	    ?vlog("open port failed: unknown result"
		  "~n   URI: ~p"
		  "~n   O:   ~p",
		  [Info#mod.request_uri,O]),
	    exit({open_port_failed,O,
		  [{uri,Info#mod.request_uri},{script,Command},
		   {env,Env},{dir,Dir}]})
    end.

env(Info) ->
    [{"DOCUMENT_NAME",document_name(Info#mod.data,Info#mod.config_db,
				    Info#mod.request_uri)},
     {"DOCUMENT_URI", document_uri(Info#mod.config_db, Info#mod.request_uri)},
     {"QUERY_STRING_UNESCAPED", query_string_unescaped(Info#mod.request_uri)},
     {"DATE_LOCAL", date_local()},
     {"DATE_GMT", date_gmt()},
     {"LAST_MODIFIED", last_modified(Info#mod.data, Info#mod.config_db,
				     Info#mod.request_uri)}
    ].

%% cgi

cgi(Info, Context, ErrorLog, R, RequestURI) ->
    ScriptAliases = httpd_util:multi_lookup(Info#mod.config_db, script_alias),
    case mod_alias:real_script_name(Info#mod.config_db, RequestURI,
				    ScriptAliases) of
	{Script, AfterScript} ->
	    exec_script(Info,Script,AfterScript,ErrorLog,Context,R);
	not_a_script ->
	    {ok, Context,
	     [{internal_info, ?NICE(RequestURI++" is not a script")}|
	      ErrorLog], httpd_util:key1search(Context, errmsg, ""),R}
    end.

remove_header([]) ->
    [];
remove_header([$\n,$\n|Rest]) ->
    Rest;
remove_header([C|Rest]) ->
    remove_header(Rest).


exec_script(Info,Script,AfterScript,ErrorLog,Context,R) ->
    process_flag(trap_exit,true),
    Aliases = httpd_util:multi_lookup(Info#mod.config_db, alias),
    {_, Path, AfterPath} = mod_alias:real_name(Info#mod.config_db,
					       Info#mod.request_uri,
					       Aliases),
    Env  = env(Info)++mod_cgi:env(Info, Path, AfterPath),
    Dir  = filename:dirname(Path),
    Port = (catch open_port({spawn,Script},[stream,{env, Env},{cd, Dir}])),
    case Port of
	P when port(P) ->
	    %% Send entity body to port.
	    Res = case Info#mod.entity_body of
		      [] ->
			  true;
		      EntityBody ->
			  (catch port_command(Port,EntityBody))
		  end,
	    case Res of
		{'EXIT', Reason} ->
		    ?vlog("port send failed:"
			  "~n   Port:   ~p"
			  "~n   URI:    ~p"
			  "~n   Reason: ~p",
			  [Port,Info#mod.request_uri,Reason]),
		    exit({open_cmd_failed,Reason,
			  [{mod,?MODULE},{port,Port},
			   {uri,Info#mod.request_uri},
			   {script,Script},{env,Env},{dir,Dir},
			   {ebody_size,sz(Info#mod.entity_body)}]});
		true ->
		    {NewErrorLog, Result} = proxy(Port, ErrorLog),
		    {ok, Context, NewErrorLog, remove_header(Result), R}
	    end;
	{'EXIT', Reason} ->
	    ?vlog("open port failed: exit"
		  "~n   URI:    ~p"
		  "~n   Reason: ~p",
		  [Info#mod.request_uri,Reason]),
	    exit({open_port_failed,Reason,
		  [{mod,?MODULE},{uri,Info#mod.request_uri},{script,Script},
		   {env,Env},{dir,Dir}]});
	O ->
	    ?vlog("open port failed: unknown result"
		  "~n   URI: ~p"
		  "~n   O:   ~p",
		  [Info#mod.request_uri,O]),
	    exit({open_port_failed,O,
		  [{mod,?MODULE},{uri,Info#mod.request_uri},{script,Script},
		   {env,Env},{dir,Dir}]})
    end.


%%
%% Port communication
%%

proxy(Port,ErrorLog) ->
    process_flag(trap_exit, true),
    proxy(Port, ErrorLog, []).

proxy(Port, ErrorLog, Result) ->
    receive
	{Port, {data, Response}} ->
	    proxy(Port, ErrorLog, lists:append(Result,Response));
	{'EXIT', Port, normal} when port(Port) ->
	    process_flag(trap_exit, false),
	    {ErrorLog, Result};
	{'EXIT', Port, Reason} when port(Port) ->
	    process_flag(trap_exit, false),
	    {[{internal_info,
	       ?NICE("Scrambled output from CGI-script")}|ErrorLog],
	     Result};
	{'EXIT', Pid, Reason} when pid(Pid) ->
	    process_flag(trap_exit, false),
	    {'EXIT', Pid, Reason};
	%% This should not happen!
	WhatEver ->
	    process_flag(trap_exit, false),
	    {ErrorLog, Result}
    end.


%% ------
%% Temporary until I figure out a way to fix send_in_chunks
%% (comments and directives that start in one chunk but end
%% in another is not handled).
%%

send_in(Info, Path,Head, {ok,FileInfo}) ->
    case file:read_file(Path) of
	{ok, Bin} ->
	    send_in1(Info, binary_to_list(Bin), Head, FileInfo);
	{error, Reason} ->
	    ?vlog("failed reading file: ~p",[Reason]),
	    {error, {open,Reason}}
    end;
send_in(Info,Path,Head,{error,Reason}) ->
    ?vlog("failed open file: ~p",[Reason]),
    {error, {open,Reason}}.

send_in1(Info, Data,Head,FileInfo) ->
    {ok, _Context, Err, ParsedBody} = parse(Info,Data,?DEFAULT_CONTEXT,[],[]),
    Size = length(ParsedBody),
    ?vdebug("send_in1 -> Size: ~p",[Size]),
    Head1 = case Info#mod.http_version of
		"HTTP/1.1"->
		    Head ++
			"Content-Length: " ++
			integer_to_list(Size) ++
			"\r\nEtag:" ++
			httpd_util:create_etag(FileInfo,Size) ++"\r\n" ++
			"Last-Modified: " ++
			httpd_util:rfc1123_date(FileInfo#file_info.mtime)  ++
			"\r\n\r\n";
		_->
		    %% i.e http/1.0 and http/0.9
		    Head ++
			"Content-Length: " ++
			integer_to_list(Size) ++
			"\r\nLast-Modified: " ++
			httpd_util:rfc1123_date(FileInfo#file_info.mtime)  ++
			"\r\n\r\n"
	    end,
    httpd_socket:deliver(Info#mod.socket_type,Info#mod.socket,
			 [Head1,ParsedBody]),
    {ok, Err, Size}.



%%
%% Addition to "Fuzzy" HTML parser. This is actually a ugly hack to
%% avoid putting to much data on the heap. To be rewritten...
%%

% -define(CHUNK_SIZE, 4096).

% send_in_chunks(Info, Path) ->
%     ?DEBUG("send_in_chunks -> Path: ~p",[Path]),
%     case file:open(Path, [read, raw]) of
% 	{ok, Stream} ->
% 	    send_in_chunks(Info, Stream, ?DEFAULT_CONTEXT,[]);
% 	{error, Reason} ->
% 	    ?ERROR("Failed open file: ~p",[Reason]),
% 	    {error, {open,Reason}}
%     end.

% send_in_chunks(Info, Stream, Context, ErrorLog) ->
%     case file:read(Stream, ?CHUNK_SIZE) of
% 	{ok, Data} ->
% 	    ?DEBUG("send_in_chunks -> read ~p bytes",[length(Data)]),
% 	    {ok, NewContext, NewErrorLog, ParsedBody}=
% 		parse(Info, Data, Context, ErrorLog, []),
% 	    httpd_socket:deliver(Info#mod.socket_type,
% 				 Info#mod.socket, ParsedBody),
% 	    send_in_chunks(Info,Stream,NewContext,NewErrorLog);
% 	eof ->
% 	    {ok, ErrorLog};
% 	{error, Reason} ->
% 	    ?ERROR("Failed read from file: ~p",[Reason]),
% 	    {error, {read,Reason}}
%     end.


%%
%% "Fuzzy" HTML parser
%%

parse(Info,Body) ->
  parse(Info, Body, ?DEFAULT_CONTEXT, [], []).

parse(Info, [], Context, ErrorLog, Result) ->
    {ok, Context, lists:reverse(ErrorLog), lists:reverse(Result)};
parse(Info,[$<,$!,$-,$-,$#|R1],Context,ErrorLog,Result) ->
  ?DEBUG("parse -> start command directive when length(R1): ~p",[length(R1)]),
  case catch parse0(R1,Context) of
    {parse_error,Reason} ->
      parse(Info,R1,Context,[{internal_info,?NICE(Reason)}|ErrorLog],
	    [$#,$-,$-,$!,$<|Result]);
    {ok,Context,Command,TagList,ValueList,R2} ->
      ?DEBUG("parse -> Command: ~p",[Command]),
      {ok,NewContext,NewErrorLog,MoreResult,R3}=
	handle(Info,Context,ErrorLog,Command,TagList,ValueList,R2),
      parse(Info,R3,NewContext,NewErrorLog,lists:reverse(MoreResult)++Result)
  end;
parse(Info,[$<,$!,$-,$-|R1],Context,ErrorLog,Result) ->
  ?DEBUG("parse -> start comment when length(R1) = ~p",[length(R1)]),
  case catch parse5(R1,[],0) of
    {parse_error,Reason} ->
      ?ERROR("parse -> parse error: ~p",[Reason]),
      parse(Info,R1,Context,[{internal_info,?NICE(Reason)}|ErrorLog],Result);
    {Comment,R2} ->
      ?DEBUG("parse -> length(Comment) = ~p, length(R2) = ~p",
	     [length(Comment),length(R2)]),
      parse(Info,R2,Context,ErrorLog,Comment++Result)
  end;
parse(Info,[C|R],Context,ErrorLog,Result) ->
  parse(Info,R,Context,ErrorLog,[C|Result]).

handle(Info,Context,ErrorLog,Command,TagList,ValueList,R) ->
  case catch apply(?MODULE,Command,[Info,Context,ErrorLog,TagList,ValueList,
				    R]) of
    {'EXIT',{undef,_}} ->
      throw({parse_error,"Unknown command "++atom_to_list(Command)++
	     " in parsed doc"});
    Result ->
      Result
  end.

parse0([],Context) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse0([$-,$-,$>|R],Context) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse0([$ |R],Context) ->
  parse0(R,Context);
parse0(String,Context) ->
  parse1(String,Context,"").

parse1([],Context,Command) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse1([$-,$-,$>|R],Context,Command) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse1([$ |R],Context,Command) ->
  parse2(R,Context,list_to_atom(lists:reverse(Command)),[],[],"");
parse1([C|R],Context,Command) ->
  parse1(R,Context,[C|Command]).

parse2([],Context,Command,TagList,ValueList,Tag) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse2([$-,$-,$>|R],Context,Command,TagList,ValueList,Tag) ->
  {ok,Context,Command,TagList,ValueList,R};
parse2([$ |R],Context,Command,TagList,ValueList,Tag) ->
  parse2(R,Context,Command,TagList,ValueList,Tag);
parse2([$=|R],Context,Command,TagList,ValueList,Tag) ->
  parse3(R,Context,Command,[list_to_atom(lists:reverse(Tag))|TagList],
	 ValueList);
parse2([C|R],Context,Command,TagList,ValueList,Tag) ->
  parse2(R,Context,Command,TagList,ValueList,[C|Tag]).

parse3([],Context,Command,TagList,ValueList) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse3([$-,$-,$>|R],Context,Command,TagList,ValueList) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse3([$ |R],Context,Command,TagList,ValueList) ->
  parse3(R,Context,Command,TagList,ValueList);
parse3([$"|R],Context,Command,TagList,ValueList) ->
  parse4(R,Context,Command,TagList,ValueList,"");
parse3(String,Context,Command,TagList,ValueList) ->
  throw({parse_error,"Premature EOF in parsed file"}).

parse4([],Context,Command,TagList,ValueList,Value) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse4([$-,$-,$>|R],Context,Command,TagList,ValueList,Value) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse4([$"|R],Context,Command,TagList,ValueList,Value) ->
  parse2(R,Context,Command,TagList,[lists:reverse(Value)|ValueList],"");
parse4([C|R],Context,Command,TagList,ValueList,Value) ->
  parse4(R,Context,Command,TagList,ValueList,[C|Value]).

parse5([],Comment,Depth) ->
  ?ERROR("parse5 -> unterminated comment of ~p bytes when Depth = ~p",
	 [length(Comment),Depth]),
  throw({parse_error,"Premature EOF in parsed file"});
parse5([$<,$!,$-,$-|R],Comment,Depth) ->
  parse5(R,[$-,$-,$!,$<|Comment],Depth+1);
parse5([$-,$-,$>|R],Comment,0) ->
  {">--"++Comment++"--!<",R};
parse5([$-,$-,$>|R],Comment,Depth) ->
  parse5(R,[$>,$-,$-|Comment],Depth-1);
parse5([C|R],Comment,Depth) ->
  parse5(R,[C|Comment],Depth).


sz(B) when binary(B) -> {binary,size(B)};
sz(L) when list(L)   -> {list,length(L)};
sz(_)                -> undefined.


%% send_error - Handle failure to send the file
%%
send_error({open,Reason},Info,Path) -> open_error(Reason,Info,Path);
send_error({read,Reason},Info,Path) -> read_error(Reason,Info,Path).


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

read_error(_Reason,_Info,Path) ->
    read_error(500,none,Path,"").

read_error(StatusCode,none,Path,Reason) ->
    {StatusCode,none,?NICE("Can't read "++Path++Reason)};
read_error(StatusCode,Info,Path,Reason) ->
    {StatusCode,Info#mod.request_uri,?NICE("Can't read "++Path++Reason)}.

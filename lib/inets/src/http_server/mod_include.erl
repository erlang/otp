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
-module(mod_include).
-export([do/1,parse/2,config/6,include/6,echo/6,fsize/6,flastmod/6,exec/6]).

-include("httpd.hrl").
-include("httpd_internal.hrl").

-define(VMODULE,"INCLUDE").

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
			    do_include(Info);
			%% A response has been generated or sent!
			_Response ->
			    {proceed,Info#mod.data}
		    end
	    end;
	%% Not a GET method!
	_ ->
	    {proceed,Info#mod.data}
    end.

do_include(Info) ->
    Path = mod_alias:path(Info#mod.data,Info#mod.config_db,
			  Info#mod.request_uri),
    Suffix = httpd_util:suffix(Path),
    case httpd_util:lookup_mime_default(Info#mod.config_db,Suffix) of
	"text/x-server-parsed-html" ->
	    HeaderStart = [{content_type, "text/html"}], 
	    case send_in(Info, Path, HeaderStart, file:read_file_info(Path)) of
		{ok, ErrorLog, Size} ->
		    {proceed, [{response, {already_sent, 200, Size}},
			       {mime_type, "text/html"} |
			       lists:append(ErrorLog, Info#mod.data)]};
		{error, Reason} ->
		    {proceed,
		     [{status,send_error(Reason,Info,Path)}|Info#mod.data]}
	    end;
	_ -> %% Unknown mime type, ignore
	    {proceed,Info#mod.data}
    end.


%%
%% config directive
%%

config(_Info, Context, ErrorLog, TagList, ValueList, R) ->
    case verify_tags("config",[errmsg,timefmt,sizefmt],
		     TagList,ValueList) of
	ok ->
	    {ok,update_context(TagList,ValueList,Context),ErrorLog,"",R};
	{error,Reason} ->
	    {ok,Context,[{internal_info,Reason}|ErrorLog],
	     proplists:get_value(errmsg,Context,""),R}
    end.

update_context([],[],Context) ->
    Context;
update_context([Tag|R1],[Value|R2],Context) ->
    update_context(R1,R2,[{Tag,Value}|Context]).

verify_tags(Command,ValidTags,TagList,ValueList) 
  when length(TagList) =:= length(ValueList) ->
    verify_tags(Command, ValidTags, TagList);
verify_tags(Command, _ValidTags, _TagList, _ValueList) ->
    {error, ?NICE(Command ++ " directive has spurious tags")}.

verify_tags(_Command, _ValidTags, []) ->
    ok;
verify_tags(Command, ValidTags, [Tag|Rest]) ->
    case lists:member(Tag, ValidTags) of
	true ->
	    verify_tags(Command, ValidTags, Rest);
	false ->
	    {error, ?NICE(Command++" directive has a spurious tag ("++
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
include(_Info, Context, ErrorLog, _TagList, _ValueList, R) ->
    {ok, Context,
     [{internal_info,?NICE("include directive has a spurious tag")}|
      ErrorLog], proplists:get_value(errmsg, Context, ""), R}.

include(Info, Context, ErrorLog, R, Path) ->
    case file:read_file(Path) of
	{ok, Body} ->
	    {ok, NewContext, NewErrorLog, Result} =
		parse(Info, binary_to_list(Body), Context, ErrorLog, []),
	    {ok, NewContext, NewErrorLog, Result, R};
	{error, _Reason} ->
	    {ok, Context, 
	     [{internal_info, ?NICE("Can't open "++Path)}|ErrorLog],
	     proplists:get_value(errmsg, Context, ""), R}
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
echo(_Info,Context,ErrorLog,[var],["DATE_LOCAL"],R) ->
    {ok,Context,ErrorLog,date_local(),R};
echo(_Info,Context,ErrorLog,[var],["DATE_GMT"],R) ->
    {ok,Context,ErrorLog,date_gmt(),R};
echo(Info,Context,ErrorLog,[var],["LAST_MODIFIED"],R) ->
    {ok,Context,ErrorLog,last_modified(Info#mod.data,Info#mod.config_db,
				       Info#mod.request_uri),R};
echo(_Info, Context, ErrorLog, _TagList, _ValueList, R) ->
    {ok,Context,
     [{internal_info,?NICE("echo directive has a spurious tag")}|
      ErrorLog],"(none)",R}.

document_name(Data,ConfigDB,RequestURI) ->
    Path = mod_alias:path(Data,ConfigDB,RequestURI),
    case inets_regexp:match(Path,"[^/]*\$") of
	{match,Start,Length} ->
	    string:substr(Path,Start,Length);
	nomatch ->
	    "(none)"
    end.

document_uri(ConfigDB, RequestURI) ->
    Aliases = httpd_util:multi_lookup(ConfigDB, alias),
    
    {_, Path, AfterPath}  = mod_alias:real_name(ConfigDB, RequestURI, Aliases),
    
    VirtualPath = string:substr(RequestURI, 1, 
				length(RequestURI)-length(AfterPath)),
    {match, Start, Length} = inets_regexp:match(Path,"[^/]*\$"),
    FileName = string:substr(Path,Start,Length),
    case inets_regexp:match(VirtualPath, FileName++"\$") of
	{match, _, _} ->
	    http_uri:decode(VirtualPath)++AfterPath;
	nomatch ->
	    string:strip(http_uri:decode(VirtualPath),right,$/)++
		"/"++FileName++AfterPath
    end.

query_string_unescaped(RequestURI) ->
  case inets_regexp:match(RequestURI,"[\?].*\$") of
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
  Aliases = httpd_util:multi_lookup(Info#mod.config_db,alias),
  {_,Path, _AfterPath}=
    mod_alias:real_name(Info#mod.config_db,VirtualPath,Aliases),
  fsize(Info, Context, ErrorLog, R, Path);
fsize(Info,Context,ErrorLog,[file],[FileName],R) ->
  Path = file(Info#mod.config_db,Info#mod.request_uri,FileName),
  fsize(Info,Context,ErrorLog,R,Path);
fsize(_Info, Context, ErrorLog, _TagList, _ValueList, R) ->
  {ok,Context,[{internal_info,?NICE("fsize directive has a spurious tag")}|
	       ErrorLog],proplists:get_value(errmsg,Context,""),R}.

fsize(_Info, Context, ErrorLog, R, Path) ->
    case file:read_file_info(Path) of
	{ok,FileInfo} ->
	    case proplists:get_value(sizefmt,Context) of
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
		     proplists:get_value(errmsg, Context, ""), R}
	    end;
	{error, _Reason} ->
	    {ok,Context,[{internal_info,?NICE("Can't open "++Path)}|ErrorLog],
	     proplists:get_value(errmsg,Context,""),R}
    end.

%%
%% flastmod directive
%%

flastmod(#mod{config_db = Db} = Info, 
	 Context, ErrorLog, [virtual], [VirtualPath],R) ->
    Aliases = httpd_util:multi_lookup(Db,alias),
    {_,Path, _AfterPath} = mod_alias:real_name(Db, VirtualPath, Aliases),
    flastmod(Info,Context,ErrorLog,R,Path);
flastmod(#mod{config_db = Db, request_uri = RequestUri} = Info, 
	 Context, ErrorLog, [file], [FileName], R) ->
    Path = file(Db, RequestUri, FileName),
    flastmod(Info, Context, ErrorLog, R, Path);
flastmod(_Info, Context, ErrorLog, _TagList, _ValueList, R) ->
    {ok,Context,
     [{internal_info,?NICE("flastmod directive has a spurious tag")}|
      ErrorLog],proplists:get_value(errmsg,Context,""),R}.

flastmod(_Info, Context, ErrorLog, R, File) ->
    case file:read_file_info(File) of
	{ok, FileInfo} ->
	    {{Yr,Mon,Day},{Hour,Minute,Second}}=FileInfo#file_info.mtime,
	    Result =
		io_lib:format("~s ~s ~2w ~w:~w:~w ~w",
			      [httpd_util:day(
				 calendar:day_of_the_week(Yr,Mon, Day)),
			       httpd_util:month(Mon),Day,Hour,Minute,Second, Yr]),
	    {ok, Context, ErrorLog, Result, R};
	{error, _Reason} ->
	    {ok,Context,[{internal_info,?NICE("Can't open "++File)}|ErrorLog],
	     proplists:get_value(errmsg,Context,""),R}
    end.

%%
%% exec directive
%%

exec(Info,Context,ErrorLog,[cmd],[Command],R) ->
    cmd(Info,Context,ErrorLog,R,Command);
exec(Info,Context,ErrorLog,[cgi],[RequestURI],R) ->
    cgi(Info,Context,ErrorLog,R,RequestURI);
exec(_Info, Context, ErrorLog, _TagList, _ValueList, R) ->
    {ok, Context,
     [{internal_info,?NICE("exec directive has a spurious tag")}|
      ErrorLog], proplists:get_value(errmsg,Context,""),R}.

%% cmd

cmd(Info, Context, ErrorLog, R, Command) ->
    process_flag(trap_exit,true),    
    Env  = env(Info),
    Dir  = filename:dirname(Command),
    Port = (catch open_port({spawn,Command},[stream,{cd,Dir},{env,Env}])),
    case Port of
	P when is_port(P) ->
	    {NewErrorLog, Result} = proxy(Port, ErrorLog),
	    {ok, Context, NewErrorLog, Result, R};
	{'EXIT', Reason} ->
	    exit({open_port_failed,Reason,
		  [{uri,Info#mod.request_uri},{script,Command},
		   {env,Env},{dir,Dir}]});
	O ->
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
	      ErrorLog], proplists:get_value(errmsg, Context, ""),R}
    end.

remove_header([]) ->
    [];
remove_header([$\n,$\n|Rest]) ->
    Rest;
remove_header([_C|Rest]) ->
    remove_header(Rest).


exec_script(#mod{config_db = Db, request_uri = RequestUri} = Info, 
	    Script, _AfterScript, ErrorLog, Context, R) ->
    process_flag(trap_exit,true),    
    Aliases = httpd_util:multi_lookup(Db, alias),
    {_, Path, AfterPath} = mod_alias:real_name(Db, RequestUri, Aliases),
    Env  = env(Info) ++ mod_cgi:env(Info, Path, AfterPath),
    Dir  = filename:dirname(Path),
    Port = (catch open_port({spawn,Script},[stream,{env, Env},{cd, Dir}])),
    case Port of
	P when is_port(P) ->
	    %% Send entity body to port.
	    Res = case Info#mod.entity_body of
		      [] ->
			  true;
		      EntityBody ->
			  (catch port_command(Port, EntityBody))
		  end,
	    case Res of
		{'EXIT', Reason} ->
		    exit({open_cmd_failed,Reason,
			  [{mod,?MODULE},{port,Port},
			   {uri,RequestUri},
			   {script,Script},{env,Env},{dir,Dir},
			   {ebody_size,sz(Info#mod.entity_body)}]});
		true ->
		    {NewErrorLog, Result} = proxy(Port, ErrorLog),
		    {ok, Context, NewErrorLog, remove_header(Result), R}
	    end;
	{'EXIT', Reason} ->
	    exit({open_port_failed,Reason,
		  [{mod,?MODULE},{uri,RequestUri},{script,Script},
		   {env,Env},{dir,Dir}]});
	O ->
	    exit({open_port_failed,O,
		  [{mod,?MODULE},{uri,RequestUri},{script,Script},
		   {env,Env},{dir,Dir}]})
    end.
    

%%
%% Port communication
%%

proxy(Port, ErrorLog) ->
    process_flag(trap_exit, true),
    proxy(Port, ErrorLog, []).

proxy(Port, ErrorLog, Result) ->
    receive
	{Port, {data, Response}} ->
	    proxy(Port, ErrorLog, lists:append(Result,Response));
	{'EXIT', Port, normal} when is_port(Port) ->
	    process_flag(trap_exit, false),
	    {ErrorLog, Result};
	{'EXIT', Port, _Reason} when is_port(Port) ->
	    process_flag(trap_exit, false),
	    {[{internal_info,
	       ?NICE("Scrambled output from CGI-script")}|ErrorLog],
	     Result};
	{'EXIT', Pid, Reason} when is_pid(Pid) ->
	    process_flag(trap_exit, false),
	    {'EXIT', Pid, Reason};
	%% This should not happen!
	_WhatEver ->
	    process_flag(trap_exit, false),
	    {ErrorLog, Result}
    end.


%% ------
%% Temporary until I figure out a way to fix send_in_chunks
%% (comments and directives that start in one chunk but end
%% in another is not handled).
%%

send_in(Info, Path, Head, {ok,FileInfo}) ->
    case file:read_file(Path) of
	{ok, Bin} ->
	    send_in1(Info, binary_to_list(Bin), Head, FileInfo);
	{error, Reason} ->
	    {error, {read,Reason}}
    end;
send_in(_Info , _Path, _Head,{error,Reason}) ->
    {error, {open,Reason}}.

send_in1(Info, Data, Head, FileInfo) ->
    {ok, _Context, Err, ParsedBody} = parse(Info,Data,?DEFAULT_CONTEXT,[],[]),
    Size = length(ParsedBody),
    LastModified = case catch httpd_util:rfc1123_date(FileInfo#file_info.mtime) of
		       Date when is_list(Date) -> [{last_modified,Date}];
		       _ -> []
		   end,
    Head1 = case Info#mod.http_version of 
		"HTTP/1.1"->
		    Head ++ [{content_length, integer_to_list(Size)},  
			     {etag, httpd_util:create_etag(FileInfo,Size)}|
			     LastModified];
		_->
		    %% i.e http/1.0 and http/0.9
		    Head ++  [{content_length, integer_to_list(Size)}|  
			      LastModified]
	    end,
    httpd_response:send_header(Info, 200, Head1),
    httpd_socket:deliver(Info#mod.socket_type,Info#mod.socket, ParsedBody),
    {ok, Err, Size}.


parse(Info,Body) ->
  parse(Info, Body, ?DEFAULT_CONTEXT, [], []).

parse(_Info, [], Context, ErrorLog, Result) ->
    {ok, Context, lists:reverse(ErrorLog), lists:reverse(Result)};
parse(Info,[$<,$!,$-,$-,$#|R1],Context,ErrorLog,Result) ->
  case catch parse0(R1,Context) of
    {parse_error,Reason} ->
      parse(Info,R1,Context,[{internal_info,?NICE(Reason)}|ErrorLog],
	    [$#,$-,$-,$!,$<|Result]);
    {ok,Context,Command,TagList,ValueList,R2} ->
	  {ok,NewContext,NewErrorLog,MoreResult,R3}=
	      handle(Info,Context,ErrorLog,Command,TagList,ValueList,R2),
	  parse(Info,R3,NewContext,NewErrorLog,
		lists:reverse(MoreResult)++Result)
  end;
parse(Info,[$<,$!,$-,$-|R1],Context,ErrorLog,Result) ->
  case catch parse5(R1,[],0) of
    {parse_error,Reason} ->
	  parse(Info,R1,Context,
		[{internal_info,?NICE(Reason)}|ErrorLog],Result);
      {Comment,R2} ->
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

parse0([], _Context) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse0([$-,$-,$>|_R], _Context) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse0([$ |R], Context) ->
  parse0(R,Context);
parse0(String, Context) ->
  parse1(String, Context,"").

parse1([], _Context, _Command) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse1([$-,$-,$>|_R], _Context, _Command) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse1([$ |R], Context, Command) ->
  parse2(R,Context,list_to_atom(lists:reverse(Command)),[],[],"");
parse1([C|R], Context, Command) ->
  parse1(R,Context,[C|Command]).

parse2([], _Context, _Command, _TagList, _ValueList, _Tag) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse2([$-,$-,$>|R], Context, Command, TagList, ValueList, _Tag) ->
  {ok,Context,Command,TagList,ValueList,R};
parse2([$ |R],Context,Command,TagList,ValueList,Tag) ->
  parse2(R,Context,Command,TagList,ValueList,Tag);
parse2([$=|R],Context,Command,TagList,ValueList,Tag) ->
  parse3(R,Context,Command,[list_to_atom(lists:reverse(Tag))|TagList],
	 ValueList);
parse2([C|R],Context,Command,TagList,ValueList,Tag) ->
  parse2(R,Context,Command,TagList,ValueList,[C|Tag]).

parse3([], _Context, _Command, _TagList, _ValueList) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse3([$-,$-,$>|_R], _Context, _Command, _TagList, _ValueList) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse3([$ |R], Context, Command, TagList, ValueList) ->
  parse3(R, Context, Command, TagList, ValueList);
parse3([$"|R], Context, Command, TagList, ValueList) ->
  parse4(R,Context,Command,TagList,ValueList,"");
parse3(_String, _Context, _Command, _TagList, _ValueList) ->
  throw({parse_error,"Premature EOF in parsed file"}).

parse4([], _Context, _Command, _TagList, _ValueList, _Value) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse4([$-,$-,$>|_R], _Context, _Command, _TagList, _ValueList, _Value) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse4([$"|R],Context,Command,TagList,ValueList,Value) ->
  parse2(R,Context,Command,TagList,[lists:reverse(Value)|ValueList],"");
parse4([C|R],Context,Command,TagList,ValueList,Value) ->
  parse4(R,Context,Command,TagList,ValueList,[C|Value]).

parse5([], _Comment, _Depth) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse5([$<,$!,$-,$-|R],Comment,Depth) ->
  parse5(R,[$-,$-,$!,$<|Comment],Depth+1);
parse5([$-,$-,$>|R],Comment,0) ->
  {">--"++Comment++"--!<",R};
parse5([$-,$-,$>|R],Comment,Depth) ->
  parse5(R,[$>,$-,$-|Comment],Depth-1);
parse5([C|R],Comment,Depth) ->
  parse5(R,[C|Comment],Depth).


sz(B) when is_binary(B) -> {binary,size(B)};
sz(L) when is_list(L)   -> {list,length(L)};
sz(_)                   -> undefined.

%% send_error - Handle failure to send the file
%%
send_error({open,Reason},Info,Path) -> 
    httpd_file:handle_error(Reason, "open", Info, Path);
send_error({read,Reason},Info,Path) -> 
    httpd_file:handle_error(Reason, "read", Info, Path).





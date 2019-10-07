%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
-module(mod_alias).

-export([do/1, 
	 real_name/3,
	 real_script_name/3,
	 default_index/2,
	 load/2,
	 store/2,
	 path/3]).

-include("httpd.hrl").
-include("httpd_internal.hrl").
-include("inets_internal.hrl").

-define(VMODULE,"ALIAS").

%% do

do(#mod{data = Data} = Info) ->
    case proplists:get_value(status, Data) of
	%% A status code has been generated!
	{_StatusCode, _PhraseArgs, _Reason} ->
	    {proceed, Data};
	%% No status code has been generated!
	undefined ->
	    case proplists:get_value(response, Data) of
		%% No response has been generated!
		undefined ->
		    do_alias(Info);
		%% A response has been generated or sent!
		_Response ->
		    {proceed, Data}
	    end
    end.

do_alias(#mod{config_db   = ConfigDB, 
	      request_uri = ReqURI,
	      socket_type = SocketType,
	      data        = Data}) ->
    {ShortPath, Path, AfterPath} = 
	real_name(ConfigDB, ReqURI, which_alias(ConfigDB)),
    %% Relocate if a trailing slash is missing else proceed!
    LastChar = lists:last(ShortPath),
    case file:read_file_info(ShortPath) of 
	{ok, FileInfo} when ((FileInfo#file_info.type =:= directory) andalso 
			     (LastChar =/= $/)) ->
	    ServerName = which_server_name(ConfigDB), 
	    Port = port_string(which_port(ConfigDB)),
	    Protocol = get_protocol(SocketType),
	    URL = Protocol ++ ServerName ++ Port ++ ReqURI ++ "/",
	    ReasonPhrase = httpd_util:reason_phrase(301),
	    Message = httpd_util:message(301, URL, ConfigDB),
	    {proceed,
	     [{response,
	       {301, ["Location: ", URL, "\r\n"
		      "Content-Type: text/html\r\n",
		      "\r\n",
		      "<HTML>\n<HEAD>\n<TITLE>",ReasonPhrase,
		      "</TITLE>\n</HEAD>\n"
		      "<BODY>\n<H1>",ReasonPhrase,
		      "</H1>\n", Message, 
		      "\n</BODY>\n</HTML>\n"]}}|
	      [{real_name, {Path, AfterPath}} | Data]]};
	_NoFile ->
	    {proceed, [{real_name, {Path, AfterPath}} | Data]}
    end.

port_string(80) ->
    "";
port_string(Port) ->
    ":" ++ integer_to_list(Port).

get_protocol(ip_comm) ->
    "http://";
get_protocol(_) ->
    %% Should clean up to have only one ssl type essl vs ssl is not relevant any more
    "https://".

%% real_name

real_name(ConfigDB, RequestURI, []) ->
    DocumentRoot = which_document_root(ConfigDB), 
    RealName = DocumentRoot ++ RequestURI,
    {ShortPath, _AfterPath} = httpd_util:split_path(RealName),
    {Path, AfterPath} = 
	httpd_util:split_path(default_index(ConfigDB, RealName)),
    {ShortPath, Path, AfterPath};

real_name(ConfigDB, RequestURI, [{MP,Replacement}| _] = Aliases)
  when element(1, MP) =:= re_pattern ->
    case longest_match(Aliases, RequestURI) of
	{match, {MP, Replacement}} ->
	    NewURI = re:replace(RequestURI, MP, Replacement, [{return,list}]),
	    {ShortPath,_} = httpd_util:split_path(NewURI),
	    {Path,AfterPath} =
		httpd_util:split_path(default_index(ConfigDB, NewURI)),
	    {ShortPath, Path, AfterPath};
	nomatch ->
	    real_name(ConfigDB, RequestURI, [])
    end;

real_name(ConfigDB, RequestURI,  [{_,_}|_] = Aliases) ->
    case longest_match(Aliases, RequestURI) of
	{match, {FakeName, RealName}} ->
	    ActualName = re:replace(RequestURI,
				    "^" ++ FakeName, RealName, [{return,list}]),
 	    {ShortPath, _AfterPath} = httpd_util:split_path(ActualName),
	    {Path, AfterPath} =
		httpd_util:split_path(default_index(ConfigDB, ActualName)),
	    {ShortPath, Path, AfterPath};
	nomatch ->
	    real_name(ConfigDB, RequestURI, [])
    end.

longest_match(Aliases, RequestURI) ->
    longest_match(Aliases, RequestURI, _LongestNo = 0, _LongestAlias = undefined).

longest_match([{FakeName, RealName} | Rest], RequestURI, LongestNo, LongestAlias) ->
    case re:run(RequestURI, "^" ++ FakeName, [{capture, first}]) of
	{match, [{_, Length}]} ->
	    if
		Length > LongestNo ->
		    longest_match(Rest, RequestURI, Length, {FakeName, RealName});
		true ->
		    longest_match(Rest, RequestURI, LongestNo, LongestAlias)
	    end;
	nomatch ->
	    longest_match(Rest, RequestURI, LongestNo, LongestAlias)
    end;
longest_match([], _RequestURI, 0, _LongestAlias) ->
    nomatch;
longest_match([], _RequestURI, _LongestNo, LongestAlias) ->
    {match, LongestAlias}.

%% real_script_name

real_script_name(_ConfigDB, _RequestURI, []) ->
    not_a_script;
real_script_name(ConfigDB, RequestURI, [{FakeName,RealName} | Rest]) ->
    case re:run(RequestURI, "^" ++ FakeName, [{capture, none}]) of
	match ->
	    ActualName0 =
		re:replace(RequestURI, "^" ++ FakeName, RealName,  [{return,list}]),
            ActualName = abs_script_path(ConfigDB, ActualName0),
	    httpd_util:split_script_path(default_index(ConfigDB, ActualName));
	nomatch ->
	    real_script_name(ConfigDB, RequestURI, Rest)
    end.

%% ERL-574: relative path in script_alias property results in malformed url
abs_script_path(ConfigDB, [$.|_] = RelPath) ->
    Root = httpd_util:lookup(ConfigDB, server_root),
    Root ++ "/" ++ RelPath;
abs_script_path(_, RelPath) ->
    RelPath.

%% default_index

default_index(ConfigDB, Path) ->
    case file:read_file_info(Path) of
	{ok, FileInfo} when FileInfo#file_info.type =:= directory ->
	    DirectoryIndex = which_directory_index(ConfigDB),
	    append_index(Path, DirectoryIndex);
	_ ->
	    Path
    end.

append_index(RealName, []) ->
    RealName;
append_index(RealName, [Index | Rest]) ->
    case file:read_file_info(filename:join(RealName, Index)) of
	{error, _Reason} ->
	    append_index(RealName, Rest);
	_ ->
	    filename:join(RealName, Index)
    end.

%% path

path(Data, ConfigDB, RequestURI) ->
    case proplists:get_value(real_name, Data) of
	undefined ->
	    DocumentRoot = which_document_root(ConfigDB), 
	    {Path, _AfterPath} = 
		httpd_util:split_path(DocumentRoot ++ RequestURI),
	    Path;
	{Path, _AfterPath} ->
	    Path
    end.

%%
%% Configuration
%%

%% load

load("DirectoryIndex " ++ DirectoryIndex, []) ->
    DirectoryIndexes = re:split(DirectoryIndex," ", [{return, list}]),
    {ok,[], {directory_index, DirectoryIndexes}};
load("Alias " ++ Alias, []) ->
    case re:split(Alias," ", [{return, list}]) of
	[FakeName, RealName] ->
	    {ok,[],{alias,{FakeName,RealName}}};
	_ ->
	    {error,?NICE(string:strip(Alias)++" is an invalid Alias")}
    end;
load("ReWrite " ++ Rule, Acc) ->
    load_re_write(Rule, Acc, "ReWrite", re_write);
load("ScriptAlias " ++ ScriptAlias, []) ->
    case re:split(ScriptAlias, " ", [{return, list}]) of
	[FakeName, RealName] ->
	    %% Make sure the path always has a trailing slash..
	    RealName1 = filename:join(filename:split(RealName)),
	    {ok, [], {script_alias, {FakeName, RealName1++"/"}}};
	_ ->
	    {error, ?NICE(string:strip(ScriptAlias)++
			      " is an invalid ScriptAlias")}
    end;
load("ScriptReWrite " ++ Rule, Acc) ->
    load_re_write(Rule, Acc, "ScriptReWrite", script_re_write).

load_re_write(Rule0, Acc, Type, Tag) ->
    case lists:dropwhile(
	   fun ($\s) -> true; ($\t) -> true; (_) -> false end,
	   Rule0) of
	"" ->
	    {error, ?NICE(string:strip(Rule0)++" is an invalid "++Type)};
	Rule ->
	    case string:chr(Rule, $\s) of
		0 ->
		    {ok, Acc, {Tag, {Rule, ""}}};
		N ->
		    {Re, [_|Replacement]} = lists:split(N-1, Rule),
		    {ok, Acc, {Tag, {Re, Replacement}}}
	    end
    end.

store({directory_index, Value} = Conf, _) when is_list(Value) ->
    case is_directory_index_list(Value) of
	true ->
	    {ok, Conf};
	false ->
	    {error, {wrong_type, {directory_index, Value}}}
    end;
store({directory_index, Value}, _) ->
    {error, {wrong_type, {directory_index, Value}}};
store({alias, {Fake, Real}} = Conf, _)
  when is_list(Fake), is_list(Real) ->
    {ok, Conf};
store({alias, Value}, _) ->
    {error, {wrong_type, {alias, Value}}};
store({re_write, {Re, Replacement}} = Conf, _)
  when is_list(Re), is_list(Replacement) ->
    case re:compile(Re) of
	{ok, MP} ->
	    {ok, {alias, {MP, Replacement}}};
	{error,_} ->
	    {error, {re_compile, Conf}}
    end;
store({re_write, _} = Conf, _) ->
    {error, {wrong_type, Conf}};
store({script_alias, {Fake, Real}} = Conf, _) 
  when is_list(Fake), is_list(Real) ->
    {ok, Conf};
store({script_alias, Value}, _) ->
    {error, {wrong_type, {script_alias, Value}}};
store({script_re_write, {Re, Replacement}} = Conf, _)
  when is_list(Re), is_list(Replacement) ->
    case re:compile(Re) of
	{ok, MP} ->
	    {ok, {script_alias, {MP, Replacement}}};
	{error,_} ->
	    {error, {re_compile, Conf}}
    end;
store({script_re_write, _} = Conf, _) ->
    {error, {wrong_type, Conf}}.

is_directory_index_list([]) ->
    true;
is_directory_index_list([Head | Tail]) when is_list(Head) ->
    is_directory_index_list(Tail);
is_directory_index_list(_) ->
    false.


%% ---------------------------------------------------------------------

which_alias(ConfigDB) ->
    httpd_util:multi_lookup(ConfigDB, alias). 

which_server_name(ConfigDB) ->
    httpd_util:lookup(ConfigDB, server_name).

which_port(ConfigDB) ->
    httpd_util:lookup(ConfigDB, port, 80). 

which_document_root(ConfigDB) ->
    httpd_util:lookup(ConfigDB, document_root, "").

which_directory_index(ConfigDB) ->
    httpd_util:lookup(ConfigDB, directory_index, []).
